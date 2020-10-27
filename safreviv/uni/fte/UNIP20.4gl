--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23/09/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP20                                                        #
#Objetivo     => Lanzado integración unificación                               #
#Fecha inicio => 23/09/2015                                                    #
################################################################################
--Lanzador UNIL60
IMPORT os

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE g_reg_modulo   RECORD  --Almacena las rutas de archivos 
        ruta_bin         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

MAIN
DEFINE p_pid                 LIKE bat_ctr_operacion.pid -- PID del proceso
       ,p_proceso_cod        LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod          LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,v_s_sql              STRING -- cadena con una instruccion SQL
       ,v_i_resultado        INTEGER -- resultado del proceso
       ,r_bnd_fin_oper       SMALLINT
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       ,v_msj_sql            CHAR(200)
       ,v_folio              DECIMAL(9,0)
       ,p_titulo             STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje            STRING -- cuerpo del mensaje enviado
       ,v_layout             LIKE cat_operacion.layout_cod
       ,v_ruta_rescate       STRING
       ,v_usuario            LIKE seg_modulo.usuario
       ,v_proceso_desc       LIKE cat_proceso.proceso_desc
       ,v_extension          LIKE cat_operacion.extension
       ,v_opera_desc         LIKE cat_operacion.opera_desc
       ,v_ruta_listados      LIKE seg_modulo.ruta_listados
       ,v_sum_total_registro INTEGER
       ,v_si_solicitudes_aceptadas_unificadas INTEGER
       ,v_si_solicitudes_aceptadas_unificador INTEGER
       ,v_total_rch_imss     INTEGER    
       ,v_s_comando          STRING 
       ,v_mensaje            CHAR(150)
       ,v_ind_actualizacion  SMALLINT
       ,v_tot_solicitudes    INTEGER 
       ,v_tot_actualizacion  INTEGER

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| "UNIP20.log")
       
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET V_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,
                                         v_extension, 
                                         v_opera_desc,
                                         v_layout, 
                                         v_ruta_rescate,
                                         v_ruta_listados,
                                         v_usuario
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = 2318 --Nuevo modelo de unificación IMSS
   LET g_opera_cod   = 2    --Integración

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   
   WHENEVER ERROR CONTINUE
   
   LET v_i_resultado = 0

   --Genera el folio de la integración 
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario_cod)
   RETURNING v_folio

   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_integra_nvo_modelo_imss(?, ?, ?, ?, ?)"
   
   PREPARE prp_integra_uni FROM v_s_sql
   EXECUTE prp_integra_uni USING p_usuario_cod, 
                                 p_proceso_cod, 
                                 p_nombre_archivo, 
                                 v_folio, 
                                 p_pid
                            INTO v_i_resultado, 
                                 v_msj_sql, 
                                 v_si_solicitudes_aceptadas_unificador,
                                 v_si_solicitudes_aceptadas_unificadas,
                                 v_ind_actualizacion

   CASE
   --Cuando no se encuentra ningún errror
   WHEN (v_i_resultado = 0)
      LET v_sum_total_registro = (v_si_solicitudes_aceptadas_unificador + v_si_solicitudes_aceptadas_unificadas)
      --Se finaliza aunque existan errores
      DISPLAY ""
      DISPLAY "-- La integración se terminó completamente. --"
      DISPLAY ""
      DISPLAY "   Folio lote o de integración: "||v_folio
      
      IF v_i_resultado = 0 AND v_sum_total_registro <> 0 THEN
         DISPLAY "   Integración realizada con exito"
      ELSE
         DISPLAY "   Integración realizada pero con errores de validación"
      END IF
    
      DISPLAY "   Total de familias     : ",v_si_solicitudes_aceptadas_unificador
      DISPLAY "   Total de unificadores : ",v_si_solicitudes_aceptadas_unificador
      DISPLAY "   Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas

      IF v_si_solicitudes_aceptadas_unificador >= 0 THEN
         --Se invoca función de notificaciones 
         CALL fn_notifica_proceso(v_folio, g_proceso_cod, p_usuario_cod)
         DISPLAY " "
         DISPLAY "-- Se ha ejecutado el proceso de notificación --"
         DISPLAY "   Folio unificación : ",v_folio
         DISPLAY "   Proceso           : ",g_proceso_cod
         DISPLAY "   Usuario           : ",p_usuario_cod

         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         DISPLAY "   Ya se puede continuar con la Preliquidación"
         
         LET p_mensaje = "   La integración se terminó completamente.","\n",
                         "   Integración realizada con exito","\n",
                         "   Folio lote o de integración : ",v_folio,"\n",
                         "   Total de familias     : ",v_si_solicitudes_aceptadas_unificador,"\n",
                         "   Total de unificadores : ",v_si_solicitudes_aceptadas_unificador,"\n",
                         "   Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas,"\n",
                         "   ","\n",
                         "   Total de registros    : ",v_sum_total_registro,"\n",
                         "   Ya se puede Continuar con la Preliquidación","\n"
      ELSE
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
         
         DISPLAY "   Codigo de error :",v_i_resultado
         DISPLAY "   Mensaje de error :",v_msj_sql           
         
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) 
         RETURNING v_mensaje
         DISPLAY v_mensaje
        
         IF(v_i_resultado <> 100)THEN
            DISPLAY "   Error. No se integró ninguna solicitud"
         ELSE
            IF(v_si_solicitudes_aceptadas_unificadas = 1)THEN
                DISPLAY "   [ Error en Detalle ]" 
            END IF
         END IF

         LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
      END IF

      DISPLAY "\n\n"

      SELECT COUNT (*)
      INTO   v_tot_solicitudes 
      FROM   safre_tmp:tmp_det_cuenta_unificadora
      
      SELECT COUNT (*)
      INTO   v_tot_actualizacion
      FROM   uni_det_unificador
      WHERE  ind_procedencia IN (0,2)
      AND    folio_unificacion = v_folio

      IF v_tot_solicitudes = v_tot_actualizacion THEN
         CALL fn_finaliza_proceso(v_folio) 
      END IF 
      
   --Cuando no se encuentra el error
   WHEN (SQLCA.SQLCODE = NOTFOUND)
      DISPLAY "NOT FOUND"
      
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_fin_oper

      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) 
      RETURNING v_mensaje

      DISPLAY "  Error. No se integró ninguna solicitud"

      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"

   --Cuando existe algún error
   WHEN (v_i_resultado < 0)
      DISPLAY SQLERRMESSAGE
      
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_fin_oper
         
      DISPLAY "Codigo Error SQL:",v_i_resultado
      DISPLAY "Error al procesar la integración en : ", v_msj_sql
      DISPLAY "No se puede continuar..."

      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
     
   END CASE

   --Se obtiene el total de rechazados
   SELECT COUNT(*)
   INTO   v_total_rch_imss    
   FROM   uni_det_unificador 
   WHERE  estado_familia = 2
   AND    estado_unificacion = 2
   AND    folio_unificacion = v_folio

   --Si existe algún registro con rechazos
   IF v_total_rch_imss > = 1 THEN 
      --Se genera el archivo de rechazos
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/UNIS14 ",
                                              p_usuario_cod, " ",
                                              g_pid  , " " ,
                                              g_proceso_cod , " " ,
                                              g_opera_cod ," ",
                                              v_folio, " ",
                                              "'",p_nombre_archivo CLIPPED, "' ",
                                              v_si_solicitudes_aceptadas_unificador ," ",
                                              v_si_solicitudes_aceptadas_unificadas ," ",
                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                              "/nohup:",g_pid USING "&&&&&",":",
                                              g_proceso_cod USING "&&&&&",":",
                                              g_opera_cod   USING "&&&&&" ,
                                              " 2>&1 &"
       --DISPLAY v_s_comando
       RUN v_s_comando

       DISPLAY "Se ha generado un archivo de rechazos"       
   END IF 
   --#  

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
      
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   WHENEVER ERROR STOP
END MAIN



#OBJETIVO: Marcar todo el proceso como finalizado, aún cuando no haya registros a liquidar.
FUNCTION fn_finaliza_proceso(p_folio_unificacion)

DEFINE r_bnd_fin_oper      SMALLINT,
       p_folio_unificacion DECIMAL(9,0),
       v_nombre_archivo    CHAR(40),
       v_nombre_log        STRING,
       v_crea_archivo      STRING,
       v_ruta_listados     CHAR(40)

   DISPLAY "   Integración no se termino completamente \n"
   DISPLAY "\n"
   DISPLAY "   No se integró ningún registro \n"
   DISPLAY "\n"

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   LET v_nombre_log = "/errnohup:",
                      g_pid         USING "&&&&&",":",
                      g_proceso_cod USING "&&&&&",":",
                      6             USING "&&&&&"

   LET v_crea_archivo = "> "||v_ruta_listados CLIPPED || v_nombre_log
   CALL FGL_SYSTEM(v_crea_archivo)

   --Inicia la PRELIQUIDACIóN
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               3,--g_opera_cod,
                               0,
                               "UNIL62",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper

   --Finaliza PRELIQUIDACIóN
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               3)   --- Clave de la operación
        RETURNING r_bnd_fin_oper

   --Inicia la LIQUIDACIóN
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               4,--g_opera_cod,
                               0,
                               "UNIL63",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
   --Finaliza LIQUIDACIóN
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               4)             --- Clave de la operación
        RETURNING r_bnd_fin_oper

   --Inicia la INDICADORES
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               5,--g_opera_cod,
                               0,
                               "UNIL64",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
   --Finaliza INDICADORES
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               5)             --- Clave de la operación
        RETURNING r_bnd_fin_oper

   --Inicia la ARCHIVO SALIDA
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               6,--g_opera_cod,
                               0,
                               "UNIL65",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
   --Finaliza ARCHIVO SALIDA
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               6)             --- Clave de la operación
        RETURNING r_bnd_fin_oper


   UPDATE glo_folio 
   SET    status = 5,
          folio_referencia = p_folio_unificacion
   WHERE  folio = p_folio_unificacion;

   SELECT nom_archivo 
   INTO   v_nombre_archivo
   FROM   bat_ctr_operacion
   WHERE  pid = g_pid
   AND    opera_cod = 1;

   UPDATE uni_det_unificador 
   SET    folio_liquidacion = p_folio_unificacion,
          f_liquidacion     = TODAY
   WHERE  folio_unificacion = p_folio_unificacion
   AND    estado_familia = 1;

   --Actualiza folio en monitor de procesos
   UPDATE bat_ctr_operacion 
   SET    nom_archivo = v_nombre_archivo
   WHERE  pid         = g_pid
   AND    proceso_cod = g_proceso_cod
   AND    opera_cod   IN (3,4,5,6);

END FUNCTION