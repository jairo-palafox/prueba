--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/11/2013
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP15                                                        #
#Objetivo     => Programa lanzado de la integración Unificación Complementaria #
#                Recurrente Por Archivo                                        # 
#Fecha inicio => 06/11/2013                                                    #
################################################################################

--Lanzador UNIL44

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
       ,p_usuario            LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,v_s_sql              STRING -- cadena con una instruccion SQL
       ,v_i_resultado        INTEGER -- resultado del proceso
       ,r_bnd_fin_oper       SMALLINT
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       ,v_msj_sql            CHAR(200)
       ,v_folio              LIKE deo_preliquida.folio_liquida
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
   
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_pid            = ARG_VAL(1)
   LET p_proceso_cod    = ARG_VAL(2)
   LET p_opera_cod      = ARG_VAL(3)
   LET p_usuario        = ARG_VAL(4)
   LET v_folio          = ARG_VAL(5)
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
   LET g_proceso_cod = 2314 --Unificación Recurrente Por Archivo
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
   CALL fn_genera_folio(g_proceso_cod,g_opera_cod,p_usuario)
   RETURNING v_folio
   
   --DISPLAY "Folio ", v_folio
   
   -- se contruye el enuncionado SQL
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_integra_recurrente(?, ?, ?, ?, ?)"
   
   PREPARE prp_integra_uni FROM v_s_sql
   EXECUTE prp_integra_uni USING p_usuario, 
                                 p_proceso_cod, 
                                 p_nombre_archivo, 
                                 v_folio, 
                                 p_pid
                            INTO v_i_resultado, 
                                 v_msj_sql, 
                                 v_si_solicitudes_aceptadas_unificador,
                                 v_si_solicitudes_aceptadas_unificadas,
                                 v_sum_total_registro

   --Valida que mensajes mostrar en el monitor de procesos de acuerdo al resultado        
   CASE
   --Cuando no se encuentra ningún errror
   WHEN (v_i_resultado = 0)
      --Se finaliza aunque existan errores
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "#    La integración se terminó completamente."
      DISPLAY "#    "
      DISPLAY "#    Folio lote o de integración: "||v_folio
      
      IF v_i_resultado = 0 AND v_sum_total_registro <> 0 THEN
         DISPLAY "#  Integración realizada con exito"
      ELSE
         DISPLAY "#  Integración realizada pero con errores de validación"
      END IF
      
      DISPLAY "#  "
      DISPLAY "#  Total de familias     : ",v_si_solicitudes_aceptadas_unificador
      DISPLAY "#  Total de unificadores : ",v_si_solicitudes_aceptadas_unificador
      DISPLAY "#  Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas
      DISPLAY "#    "

      IF v_si_solicitudes_aceptadas_unificador >= 0 THEN
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         DISPLAY "#  Ya se puede continuar con la Preliquidación"
         
         LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                         "#  La integración se terminó completamente.","\n",
                         "#  ","\n",
                         "#  Integración realizada con exito","\n",
                         "#  ","\n",
                         "#  Folio lote o de integración : ",v_folio,"\n",
                         "#  ","\n",
                         "#  Total de familias     : ",v_si_solicitudes_aceptadas_unificador,"\n",
                         "#  Total de unificadores : ",v_si_solicitudes_aceptadas_unificador,"\n",
                         "#  Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas,"\n",
                         "#  ","\n",
                         "#  Total de registros    : ",v_sum_total_registro,"\n",
                         "#  Ya se puede Continuar con la Preliquidación","\n",
                         "# # # # # # # # # # # # # # # # # # # # # # # # # #"
                         
      ELSE
         CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bnd_fin_oper
         
         DISPLAY "#  Codigo de error :",v_i_resultado,"\n"
         DISPLAY "#  Mensaje de error :",v_msj_sql           
         
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
         CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) 
         RETURNING v_mensaje
         DISPLAY v_mensaje
        
         IF(v_i_resultado <> 100)THEN
            DISPLAY "#  Error. No se integró ninguna solicitud"
         ELSE
            DISPLAY "#  ",fn_status_secciones_integradas(v_si_solicitudes_aceptadas_unificadas)
         END IF
         
         LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"           
      END IF
      
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "\n\n"
   --Cuando no se encuentra el error
   WHEN (SQLCA.SQLCODE = NOTFOUND)
      DISPLAY "NOT FOUND"
      
      CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bnd_fin_oper

      CALL fn_recupera_inconsis_opera(r_bnd_fin_oper) 
      RETURNING v_mensaje

      DISPLAY "#  Error. No se integró ninguna solicitud"

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
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_bin CLIPPED,"/UNIS08 ",
                                              g_pid  , " " ,
                                              g_proceso_cod , " " ,
                                              g_opera_cod ," ",
                                              p_usuario, " ",
                                              v_folio, " ",
                                              "'",p_nombre_archivo CLIPPED, "' ", 
                                              p_mensaje, " ",
                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                              "/nohup:",g_pid        USING "&&&&&",":",
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

FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE 
     v_si_detalle        SMALLINT
    --
    ,v_c_mensaje CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   RETURN v_c_mensaje
   
END FUNCTION 