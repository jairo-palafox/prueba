--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13/01/2016
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP23                                                        #
#Objetivo     => Lanzado de la integración Unificación Complementaria          #
#Fecha inicio => Enero 13, 2015                                                #
################################################################################

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
       ,v_tot_aceptadas      INTEGER
       ,v_tot_rechazadas     INTEGER    
       ,v_mensaje            CHAR(150)
       ,v_tot_solicitudes    INTEGER
       ,v_isam_err           INTEGER

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| "UNIP20.log")
       
   ##Ejecuta prevalidación de saldos
   -- se recuperan los parametros la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
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
   LET v_s_sql = "EXECUTE FUNCTION fn_uni_integra_complementaria(?, ?, ?, ?, ?)"

   PREPARE prp_integra_uni FROM v_s_sql

   EXECUTE prp_integra_uni USING p_usuario_cod, 
                                 g_proceso_cod, 
                                 p_nombre_archivo, 
                                 v_folio, 
                                 g_pid
                            INTO v_i_resultado,
                                 v_tot_solicitudes,
                                 v_tot_aceptadas,
                                 v_tot_rechazadas,
                                 v_isam_err,
                                 v_msj_sql

   CASE
   --Cuando no se encuentra ningún errror
   WHEN (v_i_resultado = 0)
      --LET v_sum_total_registro = (v_tot_aceptadas + v_tot_rechazadas)
      --Se finaliza aunque existan errores
      DISPLAY "  La integración se terminó completamente."
      DISPLAY " "
      DISPLAY "  Folio lote o de integración: "||v_folio
      
      --IF v_i_resultado = 0 AND v_sum_total_registro <> 0 THEN
      IF v_i_resultado = 0 AND v_tot_aceptadas <> 0 THEN
         DISPLAY "  Integración realizada con exito"
      ELSE
         DISPLAY "  Integración realizada pero con errores de validación"
      END IF
    
      DISPLAY "  Total de familias   : ",v_tot_solicitudes
      DISPLAY "  Total de aceptados  : ",v_tot_aceptadas
      DISPLAY "  Total de rechazados : ",v_tot_rechazadas

      IF v_tot_aceptadas > 0 THEN
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper
         DISPLAY "  Ya se puede continuar con la Preliquidación"
         
         LET p_mensaje = "  La integración se terminó completamente.","\n",
                         "  ","\n",
                         "  Integración realizada con exito","\n",
                         "  ","\n",
                         "  Folio lote o de integración : ",v_folio,"\n",
                         "  ","\n",
                         "  Total de familias   : ",v_tot_solicitudes,"\n",
                         "  Total de aceptados  : ",v_tot_aceptadas,"\n",
                         "  Total de rechazados : ",v_tot_rechazadas,"\n",                    
                         "  ","\n",
                         "  Ya se puede Continuar con la Preliquidación","\n"
      END IF                    

      IF v_tot_aceptadas = 0 AND v_tot_rechazadas > 0 THEN
         CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bnd_fin_oper         

         CALL fn_finaliza_proceso(v_folio)
         DISPLAY "Finaliza operaciones "

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
         
      DISPLAY " Código Error SQL:",v_i_resultado
      DISPLAY " Mensaje : ", v_msj_sql
      DISPLAY " No se puede continuar..."

      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
     
   END CASE

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
       v_nombre_archivo    CHAR(40)

   DISPLAY "    \n"
   DISPLAY "    No se integró ningún registro \n"
   DISPLAY "    \n"

   --Inicia la PRELIQUIDACIóN
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               3,--g_opera_cod,
                               0,
                               "UNIL74",
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
                               "UNIL75",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
   --Finaliza LIQUIDACIóN
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               4)             --- Clave de la operación
        RETURNING r_bnd_fin_oper

{   --Inicia ARCHIVO SALIDA
   CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               5,--g_opera_cod,
                               0,
                               "UNIL76",
                               "",
                               p_usuario_cod)
        RETURNING r_bnd_fin_oper
   --Finaliza ARCHIVO SALIDA
   CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                               g_proceso_cod, --- Clave del proceso
                               5)             --- Clave de la operación
        RETURNING r_bnd_fin_oper
}
   UPDATE glo_folio 
   SET    status = 5,
          folio_referencia = p_folio_unificacion
   WHERE  folio = p_folio_unificacion;

   SELECT nom_archivo 
   INTO   v_nombre_archivo
   FROM   bat_ctr_operacion
   WHERE  pid = g_pid
   AND    opera_cod = 1;

   --Actualiza folio en monitor de procesos
   UPDATE bat_ctr_operacion 
   SET    nom_archivo = v_nombre_archivo
   WHERE  pid         = g_pid
   AND    proceso_cod = g_proceso_cod
   AND    opera_cod   IN (3,4,5,6);

END FUNCTION