################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP01                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza          #
#                la integracion para la Unificacion de cuentas solo IMSS       #
#Fecha inicio => 21/05/2012                                                    #
################################################################################
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/08/2014
-- Fecha ultima modificacion: 05/11/2014 MHM
--===============================================================

GLOBALS "UNIG01.4gl"
GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
          g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod      LIKE cat_operacion.opera_cod -- codigo de operacion
   
   DEFINE g_reg_modulo RECORD  --Almacena las rutas de archivos 
          ruta_exp         CHAR(40),
          ruta_rescate     CHAR(40),
          ruta_listados    CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
           ruta_listados   CHAR(40)
   END RECORD       
END GLOBALS
###############################################################################
MAIN
   DEFINE p_pid                                 LIKE bat_ctr_operacion.pid, -- PID del proceso
          p_proceso_cod                         LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
          p_opera_cod                           LIKE bat_ctr_operacion.opera_cod, -- codigo de la operacion
          p_usuario_cod                         LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          v_s_sql                               STRING, -- cadena con una instruccion SQL
          v_i_resultado                         INTEGER, -- resultado del proceso
          r_bnd_fin_oper                        SMALLINT,
          p_nombre_archivo                      LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
          v_msj_sql                             CHAR(200),
          p_folio                               LIKE deo_preliquida.folio_liquida,
          p_titulo                              STRING, -- titulo del mensaje enviado en el correo
          p_mensaje                             STRING, -- cuerpo del mensaje enviado
          v_layout                              LIKE cat_operacion.layout_cod,
          v_ruta_rescate                        STRING,
          v_usuario                             LIKE seg_modulo.usuario,
          v_proceso_desc                        LIKE cat_proceso.proceso_desc,
          v_extension                           LIKE cat_operacion.extension,
          v_opera_desc                          LIKE cat_operacion.opera_desc,
          v_ruta_listados                       LIKE seg_modulo.ruta_listados,
          v_sum_total_registro                  INTEGER,
          v_si_solicitudes_aceptadas_unificadas INTEGER,
          v_si_solicitudes_aceptadas_unificador INTEGER,
          v_total_rch_imss                      INTEGER,   
          v_s_comando                           STRING,
          v_s_qry                               STRING,
          v_folio_solicitud                     DECIMAL(9,0),
          v_ind_sql                             STRING
          
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

    CALL STARTLOG(p_usuario_cod CLIPPED|| ".UNIP01.log")

   CALL fn_recupera_inf_proceso(p_proceso_cod,
                                p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario

   LET g_proceso_cod = p_proceso_cod
   LET g_opera_cod   = p_opera_cod

   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod
   
   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin,
          s.ruta_rescate,
          s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'

   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'
   
   
   LET v_i_resultado = 0

   --Si existe un folio sin liquidar, se rechaza 
   LET v_s_qry = " SELECT folio ",
              "\n  FROM   glo_folio ",
              "\n  WHERE  proceso_cod = ",g_proceso_cod,
              "\n  AND    opera_cod   = ",g_opera_cod,
              "\n  AND    status      = 0 ",
              "\n  AND    folio_referencia IS NULL "
              
   PREPARE prp_folio_sol FROM v_s_qry
   EXECUTE prp_folio_sol INTO v_folio_solicitud

   IF v_folio_solicitud IS NULL THEN 
      --Genera el folio de la integración   
      CALL fn_genera_folio(g_proceso_cod,
                           g_opera_cod,
                           v_usuario)
           RETURNING v_folio_solicitud

      
      --Crea indice a tabla temporal
      DATABASE safre_tmp

      CREATE INDEX uni_ix_2 ON tmp_det_cta_unificadas_op21 (nss_unificador_traajador)            
      CREATE INDEX uni_ix_4 ON tmp_det_cta_unificadora_op21 (nss_unificador)

      UPDATE STATISTICS FOR TABLE tmp_det_cta_unificadas_op21            
      UPDATE STATISTICS FOR TABLE tmp_det_cta_unificadora_op21      

      DATABASE safre_viv

      LET v_s_sql = "EXECUTE FUNCTION fn_uni_integra_imss(?, ?, ?, ?, ?)"
      PREPARE Prppintegrauni FROM v_s_sql
      
      EXECUTE Prppintegrauni USING p_usuario_cod, 
                                   p_proceso_cod, 
                                   p_nombre_archivo, 
                                   v_folio_solicitud, 
                                   p_pid
              INTO v_i_resultado, 
                   v_msj_sql, 
                   v_si_solicitudes_aceptadas_unificador,
                   v_si_solicitudes_aceptadas_unificadas
                   
      CASE      
         WHEN (v_i_resultado = 0) --Cuando no se encuentra ningún errror
                               
            IF v_si_solicitudes_aceptadas_unificador >= 0 THEN
               
               DISPLAY "#  Ya se puede continuar con la validación del Resultado de Confrontación"
               LET v_sum_total_registro = v_si_solicitudes_aceptadas_unificador + v_si_solicitudes_aceptadas_unificadas;
               LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                               "#  La integración se terminó completamente.","\n",
                               "#  Integración realizada con exito","\n",
                               "#  Folio lote o de integración : ",v_folio_solicitud,"\n",
                               "#  Total de familias     : ",v_si_solicitudes_aceptadas_unificador,"\n",
                               "#  Total de unificadores : ",v_si_solicitudes_aceptadas_unificador,"\n",
                               "#  Total de unificados   : ",v_si_solicitudes_aceptadas_unificadas,"\n",
                               "#  Total de registros    : ",v_sum_total_registro,"\n",
                               "#  Ya se puede continuar con la validación del Resultado de Confrontación","\n",
                               "# # # # # # # # # # # # # # # # # # # # # # # # # #"
                               
            ELSE
               CALL fn_error_opera(g_pid,
                                   g_proceso_cod,
                                   g_opera_cod)
                    RETURNING r_bnd_fin_oper
               
               DISPLAY "#  Codigo de error :",v_i_resultado,"\n"
               DISPLAY "#  Mensaje de error :",v_msj_sql           
               
               LET p_mensaje = " --- ERROR ---\n",
                               " El proceso de integración no terminó correctamente.\n",
                               " Código de error : ", r_bnd_fin_oper,"\n ",
                               " FECHA           : ",TODAY,"\n",
                               " HORA            : ",CURRENT HOUR TO SECOND,"\n"
                               
               DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
               
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
                  
         WHEN (v_i_resultado = NOTFOUND)   --Cuando no se encuentra el error
            --DISPLAY "NOT FOUND"
            
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod)
                 RETURNING r_bnd_fin_oper
            
            DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
            DISPLAY "#  Error. No se integró ninguna solicitud"
            
            LET p_mensaje = " --- ERROR ---\n",
                            " El proceso de integración no terminó correctamente.\n",
                            " Código de error : ", r_bnd_fin_oper,"\n ",
                            " FECHA           : ",TODAY,"\n",
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"
                                     
         WHEN (v_i_resultado < 0)   --Cuando existe algún error
            --DISPLAY SQLERRMESSAGE
            
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod)
                 RETURNING r_bnd_fin_oper
               
            DISPLAY " Codigo de error :",v_i_resultado,"\n"
            DISPLAY " Mensaje de error :",v_msj_sql
            DISPLAY " Error al procesar la integración"
            DISPLAY " No se puede continuar..."
         
            
            LET p_mensaje = " --- ERROR ---\n",
                            " El proceso de integración no terminó correctamente.\n",
                            " Código de error : ", r_bnd_fin_oper,"\n ",
                            " FECHA           : ",TODAY,"\n",
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"
           
      END CASE
   
      --Se obtiene sel total de rechazados
      SELECT COUNT(*)
      INTO   v_total_rch_imss    
      FROM   uni_det_unificador 
      WHERE  estado_familia = 2
      AND    estado_unificacion = 2
      AND    folio_unificacion = v_folio_solicitud
   
      IF v_total_rch_imss > = 1 THEN 
         --Se genera el archivo de rechazos
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIS04 ",
                                                 p_usuario_cod, " ",
                                                 g_pid  , " " ,
                                                 g_proceso_cod , " " ,
                                                 g_opera_cod ," ",
                                                 v_folio_solicitud, " ",
                                                 "'",p_nombre_archivo CLIPPED, "' ", 
                                                 v_si_solicitudes_aceptadas_unificador,
                                                 v_si_solicitudes_aceptadas_unificadas,
                                                 " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                                 "/nohup:",g_pid        USING "&&&&&",":",
                                                 g_proceso_cod USING "&&&&&",":",
                                                 g_opera_cod   USING "&&&&&" ,
                                                 " 2>&1 &"
         --DISPLAY v_s_comando
         RUN v_s_comando
   
         DISPLAY "Se ha generado un archivo de rechazos"
          
         CALL fn_actualiza_opera_fin(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod)
              RETURNING r_bnd_fin_oper   
          
      ELSE
         CALL fn_actualiza_opera_fin(g_pid,
                                     g_proceso_cod,
                                     g_opera_cod)
              RETURNING r_bnd_fin_oper       
      END IF 

      
      LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"
         
      CALL fn_correo_proceso(g_pid,
                             g_proceso_cod,
                             g_opera_cod,
                             "", -- no lleva archivo adjunto
                             p_titulo,
                             p_mensaje)
   ELSE
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
           RETURNING r_bnd_fin_oper

      DELETE 
      FROM   glo_ctr_archivo
      WHERE  nombre_archivo = p_nombre_archivo
      
      DISPLAY "Error al procesar la integración"
      DISPLAY "El folio " || v_folio_solicitud || " no se ha liquidado con el archivo de confronta"

      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de integración no terminó correctamente.\n",
                      " Código de error : Existen folios sin liquidar", "\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"
   END IF
END MAIN
################################################################################
{
 Obtiene la descripcion del error de la validacion y
  la muestra en mensaje para suario.
}
FUNCTION fn_mues_desc_valida(p_resultado_opera)
   DEFINE p_resultado_opera SMALLINT,
         v_descripcion     LIKE cat_bat_parametro_salida.descripcion
  
   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   --CALL fn_mensaje("Atención",v_descripcion CLIPPED,"information")
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida
###############################################################################
FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE v_si_detalle  SMALLINT,
          v_c_mensaje   CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]" 
   END IF

   RETURN v_c_mensaje
   
END FUNCTION 
