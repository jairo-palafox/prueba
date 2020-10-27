################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP07                                                        #
#Objetivo     => Lanzado  para la integración del archivo confrontado de       #
#                unificación de cuentas IMSS.                                  #
#Fecha inicio => 21/05/2012                                                    #
################################################################################         

--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/08/2014
-- Fecha ultima modificacion: 06/11/2014 MHM
--===============================================================

DATABASE safre_viv
GLOBALS "UNIG01.4gl"
GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
          
   DEFINE g_reg_modulo   RECORD  --Almacena las rutas de archivos 
          ruta_exp      CHAR(40),
          ruta_rescate  CHAR(40),
          ruta_listados CHAR(40)
   END RECORD
   
   DEFINE seg_modulo_bat RECORD
          ruta_listados CHAR(40)
   END RECORD
END GLOBALS
###############################################################################
MAIN                                                                               
   DEFINE p_pid              LIKE bat_ctr_operacion.pid,         -- PID del proceso
          p_proceso_cod      LIKE bat_ctr_operacion.proceso_cod, -- codigo del proceso
          p_opera_cod        LIKE bat_ctr_operacion.opera_cod,   -- codigo de la operacion
          p_usuario_cod      LIKE seg_usuario.usuario_cod,       -- clave del usuario firmado
          v_s_sql            STRING, -- cadena con una instruccion SQL
          v_i_resultado      INTEGER, -- resultado del proceso
          r_bnd_fin_oper     SMALLINT,
          p_nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
          v_msj_sql          CHAR(200),
          p_folio            LIKE deo_preliquida.folio_liquida,
          
          v_si_solicitudes_aceptadas_unificadas  INTEGER,
          
          p_titulo           STRING, -- titulo del mensaje enviado en el correo
          p_mensaje          STRING, -- cuerpo del mensaje enviado
          v_layout           LIKE cat_operacion.layout_cod,
          v_ruta_rescate     STRING,
          v_usuario          LIKE seg_modulo.usuario,
          v_proceso_desc     LIKE cat_proceso.proceso_desc,
          v_extension        LIKE cat_operacion.extension,
          v_opera_desc       LIKE cat_operacion.opera_desc,
          v_ruta_listados    LIKE seg_modulo.ruta_listados,
          v_resul_aceptadas  INTEGER,
          v_resul_rechazadas INTEGER,
          v_resul_pendientes INTEGER,
          v_s_comando        STRING, 
          v_s_qry            STRING,
          v_folio_solicitud  DECIMAL(9,0),
          v_cifras_control   STRING
                                                                                                                                                                                                                
   ##Ejecuta prevalidación de saldos                                                                    
   
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
                                                                
   LET g_proceso_cod = p_proceso_cod
   LET g_opera_cod   = p_opera_cod
   
   LET g_pid         = p_pid 

   CALL fn_recupera_inf_proceso(p_proceso_cod, 
                                p_opera_cod)                                             
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
                                                                                                        
   LET v_s_qry = " SELECT folio ",
              "\n  FROM   glo_folio ",
              "\n  WHERE  proceso_cod = 2309 ", --- folio de sol o21
              "\n  AND    opera_cod   = ",g_opera_cod,
              "\n  AND    status      = 0 ",
              "\n  AND    folio_referencia IS NULL "
                                       
   PREPARE prp_folio_sol FROM v_s_qry
   EXECUTE prp_folio_sol INTO v_folio_solicitud
   
   LET v_i_resultado = 0

   IF v_folio_solicitud IS NULL THEN 
      DISPLAY "No se puede continuar, no existen solicitudes previas a confrontar"
      
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)                                  
           RETURNING r_bnd_fin_oper                                                           
           
      LET p_mensaje = " --- ERROR ---\n",                                                
                      " El proceso de Integración no terminó correctamente.\n",             
                      " Código de error : ", r_bnd_fin_oper,"\n ",                             
                      " FECHA           : ",TODAY,"\n",                                        
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"     
   ELSE 
      LET v_s_sql = "EXECUTE FUNCTION fn_uni_integra_confronta_imss(?, ?, ?, ?, ?)"      
      PREPARE Prppintegrauniconfronta FROM v_s_sql
      
      -- se ejecuta el stored procedure de integracion
      EXECUTE Prppintegrauniconfronta USING p_usuario_cod, 
                                            p_proceso_cod, 
                                            p_nombre_archivo, 
                                            v_folio_solicitud, 
                                            p_pid
              INTO v_i_resultado, 
                   v_msj_sql, 
                   v_resul_aceptadas,
                   v_resul_rechazadas, 
                   v_resul_pendientes 
      
      CASE
         WHEN (SQLCA.SQLCODE = 0)
         
            LET v_cifras_control = "# # # # # # # # # # # # # # # # # # # # # # # # # #",
                                   "# \n ",
                                   "#    La integración se terminó completamente",
                                   "# \n ",
                                   "#    Folio integración confrontación: "||v_folio_solicitud,
                                   "# \n ",
                                   "#    Aceptadas confronta: "||v_resul_aceptadas,
                                   "# \n ",
                                   "#    Rechazadas confronta: "||v_resul_rechazadas,
                                   "# \n ",
                                   "#    Pendientes confronta: "||v_resul_pendientes,
                                   "# \n ",
                                   "#    Integración realizada con exito",
                                   "# \n ",
                                   "#    Registros afectados   : ",v_i_resultado, "-" ,v_msj_sql
         
            DISPLAY v_cifras_control
            
            IF v_i_resultado >= 0 THEN                                                               
               UPDATE bat_ctr_operacion
               SET    folio = v_folio_solicitud
               WHERE  pid   = p_pid
               AND    opera_cod = 2
         
               DISPLAY "#  Ya se puede Continuar con la Preliquidación"                                                                                                                                      
         
               CALL fn_actualiza_opera_fin(g_pid,
                                           g_proceso_cod,
                                           g_opera_cod)                          
                    RETURNING r_bnd_fin_oper       
         
            ELSE                                                                                     
               CALL fn_error_opera(g_pid,
                                   g_proceso_cod,
                                   g_opera_cod)                                  
                    RETURNING r_bnd_fin_oper                                                           
                    
               LET p_mensaje = " --- ERROR ---\n",                                                
                         " El proceso de Integración no terminó correctamente.\n",             
                         " Código de error : ", r_bnd_fin_oper,"\n ",                             
                         " FECHA           : ",TODAY,"\n",                                        
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"                        
                         
               DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
               
               IF(v_i_resultado <> 100)THEN                                                          
                  DISPLAY "#  Error. No se integró ninguna solicitud"                                
               ELSE                                                                                  
                  DISPLAY "#  ",fn_status_secciones_integradas(v_si_solicitudes_aceptadas_unificadas)
               END IF                                                                                
               
               LET p_mensaje = " --- ERROR ---\n",                                                   
                            " El proceso de Preliquidación no terminó correctamente.\n",             
                            " Código de error : ", r_bnd_fin_oper,"\n ",                             
                            " FECHA           : ",TODAY,"\n",                                        
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"                        
            END IF                                                                                   
            
            DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"                            
            DISPLAY "\n\n"
                                                                                                     
         WHEN (SQLCA.SQLCODE = NOTFOUND)                                                             
            
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod)                                     
                 RETURNING r_bnd_fin_oper                                                              
                 
            DISPLAY fn_recupera_inconsis_opera(r_bnd_fin_oper) 
            
            DISPLAY "#  Error. No se integró ninguna solicitud"                                      
            
            LET p_mensaje = " --- ERROR ---\n",                                                      
                            " El proceso de Integración no terminó correctamente.\n",                
                            " Código de error : ", r_bnd_fin_oper,"\n ",                             
                            " FECHA           : ",TODAY,"\n",                                        
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"                        
         WHEN (SQLCA.SQLCODE < 0)                                                                             
            CALL fn_error_opera(g_pid,
                                g_proceso_cod,
                                g_opera_cod)                                     
                 RETURNING r_bnd_fin_oper                                                              
                 
            DISPLAY "Codigo Error SQL:",SQLCA.SQLCODE                                                
            DISPLAY "Error al procesar la integración"                                               
            DISPLAY "No se puede continuar..."                                                       
            
            LET p_mensaje = " --- ERROR ---\n",                                                      
                            " El proceso de Integración no terminó correctamente.\n",             
                            " Código de error : ", r_bnd_fin_oper,"\n ",                             
                            " FECHA           : ",TODAY,"\n",                                        
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"                        
                                                                                                     
      END CASE                                                     
  
      --Recupera nombre del archivo 
      SELECT nom_archivo
      INTO   p_nombre_archivo
      FROM   bat_ctr_operacion
      WHERE  pid = g_pid
      AND    opera_cod = 1
      
      IF v_resul_rechazadas > = 1 THEN 
         --Se genera el archivo de rechazos
         LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIS05 ",
                                                 p_usuario_cod CLIPPED, " ",
                                                 g_pid  , " " ,
                                                 g_proceso_cod , " " ,
                                                 g_opera_cod ," ",
                                                 v_folio_solicitud, " ",
                                                 "'",p_nombre_archivo CLIPPED, "' ",
                                                 v_cifras_control CLIPPED,                                              
                                                 " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                                 "/nohup:",g_pid        USING "&&&&&",":",
                                                 g_proceso_cod USING "&&&&&",":",
                                                 g_opera_cod   USING "&&&&&" ,
                                                 " 2>&1 &"
          RUN v_s_comando
      
          DISPLAY "Se ha generado un archivo de rechazos"       
      END IF 
      --#  
      
      CALL fn_actualiza_opera_fin(g_pid,
                                  g_proceso_cod,
                                  g_opera_cod)                          
           RETURNING r_bnd_fin_oper              
                                                                                                        
      LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"         
                                                                                                        
      CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,                                           
                             "", -- no lleva archivo adjunto                
                             p_titulo,                                                                     
                             p_mensaje)   
   END IF
   
   WHENEVER ERROR STOP                                                                                  
END MAIN                                                                                                
################################################################################                                                                                                                                                                     
#OBJETIVO: Obtiene el statis de las secciones integradas
FUNCTION fn_status_secciones_integradas(v_si_detalle)
   DEFINE v_si_detalle SMALLINT,
          v_c_mensaje  CHAR(100)

   LET v_c_mensaje = ""

   IF(v_si_detalle = 1)THEN
      LET v_c_mensaje = v_c_mensaje CLIPPED, "   [ Error en Detalle ]"
   END IF

   RETURN v_c_mensaje

END FUNCTION