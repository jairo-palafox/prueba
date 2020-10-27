################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP06                                                        #
#Objetivo     => Programa lanzado de la integración del archivo operación 22   #
#Fecha inicio => 21/05/2012                                                    #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/09/2012
-- 03/12/2014 -- Se agrega validación si no se genera folio finaliza en error AG
--==============================================================================

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE g_reg_modulo   RECORD  --Almacena las rutas de archivos 
        ruta_exp         CHAR(40),
        ruta_rescate     CHAR(40),
        ruta_listados    CHAR(40)
       END RECORD,
       seg_modulo_bat RECORD
        ruta_listados    CHAR(40)
       END RECORD
END GLOBALS

MAIN
DEFINE p_pid                       LIKE bat_ctr_operacion.pid -- PID del proceso
       ,p_proceso_cod              LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod                LIKE bat_ctr_operacion.opera_cod -- codigo de la operacion
       ,p_usuario_cod              LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_folio                    LIKE deo_preliquida.folio_liquida
       ,r_bnd_fin_oper             SMALLINT
       ,p_nombre_archivo           LIKE glo_ctr_archivo.nombre_archivo -- nombre dle archivo
       --
       ,v_si_solicitudes_aceptadas_unificadas  SMALLINT
       ,p_titulo                  STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                 STRING -- cuerpo del mensaje enviado
       ,v_layout                  LIKE cat_operacion.layout_cod
       ,v_ruta_rescate            STRING
       ,v_usuario                 LIKE seg_modulo.usuario
       ,v_proceso_desc            LIKE cat_proceso.proceso_desc
       ,v_extension               LIKE cat_operacion.extension
       ,v_opera_desc              LIKE cat_operacion.opera_desc
       ,v_ruta_listados           LIKE seg_modulo.ruta_listados
       ,v_folio_integracion_op22  DECIMAL(9,0)
       ,v_cadena                  STRING
       ,v_s_comando               STRING
       ,v_total_rch_op22          INTEGER

       --Variables Control de errores
DEFINE v_resultado          SMALLINT, 
       v_isam               SMALLINT,  
       v_mensaje            VARCHAR(250),
       v_total_unificador   INTEGER,
       v_total_unificado    INTEGER,
       v_total_actualizados INTEGER

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension,
                  v_opera_desc,
                  v_layout,
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario
                  
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
        RETURNING v_folio_integracion_op22

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
   INTO   g_reg_modulo.*
   FROM   seg_modulo s
   WHERE  s.modulo_cod = 'uni'
   
   SELECT b.ruta_listados
   INTO   seg_modulo_bat.ruta_listados
   FROM   seg_modulo b
   WHERE  b.modulo_cod = 'bat'

   DISPLAY "#    Folio enviado: ", v_folio_integracion_op22 

   LET v_cadena = "EXECUTE FUNCTION fn_uni_integra_operacion22(?,?,?)"
                    
   PREPARE prp_integra_op22 FROM v_cadena
   EXECUTE prp_integra_op22 USING v_folio_integracion_op22,
                                  p_pid,
                                  p_nombre_archivo
                             INTO v_resultado, 
                                  v_isam,  
                                  v_mensaje,
                                  v_total_unificador,
                                  v_total_unificado,
                                  v_total_actualizados                 
  
   CASE
      WHEN (v_resultado = 0)
         DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
         DISPLAY "#    "
         DISPLAY "#    La integración se terminó completamente."
         DISPLAY "#    "
         DISPLAY "#    Folio integración operacion 22: " || v_folio_integracion_op22
         DISPLAY "#    "
         DISPLAY "#    Total de unificadores:          " || v_total_unificador
         DISPLAY "#    "
         DISPLAY "#    Total de unificados:            " || v_total_unificado 
         DISPLAY "#    "
         DISPLAY "#    Total Actualizados de Fecha:    " || v_total_actualizados 
         DISPLAY "#    "
         DISPLAY "#    Integración realizada con exito"
         DISPLAY "#    "
         DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
         
         IF v_resultado >= 0 THEN
            CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                 RETURNING r_bnd_fin_oper
            
            LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # #","\n",
                            "#  La integración se terminó completamente.","\n",
                            "#  ","\n",
                            "#  Integración realizada con exito","\n",
                            "#  ","\n",
                            "#  Folio integración Operación 22: ",v_folio_integracion_op22,"\n",
                            "#  ","\n",
                            "# # # # # # # # # # # # # # # # # # # # # # # # # #" 
         ELSE
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                 RETURNING r_bnd_fin_oper
            DISPLAY "\n ERROR   ", v_resultado, 
                    "\n ISAM    ", v_isam,     
                    "\n MENSAJE ", v_mensaje                        
            LET p_mensaje = " --- ERROR ---\n",
                            " El proceso de Integración no terminó correctamente.\n",
                            " Código de error : ", r_bnd_fin_oper,"\n ",
                            " FECHA           : ",TODAY,"\n",
                            " HORA            : ",CURRENT HOUR TO SECOND,"\n"
            DISPLAY v_mensaje
         
            IF(v_resultado <> 100)THEN
               DISPLAY "#  Error. No se integró ninguna solicitud"
            ELSE
               DISPLAY "#  ",fn_status_secciones_integradas(v_si_solicitudes_aceptadas_unificadas)
            END IF
         
            LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
         END IF
         
      WHEN (v_resultado = NOTFOUND)
         DISPLAY "NOT FOUND"
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
         DISPLAY "#  Error. No se integró ninguna solicitud"
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
      WHEN (v_resultado < 0)
         DISPLAY SQLERRMESSAGE
         DISPLAY "\n ERROR   ", v_resultado, 
                 "\n ISAM    ", v_isam,     
                 "\n MENSAJE ", v_mensaje    
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
              RETURNING r_bnd_fin_oper
         DISPLAY "Codigo Error SQL:",v_resultado
         DISPLAY "Error al procesar la integración"
         DISPLAY "No se puede continuar..."
         LET p_mensaje = " --- ERROR ---\n",
                         " El proceso de Integración no terminó correctamente.\n",
                         " Código de error : ", r_bnd_fin_oper,"\n ",
                         " FECHA           : ",TODAY,"\n",
                         " HORA            : ",CURRENT HOUR TO SECOND,"\n"
        
   END CASE
      
   --Se calcula el total de rechazados
   SELECT COUNT(folio)
   INTO   v_total_rch_op22
   FROM   uni_resp_op22_unificador
   WHERE  estado_familia = 2
   AND    folio = v_folio_integracion_op22
               
   --Si existe algúnr registro con rechazos
   IF v_total_rch_op22 > = 1 THEN        
      LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/UNIS06 ",
                                              p_usuario_cod, " ",
                                              g_pid  , " " ,
                                              g_proceso_cod , " " ,
                                              g_opera_cod ," ",                                              
                                              v_folio_integracion_op22, " ",
                                              "'",p_nombre_archivo, "' ",                                                          
                                              " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                                              "/nohup:",g_pid        USING "&&&&&",":",
                                              g_proceso_cod USING "&&&&&",":",
                                              g_opera_cod   USING "&&&&&" ,
                                              " 2>&1 &"
      DISPLAY v_s_comando
      RUN v_s_comando

      DISPLAY "Se ha generado un archivo de rechazos"           

   END IF 

   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - INTEGRACION"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN

#OBJETIVO: Obtener la descripción del error de validación
FUNCTION fn_mues_desc_valida(p_resultado_opera)
  DEFINE p_resultado_opera SMALLINT
        ,v_descripcion LIKE cat_bat_parametro_salida.descripcion
  

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   RETURN v_descripcion CLIPPED

END FUNCTION -- fn_mues_desc_valida

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
   
END FUNCTION --  