--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/08/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPER18                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de generacion de archivo      #
#                para la devolucion por pagos indevidos o en exceso solo IMSS.          #
#Fecha inicio => Agosto 06, 2012                                                        #
#########################################################################################

--Lanzador DPEL27

IMPORT os
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       --,p_i_folio        LIKE dis_preliquida.folio_liquida
       ,p_i_folio        VARCHAR(100)
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera                      SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado
       ,v_c_ruta_env_acr               LIKE seg_modulo.ruta_envio -- ruta donde se coloca el archivo
       ,v_v_nom_archivo                CHAR(40) -- nombre del archivo de salida
       ,v_v_ruta_nomarch               VARCHAR(100) -- ruta y nombre del archivo de salida
       ,v_res                          SMALLINT
       ,v_QryTxt                       CHAR(1000) 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
      
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER18.log")

   LET g_proceso_cod = g_proceso_cod_dpe_procesar_gen -- Generar Archivo POCESAR
   LET g_opera_cod   = 1 -- generacion de archivo PROCESAR
   LET INT_FLAG = FALSE

   -- Reversa operación
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera  
         
   IF(r_bandera = 0)THEN      
      --UPDATE glo_folio
      --SET status = 2
      --WHERE folio = p_i_folio
      --WHERE folio IN ( p_i_folio )
      --AND status = 3
--
      --Actualiza el Diagnóstico        
      --UPDATE dpe_sol_trab_parcial
         --SET diagnostico = 8
       --WHERE folio = p_i_folio
      --WHERE folio IN ( p_i_folio )
         --AND diagnostico = 100
--
      --UPDATE dpe_sol_trab_parcial
         --SET diagnostico = 9
--       WHERE folio = p_i_folio
      --WHERE folio IN ( p_i_folio )
         --AND diagnostico = 11

         LET p_nombre_archivo = p_nombre_archivo CLIPPED 

      LET v_QryTxt = "\n UPDATE glo_folio",
                     "\n SET status = 2", 
                     "\n WHERE folio IN ", "(", p_i_folio ,")",
                     "\n AND status = 3 ; \n",
                     --Actualiza el Diagnóstico        
                     "\n UPDATE dpe_sol_trab_parcial ",
                     "\n    SET diagnostico = 8      ",
                     "\n WHERE folio IN ", "(", p_i_folio ,")",
                     "\n    AND diagnostico = 100 ;  \n",
                     --Actualiza parciales
                     "\n UPDATE dpe_sol_trab_parcial  ",
                     "\n    SET diagnostico = 9       ",
                     "\n WHERE folio IN ", "(", p_i_folio ,")",
                     "\n    AND diagnostico = 11;  \n",
                     --Actualiza archivo
                     "\n UPDATE dpe_ctr_archivo",
                     "\n    SET estado = 3       ",
                     "\n WHERE nombre_archivo = ", "'", p_nombre_archivo ,"'",
                     "\n   AND folios = ", "'", p_i_folio ,"'",
                     "\n   AND estado = 0;     "
--DISPLAY v_QryTxt

      PREPARE prp_reverso_diag_procesar FROM v_QryTxt
      EXECUTE prp_reverso_diag_procesar 

         
      DISPLAY "Operación lista para volver a generarse."       
         -- se obtienen la ruta envio del modulo
      SELECT ruta_envio 
      INTO v_c_ruta_env_acr
      FROM seg_modulo
      WHERE modulo_cod = 'dpe'
     
       -- se crea el nombre del archivo y posteriormente se concatena con la ruta 
      LET v_v_nom_archivo = "/"|| p_nombre_archivo
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
      CALL os.Path.delete(v_c_ruta_env_acr CLIPPED || v_v_nom_archivo CLIPPED) 
      RETURNING v_res

   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - GENERACION DE ARCHIVO PROCESAR"
   
   LET p_mensaje = "Finalización de proceso - GENERACION DE ARCHIVO PROCESAR","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
END MAIN