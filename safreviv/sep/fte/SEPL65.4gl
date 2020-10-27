--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 30 de Abril de 2015
--===============================================================
################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPL65                                                        #
#Objetivo     => EXTRACTOR SE CUENTAS CON MARCA DE SEPARACIÓN DE CUENTAS       #                          #
#Fecha inicio => 30 de Abril de 2015                                           #
################################################################################

SCHEMA safre_viv
DEFINE x char(1)
DEFINE g_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- Código del proceso
       g_opera_cod       LIKE cat_operacion.opera_cod, -- Código de operacion
       g_opera_cod_ant   LIKE cat_operacion.opera_cod, -- codigo de operacion anterior
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       v_folio           LIKE glo_folio.folio,
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING
       
DEFINE g_reg_modulo        RECORD
        ruta_exp              CHAR(40),
        ruta_rescate          CHAR(40),
        ruta_listados         CHAR(40)
END RECORD

DEFINE seg_modulo_bat      RECORD
         ruta_listados        CHAR(40)
END RECORD

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CONNECT TO "safre_viv"

      -- se inicia el log del programa

  -- se asigna proceso y operacion
   LET g_proceso_cod = 2236
   LET g_opera_cod   = 1     
   LET g_opera_cod_ant = 0   

      -- se obtiene el PID del proceso
   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod

   -- se obtienen las rutas de control del modulo
   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE s.modulo_cod = 'sep'

   SELECT b.ruta_listados
     INTO seg_modulo_bat.ruta_listados
     FROM seg_modulo b
    WHERE b.modulo_cod = 'bat'

   CALL STARTLOG(g_reg_modulo.ruta_listados CLIPPED ||"/"||p_usuario_cod CLIPPED||".SEPL65.log")

    CALL fn_acepta_operacion()
    
    DISCONNECT ALL
END MAIN

FUNCTION fn_acepta_operacion()
DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_resultado_opera SMALLINT

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   --PROMPT "ruta bin :",v_ruta_ejecutable FOR CHAR x
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL651"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      
      MENU
         ON ACTION aceptar
               #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod) RETURNING r_resultado_opera
            --PROMPT "valida operacion: ", r_resultado_opera FOR CHAR x
            IF(r_resultado_opera = 0)THEN
               CALL fn_genera_pid(g_proceso_cod,
                                        g_opera_cod,
                                        p_usuario_cod) RETURNING g_pid
           -- PROMPT "pid: ", g_pid FOR CHAR x                                    
               CALL fn_inicializa_proceso(g_pid,
                                                g_proceso_cod,
                                                g_opera_cod,
                                                v_folio,
                                                "SEPL65",
                                                "NA",
                                                p_usuario_cod) RETURNING r_resultado_opera
           -- PROMPT "inicializa proceso: ", r_resultado_opera  FOR CHAR x                                               
                     IF(r_resultado_opera <> 0)THEN
                        CALL fn_muestra_inc_operacion(r_resultado_opera)
                     END IF
               CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING v_folio
               CALL fn_actualiza_opera_ini(g_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               v_folio,
                               "SEPL65",
                               "NA",
                               p_usuario_cod)
               RETURNING r_resultado_opera
              -- prompt "actualiza opera ini : ",r_resultado_opera FOR CHAR x
               IF ( r_resultado_opera = 0 ) THEN
                  LET v_comando = " nohup fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/SEPX50.42r ",
                         p_usuario_cod CLIPPED, " ",
                         g_pid                , " ",
                         g_proceso_cod        , " ",
                         g_opera_cod          , " ",
                         v_folio              , " '",
                         "NA" CLIPPED    , "' ",
                         " 1>",seg_modulo_bat.ruta_listados clipped,
                         "/nohup:",g_pid USING "&&&&&",":",
                         g_proceso_cod   USING "&&&&&",":",
                         g_opera_cod     USING "&&&&&" ,
                         " 2>&1 &"
                -- PROMPT "no hup: ", v_comando FOR CHAR x
                 RUN v_comando
                 IF(STATUS)THEN
                        CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la extracción de marcas ","about")
                 ELSE
                        CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos","about")
                 END IF
            ELSE
                # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
                CALL fn_muestra_inc_operacion(r_resultado_opera)
            END IF
         END IF
         
         ON ACTION cancelar
            EXIT MENU
      END MENU
      
   CLOSE WINDOW vtna_valicacion
END FUNCTION
