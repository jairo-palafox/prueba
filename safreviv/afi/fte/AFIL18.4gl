####################################################################################################
# Modulo       => AFI                                                                              #
# Programa     => AFIL18                                                                           #      
# Objetivo     => Lanzador para actualización de datos de contacto en BUC y generación de archivos # 
# Autor        => Jose Eduardo Ventura Bonola                                                      #
# Fecha        => 14/ENERO/2015                                                                    #
####################################################################################################

DATABASE safre_viv

GLOBALS

   DEFINE        f_inicial       DATE       --fecha inicial
   DEFINE        f_final         DATE       --fecha final
   DEFINE        f_ejecucion     DATE
   DEFINE        g_proceso_cod   INTEGER
   DEFINE        g_opera_cod     INTEGER
   DEFINE        g_usuario       CHAR (20)
   DEFINE        r_b_valida      SMALLINT
   DEFINE        p_nom_ventana   STRING
   DEFINE        p_tpo_ejecucion SMALLINT
   DEFINE        v_cuenta_reg    INTEGER
   DEFINE        v_fecha         DATE
   DEFINE        v_val           INTEGER

END GLOBALS

MAIN
   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 1814 -- numero de proceso correspondiente
   LET g_opera_cod     = 1    -- numero de operacion correspondiente

   CLOSE WINDOW SCREEN

        -- se abre la ventana de consulta
   OPEN WINDOW AFIL18 WITH FORM "AFIL181"
   CALL ui.Interface.setText ( p_nom_ventana )

   SELECT COUNT (*)
     INTO v_cuenta_reg
     FROM afi_ctr_arh_buc
     

   IF v_cuenta_reg > 0 THEN
       SELECT MAX (f_proceso)
         INTO f_inicial
         FROM afi_ctr_arh_buc
         WHERE concepto = "K"

   ELSE
    {
      SELECT MIN (f_apertura) - 1 DAY
        INTO f_inicial
        FROM afi_derechohabiente
    }
      LET f_inicial = NULL
    END IF

   LET v_val = 1
   LET v_fecha = TODAY
   LET f_ejecucion = v_fecha
   LET f_final = v_fecha-(v_val) UNITS DAY

   MENU
      BEFORE MENU
      IF f_inicial = TODAY THEN 
         CALL fn_mensaje("MENSAJE","Este proceso ya se ha ejetutado el día de hoy","information")
         EXIT MENU
        ELSE

        --LET f_inicial = f_inicial + 1
        
      DISPLAY BY NAME f_ejecucion
      DISPLAY BY NAME f_inicial
      DISPLAY BY NAME f_final
      END IF

      ON ACTION aceptar
           ---- se invoca la funcion que valida la operacion
         CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida
           -- se verifica si la operacion en proceso es valida
         IF r_b_valida <> 0 THEN  
               -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)
            DISPLAY "ERROR en fn_valida_operacion"
         ELSE 

         --IF f_inicial = TODAY THEN 
         --CALL fn_mensaje("MENSAJE","Este proceso ya se ha ejetutado el día de hoy","information")

         --ELSE
               -- se llama funcion que genera el archivo
            CALL fn_genera_archivo()
            EXIT MENU
            END IF
         --END IF
   
      ON ACTION salir
         EXIT MENU 
   END MENU

   CLOSE WINDOW AFIL18

END MAIN

--***************************************
-- funcion que manda a generar archivos *
--***************************************
FUNCTION fn_genera_archivo()

   DEFINE v_pid                    DECIMAL(9,0)
   DEFINE v_s_comando              STRING
   DEFINE v_ruta_ejecutable        LIKE seg_modulo.ruta_bin
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio

--se obtienen rutas necesarias
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid
   CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,
                              "","AFIP17","",g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "AFIP17","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = "nohup fglrun ",
                      v_ruta_ejecutable CLIPPED,
                     "/AFIP17 ",
                     g_usuario    ," ",
                     v_pid        ," ",
                     g_proceso_cod," ",
                     g_opera_cod  ," '",
                     f_inicial    ,"'" ,
                     " '", f_final, "'",
                     " ' '  1>",
                     v_ruta_listados CLIPPED ,
                     "/nohup:",
                     v_pid         USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod   USING "&&&&&" ," 2>&1 &"

                     

   RUN v_s_comando

  DISPLAY "v_s_comando", v_s_comando

  LET v_s_comando = "Se ejecutó la generación de archivos"," ",
                    "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
  CALL fn_mensaje("Cuentas",v_s_comando,"information")

END FUNCTION