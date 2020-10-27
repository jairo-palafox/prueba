#####################################################################################
#Modulo             => OCG                                                          #
#Programa           => OCGL09                                                       #
#Objetivo           => Programa Lanzador para consulta de formalizados para 43BIS   #
#Autor              => José Eduardo Ventura                                         #
#Fecha inicio       => 18 de Febrero del 2016                                       #
#####################################################################################
DATABASE safre_viv

GLOBALS
   DEFINE f_inicial           CHAR(10)
   DEFINE f_final             CHAR(10)
   DEFINE g_proceso_cod       INTEGER
   DEFINE g_opera_cod         INTEGER
   DEFINE g_usuario           CHAR (20)
   DEFINE r_b_valida          SMALLINT
   DEFINE p_nom_ventana       STRING
   DEFINE p_tpo_ejecucion     SMALLINT
   DEFINE v_comando           STRING
   DEFINE v_estado            SMALLINT
   DEFINE v_arch              STRING
   DEFINE v_valida_arch       CHAR(40)
   DEFINE bnd_arch            SMALLINT
   DEFINE v_qry               STRING
   DEFINE a                   INTEGER
   DEFINE v_fecha             CHAR(10)
END GLOBALS

MAIN 
   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 3911   -- numero de proceso correspondiente   
   LET g_opera_cod     = 1       -- numero de operacion correspondiente 

       -- se abre la ventana de consulta
   OPEN WINDOW OCGL091 WITH FORM "OCGL091"
   CALL ui.Interface.setText ( p_nom_ventana )

      --se piden fechas a consultar
      INPUT BY NAME   f_inicial, f_final ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
         IF (f_inicial IS NULL) AND
      (f_final IS NULL )THEN

      LET v_fecha = TODAY USING "dd/mm/yyyy"
      LET f_inicial = "01","/",v_fecha[4,5],"/",v_fecha[7,10]

      LET f_final = TODAY USING "dd/mm/yyyy"

      DISPLAY "condición : fechas nulas"
   END IF

   IF (f_inicial IS NULL) AND
      (f_final IS NOT NULL )THEN

      LET v_fecha = TODAY USING "dd/mm/yyyy"
      LET f_inicial = "01","/",v_fecha[4,5],"/",v_fecha[7,10]

      LET f_final = f_final

      DISPLAY "condición : fecha inicial nula"
   END IF

   IF (f_inicial IS NOT NULL) AND
      (f_final IS NULL )THEN

      LET f_inicial = f_inicial

      LET v_fecha = TODAY USING "dd/mm/yyyy"
      LET f_final = v_fecha

      DISPLAY "condición : fecha final nula"
   END IF

   DISPLAY "fechas : ",f_inicial, "  --",f_final

      ---- se invoca la funcion que valida la operacion
            CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida
            -- se verifica si la operacion en proceso es valida
            IF r_b_valida <> 0 THEN  
             -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               DISPLAY "ERROR en fn_valida_operacion"
            ELSE 
            -- se llama funcion que genera el archivo
               CALL fn_genera_archivo()
               EXIT INPUT
            END IF
       
         ON ACTION CANCEL
      EXIT INPUT
      END INPUT
   CLOSE WINDOW OCGL091

END MAIN 

-- funcion que manda a generar el archivo
FUNCTION fn_genera_archivo()

    DEFINE v_pid              DECIMAL(9,0)
    DEFINE v_s_comando        STRING
    DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin
    DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados

   SELECT ruta_bin
   INTO v_ruta_ejecutable
   FROM seg_modulo
   WHERE modulo_cod = 'ocg'

   --Obtiene ruta listados batch
    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid

   CALL fn_inicializa_proceso(v_pid,g_proceso_cod,
                              g_opera_cod,
                              "","OCGS03","",
                              g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "OCGS03","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = " nohup fglrun ",
                     v_ruta_ejecutable CLIPPED,
                     "/OCGS03 ",
                     g_usuario," ",
                     v_pid," ",
                     g_proceso_cod," ",
                     g_opera_cod," '",
                     f_inicial,"'" ,
                     " '",f_final,"' ",
                     " ' '  1>", v_ruta_listados CLIPPED ,
                     "/nohup:",v_pid USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod USING "&&&&&" ," 2>&1 &"

   RUN v_s_comando

    DISPLAY "v_s_comando", v_s_comando  

   LET v_s_comando = "Se ejecutó la generación de archivo"," ",
                   "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
   CALL fn_mensaje("Cuentas",v_s_comando,"information")

      LET v_estado = 0

   IF v_estado = 0 THEN

   CALL fn_actualiza_opera_fin(v_pid,
                               g_proceso_cod,
                               g_opera_cod)
                     RETURNING v_estado

      ELSE
 --Si ocurrio un error se actualiza el estatus como erroneo
         CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF

END FUNCTION