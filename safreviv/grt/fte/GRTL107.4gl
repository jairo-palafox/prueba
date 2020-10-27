##################################################################################
#Modulo             => GRT                                                       #
#Programa           => GRTL107                                                   #
#Objetivo           => Programa Lanzador de archivo de salida para 43BIS         #
#Autor              => José Eduardo Ventura                                      #
#Fecha inicio       => 08 de Febrero del 2016                                    #
##################################################################################
DATABASE safre_viv

GLOBALS
   DEFINE v_f_generacion      DATE
   DEFINE v_f_publicacion     DATE
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
END GLOBALS

MAIN 
   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 3909   -- numero de proceso correspondiente   
   LET g_opera_cod     = 1       -- numero de operacion correspondiente 

       -- se abre la ventana de consulta
   OPEN WINDOW GRTL107 WITH FORM "GRTL107"
   CALL ui.Interface.setText ( p_nom_ventana )

   LET v_arch = "Art43bis",TODAY USING "DDMMYYYY",".Sal"
   LET bnd_arch = 0

   LET v_qry = 
   "SELECT nom_archivo
     FROM grt_ctr_archivo
    WHERE f_proceso = TODAY"

   PREPARE prp_qry FROM v_qry
   DECLARE cur_qry CURSOR FOR prp_qry

   LET a = 1
   FOREACH cur_qry INTO v_valida_arch
      IF v_valida_arch = v_arch THEN
         CALL fn_mensaje("Information","El archivo ya fue generado el día de hoy","information")
         LET bnd_arch = 1
         EXIT FOREACH
      END IF
      LET a = a+1
   END FOREACH

   IF bnd_arch = 0 THEN

      LET v_f_generacion    = TODAY
      
      LET v_comando = "EXECUTE FUNCTION fn_cal_habil_siguiente(?)"
      
      PREPARE prp_fn_habil FROM v_comando
      
      EXECUTE prp_fn_habil USING v_f_generacion INTO v_f_publicacion
      
      --se piden fechas a consultar
      DISPLAY BY NAME   v_f_generacion,
                        v_f_publicacion  --ATTRIBUTES ( UNBUFFERED )

      MENU
         ON ACTION ACCEPT
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
               EXIT MENU
            END IF
       
         ON ACTION CANCEL
            EXIT MENU
      END MENU
   CLOSE WINDOW GRTL107
   END IF
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
   WHERE modulo_cod = 'grt'

   --Obtiene ruta listados batch
    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid

   CALL fn_inicializa_proceso(v_pid,g_proceso_cod,
                              g_opera_cod,
                              "","GRTS101","",
                              g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "GRTS101","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = "nohup fglrun ",
                     v_ruta_ejecutable CLIPPED,
                     "/GRTS101 ",
                     g_usuario," ",
                     v_pid," ",
                     g_proceso_cod," ",
                     g_opera_cod," '",
                     v_f_generacion,"'" ,
                     " '", v_f_publicacion, "'",
                     " ' '  1>", v_ruta_listados CLIPPED ,
                     "/nohup:",v_pid USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod USING "&&&&&" ," 2>&1 &"

   RUN v_s_comando

    DISPLAY "v_s_comando", v_s_comando  

   LET v_s_comando = "Se ejecutó la generación de archivo de salida 43BIS"," ",
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