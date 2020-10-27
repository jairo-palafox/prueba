--=============================================================================
##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIL17                                                                          #      
#Objetivo     => Invoca la generación de archivo para incosistencias de archivo afiliatorio ims  # 
#Fecha inicio =>                                                                                 #
##################################################################################################

DATABASE safre_viv

DEFINE        f_inicial       DATE       --fecha inicial     
DEFINE        f_final         DATE       --fecha final   
DEFINE        g_proceso_cod   INTEGER    
DEFINE        g_opera_cod     INTEGER 
DEFINE        g_usuario       CHAR (20)
DEFINE        r_b_valida      SMALLINT
DEFINE        p_nom_ventana   STRING  
DEFINE        p_tpo_ejecucion SMALLINT 

MAIN 
    LET g_usuario       = ARG_VAL (1)
    LET p_tpo_ejecucion = ARG_val (2)
    LET p_nom_ventana   = ARG_VAL (3)
    LET g_proceso_cod   = 1813  -- numero de proceso correspondiente   
    LET g_opera_cod     = 1       -- numero de operacion correspondiente 

        -- se abre la ventana de consulta
    OPEN WINDOW AFIL17 WITH FORM "AFIL17"
    CALL ui.Interface.setText ( p_nom_ventana )

    --se piden fechas a consultar
    INPUT BY NAME   f_inicial,
                    f_final  ATTRIBUTES ( UNBUFFERED )

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
                EXIT INPUT
            END IF
    
        ON ACTION CANCEL
            EXIT INPUT 
    END INPUT 
  
    CLOSE WINDOW AFIL17

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
   WHERE modulo_cod = 'afi'

   --Obtiene ruta listados batch
    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'bat'

   CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario) RETURNING v_pid
   CALL fn_inicializa_proceso(v_pid,g_proceso_cod,g_opera_cod,
                              "","AFIS02","",g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "AFIS02","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                    "/AFIS02 ",g_usuario," ",v_pid," ",
                    g_proceso_cod," ",g_opera_cod," '",f_inicial,"'" ," '", f_final, "'",
                    " ' '  1>", v_ruta_listados CLIPPED ,
                    "/nohup:",v_pid USING "&&&&&",":",
                    g_proceso_cod USING "&&&&&",":",
                    g_opera_cod USING "&&&&&" ," 2>&1 &"

   RUN v_s_comando

    DISPLAY "v_s_comando", v_s_comando  

   LET v_s_comando = "Se ejecutó la generación de archivos"," ",
                   "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
   CALL fn_mensaje("Cuentas",v_s_comando,"information")

END FUNCTION