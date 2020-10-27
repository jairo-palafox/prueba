#################################################################################
# Modulo       => AFI                                                           #
# Programa     => AFIL27                                                        #      
# Objetivo     => Lanzador para archivo BUC de actualización a datos maestros   # 
# Autor        => Jose Eduardo Ventura Bonola                                   #
# Fecha        => 06/ENERO/2016                                                 #
#################################################################################

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
   DEFINE        v_f_max         DATE
   DEFINE        v_bnd           SMALLINT
END GLOBALS

MAIN

   DEFINE bnd_ctr_ejecucion INTEGER

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_val (2)
   LET p_nom_ventana   = ARG_VAL (3)
   LET g_proceso_cod   = 1822 -- numero de proceso correspondiente
   LET g_opera_cod     = 1    -- numero de operacion correspondiente

   CLOSE WINDOW SCREEN

        -- se abre la ventana de consulta
   OPEN WINDOW AFIL27 WITH FORM "AFIL271"
   CALL ui.Interface.setText ( p_nom_ventana )

   LET bnd_ctr_ejecucion = 0

   SELECT MAX (f_proceso)
     INTO v_f_max
     FROM afi_arh_buc_adm

   --IF v_f_max IS NULL THEN
   --   SELECT MIN (f_apertura)
    --    INTO f_inicial
     --   FROM afi_derechohabiente

     -- LET f_final = (TODAY - 1)
     -- LET v_bnd   = 1
     -- LET bnd_ctr_ejecucion = 1
   --ELSE
      IF v_f_max = (TODAY) THEN
         CALL fn_mensaje("MENSAJE","El proceso ya fué ejecutado el día de hoy","information")
      ELSE
         LET f_inicial = (v_f_max)
         LET f_final   = (TODAY -1)
         LET v_bnd     = 1
      END IF
   --END IF
         
   IF v_bnd = 1 THEN

      DISPLAY "bandera ",v_bnd
      DISPLAY "f_inicial ", f_inicial
      DISPLAY "f_final ",f_final
      DISPLAY "fecha max" ,v_f_max
{
      SELECT COUNT (*)
        INTO v_cta_his
        FROM afi_his_derechohabiente
       WHERE ind_modifica NOT IN (3,4,7,8,14,15)
         AND f_modifica BETWEEN f_inicial AND f_final

      SELECT COUNT(*)
        INTO v_cta_afi
        FROM afi_derechohabiente
       WHERE f_apertura BETWEEN f_inicial AND f_final

       SELECT id_derechohabiente,
                 ind_modifica
            FROM afi_his_derechohabiente
           WHERE id_derechohabiente > 0
             AND f_modifica BETWEEN f_inicial AND f_final
       INTO TEMP afi_ctr_adm

           DELETE FROM afi_ctr_adm
            WHERE ind_modifica IN (3,4,7,8,14,15)

           SELECT id_derechohabiente,'0' AS ind_modifica
             FROM afi_derechohabiente
            WHERE id_derechohabiente > 0
              AND f_apertura BETWEEN f_inicial AND f_final
             INTO TEMP afi_crt_adm

           SELECT COUNT (*)
             INTO v_cta_afi
             FROM afi_ctr_adm

      LET v_cuenta_reg = (v_cta_afi + v_cta_his)

         DISPLAY "v_cuenta reg ",v_cuenta_reg

      IF v_cuenta_reg > 0 THEN
}
         LET f_ejecucion = TODAY

         DISPLAY BY NAME f_ejecucion
         DISPLAY BY NAME f_inicial
         DISPLAY BY NAME f_final

         MENU

            ON ACTION aceptar
           ---- se invoca la funcion que valida la operacion
               CALL fn_valida_operacion( 0,g_proceso_cod,g_opera_cod)   RETURNING r_b_valida
           -- se verifica si la operacion en proceso es valida
               IF r_b_valida <> 0 THEN  
               -- en caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_muestra_inc_operacion(r_b_valida)
                  DISPLAY "ERROR en fn_valida_operacion"
               ELSE

                  CALL fn_genera_archivo()
                  EXIT MENU
                  END IF
   
            ON ACTION salir
               EXIT MENU 
         END MENU
    {  ELSE
         LET f_ejecucion = TODAY

         DISPLAY BY NAME f_ejecucion
         DISPLAY BY NAME f_inicial
         DISPLAY BY NAME f_final
         
         CALL fn_mensaje("MENSAJE","No existen registros para generar archivo con la fecha indicada","information")

      END IF}
   END IF

   CLOSE WINDOW AFIL27

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
                              "","AFIP23","",g_usuario)  RETURNING r_b_valida

   CALL fn_actualiza_opera_ini(v_pid,g_proceso_cod,
                               g_opera_cod,"",
                               "AFIP23","",
                               g_usuario)  RETURNING r_b_valida

   LET v_s_comando = "nohup fglrun ",
                      v_ruta_ejecutable CLIPPED,
                     "/AFIP23 ",
                     g_usuario    ," ",
                     v_pid        ," ",
                     g_proceso_cod," ",
                     g_opera_cod  ," '",
                     f_inicial    ,"'" ," '",
                     f_final      ,"'" ,
                     " ' '  1>",
                     v_ruta_listados CLIPPED ,
                     "/nohup:",
                     v_pid         USING "&&&&&",":",
                     g_proceso_cod USING "&&&&&",":",
                     g_opera_cod   USING "&&&&&" ," 2>&1 &"

                     

   RUN v_s_comando

  DISPLAY "v_s_comando", v_s_comando

  LET v_s_comando = "Se ejecutó la generación de archivo BUC"," ",
                    "Verificar en el monitor de proceso la ejecución el PID ", v_pid USING "<<<<<<<<<"
  CALL fn_mensaje("Cuentas",v_s_comando,"information")

END FUNCTION