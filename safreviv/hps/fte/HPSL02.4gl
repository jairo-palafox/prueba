--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19 Junio 2015
--===============================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL02                                                        #
#Objetivo     => Carga de archivos para actualización de catálogo de mandatos  #                                     #
#Fecha inicio => 19 Junio 2015                                                  #
################################################################################

DATABASE safre_viv

DEFINE g_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- Código del proceso
       g_opera_cod_carga LIKE cat_operacion.opera_cod, -- Código de operacion
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING

MAIN
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   CALL STARTLOG(v_ruta_lst CLIPPED ||"/"||p_usuario_cod CLIPPED||".HPSL02.log")
   CALL fn_carga_archivo_mandatos()
   
END MAIN

FUNCTION fn_carga_archivo_mandatos()

DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = 3102 # Actualizacion catálogo mandatos
   LET g_opera_cod_carga = 1    #
   LET v_tpo_ejecucion   = 2    

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'
   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL021"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         LET g_pid = 0
         
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/HPSX02.42r ",
                                                             p_usuario_cod," ",
                                                             g_pid," ",
                                                             g_proceso_cod," ",
                                                             g_opera_cod_carga," ",
                                                             0," ", # folio
                                                             "NA" # archivo
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "HPSL02",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga  # true - la carga inicializa el proceso
         # Si se realizó la carga se continua con el proceso
         {IF NOT(r_bnd_carga)THEN
            CALL fn_mensaje("OP 29","Se ha cancelado la carga de información","bn_about")
         END IF}
      ELSE
         # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Salir"
              EXIT MENU
         END MENU
      END IF
   CLOSE WINDOW vtna_valicacion



END FUNCTION
       