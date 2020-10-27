--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/09/2012
--==============================================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPL51                                                     #
#Objetivo        => Validación de carga de compensacion deudor                 #
#Fecha Inicio    => Septiembre 18, 2012                                        #
################################################################################
DATABASE safre_viv
GLOBALS "SEPG02.4gl"
DEFINE g_pid             LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, # Codigo del proceso
       g_opera_cod_carga LIKE cat_operacion.opera_cod, # Código de operación
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ventana         ui.Window,
       v_ruta_lst        LIKE seg_modulo.ruta_listados
       


MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   CALL fn_carga_compensacion_deudor()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPL51                                                   #
#Descripcion       => Carga de archivo de compensacion deudor                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 18 Septiembre 2012                                       #
################################################################################
FUNCTION fn_carga_compensacion_deudor()
DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = v_proc_compensacion_deudor        # compensacion deudor 
   LET g_opera_cod_carga = v_opera_carga_compensacion_deudor # validacion de compensacion deudor
   LET v_tpo_ejecucion   = 2

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/SEPL511"
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
         LET v_comando = " "
         {LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPX40.42r ",
                                                              p_usuario_cod," ",
                                                              g_pid," ",
                                                              g_proceso_cod," ",
                                                              g_opera_cod_carga," ",
                                                              0," ", # folio
                                                              "NA" # archivo}
         DISPLAY "--*******--------********----"
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "SEPL51",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga
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