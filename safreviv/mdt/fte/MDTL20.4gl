--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL20                                                        #
#Objetivo     => Validación de archivo con origen recurrente acreditados       #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 09 Junio 2012                                                 #
#Modifico     => Alexandro Hollmann, EFP - Nota: Los ftes se ubicaban en bin   #
#Fecha modif  => 21 Junio 2012                                                 #
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
    WHERE modulo_cod = "mdt"

   CALL STARTLOG(v_ruta_lst CLIPPED ||"/"||p_usuario_cod CLIPPED||".MDTL20.log")
   CALL fn_valida_recurrente()
END MAIN

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTL20                                                   #
#Descripcion       => Validación de archivo con origen recurrente acreditados  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 09 Junio 2012                                            #
################################################################################
FUNCTION fn_valida_recurrente()
DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = 1303 # valida origen recurrente de ACR
   LET g_opera_cod_carga = 1    # Validacion origen recurrente de ACR
   LET v_tpo_ejecucion   = 2    

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/MDTL201"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      LET g_pid = 0

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga) RETURNING r_resultado_opera
      LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/MDTP20.42r ",
                                                            p_usuario_cod," ",
                                                            g_pid," ",
                                                            g_proceso_cod," ",
                                                            g_opera_cod_carga," ",
                                                            0," ",  # folio
                                                            "NA" # archivo
                                                                 
                                                                 
      IF(r_resultado_opera = 0)THEN
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "MDTL20",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga  # true - la carga inicializa el proceso
         
      ELSE
         # Muestra el mensaje del cual es la causa de que no se puede iniciar con la operacion
         --CALL fn_desplega_inc_operacion(r_resultado_opera)
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Cerrar"
              EXIT MENU
         END MENU
      END IF
   CLOSE WINDOW vtna_valicacion
END FUNCTION
