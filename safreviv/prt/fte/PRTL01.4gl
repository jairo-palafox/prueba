--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19 Marzo 2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTL01                                                        #
#Objetivo     => Validación de archivo de transferencia de saldos receptora    #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 19 Marzo 2015                                                 #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       p_cad_ventana     STRING,
       v_ventana         ui.Window,
       v_tipo_ejecucion  SMALLINT,
       v_pid             SMALLINT,
       v_inicia_proceso  BOOLEAN,
       v_comando         STRING

MAIN
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "prt"

   LET v_tipo_ejecucion = 2 # Batch
   LET v_pid            = 0
   LET v_inicia_proceso = TRUE
   

   CALL fn_valida_traspaso_receptora()
END MAIN

# Descripción: Carga archivo de traspaso de saldos receptora
FUNCTION fn_valida_traspaso_receptora()
DEFINE r_resultado_opera SMALLINT,
       r_bnd_carga       BOOLEAN
       

   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/PRTL011"
      #Se asigna el titulo de la ventana
      IF( p_cad_ventana IS NOT NULL )THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(v_pid,
                               C_PROCESO_COD_TRANS_SDO_RECEPTORA,
                               C_OPERA_COD_CARGA) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/PRTX01.42r ",p_usuario_cod," ",
                                                                            v_pid," ",
                                                                            C_PROCESO_COD_TRANS_SDO_RECEPTORA," ",
                                                                            C_OPERA_COD_CARGA," ",
                                                                            0," ", # folio
                                                                            "NA" # archivo
         CALL fn_carga_archivo(v_pid, 
                               C_PROCESO_COD_TRANS_SDO_RECEPTORA, 
                               C_OPERA_COD_CARGA, 
                               v_tipo_ejecucion,
                               "PRTL01",
                               v_comando, 
                               p_usuario_cod, 
                               v_inicia_proceso) # TRUE --> Carga inicia proceso, FALSE --> solo realiza carga de archivo
                               RETURNING r_bnd_carga  
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