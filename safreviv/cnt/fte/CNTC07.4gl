################################################################################
# VERSION: 1.0.0                                                               #
# FECHA ULTIMA MODIFICACION: 29/06/2012                                        #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#MODULO            => CNT                                                      #
#PROGRAMA          => CNTC07                                                   #
#OBJETIVO          => Programa de consulta del deudor separación de cuentas    #
#FECHA INICIO      => 29/06/2012                                               #
################################################################################
DATABASE
   safre_viv
GLOBALS "CNTG01.4gl"

MAIN
 DEFINE 
   f_ventana     ui.Window,   -- Define las propìedades de la Ventana
   f_forma       ui.Form     -- Define las propiedades de la forma

DEFINE f_fecha_ini  DATE 
DEFINE f_fecha_fin  DATE
DEFINE v_dif_subcuenta   DECIMAL(22,2),
       v_dif_deudor      DECIMAL(22,2),
       v_dif_diferencia  DECIMAL(22,2),
       v_due1_subcuenta  DECIMAL(22,2),
       v_due1_deudor     DECIMAL(22,2),
       v_deu1_diferencia DECIMAL(22,2),
       v_due2_subcuenta  DECIMAL(22,2),
       v_due2_deudor     DECIMAL(22,2),
       v_deu2_diferencia DECIMAL(22,2),
       v_sum_subcuenta   DECIMAL(22,2),
       v_sum_deudor      DECIMAL(22,2),
       v_sum_diferencia  DECIMAL(22,2)

   LET v_dif_subcuenta   = 11000
   LET v_dif_deudor      = 11000
   LET v_dif_diferencia  = 0.00
   LET v_due1_subcuenta  = 2500
   LET v_due1_deudor     = 11000
   LET v_deu1_diferencia = -8500
   LET v_due2_subcuenta  = 24000
   LET v_due2_deudor     = 11000
   LET v_deu2_diferencia = 13000
   LET v_sum_subcuenta   = 37500
   LET v_sum_deudor      = 33000
   LET v_sum_diferencia  = 4500

   -- se asignan los parametros que vienen del fglrun
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_prog     = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( g_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_prog)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW vtn_cntc07 WITH FORM "CNTC071"
      DIALOG ATTRIBUTES(UNBUFFERED)
         INPUT v_cmb_sub_cta,f_fecha_ini,f_fecha_fin
          FROM v_cmb_sub_cta,f_fecha_ini,f_fecha_fin

         BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()

            --Oculta detalle de información del deudor
            CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
            CALL fn_cmb_subcta(" IS  NOT NULL ")

         END INPUT
         
         ON ACTION consultar
            --Muestra detalle de información del deudor
            CALL f_forma.setElementHidden("grp_valida_proceso",TRUE)
            
            DISPLAY v_dif_subcuenta   TO dif_subcuenta
            DISPLAY v_dif_deudor      TO dif_deudor
            DISPLAY v_dif_diferencia  TO dif_diferencia
            DISPLAY v_due1_subcuenta  TO due1_subcuenta
            DISPLAY v_due1_deudor     TO due1_deudor
            DISPLAY v_deu1_diferencia TO deu1_diferencia
            DISPLAY v_due2_subcuenta  TO due2_subcuenta
            DISPLAY v_due2_deudor     TO due2_deudor
            DISPLAY v_deu2_diferencia TO deu2_diferencia
            DISPLAY v_sum_subcuenta   TO sum_subcuenta
            DISPLAY v_sum_deudor      TO sum_deudor
            DISPLAY v_sum_diferencia  TO sum_diferencia

         ON ACTION cancelar
            EXIT DIALOG

         ON ACTION reporte
            CALL fn_mensaje("Atención","Se genera reporte","about")

      END DIALOG 
   CLOSE WINDOW vtn_cntc07
END MAIN