##
##CONSULTA GLOBALES DE SALDO POR SUBCUENTA Y SIEFORE
##
DATABASE safre_viv

DEFINE g_fecha_valuacion    DATE
DEFINE g_hoy                DATE
DEFINE w_pp                 ui.Window
DEFINE f_pp                 ui.Form

DEFINE cont                 SMALLINT

DEFINE arr_saldos           DYNAMIC ARRAY OF RECORD
    subcuenta                   SMALLINT,
    desc_subcuenta              CHAR(30),
    siefore                     SMALLINT,
    desc_siefore                CHAR(10),
    acciones                    DECIMAL(20,2),
    pesos                       DECIMAL(20,2)
END RECORD



DEFINE g_usuario            CHAR(20)
DEFINE p_tipo_proc          CHAR(1)
DEFINE p_nombre_menu        CHAR(50)

PRIVATE DEFINE v_ind_consistencia   SMALLINT 


MAIN
    DEFINE v_bandera         SMALLINT

    LET g_usuario            = ARG_VAL(1)
    LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
    LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa

    CLOSE WINDOW SCREEN

    CALL ui.Interface.setText(p_nombre_menu)
   
    OPEN WINDOW ctac051 WITH FORM "CTAC051"

    LET w_pp = ui.Window.forName("ctac051")
    LET f_pp = w_pp.getform()

    CALL f_pp.setElementHidden("group2",1)

    CALL fn_captura_parametros() RETURNING v_bandera 
    
    CLOSE WINDOW ctac051

END MAIN

FUNCTION fn_captura_parametros()
   LET g_hoy = TODAY
   LET g_fecha_valuacion = g_hoy - 1

   INPUT g_fecha_valuacion FROM fecha_valuacion ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS) 
      {AFTER FIELD fecha_valuacion
         IF g_fecha_valuacion > TODAY THEN
            CALL fn_mensaje("Saldo Globales",
                            "La fecha ingresada no puede ser mayor al día de hoy.",
                            "about")
            NEXT FIELD fecha_valuacion
         END IF 
       } 
      ON ACTION accept
         IF g_fecha_valuacion IS NULL THEN
            CALL fn_mensaje("Saldo Globales",
                            "La fecha valuación no puede ser nula.",
                            "about")
         ELSE
            {IF g_fecha_valuacion > TODAY THEN
               CALL fn_mensaje("Saldo Globales",
                               "La fecha ingresada no puede ser mayor al día de hoy.",
                               "about")
               NEXT FIELD fecha_valuacion
            ELSE
               CALL fn_obtiene_saldo()
               CALL f_pp.setElementHidden("group2",1)
               LET int_flag = FALSE
            END IF}  
            CALL fn_obtiene_saldo()
            CALL f_pp.setElementHidden("group2",1)
            LET int_flag = FALSE
         END IF
            
      ON ACTION cancel
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT
   
   RETURN int_flag
END FUNCTION

FUNCTION fn_obtiene_saldo()


   LET cont = 1
   CALL arr_saldos.clear()

   DECLARE cur_saldo_sie CURSOR FOR
   SELECT
      sdo.subcuenta,
      sub_cta.subcuenta_desc,
      sdo.fondo_inversion,
      cat_fondo.razon_social,
      sdo.monto_acciones,
      sdo.monto_pesos,
      sdo.ind_consistencia
   FROM safre_sdo@vivws_tcp:cta_saldo_diario_global sdo
   INNER JOIN safre_viv:cat_subcuenta sub_cta ON sub_cta.subcuenta = sdo.subcuenta
   INNER JOIN safre_viv:cat_fondo_local cat_fondo ON cat_fondo.fondo = sdo.fondo_inversion
   WHERE sdo.f_saldo = g_fecha_valuacion
   ORDER BY sdo.subcuenta

   FOREACH cur_saldo_sie INTO arr_saldos[cont].*, v_ind_consistencia
      IF arr_saldos[cont].subcuenta = 48 THEN
         IF v_ind_consistencia = 0 THEN
            LET arr_saldos[cont].desc_subcuenta = arr_saldos[cont].desc_subcuenta CLIPPED , " NO CONSISTENTE" 
         ELSE
            LET arr_saldos[cont].desc_subcuenta = arr_saldos[cont].desc_subcuenta CLIPPED , " CONSISTENTE" 
         END IF 
      END IF

       LET cont = cont + 1 
   END FOREACH
   CLOSE cur_saldo_sie
   FREE cur_saldo_sie

   IF cont > 1 THEN
      DISPLAY ARRAY arr_saldos TO scr_saldos.* ATTRIBUTES (ACCEPT=FALSE)
         BEFORE DISPLAY 
            CALL f_pp.setElementHidden("group2",0)
             
         ON ACTION reporte
            CALL fn_lanza_reporte()

         ON ACTION cancel
            EXIT DISPLAY
      END DISPLAY
   ELSE
      CALL fn_mensaje("Saldo Globales",
                      "No se han generado los saldos para la fecha indicada.",
                      "about")
   END IF
   
END FUNCTION


FUNCTION fn_lanza_reporte()

 DEFINE preview   SMALLINT,
        i         INTEGER,
        vhandler   om.SaxDocumentHandler

   LET preview = TRUE
   INITIALIZE vhandler TO NULL

   LET vhandler = fn_configuracion( "CTAC051.4rp", "PDF", preview )
    
   START REPORT rep_saldo TO XML HANDLER vhandler
      FOR i = 1 TO cont - 1
         OUTPUT TO REPORT rep_saldo (arr_saldos[i].*)
      END FOR
   FINISH REPORT rep_saldo
    
END FUNCTION
  
---------------------------------------------------------------------------
FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
---------------------------------------------------------------------------

  DEFINE 
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

  -- CARGAR EL ARCHIVO 4rp
  IF NOT fgl_report_loadCurrentSettings(v_reporte) THEN
     RETURN NULL
  END IF

  CALL fgl_report_selectDevice(v_formato)
  CALL fgl_report_selectPreview(v_preview)
  
  RETURN fgl_report_commitCurrentSettings()

END FUNCTION


REPORT rep_saldo (v_saldo) 
   DEFINE v_saldo   RECORD
    subcuenta                   SMALLINT,
    desc_subcuenta              CHAR(20),
    siefore                     SMALLINT,
    desc_siefore                CHAR(10),
    acciones                    DECIMAL(20,2),
    pesos                       DECIMAL(20,2)
   END RECORD
   DEFINE v_fecha               DATE
   
   FORMAT

  FIRST PAGE HEADER
   LET v_fecha = TODAY
   PRINTX g_fecha_valuacion USING "dd-mm-yyyy"
   PRINTX g_usuario
   PRINTX v_fecha USING "dd-mm-yyyy"
  
  ON EVERY ROW
   PRINTX v_saldo.*
  
END REPORT
