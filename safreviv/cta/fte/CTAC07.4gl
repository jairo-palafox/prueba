###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC07                                                  #
#Objetivo          => CONSULTA DE SALDOS HISTORICOS                           #
#Fecha Inicio      => 31-MARZO-2015                                           #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAC07.inc"

PRIVATE DEFINE v_fcorte                DATE
PRIVATE DEFINE v_datos                 datos_generales
PRIVATE DEFINE r_detmov                DYNAMIC ARRAY OF movimientos

PRIVATE DEFINE v_saldo_total           DECIMAL(22,2)

PRIVATE DEFINE ventana_his             ui.Window
PRIVATE DEFINE forma_his               ui.Form

FUNCTION fn_consulta_saldo_historico (p_id_derechohabiente)
   DEFINE p_id_derechohabiente DECIMAL(9,0)

   IF p_id_derechohabiente IS NOT NULL AND p_id_derechohabiente > 0 THEN
      LET v_datos.id_derechohabiente = p_id_derechohabiente
      OPEN WINDOW ctac071 WITH FORM "CTAC071"
         LET ventana_his = ui.Window.forName("ctac071")
         LET forma_his = ventana_his.getForm()
         CALL fn_consulta_datos()

         INPUT v_fcorte FROM f_corte ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
            BEFORE INPUT
               LET v_fcorte = TODAY
               CALL fn_oculta_secciones()
               DISPLAY v_datos.nss              TO nss
               DISPLAY v_datos.curp             TO curp
               DISPLAY v_datos.rfc              TO rfc
               DISPLAY v_datos.nombre_completo  TO nombre_completo
               DISPLAY v_fcorte                 TO f_corte
               
            ON ACTION ACCEPT
               IF v_fcorte IS NULL THEN
                  CALL fn_mensaje("Consulta Historico",
                               "Debe de ingresar la fecha de corte para la búsqueda.",
                               "about")
               ELSE
                  IF v_fcorte > TODAY THEN
                     CALL fn_mensaje("Consulta Historico", "La fecha de corte no puede ser mayor al dia de hoy.", "about")
                  ELSE
                     CALL fn_inicializa_consulta()
                     CALL fn_muestra_saldo()
                  END IF
               END IF
            ON ACTION CANCEL
               CALL fn_finaliza_consulta()
               LET INT_FLAG = TRUE
               EXIT INPUT
         END INPUT
      
      CLOSE WINDOW ctac071
      CALL fn_finaliza_consulta()
   ELSE
      CALL fn_mensaje("Atención","Para continuar con la consulta es necesario el identificador de la cuenta","stop")
   END IF
END FUNCTION

PRIVATE FUNCTION fn_muestra_saldo()
   DEFINE arr_arbol           DYNAMIC ARRAY OF arbol
   DEFINE arr_precios         DYNAMIC ARRAY OF precios

   DEFINE v_query             STRING
   DEFINE v_grupo             STRING

   DEFINE i                  SMALLINT
   DEFINE v_subcuenta        SMALLINT
   DEFINE v_siefore          SMALLINT
   DEFINE v_movimiento       SMALLINT
   DEFINE resp_visualiza     SMALLINT
   DEFINE v_pos              SMALLINT
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE v_referencia       DECIMAL(9,0)

   LET v_saldo_total = 0

   #Se ejecuta la funcion que calcula el saldo
   LET v_query = "EXECUTE PROCEDURE sp_consulta_saldo_his(?,?)"
   PREPARE exe_calcula_saldo FROM v_query
   EXECUTE exe_calcula_saldo USING v_datos.id_derechohabiente, v_fcorte

   LET v_query = " SELECT gvf.fondo, ",
                        " TRIM(cfl.razon_social), ",
                        " gvf.precio_fondo ",
                   " FROM glo_valor_fondo gvf, ",
                        " cat_fondo_local cfl ",
                  " WHERE gvf.f_valuacion = ? ",
                    " AND cfl.fondo       = gvf.fondo "  
   PREPARE prp_precio FROM v_query
   DECLARE cur_precio CURSOR FOR prp_precio

   LET i = 1
   FOREACH cur_precio USING v_fcorte INTO arr_precios[i].*
      LET i = i + 1
   END FOREACH
   CALL arr_precios.deleteElement(i)
   
   CLOSE cur_precio
   FREE cur_precio

   #Se llena el arreglo para el arbol
   LET v_query = "SELECT * FROM tmp_arbol_saldo_his ORDER BY id "
   PREPARE exe_consulta_arbol FROM v_query
   DECLARE cur_arbol CURSOR FOR exe_consulta_arbol
    
   LET i = 1
   FOREACH cur_arbol INTO arr_arbol[i].*
      --Para el caso del nivel 1 en el campo se subcuenta viene el grupo
      IF arr_arbol[i].nivel = 1 THEN
         LET v_grupo = arr_arbol[i].subcuenta

         --Despues de obtener el grupo se cambia el valor a cero
         LET arr_arbol[i].subcuenta = 0
      END IF
      
      IF arr_arbol[i].monto_pesos IS NULL THEN
         LET arr_arbol[i].monto_pesos = 0
      END IF

      IF arr_arbol[i].monto_acciones IS NULL THEN
         LET arr_arbol[i].monto_acciones = 0
      END IF

      IF arr_arbol[i].subcuenta = 0 THEN
         IF arr_arbol[i].siefore <> 0 AND v_grupo <> 47 THEN
            LET v_saldo_total   = v_saldo_total + arr_arbol[i].monto_pesos
         END IF
      END IF

      -- se cambian las leyendas de los agrupadores principales con grupo = 6
      IF ( arr_arbol[i].nivel = 1 AND v_grupo = 6) THEN
         IF ( arr_arbol[i].siefore = 0 ) THEN
            -- Avance de pago
            LET arr_arbol[i].subcuenta_desc = "AVANCE DE PAGO"
         END IF

         IF ( arr_arbol[i].siefore = 10 ) THEN
            -- Amortizacion
            LET arr_arbol[i].subcuenta_desc = "AMORTIZACION"
         END IF

         IF ( arr_arbol[i].siefore = 11 ) THEN
            -- Vivienda
            LET arr_arbol[i].subcuenta_desc = "VIVIENDA"
         END IF
      END IF
      LET i = i + 1
   END FOREACH
   CALL arr_arbol.deleteElement(i)
   LET i = i - 1
   CLOSE cur_arbol
   FREE cur_arbol
   
   #Se elimina la tabla temporal del arbol
   LET v_query = "DROP TABLE IF EXISTS tmp_arbol_saldo_his"
   PREPARE exe_drop_arbol_temp FROM v_query
   EXECUTE exe_drop_arbol_temp

   CALL forma_his.setElementHidden("group2",0)
   CALL forma_his.setElementHidden("group3",0)
   CALL forma_his.setElementHidden("group4",0)
   CALL forma_his.setElementHidden("group5",0)
   CALL ui.Interface.refresh()

   DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY arr_arbol TO tree.* 
         BEFORE ROW
            LET v_pos = ARR_CURR() 
            IF arr_arbol[v_pos].nivel > 1 THEN
               LET v_subcuenta = arr_arbol[v_pos].subcuenta
               LET v_siefore   = arr_arbol[v_pos].siefore
               LET v_movimiento= arr_arbol[v_pos].movimiento 

               CALL despliega_detalle_mov(v_subcuenta,v_siefore, v_movimiento)
                    RETURNING resp_visualiza

               IF NOT resp_visualiza THEN
                  CALL r_detmov.clear()
               ELSE
                  CALL r_detmov.deleteelement(r_detmov.getlength())
               END IF
            ELSE
               CALL r_detmov.clear()
            END IF

            IF v_saldo_total IS NULL THEN
               LET v_saldo_total = 0
            END IF

            DISPLAY v_saldo_total TO sdo_fin

         ON ACTION CLOSE
            CALL fn_oculta_secciones()
            EXIT DIALOG
       
      END DISPLAY

      DISPLAY ARRAY r_detmov TO detalle.*
         ON ACTION ACCEPT
            #Solo se mostrara el detalle para movimientos de pag o acl
            IF r_detmov[ARR_CURR()].modulo = 'pag' OR r_detmov[ARR_CURR()].modulo = 'acl' THEN
               LET v_folio = r_detmov[ARR_CURR()].folio
               LET v_referencia = r_detmov[ARR_CURR()].referencia
               CALL fn_consulta_pago(v_folio, v_referencia)
            END IF 
         ON ACTION CLOSE
            CALL fn_oculta_secciones()
            EXIT DIALOG
      END DISPLAY

      DISPLAY ARRAY arr_precios TO precios.*
         ON ACTION CLOSE
            CALL fn_oculta_secciones()
            EXIT DIALOG
      END DISPLAY
   
   END DIALOG
   
END FUNCTION

PRIVATE FUNCTION fn_consulta_datos()
   DEFINE v_query          STRING

   LET v_query = "SELECT FIRST 1 ",
                     "id_derechohabiente, ",
                     "nss, ",
                     "rfc, ",
                     "curp, ",
                     "TRIM(nombre_af)||' '|| ",
                     "TRIM(ap_paterno_af)||' '|| ",
                     "TRIM(ap_materno_af) ",
                  "FROM afi_derechohabiente ",
                  "WHERE id_derechohabiente = ?"
   PREPARE exe_datos_generaloes FROM v_query
   EXECUTE exe_datos_generaloes  USING v_datos.id_derechohabiente 
                                 INTO  v_datos.id_derechohabiente,
                                       v_datos.nss,
                                       v_datos.rfc,
                                       v_datos.curp,
                                       v_datos.nombre_completo
   
END FUNCTION

PRIVATE FUNCTION fn_oculta_secciones()
   CALL forma_his.setElementHidden("group2",1)
   CALL forma_his.setElementHidden("group3",1)
   CALL forma_his.setElementHidden("group4",1)
   CALL forma_his.setElementHidden("group5",1)
END FUNCTION

PRIVATE FUNCTION fn_inicializa_consulta()
   
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_his
      DROP TABLE tmp_arbol_saldo_his
   WHENEVER ERROR STOP

   INITIALIZE r_detmov TO NULL
   LET v_saldo_total = 0

   CREATE TEMP TABLE tmp_movimientos_his
    (id_derechohabiente DECIMAL(9,0),
     subcuenta          SMALLINT,
     fondo_inversion    SMALLINT,
     f_liquida          DATE, 
     tipo_movimiento    INTEGER,
     monto_pesos        DECIMAL(16,6),
     monto_acciones     DECIMAL(16,6),
     f_valor            DATE,
     folio_liquida      DECIMAL(10,0),
     origen             CHAR(20),
     id_referencia      DECIMAL(9,0),
     modulo_cod         CHAR(3))

   CREATE TEMP TABLE tmp_arbol_saldo_his
    (subcuenta_desc     CHAR(70),
     siefore            SMALLINT,
     monto_pesos        DECIMAL(22,2),
     monto_acciones     DECIMAL(22,2),
     subcuenta          SMALLINT,
     movimiento         SMALLINT,
     padre_id           CHAR(40),
     id                 CHAR(40),
     nivel              SMALLINT)

END FUNCTION

PRIVATE FUNCTION fn_finaliza_consulta()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_his
      DROP TABLE tmp_arbol_saldo_his
   WHENEVER ERROR STOP
END FUNCTION

PRIVATE FUNCTION despliega_detalle_mov(p_subcuenta,p_fondo, p_movimiento)
    DEFINE i             INTEGER

    DEFINE p_subcuenta   SMALLINT
    DEFINE p_fondo       SMALLINT
    DEFINE p_movimiento  SMALLINT

    DEFINE v_query       STRING

    LET v_query = " SELECT t.f_liquida, ",
                         " t.tipo_movimiento, ",
                         " c.movimiento_desc, ",
                         " t.fondo_inversion, ",
                         " t.monto_pesos, ",
                         " t.monto_acciones, ",
                         " t.f_valor, ",
                         " t.folio_liquida, ",
                         " t.origen, ",
                         " t.id_referencia, ",
                         " t.modulo_cod ",
                    " FROM tmp_movimientos_his t, ",
                         " cat_movimiento c ",
                   " WHERE t.subcuenta       = ", p_subcuenta,
                     " AND t.fondo_inversion = ", p_fondo
                     
   IF p_movimiento > 0 THEN
      LET v_query = v_query , " AND t.tipo_movimiento = ",p_movimiento 
       
   END IF

   LET v_query = v_query , " AND c.movimiento      = t.tipo_movimiento ",
                         " ORDER BY f_liquida DESC, folio_liquida DESC "
 
    CALL r_detmov.clear()

    PREPARE prp_mov FROM v_query
    DECLARE cur_mov CURSOR FOR prp_mov

    LET i = 1

    FOREACH cur_mov INTO r_detmov[i].*
        LET i = i + 1
    END FOREACH
    CLOSE cur_mov

    IF i = 1 THEN
        RETURN 0
    ELSE
        RETURN 1
    END IF
END FUNCTION

PRIVATE FUNCTION fn_consulta_pago(p_folio, p_referencia)
   DEFINE p_folio             DECIMAL(9,0)
   DEFINE p_referencia        DECIMAL(9,0)
   DEFINE comando             STRING
   DEFINE l_ruta_bin          CHAR(40)

   INITIALIZE comando TO NULL

   SELECT ct.ruta_bin
   INTO l_ruta_bin
   FROM seg_modulo ct
   WHERE modulo_cod = 'pag'

   LET comando = "cd ",l_ruta_bin CLIPPED,"/; fglrun PAGC19 '", "p_usuario",
                "' '1' 'Consulta detalle de pago' ",
                "'", p_folio, "' '", p_referencia, "'"

    CALL ui.interface.refresh()

    LET comando = comando CLIPPED
    RUN comando
END FUNCTION 