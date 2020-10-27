###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC10                                                  #
#Objetivo          => CONSULTA DE DECRETO  EN HISTORICO PENDIENTES DE LIQUIDAR                                #
#Fecha Inicio      => 25-AGOSTO-2015                                           #
###############################################################################
DATABASE safre_viv

GLOBALS "CTAC10.inc"
--******************************************************************************
DEFINE r_valida_nss                 SMALLINT
DEFINE v_busca_nss                  INTEGER
DEFINE v_msj_alerta                 STRING
   
--******************************************************************************
#Variables para capturar los parametros que recibe la consulta
PRIVATE DEFINE p_usuario            CHAR(10)
PRIVATE DEFINE p_tipo_proc          CHAR(1)
PRIVATE DEFINE p_nombre_menu        CHAR(50)
PRIVATE DEFINE p_id_decreto         DECIMAL(9,0)

PRIVATE DEFINE v_saldo_total        DECIMAL(22,2)
PRIVATE DEFINE v_acciones_total     DECIMAL(22,2)
PRIVATE DEFINE v_precio             DECIMAL(22,6)

#Variables para el filtro de busqueda
PRIVATE DEFINE v_datos                       datos_generales

#Lista para los movimientos de decreto
PRIVATE DEFINE v_lista_decreto DYNAMIC ARRAY OF movimientos_decreto

#Variables para el manejo de la pantalla
PRIVATE DEFINE ventana                       ui.Window
PRIVATE DEFINE forma                         ui.Form

MAIN
   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_decreto         = ARG_VAL(4)

   IF p_id_decreto IS NOT NULL THEN
      LET v_datos.id_decreto = p_id_decreto
END IF

   --CALL STARTLOG(p_usuario CLIPPED ||".CTAC09.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Pendientes de Ejecutar"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW vtn_ctac101 WITH FORM "CTAC101"

   CALL fn_alertamiento()
      RETURNING v_msj_alerta
   
      LET ventana = ui.Window.forName("vtn_ctac101")
      LET forma = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos.id_decreto IS NULL THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo 
         END IF
         
         IF v_datos.id_decreto IS NOT NULL THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF

      END WHILE

   CLOSE WINDOW vtn_ctac101
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   DEFINE v_consulta_cliente              STRING
   DEFINE v_condicion                     STRING

   DEFINE i                               SMALLINT
   DEFINE v_lista_clientes         DYNAMIC ARRAY OF datos_generales

   #Se inicializan las valiables del filtro
   INITIALIZE v_datos               TO NULL
   INITIALIZE v_lista_decreto       TO NULL
   INITIALIZE v_saldo_total         TO NULL

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("group3",1)
   CALL forma.setElementHidden("group2",1)
   CALL forma.setElementHidden("group5",1)

   CONSTRUCT v_condicion ON consec_cuenta, nss, rfc, curp, nombre_trabajador
                         FROM consec_cuenta, nss, rfc, curp, nombre
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_datos.consec_cuenta  = GET_FLDBUF(consec_cuenta)
         LET v_datos.nss            = GET_FLDBUF(nss)
         LET v_datos.rfc            = GET_FLDBUF(rfc)
         LET v_datos.curp           = GET_FLDBUF(curp)
         LET v_datos.nombre_trabajador= GET_FLDBUF(nombre)
         LET INT_FLAG = FALSE
--******************************************************************************

   IF v_datos.nss IS NOT NULL THEN
      LET v_consulta_cliente=" SELECT FIRST 1 nss
      FROM afi_decreto
      WHERE nss=?"

      PREPARE exe_consulta FROM v_consulta_cliente
      EXECUTE exe_consulta INTO v_datos.nss USING v_datos.nss

      IF v_busca_nss IS NULL THEN
         CALL fn_mensaje("Consulta Decreto",
                         "No existen registros con el criterio de búsqueda. \n",
                         "about")
         LET INT_FLAG = 1
         EXIT CONSTRUCT
      ELSE
         CALL fn_valida_nss()
      END IF
   END IF

   IF v_datos.rfc IS NOT NULL THEN

   LET v_consulta_cliente=" SELECT FIRST 1 nss
      FROM afi_decreto
      WHERE rfc=?"

      PREPARE exe_consulta_rfc FROM v_consulta_cliente
      EXECUTE exe_consulta_rfc INTO v_datos.nss USING v_datos.rfc
      
      CALL fn_valida_nss()
   END IF

   IF v_datos.consec_cuenta IS NOT NULL THEN


   LET v_consulta_cliente=" SELECT FIRST 1 nss
      FROM afi_decreto
      WHERE consec_cuenta=?"

      PREPARE exe_consulta_consec_cuuenta FROM v_consulta_cliente
      EXECUTE exe_consulta_consec_cuuenta INTO v_datos.nss USING v_datos.consec_cuenta

      
  --    SELECT nss
  --    INTO v_datos.nss
  --    FROM afi_decreto
   --   WHERE rfc = v_datos.rfc

  --      CALL fn_mensaje("Consulta Decreto",
   --                      "AQUI"||v_datos.nss,
   --                      "about")
      
      CALL fn_valida_nss()
   END IF

   IF v_datos.curp IS NOT NULL THEN
      SELECT nss
      INTO v_datos.nss
      FROM afi_decreto
      WHERE curp = v_datos.curp

      CALL fn_valida_nss()
   END IF

   IF r_valida_nss <> 0 THEN
      CALL fn_mensaje("Atención",v_msj_alerta,"stop")

         LET INT_FLAG = 1
         RETURN 1
    ELSE

--******************************************************************************
         IF v_datos.consec_cuenta IS NULL AND
            v_datos.nss IS NULL AND
            v_datos.rfc IS NULL AND
            v_datos.curp IS NULL AND 
            v_datos.nombre_trabajador IS NULL THEN
            CALL fn_mensaje("Consulta Decreto",
                            "Debe de ingresar algún campo de búsqueda.",
                            "about")
            RETURN 1
         END IF
         ACCEPT CONSTRUCT
   END IF
      ON ACTION CANCEL
         LET INT_FLAG = 1
         EXIT CONSTRUCT
         
   END CONSTRUCT

   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN

      #Primero se valida que exista el precio de accion
      SELECT precio_fondo 
      INTO v_precio
      FROM glo_valor_fondo 
      WHERE fondo = 11 AND 
      f_valuacion = TODAY

      IF v_precio IS NULL OR v_precio = 0 THEN
         CALL fn_mensaje("Consulta Decreto",
                           "No se encontro el precio de accion para el dia de hoy.",
                            "about")
         INITIALIZE v_datos TO NULL
         RETURN 1
      END IF
   
      #Se buscan los datos del cliente
      LET v_consulta_cliente =   "SELECT FIRST 51 ",
                                    "id_decreto, ",
                                    "consec_cuenta," ,
                                    "nss, ",
                                    "rfc, ",
                                    "curp, ",
                                    "nombre_trabajador, ",
                                    "cve_icefa, ",
                                    "nombre_patron ",
                                 "FROM afi_decreto ",
                                 "WHERE ", v_condicion 
      PREPARE exe_consulta_cliente FROM v_consulta_cliente
      DECLARE cur_consulta_cliente CURSOR FOR exe_consulta_cliente

      LET i = 1
      FOREACH cur_consulta_cliente INTO v_lista_clientes[i].*
         LET i = i + 1
         IF i > MAX_REGISTROS THEN
            CALL fn_mensaje("Consulta Decreto",
                            "Acotar mas el criterio de búsqueda. \n"||
                            "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH
      CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
      CLOSE cur_consulta_cliente
      FREE cur_consulta_cliente

      IF v_lista_clientes.getLength() > 0 THEN
         IF v_lista_clientes.getLength() = 1 THEN
            LET v_datos.id_decreto           = v_lista_clientes[1].id_decreto
            LET v_datos.consec_cuenta        = v_lista_clientes[1].consec_cuenta
            LET v_datos.nss                  = v_lista_clientes[1].nss
            LET v_datos.rfc                  = v_lista_clientes[1].rfc
            LET v_datos.curp                 = v_lista_clientes[1].curp
            LET v_datos.nombre_trabajador      = v_lista_clientes[1].nombre_trabajador
            LET v_datos.cve_icefa            = v_lista_clientes[1].cve_icefa
            LET v_datos.nombre_patron        = v_lista_clientes[1].nombre_patron
         ELSE
            #Si se encotro mas de un cliente con el filtro de busqueda se muestra la lista para que el usuario seleccione a un cliente
            OPEN WINDOW vtn_ctac102 WITH FORM "CTAC102" --ATTRIBUTES (STYLE="dialog")
               DISPLAY ARRAY v_lista_clientes TO lista_clientes.*
                  ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_datos.id_decreto = v_lista_clientes[ARR_CURR()].id_decreto
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     INITIALIZE v_datos       TO NULL
                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW vtn_ctac102
         END IF
      ELSE
         CALL fn_mensaje("Consulta Decreto",
                         "No existen registros con el criterio de búsqueda. \n",
                         "about")
      END IF
      RETURN 1
   ELSE
      RETURN 0
   END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

--******************************************************************************
         SELECT nss
      INTO v_datos.nss
      FROM afi_decreto
      WHERE id_decreto = p_id_decreto

      CALL fn_valida_nss()
      
      IF r_valida_nss = 0 THEN
--******************************************************************************
   #primero se buscan los datos generales del cliente
   CALL fn_datos_generales()

   #Se ejecuta la funcion que consulta los movimientos de decreto
   CALL fn_consulta_decreto()

   #Se muestran los datos en pantalla
   DIALOG   ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_lista_decreto TO lista_decreto.* END DISPLAY

      BEFORE DIALOG
         CALL forma.setElementHidden("group3",0)
         CALL forma.setElementHidden("group2",0)
         CALL forma.setElementHidden("group5",0)

         DISPLAY v_datos.consec_cuenta    TO consec_cuenta
         DISPLAY v_datos.nss              TO nss
         DISPLAY v_datos.rfc              TO rfc
         DISPLAY v_datos.curp             TO curp
         DISPLAY v_datos.nombre_trabajador  TO nombre
         DISPLAY v_datos.cve_icefa        TO icefa
         DISPLAY v_datos.nombre_patron    TO patron
         DISPLAY v_saldo_total            TO saldo
         DISPLAY v_acciones_total         TO acciones_total
         DISPLAY v_precio                 TO precio

      ON ACTION ACCEPT
         LET v_datos.id_decreto = NULL

         WHENEVER ERROR CONTINUE
         DROP TABLE tmp_decreto
         WHENEVER ERROR STOP

         RETURN 1
         
         EXIT DIALOG
        
      ON ACTION cancelar
         LET v_datos.id_decreto = NULL

         WHENEVER ERROR CONTINUE
         DROP TABLE tmp_decreto
         WHENEVER ERROR STOP         
         
         RETURN 1
         EXIT DIALOG

   END DIALOG

   RETURN 1
--******************************************************************************
   ELSE 
      CALL fn_mensaje("Atención",v_msj_alerta, "stop")
      LET v_datos.id_decreto = NULL
         RETURN 1
END IF
--******************************************************************************
END FUNCTION

PRIVATE FUNCTION fn_datos_generales()
   SELECT 
          id_decreto,
          consec_cuenta,
          nss,
          rfc,
          curp,
          nombre_trabajador,
          cve_icefa,
          nombre_patron 
     INTO v_datos.*
     FROM afi_decreto
    WHERE id_decreto = v_datos.id_decreto
END FUNCTION

PRIVATE FUNCTION fn_consulta_decreto()
   DEFINE v_consulta_decreto     STRING

   DEFINE i                      SMALLINT

 {  CREATE TEMP TABLE tmp_decreto (
      id_decreto      DECIMAL(9,0),
      f_liquida       DATE,
      folio_liquida   DECIMAL(9,0),
      movimiento      VARCHAR(100),
      origen          VARCHAR(30),
      monto_acciones  DECIMAL(22,2),
      monto_pesos     DECIMAL(22,2),
      fondo_inversion SMALLINT
      )
}
   
   LET v_consulta_decreto =   "SELECT ",
                                 "mov.id_decreto, ",
                                 "mov.f_liquida, ",
                                 "mov.folio_liquida, ",
                                 "mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                                 "mov.origen, ",
                                 "mov.monto_acciones, ",
                                 "mov.monto_pesos, ",
                                 "mov.fondo_inversion ",
                              "FROM cta_his_decreto mov ",
                              "INNER JOIN tia_det_traspaso tia ON mov.id_decreto = tia.id_decreto ",
                              "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento ",
                              "WHERE mov.id_decreto = ?  AND tia.result_operacion NOT IN('01','99')"
{                              " UNION ALL ",
                              "SELECT ",
                                 "mov.id_decreto, ",
                                 "mov.f_liquida, ",
                                 "mov.folio_liquida, ",
                                 "mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                                 "mov.origen, ",
                                 "mov.monto_acciones, ",
                                 "mov.monto_pesos, ",
                                 "mov.fondo_inversion ",
                              "FROM cta_his_decreto mov ",
                              "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento ",
                              "WHERE mov.id_decreto = ? ",
                              "ORDER BY mov.f_liquida DESC"
}
                              
   PREPARE exe_consulta_decreto FROM v_consulta_decreto
   DECLARE cur_consulta_decreto CURSOR FOR exe_consulta_decreto

   LET i = 1
   FOREACH cur_consulta_decreto USING v_datos.id_decreto INTO v_lista_decreto[i].*
--g-      INSERT INTO tmp_decreto VALUES (v_lista_decreto[i].*)
DISPLAY v_lista_decreto[i].*
      LET i = i + 1
      IF i > MAX_REGISTROS THEN
         CALL fn_mensaje("Consulta Decreto",
                         "Solo se mostrara los ultimos " || MAX_REGISTROS || " movimientos de decreto.",
                         "about")
      END IF
   END FOREACH
   CALL v_lista_decreto.deleteElement(v_lista_decreto.getLength())
   CLOSE cur_consulta_decreto
   FREE cur_consulta_decreto

   SELECT SUM(mov.monto_acciones * gf.precio_fondo), SUM(monto_acciones)
   INTO v_saldo_total, v_acciones_total
   FROM cta_his_decreto mov
--g-   FROM tmp_decreto mov
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = mov.fondo_inversion AND gf.f_valuacion = TODAY)
   WHERE mov.fondo_inversion <> 0
   AND mov.id_decreto = v_datos.id_decreto

END FUNCTION

FUNCTION fn_valida_nss()

   DEFINE v_funcion_nss                STRING
   DEFINE v_cadena                     CHAR(11)
   DEFINE v_tpo_consulta               SMALLINT

   LET v_tpo_consulta = 3 

   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_nss FROM v_funcion_nss
   LET v_cadena = v_datos.nss
   EXECUTE prp_valida_nss USING v_cadena, v_tpo_consulta, p_usuario
                           INTO r_valida_nss

END FUNCTION