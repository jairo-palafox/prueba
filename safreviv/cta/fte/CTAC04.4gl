################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Módulo            => LEY 72                                                   #
#Programa          => L72C01                                                   #
#Objetivo          => Consultar información de Ley 72 para que el usuario      #
#                     seleccione la operación a ejecutar                       #
#Fecha Inicio      => 23/05/2012                                               #
#Adecuación        => Ejecución de función (store) de crédito vivienda         #
################################################################################

DATABASE safre_viv

GLOBALS "CTAC04.inc"

GLOBALS

    DEFINE v_ind_estado_cuenta SMALLINT
    DEFINE v_desc_edo_cuenta   CHAR(19)
    DEFINE v_f_estado_cuenta   DATE
DEFINE arr_det_hist_general_comp DYNAMIC ARRAY OF RECORD 
          v_nss          CHAR(11),
          v_rfc          CHAR(13),
          v_nombre       CHAR(40),
          v_folio        DECIMAL(9,0),
          v_ejercicio    SMALLINT,
          v_clave_mov    CHAR(2),
          v_empresa      CHAR(40),
          v_bimestres    SMALLINT,
          v_importe      DECIMAL(10,2),
          v_ind_verifica SMALLINT,
          v_f_movimiento DATE,
          v_desc_mov     CHAR(60),
          v_ref          CHAR(25)
END RECORD

END GLOBALS

#Variables para capturar los parámetros que recibe la consulta
   PRIVATE DEFINE p_usuario            CHAR(10)
   PRIVATE DEFINE p_tipo_proc          CHAR(1)
   PRIVATE DEFINE p_nombre_menu        CHAR(50)
   PRIVATE DEFINE p_id_fondo           DECIMAL(9,0)

   PRIVATE DEFINE v_saldo_total        DECIMAL(22,2)

   #Variables para el filtro de busqueda
   PRIVATE DEFINE v_datos                       datos_generales

   #Lista para los movimientos de decreto
   PRIVATE DEFINE v_lista_fondo DYNAMIC ARRAY OF movimientos_fondo

   #Variables para el manejo de la pantalla
   PRIVATE DEFINE ventana                       ui.Window
   PRIVATE DEFINE forma                         ui.Form

MAIN

   DEFINE v_ciclo          SMALLINT

   LET v_ciclo = 1

   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_fondo           = ARG_VAL(4)

   IF p_id_fondo IS NOT NULL THEN
      LET v_datos.id_afi_fondo72 = p_id_fondo
   END IF

   #CALL STARTLOG(p_usuario CLIPPED ||".CTAC04.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Consulta de Fondo 72"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW vtn_L72C011 WITH FORM "CTAC041"

      LET ventana = ui.Window.getCurrent()
      LET forma   = ventana.getForm()

      WHILE v_ciclo = 1
         IF v_datos.id_afi_fondo72 IS NULL THEN
            CALL fn_nueva_busqueda() RETURNING v_ciclo
         END IF

         IF v_datos.id_afi_fondo72 IS NOT NULL THEN
            CALL fn_presenta_datos() RETURNING v_ciclo
         END IF
      END WHILE

   CLOSE WINDOW vtn_L72C011

END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()

   DEFINE v_consulta_cliente              STRING
   DEFINE v_consulta_rfc                  STRING
   DEFINE v_condicion                     STRING
   DEFINE v_rfc_filtro                    STRING
   DEFINE v_rfc10                         CHAR(10)

   DEFINE v_respuesta                     INTEGER
   DEFINE i                               INTEGER
   DEFINE v_ind_seleccion                 INTEGER
   DEFINE v_ciclo                         INTEGER

   DEFINE v_lista_clientes DYNAMIC ARRAY OF RECORD
      id_afi_fondo72                      DECIMAL(9,0),
      nss                                 CHAR(11),
      rfc                                 CHAR(13),
      nombre_completo                     VARCHAR(60),
      elige_registro                      SMALLINT
   END RECORD

   DEFINE v_lista_clientes_1 DYNAMIC ARRAY OF RECORD
      id_afi_fondo72                      DECIMAL(9,0),
      nss                                 CHAR(11),
      rfc                                 CHAR(13),
      nombre_completo                     VARCHAR(60),
      elige_registro                      SMALLINT
   END RECORD

   DEFINE v_id_dh_f72                     DECIMAL(9,0)
   DEFINE v_resultado                     SMALLINT
   DEFINE v_tpo_originacion               SMALLINT
   DEFINE v_tpo_credito                   SMALLINT
   DEFINE v_num_credito                   DECIMAL(10,0)
   DEFINE v_f_otorga                      DATE
   DEFINE v_f_liquida                     DATE
   DEFINE v_desc_tipo_credito             CHAR(30)
   DEFINE v_tipo_credito_desc             CHAR(34)
   DEFINE v_estado_credito                CHAR(10)
   DEFINE v_qryTxt                        STRING

   #Se inicializan las valiables del filtro
   INITIALIZE v_datos               TO NULL
   INITIALIZE v_lista_fondo         TO NULL
   LET v_saldo_total = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("gr_resumen_saldo",1)
   CALL forma.setElementHidden("gr_busqueda",     1)
   CALL forma.setElementHidden("gr_movimientos",  1)
   CALL forma.setElementHidden("gr_det_historico",1)
   CALL forma.setElementHidden("gr_creditos",1)

   CONSTRUCT v_condicion ON nss, rfc, nombre
                         FROM nss, rfc, nombre

      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_datos.nss    = GET_FLDBUF(nss)
         LET v_datos.rfc    = GET_FLDBUF(rfc)
         LET v_datos.nombre_completo    = GET_FLDBUF(nombre)

         LET INT_FLAG = FALSE

         IF v_datos.nss IS NULL AND
            v_datos.rfc IS NULL AND 
            v_datos.nombre_completo IS NULL THEN
            CALL fn_mensaje("Consulta Fondo 72",
                            "Debe de ingresar algún campo de búsqueda.",
                            "about")
            RETURN 1
         END IF

         ACCEPT CONSTRUCT

      ON ACTION CANCEL
         LET INT_FLAG = 1
         EXIT CONSTRUCT

   END CONSTRUCT

   #Si en la secciÓn de parÁmetros de bÚsqueda se seleccionÓ aceptar pinta las siguientes secciones
   IF NOT INT_FLAG THEN
      #Se buscan los datos del cliente
      LET v_consulta_cliente =   "SELECT FIRST 51 ",
                                       " id_afi_fondo72, ",
                                       " nss, ",
                                       " rfc, ",
                                       " nombre ",
                                  "FROM afi_fondo72 ",
                                  "WHERE ", v_condicion

                                 DISPLAY v_consulta_cliente

      PREPARE exe_consulta_cliente FROM v_consulta_cliente
      DECLARE cur_consulta_cliente CURSOR FOR exe_consulta_cliente

      LET i = 1

      FOREACH cur_consulta_cliente INTO v_lista_clientes[i].*
         --Consulta id_derechohabiente
         SELECT id_derechohabiente 
         INTO   v_id_dh_f72
         FROM   afi_fondo72
         WHERE  id_afi_fondo72 = v_lista_clientes[i].id_afi_fondo72

         LET i = i + 1
         IF i > MAX_REGISTROS THEN
            CALL fn_mensaje("Consulta Fondo 72",
                            "Acotar mas el criterio de búsqueda. \n"||
                            "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH

      CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
      CLOSE cur_consulta_cliente
      FREE cur_consulta_cliente

      IF v_lista_clientes.getLength() = 0 THEN  #No se encontraron registros con el filtro original
         #Se valida que el campo RFC13 no sea nulo
         IF v_datos.rfc IS NOT NULL THEN
            LET v_rfc_filtro = v_datos.rfc CLIPPED

            IF v_rfc_filtro.getLength() > 10 THEN  #RFC > 10
               CALL fn_ventana_confirma("Atención",
                    "No existen registros con el criterio de búsqueda. \n"||
                         "Se ejecutara  la consulta con el RFC a 10 posiciones",
                     "quest") RETURNING v_respuesta

               IF v_respuesta = 1 THEN #Se ejecuta la consulta con RFC10
                  LET v_rfc10 = v_rfc_filtro.subString(1,10)
                  LET v_consulta_rfc = "SELECT FIRST 51 ",
                                              "id_afi_fondo72, ",
                                              "nss, ",
                                              "rfc, ",
                                              "nombre ",
                                         "FROM afi_fondo72 ",
                                        "WHERE rfc[1,10] = ?",
                                        "AND ind_estado_cuenta = 0" -- activas

                  PREPARE exe_consulta_rfc FROM v_consulta_rfc
                  DECLARE cur_consulta_rfc CURSOR FOR exe_consulta_rfc

                  LET i = 1

                  FOREACH cur_consulta_rfc USING v_rfc10 INTO v_lista_clientes[i].*
                     --Consulta id_derechohabiente
                     SELECT id_derechohabiente 
                     INTO   v_id_dh_f72
                     FROM   afi_fondo72
                     WHERE  id_afi_fondo72 = v_lista_clientes[i].id_afi_fondo72

                     LET i = i + 1

                     IF i > MAX_REGISTROS THEN
                        CALL fn_mensaje("Consulta Fondo 72",
                                        "Acotar mas el criterio de búsqueda. \n"||
                                        "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                                        "about")                                        
                        EXIT FOREACH
                     END IF
                  END FOREACH

                  CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
                  CLOSE cur_consulta_rfc
                  FREE cur_consulta_rfc

               END IF   #FIN consulta con RFC10
            END IF   #FIN Validación de tamaño para RFC
         END IF   #FIN RFC no nulo
      END IF   #FIN no se encontraron registros con el filtro original

      IF v_lista_clientes.getLength() > 0 THEN
         IF v_lista_clientes.getLength() < 1 THEN
            CALL fn_mensaje("Consulta Ley 72", "No se encontró información con los parámetros proporcionados","about");
         ELSE
            #Si se encontró más de un cliente con el filtro de búsqueda se muestra la lista para que el usuario seleccione a un cliente
            CALL forma.setElementHidden("gr_busqueda",0)

            DIALOG ATTRIBUTES(UNBUFFERED)
               DISPLAY ARRAY v_lista_clientes TO scr_busqueda.*

               BEFORE ROW 
                  LET INT_FLAG = FALSE
                  LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                  CALL fn_presenta_datos() RETURNING v_ciclo

                  --Consulta id_derechohabiente
                  SELECT id_derechohabiente 
                    INTO v_id_dh_f72
                    FROM afi_fondo72
                   WHERE id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72

                  IF v_id_dh_f72 IS NOT NULL THEN
                     LET v_qryTxt = "EXECUTE FUNCTION fn_credito_vivienda(",v_id_dh_f72,", 1)"
                     PREPARE prp_cred_inf FROM v_qryTxt

                     EXECUTE prp_cred_inf INTO v_resultado,
                                               v_tpo_originacion,
                                               v_tpo_credito,
                                               v_num_credito,
                                               v_f_otorga,
                                               v_f_liquida

                     IF v_resultado = 0 OR v_resultado = 2 THEN
                        --ejecuta consulta de creditos
                        SELECT desc_credito
                        INTO   v_desc_tipo_credito 
                        FROM   cat_tipo_credito
                        WHERE  tpo_originacion = v_tpo_originacion
                        AND    tpo_credito = v_tpo_credito

                        LET v_tipo_credito_desc = v_tpo_credito || " - " || v_desc_tipo_credito

                        --Determina si es crédito Vigente o Liquidado
                        CASE
                           WHEN v_resultado = 2
                              LET v_estado_credito = "LIQUIDADO"
                           WHEN v_resultado = 0
                              LET v_estado_credito = "VIGENTE"
                        END CASE
                     ELSE 
                        LET v_tipo_credito_desc = "SIN CRÉDITO"
                     END IF

                     DISPLAY v_tipo_credito_desc TO ed_tipo_credito
                     DISPLAY v_num_credito       TO ed_numero_credito
                     DISPLAY v_f_otorga          TO ed_fecha_otorga 
                     DISPLAY v_estado_credito    TO ed_edo_credito
                  END IF

               BEFORE DISPLAY 
                  CALL forma.setElementHidden("gr_resumen_saldo",0)
                  CALL forma.setElementHidden("gr_movimientos",0)
                  CALL forma.setElementHidden("gr_creditos",0)

                  {ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                     CALL fn_presenta_datos() RETURNING v_ciclo}

                  --Se consultan los detalles históricos
                  ON ACTION historico
                     CALL fn_consulta_det_hist_gral(v_lista_clientes[ARR_CURR()].nss,
                                                    v_lista_clientes[ARR_CURR()].rfc)

                     --Si no tiene movimientos
                     IF v_i_dethist = 1 THEN 
                        CALL fn_mensaje("Atención", "No se encontraron movimientos históricos", "stop")
                        CALL arr_det_hist_general_comp.clear() 
                     ELSE
                     --Muestra detalles si existen movimientos
                        CALL forma.setElementHidden("gr_det_historico",0)
                        DISPLAY ARRAY arr_det_hist_general_comp  TO scr_det_historicos.*
                        ATTRIBUTES(ACCEPT = FALSE)   
                           ON ACTION CANCEL  
                              CALL arr_det_hist_general_comp.clear() 
                              CALL forma.setElementHidden("gr_det_historico",1)
                              EXIT DISPLAY 
                        END DISPLAY
                     END IF

                  --###  Botón que ejecuta la función de unificación
                  ON ACTION unificacion 
                     CALL fn_unifica_ley72(v_condicion, p_usuario)
                  --###  Botón que ejecuta la función de separación
                  ON ACTION separacion 
                     --CALL fn_mensaje ("Atencion", "Se ejecutará la separación", "About")
                     CALL fn_consulta_expedientes_infonavit(p_usuario,v_lista_clientes[ARR_CURR()].id_afi_fondo72)

                  -- 08Julio2013. Se agrega función para modificar datos del derechohabiente en fondo 72
                  ON ACTION modificacion
                     CALL fn_modificar_datos(v_datos.id_afi_fondo72, v_datos.nss, v_datos.rfc, v_datos.nombre_completo)
                     RETURNING v_datos.nss, v_datos.rfc, v_datos.nombre_completo

                     DISPLAY v_datos.nss              TO nss
                     DISPLAY v_datos.rfc              TO rfc
                     DISPLAY v_datos.nombre_completo  TO nombre
                     CALL ui.interface.refresh()

                  ON ACTION cancelar 
                     INITIALIZE v_datos       TO NULL
                     EXIT DIALOG 

                    AFTER DISPLAY 
                    DISPLAY ""
                    
               END DISPLAY 

               
                  DISPLAY ARRAY v_lista_fondo TO scr_det_movimientos.*
                  --DISPLAY v_lista_fondo 
                     ON ACTION CANCEL 
                        EXIT DIALOG 
                  END DISPLAY               

            END DIALOG         
            CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())  
         END IF
                     
      ELSE
         CALL fn_mensaje("Consulta Fondo 72",
                         "No existen registros con el criterio de búsqueda. \n",
                         "about")
      END IF
      RETURN 1
   ELSE
      RETURN 0
   END IF

END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()

   #primero se buscan los datos generales del cliente
   CALL fn_datos_generales()

   #Se ejecuta la función que consulta los movimientos de fondo 72
   CALL fn_consulta_fondo()

   DISPLAY v_datos.nss              TO nss
   DISPLAY v_datos.rfc              TO rfc
   DISPLAY v_datos.nombre_completo  TO nombre
   DISPLAY v_saldo_total            TO saldo
   DISPLAY v_desc_edo_cuenta        TO ed_estado_cta
   DISPLAY v_f_estado_cuenta        USING "dd-mm-yyyy" TO ed_fecha_edo 

   RETURN 1

END FUNCTION

PRIVATE FUNCTION fn_datos_generales()

   SELECT id_afi_fondo72,
          nss,
          rfc,
          nombre
     INTO v_datos.*
     FROM afi_fondo72
    WHERE id_afi_fondo72 = v_datos.id_afi_fondo72

    END FUNCTION

#OBJETIVO: Consultar información del detalle de movimientos
PRIVATE FUNCTION fn_consulta_fondo()

   DEFINE v_consulta_fondo       STRING
   DEFINE i                      SMALLINT

   LET v_consulta_fondo =   "\n SELECT mov.id_afi_fondo72, ",
                            "\n        mov.f_liquida, ",
                            "\n        mov.folio_liquida, ",
                            "\n        mov.movimiento || ' - ' || TRIM(cat.movimiento_desc), ",
                            "\n        mov.origen, ",
                            "\n        mov.importe ",
                            "\n FROM   cta_fondo72 mov ",
                            "\n LEFT JOIN cat_movimiento cat ",
                            "\n ON cat.movimiento = mov.movimiento ",
                            "\n WHERE mov.id_afi_fondo72 = ", v_datos.id_afi_fondo72 ,
                            "\n ORDER BY mov.f_liquida DESC"
--DISPLAY v_consulta_fondo
   PREPARE exe_consulta_fondo FROM v_consulta_fondo
   DECLARE cur_consulta_fondo CURSOR FOR exe_consulta_fondo

   LET i = 1
   CALL v_lista_fondo.clear()
   FOREACH cur_consulta_fondo INTO v_lista_fondo[i].*
      LET i = i + 1
      IF i > MAX_REGISTROS THEN
         CALL fn_mensaje("Consulta Fondo 72",
                         "Solo se mostrara los ultimos " || MAX_REGISTROS || " movimientos de Fondo 72.",
                         "about")
      END IF
   END FOREACH

   CALL v_lista_fondo.deleteElement(v_lista_fondo.getLength())
   CLOSE cur_consulta_fondo
   FREE cur_consulta_fondo

   SELECT NVL(SUM(mov.importe),0)
     INTO v_saldo_total
     FROM cta_fondo72 mov
    WHERE mov.id_afi_fondo72 = v_datos.id_afi_fondo72
      AND mov.movimiento NOT IN (422,601,841,2022)      #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                                        #Omitimos el ABONO RETIRO FONDO 72-92, TANTO ADICIONAL **** REQ 333 *** MODIFICÓ: Ricardo Perez
                                                        #Omitimos el ABONO REVERSO OPERATIVO TANTO ADICIONAL   **** PRODINF-587 :Jaime Galeno
                                                        #Omitimos el CARGO REVERSO OPERATIVO TANTO ADICIONAL   **** Jaime Galeno
                                          
   IF v_lista_fondo.getLength() IS NULL  OR v_lista_fondo.getLength() < 1 THEN
      LET v_saldo_total = 0.0
   END IF

   SELECT ind_estado_cuenta,
          f_estado_cuenta
     INTO v_ind_estado_cuenta,
          v_f_estado_cuenta
     FROM afi_fondo72
    WHERE id_afi_fondo72 = v_datos.id_afi_fondo72

   IF v_ind_estado_cuenta = 0 THEN 
      LET v_desc_edo_cuenta = "0 - CUENTA ACTIVA"
   END IF 

   IF v_ind_estado_cuenta = 1 THEN 
      LET v_desc_edo_cuenta = "1 - CUENTA INACTIVA"
   END IF

END FUNCTION

#OBJETIVO: Consultar los movimientos históricos del NSS seleccionado
FUNCTION fn_consulta_det_hist_gral(p_nss, p_rfc)

   DEFINE p_nss            CHAR(11)
   DEFINE p_rfc            CHAR(13)
   DEFINE v_QryTxt         STRING

--DISPLAY "ENTRA LA FUNCIÓN DE CONSULTAR HISTÓRICOS", "  NSS   - ",p_nss, "   RFC   - ",p_rfc
-- Se incluye la información de los movimientos historicos de referencia, num. crédito y/o dictamen SACI2019-40
--   LET v_QryTxt = "\n SELECT nss,",
--                  "\n        rfc,",
--                  "\n        nombre,",
--                  "\n        folio,",
--                  "\n        ejercicio,",
--                  "\n        clave_mov,",
--                  "\n        empresa,",
--                  "\n        bimestres,",
--                  "\n        importe,",
--                  "\n        ind_verifica",
--                  "\n FROM   cta_his_fondo72",
--                  "\n WHERE  nss = ", "'", p_nss ,"'",
--                  "\n AND    rfc = ", "'", p_rfc ,"'"
   LET v_QryTxt = "\n SELECT a.nss,",
                  "\n        a.rfc,",
                  "\n        a.nombre,",
                  "\n        a.folio,",
                  "\n        a.ejercicio,",
                  "\n        a.clave_mov,",
                  "\n        a.empresa,",
                  "\n        a.bimestres,",
                  "\n        a.importe,",
                  "\n        a.ind_verifica,",
                  "\n        b.f_movimiento,",
                  "\n        b.desc_movimiento,",
                  "\n        b.cve_movimiento",
                  "\n FROM   cta_his_fondo72 a",
                  "\n        LEFT OUTER JOIN cta_his_fondo72_complemento b",
                  "\n                     ON a.id_cta_his_fondo72 = b.id_cta_his_fondo72",
                  "\n WHERE  nss = ", "'", p_nss ,"'",
                  "\n AND    rfc = ", "'", p_rfc ,"'"
   --DISPLAY v_QryTxt

   LET v_i_dethist = 1

   PREPARE prp_cons_historico FROM v_QryTxt
   DECLARE cur_cons_historico CURSOR FOR prp_cons_historico

   FOREACH cur_cons_historico INTO arr_det_hist_general_comp[v_i_dethist].*  
      LET v_i_dethist = v_i_dethist + 1
   END FOREACH

   CALL arr_det_hist_general_comp.deleteElement(v_i_dethist)

   --DISPLAY "Total de elementos encontrados en el histórico", v_i_dethist
END FUNCTION
