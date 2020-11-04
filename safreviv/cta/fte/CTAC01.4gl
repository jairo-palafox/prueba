
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC01                                                  #
#Objetivo          => CONSULTA DE SALDOS                                      #
#Fecha Inicio      => 20-MARZO-2012                                           #
###############################################################################

DATABASE safre_viv

GLOBALS "CTAC01.inc"

   DEFINE g_usuario                 CHAR(10)
   DEFINE p_tipo_proc               CHAR(1)
   DEFINE p_nombre_menu             CHAR(50)
   DEFINE g_id_derechohabiente      DECIMAL(9,0)
   DEFINE g_ciclo                   SMALLINT

   DEFINE v_resultado               SMALLINT
   DEFINE v_tpo_orig                SMALLINT
   DEFINE v_tpo_cred                SMALLINT
   DEFINE v_tpo_dscto               SMALLINT
   DEFINE v_valida                  SMALLINT

   DEFINE vsdo_ini                  DECIMAL(22,2)
   DEFINE vsdo_fin                  DECIMAL(22,2)
   DEFINE vrendimiento              DECIMAL(22,2)
   DEFINE v_hoy                     DATE

   DEFINE v_folio                   DECIMAL(9,0)
   DEFINE v_referencia              DECIMAL(9,0)

   DEFINE r_detmov DYNAMIC ARRAY OF RECORD
      fecha_mov                     DATE,
      tpo_mov                       INTEGER,
      desc_mov                      CHAR(80),
      fondo                         SMALLINT,
      pesos                         DECIMAL(16,6),
      acciones                      DECIMAL(16,6),
      f_valor                       DATE,
      folio                         DECIMAL(10,0),
      origen                        CHAR(20),
      referencia                    DECIMAL(9,0),
      modulo                        CHAR(3)
   END RECORD

   DEFINE g_datos_generales RECORD
      v_nss                         CHAR(11),
      v_rfc                         CHAR(13),
      v_curp                        CHAR(18),
      nombre_completo               CHAR(120),
      v_tpo_originacion             CHAR(30),
      v_tpo_credito                 CHAR(30),
      v_edo_credito                 CHAR(30),
      v_num_credito                 DECIMAL(10,0),
      v_f_otorga                    DATE,
      v_f_liquida                   DATE,
      v_descto                      CHAR(20)
   END RECORD

   DEFINE f                         ui.form
   DEFINE w                         ui.window

   DEFINE r_valida_nss              SMALLINT
   DEFINE v_nss                     CHAR(11)

   PRIVATE DEFINE ventana_his       ui.Window
   PRIVATE DEFINE forma_his         ui.Form

   #Variable para establecer la instruccion que genera el Estado de cuenta
   DEFINE v_exe_edo_cuenta          STRING
   DEFINE g_consulta                STRING

   DEFINE v_s_qryTxt                STRING --ejecución función créditos de vivienda
   DEFINE v_msj_alerta              STRING --Mensaje de alerta para consultas rojas

   DEFINE v_mov_dis                 SMALLINT
   DEFINE v_proc_dis                SMALLINT
   
MAIN

   LET g_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET g_id_derechohabiente = ARG_VAL(4)

   #CALL STARTLOG(g_usuario CLIPPED ||".CTAC01.log")

   CLOSE WINDOW SCREEN

   #Se limpian los arreglos para evitar el arrastre de datos
   CALL r_detmov.clear()
   INITIALIZE g_datos_generales.* TO NULL

   LET p_nombre_menu = "Consulta de Saldo"
   CALL ui.Interface.setText(p_nombre_menu)

   LET v_hoy = TODAY

   OPEN WINDOW ctac011 WITH FORM "CTAC011"

   CALL fn_alertamiento()
       RETURNING v_msj_alerta

   -- Feb13, 2013. ISVH. Se verifica si LQINFO esta ejecutando
   #IF ( f_existe_proceso_operacion_ejecutando(1401, NULL) ) THEN
      -- alguna operación de LQINFO se encuentra ejecutando, por lo que no se permite realizar la consulta
   #   CALL fn_mensaje("Atención","Un proceso de LQINFO se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
   #ELSE

     LET w = ui.Window.forName("ctac011")
     LET f = w.getForm()

     LET g_ciclo = TRUE

     WHILE g_ciclo
        IF g_id_derechohabiente IS NULL THEN
           LET g_id_derechohabiente = 0
           WHILE g_id_derechohabiente = 0
              CALL fn_busca_cliente() RETURNING g_id_derechohabiente
           END WHILE
        ELSE
           LET g_ciclo = FALSE
        END IF

        IF g_id_derechohabiente > 0 THEN
           CALL fn_inicializa()
           CALL fn_saldo()
        END IF

        LET g_id_derechohabiente = NULL
     END WHILE
   #END IF

   CLOSE WINDOW ctac011

END MAIN

FUNCTION fn_busca_cliente()

   --DEFINE v_nss                 CHAR(11)
   DEFINE v_curp                CHAR(18)
   DEFINE v_rfc                 CHAR(13)
   DEFINE v_condicion           STRING
   DEFINE v_query               STRING
   DEFINE i                     SMALLINT
   DEFINE v_id_derechohabiente  DECIMAL (9,0)
   DEFINE v_arr_busqueda        DYNAMIC ARRAY OF datos_generales

   INITIALIZE v_arr_busqueda TO NULL

   CALL fn_inicializa()

   CALL f.setElementHidden("group5",1)
   CALL f.setElementHidden("group4",1)
   CALL f.setElementHidden("group3",1)
   CALL f.setElementHidden("group2",1)
   CALL f.setElementHidden("group6",1)

   INPUT BY NAME v_nss,v_rfc,v_curp ATTRIBUTES(UNBUFFERED,WITHOUT DEFAULTS)

      BEFORE INPUT
         LET v_nss  = NULL
         LET v_rfc  = NULL
         LET v_curp = NULL
         CLEAR FORM 

      ON ACTION ACCEPT
 
         LET INT_FLAG    = FALSE
         LET v_condicion = NULL

         IF v_nss IS NULL AND
            v_rfc IS NULL AND
            v_curp IS NULL THEN
            CALL fn_mensaje("Consulta Saldo", "Debe de ingresar algún campo de búsqueda.", "about")
            NEXT FIELD v_nss
         END IF

         IF v_nss IS NOT NULL THEN
            IF fn_valida_caracteres(v_nss) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_nss
            END IF
            CALL fn_valida_nss()
            LET v_condicion = "\n AND nss = ","'",v_nss,"'"
         END IF

         IF v_rfc IS NOT NULL THEN
            IF fn_valida_caracteres(v_rfc) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_rfc
            END IF

            SELECT nss
              INTO v_nss
              FROM afi_derechohabiente
             WHERE rfc = v_rfc
               AND tipo_trabajador <> "V"    -- tipo nss virtual
               AND origen_afiliacion <> "S"  -- origen separación de cuentas  

            CALL fn_valida_nss()
            LET v_condicion = v_condicion ,"\n AND rfc = ","'",v_rfc,"'"
         END IF

         IF v_curp IS NOT NULL THEN
            IF fn_valida_caracteres(v_curp) <> 0 THEN
               CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD v_curp
            END IF
            
            SELECT nss
              INTO v_nss
              FROM afi_derechohabiente
             WHERE curp = v_curp
               AND tipo_trabajador <> "V"    -- tipo nss virtual
               AND origen_afiliacion <> "S"  -- origen separación de cuentas  

            CALL fn_valida_nss()
            LET v_condicion = v_condicion,"\n AND curp = ","'",v_curp,"'"
         END IF

         IF r_valida_nss = 0 THEN
            ACCEPT INPUT
         ELSE
            CALL fn_mensaje("Atención",v_msj_alerta, "stop")
         END IF

         LET g_ciclo     = FALSE
         LET INT_FLAG    = TRUE
         LET v_condicion = NULL
         EXIT INPUT

      ON ACTION CANCEL
         LET g_ciclo     = FALSE
         LET INT_FLAG    = TRUE
         LET v_condicion = NULL
         EXIT INPUT

   END INPUT

   IF NOT INT_FLAG THEN
      LET v_query = "SELECT id_derechohabiente, ",
                          "\n nss,  ",
                          "\n rfc,  ",
                          "\n curp, ",
                          "\n TRIM(nombre_af)||' '|| ",
                          "\n TRIM(ap_paterno_af)||' '|| ",
                          "\n TRIM(ap_materno_af) ",
                     "\n FROM afi_derechohabiente ",
                    "\n WHERE 1 = 1 ",
                     v_condicion CLIPPED,
                     "\n AND tipo_trabajador <> 'V' ",
                     "\n AND origen_afiliacion <> 'S' "

      PREPARE prp_busqueda FROM v_query
      DECLARE cur_busqueda CURSOR FOR prp_busqueda

      LET i = 1
      FOREACH cur_busqueda INTO v_arr_busqueda[i].*
         LET i = i + 1
         IF i > 50 THEN
            CALL fn_mensaje("Consulta Saldo",
                            "Acotar más el criterio de búsqueda. \n"||
                            "Se muestran sólo los primeros 50 registros",
                            "about")
            EXIT FOREACH
         END IF
      END FOREACH

      IF i > 1 THEN
         IF i = 2 THEN
            LET v_id_derechohabiente = v_arr_busqueda[1].id_derechohabiente
         ELSE
            OPEN WINDOW ctac012 WITH FORM "CTAC012" ATTRIBUTES (STYLE="dialog")
               DISPLAY ARRAY v_arr_busqueda TO v_arreglo.*
                  ON ACTION ACCEPT 
                     LET INT_FLAG = FALSE
                     LET v_id_derechohabiente = v_arr_busqueda[ARR_CURR()].id_derechohabiente
                     EXIT DISPLAY

                  ON ACTION CANCEL
                     LET v_id_derechohabiente = 0
                     EXIT DISPLAY
               END DISPLAY
            CLOSE WINDOW ctac012
         END IF
      ELSE
         CALL fn_mensaje("Consulta Saldo",
                         "No existe registros con el criterio de búsqueda. \n",
                         "about")
      END IF
   ELSE
      LET v_id_derechohabiente = -1
   END IF

   RETURN v_id_derechohabiente

END FUNCTION

FUNCTION fn_saldo()

--******************************************************************************
   IF g_id_derechohabiente IS NOT NULL THEN
      SELECT nss
        INTO v_nss
        FROM afi_derechohabiente
       WHERE id_derechohabiente = g_id_derechohabiente
         AND tipo_trabajador <> "V"    -- tipo nss virtual
         AND origen_afiliacion <> "S"  -- origen separación de cuentas  

      CALL fn_valida_nss()
   END IF
      IF r_valida_nss <> 0 THEN
         CALL fn_mensaje("Atención",v_msj_alerta,"stop")
     ELSE    
--******************************************************************************
   CALL fn_datos_generales()

   DISPLAY BY NAME g_datos_generales.v_nss,
                   g_datos_generales.v_rfc,
                   g_datos_generales.v_curp,
                   g_datos_generales.nombre_completo,
                   g_datos_generales.v_tpo_originacion,
                   g_datos_generales.v_tpo_credito,
                   g_datos_generales.v_edo_credito,
                   g_datos_generales.v_num_credito,
                   g_datos_generales.v_f_otorga,
                   g_datos_generales.v_f_liquida,
                   g_datos_generales.v_descto

   CALL muestra_saldos()
END IF

END FUNCTION

FUNCTION fn_datos_generales()

   LET v_resultado = ""
   LET v_tpo_orig  = ""
   LET v_tpo_cred  = ""
   LET v_tpo_dscto = ""

   LET g_datos_generales.v_tpo_originacion = ""
   LET g_datos_generales.v_tpo_credito     = ""
   LET g_datos_generales.v_edo_credito     = ""

   LET g_consulta = " SELECT FIRST 1 afi.nss       ,",
                    "        afi.rfc               ,",
                    "        afi.curp              ,",
                    "        TRIM(afi.nombre_af)||' '||",
                    "        TRIM(afi.ap_paterno_af)||' '||",
                    "        NVL(TRIM(afi.ap_materno_af),' ')",
                    " FROM afi_derechohabiente afi ",
                    " WHERE afi.id_derechohabiente = ", g_id_derechohabiente CLIPPED,
                    " AND tipo_trabajador <> 'V' ",
                    " AND origen_afiliacion <> 'S' "

   PREPARE qry_credito FROM g_consulta
   EXECUTE qry_credito INTO g_datos_generales.v_nss,
                            g_datos_generales.v_rfc,
                            g_datos_generales.v_curp,
                            g_datos_generales.nombre_completo

   LET v_valida = 1

   LET v_s_qryTxt = "EXECUTE PROCEDURE fn_edo_cred_viv(?,?)"

   PREPARE prp_credito_vivienda FROM v_s_qryTxt
   EXECUTE prp_credito_vivienda USING g_id_derechohabiente,
                                      v_valida
                                 INTO v_resultado,
                                      v_tpo_orig,
                                      v_tpo_cred,
                                      g_datos_generales.v_num_credito,
                                      g_datos_generales.v_f_otorga,
                                      g_datos_generales.v_f_liquida,
                                      v_tpo_dscto

   CASE v_resultado
     WHEN 0 LET g_datos_generales.v_edo_credito = "CREDITO VIGENTE"
     WHEN 2 LET g_datos_generales.v_edo_credito = "CREDITO LIQUIDADO"
     WHEN 3 LET g_datos_generales.v_edo_credito = "CREDITO EN TRÁMITE"
     OTHERWISE LET g_datos_generales.v_edo_credito = "SIN CRÉDITO"
   END CASE

   CASE v_tpo_dscto
     WHEN 1 LET g_datos_generales.v_descto = "PORCENTAJE"
     WHEN 2 LET g_datos_generales.v_descto = "PESOS"
     WHEN 3 LET g_datos_generales.v_descto = "VSM"
     OTHERWISE LET g_datos_generales.v_descto = ""
   END CASE

   #IF v_resultado = 0 THEN
   #   LET g_datos_generales.v_edo_credito = "CREDITO VIGENTE"
   #ELSE
   #   IF v_resultado = 2 THEN
   #      LET g_datos_generales.v_edo_credito = "CREDITO LIQUIDADO"
   #   ELSE
   #      LET g_datos_generales.v_edo_credito = "SIN CREDITO"
   #   END IF
   #END IF

   SELECT org.originacion_desc
     INTO g_datos_generales.v_tpo_originacion
     FROM cat_cre_originacion org
    WHERE org.tpo_originacion = v_tpo_orig

   SELECT cat.desc_credito
     INTO g_datos_generales.v_tpo_credito
     FROM cat_tipo_credito cat
    WHERE cat.tpo_originacion = v_tpo_orig
      AND cat.tpo_credito = v_tpo_cred

END FUNCTION

FUNCTION muestra_saldos()

   DEFINE arr_arbol DYNAMIC ARRAY OF RECORD
       subcuenta_desc               CHAR(60),
       siefore                      SMALLINT,
       monto_pesos                  DECIMAL(16,6),
       monto_acciones               DECIMAL(16,6),
       subcuenta                    SMALLINT,
       movimiento                   SMALLINT,
       padre_id                     CHAR(40),
       id                           CHAR(40),
       nivel                        SMALLINT
    END RECORD

   DEFINE arr_precios  DYNAMIC ARRAY OF RECORD
      precio_cod                    SMALLINT,
      precio_sie                    CHAR(30),
      precio_al_dia                 DECIMAL(19,14)
   END RECORD

   DEFINE i                         SMALLINT
   DEFINE v_subcuenta               SMALLINT
   DEFINE v_siefore                 SMALLINT
   DEFINE v_movimiento              SMALLINT
   DEFINE resp_visualiza            SMALLINT
   DEFINE v_pos                     SMALLINT

   DEFINE v_query                   STRING
   DEFINE v_grupo                   STRING

   LET vsdo_fin = 0
   LET resp_visualiza = 0

   PREPARE prp_proc FROM "EXECUTE PROCEDURE sp_consulta_saldo(?)"
   EXECUTE prp_proc USING g_id_derechohabiente

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
   FOREACH cur_precio USING v_hoy INTO arr_precios[i].*
      LET i = i + 1
   END FOREACH
   CLOSE cur_precio
   FREE cur_precio

   CALL arr_precios.deleteElement(i)

   PREPARE prp_arbol FROM "SELECT * FROM tmp_arbol_saldo ORDER BY id "
   DECLARE cur_arbol CURSOR FOR prp_arbol
    
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

      #IF arr_arbol[i].subcuenta = 0 THEN
      #   IF arr_arbol[i].siefore <> 0 AND v_grupo <> 47 THEN
      #      LET vsdo_fin   = vsdo_fin + arr_arbol[i].monto_pesos
      #   END IF
      #END IF
      IF arr_arbol[i].nivel = 2 
         AND arr_arbol[i].siefore = 11
         AND(arr_arbol[i].subcuenta = 4 
               OR arr_arbol[i].subcuenta = 8 
               OR arr_arbol[i].subcuenta = 42 
               OR arr_arbol[i].subcuenta = 44 
               OR arr_arbol[i].subcuenta = 55 
               OR arr_arbol[i].subcuenta = 57) THEN
            LET vsdo_fin   = vsdo_fin + arr_arbol[i].monto_pesos
      END IF

      -- se cambian las leyendas de los agrupadores principales con grupo = 6
      -- AGOSTO 13, 2012

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

   CLOSE cur_arbol
   FREE cur_arbol

   #Se elimina la tabla temporal del arbol
   LET v_query = "DROP TABLE IF EXISTS tmp_arbol_saldo"
   PREPARE exe_drop_temporal FROM v_query
   EXECUTE exe_drop_temporal

   CALL arr_arbol.deleteElement(i)
   LET i = i - 1 

   CALL f.setElementHidden("group5",0)
   CALL f.setElementHidden("group4",0)
   CALL f.setElementHidden("group3",0)
   CALL f.setElementHidden("group2",0)
   CALL f.setElementHidden("group6",0)
   CALL ui.Interface.refresh()

   DIALOG ATTRIBUTES(UNBUFFERED)
   DISPLAY ARRAY arr_arbol TO tree.* 
      BEFORE DISPLAY
         CALL DIALOG.setactionhidden("close",1)

         #Se inhabilita el llamado al estado de cuenta por peticion del usuario (PRODINFXVI-66)
         #CALL DIALOG.setActionActive("edo_cuenta",FALSE)
         #Se habilita el boton unicamente para perfiles autorizados
         #CALL DIALOG.setActionActive("edo_cuenta",fn_activa_edo_cuenta())

         #Se habilita el boton unicamente para perfiles autorizados
         #CALL DIALOG.setActionActive("mov_resumen",fn_activa_detalle_mov())
         CALL DIALOG.setActionHidden("mov_resumen",fn_oculta_detalle_mov())
         
         IF NOT resp_visualiza THEN
            CALL r_detmov.clear()
         END IF
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

         IF vsdo_fin IS NULL THEN
            LET vsdo_fin = 0
         END IF

         LET vrendimiento = vsdo_fin - vsdo_ini

         DISPLAY vsdo_fin TO sdo_fin

     {
      #Boton que invoca la generacion del estado de cuenta, se inhibe hasta nuevo aviso
      ON ACTION edo_cuenta
         LET v_exe_edo_cuenta = "cd ../../srv/bin/;fglrun SRVP09.42r '", g_datos_generales.v_nss,"'"
         RUN v_exe_edo_cuenta 
     }

     ON ACTION marcas
         CALL fn_consulta_marcas()
         
      ON ACTION mov_historico
         CALL fn_consulta_historico()
         
      ON ACTION mov_resumen
         IF fn_valida_separacion() = FALSE THEN
            CALL fn_consulta_detalle()
         ELSE
            CALL fn_mensaje("Atención","Existe un proceso de separación de cuentas con el NSS consultado,\n en cuanto concluya el trámite podrás consultar sus movimientos.","information")
         END IF
         

      ON ACTION saldo_historico
         CALL fn_consulta_saldo_historico(g_id_derechohabiente)
         
      ON ACTION CANCEL
         CALL fn_finaliza_temporal()
         EXIT DIALOG

   END DISPLAY

   DISPLAY ARRAY r_detmov TO detalle.*
      ON ACTION ACCEPT
         #Solo se mostrará el detalle para movimientos de pag o acl
         IF r_detmov[ARR_CURR()].modulo = 'pag' OR r_detmov[ARR_CURR()].modulo = 'acl' THEN
            LET v_folio = r_detmov[ARR_CURR()].folio
            LET v_referencia = r_detmov[ARR_CURR()].referencia
            IF ( f_existe_proceso_operacion_ejecutando(1401, NULL) ) THEN
               -- alguna operacion de LQINFO se encuentra ejecutando, por lo que no se permite realizar la consulta
               CALL fn_mensaje("Atención","Un proceso de LQINFO se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
            ELSE
               CALL fn_consulta_pago(v_folio, v_referencia)
            END IF
         END IF

         IF r_detmov[ARR_CURR()].modulo = 'dis' THEN
            LET v_folio = r_detmov[ARR_CURR()].folio
            LET v_referencia = r_detmov[ARR_CURR()].referencia
            LET v_mov_dis    = r_detmov[ARR_CURR()].tpo_mov
            LET v_proc_dis   = 901
            IF ( f_existe_proceso_operacion_ejecutando(901, NULL) ) THEN
               -- alguna operacion de Dispersión de Pagos se encuentra ejecutando, por lo que no se permite realizar la consulta
               CALL fn_mensaje("Atención","Un proceso de Dispersión de Pagos se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
            ELSE
               IF ( f_existe_proceso_operacion_ejecutando(902, NULL) ) THEN
                  -- alguna operacion de Avance de Pagos se encuentra ejecutando, por lo que no se permite realizar la consulta
                  CALL fn_mensaje("Atención","Un proceso de Avance de Pagos se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
               ELSE 
                  IF ( f_existe_proceso_operacion_ejecutando(932, NULL) ) THEN
                     -- alguna operacion de Aportaciones Subsecuentes se encuentra ejecutando, por lo que no se permite realizar la consulta
                     CALL fn_mensaje("Atención","Un proceso de Aportaciones Subsecuentes se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
                  ELSE
                     CALL fn_consulta_dispersion(v_folio, v_referencia, v_mov_dis, g_id_derechohabiente, v_proc_dis)
                  END IF
               END IF
            END IF
         END IF

      ON ACTION CANCEL
         EXIT DIALOG
   END DISPLAY

   DISPLAY ARRAY arr_precios TO precios.*
      ON ACTION CLOSE
         EXIT DIALOG
   END DISPLAY

   END DIALOG

END FUNCTION

FUNCTION despliega_detalle_mov(p_subcuenta,p_fondo, p_movimiento)

    DEFINE i                        INTEGER

    DEFINE p_subcuenta              SMALLINT
    DEFINE p_fondo                  SMALLINT
    DEFINE p_movimiento             SMALLINT

    DEFINE v_query                  STRING

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
                    " FROM tmp_movimientos_saldo t, ",
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

FUNCTION fn_inicializa()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_saldo
      DROP TABLE tmp_arbol_saldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_movimientos_saldo
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

   CREATE TEMP TABLE tmp_arbol_saldo
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

PRIVATE FUNCTION fn_finaliza_temporal()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_saldo
      DROP TABLE tmp_arbol_saldo
   WHENEVER ERROR STOP

END FUNCTION

FUNCTION fn_consulta_marcas()

   DEFINE comando                   STRING
   DEFINE l_ruta_bin                CHAR(40)

   INITIALIZE comando TO NULL

   SELECT ct.ruta_bin
   INTO l_ruta_bin
   FROM seg_modulo ct
   WHERE modulo_cod = 'cta'

   LET comando = "cd ",l_ruta_bin CLIPPED,"/; fglrun CTAC02 '",g_usuario,
                "' '",p_tipo_proc, "' 'Consulta de Marcas' '",g_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comando = comando CLIPPED
    RUN comando
     
END FUNCTION

FUNCTION fn_consulta_pago(p_folio, p_referencia)

   DEFINE p_folio                   DECIMAL(9,0)
   DEFINE p_referencia              DECIMAL(9,0)
   DEFINE comando                   STRING
   DEFINE l_ruta_bin                CHAR(40)

   INITIALIZE comando TO NULL

   SELECT ct.ruta_bin
   INTO l_ruta_bin
   FROM seg_modulo ct
   WHERE modulo_cod = 'pag'

   LET comando = "cd ",l_ruta_bin CLIPPED,"/; fglrun PAGC19 '",g_usuario,
                "' '",p_tipo_proc, "' 'Consulta detalle de pago' ",
                "'", p_folio, "' '", p_referencia, "'"

    CALL ui.interface.refresh()

    LET comando = comando CLIPPED
    RUN comando

END FUNCTION 

FUNCTION fn_consulta_dispersion(p_folio, p_referencia, p_movimiento, p_id_derechohabiente, p_proc_dis)

   DEFINE p_folio                   DECIMAL(9,0)
   DEFINE p_referencia              DECIMAL(9,0)
   DEFINE p_movimiento              SMALLINT
   DEFINE p_proc_dis                SMALLINT
   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE comando                   STRING
   DEFINE l_ruta_bin                CHAR(40)

   INITIALIZE comando TO NULL

   SELECT ct.ruta_bin
   INTO l_ruta_bin
   FROM seg_modulo ct
   WHERE modulo_cod = 'dis'

   LET comando = "cd ",l_ruta_bin CLIPPED,"/; fglrun DISC37 '",g_usuario, "' '",
                                                               p_proc_dis, 
                                                            "' 'Consulta detalle dispersión de pago'",
                                                            " '", p_id_derechohabiente, "'",
                                                            " '", p_movimiento, "'",
                                                            " '", p_folio, "'", 
                                                            " '", p_referencia, "'"
    DISPLAY "comando: ", comando
    CALL ui.interface.refresh()

    LET comando = comando CLIPPED
    RUN comando
END FUNCTION 

PRIVATE FUNCTION fn_consulta_historico()

   DEFINE v_finicio                 DATE
   DEFINE v_ffin                    DATE
   
   OPEN WINDOW vtn_ctac013 WITH FORM "CTAC013" --ATTRIBUTES (STYLE="dialog")

      LET ventana_his = ui.Window.forName("vtn_ctac013")
      LET forma_his = ventana_his.getForm()

      #Ocultamos las secciones de las listas porque no tienen datos
      CALL forma_his.setElementHidden("group3",1)
 
      INPUT v_finicio, v_ffin FROM finicio, ffin ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
         BEFORE INPUT
            LET v_finicio = TODAY
            LET v_ffin = TODAY
            DISPLAY g_datos_generales.v_nss           TO nss
            DISPLAY g_datos_generales.v_curp          TO curp
            DISPLAY g_datos_generales.nombre_completo TO nombre
            DISPLAY v_finicio                         TO finicio
            DISPLAY v_ffin                            TO ffin
            
         ON ACTION ACCEPT
            IF v_finicio IS NULL OR v_ffin IS NULL THEN
               CALL fn_mensaje("Consulta Historico",
                            "Debe de ingresar el rango de busqueda de búsqueda.",
                            "about")
            ELSE
               CALL fn_buscar_historico(v_finicio, v_ffin)
            END IF
         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT
   CLOSE WINDOW vtn_ctac013

END FUNCTION

PRIVATE FUNCTION fn_consulta_detalle()

   DEFINE v_finicio                 DATE
   DEFINE v_ffin                    DATE
   DEFINE v_opcion                  SMALLINT
   DEFINE v_nom_reporte             STRING
   DEFINE v_ruta_lst                LIKE seg_modulo.ruta_listados
   DEFINE cont_cta_hist             DECIMAL(10,0)
   
   OPEN WINDOW vtn_ctac014 WITH FORM "CTAC014" ATTRIBUTES (STYLE="dialog")

      LET ventana_his = ui.Window.forName("vtn_ctac014")
      LET forma_his = ventana_his.getForm()

      OPTIONS INPUT WRAP
 
      INPUT v_finicio, v_ffin, v_opcion FROM finicio, ffin, cb_tiporeporte 
                                                        ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)
         BEFORE INPUT
            LET v_finicio = TODAY
            LET v_ffin = TODAY
            LET v_opcion = 1
            DISPLAY g_datos_generales.v_nss           TO nss
            DISPLAY v_opcion                          TO cb_tiporeporte
            DISPLAY v_finicio                         TO finicio
            DISPLAY v_ffin                            TO ffin
            
            
         ON ACTION ACCEPT
            IF v_opcion IS NULL OR v_opcion = "" AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "Selecciona el tipo de categoría a consultar",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_finicio IS NULL AND v_ffin IS NULL AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "Selecciona correctamente la fecha inicial y final",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_finicio > TODAY OR v_ffin > TODAY AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "La fecha seleccionada es posterior a la permitida",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_finicio IS NULL AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "Selecciona correctamente la fecha inicial",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_finicio < MDY(01,01,1992) AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "La fecha seleccionada debe ser posterior al 1 de enero de 1992",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_ffin IS NULL AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "Selecciona correctamente la fecha final",
                            "about")
               LET INT_FLAG = TRUE
            END IF
            
            IF v_finicio > v_ffin AND INT_FLAG = FALSE THEN
               CALL fn_mensaje("Consulta Detalle de Movimientos",
                            "La fecha inicial es mayor a la fecha final",
                            "about")
               LET INT_FLAG = TRUE
            END IF

            IF INT_FLAG = FALSE THEN
              
               CALL fn_genera_detalle_movimientos(g_datos_generales.v_nss,v_opcion,v_finicio,
                                     v_ffin,2,"") RETURNING v_ruta_lst, v_nom_reporte
                                     
               IF v_ruta_lst IS NULL OR v_nom_reporte IS NULL THEN
                  CALL fn_mensaje("Consulta Detalle de Movimientos",
                               "No se encontraron movimientos en el periodo seleccionado",
                               "about")
               ELSE
                  SELECT FIRST 1 seq_cta_his_consulta_resumen.nextval
                  INTO cont_cta_hist 
	              FROM systables
    
                  INSERT INTO cta_his_consulta_resumen 
                  VALUES (cont_cta_hist,
                          g_datos_generales.v_nss,
                          v_finicio,
                          v_ffin,
                          v_opcion,
                          2,
                          'CONSULTASACI',
                          '01',
                          '000',
                          g_usuario,
                          TODAY,
                          CURRENT
                          )
                  
                  CALL fn_despliega_archivos("cta" CLIPPED,v_nom_reporte CLIPPED||".pdf")
               END IF
            ELSE
               LET INT_FLAG = FALSE
            END IF
            
         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT INPUT
      END INPUT
   CLOSE WINDOW vtn_ctac014

END FUNCTION

PRIVATE FUNCTION fn_buscar_historico(p_finicio, p_ffin)

   DEFINE p_finicio                 DATE
   DEFINE p_ffin                    DATE
   DEFINE v_calcula_historico       STRING
   DEFINE v_consulta_historico      STRING

   DEFINE v_lista_historico         DYNAMIC ARRAY OF historico
   DEFINE i                         SMALLINT

   WHENEVER ERROR CONTINUE
      LET v_calcula_historico = "execute procedure sp_gen_his_mov(?)";
      PREPARE exe_calcula_historico FROM v_calcula_historico
      EXECUTE exe_calcula_historico USING g_id_derechohabiente

      IF SQLCA.SQLCODE <> 0 THEN
         CALL fn_mensaje("Consulta Histórico",
                            "Ocurrió un problema al intentar consultar el histórico, favor de contactar al administrador.",
                            "about")
      ELSE
         #Consulta de los movimientos historicos
         LET v_consulta_historico = "SELECT ",
                                    "mov.f_liquida, ",
                                    "mov.monto_acciones, ",
                                    "mov.monto_pesos, ",
                                    "pre.precio_fondo, ",
                                    "(mov.fondo_inversion || ' - '  || fon.razon_social), ",
                                    "(mov.subcuenta || ' - ' || cta.subcuenta_corta), ",
                                    "(mov.movimiento || ' - ' || cat_mov.movimiento_desc), ",
                                    "mov.folio_liquida, ",
                                    "mov.origen ",
                                    "FROM tmp_movimiento mov ",
                                    "INNER JOIN glo_valor_fondo pre ON (pre.f_valuacion = mov.f_liquida ",
                                    "             AND pre.fondo = mov.fondo_inversion) ",
                                    "INNER JOIN cat_fondo_local fon ON fon.fondo = mov.fondo_inversion ",
                                    "INNER JOIN cat_subcuenta cta ON cta.subcuenta = mov.subcuenta ",
                                    "INNER JOIN cat_movimiento cat_mov ON cat_mov.movimiento = mov.movimiento ",
                                    "WHERE mov.f_liquida >= ? ",
                                    "AND mov.f_liquida <= ? ",
                                    "ORDER BY mov.f_liquida, mov.folio_liquida "
         PREPARE exe_consulta_historico FROM v_consulta_historico
         DECLARE cur_consulta_historico CURSOR FOR exe_consulta_historico 

         LET i = 1
         FOREACH cur_consulta_historico USING p_finicio, p_ffin INTO v_lista_historico[i].*
            LET i = i + 1
            IF i > MAX_REGISTROS THEN
               CALL fn_mensaje("Consulta Historico",
                               "Acotar mas el criterio de búsqueda. \n"||
                               "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                               "about")
               EXIT FOREACH
            END IF
         END FOREACH

         CALL v_lista_historico.deleteElement(v_lista_historico.getLength())
         CLOSE cur_consulta_historico
         FREE cur_consulta_historico

         IF v_lista_historico.getLength() > 0 THEN
            #Se presentan los datos
            DIALOG   ATTRIBUTES(UNBUFFERED)
               DISPLAY ARRAY v_lista_historico       TO lista_historico.* END DISPLAY

               BEFORE DIALOG
                  CALL forma_his.setElementHidden("group3",0)

               ON ACTION ACCEPT
                  INITIALIZE v_lista_historico TO NULL
                  CALL forma_his.setElementHidden("group3",1)
                  EXIT DIALOG
                  
            END DIALOG
         ELSE
            CALL fn_mensaje("Consulta Histórico",
                               "No se encontraron registros con el periodo indicado",
                               "about")
         END IF
      END IF
   WHENEVER ERROR STOP
   
END FUNCTION

FUNCTION fn_valida_nss()

   DEFINE v_funcion_nss             STRING
   DEFINE v_cadena                  CHAR(11)
   DEFINE v_tpo_consulta            SMALLINT

   LET v_tpo_consulta = 2
   
   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_nss FROM v_funcion_nss
   --FOR a= 1 TO v_paso_nss.getLength()
   LET v_cadena = v_nss
   EXECUTE prp_valida_nss USING v_cadena,v_tpo_consulta,g_usuario
                           INTO r_valida_nss
   --END FOR 

END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)

   DEFINE p_campo                   STRING
   RETURN p_campo.getIndexOf("*",1)

END FUNCTION

PRIVATE FUNCTION fn_activa_edo_cuenta()

   DEFINE v_respuesta               BOOLEAN
   DEFINE v_query                   STRING
   DEFINE v_perfil                  INTEGER

   LET v_respuesta = FALSE

   INITIALIZE v_perfil TO NULL 

   LET v_query =  "SELECT FIRST 1 per.perfil_cod ",
                  "FROM seg_usuario_perfil usr ",
                  "INNER JOIN seg_perfil per ON per.perfil_cod = usr.perfil_cod ",
                  "WHERE usr.usuario_cod = '", g_usuario CLIPPED ,"' ",
                  "AND (per.perfil_corta IN (", PERFILES_EDO_CTA, ") OR per.perfil_cod = ", PERFIL_ADMIN, ")"

   PREPARE exe_consulta_perfil FROM v_query
   EXECUTE exe_consulta_perfil INTO v_perfil

   IF v_perfil IS NOT NULL OR v_perfil > 0 THEN
      LET v_respuesta = TRUE
   END IF

   RETURN v_respuesta

END FUNCTION

PRIVATE FUNCTION fn_oculta_detalle_mov()

   DEFINE v_respuesta               BOOLEAN
   DEFINE v_query                   STRING
   DEFINE v_perfil                  INTEGER

   LET v_respuesta = true

   INITIALIZE v_perfil TO NULL 

   LET v_query =  "SELECT FIRST 1 per.perfil_cod ",
                  "FROM seg_usuario_perfil usr ",
                  "INNER JOIN seg_perfil per ON per.perfil_cod = usr.perfil_cod ",
                  "WHERE usr.usuario_cod = '", g_usuario CLIPPED ,"' ",
                  "AND (per.perfil_corta IN (", PERFILES_DET_MOV, ") OR per.perfil_cod = ", PERFIL_ADMIN, ")"

   PREPARE exe_consulta_perfil_mov FROM v_query
   EXECUTE exe_consulta_perfil_mov INTO v_perfil

   IF v_perfil IS NOT NULL OR v_perfil > 0 THEN
      LET v_respuesta = FALSE
   END IF

   RETURN v_respuesta

END FUNCTION

PRIVATE FUNCTION fn_valida_separacion()
   DEFINE v_tmp_valida              DECIMAL(9,0)
   DEFINE v_respuesta               BOOLEAN

   INITIALIZE v_tmp_valida TO NULL
   LET v_respuesta = TRUE 
   
   SELECT FIRST 1 id_derechohabiente
   INTO v_tmp_valida 
   FROM sfr_marca_activa
   WHERE marca  IN (280,701,702,813)
   AND id_derechohabiente = g_id_derechohabiente

   IF v_tmp_valida IS NULL THEN
      LET v_respuesta = FALSE
   END IF
   
   RETURN v_respuesta

END FUNCTION