################################################################################-
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#MODULO            => RETIRO GENERICO FONDO DE AHORRO                          #
#PROGRAMA          => RETM276                                                  #
#OBJETIVO          => PANTALLA DE CAPTURA PARA LAS SOLICITUDES DE RETIRO       #                       
#                     GENERICO DE LAS SOLICITUDES RECHAZADAS (EXCEPCIONES)     #
#FECHA INICIO      => 03/09/2014                                               #
################################################################################
DATABASE safre_viv

GLOBALS "RETG01.4gl"

GLOBALS

   CONSTANT MAX_REGISTROS     SMALLINT = 50
   CONSTANT VISIBLE           SMALLINT = 0
   CONSTANT OCULTO            SMALLINT = 1
   CONSTANT ACTIVO            SMALLINT = 1
   CONSTANT INACTIVO          SMALLINT = 0
   
   TYPE datos_generales RECORD
      id_afi_fondo72           DECIMAL(9,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      nombre_completo          VARCHAR(60)
   END RECORD

   TYPE movimientos_fondo RECORD
      id_afi_fondo72             DECIMAL(9,0),
      f_liquida                  DATE,
      folio_liquida              DECIMAL(9,0),
      movimiento                 VARCHAR(100),
      origen                     VARCHAR(30),
      monto_pesos                DECIMAL(22,2)
   END RECORD

DEFINE arr_datos_unificar DYNAMIC ARRAY OF RECORD
       v_uni_id_afi_fondo72  DECIMAL(9,0),
       v_uni_nss             CHAR(11),
       v_uni_rfc             CHAR(13),
       v_uni_nombre          CHAR(30),
       v_uni_fecha_liquida   DATE,
       v_uni_folio_liquida   DECIMAL(9,0),
       v_uni_movimiento      VARCHAR(100), 
       v_uni_origen          VARCHAR(30), 
       v_uni_monto_pesos     DECIMAL(22,2),
       v_uni_tipo_nss        SMALLINT
END RECORD

DEFINE arr_det_hist_general DYNAMIC ARRAY OF RECORD 
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
   
DEFINE v_ind_datos   INTEGER,
       v_i_dm        INTEGER,
       v_i_dethist   INTEGER     

       DEFINE v_ind_estado_cuenta SMALLINT,
       v_desc_edo_cuenta   CHAR(19),
       v_f_estado_cuenta   DATE
      DEFINE g_usuario            CHAR(10)

END GLOBALS

#Variables para capturar los parametros que recibe la consulta
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

   DEFINE v_ciclo              SMALLINT
   
   LET v_ciclo = 1
   
   LET p_usuario            = ARG_VAL(1)
   LET p_tipo_proc          = ARG_VAL(2) -- Recibe el tipo de proceso
   LET p_nombre_menu        = ARG_VAL(3) -- Recibe el nombre del programa
   LET p_id_fondo           = ARG_VAL(4)
   
   LET g_usuario = p_usuario;

   IF p_id_fondo IS NOT NULL THEN
      LET v_datos.id_afi_fondo72 = p_id_fondo
   END IF

   --CALL STARTLOG(p_usuario CLIPPED ||".RETM276.log")

   CLOSE WINDOW SCREEN

   -- se asigna el titulo del programa
   LET p_nombre_menu = "Captura de Excepciones"
   CALL ui.Interface.setText(p_nombre_menu)

   OPEN WINDOW w_276 WITH FORM "RETM2761"

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

   CLOSE WINDOW w_276
END MAIN

PRIVATE FUNCTION fn_nueva_busqueda()
   DEFINE v_consulta_cliente              STRING
   DEFINE v_consulta_rfc                  STRING
   DEFINE v_condicion                     STRING
   DEFINE v_rfc_filtro                    STRING
   DEFINE v_rfc10                         CHAR(10)
   DEFINE cbx_causal                      ui.ComboBox    -- combo de causal
   DEFINE cmb_causales                    LIKE ret_causal_retiro.causal_retiro
   DEFINE monto_causal                    DECIMAL(10,2)
   DEFINE monto_tanto_adicional           DECIMAL(10,2)
   DEFINE imp_pagar                       DECIMAL(10,2)
   DEFINE importe_beneficiario            DECIMAL(10,2)
   DEFINE importe_beneficiario_dos        DECIMAL(10,2)
   DEFINE importe_beneficiario_tres       DECIMAL(10,2)
   DEFINE importe_beneficiario_cuatro     DECIMAL(10,2)
   DEFINE importe_beneficiario_cinco      DECIMAL(10,2)
   DEFINE ed_caso_crm                     CHAR(10)
   DEFINE beneficiario                    CHAR(50)
   DEFINE ref_dap                         CHAR(15)
   DEFINE beneficiario_dos                CHAR(50)
   DEFINE ref_dap_dos                     CHAR(15)
   DEFINE beneficiario_tres               CHAR(50)
   DEFINE ref_dap_tres                    CHAR(15)
   DEFINE beneficiario_cuatro             CHAR(50)
   DEFINE ref_dap_cuatro                  CHAR(15)
   DEFINE beneficiario_cinco              CHAR(50)
   DEFINE ref_dap_cinco                   CHAR(15)
   
   DEFINE v_sec_pension                   LIKE ret_datamart.sec_pension 
   DEFINE v_respuesta                     INTEGER
   DEFINE i                               INTEGER
   DEFINE v_ind_seleccion                 INTEGER
   DEFINE v_ciclo                         INTEGER
   DEFINE v_r_ret_causal_excepcion        RECORD LIKE ret_causal_excepcion.*
   DEFINE v_r_ret_cat_banco               RECORD LIKE cat_ent_financiera.*
   DEFINE v_cadena                        STRING
   DEFINE v_r_ret_solicitud_generico      RECORD LIKE ret_solicitud_generico.*
   DEFINE v_r_ret_fondo_ahorro_generico   RECORD LIKE ret_fondo_ahorro_generico.*
   DEFINE v_r_ret_beneficiario_generico   RECORD LIKE ret_beneficiario_generico.*
   DEFINE v_consec_beneficiario           SMALLINT 
   DEFINE v_r_ret_pago_spei               RECORD LIKE ret_pago_spei.* -- registro de pago por spei
   DEFINE v_r_ret_pago_dap                RECORD LIKE ret_pago_dap.*  -- registro de pago por referencia bancaria
   DEFINE v_tpo_pago                      SMALLINT
   DEFINE v_marca_entra                   LIKE sfr_marca_activa.marca
   DEFINE v_sql                           STRING 
   DEFINE v_estado_marca                  SMALLINT 
   DEFINE v_codigo_rechazo                SMALLINT 
   DEFINE v_marca_causa                   SMALLINT 
   DEFINE v_id_derechohabiente            DECIMAL(9,0)
   DEFINE v_fecha_causa                   DATE 
   DEFINE v_usuario                       CHAR(20)
   DEFINE v_proceso_cod                   SMALLINT 
   DEFINE v_folio                         SMALLINT 
   DEFINE v_respuesta_marcaje             SMALLINT
   DEFINE v_mas_beneficiarios             SMALLINT
   DEFINE bnd_error_cad                   SMALLINT 
   DEFINE v_referencia_dap                CHAR(12)
   DEFINE v_consecutivo_ref               DECIMAL(5,0) 
   DEFINE v_anio_fecha                    CHAR(4)
   DEFINE v_existe_solicitud              DECIMAL(9,0)
   DEFINE arr_archivo              RECORD 
          v_archivo_1              STRING,
          v_archivo_2              STRING,
          v_archivo_3              STRING,
          v_archivo_4              STRING,
          v_archivo_5              STRING
   END RECORD 
   DEFINE v_num_beneficiarios      SMALLINT
   
   DEFINE v_lista_clientes         DYNAMIC ARRAY OF RECORD
      id_afi_fondo72               DECIMAL(9,0),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      nombre_completo              VARCHAR(60),
      saldo                        DECIMAL(10,2)
   END RECORD
   DEFINE v_r_lista_clientes       RECORD
      id_afi_fondo72               DECIMAL(9,0),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      nombre_completo              VARCHAR(60),
      saldo                        DECIMAL(10,2)
   END RECORD

  DEFINE v_lista_clientes_1         DYNAMIC ARRAY OF RECORD
      id_afi_fondo72               DECIMAL(9,0),
      nss                          CHAR(11),
      rfc                          CHAR(13),
      nombre_completo              VARCHAR(60),
      saldo                        DECIMAL(10,2)
   END RECORD

   DEFINE v_id_dh_f72         DECIMAL(9,0),     
           v_resultado        SMALLINT, 
          v_tpo_originacion   SMALLINT, 
          v_tpo_credito       SMALLINT, 
          v_num_credito       DECIMAL(10,0), 
          v_f_otorga          DATE,
          v_f_liquida         DATE,
          v_desc_tipo_credito CHAR(30),
          v_tipo_credito_desc CHAR(34),
          v_estado_credito    CHAR(10),
          v_id_sol_actual     DECIMAL(9,0),
          v_id_der_actual     DECIMAL(9,0),
          v_f_sol_actual      DATE,
          v_causal_excepcion  SMALLINT,
          v_rfc2              CHAR(13),
          v_nss2               CHAR(11),
          v_nombre1           VARCHAR(60),
          v_beneficiario      CHAR(50),
          v_saldo_viv72       DECIMAL(10,2),
          v_monto_tanto_adicional DECIMAL(10,2),
          v_imp_pagar         DECIMAL(10,2),
          v_clabe_bancaria    CHAR(18),
          v_dap               CHAR(15),
          v_nombre            CHAR(60)
   DEFINE v_reg_benef         DYNAMIC ARRAY OF RECORD
         v_id_sol               DECIMAL(9,0),
         v_consec_benef         SMALLINT
   END RECORD
   DEFINE v_contador          SMALLINT
   DEFINE v_por_benef         DECIMAL(6,2)

   #Se inicializan las valiables del filtro
   INITIALIZE v_datos               TO NULL
   INITIALIZE v_lista_fondo         TO NULL
   LET v_saldo_total = 0
   INITIALIZE v_reg_benef           TO NULL
   LET v_contador    = 0

   #Ocultamos las secciones de las listas porque no tienen datos
   CALL forma.setElementHidden("gr_resumen_saldo",OCULTO)
   CALL forma.setElementHidden("gr_busqueda",     OCULTO)
   CALL forma.setElementHidden("gr_movimientos",  OCULTO)
   CALL forma.setElementHidden("gr_det_historico",OCULTO)
   CALL forma.setElementHidden("gr_creditos",OCULTO)
   CALL forma.setElementHidden("grp_causales",OCULTO)
   
   CONSTRUCT v_condicion ON nss, rfc, nombre
                         FROM nss, rfc, nombre
      BEFORE CONSTRUCT
         CLEAR FORM

      ON ACTION ACCEPT
         LET v_datos.nss    = GET_FLDBUF(nss)
         LET v_datos.rfc    = GET_FLDBUF(rfc)
         LET v_datos.nombre_completo    = GET_FLDBUF(nombre)
         
         --LET INT_FLAG = FALSE

         IF v_datos.nss IS NULL AND
            v_datos.rfc IS NULL AND 
            v_datos.nombre_completo IS NULL THEN
            CALL fn_mensaje("Captura Excepciones",
                            "Debe de ingresar algún campo de búsqueda.",
                            "about")
            RETURN 1
         END IF
         ACCEPT CONSTRUCT
         
      ON ACTION CANCEL
         --LET INT_FLAG = 1
         RETURN 0
         EXIT CONSTRUCT
         
   END CONSTRUCT

   #Si en la seccion de parametros de busqueda se selecciono aceptar pinta las siguientes secciones
   --IF NOT INT_FLAG THEN
   
      #Se buscan los datos del cliente
      LET v_consulta_cliente =   "SELECT FIRST 51 ",
                                    "id_afi_fondo72, ",
                                    "nss, ",
                                    "rfc, ",
                                    "nombre ",
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
         -- Consulta el saldo para cada uno de los trabajadores del arreglo
         SELECT NVL(SUM(mov.importe),0)
         INTO v_lista_clientes[i].saldo
         FROM cta_fondo72 mov
         WHERE mov.id_afi_fondo72 = v_lista_clientes[i].id_afi_fondo72
         AND (mov.movimiento <> 422  AND #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
              mov.movimiento <> 601)     #Omitimos el ABONO RETIRO FONDO 72-92, TANTO ADICIONAL
         
         LET i = i + 1
         IF i > MAX_REGISTROS THEN
            CALL fn_mensaje("Captura Excepciones",
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
            IF v_rfc_filtro.getLength() >= 10 THEN  #RFC > 10
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
                                 "WHERE rfc[1,10] = ?" 
                  PREPARE exe_consulta_rfc FROM v_consulta_rfc
                  DECLARE cur_consulta_rfc CURSOR FOR exe_consulta_rfc
                  LET i = 1
                  FOREACH cur_consulta_rfc USING v_rfc10 INTO v_lista_clientes[i].*

                     --Consulta id_derechohabiente
                     SELECT id_derechohabiente 
                     INTO   v_id_dh_f72
                     FROM   afi_fondo72
                     WHERE  id_afi_fondo72 = v_lista_clientes[i].id_afi_fondo72

                     -- Consulta el saldo para cada uno de los trabajadores del arreglo
                     SELECT NVL(SUM(mov.importe),0)
                     INTO v_lista_clientes[i].saldo
                     FROM cta_fondo72 mov
                     WHERE mov.id_afi_fondo72 = v_lista_clientes[i].id_afi_fondo72
                     AND (mov.movimiento <> 422  AND #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                          mov.movimiento <> 601)     #Omitimos el ABONO RETIRO FONDO 72-92, TANTO ADICIONAL

                     LET i = i + 1
                     IF i > MAX_REGISTROS THEN
                        CALL fn_mensaje("Captura Excepciones",
                                        "Acotar mas el criterio de búsqueda. \n"||
                                        "Se muestran solo los primeros " || MAX_REGISTROS || " registros",
                                        "about")                                        
                        EXIT FOREACH
                     END IF
                  END FOREACH
                  CALL v_lista_clientes.deleteElement(v_lista_clientes.getLength())
                  CLOSE cur_consulta_rfc
                  FREE cur_consulta_rfc
                  IF i = 1 THEN
                     CALL fn_ventana_confirma("Atención",
                                              "¿Desea dar de alta el RFC en el catálogo de Afiliados del Fondo de Ahorro? \n",
                                              "quest") RETURNING v_respuesta
                     IF v_respuesta = 1 THEN #Se procede a la alta del RFC
                        CALL fn_alta_rfc(v_datos.rfc) RETURNING v_r_lista_clientes.*
                        IF v_r_lista_clientes.id_afi_fondo72 IS NOT NULL THEN 
                           LET v_lista_clientes[1].id_afi_fondo72  = v_r_lista_clientes.id_afi_fondo72
                           LET v_lista_clientes[1].nombre_completo = v_r_lista_clientes.nombre_completo
                           LET v_lista_clientes[1].nss             = v_r_lista_clientes.nss
                           LET v_lista_clientes[1].rfc             = v_r_lista_clientes.rfc
                           LET v_lista_clientes[1].saldo           = 0
                        ELSE 
                           CALL v_lista_clientes.clear()
                        END IF 
                     END IF   
                  END IF 
                ELSE 
                    CALL fn_ventana_confirma("Atención",
                        "¿Desea dar de alta el RFC en el catálogo de Afiliados del Fondo de Ahorro? \n","quest") RETURNING v_respuesta
                    IF v_respuesta = 1 THEN #Se procede a la alta del RFC
                        CALL fn_alta_rfc(v_datos.rfc) RETURNING v_r_lista_clientes.*
                        IF v_r_lista_clientes.id_afi_fondo72 IS NOT NULL THEN 
                            LET v_lista_clientes[1].id_afi_fondo72  = v_r_lista_clientes.id_afi_fondo72
                            LET v_lista_clientes[1].nombre_completo = v_r_lista_clientes.nombre_completo
                            LET v_lista_clientes[1].nss             = v_r_lista_clientes.nss
                            LET v_lista_clientes[1].rfc             = v_r_lista_clientes.rfc
                            LET v_lista_clientes[1].saldo           = 0
                        ELSE 
                            CALL v_lista_clientes.clear()
                        END IF 
                        
                    END IF   
                END IF   #FIN consulta con RFC10
            END IF   #FIN VAlidacion de tamaño para RFC
         END IF   #FIN RFC no nulo
      END IF   #FIN no se encontraron registros con el filtro original

      IF v_lista_clientes.getLength() > 0 THEN
         IF v_lista_clientes.getLength() < 1 THEN
            CALL fn_mensaje("Captura Excepciones", "No se encontro información con los parámetros proporcionados","about");
         ELSE
            #Si se encotro mas de un cliente con el filtro de busqueda se muestra la lista para que el usuario seleccione a un cliente
            CALL forma.setElementHidden("gr_busqueda",       VISIBLE)
            DIALOG ATTRIBUTES(UNBUFFERED)
               DISPLAY ARRAY v_lista_clientes TO scr_busqueda.*
               
               BEFORE ROW 
                  --LET INT_FLAG = FALSE
                  LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                  CALL fn_presenta_datos() RETURNING v_ciclo
                  
                  --Consulta id_derechohabiente
                  SELECT id_derechohabiente 
                  INTO   v_id_dh_f72
                  FROM   afi_fondo72
                  WHERE  id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                 
                  --CALL fn_credito_vivienda(v_lista_clientes[ARR_CURR()].id_afi_fondo72, 0)
                  CALL fn_credito_vivienda(v_id_dh_f72, 1)
                  RETURNING v_resultado,
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
                     LET v_tipo_credito_desc = "SIN CREDITO"    
                  END IF

                  DISPLAY v_tipo_credito_desc TO ed_tipo_credito
                  DISPLAY v_num_credito TO ed_numero_credito
                  DISPLAY v_f_otorga TO ed_fecha_otorga 
                  DISPLAY v_estado_credito TO ed_edo_credito

                  --Reviso si el registro actual tiene solicitud para mostrar carta finiquito
                  IF v_id_dh_f72 IS NULL THEN
                     --Doy por entendido que no existe solicitud
                     INITIALIZE v_id_sol_actual TO NULL
                     CALL DIALOG.setActionHidden("carta_finiquito",TRUE)
                  ELSE
                     --Busco la solicitud por id_derechohabiente,NSS y RFC
                     DISPLAY "Busca la solicitud con el id_derechohabiente = ", v_id_dh_f72
                     SELECT MAX(ab.id_solicitud)
                     INTO v_id_sol_actual
                     FROM ret_solicitud_generico ab, ret_sol_gen_excepcion bc
                     WHERE ab.id_derechohabiente = v_id_dh_f72
                     AND ab.id_solicitud         = bc.id_solicitud   --Join
                     AND ab.modalidad_retiro     = 2
                     -- AND ab.caso_adai            = 0
                     -- AND bc.estado_envio         = 1                 --Ya fue enviada por correo
                     AND estado_solicitud NOT IN (8,10)              --El estado de la solicitud sea 8 o 10
                     IF sqlca.sqlcode = NOTFOUND OR v_id_sol_actual IS NULL THEN
                        INITIALIZE v_id_sol_actual TO NULL
                        CALL DIALOG.setActionHidden("carta_finiquito",TRUE)
                     ELSE
                        DISPLAY "Se habilita el boton para generar la carta finiquito"
                        CALL DIALOG.setActionHidden("carta_finiquito",FALSE)
                     END IF
                  END IF
                  
               BEFORE DISPLAY 
                  CALL forma.setElementHidden("gr_resumen_saldo",VISIBLE)
                  CALL forma.setElementHidden("gr_movimientos",VISIBLE)
                  CALL forma.setElementHidden("gr_creditos",VISIBLE)
                  --CALL forma.setElementHidden("grp_causales",VISIBLE)

                  ON ACTION ACCEPT 
                     --LET INT_FLAG = FALSE
                     LET v_datos.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                     CALL fn_presenta_datos() RETURNING v_ciclo

                  --Se consultan los detalles históricos
                  ON ACTION historicos
                     CALL fn_consulta_det_hist_gral(v_lista_clientes[ARR_CURR()].nss,
                                                    v_lista_clientes[ARR_CURR()].rfc)
                     --Si no tiene movimientos  
                     IF v_i_dethist = 1 THEN 
                        CALL fn_mensaje("Atención", "No se encontraron movimientos históricos", "stop")
                        CALL arr_det_hist_general.clear() 
                     ELSE
                     --Muestra detalles si existen movimientos
                        CALL forma.setElementHidden("gr_det_historico",VISIBLE)
                        DISPLAY ARRAY arr_det_hist_general  TO scr_det_historicos.*
                        ATTRIBUTES(ACCEPT = FALSE)   
                           ON ACTION CANCEL  
                              CALL arr_det_hist_general.clear() 
                              CALL forma.setElementHidden("gr_det_historico",OCULTO)
                              EXIT DISPLAY 
                        END DISPLAY
                     END IF
                  
                  --Accion para generar Carta Finiquito
                  ON ACTION carta_finiquito
                     IF v_id_sol_actual IS NOT NULL THEN
                        -- Se valida que no sea de beneficiarios
                        SELECT COUNT(*)
                        INTO   v_num_beneficiarios
                        FROM   ret_beneficiario_generico
                        WHERE  id_solicitud = v_id_sol_actual
                        INITIALIZE v_reg_benef TO NULL
                        LET v_contador = 1
                        DECLARE cur_benef CURSOR FOR SELECT id_solicitud, consec_beneficiario
                                                     FROM   ret_beneficiario_generico
                                                     WHERE  id_solicitud = v_id_sol_actual
                        FOREACH cur_benef INTO v_reg_benef[v_contador].*
                            -- se procesan los reportes de cada beneficiario o del titular
                        
                           --Existe datos de solicitud actual por lo que muestro boton
                           --Genero y Muestro Carta Finiquito 
                           SELECT ab.id_derechohabiente, ab.f_solicitud
                             INTO v_id_der_actual,v_f_sol_actual
                             FROM ret_solicitud_generico ab,ret_sol_gen_excepcion bc
                            WHERE ab.id_solicitud = bc.id_solicitud
                              AND ab.id_solicitud = v_id_sol_actual

                           --Obtengo Causal
                           SELECT bc.causal_excepcion
                             INTO v_causal_excepcion
                             FROM ret_sol_gen_excepcion ac,ret_causal_excepcion bc
                            WHERE ac.causal_excepcion = bc.causal_excepcion
                              AND ac.id_solicitud = v_id_sol_actual

                           --Obtengo NSS, RFC y Nombre completo 
                           SELECT nss, rfc, nombre
                             INTO v_nss2, v_rfc2, v_nombre1
                             FROM afi_fondo72
                            WHERE id_derechohabiente = v_id_der_actual
                              AND rfc = v_lista_clientes[ARR_CURR()].rfc
                              AND nss = v_lista_clientes[ARR_CURR()].nss

                           --Obtengo el nombre del beneficiario
                           SELECT nombre
                             INTO v_beneficiario
                             FROM ret_beneficiario_generico
                            WHERE id_solicitud        = v_id_sol_actual
                            AND   consec_beneficiario = v_reg_benef[v_contador].v_consec_benef 

                           --Obtengo Monto normal y Monto adicional
                           SELECT a.saldo_viv72, a.tanto_adicional, b.importe, b.porcentaje
                             INTO v_saldo_viv72, v_monto_tanto_adicional, v_imp_pagar, v_por_benef
                             FROM ret_fondo_ahorro_generico a,
                                  ret_beneficiario_generico b
                            WHERE a.id_solicitud = v_id_sol_actual
                            AND   a.id_solicitud = b.id_solicitud
                            AND   b.consec_beneficiario = v_reg_benef[v_contador].v_consec_benef

                           --Generare Total a pagar
                           LET v_saldo_viv72           = v_saldo_viv72 * (v_por_benef/100)
                           LET v_monto_tanto_adicional = v_monto_tanto_adicional * (v_por_benef/100)
                           

                           --Obtengo CLABE
                           INITIALIZE v_clabe_bancaria TO NULL
                           SELECT cuenta_clabe
                             INTO v_clabe_bancaria
                             FROM ret_pago_spei
                            WHERE id_solicitud = v_id_sol_actual
                            AND   consec_beneficiario = v_reg_benef[v_contador].v_consec_benef

                           --Obtengo Referencia DAP
                           SELECT cve_referencia
                             INTO v_dap
                             FROM ret_pago_dap
                            WHERE id_solicitud = v_id_sol_actual
                            AND   consec_beneficiario = v_reg_benef[v_contador].v_consec_benef
                           DISPLAY "Los datos enviados para la generación del reporte son:"
                           DISPLAY "    v_id_sol_actual -----------", v_id_sol_actual
                           DISPLAY "    v_f_sol_actual ------------", v_f_sol_actual
                           DISPLAY "    v_causal_excepcion --------", v_causal_excepcion
                           DISPLAY "    v_nombre1 -----------------", v_nombre1
                           DISPLAY "    v_beneficiario ------------", v_beneficiario
                           DISPLAY "    v_rfc2 --------------------", v_rfc2
                           DISPLAY "    v_nss2 --------------------", v_nss2
                           DISPLAY "    v_saldo_viv72 -------------", v_saldo_viv72
                           DISPLAY "    v_monto_tanto_adicional ---", v_monto_tanto_adicional
                           DISPLAY "    v_imp_pagar ---------------", v_imp_pagar
                           DISPLAY "    v_dap ---------------------", v_dap
                           DISPLAY " El archivo generado es ", arr_archivo.v_archivo_1
                           
                           CALL fn_genera_reporte_acta_finiquito(v_id_sol_actual,
                                                                 v_f_sol_actual,
                                                                 v_causal_excepcion,
                                                                 v_nombre1,
                                                                 v_beneficiario,
                                                                 v_rfc2,
                                                                 v_nss2,
                                                                 v_saldo_viv72,
                                                                 v_monto_tanto_adicional,
                                                                 v_imp_pagar,
                                                                 v_dap,
                                                                 1,v_contador) 
                                                       RETURNING arr_archivo.v_archivo_1
                           LET v_contador = v_contador + 1
                        END FOREACH
                     END IF
                  --Se dan de alta aportaciones
                ON ACTION alta_aportaciones
                
                    CALL fn_alta_aportacion(v_datos.id_afi_fondo72,v_datos.rfc,v_datos.nss,v_datos.nombre_completo)
                    CALL fn_consulta_fondo()
                     -- Consulta el saldo nuevamente por si se dieron de alta movimientos
                     SELECT NVL(SUM(mov.importe),0)
                     INTO v_lista_clientes[ARR_CURR()].saldo
                     FROM cta_fondo72 mov
                     WHERE mov.id_afi_fondo72 = v_lista_clientes[ARR_CURR()].id_afi_fondo72
                     AND (mov.movimiento <> 422  AND #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                          mov.movimiento <> 601)     #Omitimos el ABONO RETIRO FONDO 72-92, TANTO ADICIONAL
                     DISPLAY v_lista_clientes[ARR_CURR()].saldo TO saldo
                --Se captura la solicitud
                ON ACTION Solicitud
                     --Muestra detalles si existen movimientos
                     CALL forma.setElementHidden("grp_causales",VISIBLE)
                     INPUT BY NAME 
                       cmb_causales,
                       ed_caso_crm,
                       monto_causal,
                       monto_tanto_adicional,
                       imp_pagar,
                       importe_beneficiario,
                       beneficiario,
                       ref_dap,
                       importe_beneficiario_dos,
                       beneficiario_dos,
                       ref_dap_dos,
                       importe_beneficiario_tres,
                       beneficiario_tres,
                       ref_dap_tres,
                       importe_beneficiario_cuatro,
                       beneficiario_cuatro,
                       ref_dap_cuatro,
                       importe_beneficiario_cinco,
                       beneficiario_cinco,
                       ref_dap_cinco
                      
                     WITHOUT DEFAULTS
                     ATTRIBUTES ( UNBUFFERED )
   
                     BEFORE INPUT
                         -- se verifica si el NSS/RFC ya tiene una solicitud en tramite
                         LET v_existe_solicitud = 0

                         LET importe_beneficiario        = 0
                         LET importe_beneficiario_dos    = 0
                         LET importe_beneficiario_tres   = 0
                         LET importe_beneficiario_cuatro = 0
                         LET importe_beneficiario_cinco  = 0

                         LET ref_dap        = 0
                         LET ref_dap_dos    = 0
                         LET ref_dap_tres   = 0
                         LET ref_dap_cuatro = 0
                         LET ref_dap_cinco  = 0

                         INITIALIZE beneficiario        TO NULL
                         INITIALIZE beneficiario_dos    TO NULL
                         INITIALIZE beneficiario_tres   TO NULL
                         INITIALIZE beneficiario_cuatro TO NULL
                         INITIALIZE beneficiario_cinco  TO NULL
                         INITIALIZE ed_caso_crm TO NULL

                         LET cmb_causales          = 0
                         LET monto_causal          = 0
                         LET monto_tanto_adicional = 0
                         LET imp_pagar             = 0
                         
                         SELECT id_solicitud         
                         INTO   v_existe_solicitud
                         FROM ret_solicitud_generico 
                         WHERE id_derechohabiente =  v_id_dh_f72
                         AND modalidad_retiro     =  2
                         -- precaptura, captura, aprobacion, preliq., liquid., enviada fico, conf. pago
                         -- rch fico, rechazo banco, cancelacion CxP, 
                         AND estado_solicitud IN (8,10,15,50,60,70,71,90,91,209,210,211,212,213,700)

                         IF v_existe_solicitud <> 0 THEN 
                             DISPLAY "Existe una solicitud Previa: ", v_existe_solicitud
                             CALL fn_mensaje("Atención", "Existe una solicitud en curso, no se puede continuar >" || v_existe_solicitud || "<", "stop")
                             EXIT INPUT  
                         END IF 
                         LET v_tpo_pago = 1

                         LET cbx_causal = ui.ComboBox.forName("cmb_causales")
                         CALL cbx_causal.clear()
                         
                         DECLARE  cur_causal CURSOR FOR
                         SELECT   *
                         FROM     ret_causal_excepcion
                         ORDER BY causal_excepcion
                         -- se agrega el valor nulo
                         CALL cbx_causal.addItem(NULL, "SELECCIONE UNA CAUSAL")
                         
                         -- para cada causal
                         FOREACH cur_causal INTO v_r_ret_causal_excepcion.*
                            -- se agregan las causales
                            LET v_cadena = v_r_ret_causal_excepcion.causal_excepcion || " - " || v_r_ret_causal_excepcion.desc_larga
                            CALL cbx_causal.addItem(v_r_ret_causal_excepcion.causal_excepcion, v_cadena)
                         END FOREACH
                         -- se inicia con todas las modalidades
                         LET cmb_causales = NULL
                         LET monto_causal = 0
                         LET monto_tanto_adicional = 0
                         LET imp_pagar = 0
                         LET beneficiario = ""
                         LET ed_caso_crm = ""
                         LET ref_dap = ""
                         LET v_mas_beneficiarios = 0
                         CALL forma.setElementHidden("lbl_ref_dap", VISIBLE) -- muestra etiqueta
                         CALL forma.setElementHidden("formonly.ref_dap", VISIBLE) -- muestra etiqueta
                         CALL forma.setElementHidden("lbl_importe_benefi", OCULTO) -- oculta etiqueta del campo importe
                         CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                           importe_beneficiario_dos, 
                                                                           importe_beneficiario_tres,
                                                                           importe_beneficiario_cuatro,
                                                                           importe_beneficiario_cinco
                         CALL dialog.setfieldactive("monto_causal",INACTIVO)
                         CALL dialog.setfieldactive("monto_tanto_adicional",INACTIVO)
                         CALL dialog.setfieldactive("imp_pagar",INACTIVO)
                              
                        ON CHANGE cmb_causales
                           CALL forma.setElementHidden("lbl_ref_dap", VISIBLE) -- muestra etiqueta
                           CALL forma.setElementHidden("formonly.ref_dap", VISIBLE) -- muestra campo 
                           DISPLAY "El monto" || v_saldo_total
                           CASE  
                              WHEN cmb_causales = 1 
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                                    importe_beneficiario_dos, 
                                                                                    importe_beneficiario_tres,
                                                                                    importe_beneficiario_cuatro,
                                                                                    importe_beneficiario_cinco
                                  DISPLAY "El monto causal 1" || v_saldo_total
                                  LET monto_causal = v_saldo_total
                                  LET monto_tanto_adicional = v_saldo_total
                                  LET imp_pagar = v_saldo_total * 2
                                  CALL dialog.setfieldactive("monto_causal",INACTIVO) 
                                  CALL dialog.setfieldactive("monto_tanto_adicional",INACTIVO) 
                                  CALL dialog.setfieldactive("imp_pagar",INACTIVO) 
                                  LET importe_beneficiario = imp_pagar
                                  #NEXT FIELD beneficiario
                                  NEXT FIELD ed_caso_crm
                              WHEN cmb_causales = 2 
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                                    importe_beneficiario_dos, 
                                                                                    importe_beneficiario_tres,
                                                                                    importe_beneficiario_cuatro,
                                                                                    importe_beneficiario_cinco
                                  DISPLAY "El monto causal 2" || v_saldo_total
                                  LET monto_causal = v_saldo_total
                                  LET monto_tanto_adicional = v_saldo_total
                                  LET imp_pagar = v_saldo_total * 2
                                  CALL dialog.setfieldactive("monto_causal",INACTIVO)
                                  CALL dialog.setfieldactive("monto_tanto_adicional",INACTIVO)
                                  CALL dialog.setfieldactive("imp_pagar",INACTIVO)
                                  LET importe_beneficiario = imp_pagar
                                  #NEXT FIELD beneficiario
                                  NEXT FIELD ed_caso_crm
                              WHEN cmb_causales = 3
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                                    importe_beneficiario_dos, 
                                                                                    importe_beneficiario_tres,
                                                                                    importe_beneficiario_cuatro,
                                                                                    importe_beneficiario_cinco
                                  IF v_saldo_total <= 0 THEN 
                                     CALL fn_mensaje("Comisión de Inconformidades", "No cuenta con saldo suficiente, no puede elegir esta causal", "stop") 
                                     LET cmb_causales = NULL
                                     NEXT FIELD cmb_causales  
                                  ELSE 
                                     DISPLAY "El monto causal 3" || v_saldo_total
--                                     LET monto_causal = v_saldo_total
--                                     LET monto_tanto_adicional = v_saldo_total
--                                     LET imp_pagar = v_saldo_total * 2
                                     LET monto_causal = 0
                                     LET monto_tanto_adicional = 0
                                     LET imp_pagar = 0
                                     CALL dialog.setfieldactive("monto_causal",ACTIVO)
                                     CALL dialog.setfieldactive("monto_tanto_adicional",ACTIVO)
                                     CALL dialog.setfieldactive("imp_pagar",INACTIVO)
                                     LET importe_beneficiario = imp_pagar
                                     
                                     #NEXT FIELD monto_tanto_adicional
                                     NEXT FIELD ed_caso_crm
                                  END IF 
                              WHEN cmb_causales = 4 
                                  DISPLAY "El monto causal 4" || v_saldo_total
                                  LET monto_causal = v_saldo_total
                                  LET monto_tanto_adicional = v_saldo_total
                                  LET imp_pagar = v_saldo_total * 2
                                  CALL dialog.setfieldactive("monto_causal",INACTIVO)
                                  CALL dialog.setfieldactive("monto_tanto_adicional",ACTIVO)
                                  CALL dialog.setfieldactive("imp_pagar",ACTIVO) 
                                  CALL fn_ventana_confirma("PROCESOS JUDICIALES","¿Desea capturar mas de un beneficiario?","quest") RETURNING v_respuesta; 
                                  IF (v_respuesta = 1) THEN 
                                      LET v_mas_beneficiarios = 0
                                      CALL fn_muestra_oculta_beneficiarios(VISIBLE) RETURNING importe_beneficiario, 
                                                      importe_beneficiario_dos, 
                                                      importe_beneficiario_tres,
                                                      importe_beneficiario_cuatro,
                                                      importe_beneficiario_cinco
                                  ELSE
                                      LET v_mas_beneficiarios = 1
                                  END IF 
                                  --LET importe_beneficiario = imp_pagar
                                  #NEXT FIELD monto_tanto_adicional
                                  NEXT FIELD ed_caso_crm
                              WHEN cmb_causales = 5 
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                      importe_beneficiario_dos, 
                                                      importe_beneficiario_tres,
                                                      importe_beneficiario_cuatro,
                                                      importe_beneficiario_cinco

                                 IF  v_datos.nss = '00000000000' OR v_datos.nss IS NULL THEN 
                                     DISPLAY "El monto causal 5" || v_saldo_total
                                     LET monto_causal = v_saldo_total
                                     LET monto_tanto_adicional = 0
                                     LET imp_pagar = v_saldo_total
                                     CALL dialog.setfieldactive("monto_causal",INACTIVO)
                                     CALL dialog.setfieldactive("monto_tanto_adicional",INACTIVO)
                                     CALL dialog.setfieldactive("imp_pagar",INACTIVO)
                                     LET importe_beneficiario = imp_pagar
                                     DISPLAY "El nombre a desplegar en el beneficiario es :", v_datos.nombre_completo
--                                     DISPLAY v_datos.nombre_completo TO beneficiario
                                     LET beneficiario = v_datos.nombre_completo
                                     CALL dialog.setfieldactive("beneficiario",INACTIVO)
                                     CALL dialog.setfieldactive("ref_dap",INACTIVO)
                                     #NEXT FIELD monto_tanto_adicional
                                     NEXT FIELD ed_caso_crm  
                                 ELSE
                                    CALL fn_mensaje("Atención", "No se puede elegir esta causal para el RFC/NSS proporcionado ", "stop") 
                                    LET cmb_causales = NULL
                                    NEXT FIELD cmb_causales
                                 END IF 
                              WHEN cmb_causales = 6 
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                                    importe_beneficiario_dos, 
                                                                                    importe_beneficiario_tres,
                                                                                    importe_beneficiario_cuatro,
                                                                                    importe_beneficiario_cinco
                              
                                 IF  v_datos.nss = '00000000000' OR v_datos.nss IS NULL THEN 
                                     DISPLAY "El monto causal 6" || v_saldo_total
                                     LET monto_causal = v_saldo_total
                                     LET monto_tanto_adicional = 0
                                     LET imp_pagar = v_saldo_total --* 2
                                     CALL dialog.setfieldactive("monto_causal",ACTIVO)
                                     CALL dialog.setfieldactive("monto_tanto_adicional",INACTIVO)
                                     CALL dialog.setfieldactive("imp_pagar",INACTIVO)
                                     LET importe_beneficiario = imp_pagar
                                     LET beneficiario = v_datos.nombre_completo
                                     CALL dialog.setfieldactive("beneficiario",INACTIVO)
                                     CALL dialog.setfieldactive("ref_dap",INACTIVO)
                                     
                                     #NEXT FIELD beneficiario
                                     NEXT FIELD ed_caso_crm
                                 ELSE
                                    CALL fn_mensaje("Atención", "No se puede elegir esta causal para el RFC/NSS proporcionado ", "stop") 
                                    LET cmb_causales = NULL
                                    NEXT FIELD cmb_causales
                                 END IF 
                              WHEN cmb_causales = 7 
                                  CALL fn_muestra_oculta_beneficiarios(OCULTO) RETURNING importe_beneficiario, 
                                                                                    importe_beneficiario_dos, 
                                                                                    importe_beneficiario_tres,
                                                                                    importe_beneficiario_cuatro,
                                                                                    importe_beneficiario_cinco
                              
                                  DISPLAY "El monto causal 7" || v_saldo_total
                                  LET monto_causal = v_saldo_total
                                  LET monto_tanto_adicional = v_saldo_total
                                  LET imp_pagar = v_saldo_total * 2
                                  CALL dialog.setfieldactive("monto_causal",ACTIVO)
                                  CALL dialog.setfieldactive("monto_tanto_adicional",ACTIVO)
                                  CALL dialog.setfieldactive("imp_pagar",INACTIVO) 
                                  
                                  NEXT FIELD ed_caso_crm
                              END CASE
                          ON CHANGE monto_causal
                              IF cmb_causales <> 3 THEN 
                                 LET monto_causal = v_saldo_total
                              ELSE 
                                 IF monto_causal >  v_saldo_total THEN 
                                    CALL fn_mensaje("Comisión de Inconformidades", "El monto normal no puede ser mayor al saldo", "stop") 
                                    LET monto_causal = 0
                                    NEXT FIELD monto_causal
                                 END IF 
                              END IF 
                          ON CHANGE monto_tanto_adicional
                              IF (cmb_causales = 1 OR cmb_causales = 2) THEN
                                  LET monto_tanto_adicional = v_saldo_total
                              END IF 
                              IF (monto_tanto_adicional > monto_causal ) THEN
                                 IF cmb_causales <> 4 AND cmb_causales <> 3 THEN 
                                     CALL fn_mensaje("Atención", "El tanto adicional no puede ser mayor al saldo", "stop") 
                                     LET monto_tanto_adicional = v_saldo_total
                                 END IF 
                              END IF  
                              IF cmb_causales = 3 THEN 
                                 IF monto_tanto_adicional + monto_causal > v_saldo_total THEN 
                                    CALL fn_mensaje("Comisión de Inconformidades", "El monto adicional no puede ser mayor al saldo", "stop") 
                                    LET monto_tanto_adicional = 0
                                    NEXT FIELD monto_tanto_adicional
                                 END IF 
                              END IF 
                              LET imp_pagar = monto_tanto_adicional + monto_causal
                              DISPLAY "montos despues del campo monto tanto adicional", imp_pagar, " v_saldo_total", v_saldo_total
                              --NEXT FIELD beneficiario
                          ON CHANGE imp_pagar
                              LET imp_pagar = monto_tanto_adicional + monto_causal
                          --ON CHANGE beneficiario
                          ON CHANGE ed_caso_crm
                              IF cmb_causales = 5 OR cmb_causales = 6 THEN 
                                 DISPLAY "Se detecta causal 5 o 6, se debe asignar el nombre del titular al del beneficiario :", v_datos.nombre_completo  
                                 LET beneficiario = v_datos.nombre_completo CLIPPED
                                 LET ref_dap = fn_obtiene_ref_dap()
                              ELSE
                                 NEXT FIELD beneficiario
                              END IF
                          AFTER FIELD beneficiario  
                            CALL forma.setElementHidden("lbl_ref_dap", VISIBLE) -- muestra etiqueta
                            CALL forma.setElementHidden("formonly.ref_dap", VISIBLE) -- muestra etiqueta
                            CALL dialog.setfieldactive("ref_dap",INACTIVO) 
                            LET v_tpo_pago = 2
                                                   
                            LET ref_dap = fn_obtiene_ref_dap()
                            --CALL dialog.setfieldactive("ref_dap",ACTIVO) 
                            --NEXT FIELD ref_dap 
                            IF v_mas_beneficiarios = 1 THEN 
                                NEXT FIELD importe_beneficiario
                            END IF
                          ON CHANGE ref_dap
                              IF ref_dap IS NOT NULL THEN 
                                  LET v_tpo_pago = 2
                              END IF 
                          ON CHANGE importe_beneficiario
                            CALL forma.setElementHidden("lbl_ref_dap", VISIBLE) -- muestra etiqueta
                            CALL forma.setElementHidden("formonly.ref_dap", VISIBLE) -- muestra etiqueta
                            LET ref_dap = fn_obtiene_ref_dap()
                            CALL dialog.setfieldactive("ref_dap",INACTIVO) 
                            IF (importe_beneficiario +
                                importe_beneficiario_dos +
                                importe_beneficiario_tres +
                                importe_beneficiario_cuatro +
                                importe_beneficiario_cinco) > imp_pagar THEN 
                               CALL fn_mensaje("Atención", "El monto no puede ser mayor al saldo", "stop") 
                               NEXT FIELD importe_beneficiario
                            ELSE 
                              NEXT FIELD beneficiario_dos
                            END IF 
                          ON CHANGE importe_beneficiario_dos
                            IF (importe_beneficiario +
                                importe_beneficiario_dos +
                                importe_beneficiario_tres +
                                importe_beneficiario_cuatro +
                                importe_beneficiario_cinco) > imp_pagar THEN 
                               CALL fn_mensaje("Atención", "El monto no puede ser mayor al saldo", "stop") 
                               NEXT FIELD importe_beneficiario_dos
                            ELSE 
                               CALL forma.setElementHidden("formonly.ref_dap_dos", VISIBLE) -- muestra etiqueta
                               LET ref_dap_dos = fn_obtiene_ref_dap()
                               CALL dialog.setfieldactive("ref_dap_dos",INACTIVO) 
                            END IF 
                          ON CHANGE importe_beneficiario_tres
                            IF (importe_beneficiario +
                                importe_beneficiario_dos +
                                importe_beneficiario_tres +
                                importe_beneficiario_cuatro +
                                importe_beneficiario_cinco) > imp_pagar THEN 
                               CALL fn_mensaje("Atención", "El monto no puede ser mayor al saldo", "stop") 
                               NEXT FIELD importe_beneficiario_tres
                            ELSE 
                               CALL forma.setElementHidden("formonly.ref_dap_tres", VISIBLE) -- muestra etiqueta
                               LET ref_dap_tres = fn_obtiene_ref_dap()
                               CALL dialog.setfieldactive("ref_dap_tres",INACTIVO) 
                            END IF 
                          ON CHANGE importe_beneficiario_cuatro
                            IF (importe_beneficiario +
                                importe_beneficiario_dos +
                                importe_beneficiario_tres +
                                importe_beneficiario_cuatro +
                                importe_beneficiario_cinco) > imp_pagar THEN 
                               CALL fn_mensaje("Atención", "El monto no puede ser mayor al saldo", "stop") 
                               NEXT FIELD importe_beneficiario_cuatro
                            ELSE 
                               CALL forma.setElementHidden("formonly.ref_dap_cuatro", VISIBLE) -- muestra etiqueta
                               LET ref_dap_cuatro = fn_obtiene_ref_dap()
                               CALL dialog.setfieldactive("ref_dap_cuatro",INACTIVO)
                            END IF 
                          ON CHANGE importe_beneficiario_cinco
                            IF (importe_beneficiario +
                                importe_beneficiario_dos +
                                importe_beneficiario_tres +
                                importe_beneficiario_cuatro +
                                importe_beneficiario_cinco) > imp_pagar THEN 
                               CALL fn_mensaje("Atención", "El monto no puede ser mayor al saldo", "stop") 
                               NEXT FIELD importe_beneficiario_cinco
                            ELSE 
                               CALL forma.setElementHidden("formonly.ref_dap_cinco", VISIBLE) -- muestra etiqueta
                               LET ref_dap_cinco = fn_obtiene_ref_dap()
                               CALL dialog.setfieldactive("ref_dap_cinco",INACTIVO)
                            END IF 
                          ON ACTION CANCEL
                             CALL forma.setElementHidden("grp_causales",OCULTO)
                             EXIT INPUT
                             
                          ON ACTION ACCEPT
                             --Reviso que se seleccione un causal
                             IF (cmb_causales IS NULL ) THEN 
                                 CALL fn_mensaje("Atención", "Debe seleccionar una causal", "stop") 
                                 NEXT FIELD cmb_causales
                             END IF
                             --Valido que el tanto adicional no sea nulo
                             IF monto_tanto_adicional IS NULL THEN
                                 CALL fn_mensaje("Atención", "El tanto adicional no puede ser nulo", "stop") 
                                 NEXT FIELD monto_tanto_adicional
                             END IF
                             --Valido que el tanto adicional no sea menor a cero
                             IF monto_tanto_adicional < 0 THEN
                                 CALL fn_mensaje("Atención", "El tanto adicional no puede ser menor a cero", "stop") 
                                 NEXT FIELD monto_tanto_adicional
                             END IF
                             --Valido que el tanto adicional no sea menor a cero
                             {IF beneficiario IS NULL THEN
                                 CALL fn_mensaje("Atención", "El beneficiario debe ser capturado", "stop") 
                                 NEXT FIELD beneficiario
                             END IF}
                             IF beneficiario IS NULL THEN
                                 --El nombre del beneficiario sera el nombre del titular
                                 SELECT nombre
                                 INTO v_nombre
                                 FROM afi_fondo72
                                 WHERE id_afi_fondo72 = v_datos.id_afi_fondo72
                                 IF sqlca.sqlcode = NOTFOUND THEN
                                    CALL fn_mensaje("Atención", "Imposible recuperar nombre del titular", "stop") 
                                    NEXT FIELD beneficiario
                                 ELSE
                                    IF v_nombre IS NOT NULL THEN
                                       LET beneficiario = v_nombre CLIPPED
                                    END IF
                                    DISPLAY BY NAME beneficiario
                                 END IF
                             END IF
                              IF ref_dap IS NULL THEN
                                 LET ref_dap = fn_obtiene_ref_dap()
                              END IF
                             DISPLAY "Validando los Importes"
                             IF (monto_causal + monto_tanto_adicional = 0) THEN 
                                 CALL fn_mensaje("Atención", "Los montos no pueden ser ceros", "stop") 
                                 NEXT FIELD cmb_causales
                             ELSE
                                IF cmb_causales = 3 THEN 
                                   LET imp_pagar = monto_causal + monto_tanto_adicional
                                   LET importe_beneficiario = imp_pagar
                                END IF 
                             END IF 
                             IF (cmb_causales = 1 OR cmb_causales = 2) THEN 
                                 IF (v_saldo_total = 0) THEN 
                                     CALL fn_mensaje("Atención", "La solicitud no procede porque el saldo debe ser mayor a cero", "stop") 
                                     NEXT FIELD cmb_causales
                                 END IF 
                                 --- Verifica si tiene resolucion de pension
                                 SELECT MAX(a.sec_pension)
                                 INTO   v_sec_pension
                                 FROM   ret_datamart a,                     
                                        ret_matriz_derecho b                
                                 WHERE  a.nss            = v_datos.nss
                                 AND    a.regimen        = b.regimen        
                                 AND    a.tpo_seguro     = b.tpo_seguro     
                                 AND    a.tpo_pension    = b.tpo_pension    
                                 AND    a.tpo_prestacion = b.tpo_prestacion 
                                 AND    a.tpo_prestacion <> "03";                        
            
                                 -- si se encontro un registro
                                 IF ( v_sec_pension IS NOT NULL ) THEN
                                     CALL fn_mensaje("Atención", "Existe una resolucion de pension, la solicitud no puede ser procesada", "stop") 
                                     NEXT FIELD cmb_causales
                                 END IF
                                 -- valida si tiene credito tradicional vigente
                                 IF ( v_sec_pension IS NOT NULL ) THEN
                                     CALL fn_mensaje("Atención", "Existe una resolucion de pension, la solicitud no puede ser procesada", "stop") 
                                     NEXT FIELD cmb_causales
                                 END IF
                                 IF (v_tpo_credito = 1 AND v_tpo_originacion = 1 AND v_estado_credito = "VIGENTE") THEN
                                     CALL fn_mensaje("Atención", "El derechohabiente tiene credito tradicional vigente, la solicitud no puede ser procesada", "stop") 
                                     NEXT FIELD cmb_causales
                                 END IF
                            END IF 
                            IF (cmb_causales = 4) THEN
                              IF importe_beneficiario + importe_beneficiario_dos + importe_beneficiario_tres + importe_beneficiario_cuatro + importe_beneficiario_cinco > imp_pagar THEN 
                                 CALL fn_mensaje("Atención", "La suma a pagar de los beneficiarios no puede ser mayor al importe a pagar", "stop") 
                                 NEXT FIELD importe_beneficiario
                              END IF 
                            END IF 
                            CALL fn_ventana_confirma("CONFIRMA SOLICITUD","Se guardara la solicitud, ¿Desea continuar?","quest") RETURNING v_respuesta; 
                            IF (v_respuesta = 1) THEN 

                                --Consulta id_derechohabiente
                                SELECT id_derechohabiente 
                                INTO   v_r_ret_solicitud_generico.id_derechohabiente
                                FROM   afi_fondo72
                                WHERE  id_afi_fondo72 = v_datos.id_afi_fondo72
                                IF (v_r_ret_solicitud_generico.id_derechohabiente IS NULL) THEN
                                    CALL fn_obtiene_id_derechohabiente(v_datos.nss, v_datos.rfc) RETURNING v_id_derechohabiente
                                    LET v_r_ret_solicitud_generico.id_derechohabiente = v_id_derechohabiente
                                    IF v_id_derechohabiente = 0 THEN 
                                        DISPLAY "ERROR CON EL ID_DERECHOHABIENTE: ", v_id_derechohabiente
                                        CALL fn_mensaje("Atención", "Problema con el id_derechohabiente, no se puede continuar >" || v_id_derechohabiente || "<", "stop")
                                        EXIT INPUT  
                                    END IF  
                                END IF 
                         
                                SELECT seq_ret_solicitud.NEXTVAL
                                INTO   v_r_ret_solicitud_generico.id_solicitud
                                FROM   systables 
                                WHERE  tabid = 1

                                --- Marca la cuenta
                                 LET v_r_ret_solicitud_generico.nss                    = v_datos.nss
                                 LET v_r_ret_solicitud_generico.rfc                    = v_datos.rfc
                                 LET v_r_ret_solicitud_generico.modalidad_retiro       = 2
                                 LET v_r_ret_solicitud_generico.folio                  = 0
                                 LET v_r_ret_solicitud_generico.caso_adai              = ed_caso_crm
                                 LET v_r_ret_solicitud_generico.id_archivo_envio       = 0
                                 LET v_r_ret_solicitud_generico.id_archivo_respuesta   = 0
                                 LET v_r_ret_solicitud_generico.folio_restitucion      = 0
                                 LET v_r_ret_solicitud_generico.id_archivo_cancela_cxp = 0
                                 LET v_r_ret_solicitud_generico.id_archivo_resp_cxp    = 0
--                                 LET v_r_ret_solicitud_generico.folio_afore            = NULL -- se usa en ventanilla afore
--                                 LET v_r_ret_solicitud_generico.grupo_ventanilla       = gi_ventanilla_infonavit -- solicitud iniciada en infonavit
                                 LET v_r_ret_solicitud_generico.f_solicitud            = TODAY
                                 LET v_r_ret_solicitud_generico.h_solicitud            = CURRENT HOUR TO SECOND
                                 LET v_r_ret_solicitud_generico.estado_solicitud       = 15
                                 LET v_r_ret_solicitud_generico.cod_rechazo            = 0

                                 #DISPLAY "Correre funcion fn_marca_cuenta:"
                                 #DISPLAY "Derechohabiente:",v_r_ret_solicitud_generico.id_derechohabiente
                                 #DISPLAY "Id solicitud:",v_r_ret_solicitud_generico.id_solicitud
                                 CALL fn_marca_cuenta(v_r_ret_solicitud_generico.id_derechohabiente,v_r_ret_solicitud_generico.id_solicitud) RETURNING v_respuesta_marcaje
                                 #DISPLAY "Respuesta:", v_respuesta_marcaje

                                 -- en caso de error se muestra que paso
                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL MARCAR: ", v_respuesta_marcaje
                                    #CALL fn_mensaje("Atención", "Problema con la marca >" || v_respuesta_marcaje || "<", "stop")
                                    CALL fn_mensaje("Atención", "Existe una solicitud previamente guardada para este NSS", "stop")
                                    EXIT INPUT  
                                 END IF

                                 -- se inserta el registro
                                 CALL fn_inserta_solicitud(v_r_ret_solicitud_generico.*) RETURNING v_respuesta_marcaje

                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR SOLICITUD: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con la solicitud >" || v_datos.nss || "< ", "stop")
                                    EXIT INPUT  
                                 END IF

                                 -- se inserta el medio de entrega
                                 CALL fn_inserta_sol_medio_entrega(v_r_ret_solicitud_generico.id_solicitud) RETURNING v_respuesta_marcaje

                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR EL MEDIO DE ENTREGA: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con EL MEDIO DE ENTEGA >" || v_datos.nss || "< ", "stop")
                                    EXIT INPUT  
                                 END IF

                                 -- se inserta la causal excepcion
                                 CALL fn_inserta_sol_causal(v_r_ret_solicitud_generico.id_solicitud, cmb_causales, ed_caso_crm, 0) RETURNING v_respuesta_marcaje

                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR LA CAUSAL DE LA SOLICITUD: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con la insercion de la causal >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                    EXIT INPUT  
                                 END IF


                                 -- Inserta en la tabla ret_fondo_ahorro_generico
                                 -- se asignan los datos al registro de fondo de ahorro generico
                                 LET v_r_ret_fondo_ahorro_generico.id_afi_fondo72 = v_datos.id_afi_fondo72
                                 LET v_r_ret_fondo_ahorro_generico.id_solicitud       = v_r_ret_solicitud_generico.id_solicitud
                                 LET v_r_ret_fondo_ahorro_generico.id_derechohabiente = v_r_ret_solicitud_generico.id_derechohabiente
                                 LET v_r_ret_fondo_ahorro_generico.causal_retiro      = 9
                                 LET v_r_ret_fondo_ahorro_generico.id_datamart        = NULL
                                 LET v_r_ret_fondo_ahorro_generico.f_ult_rel_laboral  = NULL
                                 LET v_r_ret_fondo_ahorro_generico.f_inicio_pension   = NULL 
                                 LET v_r_ret_fondo_ahorro_generico.nrp                = NULL
                                 LET v_r_ret_fondo_ahorro_generico.f_defuncion        = NULL
                                 LET v_r_ret_fondo_ahorro_generico.folio              = 0
                                 LET v_r_ret_fondo_ahorro_generico.saldo_viv72        = monto_causal
                                 LET v_r_ret_fondo_ahorro_generico.tanto_adicional    = monto_tanto_adicional
                                 LET v_r_ret_fondo_ahorro_generico.f_solicitud        = TODAY
                                 LET v_r_ret_fondo_ahorro_generico.f_captura          = TODAY
                                 LET v_r_ret_fondo_ahorro_generico.h_captura          = CURRENT HOUR TO MINUTE
                                 LET v_r_ret_fondo_ahorro_generico.estado_solicitud   = 15
                                 LET v_r_ret_fondo_ahorro_generico.cod_rechazo        = 0
	  
                                 -- se inserta la causal excepcion
                                 CALL fn_inserta_fondo_ahorro_gen(v_r_ret_fondo_ahorro_generico.* ) RETURNING v_respuesta_marcaje

                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR EN FONDO AHORRO GENERICO: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con la insercion en fondo ahorro generico de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                    EXIT INPUT  
                                 END IF

                                 LET v_consec_beneficiario = 1
                                 -- se asignan los datos al registro de beneficiario para el primer grupo de beneficiarios
                                 LET v_r_ret_beneficiario_generico.id_solicitud          = v_r_ret_solicitud_generico.id_solicitud
                                 LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario
                                 IF cmb_causales = 4 THEN 
                                    LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 2
                                 ELSE 
                                    LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 1
                                 END IF 
                                 LET v_r_ret_beneficiario_generico.tpo_pago              = v_tpo_pago
                                 LET v_r_ret_beneficiario_generico.cod_parentesco        = 1
                                 LET v_r_ret_beneficiario_generico.nombre                = beneficiario 
                                 LET v_r_ret_beneficiario_generico.porcentaje            = 100
                                 LET v_r_ret_beneficiario_generico.aivs                  = 0
                                 LET v_r_ret_beneficiario_generico.importe               = importe_beneficiario
                                 LET v_r_ret_beneficiario_generico.id_entidad_federativa = 9
                                 DISPLAY "El importe >", importe_beneficiario, "<"

                                 CALL fn_inserta_beneficiario(v_r_ret_beneficiario_generico.* ) RETURNING v_respuesta_marcaje

                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR EN BENEFICIARIOS: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con la insercion del beneficiario de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                    EXIT INPUT  
                                 END IF

                                 LET v_r_ret_pago_dap.id_solicitud        = v_r_ret_solicitud_generico.id_solicitud
                                 LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
                                 LET v_r_ret_pago_dap.tpo_pago            = v_tpo_pago
                                 LET v_r_ret_pago_dap.cve_referencia      = ref_dap
                                 CALL fn_inserta_dap(v_r_ret_pago_dap.* ) RETURNING v_respuesta_marcaje
                                 IF ( v_respuesta_marcaje <> 0 ) THEN
                                    DISPLAY "ERROR AL INSERTAR EN PAGO DAP: ", v_respuesta_marcaje
                                    CALL fn_mensaje("Atención", "Problema con la insercion del pago SPEI de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                    EXIT INPUT  
                                 END IF
                                 IF cmb_causales = 4 THEN 
                                    CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                                 v_r_ret_solicitud_generico.f_solicitud,
                                                 cmb_causales,
                                                 v_datos.nombre_completo,
                                                 beneficiario,
                                                 v_r_ret_solicitud_generico.rfc,
                                                 v_r_ret_solicitud_generico.nss,
                                                 importe_beneficiario/2,
                                                 importe_beneficiario/2,
                                                 importe_beneficiario,
                                                 ref_dap,
                                                 0,1)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_1
                                 ELSE 
                                    CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                                 v_r_ret_solicitud_generico.f_solicitud,
                                                 cmb_causales,
                                                 v_datos.nombre_completo,
                                                 beneficiario,
                                                 v_r_ret_solicitud_generico.rfc,
                                                 v_r_ret_solicitud_generico.nss,
                                                 monto_causal,
                                                 monto_tanto_adicional,
                                                 imp_pagar,
                                                 ref_dap,
                                                 0,1)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_1
                                 END IF 
                                 
                                 IF (beneficiario_dos IS NOT NULL AND (cmb_causales = 4)) THEN 
                                     LET v_consec_beneficiario = 2
                                 -- se asignan los datos al registro de beneficiario para el primer grupo de beneficiarios
                                     LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario
                                     LET v_r_ret_beneficiario_generico.nombre                = beneficiario_dos 
                                     LET v_r_ret_beneficiario_generico.importe               = importe_beneficiario_dos  
                                     LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 2

                                     CALL fn_inserta_beneficiario(v_r_ret_beneficiario_generico.* ) RETURNING v_respuesta_marcaje

                                     IF ( v_respuesta_marcaje <> 0 ) THEN
                                        DISPLAY "ERROR AL INSERTAR EN BENEFICIARIOS: ", v_respuesta_marcaje
                                        CALL fn_mensaje("Atención", "Problema con la insercion del segundo beneficiario de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                        EXIT INPUT  
                                     END IF

                                      LET v_r_ret_pago_dap.id_solicitud        = v_r_ret_solicitud_generico.id_solicitud
                                      LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
                                      LET v_r_ret_pago_dap.tpo_pago            = v_tpo_pago
                                      LET v_r_ret_pago_dap.cve_referencia      = ref_dap_dos
                                      CALL fn_inserta_dap(v_r_ret_pago_dap.* ) RETURNING v_respuesta_marcaje
                                      IF ( v_respuesta_marcaje <> 0 ) THEN
                                         DISPLAY "ERROR AL INSERTAR EN PAGO DAP: ", v_respuesta_marcaje
                                         CALL fn_mensaje("Atención", "Problema con la insercion del segundo pago DAP de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                         EXIT INPUT  
                                      END IF
                                      CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                              v_r_ret_solicitud_generico.f_solicitud,
                                              cmb_causales,
                                              v_datos.nombre_completo,
                                              beneficiario_dos,
                                              v_r_ret_solicitud_generico.rfc,
                                              v_r_ret_solicitud_generico.nss,
                                              importe_beneficiario_dos/2, --monto_causal,
                                              importe_beneficiario_dos/2, --monto_tanto_adicional,
                                              importe_beneficiario_dos,
                                              ref_dap_dos,
                                              0,2)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_2
                                 END IF 
                                 IF (beneficiario_tres IS NOT NULL AND (cmb_causales = 4)) THEN 
                                     LET v_consec_beneficiario = 3
                                 -- se asignan los datos al registro de beneficiario para el primer grupo de beneficiarios
                                     LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario
                                     LET v_r_ret_beneficiario_generico.nombre                = beneficiario_tres
                                     LET v_r_ret_beneficiario_generico.importe               = importe_beneficiario_tres  
                                     LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 2

                                     CALL fn_inserta_beneficiario(v_r_ret_beneficiario_generico.* ) RETURNING v_respuesta_marcaje

                                     IF ( v_respuesta_marcaje <> 0 ) THEN
                                        DISPLAY "ERROR AL INSERTAR EN BENEFICIARIOS: ", v_respuesta_marcaje
                                        CALL fn_mensaje("Atención", "Problema con la insercion del tercer beneficiario de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                        EXIT INPUT  
                                     END IF

                                      LET v_r_ret_pago_dap.id_solicitud        = v_r_ret_solicitud_generico.id_solicitud
                                      LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
                                      LET v_r_ret_pago_dap.tpo_pago            = v_tpo_pago
                                      LET v_r_ret_pago_dap.cve_referencia      = ref_dap_tres
                                      CALL fn_inserta_dap(v_r_ret_pago_dap.* ) RETURNING v_respuesta_marcaje
                                      IF ( v_respuesta_marcaje <> 0 ) THEN
                                         DISPLAY "ERROR AL INSERTAR EN PAGO DAP: ", v_respuesta_marcaje
                                         CALL fn_mensaje("Atención", "Problema con la insercion del tercer pago DAP de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                         EXIT INPUT  
                                      END IF
                                      CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                              v_r_ret_solicitud_generico.f_solicitud,
                                              cmb_causales,
                                              v_datos.nombre_completo,
                                              beneficiario_tres,
                                              v_r_ret_solicitud_generico.rfc,
                                              v_r_ret_solicitud_generico.nss,
                                              importe_beneficiario_tres/2, --monto_causal,
                                              importe_beneficiario_tres/2, --monto_tanto_adicional,
                                              importe_beneficiario_tres,
                                              ref_dap_tres,
                                              0,3)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_3
                                END IF 
                                 IF (beneficiario_cuatro IS NOT NULL AND (cmb_causales = 4)) THEN 
                                     LET v_consec_beneficiario = 4
                                 -- se asignan los datos al registro de beneficiario para el primer grupo de beneficiarios
                                     LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario
                                     LET v_r_ret_beneficiario_generico.nombre                = beneficiario_cuatro
                                     LET v_r_ret_beneficiario_generico.importe               = importe_beneficiario_cuatro  
                                     LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 2

                                     CALL fn_inserta_beneficiario(v_r_ret_beneficiario_generico.* ) RETURNING v_respuesta_marcaje

                                     IF ( v_respuesta_marcaje <> 0 ) THEN
                                        DISPLAY "ERROR AL INSERTAR EN BENEFICIARIOS: ", v_respuesta_marcaje
                                        CALL fn_mensaje("Atención", "Problema con la insercion del cuarto beneficiario de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                        EXIT INPUT  
                                     END IF

                                      LET v_r_ret_pago_dap.id_solicitud        = v_r_ret_solicitud_generico.id_solicitud
                                      LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
                                      LET v_r_ret_pago_dap.tpo_pago            = v_tpo_pago
                                      LET v_r_ret_pago_dap.cve_referencia      = ref_dap_cuatro
                                      CALL fn_inserta_dap(v_r_ret_pago_dap.* ) RETURNING v_respuesta_marcaje
                                      IF ( v_respuesta_marcaje <> 0 ) THEN
                                         DISPLAY "ERROR AL INSERTAR EN PAGO DAP: ", v_respuesta_marcaje
                                         CALL fn_mensaje("Atención", "Problema con la insercion del cuarto pago DAP de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                         EXIT INPUT  
                                      END IF
                                      CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                              v_r_ret_solicitud_generico.f_solicitud,
                                              cmb_causales,
                                              v_datos.nombre_completo,
                                              beneficiario_cuatro,
                                              v_r_ret_solicitud_generico.rfc,
                                              v_r_ret_solicitud_generico.nss,
                                              importe_beneficiario_cuatro/2, --monto_causal,
                                              importe_beneficiario_cuatro/2, --monto_tanto_adicional,
                                              importe_beneficiario_cuatro,
                                              ref_dap_cuatro,
                                              0,4)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_4
                                END IF 
                                 IF (beneficiario_cinco IS NOT NULL AND (cmb_causales = 4)) THEN 
                                     LET v_consec_beneficiario = 5
                                 -- se asignan los datos al registro de beneficiario para el primer grupo de beneficiarios
                                     LET v_r_ret_beneficiario_generico.consec_beneficiario   = v_consec_beneficiario
                                     LET v_r_ret_beneficiario_generico.nombre                = beneficiario_cinco
                                     LET v_r_ret_beneficiario_generico.importe               = importe_beneficiario_cinco  
                                     LET v_r_ret_beneficiario_generico.tpo_beneficiario      = 2

                                     CALL fn_inserta_beneficiario(v_r_ret_beneficiario_generico.* ) RETURNING v_respuesta_marcaje

                                     IF ( v_respuesta_marcaje <> 0 ) THEN
                                        DISPLAY "ERROR AL INSERTAR EN BENEFICIARIOS: ", v_respuesta_marcaje
                                        CALL fn_mensaje("Atención", "Problema con la insercion del quinto beneficiario de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                        EXIT INPUT  
                                     END IF

                                      LET v_r_ret_pago_dap.id_solicitud        = v_r_ret_solicitud_generico.id_solicitud
                                      LET v_r_ret_pago_dap.consec_beneficiario = v_consec_beneficiario -- consecutivo del beneficiario
                                      LET v_r_ret_pago_dap.tpo_pago            = v_tpo_pago
                                      LET v_r_ret_pago_dap.cve_referencia      = ref_dap_cinco
                                      CALL fn_inserta_dap(v_r_ret_pago_dap.* ) RETURNING v_respuesta_marcaje
                                      IF ( v_respuesta_marcaje <> 0 ) THEN
                                         DISPLAY "ERROR AL INSERTAR EN PAGO DAP: ", v_respuesta_marcaje
                                         CALL fn_mensaje("Atención", "Problema con la insercion del quinto pago DAP de la solicitud  >" || v_r_ret_solicitud_generico.id_solicitud || "< ", "stop")
                                         EXIT INPUT  
                                      END IF
                                      CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
                                              v_r_ret_solicitud_generico.f_solicitud,
                                              cmb_causales,
                                              v_datos.nombre_completo,
                                              beneficiario_cinco,
                                              v_r_ret_solicitud_generico.rfc,
                                              v_r_ret_solicitud_generico.nss,
                                              importe_beneficiario_cinco/2, --monto_causal,
                                              importe_beneficiario_cinco/2, --monto_tanto_adicional,
                                              importe_beneficiario_cinco,
                                              ref_dap_cinco,
                                              0,5)            --Bandera para que muestre en pantalla
                                       RETURNING arr_archivo.v_archivo_5
                                END IF 
                                CALL fn_mensaje("Atención", "Solicitud agregada satisfactoriamente", "stop") 

                                -- Finalmente muestro reporte para INFONAVIT
--                               CALL fn_genera_reporte_acta_finiquito(v_r_ret_solicitud_generico.id_solicitud,
--                                                                      v_r_ret_solicitud_generico.f_solicitud,
--                                                                      cmb_causales,
--                                                                      v_datos.nombre_completo,
--                                                                      beneficiario,
--                                                                      v_r_ret_solicitud_generico.rfc,
--                                                                      v_r_ret_solicitud_generico.nss,
--                                                                      monto_causal,
--                                                                      monto_tanto_adicional,
--                                                                      imp_pagar,
--                                                                      ref_dap,
--                                                                      0)            --Bandera para que muestre en pantalla
--                                                            RETURNING arr_archivo[1].v_archivo
                                 DISPLAY "Se envían las Actas Finiquito a CRM "
                                 CALL fn_load_pdf(arr_archivo.*,ed_caso_crm) 

                            END IF
                             ----- si es exitoso inserta en ret_solicitud_generico
                             ----- agregar en el catalogo de causales de retiro la de excepciones
                             -----
                             CALL forma.setElementHidden("grp_causales",OCULTO)
                             
                             EXIT INPUT  
        
                         ON ACTION regresar
                     END INPUT
                  ON ACTION cancelar 
                     INITIALIZE v_datos       TO NULL
                     EXIT DIALOG 
                    
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
   --ELSE
   --   RETURN 0
   --END IF
END FUNCTION

PRIVATE FUNCTION fn_presenta_datos()
   #primero se buscan los datos generales del cliente
   CALL fn_datos_generales()

   #Se ejecuta la funcion que consulta los movimientos de fondo 72
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
   SELECT 
          id_afi_fondo72,
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
   AND (mov.movimiento <> 422  #Omitimos el CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
        AND mov.movimiento <> 601) #Omitimos el ABONO RETIRO FONDO 72-92, TANTO ADICIONAL **** REQ 333 *** MODIFICO: Ricardo Perez 
   IF v_lista_fondo.getLength() IS NULL  OR v_lista_fondo.getLength() < 1 THEN
      LET v_saldo_total = 0.0
   END IF

   SELECT ind_estado_cuenta,
          f_estado_cuenta
   INTO   v_ind_estado_cuenta,
          v_f_estado_cuenta
   FROM   afi_fondo72
   WHERE  id_afi_fondo72 = v_datos.id_afi_fondo72

   IF v_ind_estado_cuenta = 0 THEN 
      LET v_desc_edo_cuenta = "0 - CUENTA ACTIVA"
   END IF 
   IF v_ind_estado_cuenta = 1 THEN 
      LET v_desc_edo_cuenta = "1 - CUENTA INACTIVA"
   END IF
END FUNCTION

#OBJETIVO: Consultar los movimientos históricos del NSS seleccionado
FUNCTION fn_consulta_det_hist_gral(p_nss, p_rfc)
DEFINE p_nss    CHAR(11), 
       p_rfc    CHAR(13),
       v_QryTxt STRING

--DISPLAY "ENTRA LA FUNCION DE CONSULTAR HISTORICOS", "  NSS   - ",p_nss, "   RFC   - ",p_rfc
       
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

   FOREACH cur_cons_historico INTO arr_det_hist_general[v_i_dethist].*  
      LET v_i_dethist = v_i_dethist + 1
   END FOREACH

   CALL arr_det_hist_general.deleteElement(v_i_dethist)

   --DISPLAY "Total de elementos encontrados en el historico", v_i_dethist
END FUNCTION

PRIVATE FUNCTION fn_inserta_solicitud(p_r_ret_solicitud_generico)
   DEFINE p_r_ret_solicitud_generico      RECORD LIKE ret_solicitud_generico.*   
   DEFINE v_respuesta                     INTEGER 

   DISPLAY "Solicitud ", p_r_ret_solicitud_generico.*
   INSERT INTO ret_solicitud_generico VALUES ( p_r_ret_solicitud_generico.* )
   
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_inserta_sol_medio_entrega(p_id_solicitud)
   DEFINE p_id_solicitud      LIKE ret_solicitud_generico.id_solicitud   
   DEFINE v_respuesta         INTEGER 
   DEFINE v_medio_entrega     SMALLINT

   DISPLAY "Inserta el medio de entrega", p_id_solicitud
   LET v_medio_entrega   = 6 --- EXCEPCIONES
   INSERT INTO ret_sol_medio_entrega VALUES (p_id_solicitud, 0, v_medio_entrega, '', CURRENT YEAR TO SECOND); 
   
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_marca_cuenta(p_id_derechohabiente, p_id_solicitud)
    DEFINE p_id_derechohabiente            INTEGER 
    DEFINE p_id_solicitud                  INTEGER 
    DEFINE v_respuesta_marcaje             SMALLINT
    DEFINE v_estado_marca                  SMALLINT 
    DEFINE v_codigo_rechazo                SMALLINT 
    DEFINE v_marca_causa                   SMALLINT
    DEFINE v_marca_entra                   SMALLINT  
    DEFINE v_fecha_causa                   DATE 
    DEFINE v_usuario                       CHAR(20)
    DEFINE v_proceso_cod                   SMALLINT 
    DEFINE v_folio                         SMALLINT 
    DEFINE v_sql                           STRING 

    LET v_marca_entra    = 802
    LET v_folio          = "0"
    LET v_estado_marca   = "0"
    LET v_codigo_rechazo = "0"
    LET v_marca_causa    = "0"
    LET v_fecha_causa    = NULL
    LET v_usuario        = g_usuario
    LET v_proceso_cod    = g_proceso_cod_ret_fondo_ahorro
     -- se prepara la ejecucion de la desmarca
    LET v_sql = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
                                    
    -- se prepara y ejecuta la funcion de marcaje    		                 
    PREPARE stm_marca_cta FROM v_sql
    EXECUTE stm_marca_cta USING p_id_derechohabiente ,
                              v_marca_entra          ,
                              p_id_solicitud         ,
                              v_estado_marca         ,
                              v_folio                ,
                              v_codigo_rechazo       ,
                              v_marca_causa          ,
                              v_fecha_causa          ,
                              v_usuario              ,
                              v_proceso_cod 
                        INTO v_respuesta_marcaje
     -- en caso de error se muestra que paso
    RETURN v_respuesta_marcaje 
END FUNCTION

PRIVATE FUNCTION fn_inserta_beneficiario(p_r_ret_beneficiario_generico)
   DEFINE p_r_ret_beneficiario_generico      RECORD LIKE ret_beneficiario_generico.*
   DEFINE v_respuesta                        INTEGER
   DEFINE v_total                            DECIMAL(22,2)       
   #Se ejecuta la funcion que consulta los movimientos de fondo 72

   --SELECT NVL(MAX(consec_beneficiario),0) + 1
   --INTO   p_r_ret_beneficiario_generico.consec_beneficiario
   --FROM   ret_beneficiario_generico
   --WHERE  id_solicitud = p_r_ret_beneficiario_generico.id_solicitud

   -- se inserta el beneficiario
   
   INSERT INTO ret_beneficiario_generico VALUES ( p_r_ret_beneficiario_generico.* )

   SELECT SUM(importe)
     INTO v_total
     FROM ret_beneficiario_generico
    WHERE id_solicitud = p_r_ret_beneficiario_generico.id_solicitud
   IF v_total <> 0 THEN 
       UPDATE ret_beneficiario_generico 
          SET porcentaje = (importe*100) / v_total 
        WHERE id_solicitud = p_r_ret_beneficiario_generico.id_solicitud
   END IF 
    
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION
{
======================================================================
Nombre: fn_cadena_nivel
Registro de modificaciones:
Autor           Fecha      Descrip. cambio
Luis Prieto     20150803   Se agrega el campo de EMail
======================================================================
}
PRIVATE FUNCTION fn_inserta_sol_causal(p_id_solicitud, p_causal, p_email, p_estado)
    DEFINE p_id_solicitud       INTEGER 
    DEFINE p_causal             SMALLINT
    DEFINE p_email              CHAR(50)
    DEFINE p_estado             SMALLINT 
    DEFINE v_respuesta          INTEGER 
    #Se ejecuta la funcion que consulta los movimientos de fondo 72

    INSERT INTO ret_sol_gen_excepcion VALUES (p_id_solicitud, p_causal, p_email, p_estado)
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_inserta_fondo_ahorro_gen(p_r_ret_fondo_ahorro_generico)
    DEFINE p_r_ret_fondo_ahorro_generico RECORD LIKE ret_fondo_ahorro_generico.*
    DEFINE v_respuesta                   INTEGER 
    #Se ejecuta la funcion que consulta los movimientos de fondo 72

    INSERT INTO ret_fondo_ahorro_generico VALUES ( p_r_ret_fondo_ahorro_generico.* )
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_inserta_spei(p_r_ret_pago_spei)
   DEFINE p_r_ret_pago_spei      RECORD LIKE ret_pago_spei.*   
   DEFINE v_respuesta            INTEGER 

   -- se inserta el beneficiario
   INSERT INTO ret_pago_spei VALUES ( p_r_ret_pago_spei.* )
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_inserta_dap(p_r_ret_pago_dap)
   DEFINE p_r_ret_pago_dap      RECORD LIKE ret_pago_dap.*   
   DEFINE v_respuesta           INTEGER 
   -- se inserta el beneficiario
   INSERT INTO ret_pago_dap VALUES ( p_r_ret_pago_dap.* )
   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_respuesta = 1   -- fallo la insercion
   ELSE
       -- Asigna respuesta afirmativa
       LET v_respuesta = 0
   END IF

   RETURN v_respuesta
END FUNCTION

PRIVATE FUNCTION fn_obtiene_id_derechohabiente(p_nss, p_rfc)
   DEFINE p_nss                 CHAR(11)
   DEFINE p_rfc                 CHAR(13)
   DEFINE v_id_derechohabiente  DECIMAL(9,0)
   DEFINE v_cuantos             DECIMAL(9,0)

   -- se obtiene el id de la secuencia             
   SELECT seq_derechohabiente.nextVal
   INTO   v_id_derechohabiente
   FROM   systables
   WHERE  tabid = 1;
            
   -- se asigna el id_derechohabiente a la tabla de relacion
   INSERT INTO afi_fondo72_d (nss, rfc, id_derechohabiente)
   VALUES (p_nss,p_rfc,v_id_derechohabiente);

   IF ( SQLCA.SQLCODE < 0 ) THEN
       -- Asigna respuesta negativa
       LET v_id_derechohabiente = 0   -- fallo la insercion
   ELSE 
       LET v_cuantos = 0
       SELECT COUNT(*)
       INTO   v_cuantos
       FROM   afi_fondo72
       WHERE  nss  = p_nss
       AND    rfc = p_rfc;
       IF v_cuantos = 1 THEN
           -- actulaiza la tabla afi_fondo72 con el id_derechohabiente obtenido
           UPDATE afi_fondo72 
           SET    id_derechohabiente = v_id_derechohabiente
           WHERE  nss  = p_nss
           AND    rfc  = p_rfc;
       END IF
   END IF

   RETURN v_id_derechohabiente
END FUNCTION
{
======================================================================
Nombre: fn_email_correcto
Fecha creacion: agosto 03, 2015
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
  Funcion que modifica un importe en número al mismo importe en letra con

Registro de modificaciones:
======================================================================
}
FUNCTION fn_email_correcto(p_email)
   DEFINE p_email       STRING
   DEFINE bnd_cadena    SMALLINT
   DEFINE i             SMALLINT
   --Borro espacios al final
   LET p_email = p_email.trim()
   --Reviso que el email no sea nulo
   IF p_email IS NULL THEN
      RETURN 1,p_email
   END IF
   --Reviso que el email tenga un simbolo de @
   LET bnd_cadena = 0
   FOR i = 1 TO p_email.getLength()
      --DISPLAY "Caracter: " || i || " : " || p_email.subString(i,i)
      IF p_email.subString(i,i) = "@" THEN
         LET bnd_cadena = 1
         EXIT FOR
      END IF
   END FOR
   --No encontre @ en la cadena
   IF NOT bnd_cadena THEN
      RETURN 2,p_email
   END IF
   --Reviso que el email tenga texto antes del simbolo @
   IF i = 1 THEN
      RETURN 3,p_email
   END IF
   --Reviso que el email tenga texto antes del simbolo @
   IF i = LENGTH(p_email) THEN
      RETURN 4,p_email
   END IF
   RETURN 0,p_email  --Email Correcto
END FUNCTION

FUNCTION fn_alta_rfc(p_rfc)
DEFINE p_rfc               CHAR(13)


DEFINE v_rec_datos_rfc   RECORD
      id_afi_fondo72       DECIMAL(9,0),
      nss                  CHAR(11),
      rfc                  CHAR(13),
      nombre_completo      VARCHAR(60),
      saldo                DECIMAL(10,2)
END RECORD
DEFINE v_nombre            CHAR(40)
DEFINE v_paterno           CHAR(40)
DEFINE v_materno           CHAR(40)
DEFINE v_respuesta         INTEGER 

    OPEN WINDOW w_alta_rfc WITH FORM "RETM2762"
      INPUT v_rec_datos_rfc.nss, v_nombre, v_paterno, v_materno
      FROM alta_nss, alta_nombre, alta_paterno, alta_materno
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE INPUT
        DISPLAY p_rfc TO alta_rfc
        LET v_rec_datos_rfc.rfc = p_rfc
        LET v_rec_datos_rfc.nombre_completo = v_nombre CLIPPED || " " || v_paterno CLIPPED || " " || v_materno CLIPPED
        IF (v_rec_datos_rfc.nombre_completo IS NULL OR v_rec_datos_rfc.nombre_completo CLIPPED = '') AND 
           (v_rec_datos_rfc.nss             IS NULL OR v_rec_datos_rfc.nss CLIPPED             = '') AND
           (v_rec_datos_rfc.rfc             IS NULL OR v_rec_datos_rfc.rfc CLIPPED             = '') THEN
            CALL fn_mensaje("Atención", "Los campos no puden estar vacios, debe capturar la información solicitada \n", "stop")
            INITIALIZE v_rec_datos_rfc TO NULL
            CONTINUE INPUT
        END IF 
      ON ACTION ACCEPT 
        -- Se valida que no esten los campos vacios
        DISPLAY p_rfc TO alta_rfc
        LET v_rec_datos_rfc.rfc = p_rfc
        LET v_rec_datos_rfc.nombre_completo = v_nombre CLIPPED || " " || v_paterno CLIPPED || " " || v_materno CLIPPED
        IF (v_rec_datos_rfc.nombre_completo IS NULL OR v_rec_datos_rfc.nombre_completo CLIPPED = '') AND 
           (v_rec_datos_rfc.nss             IS NULL OR v_rec_datos_rfc.nss CLIPPED             = '') AND
           (v_rec_datos_rfc.rfc             IS NULL OR v_rec_datos_rfc.rfc CLIPPED             = '') THEN
            CALL fn_mensaje("Atención", "Los campos no puden estar vacios, debe capturar la información solicitada \n", "stop")
            INITIALIZE v_rec_datos_rfc TO NULL
            CONTINUE INPUT
        ELSE 
            -- Valida el rfc, si no cuenta con homoclave la calcula para dar de alta el rfc a 13 posiciones
            CALL fn_valida_rfc(p_rfc, v_nombre, v_paterno, v_materno) RETURNING v_respuesta
            IF v_respuesta = 0 THEN 
                DISPLAY "el rfc original:",v_rec_datos_rfc.rfc
                DISPLAY "rfc a dar de alta:",v_datos.rfc
                LET v_rec_datos_rfc.rfc = v_datos.rfc
               CALL fn_ventana_confirma("Atención",
                   "Se dará de alta un nuevo registro en el catálogo de Afiliados del Fondo de Ahorro \n" ||
                   "¿Desea continuar? \n","quest") RETURNING v_respuesta
               IF v_respuesta = 1 THEN #Se procede a la alta del RFC
                   -- Obtiene el consecutivo para dar de alta en afi_fondo72
                   SELECT seq_afi_fondo72.nextval
                   INTO v_rec_datos_rfc.id_afi_fondo72
                   FROM   systables
                   WHERE tabid = 1
                   IF v_rec_datos_rfc.id_afi_fondo72 IS NOT NULL THEN
                       DISPLAY "Datos para dar de alta el registro"
                       DISPLAY "Id Afi. Fondo 72:", v_rec_datos_rfc.id_afi_fondo72
                       DISPLAY "RFC :", v_rec_datos_rfc.rfc
                       DISPLAY "NSS :", v_rec_datos_rfc.nss
                       DISPLAY "Nombre:", v_rec_datos_rfc.nombre_completo       
                       
                       INSERT INTO afi_fondo72
                            VALUES (v_rec_datos_rfc.id_afi_fondo72, v_rec_datos_rfc.rfc, v_rec_datos_rfc.nss, NULL, 
                                    v_rec_datos_rfc.nombre_completo, TODAY, 0, NULL);
                       CALL fn_mensaje("Atención", "Registro dado de alta exitosamente\n", "about")
                   END IF
               ELSE 
                   INITIALIZE v_rec_datos_rfc TO NULL
                   EXIT INPUT 
               END IF 
            ELSE 
               CALL fn_mensaje("Atención", "Existen inconsistencias en la información, no se procede con el alta \n", "stop")
               INITIALIZE v_rec_datos_rfc TO NULL
               CONTINUE INPUT
            END IF 
        END IF 
        EXIT INPUT 
    ON ACTION CANCEL
        INITIALIZE v_rec_datos_rfc TO NULL

        EXIT INPUT 
    END INPUT 
    CLOSE WINDOW w_alta_rfc
   
RETURN v_rec_datos_rfc.*
END FUNCTION 

FUNCTION fn_alta_aportacion(p_id_afi_fondo72, p_rfc, p_nss, p_nombre)
DEFINE p_id_afi_fondo72     DECIMAL(9,0)
DEFINE p_rfc                CHAR(13)
DEFINE p_nss                CHAR(11)
DEFINE p_nombre             CHAR(60)

DEFINE v_rec_datos_rfc    RECORD
      id_afi_fondo72        DECIMAL(9,0),
      nss                   CHAR(11),
      rfc                   CHAR(13),
      nombre_completo       VARCHAR(60),
      saldo                 DECIMAL(10,2)
END RECORD
DEFINE v_rec_aportaciones DYNAMIC ARRAY OF RECORD 
       v_anio               SMALLINT,
       v_expediente         CHAR(9),
       v_importe            DECIMAL(6,2),
       v_bimestre           DECIMAL(2,1),
       v_estado             CHAR(40)
END RECORD
DEFINE v_anio               SMALLINT
DEFINE v_expediente         CHAR(40)
DEFINE v_importe            DECIMAL(6,2)
DEFINE v_c_importe          CHAR(7)
DEFINE v_bimestre           DECIMAL(2,1)
DEFINE v_bim_string         CHAR(3)
DEFINE v_cantidad_registros SMALLINT
DEFINE i                    SMALLINT 
DEFINE v_continua           SMALLINT
DEFINE v_id_aport_fondo72   DECIMAL(9,0)

DEFINE v_respuesta          SMALLINT 
DEFINE v_mensaje            STRING
DEFINE v_fol_liq            DECIMAL(9,0)
DEFINE v_proceso            SMALLINT
DEFINE v_operacion          SMALLINT
DEFINE v_pid                INTEGER 
DEFINE v_movto_abono        SMALLINT
DEFINE v_error_validacion   SMALLINT 
DEFINE v_resultado          SMALLINT 

   OPEN WINDOW w_alta_aportacion WITH FORM "RETM2763"
      INPUT v_anio, v_expediente, v_c_importe, v_bimestre
      FROM aport_anio, aport_expediente, aport_importe, aport_bimestre
      ATTRIBUTES (UNBUFFERED, WITHOUT DEFAULTS, CANCEL=FALSE, ACCEPT=FALSE)

      BEFORE INPUT
         DISPLAY p_rfc TO aport_rfc
         DISPLAY p_nss TO aport_nss
         DISPLAY p_nombre TO aport_nombre
         LET v_movto_abono = 1941
         LET v_error_validacion = 0 
--        IF (v_anio       IS NULL OR v_anio               = 0)  AND 
--           (v_expediente IS NULL OR v_expediente CLIPPED = '') AND
--           (v_importe    IS NULL OR v_importe            = 0)  AND 
--           (v_bimestre   IS NULL OR v_bimestre           = 0)  THEN
--            CALL fn_mensaje("Atención", "Los campos no puden estar vacios, debe capturar la información solicitada \n", "stop")
--            CONTINUE INPUT
--        END IF 
         ON CHANGE aport_anio
            IF v_anio < 72 THEN 
               CALL fn_mensaje("Atención", "El año no puede ser menor a 72 \n", "stop")
               NEXT FIELD aport_anio
            END IF 
            IF v_anio > 92  THEN 
               CALL fn_mensaje("Atención", "El año no puede ser mayor a 92 \n", "stop")
               NEXT FIELD aport_anio
            END IF 
         ON CHANGE aport_bimestre
            LET v_bim_string = v_bimestre
            IF v_anio = 92 AND v_bimestre > 1.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre para el año 92 no puede ser mayor a 1.0\n", "stop")
               NEXT FIELD aport_bimestre
            END IF 
            IF v_anio = 72 AND v_bimestre < 3.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre para el año 72 no puede ser menor a 3.0\n", "stop")
               NEXT FIELD aport_bimestre
            END IF 
            IF v_bimestre > 6.5 THEN 
               CALL fn_mensaje("Atención", "El bimestre no puede ser mayor a 6.5\n", "stop")
               NEXT FIELD aport_bimestre
            END IF 
            IF v_bimestre < 1.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre no puede ser menor a 1.0\n", "stop")
               NEXT FIELD aport_bimestre
            END IF 
            IF v_bim_string[3,3] <> 0 AND v_bim_string[3,3] <> 5 THEN 
               CALL fn_mensaje("Atención", "La parte decimal del bimestre solo puede ser 0 ó 5\n", "stop")
               NEXT FIELD aport_bimestre
            END IF 
         ON CHANGE aport_importe
            CALL fn_valida_importe(v_c_importe) RETURNING v_resultado, v_mensaje, v_importe
            IF v_resultado <> 0 THEN 
               CALL fn_mensaje("Atención", v_mensaje, "stop")
               NEXT FIELD aport_importe
            ELSE 
               LET v_c_importe = v_importe
               DISPLAY v_importe TO aport_importe
            END IF 
            
            
         ON ACTION bton_registrar
            IF v_anio < 72 THEN 
               CALL fn_mensaje("Atención", "El año no puede ser menor a 72 \n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_anio > 92  THEN 
               CALL fn_mensaje("Atención", "El año no puede ser mayor a 92 \n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_anio = 92 AND v_bimestre > 1.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre para el año 92 no puede ser mayor a 1.0\n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_anio = 72 AND v_bimestre < 3.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre para el año 72 no puede ser menor a 3.0\n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_bimestre > 6.5 THEN 
               CALL fn_mensaje("Atención", "El bimestre no puede ser mayor a 6.5\n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_bimestre < 1.0 THEN 
               CALL fn_mensaje("Atención", "El bimestre no puede ser menor a 1.0\n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_bim_string[3,3] <> 0 AND v_bim_string[3,3] <> 5 THEN 
               CALL fn_mensaje("Atención", "La parte decimal del bimestre solo puede ser 0 ó 5\n", "stop")
               LET v_error_validacion = 1
            END IF 
            IF v_importe > 9999.99 THEN 
               CALL fn_mensaje("Atención", "El monto no puede ser mayor a una cantidad de 4 dígitos\n", "stop")
               NEXT FIELD aport_importe
            END IF 
            --- Valida si no se está duplicando la información en el arreglo
            LET v_cantidad_registros = v_rec_aportaciones.getLength()
            LET v_continua = 0
            IF v_anio       IS NOT NULL AND 
               v_expediente IS NOT NULL AND 
               v_importe    IS NOT NULL AND 
               v_bimestre   IS NOT NULL AND
               v_error_validacion = 0 THEN
               FOR i = 1 TO  v_cantidad_registros
                  IF v_rec_aportaciones[i].v_anio = v_anio AND 
                     v_rec_aportaciones[i].v_expediente = v_expediente AND 
                     v_rec_aportaciones[i].v_importe = v_importe AND 
                     v_rec_aportaciones[i].v_bimestre = v_bimestre THEN 
                     CALL fn_mensaje("Atención", "La información que intenta registrar ya se encuentra capturada, favor de verificar\n", "stop")
                     LET v_continua = 1
                     EXIT FOR 
                  END IF 
               END FOR 
               IF v_continua = 0 THEN
                  LET v_rec_aportaciones[v_cantidad_registros + 1].v_anio       = v_anio
                  LET v_rec_aportaciones[v_cantidad_registros + 1].v_expediente = v_expediente
                  LET v_rec_aportaciones[v_cantidad_registros + 1].v_importe    = v_importe
                  LET v_rec_aportaciones[v_cantidad_registros + 1].v_bimestre   = v_bimestre 
                  LET v_rec_aportaciones[v_cantidad_registros + 1].v_estado     = "Pendiente de Aplicar"
                  DISPLAY ARRAY v_rec_aportaciones TO tab_aportacion.* ATTRIBUTES (CANCEL=FALSE, ACCEPT=FALSE)
                  ON ACTION siguiente
                     DISPLAY "" TO aport_anio
                     LET v_anio = ""
                     DISPLAY "" TO aport_expediente
                     LET v_expediente = ""
                     DISPLAY "" TO aport_importe
                     LET v_importe = ""
                     DISPLAY "" TO aport_bimestre
                     LET v_bimestre = ""
                     LET v_bim_string = ""
                     EXIT DISPLAY 
                  ON ACTION aplicar
                     IF v_rec_aportaciones.getLength() > 0 THEN 
                        IF v_rec_aportaciones.getLength() = 1 THEN 
                           LET v_mensaje = "Se registrará ", v_rec_aportaciones.getLength(), " movimiento, ¿Desea continuar?"
                        ELSE 
                           LET v_mensaje = "Se registrarán ", v_rec_aportaciones.getLength(), " movimientos, ¿Desea continuar?"
                        END IF 
                        DISPLAY v_mensaje
                        CALL fn_ventana_confirma("Atención", v_mensaje CLIPPED, "quest") RETURNING v_respuesta
                        IF v_respuesta = 1 THEN
                           LET v_proceso = g_proceso_cod_aport_fondo_ahorro;
                           LET v_operacion = g_opera_ret_aport_fondo_ahorro;
                           DISPLAY "Validando el proceso y operacion :", v_proceso, "-", v_operacion
                           LET v_respuesta = fn_valida_operacion(0,v_proceso,v_operacion)
                           DISPLAY "Respuesta de la Validación del proceso y operación :", v_respuesta
                           IF ( v_respuesta = 0 ) THEN

                              -- se genera el pid 
                              DISPLAY "Generando el pid :", v_proceso, "-", v_operacion, "-", p_usuario
                              CALL fn_genera_pid(v_proceso, v_operacion, p_usuario) RETURNING v_pid
                              DISPLAY "Pid asignado :", v_pid
                              DISPLAY "Obteniendo el folio para la liquidación :", v_proceso, "-", v_operacion, "-", p_usuario
                              CALL fn_genera_folio(v_proceso, v_operacion,p_usuario) RETURNING v_fol_liq
                              DISPLAY "Folio obtenido :", v_fol_liq
                              DISPLAY "Inicialización del proceso :"
                              DISPLAY "       PID :", v_pid
                              DISPLAY "   PROCESO :", v_proceso
                              DISPLAY " OPERACIÓN :", v_operacion
                              DISPLAY "     FOLIO :", v_fol_liq
                              DISPLAY "  PROGRAMA : RETM276"
                              DISPLAY "   USUARIO :", p_usuario
                              CALL fn_inicializa_proceso(v_pid             ,
                                                         v_proceso         ,
                                                         v_operacion       ,
                                                         v_fol_liq         ,
                                                         "RETM276"         ,
                                                         "NA"                ,
                                                         p_usuario)  RETURNING v_respuesta
                                                                 
                              -- el proceso se registro correctamente
                              IF ( v_respuesta = 0 ) THEN

                                 -- inicia la operacion
                                 CALL fn_actualiza_opera_ini(v_pid,v_proceso,v_operacion,v_fol_liq,"RETM276","NA",p_usuario)
                                 RETURNING v_respuesta

                                 FOR i = 1 TO v_rec_aportaciones.getlength()
                                    IF v_rec_aportaciones[i].v_estado     = "Pendiente de Aplicar"  THEN 
                                       -- Se obtiene la secuencia para el id
                                       SELECT seq_ret_aport_fondo72.nextval 
                                       INTO   v_id_aport_fondo72
                                       FROM   systables 
                                       WHERE  tabid = 1
                                       
                                       INSERT INTO ret_aport_fondo72 (id_cta_aport_fondo72,
                                                                      nss,
                                                                      rfc,
                                                                      nombre,
                                                                      folio,
                                                                      ejercicio,
                                                                      clave_mov,
                                                                      empresa,
                                                                      bimestres,
                                                                      importe,
                                                                      estado_solicitud)
                                                              VALUES (v_id_aport_fondo72,
                                                                      p_nss,
                                                                      p_rfc,
                                                                      p_nombre,
                                                                      v_fol_liq,
                                                                      v_rec_aportaciones[i].v_anio,
                                                                      "AP",
                                                                      v_rec_aportaciones[i].v_expediente,
                                                                      v_rec_aportaciones[i].v_bimestre,
                                                                      v_rec_aportaciones[i].v_importe,
                                                                      60);
                                       INSERT INTO cta_fondo72 (id_afi_fondo72,
                                                                f_liquida,
                                                                subcuenta,
                                                                movimiento,
                                                                folio_liquida,
                                                                id_referencia,
                                                                importe,
                                                                f_registro,
                                                                h_registro,
                                                                origen) 
                                                        VALUES (p_id_afi_fondo72,
                                                                TODAY,
                                                                40,
                                                                v_movto_abono,
                                                                v_fol_liq,
                                                                v_id_aport_fondo72,
                                                                v_rec_aportaciones[i].v_importe,
                                                                TODAY,
                                                                CURRENT HOUR TO SECOND,
                                                                'AP PANTALLA');
                               
                                       LET v_rec_aportaciones[i].v_estado     = "Aplicado"
                                    END IF 
                                 END FOR
                                 LET v_mensaje = "Movimientos aplicados correctamente"
                                 DISPLAY v_mensaje
                                 CALL fn_mensaje("Atención", v_mensaje, "about")
                                 -- Función para finalizar la operación
                                 DISPLAY "parametros enviados a la función contable", v_fol_liq, "-", TODAY, "-", v_proceso
                                 CALL fn_aplica_contabilidad(v_fol_liq,TODAY, v_proceso) RETURNING v_respuesta, v_mensaje
                                 DISPLAY "Respuesta función contable :", v_respuesta, "-", v_mensaje
                                 IF v_respuesta = 0 THEN 
                                    DISPLAY "Se genera el reporte de liquidación", v_fol_liq, "-cta_fondo72-",p_usuario,"-",v_pid,"-",v_proceso,"-",v_operacion,"-RETM276"
                                    CALL fn_reporte_liquidacion(v_fol_liq, "cta_fondo72", p_usuario, v_pid, v_proceso, v_operacion, "RETM276", FALSE)
                                    DISPLAY "Reporte generado, Se cierra proceso"
                                    DISPLAY "Actualiza el fin de la operación :", v_pid,"-",v_proceso, "-", v_operacion
                                    CALL fn_actualiza_opera_fin(v_pid,v_proceso,v_operacion)
                                         RETURNING v_respuesta
                                    DISPLAY "Proceso terminado correctamente :", v_respuesta
                                 END IF 
                                 -- Cierra el pid
                                 EXIT DISPLAY
                              ELSE 
                                 CALL fn_mensaje("Atención", "No se pudo regristrar el proceso", "stop")
                              END IF
                           ELSE 
                              CALL fn_mensaje("Atención", "Validación de la operación no exitosa", "stop")
                           END IF
                        ELSE
                           CALL fn_mensaje("Atención", "Proceso cancelado", "about")
                        END IF 
                     ELSE 
                        CALL fn_mensaje("Atención", "No existen movimientos por aplicar", "stop")
                     END IF 
                     EXIT DISPLAY 
                  END DISPLAY 
               END IF 
            ELSE
               CALL fn_mensaje("Atención", "Falta información para realizar el registro de la aportación\n", "stop")
               LET v_error_validacion = 0 
            END IF 
            DISPLAY "" TO aport_anio
            LET v_anio = ""
            DISPLAY "" TO aport_expediente
            LET v_expediente = ""
            DISPLAY "" TO aport_importe
            LET v_importe = ""
            DISPLAY "" TO aport_bimestre
            LET v_bimestre = ""
            LET v_bim_string = ""
            NEXT FIELD aport_anio
         ON ACTION salir

            EXIT INPUT 
      END INPUT 
   CLOSE WINDOW w_alta_aportacion
   
END FUNCTION 

FUNCTION fn_aplica_contabilidad(p_folio, p_fecha_liquida, p_proceso_cod)
   DEFINE p_folio                 DECIMAL(9,0)
   DEFINE p_fecha_liquida         DATE
   DEFINE p_proceso_cod           INTEGER
   DEFINE v_cod_proceso_cnt       SMALLINT -- proceso cod de contabilidad
   DEFINE v_cod_transaccion_cnt   SMALLINT -- transaccion de contabilidad
   DEFINE v_si_resultado          SMALLINT
   DEFINE v_revisa_res            SMALLINT
   DEFINE v_cadena                STRING
   DEFINE v_mensaje_respuesta     STRING


-- Después de aplicar la liquidación.

   LET v_cod_proceso_cnt     = 116
   LET v_cod_transaccion_cnt = 0
   LET v_si_resultado        = 0

   -- se invoca el stored procedure
   LET v_cadena = "EXECUTE PROCEDURE fn_identifica_proceso_cnt(?,?,?,?,?)"
   PREPARE sid_identifica_proceso_cnt FROM v_cadena
   EXECUTE sid_identifica_proceso_cnt
   USING  p_folio              , -- Folio de liquidación del proceso
          p_fecha_liquida      , -- Fecha de liquidación del proceso
          v_cod_proceso_cnt    , -- Código de proceso contable
          p_proceso_cod        , -- Código Proceso
          v_cod_transaccion_cnt  -- codigo de transaccion
   INTO   v_si_resultado

   LET v_revisa_res = 0
   -- se verifica si LA EJECUCION FUE CORRECTA ( 1 = CORRECTO )
   IF ( v_si_resultado ) THEN
       -- se verifica el cuadre de montos Cargo vs Abono
       LET v_cadena = "EXECUTE PROCEDURE fn_revisa_reg_cnt(?,?,?)"
       PREPARE sid_fn_revisa_proceso_cnt FROM v_cadena
       EXECUTE sid_fn_revisa_proceso_cnt USING p_folio        , --Folio de liquidación del proceso
                                               p_fecha_liquida, --Fecha de liquidación del proceso
                                               v_revisa_res     --Tipo de Registro Contable 
       INTO v_si_resultado

       IF v_si_resultado = 0 THEN
          LET v_mensaje_respuesta = "El registro contable se realizó exitosamente."
          DISPLAY v_mensaje_respuesta
       ELSE
          IF v_si_resultado = 1 THEN
             LET v_mensaje_respuesta = "Error: No existe monto de abono en el registro contable."
             DISPLAY v_mensaje_respuesta
          END IF 
          IF v_si_resultado = 2 THEN
             LET v_mensaje_respuesta = "Error: No existe monto de cargo en el registro contable."
             DISPLAY v_mensaje_respuesta
          END IF 
          IF v_si_resultado = 3 THEN
             LET v_mensaje_respuesta = "Error: Diferencia de montos abono - cargo."
             DISPLAY v_mensaje_respuesta
           END IF 
       END IF 
    ELSE
       LET v_mensaje_respuesta = "ERROR: El registro contable no se pudo realizar."
       DISPLAY v_mensaje_respuesta
    END IF

    RETURN v_si_resultado, v_mensaje_respuesta

END FUNCTION 

FUNCTION fn_valida_rfc(p_rfc, p_nombre, p_paterno, p_materno)
DEFINE p_rfc               CHAR(13)
DEFINE p_nombre            CHAR(40)
DEFINE p_paterno           CHAR(40)
DEFINE p_materno           CHAR(40)

DEFINE v_rfc               CHAR(13)
DEFINE v_nombre            CHAR(40)
DEFINE v_paterno           CHAR(40)
DEFINE v_materno           CHAR(40)
DEFINE v_nombre_completo   CHAR(120)
DEFINE v_f_nacimiento      DATE
DEFINE v_sql               STRING
DEFINE v_anio              SMALLINT
DEFINE v_mensaje           STRING
DEFINE v_ocurrencias       SMALLINT 
DEFINE v_resultado_1       CHAR(45)
DEFINE v_resultado_2       CHAR(10)
DEFINE v_resultado_3       CHAR(10)
DEFINE v_resultado_4       CHAR(13)
DEFINE v_resultado_5       INTEGER
DEFINE v_resultado_6       CHAR(200)




DEFINE v_respuesta         INTEGER 


    DISPLAY "Validando información recibida"
    DISPLAY "RFC     :", p_rfc
    DISPLAY "Nombre  :", p_nombre
    DISPLAY "Paterno :", p_paterno
    DISPLAY "Materno :", p_materno
   -- se obtiene la fecha de nacimiento del RFC dado
   IF p_rfc IS NULL OR 
      p_nombre IS NULL OR 
      p_paterno IS NULL THEN 
      CALL fn_mensaje("Atención", "Sin información para validar el RFC \n", "stop")
      LET v_respuesta = 1
      LET v_rfc = ""
   ELSE 
      LET v_anio = p_rfc[5,6] + 1900
      IF  year(TODAY) - v_anio >= 100 THEN 
         LET v_anio = p_rfc[5,6] + 2000
      END IF 
      LET v_f_nacimiento = MDY(p_rfc[7,8],p_rfc[9,10],v_anio)
      -- se obtiene el RFC calculado por la función
      LET v_sql = "EXECUTE FUNCTION fn_valida_rfc(?,?,?,?)"

      DISPLAY "Envia a la función de validación del RFC"
      -- se prepara y ejecuta la funcion de marcaje    		                 
      PREPARE stm_valida_rfc FROM v_sql
      EXECUTE stm_valida_rfc USING p_rfc     ,
                                   p_paterno ,
                                   p_materno ,
                                   p_nombre
                              INTO v_respuesta, v_resultado_5, v_rfc, v_resultado_6
        DISPLAY "Resultado de la validación del RFC", v_respuesta
      IF v_respuesta = 0 THEN
        DISPLAY "El rfc calculado es :", v_rfc
        DISPLAY "El rfc enviado es   :", p_rfc
         IF v_rfc <> p_rfc THEN 
            -- se valida si ya existe el RFC calculado en el catálogo de Afiliados del Fondo de Ahorro
            SELECT COUNT(*)
            INTO   v_ocurrencias
            FROM   afi_fondo72
            WHERE  rfc = v_rfc
            DISPLAY "Valida si existe el recien calculado :", v_ocurrencias
            IF v_ocurrencias = 0 THEN 
               LET v_mensaje = "El RFC proporcionado difiere del calcuclado\n",
                               "RFC proporcionado:",p_rfc,"\n",
                               "RFC calculado    :",v_rfc,"\n",
                               "¿Desea continuar con el alta utilizando el RFC calculado? \n"
               CALL fn_ventana_confirma("Atención", v_mensaje, "quest") RETURNING v_respuesta
               IF v_respuesta = 1 THEN 
                    IF LENGTH(v_rfc CLIPPED) = 10 THEN 
                        DISPLAY "El RFC es de 10 posiciones, se obtendrá la homoclave"
                        --- se obtiene la homoclave
                        LET v_sql = "EXECUTE FUNCTION sp_calcula_homoclave_rfc(?,?)"
                        LET v_nombre_completo = p_nombre CLIPPED, " ", p_paterno CLIPPED, " ", p_materno CLIPPED
                                                    
                        -- se prepara y ejecuta la funcion de marcaje    		                 
                        PREPARE stm_obtiene_homoclave FROM v_sql
                        EXECUTE stm_obtiene_homoclave USING v_nombre_completo, v_rfc
                                                       INTO v_respuesta, 
                                                            v_resultado_1,
                                                            v_resultado_2,
                                                            v_resultado_3,
                                                            v_resultado_4
                        DISPLAY "Regreso del calculo de la homoclave :", v_respuesta, " RFC con Homoclave ", v_resultado_4
                        IF v_respuesta = 0 THEN
                            -- Valida que no exista el calculado con la homoclave
                            LET v_rfc = v_resultado_4
                            SELECT COUNT(*)
                            INTO   v_ocurrencias
                            FROM   afi_fondo72
                            WHERE  rfc = v_rfc
                            DISPLAY "Valida que no exista ", v_ocurrencias
                            IF v_ocurrencias = 0 THEN 
                                LET v_datos.rfc = v_rfc
                                LET v_respuesta = 0
                            ELSE
                                LET v_respuesta = 2
                            END IF 
                        ELSE 
                            CALL fn_mensaje("Atención", "Problemas con el cálculo de la homoclave, no se puede continuar\n", "stop")
                            LET v_respuesta = 2
                        END IF 
                    ELSE 
                        LET v_datos.rfc = v_rfc
                        LET v_respuesta = 0  -- Continuar con el alta
                    END IF 
               ELSE 
                  LET v_respuesta = 2 -- No dar de alta
               END IF
            ELSE 
               CALL fn_mensaje("Atención", "El RFC ya existe en el catálogo de Afiliados del Fondo de Ahorro, no procede el alta \n", "stop")
               LET v_respuesta = 2
            END IF 
         ELSE -- RFC igual al calculado
            -- Se valida que tenga Homoclave, si no se calcula para dar de alta el RFC con Homoclave.
            DISPLAY "Si no tiene homoclave la obtiene" 
            IF LENGTH(p_rfc CLIPPED) = 10 THEN 
                LET v_sql = "EXECUTE FUNCTION sp_calcula_homoclave_rfc(?,?)"
                LET v_nombre_completo = p_nombre CLIPPED, " ", p_paterno CLIPPED, " ", p_materno CLIPPED
                                            
                -- se prepara y ejecuta la funcion de marcaje    		                 
                PREPARE stm_valida_homoclave FROM v_sql
                EXECUTE stm_valida_homoclave USING v_nombre_completo, p_rfc
                                              INTO v_respuesta, 
                                                   v_resultado_1,
                                                   v_resultado_2,
                                                   v_resultado_3,
                                                   v_resultado_4
                DISPLAY "Se obtuvo la homoclave"
                IF v_respuesta = 0 THEN 
                    -- Valida que no exista el calculado con la homoclave
                    DISPLAY "Se valida que no exista a 10"
                    LET v_rfc = v_resultado_4
                    SELECT COUNT(*)
                    INTO   v_ocurrencias
                    FROM   afi_fondo72
                    WHERE  rfc = v_rfc
                    IF v_ocurrencias = 0 THEN 
                        LET v_datos.rfc = v_rfc
                        LET v_respuesta = 0
                    ELSE
                        LET v_respuesta = 2
                    END IF 
                    
                ELSE 
                    CALL fn_mensaje("Atención", "Problemas con el cálculo de la homoclave, no se puede continuar\n", "stop")
                    LET v_respuesta = 2
                END IF 
            ELSE 
                LET v_datos.rfc = v_rfc
                LET v_respuesta = 0  -- Continuar con el alta
            END IF 
            --LET v_respuesta = 0
         END IF 
            
      ELSE
         CALL fn_mensaje("Atención", "No se puede continuar, existen problemas al validar el RFC \n", "stop")
         LET v_respuesta = 2
      END IF 
   END IF 

RETURN v_respuesta

END FUNCTION 
FUNCTION fn_valida_importe(p_c_importe)
DEFINE p_c_importe       CHAR(20)
DEFINE v_importe         DECIMAL(14,2)
DEFINE v_resultado       SMALLINT
DEFINE v_imp_paso        CHAR(20)
DEFINE i                 SMALLINT
DEFINE j                 SMALLINT 
DEFINE v_c               CHAR(1)
DEFINE v_considera       SMALLINT
DEFINE v_mensaje         STRING
DEFINE v_c_importe       CHAR(7)
DEFINE v_encontro_digito SMALLINT
DEFINE v_long_cadena     SMALLINT
DEFINE v_importe_final   DECIMAL(6,2)

   LET j                 = 0
   LET v_considera       = 0
   LET v_encontro_digito = 0
   LET v_resultado       = 0
   LET v_mensaje         = ""
   LET v_long_cadena     = 0
   LET v_imp_paso        = ""

   
   IF LENGTH(p_c_importe) = 0 THEN 
      LET v_considera = 1
      LET v_mensaje   = "El campo no pupede estar vacío \n"
      LET v_resultado = 1
      LET v_importe   = 0
   END IF 
   LET v_long_cadena = LENGTH(p_c_importe CLIPPED)
   DISPLAY "La longitud de la cadena :", LENGTH(p_c_importe CLIPPED)
   FOR i = 1 TO v_long_cadena
      DISPLAY "El índice :", i
      LET  v_c = p_c_importe[i,i]
      DISPLAY " El caracter obtenido :", v_c
      IF v_c = "1" OR v_c = "2" OR v_c = "3" OR v_c = "4" OR v_c = "5" OR v_c = "6" OR 
         v_c = "7" OR v_c = "8" OR v_c = "9" OR v_c = "0" OR v_c = "." THEN 
            LET v_considera = 0
            LET v_imp_paso = v_imp_paso CLIPPED, v_c
      ELSE 
            DISPLAY "El caracter no válido es ", v_c
            LET v_considera = 1
            LET v_mensaje   = "La cadena solo puede contener números \n" 
            LET v_importe   = 0
            LET v_resultado = 1
            EXIT FOR
      END IF
   END FOR 
   DISPLAY "Termino el Ciclo y la cadena resultante es: ",v_imp_paso, " Considerada ", v_considera

   IF v_considera = 0 THEN
      LET v_long_cadena = LENGTH(v_imp_paso CLIPPED)
      FOR i = 1 TO v_long_cadena
         LET  v_c = v_imp_paso[i,i]
         IF v_encontro_digito = 0 THEN
            IF v_c <> '0' THEN 
               LET v_encontro_digito = 1
            END IF 
         END IF 
         IF v_encontro_digito = 1 THEN
            LET v_c_importe = v_c_importe CLIPPED, v_c
            IF v_c = '.' OR j > 0 THEN 
               LET j = j + 1
            END IF
            IF j > 2 THEN 
               EXIT FOR
            END IF 
         END IF 
      END FOR 
      LET v_importe = v_c_importe
   END IF 
   DISPLAY "El importe en cadena es ", v_c_importe, " el importe en decimal es :", v_importe
   LET v_importe_final = 0 
   IF v_importe > 9999.99 THEN 
      LET v_mensaje = "El importe no puede ser mayor a 9999.99 \n"
      LET v_resultado = 1
      LET v_importe = 0
   ELSE 
      LET v_importe_final = v_importe
   END IF 

RETURN v_resultado, v_mensaje, v_importe_final
END FUNCTION

FUNCTION fn_obtiene_ref_dap()
DEFINE v_referencia_dap  CHAR(12)
DEFINE v_consecutivo_ref DECIMAL(5,0)
DEFINE v_anio_fecha      CHAR(4)

   SELECT seq_ret_ref_bancaria72.nextVal
   INTO   v_consecutivo_ref
   FROM   systables
   WHERE  tabid = 1

   LET v_anio_fecha = YEAR(TODAY)
   LET v_referencia_dap = "50",
                        v_anio_fecha[3,4],
                        "729",
                        v_consecutivo_ref USING "&&&&&"
                        

                        
RETURN v_referencia_dap

END FUNCTION

FUNCTION fn_muestra_oculta_beneficiarios(p_bandera)
DEFINE p_bandera SMALLINT 

   CALL forma.setElementHidden("formonly.beneficiario_dos", p_bandera) -- muestra el campo CLBE BANCARIA
   CALL forma.setElementHidden("formonly.ref_dap_dos", p_bandera) -- muestra etiqueta
   CALL forma.setElementHidden("formonly.importe_beneficiario_dos", p_bandera) -- oculta el campo importe
   CALL forma.setElementHidden("formonly.beneficiario_tres", p_bandera) -- muestra el campo CLBE BANCARIA
   CALL forma.setElementHidden("formonly.ref_dap_tres", p_bandera) -- muestra etiqueta
   CALL forma.setElementHidden("formonly.importe_beneficiario_tres", p_bandera) -- oculta el campo importe
   CALL forma.setElementHidden("formonly.beneficiario_cuatro", p_bandera) -- muestra el campo CLBE BANCARIA
   CALL forma.setElementHidden("formonly.ref_dap_cuatro", p_bandera) -- muestra etiqueta
   CALL forma.setElementHidden("formonly.importe_beneficiario_cuatro", p_bandera) -- oculta el campo importe
   CALL forma.setElementHidden("formonly.beneficiario_cinco", p_bandera) -- muestra el campo CLBE BANCARIA
   CALL forma.setElementHidden("formonly.ref_dap_cinco", p_bandera) -- muestra etiqueta
   CALL forma.setElementHidden("formonly.importe_beneficiario_cinco", p_bandera) -- oculta el campo importe
   CALL forma.setElementHidden("lbl_num_2", p_bandera) -- muestra etiqueta del renglon de beneficiarios
   CALL forma.setElementHidden("lbl_num_3", p_bandera) -- muestra etiqueta del renglon de beneficiarios
   CALL forma.setElementHidden("lbl_num_4", p_bandera) -- muestra etiqueta del renglon de beneficiarios
   CALL forma.setElementHidden("lbl_num_5", p_bandera) -- muestra etiqueta del renglon de beneficiarios
   CALL forma.setElementHidden("lbl_importe_benefi", p_bandera) -- oculta etiqueta del campo importe
   CALL forma.setElementHidden("formonly.importe_beneficiario", p_bandera) -- oculta el campo importe

   RETURN 0,0,0,0,0
   
END FUNCTION 