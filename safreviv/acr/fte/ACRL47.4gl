--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>ACR                                           #
#Programa          =>ACRL47                                        #
#Objetivo          =>Programa que realiza la consulta de           #
#                    Verificación remanentes                       #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>20 Agosto 2013                                #
####################################################################

IMPORT OS
DATABASE safre_viv

DEFINE m_c_usuario_cod   LIKE seg_usuario.usuario_cod -- clave del usuario firmado

MAIN

   DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
          p_s_titulo       STRING, -- titulo de la ventana
          v_b_salir        SMALLINT -- bandera que indica si se debe o no mostrar la ventana principal

   -- se asignan los valores que vienen como parametros
   LET m_c_usuario_cod  = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se inicializan variables
   LET v_b_salir = TRUE

   -- se crea el archivo log
   CALL STARTLOG(m_c_usuario_cod CLIPPED|| ".ACRL47.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   WHILE ( v_b_salir )
      -- se invoca la función que realiza la consulta por derechohabiente
      CALL fn_consulta_nss() RETURNING v_b_salir

      IF v_b_salir = FALSE THEN
         EXIT WHILE
      END IF
   END WHILE
END MAIN

#Objetivo: Función que realiza la consulta por NSS
FUNCTION fn_consulta_nss()

   DEFINE v_s_condicion           STRING,
          v_nss                   LIKE afi_derechohabiente.nss,
          v_i_cuenta_regs         INTEGER

   DEFINE v_r_afi_derechohabiente RECORD
             id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
             nss                  LIKE afi_derechohabiente.nss,
             curp                 LIKE afi_derechohabiente.curp,
             rfc                  LIKE afi_derechohabiente.rfc,
             ap_paterno_af        LIKE afi_derechohabiente.ap_paterno_af,
             ap_materno_af        LIKE afi_derechohabiente.ap_materno_af,
             nombre_af            LIKE afi_derechohabiente.nombre_af
   END RECORD

   DEFINE v_si_cuenta_reg         SMALLINT,
          v_s_mensaje             STRING,
          v_s_qryTxt              STRING,
          v_b_continua            SMALLINT,
          r_d_id_cre_acreditado   LIKE cre_acreditado.id_cre_acreditado

   -- se inicializan varibles
   LET v_s_condicion = "1=1"
   LET v_b_continua = TRUE -- se indica que no debe salir completamente de la ventana

   OPEN WINDOW w_op_derechohab WITH FORM "ACRL471"
   CONSTRUCT v_s_condicion ON nss FROM nss
      BEFORE CONSTRUCT
         -- se obtiene el valor de la accion
         SELECT COUNT(*)
           INTO v_si_cuenta_reg
           FROM glo_valor_fondo
          WHERE fondo = 11
            AND f_valuacion = TODAY;

         -- se valida si existe precio de acción para el día de hoy
         IF v_si_cuenta_reg = 0 THEN
            LET v_s_mensaje = "No existe el precio de acción para el día de hoy.\n",
                              "No es posible continuar con la verificación de remanente."
            CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

            -- se indica que no debe continuar con la verificación de remanente
            LET v_b_continua = FALSE

            EXIT CONSTRUCT
         END IF

      ON ACTION ACCEPT
         --  se obtiene el valor del campo NSS
         LET v_nss = GET_FLDBUF(nss)

         -- se valida si se ingreso algun nss
         IF v_nss IS NULL THEN
            LET v_s_mensaje = "Se requiere capturar el NSS."
            CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

            CONTINUE CONSTRUCT
         END IF

         EXIT CONSTRUCT

      ON ACTION CANCEL
         -- se indica que no debe continuar con la verificación de remanente
         LET v_b_continua = FALSE

         EXIT CONSTRUCT
   END CONSTRUCT
   CLOSE WINDOW w_op_derechohab

   -- si el usuario ha cancelado la consulta, regresa al menú principal
   IF NOT v_b_continua THEN
      RETURN v_b_continua
   END IF

   -- se valida que exista el derechohabiente caprutado en el catalogo
   LET v_s_qryTxt = " SELECT COUNT(*)",
                    "   FROM afi_derechohabiente ",
                    "  WHERE ",v_s_condicion

   PREPARE prp_cta_nombre FROM v_s_qryTxt
   EXECUTE prp_cta_nombre INTO v_i_cuenta_regs

   -- se valida si existe información del NSS capturado
   IF v_i_cuenta_regs = 0 THEN
      LET v_s_mensaje = "El NSS ingresado no existe."
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      RETURN v_b_continua
   END IF

   -- se valida si existe más de un registro en el catalogo de derechohabientes
   IF v_i_cuenta_regs > 1 THEN 
      -- consulta listado de derechohabientes de afi para obtener el id derechohab
      CALL fn_selec_derechohabiente(v_s_condicion) RETURNING v_r_afi_derechohabiente.*

      -- en caso de haber cancelado la selección de derechohabiente regresa al menú princial
      IF v_r_afi_derechohabiente.id_derechohabiente = -1 THEN
         RETURN v_b_continua
      END IF
   ELSE
      -- consulta id_derehohabiente
      LET v_s_qryTxt = " SELECT id_derechohabiente, nss, curp, rfc, ap_paterno_af, ap_materno_af, nombre_af\n",
                       "   FROM afi_derechohabiente\n",
                       "  WHERE ", v_s_condicion

      PREPARE prp_con_id FROM v_s_qryTxt
      EXECUTE prp_con_id INTO v_r_afi_derechohabiente.*
   END IF

   DISPLAY " Derechohabiente: ", v_r_afi_derechohabiente.id_derechohabiente

   -- se verifica si existe mas de un credito para el derechohabiente en proceso
   SELECT COUNT(*)
     INTO v_i_cuenta_regs
     FROM cre_acreditado
    WHERE id_derechohabiente = v_r_afi_derechohabiente.id_derechohabiente
      AND tpo_originacion = 1
      AND estado NOT IN(10,920)

   -- se valida si existe información del NSS capturado
   IF v_i_cuenta_regs = 0 THEN
      LET v_s_mensaje = "No existe información para el NSS ingresado ó \n",
                        "no tiene un crédito originado"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      RETURN v_b_continua
   END IF

   -- en caso de encontrar mas de un credito para el derechohabiente, se pide al usuario seleccionar uno
   IF v_i_cuenta_regs > 1 THEN
      -- se invoca la funcion que muestra los créditos de un derechohabiente
      CALL fn_selec_credito(v_r_afi_derechohabiente.id_derechohabiente, v_r_afi_derechohabiente.nss) RETURNING r_d_id_cre_acreditado

      -- en caso de haber cancelado la selección de credito regresa al menú princial
      IF r_d_id_cre_acreditado = -1 THEN
         RETURN v_b_continua
      END IF
   ELSE
      -- consulta id_derehohabiente
      LET v_s_qryTxt = " SELECT c.id_cre_acreditado \n",
                       "   FROM cre_acreditado c \n",
                       "  WHERE c.id_derechohabiente = ",v_r_afi_derechohabiente.id_derechohabiente,"\n",
                       "    AND c.tpo_originacion = 1 ",
                       "    AND c.estado NOT IN(10,920) "

      PREPARE prp_slct_idCreACred FROM v_s_qryTxt
      EXECUTE prp_slct_idCreACred INTO r_d_id_cre_acreditado
   END IF

   DISPLAY " Acreditado: ", r_d_id_cre_acreditado

   {
   -- se verifica que exista información del derechohabiente
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado = ",r_d_id_cre_acreditado

   PREPARE prp_slct_count_sdoDeu FROM v_s_qryTxt
   EXECUTE prp_slct_count_sdoDeu INTO v_i_cuenta_regs

   -- si no se encontró información del derechohabiente regresa al menú principal
   IF v_i_cuenta_regs = 0 THEN
      LET v_s_mensaje = "No existe deudor para el NSS ingresado"
      CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

      RETURN v_b_continua
   END IF
   }

   -- se llama la funcion que despliega la consulta
   CALL fn_despliega_info_derechohab(v_r_afi_derechohabiente.*, r_d_id_cre_acreditado) RETURNING v_b_continua

   RETURN v_b_continua
END FUNCTION

#Objetivo: Funcion que muestra un listado de derechohabientes, para elegir el que se desea
#          consultar, esto se muestra cuando existe mas de un registro
FUNCTION fn_selec_derechohabiente(p_s_condicion)

   DEFINE p_s_condicion           STRING,
          v_s_qryTxt              STRING,
          v_indice                SMALLINT,
          v_r_afi_derhab          RECORD LIKE afi_derechohabiente.*,
          v_arr_derechohabiente   DYNAMIC ARRAY OF RECORD
             id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
             nss                  LIKE afi_derechohabiente.nss,
             curp                 LIKE afi_derechohabiente.curp,
             rfc                  LIKE afi_derechohabiente.rfc,
             ap_paterno_af        LIKE afi_derechohabiente.ap_paterno_af,
             ap_materno_af        LIKE afi_derechohabiente.ap_materno_af,
             nombre_af            LIKE afi_derechohabiente.nombre_af
   END RECORD

   DEFINE v_r_afi_derechohabiente RECORD
             id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
             nss                  LIKE afi_derechohabiente.nss,
             curp                 LIKE afi_derechohabiente.curp,
             rfc                  LIKE afi_derechohabiente.rfc,
             ap_paterno_af        LIKE afi_derechohabiente.ap_paterno_af,
             ap_materno_af        LIKE afi_derechohabiente.ap_materno_af,
             nombre_af            LIKE afi_derechohabiente.nombre_af
          END RECORD

   -- se inicializa el indice del arreglo
   LET v_indice = 1

   -- se obtiene la información del catálogo de derechohabientes
   LET v_s_qryTxt = " SELECT * ",
                    "   FROM afi_derechohabiente ",
                    "  WHERE ", p_s_condicion

   PREPARE prp_cons_derechohabiente FROM v_s_qryTxt
   DECLARE cur_derechohabiente CURSOR FOR prp_cons_derechohabiente

   FOREACH cur_derechohabiente INTO v_r_afi_derhab.*
      -- se asignan los valores al arreglo
      LET v_arr_derechohabiente[v_indice].id_derechohabiente = v_r_afi_derhab.id_derechohabiente
      LET v_arr_derechohabiente[v_indice].nss                = v_r_afi_derhab.nss
      LET v_arr_derechohabiente[v_indice].curp               = v_r_afi_derhab.curp
      LET v_arr_derechohabiente[v_indice].rfc                = v_r_afi_derhab.rfc
      LET v_arr_derechohabiente[v_indice].nombre_af          = v_r_afi_derhab.nombre_af
      LET v_arr_derechohabiente[v_indice].ap_paterno_af      = v_r_afi_derhab.ap_paterno_af
      LET v_arr_derechohabiente[v_indice].ap_materno_af      = v_r_afi_derhab.ap_materno_af

      -- se incrementa el indice del arreglo
      LET v_indice = v_indice + 1
   END FOREACH

   OPEN WINDOW w_lista_derechohabiente WITH FORM "ACRL472"
      DISPLAY ARRAY v_arr_derechohabiente TO tabla_derechohabiente.* ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )
         ON ACTION ACCEPT
            LET v_indice = arr_curr()
            LET v_r_afi_derechohabiente.id_derechohabiente = v_arr_derechohabiente[v_indice].id_derechohabiente
            LET v_r_afi_derechohabiente.nss = v_arr_derechohabiente[v_indice].nss
            LET v_r_afi_derechohabiente.curp = v_arr_derechohabiente[v_indice].curp
            LET v_r_afi_derechohabiente.rfc = v_arr_derechohabiente[v_indice].rfc
            LET v_r_afi_derechohabiente.ap_paterno_af = v_arr_derechohabiente[v_indice].ap_paterno_af
            LET v_r_afi_derechohabiente.ap_materno_af = v_arr_derechohabiente[v_indice].ap_materno_af
            LET v_r_afi_derechohabiente.nombre_af = v_arr_derechohabiente[v_indice].nombre_af

            EXIT DISPLAY

         ON ACTION CANCEL
            LET v_r_afi_derechohabiente.id_derechohabiente = -1
            EXIT DISPLAY
      END DISPLAY
   CLOSE WINDOW w_lista_derechohabiente

   RETURN v_r_afi_derechohabiente.*
END FUNCTION

#Objetivo: Funcion que muestra un listado creditos ortorgados al derechohabiente que entra
#          como parametro, el usuario debe seleccionar uno para continuar con la consulta
FUNCTION fn_selec_credito(p_id_derechohabiente, p_c_nss)

   DEFINE p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
          p_c_nss               LIKE afi_derechohabiente.nss,
          v_d_id_cre_acreditado LIKE cre_acreditado.id_cre_acreditado,
          v_sqlqry              STRING,
          v_r_cre_acreditado    RECORD
            id_cre_acreditado   LIKE cre_acreditado.id_cre_acreditado,
            id_derechohabiente  LIKE cre_acreditado.id_derechohabiente,
            tpo_originacion     LIKE cre_acreditado.tpo_originacion,
            tpo_credito         LIKE cre_acreditado.tpo_credito,
            num_credito         LIKE cre_acreditado.num_credito,
            estado              LIKE cre_acreditado.estado,
            edo_procesar        LIKE cre_acreditado.edo_procesar,
            f_otorga            LIKE cre_acreditado.f_otorga,
            sdo_deudor          LIKE cre_acreditado.sdo_deudor
   END RECORD

   DEFINE v_arr_num_creditos    DYNAMIC ARRAY OF RECORD
            id_cre_acreditado   LIKE cre_acreditado.id_cre_acreditado,
            nss                 LIKE afi_derechohabiente.nss,
            tpo_originacion     LIKE cat_cre_originacion.originacion_desc,
            desc_credito        LIKE cat_tipo_credito.desc_credito,
            num_credito         LIKE cre_acreditado.num_credito,
            desc_estado         LIKE cat_maq_credito.estado_desc,
            desc_edo_procesar   LIKE cat_maq_credito.estado_desc,
            f_otorga            LIKE cre_acreditado.f_otorga,
            sdo_deudor          LIKE cre_acreditado.sdo_deudor
   END RECORD

   DEFINE v_indice              SMALLINT

   -- se inicializa el indice del arreglo
   LET v_indice = 0

   -- se seleccionan los registros de la tabla maestro
   LET v_sqlqry = " SELECT id_cre_acreditado, id_derechohabiente, tpo_originacion, tpo_credito,\n",
                  "        num_credito, estado, edo_procesar, f_otorga, sdo_deudor\n",
                  "   FROM cre_acreditado\n",
                  "  WHERE id_derechohabiente = ",p_id_derechohabiente,"\n",
                  "    AND tpo_originacion = 1"

   PREPARE prp_slct_numCredito FROM v_sqlqry
   DECLARE cur_slct_numCredito CURSOR FOR prp_slct_numCredito

   FOREACH cur_slct_numCredito INTO v_r_cre_acreditado.*
      -- se incrementa el indice del arreglo
      LET v_indice = v_indice + 1

      -- se asignan los valore al arreglo
      LET v_arr_num_creditos[v_indice].id_cre_acreditado = v_r_cre_acreditado.id_cre_acreditado
      LET v_arr_num_creditos[v_indice].nss               = p_c_nss
      LET v_arr_num_creditos[v_indice].tpo_originacion   = fn_obt_desc_originacion(v_r_cre_acreditado.tpo_originacion)
      LET v_arr_num_creditos[v_indice].desc_credito      = fn_obt_desc_tpo_credito(v_r_cre_acreditado.tpo_credito)
      LET v_arr_num_creditos[v_indice].num_credito       = v_r_cre_acreditado.num_credito
      LET v_arr_num_creditos[v_indice].desc_estado       = fn_obt_desc_estado(v_r_cre_acreditado.estado)
      LET v_arr_num_creditos[v_indice].desc_edo_procesar = fn_obt_desc_estado(v_r_cre_acreditado.edo_procesar)
      LET v_arr_num_creditos[v_indice].f_otorga          = v_r_cre_acreditado.f_otorga
      LET v_arr_num_creditos[v_indice].sdo_deudor        = v_r_cre_acreditado.sdo_deudor
   END FOREACH    

   OPEN WINDOW w_lista_creditos WITH FORM "ACRL473"
      DISPLAY ARRAY v_arr_num_creditos TO tabla_creditos.* ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )
         ON ACTION ACCEPT
            LET v_indice = arr_curr()
            LET v_d_id_cre_acreditado = v_arr_num_creditos[v_indice].id_cre_acreditado 
            EXIT DISPLAY

         ON ACTION CANCEL
            LET v_d_id_cre_acreditado = -1

            EXIT DISPLAY
      END DISPLAY
   CLOSE WINDOW w_lista_creditos

   RETURN v_d_id_cre_acreditado
END FUNCTION

#Objetivo: Funcion que muestra el resultado de la consulta de derechohabiente
FUNCTION  fn_despliega_info_derechohab(p_r_afi_derechohabiente, p_d_id_cre_acreditado)

   DEFINE p_r_afi_derechohabiente RECORD
             id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
             nss                  LIKE afi_derechohabiente.nss,
             curp                 LIKE afi_derechohabiente.curp,
             rfc                  LIKE afi_derechohabiente.rfc,
             ap_paterno_af        LIKE afi_derechohabiente.ap_paterno_af,
             ap_materno_af        LIKE afi_derechohabiente.ap_materno_af,
             nombre_af            LIKE afi_derechohabiente.nombre_af
   END RECORD

   DEFINE p_d_id_cre_acreditado LIKE cre_acreditado.id_cre_acreditado

   DEFINE v_r_saldo_deudor        RECORD
             folio_referencia     LIKE cre_saldo_deudor.folio_referencia,
             movimiento           LIKE cre_saldo_deudor.movimiento,
             f_movimiento         LIKE cre_saldo_deudor.f_movimiento,
             monto_aivs           LIKE cre_saldo_deudor.monto_aivs,
             monto_pesos          LIKE cre_saldo_deudor.monto_pesos,
             f_proceso            LIKE cre_saldo_deudor.f_proceso
   END RECORD

   DEFINE v_ar_saldo_deudor DYNAMIC ARRAY OF RECORD
            folio_referencia     LIKE cre_saldo_deudor.folio_referencia,
            movimiento           LIKE cre_saldo_deudor.movimiento,
            f_movimiento         LIKE cre_saldo_deudor.f_movimiento,
            monto_aivs           LIKE cre_saldo_deudor.monto_aivs,
            monto_pesos          LIKE cre_saldo_deudor.monto_pesos,
            f_proceso            LIKE cre_saldo_deudor.f_proceso
   END RECORD

   DEFINE v_r_cta_movimiento RECORD
            subcuenta            LIKE cta_movimiento.subcuenta,
            desc_subcta          CHAR(40),
            monto_acciones       DECIMAL(18,6)
   END RECORD

   DEFINE v_ar_cta_movimiento DYNAMIC ARRAY OF RECORD
            subcuenta            CHAR(40),  ---LIKE cta_movimiento.subcuenta,
            monto_acciones       DECIMAL(18,6),
            monto_pesos          DECIMAL(14,2)
   END RECORD

   DEFINE v_rcd_credito RECORD
      originacion           SMALLINT,
      credito               SMALLINT,
      d_credito             CHAR(40),
      num_credito           DECIMAL(10,0),
      edo_infonavit         CHAR(40),
      edo_procesar          CHAR(40),
      fec_otorga            DATE,
      sdo_deudor            DECIMAL(12,2)
   END RECORD

   DEFINE v_ar_credito DYNAMIC ARRAY OF RECORD
      originacion           SMALLINT,
      credito               CHAR(50),
      num_credito           DECIMAL(10,0),
      edo_infonavit         CHAR(40),
      edo_procesar          CHAR(40),
      fec_otorga            DATE,
      sdo_deudor            DECIMAL(12,2)
   END RECORD

   DEFINE v_r_cre_his_acreditado  RECORD LIKE cre_his_acreditado.*
   DEFINE v_si_indice             SMALLINT
   DEFINE v_d_precio_fondo        LIKE glo_valor_fondo.precio_fondo -- precio de accion
   DEFINE v_d_tot_monto_acc       DECIMAL(18,6)
   DEFINE v_s_qryTxt              STRING
   DEFINE v_s_mensaje             STRING
   DEFINE v_iu_forma              ui.form
   DEFINE v_iu_ventana            ui.window
   DEFINE v_b_continua            SMALLINT
   DEFINE l_i_cuenta_regs         SMALLINT
   DEFINE l_bnd_act               SMALLINT

   DISPLAY " NSS    : ", p_r_afi_derechohabiente.nss
   DISPLAY " CURP   : ", p_r_afi_derechohabiente.curp
   DISPLAY " RFC    : ", p_r_afi_derechohabiente.rfc
   DISPLAY " PATERNO: ", p_r_afi_derechohabiente.ap_paterno_af
   DISPLAY " MATERNO: ", p_r_afi_derechohabiente.ap_materno_af
   DISPLAY " NOMBRE : ", p_r_afi_derechohabiente.nombre_af

   -- se inicializan variables
   LET v_b_continua = TRUE
   LET l_bnd_act    = FALSE

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_d_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   -- se inicializa el indice del arreglo
   LET v_si_indice = 0

   -- se realiza la consulta en la tabla maestro
   LET v_s_qryTxt = " SELECT folio_referencia, movimiento, f_movimiento,\n",
                    "        monto_aivs, monto_pesos, f_proceso\n",
                    "   FROM cre_saldo_deudor\n",
                    "  WHERE id_cre_acreditado = ",p_d_id_cre_acreditado

   PREPARE prp_saldo_deudor FROM v_s_qryTxt
   DECLARE cur_saldo_deudor CURSOR FOR prp_saldo_deudor

   FOREACH cur_saldo_deudor INTO v_r_saldo_deudor.*
      -- se incrementa el indice del arreglo
      LET v_si_indice = v_si_indice + 1

      LET v_ar_saldo_deudor[v_si_indice].folio_referencia = v_r_saldo_deudor.folio_referencia
      LET v_ar_saldo_deudor[v_si_indice].movimiento = v_r_saldo_deudor.movimiento
      LET v_ar_saldo_deudor[v_si_indice].f_movimiento = v_r_saldo_deudor.f_movimiento
      LET v_ar_saldo_deudor[v_si_indice].monto_aivs = v_r_saldo_deudor.monto_aivs
      LET v_ar_saldo_deudor[v_si_indice].monto_pesos = v_r_saldo_deudor.monto_pesos
      LET v_ar_saldo_deudor[v_si_indice].f_proceso = v_r_saldo_deudor.f_proceso
   END FOREACH

   -- se inicializa el indice del arreglo
   LET v_si_indice = 0

   -- se obtiene la información de la tabla del deudor
   LET v_s_qryTxt = " SELECT c.subcuenta, d.subcuenta_desc, SUM(c.monto_acciones)\n",
                    "   FROM cta_movimiento c, cat_subcuenta d \n",
                    "  WHERE c.id_derechohabiente = ",p_r_afi_derechohabiente.id_derechohabiente,"\n",
                    "    AND c.fondo_inversion = 11 \n",
                    "    AND c.subcuenta = d.subcuenta \n", 
                    "  GROUP BY c.subcuenta, d.subcuenta_desc"

   PREPARE prp_cta_movimiento FROM v_s_qryTxt
   DECLARE cur_cta_movimiento CURSOR FOR prp_cta_movimiento

   FOREACH cur_cta_movimiento INTO v_r_cta_movimiento.*
      -- se incrementa el indice del arreglo
      LET v_si_indice = v_si_indice + 1

      -- se asignan los valores en el arreglo del deudor
      LET v_ar_cta_movimiento[v_si_indice].subcuenta = v_r_cta_movimiento.subcuenta,"-",v_r_cta_movimiento.desc_subcta CLIPPED
      LET v_ar_cta_movimiento[v_si_indice].monto_acciones = v_r_cta_movimiento.monto_acciones
      LET v_ar_cta_movimiento[v_si_indice].monto_pesos = v_r_cta_movimiento.monto_acciones * v_d_precio_fondo
   END FOREACH

   LET v_s_qryTxt = " SELECT c.tpo_originacion, \n",
                    "        c.tpo_credito, \n",
                    "        f.desc_credito, \n",
                    "        c.num_credito, \n",
                    "        d.estado_desc, \n",
                    "        e.estado_desc, \n",
                    "        c.f_otorga, \n",
                    "        c.sdo_deudor \n",
                    "   FROM cre_acreditado c, \n",
                    "        cat_maq_credito d, \n",
                    "        cat_maq_credito e, \n",
                    "        cat_tipo_credito f \n",
                    "  WHERE c.id_cre_acreditado= ",p_d_id_cre_acreditado,"\n",
                    "    AND c.tpo_originacion = 1 \n",
                    "    AND c.estado = d.estado \n",
                    "    AND c.edo_procesar = e.estado \n",
                    "    AND c.tpo_credito = f.tpo_credito \n",
                    "    AND c.tpo_originacion = f.tpo_originacion "

   PREPARE prp_slct_CreACred FROM v_s_qryTxt
   DECLARE cur_slct_CreACred CURSOR FOR prp_slct_CreACred

   LET v_si_indice = 0

   FOREACH cur_slct_CreACred INTO v_rcd_credito.*
      LET v_si_indice = v_si_indice + 1

      LET v_ar_credito[v_si_indice].originacion   = v_rcd_credito.originacion
      LET v_ar_credito[v_si_indice].credito       = v_rcd_credito.credito," ",v_rcd_credito.d_credito
      LET v_ar_credito[v_si_indice].num_credito   = v_rcd_credito.num_credito
      LET v_ar_credito[v_si_indice].edo_infonavit = v_rcd_credito.edo_infonavit
      LET v_ar_credito[v_si_indice].edo_procesar  = v_rcd_credito.edo_procesar
      LET v_ar_credito[v_si_indice].fec_otorga    = v_rcd_credito.fec_otorga
      LET v_ar_credito[v_si_indice].sdo_deudor    = v_rcd_credito.sdo_deudor
   END FOREACH

   -- se abre la ventana de la información del remanente
   OPEN WINDOW w_des_derechohab WITH FORM "ACRL474"
      LET v_iu_ventana = ui.Window.getCurrent()
      LET v_iu_forma = v_iu_ventana.getForm()

      -- se desplega la información del derechohabiente
      DISPLAY p_r_afi_derechohabiente.nss,
              p_r_afi_derechohabiente.curp,
              p_r_afi_derechohabiente.rfc,
              p_r_afi_derechohabiente.ap_paterno_af,
              p_r_afi_derechohabiente.ap_materno_af,
              p_r_afi_derechohabiente.nombre_af
           TO nss,
              curp,
              rfc,
              app_paterno,
              app_materno,
              nombre

      DIALOG ATTRIBUTES (FIELD ORDER FORM, UNBUFFERED)
         DISPLAY ARRAY v_ar_saldo_deudor TO tabla_saldo_deudor.*
         END DISPLAY

         DISPLAY ARRAY v_ar_cta_movimiento TO tabla_deudor.*
         END DISPLAY

         DISPLAY ARRAY v_ar_credito TO tabla_cred_viv.*
         END DISPLAY

         ON ACTION ACCEPT

            -- se verifica que los estados sean los requeridos
            LET v_s_qryTxt = " SELECT COUNT(*)\n",
                             "   FROM cre_acreditado\n",
                             "  WHERE id_cre_acreditado = ",p_d_id_cre_acreditado,"\n",
                             "    AND estado IN (140,900,220)\n",
                             "    AND edo_procesar IN (120,5)"

            PREPARE prp_slct_count_edosCre FROM v_s_qryTxt
            EXECUTE prp_slct_count_edosCre INTO l_i_cuenta_regs

            -- si no se encontró información del derechohabiente regresa al menú principal
            IF l_i_cuenta_regs = 0 THEN
                LET v_s_mensaje = "El registro aún no está liquidado ó \n",
                                  "Procesar no ha enviado el saldo transferido, \n\n",
                                  "¿Aún así se marcará para Saldo Remanente?\n."

                --CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

                MENU "Confirmar modificación"
                   ATTRIBUTES ( STYLE = "dialog", COMMENT = v_s_mensaje, IMAGE = "question" )

                   COMMAND "Aceptar"
                     LET l_bnd_act = TRUE
                     EXIT MENU

                  COMMAND "Cancelar"
                     CALL fn_mensaje("Atención","No se marcó el registro para Saldo Remanente","stop")
                     LET l_bnd_act = FALSE
                     EXIT MENU
                END MENU
            ELSE
                LET l_bnd_act = TRUE
            END IF

            IF l_bnd_act THEN
               -- se obtiene la información de la tabla del deudor
               LET v_s_qryTxt = " SELECT SUM(monto_acciones)\n",
                                "   FROM cta_movimiento\n",
                                "  WHERE id_derechohabiente = ",p_r_afi_derechohabiente.id_derechohabiente,"\n",
                                "    AND fondo_inversion = 11"

               PREPARE prp_cta_montoAcciones FROM v_s_qryTxt
               EXECUTE prp_cta_montoAcciones INTO v_d_tot_monto_acc

               -- en caso de ser igual a cero el total en acciones no continua con el proceso de remanente
               IF SQLCA.SQLCODE = 100 OR v_d_tot_monto_acc <= 0 OR v_d_tot_monto_acc IS NULL THEN
                  LET v_s_mensaje = "No hay saldo en la subcuenta de vivienda."
                  CALL fn_mensaje("Aviso",v_s_mensaje,"stop")

                  CONTINUE DIALOG
               END IF

               -- se actualiza el registro maestro como saldo remanente
               UPDATE cre_acreditado
                  SET estado = 25,
                      edo_procesar = 70
                WHERE id_cre_acreditado = p_d_id_cre_acreditado

               -- se inserta registro en la tabla historica
               LET v_r_cre_his_acreditado.id_cre_acreditado   = p_d_id_cre_acreditado
               LET v_r_cre_his_acreditado.id_cre_ctr_archivo  = 0
               LET v_r_cre_his_acreditado.tpo_transferencia   = "03"
               LET v_r_cre_his_acreditado.edo_procesar        = 70
               LET v_r_cre_his_acreditado.diagnostico         = 0
               LET v_r_cre_his_acreditado.estado              = 25
               LET v_r_cre_his_acreditado.nss_afore           = NULL
               LET v_r_cre_his_acreditado.rfc_afore           = NULL
               LET v_r_cre_his_acreditado.paterno_afore       = NULL
               LET v_r_cre_his_acreditado.materno_afore       = NULL
               LET v_r_cre_his_acreditado.nombre_afore        = NULL
               LET v_r_cre_his_acreditado.nom_imss            = NULL
               LET v_r_cre_his_acreditado.f_proceso           = TODAY

               -- se inserta el registro en la tabla historica
               INSERT INTO cre_his_acreditado VALUES (v_r_cre_his_acreditado.*)

               -- se actualiza el estado correspondiente
               LET v_s_mensaje = "Se ha actualizado el registro para Saldo Remanente exitosamente."
               CALL fn_mensaje("Aviso",v_s_mensaje,"stop")
            END IF

            LET l_bnd_act    = FALSE
            LET v_b_continua = TRUE

            EXIT DIALOG

         ON ACTION cancelar
            LET v_b_continua = TRUE

            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW w_des_derechohab

   RETURN v_b_continua
END FUNCTION

# Objetivo: Funcion que obtiene la descripción de un tipo de originacion
FUNCTION fn_obt_desc_originacion(p_si_tpo_originacion)
   DEFINE p_si_tpo_originacion LIKE cat_cre_originacion.tpo_originacion,
          p_c_originacion_desc LIKE cat_cre_originacion.originacion_desc

   -- se selecciona la descripción del tipo de credito
   SELECT originacion_desc
     INTO p_c_originacion_desc
     FROM cat_cre_originacion
    WHERE tpo_originacion = p_si_tpo_originacion

   RETURN p_c_originacion_desc
END FUNCTION

#Objetivo: Funcion que obtiene la descripcion del tipo de credito
FUNCTION fn_obt_desc_tpo_credito(p_si_tpo_credito)
   DEFINE p_si_tpo_credito LIKE cat_tipo_credito.tpo_credito,
          v_c_desc_credito LIKE cat_tipo_credito.desc_credito,
          v_s_sqlqry       STRING

   -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
   LET v_s_sqlqry = " SELECT FIRST 1 desc_credito\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ",p_si_tpo_credito

   PREPARE prp_slctFrst_descCred FROM v_s_sqlqry
   EXECUTE prp_slctFrst_descCred INTO v_c_desc_credito

   RETURN v_c_desc_credito
END FUNCTION

#Objetivo: Funcion que obtiene la descripcion de estado o estado procesar
FUNCTION fn_obt_desc_estado(p_cve_estado)
   DEFINE p_cve_estado LIKE cat_maq_credito.estado,
          v_des_estado LIKE cat_maq_credito.estado_desc

   -- se selecciona la descripción del estado
   SELECT estado_desc
     INTO v_des_estado
     FROM cat_maq_credito
    WHERE estado = p_cve_estado

   RETURN v_des_estado
END FUNCTION
