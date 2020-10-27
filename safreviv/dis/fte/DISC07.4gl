################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 16/10/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC07                                                   #
#Objetivo          => Programa para consultar el registro de Avances de Pago   #
#                     por cuenta individual                                    #
#Fecha inicio      =>                                                          #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE v_nombre_completo   VARCHAR (100), --Nombre del derechohabiente
         g_sql_txt           STRING,
         v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING,
         p_proceso_cod       SMALLINT,
         v_ind_canc_par      SMALLINT
         
END GLOBALS

MAIN
DEFINE v_edit_nss            LIKE dis_rch_avance_pago.nss, --Almacena el NSS capturado
       v_indice              INTEGER,   --Indice de los arreglos
       v_ind_for             INTEGER    --Indice 

DEFINE arr_CuentaInd         DYNAMIC ARRAY OF RECORD       --Arreglo que recibe informacion de consulta
       folio                 LIKE dis_det_avance_pago.folio,   
       num_credito           DECIMAL(10,0),
       periodo_pago          LIKE dis_det_avance_pago.periodo_pago,
       f_pago                LIKE dis_det_avance_pago.f_pago,
       nrp                   LIKE dis_det_avance_pago.nrp,
       monto_aportacion      LIKE dis_det_avance_pago.monto_aportacion,  
       monto_amortizacion    LIKE dis_det_avance_pago.monto_amortizacion,
       estado                CHAR(52),
       id_dis_det_avance_pago DECIMAL(9,0)
       END RECORD

DEFINE g_canc_par            DYNAMIC ARRAY OF RECORD
       ava_orig_apo          DECIMAL(12,2),
       ava_orig_amo          DECIMAL(12,2),
       ava_canc_par_apo      DECIMAL(12,2),
       ava_canc_par_amo      DECIMAL(12,2),
       ava_pag_fin_apo       DECIMAL(12,2),
       ava_pag_fin_amo       DECIMAL(12,2),
       pag_pat_apo           DECIMAL(12,2),
       pag_pat_amo           DECIMAL(12,2),
       ajuste_sua_apo        DECIMAL(12,2),
       ajuste_sua_amo        DECIMAL(12,2),
       sdo_rem_apo           DECIMAL(12,2),
       sdo_rem_amo           DECIMAL(12,2),
       bimestre              CHAR(6),
       num_credito           DECIMAL(10,0),
       f_cancelacion         DATE,
       fol_cancelacion       DECIMAL(9,0)
       END RECORD

DEFINE p_usuario             LIKE seg_usuario.usuario_cod, --Clave de usuario
       p_tipo_proc           CHAR(1),                      --Tipo de proceso
       p_nombre_menu         LIKE seg_menu.opcion,         --Nombre del menú 
       p_programa            CHAR(10),                     --Nombre del programa
       r_valida_nss          DECIMAL (10,0)                --Indica si NSS existe en afi_derechohabiente

DEFINE f_ventana             ui.Window,   --Define las propìedades de la Ventana
       f_forma               ui.Form,     --Define las propiedades de la forma
       manejador_rpt         om.SaxDocumentHandler --Contenedor de Documentos para el reporte
 
  -- LET p_programa    = "DISC07"       
  LET p_usuario     = ARG_VAL(1) -- Recibe la variable de usuario
  LET p_tipo_proc   = ARG_VAL(2) -- Recibe el tipo de proceso
  LET p_nombre_menu = ARG_VAL(3) -- Recibe el nombre del programa
  LET p_proceso_cod = 902

  CLOSE WINDOW SCREEN 

  OPEN WINDOW v_ConsultaNSS WITH FORM "DISC071"
    -- se asigna el titulo del programa
    IF ( p_nombre_menu IS NOT NULL ) THEN
       CALL ui.Interface.setText(p_nombre_menu)
    END IF

    --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
    LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                    "       a.cod_proceso_valida,  ",
                    "       b.proceso_desc         ",
                    "FROM   cat_convivencia_dis a, ",
                    "       cat_proceso b          ",
                    "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                    "AND    a.cod_proceso_valida = b.proceso_cod ",
                    "AND    a.cod_convivencia    = 0             ",
                    "ORDER BY cod_proceso_valida   "
    PREPARE ps_val_proc FROM g_sql_txt
    DECLARE cur_val_proc CURSOR FOR ps_val_proc
    FOREACH cur_val_proc INTO v_proc_entra,
                              v_proc_val,
                              v_desc_proc_val
      IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
         LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
         MENU "No se puede ejecutar" 
           ATTRIBUTES ( STYLE="dialog",
           COMMENT= v_mensaje_val,
           IMAGE="information" )

           ON ACTION salir
               RETURN
         END MENU
      END IF
    END FOREACH

    LET f_ventana = ui.Window.getCurrent()
    LET f_forma   = f_ventana.getForm()

    CALL f_forma.setElementHidden("gr_detalle_cta_ind", 1) --Oculta la Sección Detalle por Cta Individual
    CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag
    CALL f_forma.setElementHidden("btn_reporte", 1) --Oculta el boton Reporte del detalle
    CALL f_forma.setElementHidden("btn_salir",1)  --Oculta botón Salir del detalle
    CALL f_forma.setFieldHidden("v_nombre",TRUE) --Oculta el cuadro de nombre del derechohabiente
    CALL f_forma.setElementHidden("lbl_nombre",TRUE) --Oculta la etiqueta del nombre

    INPUT BY NAME v_edit_nss WITHOUT DEFAULTS
    ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
      --Sale de la consulta sin hacer nada 
      ON ACTION CANCEL           
         EXIT INPUT

      ON ACTION ACCEPT
         IF length(v_edit_nss) < 11 THEN 
            CALL fn_mensaje ("ERROR", "El NSS debe ser de 11 caracteres", "information")
         ELSE         
            --Valida que el NSS capturado exista en base de datos
            CALL fn_valida_nss(v_edit_nss) RETURNING r_valida_nss

            -- Si el NSS existe 
            IF r_valida_nss >= 1 THEN 
               --Hace consulta basandose en el NSS
               CALL fn_consulta_avpag_cta_ind(v_edit_nss CLIPPED) 
               RETURNING arr_CuentaInd, v_indice

               -- Si el arreglo contiene información lo muestra en pantalla
               IF v_indice > 1 THEN              
                  DISPLAY ARRAY arr_CuentaInd TO scr_AvPagoNSS.*
                  ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)
                    --Habilita la sección de detalle y activa los botnes de reporte y salir
                    BEFORE  DISPLAY
                      CALL f_forma.setElementHidden("gr_detalle_cta_ind", 0) --Muestra la Sección Detalle por Cta Individual
                      CALL f_forma.setElementHidden("btn_reporte", 0) 
                      CALL f_forma.setElementHidden("btn_salir", 0)

                      --Habilita el cuadro del nopmbre del derechohabiente
                      CALL f_forma.setFieldHidden("v_nombre",FALSE)
                      CALL f_forma.setElementHidden("lbl_nombre",FALSE) --Oculta la etiqueta del nombre

                    BEFORE ROW
                      --Consulta informacion del resumen
                      CALL fn_consulta_canc_parc(v_edit_nss CLIPPED, 
                                                 arr_CuentaInd[ARR_CURR()].folio, 
                                                 arr_CuentaInd[ARR_CURR()].periodo_pago, 
                                                 arr_CuentaInd[ARR_CURR()].nrp,
                                                 arr_CuentaInd[ARR_CURR()].id_dis_det_avance_pago) 
                      RETURNING g_canc_par, v_ind_canc_par

                      IF v_ind_canc_par > 1 THEN
                         CALL f_forma.setElementHidden("gr_detalle_canc_pav", 0) --Muestra la sección de Resúmen de Registro Avance Pagos 
                         DISPLAY ARRAY g_canc_par TO scr_canc_par.*
                         ATTRIBUTES (CANCEL = FALSE )
                           BEFORE  DISPLAY 
                             DISPLAY "int flag --- ", INT_FLAG

                             --Hace que el display salga automáticamente sin esperar acción
                             IF INT_FLAG = 0 THEN 
                                ACCEPT DISPLAY 
                             END IF 
                         END DISPLAY
                      ELSE
                         CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag
                         --Si no existe muestra mensaje de atención 
                         CALL fn_mensaje("ATENCION","No existe información de resumen para mostrar","stop")
                      END IF

                      --Para salir termina el display y oculta el detalle y los botones
                      ON ACTION salir
                         CALL f_forma.setElementHidden("gr_detalle_cta_ind", 1) --Oculta la Sección Detalle por Cta Individual
                         CALL f_forma.setElementHidden("gr_detalle_canc_pav", 1) --Oculta la Sección Detalle Canc Parcial Ava Pag
                         CALL f_forma.setElementHidden("btn_reporte", 1) 
                         CALL f_forma.setElementHidden("btn_salir", 1)
                         CALL f_forma.setFieldHidden("v_nombre",TRUE) --Oculta el cuadro de nombre del derechohabiente
                         CALL f_forma.setElementHidden("lbl_nombre",TRUE) --Oculta la etiqueta del nombre
                         EXIT DISPLAY
                         CLEAR FORM
              
                      --Solicita la ejecucución del reporte 
                      ON ACTION reporte
                         IF fgl_report_loadCurrentSettings("DISC072.4rp") THEN 
                            CALL fgl_report_selectDevice ("PDF")
                            LET manejador_rpt = fgl_report_commitCurrentSettings()
                         END IF  

                         START REPORT rp_consulta_cta_ind TO XML HANDLER manejador_rpt
                           FOR v_ind_for = 1 TO v_indice
                               OUTPUT TO REPORT rp_consulta_cta_ind(arr_CuentaInd[v_ind_for].*,
                                                                    p_usuario, v_edit_nss)
                           END FOR
                         FINISH REPORT rp_consulta_cta_ind  
                  END DISPLAY
               ELSE 
                  -- Si la consulta no contiene información envia mensaje 
                  CALL fn_mensaje("CONSULTA", "NSS sin información de Avance de Pago", "information")
                  NEXT FIELD v_edit_nss
               END IF
            ELSE 
               -- Si el NSS capturado no existe en BD manda mensaje de error
               CALL fn_mensaje("ERROR", "El NSS Capturado no existe", "information")
               NEXT FIELD v_edit_nss
            END IF
         END IF   
    END INPUT
  CLOSE WINDOW v_ConsultaNSS
END MAIN

#OBJETIVO: Validar que el NSS capturado exista en la tabla de historico
FUNCTION fn_consulta_avpag_cta_ind(p_nss)
DEFINE r_arr_cta_ind         DYNAMIC ARRAY OF RECORD --Arreglo para registros rechazados
       folio                 LIKE dis_det_avance_pago.folio,
       num_credito           DECIMAL(10,0),
       periodo_pago          LIKE dis_det_avance_pago.periodo_pago,      
       f_pago                LIKE dis_det_avance_pago.f_pago,
       nrp                   LIKE dis_det_avance_pago.nrp,
       monto_aportacion      LIKE dis_det_avance_pago.monto_aportacion,  
       monto_amortizacion    LIKE dis_det_avance_pago.monto_amortizacion,
       estado                CHAR(52),
       id_dis_det_avance_pago DECIMAL(9,0)
       END RECORD,

       v_QryTxt              STRING,     -- Cadena para almacenar Query 
       v_indice              INTEGER,    -- Variable de indice
       v_id_dh_ci            LIKE dis_rch_avance_pago.nss,
       p_nss                 LIKE afi_derechohabiente.nss

  --Busca el id_derechohabiente en base al NSS capturado 
  SELECT MAX (id_derechohabiente) 
  INTO   v_id_dh_ci
  FROM   afi_derechohabiente
  WHERE  nss = p_nss
  IF v_id_dh_ci IS NULL THEN
     --Realiza consulta de datos solicitados en tabla de rechazos y de detalle
     LET v_QryTxt = "\n SELECT dr.folio, dr.num_credito, dr.periodo_pago, dr.f_pago,",
                    "\n        dr.nrp, dr.monto_aportacion, dr.monto_amortizacion,",
                    "\n        dr.estado || '-' || ca.desc_edo_avance AS ESTADO, 0",
                    "\n FROM   dis_rch_avance_pago dr, cat_edo_avance_pago ca",
                    "\n WHERE  dr.nss            = '",p_nss,"'",
                    "\n AND    ca.cod_edo_avance = dr.estado",
                    "\n GROUP BY 1,2,3,4,5,6,7,8,9",
                    "\n ORDER BY 1 DESC ,8"
  ELSE
     --Realiza consulta de datos solicitados en tabla de rechazos y de detalle
     LET v_QryTxt = "\n SELECT dd.folio, dd.num_credito, dd.periodo_pago, dd.f_pago,",
                    "\n        dd.nrp, dd.monto_aportacion, dd.monto_amortizacion,",
                    "\n        dd.estado || '-' || ca.desc_edo_avance AS ESTADO, dd.id_dis_det_avance_pago",
                    "\n FROM   dis_det_avance_pago dd, cat_edo_avance_pago ca",
                    "\n WHERE  id_derechohabiente = '",v_id_dh_ci CLIPPED,"'",
                    "\n AND    ca.cod_edo_avance  = dd.estado",
                    "\n GROUP BY 1,2,3,4,5,6,7,8,9",
                    "\n UNION",
                    "\n SELECT dr.folio, dr.num_credito, dr.periodo_pago, dr.f_pago,",
                    "\n        dr.nrp, dr.monto_aportacion, dr.monto_amortizacion,",
                    "\n        dr.estado || '-' || ca.desc_edo_avance AS ESTADO, 0",
                    "\n FROM   dis_rch_avance_pago dr, cat_edo_avance_pago ca",
                    "\n WHERE  id_derechohabiente = '",v_id_dh_ci CLIPPED,"'",
                    "\n AND    ca.cod_edo_avance  = dr.estado",
                    "\n GROUP BY 1,2,3,4,5,6,7,8,9",
                    "\n ORDER BY 1 DESC ,8"
  END IF 

  DISPLAY v_QryTxt
  -- Prepara la consulta para el display
  PREPARE prp_DetRechazados FROM v_QryTxt
  -- Declara el cursor para la consulta
  DECLARE cur_DetRechazados CURSOR FOR prp_DetRechazados
  LET v_indice = 1
  FOREACH cur_DetRechazados INTO r_arr_cta_ind[v_indice].*
    LET r_arr_cta_ind[v_indice].num_credito = r_arr_cta_ind[v_indice].num_credito USING "&&&&&&&&&&" 
    LET v_indice                            = v_indice + 1
  END FOREACH
      
  CALL r_arr_cta_ind.deleteElement(v_indice)

  --Obtiene nombre del derechohabiente
  CALL fn_obtiene_nombre_derechohabiente(v_id_dh_ci)
   
  -- Retorna el arreglo con detalles del folio rechazado y tamaño del arreglo
  RETURN r_arr_cta_ind, v_indice

END FUNCTION 

#OBJETIVO: Validar que la informacion capturada en el campo NSS exista
FUNCTION fn_valida_nss(p_nss)
DEFINE p_nss                 LIKE afi_derechohabiente.nss, --Parametro de entrada con el NSS 
       r_valida_nss          DECIMAL (10,0)                --Valor de retorno, indica si NSS existe o no

  --Valida si NSS existe en afi_derechohabiente
  SELECT COUNT(nss) 
  INTO   r_valida_nss
  FROM   afi_derechohabiente
  WHERE  nss = p_nss
  IF r_valida_nss = 0 OR r_valida_nss IS NULL THEN 
     SELECT COUNT(nss) 
     INTO   r_valida_nss
     FROM   dis_rch_avance_pago
     WHERE  nss = p_nss
  END IF 

  RETURN r_valida_nss
END FUNCTION 

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            LIKE afi_derechohabiente.id_derechohabiente 

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO v_nombre
   
END FUNCTION 

#Objetivo: Genera el reporte de la consulta por cuenta individual
REPORT rp_consulta_cta_ind(rpt_CuentaInd, p_usuario, p_nss )
  DEFINE rpt_CuentaInd       RECORD   --Arreglo para registros rechazados
    folio                    INTEGER,
    num_credito              DECIMAL(10,0),
    periodo_pago             LIKE dis_det_avance_pago.periodo_pago,
    f_pago                   LIKE dis_det_avance_pago.f_pago,
    nrp                      LIKE dis_det_avance_pago.nrp,
    monto_aportacion         LIKE dis_det_avance_pago.monto_aportacion,  
    monto_amortizacion       LIKE dis_det_avance_pago.monto_amortizacion,
    estado                   CHAR(52),
    id_dis_det_avance_pago   DECIMAL(9,0)
  END RECORD

  DEFINE v_fecha_reporte     DATE,  --Fecha en que se genera el reporte
         p_nss               LIKE dis_rch_avance_pago.nss, --Parametro de entrada de NSS
         p_usuario           LIKE seg_usuario.usuario_cod  --Clave de usuario

  FORMAT
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED
      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario
      PRINTX p_nss
      PRINTX v_nombre_completo
   
    ON EVERY ROW
       -- Se imprimen los resultados de la consulta
       PRINTX rpt_CuentaInd.folio
       PRINTX rpt_CuentaInd.num_credito
       PRINTX rpt_CuentaInd.periodo_pago
       PRINTX rpt_CuentaInd.f_pago USING "dd-mm-yyyy"
       PRINTX rpt_CuentaInd.nrp
       PRINTX rpt_CuentaInd.monto_aportacion
       PRINTX rpt_CuentaInd.monto_amortizacion
       PRINTX rpt_CuentaInd.estado

END REPORT

#OBJETIVO: Consultar la sección de cancelación parcial de Avances de Pago
FUNCTION fn_consulta_canc_parc(p_nss, p_folio, p_periodo_pago, p_nrp, p_id_dis_det_avance_pago)
DEFINE r_arr_canc_par        DYNAMIC ARRAY OF RECORD
       ava_orig_apo          DECIMAL(12,2),
       ava_orig_amo          DECIMAL(12,2),
       ava_canc_par_apo      DECIMAL(12,2),
       ava_canc_par_amo      DECIMAL(12,2),
       ava_pag_fin_apo       DECIMAL(12,2),
       ava_pag_fin_amo       DECIMAL(12,2),
       pag_pat_apo           DECIMAL(12,2),
       pag_pat_amo           DECIMAL(12,2),
       ajuste_sua_apo        DECIMAL(12,2),
       ajuste_sua_amo        DECIMAL(12,2),
       sdo_rem_apo           DECIMAL(12,2),
       sdo_rem_amo           DECIMAL(12,2),
       bimestre              CHAR(6),
       num_credito           DECIMAL(10,0),
       f_cancelacion         DATE,
       fol_cancelacion       DECIMAL(9,0)
       END RECORD,

       v_QryTxt              STRING,     -- Cadena para almacenar Query 
       v_indice              INTEGER,    -- Variable de indice
       v_id_dh_ci            LIKE dis_rch_avance_pago.nss,
       p_nss                 LIKE afi_derechohabiente.nss,
       p_folio               DECIMAL(9,0),
       p_periodo_pago        CHAR(6),
       p_nrp                 CHAR(11),
       p_id_dis_det_avance_pago DECIMAL(9,0),
       v_val_cpav            SMALLINT,
       v_val_comp            SMALLINT,
       v_c_monto_apo_pag     DECIMAL(12,2),
       v_c_monto_amo_pag     DECIMAL(12,2),
       v_c_edo_comp_apo      SMALLINT,
       v_c_edo_comp_amo      SMALLINT,
       v_monto_dif_apo       DECIMAL(12,2),
       v_monto_dif_amo       DECIMAL(12,2)

  LET v_indice          = 0;
  LET v_val_cpav        = 0;
  LET v_val_comp        = 0;
  LET v_c_monto_apo_pag = 0;
  LET v_c_monto_amo_pag = 0;
  LET v_c_edo_comp_apo  = 0;
  LET v_c_edo_comp_amo  = 0;
  LET v_monto_dif_apo   = 0;
  LET v_monto_dif_amo   = 0;

  CALL r_arr_canc_par.CLEAR()

  --Busca el id_derechohabiente en base al NSS capturado 
  SELECT MAX (id_derechohabiente) 
  INTO   v_id_dh_ci
  FROM   afi_derechohabiente
  WHERE  nss = p_nss

  SELECT COUNT(*)
  INTO   v_val_cpav
  FROM   dis_canc_par_ava_pag a
  WHERE  a.folio              = p_folio
  AND    a.id_derechohabiente = v_id_dh_ci
  AND    a.periodo_pago       = p_periodo_pago
  AND    a.nrp                = p_nrp
  AND    a.estado             = 1
  IF v_val_cpav >= 1 THEN
     --Realiza consulta de datos solicitados en tabla de rechazos y de detalle
     LET v_QryTxt = "\n SELECT a.monto_apo_ava,      ",
                    "\n        a.monto_amo_ava,      ",
                    "\n        a.monto_aportacion,   ",
                    "\n        a.monto_amortizacion, ",
                    "\n        a.monto_apo_fin,      ",
                    "\n        a.monto_amo_fin,      ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        0,                    ",
                    "\n        a.periodo_pago,       ",
                    "\n        a.num_credito,        ",
                    "\n        b.f_actualiza,        ",
                    "\n        a.folio               ",
                    "\n FROM   dis_canc_par_ava_pag a, ",
                    "\n        glo_folio b            ",
                    "\n WHERE  a.folio              = ",p_folio,
                    "\n AND    a.id_derechohabiente = ",v_id_dh_ci,
                    "\n AND    a.periodo_pago       = '",p_periodo_pago, "'",
                    "\n AND    a.nrp                = '",p_nrp, "'",
                    "\n AND    a.estado             = 1 ",
                    "\n AND    a.folio              = b.folio ",
                    "\n ORDER BY b.f_actualiza, a.folio"

     DISPLAY v_QryTxt
     -- Prepara la consulta para el display
     PREPARE prp_DetCancParc FROM v_QryTxt
     -- Declara el cursor para la consulta
     DECLARE cur_DetCancParc CURSOR FOR prp_DetCancParc
     LET v_indice = 1
     FOREACH cur_DetCancParc INTO r_arr_canc_par[v_indice].*
       SELECT COUNT(*)
       INTO   v_val_comp
       FROM   dis_compensa_avance
       --WHERE  id_dis_det_avance_pago = p_id_dis_det_avance_pago
       WHERE  id_derechohabiente     = v_id_dh_ci
       AND    periodo_pago           = p_periodo_pago
       AND    nrp                    = p_nrp
       IF v_val_comp > 0 THEN
          SELECT a.monto_apo_pag,
                 a.monto_amo_pag,
                 a.edo_compensa_apo,
                 a.edo_compensa_amo
          INTO   v_c_monto_apo_pag,
                 v_c_monto_amo_pag,
                 v_c_edo_comp_apo,
                 v_c_edo_comp_amo
          FROM   dis_compensa_avance a
          --WHERE  a.id_dis_det_avance_pago = p_id_dis_det_avance_pago
          WHERE  a.id_derechohabiente     = v_id_dh_ci
          AND    a.periodo_pago           = p_periodo_pago
          AND    a.nrp                    = p_nrp

          LET v_monto_dif_apo = 0;
          LET v_monto_dif_amo = 0;

          SELECT a.monto_dif_apo,
                 a.monto_dif_amo
          INTO   v_monto_dif_apo,
                 v_monto_dif_amo
          FROM   dis_det_avance_pago a
          --WHERE  folio                  = p_folio
          WHERE  a.periodo_pago           = p_periodo_pago
          AND    a.id_derechohabiente     = v_id_dh_ci
          AND    a.id_dis_det_avance_pago = p_id_dis_det_avance_pago

          LET r_arr_canc_par[v_indice].pag_pat_apo = v_c_monto_apo_pag;
          LET r_arr_canc_par[v_indice].pag_pat_amo = v_c_monto_amo_pag; 

          IF v_c_edo_comp_apo = 1 THEN
             LET r_arr_canc_par[v_indice].ajuste_sua_apo = v_monto_dif_apo;
          END IF
          IF v_c_edo_comp_amo = 1 THEN
             LET r_arr_canc_par[v_indice].ajuste_sua_amo = v_monto_dif_amo;
          END IF
          IF v_c_edo_comp_apo = 2 THEN
             LET r_arr_canc_par[v_indice].sdo_rem_apo    = v_monto_dif_apo;
          END IF
          IF v_c_edo_comp_amo = 2 THEN
             LET r_arr_canc_par[v_indice].sdo_rem_amo    = v_monto_dif_amo;
          END IF
       END IF

       LET v_indice = v_indice + 1
     END FOREACH

     CALL r_arr_canc_par.deleteElement(v_indice)

     --Obtiene nombre del derechohabiente
     CALL fn_obtiene_nombre_derechohabiente(v_id_dh_ci)
  END IF
  
  -- Retorna el arreglo con detalles del folio rechazado y tamaño del arreglo
  RETURN r_arr_canc_par, v_indice
END FUNCTION