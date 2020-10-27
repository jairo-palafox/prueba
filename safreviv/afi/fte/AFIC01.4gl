######################################################################
#Proyecto          => INFONAVIT (MEXICO)                             #
#Propietario       => E.F.P.                                         #
#Programa AFIC01   => PROGRAMA DE CONSULTA DE DERECHOHABIENTES       #
#Sistema           => SAFRE VIVIENDA                                 #
#Autor             => MAURO MUÑIZ CABALLERO                          #
#Fecha             => 27 DE MARZO DE 2012                            #
#Modifica          => EMILIO ABARCA SÁNCHEZ                          #
#Fecha Modifica    => 15 DE OCTUBRE 2016                             #
#Modifica          => Antonio Gómez - Oct 24, 2017                   #
######################################################################

IMPORT FGL WSHelper
IMPORT os
IMPORT com

DATABASE safre_viv

GLOBALS "AFIG01.4gl"
GLOBALS "AFIC011.inc"

   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo                STRING   -- título de la ventana
   DEFINE g_nss                     CHAR(11) -- nss
   DEFINE r_valida_nss              SMALLINT
   DEFINE r_valida                  SMALLINT
   DEFINE v_paso_nss                STRING
   DEFINE v_msj_alerta              STRING

MAIN

   -- se asignan los parámetros que vienen del fglrun
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_nss            = ARG_VAL(4) -- si se recibe, se envía directo a la consulta de datos

   -- se asigna el título de la ventana
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicia el LOG
   CALL STARTLOG('/safreviv_log/afi/'||g_usuario CLIPPED||'.AFIC01.log')
   
   -- se inician las variables globales
   LET g_hoy    = TODAY
   LET g_titulo = "Información"
   LET g_imagen = "information"

   CALL fn_alertamiento()
      RETURNING v_msj_alerta

   SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
     INTO g_reg_modulo.*
     FROM seg_modulo s
    WHERE modulo_cod = 'afi'

   CALL fn_proceso_principal()

END MAIN

{
======================================================================
Nombre: fn_proceso_principal
Fecha creación: 27 de Marzo de 2012
Autor: Mauro Muñiz Caballero, EFP
Narrativa del proceso que realiza:
Abre el proceso principal de consulta de datos del derechohabiente

Registro de modificaciones:
Autor       Fecha                   Descrip. cambio
Ivan Vega   22 de junio de 2012     - Se agrega NSS como parámetro. Si se recibe y es válido
                                      se abre directamente la ventana con resultados.
                                    - Si no se recibe se abre la ventana de captura de parámetros
                                      de búsqueda
======================================================================
}

-- Función principal del programa
FUNCTION fn_proceso_principal()

   DEFINE v_flg                     SMALLINT
   DEFINE v_mensaje                 STRING
   DEFINE v_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente

   CLOSE WINDOW SCREEN

   -- se abre la ventana de consulta
   OPEN WINDOW w2 WITH FORM "AFIC012"

   IF ( g_nss IS NOT NULL ) THEN
      CALL fn_valida_g_nss()

      IF r_valida = 1 THEN
         CALL fn_mensaje("Atención",v_msj_alerta,"stop")
      END IF
   END IF

   IF ( g_nss IS NOT NULL ) AND ( r_valida = 0 )  THEN
      -- se valida si el NSS existe en la lista de derechohabientes
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss               = g_nss
         AND tipo_trabajador   <> "V" 
         AND origen_afiliacion <> "S"

      -- si no existe el derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se indica al usuario en pantalla que no fue encontrado
         LET v_mensaje = "El derechohabiente con NSS [" || g_nss || "] no existe"
         CALL fn_mensaje("Atención", v_mensaje, "stop")
      ELSE
         -- si se recibió el NSS desde parámetro, se envía directamente a la consulta
         CALL fn_consulta_afi(g_nss)
      END IF
   ELSE
      -- si no se recibió el NSS desde parámetro, se habilita la captura de datos de búsqueda
      LET w_criterio = ui.window.getcurrent()
      LET w_criterio = ui.window.forName("w2")

      CALL w_criterio.settext("INFORMACIÓN DE DERECHOHABIENTES")

      LET f_criterio = w_criterio.getform()

      -- se abre el ciclo
      LET v_flg = TRUE

      WHILE ( v_flg )
         CALL fn_preconsulta() RETURNING v_flg

         -- si no se canceló la consulta
         IF ( v_flg ) THEN
           CALL fn_consulta_afi(NULL)
         END IF

         -- se reinicia el registro de derechohabiente
         INITIALIZE reg_derechohabiente.* TO NULL
      END WHILE
   END IF

   CLOSE WINDOW w2

END FUNCTION

-- Función para elegir la información del derechohabiente a desplegar
FUNCTION fn_preconsulta()

   DEFINE cont                      INTEGER     -- contador
   DEFINE x_flg                     SMALLINT
   DEFINE lc_qry                    STRING      -- query de consulta
   DEFINE v_nss                     VARCHAR(11) -- nss
   DEFINE v_rfc                     VARCHAR(13) -- rfc
   DEFINE v_curp                    VARCHAR(18) -- curp
   DEFINE v_folio                   DECIMAL(9,0)-- folio del archivo
   DEFINE v_nss_rojo                INTEGER
   DEFINE v_e_nss                   CHAR(11)
   DEFINE v_e_credito               DECIMAL(10,0)
   DEFINE lc_qry_c                  STRING 

   DEFINE rec_bus_credito RECORD
      v_id_dh                       LIKE cre_acreditado.id_derechohabiente,
      v_num_credito                 LIKE cre_acreditado.num_credito,
      v_edo_credito                 LIKE cre_acreditado.estado,
      v_edo_cre_desc                LIKE cat_maq_credito.estado_desc
   END RECORD

   DEFINE arr_busqueda DYNAMIC ARRAY OF RECORD
      nss                           CHAR(11),
      rfc                           CHAR(13),
      curp                          CHAR(18),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombres_af                    CHAR(40),
      folio                         DECIMAL(9,0),
      num_credito                   DECIMAL(10,0),
      edo_cre_desc                  CHAR(30)
   END RECORD

    LET x_flg = 0

    LET v_ind_nss  = FALSE
    LET v_ind_rfc  = FALSE
    LET v_ind_curp = FALSE

    -- se inician las variables
    LET v_nss   = NULL
    LET v_rfc   = NULL
    LET v_curp  = NULL
    LET v_folio = NULL 

    -- se abre el dialog para obtener parámetros de consulta
    DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT BY NAME v_e_nss, v_e_credito
         BEFORE INPUT
            CALL arr_busqueda.clear()
            CALL dialog.setActionHidden("close",1)
            CALL f_criterio.setelementhidden("gb_identifica",1)
            CALL f_criterio.setelementhidden("gb_credito",1)
            CALL f_criterio.setelementhidden("gb_contacto",1)

            AFTER FIELD v_e_nss
             LET v_ind_nss = FGL_BUFFERTOUCHED()

         ON ACTION ACCEPT
             IF (v_e_nss IS NULL) AND (v_e_credito IS NULL) THEN
                CALL fn_mensaje("Aviso"," Obligatoriamente debe indicar el NSS o el número de crédito para realizar la búsqueda","information")
             END IF

             CASE 
                WHEN(v_e_nss IS NOT NULL) AND (v_e_credito IS NOT NULL)
                   CALL fn_mensaje("Aviso","La consulta debe ser por nss o número de crédito","about")

                WHEN(v_e_nss IS NOT NULL) AND (v_e_credito IS NULL)
                   --se inhabilitan las columnas de No.crédito y estado
                   CALL f_criterio.setFieldHidden("arr_credito",TRUE)
                   CALL f_criterio.setFieldHidden("arr_estado",TRUE)
                   --se habilitan las columnas RFC,CURP y Folio
                   CALL f_criterio.setFieldHidden("arr_rfc",FALSE)
                   CALL f_criterio.setFieldHidden("arr_curp",FALSE)
                   CALL f_criterio.setFieldHidden("arr_folio",FALSE)

                   LET v_paso_nss = GET_FLDBUF(v_e_nss)

                   IF ( v_paso_nss IS NOT NULL ) THEN
                      IF fn_valida_caracteres(v_paso_nss) <> 0 THEN
                         CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
                         NEXT FIELD v_e_nss
                      END IF

                      CALL fn_valida_nss(v_paso_nss)

                      IF r_valida_nss = 0 THEN
                         LET lc_qry = " SELECT a.nss  ,",
                                             " a.rfc  ,",
                                             " a.curp ,",
                                             " a.ap_paterno_af ,",
                                             " a.ap_materno_af ,",
                                             " a.nombre_af, ",
                                             " a.folio_lote ",
                                " FROM afi_derechohabiente a ",
                               " WHERE nss = ",'"',v_e_nss CLIPPED,'"',
                                  " AND a.tipo_trabajador <> 'V' ",
                                  " AND a.origen_afiliacion <> 'S' "
                         --DISPLAY lc_qry 

                         PREPARE prp_pre_busqueda FROM lc_qry
                         DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda

                         LET cont= 1

                         FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                            LET cont = 1 + cont

                            IF ( cont > 32767 ) THEN
                               CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                               LET INT_FLAG = TRUE
                               EXIT DIALOG
                            END IF
                         END FOREACH

                         IF ( cont = 1 ) THEN
                            CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                            LET INT_FLAG = TRUE
                            --EXIT DIALOG
                         END IF

                         LET INT_FLAG = FALSE

                         EXIT DIALOG 
                      ELSE
                         CALL fn_mensaje("Atención",v_msj_alerta,"stop")
                         --EXIT DIALOG
                      END IF
                   END IF 

                WHEN (v_e_nss IS NULL) AND (v_e_credito IS NOT NULL)
                   IF(v_e_credito = 0) THEN 
                      CALL fn_mensaje("","El criterio de búsqueda con número de crédito  ' 0 '  no es válido","")
                      CONTINUE DIALOG 
                   ELSE 
                      --Se habilitan las columnas No.crédito y estado del crédito
                      CALL f_criterio.setFieldHidden("arr_credito", FALSE)
                      CALL f_criterio.setFieldHidden("arr_estado", FALSE)

                      --se inhabilitan las columnas RFC,CURP y Folio
                      CALL f_criterio.setFieldHidden("arr_rfc",TRUE)
                      CALL f_criterio.setFieldHidden("arr_curp",TRUE)
                      CALL f_criterio.setFieldHidden("arr_folio",TRUE)

                      --Recupera información del dh y el crèdito que tiene
                      DECLARE cur_cons_dh CURSOR FOR
                                       SELECT cre.id_derechohabiente,
                                              cre.num_credito,
                                              cre.edo_credito,
                                              maq.estado_desc
                                         FROM cre_acreditado cre, 
                                              cat_maq_credito maq 
                                        WHERE cre.num_credito = v_e_credito
                                          AND cre.estado = maq.estado

                      LET cont = 1

                      FOREACH cur_cons_dh INTO rec_bus_credito.*
                         --Busca informaci?n en afi derechohabiente
                         LET lc_qry_c = " SELECT a.nss,",
                                               " a.rfc  ,",
                                               " a.curp ,",
                                               " a.ap_paterno_af ,",
                                               " a.ap_materno_af ,",
                                               " a.nombre_af, ",
                                               " a.folio_lote ",
                                         " FROM afi_derechohabiente a ",
                                        " WHERE a.id_derechohabiente = ",rec_bus_credito.v_id_dh,
                                          " AND a.tipo_trabajador <> 'V' ",
                                          " AND a.origen_afiliacion <> 'S' "

                         PREPARE prp_qry_c FROM lc_qry_c
                         DECLARE cur_qry_c CURSOR FOR prp_qry_c 

                         FOREACH cur_qry_c INTO arr_busqueda[cont].*

                            LET arr_busqueda[cont].num_credito = rec_bus_credito.v_num_credito
                            LET arr_busqueda[cont].edo_cre_desc = rec_bus_credito.v_edo_cre_desc

                            LET cont = cont + 1
 
                            IF ( cont > 32767 ) THEN
                               CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                               LET INT_FLAG = TRUE
                               EXIT DIALOG
                            END IF

                         END FOREACH
                      END FOREACH
                   END IF
                      IF ( cont = 1 ) THEN
                         CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                         LET INT_FLAG = TRUE
                         --EXIT DIALOG
                      END IF

                      LET INT_FLAG = FALSE

                      EXIT DIALOG
             END CASE

         ON ACTION CANCEL
            LET INT_FLAG = TRUE
            EXIT DIALOG

        AFTER INPUT 
             IF ((NOT v_ind_nss) AND
                 (NOT v_ind_rfc) AND
                 (NOT v_ind_curp) AND
                 (NOT v_ind_folio)) THEN
                CALL fn_mensaje("Aviso","Debe indicar un criterio de búsqueda obligatoriamente","information")
                NEXT FIELD v_e_nss
             ELSE
                LET int_flag = FALSE
                EXIT DIALOG
             END IF

        ON KEY (INTERRUPT)
           CALL fn_mensaje("Aviso","Búsqueda cancelada","information")
           RETURN

      END INPUT

      #JGH - PRODINF-645 - No se permita acceder a los campos RFC, CURP y FOLIO como criterios de búsqueda.
      { CONSTRUCT lc_condicion ON a.nss #, a.rfc, a.curp, a.folio_lote
                            FROM nss #, rfc, curp, folio

          BEFORE CONSTRUCT
             CALL arr_busqueda.clear()

             CALL dialog.setActionHidden("close",1)
             CALL f_criterio.setelementhidden("gb_identifica",1)
             CALL f_criterio.setelementhidden("gb_credito",1)
             CALL f_criterio.setelementhidden("gb_contacto",1)

          AFTER FIELD nss
             LET v_ind_nss = FGL_BUFFERTOUCHED()

          #AFTER FIELD rfc
          #   LET v_ind_rfc = FGL_BUFFERTOUCHED()

          #AFTER FIELD curp
          #   LET v_ind_curp = FGL_BUFFERTOUCHED()

          #AFTER FIELD folio
          #   LET v_ind_folio = FGL_BUFFERTOUCHED()

          ON ACTION ACCEPT
          LET v_paso_nss = GET_FLDBUF(nss)

          IF v_paso_nss IS NULL THEN
             CALL fn_mensaje("Aviso"," Obligatoriamente debe indicar un NSS para búsqueda","information")
          END IF

          IF ( v_paso_nss IS NOT NULL ) THEN
             IF fn_valida_caracteres(v_paso_nss) <> 0 THEN
                CALL fn_mensaje("Consulta Saldo", "El carácter '*' no es válido en los parámetros de búsqueda", "about")
                NEXT FIELD nss
             END IF

             CALL fn_valida_nss()

             IF r_valida_nss = 0 THEN 
                LET lc_qry = " SELECT a.nss  ,",
                                    " a.rfc  ,",
                                    " a.curp ,",
                                    " a.ap_paterno_af ,",
                                    " a.ap_materno_af ,",
                                    " a.nombre_af, ",
                                    " a.folio_lote ",
                             " FROM afi_derechohabiente a ",
                             " WHERE ",lc_condicion CLIPPED,
                             " AND a.tipo_trabajador <> 'V' ",
                             " AND a.origen_afiliacion <> 'S' "
                --DISPLAY lc_qry 

                PREPARE prp_pre_busqueda FROM lc_qry
                DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda

                LET cont= 1

                FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                    LET cont = 1 + cont

                    IF ( cont > 32767 ) THEN
                        CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                        LET INT_FLAG = TRUE
                        EXIT DIALOG
                    END IF
                END FOREACH

                IF ( cont = 1 ) THEN
                    CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                    LET INT_FLAG = TRUE
                    --EXIT DIALOG
                END IF

                LET INT_FLAG = FALSE

                EXIT DIALOG
             ELSE
                CALL fn_mensaje("Atención",v_msj_alerta,"stop")
                --EXIT DIALOG
             END IF
          END IF

          ON ACTION cancel
              LET INT_FLAG = TRUE
              EXIT DIALOG

          AFTER CONSTRUCT
             IF ((NOT v_ind_nss  ) AND
                 (NOT v_ind_rfc  ) AND
                 (NOT v_ind_curp ) AND
                 (NOT v_ind_folio)) THEN
                CALL fn_mensaje("Aviso","Debe indicar un criterio de búsqueda obligatoriamente","information")
                NEXT FIELD nss
             ELSE
                LET int_flag = FALSE
                EXIT DIALOG
             END IF

          ON KEY (INTERRUPT)
             CALL fn_mensaje("Aviso","Búsqueda cancelada","information")
             RETURN

       END CONSTRUCT}

    END DIALOG

    CALL f_criterio.setelementhidden("gb_identifica",0)
    CALL f_criterio.setelementhidden("gb_credito",0)
    CALL f_criterio.setelementhidden("gb_contacto",0)

    -- se borra el último renglón del arreglo porque el FOREACH lo deja en blanco
    CALL arr_busqueda.deleteElement(cont)

    IF ( (cont-1) >= 1 ) THEN
       DISPLAY ARRAY arr_busqueda TO tb2.*

          ON ACTION ACCEPT

          SELECT count(*)
            INTO v_nss_rojo
            FROM afi_nss_rojo c
           WHERE c.nss = arr_busqueda[ARR_CURR()].nss
             AND c.estado_rojo = 1

          CASE 
             WHEN v_nss_rojo > 0
                LET g_nss = arr_busqueda[ARR_CURR()].nss

                CALL fn_valida_g_nss()

                IF r_valida = 1 THEN
                   CALL fn_mensaje("Atención",v_msj_alerta,"stop")
                ELSE
                   LET reg_derechohabiente.nss = arr_busqueda[ARR_CURR()].nss
                   LET INT_FLAG = FALSE
                   EXIT DISPLAY
                END IF
            WHEN v_nss_rojo = 0
               LET reg_derechohabiente.nss = arr_busqueda[ARR_CURR()].nss
               LET INT_FLAG = FALSE

               EXIT DISPLAY
          END CASE

          ON ACTION cancel
             LET INT_FLAG = TRUE
             EXIT DISPLAY

       END DISPLAY

       --se habilitan las columnas RFC,CURP y Folio para dejar el formulario original
       CALL f_criterio.setFieldHidden("arr_rfc",FALSE)
       CALL f_criterio.setFieldHidden("arr_curp",FALSE)
       CALL f_criterio.setFieldHidden("arr_folio",FALSE)

       IF NOT INT_FLAG THEN
          LET x_flg = 1
          LET INT_FLAG = FALSE
       ELSE
          LET x_flg = 0
       END IF
    END IF

    CLEAR FORM

    RETURN x_flg

END FUNCTION

#función que despliega la información del asignado elegido
FUNCTION fn_consulta_afi(p_nss)

   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_ruta_formulario         STRING -- ruta del formulario
   DEFINE v_ruta_ejecutable         LIKE seg_modulo.ruta_bin
   DEFINE v_afore_desc              LIKE cat_afore.afore_desc
   DEFINE v_ind_fallecido           SMALLINT
   DEFINE v_sexo_curp               CHAR(1)
   DEFINE v_desc_sexo               CHAR(15)

   DEFINE v_perfil_crm              SMALLINT
   DEFINE v_ventana                 ui.window
   DEFINE v_forma                   ui.form
   DEFINE v_pestana                 om.DomNode

   DEFINE vr_afi_telefono RECORD -- registro de telefono
      id_derechohabiente            DECIMAL(9,0),
      id_telefono                   SMALLINT,
      cve_lada                      CHAR(3),
      extension                     CHAR(10),
      telefono                      CHAR(13),
      tpo_telefono                  SMALLINT,
      folio_lote                    DECIMAL(9,0),
      f_proceso_tel                 DATE
   END RECORD

   DEFINE v_id_riss                 SMALLINT
   DEFINE v_desc_riss               CHAR(40)
   DEFINE v_f_riss                  DATE
   DEFINE v_imagen                  STRING
   DEFINE v_tpo_dscto               SMALLINT
   DEFINE v_s_qryTxt_cre            STRING

   DEFINE r_documentos_nss DYNAMIC ARRAY OF RECORD 
      id_sep_docto_exp              LIKE sep_docto_exp.id_sep_docto_exp,
      docto_cod                     LIKE sep_cat_docto.docto_cod,
      docto_desc                    LIKE sep_cat_docto.docto_desc,
      docto_nombre                  LIKE sep_docto_exp.docto_nombre,
      ruta_docto                    VARCHAR(200),
      extension                     STRING
   END RECORD

   DEFINE v_status                  INTEGER
   DEFINE v_tpo_notificacion        SMALLINT
   
   -- se obtiene la ruta bin del modulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   -- se construye el nombre del formulario
   LET v_ruta_formulario = v_ruta_ejecutable CLIPPED || "/AFIC01"
   DISPLAY "v_ruta_formulario: ", v_ruta_formulario

   -- si se recibe como parámetro el nss se pone en el registro global de consulta
   -- y que la consulta muestre directamente los datos
   IF ( p_nss IS NOT NULL ) THEN
      LET reg_derechohabiente.nss = p_nss
   END IF

   OPEN WINDOW DespliInfo WITH FORM ("AFIC01")
   --OPEN WINDOW DespliInfo WITH FORM v_ruta_formulario

   LET w_criterio = ui.window.getcurrent()
   --LET w_criterio = ui.window.forName("DespliInfo")
   LET v_forma    = w_criterio.getform()
   
   LET INT_FLAG = TRUE
   CALL fgl_settitle("INFORMACIÓN DE DERECHOHABIENTES")

   CALL fn_inicializa()

   PREPARE qry_asig FROM g_consulta

   EXECUTE qry_asig INTO g_id_derechohabiente,
                         reg_derechohabiente.nss,
                         reg_derechohabiente.rfc,
                         reg_derechohabiente.curp,
                         reg_derechohabiente.f_nacimiento,
                         reg_derechohabiente.ap_paterno_af,
                         reg_derechohabiente.ap_materno_af,
                         reg_derechohabiente.nombre_af,
                         reg_derechohabiente.nombre_imss,
                         reg_derechohabiente.desc_tipo_trab,
                         reg_derechohabiente.desc_origen,
                         reg_derechohabiente.f_apertura_inf,
                         reg_derechohabiente.f_apertura,
                         reg_derechohabiente.ind_estado_cuenta,
                         reg_derechohabiente.f_estado_cuenta,
                         reg_derechohabiente.sexo
--DISPLAY "v_valida",v_valida

   LET v_s_qryTxt = " SELECT FIRST 1 a.afore_cod || ' - ' || b.afore_desc
                        FROM afi_afore a, cat_afore b
                       WHERE id_derechohabiente = ?
                         AND a.afore_cod = b.afore_cod"

   PREPARE prp_cons_afore FROM v_s_qryTxt
   EXECUTE prp_cons_afore USING g_id_derechohabiente INTO v_afore_desc

   IF v_afore_desc[1,3] = 531 THEN
      LET v_afore_desc = "531 - SIN AFORE REGISTRADA"
   END IF

   SELECT desc_dscto
     INTO reg_derechohabiente.tpo_dscto
     FROM cat_tipo_descuento
    WHERE tpo_dscto = v_tpo_dscto

   LET v_s_qryTxt_cre = "EXECUTE FUNCTION fn_edo_cred_viv(?,?)"

--DISPLAY "ENVÍO,  ", g_id_derechohabiente, " - ", v_valida
   PREPARE prp_credito_vivienda FROM v_s_qryTxt_cre
   EXECUTE prp_credito_vivienda USING g_id_derechohabiente,
                                      v_valida
                                 INTO v_resultado,
                                      v_tpo_originacion,
                                      v_tpo_credito,
                                      reg_derechohabiente.num_credito,
                                      reg_derechohabiente.f_otorga,
                                      reg_derechohabiente.f_liquida,
                                      v_tpo_dscto -- agregado


--DISPLAY "RESULTADO   ", v_resultado
--DISPLAY "ORIGINACION ", v_tpo_originacion
--DISPLAY "CREDITO     ", v_tpo_credito
--DISPLAY "NO CREDITO  ", reg_derechohabiente.num_credito
--DISPLAY "F_OTORGA    ", reg_derechohabiente.f_otorga
--DISPLAY "F_LIQUIDA   ", reg_derechohabiente.f_liquida
--DISPLAY "TPO_DSCTO   ", reg_derechohabiente.tpo_dscto  -- AGREGADO

   CASE v_resultado
     WHEN 0 LET reg_derechohabiente.edo_credito = "CRÉDITO VIGENTE"
     WHEN 2 LET reg_derechohabiente.edo_credito = "CRÉDITO LIQUIDADO"
     WHEN 3 LET reg_derechohabiente.edo_credito = "CRÉDITO EN TRÁMITE"
     WHEN 5 LET reg_derechohabiente.edo_credito = "CRÉDITO CANCELADO"
     WHEN 6 LET reg_derechohabiente.edo_credito = "CRÉDITO VENCIDO"
     OTHERWISE LET reg_derechohabiente.edo_credito = "SIN CRÉDITO"
   END CASE

    SELECT org.originacion_desc
      INTO reg_derechohabiente.desc_orig_credito
      FROM cat_cre_originacion org
     WHERE org.tpo_originacion = v_tpo_originacion

    SELECT cat.desc_credito
      INTO reg_derechohabiente.desc_tpo_credito
      FROM cat_tipo_credito cat
     WHERE cat.tpo_originacion = v_tpo_originacion
       AND cat.tpo_credito = v_tpo_credito

   LET v_desc_riss                        = NULL
   LET reg_derechohabiente.desc_riss_imss = NULL
   LET reg_derechohabiente.desc_riss_inf  = NULL
   LET reg_derechohabiente.f_alta_imss    = NULL
   LET reg_derechohabiente.f_alta_inf     = NULL

   ----- INDICADORES PARA NOTIFICACIONES SAFRE VIVIENDA -----
   DECLARE cur_notifica CURSOR FOR SELECT tpo_notificacion 
                                     FROM afi_ind_notifica
                                    WHERE id_derechohabiente = g_id_derechohabiente

   FOREACH cur_notifica INTO v_tpo_notificacion
      CASE v_tpo_notificacion
         WHEN 1 LET reg_derechohabiente.ind_not_sms    = 1
         WHEN 2 LET reg_derechohabiente.ind_not_correo = 1
         WHEN 3 LET reg_derechohabiente.bloquea_correo = 1
         WHEN 4 LET reg_derechohabiente.bloquea_sms    = 1
      END CASE
   END FOREACH

   DECLARE cur_riss CURSOR FOR
      SELECT a1.id_riss, c1.desc_riss, a1.f_proceso
        FROM afi_riss a1,
             cat_riss c1
       WHERE a1.id_derechohabiente = g_id_derechohabiente
         AND a1.id_riss = c1.id_riss
      ORDER BY id_riss, f_proceso DESC

   FOREACH cur_riss INTO v_id_riss,
                         v_desc_riss,
                         v_f_riss

      IF v_id_riss IS NOT NULL THEN
         IF v_id_riss = 1 OR v_id_riss = 2 OR v_id_riss = 3 THEN 
            LET reg_derechohabiente.desc_riss_imss = v_desc_riss
            LET reg_derechohabiente.f_alta_imss    = v_f_riss
         ELSE
            LET reg_derechohabiente.desc_riss_inf = v_desc_riss
            LET reg_derechohabiente.f_alta_inf    = v_f_riss
         END IF
      END IF

      IF v_id_riss = 1 THEN
         EXIT FOREACH
      END IF
   END FOREACH

   FREE cur_riss

   ----indicador trabajador fallecido----
   SELECT "X"
     FROM afi_fallecido
    WHERE id_derechohabiente = g_id_derechohabiente
      AND estado = 10

   IF SQLCA.SQLCODE = 0 THEN
      LET v_ind_fallecido = 1
   END IF

   --Recupera sexo
   LET v_sexo_curp = reg_derechohabiente.curp[11]

   IF v_sexo_curp = "H" OR v_sexo_curp = "M" THEN
      SELECT desc_sexo
      INTO   v_desc_sexo
      FROM   cat_genero
      WHERE  genero = v_sexo_curp

      DISPLAY "SEXO ASIGNADO : ", v_desc_sexo 
   ELSE
      IF reg_derechohabiente.sexo = "1" OR reg_derechohabiente.sexo = "2" THEN
         SELECT desc_sexo
         INTO   v_desc_sexo
         FROM   cat_genero
         WHERE  sexo = reg_derechohabiente.sexo

         IF v_desc_sexo = "" THEN 
            LET v_desc_sexo = "NO ESPECIFICADO"
         END IF

         DISPLAY "SEXO CONSULTADO : ", v_desc_sexo 
      ELSE
         LET v_desc_sexo = "NO ESPECIFICADO"
      END IF
   END IF

   DISPLAY BY NAME reg_derechohabiente.*
   DISPLAY BY NAME v_afore_desc
   DISPLAY BY NAME v_ind_fallecido
   DISPLAY BY NAME v_desc_sexo

   CALL f_relacion_derechohabiente(g_id_derechohabiente)
   CALL f_credito_vivienda(g_id_derechohabiente)
   CALL f_relacion_riss(g_id_derechohabiente)
   CALL f_unificacion(g_id_derechohabiente)

   DECLARE cur_dom CURSOR FOR
   SELECT dom1.tpo_domicilio,
          dom1.id_domicilio,
          DECODE(tpo_domicilio,1,"PARTICULAR","OTRO"),
          dom1.ind_envio,
          glf.f_actualiza
     FROM afi_domicilio dom1,
    OUTER glo_folio glf
    WHERE dom1.id_derechohabiente = g_id_derechohabiente
      AND dom1.folio_lote = glf.folio
   ORDER BY dom1.f_actualiza DESC

   LET i = 1

   FOREACH cur_dom INTO domicilio_1[i].dom_cod,
                        domicilio_2[i].id_domicilio,
                        domicilio_1[i].dom_desc,
                        domicilio_2[i].ind_envio,
                        domicilio_1[i].f_proceso_dom

      IF domicilio_2[i].ind_envio = "X" THEN
         LET domicilio_1[i].envio_desc = "CORRESPONDENCIA"
      ELSE
         LET domicilio_1[i].envio_desc = ""
      END IF

      DISPLAY "g_id_derechohabiente: ", g_id_derechohabiente
      DISPLAY "dom_desc: ", domicilio_1[i].dom_desc

      LET i = i + 1
   END FOREACH

   FREE cur_dom

   DECLARE cur_correo CURSOR FOR
     SELECT corr1.valor, glf.f_actualiza
       FROM afi_contacto_electronico corr1,
      OUTER glo_folio glf
      WHERE corr1.id_derechohabiente = g_id_derechohabiente
        AND corr1.folio_lote = glf.folio
   ORDER BY glf.folio DESC

   LET iii = 1

   FOREACH cur_correo INTO correo_1[iii].*
      LET iii = iii + 1
   END FOREACH

   FREE cur_correo

   -- se leen los números telefónonicos del derechohabiente
   DECLARE cur_afitel CURSOR FOR
   SELECT afitel.id_derechohabiente   ,
          afitel.id_telefono          ,
          afitel.cve_lada             ,
          afitel.extension            ,
          afitel.telefono             ,
          afitel.tpo_telefono         ,
          afitel.folio_lote           ,
          glf.f_actualiza
     FROM afi_telefono afitel,
    OUTER glo_folio glf     
    WHERE afitel.id_derechohabiente = g_id_derechohabiente
      AND afitel.folio_lote = glf.folio
    ORDER BY glf.folio DESC

   -- se inicia el contador
   LET i = 1

   -- se borra el arreglo de telefonos
   CALL arr_telefono.clear()

   -- se transfieren los datos al arreglo
   FOREACH cur_afitel INTO vr_afi_telefono.*
      LET arr_telefono[i].tel_cod = vr_afi_telefono.tpo_telefono

      -- se indica el tipo de telefono
      CASE vr_afi_telefono.tpo_telefono
         WHEN 1
            LET arr_telefono[i].tel_desc = "PARTICULAR"
         WHEN 2
            LET arr_telefono[i].tel_desc = "OFICINA"
         WHEN 3
            LET arr_telefono[i].tel_desc = "CELULAR"
         OTHERWISE
            LET arr_telefono[i].tel_desc = "OTRO"
      END CASE

      LET arr_telefono[i].telefono      = vr_afi_telefono.telefono
      LET arr_telefono[i].pais_cod      = 52
      LET arr_telefono[i].cve_lada      = vr_afi_telefono.cve_lada
      LET arr_telefono[i].extension     = vr_afi_telefono.extension
      LET arr_telefono[i].f_proceso_tel = vr_afi_telefono.f_proceso_tel

      -- se incrementa el indice
      LET i = i + 1
   END FOREACH

   FREE cur_afitel

   IF (i) >= 1 THEN
      -- se borra el último registro que se generó por causa del foreach
      CALL domicilio_1.deleteelement(domicilio_1.getlength())
      CALL correo_1.deleteelement(correo_1.getlength())

      DIALOG ATTRIBUTES(UNBUFFERED,FIELD ORDER FORM)
         -- despligue de créditos de vivienda
         DISPLAY ARRAY credito_1 TO tb_cred_viv.*

            BEFORE DISPLAY
               CALL DIALOG.setactionhidden("close",1)
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

         END DISPLAY

         -- despliegue de domicilios
         DISPLAY ARRAY domicilio_1 TO tb5.*
            --BEFORE DISPLAY
               --CALL DIALOG.setactionhidden("close",1)

            BEFORE DISPLAY
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               CALL despliega_domicilio(g_id_derechohabiente,
                                        domicilio_2[cur_row].id_domicilio)

            --ON ACTION cancel
               --EXIT DIALOG
         END DISPLAY

         -- despligue de relación laboral
         DISPLAY ARRAY arr_relacion_derech TO t_relacion_laboral.*
            BEFORE DISPLAY
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

         END DISPLAY 

         -- despligue de relación riss
         DISPLAY ARRAY arr_riss TO t_id_riss.*
            BEFORE DISPLAY
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

         END DISPLAY 

         -- despliegue de correo electrónico
         DISPLAY ARRAY correo_1 TO tb7.*
            BEFORE DISPLAY
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()
               
            --ON ACTION cancel
               --EXIT DIALOG
         END DISPLAY

         -- despligue de números telefónicos
         DISPLAY ARRAY arr_telefono TO tb_telefono.*
            BEFORE DISPLAY
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

         END DISPLAY

         DISPLAY ARRAY r_documentos_nss TO sr_documentos_nss.*
            BEFORE ROW
               IF(r_documentos_nss[ARR_CURR()].extension = "pdf" OR
                  r_documentos_nss[ARR_CURR()].extension = "PDF")THEN
                  LET v_imagen = ""
                  DISPLAY v_imagen TO imagen
               ELSE
                  LET v_imagen = "<img gwc:attributes=\"src resourceuri('",r_documentos_nss[ARR_CURR()].docto_nombre CLIPPED,"','sepdocto')\" />"
                  DISPLAY v_imagen TO imagen
               END IF 

         END DISPLAY

         DISPLAY ARRAY unifica_1 TO tb_uni.*
            BEFORE DISPLAY
               CALL DIALOG.setactionhidden("close",1)
               LET v_imagen = ""
               DISPLAY v_imagen TO imagen

         END DISPLAY

         BEFORE DIALOG
            LET v_perfil_crm = 0
            
            SELECT usr.perfil_cod
            INTO v_perfil_crm
            FROM seg_usuario_perfil usr
            INNER JOIN seg_perfil per ON per.perfil_cod = usr.perfil_cod
            WHERE usr.usuario_cod = g_usuario
            AND per.perfil_corta = PERFIL_CRM

            --LET v_ventana = ui.window.forName("w2")
            --LET v_forma = v_ventana.getform()

            IF v_perfil_crm IS NOT NULL AND v_perfil_crm > 0 THEN
               #MOSTRAR PESTAÑA
            ELSE
               #OCULTAR PESTAÑA
                CALL v_forma.setElementHidden("gb_contacto.crm",1);

                CALL v_forma.setElementHidden("domicilio",1)
                CALL v_forma.setElementHidden("telefono",1)
                CALL v_forma.setElementHidden("correo_electronico",1)
            END IF
            
            CALL fn_recupera_documentos(reg_derechohabiente.nss) RETURNING r_documentos_nss

         {se inhibe el botón saldo para perfil "CONSUL"
         -- solicitud de consulta de saldo
         ON ACTION saldo
                CALL fn_eje_consulta(1)}

         ON ACTION datos_crm
            IF v_perfil_crm IS NOT NULL AND v_perfil_crm > 0 THEN
               CALL fn_datos_contacto_crm(reg_derechohabiente.nss) RETURNING v_status
               DISPLAY  BY NAME rec_crm_datos.*
            ELSE
               CALL fn_mensaje("Atención","NO TIENE PRIVILEGIOS PARA ACCEDER A ESTA INFORMACION DE LA CUENTA","stop")
            END IF

         ON ACTION CANCEL
            EXIT DIALOG
      END DIALOG
   ELSE
      CALL fn_mensaje("Atención","DERECHOHABIENTE NO TIENE DOMICILIO REGISTRADO","stop")
   END IF

   CLOSE WINDOW DespliInfo

END FUNCTION

-- Función para desplegar los domicilios del derechohabiente
FUNCTION despliega_domicilio(p_id_derechohabiente, p_id_domicilio)

   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE p_id_domicilio            SMALLINT

   SELECT dom.calle,
          TRIM(dom.num_exterior),
          TRIM(dom.num_interior),
          dom.colonia ,
          dom.cp,
          munic.municipio_desc,
          ciudad.ciudad_desc,
          estado.entidad_desc_larga
     INTO domicilio.calle,
          domicilio.num_ext,
          domicilio.num_int,
          domicilio.colonia_desc,
          domicilio.cp,
          domicilio.delegacion_desc,
          domicilio.ciudad_desc,
          domicilio.estado_desc
     FROM afi_domicilio dom
          LEFT JOIN cat_cp codigo ON codigo.cp = dom.cp
          LEFT JOIN cat_municipio munic ON munic.municipio = codigo.municipio
          LEFT JOIN cat_ciudad ciudad ON ciudad.ciudad = codigo.ciudad
          LEFT JOIN cat_entidad_federativa estado ON estado.entidad_federativa = codigo.entidad_federativa
    WHERE dom.id_derechohabiente = p_id_derechohabiente
      AND dom.id_domicilio = p_id_domicilio

   DISPLAY domicilio.calle,
           domicilio.num_ext,
           domicilio.num_int,
           domicilio.colonia_desc,
           domicilio.cp,
           domicilio.delegacion_desc,
           domicilio.ciudad_desc,
           domicilio.estado_desc
        TO calle,
           num_ext,
           num_int,
           colonia_desc,
           codpos,
           delegacion_desc,
           ciudad_desc,
           estado_desc

END FUNCTION

-- Función para desplegar pantalla de saldos del asignado
FUNCTION fn_eje_consulta(p_pgm)

    DEFINE p_pgm           SMALLINT

    DEFINE v_pgm           CHAR(6)
    DEFINE l_ruta_bin      CHAR(40)

    INITIALIZE comma TO NULL

    SELECT ct.ruta_bin
      INTO l_ruta_bin
      FROM seg_modulo ct
     WHERE modulo_cod = 'cta'

    IF p_pgm = 1 THEN
        LET v_pgm = 'CTAC01'
    END IF

   DISPLAY "USUARIO : ",g_usuario

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," '",g_usuario,
                "' '",p_tipo_ejecucion, "' '",p_s_titulo, "' '",g_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION

-- Se realiza consulta para visualizar las relaciones laborales del derechohabiente
FUNCTION f_relacion_derechohabiente(p_id_derechohabiente)

   DEFINE lc_qry               STRING
   DEFINE p_id_derechohabiente DECIMAL(9,0)
   DEFINE v_cont               SMALLINT

   CALL arr_relacion_derech.CLEAR()

   LET lc_qry = " SELECT rl.id_derechohabiente, \n",
                       " rl.nrp, \n",
                       " rl.f_alta_nrp, \n",
                       " rl.ind_relacion, \n",
                       " rl.folio_lote, \n",
                       " rl.f_actualiza, \n",
                       " rl.usuario \n",
                       " FROM afi_relacion_laboral rl \n",
                       " WHERE rl.id_derechohabiente = ", p_id_derechohabiente

   --DISPLAY lc_qry

   PREPARE prp_relacion_lab FROM lc_qry
   DECLARE cur_relacion_lab CURSOR FOR prp_relacion_lab

   LET v_cont= 1

   FOREACH cur_relacion_lab INTO arr_relacion_derech[v_cont].*
       ---DISPLAY "array", arr_relacion_derech[v_cont].*
       LET v_cont = v_cont + 1
   END FOREACH

   FREE cur_relacion_lab

END FUNCTION

-- Se realiza consulta para visualizar las relaciones riss del derechohabiente
FUNCTION f_relacion_riss(p_id_derechohabiente)

   DEFINE lc_qry                    STRING
   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_cont                    SMALLINT

   CALL arr_riss.CLEAR()

   LET lc_qry = " SELECT ar.nrp, \n",
                       " cr.desc_riss, \n",
                       " ar.f_movimiento, \n",
                       " ar.f_proceso, \n",
                       " cl.desc_riss_rl, \n",
                       " ar.id_riss \n",
                  " FROM afi_riss ar, \n",
                  "      cat_riss cr, \n",
                  "      cat_riss_rl cl \n",
                 " WHERE ar.id_derechohabiente = ", p_id_derechohabiente,"\n",
                 "   AND ar.id_riss = cr.id_riss \n",
                 "   AND ar.id_riss_rl = cl.id_riss_rl"

   --DISPLAY lc_qry

   PREPARE prp_relacion_riss FROM lc_qry
   DECLARE cur_relacion_riss CURSOR FOR prp_relacion_riss

   LET v_cont= 1

   FOREACH cur_relacion_riss INTO arr_riss[v_cont].*
       IF arr_riss[v_cont].id_riss = 4 THEN
          LET arr_riss[v_cont].v_desc_riss_rl = "ALTA CON NRP GENÉRICO"
       END IF 

       IF arr_riss[v_cont].id_riss = 5 THEN
          LET arr_riss[v_cont].v_desc_riss_rl = "BAJA CON NRP GENÉRICO"
       END IF

       ---DISPLAY "array", arr_riss[v_cont].*
       LET v_cont = v_cont + 1
   END FOREACH

   FREE cur_relacion_riss

END FUNCTION

FUNCTION f_credito_vivienda(p_id_derechohabiente)

   DEFINE lv_qry                    STRING
   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_cv                      SMALLINT

   DEFINE v_id_cre_acreditado       DECIMAL(9,0)
   DEFINE v_originacion             CHAR(40)
   DEFINE v_credito                 CHAR(40)
   DEFINE v_no_credito              DECIMAL(10,0)
   DEFINE v_edo_cred                CHAR(40)
   DEFINE v_edo_infonavit           CHAR(40)
   DEFINE v_estado                  SMALLINT
   DEFINE v_d_procesar              CHAR(30)
   DEFINE v_f_otorgamiento          DATE
   DEFINE v_f_culmina               DATE
   DEFINE v_f_homologa              DATE
   DEFINE v_tpo_credito             SMALLINT
   DEFINE v_entidad                 SMALLINT
   DEFINE v_ind_homologa            SMALLINT
   DEFINE v_tipo_dscto              SMALLINT
   DEFINE v_marca_inf               SMALLINT

   LET v_cv = 1

   CALL credito_1.CLEAR()

   DECLARE cur_cred CURSOR FOR
   SELECT UNIQUE c1.num_credito
     FROM cre_acreditado c1
    WHERE c1.id_derechohabiente = p_id_derechohabiente
      AND c1.tpo_credito       <> 0

   FOREACH cur_cred INTO v_no_credito
      SELECT FIRST 1 c.id_cre_acreditado,
             o.originacion_desc,
             c.tpo_credito,
             t.desc_credito,
             c.num_credito,
             v.entidad,
             v.estado_desc,
             c.estado,
             p.estado_desc,
             c.f_otorga,
             c.tpo_dscto,
             m.entidad_desc,
             t.marca_inf
        INTO v_id_cre_acreditado,
             v_originacion,
             v_tpo_credito,
             v_credito    ,
             v_no_credito ,
             v_entidad    ,
             v_edo_infonavit,
             v_estado     ,
             v_d_procesar ,
             v_f_otorgamiento,
             v_tipo_dscto,
             v_edo_cred,
             v_marca_inf
        FROM cre_acreditado c,
             cat_cre_originacion o,
             cat_tipo_credito t,
             cat_maq_credito v,
             cat_maq_credito p,
             cat_cre_entidad m
       WHERE c.id_derechohabiente = p_id_derechohabiente
         AND c.estado             = v.estado
         AND c.edo_procesar       = p.estado
         AND c.tpo_credito        = t.tpo_credito
         AND c.tpo_originacion    = t.tpo_originacion
         AND c.tpo_originacion    = o.tpo_originacion
         AND c.num_credito        = v_no_credito
         ---AND v.entidad            IN(1,2,3,5)
         AND v.entidad            = m.entidad
      ORDER BY v.entidad, c.f_otorga

      SELECT desc_dscto
        INTO credito_1[v_cv].v_tpo_dscto
        FROM cat_tipo_descuento
       WHERE tpo_dscto = v_tipo_dscto

      IF v_entidad = 2 THEN
         LET lv_qry = "SELECT FIRST 1 h.f_actualiza ",
                       " FROM cta_his_credito h",
                      " WHERE h.id_derechohabiente = ", p_id_derechohabiente,
                        " AND h.num_credito        = ", v_no_credito,
                        --" AND h.tpo_credito        = ", v_tpo_credito,
                        --" AND h.f_credito          = '", v_f_otorgamiento,"'",
                        " ORDER BY h.f_actualiza DESC "

         PREPARE qry_liq FROM lv_qry
         EXECUTE qry_liq INTO v_f_culmina

         IF v_f_culmina IS NULL OR v_f_culmina = "12/31/1899" THEN
            SELECT MAX(f_fin)
              INTO v_f_culmina
              FROM sfr_marca_historica
             WHERE id_derechohabiente = p_id_derechohabiente
               AND marca              = v_marca_inf
               AND n_referencia       = v_id_cre_acreditado

            IF v_f_culmina IS NULL OR v_f_culmina = "12/31/1899" THEN
               LET v_f_culmina = v_f_otorgamiento + 1 UNITS DAY USING "dd-mm-yyyy"
            END IF
         END IF
      END IF

      IF v_f_culmina IS NULL OR v_f_culmina = "12/31/1899" THEN
         LET v_f_culmina = "        "
      END IF

      SELECT UNIQUE MAX(f_homologa), ind_homologa
        INTO v_f_homologa, v_ind_homologa
        FROM cre_homologa_trm cht
       WHERE cht.id_cre_acreditado = v_id_cre_acreditado
         AND cht.ind_homologa      = 1
       GROUP BY 2

      IF v_f_homologa IS NULL OR
         v_f_homologa = "12/31/1899" THEN
         LET v_f_homologa = "        "
      END IF

      IF v_ind_homologa = 1 THEN
         LET v_edo_cred = v_edo_cred CLIPPED," HOMOLOGADO"
      END IF

      LET credito_1[v_cv].desc_originacion = v_originacion
      LET credito_1[v_cv].desc_credito     = v_credito
      LET credito_1[v_cv].v_num_credito    = v_no_credito
      LET credito_1[v_cv].v_edo_cred       = v_edo_cred
      LET credito_1[v_cv].v_infonavit      = v_edo_infonavit
      LET credito_1[v_cv].estado           = v_estado
      LET credito_1[v_cv].v_procesar       = v_d_procesar
      LET credito_1[v_cv].v_f_otorga       = v_f_otorgamiento
      LET credito_1[v_cv].v_f_liquida      = v_f_culmina
      LET credito_1[v_cv].v_f_homologa     = v_f_homologa

      -- se incrementa el indice
      LET v_cv = v_cv + 1

      LET v_originacion    = NULL
      LET v_credito        = NULL
      LET v_no_credito     = NULL
      LET v_edo_cred       = NULL
      LET v_edo_infonavit  = NULL
      LET v_estado         = NULL
      LET v_d_procesar     = NULL
      LET v_f_otorgamiento = NULL
      LET v_f_culmina      = NULL
      LET v_f_homologa     = NULL
   END FOREACH

   FREE cur_cred

   --CALL credito_1.deleteelement(credito_1.getlength())
END FUNCTION

################################################################################
#Modulo            => AFI                                                      #
#Programa          => AFIC01                                                   #
#Descripcion       => Función para recuperar los documentos relaciónados al nss#
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 26 Junio 2014                                            #
################################################################################
FUNCTION fn_recupera_documentos(p_nss)

DEFINE p_nss LIKE afi_derechohabiente.nss,
       --p_id_expediente LIKE sep_docto_exp.id_expediente,
       v_consulta STRING,
       v_documentos_nss DYNAMIC ARRAY OF RECORD 
         id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
         docto_cod        LIKE sep_cat_docto.docto_cod,
         docto_desc       LIKE sep_cat_docto.docto_desc,
         docto_nombre     LIKE sep_docto_exp.docto_nombre,
         ruta_docto       VARCHAR(200),
         extension        STRING
       END RECORD,
       v_documento_nss RECORD 
         id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
         docto_cod        LIKE sep_cat_docto.docto_cod,
         docto_desc       LIKE sep_cat_docto.docto_desc,
         docto_nombre     LIKE sep_docto_exp.docto_nombre
       END RECORD,
       v_indice SMALLINT

   DEFINE v_ruta_documentos   VARCHAR(100)
   DEFINE v_id_solicitud      DECIMAL(9,0)
   DEFINE v_id_cadena         VARCHAR(100)
   DEFINE v_zip               VARCHAR(100)
   DEFINE v_comando           STRING

   DEFINE v_archivo           VARCHAR(90)
   DEFINE v_canal             base.Channel 

   # Recupera los documentos relacionados al nss, según el catálogo de documentos de separación
   LET v_consulta = "\n SELECT doc.id_sep_docto_exp,",
                    "\n        cat.docto_cod,",
                    "\n        cat.docto_desc,",
                    "\n        doc.docto_nombre",
                    "\n   FROM sep_cat_docto cat LEFT OUTER JOIN sep_docto_exp doc",
                    "\n     ON doc.docto_cod = cat.docto_cod",
                    "\n        JOIN sep_expediente exp",
                    "\n     ON exp.id_expediente = doc.id_expediente",
                    "\n        JOIN sep_nss_expediente nss",
                    "\n     ON nss.id_expediente = exp.id_expediente",
                    "\n  WHERE nss.nss = ?",
                    "\n    AND cat.docto_cod IN (6,7)", # documentos: identificación oficial invadido, identificación oficial asociado
                    "\n  ORDER BY 3"

   PREPARE prp_recupera_doctos FROM v_consulta
   DECLARE cur_recupera_doctos CURSOR FOR prp_recupera_doctos

   LET v_indice = 1

   FOREACH cur_recupera_doctos USING p_nss
                                INTO v_documento_nss.*

      LET v_documentos_nss[v_indice].id_sep_docto_exp = v_documento_nss.id_sep_docto_exp
      LET v_documentos_nss[v_indice].docto_cod        = v_documento_nss.docto_cod
      LET v_documentos_nss[v_indice].docto_desc       = v_documento_nss.docto_desc
      LET v_documentos_nss[v_indice].docto_nombre     = v_documento_nss.docto_nombre      
      LET v_documentos_nss[v_indice].extension        = os.Path.extension(v_documento_nss.docto_nombre)

      IF(v_documentos_nss[v_indice].extension = "pdf" OR
         v_documentos_nss[v_indice].extension = "PDF")THEN
         LET v_documentos_nss[v_indice].ruta_docto       = "<a gwc:attributes=\"href resourceuri('",v_documento_nss.docto_nombre CLIPPED,"','sepdocto')\" target='nueva'>",v_documento_nss.docto_nombre CLIPPED,"</a>"
      END IF

      LET v_indice = v_indice + 1
   END FOREACH

   FREE cur_recupera_doctos

   #Se extrae la lista de ducumentos para Devolucion de saldo
   LET v_consulta = "SELECT FIRST 1 ",
                     "id_solicitud, ",
                     "documentos ",
                     "FROM ret_documento_devolucion ",
                     "WHERE nss = ? ",
                     "ORDER BY id_solicitud DESC "

--- Se modifica la consulta porque cambio el nombre de los archivos de imágenes, ahora se compone con el CASO CRM
--   LET v_consulta = "SELECT FIRST 1 ",
--                    "       CASE WHEN a.f_solicitud < '09/25/2018' THEN a.id_solicitud ",
--                    "                                              ELSE to_number(a.caso_adai) ",
--                    "       END AS dato_consulta, ",
--                    "       b.documentos ",
--                    "FROM   ret_solicitud_generico a, ",
--                    "       ret_documento_devolucion b ", 
--                    "WHERE  a.id_solicitud = b.id_solicitud ",
--                    "AND    a.nss = ? ",
--                    "ORDER  BY a.f_solicitud DESC  "                     
   PREPARE exe_consulta_devolucion FROM v_consulta

   INITIALIZE v_id_solicitud TO NULL
   INITIALIZE v_zip TO NULL
   EXECUTE exe_consulta_devolucion USING p_nss
                                    INTO v_id_solicitud,
                                         v_zip

   IF v_id_solicitud IS NOT NULL AND v_id_solicitud > 0 AND v_zip IS NOT NULL THEN
      #existen documentos de devolucion
      SELECT ruta_docto
      INTO v_ruta_documentos 
      FROM seg_modulo 
      WHERE modulo_cod = 'ret'
      
      LET v_comando = "unzip -u ", v_zip CLIPPED, " -d ", v_ruta_documentos CLIPPED
      RUN v_comando

      LET v_canal = base.Channel.create()
      #CALL ch.setDelimiter(".")
      LET v_id_cadena = v_id_solicitud
      LET v_comando = "ls ", v_ruta_documentos CLIPPED, "/*_", v_id_cadena CLIPPED, ".* "
      LET v_comando = "cd ", v_ruta_documentos CLIPPED, "; ls *_", v_id_cadena CLIPPED, ".* "
      DISPLAY v_comando 
      CALL v_canal.openPipe(v_comando,"r")
      WHILE v_canal.read([v_archivo])
         LET v_documentos_nss[v_indice].id_sep_docto_exp = v_indice
         LET v_documentos_nss[v_indice].docto_cod        = v_indice
         LET v_documentos_nss[v_indice].docto_nombre     = v_archivo      
         LET v_documentos_nss[v_indice].extension        = os.Path.extension(v_archivo)
         LET v_documentos_nss[v_indice].ruta_docto       = "<a gwc:attributes=\"href resourceuri('",v_archivo CLIPPED,"','retdocto')\" target='nueva'>",v_archivo CLIPPED,"</a>"

         IF v_archivo[1,6] = 'CARTA_' THEN
            LET v_documentos_nss[v_indice].docto_desc       = "Carta de conformidad para Devolucion"
            LET v_indice = v_indice + 1
         END IF

         IF v_archivo[1,10] = 'EDOCUENTA_' THEN
            LET v_documentos_nss[v_indice].docto_desc       = "Estado de cuenta"
            LET v_indice = v_indice + 1
         END IF

         IF v_archivo[1,6] = 'IFE_1_' THEN
            LET v_documentos_nss[v_indice].docto_desc       = "Identificacion oficial"
            LET v_indice = v_indice + 1
         END IF

         IF v_archivo[1,6] = 'IFE_2_' THEN
            LET v_documentos_nss[v_indice].docto_desc       = "Identificacion oficial 2"
            LET v_indice = v_indice + 1
         END IF
         
      END WHILE
     CALL v_canal.close()
   END IF

   RETURN v_documentos_nss

END FUNCTION

FUNCTION fn_valida_g_nss()

   DEFINE v_funcion                 STRING
   DEFINE v_tpo_consulta            SMALLINT

   LET v_tpo_consulta = 1

   DISPLAY "nss",g_nss
   DISPLAY "usuario",g_usuario

   LET v_funcion = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_g_nss FROM v_funcion
   EXECUTE prp_valida_g_nss USING g_nss,v_tpo_consulta,g_usuario INTO r_valida

END FUNCTION

FUNCTION fn_valida_nss(p_nss)

   DEFINE v_funcion_nss             STRING
   DEFINE v_cadena                  CHAR(11)
   DEFINE v_tpo_consulta            SMALLINT
   DEFINE p_nss                     CHAR(11)

   LET v_tpo_consulta = 1
   DISPLAY "nss",v_cadena
   DISPLAY "usuario",g_usuario

   LET v_funcion_nss = "EXECUTE PROCEDURE sp_valida_nss_rojo(?,?,?)"

   PREPARE prp_valida_nss FROM v_funcion_nss

   LET v_cadena = p_nss
   EXECUTE prp_valida_nss USING v_cadena,v_tpo_consulta,g_usuario
                           INTO r_valida_nss

END FUNCTION

FUNCTION fn_datos_contacto_crm(p_nss)

    DEFINE p_nss             CHAR(11)
    DEFINE v_status          INTEGER
    DEFINE v_url_servidor    CHAR(150)
    DEFINE v_usuario         CHAR(20)
    DEFINE v_password        CHAR(40)
    DEFINE v_intentos        DECIMAL(9,0)
    
    ----CONSULTA SERVICIO WEB CRM DATOS DE CONTACTO-----
    LET consultarDatosTrabajador.nss = p_nss

   SELECT ruta_servidor,
          usuario,
          password,
          num_reintento
     INTO v_url_servidor,
          v_usuario,
          v_password,
          v_intentos
     FROM wsv_cliente
    WHERE cve_cliente = "afi_1"

    LET DatosContactoSaciServiceWS_DatosContactoSaciPortLocation = v_url_servidor CLIPPED

    CALL com.WebServiceEngine.SetOption( "readwritetimeout",5 )
    CALL consultarDatosTrabajador_g() RETURNING v_status

    DISPLAY "STATUS", v_status

    IF v_status = 0 THEN
       IF consultarDatosTrabajadorResponse.return.e_cod = '00' THEN
          LET rec_crm_datos.crm_paterno  = consultarDatosTrabajadorResponse.return.name_last
          LET rec_crm_datos.crm_nombre = consultarDatosTrabajadorResponse.return.name_first
          LET rec_crm_datos.crm_curp = consultarDatosTrabajadorResponse.return.curp
          LET rec_crm_datos.crm_calle = consultarDatosTrabajadorResponse.return.calle
          LET rec_crm_datos.crm_colonia = consultarDatosTrabajadorResponse.return.col
          LET rec_crm_datos.crm_municipio = consultarDatosTrabajadorResponse.return.mpio
          LET rec_crm_datos.crm_celular = consultarDatosTrabajadorResponse.return.cel
          LET rec_crm_datos.crm_correo_elect = consultarDatosTrabajadorResponse.return.e_mail
          LET rec_crm_datos.crm_materno = consultarDatosTrabajadorResponse.return.name_lst2
          LET rec_crm_datos.crm_f_nacimiento = consultarDatosTrabajadorResponse.return.fe_nacmto
          LET rec_crm_datos.crm_rfc = consultarDatosTrabajadorResponse.return.rfc
          LET rec_crm_datos.crm_num_exterior = consultarDatosTrabajadorResponse.return.num_ext
          LET rec_crm_datos.crm_cod_postal = consultarDatosTrabajadorResponse.return.cp
          LET rec_crm_datos.crm_estado = consultarDatosTrabajadorResponse.return.edo
          LET rec_crm_datos.crm_telefono = consultarDatosTrabajadorResponse.return.tel
          LET rec_crm_datos.crm_num_interior = consultarDatosTrabajadorResponse.return.num_int
          LET rec_crm_datos.crm_not_sms = consultarDatosTrabajadorResponse.return.autoriza_sms
          LET rec_crm_datos.crm_not_correo_e = consultarDatosTrabajadorResponse.return.autoriza_email
          LET rec_crm_datos.crm_bloq_sms = consultarDatosTrabajadorResponse.return.bloqueo_sms
          LET rec_crm_datos.crm_bloq_correo_e = consultarDatosTrabajadorResponse.return.bloqueo_email
       ELSE
           CALL fn_mensaje("Datos Contacto CRM",
                            consultarDatosTrabajadorResponse.return.e_msg,
                            "")
       END IF
    ELSE
        INITIALIZE rec_crm_datos.* TO NULL
    END IF

    RETURN v_status
END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)

   DEFINE p_campo       STRING
   RETURN p_campo.getIndexOf("*",1)

END FUNCTION

FUNCTION f_unificacion(p_id_derechohabiente)

   DEFINE p_nss                     CHAR(11)
   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_id_dh_unificador        DECIMAL(9,0)
   DEFINE v_id_dh_unifiado          DECIMAL(9,0)
   DEFINE v_nss_unificador          CHAR(11)
   DEFINE v_nss_unificado           CHAR(11)
   DEFINE v_id_unificador           DECIMAL(9,0)
   DEFINE v_u                       SMALLINT
   CALL unifica_1.CLEAR()

   LET v_u = 1

   DECLARE cur_unificador CURSOR FOR
   SELECT r1.nss_unificador, d1.nsscta1
     FROM uni_det_unificador r1, uni_det_unificado d1
    WHERE r1.id_derechohabiente = p_id_derechohabiente
      AND r1.id_unificador = d1.id_unificador
   ORDER BY r1.id_unificador DESC

   FOREACH cur_unificador INTO unifica_1[v_u].nss_unificador,
                               unifica_1[v_u].nss_unificado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_unificador
   FREE cur_unificador

   DECLARE cur_unificado CURSOR FOR
   SELECT r2.nss_unificador, d2.nsscta1
     FROM uni_det_unificador r2, uni_det_unificado d2
    WHERE d2.id_unificador IN(SELECT n.id_unificador
                                FROM uni_det_unificado n
                               WHERE n.id_derechohabiente = p_id_derechohabiente)
      AND r2.id_unificador = d2.id_unificador
   ORDER BY r2.id_unificador DESC

   FOREACH cur_unificado INTO unifica_1[v_u].nss_unificador,
                              unifica_1[v_u].nss_unificado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_unificado
   FREE cur_unificado

   DECLARE cur_inf_unificador CURSOR FOR
   SELECT UNIQUE r3.nss nss_unificador,
          d3.nss nss_unificado
     FROM uni_inf_unificador r3,
          uni_inf_unificado d3
    WHERE r3.id_derechohabiente = p_id_derechohabiente
      AND r3.id_inf_unificador = d3.id_unificador

   FOREACH cur_inf_unificador INTO unifica_1[v_u].nss_unificador,
                                   unifica_1[v_u].nss_unificado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_inf_unificador
   FREE cur_inf_unificador

   DECLARE cur_inf_unificado CURSOR FOR
   SELECT UNIQUE r4.nss nss_unificador,
          d4.nss nss_unificado
     FROM uni_inf_unificador r4, uni_inf_unificado d4
    WHERE d4.id_unificador IN(SELECT n.id_unificador
                                FROM uni_inf_unificado n
                               WHERE n.id_derechohabiente = p_id_derechohabiente)
      AND r4.id_inf_unificador = d4.id_unificador

   FOREACH cur_inf_unificado INTO unifica_1[v_u].nss_unificador,
                                  unifica_1[v_u].nss_unificado

      LET v_u = v_u + 1
   END FOREACH

   CLOSE cur_inf_unificado
   FREE cur_inf_unificado

   CALL unifica_1.deleteelement(unifica_1.getlength())

   ---DISPLAY ARRAY unifica_1 TO tb_uni.*

END FUNCTION
