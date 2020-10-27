--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

#####################################################################
#Modulo            =>AGR                                            #
#Programa          =>AGRC04                                         #
#Objetivo          =>Programa que realiza la consulta de históricos #
#                    de devolución de saldos de anualidades         #
#Autor             =>Mauricio Sanchez, EFP                          #
#Fecha inicio      =>29 MAYO 2012                                   #
#####################################################################

IMPORT OS

DATABASE safre_viv

GLOBALS

   DEFINE g_pid               LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod       LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod         LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_tpo_transferencia LIKE dse_agrupa_devolucion.tpo_transferencia --tipo de originacion 

END GLOBALS

MAIN

   DEFINE p_tipo_ejecucion    SMALLINT -- forma como ejecutará el programa
   DEFINE p_s_titulo          STRING -- título de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRC04.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se inicializan variables
   LET g_tpo_transferencia = "43" -- 43-DSE Anualidades Garantizadas

   -- se invoca la funcion general de consulta de historico de acreditados
   CALL fn_consulta_historico_dev()

END MAIN

## ventana que muestra las opciones para consultas de acreditados ##
FUNCTION fn_consulta_historico_dev()

   DEFINE v_op_consulta CHAR(4)

   OPEN WINDOW w_op_consulta WITH FORM "AGRC041"

   INPUT v_op_consulta FROM opciones ATTRIBUTE (UNBUFFERED)

      ON ACTION ACCEPT
         IF v_op_consulta = "opt1" THEN
           CALL fn_consulta_derechohabiente_dev()
         END IF

         IF v_op_consulta = "opt2" THEN
           CALL fn_consulta_estado_dev()
         END IF

         IF v_op_consulta = "opt3" THEN
           CALL fn_consulta_historica_dev()
         END IF

         IF v_op_consulta = "opt4" THEN
           CALL fn_consulta_archivo_dev()
         END IF

      ON ACTION CANCEL
         EXIT INPUT

   END INPUT

   CLOSE WINDOW w_op_consulta

END FUNCTION 

## Funcion que realiza la consulta de derechohabiente ##
FUNCTION fn_consulta_derechohabiente_dev()

   DEFINE v_s_condicion              STRING
   DEFINE v_nss                      LIKE afi_derechohabiente.nss
   DEFINE v_curp                     LIKE afi_derechohabiente.curp
   DEFINE v_rfc                      LIKE afi_derechohabiente.rfc
   DEFINE v_app_paterno              LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_app_materno              LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_nombre                   LIKE afi_derechohabiente.nombre_af
   DEFINE v_cta_nombre               SMALLINT
   DEFINE v_id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_sqlqry                   STRING
   DEFINE v_bandera                  SMALLINT
   DEFINE v_cta_reg                  SMALLINT

   LET v_s_condicion = "1=1"
   LET v_bandera = TRUE 

   OPEN WINDOW w_op_derechohab WITH FORM "AGRC042"

   CONSTRUCT v_s_condicion
          ON nss,
             curp,
             rfc,
             ap_paterno_af,
             ap_materno_af,
             nombre_af
        FROM nss,
             curp,
             rfc,
             app_paterno,
             app_materno,
             nombre

      AFTER FIELD nombre
         NEXT FIELD nss

      ON ACTION ACCEPT
         LET v_app_paterno = GET_FLDBUF(app_paterno)
         LET v_app_materno = GET_FLDBUF(app_paterno)
         LET v_nombre = GET_FLDBUF(nombre)
         LET v_nss = GET_FLDBUF(nss)
         LET v_curp = GET_FLDBUF(curp)
         LET v_rfc = GET_FLDBUF(rfc)

         IF v_app_paterno IS NULL AND v_app_materno IS NULL AND v_nombre IS NULL AND 
            v_nss IS NULL AND v_curp IS NULL AND v_rfc IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere capturar al menos un campo","stop")
            CONTINUE CONSTRUCT
         END IF

         IF v_app_paterno IS NOT NULL AND v_nombre IS NULL THEN
            CALL fn_mensaje("Aviso","Se requiere capturar el Nombre del derechohabiente","stop")
            CONTINUE CONSTRUCT 
         END IF

         IF v_app_paterno IS NULL AND v_nombre IS NOT NULL THEN
            CALL fn_mensaje("Aviso","Se requiere capturar el Apellido paterno del derechohabiente","stop")
            CONTINUE CONSTRUCT
         END IF

         EXIT CONSTRUCT

      ON ACTION CANCEL 
         LET v_bandera = FALSE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF v_bandera = TRUE THEN 
      LET v_sqlqry = " SELECT COUNT(*)",
                     "   FROM afi_derechohabiente ",
                     "  WHERE ",v_s_condicion
   
      PREPARE prp_cta_nombre FROM v_sqlqry
      EXECUTE prp_cta_nombre INTO v_cta_nombre

      IF v_cta_nombre = 0 THEN 
         CALL fn_mensaje("Aviso","No existe información del derechohabiente","stop")
         LET v_bandera = FALSE 
      ELSE 
         IF v_cta_nombre > 1 THEN 
            --consulta listado de derechohabientes de afi para obtener el id derechohab
            CALL fn_obt_derechohabiente(v_s_condicion) RETURNING v_id_derechohabiente

            IF v_id_derechohabiente = -1 THEN
               LET v_bandera = FALSE
            ELSE
               SELECT COUNT(*) 
                 INTO v_cta_reg
                 FROM dse_agrupa_devolucion
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND tpo_transferencia = g_tpo_transferencia

               IF v_cta_reg = 0 THEN 
                  CALL fn_mensaje("Aviso","El derechohabiente no tiene créditos otorgados","stop")
                  LET v_bandera = FALSE
               END IF
            END IF
         ELSE
             --consulta id_derehohabiente
             LET v_sqlqry = " SELECT id_derechohabiente ",
                            "   FROM afi_derechohabiente ",
                            "  WHERE ", v_s_condicion
             PREPARE prp_con_id FROM v_sqlqry
             EXECUTE prp_con_id INTO v_id_derechohabiente
             DISPLAY "DERECHOHABIENTE ",  v_id_derechohabiente

             SELECT COUNT(*) 
               INTO v_cta_reg
               FROM dse_agrupa_devolucion
              WHERE id_derechohabiente = v_id_derechohabiente
                AND tpo_transferencia = g_tpo_transferencia

             IF v_cta_reg = 0 THEN 
               CALL fn_mensaje("Aviso","El derechohabiente no tiene créditos otorgados","stop")
               LET v_bandera = FALSE
             END IF
         END IF 
         --LET v_bandera = TRUE
      END IF

      IF v_bandera = TRUE THEN
         --se llama la funcion que despliega la consulta
         CALL fn_despliega_derechohab_dev(v_id_derechohabiente)
      END IF
   END IF

   CLOSE WINDOW w_op_derechohab

END FUNCTION

## Funcion que realiza la consulta de estado ##
FUNCTION fn_consulta_estado_dev()

   DEFINE v_s_condicion        STRING
   DEFINE estado               LIKE dse_agrupa_devolucion.estado
   DEFINE edo_procesar         LIKE dse_agrupa_devolucion.edo_procesar
   DEFINE ban_salir            SMALLINT 

   LET v_s_condicion = "1=1"
   LET ban_salir = FALSE

   OPEN WINDOW w_op_estado WITH FORM "AGRC044"

   CONSTRUCT v_s_condicion
          ON estado,
             edo_procesar
        FROM cmb_estado,
             cmb_procesar

      AFTER FIELD cmb_procesar
         NEXT FIELD cmb_estado

      ON ACTION ACCEPT
         CALL GET_FLDBUF(cmb_estado) RETURNING estado
         CALL GET_FLDBUF(cmb_procesar) RETURNING edo_procesar 

         IF estado IS NULL AND edo_procesar IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar al menos un campo","stop")
            CONTINUE CONSTRUCT
         END IF
         EXIT CONSTRUCT

      ON ACTION CANCEL
         LET ban_salir = TRUE
         EXIT CONSTRUCT

   END CONSTRUCT

   IF ban_salir = FALSE THEN
      CALL fn_despliega_estado_dev(v_s_condicion)
   END IF

   CLOSE WINDOW w_op_estado

END FUNCTION

## Funcion que realiza la consulta de historica ##
FUNCTION fn_consulta_historica_dev()

   DEFINE v_s_condicion        STRING
   DEFINE estado               LIKE dse_his_devolucion.estado
   DEFINE edo_procesar         LIKE dse_his_devolucion.edo_procesar
   DEFINE v_diagnostico        LIKE dse_his_devolucion.diagnostico
   DEFINE cb                   ui.ComboBox
   DEFINE v_sqlqry             STRING
   DEFINE v_cve_diag           LIKE dse_his_devolucion.diagnostico
   DEFINE v_desc_diag          LIKE cat_rechazo.desc_rechazo
   DEFINE v_edo_procesar       LIKE dse_his_devolucion.edo_procesar
   DEFINE ban_salir            SMALLINT

   LET v_s_condicion = "1=1"
   LET ban_salir = FALSE

   OPEN WINDOW w_op_historica WITH FORM "AGRC046"
   CONSTRUCT v_s_condicion
          ON estado,
             edo_procesar,
             diagnostico
        FROM cmb_estado,
             cmb_procesar,
             cmb_diagnostico

      BEFORE CONSTRUCT 
          LET cb = ui.ComboBox.forName("cmb_diagnostico")
          CALL cb.clear()
              LET v_sqlqry = " SELECT diagnostico, edo_procesar ",
                             "   FROM dse_his_devolucion ",
                             "  WHERE edo_procesar NOT IN ('10','05','30','60','120','210') ",
                             "    AND diagnostico IS NOT NULL ",
                             "  GROUP BY 1,2 "

          PREPARE prp_cons_diag FROM v_sqlqry
          DECLARE cur_diagnostico CURSOR FOR prp_cons_diag
          FOREACH cur_diagnostico INTO v_cve_diag, v_edo_procesar
             LET v_desc_diag = fn_obt_desc_diagnostico(v_cve_diag,v_edo_procesar)
             CALL cb.addItem(v_cve_diag,v_cve_diag ||"-"||v_desc_diag)
          END FOREACH

      AFTER FIELD cmb_diagnostico
         NEXT FIELD cmb_estado

      ON ACTION ACCEPT
         CALL GET_FLDBUF(cmb_estado) RETURNING estado
         CALL GET_FLDBUF(cmb_procesar) RETURNING edo_procesar
         CALL GET_FLDBUF(cmb_diagnostico) RETURNING v_diagnostico

         IF estado IS NULL AND edo_procesar IS NULL AND  v_diagnostico IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar al menos un campo","stop")
            CONTINUE CONSTRUCT
         END IF

         EXIT CONSTRUCT

      ON ACTION CANCEL
         LET ban_salir = TRUE
         EXIT CONSTRUCT

   END CONSTRUCT

   IF ban_salir = FALSE THEN
      CALL fn_despliega_historica_dev(v_s_condicion)
   END IF

   CLOSE WINDOW w_op_historica

END FUNCTION

### Funcion que permite seleccionar el archivo a consultar
FUNCTION fn_consulta_archivo_dev()

   DEFINE v_nom_archivo             LIKE cre_ctr_archivo.nom_archivo --nombre del archivo
   DEFINE v_folio                   LIKE dse_ctr_archivo.folio --folio del archivo
   DEFINE v_folio_cmb               LIKE dse_ctr_archivo.folio --identificador del archivo para el combo
   DEFINE cb                        ui.ComboBox
   DEFINE v_sqlqry                  STRING
   DEFINE v_tpo_transferencia       LIKE dse_ctr_archivo.tpo_transferencia

   OPEN WINDOW w_op_archivo WITH FORM "AGRC049"

   INPUT v_folio FROM cmb_archivo ATTRIBUTE (UNBUFFERED)
      AFTER FIELD cmb_archivo
         NEXT FIELD cmb_archivo

      BEFORE INPUT
         LET v_tpo_transferencia = "43" -- 43-DSE Anualidades Garantizadas

         LET cb = ui.ComboBox.forName("cmb_archivo")
         CALL cb.clear()
         --se consultan los archivos para el proceso correspondiente
         LET v_sqlqry = " SELECT nom_archivo, folio ",
                        "   FROM dse_ctr_archivo ",
                        "  WHERE tpo_transferencia =  ",v_tpo_transferencia

          PREPARE prp_cons_arch FROM v_sqlqry
          DECLARE cur_archivo CURSOR FOR prp_cons_arch

          --se llena el combo con los archivos existentes
          FOREACH cur_archivo INTO v_nom_archivo, v_folio_cmb
             CALL cb.addItem(v_folio_cmb,v_nom_archivo)
          END FOREACH

      ON ACTION ACCEPT
         --se valida que se seleccione un archivo
         IF v_folio IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar el archivo a consultar","stop")
            CONTINUE INPUT
         ELSE
            SELECT nom_archivo
              INTO v_nom_archivo
              FROM dse_ctr_archivo
             WHERE folio = v_folio
               AND tpo_transferencia =  v_tpo_transferencia
               AND estado = 20

            CALL fn_despliega_archivo_dev(v_folio, v_nom_archivo)
         END IF

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT

   CLOSE WINDOW w_op_archivo

END FUNCTION

###Funcion que muestra los registros encontrados para el archivo consultado
FUNCTION fn_despliega_archivo_dev(p_folio, p_nom_archivo)

   DEFINE p_folio               LIKE dse_ctr_archivo.folio
   DEFINE p_nom_archivo         LIKE dse_ctr_archivo.nom_archivo --nombre del archivo
   DEFINE v_sqlqry              STRING
   DEFINE v_cta_reg             INTEGER
   DEFINE v_indice              SMALLINT
   DEFINE v_estado              LIKE dse_agrupa_devolucion.estado
   DEFINE v_edo_procesar        LIKE dse_agrupa_devolucion.edo_procesar

   DEFINE v_rec_archivo RECORD
      estado                    LIKE dse_agrupa_devolucion.estado,
      edo_procesar              LIKE dse_agrupa_devolucion.edo_procesar,
      total_cuentas             INTEGER 
   END RECORD

   DEFINE v_ar_archivo DYNAMIC ARRAY OF RECORD
      des_estado                LIKE cat_maq_credito.estado_desc,
      des_edo_procesar          LIKE cat_maq_credito.estado_desc,
      total_cuentas             INTEGER
   END RECORD

   DEFINE v_estado_aux          LIKE dse_agrupa_devolucion.estado
   DEFINE v_edo_procesar_aux    LIKE dse_agrupa_devolucion.edo_procesar
   DEFINE v_manejador_rpt       OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin            LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte         VARCHAR(80) -- nombre del reporte
   DEFINE r_ruta_listados       LIKE seg_modulo.ruta_listados
   DEFINE v_indice_rep          SMALLINT
   DEFINE f_w                   ui.form
   DEFINE w                     ui.window
   DEFINE v_existe_archivo      INTEGER

   DISPLAY "FOLIO " ,p_folio
   -- se verifica si la información ya fue agrupada
   LET v_sqlqry = " SELECT COUNT(*)\n",
                  "   FROM dse_devolucion\n",
                  "  WHERE tpo_transferencia = ",g_tpo_transferencia,"\n",
                  "    AND folio_referencia = ",p_folio,"\n",
                  "    AND estado = 15" -- estado Agrupado

   PREPARE prp_folio_agrp FROM v_sqlqry
   EXECUTE prp_folio_agrp INTO v_cta_reg

   -- si no se econtraron registro manda aviso a usuario
   IF v_cta_reg = 0 THEN
      CALL fn_mensaje("Aviso","No existen registros agrupados para el folio seleccionado","stop")

      RETURN
   END IF

   OPEN WINDOW w_des_archivo WITH FORM "AGRC045"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm() 

      DISPLAY ARRAY v_ar_archivo TO tabla_estado.* ATTRIBUTES (UNBUFFERED)
         BEFORE DISPLAY
            LET v_sqlqry = " SELECT COUNT(*), estado, edo_procesar\n",
                           "   FROM dse_agrupa_devolucion\n",
                           "  WHERE id_derechohabiente IN (\n",
                           "        SELECT id_derechohabiente\n",
                           "          FROM dse_devolucion\n",
                           "         WHERE tpo_transferencia = ",g_tpo_transferencia,"\n",
                           "           AND folio_referencia = ",p_folio,"\n",
                           "           AND estado = 15)\n",
                           "    AND tpo_transferencia = ",g_tpo_transferencia,"\n",
                           "  GROUP BY 2,3"

            PREPARE prp_cons_archivo FROM v_sqlqry
            DECLARE cur_arch CURSOR FOR prp_cons_archivo

            LET v_indice = 1
            CALL v_ar_archivo.clear()

            OPEN cur_arch

            FOREACH cur_arch INTO v_rec_archivo.total_cuentas,
                                  v_rec_archivo.estado,
                                  v_rec_archivo.edo_procesar

               -- se asignan los valores en el arreglo
               LET v_ar_archivo[v_indice].total_cuentas = v_rec_archivo.total_cuentas
               LET v_ar_archivo[v_indice].des_estado = fn_obt_desc_estado(v_rec_archivo.estado)
               LET v_ar_archivo[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_archivo.edo_procesar)
               LET v_indice = v_indice + 1
            END FOREACH 

            FREE cur_arch

            IF v_indice = 1 THEN 
               CALL fn_mensaje("Aviso","No existen registros para el archivo seleccionado","stop")
               EXIT DISPLAY
            END IF

         ON ACTION ACCEPT
            CALL v_ar_archivo.CLEAR()
            EXIT DISPLAY

         ON ACTION CANCEL
            CALL v_ar_archivo.CLEAR()
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC044.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC044-","00000","-","00000","-","00000"
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)

               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_archivo TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_archivo(v_ar_archivo[v_indice_rep].*, p_nom_archivo)
               END FOR

               #Finaliza el reporte
               FINISH REPORT reporte_cons_archivo
                LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()
                 CALL fn_mensaje("Aviso","Se ha generado el reporte por archivo","info") 
              ELSE
                 DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
              END IF
      END DISPLAY

   CLOSE WINDOW w_des_archivo

END FUNCTION

#### Funcion que muestra un listado de derechohabientes ### 
#### para elegir el que se desea consultar              ###
FUNCTION fn_obt_derechohabiente(p_s_condicion)

   DEFINE p_s_condicion         STRING
   DEFINE v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_sqlqry              STRING
   DEFINE v_indice              SMALLINT

   DEFINE v_arr_derechohabiente DYNAMIC ARRAY OF RECORD
      id_derechohabiente        LIKE afi_derechohabiente.id_derechohabiente,
      nss                       LIKE afi_derechohabiente.nss,
      curp                      LIKE afi_derechohabiente.curp,
      rfc                       LIKE afi_derechohabiente.rfc,
      ind_nrp                   LIKE afi_derechohabiente.ind_nrp,
      f_nacimiento              LIKE afi_derechohabiente.f_nacimiento,
      nombre_imss               LIKE afi_derechohabiente.nombre_imss,
      nombre_af                 LIKE afi_derechohabiente.nombre_af,
      ap_paterno_af             LIKE afi_derechohabiente.ap_materno_af,
      ap_materno_af             LIKE afi_derechohabiente.ap_paterno_af,
      tipo_trabajador           LIKE afi_derechohabiente.tipo_trabajador,
      origen_afiliacion         LIKE afi_derechohabiente.origen_afiliacion,
      id_credito                LIKE afi_derechohabiente.id_credito,
      f_credito                 LIKE afi_derechohabiente.f_credito
   END RECORD

   LET v_indice = 1
   LET v_sqlqry = " SELECT id_derechohabiente,nss,curp,rfc,ind_nrp,f_nacimiento, ",
                  "        nombre_imss,nombre_af,ap_paterno_af,ap_materno_af,tipo_trabajador,",
                  "        origen_afiliacion,id_credito,f_credito ",
                  "   FROM afi_derechohabiente ",
                  "  WHERE ", p_s_condicion

   PREPARE prp_cons_derechohabiente FROM v_sqlqry

   DECLARE cur_derechohabiente CURSOR FOR prp_cons_derechohabiente

   FOREACH cur_derechohabiente INTO v_arr_derechohabiente[v_indice].*
      LET v_indice = v_indice + 1 
   END FOREACH    

   OPEN WINDOW w_list_derechohabiente WITH FORM "AGRC048" 

      DISPLAY ARRAY v_arr_derechohabiente TO tabla_derechohabiente.* ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )

         ON ACTION ACCEPT 
            LET v_indice = arr_curr()
            LET v_id_derechohabiente = v_arr_derechohabiente[v_indice].id_derechohabiente
            EXIT DISPLAY

         ON ACTION CANCEL
            LET v_id_derechohabiente = -1
            EXIT DISPLAY 

      END DISPLAY

   CLOSE WINDOW w_list_derechohabiente

   RETURN v_id_derechohabiente

END FUNCTION 

### Funcion que muestra el resultado de la consulta de derechohabiente ###
FUNCTION fn_despliega_derechohab_dev(p_id_derechohabiente)

   DEFINE p_id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_nss                      LIKE afi_derechohabiente.nss
   DEFINE v_curp                     LIKE afi_derechohabiente.curp
   DEFINE v_rfc                      LIKE afi_derechohabiente.rfc
   DEFINE v_ap_paterno               LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_ap_materno               LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_nombre                   LIKE afi_derechohabiente.nombre_af
   DEFINE v_tpo_credito              LIKE cre_acreditado.tpo_credito
   DEFINE v_num_credito              LIKE cre_acreditado.num_credito
   DEFINE v_f_movimiento             LIKE dse_agrupa_devolucion.f_movimiento
   DEFINE v_folio_liquida            LIKE dse_agrupa_devolucion.folio_liquida
   DEFINE v_estado                   LIKE cre_acreditado.estado
   DEFINE v_des_estado               LIKE cat_maq_credito.estado_desc
   DEFINE v_edo_procesar             LIKE cre_acreditado.edo_procesar
   DEFINE v_des_procesar             LIKE cat_maq_credito.estado_desc

   DEFINE v_rec_his_devolucion RECORD
      tpo_transferencia              LIKE dse_his_devolucion.tpo_transferencia,
      f_proceso                      LIKE dse_his_devolucion.f_proceso,
      estado                         LIKE dse_his_devolucion.estado,
      edo_procesar                   LIKE dse_his_devolucion.edo_procesar,
      diagnostico                    LIKE dse_his_devolucion.diagnostico,
      folio                          LIKE dse_his_devolucion.folio
   END RECORD

   DEFINE v_ar_his_devolucion DYNAMIC ARRAY OF RECORD
      tpo_transferencia              LIKE dse_his_devolucion.tpo_transferencia,
      f_proceso                      LIKE dse_his_devolucion.f_proceso,
      des_estado                     LIKE cat_maq_credito.estado_desc,
      des_edo_procesar               LIKE cat_maq_credito.estado_desc,
      des_diagnostico                LIKE cat_rechazo.desc_rechazo
   END RECORD

   DEFINE v_indice                   SMALLINT
   DEFINE v_indice_deudor            SMALLINT
   DEFINE v_indice_rep               SMALLINT

   DEFINE v_rec_movimiento RECORD
      movimiento                     LIKE cta_movimiento.movimiento,
      des_movimiento                 LIKE cat_movimiento.movimiento_desc,
      f_movimiento                   LIKE cta_movimiento.f_liquida,
      monto_aivs                     LIKE cta_movimiento.monto_acciones,
      monto_pesos                    LIKE cta_movimiento.monto_pesos
   END RECORD

   DEFINE v_ar_movimiento DYNAMIC ARRAY OF RECORD
      des_movimiento                 LIKE cat_movimiento.movimiento_desc,
      f_movimiento                   LIKE cta_movimiento.f_liquida,
      monto_aivs                     LIKE cta_movimiento.monto_acciones,
      monto_pesos                    LIKE cta_movimiento.monto_pesos
   END RECORD

   DEFINE v_tpo_enc                  SMALLINT -- tipo de encabezado que aparecerá en el reporte
   DEFINE v_sqlqry                   STRING
   DEFINE v_id_dse_grp_devolucion    LIKE dse_agrupa_devolucion.id_dse_grp_devolucion
   DEFINE v_lsqry                    CHAR(500)
   DEFINE v_des_tpo_credito          LIKE cat_tipo_credito.desc_credito
   DEFINE v_manejador_rpt            OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin                 LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte              VARCHAR(80) -- nombre del reporte
   DEFINE f_w                        ui.form
   DEFINE w                          ui.window
   DEFINE v_existe_archivo           INTEGER
   DEFINE r_ruta_listados            LIKE seg_modulo.ruta_listados
   DEFINE v_tabla                    CHAR(20)

  SELECT nss,curp,rfc,ap_paterno_af, ap_materno_af, nombre_af
    INTO v_nss,v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre
    FROM afi_derechohabiente 
   WHERE id_derechohabiente = p_id_derechohabiente 

  SELECT nss,curp,rfc,ap_paterno_af, ap_materno_af, nombre_af
    INTO v_nss,v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre
    FROM afi_derechohabiente 
   WHERE id_derechohabiente = p_id_derechohabiente 

  LET v_f_movimiento = ""  
  LET v_sqlqry = " SELECT FIRST 1 f_movimiento,folio_liquida,estado,edo_procesar,id_dse_grp_devolucion ",
                 "   FROM dse_agrupa_devolucion ",
                 "  WHERE id_derechohabiente = ",p_id_derechohabiente

  PREPARE prp_cons_transf FROM v_sqlqry
  EXECUTE prp_cons_transf INTO v_f_movimiento,v_folio_liquida,v_estado,v_edo_procesar,v_id_dse_grp_devolucion

  SELECT estado_desc
    INTO v_des_estado
    FROM cat_maq_credito
   WHERE estado = v_estado

  SELECT estado_desc
    INTO v_des_procesar
    FROM cat_maq_credito
   WHERE estado = v_edo_procesar

   -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
   LET v_sqlqry = " SELECT FIRST 1 desc_credito\n",
                  "   FROM cat_tipo_credito\n",
                  "  WHERE tpo_credito = ",v_tpo_credito

   PREPARE prp_slctFrst_descCred FROM v_sqlqry
   EXECUTE prp_slctFrst_descCred INTO v_des_tpo_credito

   OPEN WINDOW w_des_derechohab WITH FORM "AGRC043"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()

      DISPLAY --p_id_derechohabiente,
              v_nss,
              v_curp,
              v_rfc,
              v_ap_paterno,
              v_ap_materno,
              v_nombre,
              v_f_movimiento USING "dd-mm-yyyy",
              v_des_estado,
              v_des_procesar,
              v_folio_liquida
           TO --id_derechohabiente,
              nss,
              curp,
              rfc,
              app_paterno,
              app_materno,
              nombre,
              f_movimiento,
              estado,
              edo_procesar,
              folio_liquida

         DIALOG ATTRIBUTES (FIELD ORDER FORM, UNBUFFERED)
         DISPLAY ARRAY v_ar_his_devolucion TO tabla_devolucion.*

            BEFORE DISPLAY

            LET v_lsqry = "SELECT tpo_transferencia,f_proceso,estado,edo_procesar,diagnostico",
                          "  FROM dse_his_devolucion ",
                          " WHERE id_dse_grp_devolucion = ", v_id_dse_grp_devolucion,
                          "   AND tpo_transferencia = ",g_tpo_transferencia

            PREPARE prp_cons_his FROM v_lsqry
            DECLARE cur_his_dev CURSOR FOR prp_cons_his

            DISPLAY "CONSULTA HIST ", v_lsqry
            LET v_indice = 1

               OPEN cur_his_dev
               FOREACH cur_his_dev INTO v_rec_his_devolucion.*
                  LET v_ar_his_devolucion[v_indice].tpo_transferencia = v_rec_his_devolucion.tpo_transferencia
                  LET v_ar_his_devolucion[v_indice].f_proceso = v_rec_his_devolucion.f_proceso
                  LET v_ar_his_devolucion[v_indice].des_estado = fn_obt_desc_estado(v_rec_his_devolucion.estado)
                  LET v_ar_his_devolucion[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_his_devolucion.edo_procesar)
                  LET v_ar_his_devolucion[v_indice].des_diagnostico = fn_obt_desc_diagnostico(v_rec_his_devolucion.diagnostico, v_rec_his_devolucion.edo_procesar)

                  LET v_indice = v_indice + 1
               END FOREACH 

               FREE cur_his_dev

               --VPD relacion  o tabla para obtener el saldo deudor
               LET v_indice_deudor = 1

            SELECT t1.tabla
              FROM cat_tab_movimiento t1
            INTO TEMP tmp_cat_tab_movimiento

            INSERT INTO tmp_cat_tab_movimiento
            VALUES ("cta_movimiento")

            DECLARE cur_cta_mov CURSOR FOR
            SELECT t2.tabla
              FROM tmp_cat_tab_movimiento t2

            FOREACH cur_cta_mov INTO v_tabla
               LET v_lsqry = " SELECT c.movimiento, d.movimiento_desc, c.f_liquida, c.monto_acciones, c.monto_pesos",
                             "   FROM ",v_tabla," c, cat_movimiento d",
                             "  WHERE c.id_derechohabiente = ",p_id_derechohabiente,
                             "    AND c.movimiento IN(21,91)",
                             "    AND c.movimiento = d.movimiento"

               DISPLAY "CONSULTA DEUDOR ",v_lsqry
               PREPARE prp_cons_mov FROM v_lsqry
               DECLARE cur_mov CURSOR FOR prp_cons_mov

               OPEN cur_mov
               FOREACH cur_mov INTO v_rec_movimiento.*
                  LET v_ar_movimiento[v_indice_deudor].des_movimiento = v_rec_movimiento.movimiento
                  LET v_ar_movimiento[v_indice_deudor].f_movimiento   = v_rec_movimiento.f_movimiento --USING "dd-mm-yyyy"
                  LET v_ar_movimiento[v_indice_deudor].monto_aivs     = v_rec_movimiento.monto_aivs
                  LET v_ar_movimiento[v_indice_deudor].monto_pesos    = v_rec_movimiento.monto_pesos

                  LET v_indice_deudor = v_indice_deudor + 1
               END FOREACH
               CLOSE cur_mov
               FREE cur_mov
            END FOREACH
            CLOSE cur_cta_mov 
            FREE cur_cta_mov

         END DISPLAY

         DISPLAY ARRAY v_ar_movimiento TO tabla_movimiento.*
         END DISPLAY

         ON ACTION ACCEPT
            EXIT DIALOG

         ON ACTION cancelar
            EXIT DIALOG

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

         # Se indica que el reporte usara la plantilla creada
         IF fgl_report_loadCurrentSettings("AGRC041.4rp") THEN 
            CALL fgl_report_selectDevice("PDF") 
            LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC041-","00000","-","00000","-","00000" 
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

            -- sin preview
            CALL fgl_report_selectPreview(0)

            LET v_manejador_rpt = fgl_report_commitCurrentSettings()

            # Inicia el reporte de consulta historica por derechohabiente
            START REPORT reporte_cons_derechohabiente TO XML HANDLER v_manejador_rpt

            -- se procesan los registro de cre historico
            FOR v_indice_rep = 1 TO v_indice -1
               -- indica que el encabezado debe ser el de historico
               LET v_tpo_enc = 1
               OUTPUT TO REPORT reporte_cons_derechohabiente(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                             v_ap_paterno, v_ap_materno, v_nombre,
                                                             v_des_tpo_credito, v_num_credito, v_f_movimiento,
                                                             v_ar_his_devolucion[v_indice_rep].*,
                                                             v_ar_movimiento[1].*, v_tpo_enc)
            END FOR

            -- se procesan los registro de cre deudor
            FOR v_indice_rep = 1 TO v_indice_deudor -1
               -- indica que el encabezado debe ser el de deudor
               LET v_tpo_enc = 2
               OUTPUT TO REPORT reporte_cons_derechohabiente(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                             v_ap_paterno, v_ap_materno, v_nombre,
                                                             v_des_tpo_credito, v_num_credito, v_f_movimiento,
                                                             v_ar_his_devolucion[1].*,
                                                             v_ar_movimiento[v_indice_rep].*, v_tpo_enc)
            END FOR

            #Finaliza el reporte
            FINISH REPORT reporte_cons_derechohabiente
            LET v_existe_archivo = 1

            IF(LENGTH(r_ruta_listados) > 0)THEN
               # se revisa si existe el archivo en la ruta de listados
               CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
            END IF

            # si no existe el archivo, se oculta la imagen link que visualiza el pdf
            IF NOT(v_existe_archivo)THEN
               CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
            ELSE
               CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
            END IF

            # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
            DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
               CALL ui.Interface.refresh()
            CALL fn_mensaje("Aviso","Se ha generado el reporte por derechohabiente","info") 
         ELSE
            DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
         END IF

      CONTINUE DIALOG

   END DIALOG

   CLOSE WINDOW w_des_derechohab

END FUNCTION

### funcion que despliega la informacion de la consulta por estado ###
FUNCTION fn_despliega_estado_dev(p_s_condicion)

   DEFINE p_s_condicion       STRING
   DEFINE v_sqlqry            STRING
   DEFINE v_cta_reg           INTEGER
   DEFINE v_indice            SMALLINT
   DEFINE v_estado            LIKE dse_agrupa_devolucion.estado
   DEFINE v_edo_procesar      LIKE dse_agrupa_devolucion.edo_procesar

   DEFINE v_rec_estado RECORD
      estado                  LIKE dse_agrupa_devolucion.estado,
      edo_procesar            LIKE dse_agrupa_devolucion.edo_procesar,
      total_cuentas           INTEGER
   END RECORD

   DEFINE v_ar_estado DYNAMIC ARRAY OF RECORD
      des_estado              LIKE cat_maq_credito.estado_desc,
      des_edo_procesar        LIKE cat_maq_credito.estado_desc,
      total_cuentas           INTEGER
   END RECORD

   DEFINE v_estado_aux        LIKE dse_agrupa_devolucion.estado
   DEFINE v_edo_procesar_aux  LIKE dse_agrupa_devolucion.edo_procesar
   DEFINE v_manejador_rpt     OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin          LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte       VARCHAR(80) -- nombre del reporte
   DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados
   DEFINE f_w                 ui.form
   DEFINE w                   ui.window
   DEFINE v_existe_archivo    INTEGER
   DEFINE v_indice_rep        SMALLINT

   LET v_sqlqry = " SELECT COUNT (*)",
                  "   FROM dse_agrupa_devolucion", 
                  "  WHERE ",p_s_condicion,
                  "    AND tpo_transferencia = ",g_tpo_transferencia

   PREPARE prp_cta_reg FROM v_sqlqry
   EXECUTE prp_cta_reg INTO v_cta_reg

   OPEN WINDOW w_des_estado WITH FORM "AGRC045"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()
      LET v_sqlqry = " SELECT estado,edo_procesar",
                     "   FROM dse_agrupa_devolucion", 
                     "  WHERE ",p_s_condicion,
                     "    AND tpo_transferencia = ",g_tpo_transferencia,
                     "  GROUP BY 1,2 "

      PREPARE prp_cons_edos FROM v_sqlqry
      DECLARE cur_edos CURSOR FOR prp_cons_edos
      INITIALIZE  v_estado_aux, v_edo_procesar_aux TO NULL

      LET v_indice = 1
      CALL v_ar_estado.clear()
      OPEN cur_edos
      FOREACH cur_edos INTO v_estado, v_edo_procesar

         IF v_estado_aux = v_estado AND v_edo_procesar_aux = v_edo_procesar THEN 
            --DISPLAY "Salta registro"
         ELSE
            LET v_estado_aux = v_estado
            LET v_edo_procesar_aux = v_edo_procesar

            SELECT estado, edo_procesar, COUNT(*)
              INTO v_rec_estado.estado, v_rec_estado.edo_procesar,v_rec_estado.total_cuentas
              FROM dse_agrupa_devolucion
             WHERE tpo_transferencia = g_tpo_transferencia
               AND estado = v_estado_aux
               AND edo_procesar = v_edo_procesar_aux
             GROUP BY 1,2

            LET v_ar_estado[v_indice].total_cuentas = v_rec_estado.total_cuentas
            LET v_ar_estado[v_indice].des_estado = fn_obt_desc_estado(v_rec_estado.estado)
            LET v_ar_estado[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_estado.edo_procesar)
            LET v_indice = v_indice + 1
        END IF

      END FOREACH

      FREE cur_edos

      DISPLAY ARRAY v_ar_estado TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY 
         IF v_cta_reg = 0 THEN 
            CALL fn_mensaje("Aviso","No existen registos para los criterios seleccionados","stop")
            EXIT DISPLAY
         END IF

         ON ACTION ACCEPT
            CALL v_ar_estado.CLEAR()
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_estado.CLEAR()
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC042.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC042-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_estado TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_estado(v_ar_estado[v_indice_rep].*)
               END FOR

               #Finaliza el reporte
               FINISH REPORT reporte_cons_estado
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()      
                CALL fn_mensaje("Aviso","Se ha generado el reporte por estado","info") 
            ELSE
                DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF

      END DISPLAY

   CLOSE WINDOW w_des_estado

END FUNCTION

### Funcion que despliega la consulta historica ###
FUNCTION fn_despliega_historica_dev(p_s_condicion)

   DEFINE p_s_condicion          STRING
   DEFINE v_sqlqry               STRING
   DEFINE v_cta_reg              INTEGER
   DEFINE v_indice               SMALLINT
   DEFINE v_folio                LIKE dse_his_devolucion.folio

   DEFINE v_rec_historica RECORD
      estado                     LIKE dse_his_devolucion.estado,
      edo_procesar               LIKE dse_his_devolucion.edo_procesar,
      diagnostico                LIKE dse_his_devolucion.diagnostico,
      total_cuentas              INTEGER,
      folio                      LIKE dse_his_devolucion.folio 
   END RECORD

   DEFINE v_ar_historica DYNAMIC ARRAY OF RECORD
      des_estado                 LIKE cat_maq_credito.estado_desc,
      des_edo_procesar           LIKE cat_maq_credito.estado_desc,
      des_diagnostico            LIKE cat_rechazo.desc_rechazo,
      total_cuentas              INTEGER
   END RECORD

   DEFINE v_manejador_rpt          OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin               LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte            VARCHAR(80) -- nombre del reporte
   DEFINE r_ruta_listados          LIKE seg_modulo.ruta_listados
   DEFINE f_w                      ui.form
   DEFINE w                        ui.window
   DEFINE v_existe_archivo         INTEGER
   DEFINE v_indice_rep             SMALLINT

   LET v_sqlqry = " SELECT COUNT (*)",
                  "   FROM dse_his_devolucion", 
                  "  WHERE id_dse_grp_devolucion IN (",
                  "        SELECT id_dse_grp_devolucion ",
                  "          FROM dse_agrupa_devolucion ",   
                  "         WHERE tpo_transferencia = ",g_tpo_transferencia, ")",
                  "    AND ",p_s_condicion

   PREPARE prp_cta_reg1 FROM v_sqlqry
   EXECUTE prp_cta_reg1 INTO v_cta_reg  

   OPEN WINDOW w_des_historica WITH FORM "AGRC047"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()  
      LET v_sqlqry = " SELECT estado,edo_procesar,diagnostico, count(*) ",
                     " FROM dse_his_devolucion", 
                     " WHERE id_dse_grp_devolucion in (SELECT id_dse_grp_devolucion ",
                     " FROM dse_agrupa_devolucion ",   
                     " WHERE tpo_transferencia = ",g_tpo_transferencia, ")",
                     " AND ",p_s_condicion,
                     " GROUP BY 1,2,3",
                     " ORDER BY 1,2,3"

                     DISPLAY "CONSULTA HISTORICA ", v_sqlqry
      PREPARE prp_cons_hist FROM v_sqlqry
      DECLARE cur_hist CURSOR FOR prp_cons_hist

      LET v_indice = 1
      OPEN cur_hist
      --FOREACH cur_hist INTO v_estado, v_edo_procesar, v_diagnostico,v_id_cre_ctr_archivo
      FOREACH cur_hist INTO v_rec_historica.estado, v_rec_historica.edo_procesar, v_rec_historica.diagnostico,v_rec_historica.total_cuentas

         LET v_ar_historica[v_indice].total_cuentas = v_rec_historica.total_cuentas
         LET v_ar_historica[v_indice].des_estado = fn_obt_desc_estado(v_rec_historica.estado)
         LET v_ar_historica[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_historica.edo_procesar)
         LET v_ar_historica[v_indice].des_diagnostico = fn_obt_desc_diagnostico(v_rec_historica.diagnostico, v_rec_historica.edo_procesar)
         LET v_indice = v_indice + 1

      END FOREACH
      FREE cur_hist

      DISPLAY ARRAY v_ar_historica TO tabla_historica.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY
         IF v_cta_reg = 0 THEN 
            CALL fn_mensaje("Aviso","No existen registos para los criterios seleccionados","stop")
            EXIT DISPLAY
         END IF

         ON ACTION ACCEPT
            CALL v_ar_historica.CLEAR()
            EXIT DISPLAY

         ON ACTION CANCEL
            CALL v_ar_historica.CLEAR()
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC043.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC043-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)

               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_historica TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_historica(v_ar_historica[v_indice_rep].*)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_historica 
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()

               CALL fn_mensaje("Aviso","Se ha generado el reporte de consulta histórica","info") 
            ELSE
               DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF
      END DISPLAY

   CLOSE WINDOW w_des_historica

END FUNCTION 

## Funcion que obtiene la descripcion de estado y estado procesar
FUNCTION fn_obt_desc_estado(p_cve_estado)

   DEFINE p_cve_estado LIKE cat_maq_credito.estado
   DEFINE v_des_estado LIKE cat_maq_credito.estado_desc

   SELECT estado_desc
     INTO v_des_estado
     FROM cat_maq_credito
    WHERE estado = p_cve_estado

   RETURN v_des_estado

END FUNCTION

##OBJETIVO: Obtiene la descripcion para el diagnostico
FUNCTION fn_obt_desc_diagnostico(p_diagnostico, p_edo_procesar)

   DEFINE p_diagnostico        LIKE cre_his_acreditado.diagnostico
   DEFINE p_edo_procesar       LIKE dse_his_devolucion.edo_procesar
   DEFINE v_desc_diagnostico   LIKE cat_rechazo.desc_rechazo
   DEFINE v_tpo_rechazo        LIKE cat_rechazo.tpo_rechazo

   CASE
      WHEN (p_edo_procesar = 40 OR p_edo_procesar = 50  OR p_edo_procesar = 90 OR
            p_edo_procesar =110 OR p_edo_procesar = 190 OR p_edo_procesar = 200)
         LET v_tpo_rechazo = "RCH"      
      WHEN p_edo_procesar = 100   
         LET v_tpo_rechazo = "DEV"
      WHEN p_edo_procesar = 150
         LET v_tpo_rechazo = "SIS"
      OTHERWISE 
         LET v_tpo_rechazo = NULL
   END CASE

   IF p_diagnostico = 0 THEN 
      LET v_desc_diagnostico = "N/A"
   ELSE      
      IF v_tpo_rechazo IS NOT NULL THEN
         SELECT desc_rechazo
           INTO v_desc_diagnostico
           FROM cat_rechazo
          WHERE tpo_rechazo = v_tpo_rechazo
            AND cod_rechazo = p_diagnostico
      END IF
   END IF

   IF v_desc_diagnostico IS NULL THEN
      LET v_desc_diagnostico = "N/A"
   END IF 

   RETURN v_desc_diagnostico

END FUNCTION

#OBJETIVO: Genera el reporte de consulta por derechohabiente
REPORT reporte_cons_derechohabiente(p_id_derechohabiente,p_nss,p_curp,p_rfc,p_ap_paterno,
                                    p_ap_materno,p_nombre,p_des_tpo_credito,p_num_credito,
                                    p_f_otorga,p_rec_his_devolucion,p_rec_deudor,p_tpo_enc)

   DEFINE p_id_derechohabiente     INTEGER
   DEFINE p_nss                    LIKE afi_derechohabiente.nss
   DEFINE p_curp                   LIKE afi_derechohabiente.curp
   DEFINE p_rfc                    LIKE afi_derechohabiente.rfc
   DEFINE p_ap_paterno             LIKE afi_derechohabiente.ap_paterno_af
   DEFINE p_ap_materno             LIKE afi_derechohabiente.ap_materno_af
   DEFINE p_nombre                 LIKE afi_derechohabiente.nombre_af
   DEFINE p_des_tpo_credito        LIKE cat_tipo_credito.desc_credito
   DEFINE p_num_credito            LIKE cre_acreditado.num_credito
   DEFINE p_f_otorga               LIKE cre_acreditado.f_otorga

   DEFINE p_rec_his_devolucion RECORD
      tpo_transferencia            LIKE dse_his_devolucion.tpo_transferencia,
      f_proceso                    LIKE cre_his_acreditado.f_proceso,
      des_estado                   LIKE cat_maq_credito.estado_desc,
      des_edo_procesar             LIKE cat_maq_credito.estado_desc,
      diagnostico                  LIKE cre_his_acreditado.diagnostico
   END RECORD

   DEFINE p_rec_deudor RECORD
      des_movimiento               LIKE cat_movimiento.movimiento_desc,
      f_movimiento                 LIKE cre_saldo_deudor.f_movimiento,
      monto_aivs                   LIKE cre_saldo_deudor.monto_aivs,
      monto_pesos                  LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE p_tpo_enc                SMALLINT -- tipo de encabezado que aparecerá en el reporte
   DEFINE v_v_desc_detalle         VARCHAR(50)
   DEFINE v_fecha_reporte          DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod
      --PRINTX p_id_derechohabiente
      PRINTX p_nss
      PRINTX p_curp
      PRINTX p_rfc
      PRINTX p_ap_paterno
      PRINTX p_ap_materno
      PRINTX p_nombre
      PRINTX p_des_tpo_credito
      PRINTX p_num_credito USING "&&&&&&&&&&"
      PRINTX p_f_otorga USING "dd-mm-yyyy"

   BEFORE GROUP OF p_tpo_enc
      IF p_tpo_enc = 1 THEN
         LET v_v_desc_detalle = "TRANSFERENCIA"
      ELSE
         LET v_v_desc_detalle = "DEUDOR"
      END IF
      PRINT p_tpo_enc
      PRINTX v_v_desc_detalle

   ON EVERY ROW
      PRINTX p_rec_his_devolucion.f_proceso USING "dd-mm-yyyy"
      PRINTX p_rec_his_devolucion.des_estado
      PRINTX p_rec_his_devolucion.des_edo_procesar
      PRINTX p_rec_his_devolucion.diagnostico
      PRINTX p_rec_deudor.des_movimiento
      PRINTX p_rec_deudor.f_movimiento USING "dd-mm-yyyy"
      PRINTX p_rec_deudor.monto_aivs
      PRINTX p_rec_deudor.monto_pesos

END REPORT

#OBJETIVO: Genera el reporte de consulta por estado
REPORT reporte_cons_estado(p_rec_estado)

   DEFINE p_rec_estado RECORD 
          des_estado       LIKE cat_maq_credito.estado_desc,
          des_edo_procesar LIKE cat_maq_credito.estado_desc,
          total_cuentas    INTEGER
   END RECORD

   DEFINE v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod

   ON EVERY ROW
      PRINTX p_rec_estado.*

END REPORT

#OBJETIVO: Genera el reporte de consulta historica
REPORT reporte_cons_historica(p_rec_historica)

   DEFINE p_rec_historica RECORD
      des_estado               LIKE cat_maq_credito.estado_desc,
      des_edo_procesar         LIKE cat_maq_credito.estado_desc,
      des_diagnostico          LIKE cat_rechazo.desc_rechazo,
      total_cuentas            INTEGER 
   END RECORD

   DEFINE v_fecha_reporte      DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod

   ON EVERY ROW
      PRINTX p_rec_historica.*

END REPORT

#OBJETIVO: Genera el reporte de consulta por archivo
REPORT reporte_cons_archivo(p_rec_archivo, p_nom_archivo)

   DEFINE p_rec_archivo RECORD
      des_estado               LIKE cat_maq_credito.estado_desc,
      des_edo_procesar         LIKE cat_maq_credito.estado_desc,
      total_cuentas            INTEGER 
   END RECORD

   DEFINE v_fecha_reporte      DATE
   DEFINE p_nom_archivo        LIKE dse_ctr_archivo.nom_archivo --nombre del archivo

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod
      PRINTX p_nom_archivo

   ON EVERY ROW
      PRINTX p_rec_archivo.*

END REPORT