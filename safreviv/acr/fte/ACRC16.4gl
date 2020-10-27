--===============================================================
-- Versión: 1.0.0
-- Fecha última modificación:
--===============================================================

######################################################################
#Módulo             => AGR                                           #
#Programa           => ACRC16                                        #
#Objetivo           => Programa que realiza la consulta de deudor    #
#Autor              => Daniel Buendia, EFP                           #
#Fecha inicio       => 03 Octubre 2012                               #
#Modifica           => Mauro Muñiz Caballero                         #
#Fecha modificación => 27 de julio de 2016                           #
######################################################################

IMPORT OS

DATABASE safre_viv

GLOBALS

   DEFINE g_pid                     LIKE bat_ctr_proceso.pid --  ID del proceso
   DEFINE g_proceso_cod             LIKE cat_proceso.proceso_cod -- codigo del proceso
   DEFINE g_opera_cod               LIKE cat_operacion.opera_cod -- codigo de operacion
   DEFINE g_usuario_cod             LIKE seg_usuario.usuario_cod -- clave del usuario firmado
   DEFINE g_tpo_originacion         LIKE cre_acreditado.tpo_originacion --tipo de originacion
   DEFINE g_aviso                   CHAR(15) 

END GLOBALS

MAIN

   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING   -- título de la ventana
   DEFINE v_b_salir                 SMALLINT -- bandera que indica si se debe o no mostrar la ventana principal

   -- se recupera la clave de usuario desde parámetro
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se inicializan variables
   LET g_tpo_originacion = 4 -- Transferencia de Acreditados 
   LET v_b_salir         = TRUE
   LET g_aviso           = "Consulta deudor" 

   -- se crea el archivo log
   CALL STARTLOG(g_usuario_cod CLIPPED|| ".ACRC16.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- 30/05/2013 04:38:26 p.m.
   -- Ya no se realizarán consultas generales. Únicamente se harán consultas por derechohabiente
   -- se invoca la función general de consulta de histórico de acreditados
   --CALL fn_consulta_deudor()

   WHILE ( v_b_salir )
      -- se invoca la función que realiza la consulta por derechohabiente
      CALL fn_consulta_derechohabiente() RETURNING v_b_salir

      IF v_b_salir = FALSE THEN
         EXIT WHILE
      END IF
   END WHILE

END MAIN

#Objetivo: Función que realiza la consulta por derechohabiente
FUNCTION fn_consulta_derechohabiente()

   DEFINE v_s_condicion             STRING
   DEFINE v_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_curp                    LIKE afi_derechohabiente.curp
   DEFINE v_rfc                     LIKE afi_derechohabiente.rfc
   DEFINE v_app_paterno             LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_app_materno             LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_nombre                  LIKE afi_derechohabiente.nombre_af
   DEFINE v_cta_nombre              SMALLINT
   DEFINE v_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_d_num_credito           LIKE cre_acreditado.num_credito
   DEFINE v_id_cre_acred            DECIMAL(9,0)
   DEFINE v_sqlqry                  STRING
   DEFINE v_bandera                 SMALLINT
   DEFINE v_cta_reg                 SMALLINT
   DEFINE f_w                       ui.form  
   DEFINE w                         ui.Window 

   LET v_s_condicion = "1=1"
   LET v_bandera     = TRUE 

   OPEN WINDOW w_op_derechohab WITH FORM "ACRC164"
      LET w = ui.window.getCurrent()
        LET f_w = w.getForm()

   CONSTRUCT v_s_condicion
          ON nss
        FROM nss

      AFTER FIELD nss
         NEXT FIELD nss

      ON ACTION ACCEPT
         LET v_nss = GET_FLDBUF(nss)

         IF v_nss IS NOT NULL THEN
            IF fn_valida_caracteres(v_nss) <> 0 THEN 
               CALL fn_mensaje(g_aviso, "El carácter '*' no es válido en los parámetros de búsqueda", "about")
               NEXT FIELD nss
            END IF
         ELSE
           CALL fn_mensaje(g_aviso,"Se requiere capturar el NSS de búsqueda","stop")

           CONTINUE CONSTRUCT 
         END IF

         EXIT CONSTRUCT

      ON ACTION CANCEL
         LET v_bandera = FALSE 

         EXIT CONSTRUCT

   END CONSTRUCT

   CLOSE WINDOW w_op_derechohab

   -- si el usuario ha cancelado la consulta, regresa al menú principal
   IF v_bandera = FALSE THEN
      RETURN v_bandera
   END IF

   -- se valida que exista el derechohabiente caprutado en el catalogo
   LET v_sqlqry = " SELECT COUNT(*)",
                  "   FROM afi_derechohabiente ",
                  "  WHERE ",v_s_condicion

   PREPARE prp_cta_nombre FROM v_sqlqry
   EXECUTE prp_cta_nombre INTO v_cta_nombre

   IF v_cta_nombre = 0 THEN
      CALL fn_mensaje("Aviso","No existe información del derechohabiente","stop")

      RETURN v_bandera
   END IF

   IF v_cta_nombre > 1 THEN
      -- consulta listado de derechohabientes de afi para obtener el id derechohab
      CALL fn_selec_derechohabiente(v_s_condicion) RETURNING v_id_derechohabiente

      -- en caso de haber cancelado la selección de derechohabiente regresa al menú princial
      IF v_id_derechohabiente = -1 THEN
         RETURN v_bandera
      END IF

      -- se verifica que exista información del derechohabiente
      SELECT COUNT(*) 
        INTO v_cta_reg
        FROM cre_acreditado
       WHERE id_derechohabiente = v_id_derechohabiente

      -- si no se encontró información del derechohabiente regresa al menú principal
      IF v_cta_reg = 0 THEN 
         CALL fn_mensaje("Aviso","No existe información de creditos para el derechohabiente","stop")

         RETURN v_bandera
      END IF
   ELSE
      -- consulta id_derehohabiente
      LET v_sqlqry = " SELECT id_derechohabiente ",
                     "   FROM afi_derechohabiente ",
                     "  WHERE ", v_s_condicion

      PREPARE prp_con_id FROM v_sqlqry
      EXECUTE prp_con_id INTO v_id_derechohabiente

      --DISPLAY "DERECHOHABIENTE ",  v_id_derechohabiente

      -- se verifica que exista información del derechohabiente
      SELECT COUNT(*)
        INTO v_cta_reg
        FROM cre_acreditado
       WHERE id_derechohabiente = v_id_derechohabiente

      IF v_cta_reg = 0 THEN 
         CALL fn_mensaje("Aviso","No existe información para el derechohabiente","stop")

         RETURN v_bandera
      END IF
   END IF 

   -- se invoca la funcion que muestra los créditos de un derechohabiente
   CALL fn_selecciona_credito(v_id_derechohabiente) RETURNING v_id_derechohabiente, v_d_num_credito, v_id_cre_acred

   -- se valida que el usuario no haya cancelado la consulta
   IF v_id_derechohabiente <> -1 THEN
      -- se llama la funcion que despliega la consulta
      CALL fn_despliega_info_derechohab(v_id_derechohabiente, v_d_num_credito, v_id_cre_acred)
   END IF

   RETURN v_bandera

END FUNCTION

#Objetivo: Función que muestra un listado de derechohabientes, para elegir el que se desea
#          consultar, esto se muestra cuando existe mas de un registro
FUNCTION fn_selec_derechohabiente(p_s_condicion)

   DEFINE p_s_condicion             STRING
   DEFINE v_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_sqlqry                  STRING
   DEFINE v_indice                  SMALLINT
   DEFINE v_r_afi_derhab            RECORD LIKE afi_derechohabiente.*

   DEFINE v_arr_derechohabiente DYNAMIC ARRAY OF RECORD
      id_derechohabiente            DECIMAL(9,0),
      nss                           CHAR(11),
      curp                          CHAR(18),
      rfc                           CHAR(13),
      nombre_af                     CHAR(40),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40)
   END RECORD

   -- se inicializa el indice del arreglo
   LET v_indice = 1

   -- se obtiene la información del catálogo de derechohabientes
   LET v_sqlqry = " SELECT * ",
                  "   FROM afi_derechohabiente ",
                  "  WHERE ", p_s_condicion

   PREPARE prp_cons_derechohabiente FROM v_sqlqry
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

   OPEN WINDOW w_list_derechohabiente WITH FORM "ACRC165"
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

#Objetivo: Funcion que muestra el resultado de la consulta de derechohabiente
FUNCTION fn_despliega_info_derechohab(p_id_derechohabiente, p_d_num_credito,p_id_cre_acred)

   DEFINE p_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente
   DEFINE p_d_num_credito           LIKE cre_acreditado.num_credito
   DEFINE p_id_cre_acred            DECIMAL(9,0)
   DEFINE v_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_curp                    LIKE afi_derechohabiente.curp
   DEFINE v_rfc                     LIKE afi_derechohabiente.rfc
   DEFINE v_ap_paterno              LIKE afi_derechohabiente.ap_paterno_af
   DEFINE v_ap_materno              LIKE afi_derechohabiente.ap_materno_af
   DEFINE v_nombre                  LIKE afi_derechohabiente.nombre_af
   DEFINE v_tpo_credito             LIKE cre_acreditado.tpo_credito
   DEFINE v_num_credito             LIKE cre_acreditado.num_credito
   DEFINE v_f_otorga                LIKE cre_acreditado.f_otorga
   DEFINE v_estado                  LIKE cre_acreditado.estado
   DEFINE v_des_estado              LIKE cat_maq_credito.estado_desc
   DEFINE v_edo_procesar            LIKE cre_acreditado.edo_procesar
   DEFINE v_des_procesar            LIKE cat_maq_credito.estado_desc
   DEFINE v_id_deudor               SMALLINT
   DEFINE v_entidad                 SMALLINT
   DEFINE v_edo_credito             CHAR(100)
   DEFINE v_edo_solicitud           CHAR(100)

   DEFINE v_rec_his_transferencia RECORD
      --operacion                     LIKE cre_his_acreditado.operacion,
      f_proceso                     LIKE cre_his_acreditado.f_proceso,
      estado                        LIKE cre_his_acreditado.estado,
      estado_desc                   LIKE cat_maq_credito.estado_desc, 
      edo_procesar                  LIKE cre_his_acreditado.edo_procesar,
      edo_proc_desc                 LIKE cat_maq_credito.estado_desc,  
      diagnostico                   LIKE cre_his_acreditado.diagnostico,
      id_cre_ctr_archivo            LIKE cre_his_acreditado.id_cre_ctr_archivo
   END RECORD

   DEFINE v_ar_his_transferencia DYNAMIC ARRAY OF RECORD
       --operacion                    LIKE cre_his_acreditado.operacion,
       f_proceso                    LIKE cre_his_acreditado.f_proceso,
       des_estado                   LIKE cat_maq_credito.estado_desc,
       des_edo_procesar             LIKE cat_maq_credito.estado_desc,
       des_diagnostico              LIKE cat_rechazo.desc_rechazo
   END RECORD

   DEFINE v_indice                  SMALLINT
   DEFINE v_indice_deudor           SMALLINT
   DEFINE v_indice_rep              SMALLINT

   DEFINE v_rec_deudor RECORD
       movimiento                   LIKE cre_saldo_deudor.movimiento,
       f_movimiento                 LIKE cre_saldo_deudor.f_movimiento,
       monto_aivs                   LIKE cre_saldo_deudor.monto_aivs,
       monto_pesos                  LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE v_ar_deudor DYNAMIC ARRAY OF RECORD 
      des_movimiento                LIKE cat_movimiento.movimiento_desc,
      f_movimiento                  LIKE cre_saldo_deudor.f_movimiento,
      monto_aivs                    LIKE cre_saldo_deudor.monto_aivs,
      monto_pesos                   LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE v_sqlqry                  STRING
   DEFINE v_id_cre_acreditado       LIKE cre_acreditado.id_cre_acreditado
   DEFINE v_lsqry                   CHAR(500)
   DEFINE v_des_tpo_credito         LIKE cat_tipo_credito.desc_credito
   DEFINE v_manejador_rpt           OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
   DEFINE r_ruta_bin                LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte             VARCHAR(80) -- nombre del reporte
   DEFINE r_ruta_listados           LIKE seg_modulo.ruta_listados
   DEFINE v_indice_aux              SMALLINT
   DEFINE v_tpo_enc                 SMALLINT --tipo de encabezado que aparecerá en el reporte   
   DEFINE f_w                       ui.form
   DEFINE w                         ui.window
   DEFINE v_existe_archivo          INTEGER

   LET v_nss           = ""
   LET v_curp          = ""
   LET v_rfc           = ""
   LET v_ap_paterno    = ""
   LET v_ap_materno    = ""
   LET v_nombre        = ""
   LET v_tpo_credito   = ""
   LET v_num_credito   = ""
   LET v_f_otorga      = ""
   LET v_estado        = ""
   LET v_des_estado    = ""
   LET v_edo_procesar  = ""
   LET v_des_procesar  = ""
   LET v_id_deudor     = ""
   LET v_entidad       = ""
   LET v_edo_credito   = ""
   LET v_edo_solicitud = ""

   -- se obtiene la información del derechohabiente
   SELECT nss, curp, rfc, ap_paterno_af, ap_materno_af, nombre_af
     INTO v_nss, v_curp, v_rfc, v_ap_paterno, v_ap_materno, v_nombre
     FROM afi_derechohabiente 
    WHERE id_derechohabiente = p_id_derechohabiente

   -- se inicializa la fecha de otorgamiento
   LET v_f_otorga = ""

   -- se realiza la consulta en la tabla maestro
   LET v_sqlqry = " SELECT cre.tpo_credito,\n",
                  "        tpo.desc_credito,\n",
                  "        cre.num_credito,\n",
                  "        cre.f_otorga,\n",
                  "        cre.estado,\n",
                  "        ds1.estado_desc,\n",
                  "        cre.edo_procesar,\n",
                  "        ds2.estado_desc,\n",
                  "        cre.id_cre_acreditado,\n",
                  "        den.entidad_desc,\n",
                  "        tpo.id_deudor,\n",
                  "        ds1.entidad\n",
                  "   FROM cre_acreditado cre,\n",
                  "        cat_maq_credito ds1,\n",
                  "        cat_maq_credito ds2,\n",
                  "        cat_cre_entidad den,\n",
                  "        cat_tipo_credito tpo",
                  "  WHERE cre.id_cre_acreditado = ",p_id_cre_acred,"\n",
                  "    AND cre.estado = ds1.estado\n",
                  "    AND cre.edo_procesar = ds2.estado\n",
                  "    AND ds1.entidad = den.entidad\n",
                  "    AND cre.tpo_originacion = tpo.tpo_originacion\n",
                  "    AND cre.tpo_credito = tpo.tpo_credito"

   PREPARE prp_cons_transf FROM v_sqlqry
   EXECUTE prp_cons_transf INTO v_tpo_credito,
                                v_des_tpo_credito,
                                v_num_credito,
                                v_f_otorga,
                                v_estado,
                                v_des_estado,
                                v_edo_procesar,
                                v_des_procesar,
                                v_id_cre_acreditado,
                                v_edo_credito,
                                v_id_deudor,
                                v_entidad

--DISPLAY "estado : ", v_estado
--DISPLAY "edo proc : ", v_edo_procesar

   IF v_edo_procesar = 7 THEN
      LET v_edo_solicitud = "SOLO INFONAVIT"
   ELSE
      IF (v_edo_procesar > 5 AND v_edo_procesar < 55) OR
          v_edo_procesar = 155 THEN
         LET v_edo_solicitud = "POR MARCAR EN PROCESAR"
      ELSE
         IF v_edo_procesar > 50 AND v_edo_procesar < 70 THEN
            IF v_id_deudor  = 1 AND v_entidad = 1 THEN
               LET v_edo_solicitud = "MARCADA PROCESAR, POR CONCILIAR"
            ELSE
               LET v_edo_solicitud = "MARCADA PROCESAR"
            END IF
         ELSE
           IF v_edo_procesar > 60 AND v_edo_procesar < 120 THEN
              IF v_id_deudor  = 1 AND v_entidad = 1 THEN
                 LET v_edo_solicitud = "SOLICITUD DE SALDO, POR CONCILIAR"
              ELSE
                 LET v_edo_solicitud = "SOLICITUD DE SALDO"
              END IF
           ELSE
              IF v_edo_procesar > 110 AND v_edo_procesar < 130 THEN
                 IF v_id_deudor  = 1 AND v_entidad = 1 THEN
                    IF v_estado < 130 THEN
                       LET v_edo_solicitud = "SALDO TRANSFERIDO, POR CONCILIAR"    
                    ELSE
                       LET v_edo_solicitud = "SALDO TRANSFERIDO, CONCILIADA"
                    END IF
                 ELSE
                    LET v_edo_solicitud = "SALDO TRANSFERIDO"
                 END IF
              ELSE
                 IF v_edo_procesar > 170 AND v_edo_procesar < 210 THEN
                    LET v_edo_solicitud = "POR DESMARCAR EN PROCESAR"
                 ELSE
                    IF v_edo_procesar = 210 THEN
                       LET v_edo_solicitud = "DESMARCADA PROCESAR"
                    ELSE
                       IF v_edo_procesar = 240 THEN
                          LET v_edo_solicitud = "TRANSFERENCIA RECHAZADA"
                       END IF
                    END IF
                 END IF
              END IF
           END IF
         END IF
      END IF
   END IF

   -- se abre la ventana del derechohabiente
   OPEN WINDOW w_des_derechohab WITH FORM "ACRC166"
      LET w = ui.Window.getCurrent()
      LET f_w = w.getForm()

      DISPLAY p_id_derechohabiente,
              v_nss,
              v_curp,
              v_rfc,
              v_ap_paterno,
              v_ap_materno,
              v_nombre,
              v_des_tpo_credito,
              v_num_credito USING "&&&&&&&&&&",
              v_f_otorga,
              v_des_estado,
              v_des_procesar,
              v_edo_credito,
              v_edo_solicitud
           TO id_derechohabiente,
              nss,
              curp,
              rfc,
              app_paterno,
              app_materno,
              nombre,
              tpo_credito,
              num_credito,
              f_otorga,
              estado,
              edo_procesar,
              edo_credito,  
              edo_solicitud  

      DIALOG ATTRIBUTES (FIELD ORDER FORM, UNBUFFERED)
         DISPLAY ARRAY v_ar_his_transferencia TO tabla_trans.*
            BEFORE DISPLAY
               LET v_lsqry = " SELECT crh.f_proceso,\n",
                             "        crh.estado,\n",
                             "        ds3.estado_desc,\n",
                             "        crh.edo_procesar,\n",
                             "        ds4.estado_desc,\n",
                             "        crh.diagnostico, id_cre_ctr_archivo\n",   
                             "   FROM cre_his_acreditado crh,\n",
                             "        cat_maq_credito ds3,\n",
                             "        cat_maq_credito ds4\n",
                             "  WHERE id_cre_acreditado = ", v_id_cre_acreditado,"\n",
                             "    AND crh.estado = ds3.estado\n",
                             "    AND crh.edo_procesar = ds4.estado"

               PREPARE prp_cons_his FROM v_lsqry
               DECLARE cur_his_trans CURSOR FOR prp_cons_his

               --DISPLAY "CONSULTA HIST ", v_lsqry
               -- se inicializa el indice del arreglo
               LET v_indice = 1

               -- se abre el cursor
               OPEN cur_his_trans

               FOREACH cur_his_trans INTO v_rec_his_transferencia.*
                  LET v_ar_his_transferencia[v_indice].f_proceso = v_rec_his_transferencia.f_proceso
                  LET v_ar_his_transferencia[v_indice].des_estado = v_rec_his_transferencia.estado_desc 
                  LET v_ar_his_transferencia[v_indice].des_edo_procesar = v_rec_his_transferencia.edo_proc_desc 
                  LET v_ar_his_transferencia[v_indice].des_diagnostico = fn_obt_desc_diagnostico(v_rec_his_transferencia.diagnostico, v_rec_his_transferencia.id_cre_ctr_archivo)

                  LET v_indice = v_indice + 1
               END FOREACH 

               -- se libera el cursor
               FREE cur_his_trans

               -- se inicializa el indice del arreglo del deudor
               LET v_indice_deudor = 1

               -- se obtiene la información de la tabla del deudor
               LET v_lsqry = " SELECT movimiento, f_movimiento, monto_aivs, monto_pesos ",
                             "   FROM cre_saldo_deudor ",
                             "  WHERE id_cre_acreditado = ",v_id_cre_acreditado,"\n",
                             " ORDER BY f_movimiento DESC"

               PREPARE prp_cons_deudor1 FROM v_lsqry
               DECLARE cur_deudor1 CURSOR FOR prp_cons_deudor1
               --DISPLAY "CONSULTA DEUDOR ",v_lsqry

               -- se abre el cursor
               OPEN cur_deudor1

               FOREACH cur_deudor1 INTO v_rec_deudor.*
                  -- se obtiene la descripción del movimiento
                  SELECT movimiento_desc
                    INTO v_ar_deudor[v_indice_deudor].des_movimiento
                    FROM cat_movimiento 
                   WHERE movimiento = v_rec_deudor.movimiento

                  -- se asignan los valores en el arreglo del deudor
                  LET v_ar_deudor[v_indice_deudor].f_movimiento = v_rec_deudor.f_movimiento
                  LET v_ar_deudor[v_indice_deudor].monto_aivs = v_rec_deudor.monto_aivs
                  LET v_ar_deudor[v_indice_deudor].monto_pesos = v_rec_deudor.monto_pesos

                  -- se incrementa el indice del arreglo
                  LET v_indice_deudor = v_indice_deudor + 1
               END FOREACH 

               -- se libera el cursor
               FREE cur_deudor1

               -- se verifica que arreglo tiene mas registros
               IF v_indice > v_indice_deudor THEN
                  LET v_indice_aux = v_indice
               ELSE 
                  LET v_indice_aux = v_indice_deudor
               END IF 
         END DISPLAY

         DISPLAY ARRAY v_ar_deudor TO tabla_deudor.*
         END DISPLAY

         ON ACTION ACCEPT
            EXIT DIALOG

         ON ACTION cancelar
            EXIT DIALOG

         ON ACTION reporte
            -- recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

            -- se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("ACRC162.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 

               LET v_nom_reporte = g_usuario_cod CLIPPED || "-ACRC16-","00000","-","00000","-","00000"

               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)

               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               -- inicia el reporte de consulta historica por derechohabiente
               START REPORT rpt_deudor_derhab TO XML HANDLER v_manejador_rpt

               -- se procesan los registro de cre historico
               FOR v_indice_rep = 1 TO v_indice -1
                  -- indica que el encabezado debe ser el de historico
                  LET v_tpo_enc = 1
                  OUTPUT TO REPORT rpt_deudor_derhab(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                     v_ap_paterno, v_ap_materno, v_nombre,
                                                     v_des_tpo_credito, v_num_credito, v_f_otorga,
                                                     v_ar_his_transferencia[v_indice_rep].*,
                                                     v_ar_deudor[1].*, v_tpo_enc)
               END FOR

               -- se procesan los registro de cre deudor
               FOR v_indice_rep = 1 TO v_indice_deudor -1
                  -- indica que el encabezado debe ser el de deudor
                  LET v_tpo_enc = 2
                  OUTPUT TO REPORT rpt_deudor_derhab(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                     v_ap_paterno, v_ap_materno, v_nombre,
                                                     v_des_tpo_credito, v_num_credito, v_f_otorga,
                                                     v_ar_his_transferencia[1].*,
                                                     v_ar_deudor[v_indice_rep].*, v_tpo_enc)
               END FOR

               -- finaliza el reporte
               FINISH REPORT rpt_deudor_derhab

               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  -- se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               -- si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

               -- muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
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

##OBJETIVO: Obtiene la descripcion para el diagnostico
FUNCTION fn_obt_desc_diagnostico(p_diagnostico, p_id_cre_ctr_archivo)

   DEFINE p_diagnostico             LIKE cre_his_acreditado.diagnostico
   DEFINE p_id_cre_ctr_archivo      LIKE cre_his_acreditado.id_cre_ctr_archivo
   DEFINE v_desc_diagnostico        LIKE cat_rechazo.desc_rechazo
   DEFINE v_operacion               LIKE cre_ctr_archivo.operacion
   DEFINE v_tpo_rechazo             LIKE cat_rechazo.tpo_rechazo

   SELECT operacion
     INTO v_operacion
     FROM cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

   CASE v_operacion
      WHEN  "01"
         LET v_tpo_rechazo = "RCH"
      WHEN "14"
         LET v_tpo_rechazo = "RCH"
      WHEN "06"   
         LET v_tpo_rechazo = "DEV"
      WHEN "21"
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

#Objetivo: Genera el reporte de consulta por derechohabiente
REPORT rpt_deudor_derhab(p_id_derechohabiente,p_nss,p_curp,p_rfc,p_ap_paterno,
                         p_ap_materno,p_nombre,p_des_tpo_credito,p_num_credito,
                         p_f_otorga,p_rec_his_transferencia,p_rec_deudor,p_tpo_enc)

   DEFINE p_id_derechohabiente      INTEGER
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE p_curp                    LIKE afi_derechohabiente.curp
   DEFINE p_rfc                     LIKE afi_derechohabiente.rfc
   DEFINE p_ap_paterno              LIKE afi_derechohabiente.ap_paterno_af
   DEFINE p_ap_materno              LIKE afi_derechohabiente.ap_materno_af
   DEFINE p_nombre                  LIKE afi_derechohabiente.nombre_af
   DEFINE p_des_tpo_credito         LIKE cat_tipo_credito.desc_credito
   DEFINE p_num_credito             LIKE cre_acreditado.num_credito
   DEFINE p_f_otorga                LIKE cre_acreditado.f_otorga

   DEFINE p_rec_his_transferencia RECORD
      f_proceso                     LIKE cre_his_acreditado.f_proceso,
      des_estado                    LIKE cat_maq_credito.estado_desc,
      des_edo_procesar              LIKE cat_maq_credito.estado_desc,
      diagnostico                   LIKE cre_his_acreditado.diagnostico
   END RECORD

   DEFINE p_rec_deudor RECORD
      des_movimiento                LIKE cat_movimiento.movimiento_desc,
      f_movimiento                  LIKE cre_saldo_deudor.f_movimiento,  
      monto_aivs                    LIKE cre_saldo_deudor.monto_aivs,
      monto_pesos                   LIKE cre_saldo_deudor.monto_pesos
   END RECORD

   DEFINE p_tpo_enc                 SMALLINT -- tipo de encabezado que aparecerá en el reporte
   DEFINE v_v_desc_detalle          VARCHAR(50)
   DEFINE v_fecha_reporte           DATE

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX g_usuario_cod
      PRINTX p_id_derechohabiente
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
      PRINTX p_rec_his_transferencia.f_proceso USING "dd-mm-yyyy"
      PRINTX p_rec_his_transferencia.des_estado
      PRINTX p_rec_his_transferencia.des_edo_procesar
      PRINTX p_rec_his_transferencia.diagnostico
      PRINTX p_rec_deudor.des_movimiento
      PRINTX p_rec_deudor.f_movimiento USING "dd-mm-yyyy"
      PRINTX p_rec_deudor.monto_aivs
      PRINTX p_rec_deudor.monto_pesos

END REPORT

#Objetivo: Función que muestra un listado créditos ortorgados al derechohabiente que entra
#          como paáametro, el usuario debe seleccionar uno para continuar con la consulta
FUNCTION fn_selecciona_credito(p_id_derechohabiente) 

   DEFINE p_id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_d_num_credito           DECIMAL(10,0)
   DEFINE v_sqlqry                  STRING
   DEFINE v_id_cre_acred            DECIMAL(9,0) 

   DEFINE v_r_cre_acreditado RECORD
      id_derechohabiente            LIKE cre_acreditado.id_derechohabiente,
      tpo_originacion               LIKE cre_acreditado.tpo_originacion,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      num_credito                   LIKE cre_acreditado.num_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      f_otorga                      LIKE cre_acreditado.f_otorga,
      folio_liquida                 LIKE cre_acreditado.folio_liquida,
      id_deudor                     SMALLINT,
      id_cre_acreditado             DECIMAL(9,0)
   END RECORD

   DEFINE v_arr_num_creditos DYNAMIC ARRAY OF RECORD
      nss                           LIKE afi_derechohabiente.nss,
      tpo_originacion               LIKE cat_cre_originacion.originacion_desc,
      desc_credito                  LIKE cat_tipo_credito.desc_credito,
      num_credito                   LIKE cre_acreditado.num_credito,
      desc_estado                   LIKE cat_maq_credito.estado_desc,
      desc_edo_procesar             LIKE cat_maq_credito.estado_desc,
      f_otorga                      LIKE cre_acreditado.f_otorga,
      f_liquida                     LIKE cta_movimiento.f_liquida,
      id_deudor                     SMALLINT,
      id_cre_acreditado             DECIMAL(9,0)
   END RECORD

   DEFINE v_indice              SMALLINT 

   -- se inicializa el indice del arreglo
   LET v_indice = 1

   -- se seleccionan los registros de la tabla maestro
   LET v_sqlqry = " SELECT cre.id_derechohabiente, cre.tpo_originacion, cre.tpo_credito, cre.num_credito,\n",
                  "        cre.estado, cre.edo_procesar, cre.f_otorga, cre.folio_liquida, tpo.id_deudor,\n",
                  "        cre.id_cre_acreditado\n",
                  "   FROM cre_acreditado cre, cat_tipo_credito tpo\n",
                  "  WHERE id_derechohabiente = ",p_id_derechohabiente,"\n",    
                  "   AND cre.tpo_originacion = tpo.tpo_originacion\n",
                  "   AND cre.tpo_credito     = tpo.tpo_credito\n",
                  "   AND tpo.f_actualiza    <= cre.f_otorga\n"

   PREPARE prp_slct_numCredito FROM v_sqlqry
   DECLARE cur_slct_numCredito CURSOR FOR prp_slct_numCredito

   FOREACH cur_slct_numCredito INTO v_r_cre_acreditado.*
      -- se asignan los valore al arreglo
      LET v_arr_num_creditos[v_indice].nss               = fn_obt_nss(v_r_cre_acreditado.id_derechohabiente)
      LET v_arr_num_creditos[v_indice].tpo_originacion   = fn_obt_desc_originacion(v_r_cre_acreditado.tpo_originacion)
      LET v_arr_num_creditos[v_indice].desc_credito      = fn_obt_desc_tpo_credito(v_r_cre_acreditado.tpo_credito)
      LET v_arr_num_creditos[v_indice].num_credito       = v_r_cre_acreditado.num_credito
      LET v_arr_num_creditos[v_indice].desc_estado       = fn_obt_desc_estado(v_r_cre_acreditado.estado)
      LET v_arr_num_creditos[v_indice].desc_edo_procesar = fn_obt_desc_estado(v_r_cre_acreditado.edo_procesar)
      LET v_arr_num_creditos[v_indice].f_otorga          = v_r_cre_acreditado.f_otorga
      LET v_arr_num_creditos[v_indice].id_deudor         = v_r_cre_acreditado.id_deudor
      LET v_arr_num_creditos[v_indice].id_cre_acreditado = v_r_cre_acreditado.id_cre_acreditado

      IF v_arr_num_creditos[v_indice].id_deudor = 1 THEN
         LET v_arr_num_creditos[v_indice].f_liquida = fn_obt_fec_liquida(v_r_cre_acreditado.folio_liquida)
      END IF

      IF v_arr_num_creditos[v_indice].f_liquida IS NULL OR
         v_arr_num_creditos[v_indice].f_liquida = "12/31/1899" OR
         v_arr_num_creditos[v_indice].id_deudor = 0 THEN
         LET v_arr_num_creditos[v_indice].f_liquida = "        "
      END IF

      -- se incrementa el indice del arreglo
      LET v_indice = v_indice + 1
   END FOREACH

   OPEN WINDOW w_list_derechohabiente WITH FORM "ACRC167" 
      DISPLAY ARRAY v_arr_num_creditos TO tabla_creditos.* ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )
         ON ACTION ACCEPT 
            LET v_indice = arr_curr()
            LET v_d_num_credito = v_arr_num_creditos[v_indice].num_credito
            LET v_id_cre_acred  = v_arr_num_creditos[v_indice].id_cre_acreditado 
            EXIT DISPLAY 

         ON ACTION CANCEL
            LET p_id_derechohabiente = -1

            EXIT DISPLAY
      END DISPLAY
   CLOSE WINDOW w_list_derechohabiente

   RETURN p_id_derechohabiente, v_d_num_credito, v_id_cre_acred

END FUNCTION

#Objetivo: Funcion que obtiene la descripcion de estado o estado procesar
FUNCTION fn_obt_desc_estado(p_cve_estado)

   DEFINE p_cve_estado LIKE cat_maq_credito.estado
   DEFINE v_des_estado LIKE cat_maq_credito.estado_desc

   -- se selecciona la descripción del estado
   SELECT estado_desc
     INTO v_des_estado
     FROM cat_maq_credito
    WHERE estado = p_cve_estado

   RETURN v_des_estado

END FUNCTION

#Objetivo: Función que obtiene la descripción del tipo de crédito
FUNCTION fn_obt_desc_tpo_credito(p_si_tpo_credito)

   DEFINE p_si_tpo_credito  LIKE cat_tipo_credito.tpo_credito
   DEFINE v_c_desc_credito  LIKE cat_tipo_credito.desc_credito
   DEFINE v_c_sqlQry        STRING

   -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
   LET v_c_sqlQry = " SELECT FIRST 1 desc_credito\n",
                    "   FROM cat_tipo_credito\n",
                    "  WHERE tpo_credito = ",p_si_tpo_credito

   PREPARE prp_slctFrst_descCred FROM v_c_sqlQry
   EXECUTE prp_slctFrst_descCred INTO v_c_desc_credito

   RETURN v_c_desc_credito

END FUNCTION

#Objetivo: Función que obtiene la fecha de liquidación para un folio dado
FUNCTION fn_obt_fec_liquida(p_d_folio_liquida)

   DEFINE p_d_folio_liquida     LIKE cta_movimiento.folio_liquida
   DEFINE p_d_f_liquida         LIKE cta_movimiento.f_liquida
   DEFINE v_s_sqlQry            STRING
   DEFINE v_criterio            SMALLINT
   DEFINE v_s_qryTxt            STRING
   DEFINE v_fecha               DATE
   DEFINE v_tabla               CHAR(20)

   LET v_criterio = 0
   LET v_fecha    = ""

   LET v_s_qryTxt = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

   PREPARE prp_obt_mov FROM v_s_qryTxt
   EXECUTE prp_obt_mov USING v_criterio,
                             p_d_folio_liquida,
                             v_fecha
                        INTO v_tabla

   -- se selecciona la descripción del tipo de credito
   LET v_s_sqlQry = " SELECT FIRST 1 f_liquida\n",
                    "   FROM ",v_tabla,"\n",
                    "  WHERE folio_liquida = ",p_d_folio_liquida,"\n",
                    "    AND f_liquida IS NOT NULL",
                    "  ORDER BY f_liquida desc "

   PREPARE prp_slct_fLiquida FROM v_s_sqlQry
   EXECUTE prp_slct_fLiquida INTO p_d_f_liquida

   RETURN p_d_f_liquida

END FUNCTION

#Objetivo: Funcion que obtiene la fecha de liquidación para un folio dado
FUNCTION fn_obt_nss(p_d_id_derechohabiente)

   DEFINE p_d_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE p_c_nss                LIKE afi_derechohabiente.nss

   -- se selecciona la descripción del tipo de credito
   SELECT nss
     INTO p_c_nss
     FROM afi_derechohabiente
    WHERE id_derechohabiente = p_d_id_derechohabiente

   RETURN p_c_nss

END FUNCTION

# Objetivo: FunciÓn que obtiene la descripción de un tipo de originaciÓn
FUNCTION fn_obt_desc_originacion(p_si_tpo_originacion)

   DEFINE p_si_tpo_originacion LIKE cat_cre_originacion.tpo_originacion
   DEFINE p_c_originacion_desc LIKE cat_cre_originacion.originacion_desc

   -- se selecciona la descripción del tipo de credito
   SELECT originacion_desc
     INTO p_c_originacion_desc
     FROM cat_cre_originacion
    WHERE tpo_originacion = p_si_tpo_originacion

   RETURN p_c_originacion_desc

END FUNCTION

PRIVATE FUNCTION fn_valida_caracteres(p_campo)

   DEFINE p_campo                   STRING

   RETURN p_campo.getIndexOf("*",1)

END FUNCTION
