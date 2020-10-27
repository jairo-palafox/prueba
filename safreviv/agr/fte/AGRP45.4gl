##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP45                                             #
#Objetivo          => Actualiza deudores SAP-FICO en SACI (Lanzado).     #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 24/Noviembre/2017                                  #
#Autor modifica    => Emilio Abarca, EFP                                 #
#Fecha inicio      => 26/Febrero/2019                                    #
##########################################################################

DATABASE safre_viv

GLOBALS "../../cta/fte/CTAW15.inc"    # Archivo de variables globales del WS de consulta de saldo Procesar
GLOBALS "../../cta/fte/CTAW12.inc"    # Archivo variables globales del WS de solicitud de Marca y Desmarca Procesar

GLOBALS 
   DEFINE p_usuario        CHAR(20)
   DEFINE p_pid            DECIMAL(9,0)
   DEFINE p_proceso_cod    SMALLINT  
   DEFINE p_opera_cod      SMALLINT
   DEFINE p_archivo        STRING 
   DEFINE p_ruta_archivo   STRING
   DEFINE v_fecha          DATE 
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_ruta_lst       CHAR(40)
   DEFINE v_query          STRING 
   DEFINE v_error          SMALLINT
   DEFINE v_tot_procesa    INTEGER
   DEFINE v_tot_acept      INTEGER 
   DEFINE v_tot_rch        INTEGER 
   DEFINE v_monto_procesa  DECIMAL(12,2)
   DEFINE v_monto_acep     DECIMAL(12,2)
   DEFINE v_monto_rch      DECIMAL(12,2)
   DEFINE r_b_valida       SMALLINT

   -- Arreglo para el reporte PDF
   DEFINE g_arr_estado_credito DYNAMIC ARRAY OF RECORD
      estado_desc     CHAR(40),
      total           INTEGER,
      monto_deudor    DECIMAL(12,2)
   END RECORD
   
END GLOBALS 

MAIN
   --Recibe parámetros enviados por AGRL65
   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_archivo      = ARG_VAL(5)
   LET p_ruta_archivo = ARG_VAL(6)
   
   LET r_b_valida  = 0

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP45.log")

   SELECT ruta_bin,ruta_listados,ruta_envio
     INTO v_ruta_bin,v_ruta_lst,v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   CALL fn_display_proceso(0,"ACTUALIZACIÓN DEUDORES SAP-FICO")
   DISPLAY ""
   DISPLAY " ARCHIVO: ",p_archivo CLIPPED
   DISPLAY " RUTA   : ",p_ruta_archivo CLIPPED
   DISPLAY ""

   -- Carga de registros del archivo a la temporal
   DISPLAY " > CARGA REGISTROS EN TEMPORAL"
   CALL fn_carga_archivo_temporal()

   -- Procesa información de la temporal
   DISPLAY " > PROCESA INFORMACIÓN"
   
   LET v_query = "EXECUTE FUNCTION fn_agr_act_deudor(?,?)"
   PREPARE prp_procesa_info FROM v_query
   EXECUTE prp_procesa_info USING p_usuario,
                                  p_proceso_cod
                             INTO v_error,
                                  v_tot_procesa,
                                  v_monto_procesa,
                                  v_tot_acept,
                                  v_monto_acep,
                                  v_tot_rch,
                                  v_monto_rch
   
   IF(v_error = 0) THEN
      -- Genera marca de los deudores actualizados
      DISPLAY " > GENERA MARCA"
      CALL fn_genera_marca_deudor()

      DISPLAY " > GENERA ARCHIVOS DE SALIDA"
      CALL fn_genera_archivos_salida()

      DISPLAY ""
      DISPLAY "   El archivo de los deudores actualizados se ha generado en la ruta\n",
              "   ",v_ruta_envio CLIPPED," con nombre: ","Actualizacion_Deudor_",TODAY USING "yyyymmdd",".txt"
      DISPLAY ""
      DISPLAY "   El archivo con registros sin asignación de NSS se ha generado en la ruta\n",
              "   ",v_ruta_envio CLIPPED," con nombre: ","Registros_sin_NSS_",TODAY USING "yyyymmdd",".txt"

      DISPLAY ""
      DISPLAY "   El archivo con el estado de los deudores se ha generado en la ruta\n",
              "   ",v_ruta_envio CLIPPED, " con nombre: ",p_archivo

      DISPLAY ""
      DISPLAY " > GENERA REPORTE PDF"
      CALL fn_genera_reporte_pdf()

      --Finaliza el proceso
      CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
   
      CALL fn_display_proceso(1,"ACTUALIZACIÓN DEUDORES SAP-FICO")
   ELSE 
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
      DISPLAY " > OCURRIÓ UN ERROR AL PROCESAR LA INFORMACIÓN"
      EXIT PROGRAM
   END IF 

END MAIN 

FUNCTION fn_carga_archivo_temporal()

   DEFINE ch             base.channel
   DEFINE v_linea        CHAR(24)
   DEFINE v_num_credito  CHAR(10)
   DEFINE v_monto_deudor CHAR(14)

   -- Crea tabla temporal
   CALL fn_crea_temporal()

   LET v_num_credito  = NULL 
   LET v_monto_deudor = NULL 
   LET ch = base.Channel.create() # Creamos un objeto de la clase channel
   CALL ch.openFile(p_ruta_archivo,"r")

   WHILE TRUE

      LET v_linea = ch.readLine()

      IF(ch.isEof()) THEN
         EXIT WHILE
         DISPLAY "no encuentra registros"
      ELSE 
         -- Recupera información por linea
         LET v_num_credito = v_linea[1,10]
         LET v_monto_deudor = v_linea[11,24]

         INSERT INTO safre_tmp:tmp_deudor_sap_fico
            VALUES (v_num_credito,v_monto_deudor);
      END IF 
      
   END WHILE 

   -- Cierra archivo
   CALL ch.close()
   
END FUNCTION 

FUNCTION fn_genera_marca_deudor()

   DEFINE r_deudor_act    RECORD
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      tpo_origina        SMALLINT,
      tpo_credito        SMALLINT,
      num_credito        DECIMAL(10,0),
      f_otorga           DATE,
      nss                CHAR(11),
      rfc                CHAR(13),
      ap_materno         CHAR(40),
      ap_paterno         CHAR(40),
      nombre             CHAR(50),
      estado             SMALLINT,
      folio_archivo      DECIMAL(9,0)
   END RECORD
   DEFINE soapStatus       INTEGER
   DEFINE v_m_tpo_credito  CHAR(2)
   DEFINE v_cod_result_op  SMALLINT 
   DEFINE v_diagnostico    CHAR(3)
   DEFINE v_marca_modulo   CHAR(3)
   DEFINE v_marca_activa   INTEGER
   DEFINE v_sql_txt        STRING
   DEFINE v_result_marca   SMALLINT
   
   #Parametros de conexion WS consulta de saldo Procesar
   DEFINE v_url_servidor   LIKE wsv_cliente.ruta_servidor
   DEFINE v_usuario        LIKE wsv_cliente.usuario
   DEFINE v_password       LIKE wsv_cliente.password
   DEFINE v_intentos       LIKE wsv_cliente.num_reintento
   #Parametros de conexion WS consulta de solicitud Marca/Desmarca Procesar
   DEFINE v_url_servidor_m LIKE wsv_cliente.ruta_servidor 
   DEFINE v_usuario_m      LIKE wsv_cliente.usuario
   DEFINE v_password_m     LIKE wsv_cliente.password
   DEFINE v_intentos_m     LIKE wsv_cliente.num_reintento
   # Variables solicitud de marca a Procesar
   DEFINE g_solicita_m     tSolicMarcaVO
   DEFINE v_respuesta_m    tSolicMarcaRespVO

   # Configuración del Web Service Consulta de saldo Procesar
   SELECT ruta_servidor,
          usuario,
          password,
          num_reintento
     INTO v_url_servidor,
          v_usuario,
          v_password,
          v_intentos
     FROM wsv_cliente
    WHERE cve_cliente = 'cre_3' -- La clave 'cre_3' del catalogo de clientes de webServices corresponde a la solicitud de saldo

   -- Configuración del WS Marca PROCESAR
   SELECT ruta_servidor,
          usuario,
          password,
          num_reintento 
     INTO v_url_servidor_m,
          v_usuario_m,
          v_password_m,
          v_intentos_m
     FROM wsv_cliente 
    WHERE cve_cliente = "cre_1";

   INITIALIZE r_deudor_act.* TO NULL

   LET v_sql_txt = "EXECUTE FUNCTION fn_marca_cuenta(?,234,?,?,0,0,?,?,?,?)"
   PREPARE prp_marca_cuenta FROM v_sql_txt

   -- Obtiene deudores actualizados
   DECLARE crs_deudor_actualizado CURSOR FOR
   SELECT tmp.id_cre_acreditado,
          tmp.id_derechohabiente,
          cre.tpo_originacion,
          cre.tpo_credito,
          tmp.num_credito_arh,
          cre.f_otorga,
          afi.nss,
          afi.rfc,
          afi.ap_materno_af,
          afi.ap_paterno_af,
          afi.nombre_af,
          tmp.estado,
          arh.folio_archivo
     FROM safre_tmp:tmp_deudor_agr tmp,
          cre_acreditado cre,
          afi_derechohabiente afi,
          cre_ctr_archivo arh
    WHERE tmp.id_cre_acreditado  = cre.id_cre_acreditado
      AND cre.id_derechohabiente = afi.id_derechohabiente
      AND cre.id_cre_ctr_archivo = arh.id_cre_ctr_archivo
      AND tmp.estado NOT IN (174,290);   -- No considera los Cancelados y Trámites vencidos para generar la marca

   --LET v_m_tpo_credito = NULL
   --LET v_marca_modulo  = NUL
   LET v_diagnostico   = NULL 
   LET v_cod_result_op = NULL
   LET v_marca_activa  = 0
   
   FOREACH crs_deudor_actualizado INTO r_deudor_act.id_cre_acreditado,
                                       r_deudor_act.id_derechohabiente,
                                       r_deudor_act.tpo_origina,
                                       r_deudor_act.tpo_credito,
                                       r_deudor_act.num_credito,
                                       r_deudor_act.f_otorga,
                                       r_deudor_act.nss,
                                       r_deudor_act.rfc,
                                       r_deudor_act.ap_materno,
                                       r_deudor_act.ap_paterno,
                                       r_deudor_act.nombre,
                                       r_deudor_act.estado,
                                       r_deudor_act.folio_archivo

      --DISPLAY ""
      --DISPLAY "id_cre_acreditado: ",r_deudor_act.id_cre_acreditado ," id_derechohabiente: ", r_deudor_act.id_derechohabiente," tpo_origina: ",r_deudor_act.tpo_origina
      --DISPLAY " tpo_credito: ",r_deudor_act.tpo_credito," num_credito: ",r_deudor_act.num_credito," estado: ",r_deudor_act.estado
      
      {CASE
         WHEN r_deudor_act.tpo_origina = 1
            LET v_m_tpo_credito = "01"
            LET v_marca_modulo  = "03"
         WHEN r_deudor_act.tpo_origina = 2
            LET v_m_tpo_credito = "02"
            LET v_marca_modulo  = "16"
         WHEN r_deudor_act.tpo_origina = 4
            LET v_m_tpo_credito = "04"
            LET v_marca_modulo  = "43" 
      END CASE} 

      -- Ejecuta web service de consulta de saldo Procesar para verificar la marca
      CALL consultaSaldo(v_url_servidor CLIPPED,
                         v_usuario      CLIPPED,
                         v_password     CLIPPED,
                         r_deudor_act.ap_materno CLIPPED,
                         r_deudor_act.ap_paterno CLIPPED,
                         r_deudor_act.nombre     CLIPPED,
                         r_deudor_act.nss)
               RETURNING soapStatus,
                         ConsultaSaldoRespVO.apeMaternoBD,
                         ConsultaSaldoRespVO.apePaternoBD,
                         ConsultaSaldoRespVO.diagProceso,
                         ConsultaSaldoRespVO.nombresBD,
                         ConsultaSaldoRespVO.nss,
                         ConsultaSaldoRespVO.numAIVS92,
                         ConsultaSaldoRespVO.numAIVS97,
                         ConsultaSaldoRespVO.origenTipoCredito,
                         ConsultaSaldoRespVO.resultOperacion,
                         ConsultaSaldoRespVO.tramiteJudicial

      --DISPLAY "SoapStatus: ",soapStatus
      --DISPLAY "RespVO_origenTipoCredito: ",ConsultaSaldoRespVO.origenTipoCredito
      
      -- Ejecución correcta
      IF(soapStatus = 0) THEN

         -- Verifica si está marcado en Procesar
         -- Si la respuesta es diferente al tipo de originación 04 quiere decir que no está marcado en Procesar
         IF(ConsultaSaldoRespVO.origenTipoCredito <> "04") OR
           (ConsultaSaldoRespVO.origenTipoCredito <> "02") OR 
           (ConsultaSaldoRespVO.origenTipoCredito <> "01") THEN

           --DISPLAY "No está marcado en Procesar"
           
            --Asigna valores que se envían al WS de Marca
            LET g_solicita_m.apeMaterno          = r_deudor_act.ap_materno CLIPPED
            LET g_solicita_m.apePaterno          = r_deudor_act.ap_paterno CLIPPED
            LET g_solicita_m.fechaPresentacion   = TODAY USING "yyyymmdd"
            LET g_solicita_m.nombres             = r_deudor_act.nombre CLIPPED
            LET g_solicita_m.nss                 = r_deudor_act.nss    CLIPPED
            LET g_solicita_m.numCreditoInfonavit = r_deudor_act.num_credito CLIPPED
            LET g_solicita_m.rfc                 = r_deudor_act.rfc CLIPPED
            LET g_solicita_m.sitCredito          = 2     -- Solicitud de Marca
            LET g_solicita_m.tipoCredito         = "04"  --v_m_tpo_credito

            -- Se ejecuta el WS de solicitud de marca
            CALL fn_solicita_marca(v_url_servidor_m CLIPPED,
                                   v_usuario_m  CLIPPED,
                                   v_password_m CLIPPED,
                                   v_intentos_m,
                                   g_solicita_m.*) 
                         RETURNING v_respuesta_m.*

            LET v_diagnostico   = v_respuesta_m.diagProceso
            LET v_cod_result_op = v_respuesta_m.resultOperacion

            --DISPLAY "Diagnostico sol marca: ",v_diagnostico
            --DISPLAY "cod_result_op sol marca: ",v_cod_result_op
            
            -- Guarda en histórico la solicitud de marca
            INSERT INTO cta_his_marca_ws(
                           id_derechohabiente,
                           id_origen ,
                           modulo_cod,
                           tpo_credito,
                           marca,
                           num_credito,
                           f_infonavit,
                           marca_procesar,
                           situacion,
                           f_solicita,
                           intento,
                           cod_result_op,
                           diagnostico,
                           f_actualiza, 
                           usuario,
                           folio_archivo,
                           usr_marca)
                   VALUES (r_deudor_act.id_derechohabiente,
                           r_deudor_act.id_cre_acreditado,
                           "43",
                           r_deudor_act.tpo_credito,
                           234,
                           r_deudor_act.num_credito,
                           r_deudor_act.f_otorga,
                           "04",
                           2,   -- Solicitud de Marca
                           TODAY,
                           1,
                           v_cod_result_op,
                           v_diagnostico,
                           TODAY,
                           p_usuario,
                           r_deudor_act.folio_archivo,
                           p_usuario);

            -- En caso de ser procedente para marcar
            -- Si el diagnóstico = "  " es aceptada
            IF(v_diagnostico = "   ") OR
              (v_diagnostico = "") THEN

               -- Verifica si la marca de Procesar ya está activa en SACI
               SELECT COUNT(*)
                 INTO v_marca_activa
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = r_deudor_act.id_derechohabiente
                  AND marca =  234;

               IF(v_marca_activa = 0) THEN
                  EXECUTE prp_marca_cuenta USING r_deudor_act.id_derechohabiente,
                                                 r_deudor_act.id_cre_acreditado,
                                                 r_deudor_act.folio_archivo,
                                                 "",
                                                 "",
                                                 p_usuario,
                                                 p_proceso_cod
                                            INTO v_result_marca
               END IF
            END IF
         ELSE
            -- Al existir la marca en Procesar, sólo se verifica que exista la marca 234 en SACI
            SELECT COUNT(*)
              INTO v_marca_activa
              FROM sfr_marca_activa
             WHERE id_derechohabiente = r_deudor_act.id_derechohabiente
               AND marca =  234;

            IF(v_marca_activa = 0) THEN

               EXECUTE prp_marca_cuenta USING r_deudor_act.id_derechohabiente,
                                              r_deudor_act.id_cre_acreditado,
                                              r_deudor_act.folio_archivo,
                                              "",
                                              "",
                                              p_usuario,
                                              p_proceso_cod
                                         INTO v_result_marca
            END IF
         END IF
      END IF

   END FOREACH

   FREE crs_deudor_actualizado
   
END FUNCTION

FUNCTION fn_genera_reporte_pdf()

   DEFINE v_reporte_bin   STRING 
   DEFINE v_ruta_rpt      STRING
   DEFINE object_rpt      om.SaxDocumentHandler
   DEFINE v_qry           STRING
   DEFINE r_aux_estado    RECORD
      num_credito_arh  DECIMAL(10,0),
      monto_deudor     DECIMAL(12,2),
      ind_vigente      SMALLINT,
      estado           SMALLINT
   END RECORD

   # Obtiene información para el reporte

   -- Inicializa arreglo
   LET g_arr_estado_credito[1].estado_desc  = "Líquidado (mismo crédito)"
   LET g_arr_estado_credito[1].total        = 0
   LET g_arr_estado_credito[1].monto_deudor = 0
   LET g_arr_estado_credito[2].estado_desc  = "Líquidado (con otro crédito vigente)"
   LET g_arr_estado_credito[2].total        = 0
   LET g_arr_estado_credito[2].monto_deudor = 0
   LET g_arr_estado_credito[3].estado_desc  = "Vigente"
   LET g_arr_estado_credito[3].total        = 0
   LET g_arr_estado_credito[3].monto_deudor = 0
   LET g_arr_estado_credito[4].estado_desc  = "En trámite"
   LET g_arr_estado_credito[4].total        = 0
   LET g_arr_estado_credito[4].monto_deudor = 0
   LET g_arr_estado_credito[5].estado_desc  = "Trámite Vencido/Revisar con crédito"
   LET g_arr_estado_credito[5].total        = 0
   LET g_arr_estado_credito[5].monto_deudor = 0
   LET g_arr_estado_credito[6].estado_desc  = "Cancelado/Revisar con crédito"
   LET g_arr_estado_credito[6].total        = 0
   LET g_arr_estado_credito[6].monto_deudor = 0
   LET g_arr_estado_credito[7].estado_desc  = "Total"
   LET g_arr_estado_credito[7].total        = 0
   LET g_arr_estado_credito[7].monto_deudor = 0

   LET v_qry = "SELECT num_credito_arh,
                       sdo_deudor_act,
                       ind_vigente,
                       estado
                  FROM safre_tmp:tmp_deudor_agr;"

   PREPARE prp_estado FROM v_qry
   DECLARE crs_estado CURSOR FOR prp_estado
   
   INITIALIZE r_aux_estado.* TO NULL

   FOREACH crs_estado INTO r_aux_estado.num_credito_arh,
                           r_aux_estado.monto_deudor,
                           r_aux_estado.ind_vigente,
                           r_aux_estado.estado

      -- Incremente total global
      LET g_arr_estado_credito[7].total = g_arr_estado_credito[7].total + 1
      LET g_arr_estado_credito[7].monto_deudor = g_arr_estado_credito[7].monto_deudor + r_aux_estado.monto_deudor

      IF(r_aux_estado.estado = 18) THEN
         LET g_arr_estado_credito[4].total        = g_arr_estado_credito[4].total + 1
         LET g_arr_estado_credito[4].monto_deudor = g_arr_estado_credito[4].monto_deudor + r_aux_estado.monto_deudor
      END IF
      
      IF(r_aux_estado.estado = 20)  OR 
        (r_aux_estado.estado = 140) OR 
        (r_aux_estado.estado = 145) OR
        (r_aux_estado.estado = 148) THEN
        LET g_arr_estado_credito[3].total        = g_arr_estado_credito[3].total + 1
        LET g_arr_estado_credito[3].monto_deudor = g_arr_estado_credito[3].monto_deudor + r_aux_estado.monto_deudor
      END IF
      
      -- Si el crédito está liquidado pero con otro crédito vigente y diferente número de credito
      IF(r_aux_estado.estado = 170) AND (r_aux_estado.ind_vigente = 1) THEN
         LET g_arr_estado_credito[2].total        = g_arr_estado_credito[2].total + 1
         LET g_arr_estado_credito[2].monto_deudor = g_arr_estado_credito[2].monto_deudor + r_aux_estado.monto_deudor
      ELSE
         -- Crédito liquidado
         IF(r_aux_estado.estado = 170) AND (r_aux_estado.ind_vigente = 0) THEN
            LET g_arr_estado_credito[1].total        = g_arr_estado_credito[1].total + 1
            LET g_arr_estado_credito[1].monto_deudor = g_arr_estado_credito[1].monto_deudor + r_aux_estado.monto_deudor
         END IF 
      END IF
      
      IF(r_aux_estado.estado = 290) THEN
         LET g_arr_estado_credito[5].total        = g_arr_estado_credito[5].total + 1
         LET g_arr_estado_credito[5].monto_deudor = g_arr_estado_credito[5].monto_deudor + r_aux_estado.monto_deudor
      END IF
      
      IF(r_aux_estado.estado = 174) THEN
         LET g_arr_estado_credito[6].total        = g_arr_estado_credito[6].total + 1
         LET g_arr_estado_credito[6].monto_deudor = g_arr_estado_credito[6].monto_deudor + r_aux_estado.monto_deudor
      END IF
      
   END FOREACH

   FREE crs_estado
   
   # -----> CONFIGURACIÓN DEL REPORTE PDF <-----
   
   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP45.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",p_usuario CLIPPED,"-AGRP45-",
                       p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         START REPORT genera_PDF TO XML HANDLER object_rpt
            OUTPUT TO REPORT genera_PDF()
         FINISH REPORT genera_PDF
      ELSE
         DISPLAY "ERROR: No fué posible abrir la plantilla del reporte"
      END IF
   END IF 

END FUNCTION   

REPORT genera_PDF()

   DEFINE v_f     INTEGER

   FORMAT 
      FIRST PAGE HEADER 
         LET v_fecha     = TODAY
         
         #Encabezado
         PRINTX p_usuario
         PRINTX v_fecha USING "dd/mm/yyyy"
         #RESUMEN
         PRINTX p_archivo
         PRINTX v_tot_procesa
         PRINTX v_monto_procesa
         PRINTX v_tot_acept
         PRINTX v_monto_acep
         PRINTX v_tot_rch
         PRINTX v_monto_rch

      ON EVERY ROW
         -- Imprime arreglo 
         FOR v_f = 1 TO g_arr_estado_credito.getLength()
            PRINTX g_arr_estado_credito[v_f].estado_desc
            PRINTX g_arr_estado_credito[v_f].total       
            PRINTX g_arr_estado_credito[v_f].monto_deudor
         END FOR

END REPORT 

FUNCTION fn_genera_archivos_salida()

   DEFINE v_cadena       STRING
   DEFINE archivo1       base.channel
   DEFINE archivo2       base.Channel
   DEFINE archivo3       base.Channel
   DEFINE v_arh_deudores STRING
   DEFINE v_arh_sin_deu  STRING
   DEFINE v_arh_estatus  STRING
   DEFINE v_detalle      STRING
   DEFINE r_det_rch      RECORD
      num_credito   CHAR(10),
      causal        CHAR(3),
      desc_causal   CHAR(40)
   END RECORD
   DEFINE r_deudores_act RECORD 
      nss         CHAR(11),
      num_credito CHAR(10),
      sdo_deudor  CHAR(14),
      f_actualiza DATE
   END RECORD
   DEFINE r_estatus_credito RECORD
      num_credito_arh CHAR(10),
      sdo_deudor      CHAR(14),
      ind_vigente     SMALLINT,
      estado          SMALLINT,
      estado_desc     CHAR(40)
   END RECORD

   LET v_arh_deudores = v_ruta_envio CLIPPED,"/Actualizacion_Deudor_",TODAY USING "yyyymmdd",".txt"
   LET v_arh_sin_deu  = v_ruta_envio CLIPPED,"/Registros_sin_NSS_",TODAY USING "yyyymmdd",".txt"
   LET v_arh_estatus  = v_ruta_envio CLIPPED,"/",p_archivo CLIPPED
   
   --Deudores que se actualizaron
   LET v_cadena = "SELECT afi.nss,
                          tmp.num_credito_arh,
                          tmp.sdo_deudor_act,
                          tmp.f_actualiza
                     FROM safre_tmp:tmp_deudor_agr tmp,
                          afi_derechohabiente afi
                    WHERE tmp.id_derechohabiente = afi.id_derechohabiente
                      AND tmp.estado NOT IN (174,290)"

   LET archivo1 = base.Channel.create()

   CALL archivo1.openFile(v_arh_deudores,"w")
   
   PREPARE prp_deud_act FROM v_cadena
   DECLARE crs_deud_act CURSOR FOR prp_deud_act
 
   INITIALIZE r_deudores_act.* TO NULL
   LET v_detalle = NULL 
   
   FOREACH crs_deud_act INTO r_deudores_act.nss,
                              r_deudores_act.num_credito,
                              r_deudores_act.sdo_deudor,
                              r_deudores_act.f_actualiza

      LET v_detalle = r_deudores_act.nss,"|",
                      r_deudores_act.num_credito CLIPPED,"|",
                      r_deudores_act.sdo_deudor CLIPPED,"|",
                      r_deudores_act.f_actualiza USING "yyyymmdd","|"

      CALL archivo1.writeLine(v_detalle)
      
   END FOREACH 

   FREE crs_deud_act
   CALL archivo1.close()

   --deudores no encontrados por número de crédito
   LET v_cadena = "SELECT t.num_credito,
                          t.diagnostico,
                          r.desc_estado
                     FROM safre_tmp:tmp_sin_nss_deudor t,
                          cat_rch_acreditado r
                    WHERE t.diagnostico = r.estado"

   LET archivo2 = base.Channel.create()
   CALL archivo2.openFile(v_arh_sin_deu,"w")

   PREPARE prp_sin_deudor FROM v_cadena
   DECLARE crs_sin_deudor CURSOR FOR prp_sin_deudor

   INITIALIZE r_det_rch.* TO NULL
   LET v_detalle     = NULL 

   FOREACH crs_sin_deudor INTO r_det_rch.num_credito,
                               r_det_rch.causal,
                               r_det_rch.desc_causal

      LET v_detalle = r_det_rch.num_credito,"|",
                      r_det_rch.causal USING "&&&","-",
                      r_det_rch.desc_causal CLIPPED,"|"
      
      CALL archivo2.writeLine(v_detalle)
      
   END FOREACH 

   FREE crs_sin_deudor
   CALL archivo2.close()

   -- Archivo espejo de los deudores que no fueron rechazados
   LET v_cadena = "SELECT num_credito_arh,                          
                          sdo_deudor_act,
                          ind_vigente,
                          estado
                     FROM safre_tmp:tmp_deudor_agr"

   LET archivo3 = base.Channel.create()
   CALL archivo3.openFile(v_arh_estatus,"w")

   PREPARE prp_estatus_credito FROM v_cadena
   DECLARE crs_estatus_credito CURSOR FOR prp_estatus_credito

   INITIALIZE r_estatus_credito.* TO NULL
   LET v_detalle = NULL

   FOREACH crs_estatus_credito INTO r_estatus_credito.num_credito_arh,
                                    r_estatus_credito.sdo_deudor,
                                    r_estatus_credito.ind_vigente,
                                    r_estatus_credito.estado

      IF(r_estatus_credito.estado = 18) THEN
          LET r_estatus_credito.estado_desc = "En trámite"
      END IF
      
      IF(r_estatus_credito.estado = 20)  OR 
        (r_estatus_credito.estado = 140) OR 
        (r_estatus_credito.estado = 145) OR
        (r_estatus_credito.estado = 148) THEN
          LET r_estatus_credito.estado_desc = "Vigente"
      END IF
      
      -- Crédito liquidado pero con un crédito vigente con diferente número de crédito
      IF(r_estatus_credito.estado = 170) AND (r_estatus_credito.ind_vigente = 1)THEN
         LET r_estatus_credito.estado_desc = "Liquidado (con otro crédito vigente)"
      ELSE
         -- Crédito liquidado
         IF(r_estatus_credito.estado = 170) AND (r_estatus_credito.ind_vigente = 0) THEN
          LET r_estatus_credito.estado_desc = "Liquidado (mismo crédito)"
         END IF
      END IF
      
      IF(r_estatus_credito.estado = 290) THEN
          LET r_estatus_credito.estado_desc = "Trámite vencido/Revisar con crédito"
      END IF
      
      IF(r_estatus_credito.estado = 174) THEN
         LET r_estatus_credito.estado_desc =  "Cancelado/Revisar con crédito"
      END IF
      
      LET v_detalle = r_estatus_credito.num_credito_arh,
                      r_estatus_credito.sdo_deudor USING "&&&&&&&&&&&.&&",
                      r_estatus_credito.estado_desc
        
      CALL archivo3.writeLine(v_detalle)
      
   END FOREACH

   FREE crs_estatus_credito
   CALL archivo3.close()
   
END FUNCTION 

FUNCTION fn_crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_deudor_sap_fico
      DROP TABLE tmp_sin_nss_deudor
      DROP TABLE tmp_deudor_agr
      
   WHENEVER ERROR STOP 
      CREATE TABLE tmp_deudor_sap_fico (
                       num_credito  DECIMAL(10,0),
                       monto_deudor DECIMAL(12,2)
                       ) IN tmp_2_dbs; 

      CREATE TABLE tmp_sin_nss_deudor(
                       num_credito DECIMAL(10,0),
                       diagnostico SMALLINT
                       ) IN tmp_2_dbs;

      CREATE TABLE tmp_deudor_agr(
                       id_cre_acreditado  DECIMAL(9,0),
                       id_derechohabiente DECIMAL(9,0),
                       num_credito_arh    DECIMAL(10,0),   -- Número de crédito cargado en el archivo
                       ind_vigente        SMALLINT,   -- Número de crédito original encontrado en cre_acreditado diferente al numero de credito cargado en el archivo
                       sdo_deudor_act     DECIMAL(12,2),
                       estado             SMALLINT,
                       f_actualiza        DATE
                       ) IN tmp_2_dbs;

   DATABASE safre_viv
   
END FUNCTION 
