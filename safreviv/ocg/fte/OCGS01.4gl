--===============================================================
-- Versi�n: 1.0.0
-- Fecha �ltima modificaci�n:
--===============================================================

#####################################################################
#Modulo            => OCG                                           #
#Programa          => OCGS01                                        #
#Objetivo          => Programa que ejecuta el proceso de generaci�n #
#                     de archivo de salida de 43BIS                 #
#Autor             => Jos� Eduardo Ventura                          #
#Fecha inicio      => 15 Enero 2016                                 #
#####################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario                 LIKE seg_usuario.usuario            -- nombre del usuario
   DEFINE p_pid                     LIKE bat_ctr_proceso.pid            -- pid
   DEFINE p_proceso_cod             LIKE cat_proceso.proceso_cod        -- codigo del proceso
   DEFINE p_opera_cod               LIKE cat_operacion.opera_cod        -- codigo de la operacion de la etapa
   DEFINE p_f_generacion            DATE
   DEFINE p_f_publicacion           DATE
   DEFINE v_ruta_envio              LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE a                         INTEGER
   DEFINE ch                        base.Channel
   DEFINE v_nom_arh                 STRING
   DEFINE v_folio                   CHAR(10)
   DEFINE v_cp_arch                 STRING
   DEFINE v_cmd                     STRING
   DEFINE v_archivo                 CHAR(40)
   DEFINE v_cta_t                   INTEGER
   DEFINE v_cta_f                   INTEGER
   DEFINE v_cta_g                   INTEGER
   DEFINE v_cta_l                   INTEGER
   DEFINE v_cta_t1                  INTEGER
   DEFINE v_cta_f1                  INTEGER
   DEFINE v_cta_g1                  INTEGER
   DEFINE v_cta_l1                  INTEGER
   DEFINE v_cta_tr                  INTEGER
   DEFINE v_cta_rh                  INTEGER
   DEFINE v_cta_tr1                 INTEGER
   DEFINE v_cta_tot                 INTEGER
   DEFINE v_comando                 STRING
   DEFINE v_cta_incons              SMALLINT
   DEFINE i                         SMALLINT
   DEFINE v_consecutivo             CHAR(2)
   DEFINE v_cta                     INTEGER
   DEFINE v_id_referencia           DECIMAL(9,0)
   DEFINE v_id_detalle              DECIMAL(9,0)
   DEFINE v_id_dh                   DECIMAL(9,0)
   DEFINE v_f_respuesta             DATE
   DEFINE v_dia_sig                 DATE
   DEFINE v_respuesta               DATE
   DEFINE v_cve_ef                  SMALLINT

   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0)

   DEFINE arr_encabezado DYNAMIC ARRAY OF RECORD
      tpo_registro                  CHAR(1),
      identificador                 CHAR(8),
      destino                       CHAR(3),
      f_transferencia               CHAR(8),
      situacion                     CHAR(1),
      filler                        CHAR(619)
   END RECORD

   DEFINE arr_trabajador DYNAMIC ARRAY OF RECORD
      id_ocg                        DECIMAL(9,0),
      id_ocg_detalle                DECIMAL(9,0),
      id_dh                         DECIMAL(9,0),
      tpo_registro                  CHAR(1),
      subproceso                    CHAR(3),
      tpo_envio                     CHAR(1),
      f_envio                       CHAR(8),
      cve_ent_financiera            SMALLINT,
      nss                           CHAR(11),
      num_ctrl_ef                   CHAR(18),
      rfc                           CHAR(13),
      curp                          CHAR(18),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombre_af                     CHAR(40),
      num_bimestres                 CHAR(3),
      viv92                         CHAR(8),
      viv97                         CHAR(8),
      f_corte_subcuenta             CHAR(8),
      diagnostico                   SMALLINT,
      inconsistencias               CHAR(40),
      num_escritura                 CHAR(8),
      num_notario                   CHAR(4),
      ent_fed_notario               CHAR(2),
      municipio_notario             CHAR(3),
      num_rpp                       CHAR(15),
      folio_real                    CHAR(8),
      partida                       CHAR(6),
      foja                          CHAR(8),
      volumen                       CHAR(6),
      libro                         CHAR(6),
      tomo                          CHAR(6),
      seccion                       CHAR(6),
      ent_fed_inmueble              CHAR(2),
      domicilio_inmueble            CHAR(30),
      valor_avaluo                  CHAR(15),
      monto_credito                 CHAR(15),
      plazo_credito                 CHAR(5),
      tpo_moneda                    CHAR(2),
      tasa_base                     CHAR(20),
      margen                        CHAR(20),
      f_otorga_cred_ef              CHAR(8),
      imp_solic_uti_ocg             DECIMAL(13,2),
      f_venc_imp_solic              CHAR(8),
      imp_utilizado_ocg             DECIMAL(13,2),
      imp_aport_subsec              DECIMAL(13,2),
      bim_apor_subsec               CHAR(6),
      imp_subsec_devuelto           DECIMAL(13,2),
      f_libera_garantia             CHAR(8),
      imp_ocg_devuelto              DECIMAL(13,2),
      causa_liquidacion             CHAR(1),
      f_deposito                    CHAR(8),
      cred_convenidos               CHAR(1),
      solic_saldo                   CHAR(1),
      f_vigencia                    CHAR(8),
      filler                        CHAR(64),
      situacion                     SMALLINT,
      concepto                      SMALLINT
   END RECORD

   DEFINE arr_sumario DYNAMIC ARRAY OF RECORD
      tpo_registro                  CHAR(1),
      identificador                 CHAR(8),
      destino                       CHAR(3),
      tot_registros                 CHAR(6),
      filler                        CHAR(622)
   END RECORD

   DEFINE arr_incons DYNAMIC ARRAY OF RECORD
      inconsistencia                CHAR(2)
   END RECORD

   DEFINE v_f_liquida_cofi          DATE
   DEFINE v_cve_conyuge             CHAR(1)
   DEFINE v_nss_conyuge             CHAR(11)
   DEFINE v_anyo                    CHAR(4)
   DEFINE v_mes                     CHAR(2)
   DEFINE v_dia                     CHAR(2)
   DEFINE v_id_ocg_forma            DECIMAL(9,0)

END GLOBALS

MAIN

   DEFINE v_msj                     STRING
   DEFINE v_env_comando             STRING
   DEFINE v_ind_envio               BOOLEAN

--CLOSE WINDOW SCREEN

   -- se recuperan los par�metros
   LET p_usuario         = ARG_VAL(1)
   LET p_pid             = ARG_VAL(2)
   LET p_proceso_cod     = ARG_VAL(3)
   LET p_opera_cod       = ARG_VAL(4)
   LET p_f_generacion    = ARG_VAL(5)
   LET p_f_publicacion   = ARG_VAL(6)

   LET v_cta_t  = 0
   LET v_cta_f  = 0
   LET v_cta_g  = 0
   LET v_cta_l  = 0
   LET v_cta_t1 = 0
   LET v_cta_f1 = 0
   LET v_cta_g1 = 0
   LET v_cta_l1 = 0
   LET v_cta_rh = 0

   -- se obtienen la ruta env�o del m�dulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   SELECT MAX (estatus)
     INTO v_cta
     FROM ocg_ctr_archivo
    WHERE id_proceso = 3909
      AND f_proceso = TODAY

   IF (v_cta IS NULL) OR (v_cta < 1) THEN
      LET v_cta = 0
   END IF

   LET v_consecutivo = (v_cta + 1)

  -- DISPLAY "consecutivo para archivo : ",v_consecutivo

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/Art43bis",".Sal"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGS01.log")

   DISPLAY "=INICIA OCGS01="
   DISPLAY " USUARIO           : ",p_usuario
   DISPLAY " PID               : ",p_pid
   DISPLAY " Fecha Generaci�n  : ",p_f_generacion
   DISPLAY " Fecha Publicaci�n : ",p_f_publicacion

   -- se crea tabla temporal
   CALL fn_crea_tabla()

   -- se ejecuta la funci�n que genera la informaci�n
   CALL fn_archivo_43bis()

   -- se ejecuta la funci�n que genera el archivo de salida
   CALL fn_escribe()

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio

   LET v_cp_arch = v_ruta_envio CLIPPED ,"/Art43bis",TODAY USING "DDMMYYYY","_",v_consecutivo CLIPPED,".Sal"
   LET v_cmd     = "cp ",v_nom_arh," ",v_cp_arch

   RUN v_cmd

   --comando para convertir de formato unix a formato dos el archivo
   LET v_comando = " sed 's/$/\r/' ", v_cp_arch CLIPPED," > ",v_nom_arh CLIPPED
   RUN v_comando

   LET v_archivo = "Art43bis",TODAY USING "DDMMYYYY","_",v_consecutivo CLIPPED,".Sal"
   LET v_cta_tot = v_cta_t + v_cta_f + v_cta_g + v_cta_g1 + v_cta_l + v_cta_tr + v_cta_rh + 2

   INSERT INTO ocg_ctr_archivo
   VALUES (seq_ocg_archivo.NEXTVAL,
           v_folio,
           "",
           p_proceso_cod,
           p_opera_cod,
           v_archivo,
           v_cta_tot,
           v_cta_t,
           v_cta_f,
           v_cta_g,
           v_cta_g1,
           v_cta_l,
           v_consecutivo,
           TODAY,
           p_usuario)

   LET v_archivo = "Art43bis",TODAY USING "DDMMYYYY","_","1",".ent"

   UPDATE glo_ctr_archivo
      SET nombre_archivo = v_archivo
    WHERE nombre_archivo = "Art43bis.ent"

   UPDATE ocg_ctr_archivo
      SET nom_archivo = v_archivo
    WHERE nom_archivo = "Art43bis.ent"



   LET v_msj = "Archivo generado de forma correcta en: "

   DISPLAY ""
   DISPLAY " ",v_msj," "
   DISPLAY v_nom_arh
   DISPLAY ""
   DISPLAY "PROCESO EJECUTADO CORRECTAMENTE"
   DISPLAY ""
   DISPLAY "=FINALIZA ARCHIVO DE SALIDA 43BIS="


--******************************************************************************
-- modificaci�n para trasnferncia de archivo 24/04/2019

   -- Ejecuci�n del Script de env�o del archivo a TRM
   LET v_env_comando = "sh"," ","/opt/Interpel/Scripts/SOA18068-Prod.sh" #Env�o de Producci�n
   --LET v_env_comando = "sh"," ","/opt/Interpel/Scripts/SOA18068-QA.sh" #Env�o QA 
   RUN v_env_comando RETURNING v_ind_envio

   IF(v_ind_envio = 0) THEN
      DISPLAY " => El archivo de Salida 43Bis se ha enviado exitosamente "
      DISPLAY ""
   ELSE 
      DISPLAY " => No se pudo enviar el archivo de Salida 43Bis "
      DISPLAY ""
   END IF

END MAIN

FUNCTION fn_archivo_43bis()

   DEFINE v_qry_ef                  STRING
   DEFINE v_qry_tramite             STRING
   DEFINE v_qry_formalizacion       STRING
   DEFINE v_qry_garantia            STRING
   DEFINE v_qry_transaccion         STRING
   DEFINE v_qry_liquidacion         STRING
   DEFINE v_qry_rechazo             STRING
   DEFINE v_incons_t                INTEGER
   DEFINE v_incons_f                INTEGER
   DEFINE v_incons_g                INTEGER
   DEFINE v_incons_l                INTEGER
   DEFINE v_incons_rh               INTEGER
   DEFINE v_qry_incons              STRING
   DEFINE z                         INTEGER
   DEFINE v_incons                  STRING
   DEFINE v_det_enc                 STRING
   DEFINE v_det_sum                 STRING
   DEFINE v_incons_tr               INTEGER
   DEFINE v_proceso_cod             SMALLINT
   DEFINE v_qry_garantia_2          STRING
   DEFINE v_aux_imp_solic_uti_ocg   CHAR(15)
   DEFINE v_aux_imp_utilizado_ocg   CHAR(15)
   DEFINE v_aux_imp_aport_subsec    CHAR(15)
   DEFINE v_aux_imp_subsec_devuelto CHAR(15)
   DEFINE v_aux_imp_ocg_devuelto    CHAR(15)

   CALL arr_trabajador.clear()

   -- DISPLAY "cuenta registros tramite:",v_cta_t

   -- cadena para archivo de salida con detalle para SP001 tramite
   LET v_qry_tramite = "SELECT ocg.id_ocg_tramite,
                               (ocg.viv97*100),
                               lpad(year(ocg.f_saldo),4,0)||
                               lpad(month(ocg.f_saldo),2,0)||
                               lpad(day(ocg.f_saldo),2,0),
                               ocg.diagnostico,
                               lpad(year(ocg.f_vigencia),4,0)||
                               lpad(month(ocg.f_vigencia),2,0)||
                               lpad(day(ocg.f_vigencia),2,0),
                               ocg.cve_ent_financiera,
                               det.nss,
                               ocg.rfc,
                               ocg.curp,
                               ocg.ap_paterno,
                               ocg.ap_materno,
                               ocg.nombre,
                               ocg.tpo_credito,
                               ocg.situacion
                          FROM ocg_tramite ocg, ocg_detalle det
                         WHERE ocg.situacion          IN(20,30)
                           AND det.id_ocg_detalle     = ocg.id_ocg_detalle"
                           ---AND ocg.cve_ent_financiera = ",v_cve_ef

   --cadena para archivo de salida con detalle para SP002 fromalizaci�n
   LET v_qry_formalizacion = "SELECT ocg.id_ocg_formalizacion,
                                     ocg.cve_ent_financiera,
                                     det.nss,
                                     ocg.num_ctr_int_ef,
                                     ocg.rfc,
                                     ocg.curp,
                                     ocg.ap_paterno,
                                     ocg.ap_materno,
                                     ocg.nombre,
                                     ocg.diagnostico,
                                     ocg.num_escritura,
                                     ocg.notario,
                                     ocg.ent_fed_notario,
                                     ocg.mcpio_notario,
                                     ocg.num_rpp,
                                     ocg.folio_real,
                                     ocg.partida,
                                     ocg.foja,
                                     ocg.volumen,
                                     ocg.libro,
                                     ocg.tomo,
                                     ocg.seccion,
                                     ocg.ent_fed_inmueble,
                                     ocg.domicilio_inmueble,
                                     (ocg.valor_avaluo*100),
                                     (ocg.monto_credito*100),
                                     ocg.plazo_credito,
                                     ocg.tpo_moneda,
                                     ocg.tasa_base,
                                     ocg.margen,
                                     lpad(year(ocg.f_otorga_ent_fin),4,0)||
                                     lpad(month(ocg.f_otorga_ent_fin),2,0)||
                                     lpad(day(ocg.f_otorga_ent_fin),2,0),
                                     ocg.tpo_credito,
                                     ocg.situacion
                                FROM ocg_formalizacion ocg,
                                     ocg_detalle det
                               WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                                 AND ocg.situacion IN(20,55)"
                                 ---AND ocg.cve_ent_financiera =",v_cve_ef

   -- cadena para archivo de salida con detalle de pagos para SP003 garantia
   LET v_qry_garantia = "SELECT ocg.id_ocg_solicitud_ug,
                                t.id_ocg_detalle,
                                ocg.cve_ent_financiera,
                                det.nss,
                                ocg.num_ctr_int_ef,
                                ocg.diagnostico,
                                ocg.importe_solicitado,
                                lpad(year(ocg.f_vencimiento),4,0)||
                                lpad(month(ocg.f_vencimiento),2,0)||
                                lpad(day(ocg.f_vencimiento),2,0),
                                ocg.importe_utilizado,
                                ocg.solicitud_saldo,
                                ocg.situacion
                           FROM ocg_solicitud_uso_garantia ocg,
                                ocg_detalle det,
                                ocg_ctr_transaccion t
                          WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                            AND ocg.situacion = 100
                            AND det.subproceso = 3
                            AND ocg.id_ocg_formalizacion = t.id_ocg_formalizacion
                            AND ocg.id_derechohabiente = t.id_derechohabiente
                            AND ocg.cve_ent_financiera = t.cve_ent_financiera
                            AND ocg.importe_utilizado = t.vivienda_97
                            AND year(ocg.f_vencimiento)||lpad(month(ocg.f_vencimiento),2,0) = t.periodo_pago
                            AND t.estado = 30
                            AND ocg.solicitud_saldo = 2
                            AND t.concepto IN(407,417)
                            AND ocg.tpo_credito IN('A','C')"

                         {
                          WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                            AND ocg.situacion IN(20,100)
                            AND ocg.id_ocg_formalizacion = t.id_ocg_formalizacion
                            AND t.estado IN(40,60)"
                            ---AND ocg.cve_ent_financiera =",v_cve_ef
                         }

         -- cadena para archivo de salida con detalle de rechazos para SP003 garantia
   LET v_qry_garantia_2 = "SELECT ocg.id_ocg_solicitud_ug,
                                ocg.cve_ent_financiera,
                                det.nss,
                                ocg.num_ctr_int_ef,
                                ocg.diagnostico,
                                ocg.importe_solicitado,
                                lpad(year(ocg.f_vencimiento),4,0)||
                                lpad(month(ocg.f_vencimiento),2,0)||
                                lpad(day(ocg.f_vencimiento),2,0),
                                ocg.solicitud_saldo,
                                ocg.situacion
                           FROM ocg_solicitud_uso_garantia ocg,
                                ocg_detalle det
                          WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                            AND ocg.situacion = 20
                            AND ocg.solicitud_saldo = 2 "
                            ---AND ocg.cve_ent_financiera =",v_cve_ef

      -- cadena para archivo de salida con detalle de pagos de SP003 usos de Garant�a en tabla de transacci�n
        {
         lpad(year(ocg.f_pago),4,0)||
         lpad(month(ocg.f_pago),2,0)||
         lpad(day(ocg.f_pago),2,0)
        }

   --cadena para archivo de salida con detalle para SP004 transacciones
   --in (40,60) condici�n para estado
   LET v_qry_transaccion = "SELECT ocg.id_ocg_ctr_transaccion,
                                   ocg.cve_ent_financiera,
                                   ocg.nss,
                                   ocg.num_ctr_int_ef,
                                   (ocg.vivienda_97*100),
                                   ocg.periodo_pago,
                                   ocg.estado, 
                                   ocg.concepto
                              FROM ocg_ctr_transaccion ocg
                             WHERE ocg.estado = 30
                               AND ocg.concepto IN (807,107,307,817,117,317)
                               AND ocg.concepto NOT IN(808,108,308,408,818,118,318,418,407,417)"
                               ---AND ocg.cve_ent_financiera =",v_cve_ef

   -- cadena para archivo de salida con detalle para SP005 liquidaci�n
   LET v_qry_liquidacion = "SELECT ocg.id_ocg_liquidacion,
                                   ocg.cve_ent_financiera,
                                   det.nss,
                                   ocg.num_ctr_int_ef,
                                   ocg.diagnostico,
                                   ocg.bimestre_ap_subsec,
                                   (ocg.importe_ap_subsec*100),
                                   lpad(year(ocg.f_liberacion_gtia),4,0)||
                                   lpad(month(ocg.f_liberacion_gtia),2,0)||
                                   lpad(day(ocg.f_liberacion_gtia),2,0),
                                   (ocg.importe_devuelto*100),
                                   ocg.id_causa_liquida,
                                   lpad(year(ocg.f_deposito),4,0)||
                                   lpad(month(ocg.f_deposito),2,0)||
                                   lpad(day(ocg.f_deposito),2,0),
                                   ocg.situacion,
                                   ocg.tpo_credito
                              FROM ocg_liquidacion ocg,
                                   ocg_detalle det
                             WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                               AND ocg.situacion in (20,158,153,175,165)"
                               ---AND ocg.cve_ent_financiera =",v_cve_ef

   -- cadena para archivo de salida con detalle para registros rechazados en validaci�n
   LET v_qry_rechazo  = "SELECT id_tmp_det,
                                tpo_registro,
                                subproceso,
                                tpo_envio,
                                f_envio,
                                cve_ent_financiera,
                                nss,
                                num_ctrl_ef,
                                rfc,
                                curp,
                                ap_paterno_af,
                                ap_materno_af,
                                nombre_af,
                                num_bimestres,
                                viv92,
                                viv97,
                                f_corte_subcuenta,
                                diagnostico,
                                inconsistencias,
                                num_escritura,
                                num_notario,
                                ent_fed_notario,
                                municipio_notario,
                                num_rpp,
                                folio_real,
                                partida,
                                foja,
                                volumen,
                                libro,
                                tomo,
                                seccion,
                                ent_fed_inmueble,
                                domicilio_inmueble,
                                valor_avaluo,
                                monto_credito,
                                plazo_credito,
                                tpo_moneda,
                                tasa_base,
                                margen,
                                f_otorga_cred_ef,
                                imp_solic_uti_ocg,
                                f_venc_imp_solic,
                                imp_utilizado_ocg,
                                imp_aport_subsec,
                                bim_apor_subsec,
                                imp_subsec_devuelto,
                                f_libera_garantia,
                                imp_ocg_devuelto,
                                causa_liquidacion,
                                f_deposito,
                                cred_convenidos,
                                solic_saldo,
                                f_vigencia,
                                estado
                           FROM ocg_rechazo_ent
                          WHERE f_proceso = TODAY
                            AND estado    = 20"

   --******************************************************************************
   LET v_dia_sig = TODAY
   LET v_comando = "EXECUTE FUNCTION fn_calcula_habil_siguiente(?)"

   PREPARE prp_habil_sig_tr FROM v_comando
   EXECUTE prp_habil_sig_tr USING v_dia_sig INTO v_respuesta

   LET a = 1
   --******************************************************************************

   --DISPLAY "punto de control 1"

   SELECT COUNT(*)
     INTO v_cta_t
     FROM ocg_tramite ocg, ocg_detalle det
    WHERE ocg.situacion IN(20,30)
      AND det.id_ocg_detalle = ocg.id_ocg_detalle

   IF v_cta_t IS NULL THEN
      LET v_cta_t = 0
   END IF

   IF v_cta_t > 0 THEN
      -- se llena arreglo con datos de tabla tramite
      PREPARE prp_tramite FROM v_qry_tramite
      DECLARE cur_tramite CURSOR FOR prp_tramite

      --LET a = 1

      LET v_f_liquida_cofi = NULL
      LET v_cve_conyuge    = " "
      LET v_nss_conyuge    = "           "

      FOREACH cur_tramite INTO arr_trabajador[a].id_ocg,
                               arr_trabajador[a].viv97,
                               arr_trabajador[a].f_corte_subcuenta,
                               arr_trabajador[a].diagnostico,
                               arr_trabajador[a].f_vigencia,
                               arr_trabajador[a].cve_ent_financiera,
                               arr_trabajador[a].nss,
                               arr_trabajador[a].rfc,
                               arr_trabajador[a].curp,
                               arr_trabajador[a].ap_paterno_af,
                               arr_trabajador[a].ap_materno_af,
                               arr_trabajador[a].nombre_af,
                               arr_trabajador[a].cred_convenidos,
                               arr_trabajador[a].situacion

        IF arr_trabajador[a].cred_convenidos = "7" OR
           arr_trabajador[a].cred_convenidos = "8" THEN
           LET arr_trabajador[a].diagnostico = "00"
        END IF

         --DISPLAY "datos de arreglo tramite:",arr_trabajador[a].*
         SELECT COUNT(*)
           INTO v_incons_t
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

         -- DISPLAY "inconsistencias tramite :",v_incons_t

         IF v_incons_t > 0 THEN
            LET v_qry_incons = "SELECT inconsistencia
                                  FROM ocg_inconsistencia
                                 WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 1 "

            PREPARE prp_incons FROM v_qry_incons
            DECLARE cur_incons CURSOR FOR prp_incons

            LET z= 1

            CALL arr_incons.clear()

            FOREACH cur_incons INTO arr_incons[z].inconsistencia
               LET z= z+1
            END FOREACH

            LET v_incons = ""

            FOR z=1 TO v_incons_t
               LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia  USING "&&"
               --DISPLAY "inconsistencias :",v_incons
            END FOR

   --******************************************************************************
            LET v_incons     = v_incons CLIPPED
            LET v_cta_incons = v_incons.getLength()

            FOR i = v_incons.getLength() TO 40
               LET v_incons = v_incons CLIPPED,"0"
            END FOR 
   --******************************************************************************
         END IF

         LET arr_trabajador[a].tpo_registro     = "2"
         LET arr_trabajador[a].subproceso       = "001"
         LET arr_trabajador[a].tpo_envio        = "I"
         LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
         LET arr_trabajador[a].inconsistencias  = v_incons
         LET arr_trabajador[a].viv92            = "0.0"

         --************Modificaciones 05/05/2017 sp001 salida para cofinanciados*********

         IF (arr_trabajador[a].cred_convenidos = "7") OR
            (arr_trabajador[a].cred_convenidos = "8") THEN
            LET arr_trabajador[a].f_corte_subcuenta = NULL

            SELECT MAX(f_liquida_cofi)
              INTO v_f_liquida_cofi
              FROM ocg_fecha_mig
             WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
               AND subproceso        = 1

            LET v_anyo = year(v_f_liquida_cofi)
            LET v_mes  = month(v_f_liquida_cofi) USING "&&"
            LET v_dia  = day(v_f_liquida_cofi) USING "&&"

            LET arr_trabajador[a].foja = v_anyo||v_mes||v_dia 

            --FOREACH
               SELECT FIRST 1 marca_conyuge,
                      nss_conyuge   
                 INTO v_cve_conyuge,
                      v_nss_conyuge
                 FROM ocg_liquidacion_cofi
                WHERE id_derechohabiente IN(
                             SELECT id_derechohabiente
                               FROM ocg_tramite
                              WHERE id_ocg_tramite = arr_trabajador[a].id_ocg)
               ORDER BY f_proceso DESC
            --END FOREACH
--DISPLAY v_cve_conyuge,"  ",v_nss_conyuge

            IF v_nss_conyuge = "           " OR v_nss_conyuge = "" OR v_nss_conyuge IS NULL THEN
               LET v_nss_conyuge = "00000000000"
               LET v_cve_conyuge = "I"
            END IF

            IF v_cve_conyuge = " " OR v_cve_conyuge IS NULL OR v_cve_conyuge = "" THEN
               IF v_nss_conyuge = "           " OR v_nss_conyuge = "" OR v_nss_conyuge IS NULL THEN
                  LET v_nss_conyuge = "00000000000"
                  LET v_cve_conyuge = "I"
               ELSE
                  LET v_cve_conyuge = "C"
               END IF
            END IF

            IF v_cve_conyuge <> "C" THEN
               LET v_cve_conyuge = "I"
            END IF

            IF v_cve_conyuge = " " OR v_cve_conyuge IS NULL OR v_cve_conyuge = "" THEN
               LET v_cve_conyuge = "I"
            END IF

            LET arr_trabajador[a].filler = "   "||v_cve_conyuge||v_nss_conyuge||"                                                 "
            LET arr_trabajador[a].viv92  = "        "
            LET arr_trabajador[a].viv97  = "        "
         END IF
--******************************************************************************

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

        INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                             arr_trabajador[a].id_ocg)

         ---CALL fn_escribe()

         IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_tramite
               SET situacion = 40
             WHERE id_ocg_tramite = arr_trabajador[a].id_ocg

             CALL fn_his_tramite(20)
         END IF

         IF arr_trabajador[a].situacion = 25 THEN
            UPDATE ocg_tramite
               SET situacion = 45
             WHERE id_ocg_tramite = arr_trabajador[a].id_ocg
             CALL fn_his_tramite(25)
         END IF

         IF arr_trabajador[a].situacion = 30 THEN
            UPDATE ocg_tramite
               SET situacion = 50
             WHERE id_ocg_tramite = arr_trabajador[a].id_ocg
             CALL fn_his_tramite(30)
         END IF

         UPDATE ocg_tramite
            SET f_respuesta = v_respuesta
          WHERE id_ocg_tramite = arr_trabajador[a].id_ocg

         UPDATE ocg_fecha_mig
           SET f_respuesta = TODAY
         WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
           AND subproceso  = 1

         LET v_incons_t       = 0
         LET v_incons         = ""
         LET v_f_liquida_cofi = NULL
         LET v_cve_conyuge    = " "
         LET v_nss_conyuge    = "           "

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].diagnostico IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF
   END IF

   --******************************************************************************
   --DISPLAY "punto de CONTROL 2"

   SELECT COUNT(*)
     INTO v_cta_f
     FROM ocg_formalizacion ocg,
          ocg_detalle det
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion in (20,55)

   IF v_cta_f IS NULL THEN
      LET v_cta_f = 0
   END IF

   --DISPLAY "cuenta formalizaci�n",v_cta_f
   IF v_cta_f > 0 THEN
      -- se llena arreglo con datos de tabla fromalizaci�n
      --LET a = a +1

      PREPARE prp_formalizacion FROM v_qry_formalizacion
      DECLARE cur_formalizacion CURSOR FOR prp_formalizacion

      FOREACH cur_formalizacion INTO arr_trabajador[a].id_ocg            ,
                                     arr_trabajador[a].cve_ent_financiera,
                                     arr_trabajador[a].nss               ,
                                     arr_trabajador[a].num_ctrl_ef       ,
                                     arr_trabajador[a].rfc               ,
                                     arr_trabajador[a].curp              ,
                                     arr_trabajador[a].ap_paterno_af     ,
                                     arr_trabajador[a].ap_materno_af     ,
                                     arr_trabajador[a].nombre_af         ,
                                     arr_trabajador[a].diagnostico       ,
                                     arr_trabajador[a].num_escritura     ,
                                     arr_trabajador[a].num_notario       ,
                                     arr_trabajador[a].ent_fed_notario   ,
                                     arr_trabajador[a].municipio_notario ,
                                     arr_trabajador[a].num_rpp           ,
                                     arr_trabajador[a].folio_real        ,
                                     arr_trabajador[a].partida           ,
                                     arr_trabajador[a].foja              ,
                                     arr_trabajador[a].volumen           ,
                                     arr_trabajador[a].libro             ,
                                     arr_trabajador[a].tomo              ,
                                     arr_trabajador[a].seccion           ,
                                     arr_trabajador[a].ent_fed_inmueble  ,
                                     arr_trabajador[a].domicilio_inmueble,
                                     arr_trabajador[a].valor_avaluo      ,
                                     arr_trabajador[a].monto_credito     ,
                                     arr_trabajador[a].plazo_credito     ,
                                     arr_trabajador[a].tpo_moneda        ,
                                     arr_trabajador[a].tasa_base         ,
                                     arr_trabajador[a].margen            ,
                                     arr_trabajador[a].f_otorga_cred_ef  ,
                                     arr_trabajador[a].cred_convenidos   ,
                                     arr_trabajador[a].situacion

         --DISPLAY "datos de arreglo formalizacion :",arr_trabajador[a].*

         SELECT COUNT(*)
           INTO v_incons_f
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

         -- DISPLAY "inconsistencias tramite :",v_incons_t

         IF v_incons_f > 0 THEN
            LET v_qry_incons = "SELECT inconsistencia
                                  FROM ocg_inconsistencia
                                 WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 2 "

            PREPARE prp_incons2 FROM v_qry_incons
            DECLARE cur_incons2 CURSOR FOR prp_incons2

            CALL arr_incons.clear()

            LET z= 1

            CALL arr_incons.clear()

            FOREACH cur_incons2 INTO arr_incons[z].inconsistencia
               LET z= z+1
            END FOREACH

            LET v_incons = ""

            FOR z=1 TO v_incons_f
               LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
               --DISPLAY "inconsistencias :",v_incons
            END FOR

            --******************************************************************************
            LET v_incons = v_incons CLIPPED
            LET v_cta_incons = v_incons.getLength()

            FOR i = v_incons.getLength() TO 40
               LET v_incons = v_incons CLIPPED,"0"
            END FOR 
            --******************************************************************************
         END IF

         LET arr_trabajador[a].tpo_registro     = "2"
         LET arr_trabajador[a].subproceso       = "002"
         LET arr_trabajador[a].tpo_envio        = "I"
         LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
         LET arr_trabajador[a].inconsistencias  = v_incons
         LET arr_trabajador[a].viv92            = "0.0"
         LET arr_trabajador[a].viv97            = "0.0"

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

         INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                              arr_trabajador[a].id_ocg)
         ---CALL fn_escribe()

         -- DISPLAY "situacion formalizacion",arr_trabajador[a].situacion

         IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_formalizacion
               SET situacion = 40
             WHERE id_ocg_formalizacion = arr_trabajador[a].id_ocg
             CALL fn_his_formalizacion(20)
         END IF

         IF arr_trabajador[a].situacion = 55 THEN
            UPDATE ocg_formalizacion
               SET situacion = 60
             WHERE id_ocg_formalizacion = arr_trabajador[a].id_ocg
             CALL fn_his_formalizacion(55)

            UPDATE ocg_acreditado
               SET situacion = 60
             WHERE id_ocg_formalizacion = arr_trabajador[a].id_ocg
         END IF

         UPDATE ocg_formalizacion
            SET f_respuesta = v_respuesta
          WHERE id_ocg_formalizacion = arr_trabajador[a].id_ocg

         UPDATE ocg_fecha_mig
           SET f_respuesta = v_respuesta
         WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
           AND subproceso = 2

         LET v_incons_f = 0
         LET v_incons   = ""

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].diagnostico IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF
   END IF

   --******************************************************************************
   --DISPLAY "punto de CONTROL 3 "
   --******************************************************************************

   SELECT COUNT(*)
     INTO v_cta_g
     FROM ocg_solicitud_uso_garantia ocg,
          ocg_detalle det,
          ocg_ctr_transaccion t
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion = 100
      AND det.subproceso = 3
      AND ocg.id_ocg_formalizacion = t.id_ocg_formalizacion
      AND ocg.id_derechohabiente = t.id_derechohabiente
      AND ocg.cve_ent_financiera = t.cve_ent_financiera
      AND ocg.importe_utilizado = t.vivienda_97
      AND year(ocg.f_vencimiento)||lpad(month(ocg.f_vencimiento),2,0) = t.periodo_pago
      AND t.estado = 30
      AND ocg.solicitud_saldo = 2
      AND t.concepto IN(407,417)
      AND ocg.tpo_credito IN('A','C')

   IF v_cta_g IS NULL THEN
      LET v_cta_g = 0
   END IF

   IF v_cta_g > 0 THEN
      PREPARE prp_garantia FROM v_qry_garantia
      DECLARE cur_garantia CURSOR FOR prp_garantia

      FOREACH cur_garantia INTO arr_trabajador[a].id_ocg,
                                arr_trabajador[a].id_ocg_detalle,
                                arr_trabajador[a].cve_ent_financiera,
                                arr_trabajador[a].nss,
                                arr_trabajador[a].num_ctrl_ef,
                                arr_trabajador[a].diagnostico,
                                arr_trabajador[a].imp_solic_uti_ocg,
                                arr_trabajador[a].f_venc_imp_solic,
                                arr_trabajador[a].imp_utilizado_ocg,
                                arr_trabajador[a].solic_saldo,
                                arr_trabajador[a].situacion

         LET arr_trabajador[a].imp_solic_uti_ocg = arr_trabajador[a].imp_solic_uti_ocg * 100
         LET arr_trabajador[a].imp_utilizado_ocg = arr_trabajador[a].imp_utilizado_ocg * 100

         SELECT COUNT(*)
           INTO v_incons_g
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

         -- DISPLAY "inconsistencias garant�a :",v_incons_g
         LET v_incons = ""

         IF v_incons_g > 0 THEN
            LET v_qry_incons = "SELECT inconsistencia
                                  FROM ocg_inconsistencia
                                 WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 3 "

            PREPARE prp_incons3 FROM v_qry_incons
            DECLARE cur_incons3 CURSOR FOR prp_incons3

            CALL arr_incons.clear()

            LET z= 1

            CALL arr_incons.clear()

            FOREACH cur_incons3 INTO arr_incons[z].inconsistencia
               LET z= z+1
            END FOREACH

            LET v_incons = ""

            FOR z=1 TO v_incons_g
               LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
               --DISPLAY "inconsistencias :",v_incons
            END FOR

            --******************************************************************************
            LET v_incons = v_incons CLIPPED
            LET v_cta_incons = v_incons.getLength()

            FOR i = v_incons.getLength() TO 40
               LET v_incons = v_incons CLIPPED,"0"
            END FOR 
            --******************************************************************************
         END IF

         LET arr_trabajador[a].tpo_registro     = "2"
         LET arr_trabajador[a].subproceso       = "003"
         LET arr_trabajador[a].tpo_envio        = "I"
         LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
         LET arr_trabajador[a].inconsistencias  = v_incons
         LET arr_trabajador[a].viv92            = "0.0"
         LET arr_trabajador[a].viv97            = "0.0"

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

         INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                              arr_trabajador[a].id_ocg)
         ---CALL fn_escribe()

         IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_solicitud_uso_garantia
               SET situacion = 40
             WHERE id_ocg_solicitud_ug = arr_trabajador[a].id_ocg
             CALL fn_his_garantia(20)
         END IF

         IF arr_trabajador[a].situacion = 100 THEN
            UPDATE ocg_solicitud_uso_garantia
               SET situacion = 110
             WHERE id_ocg_solicitud_ug = arr_trabajador[a].id_ocg
             CALL fn_his_garantia(100)
         END IF

         LET v_id_referencia = arr_trabajador[a].id_ocg
         LET v_id_detalle    = arr_trabajador[a].id_ocg_detalle
         LET v_id_dh         = arr_trabajador[a].id_dh
         LET v_f_respuesta = TODAY

         UPDATE ocg_fecha_mig
            SET f_respuesta = v_f_respuesta
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
            AND subproceso = 3

--******************************************************************************

         LET arr_trabajador[a].imp_solic_uti_ocg = (arr_trabajador[a].imp_solic_uti_ocg / 100)
         LET arr_trabajador[a].imp_utilizado_ocg = (arr_trabajador[a].imp_utilizado_ocg / 100)

         SELECT id_ocg_formalizacion,id_derechohabiente
           INTO v_id_ocg_forma,arr_trabajador[a].id_dh
           FROM ocg_solicitud_uso_garantia
          WHERE id_ocg_solicitud_ug = arr_trabajador[a].id_ocg

         UPDATE ocg_ctr_transaccion
            SET estado        = 80,
                concepto      = concepto + 10,
                f_pago        = TODAY,
                f_transaccion = TODAY,
                f_proceso     = TODAY
          WHERE id_ocg_formalizacion = v_id_ocg_forma
            AND id_ocg_detalle       = v_id_detalle
            AND id_derechohabiente   = arr_trabajador[a].id_dh
            AND cve_ent_financiera   = arr_trabajador[a].cve_ent_financiera
            AND concepto             = (407)
            AND estado               = 30
            AND vivienda_97          = arr_trabajador[a].imp_utilizado_ocg
--******************************************************************************

         LET v_incons_g = 0 
         LET v_incons   = ""

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].diagnostico IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF

   END IF

   SELECT COUNT(*)
     INTO v_cta_g1
     FROM ocg_solicitud_uso_garantia ocg,
          ocg_detalle det
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion = 20
      AND ocg.solicitud_saldo = 2

   IF v_cta_g1 >= 1 THEN

      PREPARE prp_garantia_2 FROM v_qry_garantia_2
      DECLARE cur_garantia_2 CURSOR FOR prp_garantia_2

      FOREACH cur_garantia_2 INTO arr_trabajador[a].id_ocg,
                                arr_trabajador[a].cve_ent_financiera,
                                arr_trabajador[a].nss,
                                arr_trabajador[a].num_ctrl_ef,
                                arr_trabajador[a].diagnostico,
                                arr_trabajador[a].imp_solic_uti_ocg,
                                arr_trabajador[a].f_venc_imp_solic,
                                arr_trabajador[a].solic_saldo,
                                arr_trabajador[a].situacion

         LET arr_trabajador[a].imp_solic_uti_ocg = arr_trabajador[a].imp_solic_uti_ocg * 100

         SELECT COUNT(*)
           INTO v_incons_g
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

         -- DISPLAY "inconsistencias garant�a :",v_incons_g
         LET v_incons = ""

         IF v_incons_g > 0 THEN
            LET v_qry_incons = "SELECT inconsistencia
                                  FROM ocg_inconsistencia
                                 WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 3 "

            PREPARE prp_incons3_2 FROM v_qry_incons
            DECLARE cur_incons3_2 CURSOR FOR prp_incons3_2

            CALL arr_incons.clear()

            LET z= 1

            CALL arr_incons.clear()

            FOREACH cur_incons3_2 INTO arr_incons[z].inconsistencia
               LET z= z+1
            END FOREACH

            LET v_incons = ""

            FOR z=1 TO v_incons_g
               LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
               --DISPLAY "inconsistencias :",v_incons
            END FOR

            --******************************************************************************
            LET v_incons = v_incons CLIPPED
            LET v_cta_incons = v_incons.getLength()

            FOR i = v_incons.getLength() TO 40
               LET v_incons = v_incons CLIPPED,"0"
            END FOR 
         --******************************************************************************
         END IF

         LET arr_trabajador[a].tpo_registro     = "2"
         LET arr_trabajador[a].subproceso       = "003"
         LET arr_trabajador[a].tpo_envio        = "I"
         LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
         LET arr_trabajador[a].inconsistencias  = v_incons
         LET arr_trabajador[a].viv92            = "0.0"
         LET arr_trabajador[a].viv97            = "0.0"

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)
                                        
         INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                              arr_trabajador[a].id_ocg)

         ---CALL fn_escribe()

         IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_solicitud_uso_garantia
               SET situacion = 40
             WHERE id_ocg_solicitud_ug = arr_trabajador[a].id_ocg
             CALL fn_his_garantia(20)
         END IF

         IF arr_trabajador[a].situacion = 100 THEN
            UPDATE ocg_solicitud_uso_garantia
               SET situacion = 110
             WHERE id_ocg_solicitud_ug = arr_trabajador[a].id_ocg
             CALL fn_his_garantia(100)
         END IF

         LET v_id_referencia = arr_trabajador[a].id_ocg
         LET v_id_detalle    = arr_trabajador[a].id_ocg_detalle
         LET v_id_dh         = arr_trabajador[a].id_dh
         LET v_f_respuesta = TODAY

         UPDATE ocg_fecha_mig
            SET f_respuesta = v_f_respuesta
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
            AND subproceso = 3

         LET v_incons_g = 0
         LET v_incons   = ""

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].diagnostico IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF

   END IF

   --******************************************************************************
   --DISPLAY "punto de CONTROL 4"
   --******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_tr
     FROM ocg_ctr_transaccion ocg
    WHERE ocg.estado = 30  ---in (40,60)
      AND ocg.concepto in (807,107,307,817,117,317)

   IF v_cta_tr IS NULL THEN
      LET v_cta_tr = 0
   END IF

   IF v_cta_tr > 0 THEN
      PREPARE prp_transaccion FROM v_qry_transaccion
      DECLARE cur_transaccion CURSOR FOR prp_transaccion

      FOREACH cur_transaccion INTO arr_trabajador[a].id_ocg,
                                   arr_trabajador[a].cve_ent_financiera,
                                   arr_trabajador[a].nss,
                                   arr_trabajador[a].num_ctrl_ef,
                                   arr_trabajador[a].imp_aport_subsec,
                                   arr_trabajador[a].bim_apor_subsec,
                                   arr_trabajador[a].situacion,
                                   arr_trabajador[a].concepto

         ---LET arr_trabajador[a].imp_aport_subsec = arr_trabajador[a].imp_aport_subsec * 100

         LET arr_trabajador[a].cred_convenidos = ""

         SELECT f.tpo_credito
           INTO  arr_trabajador[a].cred_convenidos 
           FROM ocg_formalizacion f,ocg_ctr_transaccion t
          WHERE t.id_ocg_formalizacion = f.id_ocg_formalizacion
            AND t.id_ocg_ctr_transaccion = arr_trabajador[a].id_ocg

         SELECT COUNT(*)
           INTO v_incons_tr
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

         LET v_incons = ""

         IF v_incons_tr > 0 THEN
            LET v_qry_incons = "SELECT inconsistencia
                                  FROM ocg_inconsistencia
                                 WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 4 "

            PREPARE prp_incons5 FROM v_qry_incons
            DECLARE cur_incons5 CURSOR FOR prp_incons5

            CALL arr_incons.clear()

            LET z= 1

            CALL arr_incons.clear()

            FOREACH cur_incons5 INTO arr_incons[z].inconsistencia
               LET z= z+1
            END FOREACH

            LET v_incons = ""

            FOR z=1 TO v_incons_tr
               LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
               --DISPLAY "inconsistencias :",v_incons
            END FOR

            --******************************************************************************
            LET v_incons = v_incons CLIPPED
            LET v_cta_incons = v_incons.getLength()

            FOR i = v_incons.getLength() TO 40
               LET v_incons = v_incons CLIPPED,"0"
            END FOR 
            --******************************************************************************
         END IF

          --DISPLAY "Proceso : ",v_proceso_cod

         LET arr_trabajador[a].tpo_registro     = "2"

         IF arr_trabajador[a].concepto = 417 THEN
            LET arr_trabajador[a].subproceso    = "003"
         ELSE
            LET arr_trabajador[a].subproceso    = "004"
         END IF

         LET arr_trabajador[a].tpo_envio        = "I"
         LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
         LET arr_trabajador[a].inconsistencias  = v_incons
         LET arr_trabajador[a].viv92            = "0.0"
         LET arr_trabajador[a].viv97            = "0.0"

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

         INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                              arr_trabajador[a].id_ocg)

         ---CALL fn_escribe()


         UPDATE ocg_ctr_transaccion
            SET estado        = 80,
                concepto      = concepto + 10,
                f_pago        = TODAY,
                f_transaccion = TODAY,
                f_proceso     = TODAY
          WHERE id_ocg_ctr_transaccion = arr_trabajador[a].id_ocg

         UPDATE ocg_fecha_mig
            SET f_respuesta = TODAY
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
            AND subproceso = 4

         LET v_incons_tr = 0
         LET v_incons    = ""

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].id_ocg IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF
   END IF

   --******************************************************************************
   --DISPLAY "punto de CONTROL 5 "
   --******************************************************************************

   SELECT COUNT(*)
     INTO v_cta_l
     FROM ocg_liquidacion ocg,
          ocg_detalle det
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion in (20,158,153,175,165)

   IF v_cta_l IS NULL THEN
      LET v_cta_l = 0
   END IF

   --DISPLAY "cuenta liquidaci�n :",v_cta_l

   IF v_cta_l > 0 THEN
      -- se llena arreglo con datos de tabla liquidaci�n
      --LET a = a +1

      PREPARE prp_liquidacion FROM v_qry_liquidacion
      DECLARE cur_liquidacion CURSOR FOR prp_liquidacion

      FOREACH cur_liquidacion INTO arr_trabajador[a].id_ocg,
                                   arr_trabajador[a].cve_ent_financiera,
                                   arr_trabajador[a].nss,
                                   arr_trabajador[a].num_ctrl_ef,
                                   arr_trabajador[a].diagnostico,
                                   arr_trabajador[a].bim_apor_subsec,
                                   arr_trabajador[a].imp_subsec_devuelto,
                                   arr_trabajador[a].f_libera_garantia,
                                   arr_trabajador[a].imp_ocg_devuelto,
                                   arr_trabajador[a].causa_liquidacion,
                                   arr_trabajador[a].f_deposito,
                                   arr_trabajador[a].situacion,
                                   arr_trabajador[a].cred_convenidos

         --DISPLAY "datos de arreglo :",arr_trabajador[a].*

         SELECT COUNT(*)
           INTO v_incons_l
           FROM ocg_inconsistencia
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

          -- DISPLAY "inconsistencias tramite :",v_incons_t

          IF v_incons_l > 0 THEN
             LET v_qry_incons = "SELECT inconsistencia
                                   FROM ocg_inconsistencia
                                  WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg," AND subproceso = 5 "

             PREPARE prp_incons4 FROM v_qry_incons
             DECLARE cur_incons4 CURSOR FOR prp_incons4

             CALL arr_incons.clear()

             LET z= 1

             CALL arr_incons.clear()

             FOREACH cur_incons4 INTO arr_incons[z].inconsistencia
                LET z= z+1
             END FOREACH

             LET v_incons = ""

             FOR z=1 TO v_incons_l
                LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
                --DISPLAY "inconsistencias :",v_incons
             END FOR

             --******************************************************************************
             LET v_incons = v_incons CLIPPED
             LET v_cta_incons = v_incons.getLength()

             FOR i = v_incons.getLength() TO 40
                LET v_incons = v_incons CLIPPED,"0"
             END FOR 
             --******************************************************************************
          END IF

          LET arr_trabajador[a].tpo_registro    = "2"
          LET arr_trabajador[a].subproceso      = "005"
          LET arr_trabajador[a].tpo_envio       = "I"
          LET arr_trabajador[a].f_envio         = TODAY USING "YYYYMMDD"
          LET arr_trabajador[a].inconsistencias = v_incons
          LET arr_trabajador[a].viv92           = "0.0"
          LET arr_trabajador[a].viv97           = "0.0"

         INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        arr_trabajador[a].imp_solic_uti_ocg   ,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        arr_trabajador[a].imp_utilizado_ocg   ,
                                        arr_trabajador[a].imp_aport_subsec    ,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        arr_trabajador[a].imp_subsec_devuelto ,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        arr_trabajador[a].imp_ocg_devuelto    ,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

         INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                              arr_trabajador[a].id_ocg)

         ---CALL fn_escribe()

         IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_liquidacion
               SET situacion = 40
             WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

             CALL fn_his_liquidacion(20)
         END IF
-- se actualiza la situaci�n a 158 13/03/2017
         IF arr_trabajador[a].situacion = 158 THEN
            UPDATE ocg_liquidacion
               SET situacion = 160
             WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

           SELECT id_ocg_formalizacion
             INTO v_id_ocg_formalizacion
             FROM ocg_liquidacion 
            WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg
              AND situacion = 160

            IF v_id_ocg_formalizacion IS NOT NULL THEN

            UPDATE ocg_formalizacion
               SET situacion = 160
             WHERE id_ocg_formalizacion = v_id_ocg_formalizacion

             UPDATE ocg_acreditado
               SET situacion = 160
             WHERE id_ocg_formalizacion = v_id_ocg_formalizacion

            END IF

            CALL fn_his_liquidacion(158)
         END IF

         IF arr_trabajador[a].situacion = 153 THEN
            UPDATE ocg_liquidacion
               SET situacion = 155
             WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

            UPDATE ocg_tramite
               SET situacion = 155
             WHERE id_derechohabiente IN(
                   SELECT id_derechohabiente
                     FROM ocg_liquidacion
                    WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg)
               AND situacion = 153

           SELECT id_ocg_formalizacion
             INTO v_id_ocg_formalizacion
             FROM ocg_liquidacion 
            WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg
              AND situacion = 155

            IF v_id_ocg_formalizacion IS NOT NULL THEN

            UPDATE ocg_formalizacion
               SET situacion = 155
             WHERE id_ocg_formalizacion = v_id_ocg_formalizacion

             UPDATE ocg_acreditado
               SET situacion = 155
             WHERE id_ocg_formalizacion = v_id_ocg_formalizacion

            END IF

             CALL fn_his_liquidacion(153)
         END IF

         IF arr_trabajador[a].situacion = 175 THEN
            UPDATE ocg_liquidacion
               SET situacion = 180
             WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

             CALL fn_his_liquidacion(175)
         END IF

         IF arr_trabajador[a].situacion = 165 THEN
            UPDATE ocg_liquidacion
               SET situacion = 170
             WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

             CALL fn_his_liquidacion(165)
         END IF

         LET v_id_referencia = arr_trabajador[a].id_ocg
         LET v_id_detalle    = arr_trabajador[a].id_ocg_detalle
         LET v_id_dh         = arr_trabajador[a].id_dh
         LET v_f_respuesta   = TODAY

         UPDATE ocg_fecha_mig
            SET f_respuesta = v_respuesta
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg
            AND subproceso = 5

         LET v_incons_l = 0
         LET v_incons   = ""

         LET a = a+1
      END FOREACH
   END IF
   --******************************************************************************
   --DISPLAY "punto de CONTROL 6 "
   --******************************************************************************

   SELECT COUNT(*)
     INTO v_cta_rh
     FROM ocg_rechazo_ent
    WHERE f_proceso = TODAY
      AND estado    = 20

   IF v_cta_rh IS NULL THEN
      LET v_cta_rh = 0
   END IF

   --DISPLAY "cuenta rechazos :",v_cta_rh

   IF v_cta_rh > 0 THEN
      -- se llena arreglo con datos de tabla rechazos
      --LET a = a +1

      PREPARE prp_rechazo FROM v_qry_rechazo
      DECLARE cur_rechazo CURSOR FOR prp_rechazo

      FOREACH cur_rechazo INTO arr_trabajador[a].id_ocg,
                               arr_trabajador[a].tpo_registro,
                               arr_trabajador[a].subproceso,
                               arr_trabajador[a].tpo_envio,
                               arr_trabajador[a].f_envio,
                               arr_trabajador[a].cve_ent_financiera,
                               arr_trabajador[a].nss,
                               arr_trabajador[a].num_ctrl_ef,
                               arr_trabajador[a].rfc,
                               arr_trabajador[a].curp,
                               arr_trabajador[a].ap_paterno_af,
                               arr_trabajador[a].ap_materno_af,
                               arr_trabajador[a].nombre_af,
                               arr_trabajador[a].num_bimestres,
                               arr_trabajador[a].viv92,
                               arr_trabajador[a].viv97,
                               arr_trabajador[a].f_corte_subcuenta,
                               arr_trabajador[a].diagnostico,
                               arr_trabajador[a].inconsistencias,
                               arr_trabajador[a].num_escritura,
                               arr_trabajador[a].num_notario,
                               arr_trabajador[a].ent_fed_notario,
                               arr_trabajador[a].municipio_notario,
                               arr_trabajador[a].num_rpp,
                               arr_trabajador[a].folio_real,
                               arr_trabajador[a].partida,
                               arr_trabajador[a].foja,
                               arr_trabajador[a].volumen,
                               arr_trabajador[a].libro,
                               arr_trabajador[a].tomo,
                               arr_trabajador[a].seccion,
                               arr_trabajador[a].ent_fed_inmueble,
                               arr_trabajador[a].domicilio_inmueble,
                               arr_trabajador[a].valor_avaluo,
                               arr_trabajador[a].monto_credito,
                               arr_trabajador[a].plazo_credito,
                               arr_trabajador[a].tpo_moneda,
                               arr_trabajador[a].tasa_base,
                               arr_trabajador[a].margen,
                               arr_trabajador[a].f_otorga_cred_ef,
                               v_aux_imp_solic_uti_ocg,
                               arr_trabajador[a].f_venc_imp_solic,
                               v_aux_imp_utilizado_ocg,
                               v_aux_imp_aport_subsec,
                               arr_trabajador[a].bim_apor_subsec,
                               v_aux_imp_subsec_devuelto,
                               arr_trabajador[a].f_libera_garantia,
                               v_aux_imp_ocg_devuelto,
                               arr_trabajador[a].causa_liquidacion,
                               arr_trabajador[a].f_deposito,
                               arr_trabajador[a].cred_convenidos,
                               arr_trabajador[a].solic_saldo,
                               arr_trabajador[a].f_vigencia,
                               arr_trabajador[a].situacion

           --Valida los importes en caso de que sean nulos
         
         IF(v_aux_imp_solic_uti_ocg IS  NULL) THEN 
            LET arr_trabajador[a].imp_solic_uti_ocg = 0
         ELSE 
            LET arr_trabajador[a].imp_solic_uti_ocg = v_aux_imp_solic_uti_ocg
         END IF 

         IF (v_aux_imp_utilizado_ocg IS NULL) THEN
            LET arr_trabajador[a].imp_utilizado_ocg = 0
         ELSE 
            LET arr_trabajador[a].imp_utilizado_ocg = v_aux_imp_utilizado_ocg
         END IF 

         IF(v_aux_imp_aport_subsec IS NULL) THEN
            LET arr_trabajador[a].imp_aport_subsec = 0
         ELSE 
            LET arr_trabajador[a].imp_aport_subsec = v_aux_imp_aport_subsec
         END IF 

         IF(v_aux_imp_subsec_devuelto IS NULL) THEN
            LET arr_trabajador[a].imp_subsec_devuelto = 0
         ELSE 
            LET arr_trabajador[a].imp_subsec_devuelto = v_aux_imp_subsec_devuelto
         END IF 

         IF(v_aux_imp_ocg_devuelto IS NULL) THEN
            LET arr_trabajador[a].imp_ocg_devuelto = 0
         ELSE 
            LET arr_trabajador[a].imp_ocg_devuelto = v_aux_imp_ocg_devuelto
         END IF 

         SELECT COUNT(*)
           INTO v_incons_rh
           FROM ocg_inconsistencia_rch
          WHERE id_ocg_referencia = arr_trabajador[a].id_ocg

          IF v_incons_rh > 0 THEN
             LET v_qry_incons = "SELECT inconsistencia
                                   FROM ocg_inconsistencia_rch
                                  WHERE id_ocg_referencia = ",arr_trabajador[a].id_ocg

             PREPARE prp_incons6 FROM v_qry_incons
             DECLARE cur_incons6 CURSOR FOR prp_incons6

             CALL arr_incons.clear()

             LET z= 1

             CALL arr_incons.clear()

             FOREACH cur_incons6 INTO arr_incons[z].inconsistencia
                LET z= z+1
             END FOREACH

             LET v_incons = ""

             FOR z=1 TO v_incons_rh
                LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
                --DISPLAY "inconsistencias :",v_incons
             END FOR

             --******************************************************************************
             LET v_incons = v_incons CLIPPED
             LET v_cta_incons = v_incons.getLength()

             FOR i = v_incons.getLength() TO 40
                LET v_incons = v_incons CLIPPED,"0"
             END FOR

          END IF

             LET arr_trabajador[a].tpo_registro    = "2"
             LET arr_trabajador[a].tpo_envio       = "I"
             LET arr_trabajador[a].f_envio         = TODAY USING "YYYYMMDD"
             LET arr_trabajador[a].inconsistencias = v_incons

             INSERT INTO tmp_detalle VALUES(arr_trabajador[a].tpo_registro        ,
                                        arr_trabajador[a].subproceso          ,
                                        arr_trabajador[a].tpo_envio           ,
                                        arr_trabajador[a].f_envio             ,
                                        arr_trabajador[a].cve_ent_financiera  ,
                                        arr_trabajador[a].nss                 ,
                                        arr_trabajador[a].num_ctrl_ef         ,
                                        arr_trabajador[a].rfc                 ,
                                        arr_trabajador[a].curp                ,
                                        arr_trabajador[a].ap_paterno_af       ,
                                        arr_trabajador[a].ap_materno_af       ,
                                        arr_trabajador[a].nombre_af           ,
                                        arr_trabajador[a].num_bimestres       ,
                                        arr_trabajador[a].viv92               ,
                                        arr_trabajador[a].viv97               ,
                                        arr_trabajador[a].f_corte_subcuenta   ,
                                        arr_trabajador[a].diagnostico         ,
                                        arr_trabajador[a].inconsistencias     ,
                                        arr_trabajador[a].num_escritura       ,
                                        arr_trabajador[a].num_notario         ,
                                        arr_trabajador[a].ent_fed_notario     ,
                                        arr_trabajador[a].municipio_notario   ,
                                        arr_trabajador[a].num_rpp             ,
                                        arr_trabajador[a].folio_real          ,
                                        arr_trabajador[a].partida             ,
                                        arr_trabajador[a].foja                ,
                                        arr_trabajador[a].volumen             ,
                                        arr_trabajador[a].libro               ,
                                        arr_trabajador[a].tomo                ,
                                        arr_trabajador[a].seccion             ,
                                        arr_trabajador[a].ent_fed_inmueble    ,
                                        arr_trabajador[a].domicilio_inmueble  ,
                                        arr_trabajador[a].valor_avaluo        ,
                                        arr_trabajador[a].monto_credito       ,
                                        arr_trabajador[a].plazo_credito       ,
                                        arr_trabajador[a].tpo_moneda          ,
                                        arr_trabajador[a].tasa_base           ,
                                        arr_trabajador[a].margen              ,
                                        arr_trabajador[a].f_otorga_cred_ef    ,
                                        v_aux_imp_solic_uti_ocg,
                                        arr_trabajador[a].f_venc_imp_solic    ,
                                        v_aux_imp_utilizado_ocg,
                                        v_aux_imp_aport_subsec,
                                        arr_trabajador[a].bim_apor_subsec     ,
                                        v_aux_imp_subsec_devuelto,
                                        arr_trabajador[a].f_libera_garantia   ,
                                        v_aux_imp_ocg_devuelto,
                                        arr_trabajador[a].causa_liquidacion   ,
                                        arr_trabajador[a].f_deposito          ,
                                        arr_trabajador[a].cred_convenidos     ,
                                        arr_trabajador[a].solic_saldo         ,
                                        arr_trabajador[a].f_vigencia          ,
                                        arr_trabajador[a].filler)

             INSERT INTO tmp_id_referencia VALUES(arr_trabajador[a].subproceso,
                                                arr_trabajador[a].id_ocg)

             --******************************************************************************

          --DISPLAY "id rch : ",arr_trabajador[a].id_ocg
          --DISPLAY "estado : ",arr_trabajador[a].situacion
          IF arr_trabajador[a].situacion = 20 THEN
            UPDATE ocg_rechazo_ent
               SET estado = 40
            WHERE id_tmp_det = arr_trabajador[a].id_ocg
          END IF

         LET v_incons   = ""

         LET a = a+1
      END FOREACH

      IF arr_trabajador[a].diagnostico IS NULL THEN
         CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
      END IF
   END IF

END FUNCTION 

FUNCTION fn_escribe()

   DEFINE a1                        INTEGER
   DEFINE v_detalle                 STRING
   DEFINE v_qry_det                 STRING
   DEFINE v_det_enc                 STRING
   DEFINE v_det_sum                 STRING

   DEFINE arr_detalle DYNAMIC ARRAY OF RECORD
      tpo_registro                  CHAR(1),
      subproceso                    CHAR(3),
      tpo_envio                     CHAR(1),
      f_envio                       CHAR(8),
      cve_ent_financiera            SMALLINT,
      nss                           CHAR(11),
      num_ctrl_ef                   CHAR(18),
      rfc                           CHAR(13),
      curp                          CHAR(18),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombre_af                     CHAR(40),
      num_bimestres                 CHAR(3),
      viv92                         CHAR(8),
      viv97                         CHAR(8),
      f_corte_subcuenta             CHAR(8),
      diagnostico                   SMALLINT,
      inconsistencias               CHAR(40),
      num_escritura                 CHAR(8),
      num_notario                   CHAR(4),
      ent_fed_notario               CHAR(2),
      municipio_notario             CHAR(3),
      num_rpp                       CHAR(15),
      folio_real                    CHAR(8),
      partida                       CHAR(6),
      foja                          CHAR(8),
      volumen                       CHAR(6),
      libro                         CHAR(6),
      tomo                          CHAR(6),
      seccion                       CHAR(6),
      ent_fed_inmueble              CHAR(2),
      domicilio_inmueble            CHAR(30),
      valor_avaluo                  CHAR(15),
      monto_credito                 CHAR(15),
      plazo_credito                 CHAR(5),
      tpo_moneda                    CHAR(2),
      tasa_base                     CHAR(20),
      margen                        CHAR(20),
      f_otorga_cred_ef              CHAR(8),
      imp_solic_uti_ocg             CHAR(15),
      f_venc_imp_solic              CHAR(8),
      imp_utilizado_ocg             CHAR(15),
      imp_aport_subsec              CHAR(15),
      bim_apor_subsec               CHAR(6),
      imp_subsec_devuelto           CHAR(15),
      f_libera_garantia             CHAR(8),
      imp_ocg_devuelto              CHAR(15),
      causa_liquidacion             CHAR(1),
      f_deposito                    CHAR(8),
      cred_convenidos               CHAR(1),
      solic_saldo                   CHAR(1),
      f_vigencia                    CHAR(8),
      filler                        CHAR(64)
   END RECORD

   LET ch = base.Channel.create()

   --DISPLAY "nombre del archivo",v_nom_arh
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_encabezado.clear()
   CALL arr_detalle.clear()
   CALL arr_sumario.clear()

   LET a1 = 1

   LET arr_encabezado[1].tpo_registro    = "1"
   LET arr_encabezado[1].identificador   = "ART43BIS"
   LET arr_encabezado[1].destino         = "SER"
   LET arr_encabezado[1].f_transferencia = TODAY USING "YYYYMMDD"
   LET arr_encabezado[1].situacion       = "R"
   LET arr_encabezado[1].filler          = ""

   LET v_det_enc = arr_encabezado[1].tpo_registro,
                   arr_encabezado[1].identificador,
                   arr_encabezado[1].destino,
                   arr_encabezado[1].f_transferencia,
                   arr_encabezado[1].situacion,
                   arr_encabezado[1].filler

   CALL ch.writeLine([v_det_enc])

   LET v_qry_det = "SELECT *",
                   " FROM tmp_detalle",
                   " ORDER BY cve_ent_financiera, subproceso"

   PREPARE prp_detalle FROM v_qry_det
   DECLARE cur_detalle CURSOR FOR prp_detalle

   FOREACH cur_detalle INTO arr_detalle[a].*
      IF arr_detalle[a].cred_convenidos = "A" THEN
         LET arr_detalle[a].cred_convenidos = " "
      END IF

      LET v_detalle = arr_detalle[a].tpo_registro        ,
                      arr_detalle[a].subproceso          ,
                      arr_detalle[a].tpo_envio           ,
                      arr_detalle[a].f_envio             ,
                      arr_detalle[a].cve_ent_financiera  USING "&&&",
                      arr_detalle[a].nss                 ,
                      arr_detalle[a].num_ctrl_ef         ,
                      arr_detalle[a].rfc                 ,
                      arr_detalle[a].curp                ,
                      arr_detalle[a].ap_paterno_af       ,
                      arr_detalle[a].ap_materno_af       ,
                      arr_detalle[a].nombre_af           ,
                      arr_detalle[a].num_bimestres       ,
                      arr_detalle[a].viv92               USING "&&&&&&&&",
                      arr_detalle[a].viv97               USING "&&&&&&&&",
                      arr_detalle[a].f_corte_subcuenta   ,
                      arr_detalle[a].diagnostico         USING "&&",
                      arr_detalle[a].inconsistencias     ,
                      arr_detalle[a].num_escritura       ,
                      arr_detalle[a].num_notario         USING "&&&&",
                      arr_detalle[a].ent_fed_notario     USING "&&",
                      arr_detalle[a].municipio_notario   USING "&&&",
                      arr_detalle[a].num_rpp             ,
                      arr_detalle[a].folio_real          ,
                      arr_detalle[a].partida             ,
                      arr_detalle[a].foja                ,
                      arr_detalle[a].volumen             ,
                      arr_detalle[a].libro               ,
                      arr_detalle[a].tomo                ,
                      arr_detalle[a].seccion             ,
                      arr_detalle[a].ent_fed_inmueble    USING "&&",
                      arr_detalle[a].domicilio_inmueble  ,
                      arr_detalle[a].valor_avaluo        ,
                      arr_detalle[a].monto_credito       USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].plazo_credito       USING "&&&&&",
                      arr_detalle[a].tpo_moneda          USING "&&",
                      arr_detalle[a].tasa_base           ,
                      arr_detalle[a].margen              ,
                      arr_detalle[a].f_otorga_cred_ef    ,
                      arr_detalle[a].imp_solic_uti_ocg   USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].f_venc_imp_solic    ,
                      arr_detalle[a].imp_utilizado_ocg   USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].imp_aport_subsec    USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].bim_apor_subsec     ,
                      arr_detalle[a].imp_subsec_devuelto USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].f_libera_garantia   ,
                      arr_detalle[a].imp_ocg_devuelto    USING "&&&&&&&&&&&&&&&",
                      arr_detalle[a].causa_liquidacion   ,
                      arr_detalle[a].f_deposito          ,
                      arr_detalle[a].cred_convenidos     ,
                      arr_detalle[a].solic_saldo         ,
                      arr_detalle[a].f_vigencia          ,
                      arr_detalle[a].filler              --USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"

      CALL ch.writeLine([v_detalle])

      LET a = a+1
   END FOREACH

   IF arr_trabajador[a].diagnostico IS NULL THEN
      CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
   END IF

   LET arr_sumario[1].tpo_registro    = "3"
   LET arr_sumario[1].identificador   = "ART43BIS"
   LET arr_sumario[1].destino         = "SER"
   LET arr_sumario[1].tot_registros   = v_cta_t + v_cta_f + v_cta_g + v_cta_g1 + v_cta_l + v_cta_tr +v_cta_rh + 2
   LET arr_sumario[1].filler          = ""

   LET v_det_sum = arr_sumario[1].tpo_registro,
                   arr_sumario[1].identificador,
                   arr_sumario[1].destino,
                   arr_sumario[1].tot_registros USING "&&&&&&",
                   arr_sumario[1].filler

   CALL ch.writeLine([v_det_sum])

   CALL ch.close()

END FUNCTION

FUNCTION fn_his_tramite(v_situacion_ant)

   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_situacion               SMALLINT
   DEFINE v_situacion_ant           SMALLINT

   LET v_id_derechohabiente = ""
   LET v_situacion          = ""

   SELECT id_derechohabiente,situacion
     INTO v_id_derechohabiente,v_situacion
     FROM ocg_tramite
    WHERE id_ocg_tramite = arr_trabajador[a].id_ocg

    LET arr_trabajador[a].f_corte_subcuenta = arr_trabajador[a].f_corte_subcuenta [5,6],
                                              arr_trabajador[a].f_corte_subcuenta [7,8],
                                              arr_trabajador[a].f_corte_subcuenta [1,4]

    ---DISPLAY arr_trabajador[a].f_corte_subcuenta

    LET arr_trabajador[a].f_vigencia = arr_trabajador[a].f_vigencia [5,6],
                                       arr_trabajador[a].f_vigencia [7,8],
                                       arr_trabajador[a].f_vigencia [1,4]

    --DISPLAY arr_trabajador[a].f_vigencia

    INSERT INTO ocg_his_tramite 
        VALUES(arr_trabajador[a].id_ocg,
               arr_trabajador[a].cve_ent_financiera,
               v_id_derechohabiente,
               arr_trabajador[a].num_ctrl_ef,
               arr_trabajador[a].rfc,
               arr_trabajador[a].curp,
               arr_trabajador[a].ap_paterno_af,
               arr_trabajador[a].ap_materno_af,
               arr_trabajador[a].nombre_af,
               arr_trabajador[a].num_bimestres,
               arr_trabajador[a].viv97,
               arr_trabajador[a].f_corte_subcuenta,
               arr_trabajador[a].cred_convenidos,
               arr_trabajador[a].f_vigencia,
               arr_trabajador[a].diagnostico,
               v_situacion_ant,
               v_situacion,
               TODAY,
               p_usuario )

END FUNCTION

FUNCTION fn_his_formalizacion(v_situacion_ant)

   DEFINE v_situacion_ant           SMALLINT
   DEFINE v_mun_inmueble            DECIMAL(5,0)
   DEFINE v_f_registro_carta        DATE
   DEFINE v_usuario_reg_carta       CHAR(20)
   DEFINE v_situacion               SMALLINT
   DEFINE v_id_derechohabiente      DECIMAL(9,0)

   SELECT id_derechohabiente,
          mcpio_inmueble,
          f_registro_carta,
          usuario_reg_carta,
          situacion
     INTO v_id_derechohabiente,
          v_mun_inmueble,
          v_f_registro_carta,
          v_usuario_reg_carta,
          v_situacion
     FROM ocg_formalizacion
    WHERE id_ocg_formalizacion = arr_trabajador[a].id_ocg

    LET arr_trabajador[a].f_otorga_cred_ef = arr_trabajador[a].f_otorga_cred_ef[5,6],
                                             arr_trabajador[a].f_otorga_cred_ef[7,8],
                                             arr_trabajador[a].f_otorga_cred_ef[1,4]

   --DISPLAY "fecha otorga :",arr_trabajador[a].f_otorga_cred_ef

   INSERT INTO ocg_his_formalizacion
        VALUES ( arr_trabajador[a].id_ocg            ,
                 v_id_derechohabiente                ,
                 arr_trabajador[a].cve_ent_financiera,
                 arr_trabajador[a].num_ctrl_ef       ,
                 arr_trabajador[a].num_escritura     ,
                 arr_trabajador[a].num_notario       ,
                 arr_trabajador[a].ent_fed_notario   ,
                 arr_trabajador[a].municipio_notario ,
                 arr_trabajador[a].num_rpp           ,
                 arr_trabajador[a].folio_real        ,
                 arr_trabajador[a].partida           ,
                 arr_trabajador[a].foja              ,
                 arr_trabajador[a].volumen           ,
                 arr_trabajador[a].libro             ,
                 arr_trabajador[a].tomo              ,
                 arr_trabajador[a].seccion           ,
                 arr_trabajador[a].ent_fed_inmueble  ,
                 v_mun_inmueble                      ,
                 arr_trabajador[a].domicilio_inmueble,
                 arr_trabajador[a].valor_avaluo      ,
                 arr_trabajador[a].monto_credito     ,
                 arr_trabajador[a].plazo_credito     ,
                 arr_trabajador[a].tpo_moneda        ,
                 arr_trabajador[a].tasa_base         ,
                 arr_trabajador[a].margen            ,
                 arr_trabajador[a].cred_convenidos   ,
                 arr_trabajador[a].f_otorga_cred_ef  ,
                 v_f_registro_carta                  ,
                 arr_trabajador[a].diagnostico       ,
                 v_situacion_ant                     ,
                 v_usuario_reg_carta                 ,
                 v_situacion                         ,
                 TODAY                               ,
                 p_usuario)

END FUNCTION

FUNCTION fn_his_garantia(v_situacion_ant)

   DEFINE v_situacion_ant           SMALLINT
   DEFINE v_estado                  SMALLINT
   DEFINE v_id_derechohabiente      DECIMAL(9,0)

   SELECT id_derechohabiente,
          estado
     INTO v_id_derechohabiente,
          v_estado
     FROM ocg_liquidacion
    WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

   --DISPLAY "fecha otorga :",arr_trabajador[a].f_otorga_cred_ef

   --DISPLAY "f_venc_imp_solic  : ",arr_trabajador[a].f_venc_imp_solic

   LET arr_trabajador[a].f_venc_imp_solic =  arr_trabajador[a].f_venc_imp_solic[5,6],
                                             arr_trabajador[a].f_venc_imp_solic[7,8],
                                             arr_trabajador[a].f_venc_imp_solic[1,4]

--comentado para que no truene en insert hasta solicuionar problema con desborde de variables en los importes
-- archivo de salida 13,2   ,  archivo de entrada 15 d�gitos, tablas y validaciones de sp003 definido como 12,2

  --arr_trabajador[a].imp_solic_uti_ocg ,
  --arr_trabajador[a].imp_utilizado_ocg ,

   INSERT INTO ocg_his_solic_uso_garantia
        VALUES ( arr_trabajador[a].id_ocg            ,
                 v_id_derechohabiente                ,
                 arr_trabajador[a].cve_ent_financiera,
                 arr_trabajador[a].num_ctrl_ef       ,
                 "",
                 arr_trabajador[a].f_venc_imp_solic  ,
                 "",
                 arr_trabajador[a].cred_convenidos   ,
                 arr_trabajador[a].solic_saldo       ,
                 arr_trabajador[a].diagnostico       ,
                 v_estado                            ,
                 v_situacion_ant                     ,
                 TODAY                               ,
                 p_usuario)
END FUNCTION

FUNCTION fn_his_liquidacion(v_situacion_ant)

   DEFINE v_situacion_ant           SMALLINT
   DEFINE v_estado                  SMALLINT
   DEFINE v_id_derechohabiente      DECIMAL(9,0)

   SELECT id_derechohabiente,
          estado
     INTO v_id_derechohabiente,
          v_estado
     FROM ocg_liquidacion
    WHERE id_ocg_liquidacion = arr_trabajador[a].id_ocg

   -- DISPLAY "f_libera_garantia             : ",arr_trabajador[a].f_libera_garantia
   -- DISPLAY "arr_trabajador[a].f_deposito  : ",arr_trabajador[a].f_deposito

   LET arr_trabajador[a].f_libera_garantia = arr_trabajador[a].f_libera_garantia[5,6],
                                             arr_trabajador[a].f_libera_garantia[7,8],
                                             arr_trabajador[a].f_libera_garantia[1,4]

   IF arr_trabajador[a].f_deposito IS NOT NULL THEN
      LET arr_trabajador[a].f_deposito        = arr_trabajador[a].f_deposito[5,6],
                                                arr_trabajador[a].f_deposito[7,8],
                                                arr_trabajador[a].f_deposito[1,4]
   END IF

   INSERT INTO ocg_his_liquidacion
        VALUES ( arr_trabajador[a].id_ocg            ,
                 v_id_derechohabiente                ,
                 arr_trabajador[a].cve_ent_financiera,
                 arr_trabajador[a].num_ctrl_ef       ,
                 arr_trabajador[a].bim_apor_subsec   ,
                 arr_trabajador[a].imp_aport_subsec  ,
                 arr_trabajador[a].f_libera_garantia ,
                 arr_trabajador[a].imp_ocg_devuelto  ,
                 arr_trabajador[a].causa_liquidacion ,
                 arr_trabajador[a].f_deposito        ,
                 arr_trabajador[a].cred_convenidos   ,
                 arr_trabajador[a].diagnostico       ,
                 v_estado                            ,
                 v_situacion_ant                     ,
                 TODAY                               ,
                 p_usuario)
END FUNCTION

FUNCTION fn_crea_tabla()

   DROP TABLE IF EXISTS tmp_detalle;
   DROP TABLE IF EXISTS tmp_id_referencia;

   CREATE TABLE tmp_detalle(
      tpo_registro                  CHAR(1),
      subproceso                    CHAR(3),
      tpo_envio                     CHAR(1),
      f_envio                       CHAR(8),
      cve_ent_financiera            CHAR(3),
      nss                           CHAR(11),
      num_ctrl_ef                   CHAR(18),
      rfc                           CHAR(13),
      curp                          CHAR(18),
      ap_paterno_af                 CHAR(40),
      ap_materno_af                 CHAR(40),
      nombre_af                     CHAR(40),
      num_bimestres                 CHAR(3),
      viv92                         CHAR(8),
      viv97                         CHAR(8),
      f_corte_subcuenta             CHAR(8),
      diagnostico                   SMALLINT,
      inconsistencias               CHAR(40),
      num_escritura                 CHAR(8),
      num_notario                   CHAR(4),
      ent_fed_notario               CHAR(2),
      municipio_notario             CHAR(3),
      num_rpp                       CHAR(15),
      folio_real                    CHAR(8),
      partida                       CHAR(6),
      foja                          CHAR(8),
      volumen                       CHAR(6),
      libro                         CHAR(6),
      tomo                          CHAR(6),
      seccion                       CHAR(6),
      ent_fed_inmueble              CHAR(2),
      domicilio_inmueble            CHAR(30),
      valor_avaluo                  CHAR(15),
      monto_credito                 CHAR(15),
      plazo_credito                 CHAR(5),
      tpo_moneda                    CHAR(2),
      tasa_base                     CHAR(20),
      margen                        CHAR(20),
      f_otorga_cred_ef              CHAR(8),
      imp_solic_uti_ocg             CHAR(15),
      f_venc_imp_solic              CHAR(8),
      imp_utilizado_ocg             CHAR(15),
      imp_aport_subsec              CHAR(15),
      bim_apor_subsec               CHAR(6),
      imp_subsec_devuelto           CHAR(15),
      f_libera_garantia             CHAR(8),
      imp_ocg_devuelto              CHAR(15),
      causa_liquidacion             CHAR(1),
      f_deposito                    CHAR(8),
      cred_convenidos               CHAR(1),
      solic_saldo                   CHAR(1),
      f_vigencia                    CHAR(8),
      filler                        CHAR(64))

   CREATE TABLE tmp_id_referencia
      (subproceso                   SMALLINT,
       id_ocg_referencia            DECIMAL(9,0))

END FUNCTION
