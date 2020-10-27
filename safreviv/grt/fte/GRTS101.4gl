--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTS101                                       #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo de salida de 43BIS                 #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>15 Enero 2016                                 #
####################################################################


DATABASE safre_viv

GLOBALS
   DEFINE p_usuario           LIKE seg_usuario.usuario            -- nombre del usuario
   DEFINE p_pid               LIKE bat_ctr_proceso.pid         -- pid
   DEFINE p_proceso_cod       LIKE cat_proceso.proceso_cod        -- codigo del proceso
   DEFINE p_opera_cod         LIKE cat_operacion.opera_cod        -- codigo de la operacion de la etapa
   DEFINE p_f_generacion      DATE
   DEFINE p_f_publicacion     DATE
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio -- ruta donde se colocara el archivo
   DEFINE a                   INTEGER
   DEFINE ch                  base.Channel
   DEFINE v_nom_arh           STRING
   DEFINE v_folio             CHAR(10)
   DEFINE v_cp_arch           STRING
   DEFINE v_cmd               STRING
   DEFINE v_archivo           CHAR(40)
   DEFINE v_cta_t             INTEGER
   DEFINE v_cta_f             INTEGER
   DEFINE v_cta_g             INTEGER
   DEFINE v_cta_l             INTEGER
   DEFINE v_cta_tot           INTEGER
   DEFINE v_comando           STRING

   DEFINE arr_encabezado DYNAMIC ARRAY OF RECORD
          tpo_registro        CHAR(1),
          identificador       CHAR(8),
          destino             CHAR(3),
          f_transferencia     CHAR(8),
          situacion           CHAR(1),
          filler              CHAR(619)
   END RECORD

   DEFINE arr_trabajador DYNAMIC ARRAY OF RECORD
          id_grt              DECIMAL(9,0),
          tpo_registro        CHAR(1),
          subproceso          CHAR(3),
          tpo_envio           CHAR(1),
          f_envio             CHAR(8),
          cve_ent_financiera  CHAR(3),
          nss                 CHAR(11),
          num_ctrl_ef         CHAR(18),
          rfc                 CHAR(13),
          curp                CHAR(18),
          ap_paterno_af       CHAR(40),
          ap_materno_af       CHAR(40),
          nombre_af           CHAR(40),
          num_bimestres       CHAR(3),
          viv92               CHAR(8),
          viv97               CHAR(8),
          f_corte_subcuenta   CHAR(8),
          diagnostico         CHAR(2),
          inconsistencias     CHAR(40),
          num_escritura       CHAR(8),
          num_notario         CHAR(4),
          ent_fed_notario     CHAR(2),
          municipio_notario   CHAR(3),
          num_rpp             CHAR(15),
          folio_real          CHAR(8),
          partida             CHAR(6),
          foja                CHAR(8),
          volumen             CHAR(6),
          libro               CHAR(6),
          tomo                CHAR(6),
          seccion             CHAR(6),
          ent_fed_inmueble    CHAR(2),
          domicilio_inmueble  CHAR(30),
          valor_avaluo        CHAR(15),
          monto_credito       CHAR(15),
          plazo_credito       CHAR(5),
          tpo_moneda          CHAR(2),
          tasa_base           CHAR(20),
          margen              CHAR(20),
          f_otorga_cred_ef    CHAR(8),
          imp_solic_uti_grt   CHAR(15),
          f_venc_imp_solic    CHAR(8),
          imp_utilizado_grt   CHAR(15),
          imp_aport_subsec    CHAR(15),
          bim_apor_subsec     CHAR(6),
          imp_subsec_devuelto CHAR(15),
          f_libera_garantia   CHAR(8),
          imp_grt_devuelto    CHAR(15),
          causa_liquidacion   CHAR(1),
          f_deposito          CHAR(8),
          cred_convenidos     CHAR(1),
          solic_saldo         CHAR(1),
          f_vigencia          CHAR(8),
          filler              CHAR(64),
          estado              SMALLINT
   END RECORD

   DEFINE arr_sumario DYNAMIC ARRAY OF RECORD
          tpo_registro     CHAR(1),
          identificador    CHAR(8),
          destino          CHAR(3),
          tot_registros    CHAR(6),
          filler           CHAR(622)
   END RECORD

   DEFINE arr_incons DYNAMIC ARRAY OF RECORD
          inconsistencia CHAR(2)
   END RECORD

END GLOBALS

MAIN

DEFINE    v_msj               STRING

--CLOSE WINDOW SCREEN

   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   LET p_pid             = ARG_VAL(2)
   LET p_proceso_cod     = ARG_VAL(3)
   LET p_opera_cod       = ARG_VAL(4)
   LET p_f_generacion    = ARG_VAL(5)
   LET p_f_publicacion   = ARG_VAL(6)


   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'grt'

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/Art43bis",".Sal"

--OPEN WINDOW salida WITH FORM "GRTS1011"
--CALL ui.Interface.setText ( p_nom_ventana )

--MENU "Genera archivo de salida 43 BIS"
  -- ON ACTION ACCEPT

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".GRTS101.log")

   DISPLAY "=INICIA GRTS101="
   DISPLAY " USUARIO           : ",p_usuario
   DISPLAY " PID               : ",p_pid
   DISPLAY " Fecha Generación  : ",p_f_generacion
   DISPLAY " Fecha Publicación : ",p_f_publicacion

   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_43bis()
  -- EXIT MENU
   --ON ACTION CANCEL
   --EXIT MENU
--END MENU
--CLOSE WINDOW salida

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio

   LET v_cp_arch = v_ruta_envio CLIPPED ,"/Art43bis",TODAY USING "DDMMYYYY",v_folio CLIPPED,".Sal"
   LET v_cmd = "cp ",v_nom_arh," ",v_cp_arch
   RUN v_cmd

   LET v_comando = " sed 's/$/\r/' ", v_cp_arch CLIPPED," > ",v_nom_arh CLIPPED
   RUN v_comando

   LET v_archivo = "Art43bis",TODAY USING "DDMMYYYY",".Sal"
   LET v_cta_tot = v_cta_t + v_cta_f + v_cta_g +v_cta_l

   INSERT INTO grt_ctr_archivo
        VALUES (seq_grt_archivo.NEXTVAL,
                v_folio,
                "",
                p_proceso_cod,
                p_opera_cod,
                v_archivo,
                v_cta_tot,
                v_cta_t,
                v_cta_f,
                v_cta_g,
                "",
                v_cta_l,
                10,
                TODAY,
                p_usuario)
   

   LET v_msj = "Archivo generado de forma correcta en : "
   DISPLAY ""
   DISPLAY " ",v_msj," "
   DISPLAY v_nom_arh
   DISPLAY ""
   DISPLAY "PROCESO EJECUTADO CORRECTAMENTE"
   DISPLAY ""
   DISPLAY "=FINALIZA ARCHIVO DE SAIDA 43BIS="

END MAIN

FUNCTION fn_archivo_43bis()

   DEFINE v_qry_tramite       STRING
   DEFINE v_qry_formalizacion STRING
   DEFINE v_qry_garantia      STRING
   DEFINE v_qry_liquidacion   STRING
   DEFINE v_incons_t          INTEGER
   DEFINE v_incons_f          INTEGER
   DEFINE v_incons_g          INTEGER
   DEFINE v_incons_l          INTEGER
   DEFINE v_qry_incons        STRING
   DEFINE z                   INTEGER
   DEFINE v_incons            STRING
   DEFINE v_det_enc           STRING
   DEFINE v_det_sum           STRING

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_trabajador.clear()
   CALL arr_encabezado.clear()
   CALL arr_trabajador.clear()
   CALL arr_sumario.clear()

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

      -- DISPLAY "cuenta registros tramite:",v_cta_t 

   -- cadena para archivo de salida con detalle para SP001 tramite
   LET v_qry_tramite = "SELECT grt.id_grt_tramite,
                               grt.viv97,
                               lpad(year(grt.f_saldo),4,0)||
                               lpad(month(grt.f_saldo),2,0)||
                               lpad(day(grt.f_saldo),2,0),
                               grt.diagnostico,
                               lpad(year(grt.f_vigencia),4,0)||
                               lpad(month(grt.f_vigencia),2,0)||
                               lpad(day(grt.f_vigencia),2,0),
                               grt.cve_ent_financiera,
                               det.nss,
                               grt.rfc,
                               grt.curp,
                               grt.ap_paterno,
                               grt.ap_materno,
                               grt.nombre,
                               grt.tpo_credito,
                               grt.estado
                          FROM grt_tramite grt, grt_detalle det
                         WHERE grt.estado in (20,30)
                           AND det.id_grt_detalle = grt.id_grt_detalle
                               order by grt.cve_ent_financiera asc"
   --cadena para archivo de salida con detalle para SP002 fromalización
   LET v_qry_formalizacion = "SELECT grt.id_grt_formalizacion,
                                     grt.cve_ent_financiera,
                                     det.nss,
                                     grt.num_ctr_int_ef,
                                     t.rfc,
                                     t.curp,
                                     t.ap_paterno,
                                     t.ap_materno,
                                     t.nombre,
                                     grt.diagnostico,
                                     grt.num_escritura,
                                     grt.notario,
                                     grt.ent_fed_notario,
                                     grt.mcpio_notario,
                                     grt.num_rpp,
                                     grt.folio_real,
                                     grt.partida,
                                     grt.foja,
                                     grt.volumen,
                                     grt.libro,
                                     grt.tomo,
                                     grt.seccion,
                                     grt.ent_fed_inmueble,
                                     grt.domicilio_inmueble,
                                     grt.valor_avaluo,
                                     grt.monto_credito,
                                     grt.plazo_credito,
                                     grt.tpo_moneda,
                                     grt.tasa_base,
                                     grt.margen,
                                     lpad(year(grt.f_otorga_ent_fin),4,0)||
                                     lpad(month(grt.f_otorga_ent_fin),2,0)||
                                     lpad(day(grt.f_otorga_ent_fin),2,0),
                                     grt.tpo_credito,
                                     grt.estado
                                FROM grt_formalizacion grt,
                                     grt_tramite t,
                                     grt_detalle det
                               WHERE grt.id_grt_tramite = t.id_grt_tramite
                                 AND grt.id_grt_detalle = det.id_grt_detalle
                                 AND grt.estado in (20,55)
                                     order by grt.cve_ent_financiera asc"
   -- cadena para archivo de salida con detalle para SP003 garantia
   LET v_qry_garantia = "SELECT grt.id_grt_solicitud_ug,
                                grt.cve_ent_financiera,
                                det.nss,
                                grt.num_ctr_int_ef,
                                grt.diagnostico,
                                grt.importe_solicitado,
                                lpad(year(grt.f_vencimiento),4,0)||
                                lpad(month(grt.f_vencimiento),2,0)||
                                lpad(day(grt.f_vencimiento),2,0),
                                grt.solicitud_saldo,
                                grt.estado
                           FROM grt_solicitud_uso_garantia grt,
                                grt_detalle det
                          WHERE grt.id_grt_detalle = det.grt_detalle
                            AND grt.estado = 20
                                order by grt.cve_ent_financiera asc"

   -- cadena para archivo de salida con detalle para SP005 liquidación
   LET v_qry_liquidacion = "SELECT grt.id_grt_liquidacion,
                                   grt.cve_ent_financiera,
                                   det.nss,
                                   grt.num_ctr_int_ef,
                                   grt.diagnostico,
                                   grt.bimestre_ap_subsec,
                                   grt.importe_ap_subsec,
                                   lpad(year(grt.f_liberacion_gtia),4,0)||
                                   lpad(month(grt.f_liberacion_gtia),2,0)||
                                   lpad(day(grt.f_liberacion_gtia),2,0),
                                   grt.importe_devuelto,
                                   grt.id_causa_liquida,
                                   lpad(year(grt.f_deposito),4,0)||
                                   lpad(month(grt.f_deposito),2,0)||
                                   lpad(day(grt.f_deposito),2,0),
                                   grt.estado
                              FROM grt_liquidacion grt,
                                   grt_detalle det
                             WHERE grt.id_grt_detalle = det.id_grt_detalle
                               AND grt.estado = 20
                                   order by grt.cve_ent_financiera asc"
--******************************************************************************
LET a = 1
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_t
     FROM grt_tramite grt, afi_derechohabiente afi
    WHERE grt.estado IN (20,30)
      AND grt.id_derechohabiente = afi.id_derechohabiente

   --DISPLAY "cuenta tramite ",v_cta_t

   IF v_cta_t > 0 THEN

   -- se llena arreglo con datos de tabla tramite
   PREPARE prp_tramite FROM v_qry_tramite
   DECLARE cur_tramite CURSOR FOR prp_tramite

   --LET a = 1
   FOREACH cur_tramite INTO arr_trabajador[a].id_grt,
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
                            arr_trabajador[a].estado

      --DISPLAY "datos de arreglo tramite:",arr_trabajador[a].*
      SELECT COUNT(*)
        INTO v_incons_t
        FROM grt_inconsistencia
       WHERE id_grt_referencia = arr_trabajador[a].id_grt

      -- DISPLAY "inconsistencias tramite :",v_incons_t

      IF v_incons_t > 0 THEN

         LET v_qry_incons = "select inconsistencia
                               from grt_inconsistencia
                              where id_grt_referencia = ",arr_trabajador[a].id_grt

         PREPARE prp_incons FROM v_qry_incons
         DECLARE cur_incons CURSOR FOR prp_incons

         LET z= 1
         CALL arr_incons.clear()
         FOREACH cur_incons INTO arr_incons[z].inconsistencia
            LET z= z+1
         END FOREACH

         LET v_incons = ""

         FOR z=1 TO v_incons_t
            LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
            --DISPLAY "inconsistencias :",v_incons
         END FOR
      END IF

      LET arr_trabajador[a].tpo_registro     = "2"
      LET arr_trabajador[a].subproceso       = "001"
      LET arr_trabajador[a].tpo_envio        = "I"
      LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
      LET arr_trabajador[a].inconsistencias  = v_incons
      LET arr_trabajador[a].viv92            = "0.0"

      CALL fn_escribe()

      IF arr_trabajador[a].estado = 20 THEN
         UPDATE grt_tramite
            SET estado = 40
          WHERE id_grt_tramite = arr_trabajador[a].id_grt
          CALL fn_his_tramite(20)
      END IF

      IF arr_trabajador[a].estado = 30 THEN
         UPDATE grt_tramite
            SET estado = 50
          WHERE id_grt_tramite = arr_trabajador[a].id_grt
          CALL fn_his_tramite(30)
      END IF 

      LET a = a+1
   END FOREACH

   IF arr_trabajador[a].diagnostico IS NULL THEN
      CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
   END IF
   END IF
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_f
     FROM grt_formalizacion grt,afi_derechohabiente afi
    WHERE grt.estado IN (20,55)
      AND grt.id_derechohabiente = afi.id_derechohabiente

      --DISPLAY "cuenta formalización",v_cta_f
   IF v_cta_f > 0 THEN
   -- se llena arreglo con datos de tabla fromalización
   --LET a = a +1

   PREPARE prp_formalizacion FROM v_qry_formalizacion
   DECLARE cur_formalizacion CURSOR FOR prp_formalizacion
   
   FOREACH cur_formalizacion INTO arr_trabajador[a].id_grt            ,
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
                                  arr_trabajador[a].estado

      --DISPLAY "datos de arreglo formalizacion :",arr_trabajador[a].*

      SELECT COUNT(*)
        INTO v_incons_f
        FROM grt_inconsistencia
       WHERE id_grt_referencia = arr_trabajador[a].id_grt

      -- DISPLAY "inconsistencias tramite :",v_incons_t

      IF v_incons_f > 0 THEN

         LET v_qry_incons = "select inconsistencia
                               from grt_inconsistencia
                              where id_grt_referencia = ",arr_trabajador[a].id_grt

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
      END IF

      LET arr_trabajador[a].tpo_registro     = "2"
      LET arr_trabajador[a].subproceso       = "002"
      LET arr_trabajador[a].tpo_envio        = "I"
      LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
      LET arr_trabajador[a].inconsistencias  = v_incons
      LET arr_trabajador[a].viv92            = "0.0"
      LET arr_trabajador[a].viv97            = "0.0"

      CALL fn_escribe()

     -- DISPLAY "estado formalizacion",arr_trabajador[a].estado

      IF arr_trabajador[a].estado = 20 THEN
         UPDATE grt_formalizacion
            SET estado = 40
          WHERE id_grt_formalizacion = arr_trabajador[a].id_grt
          CALL fn_his_formalizacion(20)
      END IF

      IF arr_trabajador[a].estado = 55 THEN
         UPDATE grt_formalizacion
            SET estado = 60
          WHERE id_grt_formalizacion = arr_trabajador[a].id_grt
          CALL fn_his_formalizacion(55)
      END IF


      LET a = a+1
   END FOREACH

   IF arr_trabajador[a].diagnostico IS NULL THEN
      CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
   END IF
   END IF
--******************************************************************************

   SELECT COUNT(*)
     INTO v_cta_g
     FROM grt_solicitud_uso_garantia grt , afi_derechohabiente afi
    WHERE grt.estado = 20
      AND grt.id_derechohabiente = afi.id_derechohabiente

      --DISPLAY "cuenta garantia :",v_cta_g

   IF v_cta_g > 0 THEN

   --DISPLAY "punto control ",v_cta_g

    -- se llena arreglo con datos de tabla uso garantia
   --LET a = a +1

   PREPARE prp_garantia FROM v_qry_garantia
   DECLARE cur_garantia CURSOR FOR prp_garantia

   FOREACH cur_garantia INTO arr_trabajador[a].id_grt,
                             arr_trabajador[a].cve_ent_financiera,
                             arr_trabajador[a].nss,
                             arr_trabajador[a].num_ctrl_ef,
                             arr_trabajador[a].diagnostico,
                             arr_trabajador[a].imp_solic_uti_grt,
                             arr_trabajador[a].f_venc_imp_solic,
                             arr_trabajador[a].solic_saldo,
                             arr_trabajador[a].estado

      --DISPLAY "datos de arreglo :",arr_trabajador[a].*

      SELECT COUNT(*)
        INTO v_incons_g
        FROM grt_inconsistencia
       WHERE id_grt_referencia = arr_trabajador[a].id_grt

      -- DISPLAY "inconsistencias tramite :",v_incons_t

      IF v_incons_g > 0 THEN

         LET v_qry_incons = "select inconsistencia
                               from grt_inconsistencia
                              where id_grt_referencia = ",arr_trabajador[a].id_grt

         PREPARE prp_incons3 FROM v_qry_incons
         DECLARE cur_incons3 CURSOR FOR prp_incons3

         CALL arr_incons.clear()

         LET z= 1
         CALL arr_incons.clear()
         FOREACH cur_incons INTO arr_incons[z].inconsistencia
            LET z= z+1
         END FOREACH

         LET v_incons = ""

         FOR z=1 TO v_incons_g
            LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
            --DISPLAY "inconsistencias :",v_incons
         END FOR
      END IF

      LET arr_trabajador[a].tpo_registro     = "2"
      LET arr_trabajador[a].subproceso       = "003"
      LET arr_trabajador[a].tpo_envio        = "I"
      LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
      LET arr_trabajador[a].inconsistencias  = v_incons
      LET arr_trabajador[a].viv92            = "0.0"
      LET arr_trabajador[a].viv97            = "0.0"

      CALL fn_escribe()
      LET a = a+1
   END FOREACH

   IF arr_trabajador[a].diagnostico IS NULL THEN
      CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
   END IF
   END IF
--******************************************************************************

   SELECT COUNT(*)
     INTO v_cta_l
     FROM grt_liquidacion grt, afi_derechohabiente afi
    WHERE grt.estado = 20
      AND grt.id_derechohabiente = afi.id_derechohabiente

      --DISPLAY "cuenta liquidación :",v_cta_l

   IF v_cta_l > 0 THEN

    -- se llena arreglo con datos de tabla fromalización
   --LET a = a +1

   PREPARE prp_liquidacion FROM v_qry_liquidacion
   DECLARE cur_liquidacion CURSOR FOR prp_liquidacion
   
   FOREACH cur_liquidacion INTO arr_trabajador[a].id_grt,
                                arr_trabajador[a].cve_ent_financiera,
                                arr_trabajador[a].nss,
                                arr_trabajador[a].num_ctrl_ef,
                                arr_trabajador[a].diagnostico,
                                arr_trabajador[a].bim_apor_subsec,
                                arr_trabajador[a].imp_subsec_devuelto,
                                arr_trabajador[a].f_libera_garantia,
                                arr_trabajador[a].imp_grt_devuelto,
                                arr_trabajador[a].causa_liquidacion,
                                arr_trabajador[a].f_deposito,
                                arr_trabajador[a].estado
      --DISPLAY "datos de arreglo :",arr_trabajador[a].*

      SELECT COUNT(*)
        INTO v_incons_l
        FROM grt_inconsistencia
       WHERE id_grt_referencia = arr_trabajador[a].id_grt

      -- DISPLAY "inconsistencias tramite :",v_incons_t

      IF v_incons_l > 0 THEN

         LET v_qry_incons = "select inconsistencia
                               from grt_inconsistencia
                              where id_grt_referencia = ",arr_trabajador[a].id_grt

         PREPARE prp_incons4 FROM v_qry_incons
         DECLARE cur_incons4 CURSOR FOR prp_incons4

         CALL arr_incons.clear()

         LET z= 1
         CALL arr_incons.clear()
         FOREACH cur_incons INTO arr_incons[z].inconsistencia
            LET z= z+1
         END FOREACH

         LET v_incons = ""

         FOR z=1 TO v_incons_l
            LET v_incons = v_incons CLIPPED,arr_incons[z].inconsistencia
            --DISPLAY "inconsistencias :",v_incons
         END FOR
      END IF

      LET arr_trabajador[a].tpo_registro     = "2"
      LET arr_trabajador[a].subproceso       = "005"
      LET arr_trabajador[a].tpo_envio        = "I"
      LET arr_trabajador[a].f_envio          = TODAY USING "YYYYMMDD"
      LET arr_trabajador[a].inconsistencias  = v_incons
      LET arr_trabajador[a].viv92            = "0.0"
      LET arr_trabajador[a].viv97            = "0.0"

      CALL fn_escribe()
      LET a = a+1
   END FOREACH

   IF arr_trabajador[a].diagnostico IS NULL THEN
      CALL arr_trabajador.deleteElement(arr_trabajador.getLength())
   END IF
   END IF
--******************************************************************************
   --DISPLAY v_cta_t 
   --DISPLAY v_cta_f 
   --DISPLAY v_cta_g 
   --DISPLAY v_cta_l
-- se llena arreglo para el sumario
   

   LET arr_sumario[1].tpo_registro    = "3"
   LET arr_sumario[1].identificador   = "ART43BIS"
   LET arr_sumario[1].destino         = "SER"
   LET arr_sumario[1].tot_registros   = v_cta_t + v_cta_f + v_cta_g + v_cta_l
   LET arr_sumario[1].filler          =""

   LET v_det_sum = arr_sumario[1].tpo_registro,
                   arr_sumario[1].identificador,
                   arr_sumario[1].destino,
                   arr_sumario[1].tot_registros USING "&&&&&&",
                   arr_sumario[1].filler

   CALL ch.writeLine([v_det_sum])
--******************************************************************************

   CALL ch.close()

END FUNCTION 

FUNCTION fn_escribe()

   DEFINE v_detalle           STRING

   IF arr_trabajador[a].cred_convenidos = "A" THEN
      LET arr_trabajador[a].cred_convenidos = " "
   END IF

   LET v_detalle = 
   arr_trabajador[a].tpo_registro        ,--USING "&",
   arr_trabajador[a].subproceso          ,--USING "&&&",
   arr_trabajador[a].tpo_envio           ,--USING "&",
   arr_trabajador[a].f_envio             ,--USING "&&&&&&&&",
   arr_trabajador[a].cve_ent_financiera  USING "&&&",
   arr_trabajador[a].nss                 ,--USING "&&&&&&&&&&&",
   arr_trabajador[a].num_ctrl_ef         ,--USING "&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].rfc                 ,--USING "&&&&&&&&&&&&&",
   arr_trabajador[a].curp                ,--USING "&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].ap_paterno_af       ,--USING "########################################",
   arr_trabajador[a].ap_materno_af       ,--USING "########################################",
   arr_trabajador[a].nombre_af           ,--USING "########################################",
   arr_trabajador[a].num_bimestres       ,--USING "&&&",
   arr_trabajador[a].viv92               USING "&&&&&&&&",
   arr_trabajador[a].viv97               USING "&&&&&&&&",
   arr_trabajador[a].f_corte_subcuenta   ,--USING "&&&&&&&&",
   arr_trabajador[a].diagnostico         USING "&&",
   arr_trabajador[a].inconsistencias     ,--USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].num_escritura       ,--USING "&&&&&&&&",
   arr_trabajador[a].num_notario         ,--USING "&&&&",
   arr_trabajador[a].ent_fed_notario     ,--USING "&&",
   arr_trabajador[a].municipio_notario   ,--USING "&&&",
   arr_trabajador[a].num_rpp             ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].folio_real          ,--USING "&&&&&&&&",
   arr_trabajador[a].partida             ,--USING "&&&&&&",
   arr_trabajador[a].foja                ,--USING "&&&&&&&&",
   arr_trabajador[a].volumen             ,--USING "&&&&&&",
   arr_trabajador[a].libro               ,--USING "&&&&&&",
   arr_trabajador[a].tomo                ,--USING "&&&&&&",
   arr_trabajador[a].seccion             ,--USING "&&&&&&",
   arr_trabajador[a].ent_fed_inmueble    ,--USING "&&",
   arr_trabajador[a].domicilio_inmueble  ,--USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].valor_avaluo        ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].monto_credito       ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].plazo_credito       ,--USING "&&&&&",
   arr_trabajador[a].tpo_moneda          ,--USING "&&",
   arr_trabajador[a].tasa_base           ,--USING "&&&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].margen              ,--USING "&&&&&&&&&&&&&&&&&&&&",
   arr_trabajador[a].f_otorga_cred_ef    ,--USING "&&&&&&&&",
   arr_trabajador[a].imp_solic_uti_grt   ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].f_venc_imp_solic    ,--USING "&&&&&&&&",
   arr_trabajador[a].imp_utilizado_grt   ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].imp_aport_subsec    ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].bim_apor_subsec     ,--USING "&&&&&&",
   arr_trabajador[a].imp_subsec_devuelto ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].f_libera_garantia   ,--USING "&&&&&&&&",
   arr_trabajador[a].imp_grt_devuelto    ,--USING "&&&&&&&&&&&&&&&",
   arr_trabajador[a].causa_liquidacion   ,--USING "&",
   arr_trabajador[a].f_deposito          ,--USING "&&&&&&&&",
   arr_trabajador[a].cred_convenidos     ,--USING "&",
   arr_trabajador[a].solic_saldo         ,--USING "&",
   arr_trabajador[a].f_vigencia          ,--USING "&&&&&&&&",
   arr_trabajador[a].filler              --USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"

   CALL ch.writeLine([v_detalle])

END FUNCTION

FUNCTION fn_his_tramite(v_estado_ant)

   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_situacion          SMALLINT
   DEFINE v_estado_ant         SMALLINT

   LET v_id_Derechohabiente = ""
   LET v_situacion = ""

   SELECT id_derechohabiente,situacion
     INTO v_id_derechohabiente,v_situacion
     FROM grt_tramite
    WHERE id_grt_tramite = arr_trabajador[a].id_grt

    LET arr_trabajador[a].f_corte_subcuenta = arr_trabajador[a].f_corte_subcuenta [5,6],
                                              arr_trabajador[a].f_corte_subcuenta [7,8],
                                              arr_trabajador[a].f_corte_subcuenta [1,4]

---DISPLAY arr_trabajador[a].f_corte_subcuenta

    LET arr_trabajador[a].f_vigencia = arr_trabajador[a].f_vigencia [5,6],
                                       arr_trabajador[a].f_vigencia [7,8],
                                       arr_trabajador[a].f_vigencia [1,4]

DISPLAY arr_trabajador[a].f_vigencia
    
   INSERT INTO grt_his_tramite 
        VALUES(arr_trabajador[a].id_grt,
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
               v_estado_ant,
               v_situacion,
               TODAY,
               p_usuario )
   
END FUNCTION

FUNCTION fn_his_formalizacion(v_estado_ant)

   DEFINE v_estado_ant SMALLINT
   DEFINE v_mun_inmueble DECIMAL(5,0)
   DEFINE v_f_registro_carta DATE
   DEFINE v_usuario_reg_carta CHAR(20)
   DEFINE v_situacion SMALLINT
   DEFINE v_id_derechohabiente DECIMAL(9,0)

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
     FROM grt_formalizacion
    WHERE id_grt_formalizacion = arr_trabajador[a].id_grt

    LET arr_trabajador[a].f_otorga_cred_ef = arr_trabajador[a].f_otorga_cred_ef[5,6],
                                             arr_trabajador[a].f_otorga_cred_ef[7,8],
                                             arr_trabajador[a].f_otorga_cred_ef[1,4]

   --DISPLAY "fecha otorga :",arr_trabajador[a].f_otorga_cred_ef
   
   INSERT INTO grt_his_formalizacion
        VALUES ( arr_trabajador[a].id_grt            ,
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
                 v_estado_ant                        ,
                 v_usuario_reg_carta                 ,
                 v_situacion                         ,
                 TODAY                               ,
                 p_usuario)

END FUNCTION