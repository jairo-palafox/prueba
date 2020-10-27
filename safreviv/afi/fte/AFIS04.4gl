#################################################################################
#Programa AFIS04   => Programa lanzadoque genera archivo de salida con          #
#                     registros rechazados de movimientos op 75                 #
#Fecha             => 26 de Enero de 2015                                       #
#Autor             => Héctor Jiménez                                            #
#################################################################################
DATABASE safre_viv

GLOBALS
   DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod
   DEFINE v_proceso_cod LIKE cat_proceso.proceso_cod
END GLOBALS

MAIN

   DEFINE v_sql                 STRING
   DEFINE v_ruta_envio          LIKE seg_modulo.ruta_envio
   DEFINE v_ruta_completa       STRING                      -- ruta completa del archivo
   DEFINE v_nombre_archivo      STRING                      -- nombre del archivo
   DEFINE v_channel             base.Channel                -- apuntador para archivo
   DEFINE v_cadena              STRING                      -- para escribir registro en archivo
   DEFINE v_cadena_sumario      STRING                      -- cadena para el sumario
   DEFINE v_num_total_movs      SMALLINT                    -- contador
   DEFINE v_folio               LIKE glo_folio.folio        -- folio del proceso
   DEFINE v_sql_folio           STRING

   DEFINE v_rec_archivo RECORD
      tpo_registro        CHAR (2),
      cve_operacion       CHAR (2),
      nss_solicitud       CHAR (11),
      rfc                 CHAR (13),
      curp                CHAR (18),
      ap_paterno          CHAR (40),
      ap_materno          CHAR (40),
      nombre              CHAR (40),
      f_nacimiento        CHAR (8),
      sexo                CHAR (1),
      entidad_nacimiento  CHAR (2),
      estatus_trabajador  CHAR (1),
      filler              CHAR (222),
      cod_rechazo         SMALLINT ,
      des_rechazo         CHAR (40)
   END RECORD

   DEFINE rec_rech_afil RECORD
      tpo_registro        CHAR (2) ,
      cve_operacion       CHAR (2) ,
      nss_solicitud       CHAR (11),
      rfc                 CHAR (13),
      curp                CHAR (18),
      ap_paterno          CHAR (40),
      ap_materno          CHAR (40),
      nombre              CHAR (40),
      f_nacimiento        DATE     ,
      sexo                CHAR (1) ,
      entidad_nacimiento  CHAR (2) ,
      estatus_trabajador  CHAR (1) ,
      filler              CHAR (222),
      cod_rechazo         SMALLINT,
      des_rechazo         CHAR (40)
   END RECORD

   DEFINE v_rec_sumario RECORD
      tpo_registro              CHAR(2),
      tpo_entidad_origen        CHAR(2),
      cve_entidad_origen        CHAR(3),
      tpo_entidad_destino       CHAR(2),
      cve_entidad_destino       CHAR(3),
      identificador_servicio    CHAR(2),
      identificador_opera       CHAR(2),
      f_transferencia_lote      DATE   ,--CHAR(8),
      consecutivo_dia           CHAR(3),
      num_registros_entada      CHAR (9),
      num_registros_salida      CHAR (9),
      filler                    CHAR(359)
   END RECORD

   DEFINE v_fecha_formateada       CHAR (8)  -- Variable para formatear la fecha
   LET p_usuario_cod  = ARG_VAL(1)       -- usuario que ejecuta el proceso
   LET v_folio        = ARG_VAL(2)       -- folio del proceso
   LET v_proceso_cod  = ARG_VAL(3)       -- codigo de proceso

   -- se obtiene la ruta envío
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   -- se conforma la ruta del archivo
   LET v_nombre_archivo = "AFI_IM_RCH_", v_folio USING "&&&&&&&&&"

   LET v_ruta_completa = v_ruta_envio CLIPPED, "/", v_nombre_archivo, ".rch75"

   DISPLAY "\n_________________________________________________________________\n"
   DISPLAY "Generando archivo de registros rechazados\n"
   DISPLAY "   Archivo: ", v_ruta_completa

   -- se crea el apuntador para escribir el archivo
   LET v_channel = base.Channel.create()

   -- se abre el channel
   CALL v_channel.setDelimiter("")
   CALL v_channel.openFile(v_ruta_completa, "w")

   LET v_num_total_movs = 0

   LET v_sql = "SELECT det.*,
                       rch.cod_rechazo,
                       crch.des_rechazo
                  FROM safre_tmp:tmp_afi_det_op75 det,
                       afi_rch_afiliatorio rch,
                       afi_cat_rch crch
                 WHERE det.nss NOT IN (             
                                 SELECT nss
                                   FROM afi_derechohabiente)
                   AND det.nss = rch.nss
                   AND rch.cod_rechazo = crch.cod_rechazo
                GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15"

   PREPARE prp_rechazos FROM v_sql
   DECLARE cur_rechazos CURSOR FOR prp_rechazos

   -- escritura para los datos de el detalle del archivo
   FOREACH cur_rechazos INTO rec_rech_afil.*
      LET v_rec_archivo.tpo_registro        = rec_rech_afil.tpo_registro
      LET v_rec_archivo.cve_operacion       = rec_rech_afil.cve_operacion
      LET v_rec_archivo.nss_solicitud       = rec_rech_afil.nss_solicitud
      LET v_rec_archivo.rfc                 = rec_rech_afil.rfc
      LET v_rec_archivo.curp                = rec_rech_afil.curp
      LET v_rec_archivo.ap_paterno          = rec_rech_afil.ap_paterno
      LET v_rec_archivo.ap_materno          = rec_rech_afil.ap_materno
      LET v_rec_archivo.nombre              = rec_rech_afil.nombre
      LET v_rec_archivo.f_nacimiento        = rec_rech_afil.f_nacimiento USING "yyyymmdd"
      LET v_rec_archivo.sexo                = rec_rech_afil.sexo
      LET v_rec_archivo.entidad_nacimiento  = rec_rech_afil.entidad_nacimiento
      LET v_rec_archivo.estatus_trabajador  = rec_rech_afil.estatus_trabajador
      LET v_rec_archivo.filler              = rec_rech_afil.filler
      LET v_rec_archivo.cod_rechazo         = rec_rech_afil.cod_rechazo
      LET v_rec_archivo.des_rechazo         = rec_rech_afil.des_rechazo


      LET v_num_total_movs = v_num_total_movs + 1

      -- concatenacion de los campos
      LET v_cadena = v_rec_archivo.tpo_registro       ,
                     v_rec_archivo.cve_operacion      ,
                     v_rec_archivo.nss_solicitud      ,
                     v_rec_archivo.rfc                ,
                     v_rec_archivo.curp               ,
                     v_rec_archivo.ap_paterno         ,
                     v_rec_archivo.ap_materno         ,
                     v_rec_archivo.nombre             ,
                     v_rec_archivo.f_nacimiento       ,
                     v_rec_archivo.sexo               ,
                     v_rec_archivo.entidad_nacimiento ,
                     v_rec_archivo.estatus_trabajador ,
                     v_rec_archivo.filler             ,
                     v_rec_archivo.cod_rechazo        ,
                     v_rec_archivo.des_rechazo

      -- Se escribe registro en archivo
      CALL v_channel.writeLine(v_cadena)

   END FOREACH

   -- Se escribe el registro de sumario
   LET v_sql_folio = "SELECT tpo_registro        ,
                             tpo_entidad_origen  ,
                             cve_entidad_origen  ,
                             tpo_entidad_destino ,
                             cve_entidad_destino ,
                             id_servicio         ,
                             id_operacion        ,
                             f_transferencia     ,
                             consecutivo         ,
                             filler
                        FROM safre_tmp:tmp_afi_sum_op75"

   PREPARE prp_exe_sumario FROM v_sql_folio
   EXECUTE prp_exe_sumario INTO v_rec_sumario.tpo_registro           ,
                                v_rec_sumario.tpo_entidad_origen     ,
                                v_rec_sumario.cve_entidad_origen     ,
                                v_rec_sumario.tpo_entidad_destino    ,
                                v_rec_sumario.cve_entidad_destino    ,
                                v_rec_sumario.identificador_servicio ,
                                v_rec_sumario.identificador_opera    ,
                                v_rec_sumario.f_transferencia_lote   ,
                                v_rec_sumario.consecutivo_dia        ,
                                v_rec_sumario.filler

   LET v_fecha_formateada                   = v_rec_sumario.f_transferencia_lote USING "yyyymmdd"
   LET v_rec_sumario.consecutivo_dia        = v_rec_sumario.consecutivo_dia USING "&&&"
   LET v_rec_sumario.num_registros_entada   = v_num_total_movs USING "&&&&&&&&&"
   LET v_rec_sumario.num_registros_salida   = v_num_total_movs USING "&&&&&&&&&"

   LET v_cadena_sumario = v_rec_sumario.tpo_registro           ,
                          v_rec_sumario.tpo_entidad_origen     ,
                          v_rec_sumario.cve_entidad_origen     ,
                          v_rec_sumario.tpo_entidad_destino    ,
                          v_rec_sumario.cve_entidad_destino    ,
                          v_rec_sumario.identificador_servicio ,
                          v_rec_sumario.identificador_opera    ,
                          v_fecha_formateada,--v_rec_sumario.f_transferencia_lote   ,
                          v_rec_sumario.consecutivo_dia        ,
                          v_rec_sumario.num_registros_entada   ,
                          v_rec_sumario.num_registros_salida   ,
                          v_rec_sumario.filler

   -- Se escribe el registro en el archivo
   CALL v_channel.write([v_cadena_sumario])

   -- se cierra el archivo
   CALL v_channel.close()

   -- se libera el cursor
   FREE cur_rechazos

   DISPLAY "\n\nFinalizado...\n\n"
   DISPLAY "_________________________________________________________________\n"

END MAIN