################################################################################
#Modulo           => AFI                                                       #
#Programa         => AFIP22                                                    #
#Objetivo         => Extractor de los indicadores para notificaciones          #
#Fecha de Inicio  => MARZO 2015                                                #
################################################################################
IMPORT os
DATABASE safre_viv

DEFINE g_pid                        DECIMAL(9,0)
DEFINE g_proceso_cod                SMALLINT
DEFINE g_usuario_cod                CHAR(20)
DEFINE g_opera_cod                  SMALLINT
DEFINE g_folio                      DECIMAL(9,0)
DEFINE g_nom_archivo                CHAR(40)
DEFINE g_f_inicial                  DATE
DEFINE g_f_final                    DATE
DEFINE g_indicador                  SMALLINT

DEFINE v_nombre_archivo             STRING

MAIN
   DEFINE v_estado              SMALLINT
   DEFINE p_titulo              STRING
   DEFINE p_mensaje             STRING

   LET g_usuario_cod           = ARG_VAL(1)
   LET g_pid                   = ARG_VAL(2)
   LET g_proceso_cod           = ARG_VAL(3)
   LET g_opera_cod             = ARG_VAL(4)
   LET g_folio                 = ARG_VAL(5)
   LET g_nom_archivo           = ARG_VAL(6)
   LET g_f_inicial             = ARG_VAL(7)
   LET g_f_final               = ARG_VAL(8)
   LET g_indicador             = ARG_VAL(9)

   CALL fn_display_proceso(0,"GENERA ARCHIVO CON INDICADORES")

   CALL fn_salida_archivos() RETURNING v_estado

   IF v_estado = 0 THEN
      DISPLAY ""
      DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
      DISPLAY "ARCHIVO GENERADO EN: ", v_nombre_archivo
      DISPLAY ""

      CALL fn_actualiza_opera_fin(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_estado
      LET p_mensaje = "Genera archivo de indicadores notificaciones correcto."
   ELSE
      LET p_mensaje = "El proceso de generación de archivo de indicadores ha finalizado pero con excepciones.\nVerificar en el sistema SACI."
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"GENERA ARCHIVO CON INDICADORES")

   LET p_titulo = "Genera archivos de indicadores notificaciones ", g_folio
   CALL fn_correo_proceso(g_pid, g_proceso_cod,
                          g_opera_cod,
                          NULL, p_titulo,p_mensaje)

END MAIN

FUNCTION fn_salida_archivos()
   DEFINE v_archivo           CHAR(40)
   DEFINE v_ruta_envio        CHAR(60)
   DEFINE ch                  base.Channel

   DEFINE v_rec_encabezado       RECORD
      tpo_registro                  CHAR(2),
      entidad_origen                SMALLINT,
      operacion                     SMALLINT,
      f_generacion                  DATE        
   END RECORD

   DEFINE v_rec_detalle          RECORD
      tpo_registro                  CHAR(2) ,         
      nss                           CHAR(11),
      ind_sms                       SMALLINT,
      ind_correo                    SMALLINT,
      bloqueo_int_sms               SMALLINT,
      bloqueo_int_correo            SMALLINT,
      fuente                        SMALLINT
   END RECORD

   DEFINE v_tpo_notificacion     SMALLINT
   DEFINE v_indicador            SMALLINT

   DEFINE v_rec_sumario          RECORD
      tpo_registro                  CHAR(2),   
      tot_registros                 INTEGER,
      tot_sms                       INTEGER,
      tot_correo                    INTEGER,
      tot_correo_sms                INTEGER,
      tot_bloqueo_correo            INTEGER,
      tot_bloqueo_sms               INTEGER
   END RECORD

   DEFINE v_cadena_registro      STRING
   DEFINE v_consecutivo          SMALLINT
   DEFINE v_bandera              SMALLINT
   DEFINE f_genera               DATE
   DEFINE v_query                STRING
   
   LET f_genera = TODAY

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   LET v_consecutivo    = 0 
   LET v_nombre_archivo = v_ruta_envio CLIPPED, "/EXTDATCONT",
                          f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".extdc"

   WHILE (os.Path.exists(v_nombre_archivo ))
      LET v_consecutivo = v_consecutivo + 1
      LET v_nombre_archivo = v_ruta_envio CLIPPED, "/EXTDATCONT",
                          f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".extdc"
   END WHILE

   LET v_archivo = "EXTDATCONT",
                    f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".extdc"

   UPDATE bat_ctr_operacion
      SET nom_archivo = v_archivo
    WHERE pid = g_pid
      AND proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nombre_archivo,"wb")

   LET v_query = "SELECT '02' tpo_registro,
                         b.nss ,
                         a.tpo_notificacion,
                         a.fuente_inicio
                    FROM afi_ind_notifica a,
                         afi_derechohabiente b
                   WHERE b.id_derechohabiente = a.id_derechohabiente
                     AND a.f_inicio BETWEEN '",g_f_inicial,"' AND '",g_f_final,"'"

   IF g_indicador IS NOT NULL AND
      g_indicador >= 1 AND 
      g_indicador <= 4 THEN
      LET v_query = v_query , " AND a.tpo_notificacion = ",g_indicador
   END IF

   LET v_query = v_query , " INTO TEMP tmp_extractor_indicadores "
   PREPARE prp_datos FROM v_query
   EXECUTE prp_datos

   LET v_query = " CREATE INDEX ixtmp_extractor_indicadores1 ON tmp_extractor_indicadores ",
                 " (nss) using btree  "
   PREPARE prp_indice FROM v_query
   EXECUTE prp_indice

   ----ENCABEZADO
   LET v_rec_encabezado.tpo_registro = '01'
   LET v_rec_encabezado.entidad_origen = 0
   LET v_rec_encabezado.operacion = 99
   LET v_rec_encabezado.f_generacion = f_genera

   LET v_cadena_registro = v_rec_encabezado.tpo_registro,
                           v_rec_encabezado.entidad_origen USING "&&",
                           v_rec_encabezado.operacion USING "&&",
                           v_rec_encabezado.f_generacion USING "YYYYMMDD"

   CALL ch.writeLine(v_cadena_registro)

   LET v_indicador = 1
   LET v_rec_sumario.tot_registros = 0
   ---DETALLE

   LET v_query = " SELECT tpo_registro, tpo_notificacion, fuente_inicio ",
                 "   FROM tmp_extractor_indicadores ",
                 "  WHERE nss = ? "
   PREPARE prp_detalle FROM v_query
   DECLARE cur_detalle CURSOR FOR prp_detalle


   DECLARE cur_nss CURSOR FOR SELECT UNIQUE nss 
                                FROM tmp_extractor_indicadores  
   FOREACH cur_nss INTO v_rec_detalle.nss

      INITIALIZE v_rec_detalle.ind_sms, 
                 v_rec_detalle.ind_correo,
                 v_rec_detalle.bloqueo_int_sms,
                 v_rec_detalle.bloqueo_int_correo TO NULL
   
      FOREACH cur_detalle USING v_rec_detalle.nss
                           INTO v_rec_detalle.tpo_registro,
                                v_tpo_notificacion,
                                v_rec_detalle.fuente

         CASE v_tpo_notificacion
            WHEN 1
               LET v_rec_detalle.ind_sms = v_indicador
            WHEN 2
               LET v_rec_detalle.ind_correo = v_indicador
            WHEN 3
               LET v_rec_detalle.bloqueo_int_correo = v_indicador
            WHEN 4
               LET v_rec_detalle.bloqueo_int_sms = v_indicador
         END CASE
      END FOREACH

      IF v_rec_detalle.ind_sms = 1 AND v_rec_detalle.ind_correo = 1 THEN
            LET v_rec_sumario.tot_correo_sms = v_rec_sumario.tot_correo_sms + 1   ###TIENE AMBOS INDICADORES 
      ELSE
         IF v_rec_detalle.ind_sms = 1 THEN
            LET v_rec_sumario.tot_sms = v_rec_sumario.tot_sms + 1 ###SOLO TIENE SMS
         END IF
         IF v_rec_detalle.ind_correo = 1 THEN
            LET v_rec_sumario.tot_correo = v_rec_sumario.tot_correo + 1 ###SOLO TIENE CORREO
         END IF
         IF v_rec_detalle.bloqueo_int_correo = 1 THEN
            LET v_rec_sumario.tot_bloqueo_correo = v_rec_sumario.tot_bloqueo_correo + 1
         END IF
         IF v_rec_detalle.bloqueo_int_sms = 1 THEN 
            LET v_rec_sumario.tot_bloqueo_sms = v_rec_sumario.tot_bloqueo_sms + 1
         END IF
      END IF

      LET v_cadena_registro = v_rec_detalle.tpo_registro,
                              v_rec_detalle.nss,
                              v_rec_detalle.ind_sms USING "#",
                              v_rec_detalle.ind_correo USING "#",
                              v_rec_detalle.bloqueo_int_sms USING "#",
                              v_rec_detalle.bloqueo_int_correo USING "#",
                              v_rec_detalle.fuente USING "&&&&"
      CALL ch.writeLine(v_cadena_registro)
      LET v_rec_sumario.tot_registros = v_rec_sumario.tot_registros + 1
   END FOREACH

   ----SUMARIO
   LET  v_rec_sumario.tpo_registro = '09'

   LET v_cadena_registro = v_rec_sumario.tpo_registro,
                           v_rec_sumario.tot_registros USING "&&&&&&&&&&&&",
                           v_rec_sumario.tot_sms USING "&&&&&&&&&&&&",
                           v_rec_sumario.tot_correo USING "&&&&&&&&&&&&",
                           v_rec_sumario.tot_correo_sms USING "&&&&&&&&&&&&",
                           v_rec_sumario.tot_bloqueo_sms USING "&&&&&&&&&&&&",
                           v_rec_sumario.tot_bloqueo_correo USING "&&&&&&&&&&&&"

                           
   CALL ch.writeLine(v_cadena_registro)
   CALL ch.close()

   DISPLAY ""
   DISPLAY "*****  SACI CIFRAS CONTROL  *****"
   DISPLAY "TOTAL DE INICADORES SMS:           ", v_rec_sumario.tot_sms USING "###,###,##&"
   DISPLAY "TOTAL DE INDICADORES CORREO:       ", v_rec_sumario.tot_correo USING "###,###,##&"
   DISPLAY "TOTAL DE INDICADORES SMS Y CORREO: ", v_rec_sumario.tot_correo_sms USING "###,###,##&"
   DISPLAY "TOTAL DE BLOQUEOS SMS:             ", v_rec_sumario.tot_bloqueo_sms USING "###,###,##&"
   DISPLAY "TOTAL DE BLOQUEOS CORREO:          ", v_rec_sumario.tot_bloqueo_correo USING "###,###,##&"
   DISPLAY "TOTAL DE REGISTROS EN DETALLE:     ", v_rec_sumario.tot_registros USING "###,###,##&"
   
   DISPLAY ""

   RETURN v_bandera
END FUNCTION
