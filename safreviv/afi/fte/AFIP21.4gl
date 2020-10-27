################################################################################
#Modulo           => GLO                                                       #
#Programa         => AFIP21                                                    #
#Objetivo         => Genera archivo de datos de contacto erroneos              #
#Fecha de Inicio  => FEBERERO 2015                                             #
################################################################################
IMPORT os
DATABASE safre_viv

DEFINE g_pid                        DECIMAL(9,0)
DEFINE g_proceso_cod                SMALLINT
DEFINE g_usuario_cod                CHAR(20)
DEFINE g_opera_cod                  SMALLINT
DEFINE g_folio                      DECIMAL(9,0)
DEFINE g_nom_archivo                CHAR(40)

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

   CALL fn_display_proceso(0,"GENERA ARCHIVO RECHAZOS INDICADORES NOTIFICACIONES")

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
      LET p_mensaje = "Genera archivo de de rechazos indicadores notificaciones correcto."
   ELSE
      LET p_mensaje = "El proceso de generación de archivo de rechazos ha finalizado pero con excepciones.\nVerificar en el sistema SACI."
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"GENERA ARCHIVO RECHAZOS INDICADORES NOTIFICACIONES")

   LET p_titulo = "Genera archivos de rechazos notificaciones ", g_folio
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
      fuente                        SMALLINT,
      estado                        SMALLINT
   END RECORD

   DEFINE v_tpo_notificacion     SMALLINT
   DEFINE v_indicador            SMALLINT

   DEFINE v_rec_sumario          RECORD
      tpo_registro                  CHAR(2),   
      tot_registros                 INTEGER
   END RECORD

   DEFINE v_cadena_registro      STRING
   DEFINE v_consecutivo          SMALLINT
   DEFINE v_bandera              SMALLINT
   DEFINE f_genera               DATE
   
   LET f_genera = TODAY

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "afi"

   LET v_consecutivo    = 0 
   LET v_nombre_archivo = v_ruta_envio CLIPPED, "/DATCONTRECH",
                          f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".dconrch"

   WHILE (os.Path.exists(v_nombre_archivo))
      LET v_consecutivo = v_consecutivo + 1
      LET v_nombre_archivo = v_ruta_envio CLIPPED, "/DATCONTRECH",
                          f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".dconrch"
   END WHILE

   LET v_archivo = "DATCONTRECH",
                    f_genera USING "YYYYMMDD", v_consecutivo USING "&&&",".dconrch"

   UPDATE bat_ctr_operacion
      SET nom_archivo = v_archivo
    WHERE pid = g_pid
      AND proceso_cod = g_proceso_cod
      AND opera_cod = g_opera_cod

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nombre_archivo,"wb")

   DECLARE cur_datos CURSOR FOR SELECT '02',
                                       nss,
                                       tpo_notificacion,
                                       indicador,
                                       fuente,
                                       cod_excepcion
                                  FROM afi_rch_ind_not

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

   ---DETALLE
   FOREACH cur_datos INTO v_rec_detalle.tpo_registro,
                          v_rec_detalle.nss,
                          v_tpo_notificacion,
                          v_indicador,
                          v_rec_detalle.fuente,
                          v_rec_detalle.estado

      INITIALIZE v_rec_detalle.ind_sms, 
                 v_rec_detalle.ind_correo,
                 v_rec_detalle.bloqueo_int_sms,
                 v_rec_detalle.bloqueo_int_correo TO NULL

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
      LET v_cadena_registro = v_rec_detalle.tpo_registro,
                              v_rec_detalle.nss,
                              v_rec_detalle.ind_sms USING "#",
                              v_rec_detalle.ind_correo USING "#",
                              v_rec_detalle.bloqueo_int_sms USING "#",
                              v_rec_detalle.bloqueo_int_correo USING "#",
                              v_rec_detalle.fuente USING "&&&&",
                              v_rec_detalle.estado USING "&&&&"
      CALL ch.writeLine(v_cadena_registro)
   END FOREACH

   ----SUMARIO
   LET  v_rec_sumario.tpo_registro = '09'

   SELECT COUNT(*)
     INTO v_rec_sumario.tot_registros
     FROM afi_rch_ind_not

   LET v_cadena_registro = v_rec_sumario.tpo_registro,
                           v_rec_sumario.tot_registros USING "&&&&&&&&&&&&"
   CALL ch.writeLine(v_cadena_registro)
   CALL ch.close()
   
   RETURN v_bandera
END FUNCTION
