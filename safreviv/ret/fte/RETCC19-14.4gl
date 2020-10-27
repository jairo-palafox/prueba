--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE AMORTIZACIONES EXCEDENTES                         #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETCC19-14                                              #
#OBJETIVO          => Programa de Apoyo para desmarcar las cuentas de         #
#                     solicitudes de retiro de Amortizaciones Excedentes      #
#REQUERIMIENTO     => CCARSAFRE19-14                                          #
###############################################################################

 
DATABASE safre_viv 
#------------------------------------------------------------------------------
# MAIN
#------------------------------------------------------------------------------
MAIN
DEFINE v_contador       INTEGER
DEFINE v_resultado      SMALLINT
DEFINE v_comando        STRING
DEFINE v_nombre_archivo STRING

   LET v_nombre_archivo = ARG_VAL(1)
   
   DISPLAY "*************************************************************************************************"
   DISPLAY "**"
   DISPLAY "**"
   DISPLAY "**"
   DISPLAY CURRENT YEAR TO MINUTE, " Inicia proceso de desmarca de cuentas  "
   DISPLAY "**"
   DISPLAY "**"
   CALL fn_crea_temporal()
   DISPLAY CURRENT YEAR TO MINUTE, " Tablas temporales creadas "
   -- se asigna proceso y operacion
   CALL fn_carga_archivo(v_nombre_archivo)

   SELECT COUNT(*) 
   INTO   v_contador
   FROM   tmp_amort_exced_solic

   IF v_contador > 0 THEN 
      DISPLAY CURRENT YEAR TO MINUTE, " Resumen del Archivo a Procesar: "
      DISPLAY " "
      DISPLAY CURRENT YEAR TO MINUTE, " Se cargaron a la temporal ", v_contador  USING "<<<<<<<<<<", " registros del Archivo ", v_nombre_archivo CLIPPED
      DISPLAY " "
      SELECT COUNT(*) 
      INTO   v_contador
      FROM   tmp_amort_exced_solic
      WHERE  estado_solicitud = 8
      DISPLAY CURRENT YEAR TO MINUTE, " Registros con Estado Solicitud 8-Precapturada ", v_contador  USING "<<<<<<<<<<"
      DISPLAY " "   
      SELECT COUNT(*) 
      INTO   v_contador
      FROM   tmp_amort_exced_solic
      WHERE  estado_solicitud = 10
      DISPLAY CURRENT YEAR TO MINUTE, " Registros con Estado Solicitud 10-Crecapturada ", v_contador  USING "<<<<<<<<<<"
      DISPLAY " "
      DISPLAY " "
      DISPLAY CURRENT YEAR TO MINUTE, " Validando Solicitudes "
      DISPLAY " "
      SELECT COUNT(*) 
      INTO   v_contador
      FROM   tmp_amort_exced_solic a,
             ret_solicitud_generico b
      WHERE  a.nss = b.nss
      AND    a.caso_adai = b.caso_adai
      AND    a.estado_solicitud = b.estado_solicitud
      AND    b.modalidad_retiro = 9
      DISPLAY CURRENT YEAR TO MINUTE, " Solicitudes Encontradas para desmarcar :", v_contador  USING "<<<<<<<<<<"
      DISPLAY " "
      DISPLAY CURRENT YEAR TO MINUTE, " Las solicitudes no encontradas se reportarán en el archivo :", v_nombre_archivo CLIPPED, "_RECHAZADOS.txt"
      DISPLAY " "
      DISPLAY CURRENT YEAR TO MINUTE, " Inicia desmarca "
      DISPLAY " "
      CALL fn_procesa_informacion()
      DISPLAY " "
      DISPLAY " "
      DISPLAY CURRENT YEAR TO MINUTE, " Descargando rechazados "
      DISPLAY " "
      CALL fn_descarga_temporales(v_nombre_archivo)
      DISPLAY CURRENT YEAR TO MINUTE, " Termina proceso "
   ELSE 
      DISPLAY CURRENT YEAR TO MINUTE, " No existe información por procesar "
   END IF 
   
END MAIN 
#------------------------------------------------------------------------------
# fn_crea_temporal
#------------------------------------------------------------------------------
FUNCTION fn_crea_temporal()
DEFINE v_query  STRING

   DROP TABLE IF EXISTS tmp_amort_exced_solic;

   CREATE TEMP TABLE tmp_amort_exced_solic (
      nss               CHAR(11),
      caso_adai         CHAR(10),
      estado_solicitud  SMALLINT);

   CREATE INDEX idxtmp_amort_exced_solic   ON tmp_amort_exced_solic (nss); 

   DROP TABLE IF EXISTS tmp_amort_exced_solic_rch;

   CREATE TEMP TABLE tmp_amort_exced_solic_rch (
      nss               CHAR(11),
      caso_adai         CHAR(10),
      estado_solicitud  SMALLINT);

   DROP TABLE IF EXISTS tmp_amort_exced_solic_aceptadas;

   CREATE TEMP TABLE tmp_amort_exced_solic_aceptadas (
      id_solicitud      DECIMAL(10,0));

   LET v_query="EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
   -- se ejecuta
   PREPARE stm_desmarcaje FROM  v_query
   
END FUNCTION
#------------------------------------------------------------------------------
# fn_carga_archivo
#------------------------------------------------------------------------------
FUNCTION fn_carga_archivo(p_nombre_archivo)
DEFINE v_sql            STRING 
DEFINE v_archivo        CHAR(100)
DEFINE p_nombre_archivo STRING

   LET v_archivo = "/safreviv_req/CCARSAFRE19-14/", p_nombre_archivo CLIPPED

   LOAD FROM v_archivo
   INSERT INTO tmp_amort_exced_solic
   
END FUNCTION
#------------------------------------------------------------------------------
# fn_procesa_informacion
#------------------------------------------------------------------------------
FUNCTION fn_procesa_informacion()       
DEFINE v_resultado           INTEGER -- recibe el resultado de la ejecucion del servicio 
DEFINE v_contador            INTEGER 
DEFINE v_cant_solicitudes    SMALLINT

DEFINE rec_solicitud RECORD 
         nss               CHAR(11),
         caso_crm          CHAR(10),
         estado_solicitud  SMALLINT
END RECORD 

DEFINE v_marca_entra        SMALLINT 
DEFINE v_proceso_cod        SMALLINT
DEFINE v_folio              DECIMAL(9,0)
DEFINE v_estado_marca       SMALLINT
DEFINE v_codigo_rechazo     SMALLINT
DEFINE v_marca_causa        SMALLINT
DEFINE v_fecha_causa        DATE
DEFINE v_usuario            CHAR(20)
DEFINE v_cod_rechazo        SMALLINT 
DEFINE v_id_derechohabiente DECIMAL(10,0)
DEFINE v_id_solicitud       DECIMAL(10,0)
DEFINE v_respuesta_marcaje  SMALLINT
DEFINE v_procesadas         INTEGER
DEFINE v_inexistentes       INTEGER
DEFINE v_no_marcadas        INTEGER
DEFINE v_desmarcadas        INTEGER
DEFINE v_no_desmarcadas     INTEGER

    LET v_marca_entra        = 810
    LET v_proceso_cod        = 1530
    LET v_folio              = "0"
    LET v_estado_marca       = "0"
    LET v_codigo_rechazo     = "0"
    LET v_marca_causa        = "0"
    LET v_fecha_causa        = NULL
    LET v_usuario            = "OPSISSACI"
    LET v_cod_rechazo        = 80
    LET v_id_derechohabiente = 0
    LET v_id_solicitud       = 0
    LET v_procesadas         = 0
    LET v_inexistentes       = 0
    LET v_no_marcadas        = 0
    LET v_desmarcadas        = 0
    LET v_no_desmarcadas     = 0

   DECLARE cur_procesa_solicitud CURSOR FOR
   SELECT * 
   FROM   tmp_amort_exced_solic

   -- se leen las solicitudes de estos casos
   FOREACH cur_procesa_solicitud INTO rec_solicitud.*
      LET v_contador = v_contador + 1
      LET v_cant_solicitudes = 0

      SELECT COUNT(*)
      INTO   v_cant_solicitudes
      FROM   ret_solicitud_generico
      WHERE  nss              = rec_solicitud.nss
      AND    caso_adai        = rec_solicitud.caso_crm
      AND    estado_solicitud IN (8,10)
      AND    modalidad_retiro = 9

      IF v_cant_solicitudes = 1 THEN 
         -- Se procede con la desmarca
         SELECT id_derechohabiente, id_solicitud
         INTO   v_id_derechohabiente, v_id_solicitud
         FROM   ret_solicitud_generico
         WHERE  nss              = rec_solicitud.nss
         AND    caso_adai        = rec_solicitud.caso_crm
         AND    estado_solicitud IN (8,10)
         AND    modalidad_retiro = 9
         -- Se busca la marca
         SELECT COUNT(*) 
         INTO   v_cant_solicitudes
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    n_referencia       = v_id_solicitud
         AND    marca              = v_marca_entra
         IF v_cant_solicitudes = 1 THEN 
            -- se prepara la ejecucion de la desmarca
            EXECUTE stm_desmarcaje USING v_id_derechohabiente, 
                                         v_marca_entra       ,
                                         v_id_solicitud      ,
                                         v_estado_marca      ,
                                         v_marca_causa       ,
                                         v_usuario           ,
                                         v_proceso_cod 
               INTO v_respuesta_marcaje
            IF v_respuesta_marcaje = 0 THEN 
               LET v_desmarcadas = v_desmarcadas + 1
               CALL fn_actualiza_solicitud(v_id_solicitud);
            ELSE
               LET v_no_desmarcadas = v_no_desmarcadas + 1
               CALL fn_inserta_rechazo(rec_solicitud.*)
            END IF
         ELSE
            LET v_no_marcadas = v_no_marcadas + 1
            CALL fn_inserta_rechazo(rec_solicitud.*)
         END IF 
      ELSE
         LET v_inexistentes = v_inexistentes + 1
         CALL fn_inserta_rechazo(rec_solicitud.*)
      END IF 
   END FOREACH

   DISPLAY " "
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Concluye desmarca :"
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Solicitudes Procesadas    :", v_contador        USING "<<<<<<<<<<"
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Desmarcadas exitosamente  :", v_desmarcadas     USING "<<<<<<<<<<"
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Inexistentes              :", v_inexistentes    USING "<<<<<<<<<<"
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Sin marca                 :", v_no_marcadas     USING "<<<<<<<<<<"
   DISPLAY " "
   DISPLAY CURRENT YEAR TO MINUTE, " Con problema al desmarcar :", v_no_desmarcadas  USING "<<<<<<<<<<"
   DISPLAY " "
   
END FUNCTION 
#------------------------------------------------------------------------------
# fn_inserta_rechazo
#------------------------------------------------------------------------------
FUNCTION fn_inserta_rechazo(p_rec_solicitud)
   DEFINE p_rec_solicitud RECORD 
         nss                CHAR(11),
         caso_crm           CHAR(10),
         estado_solicitud   SMALLINT
   END RECORD 

   INSERT INTO tmp_amort_exced_solic_rch VALUES (p_rec_solicitud.nss,p_rec_solicitud.caso_crm,p_rec_solicitud.estado_solicitud)

END FUNCTION 
#------------------------------------------------------------------------------
# fn_actualiza_solicitud
#------------------------------------------------------------------------------
FUNCTION fn_actualiza_solicitud(p_id_solicitud)
   DEFINE p_id_solicitud        DECIMAL(9,0)

   UPDATE ret_solicitud_generico
   SET    estado_solicitud = 100,
          cod_rechazo = 80
   WHERE  id_solicitud = p_id_solicitud
   AND    estado_solicitud IN (8,10);
   
   UPDATE ret_amort_excedente
   SET    estado_solicitud = 100,
          cod_rechazo = 80
   WHERE  id_solicitud = p_id_solicitud
   AND    estado_solicitud IN (8,10);

   INSERT INTO tmp_amort_exced_solic_aceptadas VALUES (p_id_solicitud);
END FUNCTION
#------------------------------------------------------------------------------
# fn_descarga_temporales
#------------------------------------------------------------------------------
FUNCTION fn_descarga_temporales (p_nombre_archivo)
   DEFINE v_resultado         SMALLINT
   DEFINE v_archivo_salida    CHAR(100)
   DEFINE v_fecha_hora_genera CHAR (14)
   DEFINE v_contador          INTEGER 
   DEFINE p_nombre_archivo    STRING
   
   SELECT to_char(current year to day, "%Y%m%d") 
   INTO   v_fecha_hora_genera
   FROM   systables 
   WHERE  tabid = 1

   LET v_contador = 0
   -- cuanta los registros a bajar
   SELECT COUNT(*) 
   INTO   v_contador
   FROM   tmp_amort_exced_solic_rch
   IF v_contador > 0 THEN 
      LET v_archivo_salida = "/safreviv_req/CCARSAFRE19-14/", p_nombre_archivo CLIPPED, "_RECHAZADOS_", v_fecha_hora_genera
      DISPLAY " " 
      DISPLAY " " 
      DISPLAY "Bajando los rechazados"
      DISPLAY " " 
      DISPLAY " Se genera el archivo ", v_archivo_salida CLIPPED , " de rechazados con ", v_contador USING "<<<<<<<<<<", " registros."
      UNLOAD TO v_archivo_salida
      SELECT * 
      FROM   tmp_amort_exced_solic_rch;
      DISPLAY " " 
      DISPLAY "Rechazados generado :",  v_archivo_salida
      DISPLAY " " 
      
   END IF 

   LET v_archivo_salida = "/safreviv_req/CCARSAFRE19-14/", p_nombre_archivo CLIPPED, "_ACEPTADOS.unl"
   UNLOAD TO v_archivo_salida
   SELECT * 
   FROM   tmp_amort_exced_solic_aceptadas;
   
END FUNCTION
