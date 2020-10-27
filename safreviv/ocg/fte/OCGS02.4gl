--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>OCG                                           #
#Programa          =>OCGS02                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo de tramitados de 43BIS             #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>16 febrero 2016                               #
####################################################################


DATABASE safre_viv

GLOBALS

   DEFINE p_usuario           LIKE seg_usuario.usuario            -- nombre del usuario
   DEFINE p_pid               LIKE bat_ctr_proceso.pid            -- pid
   DEFINE p_proceso_cod       LIKE cat_proceso.proceso_cod        -- codigo del proceso
   DEFINE p_opera_cod         LIKE cat_operacion.opera_cod        -- codigo de la operacion de la etapa
   DEFINE p_f_inicial         CHAR(10)
   DEFINE p_f_final           CHAR(10)
   DEFINE v_ruta_envio        LIKE seg_modulo.ruta_envio
   DEFINE v_nom_arh           STRING
   DEFINE v_folio             CHAR(10)
   DEFINE ch                  base.Channel
   DEFINE v_detalle           STRING
   DEFINE a                   SMALLINT
   DEFINE v_cta_t             INTEGER
   DEFINE v_cta_f             INTEGER
   DEFINE bnd_datos1          SMALLINT
   DEFINE bnd_datos2          SMALLINT
   DEFINE v_nom_arh_resp      STRING
   DEFINE v_fecha             STRING
   DEFINE v_s_comando         STRING

   DEFINE arr_tramitados DYNAMIC ARRAY OF RECORD
      situacion          CHAR(3),
      nss                CHAR(11),
      ent_financiera     CHAR(3),
      tpo_credito        CHAR(1),
      f_proceso          DATE,
      f_respuesta        DATE,
      f_carga            DATE,
      f_liquidacion      DATE,
      f_vigencia         DATE
   END RECORD

END GLOBALS

MAIN

DEFINE v_msj             STRING

   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   LET p_pid             = ARG_VAL(2)
   LET p_proceso_cod     = ARG_VAL(3)
   LET p_opera_cod       = ARG_VAL(4)
   LET p_f_inicial       = ARG_VAL(5)
   LET p_f_final         = ARG_VAL(6)

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

    LET v_fecha = TODAY USING "ddmmyyyy"

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/Solicitudes_tramite",".tmef"
   LET v_nom_arh_resp = v_ruta_envio CLIPPED ,"/Solicitudes_tramite",v_fecha,".tmef"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".ocgS102.log")

   DISPLAY "=INICIA ocgS101="
   DISPLAY " USUARIO           : ",p_usuario
   DISPLAY " PID               : ",p_pid
   DISPLAY " Fecha inicial     : ",p_f_inicial
   DISPLAY " Fecha final       : ",p_f_final

   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_tramitados()

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio

   LET v_msj = "Archivo generado de forma correcta en : "
   DISPLAY ""
   DISPLAY " ",v_msj," "
   DISPLAY v_nom_arh
   DISPLAY ""
   DISPLAY "PROCESO EJECUTADO CORRECTAMENTE"
   DISPLAY ""
   DISPLAY "=FINALIZA ARCHIVO DE SAIDA TRAMITADOS 43BIS="

END MAIN

FUNCTION fn_archivo_tramitados()

   DEFINE v_qry_tramite       STRING
   DEFINE v_qry_formalizacion STRING

   LET p_f_inicial = p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
   LET p_f_final = p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]

 --  DISPLAY "fecha inicial ocgs :",p_f_inicial
 --  DISPLAY "fecha final   ocgs :",p_f_final

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh_resp,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_tramitados.clear()
   -- cadena para archivo de salida con detalle para SP001 tramite
   LET v_qry_tramite = "SELECT ocg.situacion,
                               det.nss,
                               ocg.cve_ent_financiera,
                               ocg.tpo_credito,
                               det.f_proceso,
                               mig.f_respuesta,
                               mig.f_carga,
                               mig.f_liquida_cofi,
                               ocg.f_vigencia
                          FROM ocg_tramite ocg, ocg_detalle det,ocg_fecha_mig mig
                         WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                           AND ocg.id_ocg_tramite = mig.id_ocg_referencia
                           AND mig.subproceso = 1
                           AND ocg.id_ocg_tramite not in (select f.id_ocg_tramite 
                                                            from ocg_formalizacion f 
                                                           where f.situacion between 50 and 80
                                                             and f.id_ocg_tramite is not null)
                           AND det.f_proceso between ","'",p_f_inicial,"'"," and ","'",p_f_final,"'", "
                           AND ocg.situacion in (50)
                      order by ocg.cve_ent_financiera asc"

LET a = 1
LET bnd_datos1 = 0
LET bnd_datos2 = 0
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_t
     FROM ocg_tramite ocg, ocg_detalle det
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.id_ocg_tramite not in (SELECT f.id_ocg_tramite 
                                       FROM ocg_formalizacion f
                                      WHERE f.situacion BETWEEN 50 and 80
                                        AND f.id_ocg_tramite IS not NULL)
      AND det.f_proceso BETWEEN p_f_inicial AND p_f_final
      AND ocg.situacion in (25,30,50)

   --DISPLAY "cuenta tramite ",v_cta_t

   IF v_cta_t > 0 THEN

      -- se llena arreglo con datos de tabla tramite
      PREPARE prp_tramite FROM v_qry_tramite
      DECLARE cur_tramite CURSOR FOR prp_tramite

      --LET a = 1
      FOREACH cur_tramite INTO arr_tramitados[a].*

         CALL fn_escribe()

         LET a = a+1
      END FOREACH

      IF arr_tramitados[a].nss IS NULL THEN
         CALL arr_tramitados.deleteElement(arr_tramitados.getLength())
      END IF
   ELSE
      LET bnd_datos1 = 1
   END IF

IF (bnd_datos1 = 1) THEN --AND (bnd_datos2 = 1) THEN
   CALL fn_sin_datos()
END IF
   CALL ch.close()

   LET v_s_comando = "sed 's/$/\r/' ",v_nom_arh_resp," > ",v_nom_arh
   RUN v_s_comando

END FUNCTION 

FUNCTION fn_escribe()

   DEFINE v_detalle           STRING

   LET v_detalle = 
   "001"                                          ,"|",
   arr_tramitados[a].situacion      CLIPPED       ,"|",
   arr_tramitados[a].nss            CLIPPED       ,"|",
   arr_tramitados[a].ent_financiera USING "&&&"   ,"|",
   arr_tramitados[a].tpo_credito    CLIPPED       ,"|",
   arr_tramitados[a].f_proceso    USING "yyyymmdd","|",
   arr_tramitados[a].f_respuesta  USING "yyyymmdd","|",
   arr_tramitados[a].f_carga      USING "yyyymmdd","|",
   arr_tramitados[a].f_liquidacion USING "yyyymmdd","|",
   arr_tramitados[a].f_vigencia   USING "yyyymmdd"
   CALL ch.writeLine([v_detalle])

END FUNCTION

FUNCTION fn_sin_datos()

   LET v_detalle = "NO SE ENCONTRARON REGISTROS CON LOS PARÁMETROS DE FECHA INGRESADOS"

   CALL ch.writeLine([v_detalle])
END FUNCTION