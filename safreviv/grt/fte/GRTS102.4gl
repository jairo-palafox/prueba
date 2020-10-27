--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTS102                                       #
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
   DEFINE bnd_datos           SMALLINT

   DEFINE arr_tramitados DYNAMIC ARRAY OF RECORD
      cve_ent_financiera CHAR(3),
      nss                CHAR(11),
      paterno            CHAR(40),
      materno            CHAR(40),
      nombre             CHAR(40),
      tpo_credito        CHAR(3)
   END RECORD

END GLOBALS

MAIN

DEFINE v_msj             STRING

--CLOSE WINDOW SCREEN

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
    WHERE modulo_cod = 'grt'

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/Solicitudes_tramite",".Sal"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".GRTS102.log")

   DISPLAY "=INICIA GRTS101="
   DISPLAY " USUARIO           : ",p_usuario
   DISPLAY " PID               : ",p_pid
   DISPLAY " Fecha inicial     : ",p_f_inicial
   DISPLAY " Fecha final       : ",p_f_final

   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_tramitados()
  -- EXIT MENU
   --ON ACTION CANCEL
   --EXIT MENU
--END MENU
--CLOSE WINDOW salida

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio

  -- LET v_cp_arch = v_ruta_envio CLIPPED ,"/Tramitados",TODAY USING "DDMMYYYY",v_folio CLIPPED,".Sal"
   --LET v_cmd = "cp ",v_nom_arh," ",v_cp_arch
   --RUN v_cmd

   --LET v_comando = " sed 's/$/\r/' ", v_cp_arch CLIPPED," > ",v_nom_arh CLIPPED
   --RUN v_comando

   --LET v_archivo = "Tamitados",TODAY USING "DDMMYYYY",".Sal"


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

{   IF (p_f_inicial = "") AND
      (p_f_final = "" )THEN

      LET p_f_inicial = "01",MONTH(today),YEAR(TODAY)

      LET p_f_final = TODAY

      DISPLAY "condición : fechas nulas"
   END IF

   IF (p_f_inicial IS NULL) AND
      (p_f_final IS NOT NULL )THEN

      LET p_f_inicial = "01",MONTH(today),YEAR(TODAY)

      LET p_f_final = p_f_final

      DISPLAY "condición : fechas nulas"
   END IF

   IF (p_f_inicial IS NOT NULL) AND
      (p_f_final IS NULL )THEN

      LET p_f_inicial = p_f_inicial

      LET p_f_final = TODAY USING "dd-mm-yyyy"

      DISPLAY "condición : fechas nulas"
   END IF}


   LET p_f_inicial = p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
   LET p_f_final = p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]

   DISPLAY "fecha inicial grts :",p_f_inicial
   DISPLAY "fecha final   grts :",p_f_final

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_tramitados.clear()
   -- cadena para archivo de salida con detalle para SP001 tramite
   LET v_qry_tramite = "SELECT grt.cve_ent_financiera,
                               det.nss,
                               grt.ap_paterno,
                               grt.ap_materno,
                               grt.nombre,
                               grt.tpo_credito
                          FROM grt_tramite grt, grt_detalle det
                         WHERE grt.id_grt_detalle = det.id_grt_detalle
                           AND grt.id_grt_tramite not in (select id_grt_tramite from grt_formalizacion)
                           AND det.f_proceso between ","'",p_f_inicial,"'"," and ","'",p_f_final,"'", "
                      order by grt.cve_ent_financiera asc"
                      
   --cadena para archivo de salida con detalle para SP002 fromalización
   LET v_qry_formalizacion = "SELECT grt.cve_ent_financiera,
                                     det.nss,
                                     t.ap_paterno,
                                     t.ap_materno,
                                     t.nombre,
                                     t.tpo_credito
                                FROM grt_formalizacion grt,
                                     grt_detalle det,
                                     grt_tramite t
                               WHERE grt.id_grt_tramite = t.id_grt_tramite
                                 and grt.id_grt_detalle = det.id_grt_detalle
                                 AND grt.estado in (20,40,50)
                                 AND det.f_proceso between ","'",p_f_inicial,"'"," and ","'",p_f_final,"'", "
                            order by grt.cve_ent_financiera asc"

--******************************************************************************
LET a = 1
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_t
     FROM grt_tramite grt, grt_detalle det
    WHERE grt.id_grt_detalle = det.id_grt_detalle
      AND grt.id_grt_tramite not in (select id_grt_tramite from grt_formalizacion)
      AND det.f_proceso BETWEEN p_f_inicial AND p_f_final

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
      LET bnd_datos = 1
   END IF
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_f
     FROM grt_formalizacion grt,
          grt_detalle det,
          grt_tramite t
    WHERE grt.id_grt_tramite = t.id_grt_tramite
      and grt.id_grt_detalle = det.id_grt_detalle
      AND grt.estado in (20,40,50)
      AND det.f_proceso BETWEEN p_f_inicial AND p_f_final

      --DISPLAY "cuenta formalización",v_cta_f
   IF v_cta_f > 0 THEN
   -- se llena arreglo con datos de tabla fromalización
   --LET a = a +1

   PREPARE prp_formalizacion FROM v_qry_formalizacion
   DECLARE cur_formalizacion CURSOR FOR prp_formalizacion
   
   FOREACH cur_formalizacion INTO arr_tramitados[a].*

      CALL fn_escribe()

      LET a = a+1
   END FOREACH

   IF arr_tramitados[a].nss IS NULL THEN
      CALL arr_tramitados.deleteElement(arr_tramitados.getLength())
   END IF
   ELSE
      LET bnd_datos = 1
   END IF
--******************************************************************************

IF bnd_datos = 1 THEN
   CALL fn_sin_datos()
END IF
   CALL ch.close()

END FUNCTION 

FUNCTION fn_escribe()

   DEFINE v_detalle           STRING

   LET v_detalle = 
   arr_tramitados[a].cve_ent_financiera  USING "&&&","|",
   arr_tramitados[a].nss CLIPPED                    ,"|",
   arr_tramitados[a].paterno CLIPPED                ," ",
   arr_tramitados[a].materno CLIPPED                ," ",
   arr_tramitados[a].nombre CLIPPED                 ,"|",
   arr_tramitados[a].tpo_credito CLIPPED

   CALL ch.writeLine([v_detalle])

END FUNCTION

FUNCTION fn_sin_datos()

   LET v_detalle = "NO SE ENCONTRARON REGISTROS CON LOS PARÁMETROS DE FECHA INGRESADOS"

   CALL ch.writeLine([v_detalle])
END FUNCTION