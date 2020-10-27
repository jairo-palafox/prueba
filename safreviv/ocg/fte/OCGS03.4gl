--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>OCG                                           #
#Programa          =>OCGS03                                        #
#Objetivo          =>Programa que ejecuta el proceso de generación #
#                    de archivo de formalizados de 43BIS           #
#Autor             =>José Eduardo Ventura                          #
#Fecha inicio      =>18 febrero 2016                               #
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
   DEFINE v_qry_acreditado    STRING
   DEFINE v_qry_liquidado     STRING
   DEFINE v_dia               STRING
   DEFINE v_nom_arh_resp      STRING
   DEFINE v_s_comando         STRING


   DEFINE arr_formalizados DYNAMIC ARRAY OF RECORD
      id_ocg_formalizacion DECIMAL(9,0),
      nss                CHAR(11),
      rfc                CHAR(13),
      curp               CHAR(18),
      num_ctr_int        CHAR(18),
      tpo_credito        CHAR(1),
      situacion          CHAR(3),
      sub_proceso        CHAR(3),
      ent_financiera     CHAR(3),
      producto           CHAR(1),
      año_ejercicio      CHAR(4),
      f_reg_carta        DATE,
       f_liquida_credito     DATE,
          f_formalizacion       DATE,
          f_solic_marca_prcr    DATE,
          f_conf_marca_prcr     DATE,
          f_solic_desmarca_prcr DATE,
          f_conf_desmarca_prcr  DATE,
          id_causa_liquida DECIMAL(2)
   END RECORD

   DEFINE arr_acreditado DYNAMIC ARRAY OF RECORD
          f_liquida_credito     DATE,
          f_formalizacion       DATE,
          f_solic_marca_prcr    DATE,
          f_conf_marca_prcr     DATE,
          f_solic_desmarca_prcr DATE,
          f_conf_desmarca_prcr  DATE 
   END RECORD

   DEFINE arr_liquidado DYNAMIC ARRAY OF RECORD
          id_causa_liquida DECIMAL(2,0)
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

   LET v_dia = TODAY USING "ddmmyyyy"

   LET v_nom_arh = v_ruta_envio CLIPPED ,"/acresit",".crst"
   LET v_nom_arh_resp = v_ruta_envio CLIPPED ,"/acresit",v_dia,".crst"

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".ocgS103.log")

   DISPLAY "=INICIA ocgS103="
   DISPLAY " USUARIO           : ",p_usuario
   DISPLAY " PID               : ",p_pid
   DISPLAY " Fecha inicial     : ",p_f_inicial
   DISPLAY " Fecha final       : ",p_f_final

   -- se ejecuta la función que genera el archivo de salida de liquidación
   CALL fn_archivo_formalizados()

   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio

   LET v_msj = "Archivo generado de forma correcta en : "
   DISPLAY ""
   DISPLAY " ",v_msj," "
   DISPLAY v_nom_arh
   DISPLAY ""
   DISPLAY "PROCESO EJECUTADO CORRECTAMENTE"
   DISPLAY ""
   DISPLAY "=FINALIZA ARCHIVO DE SAIDA FORMALIZADOS 43BIS="

END MAIN

FUNCTION fn_archivo_formalizados()

   DEFINE v_qry_formalizacion STRING

   LET p_f_inicial = p_f_inicial[4,5],"/",p_f_inicial[1,2],"/",p_f_inicial[7,10]
   LET p_f_final = p_f_final[4,5],"/",p_f_final[1,2],"/",p_f_final[7,10]

   --DISPLAY "fecha inicial ocgs :",p_f_inicial
  -- DISPLAY "fecha final   ocgs :",p_f_final

   LET ch = base.Channel.create()
   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

   CALL arr_formalizados.clear()

   --cadena para archivo de salida con detalle para SP002 fromalización
   LET v_qry_formalizacion = "SELECT ocg.id_ocg_formalizacion,
                                     det.nss,
                                     ocg.rfc,
                                     ocg.curp,
                                     ocg.num_ctr_int_ef,
                                     ocg.tpo_credito,
                                     ocg.situacion,
                                     lpad(det.subproceso,3,0),
                                     lpad(ocg.cve_ent_financiera,3,0),
                                     ocg.tpo_credito,
                                     YEAR(ocg.f_saldo),
                                     ocg.f_registro_carta,
                                        acre.f_liquida_credito,                                         
                                        acre.f_formalizacion,      
                                        acre.f_solic_marca_prcr,   
                                        acre.f_conf_marca_prcr,    
                                        acre.f_solic_desmarca_prcr,
                                        acre.f_conf_desmarca_prcr,
                                        liq.id_causa_liquida
                                FROM ocg_formalizacion ocg,ocg_acreditado acre, OUTER ocg_liquidacion liq,
                                     ocg_detalle det
                               WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
                                 AND ocg.id_ocg_formalizacion = acre.id_ocg_formalizacion
                                 AND ocg.id_ocg_formalizacion = liq.id_ocg_formalizacion
                                 AND ocg.situacion in (55,60,70,80,140,150,160)
                                 AND det.f_proceso between ","'",p_f_inicial,"'"," and ","'",p_f_final,"'", "
                            order by ocg.cve_ent_financiera asc"

--******************************************************************************
LET a = 1
--******************************************************************************
   SELECT COUNT(*)
     INTO v_cta_f
     FROM ocg_formalizacion ocg,
          ocg_detalle det
    WHERE ocg.id_ocg_detalle = det.id_ocg_detalle
      AND ocg.situacion in (55,60,70,80,140,150,160)
      AND det.f_proceso BETWEEN p_f_inicial AND p_f_final

      --DISPLAY "cuenta formalización",v_cta_f
   IF v_cta_f > 0 THEN
   -- se llena arreglo con datos de tabla fromalización
   --LET a = a +1

   PREPARE prp_formalizacion FROM v_qry_formalizacion
   DECLARE cur_formalizacion CURSOR FOR prp_formalizacion
   
   FOREACH cur_formalizacion INTO arr_formalizados[a].*
{
         LET v_qry_acreditado = "SELECT f_liquida_credito,                                         
                                        f_formalizacion,      
                                        f_solic_marca_prcr,   
                                        f_conf_marca_prcr,    
                                        f_solic_desmarca_prcr,
                                        f_conf_desmarca_prcr
                                   FROM ocg_acreditado
                                  WHERE id_ocg_formalizacion = ",arr_formalizados[a].id_ocg_formalizacion,
                                    "AND situacion = ",arr_formalizados[a].situacion
   PREPARE prp_accreditado FROM v_qry_acreditado
   DECLARE cur_acreditado CURSOR FOR prp_accreditado
   
   FOREACH cur_acreditado INTO arr_acreditado[a].*
   END FOREACH

   CALL arr_acreditado.deleteElement(a+1)

         LET v_qry_liquidado = "SELECT id_causa_liquida
                                  FROM ocg_liquidacion
                                 WHERE id_ocg_formalizacion = ",arr_formalizados[a].id_ocg_formalizacion,
                                  "AND situacion = ",arr_formalizados[a].situacion

   PREPARE prp_liquidado FROM v_qry_liquidado
   DECLARE cur_liquidado CURSOR FOR prp_liquidado
   
   FOREACH cur_liquidado INTO arr_liquidado[a].*
   END FOREACH

      IF arr_formalizados[a].nss IS NULL THEN
      CALL arr_formalizados.deleteElement(arr_formalizados.getLength())
   END IF
}
         CALL fn_escribe(a)
      LET a = a+1
   END FOREACH

   IF arr_formalizados[a].nss IS NULL THEN
      CALL arr_formalizados.deleteElement(arr_formalizados.getLength())
   END IF
   ELSE
      LET bnd_datos = 1
   END IF
--******************************************************************************

IF bnd_datos = 1 THEN
   CALL fn_sin_datos()
END IF
   CALL ch.close()

      LET v_s_comando = "mv ",v_nom_arh," ",v_nom_arh_resp
      --DISPLAY v_s_comando
      RUN v_s_comando

      LET v_s_comando = "sed 's/$/\r/' ",v_nom_arh_resp," > ",v_nom_arh
      RUN v_s_comando

END FUNCTION 

FUNCTION fn_escribe(a)

   DEFINE v_detalle           STRING
   DEFINE a INTEGER
   DEFINE v_liquida_credito     CHAR(8)
   DEFINE v_solic_marca_prcr    CHAR(8)
   DEFINE v_conf_marca_prcr     CHAR(8)
   DEFINE v_solic_desmarca_prcr CHAR(8)
   DEFINE v_conf_desmarca_prcr  CHAR(8)
   DEFINE v_reg_carta           CHAR(8)
   DEFINE v_formalizacion       CHAR(8)

   LET v_liquida_credito     = arr_formalizados[a].f_liquida_credito     USING "yyyymmdd"
   LET v_solic_marca_prcr    = arr_formalizados[a].f_solic_marca_prcr    USING "yyyymmdd"
   LET v_conf_marca_prcr     = arr_formalizados[a].f_conf_marca_prcr     USING "yyyymmdd"
   LET v_solic_desmarca_prcr = arr_formalizados[a].f_solic_desmarca_prcr USING "yyyymmdd"
   LET v_conf_desmarca_prcr  = arr_formalizados[a].f_conf_desmarca_prcr  USING "yyyymmdd"
   LET v_reg_carta           = arr_formalizados[a].f_reg_carta           USING "yyyymmdd"
   LET v_formalizacion       = arr_formalizados[a].f_formalizacion       USING "yyyymmdd"
   
   LET v_detalle = 
   arr_formalizados[a].nss                CLIPPED,"|",
   arr_formalizados[a].rfc                CLIPPED,"|",
   arr_formalizados[a].curp               CLIPPED,"|",
   arr_formalizados[a].num_ctr_int        CLIPPED,"|",
   arr_formalizados[a].tpo_credito        CLIPPED,"|",
   arr_formalizados[a].situacion          CLIPPED,"|",
   arr_formalizados[a].sub_proceso        CLIPPED,"|",
   arr_formalizados[a].ent_financiera     CLIPPED,"|",
   arr_formalizados[a].producto           CLIPPED,"|",
   arr_formalizados[a].año_ejercicio      CLIPPED,"|",
   arr_formalizados[a].id_causa_liquida   USING "&&" CLIPPED,"|",
   v_liquida_credito                      CLIPPED,"|",
   v_formalizacion                        CLIPPED,"|",
   v_solic_marca_prcr                     CLIPPED,"|",
   v_conf_marca_prcr                      CLIPPED,"|",
   v_solic_desmarca_prcr                  CLIPPED,"|",
   v_conf_desmarca_prcr                   CLIPPED,"|",
   v_reg_carta                            CLIPPED

   CALL ch.writeLine([v_detalle])

END FUNCTION

FUNCTION fn_sin_datos()

   LET v_detalle = "NO SE ENCONTRARON REGISTROS CON LOS PARÁMETROS DE FECHA INGRESADOS"

   CALL ch.writeLine([v_detalle])
END FUNCTION