--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

######################################################################
#Modulo            =>OCG                                             #
#Programa          =>OCGS04                                          #
#Objetivo          =>Programa para generar detalle de movimientos    #
#                   del sistema de administración de créditos 43BIS  #
#Autor             =>José Eduardo Ventura                            #
#Fecha inicio      =>17 Octubre 2016                                 #
######################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_usuario                 CHAR(20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_titulo                  CHAR(30)
   DEFINE v_ruta_envio              CHAR(50)
   DEFINE v_nom_archivo             STRING
   DEFINE v_nss                     CHAR(11)
   DEFINE v_id_dh                   DECIMAL(9,0)
   DEFINE v_paterno                 CHAR(40)
   DEFINE v_materno                 CHAR(40)
   DEFINE v_nombre_af               CHAR(40)
   DEFINE v_nombre_completo         STRING
   DEFINE v_acciones                DECIMAL(16,6)
   DEFINE v_precio_fondo            DECIMAL(19,14)
   DEFINE z                         INTEGER
   DEFINE ch                        base.Channel
   DEFINE v_detalle                 STRING
   DEFINE v_nom_arh                 STRING

   DEFINE arr_detalle   DYNAMIC ARRAY OF RECORD
      nss                       CHAR(11),
      id_derechohabiente        DECIMAL(9,0),
      cve_ef                    CHAR(3),
      cve_ent_financiera        CHAR(100),
      estado_desc               CHAR(100),
      sum_imp_ap_subsec         DECIMAL(16,2),
      sum_imp_dev_ap_subsec     DECIMAL(16,2),
      sum_imp_uso_gtia          DECIMAL(16,2),
      sum_imp_dev_uso_gtia      DECIMAL(16,2),
      concepto_desc             CHAR(100),
      check_box                 SMALLINT
   END RECORD

   DEFINE arr_cons_detalle DYNAMIC ARRAY OF RECORD
      nss               CHAR(11),
      f_transaccion     DATE,
      vivienda_97       DECIMAL(15,2),
      periodo_pago      CHAR(6),
      f_pago            DATE,
      concepto          CHAR(50),
      cve_ent_financiera SMALLINT
   END RECORD

     DEFINE arr_cons_detalle1 DYNAMIC ARRAY OF RECORD
      nss               CHAR(11),
      f_transaccion     DATE,
      vivienda_97       DECIMAL(15,2),
      periodo_pago      CHAR(6),
      f_pago            DATE,
      concepto          CHAR(50),
      cve_ent_financiera SMALLINT
   END RECORD

END GLOBALS

MAIN
   -- se recuperan los parametros
   LET p_usuario         = ARG_VAL(1)
   LET p_tpo_ejecucion   = ARG_VAL(2)
   LET p_titulo          = ARG_VAL(3)

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   -- Se asigna el título de la ventana
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGS04.log")

   OPEN WINDOW OCGS04 WITH FORM "OCGS041"

      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED)

      ON ACTION ACCEPT
         DISPLAY v_nss
         CALL fn_busqueda_actual()
         CALL fn_busqueda_historica()
         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
      END INPUT

   CLOSE WINDOW OCGS04

END MAIN

-- búsqueda de datos en tablas actuales de OCG
FUNCTION fn_busqueda_actual()

   DEFINE v_saldo               DECIMAL(12,2)
   DEFINE v_cnt                 INTEGER
   DEFINE a                     SMALLINT
   DEFINE b                     SMALLINT
   DEFINE v_cadena              STRING
   DEFINE v_qry                 STRING
   DEFINE v_qry_ef              STRING
   DEFINE v_importe             DECIMAL(16,2)
   DEFINE v_s_detalle           STRING
   DEFINE v_mensaje             STRING
   DEFINE v_nom_reporte         STRING
   DEFINE v_s_reporte           STRING
   DEFINE archivo_txt           STRING
   DEFINE v_entidad             CHAR(3) --SMALLINT

   DEFINE arr_concepto DYNAMIC ARRAY OF RECORD
      concepto         CHAR(12)
   END RECORD

{
"117" --concepto para aporcación apoyo infonavit
"817" --concepto para aportación cofi
"417" --concepto para uso de garantía
"108" --concepto par devolución de aportación apoyo infonavit 
"808" --concepto para devolución de aportación cofi
"408" --concepto para devolución de uso garantía
"508" --concepto para devolucion de no acredsitados
}

   LET arr_concepto[1].concepto = "117,817,317" --conceptos para aportación apoyo infonavit
   LET arr_concepto[2].concepto = "417"         --concepto para uso de garantía
   LET arr_concepto[3].concepto = "508"         --concepto par devolución de aportación
   LET arr_concepto[4].concepto = "608"         --concepto para devolución de uso garantía

   SELECT id_derechohabiente,
          ap_paterno_af,
          ap_materno_af,
          nombre_af
     INTO v_id_dh,
          v_paterno,
          v_materno,
          v_nombre_af
     FROM afi_derechohabiente
    WHERE nss = v_nss

   IF v_id_dh IS NULL THEN
       CALL fn_mensaje("", "NSS no existe en base de datos","")
   ELSE

   LET v_nombre_completo = v_paterno CLIPPED," ",v_materno CLIPPED," ",v_nombre_af CLIPPED

   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
     AND f_valuacion = TODAY

   SELECT SUM(monto_acciones)
     INTO v_acciones
     FROM cta_movimiento
    WHERE id_derechohabiente = v_id_dh
      AND subcuenta = 4
      AND fondo_inversion = 11

   LET v_saldo =(v_acciones * v_precio_fondo)

   LET b = 1


   FOR a = 1 TO arr_concepto.getLength()

      LET v_cadena = " "

      --IF arr_concepto[a].concepto = "117,817" THEN
         LET v_cadena = "AND tr.estado IN (70,80) "
      --END IF

   --query para registros recurrentes de aportaciones subsecuentes y usos de garantía
      LET v_qry   = "SELECT tr.id_derechohabiente,
                            a.nss,
                            c.cve_ent_financiera,
                            c.cve_ent_financiera||' -'||trim(c.ent_financiera_desc)||' - '||
                            trim(e.edo_credito_desc),
                            cat.situacion_desc,
                            sum(tr.vivienda_97)
                       FROM ocg_ctr_transaccion tr,
                            afi_derechohabiente a,
                            cat_entidad_financiera c,
                            cat_ocg_situacion cat,
                            ocg_formalizacion f,
                            cat_ocg_estado e
                      WHERE tr.id_derechohabiente = a.id_derechohabiente
                        AND tr.cve_ent_financiera = c.cve_ent_financiera
                        AND tr.id_ocg_formalizacion = f.id_ocg_formalizacion
                        AND f.situacion = cat.situacion
                        AND f.estado = e.edo_credito
                        AND tr.id_derechohabiente = ",v_id_dh,
                        " AND concepto IN (",arr_concepto[a].concepto,") ",
                        v_cadena,
                        "GROUP BY 1,2,3,4,5 ",
                        "ORDER BY  3"

      --DISPLAY v_qry

      PREPARE prp_detalle FROM v_qry
      DECLARE curr_detalle CURSOR FOR prp_detalle

      FOREACH curr_detalle INTO    arr_detalle[b].id_derechohabiente,
                                   arr_detalle[b].nss,
                                   arr_detalle[b].cve_ef,
                                   arr_detalle[b].cve_ent_financiera,
                                   arr_detalle[b].estado_desc,
                                   v_importe
      
      IF b >= 2 THEN
      
        IF arr_detalle[b].cve_ent_financiera = arr_detalle[b-1].cve_ent_financiera THEN
            LET b = b-1
        END IF 
      END IF
 
         IF a= 1 THEN
            LET arr_detalle[b].sum_imp_ap_subsec = v_importe
          --  LET arr_detalle[b].concepto_desc = "Aportación Subsecuente"
         END IF

         IF a= 2 THEN
            LET arr_detalle[b].sum_imp_uso_gtia = v_importe
          --  LET arr_detalle[b].concepto_desc = "Solicitud Uso Garantía"
         END IF

         IF a= 3 THEN
            LET arr_detalle[b].sum_imp_dev_ap_subsec = v_importe
          --  LET arr_detalle[b].concepto_desc = "Devolución Aportación Subsecuente"
         END IF

         IF a= 4 THEN
            LET arr_detalle[b].sum_imp_dev_uso_gtia = v_importe
            --LET arr_detalle[b].concepto_desc = "Devolución Uso Garantía"
         END IF
         
            LET b = b+1

      END FOREACH

         CALL arr_detalle.deleteElement(b)
         
   END FOR
  
   
   DISPLAY v_nombre_completo TO v_nombre
   DISPLAY BY NAME v_saldo

   IF arr_detalle.getLength() >= 1 THEN

   INPUT ARRAY arr_detalle FROM tabla_1.* ATTRIBUTE (UNBUFFERED , WITHOUT DEFAULTS,
                                                              APPEND ROW = FALSE,
                                                              DELETE ROW = FALSE,
                                                              INSERT ROW = FALSE)

      ON ACTION ACCEPT
      SELECT ruta_envio
        INTO v_ruta_envio
        FROM seg_modulo
       WHERE modulo_cod = 'ocg'

      LET v_nom_arh     = v_ruta_envio CLIPPED ,"/det_movimientos_43bis",".txt"
      LET v_nom_reporte = "det_movimientos_43bis.txt"

      LET ch = base.Channel.create()
      CALL ch.openFile(v_nom_arh,"w" )
      CALL ch.setDelimiter(NULL)

      LET z = 1

         FOR b = 1 TO arr_detalle.getLength()

            IF arr_detalle[b].check_box = 1 THEN

               --LET v_entidad = arr_detalle[b].cve_ent_financiera[1,3] 
               

               LET v_s_detalle ="SELECT t.nss,
                                        t.f_transaccion,
                                        t.vivienda_97,
                                        t.periodo_pago,
                                        t.f_pago,
                                        t.concepto,
                                        t.cve_ent_financiera
                                   FROM ocg_ctr_transaccion t,
                                        ocg_formalizacion fz,  
                                        cat_concepto_ocg c
                                  WHERE t.id_derechohabiente = ",arr_detalle[b].id_derechohabiente,
                                  " AND t.id_ocg_formalizacion = fz.id_ocg_formalizacion",
                                  " AND t.cve_ent_financiera = ",arr_detalle[b].cve_ef,
                                  " AND t.estado in (70,80)
                                    AND t.concepto in (117,817,417,317,608,508)
                                    AND t.concepto = c.cod_concepto_ocg
                                    ORDER BY f_pago"

               --DISPLAY "Detalle: ", v_s_detalle

               PREPARE prp_detalle_mov FROM v_s_detalle
               DECLARE curr_detalle_mov CURSOR FOR prp_detalle_mov

               FOREACH curr_detalle_mov INTO arr_cons_detalle[z].*
                  IF (arr_cons_detalle[z].concepto = "117") OR 
                     (arr_cons_detalle[z].concepto = "817") OR
                     (arr_cons_detalle[z].concepto = "317") THEN
                     LET arr_cons_detalle[z].concepto = "Aportación Subsecuente"
                  END IF

                  IF (arr_cons_detalle[z].concepto = "417") THEN
                      LET arr_cons_detalle[z].concepto = "Uso de Garantía"
                  END IF

                  IF (arr_cons_detalle[z].concepto = "508") THEN
                      LET arr_cons_detalle[z].concepto = "Devolución Aportación Subsecuente"
                  END IF

                  IF (arr_cons_detalle[z].concepto = "608") THEN
                      LET arr_cons_detalle[z].concepto = "Devolución Uso de Garantía"
                  END IF                  

                  CALL fn_escribe(z)
                  LET z = z+1
               END FOREACH

               CALL arr_cons_detalle.deleteElement(arr_cons_detalle.getLength())

            END IF
         END FOR

          CALL ch.close()

         IF arr_cons_detalle.getLength() >= 1 THEN
            LET v_mensaje = "El archivo fue generado de forma correcta en \n",v_nom_arh
            CALL fn_mensaje("Aviso",v_mensaje,"info")
            LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','ocg')\" target='nueva'>", v_nom_reporte CLIPPED,"</a>"
            DISPLAY v_s_reporte TO archivo_txt
            DISPLAY ARRAY arr_cons_detalle TO tab_detalle.*
            ON ACTION reporte
               CALL fn_genera_reporte()
               END DISPLAY
            EXIT INPUT
         ELSE
            CALL fn_mensaje("Aviso","Archivo generado sin registros","info")
            EXIT INPUT
         END IF


      ON ACTION CANCEL
         EXIT INPUT

      END INPUT

      ELSE
         CALL fn_mensaje("Aviso","No se encontraron movimientos para NSS ingresado","info")
      END IF

   END IF
END FUNCTION

FUNCTION fn_escribe(z)

   DEFINE v_detalle           STRING
   DEFINE z                   INTEGER

   LET v_detalle = arr_cons_detalle[z].nss CLIPPED, "|",
                   arr_cons_detalle[z].concepto CLIPPED,"|",
                   arr_cons_detalle[z].f_pago CLIPPED USING "yyyymmdd","|",
                  -- arr_cons_detalle[z].f_transaccion CLIPPED,"|",
                   arr_cons_detalle[z].periodo_pago CLIPPED,"|",
                   arr_cons_detalle[z].vivienda_97 CLIPPED,"|",
                   arr_cons_detalle[z].cve_ent_financiera CLIPPED

   CALL ch.writeLine([v_detalle])

END FUNCTION

-- Búsqueda de datos en tablas históricas de migracion 43bis
FUNCTION fn_busqueda_historica()

END FUNCTION

FUNCTION fn_genera_reporte()

   DEFINE v_reporte           STRING   -- Variable para nombre del reporte
   DEFINE v_ruta_reporte      STRING   -- Variable para ruta final del reporte
   DEFINE v_excepcion         SMALLINT
   DEFINE v_query             STRING   -- Variable para consulta de saldos para reporte
   DEFINE v_ruta_listados     CHAR (40)-- Variable para ruta de salida del reporte
   DEFINE report_handler      om.SaxDocumentHandler


   DEFINE r_reporte RECORD
          v_cuenta INTEGER
   END RECORD

   LET v_reporte = "OCGS04.4rp"

-- ruta para guardar el reporte
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        p_usuario CLIPPED , "-", -- usuario
                        "OCGS04","-",--".pdf" -- programa
                        --v_pid USING "&&&&&","-", -- PID
                        --g_proceso_cod USING "&&&&&", "-", -- código del proceso
                        --g_opera_cod   USING "&&&&&",
                        TODAY USING "ddmmyyyy",".pdf" -- código de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN

-- Inicia reporte
      START REPORT rep_resultados TO XML HANDLER report_handler

      FOR z = 1 TO arr_detalle.getLength()
      -- Salida de reporte
         IF arr_detalle[z].sum_imp_ap_subsec IS NULL THEN
            LET arr_detalle[z].sum_imp_ap_subsec = 0
         END IF

         IF arr_detalle[z].sum_imp_dev_ap_subsec IS NULL THEN
            LET arr_detalle[z].sum_imp_dev_ap_subsec = 0
         END IF

         IF arr_detalle[z].sum_imp_dev_uso_gtia IS NULL THEN
            LET arr_detalle[z].sum_imp_dev_uso_gtia = 0
         END IF

         IF arr_detalle[z].sum_imp_uso_gtia IS NULL THEN
            LET arr_detalle[z].sum_imp_uso_gtia = 0
         END IF
    
         OUTPUT TO REPORT rep_resultados(arr_cons_detalle1[z].*,arr_detalle[z].cve_ent_financiera,1)
      END FOR

      FOR z = 1 TO arr_cons_detalle.getLength()
      -- Salida de reporte
            OUTPUT TO REPORT rep_resultados(arr_cons_detalle[z].*,'',2)
      END FOR

-- Finaliza reporte
    FINISH REPORT rep_resultados

    END IF
END FUNCTION

--******************************************
-- Se cachan datos para generar el reporte *
--******************************************
REPORT rep_resultados(p_reporte,p_entidad,p_grupo)

DEFINE p_reporte RECORD
      nss                CHAR(11),
      f_transaccion      DATE,
      vivienda_97        DECIMAL(15,2),
      periodo_pago       CHAR(6),
      f_pago             DATE,
      concepto           CHAR(50),
      cve_ent_financiera SMALLINT
   END RECORD

   DEFINE p_entidad RECORD
      entidad CHAR(100)
   END RECORD

   DEFINE p_grupo                  SMALLINT
   DEFINE v_fecha_reporte          DATE
   DEFINE v_usuario                CHAR (20)
   DEFINE a                        SMALLINT
   DEFINE v_sum_ap                 DECIMAL(15,2)
   DEFINE v_sum_ug                 DECIMAL(15,2)
   DEFINE v_sum_dev_ap             DECIMAL(15,2)
   DEFINE v_sum_dev_ug             DECIMAL(15,2)
   DEFINE v_ent_financiera         CHAR(60)

   
   ORDER BY p_grupo,
            p_reporte.cve_ent_financiera,
            p_reporte.f_pago
   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY
         LET v_sum_ug        = 0
         LET v_sum_dev_ap    = 0
         LET v_sum_dev_ug    = 0
         LET v_sum_ap        = 0

         FOR a = 1 TO arr_detalle.getLength()
            LET v_sum_ap     = v_sum_ap + arr_detalle[a].sum_imp_ap_subsec
            LET v_sum_ug     = v_sum_ug + arr_detalle[a].sum_imp_uso_gtia
            LET v_sum_dev_ap = v_sum_dev_ap + arr_detalle[a].sum_imp_dev_ap_subsec
            LET v_sum_dev_ug = v_sum_dev_ug + arr_detalle[a].sum_imp_dev_uso_gtia
         END FOR

         CASE
            WHEN v_sum_ap IS NULL
               LET v_sum_ap     = 0
            WHEN v_sum_ug IS NULL
               LET v_sum_ug     = 0
            WHEN v_sum_dev_ap IS NULL
               LET v_sum_dev_ap = 0
            WHEN v_sum_dev_ug IS NULL
               LET v_sum_dev_ug = 0
         END CASE

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX v_sum_ap       
         PRINTX v_sum_ug
         PRINTX v_sum_dev_ap
         PRINTX v_sum_dev_ug

      ON EVERY ROW
{
         IF v_ent_financiera = p_reporte.cve_ent_financiera THEN
            LET v_ent_financiera = ""
         ELSE
            LET v_ent_financiera = p_reporte.cve_ent_financiera
            --PRINTX v_ent_financiera
         END IF
}        
         PRINTX p_grupo
         PRINTX p_entidad.entidad
         PRINTX v_nss
         PRINTX v_nombre_completo
         PRINTX p_reporte.concepto
         PRINTX p_reporte.f_pago USING "dd/mm/yyyy"
         PRINTX p_reporte.nss
         PRINTX p_reporte.periodo_pago
         PRINTX p_reporte.vivienda_97
         PRINTX p_reporte.f_transaccion
         PRINTX p_reporte.cve_ent_financiera
         PRINTX v_ent_financiera

      ON LAST ROW

END REPORT