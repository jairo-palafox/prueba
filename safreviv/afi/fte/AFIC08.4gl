################################################################################
#Modulo           => AFI                                                       #
#Programa         => AFIC08                                                    #
#Objetivo         => Consulta de indicadores entrada via servicio WEB          #
#Fecha de Inicio  => FEBRERO 2015                                              #
################################################################################
DATABASE safre_viv

DEFINE g_usuario        CHAR(20)
DEFINE g_arr_globales   DYNAMIC ARRAY OF RECORD
   tpo_notificacion        SMALLINT,
   desc_notificacion       CHAR(40),
   indicador               SMALLINT,
   desc_indicador          CHAR(40),
   totales                 INTEGER
END RECORD

DEFINE g_arr_detalle    DYNAMIC ARRAY OF RECORD
   nss                     CHAR(11),
   tpo_notificacion        SMALLINT,
   desc_notificacion       CHAR(40),
   indicador               SMALLINT,
   desc_indicador          CHAR(40),
   f_inicio                DATE,
   fuente                  SMALLINT,
   desc_fuente             CHAR(40),
   estado                  SMALLINT,
   desc_estado             CHAR(40)
END RECORD

DEFINE g_r_busqueda     RECORD
   nss                     STRING,
   fecha                   STRING,
   tpo_notificacion        STRING,
   indicador               STRING,
   fuente                  STRING,
   estado                  STRING
END RECORD
MAIN
   DEFINE p_tipo_ejecucion    SMALLINT
   DEFINE p_s_titulo          STRING

   CLOSE WINDOW SCREEN

   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED||'.AFIC08.log')
   CALL fn_consulta_indicadores()

END MAIN

FUNCTION fn_consulta_indicadores()
   DEFINE v_condicion            STRING
   DEFINE v_tot_reg              INTEGER
   DEFINE v_pos                  SMALLINT

   OPEN WINDOW afic081 WITH FORM "AFIC081"

   DIALOG ATTRIBUTES(UNBUFFERED)
      CONSTRUCT v_condicion ON a.nss, b.indicador, b.f_inicio, b.edo_indicador, b.tpo_notificacion, b.fuente
                          FROM v_nss, v_indicador, v_fecha, v_estado, v_tpo_notificacion, v_fuente
      
      END CONSTRUCT

      DISPLAY ARRAY g_arr_globales TO global.*
         BEFORE ROW
            LET v_pos = ARR_CURR()
            CALL fn_llena_detalle(g_arr_globales[v_pos].tpo_notificacion, 
                                  g_arr_globales[v_pos].indicador)
      END DISPLAY

      DISPLAY ARRAY g_arr_detalle TO detalle.*
      END DISPLAY

      ON ACTION ACCEPT
         IF v_condicion = '1=1' THEN
            CALL fn_mensaje ("Notificaciones",
                             "Debe de ingresar un rango de búsqueda",
                             "information")
         ELSE
            CALL fn_llena_arreglos(v_condicion) RETURNING v_tot_reg
         END IF

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION CLOSE
         EXIT DIALOG

      ON ACTION reporte
         CALL GET_FLDBUF( customer.* ) RETURNING g_r_busqueda.*
         CALL fn_genera_reporte()
   END DIALOG
   
   CLOSE WINDOW afic081

END FUNCTION

FUNCTION fn_llena_arreglos(p_condicion)
   DEFINE p_condicion            STRING
   DEFINE v_query                STRING
   DEFINE v_total                INTEGER
   DEFINE i                      INTEGER

   WHENEVER ERROR CONTINUE
   DROP TABLE  tmp_cons_ind_notif
   WHENEVER ERROR STOP
   
   LET v_query = 'SELECT a.nss,
                         b.*
                    FROM afi_derechohabiente a,
                         afi_his_ind_notifica b
                   WHERE b.id_derechohabiente = a.id_derechohabiente
                     AND ', p_condicion,
                  ' INTO TEMP  tmp_cons_ind_notif '

   PREPARE prp_consulta FROM v_query
   EXECUTE prp_consulta

   SELECT COUNT(*)
     INTO v_total
     FROM tmp_cons_ind_notif

     
   IF v_total > 0 THEN
      CALL g_arr_detalle.clear()
      CALL g_arr_globales.clear() 
      -----LLENA GLOBALES

      LET v_query = 'SELECT a.tpo_notificacion,
                            b.desc_notificacion,
                            indicador,
                            CASE a.indicador
                               WHEN 0 THEN "DESMARCAR"
                               WHEN 1 THEN "MARCAR"
                               ELSE "NO VALIDO"
                            END CASE,
                            COUNT(*)
                       FROM tmp_cons_ind_notif a,
                            cat_afi_tpo_notifica b
                      WHERE b.tpo_notificacion = a.tpo_notificacion 
                      GROUP BY 1,2,3,4
                      ORDER BY 1,3'
      PREPARE prp_globales FROM v_query
      DECLARE cur_globales CURSOR FOR prp_globales

      LET i = 1 
      FOREACH cur_globales INTO g_arr_globales[i].*
         LET i = i + 1
      END FOREACH
      CLOSE cur_globales
      FREE cur_globales
   END IF

   RETURN v_total
END FUNCTION 

FUNCTION fn_llena_detalle(p_tpo_notificacion, p_indicador)
   DEFINE p_tpo_notificacion     SMALLINT
   DEFINE p_indicador            SMALLINT
   DEFINE i                      INTEGER
   DEFINE v_query                STRING

   CALL g_arr_detalle.clear()
                                         
   LET i = 1

   LET v_query = 'SELECT a.nss, 
                         a.tpo_notificacion,
                         b.desc_notificacion,
                         a.indicador,
                         CASE a.indicador
                            WHEN 0 THEN "DESMARCAR"
                            WHEN 1 THEN "MARCAR"
                            ELSE "NO VALIDO"
                         END CASE,
                         a.f_inicio,
                         a.fuente,
                         NVL(c.desc_fuente ,"N/A"),
                         a.edo_indicador,
                         CASE a.edo_indicador
                           WHEN 0  THEN "ACEPTADO"
                           WHEN 90 THEN "YA EXISTE LA MARCA"
                           WHEN 99 THEN "NO EXISTE LA MARCA"
                         END CASE
                    FROM tmp_cons_ind_notif a,
                         cat_afi_tpo_notifica b,
                         OUTER cat_afi_notif_fuente c
                   WHERE a.tpo_notificacion  = ?
                     AND a.indicador         = ?
                     AND b.tpo_notificacion = a.tpo_notificacion 
                     AND c.fuente           = a.fuente '

   PREPARE prp_detalle FROM v_query
   DECLARE cur_detalle CURSOR FOR prp_detalle
   
   FOREACH cur_detalle USING p_tpo_notificacion,
                             p_indicador
                        INTO g_arr_detalle[i].*
      LET i = i + 1
      IF i > 100 THEN
         CALL fn_mensaje ("Notificaciones",
                          "Se excedió el límite de registros en detalle,\n acotar busqueda.",
                          "information")
         EXIT FOREACH
      END IF
   END FOREACH
   CLOSE cur_detalle
   FREE cur_detalle
   
END FUNCTION

FUNCTION  fn_genera_reporte()
   DEFINE v_reporte           STRING
   DEFINE v_excepcion         SMALLINT
   DEFINE i                   INTEGER
   DEFINE j                   INTEGER
   DEFINE v_bloque            SMALLINT
   DEFINE v_globales          RECORD
      tpo_notificacion           SMALLINT,
      desc_notificacion          CHAR(40),
      indicador                  SMALLINT,
      desc_indicador             CHAR(40),
      totales                    INTEGER
   END RECORD

   DEFINE v_detalle       RECORD
      nss                        CHAR(11),
      tpo_notificacion           SMALLINT,
      desc_notificacion          CHAR(40),
      indicador                  SMALLINT,
      desc_indicador             CHAR(40),
      f_inicio                   DATE,
      fuente                     SMALLINT,
      desc_fuente                CHAR(40),
      estado                     SMALLINT,
      desc_estado                CHAR(40)
   END RECORD

   DEFINE v_query             STRING
   DEFINE report_handler      om.SaxDocumentHandler   LET v_reporte = "AFIC081.4rp"

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      CALL fn_mensaje ("Notificaciones", 
                        "[ SACI EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE",
                       "information")
      LET v_excepcion = 1
   END IF
   
   LET v_query = 'SELECT a.nss, 
                         a.tpo_notificacion,
                         b.desc_notificacion,
                         a.indicador,
                         CASE a.indicador
                            WHEN 0 THEN "DESMARCAR"
                            WHEN 1 THEN "MARCAR"
                            ELSE "NO VALIDO"
                         END CASE,
                         a.f_inicio,
                         a.fuente,
                         NVL(c.desc_fuente ,"N/A"),
                         a.edo_indicador,
                         CASE a.edo_indicador
                           WHEN 0  THEN "ACEPTADO"
                           WHEN 90 THEN "YA EXISTE LA MARCA"
                           WHEN 99 THEN "NO EXISTE LA MARCA"
                         END CASE
                    FROM tmp_cons_ind_notif a,
                         cat_afi_tpo_notifica b,
                         OUTER cat_afi_notif_fuente c
                   WHERE a.tpo_notificacion  = ?
                     AND a.indicador         = ?
                     AND b.tpo_notificacion = a.tpo_notificacion 
                     AND c.fuente           = a.fuente '

   PREPARE prp_rep_detalle FROM v_query
   DECLARE cur_rep_detalle CURSOR FOR prp_rep_detalle
   
   IF NOT v_excepcion THEN

      START REPORT rep_consulta TO XML HANDLER report_handler

      FOR i = 1 TO g_arr_globales.getLength()
         LET v_bloque = 1
         OUTPUT TO REPORT rep_consulta(v_bloque,g_arr_globales[i].*, v_detalle.*)

         LET v_bloque = 2
         LET j = 1

         FOREACH cur_rep_detalle USING g_arr_globales[i].tpo_notificacion,
                                       g_arr_globales[i].indicador
                        INTO v_detalle.*
            IF j > 100 THEN
               EXIT FOREACH
            ELSE
               OUTPUT TO REPORT rep_consulta(v_bloque,v_globales.*, v_detalle.*)
            END IF
            LET j = j + 1

         END FOREACH
      END FOR
      FINISH REPORT rep_consulta
   END IF
END FUNCTION

REPORT rep_consulta(p_reporte)
   DEFINE p_reporte           RECORD
      v_bloque                   SMALLINT,
      tpo_notificacion           SMALLINT,
      desc_notificacion          CHAR(40),
      indicador                  SMALLINT,
      desc_indicador             CHAR(40),
      totales                    INTEGER,
      nss                        CHAR(11),
      tpo_notificacion_det       SMALLINT,
      desc_notificacion_det      CHAR(40),
      indicador_det              SMALLINT,
      desc_indicador_det         CHAR(40),
      f_inicio                   DATE,
      fuente                     SMALLINT,
      desc_fuente                CHAR(40),
      estado                     SMALLINT,
      desc_estado                CHAR(40)
   END RECORD

   DEFINE p_r_encabezado    RECORD
        p_usuario_cod         STRING,
        p_fecha               DATE,
        p_folio               DECIMAL(9,0),
        p_archivo             STRING
   END RECORD

   ORDER BY p_reporte.v_bloque, p_reporte.tpo_notificacion, p_reporte.tpo_notificacion_det,
            p_reporte.indicador_det, p_reporte.f_inicio
   FORMAT
      
      FIRST PAGE HEADER
         LET p_r_encabezado.p_fecha = TODAY
         LET p_r_encabezado.p_usuario_cod = g_usuario
                
         PRINTX p_r_encabezado.*, g_r_busqueda.*

      ON EVERY ROW
         PRINTX p_reporte.*

END REPORT