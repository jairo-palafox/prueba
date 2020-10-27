################################################################################
#Modulo           => AFI                                                       #
#Programa         => AFIC09                                                    #
#Objetivo         => Consulta de arvhivo indicadores entrada via servicio WEB  #
#Fecha de Inicio  => FEBRERO 2015                                              #
################################################################################
DATABASE safre_viv

DEFINE g_usuario        CHAR(20)
DEFINE g_proceso_cod    SMALLINT

DEFINE g_arr_detalle    DYNAMIC ARRAY OF RECORD
   archivo                 CHAR(40),
   folio_lote              INTEGER,
   fecha_carga             DATE,
   estado                  SMALLINT,
   desc_estado             CHAR(40),
   tot_detalle             INTEGER,
   tot_sumario             INTEGER
END RECORD

MAIN
   DEFINE p_tipo_ejecucion    SMALLINT
   DEFINE p_s_titulo          STRING

   CLOSE WINDOW SCREEN

   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET g_proceso_cod    = 1815

   IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED||'.AFIC09.log')
   CALL fn_consulta_archivos()

END MAIN


FUNCTION fn_consulta_archivos ()
   DEFINE v_condicion            STRING
   DEFINE v_tot_reg              INTEGER

   OPEN WINDOW afic091 WITH FORM "AFIC091"

   DIALOG ATTRIBUTES(UNBUFFERED)
      CONSTRUCT v_condicion ON a.nombre_archivo, a.folio, a.f_actualiza
                          FROM v_archivo, v_folio, v_fecha
      
      END CONSTRUCT

      DISPLAY ARRAY g_arr_detalle TO detalle.*
      END DISPLAY

      ON ACTION ACCEPT
         IF v_condicion = '1=1' THEN
            CALL fn_mensaje ("Notificaciones",
                             "Debe de ingresar un rango de búsqueda",
                             "information")
         ELSE
            CALL fn_llena_arreglo(v_condicion) RETURNING v_tot_reg
         END IF

      ON ACTION CANCEL
         EXIT DIALOG

      ON ACTION CLOSE
         EXIT DIALOG

      ON ACTION reporte
         CALL fn_genera_reporte()
   END DIALOG
   
   CLOSE WINDOW afic091
END FUNCTION

FUNCTION fn_llena_arreglo(p_condicion)
   DEFINE p_condicion            STRING
   DEFINE v_query                STRING
   DEFINE i                      INTEGER

   CALL g_arr_detalle.clear()

   LET v_query = ' SELECT a.nombre_archivo, 
                          a.folio,
                          a.f_actualiza,
                          a.estado,
                          CASE a.estado
                            WHEN 1 THEN "VALIDADO"
                            WHEN 2 THEN "INTEGRADO"
                            WHEN 3 THEN "REVERSADO"
                          END CASE,
                          b.tot_detalle,
                          b.tot_sumario
                     FROM glo_ctr_archivo a,
                          afi_ind_cifras b
                    WHERE a.proceso_cod = ', g_proceso_cod,
                    ' AND b.folio_lote = a.folio AND ',p_condicion 

   PREPARE prp_archivos_indica FROM v_query
   DECLARE cur_archivos_indica CURSOR FOR prp_archivos_indica

   LET i = 1

   FOREACH cur_archivos_indica INTO g_arr_detalle[i].*
      LET i = i + 1
   END FOREACH

   RETURN i
END FUNCTION 

FUNCTION fn_genera_reporte()
   DEFINE v_reporte           STRING
   DEFINE i                   INTEGER
   DEFINE v_ruta_reporte      STRING
   DEFINE v_excepcion         SMALLINT

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte           RECORD
      indicador                  SMALLINT,
      tpo_notificacion           SMALLINT,
      edo_indicador              SMALLINT,
      tot_indicador              INTEGER
   END RECORD

   LET v_reporte = "AFIC091.4rp"

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      CALL fn_mensaje ("Notificaciones",
                       "no se encuentra la plantilla para generar el reporte: "|| v_reporte,
                       "")
      LET v_excepcion = 1
   END IF
   
   IF NOT v_excepcion THEN
      START REPORT rep_indicadores TO XML HANDLER report_handler
      FOR i = 1 TO (g_arr_detalle.getLength()-1)
         OUTPUT TO REPORT rep_indicadores(g_arr_detalle[i].*)
      END FOR
      FINISH REPORT rep_indicadores
   END IF
END FUNCTION

REPORT rep_indicadores(p_reporte)
   DEFINE p_reporte           RECORD
      archivo                 CHAR(40),
      folio_lote              INTEGER,
      fecha_carga             DATE,
      estado                  SMALLINT,
      desc_estado             CHAR(40),
      tot_detalle             INTEGER,
      tot_sumario             INTEGER
   END RECORD

   DEFINE p_r_encabezado    RECORD
        p_usuario_cod         STRING,
        p_fecha               DATE
   END RECORD

   DEFINE v_suma              INTEGER

   FORMAT
      FIRST PAGE HEADER
         LET p_r_encabezado.p_fecha = TODAY
         LET p_r_encabezado.p_usuario_cod = g_usuario

         PRINTX p_r_encabezado.*

      ON EVERY ROW
         PRINTX p_reporte.*


      ON LAST ROW
         LET v_suma = COUNT(*)
         PRINTX v_suma
END REPORT
