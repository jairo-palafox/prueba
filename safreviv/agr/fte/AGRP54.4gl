##############################################################################
#Modulo            => AGR                                                    #
#Programa          => AGRP54                                                 #
#Objetivo          => Programa que genera reporte de adelantos por Op.09.    #
#Autor             => Emilio Abarca, EFP                                     #
#Fecha inicio      => 19/Noviembre/2018                                      #
##############################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   
   -- Variables para el reporte
   DEFINE v_ruta_bin          CHAR(40)
   DEFINE v_reporte_bin       STRING 
   DEFINE v_ruta_rpt          STRING
   DEFINE object_rpt          om.SaxDocumentHandler
   DEFINE v_f_genera          DATE
   -- Type para definición de arreglos
   TYPE rec_arreglo           RECORD
      modulo_cod   CHAR(2),
      total        INTEGER,
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6)
   END RECORD
   -- Type para definición de totales globales
   TYPE rec_total             RECORD
      total        INTEGER,
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6)
   END RECORD

   TYPE arr_global DYNAMIC ARRAY OF rec_arreglo --Arreglo global

   -- Arreglos
   DEFINE arreglo_ag         arr_global
   DEFINE arreglo_ta         arr_global
   DEFINE arreglo_ug         arr_global
   
   -- Record totales
   DEFINE r_total_ag         rec_total
   DEFINE r_total_ta         rec_total
   DEFINE r_total_ug         rec_total
   DEFINE r_total_glo        rec_total
   
END GLOBALS 

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP54.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   OPEN WINDOW vtn1 WITH FORM "AGRP541"
      LET v_f_genera = TODAY 
      
      MENU ""
         BEFORE MENU 
            DISPLAY v_f_genera TO e_fecha

         ON ACTION ACCEPT 
            CALL fn_adelantos_op09()
            CALL fn_mensaje("","El reporte PDF se ha generado correctamente","")
            EXIT MENU
            
         ON ACTION CANCEL 
            EXIT MENU
            
      END MENU 
   CLOSE WINDOW vtn1
   
END MAIN 

FUNCTION fn_adelantos_op09()

   DEFINE r_adelantos   RECORD
      modulo_orig  CHAR(2),
      modulo_cod   CHAR(2),
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      aivs_total   DECIMAL(16,6),
      total        INTEGER
   END RECORD
   -- contadores
   DEFINE v_c_ag        INTEGER
   DEFINE v_c_ta        INTEGER
   DEFINE v_c_ug        INTEGER
   
   -- Inicializa arreglos y totales
   LET arreglo_ag[1].modulo_cod = "  "
   LET arreglo_ag[1].total      = 0
   LET arreglo_ag[1].aivs92     = 0
   LET arreglo_ag[1].aivs97     = 0
   LET arreglo_ag[1].aivs_total = 0

   LET arreglo_ta[1].modulo_cod = "  "
   LET arreglo_ta[1].total      = 0
   LET arreglo_ta[1].aivs92     = 0
   LET arreglo_ta[1].aivs97     = 0
   LET arreglo_ta[1].aivs_total = 0

   LET arreglo_ug[1].modulo_cod = "  "
   LET arreglo_ug[1].total      = 0
   LET arreglo_ug[1].aivs92     = 0
   LET arreglo_ug[1].aivs97     = 0
   LET arreglo_ug[1].aivs_total = 0

   LET r_total_ag.total      = 0
   LET r_total_ag.aivs92     = 0
   LET r_total_ag.aivs97     = 0
   LET r_total_ag.aivs_total = 0

   LET r_total_ta.total      = 0
   LET r_total_ta.aivs92     = 0
   LET r_total_ta.aivs97     = 0
   LET r_total_ta.aivs_total = 0

   LET r_total_ug.total      = 0
   LET r_total_ug.aivs92     = 0
   LET r_total_ug.aivs97     = 0
   LET r_total_ug.aivs_total = 0

   LET r_total_glo.total      = 0
   LET r_total_glo.aivs92     = 0
   LET r_total_glo.aivs97     = 0
   LET r_total_glo.aivs_total = 0
  
   -- Query global
   DECLARE crs_extractores CURSOR FOR 
   SELECT modulo_orig,
          modulo_cod,
          SUM(aivs92_09),
          SUM(aivs97_09),
          SUM(aivs92_09 + aivs97_09),
          COUNT(*)
     FROM cre_extr_adelanto
    GROUP BY 1,2;

   INITIALIZE r_adelantos.* TO NULL 
   
   -- inicializa contadores
   LET v_c_ag = 1
   LET v_c_ta = 1
   LET v_c_ug = 1
   
   FOREACH crs_extractores INTO r_adelantos.modulo_orig,
                                r_adelantos.modulo_cod,
                                r_adelantos.aivs92,
                                r_adelantos.aivs97,
                                r_adelantos.aivs_total,
                                r_adelantos.total
      CASE

         -- Extractor AG
         WHEN r_adelantos.modulo_orig = "AG"
            -- LLena arreglo AG
            LET arreglo_ag[v_c_ag].modulo_cod = r_adelantos.modulo_cod
            LET arreglo_ag[v_c_ag].total      = r_adelantos.total
            LET arreglo_ag[v_c_ag].aivs92     = r_adelantos.aivs92
            LET arreglo_ag[v_c_ag].aivs97     = r_adelantos.aivs97
            LET arreglo_ag[v_c_ag].aivs_total = r_adelantos.aivs_total

            -- Incrementa record total
            LET r_total_ag.total      = r_total_ag.total      + r_adelantos.total
            LET r_total_ag.aivs92     = r_total_ag.aivs92     + r_adelantos.aivs92
            LET r_total_ag.aivs97     = r_total_ag.aivs97     + r_adelantos.aivs97
            LET r_total_ag.aivs_total = r_total_ag.aivs_total + r_adelantos.aivs_total
            
            -- Incrementa contador
            LET v_c_ag = v_c_ag + 1

         -- Extractor TA
         WHEN r_adelantos.modulo_orig = "TA"
            -- Llena arreglo TA
            LET arreglo_ta[v_c_ta].modulo_cod = r_adelantos.modulo_cod
            LET arreglo_ta[v_c_ta].total      = r_adelantos.total
            LET arreglo_ta[v_c_ta].aivs92     = r_adelantos.aivs92
            LET arreglo_ta[v_c_ta].aivs97     = r_adelantos.aivs97
            LET arreglo_ta[v_c_ta].aivs_total = r_adelantos.aivs_total

            -- Incrementa record total
            LET r_total_ta.total      = r_total_ta.total      + r_adelantos.total
            LET r_total_ta.aivs92     = r_total_ta.aivs92     + r_adelantos.aivs92
            LET r_total_ta.aivs97     = r_total_ta.aivs97     + r_adelantos.aivs97
            LET r_total_ta.aivs_total = r_total_ta.aivs_total + r_adelantos.aivs_total
   
            -- Incrementa conmtador
            LET v_c_ta = v_c_ta + 1

         -- Extractor UG
         WHEN r_adelantos.modulo_orig = "UG"

            -- Llena arreglo UG
            LET arreglo_ug[v_c_ug].modulo_cod = r_adelantos.modulo_cod
            LET arreglo_ug[v_c_ug].total      = r_adelantos.total
            LET arreglo_ug[v_c_ug].aivs92     = r_adelantos.aivs92
            LET arreglo_ug[v_c_ug].aivs97     = r_adelantos.aivs97
            LET arreglo_ug[v_c_ug].aivs_total = r_adelantos.aivs_total

            LET r_total_ug.total      = r_total_ug.total      + r_adelantos.total
            LET r_total_ug.aivs92     = r_total_ug.aivs92     + r_adelantos.aivs92
            LET r_total_ug.aivs97     = r_total_ug.aivs97     + r_adelantos.aivs97
            LET r_total_ug.aivs_total = r_total_ug.aivs_total + r_adelantos.aivs_total
   
            -- Incrementa contador
            LET v_c_ug = v_c_ug + 1
            
      END CASE 
      
   END FOREACH

   -- Obtiene monto global
   LET r_total_glo.total      = r_total_ag.total  + r_total_ta.total  + r_total_ug.total
   LET r_total_glo.aivs92     = r_total_ag.aivs92 + r_total_ta.aivs92 + r_total_ug.aivs92
   LET r_total_glo.aivs97     = r_total_ag.aivs97 + r_total_ta.aivs97 + r_total_ug.aivs97  
   LET r_total_glo.aivs_total = r_total_ag.aivs_total + r_total_ta.aivs_total + r_total_ug.aivs_total

   # ~~~~~~~~ Configuración del reporte PDF ~~~~~~~~~~ #

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP541.4rp"
   LET v_ruta_rpt    = "Reporte_adelantos_",g_usuario CLIPPED,".pdf"
 
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(1) -- Se muestra en automático
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         
         START REPORT imprime_pdf TO XML HANDLER object_rpt

            OUTPUT TO REPORT imprime_pdf()

         FINISH REPORT imprime_pdf

      END IF
   ELSE
      CALL fn_mensaje("","No fué posible abrir la platilla del reporte","")
      EXIT PROGRAM 
   END IF
   
END FUNCTION

REPORT imprime_pdf()

   DEFINE v_f                INTEGER

   FORMAT
      FIRST PAGE HEADER
         
         #Encabezado
         PRINTX g_usuario
         PRINTX v_f_genera USING "dd/mm/yyyy"
         
         #Imprime total AG
         PRINTX r_total_ag.total     
         PRINTX r_total_ag.aivs92    
         PRINTX r_total_ag.aivs97    
         PRINTX r_total_ag.aivs_total
         --Imprime total TA
         PRINTX r_total_ta.total      
         PRINTX r_total_ta.aivs92     
         PRINTX r_total_ta.aivs97     
         PRINTX r_total_ta.aivs_total 
         --Imprime total UG
         PRINTX r_total_ug.total      
         PRINTX r_total_ug.aivs92     
         PRINTX r_total_ug.aivs97     
         PRINTX r_total_ug.aivs_total 
        --Imprime total Global
         PRINTX r_total_glo.total     
         PRINTX r_total_glo.aivs92    
         PRINTX r_total_glo.aivs97    
         PRINTX r_total_glo.aivs_total

      ON EVERY ROW 
         -- Arreglo extractor AG
         FOR v_f = 1 TO arreglo_ag.getLength()
            PRINTX arreglo_ag[v_f].modulo_cod
            PRINTX arreglo_ag[v_f].total
            PRINTX arreglo_ag[v_f].aivs92
            PRINTX arreglo_ag[v_f].aivs97
            PRINTX arreglo_ag[v_f].aivs_total
         END FOR

         -- Arreglo extractor TA
         FOR v_f = 1 TO arreglo_ta.getLength()
            PRINTX arreglo_ta[v_f].modulo_cod
            PRINTX arreglo_ta[v_f].total
            PRINTX arreglo_ta[v_f].aivs92
            PRINTX arreglo_ta[v_f].aivs97
            PRINTX arreglo_ta[v_f].aivs_total
         END FOR

         -- Arreglo extractor UG
         FOR v_f = 1 TO arreglo_ug.getLength()
            PRINTX arreglo_ug[v_f].modulo_cod
            PRINTX arreglo_ug[v_f].total
            PRINTX arreglo_ug[v_f].aivs92
            PRINTX arreglo_ug[v_f].aivs97
            PRINTX arreglo_ug[v_f].aivs_total
         END FOR

END REPORT
