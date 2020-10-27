MAIN
DEFINE i SMALLINT,
       j SMALLINT,
       report_handler         om.SaxDocumentHandler

  DISPLAY "Esta es una prueba de reporte mayeyo con varias hojas"

  -- se indica que el reporte usara la plantilla creada
  --IF ( fgl_report_loadCurrentSettings("../../ret/bin/RPTMAYEYO.4rp") ) THEN  -- if  the file loaded OK
  IF ( fgl_report_loadCurrentSettings("/ds/safreviv/ret/bin/RPTMAYEYO.4rp") ) THEN  -- if  the file loaded OK      
     -- sin preview
     --CALL fgl_report_selectPreview(0)  
         
     LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
         


  
     START REPORT rpt_mayeyo TO XML HANDLER report_handler

     FOR j = 1 TO 5
        FOR i = 1 TO 30
           OUTPUT TO REPORT rpt_mayeyo(i, j)

        END FOR
     END FOR
  
     FINISH REPORT rpt_mayeyo
  ELSE
    DISPLAY "no se pudo abrir la plantilla mayeya"
  END IF

END MAIN

REPORT rpt_mayeyo(li_i, li_j)
DEFINE li_i SMALLINT,
       li_j SMALLINT,
    ls_cadena STRING

FORMAT

  BEFORE GROUP OF li_j
     LET ls_cadena = "Grupo de J ", li_j USING "&&"
     PRINTX ls_cadena 

  ON EVERY ROW
     PRINTX li_i

END REPORT