##############################################################################
#Modulo            => AGR                                                    #
#Programa          => AGRP54                                                 #
#Objetivo          => Programa genera reporte de cifras control adelanto.    #
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

   -- Arreglo para el reporte de cifras control adelantos
   DEFINE arr_cifras   DYNAMIC ARRAY OF RECORD
      modulo     CHAR(3),
      total      INTEGER,
      aivs92     DECIMAL(22,6),
      aivs97     DECIMAL(22,6),
      aivs_total DECIMAL(22,6),
      porcentaje CHAR(12)
   END RECORD
   DEFINE rec_t_cifras  RECORD
      total     INTEGER,
      aivs92       DECIMAL(22,6),
      aivs97       DECIMAL(22,6),
      aivs_total   DECIMAL(22,6),
      porcentaje   CHAR(12)
   END RECORD
   DEFINE v_aux_porcentaje  DECIMAL(6,2) 
   
END GLOBALS 

MAIN 

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP56.log")

   CLOSE WINDOW SCREEN 

   SELECT ruta_bin
     INTO v_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   OPEN WINDOW vtn1 WITH FORM "AGRP561"
      LET v_f_genera = TODAY

      MENU ""
         BEFORE MENU 
            DISPLAY v_f_genera TO e_fecha
              
           ON ACTION ACCEPT 
              CALL fn_adelantos_cifras_control()
              CALL fn_mensaje("","El reporte PDF se ha generado correctamente","")
              EXIT MENU 

         ON ACTION CANCEL 
            EXIT MENU

      END MENU  
   CLOSE WINDOW vtn1
   
END MAIN 

FUNCTION fn_adelantos_cifras_control()

   DEFINE v_c               INTEGER
   DEFINE v_subcuenta       SMALLINT
   DEFINE v_monto_acciones  DECIMAL(22,6)

   -- Inicializa arreglo
   LET arr_cifras[1].modulo     = "   "
   LET arr_cifras[1].total      = 0
   LET arr_cifras[1].aivs92     = 0
   LET arr_cifras[1].aivs97     = 0
   LET arr_cifras[1].aivs_total = 0

   LET rec_t_cifras.total      = 0
   LET rec_t_cifras.aivs92     = 0
   LET rec_t_cifras.aivs97     = 0
   LET rec_t_cifras.aivs_total = 0
   
   LET v_aux_porcentaje = 0
   
   -- Query global
   DECLARE crs_cifras_control CURSOR FOR 
   SELECT modulo,
          subcuenta,
          SUM(monto_acciones),
          COUNT(*)
     FROM cbd_movimiento_adelanto
    GROUP BY 1,2
    ORDER BY modulo;

   LET v_c = 1
   LET v_subcuenta = NULL
   LET v_monto_acciones = 0
   
   FOREACH crs_cifras_control INTO arr_cifras[v_c].modulo,
                                   v_subcuenta,
                                   v_monto_acciones,
                                   arr_cifras[v_c].total

      -- Inicializa valores
      LET arr_cifras[v_c].aivs92 = 0
      LET arr_cifras[v_c].aivs97 = 0
      
      -- Incrementa total global
      LET rec_t_cifras.total  = rec_t_cifras.total  + arr_cifras[v_c].total
      LET rec_t_cifras.aivs_total = rec_t_cifras.aivs_total + v_monto_acciones
      
      IF(v_subcuenta = 8) THEN 
         LET arr_cifras[v_c].aivs92 = v_monto_acciones
         LET arr_cifras[v_c].aivs_total = v_monto_acciones
         LET rec_t_cifras.aivs92 = rec_t_cifras.aivs92 + v_monto_acciones
      ELSE 
         LET arr_cifras[v_c].aivs97 = v_monto_acciones
         LET arr_cifras[v_c].aivs_total =  v_monto_acciones
         LET rec_t_cifras.aivs97 = rec_t_cifras.aivs97 + v_monto_acciones
      END IF 

      -- Verifica que el arreglo tenga más de un registro para evaluar el registro anterior
      IF(v_c >= 2) THEN
         IF(arr_cifras[v_c].modulo = arr_cifras[v_c -1].modulo) THEN
            -- Pasa los datos a la fila anterior
            IF(v_subcuenta = 8) THEN 
               LET arr_cifras[v_c -1].aivs92     = v_monto_acciones
               LET arr_cifras[v_c -1].aivs_total = arr_cifras[v_c -1].aivs_total + v_monto_acciones
            ELSE 
               LET arr_cifras[v_c -1].aivs97 = v_monto_acciones
               LET arr_cifras[v_c -1].aivs_total = arr_cifras[v_c -1].aivs_total + v_monto_acciones
            END IF

            -- Incrementa total
            LET arr_cifras[v_c -1].total = arr_cifras[v_c -1].total  + arr_cifras[v_c].total

            --Elimina fila
            CALL arr_cifras.deleteElement(v_c)

            LET v_c = v_c - 1
            
         END IF
 
      END IF 
      
      LET v_c = v_c + 1

   END FOREACH

   -- Elimina fila en blanco
    IF(v_c > 1) THEN
      -- Elimina fila en blanco
      IF(arr_cifras[arr_cifras.getLength()].modulo IS NULL) THEN
         CALL arr_cifras.deleteElement(arr_cifras.getLength())
      END IF
   END IF

   -- Porcentaje global
   LET v_aux_porcentaje = (rec_t_cifras.aivs_total / rec_t_cifras.aivs_total) * 100
   LET rec_t_cifras.porcentaje = v_aux_porcentaje CLIPPED,"%"

     # ~~~~~~~~ Configuración del reporte PDF ~~~~~~~~~~ #

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP561.4rp"
   LET v_ruta_rpt    = "Reporte_cifras_control_",g_usuario CLIPPED,".pdf"
 
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

   DEFINE v_f     INTEGER

   FORMAT
      FIRST PAGE HEADER
         LET v_aux_porcentaje = 0
         #Encabezado
         PRINTX g_usuario
         PRINTX v_f_genera USING "dd/mm/yyyy"
         
         #Imprime total AG
         PRINTX rec_t_cifras.total     
         PRINTX rec_t_cifras.aivs92    
         PRINTX rec_t_cifras.aivs97    
         PRINTX rec_t_cifras.aivs_total
         PRINTX rec_t_cifras.porcentaje

      ON EVERY ROW 
         -- Arreglo extractor AG
         FOR v_f = 1 TO arr_cifras.getLength()
            PRINTX arr_cifras[v_f].modulo
            PRINTX arr_cifras[v_f].total
            PRINTX arr_cifras[v_f].aivs92
            PRINTX arr_cifras[v_f].aivs97
            PRINTX arr_cifras[v_f].aivs_total
            --Porcentaje
            LET v_aux_porcentaje = (arr_cifras[v_f].aivs_total / rec_t_cifras.aivs_total) * 100
            LET arr_cifras[v_f].porcentaje = v_aux_porcentaje CLIPPED,"%"
            PRINTX arr_cifras[v_f].porcentaje
         END FOR

END REPORT