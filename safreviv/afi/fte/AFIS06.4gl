######################################################################
#Modulo            => AFI                                            #
#Programa          => AFIS05                                         #
#Objetivo          => Reporte de actualizacion de datos en BATCH.    #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 26/Marzo/2018                                  #
######################################################################

DATABASE safre_viv

GLOBALS 
   DEFINE g_usuario           CHAR(20)
   DEFINE g_titulo            STRING
   DEFINE g_tipo_ejecucion    SMALLINT
   DEFINE v_f_ini             DATE 
   DEFINE v_f_fin             DATE 
   --variables archivo de salida
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE v_arh_salida        STRING 
   --variables para el reporte pdf
   DEFINE v_reporte_bin       STRING
   DEFINE v_ruta_bin          CHAR(40)
   DEFINE v_ruta_rpt          STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_manejador_rpt     OM.SaxDocumentHandler
   
   --Arreglo archivos procesados
   DEFINE arr_archivos DYNAMIC ARRAY OF RECORD
      proceso_cod     SMALLINT,
      proceso_desc    CHAR(40),
      nombre_archivo  CHAR(40),
      folio           DECIMAL(10,0),
      usuario         CHAR(20),
      f_actualiza     DATE,
      check_box       SMALLINT
   END RECORD 
   
   --Record total global
   DEFINE r_total_global  RECORD
      total      INTEGER,
      porcentaje CHAR(12)
   END RECORD 
   
   --Arreglo inf. del reporte pdf
   DEFINE arr_det_archivo   DYNAMIC ARRAY OF RECORD
      nombre_archivo     CHAR(40),
      proceso_desc       CHAR(40),
      total              INTEGER,
      usuario            CHAR(20),
      f_proceso          DATE,
      porcentaje         CHAR(12) 
   END RECORD 
   
   DEFINE v_aux_porcentaje DECIMAL(6,2)
   
END GLOBALS 

MAIN

   LET g_usuario          = ARG_VAL  (1)
   LET g_tipo_ejecucion   = ARG_VAL  (2)
   LET g_titulo           = ARG_VAL  (3)

   IF (g_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

   SELECT ruta_bin,ruta_envio,ruta_listados
     INTO v_ruta_bin,v_ruta_envio,v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi';
    
   CALL STARTLOG(g_usuario CLIPPED|| ".AFIS06.log")

   CLOSE WINDOW SCREEN

   CALL reporte_batch()
   
END MAIN 

FUNCTION reporte_batch()

   DEFINE v_mensaje           STRING
   DEFINE v_qry_arh           STRING
   DEFINE k                   INTEGER 
   DEFINE w                   ui.Window
   DEFINE f                   ui.Form
   DEFINE v_c                 INTEGER
   DEFINE v_ind_true          SMALLINT
   DEFINE v_respuesta         SMALLINT 
   
   OPEN WINDOW vtn1 WITH FORM "AFIS061"

      LET w = ui.Window.getCurrent()
      LET f = w.getForm()

      -- Oculta grupo
      CALL f.setElementHidden("archivo",1)
      
      INPUT BY NAME v_f_ini,v_f_fin ATTRIBUTE(UNBUFFERED,WITHOUT DEFAULTS)
         BEFORE INPUT 
            LET v_f_ini = NULL
            LET v_f_fin = NULL 
            
         ON ACTION ACCEPT 
            # VALIDA CAPTURA
            IF(v_f_ini IS NULL) AND (v_f_fin IS NULL) THEN
               CALL fn_mensaje("","Debe ingresar el rango de fechas","")
               NEXT FIELD v_f_ini
            END IF 
            IF(v_f_ini IS NULL) AND (v_f_fin IS NOT NULL) THEN
               CALL fn_mensaje("","Ingresa la fecha inicial","")
               NEXT FIELD v_f_ini
            END IF 
            IF(v_f_ini IS NOT NULL) AND (v_f_fin IS NULL) THEN
               CALL fn_mensaje("","Ingresa la fecha final","")
               NEXT FIELD v_f_fin
            END IF 
            IF(v_f_ini > v_f_fin) THEN
               CALL fn_mensaje("","La fecha final debe ser mayor a la fecha inicial","")
               NEXT FIELD v_f_fin
            END IF 

            #Limpia arreglo
            CALL arr_archivos.clear()
            
            # carga arreglo dde archivos
            LET v_qry_arh = "SELECT g.proceso_cod,
                                    c.proceso_desc,
                                    g.nombre_archivo,
                                    g.folio,
                                    g.usuario,
                                    g.f_actualiza
                               FROM glo_ctr_archivo g, 
                                    cat_proceso c
                              WHERE g.proceso_cod IN (1803,1819,1820,1821,1823)
                                AND g.opera_cod = 1
                                AND g.proceso_cod = c.proceso_cod
                                AND g.f_actualiza >= ","'",v_f_ini,"'","
                                AND g.f_actualiza <= ","'",v_f_fin,"'","
                                AND g.estado      = 2
                                AND g.folio IS NOT NULL
                                ORDER BY g.f_actualiza;"

            PREPARE prp_archivos FROM v_qry_arh
            DECLARE crs_archivos CURSOR FOR prp_archivos

            LET k = 1

            FOREACH crs_archivos INTO arr_archivos[k].proceso_cod,
                                       arr_archivos[k].proceso_desc,
                                       arr_archivos[k].nombre_archivo,
                                       arr_archivos[k].folio,
                                       arr_archivos[k].usuario,
                                       arr_archivos[k].f_actualiza
                                       
               IF(arr_archivos[k].usuario = "SAFREVIV") OR 
                 (arr_archivos[k].usuario = "safreviv") THEN
                  LET arr_archivos[k].usuario = "INFONAVIT"
               END IF 
               
               # Check-box desmarcado
               LET arr_archivos[k].check_box = 0 --FALSE
              
               LET k = k + 1
               
            END FOREACH 

            IF(k = 1) THEN
               CALL fn_mensaje("","No se encontraron archivos procesados en el periodo seleccionado","")
               CONTINUE INPUT
            ELSE 
               IF(arr_archivos[arr_archivos.getLength()].proceso_cod IS NULL) THEN
                  CALL arr_archivos.deleteElement(arr_archivos.getLength()) 
               END IF
               # Muestra arreglo de archivos procesados en ese periodo
               CALL f.setElementHidden("archivo",0)

               INPUT ARRAY arr_archivos FROM record1.* ATTRIBUTE (UNBUFFERED,WITHOUT DEFAULTS,
                                                                     APPEND ROW = FALSE,
                                                                     DELETE ROW = FALSE,
                                                                     INSERT ROW = FALSE)
                  ON ACTION ACCEPT 
                     LET v_ind_true = 0
                     # Valida que se haya seleccionado por lo menos un archivo
                     FOR v_c = 1 TO arr_archivos.getLength()
                        IF(arr_archivos[v_c].check_box = 1) THEN
                           LET v_ind_true = 1
                           EXIT FOR
                        END IF 
                     END FOR 
                     IF(v_ind_true = 0) THEN
                        CALL fn_mensaje("","Selecciona por lo menos un archivo","")
                     ELSE
                        LET v_respuesta = fn_ventana_confirma("","¿Está seguro de generar el reporte?","")

                        IF(v_respuesta = 0) THEN
                           CALL fn_mensaje("","Se ha cancelado la operación","")
                           CALL arr_archivos.clear()  --Limpia arreglo
                           CALL f.setElementHidden("archivo",1) --Oculta arreglo de archivos
                           LET v_f_ini = NULL 
                           LET v_f_fin = NULL 
                           EXIT INPUT 
                        ELSE 
                           # Llamado a funciones que generan el reporte
                           CALL genera_reporte()
                           CALL genera_archivo_salida()
                          
                           # MENSAJE DE FINALIZACIÓN
                           LET v_mensaje = "La información se ha generado correctamente \n",
                                           "Reporte PDF: ",v_ruta_rpt CLIPPED,"\n",
                                            "Archivo de salida: ",v_arh_salida CLIPPED
                                            
                           CALL fn_mensaje("",v_mensaje,"")
                           
                           --Limpia arreglo
                           CALL arr_archivos.clear() 
                           CALL f.setElementHidden("archivo",1) --Oculta arreglo de archivos
                           LET v_f_ini = NULL 
                           LET v_f_fin = NULL 
                           EXIT INPUT 
                           #
                        END IF 
                     END IF 
                     
                  ON ACTION CANCEL
                     --Limpia arraglo
                     CALL arr_archivos.clear()
                     --oculta grupo archivos procesados
                     CALL f.setElementHidden("archivo",1)
                     LET v_f_ini = NULL 
                     LET v_f_fin = NULL 
                     EXIT INPUT
                  
               END INPUT  
            
            END IF 
            
         ON ACTION CANCEL 
            EXIT INPUT 
            
      END INPUT 
   
   CLOSE WINDOW vtn1
   
END FUNCTION 

FUNCTION genera_reporte()

   DEFINE f   INTEGER --Contador del FOR
   DEFINE z   INTEGER --Contador de archivos marcados

   --Inicializa variables
   LET r_total_global.total = 0
   
   CALL arr_det_archivo.clear()

   LET z = 1 --contador para el arreglo del detalle
   
   # Obtiene información para el reporte
   FOR f = 1 TO arr_archivos.getLength()
   
      #Recupera sólo los que estén marcados
      IF(arr_archivos[f].check_box = 1) THEN

         LET arr_det_archivo[z].proceso_desc   = arr_archivos[f].proceso_desc
         LET arr_det_archivo[z].nombre_archivo = arr_archivos[f].nombre_archivo
         LET arr_det_archivo[z].usuario        = arr_archivos[f].usuario
         LET arr_det_archivo[z].f_proceso      = arr_archivos[f].f_actualiza

           
         #Obtiene total de registros por cada archivo
         SELECT COUNT(*)
           INTO arr_det_archivo[z].total
           FROM afi_his_derechohabiente his,
                afi_derechohabiente afi,
                cat_afi_ind_modifica ind
          WHERE his.id_derechohabiente  = afi.id_derechohabiente
            AND his.f_modifica          = arr_archivos[f].f_actualiza
            AND his.folio_lote_modifica = arr_archivos[f].folio
            AND his.ind_modifica        = ind.ind_modifica;
            
         #Incrementa total global
         LET r_total_global.total = r_total_global.total + arr_det_archivo[z].total

         LET z = z + 1
         
      END IF 
      
   END FOR

   #Calcula Porcentaje global 
   LET v_aux_porcentaje = 0
   LET v_aux_porcentaje = (r_total_global.total / r_total_global.total) * 100
   LET r_total_global.porcentaje = v_aux_porcentaje CLIPPED,"%"

   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AFIS061.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",g_usuario CLIPPED,"-","AFIS06","-",TODAY USING "yyyymmdd",".pdf"
                       
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      IF (v_manejador_rpt IS NOT NULL) THEN

         START REPORT genera_PDF TO XML HANDLER v_manejador_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF

      END IF
   ELSE
       DISPLAY "ERROR: No fue posible abrir plantilla del reporte"
   END IF

END FUNCTION

REPORT genera_PDF()

   DEFINE v_dia   DATE
   DEFINE c       INTEGER  

   FORMAT 
   FIRST PAGE HEADER

      LET v_dia = TODAY
   
      #IMPRIME ENCABEZADO
      PRINTX g_usuario
      PRINTX v_dia   USING "dd/mm/yyyy"
      PRINTX v_f_ini USING "dd/mm/yyyy"
      PRINTX v_f_fin USING "dd/mm/yyyy"
      #Total global
      PRINTX r_total_global.total
      PRINTX r_total_global.porcentaje
      
   ON EVERY ROW 
      # Obtiene porcentajes e imprime arreglo de archivos procesados
      FOR c = 1 TO arr_det_archivo.getLength()
         IF(arr_det_archivo[c].nombre_archivo IS NOT NULL) THEN
            LET v_aux_porcentaje = 0
            LET v_aux_porcentaje = (arr_det_archivo[c].total / r_total_global.total) * 100
            LET arr_det_archivo[c].porcentaje = v_aux_porcentaje CLIPPED,"%"
         END IF 

         #Imprime
         PRINTX arr_det_archivo[c].nombre_archivo
         PRINTX arr_det_archivo[c].proceso_desc
         PRINTX arr_det_archivo[c].total
         PRINTX arr_det_archivo[c].usuario
         PRINTX arr_det_archivo[c].f_proceso USING "dd/mm/yyyy"
         PRINTX arr_det_archivo[c].porcentaje
         
      END FOR 
   
END REPORT


FUNCTION genera_archivo_salida()

   DEFINE archivo     base.Channel
   DEFINE x           INTEGER --contador para FOR
   DEFINE c           INTEGER
   DEFINE v_detalle   STRING 
   --record para salida del archivo
   DEFINE r_act_datos RECORD
      usuario          CHAR(8),
      usuario_desc     CHAR(50),
      nss              CHAR(11),
      nombre_completo  CHAR(50),
      ind_modifica     CHAR(10),
      modifica_desc    CHAR(20),
      proceso          CHAR(40),
      nombre_archivo   CHAR(40),
      f_proceso        CHAR(8)
   END RECORD
   
   DEFINE v_aux_nombre     CHAR(40)
   DEFINE v_aux_ap_paterno CHAR(40)
   DEFINE v_aux_ap_materno CHAR(40)
   

   LET v_arh_salida = v_ruta_envio CLIPPED,"/","HIS_ACT_DATOS_BATCH_",TODAY USING "yyyymmdd",".txt"
   LET archivo      = base.Channel.create()
   CALL archivo.openFile(v_arh_salida,"w")

   FOR X = 1 TO arr_archivos.getLength()
      --Si el check_box está marcado
      IF(arr_archivos[x].check_box = 1) THEN

         # Búsca información por cada archivo seleccionado
         DECLARE crs_detalle_batch CURSOR FOR 
         SELECT afi.nss,
                 afi.nombre_af,
                 afi.ap_paterno_af,
                 afi.ap_materno_af,
                 his.ind_modifica,
                 ind.ind_modifica_desc
           FROM afi_his_derechohabiente his,
                afi_derechohabiente afi,
                cat_afi_ind_modifica ind
          WHERE his.id_derechohabiente  = afi.id_derechohabiente
            AND his.f_modifica          = arr_archivos[x].f_actualiza
            AND his.folio_lote_modifica = arr_archivos[x].folio
            AND his.ind_modifica        = ind.ind_modifica
         
         INITIALIZE r_act_datos.* TO NULL

         LET v_detalle        = NULL 
         LET v_aux_nombre     = NULL 
         LET v_aux_ap_paterno = NULL 
         LET v_aux_ap_materno = NULL 

         LET c = 1
         
         FOREACH crs_detalle_batch INTO r_act_datos.nss,
                                         v_aux_nombre,    
                                         v_aux_ap_paterno,
                                         v_aux_ap_materno, 
                                         r_act_datos.ind_modifica ,
                                         r_act_datos.modifica_desc

            LET r_act_datos.usuario = arr_archivos[x].usuario
            
            #Busca descripción del usuario
            SELECT usuario_desc
              INTO r_act_datos.usuario_desc
              FROM seg_usuario
             WHERE usuario_cod = arr_archivos[x].usuario

            IF(r_act_datos.usuario_desc IS NULL) THEN
               LET r_act_datos.usuario_desc = "Infonavit" 
            END IF 
             
            LET r_act_datos.nombre_completo = v_aux_nombre CLIPPED,v_aux_ap_paterno CLIPPED,v_aux_ap_materno CLIPPED
            LET r_act_datos.proceso         = arr_archivos[x].proceso_desc
            LET r_act_datos.nombre_archivo  = arr_archivos[x].nombre_archivo
            LET r_act_datos.f_proceso       = arr_archivos[x].f_actualiza USING "ddmmyyyy"
            
            #Crea cadena de detalle
            LET v_detalle = r_act_datos.usuario,
                            r_act_datos.usuario_desc,
                            r_act_datos.nss ,
                            r_act_datos.nombre_completo  ,
                            r_act_datos.ind_modifica ,
                            r_act_datos.modifica_desc,
                            r_act_datos.proceso,
                            r_act_datos.nombre_archivo,
                            r_act_datos.f_proceso

            --Escribe en el archivo de salida
            CALL archivo.writeLine(v_detalle)
      
            LET c = c + 1
            
         END FOREACH 
   
      END IF
      
   END FOR 

   CALL archivo.close()
   
END FUNCTION
