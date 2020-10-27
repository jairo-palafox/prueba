######################################################################
#Modulo            => AFI                                            #
#Programa          => AFIS05                                         #
#Objetivo          => Generación de reporte que muestre el detalle   #
#                     de la operación de actualización de datos del  #
#                     derechohabiente.                               #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 23/Marzo/2018                                  #
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
   DEFINE v_aux_porcentaje    DECIMAL(6,2)
   
   DEFINE r_total_glo         RECORD
      total           INTEGER, 
      porcentaje       CHAR(12),
      total_linea      INTEGER,
      porcentaje_linea CHAR(12),
      total_batch      INTEGER,
      porcentaje_batch CHAR(12)
   END RECORD 
   
   DEFINE arr_detalle     DYNAMIC ARRAY OF RECORD
      usuario      CHAR(20),
      proceso_cod  SMALLINT,
      proceso_desc CHAR(40),
      total        INTEGER ,
      f_proceso    DATE,
      porcentaje   CHAR(12)
   END RECORD 
   
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
    
   CALL STARTLOG(g_usuario CLIPPED|| ".AFIS05.log")

   CLOSE WINDOW SCREEN

   CALL reporte_general()
   
END MAIN 

FUNCTION reporte_general()

   DEFINE v_mensaje     STRING 
   
   OPEN WINDOW vtn1 WITH FORM "AFIS051"

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
            
            # LLAMADO A FUNCIONES QUE GENERAN EL PDF Y EL ARCHIVO DE SALIDA
            CALL genera_archivo_salida() 
            CALL genera_reporte()
            
            # MENSAJE DE FINALIZACIÓN
            LET v_mensaje = "La información se ha generado correctamente \n",
                            "Archivo de salida: ",v_arh_salida CLIPPED,"\n",
                            "Reporte PDF: ",v_ruta_rpt CLIPPED 
                            
            CALL fn_mensaje("",v_mensaje,"")

            #Limpia variables
            LET v_f_ini = NULL 
            LET v_f_fin = NULL
            
         ON ACTION CANCEL 
            EXIT INPUT 
      END INPUT 
   
   CLOSE WINDOW vtn1
   
END FUNCTION

FUNCTION genera_archivo_salida()

   DEFINE v_query           STRING 
   DEFINE archivo           base.Channel
   DEFINE c                 INTEGER
   DEFINE v_detalle         STRING 
   
   DEFINE r_act_datos       RECORD
      usuario            CHAR(8),
      usuario_desc       CHAR(50),
      nss                CHAR(11),
      nombre_completo    CHAR(50),
      ind_modifica       CHAR(10),
      modifica_desc      CHAR(20),
      f_modifica         CHAR(8),
      proceso_cod        SMALLINT, 
      proceso_desc       CHAR(40),
      nombre_archivo     CHAR(40),
      f_proceso_arh      CHAR(8)
   END RECORD 

   --variables auxiliares de recuperación de datos
   DEFINE v_aux_nombre        CHAR(40)
   DEFINE v_aux_ap_paterno    CHAR(40)
   DEFINE v_aux_ap_materno    CHAR(40)
   DEFINE v_aux_f_modifica    DATE 
   DEFINE v_aux_f_proceso_arh DATE 
   DEFINE v_aux_folio         DECIMAL(9,0)
   
   LET v_arh_salida = v_ruta_envio CLIPPED,"/","HIS_ACT_DATOS_",TODAY USING "yyyymmdd",".txt"
   LET archivo      = base.Channel.create()
   CALL archivo.openFile(v_arh_salida,"w")

   LET v_query = "SELECT glo.usuario,
                     NVL(seg.usuario_desc,'Infonavit'),
                         afi.nss,
                         afi.nombre_af,
                         afi.ap_paterno_af,
                         afi.ap_materno_af,
                         his.folio_lote_modifica,
                         his.ind_modifica,
                         ind.ind_modifica_desc,
                         his.f_modifica,
                         glo.proceso_cod,
                         cat.proceso_desc
                    FROM afi_his_derechohabiente his,
                         afi_derechohabiente afi,
                         glo_folio glo,
                         cat_proceso cat,
                   OUTER seg_usuario seg,
                         cat_afi_ind_modifica ind
                   WHERE his.id_derechohabiente  = afi.id_derechohabiente
                     AND his.f_modifica >= ","'",v_f_ini,"'","
                     AND his.f_modifica <= " ,"'",v_f_fin,"'","
                     AND his.folio_lote_modifica = glo.folio
                     AND glo.proceso_cod IN (1803,1819,1820,1821,1823,1824,1825)
                     AND glo.proceso_cod  = cat.proceso_cod
                     AND glo.usuario      = seg.usuario_cod
                     AND his.ind_modifica = ind.ind_modifica;"

   PREPARE prp_act_datos FROM v_query
   DECLARE crs_act_datos CURSOR FOR prp_act_datos

   INITIALIZE r_act_datos.* TO NULL

   LET v_aux_nombre        = NULL        
   LET v_aux_ap_paterno    = NULL 
   LET v_aux_ap_materno    = NULL 
   LET v_aux_f_modifica    = NULL 
   LET v_aux_f_proceso_arh = NULL 
   LET v_aux_folio         = NULL
   LET v_detalle           = NULL 

   LET c = 1

   FOREACH crs_act_datos INTO r_act_datos.usuario,
                               r_act_datos.usuario_desc,
                               r_act_datos.nss          ,
                               v_aux_nombre,        
                               v_aux_ap_paterno, 
                               v_aux_ap_materno,
                               v_aux_folio,
                               r_act_datos.ind_modifica ,
                               r_act_datos.modifica_desc,
                               v_aux_f_modifica,
                               r_act_datos.proceso_cod,
                               r_act_datos.proceso_desc

      # Concatena nombre completo
      LET r_act_datos.nombre_completo = v_aux_nombre CLIPPED,v_aux_ap_paterno CLIPPED,v_aux_ap_materno CLIPPED
      
      # Asigna fecha de modificación del registro
      LET r_act_datos.f_modifica = v_aux_f_modifica USING "ddmmyyyy"

      # CAMBIA USUARIO A "INFONAVIT" EN CASO DE SER "SAFREVIV"
      IF(r_act_datos.usuario = "safreviv") OR (r_act_datos.usuario = "SAFREVIV") THEN
         LET r_act_datos.usuario = "INFONAVIT" 
      END IF 

      LET r_act_datos.nombre_archivo = NULL
      LET r_act_datos.f_proceso_arh  = NULL 
      
      # PROCESOS DE ACTUALIZACIÓN BATCH
      IF(r_act_datos.proceso_cod = 1819) OR (r_act_datos.proceso_cod = 1820) OR 
        (r_act_datos.proceso_cod = 1821) OR (r_act_datos.proceso_cod = 1803) OR 
        (r_act_datos.proceso_cod = 1823) THEN
        
         -- Búsca en glo_ctr_archivo, tabla donde registra la actualización BATCH
         SELECT FIRST 1 nombre_archivo,f_actualiza
           INTO r_act_datos.nombre_archivo,v_aux_f_proceso_arh
           FROM glo_ctr_archivo
          WHERE proceso_cod = r_act_datos.proceso_cod
            AND folio       = v_aux_folio
            AND f_actualiza = v_aux_f_modifica
            ORDER BY f_actualiza DESC;

         LET r_act_datos.f_proceso_arh = v_aux_f_proceso_arh USING "ddmmyyyy"
         
      END IF 

      LET v_detalle = r_act_datos.usuario,
                      r_act_datos.usuario_desc,
                      r_act_datos.nss,
                      r_act_datos.nombre_completo,
                      r_act_datos.ind_modifica,
                      r_act_datos.modifica_desc,
                      r_act_datos.f_modifica,
                      r_act_datos.proceso_desc,
                      r_act_datos.nombre_archivo,
                      r_act_datos.f_proceso_arh

      --Escribe en el archivo de salida
      CALL archivo.writeLine(v_detalle)
      
      LET c = c + 1

   END FOREACH 

   CALL archivo.close()

END FUNCTION  

FUNCTION genera_reporte()

   DEFINE v_qry_rpt   STRING
   DEFINE k           INTEGER 
   
   # Obtiene información para el reporte

   LET v_qry_rpt = "SELECT glo.usuario,
                           glo.proceso_cod,
                           cat.proceso_desc,
                           glo.f_actualiza,
                           COUNT(*)
                     FROM afi_his_derechohabiente his,
                          afi_derechohabiente afi,
                          glo_folio glo,
                          cat_proceso cat,
                          cat_afi_ind_modifica ind
                    WHERE his.id_derechohabiente  = afi.id_derechohabiente
                      AND his.f_modifica >= ","'",v_f_ini,"'","
                      AND his.f_modifica <= " ,"'",v_f_fin,"'","
                      AND his.folio_lote_modifica = glo.folio
                      AND glo.proceso_cod IN (10803,1819,1820,1821,1823,1824,1825)
                      AND glo.proceso_cod  = cat.proceso_cod
                      AND his.ind_modifica = ind.ind_modifica
                      GROUP BY 1,2,3,4;"
                      
   # Limpia Arreglo
   CALL arr_detalle.clear()
   
   # Inicializa totales globales para el reporte
   LET r_total_glo.total       = 0
   LET r_total_glo.total_linea = 0
   LET r_total_glo.total_batch = 0
   
   # En caso de no obtener registros para el detalle
   LET arr_detalle[1].usuario      = NULL
   LET arr_detalle[1].proceso_cod  = 0
   LET arr_detalle[1].proceso_desc = NULL
   LET arr_detalle[1].total        = 0
   LET arr_detalle[1].f_proceso    = NULL 
   LET arr_detalle[1].porcentaje   = 0,"%"

   PREPARE prp_det FROM v_qry_rpt
   DECLARE crs_det CURSOR FOR prp_det

   LET k = 1 

   FOREACH crs_det INTO arr_detalle[k].usuario,
                         arr_detalle[k].proceso_cod,
                         arr_detalle[k].proceso_desc,
                         arr_detalle[k].f_proceso,
                         arr_detalle[k].total

      #Suma totales al total global
      LET r_total_glo.total = r_total_glo.total + arr_detalle[k].total

      # Identifica si la actualización fue en BATCH
      IF(arr_detalle[k].proceso_cod = 1819) OR (arr_detalle[k].proceso_cod = 1820) OR 
        (arr_detalle[k].proceso_cod = 1821) OR (arr_detalle[k].proceso_cod = 1803) OR 
        (arr_detalle[k].proceso_cod = 1823) THEN 
        LET r_total_glo.total_batch    = r_total_glo.total_batch + arr_detalle[k].total
      ELSE 
         # FUÉ EN LINEA
         LET r_total_glo.total_linea   = r_total_glo.total_linea + arr_detalle[k].total
      END IF 

      # RENOMBRA EL USUARIO EN CASO DE SER "SAFREVIV"
      IF(arr_detalle[k].usuario = "SAFREVIV") OR (arr_detalle[k].usuario = "safreviv") THEN
         LET arr_detalle[k].usuario = "INFONAVIT" 
      END IF
      
      LET k = k + 1
      
   END FOREACH 

   # Elimina fila en blanco del arreglo
   IF(arr_detalle[arr_detalle.getLength()].usuario IS NULL) THEN
      CALL arr_detalle.deleteElement(arr_detalle.getLength()) 
   END IF 

   # CALCULA PROCENTAJES PARA EL TOTAL GLOBAL
   --GLOBAL
   LET v_aux_porcentaje = 0
   LET v_aux_porcentaje = (r_total_glo.total / r_total_glo.total) * 100
   LET r_total_glo.porcentaje = v_aux_porcentaje CLIPPED,"%"
   # Porcentaje en LINEA
   LET v_aux_porcentaje = (r_total_glo.total_linea / r_total_glo.total) * 100
   LET r_total_glo.porcentaje_linea = v_aux_porcentaje CLIPPED,"%"
   # Porcentaje en BATCH
   LET v_aux_porcentaje = (r_total_glo.total_batch / r_total_glo.total) * 100
   LET r_total_glo.porcentaje_batch = v_aux_porcentaje CLIPPED,"%"
   
   #################################################
   #   CONFIGURACIÓN PARA SALIDA DEL REPORTE PDF   #
   #################################################

   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AFIS051.4rp"
   LET v_ruta_rpt    = v_ruta_listados CLIPPED,"/",g_usuario CLIPPED,"-","AFIS05","-",TODAY USING "yyyymmdd",".pdf"
                       
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
   DEFINE f       INTEGER  

   FORMAT 
   FIRST PAGE HEADER

      LET v_dia = TODAY
   
      #IMPRIME ENCABEZADO
      PRINTX g_usuario
      PRINTX v_dia   USING "dd/mm/yyyy"
      PRINTX v_f_ini USING "dd/mm/yyyy"
      PRINTX v_f_fin USING "dd/mm/yyyy"
      #Total global
      PRINTX r_total_glo.total
      PRINTX r_total_glo.porcentaje
      PRINTX r_total_glo.total_linea
      PRINTX r_total_glo.porcentaje_linea
      PRINTX r_total_glo.total_batch
      PRINTX r_total_glo.porcentaje_batch 
      
   ON EVERY ROW 
      # Obtiene porcentajes e imprime arreglo
      FOR f = 1 TO arr_detalle.getLength()
         IF(arr_detalle[f].usuario IS NOT NULL) THEN
            LET v_aux_porcentaje = 0
            LET v_aux_porcentaje = (arr_detalle[f].total / r_total_glo.total) * 100
            LET arr_detalle[f].porcentaje = v_aux_porcentaje CLIPPED,"%"
         END IF 

         #Imprime
         PRINTX arr_detalle[f].usuario
         PRINTX arr_detalle[f].proceso_desc
         PRINTX arr_detalle[f].total
         PRINTX arr_detalle[f].f_proceso USING "dd/mm/yyyy"
         PRINTX arr_detalle[f].porcentaje
         
      END FOR 
   
END REPORT
