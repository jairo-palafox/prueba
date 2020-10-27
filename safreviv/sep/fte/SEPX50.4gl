################################################################################
#Modulo           => SEP                                                       #
#Programa         => SEPX50                                                    #
#Objetivo         => RESUMEN EXTRACTOR CUENTAS MARCADAS EN PROCESO DE SEPARACIÓN DE CUENTAS                       #
#Fecha de Inicio  => ABRIL 2015                                                #
################################################################################
SCHEMA safre_viv

GLOBALS
    DEFINE g_archivo_envio   CHAR(30)
    DEFINE g_ruta_seg_modulo CHAR(255)
    DEFINE g_ruta_listados   CHAR(255)
    DEFINE g_big_total       INTEGER

    DEFINE g_pid                        DECIMAL(9,0)
    DEFINE g_proceso_cod                SMALLINT
    DEFINE g_usuario_cod                CHAR(20)
    DEFINE g_opera_cod                  SMALLINT
    DEFINE g_folio                      DECIMAL(9,0)
    DEFINE g_nom_archivo                CHAR(40)
    DEFINE g_resultado_opera            SMALLINT
END GLOBALS

MAIN
    --Variables de errores
   DEFINE v_estado              SMALLINT
   DEFINE r_cod_error           SMALLINT
   DEFINE r_mensaje_error       VARCHAR(255)

   --Variables del proceso
   DEFINE f om.SaxDocumentHandler
   DEFINE i  INTEGER
   DEFINE v_ejecuta_sp      STRING
   DEFINE v_crea_tabla      STRING
   DEFINE v_inserta_tabla   STRING
   DEFINE v_borra_tabla     STRING
   DEFINE v_ruta_completa   STRING
   DEFINE v_ruta_seg_modulo STRING 
   DEFINE v_ruta_reporte    STRING

    --Array con los valores a escribir en el archivo plano
    DEFINE v_sp DYNAMIC ARRAY OF RECORD
       v_nss_invadido      CHAR(11),
       v_afore_nss         CHAR(3),
       v_fecha_de_marca    CHAR(8),
       v_apellido_paterno  CHAR(40),
       v_apellido_materno  CHAR(40),
       v_nombre            CHAR(40),
       v_curp_nss_invadido CHAR(18),
       v_diagnostico       CHAR(2),
       v_clasificacion     CHAR(1),
       v_credito_infonavit CHAR(1),
       v_nss_asociado      CHAR(11),
       v_estado            CHAR(10)
    END RECORD

    --Recojiendo variables de entrdada
   LET g_usuario_cod           = ARG_VAL(1)
   LET g_pid                   = ARG_VAL(2)
   LET g_proceso_cod           = ARG_VAL(3)
   LET g_opera_cod             = ARG_VAL(4)
   LET g_folio                 = ARG_VAL(5)
   LET g_nom_archivo           = ARG_VAL(6)

        --QUERYS A EJECUTAR
    LET v_inserta_tabla = "INSERT INTO tmp_sep_op27 VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
    LET v_ejecuta_sp = "EXECUTE PROCEDURE sp_sep_extraccion_marcas_separacion()"
    LET v_borra_tabla = "DROP TABLE IF EXISTS tmp_sep_op27;"
    LET v_crea_tabla ="
    DROP TABLE IF EXISTS tmp_sep_op27;
    CREATE TEMP TABLE tmp_sep_op27(
       v_nss_invadido      CHAR(11),
       v_afore_nss         CHAR(3),
       v_fecha_de_marca    CHAR(8),
       v_apellido_paterno  CHAR(40),
       v_apellido_materno  CHAR(40),
       v_nombre            CHAR(40),
       v_curp_nss_invadido CHAR(18),
       v_diagnostico       CHAR(2),
       v_clasificacion     CHAR(1),
       v_credito_infonavit CHAR(1),
       v_nss_asociado      CHAR(11),
       v_estado            CHAR(10)
    );"

    --######### 
    CONNECT TO "safre_viv"
    
    CALL fn_display_proceso(0,"EXTRACCION MARCAS SEPARACION") 

    --MANEJO DE ERRORES
    WHENEVER SQLERROR CONTINUE
    --CREANDO LA TABLA TEMPORAL
    EXECUTE IMMEDIATE v_crea_tabla

    --Inserta datos en la tabla temporal
    PREPARE prp_llena_tabla FROM v_inserta_tabla
    --SE extraen los registros del STORED PROCEDURE y se insertan en la tabla temporal
    PREPARE prp_procedure FROM v_ejecuta_sp
    DECLARE cursor_procedure CURSOR FOR prp_procedure
    LET i = 1
    FOREACH cursor_procedure INTO v_sp[i].*
        DECLARE cursor_tabla CURSOR FOR prp_llena_tabla
        OPEN cursor_tabla
            PUT cursor_tabla FROM v_sp[i].*
            LET i = i + 1
        FLUSH cursor_tabla
        CLOSE cursor_tabla
    END FOREACH
    LET g_big_total = i-1
    DISPLAY "REGISTROS PROCESADOS : ",g_big_total
    --Manejo de error
    IF SQLCA.SQLCODE != 0 THEN
       DISPLAY "Error al ejecutar Stored Procedure"
       LET r_cod_error = SQLCA.SQLCODE
       LET r_mensaje_error = SQLCA.SQLERRM
    ELSE 
       DISPLAY "Stored Procedure ejecutado sin errores"
       --Definiendo la ruta de envío del archivo desde la tabla asi como su nombre 
       SELECT ruta_envio,ruta_listados INTO g_ruta_seg_modulo,g_ruta_listados FROM seg_modulo WHERE modulo_cod = "sep"
       LET v_ruta_seg_modulo = g_ruta_seg_modulo CLIPPED
       LET g_archivo_envio = "marcadasSep",TODAY USING "ddmmyyyy",".exct"
       LET v_ruta_completa = v_ruta_seg_modulo||"/"||g_archivo_envio
       
       --Ahora se procede a enviar el archivo desde la funcion UNLOAD
       UNLOAD TO v_ruta_completa SELECT * FROM tmp_sep_op27 ORDER BY 4
      --manejo de errores
       IF SQLCA.SQLCODE != 0 THEN
          DISPLAY "Error al generar archivo de resultados"
          LET r_cod_error = SQLCA.SQLCODE
          LET r_mensaje_error = SQLCA.SQLERRM
       ELSE 
          DISPLAY "Archivo generado sin errores en la siguiente ubicacion: ",v_ruta_completa
          --Ejecutamos el reporte
          IF fgl_report_loadCurrentSettings("SEPX50.4rp") THEN
              CALL fgl_report_selectDevice("PDF")
              CALL fgl_report_selectPreview(FALSE)     -- loaded OK
                  --Nombrando reporte con funcion para que el monitor de procesos lo reconozca
              LET  v_ruta_reporte = fn_nombra_reporte(g_ruta_listados,g_usuario_cod,"SEPX50",g_pid,g_proceso_cod,g_opera_cod)
              CALL fgl_report_setOutputFileName(v_ruta_reporte)

              LET f = fgl_report_commitCurrentSettings()
              CALL run_report1_to_handler(f,g_big_total,g_ruta_seg_modulo,g_archivo_envio)
                    RETURNING r_cod_error,r_mensaje_error
              IF r_cod_error = 0 THEN
                 DISPLAY "Reporte generado sin errores en la siguiente ubicacion: ",v_ruta_reporte
                 LET r_cod_error = 0
                 LET v_estado = 0 
              ELSE 
                 DISPLAY "Error al generar el reporte"
              END IF
           END IF
       END IF
    END IF
   
   --Se elimina la tabla temporal al terminar el proceso
   EXECUTE IMMEDIATE v_borra_tabla

   --
   --"INSERT INTO glo_ctr_archivo
   --     VALUES (g_proceso_cod, 1,?, g_folio, 2, TODAY, g_usuario)"
   -- g_proceso_cod,v_ruta_reporte,g_folio,g_usuario
        
   IF (r_cod_error = 0) THEN
      --########### TERMINA PROCESO SE ENVIAN DATOS AL MONITOR
      CALL fn_actualiza_opera_fin(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_estado
      DISPLAY "Extraccion de marcas separacion ejecutada con exito."
      CALL fn_display_proceso(1,"EXTRACCION MARCAS SEPARACION") 
   ELSE
 --El uno indca que ocurrio un error al ejecutarse
      DISPLAY "\n [SAFREVIV EXCEPCION ] "
      DISPLAY "Se presentó el siguiente error de ejecución : ",r_cod_error
      DISPLAY "Descripción del error: ",r_mensaje_error,"\n"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   
DISCONNECT ALL
END MAIN

FUNCTION fn_nombra_reporte(p_ruta,p_usuario,p_programa,p_pid,p_proceso,p_operacion)

DEFINE  p_ruta      CHAR(255),
        p_usuario   CHAR(20),
        p_programa  CHAR(10),
        p_pid       DECIMAL(9,0),
        p_proceso   SMALLINT,
        p_operacion SMALLINT,
        p_salida    CHAR(255)

        LET p_salida = p_ruta CLIPPED,"/",p_usuario CLIPPED,"-",p_programa CLIPPED,"-",p_pid USING "&&&&&","-",p_proceso USING "&&&&&","-",p_operacion USING "&&&&&",".pdf"
        RETURN p_salida
        
END FUNCTION
