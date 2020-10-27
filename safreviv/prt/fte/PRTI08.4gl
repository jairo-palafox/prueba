################################################################################
#Modulo           => PRT                                                       #
#Programa         => PRTI08                                                    #
#Objetivo         => REPORTE DE ACLARACIONES PORTABILIDAD                      #
#Fecha de Inicio  => NOVIEMBRE 2015                                            #
################################################################################
DATABASE "safre_viv"

    DEFINE g_pid                        DECIMAL(9,0)
    DEFINE g_proceso_cod                SMALLINT
    DEFINE g_usuario_cod                CHAR(20)
    DEFINE g_opera_cod                  SMALLINT
    DEFINE g_folio                      DECIMAL(9,0)
    DEFINE g_nom_archivo                CHAR(40)
    

MAIN

   --Array con los valores a escribir en el reporte 
   DEFINE t_consulta DYNAMIC ARRAY OF RECORD
      v_id             INTEGER,
      v_nss            CHAR(11),
      v_num_caso       DECIMAL(10,0),
      v_fecha          CHAR(10),
      v_estatus        CHAR(40),
      v_tipifica       CHAR(120),   
      v_diagnostico    CHAR(120)
   END RECORD
   --Array del filtro usado para el reporte
   DEFINE t_filtro RECORD
       v_nss       CHAR(11),
       v_num_caso  DECIMAL(10,0),
       v_estatus   CHAR(40),
       v_f_ini     CHAR(10),
       v_f_fin     CHAR(10)
   END RECORD
   DEFINE i        INTEGER
   DEFINE c        INTEGER
   
   --Variables de errores
   DEFINE r_cod_error           SMALLINT
   DEFINE r_mensaje_error       VARCHAR(255)
   DEFINE v_estado              SMALLINT

   DEFINE v_report_handler      om.SaxDocumentHandler
   DEFINE v_ruta_reporte        STRING
   DEFINE v_ruta_bin            CHAR(40)
   DEFINE v_ruta_lst            CHAR(40)
   
    --Recojiendo variables de entrdada
   LET g_usuario_cod           = ARG_VAL(1)
   LET g_pid                   = ARG_VAL(2)
   LET g_proceso_cod           = ARG_VAL(3)
   LET g_opera_cod             = ARG_VAL(4)
   LET g_folio                 = ARG_VAL(5)
   LET g_nom_archivo           = ARG_VAL(6)

   --Obteniendo la ruta del ejecutable
   SELECT l.ruta_listados,l.ruta_bin
      INTO v_ruta_lst,v_ruta_bin
      FROM seg_modulo l
      WHERE l.modulo_cod = 'prt'


   --MANEJO DE ERRORES
    WHENEVER SQLERROR CONTINUE

    CALL fn_display_proceso(0,"GENERACIÓN DE REPORTE ACLARACIONES PORTABILIDAD")
   --Se Procede a llenar el filto
   SELECT a.nss,a.num_caso,(SELECT estatus_aclaracion_desc FROM prt_cat_estatus_aclaracion WHERE estatus_aclaracion = a.estatus_aclaracion),a.f_ini,a.f_fin 
      INTO t_filtro.*
      FROM tmp_aclaracion_filtro a
   IF SQLCA.SQLCODE != 0 THEN
       DISPLAY "Error al generar el reporte: No puede acceder al filtro"
       LET r_cod_error = SQLCA.SQLCODE
       LET r_mensaje_error = SQLCA.SQLERRM
       CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   --Ajustando las fechas
   IF t_filtro.v_f_ini IS NOT NULL THEN
      LET t_filtro.v_f_ini = t_filtro.v_f_ini[4,5],"/",t_filtro.v_f_ini[1,2],"/",t_filtro.v_f_ini[7,10]
   END IF
   IF t_filtro.v_f_fin IS NOT NULL THEN
      LET t_filtro.v_f_fin = t_filtro.v_f_fin[4,5],"/",t_filtro.v_f_fin[1,2],"/",t_filtro.v_f_fin[7,10]
   END IF
    
   --Se procede a llenar el arreglo dinámico
   LET i = 1
   PREPARE prp_llena_t FROM "SELECT * FROM tmp_consulta_aclaracion_portabilidad ORDER BY 1"
   DECLARE cur_llena_t CURSOR FOR prp_llena_t
   FOREACH cur_llena_t INTO t_consulta[i].*
      LET i = i+1
   END FOREACH
   CALL t_consulta.deleteElement(i)
   LET i = i-1
   DISPLAY "REGISTROS PROCESADOS : ",i
   
   IF SQLCA.SQLCODE != 0 THEN
       DISPLAY "Error al generar el reporte: Problema con la tabla temporal de consulta"
       LET r_cod_error = SQLCA.SQLCODE
       LET r_mensaje_error = SQLCA.SQLERRM
       CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
    ELSE
       IF(fgl_report_loadCurrentSettings(v_ruta_bin CLIPPED||"/PRTI081.4rp"))THEN
         LET v_ruta_reporte = fn_nombra_reporte(v_ruta_lst,g_usuario_cod,"PRTI08",g_pid,g_proceso_cod,g_opera_cod)
         #Ruta de salida del reporte
         CALL fgl_report_setOutputFileName(v_ruta_reporte)
            # COnfigura la salida en tipo PDF
         CALL fgl_report_selectDevice("PDF")
         # Indica que no hay previsualizacion
         CALL fgl_report_selectPreview(0)
         # Asigna la configuración en el menejador del reporte
         LET v_report_handler = fgl_report_commitCurrentSettings()

         START REPORT fn_reporte_aclaracion TO XML HANDLER v_report_handler
            FOR c = 1 TO i
               OUTPUT TO REPORT fn_reporte_aclaracion(t_consulta[c].*,t_filtro.*,i)
            END FOR
         FINISH REPORT fn_reporte_aclaracion
            
       ELSE
         CALL fn_mensaje("AVISO","Ocurrió un error al cargar plantilla de reporte","information")
       END IF

       IF (r_cod_error = 0) THEN
          --########### TERMINA PROCESO SE ENVIAN DATOS AL MONITOR
          CALL fn_actualiza_opera_fin(g_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_estado
          DISPLAY "Generación de Reporte de Aclaraciones Portabilidad exitosa"
          CALL fn_display_proceso(1,"GENERACIÓN DE REPORTE ACLARACIONES PORTABILIDAD")
       ELSE
          --El uno indca que ocurrio un error al ejecutarse
          DISPLAY "\n [SAFREVIV EXCEPCION ] "
          DISPLAY "Se presentó el siguiente error de ejecución : ",r_cod_error
          DISPLAY "Descripción del error: ",r_mensaje_error,"\n"
          CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
     END IF
    END IF
    --Borrando tablas temporales del proceso PRTC08
    DROP TABLE IF EXISTS tmp_consulta_aclaracion_portabilidad
    DROP TABLE IF EXISTS tmp_aclaracion_filtro
    
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

REPORT fn_reporte_aclaracion(tabla_consulta,filtro_consulta,total_reg)
   DEFINE tabla_consulta RECORD
      v_id             INTEGER,
      v_nss            CHAR(11),
      v_num_caso       DECIMAL(10,0),
      v_fecha          CHAR(10),
      v_estatus        CHAR(40),
      v_tipifica       CHAR(120),   
      v_diagnostico    CHAR(120)
   END RECORD
   DEFINE filtro_consulta RECORD
       v_nss       CHAR(11),
       v_num_caso  DECIMAL(10,0),
       v_estatus   CHAR(40),
       v_f_ini     CHAR(10),
       v_f_fin     CHAR(10)
   END RECORD
   DEFINE total_reg INTEGER
   DEFINE f_emision CHAR(10)
   
   FORMAT 
      FIRST PAGE HEADER
         LET f_emision = TODAY
         LET f_emision = f_emision[4,5],"/",f_emision[1,2],"/",f_emision[7,10]
         PRINTX filtro_consulta.*,f_emision,total_reg
      
      ON EVERY ROW
         PRINTX tabla_consulta.*

END REPORT