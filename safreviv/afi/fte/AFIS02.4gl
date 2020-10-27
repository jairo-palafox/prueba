--=============================================================================
##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIS02                                                                          #
#Objetivo     => Generación de archivo para incosistencias de archivo afiliatorio ims            #
#Fecha inicio =>                                                                                 #
##################################################################################################


DATABASE safre_viv
 

DEFINE        v_query         STRING      --para obtener datos de tabla
DEFINE        f_inicial       DATE        --fecha inicial
DEFINE        f_final         DATE        --fecha final
DEFINE        v_pid           DECIMAL(9,0)
DEFINE        g_proceso_cod   INTEGER
DEFINE        g_opera_cod     INTEGER
DEFINE        g_usuario       CHAR(20)
DEFINE        v_nombre_archivo STRING     -- variable de direccion para generacion de archivo 
DEFINE        v_estado        SMALLINT
DEFINE        v_ruta_envio    LIKE seg_modulo.ruta_envio
DEFINE        v_consulta      STRING


MAIN

   --Obtiene ruta envio de afi
    SELECT ruta_envio
    INTO v_ruta_envio
    FROM seg_modulo
    WHERE modulo_cod = 'afi'

    -- parametros que vienen de lanzador
    LET g_usuario       = ARG_VAL (1)
    LET v_pid           = ARG_VAL (2)
    LET g_proceso_cod   = ARG_VAL (3)
    LET g_opera_cod     = ARG_VAL (4) 
    LET f_inicial       = ARG_VAL (5)
    LET f_final         = ARG_VAL (6)
    LET v_nombre_archivo = v_ruta_envio CLIPPED ,"/Incons_",TODAY USING "DDMMYYYY",".rsin"
    LET v_estado = 0

    CALL fn_display_proceso(0,"GENERA ARCHIVO DE INCONSISTENCIAS")
    DISPLAY ""
    DISPLAY "FECHAS DE BÚSQUEDA"
    DISPLAY "Fecha Inicial: ", f_inicial USING "DD-MM-YYYY"
    DISPLAY "Fecha Final:   ", f_final USING "DD-MM-YYYY"
    DISPLAY ""
   CALL fn_genera_archivo()

   IF v_estado = 0 THEN

      DISPLAY ""
      DISPLAY "PROCESO EJECUTADO CORRECTAMENTE..."
      DISPLAY ""
      DISPLAY "El archivo fue generado en ", v_nombre_archivo

      CALL fn_actualiza_opera_fin(v_pid,
                                 g_proceso_cod,
                                 g_opera_cod)
                                 RETURNING v_estado

   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, g_proceso_cod, g_opera_cod)  RETURNING v_estado
   END IF
   --Se ejecutan los displays
   CALL fn_display_proceso(1,"GENERA ARCHIVO INCONSISTENCIAS")
 
END MAIN

FUNCTION fn_genera_archivo ()

    -- valores generales para v_query
    LET v_consulta = "
                    SELECT  afi.nss ,
                            ris.nrp ,
                            lpad(day(ris.f_movimiento),2,0)||'/'||
                            lpad(month(ris.f_movimiento),2,0)||'/'||
                            year(ris.f_movimiento)
                            as f_movimiento ,
                            lpad(day(ris.f_proceso),2,0)||'/'||
                            lpad(month(ris.f_proceso),2,0)||'/'||
                            year(ris.f_proceso)
                            as f_proceso,
                            ris.id_riss_rl,
                            c.desc_riss_rl
                    FROM    afi_riss ris, afi_derechohabiente afi,
                            cat_riss_rl c
                    WHERE   ris.id_derechohabiente = afi.id_derechohabiente
                    AND     ris.id_riss in (1,2,3)
                    AND     ris.id_riss_rl = c.id_riss_rl
                    AND     ris.id_riss_rl IN ( 0,3,4,5)"
                    
    --si solo existe fecha final,se realiza carga de todos los datos anteriores o iguales a fecha final
    IF f_inicial IS NULL AND f_final IS NOT NULL THEN
        LET v_query = v_consulta , " AND ris.f_proceso <=", "'",f_final,"'"
    END IF

    --si solo existe fecha inicial,se genera carga de datos mayores o iguales a fecha inicial
    IF f_inicial IS NOT NULL AND  f_final IS NULL THEN
        LET v_query = v_consulta , " AND ris.f_proceso >= ", "'",f_inicial,"'"
    END IF

    --carga archivos entre fecha inical y final ingresadas
    IF f_inicial IS NOT NULL AND f_final IS NOT NULL THEN
        LET v_query = v_consulta , " AND ris.f_proceso BETWEEN '", f_inicial, "' and '", f_final,"'"
    END IF 

    IF f_inicial IS NULL AND f_final IS NULL THEN
        LET v_query = v_consulta
    END IF


UNLOAD TO  v_nombre_archivo v_query
CALL fn_genera_reporte()
END FUNCTION


FUNCTION fn_genera_reporte()

   DEFINE v_reporte           STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_ruta_reporte      STRING
   DEFINE v_excepcion         SMALLINT
   DEFINE v_query_reporte     STRING
   DEFINE v_consulta_reporte  STRING
   --DEFINE a                   INTEGER

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte         RECORD
          descripcion         CHAR (40),
          cantidad            INTEGER
   END RECORD

   LET v_reporte = "AFIS02.4rp"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AFIS02", "-", -- programa
                        v_pid USING "&&&&&","-", -- PID
                        g_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        g_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN


      LET v_query_reporte = 'SELECT  c.desc_riss_rl,
                                     count(*)
                             FROM    afi_riss ris,
                                     cat_riss_rl c
                             WHERE   ris.id_riss in (1,2,3)
                             AND     ris.id_riss_rl = c.id_riss_rl
                             AND     ris.id_riss_rl IN ( 0,3,4,5)'

   --si solo existe fecha final,se realiza carga de todos los datos anteriores o iguales a fecha final
       IF f_inicial IS NULL AND f_final IS NOT NULL THEN
           LET v_consulta_reporte = v_query_reporte , " AND ris.f_proceso <=", "'",f_final,"'","GROUP BY 1 ORDER BY 1,2"
    END IF

   --si solo existe fecha inicial,se genera carga de datos mayores o iguales a fecha inicial
       IF f_inicial IS NOT NULL AND  f_final IS NULL THEN
           LET v_consulta_reporte = v_query_reporte , " AND ris.f_proceso >= ", "'",f_inicial,"'","GROUP BY 1 ORDER BY 1,2"
    END IF

    --carga archivos entre fecha inical y final ingresadas
       IF f_inicial IS NOT NULL AND f_final IS NOT NULL THEN
           LET v_consulta_reporte = v_query_reporte , " AND ris.f_proceso BETWEEN '", f_inicial, "' and '", f_final,"'","GROUP BY 1 ORDER BY 1,2"
       END IF

       IF f_inicial IS NULL AND f_final IS NULL THEN
          LET v_consulta_reporte = v_query_reporte,"GROUP BY 1 ORDER BY 1,2"
       END IF

      PREPARE prp_reporte FROM v_consulta_reporte
      DECLARE cur_resultados CURSOR FOR  prp_reporte

      START REPORT rep_resultados TO XML HANDLER report_handler

      --LET a = 1

      FOREACH cur_resultados INTO r_reporte.*

         --LET a = a +1
            OUTPUT TO REPORT rep_resultados(r_reporte.*)

      END FOREACH

      FINISH REPORT rep_resultados

    END IF
END FUNCTION

REPORT rep_resultados(p_reporte)
   
   DEFINE p_reporte           RECORD
          descripcion         CHAR (40),
          cantidad            INTEGER
   END RECORD

   DEFINE v_fecha_reporte     DATE
   DEFINE m_v_usuario         CHAR (20)
   DEFINE v_total_general     STRING
   DEFINE v_total_general_reporte RECORD 
          total_general       INTEGER
    END RECORD
   DEFINE v_cant_total STRING

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY
         LET m_v_usuario = g_usuario

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX m_v_usuario

      ON EVERY ROW 
         PRINTX p_reporte.*

      ON LAST ROW
      
         --PRINTX p_reporte.*

         LET v_total_general = 'SELECT count(*)
                                 FROM  afi_riss ris,
                                       cat_riss_rl c
                                 WHERE ris.id_riss in (1,2,3)
                                   AND ris.id_riss_rl = c.id_riss_rl
                                   AND ris.id_riss_rl IN ( 0,3,4,5)'

         IF f_inicial IS NULL AND f_final IS NOT NULL THEN
            LET v_cant_total = v_total_general , " AND ris.f_proceso <=", "'",f_final,"'"
         END IF
    --si solo existe fecha inicial,se genera carga de datos mayores o iguales a fecha inicial    
         IF f_inicial IS NOT NULL AND  f_final IS NULL THEN
            LET v_cant_total = v_total_general , " AND ris.f_proceso >= ", "'",f_inicial,"'"
        END IF
    --carga archivos entre fecha inical y final ingresadas
         IF f_inicial IS NOT NULL AND f_final IS NOT NULL THEN
            LET v_cant_total = v_total_general , " AND ris.f_proceso BETWEEN '", f_inicial, "' and '", f_final,"'"
         END IF 

         IF f_inicial IS NULL AND f_final IS NULL THEN
            LET v_cant_total = v_total_general
         END IF

         DECLARE cur_total CURSOR FROM v_cant_total
         FOREACH cur_total INTO v_total_general_reporte.*
         END FOREACH

         PRINTX v_total_general_reporte.*

END REPORT