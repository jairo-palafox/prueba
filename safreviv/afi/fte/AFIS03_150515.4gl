--=============================================================================
##################################################################################################
#Modulo       => AFI                                                                             #
#Programa     => AFIS02                                                                          #
#Objetivo     => Generación de reporte consultas rojas                                           #
#Fecha inicio =>                                                                                 #
##################################################################################################


DATABASE safre_viv
 

DEFINE        f_inicial       DATE        --fecha inicial
DEFINE        f_final         DATE        --fecha final
DEFINE        g_usuario       CHAR(20) 
DEFINE        p_nom_ventana   STRING  
DEFINE        p_tpo_ejecucion SMALLINT  

MAIN


    LET g_usuario       = ARG_VAL (1)
    LET p_tpo_ejecucion = ARG_val (2)
    LET p_nom_ventana   = ARG_VAL (3)

    OPEN WINDOW AFIS03 WITH FORM "AFIS031"
    CALL ui.Interface.setText ( p_nom_ventana )

    --se piden fechas a consultar
    INPUT BY NAME   f_inicial,
                    f_final  ATTRIBUTES ( UNBUFFERED )

    ON ACTION ACCEPT                 
      CALL fn_genera_reporte()
      CALL fn_mensaje("Mensaje","El reporte fue creado correctamente","information")
      EXIT INPUT
   ON ACTION CANCEL
      EXIT INPUT 
   END INPUT 
END MAIN



FUNCTION fn_genera_reporte()

   DEFINE v_reporte           STRING
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_ruta_reporte      STRING
   DEFINE v_excepcion         SMALLINT
   DEFINE v_query_reporte     STRING
   DEFINE v_consulta_reporte  STRING
   DEFINE v_cuenta            INTEGER
   DEFINE v_dia               CHAR (8)
   --DEFINE a                   INTEGER

   DEFINE report_handler      om.SaxDocumentHandler

   DEFINE r_reporte    RECORD
          usuario         CHAR (20),
          nss             CHAR (11),
          derechohabiente CHAR (60),
          tpo_consulta    CHAR (50),
          f_consulta      DATE,
          h_consulta      DATETIME HOUR TO SECOND
   END RECORD

   LET v_reporte = "AFIS03.4rp"
   LET v_dia = TODAY USING "DDMMYYYY"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'afi'

   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                        g_usuario CLIPPED , "-", -- usuario
                        "AFIS03", "-",-- programa
                        v_dia,
                        ".pdf"
DISPLAY v_dia
DISPLAY v_ruta_reporte
   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_selectPreview(1)
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      LET report_handler = fgl_report_commitCurrentSettings()      -- commit the file settings
   ELSE
      DISPLAY "[ SAFRE EXCEPCIÓN ] NO SE ENCUENTRA LA PLANTILLA PARA GENERAR EL REPORTE: ", v_reporte
      LET v_excepcion = 1
   END IF
   IF NOT v_excepcion THEN


      LET v_query_reporte = 'SELECT  a.usuario,
                                     a.nss,
                                     trim(ap_paterno_af)||" "||
                                     trim(ap_materno_af)||" "||
                                     trim(nombre_af),
                                     CASE WHEN tpo_consulta = 1 THEN "Datos generales"
                                     WHEN tpo_consulta = 2 THEN "Saldo régimen actual"
                                     WHEN tpo_consulta = 3 THEN "Saldo decreto"
                                     WHEN tpo_consulta = 4 THEN "Saldo Fondo anterior"
                                     WHEN tpo_consulta = 5 THEN "Marcas"
                                     ELSE "No especificado" END,
                                     a.f_consulta,
                                     a.h_consulta
                             FROM    afi_log_consulta a,afi_derechohabiente b
                             WHERE   a.nss = b.nss'

   --si solo existe fecha final,se realiza carga de todos los datos anteriores o iguales a fecha final
       IF f_inicial IS NULL AND f_final IS NOT NULL THEN

          SELECT COUNT (*)
            INTO v_cuenta
            FROM afi_log_consulta
           WHERE f_consulta <= f_final

            IF v_cuenta <> 0 THEN 
            
              LET v_consulta_reporte = v_query_reporte , " and f_consulta <=", "'",f_final,"'"
           ELSE 
              CALL fn_mensaje("Mensaje","No existen datos con el criterio de búsqueda","information")
           END IF
      END IF
      

   --si solo existe fecha inicial,se genera carga de datos mayores o iguales a fecha inicial
       IF f_inicial IS NOT NULL AND  f_final IS NULL THEN

          SELECT COUNT (*)
            INTO v_cuenta
            FROM afi_log_consulta
           WHERE f_consulta >= f_inicial

            IF v_cuenta <> 0 THEN 
            
              LET v_consulta_reporte = v_query_reporte , " and f_consulta >=", "'",f_inicial,"'"
           ELSE 
              CALL fn_mensaje("Mensaje","No existen datos con el criterio de busqueda","information")
           END IF
        END IF

    --carga archivos entre fecha inical y final ingresadas
       IF f_inicial IS NOT NULL AND f_final IS NOT NULL THEN

       SELECT COUNT (*)
            INTO v_cuenta
            FROM afi_log_consulta
           WHERE (f_consulta BETWEEN f_inicial AND f_final)

            IF v_cuenta <> 0 THEN
              LET v_consulta_reporte = v_query_reporte , " and f_consulta BETWEEN '", f_inicial, "' and '", f_final,"'"
           ELSE 
              CALL fn_mensaje("Mensaje","No existen datos con el criterio de busqueda","information")
           END IF
        END IF

       IF f_inicial IS NULL AND f_final IS NULL THEN

       SELECT COUNT (*)
            INTO v_cuenta
            FROM afi_log_consulta

            IF v_cuenta <> 0 THEN
               LET v_consulta_reporte = v_query_reporte
        ELSE
           CALL fn_mensaje("Mensaje","No existen datos con el criterio de busqueda","information")
       END IF
    END IF

      PREPARE prp_reporte FROM v_consulta_reporte
      DECLARE cur_resultados CURSOR FOR  prp_reporte

      START REPORT rep_resultados TO XML HANDLER report_handler


      FOREACH cur_resultados INTO r_reporte.*


            OUTPUT TO REPORT rep_resultados(r_reporte.*)

      END FOREACH

      FINISH REPORT rep_resultados

    END IF
END FUNCTION

REPORT rep_resultados(p_reporte)
   
   DEFINE p_reporte     RECORD
          usuario         CHAR (20),
          nss             CHAR (11),
          derechohabiente CHAR (60),
          tpo_consulta    CHAR (50),
          f_consulta      DATE,
          h_consulta      DATETIME HOUR TO SECOND
   END RECORD

   DEFINE v_fecha_reporte     DATE
   DEFINE m_v_usuario         CHAR (20)

   FORMAT

      FIRST PAGE HEADER

         LET v_fecha_reporte = TODAY
         LET m_v_usuario = g_usuario

         PRINTX v_fecha_reporte USING "DD-MM-YYYY"
         PRINTX m_v_usuario

      ON EVERY ROW 
         PRINTX p_reporte.*
        

END REPORT