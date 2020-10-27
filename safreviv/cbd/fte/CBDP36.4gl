##############################################################
#MODULO             CBD                                      #
#PROGRAMA           CBDP36                                   #
#OBJETIVO           PROGRAMA QUE EJECUTA LA INTEGRACION DEL  #
#                   AJUSTE DE SALDO PROCESO 2116             #
#FECHA              septiembre 20015                         #
##############################################################

DATABASE safre_viv
DEFINE g_pid decimal(9,0)
DEFINE g_proceso_cod SMALLINT 
DEFINE g_opera_cod SMALLINT 
DEFINE g_folio DECIMAL(9,0)
DEFINE g_archivo char(40)
DEFINE g_usuario_cod char(20)

DEFINE g_lista_cifras DYNAMIC ARRAY OF RECORD 
    folio decimal(9,0),
    subcuenta smallint,
    fondo_inversion smallint,
    total_acciones decimal(26,6),
    total_pesos     DECIMAL (22,2),
    total_registros DECIMAL (9,0)
    
END record

MAIN 
    DEFINE v_estado SMALLINT 
    LET g_usuario_cod=arg_val(1)
    LET g_pid=arg_val(2)
    LET g_proceso_cod = arg_val(3)
    LET g_opera_cod = arg_val(4)
    LET g_folio = arg_val(5)
    LET g_archivo=arg_val(6)

    CALL fn_display_proceso(0,"Inicia integración")
    CALL fn_integra_archivo() RETURNING v_estado

    IF v_estado = 0 THEN 
    
        CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
        DISPLAY "Integracion realizada con exito"

        UPDATE glo_ctr_archivo
        SET folio=g_folio,
        estado=2
        WHERE nombre_archivo=g_archivo 

       CALL  fn_carga_arr()

       CALL fn_genera_reporte()
        
    ELSE 
        DISPLAY "El proceso de la validación ha finalizado pero con excepciones, verificar en el sistema saci.\nVer"
        CALL fn_error_opera(g_pid, g_proceso_cod,g_opera_cod) RETURNING v_estado
        UPDATE glo_ctr_archivo
            SET folio=g_folio
            WHERE nombre_archivo=g_archivo

            UPDATE glo_ctr_archivo
            SET estado=3
            where folio=g_folio
    END IF
    CALL fn_display_proceso(1,"Inicia integración")


END MAIN 

FUNCTION fn_carga_arr()
    DEFINE i        integer
     prepare prp_cifras  FROM  "SELECT subcuenta,
                                       fondo_inversion,total_acciones,
                                       total_pesos,
                                       total_registros
                            FROM cbd_cifras_ajuste_saldo where folio=?" 
    DECLARE cur_cifras CURSOR FOR prp_cifras
    LET i=1
    FOREACH cur_cifras USING g_folio INTO g_lista_cifras[i].subcuenta,
                                            g_lista_cifras[i].fondo_inversion,
                                            g_lista_cifras[i].total_acciones,
                                            g_lista_cifras[i].total_pesos,
                                            g_lista_cifras[i].total_registros 
        LET i=i+1
    END FOREACH 
END FUNCTION 

FUNCTION fn_integra_archivo()
     DEFINE v_sql_procedure STRING 
     DEFINE v_status    SMALLINT 
     DEFINE r_cod_error SMALLINT 
     DEFINE r_error_isam INTEGER 
     DEFINE r_mensaje_error varchar(255)

     LET v_sql_procedure= "EXECUTE FUNCTION fn_cbd_integra_archivo_2116(?,?)"
    PREPARE prp_procedure FROM v_sql_procedure
   EXECUTE  prp_procedure USING g_folio, g_usuario_cod 
                            INTO r_cod_error, r_error_isam,r_mensaje_error
    IF r_cod_error = 0 THEN 
        DISPLAY ""
        DISPLAY r_mensaje_error
    ELSE 
        DISPLAY "#######Exception safre######"
        DISPLAY "Codigo error ", r_cod_error
        DISPLAY "error isam ", r_error_isam
        DISPLAY "Detalle ", r_mensaje_error
        LET v_status=1
    END if
    RETURN v_status
END FUNCTION 


 PRIVATE  FUNCTION fn_genera_reporte()
   DEFINE reporte           om.SaxDocumentHandler
   DEFINE i                 SMALLINT
    DEFINE v_reporte        STRING
    DEFINE v_ruta_listados  char(40)
    DEFINE v_ruta_reporte   STRING

   LET v_reporte= "CBDP36.4rp"

    SELECT ruta_listados
    INTO v_ruta_listados
    FrOM seg_modulo
    WHERE modulo_cod="cbd"

    LET v_ruta_reporte=v_ruta_listados CLIPPED , "/" ,
                      g_usuario_cod CLIPPED, "-", 
                      "CBDP36","-",
                      g_pid USING "&&&&&", "-",
                      g_proceso_cod USING "&&&&&", "-",
                      g_opera_cod USING "&&&&&",".pdf" 
   
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      
      IF reporte IS NOT NULL THEN
         START REPORT cifras TO XML HANDLER reporte
            FOR i = 1 TO g_lista_cifras.getLength()-1
               OUTPUT TO REPORT cifras(g_lista_cifras[i].*)
               
            END FOR 
         FINISH REPORT cifras
      END IF
   END IF
END FUNCTION

REPORT cifras(p_lista)

    DEFINE p_lista RECORD 
        folio decimal(9,0),
        subcuenta smallint,
        fondo_inversion smallint,
        total_acciones decimal(26,6),
        total_pesos     DECIMAL (22,2),
        total_registros DECIMAL (9,0)
    
    END record


   DEFINE v_today     DATE
   
   ORDER BY p_lista.subcuenta , p_lista.fondo_inversion

   FORMAT

   FIRST PAGE HEADER

      LET v_today = TODAY
      
      PRINTX   g_folio,
               g_archivo,
               v_today USING "DD-MM-YYYY"

   ON EVERY ROW
      PRINTX   p_lista.*

   
      
END REPORT
