--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETF02                                                                 #
#OBJETIVO     => REPORTE DE CONSULTA DE DETALLE DEL PROGRAMA RETF02                     #
#Fecha inicio => Diciembre 18, 2014                                                     #
#Modificacion =>                                                                        #
#Autor        => Maria de Lourdes Padilla Moreno                                        #
#########################################################################################
{

DATABASE safre_viv

GLOBALS

   DEFINE g_ar_combinado        DYNAMIC ARRAY OF RECORD
            id_tipo_retiro     SMALLINT,
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_fondo_ahorro.f_solicitud,
            pes_viv72          DECIMAL (19,6),
            pes_viv49          DECIMAL (19,6),
            aivs_viv92         DECIMAL (19,6),
            aivs_viv97         DECIMAL (19,6),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18)
       END RECORD
    DEFINE g_usuario_cod  LIKE seg_usuario.usuario_cod 
END GLOBALS


FUNCTION fn_genera_reporte()
   
   DEFINE v_ruta_listados     CHAR(40)
   DEFINE v_reporte           STRING
   DEFINE v_ruta_reporte      STRING
   DEFINE report_handler      om.SaxDocumentHandler
   DEFINE v_excepcion         SMALLINT  
   DEFINE i                   SMALLINT

   
    LET v_reporte = "RETF026.4rp"
    LET i = 1   --Inicializa variable para que recorra arreglo
    ---
    SELECT ruta_listados
        INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = 'ret'

    ##Obtengo la ruta donde lo voy a poner 
    IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETF026.4rp") ) THEN 
      CALL fgl_report_selectDevice ("PDF")        
      LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","Consultas_generales_detalle"                
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      CALL fgl_report_selectPreview(1)
      LET report_handler = fgl_report_commitCurrentSettings()
    ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla Reporte11_1.4rp", "stop")      
    END IF 

    --DISPLAY "size", g_ar_combinado.getLength()
    IF NOT v_excepcion THEN
      START REPORT rpt_modulo TO XML HANDLER report_handler
      FOR i = 1 TO g_ar_combinado.getLength()
         OUTPUT TO REPORT rpt_modulo(g_ar_combinado[i].*,g_usuario_cod)
      END FOR      
        
      FINISH REPORT rpt_modulo
    END IF
END FUNCTION


REPORT rpt_modulo(p_combinado,g_usuario)

    DEFINE p_combinado        RECORD
            id_tipo_retiro     SMALLINT,
            id_solicitud       DECIMAL(9,0),
            id_derechohabiente DECIMAL(9,0),
            nss                CHAR(20),
            f_solicitud        LIKE ret_fondo_ahorro.f_solicitud,
            pes_viv72          DECIMAL (19,6),
            pes_viv49          DECIMAL (19,6),
            aivs_viv92         DECIMAL (19,6),
            aivs_viv97         DECIMAL (19,6),
            estado_solicitud   CHAR(18),
            cod_rechazo        CHAR(18)
       END RECORD
   DEFINE g_usuario  LIKE seg_usuario.usuario_cod 
   DEFINE v_fecha                DATE
                          
   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
        
         PRINTX v_fecha
         PRINTX g_usuario

      ON EVERY ROW
         PRINTX p_combinado.*
END REPORT
}