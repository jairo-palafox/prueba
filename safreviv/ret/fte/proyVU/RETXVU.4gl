################################################################################
#PROYECTO        => VENTANILLA UNICA                                           #
#PROPIETARIO     => OMNISYS                                                    #
#------------------------------------------------------------------------------#
#MODULO          => RET                                                        #
#PROGRAMA        => RETXVU                                                     #
#OBJETIVO        => Biblioteca de funciones para ventanilla unica              #
#                => DE FA, LEY73 Y SOLO INFONAVIT                              #
#FECHA CREACION  => 28-OCTUBRE-2020                                            #
#VERSION         => 1.0.0                                                      #
#MODIFICACION    =>                                                            #
################################################################################
IMPORT com
IMPORT os
IMPORT security
DATABASE safre_viv 

{
======================================================================
Clave: 
Nombre: fn_genera_acuse_solicitud_retiro_vu
Fecha creacion: Octubre 28, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Genera un reporte en formato PDF que contiene el cuerpo de un acuse de
generacion de solicitud de retiro por ventanilla unica

Parametros de entrada
PARAM                            DESCRIPCION                       OBLIGATORIO
p_nss                           NSS del derechohabiente                X
p_fecha_hora_tramite            Fecha y hora del tramite               X
p_caso_crm                      Clave del caso CRM/ADAI                X
p_medio_solicitud               Cadena de texto que indica el origen   X
                                de la solicitud: tableta, portal,
                                internet, etc
p_pesos_viv92                   Monto en pesos de viv92                X
p_pesos_viv97                   Monto en pesos de viv97                X
p_pesos_fa                      Monto en pesos de fondo de ahorro      X
p_pesos_total                   Monto en pesos, suma de viv92 + viv97
                                + fondo de ahorro. NO SE VALIDA, se
                                usara el monto enviado                 X
                                
p_mensaje_integrado_infonavit   Mensaje integrado para uso de
                                INFONAVIT. Si se manda vacio no se
                                agrega al formato PDF
p_sello_digital                 Cadena de bytes en base64 que          X
                                describe el sello digital del
                                documento

Parametros de salida
PARAM                            DESCRIPCION                
v_bytes_pdf                      Cadena de bytes del archivo PDF de la solicitud de retiro
                                
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_acuse_solicitud_retiro_vu(p_nss, p_fecha_hora_tramite, p_caso_crm, p_medio_solicitud, p_pesos_viv92, p_pesos_viv97, p_pesos_fa, p_pesos_total, p_mensaje_integrado_infonavit, p_sello_digital)
DEFINE p_nss LIKE afi_derechohabiente.nss
DEFINE p_fecha_hora_tramite STRING
DEFINE p_caso_crm           STRING
DEFINE p_medio_solicitud    STRING
DEFINE p_pesos_viv92 LIKE ret_control_vu.saldo_pesos_viv92
DEFINE p_pesos_viv97 LIKE ret_control_vu.saldo_pesos_viv97
DEFINE p_pesos_fa LIKE ret_control_vu.saldo_pesos_fa
DEFINE p_pesos_total LIKE ret_control_vu.saldo_total_pesos
DEFINE p_sello_digital STRING
DEFINE v_bytes_pdf BYTE
DEFINE v_report_handler om.SaxDocumentHandler -- handler para el reporte
DEFINE v_curp           LIKE afi_derechohabiente.curp
DEFINE v_nombre_imss    LIKE afi_derechohabiente.nombre_imss
DEFINE p_mensaje_integrado_infonavit STRING
DEFINE v_archivo_pdf STRING
DEFINE v_resultado_borrado SMALLINT
DEFINE v_directorio_temporal STRING


    -- se obtienen los datos del derechohabiente
    SELECT curp
          ,REPLACE(nombre_imss,"$"," ")
    INTO v_curp, v_nombre_imss
    FROM afi_derechohabiente
    WHERE nss = p_nss

    -- el nombre del archivo PDF es un UUID
    LET v_directorio_temporal = FGL_GETENV("RETXVU_RUTA_TEMPORAL_PDF_ASR")
    LET v_archivo_pdf = v_directorio_temporal || "asr_" || com.Util.CreateUUIDString() || ".pdf"
    DISPLAY "Archivo PDF: ", v_archivo_pdf
    
    -- se genera el reporte en formato pdf
    IF fgl_report_loadCurrentSettings("RETWS43_acuse_solicitud_retiro.4rp") THEN -- se asgina la plantilla

       CALL fgl_report_selectDevice("PDF")
       CALL fgl_report_selectPreview(FALSE)
       CALL fgl_report_setOutputFileName(v_archivo_pdf)
       LET v_report_handler = fgl_report_commitCurrentSettings()

       
       START REPORT rpt_acuse_creacion_solicitud TO XML HANDLER v_report_handler
    
       OUTPUT TO REPORT rpt_acuse_creacion_solicitud(p_nss, v_curp, v_nombre_imss, p_fecha_hora_tramite, p_caso_crm, p_medio_solicitud, p_pesos_viv92, p_pesos_viv97, p_pesos_fa, p_pesos_total, p_mensaje_integrado_infonavit, p_sello_digital)

       FINISH REPORT rpt_acuse_creacion_solicitud
       
       -- se obtienen los bytes del reporte
       LOCATE v_bytes_pdf IN FILE
       CALL fn_genera_bytes_pdf(v_archivo_pdf) RETURNING v_bytes_pdf

       -- se elimina el archivo original
       IF ( UPSHIFT(FGL_GETENV("RETXVU_ELIMINAR_PDF_ASR")) = "TRUE" ) THEN
          CALL os.Path.delete(v_archivo_pdf) RETURNING v_resultado_borrado
       END IF
       
       -- se devuelve el reporte
       RETURN v_bytes_pdf
     
    ELSE
       RETURN NULL
    END IF
   
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_genera_bytes_pdf
Fecha creacion: Octubre 28, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Genera la cadena de bytes de un 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_bytes_pdf(p_archivo_reporte)
DEFINE p_archivo_reporte STRING -- nombre del archivo
DEFINE v_bytes_archivo BYTE

   -- se abre el archivo
   LOCATE v_bytes_archivo IN FILE

   -- se obtienen los bytes
   CALL v_bytes_archivo.readFile(p_archivo_reporte)
   
   RETURN v_bytes_archivo
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_genera_cadena_original_solicitud_retiro_vu
Fecha creacion: Noviembre 03, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Genera la concatenacion de valores que forman la cadena original que se
utilizara para sellar la solicitud de retiro y obtiene el hash256 de esta cadena
que es devuelta como valor de retorno

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
Ivan Vega     Noviembre 10, 2020      - Se confirma la construccion de la cadena original para el
                                        sello digital con Benjamin Rodriguez, siendo la definicion:
                                        caso_crm|nss|curp|fecha|apellido paterno|apellido materno|nombre de pila|total pesos formateado
======================================================================
}
FUNCTION fn_genera_sello_cadena_original_solicitud_retiro_vu(p_nss, p_medio_entrega, p_caso_crm, p_id_solicitud_fa, p_id_solicitud_ley73, p_id_solicitud_si, p_total_pesos)
DEFINE p_nss                LIKE afi_derechohabiente.nss
DEFINE p_medio_entrega      LIKE ret_sol_medio_entrega.medio_entrega
DEFINE p_caso_crm           STRING
DEFINE p_id_solicitud_fa    LIKE ret_control_vu.id_solicitud_fa
DEFINE p_id_solicitud_ley73 LIKE ret_control_vu.id_solicitud_ley73
DEFINE p_id_solicitud_si    LIKE ret_control_vu.id_solicitud_si
DEFINE p_total_pesos        LIKE ret_control_vu.saldo_total_pesos
DEFINE v_curp               LIKE afi_derechohabiente.curp
DEFINE v_nombre             LIKE afi_derechohabiente.nombre_af
DEFINE v_apellido_paterno   LIKE afi_derechohabiente.ap_paterno_af
DEFINE v_apellido_materno   LIKE afi_derechohabiente.ap_materno_af
DEFINE v_cadena_original    STRING
DEFINE v_hash               STRING
DEFINE v_hasher             security.Digest
DEFINE v_sello              LIKE ret_sol_medio_entrega.sello
DEFINE v_id_solicitud_fa    STRING
DEFINE v_id_solicitud_ley73 STRING
DEFINE v_id_solicitud_si    STRING

   -- se obtienen los datos del nss
   SELECT curp
         ,nombre_af
         ,ap_paterno_af
         ,ap_materno_af
   INTO  v_curp
        ,v_nombre
        ,v_apellido_paterno
        ,v_apellido_materno
   FROM afi_derechohabiente
   WHERE nss = p_nss

   -- se genera la cadena original
   -- caso_crm|nss|curp|fecha|appat|apmat|nombre|total
   LET v_cadena_original = p_caso_crm, "|", 
                           p_nss, "|",
                           v_curp, "|",
                           TODAY USING "dd/mm/yyyy", "|",
                           v_apellido_paterno CLIPPED, "|",
                           v_apellido_materno CLIPPED, "|",
                           v_nombre CLIPPED, "|",
                           p_total_pesos USING "<,<<<,<<$.&&"


                           
   DISPLAY "Cadena original generada"
   DISPLAY v_cadena_original
                           
   -- se obtiene el hash256 de la cadena original
   TRY
      LET v_hasher = security.Digest.CreateDigest("SHA256")
      CALL v_hasher.AddStringData(v_cadena_original)
      LET v_hash = v_hasher.DoHexBinaryDigest()
      LET v_sello = v_hash
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   -- se guarda el hash en la tabla de registro de hashes
   -- si hay fondo de ahorro
   IF ( p_id_solicitud_fa IS NOT NULL AND p_id_solicitud_fa <> 0 ) THEN
       UPDATE ret_sol_medio_entrega
       SET sello = v_sello
       WHERE id_solicitud = p_id_solicitud_fa
   END IF

   -- si hay ley73
   IF ( p_id_solicitud_ley73 IS NOT NULL AND p_id_solicitud_ley73 <> 0 ) THEN
       UPDATE ret_sol_medio_entrega
       SET sello = v_sello
       WHERE id_solicitud = p_id_solicitud_ley73
   END IF
   
   -- si hay solo infonavit
   IF ( p_id_solicitud_si IS NOT NULL AND p_id_solicitud_si <> 0 ) THEN
       UPDATE ret_sol_medio_entrega
       SET sello = v_sello
       WHERE id_solicitud = p_id_solicitud_si
   END IF
   
   -- se devuelve el hash
   RETURN v_hash
END FUNCTION

{
======================================================================
Clave: 
Nombre: rpt_acuse_creacion_solicitud
Fecha creacion: Octubre 29, 2020
Autor: Ivan Vega, Omnisys
Narrativa del proceso que realiza:
Genera el archivo PDF del formato de acuse de creacion de solicitud de
retiro por ventanilla unica

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
REPORT rpt_acuse_creacion_solicitud(p_nss, p_curp, p_nombre_imss, p_fecha_hora_tramite, p_caso_crm, p_medio_solicitud, p_pesos_viv92, p_pesos_viv97, p_pesos_fa, p_pesos_total, p_mensaje_integrado_infonavit, p_sello_digital)
DEFINE p_nss LIKE afi_derechohabiente.nss
DEFINE p_curp        LIKE afi_derechohabiente.curp
DEFINE p_nombre_imss LIKE afi_derechohabiente.nombre_imss
DEFINE p_fecha_hora_tramite STRING
DEFINE p_caso_crm           STRING
DEFINE p_medio_solicitud    STRING
DEFINE p_pesos_viv92 LIKE ret_control_vu.saldo_pesos_viv92
DEFINE p_pesos_viv97 LIKE ret_control_vu.saldo_pesos_viv97
DEFINE p_pesos_fa LIKE ret_control_vu.saldo_pesos_fa
DEFINE p_pesos_total LIKE ret_control_vu.saldo_total_pesos
DEFINE p_mensaje_integrado_infonavit STRING
DEFINE p_sello_digital STRING

   FORMAT 
      ON EVERY ROW
         PRINT p_nss, p_curp, p_nombre_imss, p_fecha_hora_tramite, p_caso_crm, p_medio_solicitud, p_pesos_viv92, p_pesos_viv97, p_pesos_fa, p_pesos_total, p_mensaje_integrado_infonavit, p_sello_digital

END REPORT