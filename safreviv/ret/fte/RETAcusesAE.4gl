--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS19                                                 #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO DE LA DEVOLUCIÓN AUTOMÁTICA DE AMORTIZACIONES    #
#                     EXCEDENTES A TRAVÉS DE LA FIEL                          #
#FECHA INICIO      => 30-NOV-2017                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
PRIVATE DEFINE v_archivo_pdf STRING 
GLOBALS

END GLOBALS

#
# MAIN
#
MAIN
DEFINE v_resultado       INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log        STRING,
       v_cadena          STRING,
       v_ruta_ejecutable VARCHAR(40)
DEFINE v_reg_acuse RECORD 
           nss         CHAR(11),
           id_derechohabiente   DECIMAL(9,0),
           estado_solicitud     SMALLINT,
           cod_rechazo          SMALLINT,
           total_aivs           DECIMAL(14,6),
           total_importe        DECIMAL(14,2),
           id_solicitud         DECIMAL(9,0),
           modalidad_retiro     SMALLINT,
           curp                 CHAR(18),
           nombre               CHAR(40),
           paterno              CHAR(40),
           materno              CHAR(40),
           rfc                  CHAR(13),
           caso_crm             CHAR(10),
           sello                CHAR(64),
           f_registro           CHAR(16),
           cuenta_clabe         CHAR(18)
   END RECORD 

   DEFINE v_nss                 CHAR(11)


   LET v_nss = ARG_VAL(1)
   
   DECLARE cur_acuses CURSOR FOR
      SELECT a.nss, a.id_derechohabiente, a.estado_solicitud, a.cod_rechazo,
             b.total_aivs, b.total_importe*(-1), a.id_solicitud, a.modalidad_retiro,
             c.curp, c.nombre_af, c.ap_paterno_af, c.ap_materno_af, c.rfc,
             a.caso_adai, d.sello, TO_CHAR(d.f_registro,'%d/%m/%Y %R'), e.cuenta_clabe
      FROM   ret_solicitud_generico a,
             ret_amort_excedente b,
             afi_derechohabiente c,
             ret_sol_medio_entrega d,
             ret_pago_spei e
      WHERE  a.id_solicitud = b.id_solicitud
      AND    a.id_solicitud = d.id_solicitud
      AND    a.id_solicitud = e.id_solicitud
      AND    a.id_derechohabiente = c.id_derechohabiente
      --AND    a.id_solicitud BETWEEN 11463574 AND 11466106
      AND    a.nss = v_nss
      AND    a.modalidad_retiro = 9

   FOREACH cur_acuses INTO v_reg_acuse.*
      CALL fn_genera_acuse(v_reg_acuse.*)
   END FOREACH

END MAIN

{
======================================================================
Nombre: fn_genera_acuse
======================================================================
}
FUNCTION fn_genera_acuse(p_reg_acuse)

DEFINE p_reg_acuse RECORD 
           nss         CHAR(11),
           id_derechohabiente   DECIMAL(9,0),
           estado_solicitud     SMALLINT,
           cod_rechazo          SMALLINT,
           total_aivs           DECIMAL(14,6),
           total_importe        DECIMAL(14,2),
           id_solicitud         DECIMAL(9,0),
           modalidad_retiro     SMALLINT,
           curp                 CHAR(18),
           nombre               CHAR(40),
           paterno              CHAR(40),
           materno              CHAR(40),
           rfc                  CHAR(13),
           caso_crm             CHAR(10),
           sello                CHAR(64),
           f_registro           CHAR(16),
           cuenta_clabe         CHAR(18)
   END RECORD 


   CALL fn_genera_reporte(p_reg_acuse.nss,
                          p_reg_acuse.rfc,
                          p_reg_acuse.paterno,p_reg_acuse.materno, p_reg_acuse.nombre, 
                          p_reg_acuse.f_registro,p_reg_acuse.id_solicitud,p_reg_acuse.total_importe,
                          p_reg_acuse.sello,p_reg_acuse.caso_crm,p_reg_acuse.cuenta_clabe)
   CALL fn_load_pdf(v_ruta_pdf, v_archivo_pdf, p_reg_acuse.caso_crm)--Se crea, se envia y se borra el reporte.pdf

END FUNCTION 

PRIVATE  FUNCTION fn_genera_reporte(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                    p_id_solicitud,p_pesos,p_sello,
                                    p_caso,p_clabe)

    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92--
    DEFINE p_sello           CHAR(64) -- Acuse Generado
    DEFINE p_caso            CHAR(10) -- Caso CRM 
    DEFINE p_clabe           CHAR(18) -- Clabe interbancaria  
    DEFINE v_tramite         CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo           CHAR(55)
    DEFINE medioSolicitud    CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE pesosTotal        CHAR(18) -- Suma en pesos total devuelto
    DEFINE archivo           BYTE
    DEFINE estadoConsulta    SMALLINT     -- Resultado de la Consulta
    DEFINE codRechazo        SMALLINT      -- Código de rechazo
    DEFINE reporte           om.SaxDocumentHandler
    DEFINE i                 SMALLINT
    DEFINE v_reporte         STRING
    DEFINE v_archivo         STRING 
    DEFINE v_ruta_listados   CHAR(40)
    DEFINE v_ruta_reporte    STRING
    DEFINE f_inicio          DATE
    DEFINE f_fin             DATE
    DEFINE v_query           STRING 
    DEFINE v_nombre          CHAR(120)
    DEFINE v_rfc             CHAR(13)
    DEFINE v_curp            CHAR(18)
    DEFINE v_nombre_stg      STRING 
    DEFINE v_fecha_paso      CHAR(16)
    DEFINE v_fecha           CHAR(16)
    DEFINE v_aviso           CHAR(255)
    
    
   LET v_reporte= "RETAcusesAE.4rp"
   LET v_aviso = NULL
   LET v_archivo = NULL

    SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
    WHERE modulo_cod = "ret"

    LET v_nombre_stg = v_nombre
    LET v_nombre_stg = v_nombre_stg CLIPPED
    LET v_archivo =  p_nss CLIPPED,"_", 
                     p_rfc CLIPPED,"_",
                     p_id_solicitud USING "&&&&&&&&&&","_"
                     ||YEAR(TODAY) CLIPPED ||MONTH(TODAY) CLIPPED
                     ||DAY(TODAY) CLIPPED,".pdf" 
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" , v_archivo CLIPPED 
    
    LET v_ruta_pdf    = v_ruta_reporte
    LET v_archivo_pdf = v_archivo
   DISPLAY "El archivo :", v_reporte
   DISPLAY "Ruta reporte :", v_ruta_reporte
   -- Busca si hay aviso que publicar en el acuse
 --  SELECT aviso
 --  INTO   v_aviso
 --  FROM   ret_aviso_pdf_ssv
 --  WHERE  f_vig_inicio <= TODAY 
 --  AND    f_vig_final  >= TODAY 
 --  IF v_aviso IS NOT NULL THEN  --- Relaciona el mensaje con la solicitud
 --     INSERT INTO ret_sol_aviso_pdf VALUES (p_id_solicitud, v_aviso);
 --  END IF 
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY "NSS reporte ", p_nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_acuse TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                                       p_id_solicitud,p_pesos,p_sello, v_aviso,
                                       p_caso,p_clabe)
         FINISH REPORT pdf_acuse
      END IF
   END IF
  
END FUNCTION 

REPORT pdf_acuse(p_nss, p_rfc, p_paterno, p_materno, p_nombre,p_fecha_hora,
                 p_id_solicitud,p_pesos,p_sello, p_aviso, p_caso,p_clabe) 
    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_rfc             LIKE afi_derechohabiente.rfc
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos           DECIMAL(22,2) -- Vivienda 92
    DEFINE p_sello           CHAR(64) -- Acuse Generado
    DEFINE p_aviso           CHAR(255)  --- Mensaje eventual 
    DEFINE p_caso            CHAR(10)  -- Caso CRM
    DEFINE p_clabe           CHAR(18)  -- Clabe interbancaria

    DEFINE v_tramite            CHAR(50)     -- Descripción del Trámite
    DEFINE v_grupo              CHAR(55)
    DEFINE v_medioSolicitud     CHAR(10)     -- Medio por el cual se hizo la solicitud 
    DEFINE v_pesos              CHAR(18) -- Vivienda 92
    DEFINE v_pesosViv97         CHAR(18) -- Vivienda 97
    DEFINE v_pesosTotal         CHAR(18) -- Suma en pesos total devuelto

    DEFINE v_nombre               CHAR(60)

   FORMAT

   FIRST PAGE HEADER

      PRINTX p_nss
      PRINTX p_rfc
      LET v_nombre = p_nombre CLIPPED, " ", p_paterno CLIPPED, " ", p_materno CLIPPED
      PRINTX v_nombre
      PRINTX p_clabe
      PRINTX p_fecha_hora
      PRINTX p_id_solicitud
      PRINTX p_caso
      LET v_medioSolicitud = "En línea"
      PRINTX v_medioSolicitud
      LET v_pesos = p_pesos USING "$$$,$$$,$$&.&&"
      PRINTX v_pesos
      PRINTX p_sello
      PRINTX p_aviso


END REPORT

PUBLIC FUNCTION fn_load_pdf(v_ruta_reporte, v_archivo_reporte, p_caso)
   DEFINE archivo           BYTE
   DEFINE v_archivo         STRING 
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_archivo_reporte STRING 
   DEFINE v_comando         STRING 
   DEFINE p_caso            CHAR(10)             
   DEFINE v_resultado       SMALLINT 

   LOCATE archivo IN MEMORY
   #DISPLAY v_ruta_reporte
   CALL archivo.readFile(v_ruta_reporte)
   --LET ws_ret_generico_solicitud_out.archivoPdf = archivo
   CALL security.Base64.LoadBinary(v_ruta_reporte) RETURNING v_archivo
   DISPLAY "Parámetros enviados a la Rutina de Adjunta Documentos"
   LET v_archivo_reporte = 'Acuse'
   DISPLAY "v_archivo_reporte: ", v_archivo_reporte
   DISPLAY "v_ruta_reporte: ",v_ruta_reporte
   DISPLAY "El archivo en base 64", v_archivo
   CALL fn_adjunta_documento(v_archivo_reporte, v_archivo, p_caso) RETURNING v_resultado
   LET v_comando="rm "||v_ruta_reporte
   RUN v_comando 
   
END FUNCTION


FUNCTION fn_adjunta_documento(p_nombre_archivo, p_archivo, p_caso)
DEFINE p_nombre_archivo STRING 
DEFINE p_archivo        STRING 
DEFINE p_caso           CHAR(10)
DEFINE v_regreso        SMALLINT 
DEFINE v_codigo         INTEGER 

   DEFINE arr_documentos RECORD
         nombre_documento STRING, 
         documento        STRING 
   END RECORD 

  DISPLAY "Parametros recibidos para el consumo de la funcion de documentos"
  DISPLAY "p_archivo: ",p_archivo
  DISPLAY "p_nombre_archivo: ", p_nombre_archivo
   LET v_regreso = 0
   LET arr_documentos.nombre_documento = p_nombre_archivo
   LET arr_documentos.documento        = p_archivo
   CALL fn_adjunta_docto_crm(p_caso, arr_documentos.*) RETURNING v_regreso, v_codigo
   IF v_regreso = 0 THEN 
--      IF v_codigo = 0 THEN 
         DISPLAY "Documento enviado a CRM exitosamente :", p_caso
--      ELSE 
--         DISPLAY "No se pudo integrar el documento, respuesta del Servicio Adjunta Documento :", v_codigo
--      END IF
   ELSE 
      DISPLAY "Problemas al invocar el servicio de Adjunta Dcoumentos :", v_regreso
   END IF 

RETURN v_regreso

END FUNCTION

