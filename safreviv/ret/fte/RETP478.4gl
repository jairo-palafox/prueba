--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETP478                                                 #
#OBJETIVO          => PROGRAMA LANZADO PARA LA GENERACIÓN DE CARTAS DE        #
#                     NEGATIVA DEL FONDO DE AHORRO                            #
#FECHA INICIO      => 13-MAR-2019                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
PRIVATE DEFINE v_archivo_pdf STRING 
-- registro de entrada para la consulta

#
# MAIN
#
MAIN
DEFINE v_resultado        INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log         STRING,
       v_cadena           STRING,
       v_ruta_ejecutable  VARCHAR(40),
       p_pid              LIKE bat_ctr_operacion.pid,         # PID del proceso
       p_proceso_cod      LIKE bat_ctr_operacion.proceso_cod, # codigo del proceso
       p_opera_cod        LIKE bat_ctr_operacion.opera_cod,   # codigo de la operacion
       p_usuario_cod      LIKE seg_usuario.usuario_cod,       # clave del usuario firmado
       p_folio            LIKE ret_preliquida.folio_liquida,
       v_certificado      STRING,
       v_rfc_funcionario  CHAR(13) 
       
      
  -- se obtiene la ruta ejecutable
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = "ret"
  
  
   # recupera parametros
   --LET p_rfc           = ARG_VAL(1)
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   
   CALL f_obtiene_certificado() RETURNING v_resultado, v_certificado, v_rfc_funcionario
   DISPLAY "Se obtuvo el certificado, resultado :", v_resultado
   IF v_resultado = TRUE THEN 
      CALL fn_procesa_cartas_negativa(v_certificado, v_rfc_funcionario) RETURNING v_resultado
   END IF 
   IF v_resultado = 0 THEN
      DISPLAY "Proceso concluido exitosamente: "
      DISPLAY CURRENT YEAR TO SECOND
   ELSE 
      DISPLAY "Proceso concluido con error: "
      DISPLAY CURRENT YEAR TO SECOND
   END IF 
      
END MAIN

{
======================================================================
Clave: 
Nombre: fn_procesa_cartas_negativa
Fecha creacion: Marzo 13, 2019
Autor: Ricardo Pérez, EFP
Narrativa del proceso que realiza:
Genera las cartas de Negativa del Fondo de Ahorro

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_procesa_cartas_negativa(p_certificado, p_rfc_funcionario)
DEFINE v_indice_retiro       SMALLINT,
       p_certificado         STRING,
       p_rfc_funcionario     CHAR(13),
       v_nss                 LIKE afi_fondo72.nss,
       v_rfc                 LIKE afi_fondo72.rfc,
       v_indice_modalidad    SMALLINT, -- indice de modalidad de retiro
       v_indice_beneficiario SMALLINT, -- contador de beneficiarios
       v_existe_beneficiario SMALLINT, -- booleana que indica si esta bien el registro de beneficiario
       v_cta_clabe_correcta  SMALLINT, -- booleana que indica si la cuenta clabe tiene estructura correcta
	    v_requiere_DAP        SMALLINT, -- booleana que indica si se necesita DAP
       v_modalidad_procesada SMALLINT, -- Indica si ya se proceso una solicitud de ley 73
       v_id_derechohabiente  DECIMAL(10,0) -- Identificador único del trabajador
DEFINE v_arr_cartas RECORD 
         v_nss               CHAR(11),
         v_tipo_carta        CHAR(1),
         v_id_afi_fondo72    DECIMAL(9,0),
         v_caso_crm          CHAR(10),
         v_nombre            CHAR(120),
         v_fecha_mov         DATE,
         v_monto_mov         DECIMAL(22,2),
         v_cve_mov           CHAR(20)
   END RECORD 
DEFINE v_id_peticion         DECIMAL(9,0)
DEFINE v_resultado           SMALLINT 
DEFINE v_sql                 STRING        
   

   LET v_sql = "SELECT rsnfa.nss, rsnfa.tipo_carta, rsnfa.id_afi_fondo72, rsnfa.caso_crm, nombre,'','','', rsnfa.id_peticion "||
               "FROM   ret_sol_negativas_fondo_ahorro rsnfa,                                      "||
               "       afi_fondo72                       af                                       "||
               "WHERE  rsnfa.id_afi_fondo72   = af.id_afi_fondo72                                 "||
               "AND    af.ind_estado_cuenta   = 0                                                 "||
               "AND    rsnfa.estado_solicitud = 10                                                "--||
--               "AND    rsnfa.id_peticion IN (2,22)                                                "

   PREPARE prp_carta_negativa FROM v_sql
   DECLARE cur_carta_negativa CURSOR FOR prp_carta_negativa

   FOREACH cur_carta_negativa INTO v_arr_cartas.*, v_id_peticion
      CALL fn_busca_mov_cargo_fa(v_arr_cartas.v_nss) RETURNING v_arr_cartas.v_tipo_carta,
                                                                        v_arr_cartas.v_fecha_mov, 
                                                                        v_arr_cartas.v_cve_mov,
                                                                        v_arr_cartas.v_monto_mov
      IF v_arr_cartas.v_tipo_carta IS NOT NULL OR v_arr_cartas.v_nss = '01755355367' THEN  
         IF v_arr_cartas.v_nss = '01755355367' THEN 
            LET v_arr_cartas.v_tipo_carta = "T" 
            LET v_arr_cartas.v_fecha_mov = '10/03/2010'
            LET v_arr_cartas.v_cve_mov = '1923856389'
            LET v_arr_cartas.v_monto_mov = 100.00
         END IF 
         CALL fn_genera_carta_negativa(v_arr_cartas.*, p_certificado, p_rfc_funcionario) RETURNING v_resultado
         -- Actualiza la solicitud para no volverse a considerar
         UPDATE ret_sol_negativas_fondo_ahorro
         SET    estado_solicitud = 800,
                f_genera_carta   = TODAY,
                f_adjunta_docto  = TODAY,
                tipo_carta       = v_arr_cartas.v_tipo_carta
         WHERE  id_peticion      = v_id_peticion
         DISPLAY "Petición Actualizada"
      ELSE 
         DISPLAY "------------------------------------------------------------------------------------" 
         DISPLAY "No se puedo determinar el tipo de carta a generar, este registro no será considerado"
         DISPLAY "NSS ....:", v_arr_cartas.v_nss
         DISPLAY "Caso CRM:", v_arr_cartas.v_caso_crm
         DISPLAY "------------------------------------------------------------------------------------\n\n" 
      END IF 
   END FOREACH
   RETURN v_resultado
END FUNCTION

{
======================================================================
Nombre: fn_genera_carta_negativa
Fecha creacion: Marzo 14, 2019
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Genera la carta de Negativa del Fondo de Ahorro para un NSS dado

Registro de modificaciones:
Autor           Fecha      Descrip. cambio
======================================================================
}
FUNCTION fn_genera_carta_negativa(p_arr_cartas, p_certificado, p_rfc_funcionario)
DEFINE p_arr_cartas RECORD 
         v_nss               CHAR(11),
         v_tipo_carta        CHAR(1),
         v_id_afi_fondo72    DECIMAL(9,0),
         v_caso_crm          CHAR(10),
         v_nombre            CHAR(120),
         v_fecha_mov         DATE,
         v_monto_mov         DECIMAL(22,2),
         v_cve_mov           CHAR(20)
   END RECORD 
DEFINE p_certificado         STRING
DEFINE p_rfc_funcionario     CHAR(13)

DEFINE v_resultado           SMALLINT 

DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
       p_nss                  LIKE afi_derechohabiente.nss, 
       p_tipo_carta           CHAR(1),
       p_estado_solicitud     SMALLINT      , -- estatus de la solicitud
       p_rechazo              SMALLINT      , -- booleana que indica si esta rechazada la solicitud
       p_aivs_viv92           DECIMAL(24,6),
       p_pesos_viv92          DECIMAL(22,2),
       p_aivs_viv97           DECIMAL(24,6),
       p_pesos_viv97          DECIMAL(22,2),
       p_id_solicitud         LIKE ret_fondo_ahorro.id_solicitud, -- num de solicitud
       p_indice_modalidad     SMALLINT, -- indice de la modalidad del retiro
       v_id_solicitud         LIKE ret_fondo_ahorro.id_solicitud,
       v_marca_ley73          LIKE sfr_marca.marca, -- marca de amortizaciones excedentes
       v_saldo_poseido        LIKE ret_det_fondo72.saldo_viv72, -- saldo del trabajador en fondo72
       v_r_ret_ley73_generico RECORD LIKE ret_ley73_generico.*, -- registro de retiro de ley73
       v_conteo               SMALLINT,
       v_tipo_pago            SMALLINT,
       v_total_aivs           DECIMAL(24,6), -- total de AIVs
       v_total_pesos          DECIMAL(22,2), -- total de pesos
       v_sql                  STRING, -- cadena con enunciado SQL
--       p_folio                decimal(9,0),
       p_gpo_ley73            smallint,
       v_subgrupo             smallint,
       p_importe_viv97_anexo1   decimal(12,2),
       v_cadena               STRING,  -- Cadena de caracteres para generar el SHA
       v_algoritmo            CHAR(6),
       v_curp                 CHAR(18),
       v_rfc                  CHAR(13),
       v_nombre               CHAR(120),
       v_ape_paterno          CHAR(40),
       v_ape_materno          CHAR(40),
       v_sha                  STRING,
       v_c_sha                CHAR(64),
       v_fecha_paso           CHAR(16),
       v_fecha_hora           CHAR(16),
       i                      SMALLINT,
       v_result               SMALLINT,
       v_error,               STRING,
       v_sello                STRING 
       
   LET v_cadena = ""
   LET v_algoritmo = ""
   LET v_sha = ""
   LET v_rfc = ""
   LET v_fecha_paso = CURRENT YEAR TO MINUTE
   LET v_nombre = ""
   FOR i = 1 TO LENGTH(p_arr_cartas.v_nombre)
      LET v_nombre = v_nombre CLIPPED, p_arr_cartas.v_nombre[i]  
   END FOR   
       
   LET v_fecha_hora = v_fecha_paso[9,10],"/",v_fecha_paso[6,7],"/",v_fecha_paso[1,4]," ",v_fecha_paso[12,16]
   LET v_cadena = p_arr_cartas.v_caso_crm, p_arr_cartas.v_nss, v_nombre CLIPPED,v_fecha_hora
   CALL fn_obtiene_sello_digital(v_cadena) RETURNING v_result, v_error, v_sello
   --         LET v_algoritmo = "SHA256"
   --         CALL ERRORLOG("cadena>"||v_cadena||"<") 
   --         CALL ERRORLOG("algoritmo>"||v_algoritmo||"<")

   --         CALL fn_hash_local(v_cadena CLIPPED , v_algoritmo) RETURNING v_sha
   --         CALL ERRORLOG("sha>"||v_sha||"<")
   LET v_c_sha = v_sha
   --         CALL ERRORLOG("c_sha>"||v_c_sha||"<")
   --         UPDATE ret_sol_medio_entrega
   --         SET    sello = v_c_sha,
   --                f_registro = CURRENT YEAR TO MINUTE  
   --         WHERE  id_solicitud = p_id_solicitud;
   -- obtenemos el caso_crm de la tabla
   --         SELECT caso_adai
   --         INTO   ws_ret_generico_solicitud_in.caso_adai
   --         FROM   ret_solicitud_generico
   --         WHERE  id_solicitud = p_id_solicitud;
   CALL fn_genera_reporte(p_arr_cartas.*,v_sello,v_cadena)
   CALL fn_load_pdf_fa(v_ruta_pdf, v_archivo_pdf, p_arr_cartas.v_caso_crm, p_certificado, p_rfc_funcionario)--Se crea, se envia y se borra el reporte.pdf

RETURN v_resultado
END FUNCTION 

{
======================================================================
Clave: 
Nombre: fn_hash
Fecha creacion: Marzo 03, 2016
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
Genera un código HASH (pasado por parámetro) de una cadena de texto STRING
(pasada por parámetro),

códigos HASH permitidos:
  - SHA1 (Recomendado)
  - SHA512
  - SHA384
  - SHA256
  - SHA224
  - MD5

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_hash_local(toDigest, algo)

   DEFINE toDigest, algo, result STRING
   DEFINE dgst security.Digest

--   IF algo IS NULL OR algo = "" THEN
--      LET algo = "SHA1" --(Default)
--   END IF
--   LET algo = "SHA256"

   TRY
      LET dgst = security.Digest.CreateDigest("SHA256")
      CALL dgst.AddStringData(toDigest)
      --LET result = dgst.DoBase64Digest()
      LET result = dgst.DoHexBinaryDigest()
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      EXIT PROGRAM(-1)
   END TRY

   RETURN result
   
END FUNCTION

PUBLIC FUNCTION fn_load_pdf_fa(v_ruta_reporte, v_archivo_reporte, p_caso, p_certificado, p_rfc_funcionario)
   DEFINE archivo           BYTE
   DEFINE v_archivo         STRING 
   DEFINE v_archivo_sellado STRING 
   DEFINE v_pdf_sellado     STRING 
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_archivo_reporte STRING 
   DEFINE v_comando         STRING 
   DEFINE p_caso            CHAR(10)             
   DEFINE v_resultado       SMALLINT 
   DEFINE p_certificado     STRING 
   DEFINE p_rfc_funcionario CHAR(13)
   DEFINE v_algo            STRING 
   DEFINE v_ruta_rep_sellado STRING 

   LOCATE archivo IN MEMORY
   DISPLAY "Parámetros enviados a la Rutina de Adjunta Documentos"
   LET v_archivo_reporte = 'Carta Negativa'
   --DISPLAY "v_archivo_reporte: ", v_archivo_reporte
   --DISPLAY "v_ruta_reporte: ",v_ruta_reporte
   LET v_ruta_rep_sellado = v_ruta_reporte || "_sellado.pdf"
   #DISPLAY v_ruta_reporte
   CALL archivo.readFile(v_ruta_reporte)
   CALL security.Base64.LoadBinary(v_ruta_reporte) RETURNING v_archivo
--   DISPLAY "El archivo en base 64", v_archivo
   --CALL f_obtiene_certificado() RETURNING v_certificado
   IF p_certificado IS NOT NULL THEN 
      --CALL fn_hash_local(v_certificado, v_algo) RETURNING v_certificado 
--      DISPLAY "Se obtiene la firma para incluirlo en el PDF"
--      CALL f_obtiene_firma(p_certificado,v_archivo) RETURNING v_archivo_sellado
--      DISPLAY "Regresa de la función f_obtiene_firma"
--      IF v_archivo_sellado IS NOT NULL THEN 
         DISPLAY "Se envia sellar el PDF"
         CALL f_sella_pdf(v_archivo, p_rfc_funcionario CLIPPED) RETURNING v_pdf_sellado
         DISPLAY "Regresa de la función f_sella_pdf"
         IF v_pdf_sellado IS NOT NULL THEN 
            CALL fn_adjunta_documento_fa(v_archivo_reporte, v_pdf_sellado, p_caso) RETURNING v_resultado
            CALL security.Base64.SaveBinary(v_ruta_rep_sellado,v_pdf_sellado)
            DISPLAY "Se manda el documento a CRM"
         ELSE 
            DISPLAY "No se obtuvo el PDF sellado, no se enviará a CRM"
         END IF 
--      ELSE 
--         DISPLAY "No se pudo obtener el PDF para sellar"
--      END IF 
   ELSE
      DISPLAY "No se pudo obtener el certificado!!!"
   END IF 
--   LET v_comando="rm "||v_ruta_reporte
--   RUN v_comando 
   
END FUNCTION

PRIVATE  FUNCTION fn_genera_reporte(p_arr_cartas,p_sello, p_cadena_original)
DEFINE p_arr_cartas RECORD 
         v_nss               CHAR(11),
         v_tipo_carta        CHAR(1),
         v_id_afi_fondo72    DECIMAL(9,0),
         v_caso_crm          CHAR(10),
         v_nombre            CHAR(120),
         v_fecha_mov         DATE,
         v_monto_mov         DECIMAL(22,2),
         v_cve_mov           CHAR(20)
   END RECORD 
DEFINE p_sello           STRING -- Acuse Generado
DEFINE p_cadena_original STRING

    DEFINE p_nss             LIKE afi_derechohabiente.nss
    DEFINE p_curp            LIKE afi_derechohabiente.curp
    DEFINE p_paterno         CHAR(40)     -- Apellido paterno
    DEFINE p_materno         CHAR(40)     -- Apellido materno
    DEFINE p_nombre          CHAR(40)     -- Nombre
    DEFINE p_fecha_hora      CHAR(16)     -- Fecha de la Solicitud
    DEFINE p_id_solicitud    LIKE ret_solicitud_generico.id_solicitud
    DEFINE p_pesos_viv92     DECIMAL(22,2) -- Vivienda 92
    DEFINE p_pesos_viv97     DECIMAL(22,2) -- Vivienda 97
    DEFINE p_caso            CHAR(10) -- Caso CRM 
    DEFINE p_tipo_carta      CHAR(1)  -- Tipo de carta de Negativa D-Devolución, T-Traspaso
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
    DEFINE v_ruta_bin        CHAR(40)
    DEFINE v_ruta_reporte    STRING
    DEFINE f_inicio          DATE
    DEFINE f_fin             DATE
    DEFINE v_query           STRING 
    DEFINE v_nombre          CHAR(120)
    DEFINE v_rfc             CHAR(13)
    DEFINE v_curp            CHAR(18)
    DEFINE v_fecha_paso      CHAR(16)
    DEFINE v_fecha           CHAR(16)
    DEFINE v_aviso           CHAR(255)
    DEFINE v_f_creacion_pdf  CHAR(8)
    
    
   LET v_aviso = NULL
   LET v_archivo = NULL
   LET v_f_creacion_pdf = TODAY USING "YYYYMMDD"

   SELECT ruta_listados, ruta_bin
   INTO v_ruta_listados, v_ruta_bin
   FROM seg_modulo
   WHERE modulo_cod = "ret"

   LET v_reporte = v_ruta_bin CLIPPED, "/RETP478.4rp"
   
   LET v_archivo =  p_arr_cartas.v_nss CLIPPED,"_", 
                  p_arr_cartas.v_caso_crm USING "&&&&&&&&&&","_"
                  ||v_f_creacion_pdf||".pdf" 
   LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" , v_archivo CLIPPED 

   LET v_ruta_pdf    = v_ruta_reporte
   LET v_archivo_pdf = v_archivo
   --DISPLAY "El archivo :", v_reporte
   --DISPLAY "Ruta reporte :", v_ruta_reporte
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY "NSS reporte ", p_arr_cartas.v_nss
      IF reporte IS NOT NULL THEN
         START REPORT pdf_carta_negativa TO XML HANDLER reporte
            OUTPUT TO REPORT pdf_carta_negativa(p_arr_cartas.*,p_sello, p_cadena_original)
         FINISH REPORT pdf_carta_negativa
      END IF
   END IF
  
END FUNCTION 

REPORT pdf_carta_negativa(p_arr_cartas,p_sello, p_cadena_original) 
DEFINE p_arr_cartas RECORD 
         v_nss               CHAR(11),
         v_tipo_carta        CHAR(1),
         v_id_afi_fondo72    DECIMAL(9,0),
         v_caso_crm          CHAR(10),
         v_nombre            CHAR(120),
         v_fecha_mov         DATE,
         v_monto_mov         DECIMAL(22,2),
         v_cve_mov           CHAR(20)
   END RECORD 

DEFINE p_nss             LIKE afi_derechohabiente.nss
DEFINE p_nombre          STRING     -- Nombre
DEFINE p_sello           STRING-- Acuse Generado
DEFINE p_cadena_original STRING
DEFINE p_tipo_carta      CHAR(1)
DEFINE p_fecha_mov       DATE
DEFINE p_monto_mov       STRING 
DEFINE p_cve_mov         STRING 

DEFINE v_titulo_reporte    CHAR(150)
DEFINE v_fecha_reporte     CHAR(100)
DEFINE v_gerencia          STRING 
DEFINE v_caso_goci         STRING 
DEFINE v_nombre               CHAR(60)
DEFINE v_parrafo1          STRING 
DEFINE v_parrafo2          STRING
DEFINE v_parrafo3          STRING
DEFINE v_parrafo3a         STRING 
DEFINE v_mes               CHAR(10) 
DEFINE v_monto_letra       STRING
DEFINE v_dia               CHAR(2)
DEFINE v_anio              CHAR(4)
    
   FORMAT

   FIRST PAGE HEADER
      CASE MONTH(TODAY)
         WHEN 1
            LET v_mes = "enero"
         WHEN 2 
            LET v_mes = "febrero"
         WHEN 3 
            LET v_mes = "marzo"
         WHEN 4 
            LET v_mes = "abril"
         WHEN 5 
            LET v_mes = "mayo"
         WHEN 6 
            LET v_mes = "junio"
         WHEN 7 
            LET v_mes = "julio"
         WHEN 8 
            LET v_mes = "agosto"
         WHEN 9
            LET v_mes = "septiembre"
         WHEN 10
            LET v_mes = "octubre"
         WHEN 11
            LET v_mes = "noviembre"
         WHEN 12
            LET v_mes = "diciembre"
      END CASE 
      
      CALL fn_importe_monto(p_arr_cartas.v_monto_mov) RETURNING v_monto_letra
      LET p_sello = "<",p_sello CLIPPED, ">"
      LET v_dia = DAY(TODAY)
      LET v_anio = YEAR(TODAY)
      LET p_cadena_original = "<", p_cadena_original,">"
      LET v_fecha_reporte = "Ciudad de México, a ", v_dia CLIPPED, " de ", v_mes CLIPPED, " de ", v_anio CLIPPED
      LET v_gerencia = "Gerencia Sr. de Administración al Patrimonio Social y Servicios"
      LET v_caso_goci = "GAPSS/",p_arr_cartas.v_caso_crm CLIPPED, "/", v_anio CLIPPED 
      LET v_nombre = "C. ", p_arr_cartas.v_nombre CLIPPED
      LET v_parrafo3 = "También queremos informarle que usted podrá ejercer el derecho de presentar un recurso de inconformidad con fundamento ",
                       "en el Artículo 52 de la Ley del Infonavit el cual establece: "
      LET v_parrafo3a = '"...En los casos de inconformidad de las empresas, de los ',
                        'trabajadores o sus beneficiarios sobre la inscripción en el Instituto, derecho a créditos, cuantía de aportaciones y de ',
                        'descuentos, así como sobre cualquier acto del instituto que lesione derechos de los trabajadores inscritos, de sus ',
                        'beneficiarios o de los patrones, se podrá promover ante el propio Instituto un recurso de inconformidad. El Reglamento ',
                        'correspondiente, determinará la forma y términos en que se podrá interponer el recurso de inconformidad a que se refiere ',
                        'este artículo. El que será presentado o remitido por conducto del servicio postal mexicano a la Comisión de Inconformidades, ',
                        'con domicilio en Barranca del Muerto 280, oficina 104, colonia Guadalupe Inn, código postal 01029, Alcaldía Álvaro Obregón en ',
                        'Ciudad de México..."'
      
      IF p_arr_cartas.v_tipo_carta = 'D' THEN
         LET v_titulo_reporte = "Carta De Negativa De La Devolución Del Fondo De Ahorro 72-92 Por Entrega Anterior"
         LET v_parrafo1 = "Por este medio me permito informarle que las aportaciones patronales que se encontraban registradas a favor del C.",
                          " ", v_nombre CLIPPED, ", dentro del periodo comprendido del 1 de mayo ",
                          "de 1972 al 29 de febrero de 1992, ante el Infonavit con el número de seguridad social NSS ", p_arr_cartas.v_nss, ", fueron devueltas ",
                          "el día ", p_arr_cartas.v_fecha_mov USING "DD/MM/YYYY", ", mediante dictamen/referencia ", p_arr_cartas.v_cve_mov CLIPPED, " por la cantidad de ",
                          p_arr_cartas.v_monto_mov USING "$###,##&.&&", " (",v_monto_letra CLIPPED ,")." 
         LET v_parrafo2 = "Lo anterior de conformidad con lo dispuesto por el artículo 141 Fracciones I y II de la Ley Federal del Trabajo."
      ELSE 
         LET v_titulo_reporte = "Carta De Negativa De La Devolución Del Fondo De Ahorro 72-92 Por Transferencia A Un Crédito Otorgado Por El Infonavit"
         LET v_parrafo1 = "Por este conducto informo a usted que no es posible tramitar su solicitud, en virtud de que el saldo que tenía acumulado ",
                          "por las aportaciones realizadas por su patrón(es) al Fondo Nacional de la Vivienda, constituida a su favor ante el ",
                          "Infonavit durante el período del 1° de mayo de 1972 al 29 de febrero de 1992 por concepto de aportaciones a su Fondo ",
                          "de Ahorro le fue aplicada en un 100% a la amortización de su crédito número ", p_arr_cartas.v_cve_mov CLIPPED, " por un monto de ",
                          p_arr_cartas.v_monto_mov USING "$###,##&.&&", " (",v_monto_letra CLIPPED ,"), con fecha ", p_arr_cartas.v_fecha_mov USING "DD/MM/YYYY", "."
         LET v_parrafo2 = "Lo anterior de conformidad con lo dispuesto por el artículo 141 Fracción III de la Ley Federal del Trabajo."
      END IF 

      PRINTX v_titulo_reporte
      PRINTX v_fecha_reporte
      PRINTX v_gerencia
      PRINTX v_caso_goci
      PRINTX v_nombre
      PRINTX v_parrafo1
      PRINTX v_parrafo2
      PRINTX v_parrafo3
      
      PRINTX v_parrafo3a
      
      PRINTX p_sello
      PRINTX p_cadena_original

END REPORT

FUNCTION fn_adjunta_documento_fa(p_nombre_archivo, p_archivo, p_caso)
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
  --DISPLAY "p_archivo: ",p_archivo
  --DISPLAY "p_nombre_archivo: ", p_nombre_archivo
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

FUNCTION f_obtiene_certificado()
DEFINE v_indice_retiro  SMALLINT,
      v_cadena          VARCHAR(120),
      v_rfc             CHAR(13),
      v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
      v_ruta_log        STRING,
      v_consulta        STRING,
      v_indice          INTEGER,
      v_sello           STRING,
      v_error           STRING,
      v_result          SMALLINT

   --Busca el rfc del funcionario que firmará los PDF
   SELECT a.rfc,
          TRIM(b.nombre_af) || " " || TRIM(b.ap_paterno_af) || " " || TRIM(b.ap_materno_af) AS nombre
   INTO   v_rfc, v_cadena
   FROM   ret_rfc_firma_pdf a,
          afi_derechohabiente b
   WHERE  a.rfc = b.rfc
   AND    a.id_firma = 2;
   LET v_result = TRUE
   --LET v_cadena   = "ESTAESLACADENAQUECONVERTIRAENHASH"

   DISPLAY "Datos para obtener el certificado:"
   DISPLAY "Cadena  : ", v_cadena
   DISPLAY "El RFC  : ", v_rfc

   --- Se validan los parámetros de entrada
   IF v_rfc IS NULL THEN 
      DISPLAY "No se encontró información para enviar al servicio de consulta del certificado"
      LET v_sello = "SIN INFORMACIÓN"
      LET v_result = FALSE 
   ELSE 
      DISPLAY "Se busca el certificado para el RFC :",v_rfc
      CALL fn_obtiene_certificado(v_rfc CLIPPED, v_rfc CLIPPED )  RETURNING v_result, v_error, v_sello 
      IF v_result = TRUE THEN 
         DISPLAY "El sello recibido es ", v_sello
      ELSE 
         LET v_sello = "INCORRECTO"
         DISPLAY "Problemas al obtener el sello ", v_error
      END IF 
   END IF 
   RETURN v_result, v_sello, v_rfc
END FUNCTION

FUNCTION f_obtiene_firma(p_certificado,p_pdf)
DEFINE v_indice_retiro SMALLINT,
      v_cadena         STRING,
      p_certificado    STRING,
      p_pdf            STRING,
      v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
      v_ruta_log        STRING,
      v_consulta        STRING,
      v_indice          INTEGER,
      v_sello           STRING,
      v_error           STRING,
      v_result          SMALLINT


   DISPLAY "Se busca la firma para el PDF:"

   --- Se validan los parámetros de entrada
   IF p_pdf IS NULL THEN 
      DISPLAY "No se tiene PDF para enviar a firma"
      LET v_sello = ""
   ELSE 
      DISPLAY "Se envia información para firma con los datos:"
      DISPLAY "Cadena :", p_certificado
     -- DISPLAY "PDF    ;", p_pdf
     
      CALL fn_obtiene_firma(p_certificado, p_pdf) RETURNING v_result, v_error, v_sello 
      IF v_result = TRUE THEN 
         DISPLAY "Se firmó el PDF correctamente "
      ELSE 
         LET v_sello = ""
         DISPLAY "Problemas al firmar el PDF, error ", v_error
      END IF 
   END IF 
   RETURN v_sello
END FUNCTION

FUNCTION f_sella_pdf(p_pdf, p_rfc_funcionario)
DEFINE v_indice_retiro SMALLINT,
      v_cadena         STRING,
      p_pdf            STRING,
      v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
      v_ruta_log        STRING,
      v_consulta        STRING,
      v_indice          INTEGER,
      v_sello           STRING,
      v_error           STRING,
      v_result          SMALLINT,
      p_rfc_funcionario STRING 

   DISPLAY "Se recibe PDF para sellar:"

   --- Se validan los parámetros de entrada
   IF p_pdf IS NULL THEN 
      DISPLAY "Debe enviar el PDF para sellarse"
      LET v_sello = ""
   ELSE 
      DISPLAY "Se envía PDF para sellarse :"
      CALL fn_obtiene_sellado_pdf(p_pdf, p_rfc_funcionario, "CIUDAD DE MEXICO") RETURNING v_result, v_error, v_sello 
      IF v_result = TRUE THEN 
         DISPLAY "Documento PDF correctamente sellado"
      ELSE 
         LET v_sello = ""
         DISPLAY "Problemas al sellar el documento PDF, error ", v_error
      END IF 
   END IF 
   RETURN v_sello
END FUNCTION
