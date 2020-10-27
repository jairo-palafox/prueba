IMPORT SECURITY

{
======================================================================
Nombre: fn_importe_monto
Fecha creacion: agosto 03, 2015
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
  Funcion que modifica un importe en número al mismo importe en letra

Registro de modificaciones:
======================================================================
}
FUNCTION fn_importe_monto(monto)
   DEFINE monto      DECIMAL(10,2)
   DEFINE monto1     DECIMAL(10,2)
   DEFINE monto2     DECIMAL(10,2)
   DEFINE c_monto    STRING
   DEFINE monto_c    STRING
   DEFINE largo      SMALLINT
   DEFINE pos_pto    SMALLINT
   DEFINE centavos   SMALLINT
   DEFINE millones   SMALLINT
   DEFINE miles      SMALLINT
   DEFINE unos       SMALLINT

   --Obtengo los valores para cada unidad
   LET c_monto    = monto
   LET largo      = c_monto.getLength()
   LET pos_pto    = c_monto.getIndexOf(".",1)
   LET monto_c    = c_monto.subString(pos_pto+1,largo)
   LET centavos   = monto_c
   LET millones   = monto / 1000000
   LET monto1     = monto - (millones * 1000000)
   LET miles      = monto1 / 1000
   LET monto2     = monto1 - (miles * 1000)
   LET unos       = monto2 / 1
   LET c_monto    = ""
   DISPLAY "Valores Recibidos"
   DISPLAY "c_monto    = ",c_monto
   DISPLAY "largo      = ",largo
   DISPLAY "pos_pto    = ",pos_pto
   DISPLAY "monto_c    = ",monto_c
   DISPLAY "centavos   = ",centavos
   DISPLAY "millones   = ",millones
   DISPLAY "monto1     = ",monto1
   DISPLAY "miles      = ",miles
   DISPLAY "monto2     = ",monto2
   DISPLAY "unos       = ",unos
   --Defino el nivel y realizo la transformación
   IF millones > 0 THEN
      CALL fn_cadena_nivel(millones,3) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   DISPLAY "c_monto millones =",c_monto
   IF miles > 0 THEN
      CALL fn_cadena_nivel(miles,2) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   DISPLAY "c_monto miles =",c_monto
   IF unos > 0 THEN
      CALL fn_cadena_nivel(unos,1) RETURNING monto_c
      LET c_monto = c_monto.append(monto_c)
   END IF
   DISPLAY "c_monto unos =",c_monto
   IF millones > 0 AND miles = 0 AND unos = 0 THEN
      LET c_monto = c_monto.append("de ")
   END IF
   DISPLAY "c_monto millones > 0 =",c_monto
   IF (millones+miles+unos) = 0 THEN
      LET c_monto  = "cero pesos " , centavos USING "&&" , "/100 M.N."
   ELSE
      IF monto >= 2 THEN
         LET monto_c = "pesos " , centavos USING "&&" , "/100 M.N."
      ELSE
         LET monto_c = "peso " , centavos USING "&&" , "/100 M.N."
      END IF
      LET c_monto = c_monto.append(monto_c)
   END IF
   DISPLAY "c_monto respuesta =",c_monto
   RETURN c_monto
   
END FUNCTION

{
======================================================================
Nombre: fn_cadena_nivel
Fecha creacion: agosto 03, 2015
Autor: Luis Felipe Prieto, EFP
Narrativa del proceso que realiza:
  Funcion que obtiene la cadena en base al nivel
     - Millones
     - Miles
     - Cientos

Registro de modificaciones:
======================================================================
}
FUNCTION fn_cadena_nivel(numero,grupo)
   DEFINE numero,grupo  SMALLINT
   DEFINE c_numero      CHAR(3)
   DEFINE unidades      SMALLINT
   DEFINE decenas       SMALLINT
   DEFINE centenas      SMALLINT
   DEFINE c_l           STRING
   
   LET c_numero = numero USING "&&&"
   LET centenas = c_numero[1]
   LET decenas  = c_numero[2]
   LET unidades = c_numero[3]
   IF centenas > 0 THEN
      CASE centenas
         WHEN 9 LET c_l = "novecientos "
         WHEN 8 LET c_l = "ochocientos "
         WHEN 7 LET c_l = "setecientos "
         WHEN 6 LET c_l = "seiscientos "
         WHEN 5 LET c_l = "quinientos "
         WHEN 4 LET c_l = "cuatrocientos "
         WHEN 3 LET c_l = "trescientos "
         WHEN 2 LET c_l = "doscientos "
         WHEN 1
            IF unidades = 0 AND decenas = 0 THEN
               LET c_l = "cien "
            ELSE
               LET c_l = "ciento "
            END IF
      END CASE
   END IF
   IF decenas > 0 THEN
      IF decenas > 2 THEN
         CASE decenas
            WHEN 9 LET c_l = c_l.append("noventa ")
            WHEN 8 LET c_l = c_l.append("ochenta ")
            WHEN 7 LET c_l = c_l.append("setenta ")
            WHEN 6 LET c_l = c_l.append("sesenta ")
            WHEN 5 LET c_l = c_l.append("cincuenta ")
            WHEN 4 LET c_l = c_l.append("cuarenta ")
            WHEN 3 LET c_l = c_l.append("treinta ")
         END CASE
         IF unidades > 0 THEN
            LET c_l = c_l.append("y ")
         END IF
      ELSE
         CASE decenas
            WHEN 2 
               IF unidades = 0 THEN  
                  LET c_l = c_l.append("veinte ")
               ELSE
                  LET c_l = c_l.append("veinti")
               END IF
            WHEN 1
               IF unidades = 0 THEN
                  LET c_l = c_l.append("diez ")
               ELSE
                  IF unidades > 5 THEN
                     LET c_l = c_l.append("dieci")
                  END IF
               END IF
         END CASE
      END IF
   END IF
   IF unidades > 0 THEN
      CASE unidades
         WHEN 9 LET c_l = c_l.append("nueve ")
         WHEN 8 LET c_l = c_l.append("ocho ")
         WHEN 7 LET c_l = c_l.append("siete ")
         WHEN 6 LET c_l = c_l.append("seis ")
         WHEN 5
            IF decenas = 1 THEN
               LET c_l = c_l.append("quince ")
            ELSE
               LET c_l = c_l.append("cinco ")
            END IF
         WHEN 4
            IF decenas = 1 THEN
               LET c_l = c_l.append("catorce ")
            ELSE
               LET c_l = c_l.append("cuatro ")
            END IF
         WHEN 3
            IF decenas = 1 THEN
               LET c_l = c_l.append("trece ")
            ELSE
               LET c_l = c_l.append("tres ")
            END IF
         WHEN 2
            IF decenas = 1 THEN
               LET c_l = c_l.append("doce ")
            ELSE
               LET c_l = c_l.append("dos ")
            END IF
         WHEN 1
            IF decenas = 1 THEN
               LET c_l = c_l.append("once ")
            ELSE
               IF grupo = 2 THEN
                  IF numero > 1 THEN
                     LET c_l = c_l.append("un ")
                  END IF
               ELSE
                  LET c_l = c_l.append("un ")
               END IF
            END IF
      END CASE
   END IF
   CASE grupo
      WHEN 3 
         IF numero > 1 THEN
            LET c_l = c_l.append("millones ")
         ELSE
            LET c_l = c_l.append("millón ")
         END IF
      WHEN 2
         LET c_l = c_l.append("mil ")
   END CASE
   --LET c_l = c_l.toUpperCase()
   RETURN c_l
END FUNCTION

--Función que general el reporte de acta de finiquito
FUNCTION fn_genera_reporte_acta_finiquito(p_id_solicitud,
                                          p_fecha,p_causal,
                                          p_nombre,
                                          p_beneficiario,
                                          p_rfc,
                                          p_nss,
                                          p_monto_causal,
                                          p_monto_tanto_adicional,
                                          p_imp_pagar,
                                          p_dap,
                                          p_b_despliegue_pantalla,
                                          p_consecutivo)
   DEFINE
      p_id_solicitud       DECIMAL(9,0),
      p_fecha              DATE,
      p_causal             SMALLINT,
      v_desc_causal        CHAR(50),
      p_nombre             VARCHAR(60),
      p_beneficiario       CHAR(50),
      p_rfc                CHAR(13),
      p_nss                CHAR(11),
      p_monto_causal       DECIMAL(10,2),
      p_monto_tanto_adicional DECIMAL(10,2),
      p_imp_pagar          DECIMAL(10,2),
      p_dap                CHAR(15),
      v_monto_letra        STRING,
      v_ruta_reporte       STRING ,-- ruta del archivo del reporte       
      v_ruta_listados      STRING ,-- ruta de los listados
      v_ruta_ejecutable    STRING ,-- ruta del ejecutable
      manejador_rpt        om.SaxDocumentHandler,
      p_b_despliegue_pantalla SMALLINT,
      p_consecutivo        SMALLINT 

   -- Obtengo la descripcion larga del causal
   SELECT desc_larga
     INTO v_desc_causal
     FROM ret_causal_excepcion
    WHERE causal_excepcion = p_causal

   -- Genero importe a pagar en letra
   CALL fn_importe_monto(p_imp_pagar) RETURNING v_monto_letra
   
   -- Recupera la ruta de listados en el que se enviara el archivo
   CALL fn_rutas("ret") RETURNING v_ruta_ejecutable, v_ruta_listados

   -- Obtengo el nombre del reporte a generar
   LET v_ruta_reporte = v_ruta_listados.trim() , "/" ,
                        "acta_finiquito_",
                        p_id_solicitud USING "&&&&&&&&&",
                        "_",
                        p_consecutivo USING "&", -- Id de Solicitud
                        ".pdf"

   -- Se asigna la plantilla para generar el reporte
   
   IF ( fgl_report_loadCurrentSettings("../../ret/bin/RETM2771.4rp") ) THEN
      CALL fgl_report_selectDevice ("PDF")
      -- si no se pidio el reporte en pantalla
      IF (p_b_despliegue_pantalla) THEN
         CALL fgl_report_selectPreview(TRUE)
      ELSE
         -- sin preview
         CALL fgl_report_selectPreview(FALSE)
      END IF
      CALL fgl_report_setOutputFileName(v_ruta_reporte)
      -- se indica que se escriba en archivo
      LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE         
      CALL fn_mensaje("Atención","No fue posible generar el reporte. No se encuentra la plantilla RETM2771.4rp", "stop")
      RETURN NULL
   END IF


   --Inicia el reporte de Acta de Finiquito por DAP
   START REPORT rpt_acta_finiquito_dap TO XML HANDLER manejador_rpt
   OUTPUT TO REPORT rpt_acta_finiquito_dap(p_fecha,
                                       v_desc_causal,
                                       p_nombre,
                                       p_beneficiario,
                                       p_rfc,
                                       p_nss,
                                       p_monto_causal,
                                       p_monto_tanto_adicional,
                                       p_imp_pagar,
                                       v_monto_letra,
                                       p_dap)
   FINISH REPORT rpt_acta_finiquito_dap
   

   --Se regresa la ubicacion del archivo
   RETURN v_ruta_reporte
   
END FUNCTION

REPORT rpt_acta_finiquito_clabe(p_desc_causal,p_nombre,p_beneficiario,p_rfc,p_nss,p_monto_causal,p_monto_tanto_adicional,p_imp_pagar,p_monto_letra,p_clabe_bancaria)
   DEFINE
      p_desc_causal        VARCHAR(50),
      p_nombre             VARCHAR(60),
      p_beneficiario       CHAR(50),
      p_rfc                CHAR(13),
      p_nss                CHAR(11),
      p_monto_causal       DECIMAL(10,2),
      p_monto_tanto_adicional DECIMAL(10,2),
      p_imp_pagar          DECIMAL(10,2),
      p_clabe_bancaria     CHAR(18),
      p_monto_letra        STRING
   FORMAT
      FIRST PAGE HEADER
         PRINTX p_desc_causal,p_nombre,p_beneficiario,p_rfc,p_nss,p_monto_causal,
         p_monto_tanto_adicional,p_imp_pagar,p_clabe_bancaria,p_monto_letra
END REPORT

REPORT rpt_acta_finiquito_dap(p_fecha,p_desc_causal,p_nombre,p_beneficiario,p_rfc,p_nss,p_monto_causal,p_monto_tanto_adicional,p_imp_pagar,p_monto_letra,p_dap)
   DEFINE
      p_fecha              DATE,
      p_desc_causal        VARCHAR(50),
      p_nombre             VARCHAR(60),
      p_beneficiario       CHAR(50),
      p_rfc                CHAR(13),
      p_nss                CHAR(11),
      p_monto_causal       DECIMAL(10,2),
      p_monto_tanto_adicional DECIMAL(10,2),
      p_imp_pagar          DECIMAL(10,2),
      p_dap                CHAR(15),
      p_monto_letra        STRING
   FORMAT
      FIRST PAGE HEADER
         PRINTX p_fecha,p_desc_causal,p_nombre,p_beneficiario,p_rfc,p_nss,p_monto_causal,
         p_monto_tanto_adicional,p_imp_pagar,p_dap,p_monto_letra
END REPORT
PUBLIC FUNCTION fn_load_pdf(arr_reporte, p_caso)
   DEFINE arr_byte_archivo DYNAMIC ARRAY OF RECORD 
          archivo           BYTE
   END RECORD 
   DEFINE arr_archivo RECORD 
          v_archivo_reporte STRING,
          v_archivo_1       STRING,
          v_archivo_2       STRING,
          v_archivo_3       STRING
   END RECORD 
   DEFINE arr_reporte RECORD
          v_ruta_reporte_1    STRING,
          v_ruta_reporte_2    STRING,
          v_ruta_reporte_3    STRING
   END RECORD 
   DEFINE v_comando         STRING 
   DEFINE p_caso            CHAR(10)             
   DEFINE v_resultado       SMALLINT
   DEFINE v_indice          SMALLINT   
   LET arr_archivo.v_archivo_reporte = 'ActaFiniquito'
   LET v_indice = 0
      
   IF arr_reporte.v_ruta_reporte_1 IS NOT NULL THEN 
      LET v_indice = v_indice + 1
      LOCATE arr_byte_archivo[v_indice].archivo IN MEMORY
      #DISPLAY v_ruta_reporte
      CALL arr_byte_archivo[v_indice].archivo.readFile(arr_reporte.v_ruta_reporte_1)
      CALL security.Base64.LoadBinary(arr_reporte.v_ruta_reporte_1) RETURNING arr_archivo.v_archivo_1
   END IF 
   IF arr_reporte.v_ruta_reporte_2 IS NOT NULL THEN
      LET v_indice = v_indice + 1 
      LOCATE arr_byte_archivo[v_indice].archivo IN MEMORY
      #DISPLAY v_ruta_reporte
      CALL arr_byte_archivo[v_indice].archivo.readFile(arr_reporte.v_ruta_reporte_2)
      CALL security.Base64.LoadBinary(arr_reporte.v_ruta_reporte_2) RETURNING arr_archivo.v_archivo_2
   END IF 
   IF arr_reporte.v_ruta_reporte_3 IS NOT NULL THEN
      LET v_indice = v_indice + 1
      LOCATE arr_byte_archivo[v_indice].archivo IN MEMORY
      #DISPLAY v_ruta_reporte
      CALL arr_byte_archivo[v_indice].archivo.readFile(arr_reporte.v_ruta_reporte_3)
      CALL security.Base64.LoadBinary(arr_reporte.v_ruta_reporte_3) RETURNING arr_archivo.v_archivo_3
   END IF 
--   IF arr_reporte.v_ruta_reporte_4 IS NOT NULL THEN
--      LET v_indice = v_indice + 1
--      LOCATE arr_byte_archivo[v_indice].archivo IN MEMORY
      #DISPLAY v_ruta_reporte
--      CALL arr_byte_archivo[v_indice].archivo.readFile(arr_reporte.v_ruta_reporte_4)
--      CALL security.Base64.LoadBinary(arr_reporte.v_ruta_reporte_4) RETURNING arr_archivo.v_archivo_4
--   END IF 
--   IF arr_reporte.v_ruta_reporte_5 IS NOT NULL THEN
--      LET v_indice = v_indice + 1
--      LOCATE arr_byte_archivo[v_indice].archivo IN MEMORY
      #DISPLAY v_ruta_reporte
--      CALL arr_byte_archivo[v_indice].archivo.readFile(arr_reporte.v_ruta_reporte_5)
--      CALL security.Base64.LoadBinary(arr_reporte.v_ruta_reporte_3) RETURNING arr_archivo.v_archivo_5
--   END IF 

   DISPLAY "Parámetros enviados a la Rutina de Adjunta Multi Documentos"
--   DISPLAY "v_archivo_reporte: ", v_archivo_reporte
--   DISPLAY "El archivo en base 64", v_archivo
   CALL fn_adjunta_documento(arr_archivo.*, p_caso) RETURNING v_resultado
   IF arr_reporte.v_ruta_reporte_1 IS NOT NULL THEN 
      LET v_comando="rm "||arr_reporte.v_ruta_reporte_1
      RUN v_comando 
   END IF 
   IF arr_reporte.v_ruta_reporte_2 IS NOT NULL THEN 
      LET v_comando="rm "||arr_reporte.v_ruta_reporte_2
      RUN v_comando 
   END IF 
   IF arr_reporte.v_ruta_reporte_3 IS NOT NULL THEN 
      LET v_comando="rm "||arr_reporte.v_ruta_reporte_3
      RUN v_comando 
   END IF 
--   IF arr_reporte.v_ruta_reporte_4 IS NOT NULL THEN 
--      LET v_comando="rm "||arr_reporte.v_ruta_reporte_4
--      RUN v_comando 
--   END IF 
--   IF arr_reporte.v_ruta_reporte_5 IS NOT NULL THEN 
--      LET v_comando="rm "||arr_reporte.v_ruta_reporte_5
--      RUN v_comando 
--   END IF 
   
END FUNCTION

FUNCTION fn_adjunta_documento(arr_documentos, p_caso)
DEFINE p_nombre_archivo STRING 
DEFINE p_archivo        STRING 
DEFINE p_caso           CHAR(10)
DEFINE v_regreso        SMALLINT 
DEFINE v_codigo         INTEGER 

   DEFINE arr_documentos RECORD
         nombre_documento STRING, 
         documento_1      STRING,
         documento_2      STRING,
         documento_3      STRING 
   END RECORD 

  DISPLAY "Parametros recibidos para el consumo de la funcion de documentos"
--  DISPLAY "p_archivo: ",p_archivo
  DISPLAY "p_nombre_archivo: ", p_nombre_archivo
   LET v_regreso = 0
   CALL fn_adjunta_multidocto_crm(p_caso, arr_documentos.*) RETURNING v_regreso, v_codigo
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
