IMPORT JAVA Sello.ClienteSelloDigital 
IMPORT JAVA Certificado.ObtieneCertificado
IMPORT JAVA Firma.FirmaPDF
IMPORT JAVA SelladoPDF.SellaPDF

IMPORT util
IMPORT security


SCHEMA safre_viv

#+ Función para la generación del Sello digital
#+ fn_llama_servicio_rest(p_cadena) RETURNING v_result, v_error, v_sello
#+
#+ La función llama al servicio REST para la obtención de un sello digital enviando como parametro la cadena de caracteres
#+
#+ @code
#+ CALL fn_llama_servicio_rest(p_cadena) RETURING v_result, v_error, v_sello
#+
#+ @param p_cadena STRING Cadena de caracteres para la obtención del sello digital
#+
#+ @return v_result SMALLINT TRUE/FALSE Indica si el llamado a la función fue correcto
#+
#+ @return v_error STRING Mensaje de error 
#+
#+ @return v_sello STRING Sello digital generado
#+
#+ códigos HASH permitidos:
#+   - SHA1 (Recomendado)
#+   - SHA512
#+   - SHA384
#+   - SHA256
#+   - SHA224
#+   - MD5
#+ 


FUNCTION fn_obtiene_sello_digital(p_cadena)
DEFINE p_cadena STRING

DEFINE error_code INTEGER
DEFINE error_desc STRING
DEFINE error_text STRING


DEFINE cadena_b64 STRING 

DEFINE algo STRING
DEFINE dgst security.Digest

DEFINE cadena_json STRING 
DEFINE cadena_json_response STRING 

DEFINE obj_cliente ClienteSelloDigital  
DEFINE v_resp_jar STRING

    LET obj_cliente = ClienteSelloDigital.create()

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA256" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(p_cadena)
      LET cadena_b64 = dgst.DoBase64Digest()
      -- Se llama al jar del Sello Digital
      LET v_resp_jar = obj_cliente.ClienteSelloDigital(cadena_b64)
      
     -- DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION

FUNCTION fn_obtiene_sello_pdf(p_cer, p_pdf)
DEFINE p_cer  BYTE
DEFINE p_pdf  BYTE

DEFINE error_code INTEGER
DEFINE error_desc STRING
DEFINE error_text STRING


DEFINE address STRING
DEFINE cadena_b64 STRING 

DEFINE algo STRING
DEFINE dgst security.Digest

DEFINE cadena_json STRING 
DEFINE cadena_json_response STRING 

DEFINE obj_cliente ClienteSelloDigital  
DEFINE v_resp_jar STRING

    LET obj_cliente = ClienteSelloDigital.create()

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA256" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(p_cer)
      LET cadena_b64 = dgst.DoBase64Digest()
      -- Se llama al jar del Sello Digital
      LET v_resp_jar = obj_cliente.ClienteSelloDigital(cadena_b64)
      
      --DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION

FUNCTION fn_obtiene_certificado(p_cadena, p_rfc)
DEFINE p_cadena STRING
DEFINE p_rfc    STRING

DEFINE error_code INTEGER
DEFINE error_desc STRING
DEFINE error_text STRING


DEFINE cadena_b64 STRING 

DEFINE algo STRING
DEFINE dgst security.Digest

DEFINE cadena_json STRING 
DEFINE cadena_json_response STRING 

DEFINE obj_cliente ObtieneCertificado  
DEFINE v_resp_jar STRING

    LET obj_cliente = ObtieneCertificado.create()

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA256" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(p_cadena)
      LET cadena_b64 = dgst.DoBase64Digest()
      -- Se llama al jar del Sello Digital
      LET v_resp_jar = obj_cliente.ObtieneCertificado(cadena_b64, p_rfc)
      
     -- DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION

FUNCTION fn_obtiene_firma(p_certificado, p_pdf)
DEFINE p_certificado STRING
DEFINE p_pdf         STRING

DEFINE error_code INTEGER
DEFINE error_desc STRING
DEFINE error_text STRING


DEFINE cadena_b64 STRING 

DEFINE algo STRING
DEFINE dgst security.Digest

DEFINE cadena_json STRING 
DEFINE cadena_json_response STRING 

DEFINE obj_cliente FirmaPDF  
DEFINE v_resp_jar STRING

    LET obj_cliente = FirmaPDF.create()

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA256" --(Default)
   END IF

   TRY
      LET dgst = security.Digest.CreateDigest(algo)
      CALL dgst.AddStringData(p_certificado)
      LET cadena_b64 = dgst.DoBase64Digest()
      -- Se llama al jar del Sello Digital
      DISPLAY "Se envia a Firma"
--      DISPLAY "PDF ", p_pdf
      LET v_resp_jar = obj_cliente.FirmaPDF(p_certificado, p_pdf)
      
--      DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION

FUNCTION fn_obtiene_sellado_pdf(p_pdf, p_rfc, p_localidad)
DEFINE p_pdf       STRING
DEFINE p_rfc       STRING 
DEFINE p_localidad STRING


DEFINE error_code INTEGER
DEFINE error_desc STRING
DEFINE error_text STRING


DEFINE cadena_b64 STRING 

DEFINE algo STRING
DEFINE dgst security.Digest

DEFINE cadena_json STRING 
DEFINE cadena_json_response STRING 

DEFINE obj_cliente SellaPDF  
DEFINE v_resp_jar STRING

    LET obj_cliente = SellaPDF.create()

   IF algo IS NULL OR algo = "" THEN
      LET algo = "SHA256" --(Default)
   END IF

   TRY
--      LET dgst = security.Digest.CreateDigest(algo)
--      CALL dgst.AddStringData(p_pdf)
--      LET cadena_b64 = dgst.DoBase64Digest()
      -- Se llama al jar del Sello Digital
      LET v_resp_jar = obj_cliente.SellaPDF(p_pdf, p_rfc, p_localidad)
      
      DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION
