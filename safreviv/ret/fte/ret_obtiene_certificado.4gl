IMPORT JAVA Sello.ObtieneCertificado
IMPORT util
IMPORT security


SCHEMA safre_viv

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
      
      DISPLAY "La respuesta del JAR: ",v_resp_jar
      --LET cadena_b64 = dgst.DoHexBinaryDigest()
      RETURN TRUE, " ", v_resp_jar
   CATCH
      DISPLAY "ERROR : ", STATUS, " - ", SQLCA.SQLERRM
      RETURN FALSE, error_desc, " "
   END TRY
END FUNCTION
