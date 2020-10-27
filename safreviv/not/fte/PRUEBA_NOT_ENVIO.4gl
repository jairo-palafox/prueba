
IMPORT security
GLOBALS "NOTW02.inc"
DATABASE safre_viv


DEFINE v_inn         tDT_CargarCampana_in
DEFINE v_outt        tDT_CargarCampana_out

DEFINE p_nombre_archivo     STRING 

MAIN
    DEFINE archivo           STRING
    DEFINE v_ruta_archivo    STRING 
    DEFINE v_ruta            char(40)
    LET p_nombre_archivo= ARG_VAL(1)


    SELECT ruta_envio
    INTO  v_ruta
    FROM seg_modulo
    WHERE modulo_cod='not'

    LET v_ruta_archivo=v_ruta CLIPPED || "/" ||p_nombre_archivo

    
   CALL security.Base64.LoadBinary(v_ruta_archivo) RETURNING archivo
   LET v_inn.archivoB64 = archivo CLIPPED
   LET v_inn.fechaOperacion = TODAY USING 'yyyymmdd'
   LET v_inn.nombreArchivo = fn_elimina_extension(p_nombre_archivo CLIPPED)
   LET v_inn.idCampana = p_nombre_archivo.subString(1,7)   #"SACI_06"
   CALL fn_envia_notificacion(v_inn.*) RETURNING  v_outt.*
   DISPLAY v_outt.*
    


END MAIN 

FUNCTION fn_elimina_extension(p_nombre_archivo)
    DEFINE p_nombre_archivo STRING 
    DEFINE v_nom_arch       varchar(100)
    LET p_nombre_archivo=p_nombre_archivo.subString(1,p_nombre_archivo.getIndexOf(".",1)-1)
    LET v_nom_arch=p_nombre_archivo CLIPPED 
    RETURN v_nom_arch
END FUNCTION 