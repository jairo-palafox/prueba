DATABASE safre_viv

MAIN

   DEFINE noCaso DECIMAL(10,0)
   DEFINE salida RECORD
      s_nss     CHAR(11),
      s_noCaso  DECIMAL(10,0),
      s_paterno CHAR(40), 
      s_materno CHAR(40),
      s_nombre  CHAR(40),
      s_estatus SMALLINT,
      s_diag    SMALLINT
   END RECORD

   LET noCaso = ARG_VAL(1)

   IF noCaso IS NULL THEN
      DISPLAY "ingrese numero de caso"
   ELSE       
      CALL fn_genera_diagnostico_cancelacion(noCaso)
         RETURNING salida.s_nss,
                salida.s_noCaso,
                salida.s_paterno,
                salida.s_materno,
                salida.s_nombre,
                salida.s_estatus,
                salida.s_diag             

      DISPLAY salida.*
   END IF
             
END MAIN
