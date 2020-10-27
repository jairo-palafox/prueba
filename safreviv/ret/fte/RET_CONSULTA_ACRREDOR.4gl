GLOBALS "RET_CACRREDOR.inc"

MAIN

  -- datos de entrada
DEFINE v_rfc      CHAR(16),
       v_acreedor CHAR(10),
       v_cuenta   CHAR(18),
       v_contador  INTEGER,
       v_status   SMALLINT

   -- se invoca la consulta
   DISPLAY "Invocando consulta de acreedor"

   LET v_rfc = "VEHI810205TY1"
   LET v_acreedor = "AAAA009988"
   LET v_cuenta = "441223344556677889"

   LET ns1consultarAcreedor.rfc = v_rfc
   LET ns1consultarAcreedor.acreedor = v_acreedor
   LET ns1consultarAcreedor.cuenta = v_cuenta
   
   CALL consultarAcreedor_g() RETURNING v_status

   DISPLAY "Estado de la consulta: ", v_status

   -- se verifica la salida
  DISPLAY "retorno    : ", ns1consultarAcreedorResponse.acreedor.retorno 
  DISPLAY "acreedor   : ", ns1consultarAcreedorResponse.acreedor.acreedor 
  DISPLAY "grupoCuenta: ", ns1consultarAcreedorResponse.acreedor.grupoCuenta

  FOR v_contador = 1 TO ns1consultarAcreedorResponse.acreedor.detalle.getLength()

     DISPLAY "acreedor  : ", ns1consultarAcreedorResponse.acreedor.detalle[v_contador].acreedor
     DISPLAY "claveBanco: ", ns1consultarAcreedorResponse.acreedor.detalle[v_contador].claveBanco
              
  END FOR
  
END MAIN