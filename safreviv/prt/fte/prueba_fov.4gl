MAIN 
DEFINE
  v_entrada RECORD
     v_nss        CHAR(11),
     v_curp       CHAR(18),
     v_id_credito DECIMAL(10,0),
     v_folio      DECIMAL(10,0)     
  END RECORD,
  v_wserror INTEGER,
  salida RECORD
   Curp CHAR(18),
   FolioFovissste DECIMAL(10,0),
   FolioInfonavit DECIMAL(10,0),
   IdDiagnostico  SMALLINT,
   IdMotivoRechazo STRING ATTRIBUTE(XMLName="IdMotivoRechazo",XMLOptional),
   IdTipoCredito CHAR(2),
   Nss CHAR(11),
   NumCredito CHAR(10),
   SaldoAdeudo DECIMAL(13,2)
  END RECORD

   LET v_entrada.v_nss        = ARG_VAL(1)
   LET v_entrada.v_curp       = ARG_VAL(2)
   LET v_entrada.v_id_credito = ARG_VAL(3)
   LET v_entrada.v_folio      = ARG_VAL(4)

   CALL fn_validarCredito(v_entrada.v_nss,
                          v_entrada.v_curp,
                          v_entrada.v_id_credito,
                          v_entrada.v_folio) RETURNING v_wserror,salida.*

   DISPLAY v_wserror
   DISPLAY salida.*

END MAIN