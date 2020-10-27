IMPORT com

GLOBALS "RETWS_CONSULTA_DATAMART.inc"

-- Programa cliente de Webservice de consulta de Datamart en SAFRE-Vivienda
MAIN
  DEFINE op1        INTEGER
  DEFINE op2        INTEGER
  DEFINE result     tfn_consulta_datamartResponse_fn_consulta_datamartResponse
  DEFINE wsstatus   INTEGER
  DEFINE v_nss      CHAR(11), -- NSS consultado
         v_rfc      CHAR(13)  -- RFC consultado
  
  LET op1 = 1
  LET op2 = 2

  OPEN WINDOW w_consulta_ws WITH FORM "RETWS_CDATAMART01"
{
  brokerok SMALLINT ATTRIBUTE(XSDshort,XMLName="brokerok"),
  nss STRING ATTRIBUTE(XSDMaxLength="11",XMLName="nss"),
  curp STRING ATTRIBUTE(XSDMaxLength="18",XMLName="curp"),
  nombre STRING ATTRIBUTE(XSDMaxLength="40",XMLName="nombre"),
  nombreafore STRING ATTRIBUTE(XSDMaxLength="40",XMLName="nombreafore"),
  paternoafore STRING ATTRIBUTE(XSDMaxLength="40",XMLName="paternoafore"),
  maternoafore STRING ATTRIBUTE(XSDMaxLength="40",XMLName="maternoafore"),
  secuencia SMALLINT ATTRIBUTE(XSDshort,XMLName="secuencia"),
  tipomovimiento STRING ATTRIBUTE(XSDMaxLength="3",XMLName="tipomovimiento"),
  regimen SMALLINT ATTRIBUTE(XSDshort,XMLName="regimen"),
  tiposeguro STRING ATTRIBUTE(XSDMaxLength="2",XMLName="tiposeguro"),
  tipopension STRING ATTRIBUTE(XSDMaxLength="2",XMLName="tipopension"),
  tipoprestacion STRING ATTRIBUTE(XSDMaxLength="2",XMLName="tipoprestacion"),
  articulonegativa STRING ATTRIBUTE(XSDMaxLength="3",XMLName="articulonegativa"),
  fraccionnegativa STRING ATTRIBUTE(XSDMaxLength="2",XMLName="faccionnegativa"),
  considerando STRING ATTRIBUTE(XSDMaxLength="2",XMLName="considerando"),
  usuario STRING ATTRIBUTE(XSDMaxLength="20",XMLName="usuario"),
  fechainiciopension DATE ATTRIBUTE(XMLName="fechainiciopension"),
  fechaemisionresolucion DATE ATTRIBUTE(XMLName="fechaemisionresolucion"),
  numeroresolucion STRING ATTRIBUTE(XSDMaxLength="10",XMLName="numeroresolucion"),
  porcentajeevaluacion DECIMAL(5,2) ATTRIBUTE(XSDTotalDigits="5",XSDFractionDigits="2",XMLName="porcentajeevaluacion"),
  semanascotizadas INTEGER ATTRIBUTE(XMLName="semanascotizadas"),
  diagnosticoregistro STRING ATTRIBUTE(XSDMaxLength="3",XMLName="diagnosticoregistro"),
  fechacargadatamart DATE ATTRIBUTE(XMLName="fechacargadatamart"),
  fechaproceso DATE ATTRIBUTE(XMLName="fechaproceso"),
  mensaje STRING ATTRIBUTE(XMLName="mensaje")

}
  
  INPUT v_nss, v_rfc
  WITHOUT DEFAULTS 
  FROM v_nss, v_rfc
  ATTRIBUTES ( UNBUFFERED )

     ON ACTION limpiar
        -- se limpia la pantalla
        LET v_nss = NULL
        LET v_rfc = NULL
        
        LET result.brokerok               = NULL
        LET result.nss 	                  = NULL
        LET result.curp 	                = NULL
        LET result.nombre 	              = NULL
        LET result.nombreafore 	          = NULL
        LET result.paternoafore           = NULL
        LET result.maternoafore           = NULL
        LET result.secuencia              = NULL
        LET result.tipomovimiento         = NULL
        LET result.regimen 	              = NULL
        LET result.tiposeguro 	          = NULL
        LET result.tipopension 	          = NULL
        LET result.tipoprestacion 	      = NULL
        LET result.articulonegativa       = NULL
        LET result.fraccionnegativa 	    = NULL
        LET result.considerando 	        = NULL
        LET result.usuario 	              = NULL
        LET result.fechainiciopension 	  = NULL
        LET result.fechaemisionresolucion = NULL
        LET result.numeroresolucion 	    = NULL
        LET result.porcentajeevaluacion 	= NULL 
        LET result.semanascotizadas 	    = NULL
        LET result.diagnosticoregistro 	  = NULL
        LET result.fechacargadatamart 	  = NULL
        LET result.fechaproceso 	        = NULL
        LET result.mensaje 	              = NULL
        DISPLAY BY NAME  result.*

     ON ACTION ACCEPT
  
        CALL fn_consulta_datamart(v_nss, v_rfc) RETURNING wsstatus, result.*

        IF ( wsstatus = 0 ) THEN
           DISPLAY "Resultado: ", result.*

           -- se despliega el resultado de la consulta
           DISPLAY BY NAME  result.brokerok              ,
                            result.nss 	                ,
                            result.curp 	                ,
                            result.nombre 	              ,
                            result.nombreafore 	        ,
                            result.paternoafore          ,
                            result.maternoafore          ,
                            result.secuencia             ,
                            result.tipomovimiento        ,
                            result.regimen 	            ,
                            result.tiposeguro 	          ,
                            result.tipopension 	        ,
                            result.tipoprestacion 	      ,
                            result.articulonegativa      ,
                            result.fraccionnegativa 	    ,
                            result.considerando 	        ,
                            result.usuario 	            ,
                            result.fechainiciopension 	  ,
                            result.fechaemisionresolucion,
                            result.numeroresolucion 	    ,
                            result.porcentajeevaluacion 	,
                            result.semanascotizadas 	    ,
                            result.diagnosticoregistro 	,
                            result.fechacargadatamart 	  ,
                            result.fechaproceso 	        ,
                            result.mensaje 	            


           MENU

              COMMAND "Regresar"
                 EXIT MENU
           END MENU
        ELSE

           -- Use the global wsError record
           DISPLAY "Error: ", wsError.description
        END IF

  END INPUT

  CLOSE WINDOW w_consulta_ws 

  END MAIN
