####################################################################
#Modulo            =>BUS                                           #
#Programa          =>CLIENTE_SUMA                                  #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>19 NOVIEMBRE 2013                             #
####################################################################

GLOBALS

   CONSTANT WS_PAGOS_TRM CHAR(5) = 'pag_1'
   
   TYPE consultaPago RECORD
      NSS STRING,
      NRP STRING,
      folioSUA STRING,
      fechaPago STRING
   END RECORD

   TYPE respuestaPagosHistoricos RECORD 
      fechaEntradaTRM STRING,
      fechaPago STRING,
      fechaProceso STRING,
      folioSUA STRING,
      ICodigo STRING,
      IDescripcion STRING,
      idConceptoPago STRING,
      importePago STRING,
      NRP STRING,
      NSS STRING,
      noDocCtaCorrienteContractual STRING,
      periodoPago STRING
   END RECORD


END GLOBALS