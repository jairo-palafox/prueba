################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTW11.inc                                               #
#Objetivo          => Archivo de variables para solicitud traspaso receptora   #
#Fecha inicio      => 07 Abril 2015                                            #
################################################################################

GLOBALS

   CONSTANT SERVICIO_INICIADOR   SMALLINT = 2 # tipo de cliente iniciador
   
   #Constantes para el manejo de los codigos de respuesta
   CONSTANT COD_OPR_OK        CHAR(2) = 'AC'
   CONSTANT COD_OPR_ERROR     CHAR(2) = 'ER'

   CONSTANT COD_OPER_CLIENTE                      CHAR(50) = 'CODIGO_SAFRE'

   CONSTANT COD_ERROR_ENVIO                       SMALLINT = 1

   CONSTANT CAMPO_ARREGLO                         CHAR(1) = 'A'

   TYPE detalle_request RECORD
      num_campo            INTEGER,
      nombre_campo         VARCHAR(60),
      valor                VARCHAR(100)
   END RECORD

   TYPE rechazos RECORD
      cod_rechazo          VARCHAR(6),
      desc_rechazo         VARCHAR(100)
   END RECORD
   
END GLOBALS