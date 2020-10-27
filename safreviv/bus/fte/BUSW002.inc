####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSWS02.inc                                   #
#Objetivo          =>Archivo de variables globales                 #
#Fecha inicio      =>21 NOVIEMBRE 2013                             #
####################################################################

GLOBALS

   CONSTANT ORIGEN_AFORE   SMALLINT = 1
   
   #Constantes para el manejo de los codigos de respuesta
   CONSTANT COD_OPR_OK        CHAR(2) = 'AC'
   CONSTANT COD_OPR_ERROR     CHAR(2) = 'ER'

   #Constantes para el manejo de los motivos de rechazo
   CONSTANT ERROR_DE_SISTEMA                      SMALLINT = 1
   CONSTANT CAMPO_NO_EXISTE                       SMALLINT = 2
   CONSTANT CAMPO_INVALIDO                        SMALLINT = 3

   CONSTANT COD_OPER_CLIENTE                      CHAR(3) = 'CODIGO_SAFRE'

   CONSTANT COD_ERROR_ENVIO                       SMALLINT = 1

   CONSTANT CAMPO_ARREGLO                         CHAR(1) = 'A'

   #Constantes para indicar la estructura de un arreglo
   CONSTANT NOMBRE_ARREGLO                         CHAR(13) = "nombreArreglo"
   CONSTANT NOMBRE_ELEMENTO                        CHAR(14) = "nombreElemento"

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