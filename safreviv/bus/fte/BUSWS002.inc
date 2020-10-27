####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSWS02.inc                                   #
#Objetivo          =>Archivo de variables globales                 #
#Fecha inicio      =>24 SEPTIEMBRE 2013                            #
####################################################################

GLOBALS

   CONSTANT ORIGEN_PROCESAR   SMALLINT = 0
   
   #Constantes para el manejo de los codigos de respuesta
   CONSTANT COD_OK            CHAR(2) = 'OK'
   CONSTANT COD_ERROR         CHAR(2) = 'RE'
   CONSTANT COD_OPR_OK        CHAR(2) = 'AC'
   CONSTANT COD_OPR_ERROR     CHAR(2) = 'RE'

   #Constantes para el manejo de los motivos de rechazo
   CONSTANT ERROR_DE_SISTEMA                      SMALLINT = 1
   CONSTANT CAMPO_NO_EXISTE                       SMALLINT = 2
   CONSTANT CAMPO_INVALIDO                        SMALLINT = 3
   CONSTANT CAMPO_NO_CATALOGADO                   SMALLINT = 4
   
   CONSTANT ERROR_REGISTRO_TRAMITE                CHAR(7) = 'BUS1001'
   CONSTANT ERROR_CONFIG_OPERACION                CHAR(7) = 'BUS2004'
   CONSTANT ERROR_XML_MAL_FORMADO                 CHAR(7) = 'BUS2006'

   CONSTANT DESC_ERROR_VALIDACION                 VARCHAR(100) = 'Ocurrió un error de validación al descomponer el campo de notificacionXML'
   CONSTANT DESC_TRAMITE_VALIDO                   VARCHAR(100) = 'El trámite se registró correctamente en el sistema'
   
   CONSTANT RECHAZO_GENERICO                      VARCHAR(10) = 'EB9999'
   CONSTANT DES_RECHAZO_GENERICO                  VARCHAR(100) = 'ERROR NO CATALOGADO, existe un valor no valido en el campo '
   CONSTANT DES_RECHAZO_GENERICO_NULL             VARCHAR(100) = 'ERROR NO CATALOGADO, existe un valor nulo en el campo '
   CONSTANT DES_RECHAZO_GENERICO_DUP              VARCHAR(100) = 'ERROR NO CATALOGADO, existe duplicidad en el campo '
   CONSTANT DES_RECHAZO_GENERICO_NO_CATALOGADO    VARCHAR(100) = 'ERROR NO CATALOGADO, el siguiente campo no pertenece al contrato: '
   CONSTANT DES_RECHAZO_NUMERO_CAMPOS             VARCHAR(100) = 'El número de campos dentro de notificacionXML no corresponde con el contrato de la operación'

   CONSTANT RECHAZO_GENERICO_ARREGLO              VARCHAR(10) = 'EB9998'
   CONSTANT DES_ARREGLO_SIN_DEFINICION            VARCHAR(100) = 'ERROR al obtener la definicion del arreglo'
   CONSTANT DES_XML_MAL_FORMADO_ARREGLO           VARCHAR(100) = 'La estructura del XML no se encuentra correctamente formada en el arreglo'
   CONSTANT DES_RECHAZO_GENERICO_ARREGLO          VARCHAR(100) = 'ERROR NO CATALOGADO, existe un valor no valido dentro de un arreglo en el campo '
   CONSTANT DES_RECHAZO_GENERICO_ARREGLO_NULL     VARCHAR(100) = 'ERROR NO CATALOGADO, existe un valor nulo dentro de un arreglo en el campo '
   CONSTANT DES_RECHAZO_NUMERO_CAMPOS_ARREGLO     VARCHAR(100) = 'El número de campos dentro de un arreglo no corresponde con el contrato de la operación'

   CONSTANT CAMPO_NSS                             CHAR(3) = 'nss'
   CONSTANT CAMPO_NSS_IMSS                        CHAR(7) = 'nssImss'
   CONSTANT CAMPO_CURP                            CHAR(4) = 'curp'
   CONSTANT CAMPO_FECHA                           CHAR(1) = 'F'
   CONSTANT CAMPO_FECHA_2                         CHAR(1) = 'f'
   CONSTANT CAMPO_NUMERICO                        CHAR(1) = 'N'
   CONSTANT CAMPO_DECIMAL                         CHAR(1) = 'D'
   CONSTANT CAMPO_ALFABETICO                      CHAR(1) = 'C'
   CONSTANT CAMPO_ALFANUMERICO                    CHAR(1) = 'X'
   CONSTANT CAMPO_ARREGLO                         CHAR(1) = 'A'

   #Constantes para leer el campo notificacionXML
   CONSTANT ELEMENTO                               CHAR(5) = 'entry'
   CONSTANT NOMBRE_CAMPO                           CHAR(3) = 'key'
   CONSTANT VALOR_CAMPO                            CHAR(5) = 'value'

   #Constantes para indicar la estructura de un arreglo
   CONSTANT NOMBRE_ARREGLO                         CHAR(13) = "nombreArreglo"
   CONSTANT NOMBRE_ELEMENTO                        CHAR(14) = "nombreElemento"

   TYPE detalle_request RECORD
      num_campo            INTEGER,
      nombre_campo         VARCHAR(60),
      valor                VARCHAR(100),
      valor_xml            STRING,
      ind_existe           SMALLINT
   END RECORD

   TYPE rechazos RECORD
      cod_rechazo          VARCHAR(6),
      desc_rechazo         VARCHAR(100)
   END RECORD

   TYPE contrato_bloque RECORD
      id_cat_bus_detalle_bloque  DECIMAL(9,0), 
      cve_natural_bloque         VARCHAR(40), 
      tipo_dato                  CHAR(1),
      ind_opcional               INTEGER
   END RECORD
   
END GLOBALS