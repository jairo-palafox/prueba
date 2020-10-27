###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS15                                                 #
#Objetivo          => WS PARA CONSULTA DATOS DE UN CODIGO POSTAL              #
#Fecha Inicio      => 08-DICIEMBRE-2017                                       #
###############################################################################

IMPORT com
IMPORT xml


GLOBALS "AFIWS15.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateConsultaCPService() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      #
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      #
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM   # The Application server has closed the connection
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF
   END WHILE
END MAIN


#-------------------------------------------------------------------------------
# Service: ConsultaCPService
# Port:    ConstulaCP
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConsultaCPService
#   RETURNING soapstatus
#
FUNCTION CreateConsultaCPService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaCPService","http://service.saci.efp.com")

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(ConstulaCPHttpIn)
    CALL service.registerOutputHttpVariable(ConstulaCPHttpOut)


    #
    # Operation: consultar
    #

    # Publish Operation : consultar
    LET operation = com.WebOperation.CreateDOCStyle("consultar","consultar",consultaRequest,consultaResponse)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION consultar()
   DEFINE v_query       STRING
   DEFINE v_colonia     INTEGER
   DEFINE v_desc        VARCHAR(50)
   DEFINE v_item        INTEGER

   DATABASE safre_viv

   INITIALIZE consultaResponse.consultarReturn.* TO NULL
   
   SELECT 
      cp.cp,
      cp.entidad_federativa,
      TRIM(fed.entidad_desc_larga),
      cp.ciudad,
      TRIM(cd.ciudad_desc),
      cp.municipio,
      TRIM(mun.municipio_desc)
   INTO
      consultaResponse.consultarReturn.cp,
      consultaResponse.consultarReturn.entidadFederativa.cveEntFederativa,
      consultaResponse.consultarReturn.entidadFederativa.descEntFederativa,
      consultaResponse.consultarReturn.ciudad.cveCiudad,
      consultaResponse.consultarReturn.ciudad.descCiudad,
      consultaResponse.consultarReturn.municipio.cveMunicipio,
      consultaResponse.consultarReturn.municipio.descMunicipio
   FROM cat_cp cp
   INNER JOIN cat_entidad_federativa fed ON fed.entidad_federativa = cp.entidad_federativa
   LEFT JOIN cat_ciudad cd ON cd.ciudad = cp.ciudad
   INNER JOIN cat_municipio mun ON mun.municipio = cp.municipio
   WHERE cp.cp = consultaRequest.request.cp

   IF consultaResponse.consultarReturn.cp IS NULL THEN
      INITIALIZE consultaResponse.consultarReturn.* TO NULL
      LET consultaResponse.consultarReturn.codRespuesta = SIN_REGISTROS
   ELSE
      LET consultaResponse.consultarReturn.codRespuesta = CONSULTA_EXITOSA
      LET v_query =  "SELECT ", 
                        "colonia, ",
                        "colonia_desc ",
                     "FROM cat_colonia ",
                     "WHERE cp = ? "
      PREPARE exe_consulta_colonia FROM v_query
      DECLARE cur_consulta_colonia CURSOR FOR exe_consulta_colonia

      INITIALIZE consultaResponse.consultarReturn.listaColonias TO NULL
      LET v_item = 1
      FOREACH cur_consulta_colonia USING consultaRequest.request.cp
                                    INTO v_colonia,
                                         v_desc
         IF v_colonia IS NOT NULL AND v_colonia > 0 THEN
            LET consultaResponse.consultarReturn.listaColonias.colonia[v_item].cveColonia = v_colonia
            LET consultaResponse.consultarReturn.listaColonias.colonia[v_item].descColonia = v_desc CLIPPED
            LET v_item = v_item + 1
         END IF
      END FOREACH
   END IF
END FUNCTION