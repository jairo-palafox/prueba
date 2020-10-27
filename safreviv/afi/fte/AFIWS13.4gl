###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS13                                                 #
#Objetivo          => WS PARA CONSULTA DATOS DE CONTACTO                      #
#Fecha Inicio      => 05-SEPTIEMBRE-2017                                      #
###############################################################################

IMPORT com
IMPORT xml


GLOBALS "AFIWS13.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateConsultaDatosSACIService() RETURNING servicio

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
# Service: ConsultaDatosSACIService
# Port:    ConstulaDatosSACI
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConsultaDatosSACIService
#   RETURNING soapstatus
#
FUNCTION CreateConsultaDatosSACIService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaDatosSACIService","http://service.saci.efp.com")

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(ConstulaDatosSACIHttpIn)
    CALL service.registerOutputHttpVariable(ConstulaDatosSACIHttpOut)


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
   DEFINE v_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_id_derechohabiente_tmp     DECIMAL(9,0)
   DEFINE v_trabajador                 trabajador_saci
   DEFINE v_fNacimiento                DATE
   DEFINE v_genero                     DATE
   DEFINE v_indFallecido               INTEGER
   DEFINE v_query                      STRING
   DEFINE v_consulta_valida            BOOLEAN
   DEFINE v_contador                   SMALLINT
   DATABASE safre_viv

   LET v_consulta_valida = FALSE
   LET v_query = "SELECT FIRST 50 ",
                     "afi.id_derechohabiente, ",
                     "afi.nss, ",
                     "afi.rfc, ",
                     "afi.curp, ",
                     "TRIM(afi.ap_paterno_af), ",
                     "TRIM(afi.ap_materno_af), ",
                     "TRIM(afi.nombre_af), ",
                     "afo.afore_cod || ' - ' || TRIM(cat.afore_desc), ",
                     "afi.f_nacimiento, ",
                     "gen.genero, ",
                     "fal.estado ",
                  "FROM afi_derechohabiente afi ",
                  "LEFT JOIN afi_afore afo ON afo.id_derechohabiente = afi.id_derechohabiente ",
                  "LEFT JOIN cat_afore cat ON cat.afore_cod = afo.afore_cod ",
                  "LEFT JOIN cat_genero gen ON gen.sexo = afi.sexo ",
                  "LEFT JOIN afi_fallecido fal ON (fal.id_derechohabiente = afi.id_derechohabiente AND fal.estado = 10) ",
                  "WHERE "

   IF consultaRequest.request.nss IS NOT NULL AND consultaRequest.request.nss.getLength() > 0 THEN
      LET v_query = v_query, "afi.nss = '", consultaRequest.request.nss, "' "
      LET v_consulta_valida = TRUE
   END IF

   IF consultaRequest.request.rfc IS NOT NULL AND consultaRequest.request.rfc.getLength() > 0 THEN
      IF v_consulta_valida THEN
         LET v_query = v_query, "AND "
      END IF
      LET v_query = v_query, "afi.rfc = '", consultaRequest.request.rfc, "' "
      LET v_consulta_valida = TRUE
   END IF

   IF consultaRequest.request.curp IS NOT NULL AND consultaRequest.request.curp.getLength() > 0 THEN
      IF v_consulta_valida THEN
         LET v_query = v_query, "AND "
      END IF
      LET v_query = v_query, "afi.curp = '", consultaRequest.request.curp, "' "
      LET v_consulta_valida = TRUE
   END IF

   PREPARE exe_consulta_afiliado FROM v_query
   DECLARE cur_consulta_afiliado CURSOR FOR exe_consulta_afiliado
   
   INITIALIZE consultaResponse.consultarReturn.* TO NULL
   LET v_contador = 0
   FOREACH cur_consulta_afiliado INTO v_id_derechohabiente_tmp, v_trabajador.*
      IF v_id_derechohabiente_tmp IS NOT NULL AND v_id_derechohabiente_tmp > 0 THEN
         LET v_contador = v_contador + 1
         IF v_trabajador.rfc IS NULL THEN
            LET v_trabajador.rfc = "             "
         END IF

         IF v_trabajador.curp IS NULL THEN
            LET v_trabajador.curp = " "
         END IF

         IF v_trabajador.apellidoMaterno IS NULL THEN
            LET v_trabajador.apellidoMaterno = " "
         END IF
         
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].nss = v_trabajador.nss
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].rfc = v_trabajador.rfc
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].curp = v_trabajador.curp
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].apellidoPaterno = v_trabajador.apellidoPaterno CLIPPED
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].apellidoMaterno = v_trabajador.apellidoMaterno CLIPPED
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].nombre = v_trabajador.nombre CLIPPED
         LET consultaResponse.consultarReturn.listaTrabajadores.trabajador[v_contador].afore = v_trabajador.afore CLIPPED
         LET v_id_derechohabiente = v_id_derechohabiente_tmp
         LET v_fNacimiento = v_trabajador.fNacimiento
         LET v_genero = v_trabajador.genero
         LET v_indFallecido = v_trabajador.indFallecido
      END IF
      
   END FOREACH

   IF v_contador > 0 THEN
      IF v_contador = 1 THEN
         #Solo se encontro un afiliado
         LET consultaResponse.consultarReturn.nss = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].nss
         LET consultaResponse.consultarReturn.rfc = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].rfc
         LET consultaResponse.consultarReturn.curp = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].curp
         LET consultaResponse.consultarReturn.apellidoPaterno = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].apellidoPaterno CLIPPED
         LET consultaResponse.consultarReturn.apellidoMaterno = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].apellidoMaterno CLIPPED
         LET consultaResponse.consultarReturn.nombre = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].nombre CLIPPED
         LET consultaResponse.consultarReturn.afore = consultaResponse.consultarReturn.listaTrabajadores.trabajador[1].afore CLIPPED
         LET consultaResponse.consultarReturn.fNacimiento = v_fNacimiento 
         LET consultaResponse.consultarReturn.genero = v_genero CLIPPED
         IF v_indFallecido IS NULL OR v_indFallecido <= 0 THEN
            LET consultaResponse.consultarReturn.indFallecido = 0
         ELSE
            LET consultaResponse.consultarReturn.indFallecido = 1
         END IF
         
         INITIALIZE consultaResponse.consultarReturn.listaTrabajadores TO NULL

         CALL fn_busca_datos_contacto(v_id_derechohabiente)

         #Se valida si el NSS esta catalogado como 'consulta roja'
         INITIALIZE v_id_derechohabiente TO NULL
         SELECT FIRST 1 id_derechohabiente
         INTO v_id_derechohabiente
         FROM afi_nss_rojo
         WHERE nss = consultaResponse.consultarReturn.nss
         AND estado_rojo = 1

         
         IF v_id_derechohabiente IS NOT NULL THEN
            LET consultaResponse.consultarReturn.codRespuesta = EXITOSA_ROJA
         ELSE
            LET consultaResponse.consultarReturn.codRespuesta = CONSULTA_EXITOSA
         END IF
      ELSE
        #Se encontro mas de un afiliado
        LET consultaResponse.consultarReturn.codRespuesta = VARIOS_REGISTROS
      END IF
   ELSE
      #No existe registros con el criterio de búsqueda
      INITIALIZE consultaResponse.consultarReturn.* TO NULL
      LET consultaResponse.consultarReturn.codRespuesta = SIN_REGISTROS
   END IF
   
   CLOSE DATABASE
   
END FUNCTION

PRIVATE FUNCTION fn_busca_datos_contacto(p_id_derechohabiente)
   DEFINE p_id_derechohabiente            DECIMAL(9,0)
   DEFINE v_id_domicilio                  DECIMAL(9,0)
   DEFINE v_id_telefono                   DECIMAL(9,0)
   DEFINE v_id_correo                     DECIMAL(9,0)
   DEFINE v_domicilio                     domicilioTrabajador
   DEFINE v_telefono                      telefonoTrabajador
   DEFINE v_correo                        correoTrabajador
   DEFINE v_query                         STRING
   DEFINE v_contador                      SMALLINT

   LET v_query =  "SELECT ",
                     "dom.id_domicilio, ",
                     "dom.calle, ",
                     "TRIM(dom.num_exterior), ",
                     "TRIM(dom.num_interior), ",
                     "dom.colonia, ",
                     "codigo.municipio, ",
                     "dom.cp, ",
                     "codigo.entidad_federativa, ",
                     "'MEX', ",
                     "dom.tpo_domicilio ", 
                  "FROM afi_domicilio dom ",
                  "LEFT JOIN cat_cp codigo ON codigo.cp = dom.cp ",
                  "WHERE dom.id_derechohabiente = ?"
   PREPARE exe_consulta_domicilio FROM v_query
   DECLARE cur_consulta_domicilio CURSOR FOR exe_consulta_domicilio
   
   INITIALIZE consultaResponse.consultarReturn.listaDomicilios.* TO NULL
   INITIALIZE v_id_domicilio TO NULL
   LET v_contador = 0
   FOREACH cur_consulta_domicilio USING p_id_derechohabiente INTO v_id_domicilio, v_domicilio.*
      IF v_id_domicilio IS NOT NULL AND v_id_domicilio > 0 THEN
         LET v_contador = v_contador + 1
         LET consultaResponse.consultarReturn.listaDomicilios.domicilio[v_contador].* = v_domicilio.*
         LET consultaResponse.consultarReturn.listaDomicilios.domicilio[v_contador].delegacionOMunicipio = v_domicilio.delegacionOMunicipio CLIPPED
         LET consultaResponse.consultarReturn.listaDomicilios.domicilio[v_contador].entidadFederativa = v_domicilio.entidadFederativa CLIPPED 
      END IF
   END FOREACH

   LET v_query =  "SELECT ",
                     "tel.id_telefono, ",
                     "tel.tpo_telefono, ",
                     "tel.cve_lada, ",
                     "tel.telefono, ",
                     "tel.extension ",
                  "FROM afi_telefono tel ",
                  "WHERE tel.id_derechohabiente = ?"
   PREPARE exe_consulta_telefono FROM v_query
   DECLARE cur_consulta_telefono CURSOR FOR exe_consulta_telefono
   
   INITIALIZE consultaResponse.consultarReturn.listaTelefonos.* TO NULL
   INITIALIZE v_id_telefono TO NULL
   LET v_contador = 0
   FOREACH cur_consulta_telefono USING p_id_derechohabiente INTO v_id_telefono, v_telefono.*
      IF v_id_telefono IS NOT NULL AND v_id_telefono > 0 THEN
         LET v_contador = v_contador + 1
         LET consultaResponse.consultarReturn.listaTelefonos.telefono[v_contador].* = v_telefono.*
      END IF
   END FOREACH

   LET v_query =  "SELECT ",
                     "id_contacto_electronico, ",
                     "tpo_correo, ",
                     "valor ",
                  "FROM afi_contacto_electronico ",
                  "WHERE id_derechohabiente = ?"
   PREPARE exe_consulta_correo FROM v_query
   DECLARE cur_consulta_correo CURSOR FOR exe_consulta_correo
   
   INITIALIZE consultaResponse.consultarReturn.listaCorreos.* TO NULL
   INITIALIZE v_id_correo TO NULL
   LET v_contador = 0
   FOREACH cur_consulta_correo USING p_id_derechohabiente INTO v_id_correo, v_correo.*
      IF v_id_correo IS NOT NULL AND v_id_correo > 0 THEN
         LET v_contador = v_contador + 1
         LET consultaResponse.consultarReturn.listaCorreos.correo[v_contador].* = v_correo.*
      END IF
   END FOREACH
   
   
END FUNCTION