###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS17                                                 #
#Objetivo          => WS PARA ACTUALIZAR DATOS BASE CURP                      #
#Fecha Inicio      => 25-OCTUBRE-2018                                         #
###############################################################################

IMPORT com
IMPORT xml


GLOBALS "AFIWS21.inc"

PRIVATE DEFINE v_actualiza          STRING

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateActualizaCurpService() RETURNING servicio

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
# Service: ActualizaCurpService
# Port:    ActualizaCurp
#-------------------------------------------------------------------------------
#
# FUNCTION CreateActualizaCurpService
#   RETURNING soapstatus
#
FUNCTION CreateActualizaCurpService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ActualizaCurpService","http://service.saci.efp.com")

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(ActualizaCurpHttpIn)
    CALL service.registerOutputHttpVariable(ActualizaCurpHttpOut)


    #
    # Operation: consultar
    #

    # Publish Operation : consultar
    LET operation = com.WebOperation.CreateDOCStyle("actualizar","actualizar",actualizaRequest,actualizaResponse)
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

FUNCTION actualizar()
   DEFINE v_id_derechohabiente         DECIMAL(9,0)

   DATABASE safre_viv
   INITIALIZE v_id_derechohabiente TO NULL

   SELECT id_derechohabiente 
   INTO v_id_derechohabiente
   FROM afi_derechohabiente
   WHERE nss = actualizaRequest.request.nss

   IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente > 0 THEN
      IF fn_valida_campos() THEN
         
         CALL fn_actualiza_datos_generales(v_id_derechohabiente)
         
         LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_EXITOSA
         LET actualizaResponse.actualizaReturn.descripcion = "SE ACTUALIZARON LOS DATOS DE LA CUENTA CORRECTAMENTE"
      END IF
   ELSE
      LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_RECHAZADA
      LET actualizaResponse.actualizaReturn.descripcion = "CUENTA NO LOCALIZADA"
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_valida_campos()
   DEFINE v_ind_act              SMALLINT
   DEFINE v_resultado            BOOLEAN
   DEFINE v_nombre_imss          VARCHAR(50)
   DEFINE v_sexo                 INTEGER

   LET v_actualiza = "SET "
   LET v_ind_act = 0
   LET v_resultado = TRUE


   IF actualizaRequest.request.curp IS NOT NULL THEN
      LET v_actualiza = v_actualiza, "curp = '", actualizaRequest.request.curp CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.apellidoPaterno IS NOT NULL THEN 
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "ap_paterno_af = '", actualizaRequest.request.apellidoPaterno CLIPPED, "' "
      LET v_ind_act = 1
      LET v_nombre_imss = actualizaRequest.request.apellidoPaterno CLIPPED
   END IF

   IF actualizaRequest.request.apellidoMaterno IS NOT NULL THEN
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "ap_materno_af = '", actualizaRequest.request.apellidoMaterno CLIPPED, "' "
      LET v_ind_act = 1
      LET v_nombre_imss = v_nombre_imss, "$", actualizaRequest.request.apellidoMaterno CLIPPED
   ELSE
      LET v_nombre_imss = v_nombre_imss, "$"
   END IF

   IF actualizaRequest.request.nombre IS NOT NULL THEN 
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "nombre_af = '", actualizaRequest.request.nombre CLIPPED, "' "
      LET v_ind_act = 1
      LET v_nombre_imss = v_nombre_imss, "$", actualizaRequest.request.nombre CLIPPED
   END IF

   #IF v_nombre_imss IS NOT NULL THEN
   #   IF v_ind_act = 1 THEN
   #      LET v_actualiza = v_actualiza, ", "
   #   END IF
   #   LET v_actualiza = v_actualiza, "nombre_imss = '", v_nombre_imss CLIPPED, "' "
   #   LET v_ind_act = 1
   #END IF
   
   IF actualizaRequest.request.fNacimiento IS NOT NULL THEN 
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "f_nacimiento = '", actualizaRequest.request.fNacimiento USING "mm/dd/yyyy", "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.genero IS NOT NULL THEN
      SELECT sexo
      INTO v_sexo 
      FROM cat_genero 
      WHERE genero = actualizaRequest.request.genero

      IF v_sexo IS NOT NULL THEN
         IF v_ind_act = 1 THEN
            LET v_actualiza = v_actualiza, ", "
         END IF
         LET v_actualiza = v_actualiza, "sexo = ", v_sexo, " "
         LET v_ind_act = 1
      ELSE
         RETURN fn_rechaza_actualizacion("EL VALOR DEL CAMPO GENERO NO ES VALIDO EN EL SISTEMA");
      END IF
   END IF

   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_rechaza_actualizacion(p_mensaje)
   DEFINE p_mensaje        STRING

   LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_RECHAZADA
   LET actualizaResponse.actualizaReturn.descripcion = p_mensaje CLIPPED
   
   RETURN FALSE
END FUNCTION

PRIVATE FUNCTION fn_actualiza_datos_generales(p_id_derechohabiente)
   DEFINE p_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_query                      STRING
   #Se actualizan los datos generales
   LET v_query = "UPDATE afi_derechohabiente ", v_actualiza, " WHERE id_derechohabiente = ?"
   DISPLAY " "
   DISPLAY "Se actualiza una cuenta, ID: ", p_id_derechohabiente
   DISPLAY "Query = ", v_query CLIPPED
   PREPARE exe_actualiza_datos FROM v_query
   EXECUTE exe_actualiza_datos USING p_id_derechohabiente

   #Se registra en bitacora la actualizacion de datos de contacto

   INSERT INTO afi_bitacora_datos(  id_afi_bitacora_datos,
                                    id_derechohabiente,
                                    f_actualiza,
                                    usuario_actualiza,
                                    id_sistema)
                           VALUES(  seq_afi_bitacora_datos.nextval,
                                    p_id_derechohabiente,
                                    TODAY,
                                    actualizaRequest.request.caso,
                                    ID_SISTEMA_CRM);
   
END FUNCTION