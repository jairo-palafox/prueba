#-------------------------------------------------------------------------------
# File: WSValidaCurpService.4gl
# GENERATED BY fglwsdl 141859
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


IMPORT com
IMPORT xml


GLOBALS "AFIWS20.inc"
GLOBALS "AFIW08.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateWSValidaCurpService() RETURNING servicio

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
# Service: ConsultaCurpSaciService
# Port:    ConsultaCurpSaci
#-------------------------------------------------------------------------------
#
# FUNCTION CreateWSValidaCurpService
#   RETURNING soapstatus
#
FUNCTION CreateWSValidaCurpService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("WSValidaCurpService","http://consulta.saci.efp.com")
    CALL service.setFeature("Soap1.1",TRUE)

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(WSValidaCurpHttpIn)
    CALL service.registerOutputHttpVariable(WSValidaCurpHttpOut)


    #
    # Operation: consultaCurp
    #

    # Publish Operation : consultaCurp
    LET operation = com.WebOperation.CreateDOCStyle("consultaCurp","consultaCurp",ns2consultaRequest,ns2consultaCurpReturn)
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

PUBLIC FUNCTION consultaCurp()
   DEFINE v_nss                  CHAR(11)
   DEFINE v_curp                 CHAR(18)
   DEFINE v_datos_saci           trabajador
   DEFINE v_fallecido_saci       INTEGER
   DEFINE v_valida_uni           DECIMAL(9,0)
   DEFINE v_curp_igual           BOOLEAN
   DEFINE v_num_dif              INTEGER
   #DEFINE v_temporal             INTEGER

   DEFINE v_respuesta_renapo     respuesta_renapo   #Variable para "cachar" la respuesta del WS

   DATABASE safre_viv

   INITIALIZE ns2consultaCurpReturn.datosTrabajador.* TO NULL
   INITIALIZE ns2consultaCurpReturn.* TO NULL
   
   LET v_nss = ns2consultaRequest.nss
   LET v_curp = ns2consultaRequest.curp
   LET v_num_dif = 0

   #Primero se valida que el NSS sea valido para la consulta
   IF v_nss IS NULL THEN
      LET ns2consultaCurpReturn.diagnostico = 'E01'
      LET ns2consultaCurpReturn.descripcion = 'NSS no valido'
   ELSE
      #Se busca el NSS
      SELECT
         afi.id_derechohabiente,
         afi.nss,
         afi.curp,
         afi.nombre_af,
         afi.ap_paterno_af,
         afi.ap_materno_af,
         afi.f_nacimiento,
         fal.estado
      INTO
         v_datos_saci.*,
         v_fallecido_saci
      FROM afi_derechohabiente afi
      LEFT JOIN cat_genero cat ON cat.sexo = afi.sexo
      LEFT JOIN afi_fallecido fal ON (fal.id_derechohabiente = afi.id_derechohabiente AND fal.estado = 10)
      WHERE afi.nss = v_nss

      IF v_datos_saci.id_derechohabiente IS NULL OR v_datos_saci.id_derechohabiente <= 0 THEN
         #No se encontro el registro en SACI
         LET ns2consultaCurpReturn.diagnostico = '001'
         LET ns2consultaCurpReturn.descripcion = 'NSS no localizado en SACI'
      ELSE
         #Se valida la marca de fallecido
         IF v_fallecido_saci IS NOT NULL AND v_fallecido_saci > 0 THEN
            LET ns2consultaCurpReturn.diagnostico = '003'
            LET ns2consultaCurpReturn.descripcion = 'Cuenta con marca de fallecido en SACI'  
         ELSE
            #Se valida si la cuenta se encuentra en unificacion
            SELECT id_derechohabiente
            INTO v_valida_uni
            FROM sfr_marca_activa 
            WHERE id_derechohabiente = v_datos_saci.id_derechohabiente 
            AND marca IN (501,502,503,504,511,512,812)

            IF v_valida_uni IS NOT NULL AND v_valida_uni > 0 THEN
               LET ns2consultaCurpReturn.diagnostico = '004'
               LET ns2consultaCurpReturn.descripcion = 'Cuenta en proceso de Unificación'
            ELSE
               #Se valida si la CURP capturada es igual a la registrada en SACI
               IF v_datos_saci.curp IS NOT NULL AND v_datos_saci.curp = v_curp THEN
                  LET v_curp_igual = TRUE
               ELSE
                  LET v_curp_igual = FALSE
               END IF #FIN valida que curp sea igual en SACI

               #Se ejecuta la consulta en RENAPO
               CALL fn_consulta_curp_renapo(v_curp) RETURNING v_respuesta_renapo.*
               IF v_respuesta_renapo.status_operacion <> "EXITOSO" THEN
                  LET ns2consultaCurpReturn.diagnostico = '002'
                  LET ns2consultaCurpReturn.descripcion = 'CURP no localizada en RENAPO'
               ELSE
                  #Se valida el status de la CURP segun RENAPO
                  IF v_respuesta_renapo.status_curp = 'BD' THEN
                     #CURP con marca de fallecido
                     IF v_curp_igual THEN
                        LET ns2consultaCurpReturn.diagnostico = '007'
                        LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP igual en SACI y marca de fallecido en RENAPO'
                     ELSE
                        LET ns2consultaCurpReturn.diagnostico = '011'
                        LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP diferente en SACI y marca de fallecido en RENAPO'
                     END IF #FIN valida CURP igual en SACI
                  ELSE
                     IF v_respuesta_renapo.status_curp = 'BDA' OR v_respuesta_renapo.status_curp = 'BCC'
                     OR v_respuesta_renapo.status_curp = 'BCN' THEN
                     #CURP historica en RENAPO
                        IF v_curp_igual THEN
                           LET ns2consultaCurpReturn.diagnostico = '008'
                           LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP igual en SACI y marca de CURP hist�rica en RENAPO'
                        ELSE
                           LET ns2consultaCurpReturn.diagnostico = '012'
                           LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP diferente en SACI y marca de CURP hist�rica en RENAPO'
                        END IF #FIN valida CURP igual en SACI
                     ELSE
                        #Se eliminan los espacios en blanco de la derecha para los campos del nombre
                        LET v_datos_saci.nombre = v_datos_saci.nombre CLIPPED
                        LET v_datos_saci.primerApellido = v_datos_saci.primerApellido CLIPPED
                        LET v_datos_saci.segundoApellido = v_datos_saci.segundoApellido CLIPPED

                        LET v_respuesta_renapo.nombre = v_respuesta_renapo.nombre CLIPPED
                        LET v_respuesta_renapo.apellido_paterno = v_respuesta_renapo.apellido_paterno CLIPPED
                        LET v_respuesta_renapo.apellido_materno = v_respuesta_renapo.apellido_materno CLIPPED

                        IF v_respuesta_renapo.nombre <> v_datos_saci.nombre THEN
                           CALL fn_compara_cadena(v_datos_saci.nombre, v_respuesta_renapo.nombre) RETURNING v_num_dif
                        END IF

                        IF v_respuesta_renapo.apellido_paterno <> v_datos_saci.primerApellido THEN
                           LET v_num_dif = v_num_dif + fn_compara_cadena(v_datos_saci.primerApellido, v_respuesta_renapo.apellido_paterno)
                        END IF

                        IF v_respuesta_renapo.apellido_materno <> v_datos_saci.segundoApellido THEN
                           LET v_num_dif = v_num_dif + fn_compara_cadena(v_datos_saci.segundoApellido, v_respuesta_renapo.apellido_materno)
                        END IF

                        #DISPLAY " "
                        #DISPLAY "Pruebas de comparacion..."
                        #DISPLAY "*************************Prueba 1************************"
                        #CALL fn_compara_cadena("NOMBRE","NOMBRES") RETURNING v_temporal
                        #DISPLAY "*********************************************************"
                        #DISPLAY " "
                        #DISPLAY "*************************Prueba 2************************"
                        #CALL fn_compara_cadena("NOMBRES","NOMBRE") RETURNING v_temporal
                        #DISPLAY "*********************************************************"
                        #DISPLAY " "
                        #DISPLAY "*************************Prueba 3************************"
                        #CALL fn_compara_cadena("NoMBRE","NOMBRE") RETURNING v_temporal
                        #DISPLAY "*********************************************************"
                        #DISPLAY " "
                        #DISPLAY "*************************Prueba 4************************"
                        #CALL fn_compara_cadena("NMBRE","NOMBRE") RETURNING v_temporal
                        #DISPLAY "*********************************************************"
                        IF v_num_dif <= 7 THEN
                           LET ns2consultaCurpReturn.nss = v_nss
                           LET ns2consultaCurpReturn.curp = v_curp
                           LET ns2consultaCurpReturn.datosTrabajador.nombre = v_respuesta_renapo.nombre
                           LET ns2consultaCurpReturn.datosTrabajador.primerApellido = v_respuesta_renapo.apellido_paterno
                           LET ns2consultaCurpReturn.datosTrabajador.segundoApellido = v_respuesta_renapo.apellido_materno
                           LET ns2consultaCurpReturn.datosTrabajador.fechaNacimiento = v_respuesta_renapo.fecha_nacimiento
                           LET ns2consultaCurpReturn.datosTrabajador.codSexo = v_respuesta_renapo.sexo
                           IF v_curp_igual THEN
                              LET ns2consultaCurpReturn.diagnostico = '005'
                              LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP igual en SACI y diferencia menor a 7 caracteres en nombre'
                           ELSE
                              LET ns2consultaCurpReturn.diagnostico = '009'
                              LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP diferente en SACI y diferencia menor a 7 caracteres en nombre'
                           END IF #FIN valida CURP igual en SACI
                        ELSE
                           IF v_curp_igual THEN
                              LET ns2consultaCurpReturn.diagnostico = '006'
                              LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP igual en SACI y diferencia mayor a 7 caracteres en nombre'
                           ELSE
                              LET ns2consultaCurpReturn.diagnostico = '010'
                              LET ns2consultaCurpReturn.descripcion = 'Consulta con CURP diferente en SACI y diferencia mayor a 7 caracteres en nombre'
                           END IF #FIN valida CURP igual en SACI
                        END IF #FIN valida diferencia de 7 caracteres
                     END IF #Fin valida CURP historica en RENAPO
                  END IF #FIN valida marca de fallecido en RENAPO
               END IF #Fin valida status de consulta RENAPO
            END IF #Fin valida Unificacion
         END IF #Fin valida fallecido
      END IF #FIN valida NSS en SACI
   END IF #FIN NSS nulo
END FUNCTION

PRIVATE FUNCTION fn_compara_cadena(p_cadena1, p_cadena2)
   DEFINE p_cadena1        STRING
   DEFINE p_cadena2        STRING
   DEFINE v_num_dif        INTEGER
   DEFINE i                INTEGER
   DEFINE v_char_2         CHAR(1)

   LET v_num_dif = 0

   #DISPLAY " "
   #DISPLAY "Comparando ", p_cadena1, " con ", p_cadena2, " ..."
   FOR i = 1 TO p_cadena1.getLength()
      LET v_char_2 = p_cadena2.getCharAt(i)
      IF v_char_2 IS NULL THEN
         LET v_char_2 = ' '
      END IF
      IF p_cadena1.getCharAt(i) <> v_char_2 THEN
         LET v_num_dif = v_num_dif + 1
         #DISPLAY p_cadena1.getCharAt(i)," <> ", v_char_2
         #DISPLAY "v_num_dif = ", v_num_dif
      END IF
   END FOR

   IF p_cadena2.getLength() > p_cadena1.getLength() THEN
      #DISPLAY " "
      #DISPLAY "Cadena2 con mas caracteres que Cadena1..."
      LET v_num_dif = v_num_dif + (p_cadena2.getLength() - p_cadena1.getLength())
      #DISPLAY "v_num_dif = ", v_num_dif
   END IF
   #DISPLAY "Salida v_num_dif = ", v_num_dif
   RETURN v_num_dif
END FUNCTION 