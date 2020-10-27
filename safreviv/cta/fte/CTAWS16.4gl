###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAWS16                                                 #
#Objetivo          => WS PARA CONSULTA DE SALDOS SACI                         #
#Fecha Inicio      => 05-SEPTIEMBRE-2017                                      #
###############################################################################

IMPORT com
IMPORT xml


GLOBALS "CTAWS16.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateConstulaSaldoSACIService() RETURNING servicio

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
# Service: ConstulaSaldoSACIService
# Port:    ConstulaSaldoSACI
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConstulaSaldoSACIService
#   RETURNING soapstatus
#
FUNCTION CreateConstulaSaldoSACIService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConstulaSaldoSACIService","http://service.saci.efp.com")

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(ConstulaSaldoSACIHttpIn)
    CALL service.registerOutputHttpVariable(ConstulaSaldoSACIHttpOut)


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
   DEFINE v_trabajador                 trabajador
   DEFINE v_query                      STRING
   DEFINE v_consulta_valida            BOOLEAN
   DEFINE v_contador                   SMALLINT
   DEFINE v_tabla		                  VARCHAR(25)
   DEFINE v_subcuenta                  INTEGER
   DEFINE v_monto_pesos                DECIMAL(16,2)

   DEFINE v_id_correo                  DECIMAL(9,0)
   DEFINE v_correo                     VARCHAR(40)

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
                     "TRIM(cat.afore_desc) ",
                  "FROM afi_derechohabiente afi ",
                  "LEFT JOIN afi_afore afo ON afo.id_derechohabiente = afi.id_derechohabiente ",
                  "LEFT JOIN cat_afore cat ON cat.afore_cod = afo.afore_cod ",
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
         LET consultaResponse.consultarReturn.listaTrabajaroes.trabajador[v_contador].* = v_trabajador.*
         LET v_id_derechohabiente = v_id_derechohabiente_tmp
      END IF
      
   END FOREACH

   IF v_contador > 0 THEN
      IF v_contador = 1 THEN
         IF (fn_valida_cuenta(v_id_derechohabiente)) THEN
            #Solo se encontro un afiliado
            LET consultaResponse.consultarReturn.nss = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].nss
            LET consultaResponse.consultarReturn.rfc = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].rfc
            LET consultaResponse.consultarReturn.curp = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].curp
            LET consultaResponse.consultarReturn.apellidoPaterno = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].apellidoPaterno CLIPPED
            LET consultaResponse.consultarReturn.apellidoMaterno = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].apellidoMaterno CLIPPED
            LET consultaResponse.consultarReturn.nombre = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].nombre CLIPPED
            LET consultaResponse.consultarReturn.afore = consultaResponse.consultarReturn.listaTrabajaroes.trabajador[1].afore CLIPPED
            LET consultaResponse.consultarReturn.saldoViv92 = 0
            LET consultaResponse.consultarReturn.saldoViv97 = 0
            
            INITIALIZE consultaResponse.consultarReturn.listaTrabajaroes TO NULL

            #Se busca la tabla que contiene el saldo activo
            --Se busca cual es la tabla disponible
            SELECT tabla_saldo 
            INTO v_tabla 
            FROM safre_sdo@vivws_tcp:glo_saldo 
            WHERE ind_saldo = 1

            LET v_query = "SELECT ",
                           "cat.id_subcuenta, ",
                           "sum(sdo.monto_pesos) monto_pesos ",
                           "FROM safre_sdo@vivws_tcp:", v_tabla, " sdo ",
                           "INNER JOIN cat_subcuenta_preca cat ON cat.subcuenta = sdo.subcuenta ",
                           "WHERE sdo.id_derechohabiente = ? ",
                           "GROUP BY cat.id_subcuenta "
            PREPARE exe_consulta_saldo FROM v_query
            DECLARE cur_consulta_saldo CURSOR FOR exe_consulta_saldo
            
            #DISPLAY " "
            #DISPLAY v_query
            #DISPLAY " "
            
            FOREACH cur_consulta_saldo USING v_id_derechohabiente
                                       INTO  v_subcuenta,
                                       v_monto_pesos
               IF v_subcuenta = VIVIENDA_97 THEN
                  LET consultaResponse.consultarReturn.saldoViv97 = v_monto_pesos
               END IF

               IF v_subcuenta = VIVIENDA_92 THEN
                  LET consultaResponse.consultarReturn.saldoViv92 = v_monto_pesos
               END IF
            END FOREACH

            #Se busca el correo electronico del trabajador
            LET v_query =  "SELECT FIRST 1 ",
                              "id_contacto_electronico, ",
                              "valor ",
                           "FROM afi_contacto_electronico ",
                           "WHERE id_derechohabiente = ? ",
                           "AND tpo_correo = 1 "
            PREPARE exe_consulta_correo FROM v_query
            EXECUTE exe_consulta_correo USING   v_id_derechohabiente
                                          INTO  v_id_correo,
                                                v_correo
                                       

            IF v_id_correo IS NOT NULL AND v_id_correo > 0 THEN
               LET consultaResponse.consultarReturn.correo = v_correo CLIPPED
            ELSE
               INITIALIZE consultaResponse.consultarReturn.correo TO NULL
            END IF 

            INITIALIZE v_id_derechohabiente TO NULL

            #Se valida si el NSS esta catalogado como 'consulta roja'
            SELECT FIRST 1 id_derechohabiente
            INTO v_id_derechohabiente
            FROM afi_nss_rojo
            WHERE nss = consultaResponse.consultarReturn.nss
            AND estado_rojo = 1

            DISPLAY "nss: ", consultaResponse.consultarReturn.nss
            DISPLAY "id_derechohabiente: ", v_id_derechohabiente
            
            IF v_id_derechohabiente IS NOT NULL THEN
               LET consultaResponse.consultarReturn.codRespuesta = EXITOSA_ROJA
            ELSE
               LET consultaResponse.consultarReturn.codRespuesta = CONSULTA_EXITOSA
            END IF
         ELSE
            #Cuenta inactiva
            #No existe registros con el criterio de búsqueda
            INITIALIZE consultaResponse.consultarReturn.* TO NULL
            LET consultaResponse.consultarReturn.codRespuesta = CUENTA_INACTIVA
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

PRIVATE FUNCTION fn_valida_cuenta(p_id_derechohabiente)
   DEFINE p_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_tmp_valida              DECIMAL(9,0)
   DEFINE v_respuesta               BOOLEAN

   INITIALIZE v_tmp_valida TO NULL
   
   SELECT id_derechohabiente
   INTO v_tmp_valida 
   FROM sfr_marca_activa
   WHERE marca  IN (150,151,160)
   AND id_derechohabiente = p_id_derechohabiente

   IF v_tmp_valida IS NULL THEN
      LET v_respuesta = TRUE
   ELSE
      LET v_respuesta = FALSE
   END IF
   RETURN v_respuesta
END FUNCTION