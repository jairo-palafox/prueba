###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS14                                                 #
#Objetivo          => WS PARA ACTUALIZAR DATOS DE CONTACTO                    #
#Fecha Inicio      => 05-SEPTIEMBRE-2017                                      #
###############################################################################

IMPORT com
IMPORT xml


GLOBALS "AFIWS14.inc"

PRIVATE DEFINE v_actualiza          STRING

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateActualizaDatosSACIService() RETURNING servicio

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
# Service: ActualizaDatosSACIService
# Port:    ActualizaDatosSACI
#-------------------------------------------------------------------------------
#
# FUNCTION CreateActualizaDatosSACIService
#   RETURNING soapstatus
#
FUNCTION CreateActualizaDatosSACIService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ActualizaDatosSACIService","http://service.saci.efp.com")

    # Handle HTTP register methods
    CALL service.registerInputHttpVariable(ActualizaDatosSACIHttpIn)
    CALL service.registerOutputHttpVariable(ActualizaDatosSACIHttpOut)


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
      IF fn_valida_usuario() THEN
         IF fn_valida_campos() THEN
            
            CALL fn_actualiza_datos_generales(v_id_derechohabiente)
            
            LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_EXITOSA
            LET actualizaResponse.actualizaReturn.descripcion = "SE ACTUALIZARON LOS DATOS DE CONTACTO CORRECTAMENTE"
         END IF 
      END IF
   ELSE
      LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_RECHAZADA
      LET actualizaResponse.actualizaReturn.descripcion = "CUENTA NO LOCALIZADA"
   END IF
   
END FUNCTION

PRIVATE FUNCTION fn_valida_usuario()
   DEFINE v_resultado            BOOLEAN
   DEFINE v_usr_tmp              VARCHAR(20)
   DEFINE v_id_derechohabiente   DECIMAL(9,0)

   INITIALIZE v_usr_tmp TO NULL
   
   SELECT 
      usuario_cod
   INTO 
      v_usr_tmp
   FROM seg_usuario
   WHERE usuario_cod = actualizaRequest.request.usuarioCod
   AND ind_activo = 1

   IF v_usr_tmp IS NULL THEN
      LET v_resultado = fn_rechaza_actualizacion("USUARIO NO VALIDO EN EL SISTEMA")
   ELSE
      INITIALIZE v_usr_tmp TO NULL
      
      SELECT 
         usr.usuario_cod
      INTO
         v_usr_tmp
      FROM seg_usuario_perfil usr
      INNER JOIN seg_perfil per ON per.perfil_cod = usr.perfil_cod
      WHERE usr.usuario_cod = actualizaRequest.request.usuarioCod
      AND per.perfil_corta = PERFIL_DATOS_CONTACTO

      IF v_usr_tmp IS NULL THEN
         LET v_resultado = fn_rechaza_actualizacion("USUARIO SIN AUTORIZACIÓN PARA ACTUALIZAR LOS DATOS DE CONTACTO")
      ELSE
         #Se valida si el NSS esta catalogado como 'consulta roja'
         INITIALIZE v_id_derechohabiente TO NULL
         
         SELECT FIRST 1 id_derechohabiente
         INTO v_id_derechohabiente
         FROM afi_nss_rojo
         WHERE nss = actualizaRequest.request.nss
         AND estado_rojo = 1

         IF v_id_derechohabiente IS NOT NULL AND v_id_derechohabiente > 0 THEN
            #CONSULTA ROJA
            INITIALIZE v_usr_tmp TO NULL
      
            SELECT 
               usr.usuario_cod
            INTO
               v_usr_tmp
            FROM seg_usuario_perfil usr
            INNER JOIN seg_perfil per ON per.perfil_cod = usr.perfil_cod
            WHERE usr.usuario_cod = actualizaRequest.request.usuarioCod
            AND per.perfil_corta = PERFIL_CONSULTA_ROJA

            IF v_usr_tmp IS NULL THEN
               LET v_resultado = fn_rechaza_actualizacion("USUARIO SIN AUTORIZACIÓN PARA ACTUALIZAR LOS DATOS DE PERSONAS POLÍTICAMENTE EXPUESTAS")
            ELSE
               LET v_resultado = TRUE
            END IF
         ELSE
            LET v_resultado = TRUE
         END IF 
      END IF
   END IF
   
   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_valida_campos()
   DEFINE v_afore_tmp            SMALLINT
   DEFINE v_ind_act              SMALLINT
   DEFINE v_resultado            BOOLEAN

   LET v_actualiza = "SET "
   LET v_ind_act = 0

   IF actualizaRequest.request.rfc IS NOT NULL THEN
      LET v_actualiza = v_actualiza, "rfc = '", actualizaRequest.request.rfc CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.curp IS NOT NULL THEN
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "curp = '", actualizaRequest.request.curp CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.apellidoPaterno IS NOT NULL THEN 
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "ap_paterno_af = '", actualizaRequest.request.apellidoPaterno CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.apellidoMaterno IS NOT NULL THEN
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "ap_materno_af = '", actualizaRequest.request.apellidoMaterno CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.nombre IS NOT NULL THEN 
      IF v_ind_act = 1 THEN
         LET v_actualiza = v_actualiza, ", "
      END IF
      LET v_actualiza = v_actualiza, "nombre_af = '", actualizaRequest.request.nombre CLIPPED, "' "
      LET v_ind_act = 1
   END IF

   IF actualizaRequest.request.afore IS NOT NULL THEN 
      SELECT afore_cod
      INTO v_afore_tmp
      FROM cat_afore 
      WHERE afore_cod = actualizaRequest.request.afore

      IF v_afore_tmp IS NULL THEN
         RETURN fn_rechaza_actualizacion("CÓDIGO DE AFORE NO VALIDO")
      END IF
   END IF

   #IF actualizaRequest.request.fNacimiento IS NOT NULL THEN 
   #   IF v_ind_act = 1 THEN
   #      LET v_actualiza = v_actualiza, ", "
   #   END IF
   #   LET v_actualiza = v_actualiza, "nombre_af = '", actualizaRequest.request.nombre CLIPPED, "' "
   #   LET v_ind_act = 1
   #END IF

   IF actualizaRequest.request.genero IS NOT NULL THEN 
   END IF

   IF actualizaRequest.request.indFallecido IS NOT NULL THEN 
   END IF

   LET v_resultado = fn_valida_direcciones()

   IF v_resultado THEN
      LET v_resultado = fn_valida_telefonos()
      IF v_resultado THEN
         LET v_resultado = fn_valida_correos()
      END IF
   END IF
   RETURN v_resultado
END FUNCTION

PRIVATE FUNCTION fn_valida_direcciones()
   DEFINE v_item           INTEGER
   DEFINE v_valida_tpo     SMALLINT
   DEFINE v_valida_cp      CHAR(5)
   
   IF actualizaRequest.request.listaDomicilios.domicilio.getLength() > 0 THEN
      #Se enviaron direcciones a actualizar
      DROP TABLE IF EXISTS tmp_tpo_dom_tbl
      
      CREATE TEMP TABLE tmp_tpo_dom_tbl (tpo_dom SMALLINT)

      FOR v_item = 1 TO actualizaRequest.request.listaDomicilios.domicilio.getLength()
         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO TIPO DE DOMICILIO NO PUEDE SER NULO")
         ELSE
            LET v_valida_tpo = 0
            
            SELECT tpo_dom
            INTO v_valida_tpo
            FROM tmp_tpo_dom_tbl
            WHERE tpo_dom = actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio
            
            IF v_valida_tpo IS NOT NULL AND v_valida_tpo > 0 THEN
               RETURN fn_rechaza_actualizacion("NO ESTA PERMITIDO ALMACENAR MAS DE UN DOMICILIO POR CADA TIPO")
            ELSE
               INSERT INTO tmp_tpo_dom_tbl(tpo_dom) VALUES (actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio)
            END IF
         END IF

         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].calle IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO CALLE NO PUEDE SER NULO")
         END IF

         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].colonia IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO COLONIA NO PUEDE SER NULO")
         END IF

         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].delegacionOMunicipio IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO MUNICIPIO NO PUEDE SER NULO")
         END IF

         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].codigoPostal IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO CODIGO POSTAL NO PUEDE SER NULO")
         END IF

         IF actualizaRequest.request.listaDomicilios.domicilio[v_item].entidadFederativa IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO ENTIDAD FEDERATIVA NO PUEDE SER NULO")
         END IF

         INITIALIZE v_valida_cp TO NULL
         
         SELECT cp
         INTO v_valida_cp
         FROM cat_cp
         WHERE cp = actualizaRequest.request.listaDomicilios.domicilio[v_item].codigoPostal
         AND entidad_federativa = actualizaRequest.request.listaDomicilios.domicilio[v_item].entidadFederativa
         AND municipio = actualizaRequest.request.listaDomicilios.domicilio[v_item].delegacionOMunicipio

         IF v_valida_cp IS NULL THEN
            RETURN fn_rechaza_actualizacion("LA ENTIDAD FEDERATIVA O EL MUNICIPIO NO CORRESPONDEN CON EL CÓDIGO POSTAL")
         END IF

      END FOR
   END IF
   RETURN TRUE
END FUNCTION

PRIVATE FUNCTION fn_valida_telefonos()
   DEFINE v_item           INTEGER
   DEFINE v_valida_tpo     SMALLINT
   
   IF actualizaRequest.request.listaTelefonos.telefono.getLength() > 0 THEN
      #Se enviaron direcciones a actualizar
      DROP TABLE IF EXISTS tmp_tpo_tel_tbl
      
      CREATE TEMP TABLE tmp_tpo_tel_tbl (tpo_tel SMALLINT)

      FOR v_item = 1 TO actualizaRequest.request.listaTelefonos.telefono.getLength()
         IF actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO TIPO DE TELEFONO NO PUEDE SER NULO")
         ELSE
            LET v_valida_tpo = 0
            
            SELECT tpo_tel
            INTO v_valida_tpo
            FROM tmp_tpo_tel_tbl
            WHERE tpo_tel = actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono
            
            IF v_valida_tpo IS NOT NULL AND v_valida_tpo > 0 THEN
               RETURN fn_rechaza_actualizacion("NO ESTA PERMITIDO ALMACENAR MAS DE UN TELEFONO POR CADA TIPO")
            ELSE
               INSERT INTO tmp_tpo_tel_tbl(tpo_tel) VALUES (actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono)
            END IF
         END IF

         IF actualizaRequest.request.listaTelefonos.telefono[v_item].telefono IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO TELEFONO NO PUEDE SER NULO")
         END IF

      END FOR
   END IF
   RETURN TRUE
END FUNCTION

PRIVATE FUNCTION fn_valida_correos()
   DEFINE v_item           INTEGER
   DEFINE v_valida_tpo     SMALLINT
   
   IF actualizaRequest.request.listaCorreos.correo.getLength() > 0 THEN
      #Se enviaron direcciones a actualizar
      DROP TABLE IF EXISTS tmp_tpo_cor_tbl
      
      CREATE TEMP TABLE tmp_tpo_cor_tbl (tpo_cor SMALLINT)

      FOR v_item = 1 TO actualizaRequest.request.listaCorreos.correo.getLength()
         IF actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO TIPO DE CORREO NO PUEDE SER NULO")
         ELSE
            LET v_valida_tpo = 0
            
            SELECT tpo_cor
            INTO v_valida_tpo
            FROM tmp_tpo_cor_tbl
            WHERE tpo_cor = actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo
            
            IF v_valida_tpo IS NOT NULL AND v_valida_tpo > 0 THEN
               RETURN fn_rechaza_actualizacion("NO ESTA PERMITIDO ALMACENAR MAS DE UN CORREO ELECTRONICO POR CADA TIPO")
            ELSE
               INSERT INTO tmp_tpo_cor_tbl(tpo_cor) VALUES (actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo)
            END IF
         END IF

         IF actualizaRequest.request.listaCorreos.correo[v_item].correo IS NULL THEN
            RETURN fn_rechaza_actualizacion("EL CAMPO CORREO NO PUEDE SER NULO")
         END IF

      END FOR
   END IF
   RETURN TRUE
END FUNCTION

PRIVATE FUNCTION fn_rechaza_actualizacion(p_mensaje)
   DEFINE p_mensaje        STRING

   LET actualizaResponse.actualizaReturn.codRespuesta = ACTUALIZACION_RECHAZADA
   LET actualizaResponse.actualizaReturn.descripcion = p_mensaje

   DROP TABLE IF EXISTS tmp_tpo_dom_tbl
   DROP TABLE IF EXISTS tmp_tpo_tel_tbl
   DROP TABLE IF EXISTS tmp_tpo_cor_tbl
   
   RETURN FALSE
END FUNCTION

PRIVATE FUNCTION fn_actualiza_datos_generales(p_id_derechohabiente)
   DEFINE p_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_query                      STRING
   #Se actualizan los datos generales
   LET v_query = "UPDATE afi_derechohabiente ", v_actualiza, " WHERE id_derechohabiente = ?"
   PREPARE exe_actualiza_datos FROM v_query
   EXECUTE exe_actualiza_datos USING p_id_derechohabiente

   #Se actualiza la Afore

   DELETE FROM  afi_afore WHERE id_derechohabiente = p_id_derechohabiente

   INSERT INTO afi_afore ( id_derechohabiente,
                           afore_cod,
                           usuario,
                           f_actualiza) 
                  VALUES ( p_id_derechohabiente,
                           actualizaRequest.request.afore,
                           actualizaRequest.request.usuarioCod,
                           TODAY)


   CALL fn_actualiza_direcciones(p_id_derechohabiente)

   CALL fn_actualiza_telefonos(p_id_derechohabiente)

   CALL fn_actualiza_correos(p_id_derechohabiente)

   #Se registra en bitacora la actualizacion de datos de contacto

   INSERT INTO afi_bitacora_datos(  id_afi_bitacora_datos,
                                    id_derechohabiente,
                                    f_actualiza,
                                    usuario_actualiza,
                                    id_sistema)
                           VALUES(  seq_afi_bitacora_datos.nextval,
                                    p_id_derechohabiente,
                                    TODAY,
                                    actualizaRequest.request.usuarioCod,
                                    actualizaRequest.request.id_origen_cambio);
   
END FUNCTION

PRIVATE FUNCTION fn_actualiza_direcciones(p_id_derechohabiente)
   DEFINE p_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_item                       INTEGER

   IF actualizaRequest.request.listaDomicilios.domicilio.getLength() > 0 THEN
      FOR v_item = 1 TO actualizaRequest.request.listaDomicilios.domicilio.getLength()
         DELETE FROM afi_domicilio 
         WHERE id_derechohabiente = p_id_derechohabiente 
         AND tpo_domicilio = actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio

         DELETE FROM afi_domicilio 
         WHERE id_derechohabiente = p_id_derechohabiente 
         AND id_domicilio = actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio

         INSERT INTO afi_domicilio (id_derechohabiente,
                                    id_domicilio,
                                    tpo_domicilio,
                                    ind_envio,
                                    calle,
                                    num_exterior,
                                    num_interior,
                                    colonia,
                                    cp,
                                    entre_calle1,
                                    entre_calle2,
                                    folio_lote,
                                    f_actualiza,
                                    usuario) 
                           VALUES ( p_id_derechohabiente,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].tpo_domicilio,
                                    'X',
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].calle,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].numeroExterior,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].numeroInterior,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].colonia,
                                    actualizaRequest.request.listaDomicilios.domicilio[v_item].codigoPostal,
                                    ' ',
                                    ' ',
                                    1,
                                    TODAY,
                                    actualizaRequest.request.usuarioCod)
         
      END FOR
   END IF
END FUNCTION

PRIVATE FUNCTION fn_actualiza_telefonos(p_id_derechohabiente)
   DEFINE p_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_item                       INTEGER

   IF actualizaRequest.request.listaTelefonos.telefono.getLength() > 0 THEN
      FOR v_item = 1 TO actualizaRequest.request.listaTelefonos.telefono.getLength()
         DELETE FROM afi_telefono 
         WHERE id_derechohabiente = p_id_derechohabiente
         AND tpo_telefono = actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono

         DELETE FROM afi_telefono 
         WHERE id_derechohabiente = p_id_derechohabiente
         AND id_telefono = actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono

         INSERT INTO afi_telefono ( id_derechohabiente,
                                    id_telefono,
                                    cve_lada,
                                    extension,
                                    telefono,
                                    tpo_telefono,
                                    folio_lote) 
                           VALUES ( p_id_derechohabiente,
                                    actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono,
                                    actualizaRequest.request.listaTelefonos.telefono[v_item].cve_lada,
                                    actualizaRequest.request.listaTelefonos.telefono[v_item].extension,
                                    actualizaRequest.request.listaTelefonos.telefono[v_item].telefono,
                                    actualizaRequest.request.listaTelefonos.telefono[v_item].tpo_telefono,
                                    1);
         
      END FOR
   END IF

END FUNCTION

PRIVATE FUNCTION fn_actualiza_correos(p_id_derechohabiente)
   DEFINE p_id_derechohabiente         DECIMAL(9,0)
   DEFINE v_item                       INTEGER

   IF actualizaRequest.request.listaCorreos.correo.getLength() > 0 THEN
      FOR v_item = 1 TO actualizaRequest.request.listaCorreos.correo.getLength()
         DELETE FROM afi_contacto_electronico 
         WHERE id_derechohabiente = p_id_derechohabiente
         AND tpo_correo = actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo

         DELETE FROM afi_contacto_electronico 
         WHERE id_derechohabiente = p_id_derechohabiente
         AND id_contacto_electronico = actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo

         INSERT INTO afi_contacto_electronico ( id_derechohabiente,
                                                id_contacto_electronico,
                                                tpo_correo,
                                                valor,
                                                folio_lote,
                                                f_actualiza,
                                                usuario) 
                                       VALUES ( p_id_derechohabiente,
                                                actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo,
                                                actualizaRequest.request.listaCorreos.correo[v_item].tpo_correo,
                                                actualizaRequest.request.listaCorreos.correo[v_item].correo,
                                                1,
                                                TODAY,
                                                actualizaRequest.request.usuarioCod);
      END FOR
   END IF

END FUNCTION