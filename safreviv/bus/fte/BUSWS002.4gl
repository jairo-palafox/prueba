####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio del        #
#                    bus de tramites                               #
#Fecha inicio      =>24 SEPTIEMBRE 2013                            #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

GLOBALS "BUSWS001.inc"
GLOBALS "BUSWS002.inc"

#Variables de entrada del tramite
PRIVATE DEFINE v_folio_procesar        VARCHAR(50)
PRIVATE DEFINE v_id_sistema            CHAR(10)
PRIVATE DEFINE v_id_ebusiness          CHAR(10)
PRIVATE DEFINE v_id_portafolio         CHAR(10)
PRIVATE DEFINE v_id_servicio           CHAR(10)
PRIVATE DEFINE v_id_cliente            CHAR(10)
PRIVATE DEFINE v_id_canal              CHAR(10)
PRIVATE DEFINE v_cod_oper_cliente      CHAR(50)
PRIVATE DEFINE v_folio_transaccion     CHAR(50)
PRIVATE DEFINE v_cod_respuesta_opr     CHAR(2)
PRIVATE DEFINE v_id_proceso            VARCHAR(3)
PRIVATE DEFINE v_id_operacion          VARCHAR(4)

PRIVATE DEFINE v_rechazos_solicitud    DYNAMIC ARRAY OF rechazos
PRIVATE DEFINE v_parametros_negocio    DYNAMIC ARRAY OF detalle_request
PRIVATE DEFINE v_parametros_bloque     DYNAMIC ARRAY OF detalle_request
PRIVATE DEFINE v_nss                   CHAR(11)
PRIVATE DEFINE v_curp                  CHAR(18)

#Variables de salida
PRIVATE DEFINE v_r_folio_Ack           CHAR(50)
PRIVATE DEFINE v_r_cod_respuesta       CHAR(2)
PRIVATE DEFINE v_r_cod_respuesta_opr   CHAR(2)
PRIVATE DEFINE v_r_desc_respuesta      VARCHAR(100)
PRIVATE DEFINE v_r_cod_oper_cliente    CHAR(50)
PRIVATE DEFINE v_r_rechazos_respuesta  DYNAMIC ARRAY OF rechazos

#Variables para el manejo de las validaciones
PRIVATE DEFINE v_id_cat_bus_contrato   DECIMAL(9,0)
PRIVATE DEFINE v_id_bus_tramite        DECIMAL(9,0)

#Variables para el manejo de la bitacora
PRIVATE DEFINE v_id_bus_solicitud_tramite        DECIMAL(9,0)

PRIVATE DEFINE v_hora_inicio           DATETIME FRACTION TO FRACTION(3)
PRIVATE DEFINE v_hora_fin              DATETIME FRACTION TO FRACTION(3) 

{
FUNCTION fn_prepara_consulta()
   DEFINE v_fn_genera_folio_bus              STRING
   DEFINE v_fn_genera_tramite                STRING
   DEFINE v_fn_inserta_solicitud_tramite     STRING
   DEFINE v_actualiza_nss                    STRING
   DEFINE v_inserta_error_solicitud          STRING
   DEFINE v_inserta_detalle_solicitud        STRING
   DEFINE v_fn_inserta_respuesta_tramite     STRING
   DEFINE v_inserta_error_respuesta          STRING
   DEFINE v_consulta_rechazo                 STRING
   DEFINE v_consulta_error                   STRING
   DEFINE v_consulta_detalle_contrato        STRING
   DEFINE v_consulta_numero_campos           STRING
   DEFINE v_consulta_bloque                  STRING
   DEFINE v_consulta_detalle_bloque          STRING
   DEFINE v_seq_bus_detalle_solicitud        STRING
   DEFINE v_inserta_detalle_bloque           STRING

   DEFINE v_fn_valida_caracter_especial      STRING
   DEFINE v_fn_valida_nss                    STRING
   DEFINE v_fn_valida_numero                 STRING
   DEFINE v_fn_valida_letra                  STRING

   DATABASE safre_viv

   LET v_fn_genera_folio_bus = "EXECUTE FUNCTION fn_genera_folio_bus(?,?)"
   PREPARE exe_fn_genera_folio_bus FROM v_fn_genera_folio_bus

   LET v_fn_genera_tramite = "EXECUTE FUNCTION fn_genera_tramite(?,?,?,?)"
   PREPARE exe_fn_genera_tramite FROM v_fn_genera_tramite

   LET v_fn_inserta_solicitud_tramite = "EXECUTE FUNCTION fn_inserta_solicitud_tramite(?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_solicitud_tramite FROM v_fn_inserta_solicitud_tramite

   LET v_actualiza_nss = "UPDATE bus_solicitud_tramite set nss = ?, curp = ? WHERE id_bus_solicitud_tramite = ?"
   PREPARE exe_actualiza_nss FROM v_actualiza_nss

   LET v_inserta_error_solicitud = "INSERT INTO bus_error_solicitud (id_bus_error_solicitud,id_bus_solicitud_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_solicitud.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_solicitud FROM v_inserta_error_solicitud

   LET v_seq_bus_detalle_solicitud = "SELECT seq_bus_detalle_solicitud.NEXTVAL from systables WHERE tabid=1"
   PREPARE exe_seq_bus_detalle_solicitud FROM v_seq_bus_detalle_solicitud
   
   LET v_inserta_detalle_solicitud = "INSERT INTO bus_detalle_solicitud (id_bus_solicitud_detalle,id_bus_solicitud_tramite,nombre_campo,valor) ",
                                    "VALUES (?,?,?,?)"
   PREPARE exe_inserta_detalle_solicitud FROM v_inserta_detalle_solicitud

   LET v_fn_inserta_respuesta_tramite = "EXECUTE FUNCTION fn_inserta_respuesta_tramite(?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_respuesta_tramite FROM v_fn_inserta_respuesta_tramite

   LET v_inserta_error_respuesta = "INSERT INTO bus_error_respuesta (id_bus_error_respuesta,id_bus_respuesta_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_respuesta.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_respuesta FROM v_inserta_error_respuesta

   LET v_consulta_rechazo = "SELECT cod_rechazo, desc_rechazo FROM cat_bus_rechazo WHERE nombre_campo = ? AND tipo_rechazo = ?";
   PREPARE exe_consulta_rechazo FROM v_consulta_rechazo
   
   LET v_consulta_error = "SELECT desc_rechazo FROM cat_bus_rechazo WHERE cod_rechazo = ? AND tipo_rechazo = ?";
   PREPARE exe_consulta_error FROM v_consulta_error

   LET v_consulta_detalle_contrato = "SELECT id_cat_bus_detalle_contrato, cve_natural, tipo_dato, ind_opcional ",
                                     "FROM cat_bus_detalle_contrato  WHERE id_cat_bus_contrato = ? ORDER BY orden"
   PREPARE exe_consulta_detalle_contrato FROM v_consulta_detalle_contrato

   LET v_consulta_numero_campos = "SELECT COUNT(*) FROM cat_bus_detalle_contrato WHERE id_cat_bus_contrato = ?"
   PREPARE exe_consulta_numero_campos FROM v_consulta_numero_campos

   LET v_fn_valida_caracter_especial = "EXECUTE FUNCTION fn_valida_caracter_especial(?)"
   PREPARE exe_fn_valida_caracter_especial FROM v_fn_valida_caracter_especial

   LET v_fn_valida_nss = "EXECUTE FUNCTION fn_valida_nss(?)"
   PREPARE exe_fn_valida_nss FROM v_fn_valida_nss

   LET v_fn_valida_numero = "EXECUTE FUNCTION fn_valida_numero(?)"
   PREPARE exe_fn_valida_numero FROM v_fn_valida_numero

   LET v_fn_valida_letra = "EXECUTE FUNCTION fn_valida_letra(?)"
   PREPARE exe_fn_valida_letra FROM v_fn_valida_letra

   LET v_consulta_bloque = "SELECT ",
                           "cza.cve_natural, ",
                           "blo.cve_natural, ",
                           "blo.id_cat_bus_bloque ",
                           "FROM cat_bus_detalle_contrato cza ",
                           "INNER JOIN cat_bus_bloque blo ON blo.id_cat_bus_detalle_contrato = cza.id_cat_bus_detalle_contrato ",
                           "WHERE cza.id_cat_bus_detalle_contrato = ?"
   PREPARE exe_consulta_bloque FROM v_consulta_bloque

   LET v_consulta_detalle_bloque =  "SELECT id_cat_bus_detalle_bloque, cve_natural_bloque, tipo_dato, ind_opcional ",
                                    "FROM cat_bus_detalle_bloque  WHERE id_cat_bus_bloque = ? ORDER BY orden"
   PREPARE exe_consulta_detalle_bloque FROM v_consulta_detalle_bloque

   LET v_inserta_detalle_bloque = "INSERT INTO bus_detalle_bloque (id_bus_detalle_bloque,id_bus_solicitud_detalle,id_bus_agrupa_bloque,nombre_campo_bloque,valor) ",
                                  "VALUES (seq_bus_detalle_bloque.NEXTVAL,?,?,?,?);"
   PREPARE exe_inserta_detalle_bloque FROM v_inserta_detalle_bloque
   
END FUNCTION
}
FUNCTION fn_notificar_tramite()
   DEFINE i                         SMALLINT
   DEFINE v_ind_error_mapa          SMALLINT
   DEFINE v_ind_error_estructura    SMALLINT
   DEFINE v_ind_rechazo             SMALLINT
   DEFINE v_ind_genera_acuse        SMALLINT

   DEFINE v_campo_encabezado        VARCHAR(60)
   DEFINE v_cod_rechazo          VARCHAR(10)
   DEFINE v_desc_rechazo         VARCHAR(100)

   ###############################################################################################################################################################
   DEFINE v_fn_genera_folio_bus              STRING
   DEFINE v_fn_genera_tramite                STRING
   DEFINE v_fn_inserta_solicitud_tramite     STRING
   DEFINE v_actualiza_nss                    STRING
   DEFINE v_inserta_error_solicitud          STRING
   DEFINE v_inserta_detalle_solicitud        STRING
   DEFINE v_fn_inserta_respuesta_tramite     STRING
   DEFINE v_inserta_error_respuesta          STRING
   DEFINE v_consulta_rechazo                 STRING
   DEFINE v_consulta_error                   STRING
   DEFINE v_consulta_detalle_contrato        STRING
   DEFINE v_consulta_numero_campos           STRING
   DEFINE v_consulta_bloque                  STRING
   DEFINE v_consulta_detalle_bloque          STRING
   DEFINE v_seq_bus_detalle_solicitud        STRING
   DEFINE v_inserta_detalle_bloque           STRING

   DEFINE v_fn_valida_caracter_especial      STRING
   DEFINE v_fn_valida_nss                    STRING
   DEFINE v_fn_valida_numero                 STRING
   DEFINE v_fn_valida_letra                  STRING
   ###############################################################################################################################################################

   LET v_hora_inicio = CURRENT FRACTION TO FRACTION(3)

   INITIALIZE v_rechazos_solicitud TO NULL 
   INITIALIZE v_parametros_negocio TO NULL 
   INITIALIZE v_r_rechazos_respuesta TO NULL 

   INITIALIZE v_id_bus_tramite TO NULL
   INITIALIZE v_id_cat_bus_contrato TO NULL

   INITIALIZE v_r_folio_Ack TO NULL
   INITIALIZE v_r_cod_respuesta TO NULL
   INITIALIZE v_r_cod_respuesta_opr TO NULL
   INITIALIZE v_r_desc_respuesta TO NULL

   ###############################################################################################################################################################
   DATABASE safre_viv

   LET v_fn_genera_folio_bus = "EXECUTE FUNCTION fn_genera_folio_bus(?,?)"
   PREPARE exe_fn_genera_folio_bus FROM v_fn_genera_folio_bus

   LET v_fn_genera_tramite = "EXECUTE FUNCTION fn_genera_tramite(?,?,?,?)"
   PREPARE exe_fn_genera_tramite FROM v_fn_genera_tramite

   LET v_fn_inserta_solicitud_tramite = "EXECUTE FUNCTION fn_inserta_solicitud_tramite(?,?,?,?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_solicitud_tramite FROM v_fn_inserta_solicitud_tramite

   LET v_actualiza_nss = "UPDATE bus_solicitud_tramite set nss = ?, curp = ? WHERE id_bus_solicitud_tramite = ?"
   PREPARE exe_actualiza_nss FROM v_actualiza_nss

   LET v_inserta_error_solicitud = "INSERT INTO bus_error_solicitud (id_bus_error_solicitud,id_bus_solicitud_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_solicitud.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_solicitud FROM v_inserta_error_solicitud

   LET v_seq_bus_detalle_solicitud = "SELECT seq_bus_detalle_solicitud.NEXTVAL from systables WHERE tabid=1"
   PREPARE exe_seq_bus_detalle_solicitud FROM v_seq_bus_detalle_solicitud
   
   LET v_inserta_detalle_solicitud = "INSERT INTO bus_detalle_solicitud (id_bus_solicitud_detalle,id_bus_solicitud_tramite,nombre_campo,valor) ",
                                    "VALUES (?,?,?,?)"
   PREPARE exe_inserta_detalle_solicitud FROM v_inserta_detalle_solicitud

   LET v_fn_inserta_respuesta_tramite = "EXECUTE FUNCTION fn_inserta_respuesta_tramite(?,?,?,?,?,?,?)"
   PREPARE exe_fn_inserta_respuesta_tramite FROM v_fn_inserta_respuesta_tramite

   LET v_inserta_error_respuesta = "INSERT INTO bus_error_respuesta (id_bus_error_respuesta,id_bus_respuesta_tramite,cod_error,desc_error) ",
                                    "VALUES (seq_bus_error_respuesta.NEXTVAL,?,?,?)"
   PREPARE exe_inserta_error_respuesta FROM v_inserta_error_respuesta

   LET v_consulta_rechazo = "SELECT cod_rechazo, desc_rechazo FROM cat_bus_rechazo WHERE nombre_campo = ? AND tipo_rechazo = ?";
   PREPARE exe_consulta_rechazo FROM v_consulta_rechazo
   
   LET v_consulta_error = "SELECT desc_rechazo FROM cat_bus_rechazo WHERE cod_rechazo = ? AND tipo_rechazo = ?";
   PREPARE exe_consulta_error FROM v_consulta_error

   LET v_consulta_detalle_contrato = "SELECT id_cat_bus_detalle_contrato, cve_natural, tipo_dato, ind_opcional ",
                                     "FROM cat_bus_detalle_contrato  WHERE id_cat_bus_contrato = ? ORDER BY orden"
   PREPARE exe_consulta_detalle_contrato FROM v_consulta_detalle_contrato

   LET v_consulta_numero_campos = "SELECT COUNT(*) FROM cat_bus_detalle_contrato WHERE id_cat_bus_contrato = ?"
   PREPARE exe_consulta_numero_campos FROM v_consulta_numero_campos

   LET v_fn_valida_caracter_especial = "EXECUTE FUNCTION fn_valida_caracter_especial(?)"
   PREPARE exe_fn_valida_caracter_especial FROM v_fn_valida_caracter_especial

   LET v_fn_valida_nss = "EXECUTE FUNCTION fn_valida_nss(?)"
   PREPARE exe_fn_valida_nss FROM v_fn_valida_nss

   LET v_fn_valida_numero = "EXECUTE FUNCTION fn_valida_numero(?)"
   PREPARE exe_fn_valida_numero FROM v_fn_valida_numero

   LET v_fn_valida_letra = "EXECUTE FUNCTION fn_valida_letra(?)"
   PREPARE exe_fn_valida_letra FROM v_fn_valida_letra

   LET v_consulta_bloque = "SELECT ",
                           "cza.cve_natural, ",
                           "blo.cve_natural, ",
                           "blo.id_cat_bus_bloque ",
                           "FROM cat_bus_detalle_contrato cza ",
                           "INNER JOIN cat_bus_bloque blo ON blo.id_cat_bus_detalle_contrato = cza.id_cat_bus_detalle_contrato ",
                           "WHERE cza.id_cat_bus_detalle_contrato = ?"
   PREPARE exe_consulta_bloque FROM v_consulta_bloque

   LET v_consulta_detalle_bloque =  "SELECT id_cat_bus_detalle_bloque, cve_natural_bloque, tipo_dato, ind_opcional ",
                                    "FROM cat_bus_detalle_bloque  WHERE id_cat_bus_bloque = ? ORDER BY orden"
   PREPARE exe_consulta_detalle_bloque FROM v_consulta_detalle_bloque

   LET v_inserta_detalle_bloque = "INSERT INTO bus_detalle_bloque (id_bus_detalle_bloque,id_bus_solicitud_detalle,id_bus_agrupa_bloque,nombre_campo_bloque,valor) ",
                                  "VALUES (seq_bus_detalle_bloque.NEXTVAL,?,?,?,?);"
   PREPARE exe_inserta_detalle_bloque FROM v_inserta_detalle_bloque

   ###################################################################################################################################################################

   #por defecto se establece que NO se genera folio de acuse
   LET v_ind_genera_acuse = 0
   
   #Primero se obtienen todos los valores de entrada
   LET v_id_servicio       = ns2notificarTramiteRequest.encabezado.idServicio
   LET v_id_cliente        = ns2notificarTramiteRequest.encabezado.idCliente
   
   # sólo se deben recibir numéricos
   LET v_id_proceso        = ns2notificarTramiteRequest.encabezado.idProceso
   LET v_id_operacion      = ns2notificarTramiteRequest.encabezado.idOperacion
      
   LET v_folio_procesar    = ns2notificarTramiteRequest.cuerpo.folioDeTramiteProcesar
   LET v_folio_transaccion = ns2notificarTramiteRequest.cuerpo.folioDeTransaccion
   LET v_cod_respuesta_opr = ns2notificarTramiteRequest.cuerpo.codRespuestaOpr

   CALL STARTLOG("/safreviv_log/bus/debug_generico_" || v_id_proceso CLIPPED || "_" || v_id_operacion CLIPPED || "_" || v_folio_procesar CLIPPED || ".log")
   CALL ERRORLOG("*************************************************************************************************************************************")
   CALL ERRORLOG("NUEVA PETICIÓN")

   IF v_id_proceso IS NULL OR v_id_proceso == '' THEN
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      LET v_campo_encabezado = 'idProceso'
      EXECUTE exe_consulta_rechazo USING v_campo_encabezado,
                                       CAMPO_NO_EXISTE
                                 INTO  v_cod_rechazo,
                                      v_desc_rechazo

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo

      CALL ERRORLOG("ERROR: el campo idProceso llego vacio...")

      CALL fn_genera_salida()
      RETURN
   END IF

   IF v_id_operacion IS NULL OR v_id_operacion == '' THEN
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      LET v_campo_encabezado = 'idOperacion'
      EXECUTE exe_consulta_rechazo USING v_campo_encabezado,
                                       CAMPO_NO_EXISTE
                                 INTO  v_cod_rechazo,
                                      v_desc_rechazo

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo

      CALL ERRORLOG("ERROR: el campo idOperacion llego vacio...")

      CALL fn_genera_salida()
      RETURN
   END IF

   IF v_id_servicio IS NULL OR v_id_servicio == '' THEN
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      LET v_campo_encabezado = 'idServicio'
      EXECUTE exe_consulta_rechazo USING v_campo_encabezado,
                                       CAMPO_NO_EXISTE
                                 INTO  v_cod_rechazo,
                                      v_desc_rechazo

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo

      CALL ERRORLOG("ERROR: el campo idServicio llego vacio...")
      
      CALL fn_genera_salida()
      RETURN
   END IF

   IF v_id_cliente IS NULL OR v_id_cliente == '' THEN
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      LET v_campo_encabezado = 'idCliente'
      EXECUTE exe_consulta_rechazo USING v_campo_encabezado,
                                       CAMPO_NO_EXISTE
                                 INTO  v_cod_rechazo,
                                      v_desc_rechazo

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo

      CALL ERRORLOG("ERROR: el campo idCliente llego vacio...")

      CALL fn_genera_salida()
      RETURN
   END IF

   IF v_folio_procesar IS NULL OR v_folio_procesar == '' THEN
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      LET v_campo_encabezado = 'folioDeTramiteProcesar'
      EXECUTE exe_consulta_rechazo USING v_campo_encabezado,
                                       CAMPO_NO_EXISTE
                                 INTO  v_cod_rechazo,
                                      v_desc_rechazo

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo

      CALL ERRORLOG("ERROR: el campo folioDeTramiteProcesar llego vacio...")

      CALL fn_genera_salida()
      RETURN
   END IF

   WHENEVER ERROR CONTINUE

   #aqui se valida que el proceso y operacion sean validos en el sistema
   EXECUTE exe_fn_genera_tramite USING v_id_proceso,
                                       v_id_operacion,
                                       v_folio_procesar,
                                       ORIGEN_PROCESAR
                                 INTO  v_id_bus_tramite,
                                       v_id_cat_bus_contrato

   IF SQLCA.SQLCODE <> 0 THEN
      CALL ERRORLOG("ERROR: Ocurrio un error al ejecutar la funcion que registra el tramite en la bitacora")
      CALL ERRORLOG(SQLCA.SQLCODE || " : " || SQLERRMESSAGE)
      CALL ERRORLOG("PARAMETROS: ")
      CALL ERRORLOG("")
      CALL ERRORLOG("id_proceso: " || v_id_proceso)
      CALL ERRORLOG("id_operacion: " || v_id_operacion)
      CALL ERRORLOG("folio_procesar: " || v_folio_procesar)
      CALL ERRORLOG("origen: " || ORIGEN_PROCESAR)
      
      LET v_r_cod_respuesta = COD_ERROR     
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      #EXECUTE exe_consulta_error USING ERROR_REGISTRO_TRAMITE,
      #                                 ERROR_DE_SISTEMA
      #                           INTO  v_r_desc_respuesta

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = ERROR_REGISTRO_TRAMITE
      LET v_r_rechazos_respuesta[1].desc_rechazo = SQLCA.SQLCODE || " - " || SQLERRMESSAGE

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = ERROR_REGISTRO_TRAMITE
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_r_desc_respuesta
   
   ELSE     #FIN VALIDACION DE TRAMITE CON ERROR
      #Se valida que el resultado de la funcion que genera el tramite encontro la configuracion de la operacion solicitada
      CALL ERRORLOG("El resultado del registro en bitacora fue id_en_bitacora = " || v_id_bus_tramite || ", id_contrato = " || v_id_cat_bus_contrato)
      IF v_id_bus_tramite > 0 AND v_id_cat_bus_contrato > 0 THEN
         CALL ERRORLOG("El registro en bitacora fue correcto")
         #Se inserta la solicitud
         EXECUTE exe_fn_inserta_solicitud_tramite USING  v_id_bus_tramite,
                                                         v_id_sistema,
                                                         v_id_ebusiness,    
                                                         v_id_portafolio,   
                                                         v_id_servicio,
                                                         v_id_cliente,
                                                         v_id_canal,
                                                         v_cod_oper_cliente,
                                                         v_folio_transaccion,
                                                         v_cod_respuesta_opr
                                                   INTO  v_id_bus_solicitud_tramite
         #Se busca si llegaron codigos de rechazo
         LET v_ind_rechazo = 0
         FOR i = 1 TO ns2notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo.getLength()
            CALL v_rechazos_solicitud.appendElement()
            LET v_rechazos_solicitud[i].desc_rechazo = ns2notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo[i].descripcionRechazo
            LET v_rechazos_solicitud[i].cod_rechazo = ns2notificarTramiteRequest.cuerpo.motivosRechazo.motivoRechazo[i].idMotivoRechazo
            EXECUTE exe_inserta_error_solicitud USING v_id_bus_solicitud_tramite, 
                                                      v_rechazos_solicitud[i].cod_rechazo,
                                                      v_rechazos_solicitud[i].desc_rechazo
            LET v_ind_rechazo = 1
         END FOR

         IF (v_ind_rechazo = 0) THEN

            IF ns2notificarTramiteRequest.cuerpo.notificacionXML IS NULL OR ns2notificarTramiteRequest.cuerpo.notificacionXML == '' THEN
               CALL ERRORLOG("Error: El campo notificacionXML llego vacio o nulo en la peticion")
               LET v_r_cod_respuesta = COD_ERROR
               LET v_r_cod_respuesta_opr = COD_OPR_ERROR
               EXECUTE exe_consulta_error USING ERROR_XML_MAL_FORMADO,
                                                ERROR_DE_SISTEMA
                                          INTO  v_r_desc_respuesta

               CALL v_r_rechazos_respuesta.appendElement()
               LET v_r_rechazos_respuesta[1].cod_rechazo = ERROR_XML_MAL_FORMADO
               LET v_r_rechazos_respuesta[1].desc_rechazo = "Dato notificacionXML no existe"

               CALL fn_genera_salida()
               RETURN
            END IF
   
            #Se descompone el mapa del campo generico solo si no existen codigos de rechazo
            INITIALIZE v_nss TO NULL
            INITIALIZE v_curp TO NULL
            CALL fn_separa_mapa_parametros(ns2notificarTramiteRequest.cuerpo.notificacionXML) RETURNING v_ind_error_mapa

            #Si la variable v_ind_error_mapa <> 0 significa que ocurrio un error al intentar descomponer el mapa de valores
            IF v_ind_error_mapa = 0 THEN
               IF (v_nss IS NOT NULL OR v_curp IS NOT NULL) THEN
                  EXECUTE exe_actualiza_nss USING v_nss, v_curp, v_id_bus_solicitud_tramite
               END IF

               #Se valida la estructura de los campos dentro del XML
               CALL fn_valida_estructura_mapa() RETURNING v_ind_error_estructura

               IF v_ind_error_estructura = 0 THEN
                  #este bloque corresponde a los tramites que pasaron correctamente las validaciones de estructura y es necesario ejecutar el negocio
                  LET v_ind_genera_acuse = 1
               END IF

            END IF   #FIN MAPA CON ESTRUCTURA CORRECTO
         ELSE     #FIN MENSAJE SIN CODIGOS DE RECHAZO
            #si el mensaje de entrada contiene errores se establece que se tiene que enviar folio de acuse
            LET v_ind_genera_acuse = 1
         END IF    #FIN MENSAJE CON CODIGOS DE RECHAZO

         IF v_ind_genera_acuse = 1 THEN
            LET v_r_cod_respuesta = COD_OK
            LET v_r_cod_respuesta_opr = COD_OPR_OK
            LET v_r_desc_respuesta = DESC_TRAMITE_VALIDO

            #Se ejecuta el negocio
            CALL fn_ejecuta_negocio()
            
            #Se genera el folio de acuse
            EXECUTE exe_fn_genera_folio_bus USING v_id_proceso, v_id_operacion
                                            INTO v_r_folio_Ack
            LET v_r_folio_Ack = v_r_folio_Ack CLIPPED
            CALL ERRORLOG("El folio de acuse de la petición es: " || v_r_folio_Ack)
         END IF 
         
      ELSE     #FIN CONFIGURACION DE TRAMITE CORRECTA
         CALL ERRORLOG("Error: La funcion que registra el tramite en la bitacora no se ejecuto correctamente")
         CALL ERRORLOG("Parametros: ")
         CALL ERRORLOG("")
         CALL ERRORLOG("id_proceso: " || v_id_proceso)
         CALL ERRORLOG("id_operacion: " || v_id_operacion)
         CALL ERRORLOG("folio_procesar: " || v_folio_procesar)
         CALL ERRORLOG("origen: " || ORIGEN_PROCESAR)
         CALL ERRORLOG("")
         CALL ERRORLOG("Respuesta:")
         CALL ERRORLOG("")
         CALL ERRORLOG("id en bitacora: " || v_id_bus_tramite)
         CALL ERRORLOG("id del contrato: " || v_id_cat_bus_contrato)
         LET v_r_cod_respuesta = COD_ERROR     
         LET v_r_cod_respuesta_opr = COD_OPR_ERROR
         EXECUTE exe_consulta_error USING ERROR_CONFIG_OPERACION,
                                          ERROR_DE_SISTEMA
                                    INTO  v_r_desc_respuesta

         CALL v_r_rechazos_respuesta.appendElement()
         LET v_r_rechazos_respuesta[1].cod_rechazo = ERROR_CONFIG_OPERACION
         LET v_r_rechazos_respuesta[1].desc_rechazo = v_r_desc_respuesta
      END IF   #FIN CONFIGURACION DE TRAMITE INCORRECTA
   END IF   #FIN VALIDACION DE TRAMITE SIN ERROR

   #Se genera la salida del WS
   CALL fn_genera_salida()
   CALL ERRORLOG("SALIDA FINAL:")
   CALL ERRORLOG("ssnrop.codRespuesta: " || ns2notificarTramiteResponse.ssnrop.codRespuesta)
   CALL ERRORLOG("ssnrop.codRespuestaOpr: " || ns2notificarTramiteResponse.ssnrop.codRespuestaOpr)
   #CALL ERRORLOG("ssnrop.codoper: " || ns2notificarTramiteResponse.ssnrop.codoper)
   #CALL ERRORLOG("ssnrop.codoperCliente: " || ns2notificarTramiteResponse.ssnrop.codoperCliente)
   CALL ERRORLOG("ssnrop.descRespuesta: " || ns2notificarTramiteResponse.ssnrop.descRespuesta)
   CALL ERRORLOG("ssnrop.fecha: " || ns2notificarTramiteResponse.ssnrop.fecha)
   CALL ERRORLOG("ssnrop.tiempoRespuesta: " || ns2notificarTramiteResponse.ssnrop.tiempoRespuesta)
   IF ns2notificarTramiteResponse.ssnrop.motivos.motivo.getLength() > 0 THEN
      CALL ERRORLOG("ssnrop.motivos.motivo[1].idMotivo: " || ns2notificarTramiteResponse.ssnrop.motivos.motivo[1].idMotivo)
      CALL ERRORLOG("ssnrop.motivos.motivo[1].descripcion: " || ns2notificarTramiteResponse.ssnrop.motivos.motivo[1].descripcion)
   END IF

   CALL ERRORLOG("objetoRespuesta.folioAck: " || ns2notificarTramiteResponse.objetoRespuesta.folioAck)
   
   WHENEVER ERROR STOP
END FUNCTION

PRIVATE FUNCTION fn_separa_mapa_parametros(p_notificacionXML)
   DEFINE p_notificacionXML            STRING
   DEFINE v_id_bus_solicitud_detalle   DECIMAL(9,0)

   DEFINE doc                    xml.DomDocument
   DEFINE lista                  xml.DomNodeList
   DEFINE item                   xml.DomNode

   DEFINE lista_valor            xml.DomNodeList
   DEFINE nodo                   xml.DomNode
   DEFINE nombre                 STRING
   DEFINE valor                  STRING
   DEFINE i                      SMALLINT
   DEFINE v_ind_error_mapa       SMALLINT
   DEFINE v_num_caracter         INTEGER
   DEFINE v_caracter_fin         INTEGER
   DEFINE v_ind_arreglo          SMALLINT

   LET v_ind_error_mapa = 0
   TRY 
      LET doc = xml.DomDocument.Create()
      CALL doc.loadFromString(p_notificacionXML)
      
   CATCH
      CALL ERRORLOG("Error: No fue posible cargar el campo notificacionXML para obtener los campos")
      CALL ERRORLOG(p_notificacionXML)
      LET v_ind_error_mapa = 1
      LET v_r_cod_respuesta = COD_ERROR
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR
      EXECUTE exe_consulta_error USING ERROR_XML_MAL_FORMADO,
                                       ERROR_DE_SISTEMA
                                 INTO  v_r_desc_respuesta

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = ERROR_XML_MAL_FORMADO
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_r_desc_respuesta
         
      RETURN v_ind_error_mapa
   END TRY
   #Primero se obtienen todos los elementos 'entry'
   CALL doc.getElementsByTagName(ELEMENTO) RETURNING lista
   FOR i = 1 TO lista.getCount()
      CALL lista.getItem(i) RETURNING item
      TRY 
         #se busca el nombre del campo
         CALL item.getElementsByTagName(NOMBRE_CAMPO) RETURNING lista_valor
         CALL lista_valor.getItem(1) RETURNING nodo
         CALL nodo.getFirstChild() RETURNING nodo
         CALL nodo.toString() RETURNING nombre
      CATCH
         LET nombre = ''
      END TRY 
      #se busca el valor del campo
      TRY 
         CALL item.getElementsByTagName(VALOR_CAMPO) RETURNING lista_valor
         CALL lista_valor.getItem(1) RETURNING nodo
         CALL nodo.getFirstChild() RETURNING nodo
         CALL nodo.toString() RETURNING valor
      CATCH
         #En caso de error el valor se deja en nulo
         LET valor = NULL
      END TRY
      CALL v_parametros_negocio.appendElement()
      LET v_parametros_negocio[i].num_campo = i
      LET v_parametros_negocio[i].ind_existe = 0
      LET v_parametros_negocio[i].nombre_campo = nombre

      #Se valida si el campo contiene un valor sencillo o si es un arreglo
      CALL valor.getIndexOf('<![CDATA[', 1) RETURNING v_num_caracter
      IF v_num_caracter > 0 THEN
         #Significa que el valor contiene un arreglo
         LET v_num_caracter = v_num_caracter + 9
         LET v_caracter_fin = valor.getLength() - 3
         CALL valor.subString(v_num_caracter, v_caracter_fin) RETURNING valor
         LET v_parametros_negocio[i].valor = "Arreglo de ", nombre CLIPPED
         LET v_ind_arreglo = 1
      ELSE
         CALL valor.getIndexOf('</', 1) RETURNING v_num_caracter
         IF v_num_caracter > 0 THEN
            LET v_parametros_negocio[i].valor = "Arreglo de ", nombre CLIPPED
            LET v_ind_arreglo = 1
         ELSE
            #Se trata de un campo normal
            LET v_parametros_negocio[i].valor = valor
            LET v_ind_arreglo = 0
         END IF
      END IF

      #Se agrega la separacion del nss y curp
      IF nombre = CAMPO_NSS_IMSS THEN
         LET v_nss = valor
      END IF
      IF nombre = CAMPO_CURP THEN
         LET v_curp = valor
      END IF

      EXECUTE exe_seq_bus_detalle_solicitud INTO v_id_bus_solicitud_detalle 

      EXECUTE exe_inserta_detalle_solicitud USING  v_id_bus_solicitud_detalle,
                                                   v_id_bus_solicitud_tramite,
                                                   v_parametros_negocio[i].nombre_campo,
                                                   v_parametros_negocio[i].valor

      IF v_ind_arreglo = 1 THEN
         CALL fn_inserta_detalle_arreglo(v_id_bus_solicitud_detalle, valor) RETURNING v_ind_error_mapa, valor
         LET v_parametros_negocio[i].valor_xml = valor
      END IF
   END FOR
   
   RETURN v_ind_error_mapa
END FUNCTION

#Metodo que valida el campo notificacionXML sin considerar el orden de los campos
PRIVATE FUNCTION fn_valida_estructura_mapa()
   DEFINE v_id_detalle_contrato  DECIMAL(9,0)
   DEFINE v_campo                VARCHAR(60)
   DEFINE v_tipo_dato            CHAR(1)
   DEFINE v_ind_opcional         INTEGER
   
   DEFINE i                      SMALLINT
   DEFINE v_ind_error            SMALLINT
   DEFINE v_ind_valido           SMALLINT

   LET v_ind_error = 0
   
   #Buscamos el mapa de valores configurados para el tramite
   DECLARE cur_consulta_detalle_contrato CURSOR FOR exe_consulta_detalle_contrato
   LET i = 1
   FOREACH cur_consulta_detalle_contrato USING v_id_cat_bus_contrato INTO v_id_detalle_contrato, v_campo, v_tipo_dato, v_ind_opcional
      LET v_ind_valido = 0
      #Se busca en el arreglo del request el campo del contrato
      FOR i = 1 TO v_parametros_negocio.getLength()
         IF v_parametros_negocio[i].nombre_campo CLIPPED = v_campo CLIPPED THEN
            LET v_ind_valido = 1
            IF v_parametros_negocio[i].ind_existe = 1 THEN
               #Campo duplicado en el request
               CALL ERRORLOG("Error: el campo " || v_campo || " se encuentra duplicado")
               LET v_r_cod_respuesta = COD_ERROR
               LET v_r_cod_respuesta_opr = COD_OPR_ERROR
               LET v_r_desc_respuesta = DESC_ERROR_VALIDACION

               CALL v_r_rechazos_respuesta.appendElement()
               LET v_r_rechazos_respuesta[1].cod_rechazo = RECHAZO_GENERICO
               LET v_r_rechazos_respuesta[1].desc_rechazo = DES_RECHAZO_GENERICO_DUP, v_campo CLIPPED
               LET v_ind_error = 1
               RETURN v_ind_error
            ELSE
               IF v_parametros_negocio[i].valor IS NOT NULL AND LENGTH(v_parametros_negocio[i].valor CLIPPED) > 0 THEN
                  #El campo es correcto, ahora se valida el tipo de dato
                  CALL fn_valida_campo(v_id_detalle_contrato, 
                                       v_parametros_negocio[i].nombre_campo, 
                                       v_parametros_negocio[i].valor,
                                       v_parametros_negocio[i].valor_xml, 
                                       v_tipo_dato) RETURNING v_ind_error
                  IF v_ind_error <> 0 THEN
                     CALL fn_genera_error_estructura(v_campo, CAMPO_INVALIDO)
                     RETURN v_ind_error
                  END IF
               ELSE
                  #Se verifica si el campo esta configurado como opcional
                  IF v_ind_opcional IS NULL OR v_ind_opcional <> 1 THEN
                     LET v_ind_error = 1
                     CALL fn_genera_error_estructura(v_campo, CAMPO_INVALIDO)
                     RETURN v_ind_error
                  END IF
               END IF
            END IF
            LET v_parametros_negocio[i].ind_existe = 1
            EXIT FOR
         END IF
      END FOR
      IF v_ind_valido <> 1 THEN
         LET v_ind_error = 1
         CALL fn_genera_error_estructura(v_campo CLIPPED, CAMPO_NO_EXISTE)
         RETURN v_ind_error
      END IF
   END FOREACH
   FOR i = 1 TO v_parametros_negocio.getLength()
      IF v_parametros_negocio[i].ind_existe <> 1 THEN
         LET v_ind_error = 1
         CALL fn_genera_error_estructura(v_parametros_negocio[i].nombre_campo CLIPPED, CAMPO_NO_CATALOGADO)
         RETURN v_ind_error
      END IF
   END FOR
   RETURN v_ind_error
END FUNCTION

PRIVATE FUNCTION fn_ejecuta_negocio()
   DEFINE v_sp_bus_ejecuta_negocio        VARCHAR(255)
   DEFINE v_consecutivo                   STRING
   DEFINE v_rura_exe                      VARCHAR(100)
   DEFINE v_ruta_log                      VARCHAR(100)

   DEFINE v_tipo_programa                 CHAR(3)
   DEFINE v_programa                      CHAR(40)
   DEFINE v_modulo_programa               CHAR(3)
   DEFINE v_rura_exe_programa             VARCHAR(100)
   DEFINE v_ruta_ejecuta_programa              VARCHAR(255)




   DEFINE v_comando           STRING

   LET v_consecutivo = v_id_bus_tramite

   SELECT ruta_bin, ruta_bitacora 
   INTO v_rura_exe, v_ruta_log
   FROM seg_modulo
   WHERE modulo_cod = 'bus'

   SELECT a.tipo_programa          ,
          a.programa               ,
          f.modulo_cod
     INTO v_tipo_programa          ,
          v_programa               ,
          v_modulo_programa              
     FROM cat_bus_negocio       a  ,
          cat_bus_contrato      b  ,
          bus_solicitud_tramite c  ,
          bus_tramite           d  ,
          cat_bus_operacion     e  ,
          cat_bus_proceso       f
    WHERE a.id_cat_bus_negocio       = b.id_cat_bus_negocio
      AND b.id_cat_bus_contrato      = v_id_cat_bus_contrato      -- id del contrato entrante
      AND c.id_bus_solicitud_tramite = v_id_bus_solicitud_tramite -- id de la solicitud entrante
      AND c.id_bus_tramite           = d.id_bus_tramite
      AND d.id_cat_bus_contrato      = b.id_cat_bus_contrato
      AND b.id_cat_bus_operacion     = e.id_cat_bus_operacion
      AND e.id_cat_bus_proceso       = f.id_cat_bus_proceso

   SELECT ruta_bin
   INTO v_rura_exe_programa
   FROM seg_modulo
   WHERE modulo_cod = v_modulo_programa
 
   IF v_tipo_programa = "PR" THEN
       LET v_ruta_ejecuta_programa = " ","\'",v_rura_exe_programa CLIPPED, "/" , v_programa CLIPPED," ",v_id_bus_solicitud_tramite CLIPPED," ",v_folio_procesar CLIPPED, "\'"

       LET v_sp_bus_ejecuta_negocio = "EXECUTE PROCEDURE sp_bus_ejecuta_negocio_pr(",
                                          v_id_bus_tramite, ", ",
                                      "\"",v_folio_procesar CLIPPED, "\", ",
                                      v_id_bus_solicitud_tramite, ", ",
                                      v_id_cat_bus_contrato, "); "
       LET v_comando = "cd ", v_rura_exe CLIPPED, ";nohup ./ejecuta_negocio_pr.sh ","'", v_sp_bus_ejecuta_negocio CLIPPED, "'", v_ruta_ejecuta_programa CLIPPED, " 1> ", v_ruta_log CLIPPED, "/bus_solicitud_", v_consecutivo CLIPPED, ".log 2>&1 &"
   ELSE 

       LET v_sp_bus_ejecuta_negocio = "EXECUTE PROCEDURE sp_bus_ejecuta_negocio(",
                                          v_id_bus_tramite, ", ",
                                      "\"",v_folio_procesar CLIPPED, "\", ",
                                      v_id_bus_solicitud_tramite, ", ",
                                      v_id_cat_bus_contrato, "); "
       
       LET v_comando = "cd ", v_rura_exe CLIPPED, ";nohup ./ejecuta_negocio.sh ","'", v_sp_bus_ejecuta_negocio CLIPPED, "' 1> ", v_ruta_log CLIPPED, "/bus_solicitud_", v_consecutivo CLIPPED, ".log 2>&1 &"
   END IF

   RUN v_comando

   CALL ERRORLOG("La petición se recibió correctamente, el folioProcesar " || v_folio_procesar CLIPPED || " se envió negocio")
   
END FUNCTION 

PRIVATE FUNCTION fn_genera_salida()
   DEFINE i                               SMALLINT
   DEFINE v_id_bus_respuesta_tramite      DECIMAL(9,0)

   LET ns2notificarTramiteResponse.ssnrop.codoper = v_r_cod_oper_cliente
   LET ns2notificarTramiteResponse.ssnrop.codRespuesta = v_r_cod_respuesta
   LET ns2notificarTramiteResponse.ssnrop.codRespuestaOpr = v_r_cod_respuesta_opr
   LET ns2notificarTramiteResponse.ssnrop.descRespuesta = v_r_desc_respuesta
   LET ns2notificarTramiteResponse.objetoRespuesta.folioAck = v_r_folio_Ack CLIPPED

   IF v_id_bus_tramite IS NOT NULL AND v_id_bus_tramite > 0 THEN
      EXECUTE exe_fn_inserta_respuesta_tramite USING  v_id_bus_tramite,
                                                      v_id_bus_solicitud_tramite,
                                                      v_r_cod_respuesta,
                                                      v_r_cod_respuesta_opr,
                                                      v_r_desc_respuesta,
                                                      v_r_cod_oper_cliente,
                                                      v_r_folio_Ack
                                                INTO  v_id_bus_respuesta_tramite
   END IF
   

   FOR i = 1 TO v_r_rechazos_respuesta.getLength()
      IF v_id_bus_respuesta_tramite IS NOT NULL AND v_r_rechazos_respuesta[i].cod_rechazo IS NOT NULL THEN
         CALL ns2notificarTramiteResponse.ssnrop.motivos.motivo.appendElement()
         LET ns2notificarTramiteResponse.ssnrop.motivos.motivo[i].idMotivo = v_r_rechazos_respuesta[i].cod_rechazo
         LET ns2notificarTramiteResponse.ssnrop.motivos.motivo[i].descripcion = v_r_rechazos_respuesta[i].desc_rechazo
         EXECUTE exe_inserta_error_respuesta USING v_id_bus_respuesta_tramite, 
                                                   v_r_rechazos_respuesta[i].cod_rechazo, 
                                                   v_r_rechazos_respuesta[i].desc_rechazo
      END IF
   END FOR

   LET ns2notificarTramiteResponse.ssnrop.fecha = CURRENT YEAR TO FRACTION(3)
   LET v_hora_fin = CURRENT FRACTION TO FRACTION(3)
   LET ns2notificarTramiteResponse.ssnrop.tiempoRespuesta = (v_hora_fin - v_hora_inicio), " ms"  --VALIDAR
     
END FUNCTION

PRIVATE FUNCTION fn_valida_campo(p_id_detalle_contrato, p_campo, p_valor, p_valor_xml, p_tipo)
   DEFINE p_id_detalle_contrato  DECIMAL(9,0)
   DEFINE p_campo                VARCHAR(60)
   DEFINE p_valor                VARCHAR(80)
   DEFINE p_valor_xml            STRING
   DEFINE v_valor_s              STRING
   DEFINE p_tipo                 CHAR(1)
   DEFINE v_caracter             CHAR(1)

   DEFINE v_tmp_fecha            DATE
   DEFINE i                      SMALLINT
   DEFINE v_resultado            INTEGER
   DEFINE v_ind_punto            SMALLINT

   LET v_ind_punto = 0
   LET v_valor_s = p_valor

   IF p_campo == CAMPO_NSS_IMSS THEN
      EXECUTE exe_fn_valida_nss USING p_valor
                                 INTO v_resultado
      RETURN v_resultado
   ELSE
      IF p_tipo == CAMPO_FECHA THEN
         TRY
            LET v_tmp_fecha = MDY(v_valor_s.subString(5,6),v_valor_s.subString(7,8),v_valor_s.subString(1,4))
            
            IF v_tmp_fecha IS NULL THEN
               RETURN 1
            END IF
         CATCH
            RETURN 1    #Fecha no valida
         END TRY
      ELSE
         IF p_tipo = CAMPO_FECHA_2 THEN
            TRY
               LET v_tmp_fecha = MDY(v_valor_s.subString(6,7),v_valor_s.subString(9,10),v_valor_s.subString(1,4))
               IF v_tmp_fecha IS NULL THEN
                  RETURN 1
               END IF
               IF v_valor_s.getCharAt(5) <> '/' OR v_valor_s.getCharAt(8) <> '/' THEN
                  RETURN 1
               END IF
            CATCH
               RETURN 1    #Fecha no valida
            END TRY
         ELSE
            IF p_tipo = CAMPO_ARREGLO THEN
               CALL fn_valida_arreglo(p_id_detalle_contrato, p_valor_xml) RETURNING v_resultado
               RETURN v_resultado
            ELSE
               FOR i= 1 TO v_valor_s.getLength()
                  LET v_caracter = v_valor_s.getCharAt(i)
                  CASE p_tipo
                     WHEN CAMPO_NUMERICO
                        EXECUTE exe_fn_valida_numero USING v_caracter INTO v_resultado
                        IF v_resultado = 0 THEN
                           RETURN 1
                        END IF
                     
                     WHEN CAMPO_ALFANUMERICO
                        EXECUTE exe_fn_valida_caracter_especial USING v_caracter INTO v_resultado
                        IF v_resultado = 1 AND v_caracter <> '$' THEN
                           RETURN 1
                        END IF 
               
                     WHEN CAMPO_ALFABETICO
                        IF v_caracter <> ' ' THEN
                           EXECUTE exe_fn_valida_letra USING v_caracter INTO v_resultado
                           IF v_resultado = 0 THEN
                              RETURN 1
                           END IF
                        END IF
               
                     WHEN CAMPO_DECIMAL
                        IF v_caracter == '.' THEN
                           #Para los campos decimales
                           IF v_ind_punto = 1 THEN
                              RETURN 1 #Significa que el valor tiene dos puntos decimales
                           END IF
                           LET v_ind_punto = 1
                        ELSE
                           EXECUTE exe_fn_valida_numero USING v_caracter INTO v_resultado
                           IF v_resultado = 0 THEN
                              RETURN 1
                           END IF
                        END IF
                        
                     OTHERWISE
                        RETURN 1
                  
                  END CASE
               END FOR
            END IF
         END IF
      END IF
   END IF
   RETURN 0
END FUNCTION

PRIVATE FUNCTION fn_inserta_detalle_arreglo(p_id_bus_solicitud_detalle, p_valor_xml)
   DEFINE p_id_bus_solicitud_detalle  DECIMAL(9,0)
   DEFINE p_valor_xml               STRING

   DEFINE v_num_elementos           INTEGER
   DEFINE v_num_bloque              SMALLINT
   DEFINE v_ind_error_arreglo       SMALLINT
   DEFINE i                         INTEGER
   DEFINE y                         INTEGER
   DEFINE v_num_item                INTEGER
   DEFINE v_count_nodo              INTEGER
   DEFINE v_num_nodo                INTEGER

   DEFINE v_campo                   VARCHAR(60)
   DEFINE v_valor                   VARCHAR(100)

   DEFINE v_doc                     xml.DomDocument
   DEFINE v_nodo_padre              xml.DomNode   
   DEFINE v_item                    xml.DomNode
   DEFINE v_nodo_xml                xml.DomNode

   LET v_ind_error_arreglo = 0
   LET v_num_bloque = 1
   
   TRY
      #Se crea el XML con el valor para identificar si es un XML valido
      LET v_doc = xml.DomDocument.Create()
      CALL v_doc.loadFromString(p_valor_xml)
      CALL v_doc.normalize()
      #Se obtiene el elemento padre y se valida que en el XML solo se tenga un elemento raiz
      CALL v_doc.getDocumentNodesCount() RETURNING v_num_elementos
      IF v_num_elementos IS NULL OR v_num_elementos <> 1 THEN
         LET v_ind_error_arreglo = 1
      ELSE
         LET v_nodo_padre = v_doc.getDocumentNodeItem(1)
         LET v_valor = v_nodo_padre.getNodeName() CLIPPED
         EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, NOMBRE_ARREGLO, v_valor
         LET v_num_elementos = v_nodo_padre.getChildrenCount()
         IF v_num_elementos IS NULL OR v_num_elementos < 1 THEN
            LET v_ind_error_arreglo = 1
         ELSE
            #Obtenemos todos los elementos hijos dentro
            LET v_num_item = 1
            FOR i = 1 TO v_num_elementos
               LET v_item = v_nodo_padre.getChildNodeItem(v_num_item)
               IF v_item IS NULL OR v_item.getLocalName() = "#text" THEN
                  #En este caso el elemento se considera basura ya que corresponde a un caracter especial o a un salto de linea
                  CALL v_nodo_padre.removeChild(v_item)
                  LET v_num_item = v_num_item - 1
               ELSE
                  #En ete caso se trata de un elemento valido por lo que se guarda en la bitacora
                  LET v_count_nodo = v_item.getChildrenCount()
                  IF v_count_nodo IS NULL OR v_count_nodo < 1 THEN
                     LET v_ind_error_arreglo = 1
                     EXIT FOR
                  ELSE
                     IF v_num_bloque = 1 THEN
                        LET v_valor = v_item.getNodeName() CLIPPED
                        EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, NOMBRE_ELEMENTO, v_valor
                     END IF
                     LET v_num_bloque = v_num_bloque +1
                     LET v_num_nodo = 1
                     FOR y = 1 TO v_count_nodo
                        LET v_nodo_xml = v_item.getChildNodeItem(v_num_nodo)
                        IF v_nodo_xml IS NULL OR v_nodo_xml.getLocalName() = "#text" THEN
                           #En este caso el elemento se considera basura ya que corresponde a un caracter especial o a un salto de linea
                           CALL v_item.removeChild(v_nodo_xml)
                           LET v_num_nodo = v_num_nodo - 1
                        ELSE
                           #Nodo valido, se guarda en la bitacora
                           LET v_campo = v_nodo_xml.getNodeName() CLIPPED
                           CALL v_nodo_xml.getFirstChild() RETURNING v_nodo_xml
                           LET v_valor = v_nodo_xml.toString() CLIPPED
                           EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, v_campo, v_valor
                        END IF
                        LET v_num_nodo = v_num_nodo + 1
                     END FOR
                  END IF
               END IF
               LET v_num_item = v_num_item + 1
            END FOR
         END IF
         LET p_valor_xml = v_nodo_padre.toString()
      END IF
   CATCH
      LET v_ind_error_arreglo = 1
      LET v_num_bloque = 1
      LET v_campo = "ERROR"
      LET v_valor = "XML invalido"
      DELETE FROM bus_detalle_bloque WHERE id_bus_solicitud_detalle = p_id_bus_solicitud_detalle
      EXECUTE exe_inserta_detalle_bloque USING p_id_bus_solicitud_detalle, v_num_bloque, v_campo, v_valor

      LET v_r_cod_respuesta = COD_ERROR
      LET v_r_cod_respuesta_opr = COD_OPR_ERROR

      CALL v_r_rechazos_respuesta.appendElement()
      LET v_r_rechazos_respuesta[1].cod_rechazo = RECHAZO_GENERICO_ARREGLO CLIPPED
      LET v_r_rechazos_respuesta[1].desc_rechazo = DES_XML_MAL_FORMADO_ARREGLO CLIPPED
   END TRY

   IF v_ind_error_arreglo = 1 THEN
      LET p_valor_xml = NULL
   END IF
   RETURN v_ind_error_arreglo, p_valor_xml
END FUNCTION

#Funcion que valida el arreglo sin considerar el orden
PRIVATE FUNCTION fn_valida_arreglo(p_id_detalle_contrato, p_valor_xml)
   DEFINE p_id_detalle_contrato  DECIMAL(9,0)
   DEFINE p_valor_xml            STRING
   DEFINE v_resutado             INTEGER

   DEFINE v_nombre_lista         VARCHAR(60)
   DEFINE v_nombre_item          VARCHAR(60)
   DEFINE v_id_cat_bus_bloque    DECIMAL(9,0)
   DEFINE v_contrato_bloque      DYNAMIC ARRAY OF contrato_bloque

   DEFINE i                      INTEGER
   DEFINE v_num_campo            INTEGER
   DEFINE v_num_campo_bloque     INTEGER
   DEFINE v_campo_bloque         STRING
   DEFINE v_valor_bloque         STRING
   DEFINE v_ind_valido           SMALLINT

   DEFINE doc                    xml.DomDocument
   DEFINE lista                  xml.DomNodeList
   DEFINE item                   xml.DomNode
   DEFINE nodo_xml               xml.DomNode
   
   #Primero se busca la definicion del XML
   EXECUTE exe_consulta_bloque USING p_id_detalle_contrato INTO v_nombre_lista, v_nombre_item, v_id_cat_bus_bloque
   IF v_id_cat_bus_bloque IS NOT NULL THEN
      LET v_resutado = 0
      LET v_nombre_item = v_nombre_item CLIPPED
      LET v_nombre_lista = v_nombre_lista CLIPPED
      TRY
         #Se crea el XML con el valor para identificar si es un XML valido
         LET doc = xml.DomDocument.Create()
         CALL doc.loadFromString(p_valor_xml)
         #Se obtiene el primer nivel del XML
         CALL doc.getElementsByTagName(v_nombre_lista) RETURNING lista
         IF lista.getCount() <> 1 THEN
            #Si el primer nivel del XML no regresa un elemento se considera como un campo invalido
            LET v_resutado = 1
         ELSE
            #Se obtiene el segundo nivel del XML
            CALL lista.getItem(1) RETURNING item
            CALL item.getElementsByTagName(v_nombre_item) RETURNING lista
            IF lista.getCount() < 1 THEN
               #Si el segundo nivel no contiene almenos un elementos de considera un campo invalido
               LET v_resutado = 1
            ELSE
               #Se consulta el contrato del arreglo
               #Buscamos el mapa de valores configurados para el tramite
               DECLARE cur_consulta_detalle_bloque CURSOR FOR exe_consulta_detalle_bloque
               INITIALIZE v_contrato_bloque TO NULL
               LET v_num_campo = 1
               FOREACH cur_consulta_detalle_bloque USING v_id_cat_bus_bloque INTO v_contrato_bloque[v_num_campo].*
                  LET v_num_campo = v_num_campo + 1
               END FOREACH
               IF v_contrato_bloque[v_contrato_bloque.getLength()].id_cat_bus_detalle_bloque IS NULL THEN
                  CALL v_contrato_bloque.deleteElement(v_contrato_bloque.getLength())
               END IF
               FOR i= 1 TO lista.getCount()  #Lista de bloques
                  IF v_resutado = 1 THEN
                     EXIT FOR
                  END IF
                  #Obtenemos el bloque de datos del arreglo
                  CALL lista.getItem(i) RETURNING item
                  
                  #Se extraen los campos del request
                  INITIALIZE v_parametros_bloque TO NULL
                  FOR v_num_campo_bloque = 1 TO item.getChildrenCount()
                     CALL v_parametros_bloque.appendElement()
                     LET v_parametros_bloque[v_num_campo_bloque].num_campo = v_num_campo_bloque
                     LET v_parametros_bloque[v_num_campo_bloque].ind_existe = 0
                     
                     CALL item.getChildNodeItem(v_num_campo_bloque) RETURNING nodo_xml
                     LET v_parametros_bloque[v_num_campo_bloque].nombre_campo = nodo_xml.getNodeName() CLIPPED
                     
                     CALL nodo_xml.getFirstChild() RETURNING nodo_xml
                     LET v_parametros_bloque[v_num_campo_bloque].valor = nodo_xml.toString() CLIPPED
                  END FOR
                     
                  #Barremos el contrato para validar los campos
                  FOR v_num_campo = 1 TO v_contrato_bloque.getLength()     #Lista con todos los campos del contrato
                     LET v_ind_valido = 0

                     #Se busca en el arreglo del request el campo del contrato
                     FOR v_num_campo_bloque = 1 TO v_parametros_bloque.getLength()       #Lista con los campos del bloque
                        LET v_campo_bloque = v_parametros_bloque[v_num_campo_bloque].nombre_campo CLIPPED
                        LET v_valor_bloque = v_parametros_bloque[v_num_campo_bloque].valor CLIPPED
                        
                        IF v_contrato_bloque[v_num_campo].cve_natural_bloque CLIPPED = v_campo_bloque CLIPPED THEN
                           LET v_ind_valido = 1
                           IF v_parametros_bloque[v_num_campo_bloque].ind_existe = 1 THEN
                              #Campo duplicado en el request
                              LET v_r_cod_respuesta = COD_ERROR
                              LET v_r_cod_respuesta_opr = COD_OPR_ERROR
                              LET v_r_desc_respuesta = DESC_ERROR_VALIDACION

                              CALL v_r_rechazos_respuesta.appendElement()
                              LET v_r_rechazos_respuesta[1].cod_rechazo = RECHAZO_GENERICO
                              LET v_r_rechazos_respuesta[1].desc_rechazo = DES_RECHAZO_GENERICO_DUP, v_campo_bloque CLIPPED
                              LET v_resutado = 1
                              RETURN v_resutado
                           ELSE
                              IF v_valor_bloque IS NOT NULL AND LENGTH(v_valor_bloque CLIPPED) > 0 THEN
                                 #El campo es correcto, ahora se valida el tipo de dato
                                 CALL fn_valida_campo(v_contrato_bloque[v_num_campo].id_cat_bus_detalle_bloque, 
                                                      v_campo_bloque, 
                                                      v_valor_bloque,
                                                      NULL,
                                                      v_contrato_bloque[v_num_campo].tipo_dato) RETURNING v_resutado
                                 IF v_resutado <> 0 THEN
                                    CALL fn_genera_error_estructura_arreglo(v_contrato_bloque[v_num_campo].cve_natural_bloque, CAMPO_INVALIDO)
                                    RETURN v_resutado
                                 END IF
                              ELSE
                                 #Se verifica si el campo esta configurado como opcional
                                 IF v_contrato_bloque[v_num_campo].ind_opcional IS NULL OR v_contrato_bloque[v_num_campo].ind_opcional <> 1 THEN
                                    LET v_resutado = 1
                                    CALL fn_genera_error_estructura_arreglo(v_contrato_bloque[v_num_campo].cve_natural_bloque, CAMPO_NO_EXISTE)
                                    RETURN v_resutado
                                 END IF
                              END IF
                              LET v_parametros_bloque[v_num_campo_bloque].ind_existe = 1
                              EXIT FOR
                           END IF 
                        END IF
                     END FOR     #FIN de la Lista con los campos del bloque
                     IF v_ind_valido <> 1 THEN
                        LET v_resutado = 1
                        CALL fn_genera_error_estructura_arreglo(v_contrato_bloque[v_num_campo].cve_natural_bloque CLIPPED, CAMPO_NO_EXISTE)
                        RETURN v_resutado
                     END IF
                  END FOR  #FIN de la Lista con todos los campos del contrato
                  
                  #Se valida que todos los campos en el request sean parte del contrato
                  FOR v_num_campo_bloque = 1 TO v_parametros_bloque.getLength()
                     IF v_parametros_bloque[v_num_campo_bloque].ind_existe <> 1 THEN
                        LET v_resutado = 1
                        CALL fn_genera_error_estructura_arreglo(v_parametros_bloque[v_num_campo_bloque].nombre_campo CLIPPED, CAMPO_NO_CATALOGADO)
                        RETURN v_resutado
                     END IF
                  END FOR
               END FOR     #FIN de la Lista de bloques
            END IF
         END IF
      CATCH
         LET v_resutado = 1
      END TRY
   ELSE
      LET v_resutado = 1
   END IF
   RETURN v_resutado
END FUNCTION

PRIVATE FUNCTION fn_genera_error_estructura(p_campo, p_tipo_error)
   DEFINE p_campo                VARCHAR(60)
   DEFINE p_tipo_error           SMALLINT

   DEFINE v_cod_rechazo          VARCHAR(10)
   DEFINE v_desc_rechazo         VARCHAR(100)

   LET v_r_cod_respuesta = COD_ERROR
   LET v_r_cod_respuesta_opr = COD_OPR_ERROR
   LET v_r_desc_respuesta = DESC_ERROR_VALIDACION
   EXECUTE exe_consulta_rechazo USING p_campo,
                                      p_tipo_error
                                INTO  v_cod_rechazo,
                                      v_desc_rechazo

   CALL v_r_rechazos_respuesta.appendElement()
   IF v_cod_rechazo IS NOT NULL THEN
      LET v_r_rechazos_respuesta[1].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo CLIPPED
   ELSE
      IF p_tipo_error == CAMPO_INVALIDO THEN
         LET v_desc_rechazo = DES_RECHAZO_GENERICO, p_campo
      ELSE
         IF p_tipo_error == CAMPO_NO_CATALOGADO THEN
            LET v_desc_rechazo = DES_RECHAZO_GENERICO_NO_CATALOGADO, p_campo
         ELSE
            LET v_desc_rechazo = DES_RECHAZO_GENERICO_NULL, p_campo
         END IF
      END IF
      LET v_r_rechazos_respuesta[1].cod_rechazo = RECHAZO_GENERICO CLIPPED
      LET v_r_rechazos_respuesta[1].desc_rechazo = v_desc_rechazo CLIPPED
   END IF
   
      
END FUNCTION

PRIVATE FUNCTION fn_genera_error_estructura_arreglo(p_campo, p_tipo_error)
   DEFINE p_campo                VARCHAR(60)
   DEFINE p_tipo_error           SMALLINT

   DEFINE v_cod_rechazo          VARCHAR(10)
   DEFINE v_desc_rechazo         VARCHAR(100)

   LET v_r_cod_respuesta = COD_ERROR
   LET v_r_cod_respuesta_opr = COD_OPR_ERROR
   LET v_r_desc_respuesta = DESC_ERROR_VALIDACION
   EXECUTE exe_consulta_rechazo USING p_campo,
                                      p_tipo_error
                                INTO  v_cod_rechazo,
                                      v_desc_rechazo

   CALL v_r_rechazos_respuesta.appendElement()
   IF v_cod_rechazo IS NOT NULL THEN
      LET v_r_rechazos_respuesta[2].cod_rechazo = v_cod_rechazo
      LET v_r_rechazos_respuesta[2].desc_rechazo = v_desc_rechazo CLIPPED
   ELSE
      IF p_tipo_error == CAMPO_INVALIDO THEN
         LET v_desc_rechazo = DES_RECHAZO_GENERICO, p_campo
      ELSE
         IF p_tipo_error == CAMPO_NO_CATALOGADO THEN
            LET v_desc_rechazo = DES_RECHAZO_GENERICO_NO_CATALOGADO, p_campo
         ELSE
            LET v_desc_rechazo = DES_RECHAZO_GENERICO_NULL, p_campo
         END IF
      END IF
      LET v_r_rechazos_respuesta[2].cod_rechazo = RECHAZO_GENERICO_ARREGLO CLIPPED
      LET v_r_rechazos_respuesta[2].desc_rechazo = v_desc_rechazo CLIPPED
   END IF
   
END FUNCTION
