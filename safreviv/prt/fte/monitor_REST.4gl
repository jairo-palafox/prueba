--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/10/2015
--==============================================================================

################################################################################
#Modulo       => GLO                                                           #
#Programa     =>                                                               #
#Objetivo     => Servicio de administración de llamadas a servicios            #
#Fecha inicio => 22 Octubre 2015                                               #
################################################################################
IMPORT COM
IMPORT XML

SCHEMA "safre_viv"

PRIVATE CONSTANT C_GENERO_INTERNAL_DELEGATE = "_GENERO_INTERNAL_DELEGATE_"

MAIN
DEFINE v_peticion_ws  com.HttpServiceRequest,
       v_url          STRING,
       p_servicio_cod INTEGER,
       v_conteo_enc   SMALLINT,
       v_indice       SMALLINT,
       p_ip_externa   CHAR(15),
       v_metodo       CHAR(10),
       v_datos_cliente STRING,
       v_trama         STRING
   
   CALL com.WebServiceEngine.Start()

   WHILE TRUE
      TRY
         LET v_peticion_ws = com.WebServiceEngine.GetHttpServiceRequest(-1)
         IF( v_peticion_ws IS NULL )THEN
            EXIT WHILE
         ELSE
            --LET v_url = v_peticion_ws.getUrl()
            --DISPLAY v_url
             
            LET p_servicio_cod = v_peticion_ws.getRequestHeader("X-FourJs-Environment-Parameter-servicio_cod")
            LET p_ip_externa   = v_peticion_ws.getRequestHeader("X-FourJs-Environment-Variable-REMOTE_ADDR")
            # Sólo se registran las solicitudes de negocio
            LET v_metodo = v_peticion_ws.getMethod()
            IF( v_metodo = "POST" )THEN
               # Recupera el encabezado para reedirigirlo al servicio del negocio
               CALL v_peticion_ws.getRequestHeaderCount() RETURNING v_conteo_enc
               LET v_datos_cliente = ""
               FOR v_indice = 1 TO v_conteo_enc
                  # El encabezado Content-Length es calculado internamente
                  IF( v_peticion_ws.getRequestHeaderName(v_indice) <> "Content-Length" )THEN
                     --DISPLAY v_peticion_ws.getRequestHeaderName(v_indice)||" : "||v_peticion_ws.getRequestHeaderValue(v_indice)
                     LET v_datos_cliente = v_datos_cliente,v_peticion_ws.getRequestHeaderName(v_indice)||" : "||v_peticion_ws.getRequestHeaderValue(v_indice),";"
                     CALL v_peticion_ws.setResponseHeader(v_peticion_ws.getRequestHeaderName(v_indice),v_peticion_ws.getRequestHeaderValue(v_indice))
                  END IF
               END FOR
               # Recupera la trama XML para enviarla al servicio del negocio
               LET v_trama = v_peticion_ws.readTextRequest()
               CALL fn_registra_peticion_ws(p_servicio_cod,
                                            p_ip_externa,
                                            v_metodo,
                                            --v_datos_cliente||" "|| # registra inf del cliente en la trama 
                                            v_trama)
               # Indica al GAS se reedirigirá el control al servicio de negocio
               CALL v_peticion_ws.sendTextResponse(307,C_GENERO_INTERNAL_DELEGATE,v_trama)
            ELSE
               # Recupera el encabezado para reedirigirlo al servicio del negocio
               CALL v_peticion_ws.getRequestHeaderCount() RETURNING v_conteo_enc
               LET v_datos_cliente = ""
               FOR v_indice = 1 TO v_conteo_enc
                  # El encabezado Content-Length es calculado internamente
                  IF( v_peticion_ws.getRequestHeaderName(v_indice) <> "Content-Length" )THEN
                     --DISPLAY v_peticion_ws.getRequestHeaderName(v_indice)||" : "||v_peticion_ws.getRequestHeaderValue(v_indice)
                     LET v_datos_cliente = v_datos_cliente,v_peticion_ws.getRequestHeaderName(v_indice)||" : "||v_peticion_ws.getRequestHeaderValue(v_indice),";"
                     CALL v_peticion_ws.setResponseHeader(v_peticion_ws.getRequestHeaderName(v_indice),v_peticion_ws.getRequestHeaderValue(v_indice))
                  END IF
               END FOR
               CALL v_peticion_ws.sendResponse(307,C_GENERO_INTERNAL_DELEGATE)
            END IF
         END IF
      CATCH
         EXIT WHILE 
      END TRY
   END WHILE
END MAIN

FUNCTION fn_registra_peticion_ws(p_servicio_cod,p_ip_externa,p_metodo_http,p_objeto_peticion)
DEFINE p_servicio_cod      INTEGER,
       p_ip_externa        CHAR(15),
       p_metodo_http       CHAR(10),
       p_objeto_peticion   STRING,
       v_objeto_peticion   TEXT,
       v_consulta          STRING,
       v_fecha             DATETIME YEAR TO SECOND,
       v_registra_peticion LIKE cat_servicio.registra_peticion

   TRY      
      CONNECT TO "safre_viv"
      LET v_fecha = CURRENT YEAR TO SECOND
      # Verifica si hay que registrar la trama XML
      LET v_consulta = " SELECT registra_peticion",
                       "   FROM cat_servicio",
                       "  WHERE servicio_cod = ?"
      PREPARE prp_recupera_registra_obj FROM v_consulta
      EXECUTE prp_recupera_registra_obj USING p_servicio_cod
                                         INTO v_registra_peticion
      LOCATE v_objeto_peticion IN MEMORY
      IF NOT( v_registra_peticion )THEN
         LET v_objeto_peticion = ""
      ELSE
         LET v_objeto_peticion = p_objeto_peticion
      END IF
      # Registra en bitácora
      LET v_consulta = " INSERT INTO bat_ctr_servicio", 
                       "(pid,",
                       " servicio_cod,",
                       " f_registro,",
                       " registra_obj,",
                       " ip_externa,",
                       " metodo_http,",
                       " objeto_peticion)",
                       " VALUES(seq_bat_ctr_servicio.NEXTVAL,?,?,?,?,?,?)"
       PREPARE prp_inserta_registro_ws FROM v_consulta
       EXECUTE prp_inserta_registro_ws USING p_servicio_cod,
                                             v_fecha,
                                             v_registra_peticion,
                                             p_ip_externa,
                                             p_metodo_http,
                                             v_objeto_peticion

      FREE v_objeto_peticion
      DISCONNECT "safre_viv" 

   CATCH
      DISPLAY "Ocurrió un error al registrar petición de servicio"
      DISPLAY "Código:   ",SQLCA.sqlcode
      DISPLAY "Mensaje:  ",SQLCA.sqlerrm
      DISPLAY "Servicio: ",p_servicio_cod
      DISPLAY "Fecha:    ",v_fecha
      DISCONNECT "safre_viv" 
   END TRY

END FUNCTION