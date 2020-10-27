--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/09/2015
--==============================================================================

################################################################################
#Modulo       =>                                                               #
#Programa     => monitor                                                       #
#Objetivo     => Monitorea estado de servicios web catalogados en cat_servicio #
#Fecha inicio => 21 Septiembre 2015                                            #
################################################################################
IMPORT com
IMPORT xml

SCHEMA safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_titulo_ventana  STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_estructura DYNAMIC ARRAY OF RECORD         
         v_nodo      VARCHAR(160),
         v_extendido BOOLEAN,
         v_id_padre  VARCHAR(160),
         v_id_nodo   VARCHAR(160)
       END RECORD

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)   

   TRY
      CONNECT TO "safre_viv"
      CALL fn_inicializa_consultas()
      CALL fn_consulta_servicios()
      DISCONNECT "safre_viv"
   CATCH
      DISCONNECT "safre_viv"
   END TRY
   
END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'prt'

   LET v_consulta = " SELECT FIRST 100 pid,",
                    "        f_registro,",
                    --"        CASE registra_obj WHEN 1 THEN 'SI' WHEN 0 THEN 'NO' END CASE,",
                    "        registra_obj,",
                    "        ip_externa,",
                    "        metodo_http,",
                    "        objeto_peticion",
                    "   FROM bat_ctr_servicio",
                    "  WHERE servicio_cod = ?",
                    "  ORDER BY pid DESC"
   PREPARE prp_recupera_bitacora FROM v_consulta

   LET v_consulta = " SELECT objeto_peticion",
                    "   FROM bat_ctr_servicio",
                    "  WHERE pid = ?"
   PREPARE prp_recupera_obj FROM v_consulta

   LET v_consulta = " SELECT modulo_cod,",
                    "        modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE 1 = 1",
                    "  ORDER BY modulo_cod"
   PREPARE prp_recupera_modulos FROM v_consulta

   LET v_consulta = " SELECT servicio_cod,",
                    "        servicio_desc",
                    "   FROM cat_servicio",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_recupera_servicios_filtro FROM v_consulta

   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 15 )
END FUNCTION

# Descripción: Muestra y administra pantalla de monitor de servicios
FUNCTION fn_consulta_servicios()
DEFINE v_servicios DYNAMIC ARRAY OF RECORD
          v_id_servicio   LIKE cat_servicio.id_servicio,
          v_imagen        STRING,
          v_servicio_cod  LIKE cat_servicio.servicio_cod,
          v_servicio_desc LIKE cat_servicio.servicio_desc,
          v_modulo_desc   LIKE seg_modulo.modulo_desc,
          v_estado        STRING,
          v_wsdl          STRING
       END RECORD,
       v_bitacora DYNAMIC ARRAY OF RECORD
          v_pid             LIKE bat_ctr_servicio.pid,
          v_f_registro      LIKE bat_ctr_servicio.f_registro,
          v_registro_objeto LIKE bat_ctr_servicio.registra_obj,
          v_ip_externa      LIKE bat_ctr_servicio.ip_externa,
          v_metodo_http     LIKE bat_ctr_servicio.metodo_http
       END RECORD,
       r_filtros RECORD
         v_modulo_cod   LIKE cat_servicio.modulo_cod,
         v_servicio_cod LIKE cat_servicio.servicio_cod,
         --v_estado       CHAR(1)
         v_activo   BOOLEAN,
         v_inactivo BOOLEAN
       END RECORD,
       v_objeto_peticion LIKE bat_ctr_servicio.objeto_peticion,
       r_consulta  BOOLEAN

   OPEN WINDOW vtna_consulta_servicios WITH FORM v_ruta_ejecutable CLIPPED || "/monitor01"

      DIALOG ATTRIBUTE (UNBUFFERED)

         # Servicios
         DISPLAY ARRAY v_servicios TO sr_servicios.*

            BEFORE ROW
               CALL v_bitacora.clear()
               DISPLAY "" TO objeto_peticion
               CALL v_estructura.clear()
               # Recupera Bitácora del servio
               CALL fn_recupera_bitacora_ws(v_servicios[ARR_CURR()].v_servicio_cod) RETURNING v_bitacora

         END DISPLAY

         #Bitácora
         DISPLAY ARRAY v_bitacora TO sr_bitacora.*
         
            BEFORE ROW 
               DISPLAY "" TO objeto_peticion
               CALL v_estructura.clear()
               # Recupera trama XML si fué registrada
               IF( v_bitacora[ARR_CURR()].v_registro_objeto == 1 )THEN
                  CALL fn_recupera_obj_peticion(v_bitacora[ARR_CURR()].v_pid) RETURNING v_objeto_peticion
                  DISPLAY v_objeto_peticion TO objeto_peticion
                  CALL fn_genera_manejador_estructura(v_objeto_peticion) --RETURNING r_estructura
               END IF

         END DISPLAY

         # Estructura XML 
         DISPLAY ARRAY v_estructura TO sr_datos.*

         END DISPLAY

         BEFORE DIALOG
            LOCATE v_objeto_peticion IN MEMORY
            IF(p_titulo_ventana IS NOT NULL)THEN
               CALL ui.Interface.setText(p_titulo_ventana)
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo_ventana)
               LET v_forma = v_ventana.getForm()
            END IF
            LET r_filtros.v_activo   = TRUE
            LET r_filtros.v_inactivo = FALSE
            CALL fn_recupera_servicios(r_filtros.*) RETURNING v_servicios

         ON ACTION actualizar
            # Recupera Servicios
            CALL fn_recupera_servicios(r_filtros.*) RETURNING v_servicios

         ON ACTION filtrar
            CALL fn_filtra_servicios(r_filtros.*) RETURNING r_filtros.*,r_consulta
            IF( r_consulta )THEN
               CALL v_bitacora.clear()
               CALL v_estructura.clear()
               DISPLAY "" TO objeto_peticion
               CALL fn_recupera_servicios(r_filtros.*) RETURNING v_servicios
               IF( v_servicios.getLength() = 0 )THEN
                  CALL fn_mensaje(p_titulo_ventana,"No se encontraron registros para el criterio dado","information")
               END IF
            END IF
         
         ON ACTION cancelar
            FREE v_objeto_peticion
            EXIT DIALOG

      END DIALOG

   CLOSE WINDOW vtna_consulta_servicios

END FUNCTION

# Descripción: Recupera los servicios registrados, según el filtro
FUNCTION fn_recupera_servicios(p_filtros)
DEFINE p_filtros RECORD
         v_modulo_cod   LIKE cat_servicio.modulo_cod,
         v_servicio_cod LIKE cat_servicio.servicio_cod,
         --v_estado       CHAR(1)
         v_activo   BOOLEAN,
         v_inactivo BOOLEAN
       END RECORD,
       v_servicios DYNAMIC ARRAY OF RECORD
          v_id_servicio   LIKE cat_servicio.id_servicio,
          v_imagen        STRING,
          v_servicio_cod  LIKE cat_servicio.servicio_cod,
          v_servicio_desc LIKE cat_servicio.servicio_desc,
          v_modulo_desc   LIKE seg_modulo.modulo_desc,
          v_estado        STRING,
          v_wsdl          STRING
       END RECORD,
       v_servicio RECORD 
          v_id_servicio   LIKE cat_servicio.id_servicio,
          v_servicio_cod  LIKE cat_servicio.servicio_cod,
          v_servicio_desc LIKE cat_servicio.servicio_desc,
          v_modulo_desc   LIKE seg_modulo.modulo_desc,
          v_url           LIKE cat_servicio.url
       END RECORD,
       v_consulta STRING,
       v_filtro   STRING,
       v_indice   INTEGER,
       r_servicio_activo BOOLEAN,
       r_wsdl            STRING

   LET v_filtro = "1 = 1"
   LET v_consulta = " SELECT cat.id_servicio,",
                    "        cat.servicio_cod,",
                    "        cat.servicio_desc,",
                    "        seg.modulo_desc,",
                    "        cat.url",
                    "   FROM cat_servicio cat LEFT OUTER JOIN seg_modulo seg",
                    "     ON seg.modulo_cod = cat.modulo_cod",
                    "  WHERE " 
   IF( p_filtros.v_modulo_cod IS NOT NULL AND  p_filtros.v_modulo_cod <> " " )THEN
      LET v_filtro = v_filtro," AND cat.modulo_cod = '",p_filtros.v_modulo_cod,"'"
   END IF
   IF( p_filtros.v_servicio_cod IS NOT NULL AND p_filtros.v_servicio_cod <> 0 )THEN
      LET v_filtro = v_filtro," AND cat.servicio_cod = ",p_filtros.v_servicio_cod
   END IF

   LET v_consulta = v_consulta, v_filtro
   --DISPLAY v_consulta
   PREPARE prp_recupera_servicios FROM v_consulta
   
   LET v_indice = 1
   DECLARE cur_recupera_servicios CURSOR FOR prp_recupera_servicios
   FOREACH cur_recupera_servicios INTO v_servicio.*
      # Realiza petición HTTP para consultar estado del WS
      CALL fn_consulta_estado_ws(v_servicio.v_url) RETURNING r_servicio_activo,
                                                             r_wsdl
      --CASE p_filtros.v_estado # Estado del servicio
      CASE
         WHEN p_filtros.v_inactivo = TRUE AND p_filtros.v_activo = FALSE # Inactivo
            IF NOT( r_servicio_activo )THEN
               LET v_servicios[v_indice].v_id_servicio   = v_servicio.v_id_servicio
               LET v_servicios[v_indice].v_servicio_cod  = v_servicio.v_servicio_cod
               LET v_servicios[v_indice].v_servicio_desc = v_servicio.v_servicio_desc
               LET v_servicios[v_indice].v_modulo_desc   = v_servicio.v_modulo_desc
               LET v_servicios[v_indice].v_estado = "INACTIVO"
               LET v_servicios[v_indice].v_imagen = "quest"
               LET v_servicios[v_indice].v_wsdl   = NULL
               LET v_indice = v_indice + 1
            END IF
         WHEN p_filtros.v_activo = TRUE AND p_filtros.v_inactivo = FALSE
            IF( r_servicio_activo )THEN
               LET v_servicios[v_indice].v_id_servicio   = v_servicio.v_id_servicio
               LET v_servicios[v_indice].v_servicio_cod  = v_servicio.v_servicio_cod
               LET v_servicios[v_indice].v_servicio_desc = v_servicio.v_servicio_desc
               LET v_servicios[v_indice].v_modulo_desc   = v_servicio.v_modulo_desc
               LET v_servicios[v_indice].v_estado = "ACTIVO"
               LET v_servicios[v_indice].v_imagen = "services"
               LET v_servicios[v_indice].v_wsdl   = NULL
               LET v_indice = v_indice + 1
            END IF

         WHEN p_filtros.v_activo = TRUE AND p_filtros.v_inactivo = TRUE # Todos
            LET v_servicios[v_indice].v_id_servicio   = v_servicio.v_id_servicio
            LET v_servicios[v_indice].v_servicio_cod  = v_servicio.v_servicio_cod
            LET v_servicios[v_indice].v_servicio_desc = v_servicio.v_servicio_desc
            LET v_servicios[v_indice].v_modulo_desc   = v_servicio.v_modulo_desc
            IF( r_servicio_activo )THEN
               LET v_servicios[v_indice].v_estado = "ACTIVO"
               LET v_servicios[v_indice].v_imagen = "services"
               LET v_servicios[v_indice].v_wsdl   = NULL
            ELSE               
               LET v_servicios[v_indice].v_estado = "INACTIVO"
               LET v_servicios[v_indice].v_imagen = "quest"
               LET v_servicios[v_indice].v_wsdl   = NULL
            END IF
            LET v_indice = v_indice + 1         
      END CASE
   END FOREACH 
   FREE cur_recupera_servicios

   RETURN v_servicios
END FUNCTION

# Descripción: Consulta bitácora del WS
FUNCTION fn_recupera_bitacora_ws(p_servicio_cod)
DEFINE p_servicio_cod LIKE cat_servicio.servicio_cod,
       v_bitacora DYNAMIC ARRAY OF RECORD
          v_pid             LIKE bat_ctr_servicio.pid,
          v_f_registro      LIKE bat_ctr_servicio.f_registro,
          v_registro_objeto LIKE bat_ctr_servicio.registra_obj,
          v_ip_externa      LIKE bat_ctr_servicio.ip_externa,
          v_metodo_http     LIKE bat_ctr_servicio.metodo_http          
       END RECORD,
       v_indice INTEGER

   LET v_indice = 1
   DECLARE cur_recupera_bitacora CURSOR FOR prp_recupera_bitacora
   FOREACH cur_recupera_bitacora USING p_servicio_cod
                                  INTO v_bitacora[v_indice].*
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_bitacora
   IF( v_bitacora[v_bitacora.getLength()].v_pid IS NULL)THEN
      CALL v_bitacora.deleteElement(v_bitacora.getLength())
   END IF

   RETURN v_bitacora
END FUNCTION 

# Descripción: Consulta trama XML de la bitácora
FUNCTION fn_recupera_obj_peticion(p_pid)
DEFINE p_pid LIKE bat_ctr_servicio.pid,
       v_objeto_peticion LIKE bat_ctr_servicio.objeto_peticion

   LOCATE v_objeto_peticion IN MEMORY
   EXECUTE prp_recupera_obj USING p_pid
                             INTO v_objeto_peticion

   RETURN v_objeto_peticion
END FUNCTION 

# Descripción: Genera fitltro de consulta de servicios
FUNCTION fn_filtra_servicios(p_filtros)
DEFINE p_filtros RECORD
         v_modulo_cod   LIKE cat_servicio.modulo_cod,
         v_servicio_cod LIKE cat_servicio.servicio_cod,
         --v_estado       CHAR(1)
         v_activo   BOOLEAN,
         v_inactivo BOOLEAN
       END RECORD,
       v_consulta  BOOLEAN,
       v_combo_modulo   ui.ComboBox,
       v_combo_servicio ui.ComboBox

   INPUT p_filtros.v_modulo_cod,
         p_filtros.v_servicio_cod,
         p_filtros.v_activo,
         p_filtros.v_inactivo
    FROM filtro_modulo,
         filtro_servicio,
         --filtro_estado,
         filtro_activo,
         filtro_inactivo ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED, WITHOUT DEFAULTS)

      BEFORE FIELD filtro_modulo
         LET v_consulta = FALSE
         LET v_combo_modulo = ui.ComboBox.forName("formonly.filtro_modulo")
         CALL fn_llena_combo_modulos(v_combo_modulo)

      ON CHANGE filtro_activo
         IF NOT( p_filtros.v_inactivo )THEN
            IF NOT( p_filtros.v_activo )THEN
               LET p_filtros.v_inactivo = TRUE
            END IF
         END IF

      ON CHANGE filtro_inactivo
         IF NOT( p_filtros.v_activo )THEN
            IF NOT( p_filtros.v_inactivo )THEN
               LET p_filtros.v_activo = TRUE
            END IF
         END IF
         
      AFTER FIELD filtro_modulo
         LET v_combo_servicio = ui.ComboBox.forName("formonly.filtro_servicio")
         CALL fn_llena_combo_servicios(v_combo_servicio,
                                       p_filtros.v_modulo_cod)
         
      ON ACTION aceptar
         LET v_consulta = TRUE
         ACCEPT INPUT

      ON ACTION cancelar
         LET v_consulta = FALSE
         --INITIALIZE p_filtros.* TO NULL
         EXIT INPUT

   END INPUT

   RETURN p_filtros.*,
          v_consulta
END FUNCTION

# Descripción: Llena combo de módulos para filtro de servicios
FUNCTION fn_llena_combo_modulos(p_combo_modulo)
DEFINE p_combo_modulo ui.ComboBox,
       v_modulos RECORD
         v_modulo_cod  LIKE seg_modulo.modulo_cod,
         v_modulo_desc LIKE seg_modulo.modulo_desc
       END RECORD

   CALL p_combo_modulo.clear()
   DECLARE cur_recupera_modulos CURSOR FOR prp_recupera_modulos
   FOREACH cur_recupera_modulos INTO v_modulos.v_modulo_cod,
                                     v_modulos.v_modulo_desc
      CALL p_combo_modulo.addItem(v_modulos.v_modulo_cod,
                                  v_modulos.v_modulo_desc)

   END FOREACH

   FREE cur_recupera_modulos
     
END FUNCTION

# Descripción: Llena combo de servicios para el filtro
FUNCTION fn_llena_combo_servicios(p_combo_servicio,p_modulo_cod)
DEFINE p_combo_servicio ui.ComboBox,
       p_modulo_cod     LIKE cat_servicio.modulo_cod,
       v_servicios RECORD
         v_servicio_cod  LIKE cat_servicio.servicio_cod,
         v_servicio_desc LIKE cat_servicio.servicio_desc
       END RECORD

   CALL p_combo_servicio.clear()
   DECLARE cur_recupera_servicios_filtro CURSOR FOR prp_recupera_servicios_filtro
   FOREACH cur_recupera_servicios_filtro USING p_modulo_cod
                                          INTO v_servicios.v_servicio_cod,
                                               v_servicios.v_servicio_desc
      CALL p_combo_servicio.addItem(v_servicios.v_servicio_cod,
                                    v_servicios.v_servicio_desc)

   END FOREACH

   FREE cur_recupera_servicios_filtro
     
END FUNCTION

# Descripción: Genera el manejador para recuperar la estructura de datos de la trama XML
FUNCTION fn_genera_manejador_estructura(p_mensaje)
DEFINE p_mensaje   TEXT,
       v_manejador xml.StaxReader

   TRY
      CALL v_estructura.clear()
      LET  v_manejador = xml.StaxReader.Create()
      CALL  v_manejador.readFromText(p_mensaje)
      CALL fn_genera_estructura_datos(v_manejador,NULL) RETURNING v_manejador
   CATCH
      DISPLAY  "StaxReader ERROR :",STATUS||" ("||SQLCA.SQLERRM||")"    
   END TRY

END FUNCTION 

# Descripción: Genera estructura de datos de la trama XML para presentar en árbol
FUNCTION fn_genera_estructura_datos(p_manejador,p_id_padre)
DEFINE p_manejador xml.StaxReader,
       v_evento    STRING,
       p_id_padre    VARCHAR(160),
       v_id_etiqueta VARCHAR(160),
       v_manejador   xml.StaxReader,
       v_cdata       TEXT
       
   TRY    
      WHILE( TRUE )
         LET  v_evento = p_manejador.getEventType()
         CASE v_evento      
            {WHEN  "START_DOCUMENT"
               DISPLAY  "Inicio de documento"
               DISPLAY  "XML Version  : ",reader.getVersion()
               DISPLAY  "XML Encoding : ",reader.getEncoding()
               IF(  reader.standaloneSet() )THEN
                  IF( reader.isStandalone() )THEN
                     DISPLAY  "Standalone   : yes"
                  ELSE
                     DISPLAY  "Standalone   : no"
                  END IF
               END IF}
            {WHEN  "END_DOCUMENT"
               DISPLAY  "Document reading finished"}
               
            WHEN "START_ELEMENT"
               IF( p_manejador.isEmptyElement() )THEN
                  --DISPLAY  "<"||p_manejador.getName()||"/>"
               ELSE
                  --DISPLAY  "<"||p_manejador.getName()||">"
    
                  LET v_estructura[v_estructura.getLength() + 1].v_id_padre = p_id_padre
                  LET v_estructura[v_estructura.getLength()].v_id_nodo      = p_manejador.getLocalName()
                  LET v_estructura[v_estructura.getLength()].v_nodo         = p_manejador.getLocalName()
                  LET v_estructura[v_estructura.getLength()].v_extendido    = TRUE

                  LET v_id_etiqueta = p_manejador.getLocalName()
                  IF( p_manejador.hasNext() )THEN
                     CALL p_manejador.next()
                     CALL fn_genera_estructura_datos(p_manejador,v_id_etiqueta) RETURNING p_manejador
                  END IF
               END IF
               {FOR  v_indice = 1 TO  v_manejador.getNamespaceCount()
                  DISPLAY  "xmlns:"||v_manejador.getNamespacePrefix(v_indice)||"="||v_manejador.getNamespaceURI(v_indice)
               END FOR}
               {FOR  v_indice = 1 TO v_manejador.getAttributeCount()
                  IF( v_manejador.getAttributePrefix(v_indice))THEN
                     DISPLAY  v_manejador.getAttributePrefix(v_indice)||":"||v_manejador.getAttributeLocalName(v_indice)||"="||v_manejador.getAttributeValue(v_indice)
                  ELSE
                     DISPLAY  v_manejador.getAttributeLocalName(v_indice)||"="||v_manejador.getAttributeValue(v_indice)
                  END IF
               END FOR}
               
            WHEN "END_ELEMENT"
               --DISPLAY  "</"||p_manejador.getName()||">"
               RETURN p_manejador
          
            WHEN "CHARACTERS"
               LET v_estructura[v_estructura.getLength() + 1].v_id_padre = p_id_padre
               LET v_estructura[v_estructura.getLength()].v_id_nodo      = p_manejador.getText()
               LET v_estructura[v_estructura.getLength()].v_nodo         = p_manejador.getText()
               LET v_estructura[v_estructura.getLength()].v_extendido    = TRUE
               {IF( p_manejador.hasText() AND NOT p_manejador.isIgnorableWhitespace() )THEN
                  DISPLAY  "CHARACTERS :",p_manejador.getText()
               END IF}
               
            {WHEN "COMMENT"
               IF( reader.hasText() )THEN
                  DISPLAY "Comment :",reader.getText()
               END IF}
               
            WHEN "CDATA"
               IF(  p_manejador.hasText() )THEN
                  --DISPLAY  "CDATA :", p_manejador.getText()
                  LOCATE v_cdata IN MEMORY
                  # Para los CDATA genera otro manejador y poder agregarlo en el árbol 
                  LET v_cdata = p_manejador.getText()
                  LET  v_manejador = xml.StaxReader.Create()
                  CALL  v_manejador.readFromText(v_cdata)
                  LET v_estructura[v_estructura.getLength() + 1].v_id_padre = p_id_padre
                  LET v_estructura[v_estructura.getLength()].v_id_nodo      = "CDATA"
                  LET v_estructura[v_estructura.getLength()].v_nodo         = "CDATA"
                  LET v_estructura[v_estructura.getLength()].v_extendido    = TRUE
                  CALL fn_genera_estructura_datos(v_manejador,"CDATA") RETURNING p_manejador
                  FREE v_cdata
               END IF
               
            WHEN "PROCESSING_INSTRUCTION"
               DISPLAY  "PI :",p_manejador.getPITarget(),p_manejador.getPIData()
               
            WHEN "ENTITY_REFERENCE"
               DISPLAY  "Entity name :",p_manejador.getName()
               
            OTHERWISE
               --DISPLAY  "Unknown "||event||" node"
               
         END CASE
         
         IF( p_manejador.hasNext() )THEN
            CALL  p_manejador.next()
         ELSE
            CALL  p_manejador.close()
            EXIT WHILE
         END IF
      END WHILE
   CATCH
      DISPLAY "StaxReader ERROR :",STATUS||" ("||SQLCA.SQLERRM||")"    
   END TRY

   RETURN p_manejador
END FUNCTION