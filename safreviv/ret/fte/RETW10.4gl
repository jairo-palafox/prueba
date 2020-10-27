--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETW10                                                                 #
#Objetivo     => Servidor de Webservices de consulta de datos de datamart               #
#Fecha inicio => 05 octubre 2012                                                        #
#########################################################################################
IMPORT com
DATABASE safre_viv

GLOBALS "RETG01.4gl"

GLOBALS
-- parametros de entrada
DEFINE gr_consulta_datamart_in RECORD
  nss  VARCHAR(11), -- NSS consultado
  rfc  VARCHAR(13)  -- RFC consultado, que puede contener homoclave
END RECORD

-- parametros de salida
DEFINE gr_consulta_datamart_out RECORD
   brokerok                SMALLINT    , -- booleana TRUE = consulta correcta; FALSE = consulta incorrecta
   nss                     CHAR(11)    , -- nss consultado o de respuesta
   curp                    CHAR(18)    , -- CURP consultado
   nombre                  char(40)    , -- Nombre de la persona (IMSS)
   nombreafore             char(40)    , -- Nombre afore consultado
   paternoafore            char(40)    , -- Apellido paterno afore consultado
   maternoafore            char(40)    , -- Apellido materno afore consultado
   secuencia               smallint    , -- Secuencia de pensión consultada
   tipomovimiento          char(3)     , -- tipo de movimiento
   regimen                 smallint    , -- regimen
   tiposeguro              char(2)     , -- tipo de seguro
   tipopension             char(2)     , -- tipo de pensión
   tipoprestacion          char(2)     , -- tipo de prestación
   articulonegativa        char(3)     , -- articulo negativa de pensión
   fraccionnegativa        char(2)     , -- fracción negativa de pensión
   considerando            char(2)     , -- considerando
   usuario                 CHAR(20)    , -- usuario
   fechainiciopension      date        , -- fecha de inicio de pensión
   fechaemisionresolucion  date        , -- fecha de resolución
   numeroresolucion        CHAR(10)    , -- número de la resolución
   porcentajeevaluacion    decimal(5,2), -- porcentaje de valuación
   semanascotizadas        integer     , -- semanas cotizadas
   diagnosticoregistro     char(3)     , -- diagnostico del retiro
   fechacargadatamart      date        , -- fecha de carga en datamart
   fechaproceso            date        , -- fecha de proceso
   mensaje                 STRING        -- mensaje con resultado de consulta
END RECORD
END GLOBALS

-- Funcion principal del servidor
MAIN
DEFINE v_resultado INTEGER -- resultado de la ejecucion del webservice

   -- se invoca la creacion del servicio
   CALL fn_crea_servicio()
   
   -- se inicia el servidor
   CALL com.WebServiceEngine.Start()
   
   -- se inicia un ciclo infinito para reponder las peticiones
   WHILE ( TRUE )
      LET v_resultado = com.WebServiceEngine.ProcessServices(-1)
           
      CASE v_resultado
         WHEN 0
            DISPLAY "Peticion procesada." 
      
         WHEN -1
            DISPLAY "Se rebasó el tiempo de espera máximo."
      
         WHEN -2
            DISPLAY "Desconectado del servidor de aplicaciones."
            EXIT PROGRAM 
      
         WHEN -3
            DISPLAY "Se perdió la conexión con el cliente."
      
         WHEN -4
            DISPLAY "Se interrumpió la ejecución del servidor con Ctrl-C."
      
         WHEN -10
            DISPLAY "Error interno en el servidor."
      END CASE
      
      IF ( int_flag <> 0 ) THEN
         LET int_flag = 0
         EXIT WHILE
      END IF 
   END WHILE
   
   DISPLAY "Se detuvo el servidor."
  
END MAIN

{
======================================================================
Nombre: fn_crea_servicio
Fecha creacion: Octubre 5, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Genera y publica el servicio de consulta Web

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_crea_servicio()                     
DEFINE v_servicio    com.WebService,    # A WebService
       v_operacion   com.WebOperation  # Operation of a WebService

   -- se crea el servicio
   LET v_servicio = com.WebService.CreateWebService("ConsultaDatamart","http://service.infonavit.org.mx")

   -- se declara la operacion
   LET v_operacion = com.WebOperation.CreateDOCStyle("fn_consulta_datamart", "fn_consulta_datamart", gr_consulta_datamart_in, gr_consulta_datamart_out)

   -- se publica el servicio
   CALL v_servicio.publishOperation(v_operacion,NULL)
   
   -- se registra el sercicio
  CALL com.WebServiceEngine.RegisterService(v_servicio)
   
END FUNCTION
{
======================================================================
Nombre: fn_consulta_datamart
Fecha creacion: Octubre 5, 2012
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Consulta los datos de datamart de un NSS y/o RFC dados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_datamart()
DEFINE  v_sql                 STRING, -- cadena con enunciado SQL
        v_max_sec_pension     SMALLINT, -- maxima secuencia de pension
        v_conteo              SMALLINT, -- contador
        v_r_datamart          RECORD LIKE ret_datamart.*, -- registro de la tabla ret_datamart
        v_r_cza_datamart      RECORD LIKE ret_cza_datamart.*,
        v_ruta_log            STRING,
        v_nombre_archivo_log  STRING,
        v_cadena              STRING,
        v_nss                 VARCHAR(11)
        
        
   -- se crea la ruta del log
   --LET v_ruta_log = "/ds/safreviv/ret/bin/"
   
   LET v_cadena = TODAY USING "ddmmyyyy"
   
   LET v_nombre_archivo_log = "RETW10.", v_cadena
   
   -- si se tiene NSS se agrega el NSS consultado al LOG
   IF ( gr_consulta_datamart_in.nss IS NOT NULL ) THEN
      LET v_nombre_archivo_log = v_nombre_archivo_log, "_", gr_consulta_datamart_in.nss CLIPPED
   END IF
   
   -- si se tiene RFC se agrega el NSS consultado al LOG
   IF ( gr_consulta_datamart_in.rfc IS NOT NULL ) THEN
      LET v_nombre_archivo_log = v_nombre_archivo_log, "_", gr_consulta_datamart_in.rfc CLIPPED
   END IF
   
   -- se agrea la extension log
   LET v_nombre_archivo_log = v_nombre_archivo_log, ".log"
   
   LET v_ruta_log = v_nombre_archivo_log

   DISPLAY "Ruta del archivo LOG: ", v_ruta_log
   
   -- se inicia el log del programa
   CALL STARTLOG( v_ruta_log )
        
   -- se inicia el registro de resultado en NULO
   INITIALIZE gr_consulta_datamart_out.* TO NULL
        
   -- si se recibieron el NSS y el RFC nulos
   IF ( (gr_consulta_datamart_in.nss CLIPPED) IS NULL AND (gr_consulta_datamart_in.rfc CLIPPED) IS NULL ) THEN
      -- se devuelve error. EL NSS no se encuentra en ret_datamart
      -- se transfieren los datos al registro de respuesta
      LET gr_consulta_datamart_out.brokerok = FALSE
      LET gr_consulta_datamart_out.mensaje  = "No se recibió NSS o RFC. No se puede realizar la consulta"
      
      CALL ERRORLOG("No se recibió NSS ni RFC. Se finaliza la consulta.")
      
      -- se finaliza la ejecucion
      RETURN
   END IF
        
   -- se verifica si se tiene NSS
   IF ( gr_consulta_datamart_in.nss IS NOT NULL ) THEN
      IF ( gr_consulta_datamart_in.rfc IS NOT NULL ) THEN
         CALL ERRORLOG("Se recibio NSS y RFC")
         CALL ERRORLOG("Se recibio RFC")
         LET v_cadena = "RFC: ", gr_consulta_datamart_in.rfc
         CALL ERRORLOG(v_cadena)
      END IF
      
      LET v_cadena = "NSS: ", gr_consulta_datamart_in.nss
      CALL ERRORLOG(v_cadena)
      
      CALL ERRORLOG("Se realiza la consulta usando NSS")
      
      -- se obtiene la maxima secuencia de pension del derechohabiente
      SELECT MAX(sec_pension)
      INTO   v_max_sec_pension
      FROM   ret_datamart
      WHERE  nss = gr_consulta_datamart_in.nss
      
      -- si se obtuvo la scuencia de pension, se obtiene el registro
      IF ( v_max_sec_pension IS NOT NULL ) THEN
         CALL ERRORLOG("Se enviará última secuencia de pensión")
         -- se obtiene el registro que tiene la secuencia de pension maxima
         SELECT  *
         INTO    v_r_datamart.*
         FROM    ret_datamart
         WHERE   nss = gr_consulta_datamart_in.nss
         AND     sec_pension = v_max_sec_pension
         
         -- se obtienen los datos del encabezado del folio en cuestion
         SELECT *
         INTO   v_r_cza_datamart.*
         FROM   ret_cza_datamart
         WHERE  folio = v_r_datamart.folio
         
         -- se transfieren los datos al registro de respuesta
         LET gr_consulta_datamart_out.brokerok               = TRUE
         LET gr_consulta_datamart_out.nss                    = gr_consulta_datamart_in.nss
         LET gr_consulta_datamart_out.curp                   = v_r_datamart.curp
         LET gr_consulta_datamart_out.nombre                 = v_r_datamart.nombre_datamart
         LET gr_consulta_datamart_out.nombreafore            = v_r_datamart.nombre_afore
         LET gr_consulta_datamart_out.paternoafore           = v_r_datamart.paterno_afore
         LET gr_consulta_datamart_out.maternoafore           = v_r_datamart.materno_afore
         LET gr_consulta_datamart_out.secuencia              = v_r_datamart.sec_pension
         LET gr_consulta_datamart_out.tipomovimiento         = v_r_datamart.tpo_movimiento
         LET gr_consulta_datamart_out.regimen                = v_r_datamart.regimen
         LET gr_consulta_datamart_out.tiposeguro             = v_r_datamart.tpo_seguro
         LET gr_consulta_datamart_out.tipopension            = v_r_datamart.tpo_pension
         LET gr_consulta_datamart_out.tipoprestacion         = v_r_datamart.tpo_prestacion
         LET gr_consulta_datamart_out.articulonegativa       = v_r_datamart.articulo_negativa
         LET gr_consulta_datamart_out.fraccionnegativa       = v_r_datamart.fraccion_negativa
         LET gr_consulta_datamart_out.considerando           = v_r_datamart.num_considerando
         LET gr_consulta_datamart_out.usuario                = v_r_cza_datamart.usuario
         LET gr_consulta_datamart_out.fechainiciopension     = v_r_datamart.f_inicio_pension
         LET gr_consulta_datamart_out.fechaemisionresolucion = v_r_datamart.f_resolucion
         LET gr_consulta_datamart_out.numeroresolucion       = 0
         LET gr_consulta_datamart_out.porcentajeevaluacion   = v_r_datamart.porcentaje_valuacion
         LET gr_consulta_datamart_out.semanascotizadas       = v_r_datamart.semanas_cotizadas
         LET gr_consulta_datamart_out.diagnosticoregistro    = v_r_datamart.diag_registro
         LET gr_consulta_datamart_out.fechacargadatamart     = v_r_cza_datamart.f_carga_datamart
         LET gr_consulta_datamart_out.fechaproceso           = v_r_cza_datamart.f_carga_infonavit
         LET gr_consulta_datamart_out.mensaje                = "Consulta realizada correctamente. Se envió última secuencia de pensión del NSS dado."
      ELSE
         -- se devuelve error. EL NSS no se encuentra en ret_datamart
         -- se transfieren los datos al registro de respuesta
         LET gr_consulta_datamart_out.brokerok               = FALSE
         LET gr_consulta_datamart_out.mensaje                = "No se encontraron datos en el Datamart para el NSS dado"
      END IF
      
   ELSE
      -- se verifica si se tiene el RFC
      IF ( (gr_consulta_datamart_in.rfc CLIPPED) IS NOT NULL ) THEN
         -- si el RFC no tiene al menos 10 caracteres, se rechaza la consulta
         IF ( LENGTH(gr_consulta_datamart_in.rfc CLIPPED) < 10 ) THEN
            LET gr_consulta_datamart_out.brokerok               = FALSE
            LET gr_consulta_datamart_out.mensaje                = "El RFC proporcionado no cuenta con la longitud suficiente. No se puede continuar con la consulta."
            RETURN
         END IF

         SELECT COUNT(*)
         INTO   v_conteo
         FROM   afi_derechohabiente
         WHERE  rfc = gr_consulta_datamart_in.rfc

         IF ( v_conteo > 1 ) THEN
            -- hay mas de un derechohabiente con el RFC dado. es necesario especificar
            LET gr_consulta_datamart_out.brokerok               = FALSE
            LET gr_consulta_datamart_out.mensaje                = "El RFC proporcionado coincide con más de un NSS. Es necesario hacer más específica la búsqueda."
         ELSE
            -- se obtiene el NSS asociado al RFC
            SELECT nss
            INTO   v_nss
            FROM   afi_derechohabiente
            WHERE  rfc = gr_consulta_datamart_in.rfc

            -- se obtiene la maxima secuencia de pension del derechohabiente
            SELECT MAX(sec_pension)
            INTO   v_max_sec_pension
            FROM   ret_datamart
            WHERE  nss = v_nss

            -- si existe la secuencia de pension
            IF ( v_max_sec_pension IS NULL ) THEN
               -- se devuelve error. EL NSS no se encuentra en ret_datamart
               -- se transfieren los datos al registro de respuesta
               LET gr_consulta_datamart_out.brokerok = FALSE
               LET gr_consulta_datamart_out.mensaje  = "No se encontraron datos en el Datamart para el NSS dado"
            ELSE
               -- solo hay un derechohabiente, se obtiene el NSS y se ejecuta la misma consulta
               SELECT  *
               INTO    v_r_datamart.*
               FROM    ret_datamart
               WHERE   nss = v_nss
               AND     sec_pension = v_max_sec_pension
         
               -- se obtienen los datos del encabezado del folio en cuestion
               SELECT *
               INTO   v_r_cza_datamart.*
               FROM   ret_cza_datamart
               WHERE  folio = v_r_datamart.folio
         
               -- se transfieren los datos al registro de respuesta
               LET gr_consulta_datamart_out.brokerok               = TRUE
               LET gr_consulta_datamart_out.nss                    = v_nss
               LET gr_consulta_datamart_out.curp                   = v_r_datamart.curp
               LET gr_consulta_datamart_out.nombre                 = v_r_datamart.nombre_datamart
               LET gr_consulta_datamart_out.nombreafore            = v_r_datamart.nombre_afore
               LET gr_consulta_datamart_out.paternoafore           = v_r_datamart.paterno_afore
               LET gr_consulta_datamart_out.maternoafore           = v_r_datamart.materno_afore
               LET gr_consulta_datamart_out.secuencia              = v_r_datamart.sec_pension
               LET gr_consulta_datamart_out.tipomovimiento         = v_r_datamart.tpo_movimiento
               LET gr_consulta_datamart_out.regimen                = v_r_datamart.regimen
               LET gr_consulta_datamart_out.tiposeguro             = v_r_datamart.tpo_seguro
               LET gr_consulta_datamart_out.tipopension            = v_r_datamart.tpo_pension
               LET gr_consulta_datamart_out.tipoprestacion         = v_r_datamart.tpo_prestacion
               LET gr_consulta_datamart_out.articulonegativa       = v_r_datamart.articulo_negativa
               LET gr_consulta_datamart_out.fraccionnegativa       = v_r_datamart.fraccion_negativa
               LET gr_consulta_datamart_out.considerando           = v_r_datamart.num_considerando
               LET gr_consulta_datamart_out.usuario                = v_r_cza_datamart.usuario
               LET gr_consulta_datamart_out.fechainiciopension     = v_r_datamart.f_inicio_pension
               LET gr_consulta_datamart_out.fechaemisionresolucion = v_r_datamart.f_resolucion
               LET gr_consulta_datamart_out.numeroresolucion       = 0
               LET gr_consulta_datamart_out.porcentajeevaluacion   = v_r_datamart.porcentaje_valuacion
               LET gr_consulta_datamart_out.semanascotizadas       = v_r_datamart.semanas_cotizadas
               LET gr_consulta_datamart_out.diagnosticoregistro    = v_r_datamart.diag_registro
               LET gr_consulta_datamart_out.fechacargadatamart     = v_r_cza_datamart.f_carga_datamart
               LET gr_consulta_datamart_out.fechaproceso           = v_r_cza_datamart.f_carga_infonavit
               LET gr_consulta_datamart_out.mensaje                = "Consulta realizada correctamente. Se envió última secuencia de pensión del NSS/RFC dado."
            END IF
         END IF
      ELSE
         LET gr_consulta_datamart_out.brokerok = FALSE
         LET gr_consulta_datamart_out.mensaje  = "No se recibió NSS o RFC para realizar la consulta."
      END IF
   END IF
      

END FUNCTION