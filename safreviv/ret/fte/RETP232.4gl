--=============================================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--=============================================================================
##############################################################################################
#MODULO            =>RET                                                                     #
#PROGRAMA          =>RETP232                                                                 #
#OBJETIVO          =>Programa que realiza las notificaciones correspondientes con SAP-FICO   #
#                    del retiro de Aportaciones voluntarias                                  #
#                                                                                            #
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS "consultaAcreedor.dir/ret_consultaAcreedor.inc"
GLOBALS "creaAcreedor.dir/ret_crearAcreedor.inc"
GLOBALS "generarPago.dir/ret_generaPago.inc"
GLOBALS "consultaPago.dir/ret_consultaPago.inc"
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod           LIKE seg_usuario.usuario_cod, -- nombre del usuario
       p_folio                 LIKE glo_folio.folio, -- numero de folio
       v_bandera               SMALLINT,
       p_titulo                STRING, -- titulo del mensaje enviado en el correo
       p_mensaje               STRING, -- cuerpo del mensaje enviado       
       v_s_sql                 STRING, -- cadena con enunciado SQL
       v_r_afi_derechohabiente RECORD LIKE afi_derechohabiente.*, -- registro de derechohabiente
       v_contador              SMALLINT, -- contador de registros
       v_total_aivs            LIKE ret_voluntaria.total_aivs,
       v_id_solicitud          LIKE ret_voluntaria.id_solicitud,
       v_clave_acreedor        VARCHAR(10),
       v_ws_status             SMALLINT, -- estatus de ejecucion de un webservice
       v_folio_restitucion     LIKE glo_folio.folio,
       v_i_resultado           INTEGER,                             -- resultado del proceso
       v_error_isam           INTEGER,
       v_mensaje              VARCHAR(255),
       v_solicitud_error      LIKE ret_fondo_ahorro.id_solicitud


   -- se reciben los parametros del programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET g_nombre_archivo = ARG_VAL(6)
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETP232.log")
          
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(0, "NOTIFICACIÓN A SAP-FICO")

   DISPLAY "_______________________________________________"
   DISPLAY "Iniciando comunicación con SAP-FICO"

   DISPLAY "==========================================================\n",
           "      Verificando existencia de cuentas de acreedor"
           
   DECLARE cur_buscaacreedor CURSOR FOR
   SELECT b.*, a.total_aivs, a.id_solicitud
   FROM   ret_voluntaria a     ,
          afi_derechohabiente b
   WHERE  a.folio = p_folio
   AND    a.id_derechohabiente = b.id_derechohabiente
   
   -- se busca si existe la cuenta de acreedor de cada derechohabiente con retiro
   FOREACH cur_buscaacreedor INTO v_r_afi_derechohabiente.*, v_total_aivs, v_id_solicitud
   
      -- se consulta con webservice si existe el derechohabiente con cuenta de acreedor en SAP-FICO
      LET ns1consultarAcreedor.rfc = v_r_afi_derechohabiente.rfc
      
      --CALL consultarAcreedor_g() RETURNING v_ws_status
-- PARA PRUEBAS. BOPRRAR
      LET v_ws_status = 0


      
      -- si no hubo error
      IF ( v_ws_status = 0 ) THEN
         -- resultado de la consulta
         DISPLAY "Resultado de la consulta"
         DISPLAY "Cod. Retorno     : ", ns1consultarAcreedorResponse.acreedor.retorno 
         DISPLAY "Clave de Acreedor: ", ns1consultarAcreedorResponse.acreedor.acreedor
         DISPLAY "Grupo Cuenta     : ", ns1consultarAcreedorResponse.acreedor.grupoCuenta
         --DISPLAY ns1consultarAcreedorResponse.acreedor.detalle DYNAMIC ARRAY ATTRIBUTE(XMLList) OF tns1DetalleAcreedor ATTRIBUTE(XMLName="detalle",XMLNamespace="")

-- PARA PRUEBAS. BOPRRAR
         LET ns1consultarAcreedorResponse.acreedor.retorno = 1
         
         -- si no se encontro
         IF ( ns1consultarAcreedorResponse.acreedor.retorno = 1 ) THEN
            DISPLAY "No encontrado. Se procede a generar cuenta de Acreedor"
            -- se genera el acreedor
            LET ns1crear.acreedor.sociedad                 = "INFO"
            LET ns1crear.acreedor.grupoCuenta              = "DESA"
            LET ns1crear.acreedor.nombreAcreedor1          = v_r_afi_derechohabiente.nombre_af CLIPPED, " ", v_r_afi_derechohabiente.ap_paterno_af CLIPPED, " ", v_r_afi_derechohabiente.ap_materno_af CLIPPED
            LET ns1crear.acreedor.nombreAcreedor2          = ""
            LET ns1crear.acreedor.clavePais                = "MX"
            LET ns1crear.acreedor.delegacion               = "11"
            LET ns1crear.acreedor.noExterno                = ""
            LET ns1crear.acreedor.rfc                      = v_r_afi_derechohabiente.rfc
            LET ns1crear.acreedor.rfcAnterior              = ""
            LET ns1crear.acreedor.viaPago                  = "T"
            LET ns1crear.acreedor.cuentas[1].clabe         = "112233445566778899"
            LET ns1crear.acreedor.cuentas[1].referencia    = "INACTIVA"
            LET ns1crear.acreedor.cuentas[1].clabeAnterior = ""

-- valores enviados
DISPLAY "VALORES ENVIADOS A CREACION DE ACREEDOR: "
DISPLAY "sociedad       : ", ns1crear.acreedor.sociedad                
DISPLAY "grupoCuenta    : ", ns1crear.acreedor.grupoCuenta             
DISPLAY "nombreAcreedor1: ", ns1crear.acreedor.nombreAcreedor1         
DISPLAY "nombreAcreedor2: ", ns1crear.acreedor.nombreAcreedor2         
DISPLAY "clavePais      : ", ns1crear.acreedor.clavePais               
DISPLAY "delegacion     : ", ns1crear.acreedor.delegacion              
DISPLAY "noExterno      : ", ns1crear.acreedor.noExterno               
DISPLAY "rfc            : ", ns1crear.acreedor.rfc                     
DISPLAY "rfcAnterior    : ", ns1crear.acreedor.rfcAnterior             
DISPLAY "viaPago        : ", ns1crear.acreedor.viaPago                 
DISPLAY "clabe          : ", ns1crear.acreedor.cuentas[1].clabe        
DISPLAY "referencia     : ", ns1crear.acreedor.cuentas[1].referencia   
DISPLAY "clabeAnterior  : ", ns1crear.acreedor.cuentas[1].clabeAnterior


            -- se genera la cuenta
            --CALL crear_g() RETURNING v_ws_status
-- PARA PRUEBAS. BOPRRAR
LET v_ws_status = 0
            
            IF ( v_ws_status = 0 ) THEN
               DISPLAY "WS ejecutado correctamente"

-- PARA PRUEBAS. BOPRRAR
LET ns1crearResponse.respuestaCrear.noAcreedor = v_r_afi_derechohabiente.nss
LET v_clave_acreedor = v_r_afi_derechohabiente.nss
LET ns1crearResponse.respuestaCrear.retorno = 0
LET ns1crearResponse.respuestaCrear.mensaje = "Cuenta de Acreedor creada correctamente"
              
               DISPLAY "NoAcreedor: ", ns1crearResponse.respuestaCrear.noAcreedor
               DISPLAY "Retorno   : ", ns1crearResponse.respuestaCrear.retorno
               DISPLAY "Mensaje   : ", ns1crearResponse.respuestaCrear.mensaje
            ELSE
               DISPLAY "Error al invocar creación de acreedor: ", v_ws_status
               DISPLAY "code       : ", wsError.code
               DISPLAY "codeNS     : ", wsError.codeNS
               DISPLAY "description: ", wsError.description
               DISPLAY "action     : ", wsError.action
            END IF
            
         END IF
      ELSE
         DISPLAY "Error al ejecutar consulta de acreedor con derechohabiente"
         DISPLAY "NSS: ", v_r_afi_derechohabiente.nss
         DISPLAY "RFC: ", v_r_afi_derechohabiente.rfc
         DISPLAY "Error al consulta de acreedor: ", v_ws_status
         DISPLAY "code       : ", wsError.code
         DISPLAY "codeNS     : ", wsError.codeNS
         DISPLAY "description: ", wsError.description
         DISPLAY "action     : ", wsError.action
      END IF


-- esto se debe descomentar al finalizar las pruebas      
--   END FOREACH
   
   -- se libera el cursor
--   FREE cur_buscaacreedor

   

{
      -- se consulta con webservice si existe el derechohabiente con cuenta de acreedor en SAP-FICO
      LET ns1consultarAcreedor.acreedor = "0000000020"
      
      CALL consultarAcreedor_g() RETURNING v_ws_status
      
      -- si no hubo error
      IF ( v_ws_status = 0 ) THEN
         -- resultado de la consulta
         DISPLAY "Resultado de la consulta"
         DISPLAY "Cod. Retorno     : ", ns1consultarAcreedorResponse.acreedor.retorno 
         DISPLAY "Clave de Acreedor: ", ns1consultarAcreedorResponse.acreedor.acreedor
         DISPLAY "Grupo Cuenta     : ", ns1consultarAcreedorResponse.acreedor.grupoCuenta
         --DISPLAY ns1consultarAcreedorResponse.acreedor.detalle DYNAMIC ARRAY ATTRIBUTE(XMLList) OF tns1DetalleAcreedor ATTRIBUTE(XMLName="detalle",XMLNamespace="")
      ELSE
         DISPLAY "Error al ejecutar consulta de acreedor con derechohabiente"
         DISPLAY "NSS: ", v_r_afi_derechohabiente.nss
         DISPLAY "RFC: ", v_r_afi_derechohabiente.rfc
      END IF
}
   DISPLAY "==========================================================\n"
   
   DISPLAY "==========================================================\n"
   DISPLAY "            Generación de órdenes de pago"
{
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://services.infonavit.org.mx">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:generar>
         <viaPago>T</viaPago>
         <concepto>15</concepto>
         <delegacion>09</delegacion>
         <noReferencia>1234</noReferencia>
         <monto>1.00</monto>
         <nombreCliente>JMP</nombreCliente>
         <noAcreedor>30</noAcreedor>
         <noCheque>0</noCheque>
         <beneficiario></beneficiario>
      </ser:generar>
   </soapenv:Body>
</soapenv:Envelope>
}   
{
-- datos de prueba

   LET ns1generar.viaPago       = "T"
   LET ns1generar.concepto      = "15"
   LET ns1generar.delegacion    = "11"
-- num de referencia bancaria
   LET ns1generar.noReferencia  = "1234"
   LET ns1generar.monto         = 1000.00
   LET ns1generar.nombreCliente = "JMP"
   LET ns1generar.noAcreedor    = "0000000020"
   LET ns1generar.noCheque      = "0123456"
   LET ns1generar.beneficiario  = "JMP"
}

   LET ns1generar.viaPago       = "T"
   LET ns1generar.concepto      = "15"
   LET ns1generar.delegacion    = "11"
   -- num de referencia bancaria
   LET ns1generar.noReferencia  = v_r_afi_derechohabiente.nss
   LET ns1generar.monto         = v_total_aivs
   LET ns1generar.nombreCliente = v_r_afi_derechohabiente.nombre_af CLIPPED, " ", v_r_afi_derechohabiente.ap_paterno_af CLIPPED, " ", v_r_afi_derechohabiente.ap_materno_af CLIPPED
   LET ns1generar.noAcreedor    = v_clave_acreedor
   LET ns1generar.noCheque      = v_id_solicitud
   LET ns1generar.beneficiario  = v_r_afi_derechohabiente.nombre_af CLIPPED, " ", v_r_afi_derechohabiente.ap_paterno_af CLIPPED, " ", v_r_afi_derechohabiente.ap_materno_af CLIPPED


   DISPLAY "Datos enviados a generacion de cuenta por pagar:"
   DISPLAY "viaPago      :", ns1generar.viaPago      
   DISPLAY "concepto     :", ns1generar.concepto     
   DISPLAY "delegacion   :", ns1generar.delegacion   
   DISPLAY "noReferencia :", ns1generar.noReferencia 
   DISPLAY "monto        :", ns1generar.monto        
   DISPLAY "nombreCliente:", ns1generar.nombreCliente
   DISPLAY "noAcreedor   :", ns1generar.noAcreedor   
   DISPLAY "noCheque     :", ns1generar.noCheque     
   DISPLAY "beneficiario :", ns1generar.beneficiario 


   -- se invoca la creacion de la orden de pago
   --CALL generar_g() RETURNING v_ws_status

-- borrar despues de pruebas
LET v_ws_status = 0
   
            
   IF ( v_ws_status = 0 ) THEN
      DISPLAY "Creacion de orden de pago WS ejecutada correctamente"

-- borrar despues de pruebas
LET ns1generarResponse.documento.referenciaUnica = v_id_solicitud
LET ns1generarResponse.documento.numeroDocumentoContable = v_id_solicitud
LET ns1generarResponse.documento.ejercicio = YEAR(TODAY)
LET ns1generarResponse.documento.estatus = 0

      
      DISPLAY "REFERENCIAuNICA        : ", ns1generarResponse.documento.referenciaUnica
      DISPLAY "NUMEROdOCUMENTOcONTABLE: ", ns1generarResponse.documento.numeroDocumentoContable
      DISPLAY "EJERCICIO              : ", ns1generarResponse.documento.ejercicio
      DISPLAY "ESTATUS                : ", ns1generarResponse.documento.estatus
   ELSE
      DISPLAY "Error al invocar creacion de orden de pago: ", v_ws_status
      DISPLAY "code       : ", wsError.code
      DISPLAY "codeNS     : ", wsError.codeNS
      DISPLAY "description: ", wsError.description
      DISPLAY "action     : ", wsError.action
   END IF   
   
   DISPLAY "==========================================================\n"
      
      
   DISPLAY "==========================================================\n"
   DISPLAY "            Consulta de pagos realizados"
{
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://services.infonavit.org.mx">
   <soapenv:Header/>
   <soapenv:Body>
      <ser:consultar>
         <entrada>
            <!--Optional:-->
            <documento>3800000025</documento>
            <!--Optional:-->
            <ejercicio>2013</ejercicio>
            <!--Optional:-->
            <sociedad>INFO</sociedad>
         </entrada>
      </ser:consultar>
   </soapenv:Body>
</soapenv:Envelope>
 }  
{
   LET consultar.entrada.documento = "3800000025"
   LET consultar.entrada.ejercicio = "2013"
   LET consultar.entrada.sociedad  = "INFO"
}

   LET consultar.entrada.documento = v_id_solicitud
   LET consultar.entrada.ejercicio = YEAR(TODAY)
   LET consultar.entrada.sociedad  = "INFO"
   
   
   -- valores enviados a consulta de pago
   DISPLAY "Valores enviados a consulta de pago:"
   DISPLAY "documento: ", consultar.entrada.documento
   DISPLAY "ejercicio: ", consultar.entrada.ejercicio
   DISPLAY "sociedad : ", consultar.entrada.sociedad

   -- se invoca la consulta de saldo
   --CALL consultar_g() RETURNING v_ws_status

-- para pruebas
LET v_ws_status = 0
   
   IF ( v_ws_status = 0 ) THEN
      DISPLAY "CONSULTA DE PAGO WS ejecutada correctamente"
{
      FOR v_contador = 1 TO consultarResponse.salida.getLength()
         DISPLAY "documento         : ", consultarResponse.salida[v_contador].documento
         DISPLAY "ejercicio         : ", consultarResponse.salida[v_contador].ejercicio
         DISPLAY "estatus           : ", consultarResponse.salida[v_contador].estatus
         DISPLAY "importe           : ", consultarResponse.salida[v_contador].importe
         DISPLAY "indicadorRetencion: ", consultarResponse.salida[v_contador].indicadorRetencion
         DISPLAY "referencia        : ", consultarResponse.salida[v_contador].referencia
         DISPLAY "fechaPago         : ", consultarResponse.salida[v_contador].fechaPago
      END FOR
}
   ELSE
      DISPLAY "Error al invocar consulta de pago: ", v_ws_status
      DISPLAY "code       : ", wsError.code
      DISPLAY "codeNS     : ", wsError.codeNS
      DISPLAY "description: ", wsError.description
      DISPLAY "action     : ", wsError.action
   END IF   

   DISPLAY "==========================================================\n"   




-- este se quita al finalizar las pruebas
-- se actualiza la solicitud a pagada o rechazada
IF ( v_r_afi_derechohabiente.nss <> "33856132676" ) THEN
   -- aceptados
   UPDATE ret_voluntaria
   SET estado_solicitud = 70
   WHERE id_solicitud = v_id_solicitud
ELSE
   -- rechazado
   UPDATE ret_voluntaria
   SET estado_solicitud = 100
   WHERE id_solicitud = v_id_solicitud

END IF

END FOREACH
   
-- se libera el cursor
FREE cur_buscaacreedor
   
   -- se envia la cadena que indica el fin de la etapa
   CALL fn_display_proceso(1, "NOTIFICACIÓN A SAP-FICO")

   -- se actualiza el folio a notificado
   LET v_s_sql = "\n UPDATE glo_folio",
                 "\n SET status = 3",         
                 "\n WHERE proceso_cod = ", g_proceso_cod,
                 "\n AND status = 2 ",
                 "\n AND folio  = ", p_folio

   PREPARE pr_folio  FROM v_s_sql
   EXECUTE pr_folio


   LET consultar.entrada.documento = "3800000025"
   LET consultar.entrada.ejercicio = "2013"
   LET consultar.entrada.sociedad  = "INFO"
   
   -- valores enviados a consulta de pago
   DISPLAY "Valores enviados a consulta de pago:"
   DISPLAY "documento: ", consultar.entrada.documento
   DISPLAY "ejercicio: ", consultar.entrada.ejercicio
   DISPLAY "sociedad : ", consultar.entrada.sociedad

   -- se invoca la consulta de saldo
   CALL consultar_g() RETURNING v_ws_status

-- para pruebas
--LET v_ws_status = 0
   
   IF ( v_ws_status = 0 ) THEN
      DISPLAY "CONSULTA DE PAGO WS ejecutada correctamente"

      FOR v_contador = 1 TO consultarResponse.salida.getLength()
         DISPLAY "documento         : ", consultarResponse.salida[v_contador].documento
         DISPLAY "ejercicio         : ", consultarResponse.salida[v_contador].ejercicio
         DISPLAY "estatus           : ", consultarResponse.salida[v_contador].estatus
         DISPLAY "importe           : ", consultarResponse.salida[v_contador].importe
         DISPLAY "indicadorRetencion: ", consultarResponse.salida[v_contador].indicadorRetencion
         DISPLAY "referencia        : ", consultarResponse.salida[v_contador].referencia
         DISPLAY "fechaPago         : ", consultarResponse.salida[v_contador].fechaPago
      END FOR
   ELSE
      DISPLAY "No funciona servicio de consilta de pago"
                     DISPLAY "ERROR al invocar webservice de confirmacion de pago ADAI"
                     DISPLAY "CODE       : ", wsError.code
                     DISPLAY "CODENS     : ", wsError.codeNS
                     DISPLAY "DESRIPTION : ", wsError.description
                     DISPLAY "ACTION     : ", wsError.action
      END IF


      
   {
   -- se finaliza la operacion
   CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING v_bandera
}
   -- se complementa el mensaje
   LET p_mensaje = "RETIRO DE APORTACIONES VOLUNTARIAS - NOTIFICACIÓN A SAP-FICO."
                        
   -- se crea el titulo del mensaje
   LET p_titulo = "Finalización de operación - RETIROS APORTACIONES VOLUNTARIAS - NOTIFICACIÓN A SAP-FICO"
               
   -- se invoca el envio de correo electronico de notificacion
{
   CALL fn_correo_proceso(g_pid, g_proceso_cod, g_opera_cod,
                          NULL, -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
}
END MAIN