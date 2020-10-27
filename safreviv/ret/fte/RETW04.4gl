--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETW04                                                  #
#OBJETIVO          => WS SOLICITUD DE RESOLUCION ADAI  FONDO AHORRO           #
#FECHA INICIO      => 03-MAY-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro viv7292
--/opt/fourjs/2.32/gas/. ./envas
--httpdispatch -f as_safreviv_ws2.xcf
--port 9187

IMPORT FGL WSHelper
IMPORT com

DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
GLOBALS
-- registro de entrada al webservice
DEFINE r_aprueba_solicitud_in RECORD
         nss            CHAR(11), -- nss del trabajador
         rfc            CHAR(13), -- rfc del trabajador
         caso_adai      SMALLINT, -- caso en adai
         autoriza_pago  SMALLINT  -- autorizacion/rechazo de pago
       END RECORD,
       -- registro de respuesta del webservice
       r_aprueba_solicitud_out    RECORD
         nss          CHAR(11), -- Numero de seguridad social del trabajador
         rfc          CHAR(13), -- rfc del trabajador
         res_op       SMALLINT, -- Respuesta de operación.  Aceptado / Rechazado
         cod_rechazo  SMALLINT  -- Código de rechazo       
       END RECORD
END GLOBALS

DEFINE serverURL STRING
DEFINE SCREEN    BOOLEAN

&define display_status(status) \
  IF NOT screen THEN \
    DISPLAY status \
  ELSE \
    DISPLAY status TO MSG \
    MENU ON idle 1 EXIT MENU ON ACTION close EXIT PROGRAM END MENU \
  END IF
#
# MAIN
#
MAIN
  DEFINE ret INTEGER  
      
  DEFER INTERRUPT

  LET SCREEN = FALSE
  
  #
  # Create Retiro service
  #
  CALL CreateRetiroService(FALSE)
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=8090)
  # 
  CALL com.WebServiceEngine.Start()
  display_status("The server is listening.")

  IF SCREEN THEN
    MENU
      ON IDLE 1
        LET ret = com.WebServiceEngine.ProcessServices(1)
        CASE ret
          WHEN 0
            DISPLAY "Request processed." TO msg
          WHEN -1
            DISPLAY "No request..." TO msg
          WHEN -2
            DISPLAY "Disconnected from application server." TO msg
            EXIT PROGRAM   # The Application server has closed the connection
          WHEN -3
            DISPLAY "Client Connection lost." TO msg
          WHEN -4
            DISPLAY "Server interrupted with Ctrl-C." TO msg
          WHEN -10
            DISPLAY "Internal server error." TO msg
        END CASE
      ON ACTION CLOSE
        EXIT PROGRAM
    END MENU
  ELSE
    WHILE TRUE
      LET ret = com.WebServiceEngine.ProcessServices(-1)
      CASE ret
        WHEN 0
          DISPLAY "Request processed." 
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
      IF INT_FLAG<>0 THEN
        LET INT_FLAG=0
        EXIT WHILE
      END IF     
    END WHILE
    DISPLAY "Server stopped"
  END IF
END MAIN

{
======================================================================
Clave: 
Nombre: CreateRetiroService
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Crear el servicio web

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION CreateRetiroService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS = "http://localhost/"
  LET serviceNS = "http://www.infonavit.gob.mx/"

  TRY
    #
    # Create retiro Web Service
    #
    LET serv = com.WebService.CreateWebService("apruebaSolicitudFA",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_resol_adai 
    LET op = com.WebOperation.CreateRPCStyle("fn_aprueba_solicitud_fa","fn_aprueba_solicitud_fa",r_aprueba_solicitud_in,r_aprueba_solicitud_out)
    CALL serv.publishOperation(op,NULL)

    IF generateWSDL THEN
      #
      # Generate WSDL
      #
      LET ret = serv.saveWSDL(serverURL)
      IF ret=0 THEN
        DISPLAY "WSDL saved"      
      ELSE
        DISPLAY "ERROR: Unable to save WSDL"
      END IF
    ELSE
      #
      # Register service  
      #
      CALL com.WebServiceEngine.RegisterService(serv)  
      display_status("Resolucion Adai Service registered")
    END IF
    
  CATCH
    display_status("Unable to create 'Resolacion Adai' Web Service :"||STATUS)
    EXIT PROGRAM
  END TRY
END FUNCTION

FUNCTION exitHelp()
  --DISPLAY "Usage: "
  DISPLAY "Uso: "
  DISPLAY "  ", arg_val(0)
  --DISPLAY "    Start the server on port defined by FGLAPPSERVER"
  DISPLAY "    Inicar el servidor en el puerto definido en FGLAPPSERVER"
  DISPLAY "  ", arg_val(0), " -W serverurl"
  --DISPLAY "    Generate the WSDL file for the given url"
  DISPLAY "    Genera el archivo WSDL para la URL dada"
  DISPLAY "  ", arg_val(0), " -S port"
  --DISPLAY "    Start service in graphical mode and on given port"
  DISPLAY "    Iniciar el serviciuo en modo grafico y en el puerto dado"
  EXIT PROGRAM
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_aprueba_solicitud_fa
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida y aprueba una solicitud de retiro por fondo de Ahorro
mediante webservice

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_aprueba_solicitud_fa()
   
   -- se invoca la validacion de los datos
   CALL fn_valida_resolucion(r_aprueba_solicitud_in.nss, r_aprueba_solicitud_in.rfc, r_aprueba_solicitud_in.autoriza_pago)
END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_valida_resolucion
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Valida que exista una solicitud de retiro de fondo de ahorro en estatus
en espera de confirmacion/aprobacion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_valida_resolucion(p_nss, p_rfc, p_autoriza_pago)
DEFINE p_nss                LIKE afi_derechohabiente.nss,
       p_rfc                LIKE afi_derechohabiente.rfc,
       p_autoriza_pago      SMALLINT, -- booleana que indica si se debe pagar
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_id_solicitud       LIKE ret_fondo_ahorro.id_solicitud -- id de la solicitud de retiro

   -- se obtiene el id_derechohabiente 
   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_fondo72
   WHERE  nss = p_nss
   AND    rfc = p_rfc
   AND    id_derechohabiente IS NOT NULL
   
   -- si no se encuentra el NSS en vivienda
   IF ( v_id_derechohabiente IS NULL ) THEN
      
      -- se verifica si esta en la tabla de id_derechohabientes nuevos
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_fondo72_d
      WHERE  nss = p_nss
      AND    rfc = p_rfc
   END IF
   
   -- si existe el id_derechohabiente
   IF ( v_id_derechohabiente IS NOT NULL ) THEN
      -- se verifica si tiene una solicitud pendiente de aprobacion
      SELECT id_solicitud
      INTO   v_id_solicitud
      FROM   ret_fondo_ahorro
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    folio = 0
      AND    estado_solicitud = 10
      
      -- si aparece su solicitud
      IF ( v_id_solicitud IS NOT NULL ) THEN
           
         -- si se recibio senal para autorizar el pago
         IF ( p_autoriza_pago ) THEN
            -- se autoriza la solicitud
            CALL fn_cambio_estado("A", v_id_solicitud, p_nss, p_rfc, v_id_derechohabiente) --Aprobado Pagar
         ELSE
            -- se rechaza
            CALL fn_cambio_estado("R", v_id_solicitud, p_nss, p_rfc, v_id_derechohabiente) --Rechazado no Pagar
         END IF
      ELSE 
         LET r_aprueba_solicitud_out.nss         = p_nss
         LET r_aprueba_solicitud_out.rfc         = p_rfc
         LET r_aprueba_solicitud_out.res_op      = 2
         LET r_aprueba_solicitud_out.cod_rechazo = 999 -- no se encuentra la solicitud
      END IF
   ELSE
      -- se indica que no se reconoce el nss/rfc
      LET r_aprueba_solicitud_out.nss         = p_nss
      LET r_aprueba_solicitud_out.rfc         = p_rfc
      LET r_aprueba_solicitud_out.res_op      = 1
      LET r_aprueba_solicitud_out.cod_rechazo = 666 -- nss/rfc no existen
   END IF

END FUNCTION

{
======================================================================
Clave: 
Nombre: fn_trabajador_credito_vigente
Fecha creacion: Marzo 22, 2013
Autor: Ivan Vega, EFP
Narrativa del proceso que realiza:
Verifica si un derechohabiente tiene un credito vigente

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_cambio_estado(v_estado, p_id_solicitud, p_nss, p_rfc, p_id_derechohabiente)
DEFINE v_estado              CHAR(1)     ,
       p_id_solicitud        DECIMAL(9,0),
       p_nss                 CHAR(11),
       p_rfc                 CHAR(13),
       p_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
       v_i_estado_marca      INTEGER     ,
       v_marca_fondo_ahorro  INTEGER
  

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro   = 802; -- marca para fondo ahorro
   LET v_i_estado_marca       = 0;
   
   -- se aprueba la solicitud
   IF ( v_estado = "A" ) THEN 
     
      UPDATE ret_fondo_ahorro 
      SET    estado_solicitud = 15
      WHERE  id_solicitud     = p_id_solicitud
      AND    estado_solicitud = 10
      
      -- se responde al WS que la solicitud se aprobo
      LET r_aprueba_solicitud_out.nss         = p_nss
      LET r_aprueba_solicitud_out.rfc         = p_rfc
      LET r_aprueba_solicitud_out.res_op      = 1 -- correcta
      LET r_aprueba_solicitud_out.cod_rechazo = 0 -- aprobada
   END IF
  
   -- se rechaza la solicitud
   IF ( v_estado = "R" ) THEN 
      UPDATE ret_fondo_ahorro 
      SET    estado_solicitud   = 100,
             cod_rechazo        = 80
      WHERE  id_solicitud       = p_id_solicitud
      AND    estado_solicitud   = 10

      -- se responde al WS que la solicitud se aprobo
      LET r_aprueba_solicitud_out.nss         = p_nss
      LET r_aprueba_solicitud_out.rfc         = p_rfc
      LET r_aprueba_solicitud_out.res_op      = 1 -- correcta
      LET r_aprueba_solicitud_out.cod_rechazo = 80 -- solicitud ha sido rechazada
                                               
      -- se desmarca la cuenta pues fue rechazada la solicitud
      
      PREPARE pr_cambio_estatus FROM "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
      EXECUTE pr_cambio_estatus USING  p_id_derechohabiente
                                       ,v_marca_fondo_ahorro -- desmarca de fondo ahorro
                                       ,p_id_solicitud -- identificador de registro de archivo o lote
                                       ,"40" -- estado marca
                                       ,"0"  -- marca de la causa
                                       ,"safreviv"
                                       ,"1503" -- retiro Fondo de ahorro Webservices
         INTO v_i_estado_marca
   END IF 
 
END FUNCTION
