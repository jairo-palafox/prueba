
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
------------------------------------------------------------------------------#
#Modulo            => ret                                                     #
#Programa          => RETWE06                                                 #
#Objetivo          => Genera la solicitud de retiro de Ley 73                 #
#Fecha Inicio      => 06-Jun-2012                                             #
###############################################################################

--disparador de ws solicitud de retiro ley 73 
--httpdispatch -f as_safreviv_ws5.xcf
--port  9190

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS
  DEFINE 
    ret_solicitud_ley73 RECORD
       g_nss            CHAR(11),     -- NSS DERECHOHABIENTE
       g_grupo_tr       VARCHAR(4)    -- GRUPO DE TRABAJADOR 
       
    END RECORD,

    ret_respuesta RECORD
      r_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
      r_nombre_af       CHAR(40)     ,--Nombre  Nombre del trabajador Texto
      r_paterno_af      CHAR(40)     ,--Apellido paterno  Apellido paterno del trabajador Texto
      r_materno_af      CHAR(40)     ,--Apellido materno  Apellido materno del trabajador Texto
      r_importeviv97    DECIMAL(12,2),--Importe de Vivienda 97 Numérico
      r_importeviv92    DECIMAL(12,2),--Importe de Vivienda 92 Numérico
      r_cod_ret         SMALLINT     ,--Código de retorno Según cátalogo a dos posiciones Numérico
      r_mensaje_cod_ret CHAR(100)    ,--Mensaje de código de retorno Texto
      r_rfc_af          CHAR(13)     ,--RFC   RFC del trabajador
      r_curp_af         CHAR(18)     ,--CURP  CURP del trabajador
      --r_nombre_NRP      CHAR(40)   ,--NRP   Número de Registro Patronal
      --r_rfc_pat         CHAR(13)   ,--Nombre   Nombre del NRP
      r_marca_jur       SMALLINT      --Marca Jurídico   Estatús marca en sistema
    END RECORD
    
 DEFINE g_id_derechohabiente DECIMAL(9,0)
       ,g_nss                CHAR(11)  -- NSS DERECHOHABIENTE
       ,g_grupo_tr           SMALLINT  -- Grupo de trabajador
       ,g_viv97_val          DECIMAL(12,2)
       ,g_viv92_val          DECIMAL(12,2)
       ,g_aivs97             DECIMAL(12,2)
       ,g_aivs92             DECIMAL(12,2)

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
      
  LET SCREEN = FALSE
  #
  # Check arguments
  #
  --fglrun RETW01 
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRechazoService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE010" ATTRIBUTES(TEXT = "Retiro Ley 73", STYLE="naked")
      display_status(" Retiro Ley 73 Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create  Retiro Ley 73
  #
  CALL CreateRechazoService(FALSE)
    
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=9190)
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

#
# Create Rechazo Retiro RPC/Literal service
#
FUNCTION CreateRechazoService(generateWSDL)
  DEFINE serv         com.WebService       # WebService
  DEFINE op           com.WebOperation     # Operation of a WebService
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://localhost/"

  TRY
  
    #
    # Create Rechazo retiro Web Service
    #
    LET serv = com.WebService.CreateWebService("RetiroLey73",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_rechazo 
    LET op = com.WebOperation.CreateRPCStyle("fn_genera_solicitud_retiro_ley73","fn_genera_solicitud_retiro_ley73",ret_solicitud_ley73,ret_respuesta)
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
      display_status("Retiro Ley 73 registered")
    END IF
    
  CATCH
    display_status("Unable to create 'Rechazo Retiro' Web Service :"||STATUS)
    EXIT PROGRAM
  END TRY
    
END FUNCTION



FUNCTION exitHelp()
  DISPLAY "Usage: "
  DISPLAY "  ", arg_val(0)
  DISPLAY "    Start the server on port defined by FGLAPPSERVER"
  DISPLAY "  ", arg_val(0), " -W serverurl"
  DISPLAY "    Generate the WSDL file for the given url"
  DISPLAY "  ", arg_val(0), " -S port"
  DISPLAY "    Start service in graphical mode and on given port"
  EXIT PROGRAM
END FUNCTION

#
# USER PUBLIC FUNCTIONS
#

{ ==========================================================================
Nombre: fn_genera_solicitud_retiro_ley73
Narrativa del proceso que realiza:
Valida y genera (en caso de ser valida) una solicitud de retiro para Retiro
Ley 73 de un trabajador identificado por NSS y un grupo al que pertenece
de acuerdo con la reglamentacion de los retiros

Parametros de Entrada:
  Se usa el registro global
  
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_genera_solicitud_retiro_ley73()

   -- se inician las variables de trabajo
   LET g_id_derechohabiente = 0

   LET  g_viv97_val = 0
   LET  g_viv92_val = 0
   LET  g_aivs97    = 0
   LET  g_aivs92    = 0

   -- se asignan las variables de consulta
   -- NSS del trabajador
   LET g_nss      = ret_solicitud_ley73.g_nss
   -- Grupo al que pertenece el trabajador
   LET g_grupo_tr = ret_solicitud_ley73.g_grupo_tr

   -- se invoca la validacion de la solicitud
   CALL fn_valida_solicitud_retiro_ley73()
END FUNCTION


{ ==========================================================================
Nombre: fn_valida_solicitud_retiro_ley73

Narrativa del proceso que realiza:
Valida una solicitud de retiro Ley 73 de acuerdo con el NSS y las clausulas del
grupo al que pertenece el NSS dado


Parametros de Entrada:
  Se usa el registro global
  
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_valida_solicitud_retiro_ley73()

   -- todos los datos debieron recibirse en la solicitud
   IF ( g_nss IS NULL OR g_grupo_tr IS NULL ) THEN    
      LET ret_respuesta.r_cod_ret          = 99 --ERV
      LET ret_respuesta.r_mensaje_cod_ret  = "Todos los campos son oblgatorios"
      LET ret_respuesta.r_importeviv97     = 0
      LET ret_respuesta.r_importeviv92     = 0             
   ELSE

      -- 1.5 Se valida que exista el trabajador (NSS) en la base de datos
      IF ( fn_valida_nss(g_nss) ) THEN

         -- 2.0 se verifica el grupo al que pertenece la solicitud de retiro
         CASE g_grupo_tr
            -- Tipo de proceso "0101" es grupo 1
            WHEN "0101"
              CALL fn_grupo_1()

            -- PONER AQUI LA INVOCACION PARA EL RESTO DE LOS GRUPOS  


            -- grupo no reconocido           
            OTHERWISE
               DISPLAY "Grupo no reconocido"
         END CASE 
      ELSE 
         -- el NSS dado no existe, se rechaza la solicitud
         CALL fn_rechaza_solicitud(23)
      END IF
   END IF
END FUNCTION

#objetivo Realiza las validaciones a una solicitud del grupo 1
FUNCTION fn_grupo_1()
DEFINE v_saldo_total_vivienda DECIMAL(22,2) -- saldo total de vivienda
      ,v_bnd_stats  SMALLINT
      ,v_estado     SMALLINT 
      ,f_resol      DATE

   -- se obtiene el id_derechohabiente del nss
   SELECT id_derechohabiente
   INTO   g_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = g_nss

   -- si la solicitud ya existe, se rechaza pues siguen en tramite
   IF ( fn_solicitud_existente() ) THEN 
      --DISPLAY "existe solicitud previa en tramite"
      CALL fn_rechaza_solicitud(30)
   ELSE
      -- Grupo 1.2 Se obtiene el saldo total del trabajador
      -- sumando el saldo de viv92 y viv97
      CALL fn_calcula_saldo_viv97_viv92() 
           RETURNING g_viv97_val , g_viv92_val

      LET v_saldo_total_vivienda = g_viv97_val + g_viv92_val
      
      -- Grupo 1.3 El saldo debe ser mayor a cero
      IF ( v_saldo_total_vivienda > 0 ) THEN

         -- se revisa si el trabajador tiene un tramite judicial
         CALL fn_ret_tramite_jud() RETURNING v_bnd_stats

         IF ( v_bnd_stats ) THEN
            #se rechaza por tener tarmite judicial dependiendo del cazo
            CALL fn_rechaza_solicitud(63)
            --CALL fn_rechaza_solicitud(64)
         ELSE 
            CALL fn_resol_aprob() RETURNING v_estado, f_resol

            IF ( v_estado ) THEN
               IF f_resol >= '13/01/2012' THEN
                  IF ( fn_credito_vigente() ) THEN
                     #se separa para saber si esta en tramite (81)o si es vigente(80)
                     --CALL fn_rechaza_solicitud(80)
                     CALL fn_rechaza_solicitud(81) 
                  ELSE  
                     CALL fn_captura_solicitud(5,0)
                  END IF 
               END IF 
            ELSE 
               --DISPLAY "no tiene resolucion en el spes"
               #No esta dado de alta en tabla de pensiones
               CALL fn_rechaza_solicitud(82)
            END IF 
         END IF 
      ELSE            
         CALL fn_rechaza_solicitud(19)
      END IF
   END IF 
END FUNCTION

#objetivo validaciones por el grpo de derechohabiente 2-4
FUNCTION fn_grupo_2_3_4()
  DEFINE v_bnd_stats  SMALLINT
        ,v_viv_total  DECIMAL(12,2) 
      
  #objetivo valida si existe tramite 
  #casos pagados por juridico laudo 63
  #casos pagados ley 73  amparo     64 
  CALL fn_ret_tramite_jud() RETURNING v_bnd_stats 
  IF v_bnd_stats THEN
     --DISPLAY "Se rechaza la solicitud por retiro causa"
     
     ## el error que muestra depende de que tramite se haya encontrado
     CALL fn_rechaza_solicitud(63)
     --CALL fn_rechaza_solicitud(64)
  ELSE
     SELECT id_derechohabiente
     INTO g_id_derechohabiente
       FROM afi_derechohabiente
      WHERE nss = g_nss 

      DISPLAY g_nss,g_id_derechohabiente

      IF fn_solicitud_existente() THEN 
        --DISPLAY "existe solicitud previa en tramite"
        CALL fn_rechaza_solicitud(30)
        RETURN 
      END IF 

     --DISPLAY "Se valida credito vigente" 
     #objetivo valida credito vigente tramite judicial (Ley 73) 
     IF fn_credito_vigente() THEN
        --DISPLAY "Se marca la cuenta como en proceso de transferencia de acreditados"

        ## depende de que tramite se haya realizado
       --CALL fn_rechaza_solicitud(80)
       CALL fn_rechaza_solicitud(81)
     ELSE
        --DISPLAY "verifica si existe registro en ret_transferencia tipo b"

        #objetivo existe transferencia tipo B
        CALL fn_validacion_transf_tpo_b() 
             RETURNING v_bnd_stats, g_viv97_val, g_viv92_val

       IF v_bnd_stats THEN       
         --DISPLAY "se leen los montos transferidos"
         --DISPLAY "se guarda solicitud y se marca la cuenta y se le informa al portal"
         --DISPLAY "que la solicitd ha sido aceptada"

         #objetivo recupera saldos valuados de viv97 y viv92
         CALL fn_captura_solicitud(5,0) 
       ELSE
          CALL fn_calcula_saldo_viv97_viv92() RETURNING g_viv97_val , g_viv92_val
          --DISPLAY "se suman los saldos de viv97 y viv92 grupo 2"
          --DISPLAY g_viv97_val , g_viv92_val
          LET v_viv_total = 0
          LET v_viv_total = g_viv97_val + g_viv92_val
          --DISPLAY "posterior a la sma de saldo ",v_viv_total 
          IF v_viv_total > 0 THEN

            --DISPLAY "se guarda solicitud y se marca la cuenta y se le informa al portal"
            --DISPLAY "que la solicitd ha sido aceptada"

            #objetivo recupera saldos valuados de viv97 y viv92
            CALL fn_captura_solicitud(5,0)  
          ELSE 
            --DISPLAY "Se rechaza la solicitud por retiro causa sin saldo"
            CALL fn_rechaza_solicitud(19)      
          END IF 
       END IF    
     END IF
  END IF 
END FUNCTION

#objetivo Captura de solcitd correcta 
FUNCTION fn_captura_solicitud(v_estado,v_cod_rec)

DEFINE ret_resp_carga  RECORD
      r_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
      r_nombre_af       CHAR(40)     ,--Nombre  Nombre del trabajador    Texto
      r_paterno_af      CHAR(40)     ,--Apellido paterno  Apellido paterno del trabajador   Texto  
      r_materno_af      CHAR(40)     ,--Apellido materno  Apellido materno del trabajador   Texto             
      r_importeviv97    DECIMAL(12,2),--Importe de Vivienda 97     Numérico
      r_importeviv92    DECIMAL(12,2),--Importe de Vivienda 92     Numérico
      r_cod_ret         SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones   Numérico 
      r_mensaje_cod_ret CHAR(100)    ,--Mensaje   Mensaje de código de retorno    Texto 
      r_rfc_af          CHAR(13)     ,--RFC   RFC del trabajador   
      r_curp_af         CHAR(18)     ,--CURP  CURP del trabajador   
      --r_nombre_NRP      CHAR(40)     ,--NRP   Número de Registro Patronal 
      --r_rfc_pat         CHAR(13)     ,--Nombre   Nombre del NRP   
      r_marca_jur       SMALLINT      --Marca Jurídico   Estatús marca en sistema 
    END RECORD
DEFINE v_query          STRING
DEFINE v_estado         SMALLINT
DEFINE v_cod_rec        SMALLINT
DEFINE v_id_solictud    DECIMAL(9,0)

DEFINE v_cod_ret        SMALLINT 
DEFINE v_cod_ret_desc   CHAR(50) 
DEFINE v_count          SMALLINT  
--DEFINE v_nombre_nrp     CHAR(40)
--DEFINE v_rfc_nrp        CHAR(13)

LET v_cod_ret       = v_cod_rec

IF v_cod_ret IS NOT NULL THEN
   SELECT desc_larga 
     INTO v_cod_ret_desc
     FROM   ret_codigo_retorno
    WHERE cod_retorno = v_cod_ret
END IF

IF v_cod_ret_desc IS NULL THEN
  LET v_cod_ret_desc="No Capturados"
END IF

  IF v_cod_rec = 23 THEN
     LET ret_respuesta.r_cod_ret         = v_cod_rec
     LET ret_respuesta.r_mensaje_cod_ret = v_cod_ret_desc      
    RETURN 
  END IF  

  IF (g_viv97_val < 0 OR g_viv92_val < 0 ) AND v_cod_rec <> 0 THEN  
     LET g_viv92_val = 0
     LET g_viv97_val = 0
  END IF 

  LET v_query =  "\n SELECT nss                                      ",
                 "\n      ,nombre_af                                 ",
                 "\n      ,ap_paterno_af                             ",
                 "\n      ,ap_materno_af                             ",
                 "\n      ,",g_viv97_val,"                           ",
                 "\n      ,",g_viv92_val,"                           ",
                 "\n      ,",v_cod_ret,"                             ",
                 "\n      ,'",v_cod_ret_desc CLIPPED ,"'             ",
                 "\n      ,rfc                                       ",
                 "\n      ,curp                                      ",
                 "\n  FROM afi_derechohabiente                       ",
                 "\n WHERE id_derechohabiente = ",g_id_derechohabiente
                 
       --DISPLAY v_query
       PREPARE cons_n FROM v_query
       EXECUTE cons_n INTO ret_resp_carga.*

CALL valida_previsita() RETURNING  v_count , v_id_solictud
IF v_count > 0 THEN
   IF v_estado = 5 THEN
      LET v_estado = 10 
   END IF 
       LET v_query ="EXECUTE PROCEDURE sp_cambio_status_ley73(?,?,?,?,?,?,?,?,?)"
       PREPARE status_ley73 FROM  v_query
       EXECUTE status_ley73 USING g_id_derechohabiente
                                 ,v_id_solictud
                                 ,v_estado
                                 ,v_cod_rec
                                 ,g_viv97_val
                                 ,g_viv92_val
                                 ,g_aivs97
                                 ,g_aivs92
                                 ,'M'

     LET ret_respuesta.* = ret_resp_carga.*    
ELSE 
   IF v_cod_ret <> 30 THEN
       LET v_query ="EXECUTE PROCEDURE sp_insert_solicitud_ret_ley73(?,?,?,?,?,?,?,?,?)"
   
       PREPARE insert_ley73 FROM  v_query
       EXECUTE insert_ley73 USING g_id_derechohabiente
                                 ,g_grupo_tr
                                 ,g_aivs92
                                 ,g_aivs97
                                 ,g_viv92_val
                                 ,g_viv97_val
                                 ,v_cod_ret
                                 ,v_estado
                                 ,v_cod_rec
   END IF
   LET ret_respuesta.* = ret_resp_carga.*
END IF
END FUNCTION

#Objetivo valida si la solicitud ya fue consultada
FUNCTION  valida_previsita()
DEFINE v_id_derechohabiente DECIMAL(9,0) 
DEFINE v_id_solicitud       DECIMAL(9,0)
 
     SELECT COUNT(id_derechohabiente),NVL(id_solicitud,0)
       INTO v_id_derechohabiente,v_id_solicitud
       FROM ret_ley73
      WHERE id_derechohabiente = g_id_derechohabiente
        AND estado_solicitud = 5
        GROUP BY id_solicitud
        
       RETURN v_id_derechohabiente ,v_id_solicitud 
END FUNCTION

#objetivo valida si existe tramite judicial
#casos pagados por juridico laudo 63
#casos pagados ley 73  amparo     64
FUNCTION fn_ret_tramite_jud() --ERV
  RETURN 0
END FUNCTION  

#objetivo valida credito vigente tramite judicial (Ley 73) 
FUNCTION fn_credito_vigente()
   DEFINE v_credito    SMALLINT
   --DEFINE v_id_derechohabiente DECIMAL(9,0)

   SELECT COUNT(*)
     INTO v_credito
     FROM cta_credito
    WHERE id_derechohabiente  = g_id_derechohabiente
    --AND TODAY BETWEEN f_credito AND f_credito UNITS YEAR --ERV

    IF v_credito > 1 THEN
       LET v_credito = 1
    END IF 

RETURN v_credito
END FUNCTION

#objetivo existe transferencia tipo B
FUNCTION fn_validacion_transf_tpo_b()
   DEFINE v_bnd_count          SMALLINT
   DEFINE v_precio_fondo       SMALLINT 
   DEFINE f_ultima_val         DATE 
   DEFINE v_viv97_val          DECIMAL(14,2)
   DEFINE v_viv92_val          DECIMAL(14,2)
   DEFINE v_subcuenta          SMALLINT 
   DEFINE v_fec_saldo          DATE 
   DEFINE v_s_sql              STRING 
   DEFINE v_resultado_consulta SMALLINT  
   DEFINE v_fondo              SMALLINT 
   
 
   LET v_bnd_count = 0
   LET g_aivs97    = 0
   LET g_aivs92    = 0
   LET v_fondo     =  11

   SELECT  1,aivs_viv97
     INTO  v_bnd_count,g_aivs97
     FROM
       ret_transferencia tr,
       ret_matriz_derecho md
    WHERE md.id_ret_matriz_derecho = tr.id_ret_matriz_derecho
      AND tpo_retiro = "B"
      AND id_derechohabiente = g_id_derechohabiente

    LET f_ultima_val = TODAY ;
   SELECT NVL(precio_fondo,0)
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE  f_valuacion = f_ultima_val
      AND fondo  =  v_fondo;

    --correccion por falta de datos
    IF v_precio_fondo <= 0 THEN
      SELECT MAX(f_valuacion)
        INTO f_ultima_val
        FROM glo_valor_fondo
       WHERE fondo  =  v_fondo;

      SELECT precio_fondo
        INTO v_precio_fondo
        FROM glo_valor_fondo
       WHERE f_valuacion = f_ultima_val
         AND fondo  =  v_fondo;
    END IF

   LET v_s_sql = "EXECUTE FUNCTION fn_recupera_saldo_valuado(?,?,?,?,?)" 
   LET v_subcuenta = 8

   -- se prepara la ejecucion del stored procedure para la integracion
   PREPARE sid_saldo_92_ FROM v_s_sql
   EXECUTE sid_saldo_92_ USING g_nss,g_id_derechohabiente,v_subcuenta,v_fec_saldo,v_fondo
                          INTO  v_viv92_val, g_aivs92,v_resultado_consulta
    
   LET v_viv97_val = g_aivs97 * v_precio_fondo
   LET v_viv92_val = g_aivs92 * v_precio_fondo
 
  RETURN v_bnd_count, v_viv97_val , v_viv92_val
END FUNCTION 

#objetivo recupera saldos valuados de viv97 y viv92
FUNCTION fn_calcula_saldo_viv97_viv92()
   DEFINE v_viv97_val          DECIMAL (12,2)
         ,v_viv92_val          DECIMAL (12,2)

   DEFINE v_resultado_consulta          SMALLINT
   DEFINE v_s_sql                       VARCHAR(200)
   DEFINE v_subcuenta_92                SMALLINT
   DEFINE v_subcuenta_97                SMALLINT
   DEFINE v_fec_saldo                   DATE
   DEFINE v_fondo                       DECIMAL(9,0)
 
   LET v_fec_saldo = TODAY
   LET v_subcuenta_97 = 4
   LET v_subcuenta_92 = 8
   LET v_viv97_val    = 0
   LET v_viv92_val    = 0
   LET v_fondo        = 11 
       
   --Operacion aplica con calculo de la funcion saldo_valuado_dia
   LET v_s_sql = "EXECUTE FUNCTION fn_recupera_saldo_valuado(?,?,?,?,?)"

   -- se prepara la ejecucion del stored procedure de la valuacion para subcuenta 4
   PREPARE sid_saldo_97 FROM v_s_sql
   EXECUTE sid_saldo_97 USING g_nss,g_id_derechohabiente,v_subcuenta_97,v_fec_saldo,v_fondo
                          INTO  v_viv97_val, g_aivs97,v_resultado_consulta

   LET v_s_sql = "EXECUTE FUNCTION fn_recupera_saldo_valuado(?,?,?,?,?)" 

   -- se prepara la ejecucion del stored procedure de la valuacion para subcuenta 8
   PREPARE sid_saldo_92 FROM v_s_sql
   EXECUTE sid_saldo_92 USING g_nss,g_id_derechohabiente,v_subcuenta_92,v_fec_saldo,v_fondo
                         INTO  v_viv92_val, g_aivs92,v_resultado_consulta

   --DISPLAY  "v_viv92_val, g_aivs92,v_resultado_consulta " ,
              --v_viv92_val, g_aivs92,v_resultado_consulta
              --
    --DISPLAY "v_viv97_val, g_aivs97,v_resultado_consulta ",
              --v_viv97_val, g_aivs97,v_resultado_consulta 
RETURN  v_viv97_val ,v_viv92_val
END FUNCTION 

#objetvo Validar si el nss existe en la bd
FUNCTION fn_valida_nss(p_nss)
DEFINE p_nss                CHAR(11), -- NSS solicitante
       v_id_derechohaBiente DECIMAL(9,0), -- id derechohabiente buscado
       v_existe_nss         SMALLINT

   -- se asume que el derechohabiente existe
   LET v_existe_nss = TRUE

   -- se busca el NSS en la tabla de derechohabietnes       
   SELECT id_derechohabiente 
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente  
   WHERE  nss = p_nss

   -- si no se encontro, se indica con la booleana
   IF ( v_id_derechohabiente IS NULL ) THEN
      LET v_existe_nss = FALSE
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_existe_nss
END FUNCTION 

#objetivo funcion genenerica para el rechazo de la solicitud
FUNCTION fn_rechaza_solicitud(v_cod_rechazo)
  DEFINE v_cod_rechazo SMALLINT  
 
   CALL fn_captura_solicitud(100,v_cod_rechazo)
 
END FUNCTION

#obejetivo validar si el derechohabiente tiene resolcion aprobada en spes
FUNCTION fn_resol_aprob()
DEFINE f_id_ret_matriz_derecho    DATE
      ,v_count_ret_matriz_derecho SMALLINT 

      SELECT COUNT(id_datamart), MAX(f_resolucion)
        INTO v_count_ret_matriz_derecho,f_id_ret_matriz_derecho
        FROM  ret_datamart
       WHERE nss           = g_nss
         and f_resolucion >= '01/13/2012'

       #se suprimio la validacion de Ley73
   --SELECT COUNT(id_datamart) ,max(f_inicio_pension)
     --INTO v_count_ret_matriz_derecho,f_id_ret_matriz_derecho
     --FROM ret_datamart a
         --,ret_matriz_derecho b
    --WHERE a.id_derechohabiente = g_id_derechohabiente
      --AND b.tpo_retiro     = 'G'
      --AND TODAY BETWEEN f_inicio_pension AND f_inicio_pension + 10 UNITS YEAR
      --AND a.regimen        = b.regimen
      --AND a.tpo_seguro     = 'TJ'
      --AND a.tpo_pension    = b.tpo_pension
      --AND a.tpo_prestacion = b.tpo_prestacion

 IF v_count_ret_matriz_derecho > 0 THEN
   LET v_count_ret_matriz_derecho = 1
 END IF

 RETURN v_count_ret_matriz_derecho ,f_id_ret_matriz_derecho  
END FUNCTION

#Objetvo valda si existe en tramite solicitud con del derechohabiente 
FUNCTION fn_solicitud_existente()
DEFINE v_count      SMALLINT

--DISPLAY "valida solicitud previas se encuntran en tramite"
 SELECT COUNT(*)
   INTO v_count
   FROM ret_ley73
  WHERE id_derechohabiente = g_id_derechohabiente
    AND estado_solicitud > 5 AND estado_solicitud <= 80 
   
 RETURN v_count
END FUNCTION 