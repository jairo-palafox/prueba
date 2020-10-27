
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => ret                                                     #
#Programa          => RETWE09                                                 #
#Objetivo          => ws autoriza de tramite judicial                         #
#Fecha Inicio      => 06-Jun-2012                                             #
###############################################################################

--disparador de ws autoriza ley 73 
--httpdispatch -f as_safreviv_ws8.xcf
--port  9193

IMPORT FGL WSHelper
IMPORT com
IMPORT XML 

DATABASE safre_viv
# 
# USER GLOBALS VARIABLES
#
GLOBALS
  DEFINE 
    ret_genera_autorizacion RECORD
      g_nss              CHAR(11)           --NSS DERECHOHABIENTE
      ,g_id_beneficiario SMALLINT           --1 titular, 2 beneficiario                                    Binario
      ,g_nombre          CHAR(40)           --Nombre beneficiario                                          Texto  
      ,g_ape_pat         CHAR(20)           --Apellido paterno beneficiario                                Texto  
      ,g_ape_mat         CHAR(20)           --Apellido materno beneficiario                                Texto  
      ,g_cve_bancaria    CHAR(18)           --CLABE                                                        Numéric
      ,g_cve_banco       CHAR(18)           --Clave de Banco                                               Numéric
      ,g_entidad         CHAR(2)            --Entidad federativa                                           Numéric
      ,g_f_aut           DATE               --Fecha autorización                                           Texto 
      ,g_tpo_proceso     SMALLINT           --Tipo de grupo de pensionados                                 Numéri
      ,g_f_marca         DATE               --Fecha de marca                                               Texto 
      ,g_cve_afore       SMALLINT           --Clave de AFORE                                               Numéri
      ,g_caso_adai       SMALLINT           --Número de caso en ADAI                                       Numéri
      ,g_num_laudo       SMALLINT           --Número de laudo (grupo 2, 3)                                 Texto 
      ,g_num_junta       SMALLINT           --Número de junta de conciliación y arbitraje (grupos 2 y 3)   Texto 

       
    END RECORD,

    ret_respuesta RECORD
      r_g_nss           CHAR(11)     ,--Número de seguridad social del trabajador
      r_cod_ret         SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones   Numérico 
      r_mensaje_cod_ret CHAR(100)     --Mensaje de código de retorno    Texto 
    END RECORD
    
 DEFINE g_id_derechohabiente DECIMAL(9,0)
        ,g_nss               CHAR(11)           --NSS DERECHOHABIENTE
        ,g_id_beneficiario   SMALLINT           --1 titular, 2 beneficiario                                    Binario
        ,g_nombre            CHAR(40)           --Nombre beneficiario                                          Texto  
        ,g_ape_pat           CHAR(20)           --Apellido paterno beneficiario                                Texto  
        ,g_ape_mat           CHAR(20)           --Apellido materno beneficiario                                Texto  
        ,g_cve_bancaria      CHAR(18)           --CLABE                                                        Numéric
        ,g_cve_banco         CHAR(18)           --Clave de Banco                                               Numéric
        ,g_entidad           CHAR(2)            --Entidad federativa                                           Numéric
        ,g_f_aut             DATE               --Fecha autorización                                           Texto 
        ,g_tpo_proceso       SMALLINT           --Tipo de grupo de pensionados                                 Numéri
        ,g_f_marca           DATE               --Fecha de marca                                               Texto 
        ,g_cve_afore         SMALLINT           --Clave de AFORE                                               Numéri
        ,g_caso_adai         SMALLINT           --Número de caso en ADAI                                       Numéri
        ,g_num_laudo         SMALLINT           --Número de laudo (grupo 2, 3)                                 Texto 
        ,g_num_junta         SMALLINT           --Número de junta de conciliación y arbitraje (grupos 2 y 3)   Texto 

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
  # Check arguments
  #
  --fglrun RETWE09
  IF num_args() = 2 AND arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateRechazoService(TRUE)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 AND arg_val(1) = "-S" THEN
      LET SCREEN = TRUE
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "RETWE010" ATTRIBUTES(TEXT = "Autoriza Ley 73", STYLE="naked")
      display_status(" Autoriza Ley 73 Startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM
      END IF
    END IF
  END IF
  
  #
  # Create  Autoriza Ley 73
  #
  CALL CreateRechazoService(FALSE)
    
  #
  # Start the server
  #
  display_status("Starting server...")
  #
  # Starts the server on the port number specified by the FGLAPPSERVER environment variable
  #  (EX: FGLAPPSERVER=9192)
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
    LET serv = com.WebService.CreateWebService("AutorizaLey73",serviceNS)
  
    #
    # Publish the functions
    #
    
    # fn_autoriza_ley73
    LET op = com.WebOperation.CreateRPCStyle("fn_autoriza_ley73","fn_autoriza_ley73",ret_genera_autorizacion,ret_respuesta)
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
      display_status(" Autoriza Ley 73 registered")
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

#Objetivo recoleccion de infomacion y control de validacones
FUNCTION fn_autoriza_ley73()
    
    LET g_nss                = ret_genera_autorizacion.g_nss
    LET g_id_beneficiario    = ret_genera_autorizacion.g_id_beneficiario
    LET g_nombre             = ret_genera_autorizacion.g_nombre
    LET g_ape_pat            = ret_genera_autorizacion.g_ape_pat
    LET g_ape_mat            = ret_genera_autorizacion.g_ape_mat
    LET g_cve_bancaria       = ret_genera_autorizacion.g_cve_bancaria
    LET g_cve_banco          = ret_genera_autorizacion.g_cve_banco
    LET g_entidad            = ret_genera_autorizacion.g_entidad
    LET g_f_aut              = ret_genera_autorizacion.g_f_aut
    LET g_tpo_proceso        = ret_genera_autorizacion.g_tpo_proceso
    LET g_f_marca            = ret_genera_autorizacion.g_f_marca
    LET g_cve_afore          = ret_genera_autorizacion.g_cve_afore
    LET g_caso_adai          = ret_genera_autorizacion.g_caso_adai
    LET g_num_laudo          = ret_genera_autorizacion.g_num_laudo
    LET g_num_junta          = ret_genera_autorizacion.g_num_junta
    
    CALL fn_validaciones()
END FUNCTION 

#Objetivo fn lanzadora de las validaciones del modulo
FUNCTION fn_validaciones() 
DEFINE v_bnd_autoriza SMALLINT 
           --25      Todos los campos deben tener datos, son obligatorios
           --26      Fecha incorrecta
           --36      Error en datos bancarios
           --50      La entidad debe estar entre 01 y 33
           --51      El NSS no tiene estatus en proceso
           --70      Número de Laudo inválido
           --71      Número de Junta inválido
           --72      Clabe bancaria difiere de la clave de banco
   LET v_bnd_autoriza = 0 

   IF    g_nss                IS NULL    OR g_id_beneficiario    IS NULL
      OR g_nombre             IS NULL    OR g_ape_pat            IS NULL
      OR g_ape_mat            IS NULL    OR g_cve_bancaria       IS NULL
      OR g_cve_banco          IS NULL    OR g_entidad            IS NULL
      OR g_f_aut              IS NULL    OR g_tpo_proceso        IS NULL
      OR g_f_marca            IS NULL    OR g_cve_afore          IS NULL
      OR g_caso_adai          IS NULL    OR g_num_laudo          IS NULL
      OR g_num_junta          IS NULL    THEN
      DISPLAY "todos los campos son obligatorios"
       CALL fn_rechazo(25)
       LET v_bnd_autoriza = 1
    END IF 

    IF g_f_aut IS NULL OR g_f_marca IS NULL THEN 
      DISPLAY "No se que fecha debe de validar estoy tomando la de la autorizacion" 
      CALL fn_rechazo(26)
      LET v_bnd_autoriza = 1
    END IF

    IF  fn_val_datos_bancarios() THEN
       DISPLAY "Error datos bancarios"
       CALL fn_rechazo(36)
       LET v_bnd_autoriza = 1
    END IF  
    
    IF NOT (g_entidad >= 01 AND g_entidad <= 33) THEN
       DISPLAY "La entidad esta fuera de limites permitidos 1-33 "
       CALL fn_rechazo(50)
       LET v_bnd_autoriza = 1
    END IF 

    IF fn_valida_nss_proceso() THEN
       DISPLAY "el nss esta en proceso"
       CALL fn_rechazo(51)
       LET v_bnd_autoriza = 1
    END IF 

    IF fn_val_laudo() THEN 
        DISPLAY "error Laudo Erroroneo"
       CALL fn_rechazo(70)
       LET v_bnd_autoriza = 1
    END IF

    IF fn_val_junta() THEN 
        DISPLAY "error junta Erroronea"
       CALL fn_rechazo(71)
       LET v_bnd_autoriza = 1
    END IF  

    IF v_bnd_autoriza = 0 THEN
     CALL fn_genera_autorizacion()
   END IF  
END FUNCTION

#Objetivos Valda la informacion del laudo
FUNCTION fn_val_laudo()
  --g_num_laudo
  DISPLAY "entro a validacion de laudo"
  RETURN  0
END FUNCTION

#Objetivos Valida la informacion de la junta
FUNCTION fn_val_junta()
  --g_num_junta
  DISPLAY "entro a validacion de junta"
  RETURN  0
END FUNCTION
 
#Objetivo valida que no exista el nss en proceso
 FUNCTION fn_valida_nss_proceso()
  DISPLAY "entro a validacion de nss en proceso"
  RETURN 0 
END FUNCTION

#Objetvo validacion de la informacioin bancaria
FUNCTION fn_val_datos_bancarios()
   DISPLAY "validaciion de datos bancarios"
  RETURN 0
END FUNCTION 

#Objetivo funcion Generica para rechazos
FUNCTION fn_rechazo(v_cod)
    DEFINE v_cod SMALLINT  

    LET ret_respuesta.r_g_nss   = g_nss
    LET ret_respuesta.r_cod_ret = v_cod

    IF ret_respuesta.r_cod_ret IS NULL THEN
        LET ret_respuesta.r_mensaje_cod_ret = "Codigo no existente"
    ELSE  
         SELECT desc_larga 
           INTO ret_respuesta.r_mensaje_cod_ret
           FROM   ret_codigo_retorno
          WHERE cod_retorno = ret_respuesta.r_cod_ret

          IF ret_respuesta.r_mensaje_cod_ret IS NULL THEN
             LET ret_respuesta.r_mensaje_cod_ret = "No cargados"
          END IF 
    END IF
END FUNCTION

#Objetivo instrucciones para generar la atorizacion
FUNCTION fn_genera_autorizacion()
DEFINE v_query        STRING
DEFINE v_err_sql      SMALLINT 

     SELECT id_derechohabiente
         INTO  g_id_derechohabiente
         FROM afi_derechohabiente 
         WHERE nss = g_nss

               DISPLAY
                             g_id_derechohabiente 
                            ," , ",g_id_beneficiario
                            ," , ",g_nombre
                            ," , ",g_ape_pat
                            ," , "," ",g_ape_mat   
                            ," , ",g_cve_bancaria  
                            ," , ",g_cve_banco     
                            ," , ",g_entidad       
                            ," , '",g_f_aut         
                            ,"' , '",g_f_marca       
                            ,"' , ",g_cve_afore     
                            ," , ",g_caso_adai     
                            ," , ",g_num_laudo     
                            ," , ",g_num_junta
                        
   LET  v_query = "EXECUTE FUNCTION fn_insert_autoriza_tramite(?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE fn_autoriza FROM v_query
   EXECUTE fn_autoriza USING g_id_derechohabiente 
                            ,g_id_beneficiario
                            ,g_nombre
                            ,g_ape_pat
                            ,g_ape_mat
                            ,g_cve_bancaria
                            ,g_cve_banco
                            ,g_entidad
                            ,g_f_aut
                            ,g_f_marca
                            ,g_cve_afore
                            ,g_caso_adai
                            ,g_num_laudo
                            ,g_num_junta
                        INTO v_err_sql

    DISPLAY v_err_sql

    DISPLAY "Genera Autorizacion"
    LET ret_respuesta.r_g_nss   = g_nss
    LET ret_respuesta.r_cod_ret = 0

    IF ret_respuesta.r_cod_ret IS NULL THEN
        LET ret_respuesta.r_mensaje_cod_ret = "Codigo no existente"
    ELSE  
         SELECT desc_larga 
           INTO ret_respuesta.r_mensaje_cod_ret
           FROM   ret_codigo_retorno
          WHERE cod_retorno = ret_respuesta.r_cod_ret

          IF ret_respuesta.r_mensaje_cod_ret IS NULL THEN
             LET ret_respuesta.r_mensaje_cod_ret = "No cargados"
          END IF 
    END IF
    
 
END FUNCTION  


