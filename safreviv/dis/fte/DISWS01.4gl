################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 02/09/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISWS01                                                  #
#Métodos           => consultaPagosSaci - Consulta los pagos de SACI           #
#Objetivo          => Ficha Servicio Web SACI-TRM caso regla 27                #
#Fecha inicio      => 02/09/2015                                               #
################################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

# 
# RECORD VARIABLES
#

GLOBALS
# Variables de entrada consultaPagosSaci
DEFINE ConsultaPagosRequestVO RECORD
  nrp CHAR(11),
  bimestre CHAR(6),
  nss CHAR(11)
END RECORD

# Variables generales de salida consultaPagosSaci
DEFINE ConsultaPagosResponseVO RECORD
  numeroRegistroPago INTEGER,
  nss CHAR(11),
  nrp CHAR(11),
  bimestre CHAR(6),
  codigoRetorno INTEGER, 
  mensaje CHAR(70)
END RECORD       

# Variables de detalle de montos consultaPagosSaci
DEFINE DetalleMontosVO DYNAMIC ARRAY OF RECORD
  monto DECIMAL(12,2),
  fechaPagoPatron DATE, 
  tipoMov INTEGER
END RECORD 

END GLOBALS

DEFINE serverURL    STRING
DEFINE show_stat    BOOLEAN          

&define display_status(status) \
  IF NOT show_stat THEN \
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

  LET show_stat = false
  #
  # Check arguments
  #
  IF num_args() = 2 and arg_val(1) = "-W" THEN
      LET serverURL = arg_val(2)
      CALL CreateDISWS01Service(true)
      EXIT PROGRAM
  ELSE 
    IF num_args() = 2 and arg_val(1) = "-S" THEN
      LET show_stat = true
      CALL fgl_setenv("FGLAPPSERVER",arg_val(2))
      CLOSE WINDOW SCREEN
      OPEN WINDOW w WITH FORM "serverStatus" ATTRIBUTES(TEXT = "DISWS01 service", STYLE="naked")
      display_status("DISWS01 service startup")
    ELSE
      IF num_args() <> 0 THEN
        CALL exitHelp()
        EXIT PROGRAM    
      END IF
    END IF
  END IF

  #
  # Create the service
  #
  CALL CreateDISWS01Service(false)
    
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

  IF show_stat THEN
    MENU
      ON idle 1
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
      ON ACTION close
        EXIT program
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
      IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
      END IF     
    END WHILE
    DISPLAY "Server stopped"
  END IF
END MAIN

#
# Create CreateDISWS01Service  service
#
FUNCTION CreateDISWS01Service(generateWSDL)
  DEFINE serv                   com.WebService       # WebService
  DEFINE op_consultaPagosSaci   com.WebOperation     # Operation of a WebService  
  DEFINE serviceNS    STRING
  DEFINE generateWSDL SMALLINT
  DEFINE ret          INTEGER

  LET serviceNS       = "http://tempuri.org/"

  TRY
  
    #
    # Create DISWS01 Web Service
    #
    LET serv = com.WebService.CreateWebService("ConsultaPagosSaci",serviceNS)
  
    #
    # Publish the functions
    #
    
    # Phone operation
    LET op_consultaPagosSaci = com.WebOperation.CreateDOCStyle("consultaPagosSaci","consultaPagosSaci",ConsultaPagosRequestVO,ConsultaPagosResponseVO) 
    CALL serv.publishOperation(op_consultaPagosSaci,NULL)
    
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
      display_status("DISWS01 Service registered")
    END IF
    
  CATCH
    display_status("Unable to create 'DISWS01' Web Service :"||STATUS)
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

FUNCTION consultaPagosSaci() 

   DEFINE v_id_derechohabiente DECIMAL(9,0)
   DEFINE v_ano_pago           INTEGER
   DEFINE v_bimestre_pago      INTEGER
   DEFINE v_con_mon_apo        INTEGER
   DEFINE v_con_mon_amo        INTEGER 
   DEFINE v_mon_apo            DECIMAL(12,2)
   DEFINE v_mon_amo            DECIMAL(12,2)
   DEFINE v_f_pago             DATE
   DEFINE gr_sql               STRING
   DEFINE v_indice             INTEGER
   
   DISPLAY "\n ------------------------- consultaPagosSaci -------------------------"
   DISPLAY "\n nrp: -",ConsultaPagosRequestVO.nrp,"-"
   DISPLAY "\n bimestre: -",ConsultaPagosRequestVO.bimestre,"-"
   DISPLAY "\n nss: -",ConsultaPagosRequestVO.nss,"-"

   -- Validaciones
   IF LENGTH(ConsultaPagosRequestVO.nrp) <> 11 THEN
     DISPLAY "2. El Número de Registro Patronal no contiene 10 caracteres."
     LET ConsultaPagosResponseVO.codigoRetorno = 2
     LET ConsultaPagosResponseVO.mensaje = "El Número de Registro Patronal no contiene 10 caracteres."      
     RETURN
   END IF 
   
   -- Valida el número de caracteres del periodo
   IF LENGTH(ConsultaPagosRequestVO.bimestre) <> 6 THEN
   	 DISPLAY "3. El Bimestre no contiene 6 caracteres."
   	 LET ConsultaPagosResponseVO.codigoRetorno = 3
   	 LET ConsultaPagosResponseVO.mensaje = "El Bimestre no contiene 6 caracteres."   	  
   	 RETURN
   	 
   	 --Valida la estructura del periodo    	   	 
   	 LET v_ano_pago      = ConsultaPagosRequestVO.bimestre[1,4]
     LET v_bimestre_pago = ConsultaPagosRequestVO.bimestre[5,6]
  
     IF v_ano_pago < 1900 AND v_ano_pago > YEAR(TODAY)THEN        
       DISPLAY "4. El Bimestre no es válido."	
       LET ConsultaPagosResponseVO.codigoRetorno = 4
   	   LET ConsultaPagosResponseVO.mensaje = "El Bimestre no es válido."   	   
   	   RETURN
     ELSE   	
       IF v_bimestre_pago < 1 AND v_bimestre_pago > 6 THEN 
    	   DISPLAY "4. El Bimestre no es válido."	
         LET ConsultaPagosResponseVO.codigoRetorno = 4
   	     LET ConsultaPagosResponseVO.mensaje = "El Bimestre no es válido."   	     
   	     RETURN       
       END IF
     END IF        	 	    	 	     
   END IF

   -- Valida el número de caracteres del NSS.
   IF LENGTH(ConsultaPagosRequestVO.nss) <> 11 THEN
     DISPLAY "5. El Número de Seguridad Social no contiene 11 caracteres."
     LET ConsultaPagosResponseVO.codigoRetorno = 5
     LET ConsultaPagosResponseVO.mensaje = "El Número de Seguridad Social no contiene 11 caracteres."       
     RETURN   
   END IF
   
   LET gr_sql = "\n SELECT id_derechohabiente ",
                "\n   FROM afi_derechohabiente ",
                "\n  WHERE nss = ? "   
   
   PREPARE ps_sl_ad FROM gr_sql
   EXECUTE ps_sl_ad USING ConsultaPagosRequestVO.nss                            

   IF SQLCA.SQLCODE = 100 THEN   
   	 DISPLAY "6. El Número de Seguridad Social no está registrado en la Base de Datos."
     LET ConsultaPagosResponseVO.codigoRetorno = 6
     LET ConsultaPagosResponseVO.mensaje = "El Número de Seguridad Social no está registrado en la Base de Datos."     
     RETURN     
   ELSE 
   	 IF SQLCA.SQLCODE < 0 THEN    	 	 
   	 	 DISPLAY "7. Error al obtener información de la Base de Datos."
   	 	 DISPLAY "\n Valida NSS en BD: -",gr_sql,"-"
       DISPLAY "\n NSS: -",ConsultaPagosRequestVO.nss,"-"
       RETURN
   	 END IF
   END IF	
   
   INITIALIZE gr_sql TO NULL
   INITIALIZE v_con_mon_apo TO NULL
   INITIALIZE v_con_mon_amo TO NULL
   INITIALIZE v_id_derechohabiente TO NULL

   -- Busca la información general en dis_det_avance_pago
   LET gr_sql = "\n   SELECT COUNT(dd.monto_aportacion), ",
                "\n          COUNT(dd.monto_amortizacion), ",
                "\n          af.nss, ",
                "\n          dd.nrp, ",
                "\n          dd.periodo_pago, ",
                "\n          af.id_derechohabiente ",              
                "\n     FROM dis_det_avance_pago dd, ", 
                "\n          afi_derechohabiente af ",
                "\n    WHERE af.id_derechohabiente = dd.id_derechohabiente ",
                "\n      AND (dd.monto_aportacion <> 0 OR dd.monto_amortizacion <> 0) ",
                "\n      AND dd.nrp = ? ",
                "\n      AND dd.periodo_pago = ? ",
                "\n      AND af.nss = ? ",             
                "\n GROUP BY dd.nss, ",
                "\n          dd.nrp, ",
                "\n          dd.periodo_pago, ",
                "\n          af.id_derechohabiente "
             
   DISPLAY "Información general: ", gr_sql
   
   PREPARE ps_sl_ddap FROM gr_sql
   EXECUTE ps_sl_ddap 
     USING ConsultaPagosRequestVO.nrp, ConsultaPagosRequestVO.bimestre, ConsultaPagosRequestVO.nss
      INTO v_con_mon_apo, 
           v_con_mon_amo, 
           ConsultaPagosResponseVO.nss, 
           ConsultaPagosResponseVO.nrp, 
           ConsultaPagosResponseVO.bimestre,
           v_id_derechohabiente
           
   LET ConsultaPagosResponseVO.numeroRegistroPago = v_con_mon_apo + v_con_mon_amo
      
   {IF SQLCA.SQLCODE = 100 THEN   
   	 DISPLAY "1. No se encontró información con los criterios especificados."
     LET ConsultaPagosResponseVO.codigoRetorno = 1
     LET ConsultaPagosResponseVO.mensaje = "No se encontró información con los criterios especificados."
     RETURN     
   ELSE }
   	 IF SQLCA.SQLCODE < 0 THEN    	 	 
   	 	 DISPLAY "7. Error al obtener información de la Base de Datos."
   	 	 DISPLAY "\n Información general: -",gr_sql,"-"
       DISPLAY "\n NRP: -",ConsultaPagosRequestVO.nrp,"-"
       DISPLAY "\n PERIODO DE PAGO: -",ConsultaPagosRequestVO.bimestre,"-" 
       DISPLAY "\n NSS: -",ConsultaPagosRequestVO.nss,"-"
       RETURN
   	 END IF
   {END IF}
   
   INITIALIZE gr_sql TO NULL
   
   
   -- Busca la información general en dis_interface_hs
   LET gr_sql = "\n   SELECT COUNT(dd.imp_ap_pat), ",
                "\n          COUNT(dd.imp_am_cre), ",
                "\n          af.nss, ",
                "\n          dd.nrp, ",
                "\n          dd.periodo_pago, ",
                "\n          af.id_derechohabiente ",              
                "\n     FROM dis_interface_hs dd, ", 
                "\n          afi_derechohabiente af ",
                "\n    WHERE af.id_derechohabiente = dd.id_derechohabiente ",
                "\n      AND (dd.imp_ap_pat <> 0 OR dd.imp_am_cre <> 0) ",
                "\n      AND dd.nrp = ? ",
                "\n      AND dd.periodo_pago = ? ",
                "\n      AND af.nss = ? ",             
                "\n GROUP BY dd.nss, ",
                "\n          dd.nrp, ",
                "\n          dd.periodo_pago, ",
                "\n          af.id_derechohabiente "
             
   DISPLAY "Información general: ", gr_sql
   
   PREPARE ps_sl_di FROM gr_sql
   EXECUTE ps_sl_di 
     USING ConsultaPagosRequestVO.nrp, ConsultaPagosRequestVO.bimestre, ConsultaPagosRequestVO.nss
      INTO v_con_mon_apo, 
           v_con_mon_amo, 
           ConsultaPagosResponseVO.nss, 
           ConsultaPagosResponseVO.nrp, 
           ConsultaPagosResponseVO.bimestre,
           v_id_derechohabiente
           
   LET ConsultaPagosResponseVO.numeroRegistroPago = v_con_mon_apo + v_con_mon_amo
      
   {IF SQLCA.SQLCODE = 100 THEN   
   	 DISPLAY "1. No se encontró información con los criterios especificados."
     LET ConsultaPagosResponseVO.codigoRetorno = 1
     LET ConsultaPagosResponseVO.mensaje = "No se encontró información con los criterios especificados."
     RETURN     
   ELSE}
   	 IF SQLCA.SQLCODE < 0 THEN    	 	 
   	 	 DISPLAY "7. Error al obtener información de la Base de Datos."
   	 	 DISPLAY "\n Información general: -",gr_sql,"-"
       DISPLAY "\n NRP: -",ConsultaPagosRequestVO.nrp,"-"
       DISPLAY "\n PERIODO DE PAGO: -",ConsultaPagosRequestVO.bimestre,"-" 
       DISPLAY "\n NSS: -",ConsultaPagosRequestVO.nss,"-"
       RETURN
   	 END IF
   {END IF}
   
   IF (v_con_mon_apo = 0 OR v_con_mon_apo IS NULL) AND (v_con_mon_amo = 0 or v_con_mon_amo = 0) THEN
     DISPLAY "1. No se encontró información con los criterios especificados."
     LET ConsultaPagosResponseVO.codigoRetorno = 1
     LET ConsultaPagosResponseVO.mensaje = "No se encontró información con los criterios especificados."
     RETURN
   END IF   
   
   INITIALIZE gr_sql TO NULL
   INITIALIZE v_mon_apo TO NULL
   INITIALIZE v_mon_amo TO NULL
   INITIALIZE v_f_pago  TO NULL
   
   
   --Busca el detalle de los montos en dis_det_avance_pago
   LET gr_sql = "\n SELECT monto_aportacion, ", 
                "\n        monto_amortizacion, ",
                "\n        f_pago ",   
                "\n   FROM dis_det_avance_pago ",
                "\n  WHERE nrp = '",ConsultaPagosRequestVO.nrp,"'",
                "\n    AND periodo_pago = '",ConsultaPagosRequestVO.bimestre,"'",
                "\n    AND id_derechohabiente = ",v_id_derechohabiente,
                "\n    AND (monto_aportacion <> 0 OR  monto_amortizacion <> 0) "              
  
   DISPLAY "Información detalle dis_det_avance_pago: ", gr_sql
   
   PREPARE ps_sl_dd FROM gr_sql
   DECLARE cr_sl_dd CURSOR FOR ps_sl_dd 
   	
   IF SQLCA.SQLCODE = 100 THEN   
   	 DISPLAY "1. No se encontró información con los criterios especificados."
     LET ConsultaPagosResponseVO.codigoRetorno = 1
     LET ConsultaPagosResponseVO.mensaje = "No se encontró información con los criterios especificados."
     RETURN     
   ELSE 
   	 IF SQLCA.SQLCODE < 0 THEN    	 	 
   	 	 DISPLAY "7. Error al obtener información de la Base de Datos."
   	 	 DISPLAY "\n Información detalle: -",gr_sql,"-"
       DISPLAY "\n NRP: -",ConsultaPagosRequestVO.nrp,"-"
       DISPLAY "\n PERIODO DE PAGO: -",ConsultaPagosRequestVO.bimestre,"-" 
       DISPLAY "\n ID DERECHOHABIENTE: -",v_id_derechohabiente,"-"
       RETURN
   	 END IF
   END IF	
    
   LET v_indice = 1   
   FOREACH cr_sl_dd INTO v_mon_apo, v_mon_amo, v_f_pago   	   
   
      -- Montos tipoMov = 3 (avance de amortizacion) 
     IF v_mon_amo > 0 THEN
      	 DISPLAY "tipoMov = 3 (avance de amortizacion)"
      	 LET DetalleMontosVO[v_indice].monto = v_mon_amo 
         LET DetalleMontosVO[v_indice].fechaPagoPatron = v_f_pago
         LET DetalleMontosVO[v_indice].tipoMov = 3                
         LET v_indice = v_indice + 1
     END IF
      
      -- Total de montos tipoMov = 1 (avance de aportación)
      IF ConsultaPagosRequestVO.bimestre > 200505 AND v_mon_apo > 0.00 THEN 
      	 DISPLAY "tipoMov = 1 (avance de aportación)"
         LET DetalleMontosVO[v_indice].monto = v_mon_apo 
         LET DetalleMontosVO[v_indice].fechaPagoPatron = v_f_pago
         LET DetalleMontosVO[v_indice].tipoMov = 1         
         LET v_indice = v_indice + 1        
      ELSE
      	  -- Total de montos tipoMov = 2 (pagos virtuales)
      	  IF ConsultaPagosRequestVO.bimestre <= 200505 AND v_mon_apo > 0.00 THEN
      	    DISPLAY "tipoMov = 2 (pagos virtuales)"
      	    LET DetalleMontosVO[v_indice].monto = v_mon_apo 
            LET DetalleMontosVO[v_indice].fechaPagoPatron = v_f_pago
            LET DetalleMontosVO[v_indice].tipoMov = 2  
            LET v_indice = v_indice + 1
      	  END IF       
      END IF            
   END FOREACH
   
      
   --Busca el detalle de los montos en dis_interface_hs                
   LET gr_sql = "\n SELECT imp_ap_pat, ",
                "\n        imp_am_cre, ", 
                "\n        f_pago ", 
                "\n   FROM dis_interface_hs ", 
                "\n  WHERE nrp = '",ConsultaPagosRequestVO.nrp,"'",
                "\n    AND periodo_pago = '",ConsultaPagosRequestVO.bimestre,"'",
                "\n    AND id_derechohabiente = ",v_id_derechohabiente,
                "\n    AND (imp_ap_pat <> 0 OR imp_am_cre <> 0) " 
                
   DISPLAY "Información detalle cta_his_pagos: ", gr_sql
   
   PREPARE ps_sl_chp FROM gr_sql
   DECLARE cr_sl_chp CURSOR FOR ps_sl_chp 
   	
   IF SQLCA.SQLCODE < 0 THEN    	 	 
   	  DISPLAY "7. Error al obtener información de la Base de Datos."
   	 	DISPLAY "\n Información detalle cta_his_pagos: -",gr_sql,"-"
      DISPLAY "\n NRP: -",ConsultaPagosRequestVO.nrp,"-"
      DISPLAY "\n PERIODO DE PAGO: -",ConsultaPagosRequestVO.bimestre,"-" 
      DISPLAY "\n ID DERECHOHABIENTE: -",v_id_derechohabiente,"-"
      RETURN   	 
   END IF	
    
   LET v_indice = 1   
   FOREACH cr_sl_chp INTO v_mon_apo, v_mon_amo, v_f_pago   	                   
   	  -- Montos tipoMov = 3 (avance de amortizacion) 
      IF v_mon_apo > 0 THEN
       	 DISPLAY "tipoMov = 4 (pagos)"
       	 LET DetalleMontosVO[v_indice].monto = v_mon_apo 
         LET DetalleMontosVO[v_indice].fechaPagoPatron = v_f_pago
         LET DetalleMontosVO[v_indice].tipoMov = 4                
         LET v_indice = v_indice + 1
      END IF
      IF v_mon_amo > 0 THEN 
      	 DISPLAY "tipoMov = 5 (amortizaciones)"
       	 LET DetalleMontosVO[v_indice].monto = v_mon_amo 
         LET DetalleMontosVO[v_indice].fechaPagoPatron = v_f_pago
         LET DetalleMontosVO[v_indice].tipoMov = 5                
         LET v_indice = v_indice + 1
      END IF   	
   END FOREACH                                 
  
   LET v_indice = v_indice - 1
   CALL DetalleMontosVO.deleteElement(DetalleMontosVO.getLength())
      
   LET ConsultaPagosResponseVO.codigoRetorno = 0	
   LET ConsultaPagosResponseVO.mensaje = "Consulta exitosa."
   
   RETURN          				
END FUNCTION