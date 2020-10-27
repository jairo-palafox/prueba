--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => PAG                                                                    #
#PROGRAMA     => PAGC80                                                                 #
#OBJETIVO     => Consulta de pagos en TRM (obtine pagos de excepciones en TRM)          #
#Fecha inicio => 10 de Febrero 2014                                                     #
#Autor        => GERARDO ALFONSO VEGA PAREDES
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv

GLOBALS "PAGG01.4gl"

GLOBALS "PAGW01.inc"

GLOBALS 
      
   DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
          p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
          p_s_titulo          STRING   -- titulo de la ventana 
    
END GLOBALS 

{
======================================================================
Clave: 
Nombre: main
Fecha creacion: febrero 10, 2014
Autor: Gerardo Vega, EFP
Narrativa del proceso que realiza:
Abre la ventana de captura de datos para realizar la consulta de pago TRM

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
GERARDO VEGA    23-jun-14       Se agrego movimiento 681 para aceptar pagos RISS

======================================================================
}

DEFINE v_parametros consultaPago                #Variable para los parametros que se enviaran al WS
DEFINE v_respuesta  respuestaPagosHistoricos    #Variable para "cachar" la respuesta del WS 

MAIN 

   DEFINE
       -- parametros de consulta
       v_nss          LIKE afi_derechohabiente.nss, 
       v_nrp          LIKE cta_his_pagos.nrp,
       v_folio_sua    LIKE cta_his_pagos.folio_sua,
       v_fecha_pago   LIKE cta_his_pagos.f_pago,
       v_periodo_pago LIKE cta_his_pagos.periodo_pago,
       v_bim_pago     LIKE cta_his_pagos.periodo_pago,
       v_imp_ap_pat   LIKE cta_his_pagos.imp_ap_pat,
       v_dato         CHAR(01),
       v_origen       LIKE pag_extractor_preca.origen_cod,
       v_folio        DECIMAL(12,0),    --LIKE cta_his_pagos.folio,
       v_id_referencia LIKE cta_his_pagos.id_referencia,
       v_pp_par       CHAR(06),
       v_pp_non       CHAR(06),
       v_bim_pago_par CHAR(06),
       v_bim_pago_non CHAR(06), 
       v_f_consulta   DATE    ,
       v_detalle      CHAR(60),
       v_ind_liquidacion    SMALLINT,
       v_existe_pago        SMALLINT,
       v_liquida            SMALLINT,
       v_ejecuta_busqueda   SMALLINT,
       v_pago_si            SMALLINT,
       v_id_derechohabiente DECIMAL(9,0),
       v_noDoc              CHAR(09),
       v_noDoc2             CHAR(12),
       v_sua_char           CHAR(06),
       v_pago_no_exitoso    SMALLINT,
       
       v_formulario        ui.Form -- para modificar el formulario

   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_consulta WITH FORM "PAGC801"

   -- se capturan los datos de la consulta
   INPUT BY NAME
      v_nss,
      v_nrp,
      v_folio_sua,
      v_fecha_pago,
      v_periodo_pago,
      v_imp_ap_pat       
   WITHOUT DEFAULTS
   ATTRIBUTES ( UNBUFFERED )
   
      BEFORE INPUT
         -- se obtiene control del formulario
         LET v_formulario = DIALOG.getForm()
         LET v_fecha_pago = TODAY
                  
      AFTER FIELD v_nss
       -- se validan los datos capturados
         IF v_nss IS NULL THEN
            CALL fn_mensaje("Atención","EL NSS no puede ser nulo","stop")
            NEXT FIELD v_nss
         END IF
         
         IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 11 ) THEN
            CALL fn_mensaje("Atención","La longitud del NSS debe ser de 11 caracteres","stop")
            NEXT FIELD v_nss
         END IF        
         CONTINUE INPUT         

      AFTER FIELD v_nrp
         IF v_nrp IS NULL THEN
            CALL fn_mensaje("Atención","El NRP no puede ser nulo","stop")        
            NEXT FIELD v_nrp
         END IF   
         
      AFTER FIELD v_folio_sua
         IF v_folio_sua IS NULL THEN
            CALL fn_mensaje("Atención","El Folio SUA no puede ser nulo","stop") 
            NEXT FIELD v_folio_sua       
         END IF   

      AFTER FIELD v_fecha_pago
         IF v_fecha_pago IS NULL THEN
            CALL fn_mensaje("Atención","La Fecha de pago no puede ser nulo","stop") 
            NEXT FIELD v_fecha_pago       
         END IF   
          
      AFTER FIELD v_periodo_pago
         IF v_periodo_pago IS NULL THEN
            CALL fn_mensaje("Atención","El Bimestre de pago no puede ser nulo","stop") 
            NEXT FIELD v_periodo_pago       
         END IF   
         
         IF v_periodo_pago[5,6] > 6 THEN   
            CALL fn_mensaje("Atención","El Bimestre no puede ser mayor a 6","stop") 
            NEXT FIELD v_periodo_pago                                                
         END IF                            

        NEXT FIELD v_nss

      -- cancelar
      ON ACTION cancel
         EXIT INPUT
         
      ON ACTION accept
         -- se validan los datos capturados
         IF ( v_nss IS NOT NULL AND LENGTH(v_nss CLIPPED) <> 11 ) THEN
            CALL fn_mensaje("Atención","La longitud del NSS debe ser de 11 caracteres","stop")
            CONTINUE INPUT
         END IF

         IF v_nss IS NULL THEN
            CALL fn_mensaje("Atención","EL NSS no puede ser nulo","stop")
            NEXT FIELD v_nss
         END IF
         
         IF v_nrp IS NULL THEN
            CALL fn_mensaje("Atención","El NRP no puede ser nulo","stop")        
            NEXT FIELD v_nrp
         END IF   
         
         IF v_folio_sua IS NULL THEN
            CALL fn_mensaje("Atención","El Folio SUA no puede ser nulo","stop") 
            NEXT FIELD v_folio_sua       
         END IF   

         IF v_fecha_pago IS NULL THEN
            CALL fn_mensaje("Atención","La Fecha de pago no puede ser nulo","stop") 
            NEXT FIELD v_fecha_pago       
         END IF   

         IF v_periodo_pago IS NULL THEN
            CALL fn_mensaje("Atención","El Periodo de pago no puede ser nulo","stop") 
            NEXT FIELD v_periodo_pago       
         END IF   

         IF v_periodo_pago[5,6] > 6 THEN   
            CALL fn_mensaje("Atención","El Bimestre no puede ser mayor a 6","stop") 
            NEXT FIELD v_periodo_pago                                                
         END IF               

---==================================================================================

         LET v_bim_pago        = NULL
         LET v_ind_liquidacion = NULL
         LET v_id_referencia   = NULL
         LET v_id_derechohabiente = NULL
         LET v_folio           = NULL


         LET v_bim_pago_par = NULL
         LET v_pp_par       = NULL
         LET v_bim_pago_non = NULL
         LET v_pp_non       = NULL
         LET v_existe_pago  = 0
         LET v_liquida      = 0
         LET v_pago_si      = 0
         LET v_ejecuta_busqueda = 0
         LET v_pago_no_exitoso = 0

         -- Cero significa que es solo Infonavit y no se convierte a bim_pago 

         DISPLAY "folio_sua:", v_folio_sua
         
         IF v_folio_sua <> 0 THEN  
         --  Busca  el pago como aclaratorio 
   
            CASE v_periodo_pago[5,6]
               WHEN "06" 
                  LET v_pp_par = v_periodo_pago[1,4],"12"
                  LET v_pp_non = v_periodo_pago[1,4],"11"
               WHEN "05" 
                  LET v_pp_par = v_periodo_pago[1,4],"10"
                  LET v_pp_non = v_periodo_pago[1,4],"09"
               WHEN "04" 
                  LET v_pp_par = v_periodo_pago[1,4],"08"
                  LET v_pp_non = v_periodo_pago[1,4],"07"
               WHEN "03" 
                  LET v_pp_par = v_periodo_pago[1,4],"06"
                  LET v_pp_non = v_periodo_pago[1,4],"05" 
               WHEN "02" 
                  LET v_pp_par = v_periodo_pago[1,4],"04"
                  LET v_pp_non = v_periodo_pago[1,4],"03"     
               WHEN "01" 
                  LET v_pp_par = v_periodo_pago[1,4],"02"
                  LET v_pp_non = v_periodo_pago[1,4],"01"
            END CASE

            LET v_bim_pago_par = v_pp_par
            LET v_bim_pago_non = v_pp_non

            DECLARE c_pag CURSOR FOR
                  SELECT det.periodo_pago       ,
                         det.ind_liquidacion    ,
                         det.id_derechohabiente ,
                         det.folio              ,
                         det.id_referencia      
                  FROM   cta_his_pagos det,
                         afi_derechohabiente afi
                  WHERE  afi.nss                = v_nss
                  AND    det.id_derechohabiente = afi.id_derechohabiente
                  AND    det.folio_sua          = v_folio_sua
                  AND    det.f_pago             = v_fecha_pago
                  AND    det.nrp                = v_nrp
                  AND    det.ind_liquidacion <> -1
                  AND    det.periodo_pago       IN (v_bim_pago_non, v_bim_pago_par)
                  ORDER BY det.folio desc,
                           det.id_referencia desc
            FOREACH c_pag INTO v_bim_pago,
                               v_ind_liquidacion,
                               v_id_derechohabiente,
                               v_folio             ,
                               v_id_referencia

               LET v_existe_pago = 1

               SELECT COUNT(*)
                 INTO v_liquida 
                 FROM cta_movimiento mov
                WHERE folio_liquida = v_folio
                  AND id_referencia = v_id_referencia
                  AND id_derechohabiente = v_id_derechohabiente
                  AND subcuenta         IN (4,44,55,60)   
                  AND movimiento        IN (1,61,221,41,51,81,451,681,691,701,711,761,771,791,741,781,811,731,801,831,751,821) 

               IF v_liquida IS NULL OR
                  v_liquida = 0 THEN
                  LET v_pago_no_exitoso = 1
                  CONTINUE FOREACH
               ELSE
                  LET v_ejecuta_busqueda = 1
                  EXIT FOREACH
               END IF
                  
               DISPLAY "Existe como otro pago"
               DISPLAY "Folio otro pago:", v_folio
               DISPLAY "REferencia otro pago:", v_id_referencia
               DISPLAY "ind_liquidacion otro pago:", v_ind_liquidacion
               
               EXIT FOREACH
            END FOREACH
            CLOSE c_pag

            IF v_bim_pago IS NULL THEN       -- REGISTRO EN ACLARATORIO --
               LET v_ind_liquidacion = NULL
               LET v_id_referencia   = NULL
               LET v_id_derechohabiente = NULL
               LET v_folio           = NULL

               DECLARE c_acl CURSOR FOR
                  SELECT det.periodo_pago       ,
                         det.ind_liquidacion    ,
                         com.id_derhab_nuevo    ,
                         det.folio              ,
                         det.id_referencia      
                  FROM   cta_pag_complemento com,
                         cta_his_pagos det,
                         afi_derechohabiente afi
                  WHERE  afi.nss                = v_nss
                  AND    com.id_derhab_nuevo    = afi.id_derechohabiente
                  AND    det.folio              = com.folio
                  AND    det.id_referencia      = com.id_referencia
                  AND    det.folio_sua          = v_folio_sua
                  AND    det.f_pago             = v_fecha_pago
                  AND    det.nrp                = v_nrp
                  AND    det.ind_liquidacion    <> -1
                  AND    det.periodo_pago       IN (v_bim_pago_non, v_bim_pago_par)
                  ORDER BY det.folio desc,
                           det.id_referencia desc
                  
               FOREACH c_acl INTO v_bim_pago,
                                  v_ind_liquidacion,
                                  v_id_derechohabiente,
                                  v_folio             ,
                                  v_id_referencia
                  LET v_existe_pago = 1
        
                       SELECT COUNT(*)
                         INTO v_liquida 
                         FROM cta_movimiento
                        WHERE folio_liquida = v_folio
                          AND id_referencia = v_id_referencia
                       AND id_derechohabiente = v_id_derechohabiente
                          AND subcuenta         IN (4,44,55,60)   
                          AND movimiento        IN (1,61,221,41,51,81,451,681,691,701,711,761,771,791,741,781,811,731,801,831,751,821) 
        
                       IF v_liquida IS NULL OR
                          v_liquida = 0 THEN
                          CONTINUE FOREACH
                       ELSE
                          LET v_ejecuta_busqueda = 1
                           DISPLAY "Existe en Aclaracion"
                           DISPLAY "Folio acl:", v_folio
                           DISPLAY "REferencia acl:", v_id_referencia
                           DISPLAY "ind_liquidacion acl:", v_ind_liquidacion
                           EXIT FOREACH
                      END IF
              END FOREACH
              CLOSE c_acl

            END IF
            
            DISPLAY "indicador liquidacion:", v_ind_liquidacion

            IF v_ind_liquidacion IS NOT NULL THEN
               LET v_existe_pago =  1
            END IF
         ELSE                                        -- PAGO SOLO INFONAVIT POR FOLIO_SUA = 0
            LET v_bim_pago           = v_periodo_pago    
            LET v_ind_liquidacion    = NULL
            LET v_id_referencia      = NULL
            LET v_id_derechohabiente = NULL
            LET v_folio              = NULL
            
            DECLARE c_si CURSOR FOR
               SELECT det.periodo_pago       ,
                      det.ind_liquidacion    ,
                      det.id_derechohabiente ,
                      det.folio              ,
                      det.id_referencia
               FROM   cta_his_pagos det,
                      afi_derechohabiente afi
               WHERE  afi.nss                = v_nss
               AND    det.id_derechohabiente = afi.id_derechohabiente
               AND    det.folio_sua          = v_folio_sua
               AND    det.f_pago             = v_fecha_pago
               AND    det.nrp                = v_nrp
               AND    det.ind_liquidacion <> -1
               AND    det.periodo_pago       = v_bim_pago
               ORDER BY det.folio desc,
                        det.id_referencia desc
               
            FOREACH c_si INTO v_bim_pago,
                              v_ind_liquidacion,
                              v_id_derechohabiente,
                              v_folio             ,
                              v_id_referencia
                              
               LET v_existe_pago = 1
        
               SELECT COUNT(*)
                 INTO v_liquida 
                 FROM cta_movimiento
                WHERE folio_liquida = v_folio
                  AND id_referencia = v_id_referencia
        --250314
        AND id_derechohabiente = v_id_derechohabiente
                  AND subcuenta         IN (4,44,55,60)   
                  AND movimiento        IN (1,61,221,41,51,81,451,681,691,701,711,761,771,791,741,781,811,731,801,831,751,821) 
        
               IF v_liquida IS NULL OR
                  v_liquida = 0 THEN
                  CONTINUE FOREACH
               ELSE
                  LET v_ejecuta_busqueda = 1
                  EXIT FOREACH
               END IF
            END FOREACH
            CLOSE c_si
            
            LET v_pago_si      = 1
 
         END IF

         DISPLAY "existe_pago:", v_existe_pago
         

         LET v_dato = NULL
         LET v_f_consulta = NULL


 --250314 Se creo este IF cuando se trata de SoloINF
 --250314 ya que cuando se captura por primera vez se
 --250314 coloca el folio safre en el folio_sua y cuando 
 --250314 se captura nuevamente ya es diferente el folio_sua

         IF v_folio_sua = 0 THEN 
            SELECT f_consulta
            INTO   v_f_consulta
            FROM   pag_extractor_preca
            WHERE  nss          = v_nss
            AND    nrp          = v_nrp
--250314            AND    folio_sua    = v_folio_sua
            AND    f_pago       = v_fecha_pago
            AND    periodo_pago = v_periodo_pago
            GROUP BY f_consulta
         ELSE
            SELECT f_consulta
            INTO   v_f_consulta
            FROM   pag_extractor_preca
            WHERE  nss          = v_nss
            AND    nrp          = v_nrp
            AND    folio_sua    = v_folio_sua
            AND    f_pago       = v_fecha_pago
            AND    periodo_pago = v_periodo_pago
            GROUP BY f_consulta         
         END IF
            
         DISPLAY "f_consulta:", v_f_consulta
         DISPLAY "bimestre:", v_bim_pago
         DISPLAY "periodo_pago:", v_periodo_pago

         
         IF v_f_consulta IS NOT NULL THEN 
            IF v_f_consulta = TODAY THEN 

               IF v_folio_sua = 0 THEN
                  SELECT TRIM(id_codigo) ||' '||TRIM(id_descripcion)
                  INTO v_detalle
                  FROM pag_extractor_preca 
                  WHERE  nss          = v_nss
                  AND    nrp          = v_nrp
--250314                  AND    folio_sua    = v_folio_sua
                  AND    f_pago       = v_fecha_pago
                  AND    periodo_pago = v_periodo_pago
                  AND    f_consulta   = v_f_consulta
               ELSE
                  SELECT TRIM(id_codigo) ||' '||TRIM(id_descripcion)
                  INTO v_detalle
                  FROM pag_extractor_preca 
                  WHERE  nss          = v_nss
                  AND    nrp          = v_nrp
                  AND    folio_sua    = v_folio_sua
                  AND    f_pago       = v_fecha_pago
                  AND    periodo_pago = v_periodo_pago
                  AND    f_consulta   = v_f_consulta
               END IF 

               CALL fn_mensaje("Atención","Este pago ya fue extraido el día de hoy.\nTRM: "||v_detalle,"stop")      
               NEXT FIELD v_nss
            ELSE
               IF v_folio_sua = 0 THEN
                  UPDATE pag_extractor_preca
                  SET f_consulta = TODAY
                  WHERE nss          = v_nss
                  AND   nrp          = v_nrp
                  AND   f_pago       = v_fecha_pago
                  AND   periodo_pago = v_periodo_pago
               ELSE
                  UPDATE pag_extractor_preca
                  SET f_consulta = TODAY
                  WHERE nss          = v_nss
                  AND   nrp          = v_nrp
                  AND   folio_sua    = v_folio_sua
                  AND   f_pago       = v_fecha_pago
                  AND   periodo_pago = v_periodo_pago               
               END IF
               CALL fn_mensaje("Atención","Pago extraido anteriormente, se incluye nuevamente en el extractor","stop")      
               EXIT INPUT
            END IF
         ELSE 
            DISPLAY "Pago no capturado"
            
--            IF v_existe_pago = 1 THEN   -- antes del 25-abr-2014
            IF v_existe_pago = 1 AND v_ejecuta_busqueda = 0 THEN  -- Existe pago en historio pero no esta liquidado
               DISPLAY "Existe en SAFRE"
               
               IF v_ejecuta_busqueda = 0 AND v_pago_no_exitoso = 0 THEN
                  DISPLAY "Existe en SAFRE en Aclaracion"
                  CALL fn_mensaje("Atención","El Pago se encuentra en Aclaratorio.","stop")      
                  NEXT FIELD v_nss
               ELSE
                  DISPLAY "Existe en SAFRE no exitoso"
                  CALL fn_mensaje("Atención","Pago no exitoso.","stop")      
                  NEXT FIELD v_nss               
               END IF
               
            ELSE
               DISPLAY "No Existe en SAFRE"
            END IF   
            
---==================================================================================

            LET v_origen = NULL
            
--------            LET v_folio  = 0
            
            IF v_ejecuta_busqueda = 1 THEN
               DISPLAY "Busca detalle en SAFRE"

                  IF v_folio_sua = 0 THEN 
                     LET v_origen = 0  -- ORIGEN SAFRE SOLO INFONAVIT
                  ELSE
                     LET v_origen = 1  -- ORIGEN SAFRE LOS QUE NO SON SOLO INFONAVIT
                  END IF
                  
                  CALL fn_consulta_pago_saci(v_nss,v_folio_sua,v_fecha_pago,v_nrp,v_bim_pago,v_periodo_pago,v_folio,v_id_referencia) RETURNING v_respuesta.*,v_folio
                  
                  DISPLAY "Fecha Entrada SACI =", v_respuesta.fechaEntradaTRM
                  DISPLAY "NSS                =", v_respuesta.NSS
                  DISPLAY "NRP                =", v_respuesta.NRP
                  DISPLAY "Folio SUA          =", v_respuesta.folioSUA
                  DISPLAY "Fecha de pago      =", v_respuesta.fechaPago
                  DISPLAY "Importe Pago       =", v_respuesta.importePago            
                  CALL fn_ingresa_datos(v_origen,v_folio,v_folio_sua)
               
                  CALL fn_mensaje("Atención","Registro exitoso en SACI","stop")
                  EXIT INPUT
                  
            ELSE
               DISPLAY "Busca pago en TRM"
               
                  -- Se igualan las variables para ejecutar el WSDL 
                  LET v_parametros.NRP       = v_nrp
                  LET v_parametros.NSS       = v_nss
                  LET v_parametros.fechaPago = v_fecha_pago USING "dd.mm.yyyy"
                  LET v_sua_char = v_folio_sua USING '&&&&&&'
                  LET v_parametros.folioSUA  = v_sua_char      ----v_folio_sua
                  
                  #Se ejecuta la funcion que invoca al WS y se guarda la respuesta en la variable v_respuesta.*
                  #NOTA: Esta funcion se ejecuta por cada registro
                  CALL fn_consulta_pago_trm(v_parametros.*) RETURNING v_respuesta.*

                  #En caso de que se presente algun problema con el WS la funcion regresara los campos:
                     #  v_respuesta.ICodigo        = "-1"
                     #  v_respuesta.IDescripcion   = "DESCRIPCION DEL ERROR"
                  
                  DISPLAY "Respuesta            = ", v_respuesta.ICodigo
                  DISPLAY "Descripcion          = ", v_respuesta.IDescripcion
                  
                  IF v_respuesta.ICodigo <> '-1' THEN  -- -1 significa que se conectó
                  #Consulta exitosa

                     LET v_origen = 2      -- ORIGEN TRM
--                     LET v_noDoc2 = v_respuesta.noDocCtaCorrienteContractual
--                     LET v_noDoc  = v_noDoc2[4,12]
--                     LET v_folio  = v_noDoc
                     LET v_folio = v_respuesta.noDocCtaCorrienteContractual

                     IF v_respuesta.ICodigo = '00' THEN
                        DISPLAY "Fecha Entrada TRM =", v_respuesta.fechaEntradaTRM
                        DISPLAY "NSS               =", v_respuesta.NSS
                        DISPLAY "NRP               =", v_respuesta.NRP
                        DISPLAY "Folio SUA         =", v_respuesta.folioSUA
                        DISPLAY "Fecha de pago     =", v_respuesta.fechaPago
                        DISPLAY "Importe Pago      =", v_respuesta.importePago
                        CALL fn_ingresa_datos(v_origen,v_folio,0)
                        CALL fn_mensaje("Atención","Registro exitoso en TRM ","stop") 
                        EXIT INPUT
                     ELSE
                        CALL fn_mensaje("Atención","Mensaje enviado por TRM: "||v_respuesta.IDescripcion,"stop")
                        CALL fn_ingresa_datos2(v_origen,v_folio,0,v_nss,v_nrp,v_folio_sua,v_fecha_pago,v_periodo_pago) --v_bim_pago)
                     END IF           
                  ELSE
      
                     IF v_respuesta.ICodigo IS NOT NULL THEN
                        #Conexión no exitosa
                        CALL fn_mensaje("Atención","Error de comunicación con servicio de TRM","stop")
                        NEXT FIELD v_nss
                     ELSE
                        #Conexión no exitosa
                        CALL fn_mensaje("Atención","Respuesta de TRM vacia, favor de validar con TRM","stop")
                        NEXT FIELD v_nss 
                     END IF
      
                  END IF 
            END IF
         END IF -- De f_consulta
         
      END INPUT

  CLOSE WINDOW w_consulta
END MAIN

FUNCTION fn_consulta_pago_saci(v_nss,v_folio_sua,v_fecha_pago,v_nrp,v_bim_pago,v_periodo_pago,l_folio, l_id_referencia)
   
   DEFINE v_nss           LIKE afi_derechohabiente.nss, 
          v_nrp           LIKE cta_his_pagos.nrp,
          v_folio_sua     LIKE cta_his_pagos.folio_sua,
          v_fecha_pago    LIKE cta_his_pagos.f_pago,
          v_periodo_pago  LIKE cta_his_pagos.periodo_pago,
          v_importe       LIKE cta_his_pagos.imp_ap_pat,
          v_folio         DECIMAL(12,0),    --LIKE cta_his_pagos.folio,
          v_bim_pago      LIKE cta_his_pagos.periodo_pago,
          l_folio         LIKE cta_his_pagos.folio,
          l_id_referencia LIKE cta_his_pagos.id_referencia

   DEFINE v_fecha DATE

   SELECT imp_ap_pat,
          folio
   INTO   v_importe,
          v_folio
   FROM   cta_his_pagos det, 
          cta_movimiento mov   
   WHERE  det.folio              = l_folio
   AND    det.id_referencia      = l_id_referencia
   AND    det.ind_liquidacion  <> -1
   AND    mov.folio_liquida      = det.folio
   AND    mov.id_referencia      = det.id_referencia
   AND    mov.subcuenta         IN (4,44) 
   AND    mov.movimiento        IN (1,61,221,41,51,81,451,681)

   LET v_respuesta.nss = v_nss
   LET v_respuesta.nrp = v_nrp             
   LET v_respuesta.folioSUA = v_folio_sua

   LET v_respuesta.periodoPago = v_periodo_pago  -- CAPTURADO
   LET v_respuesta.importePago = v_importe

--g-
--g-DISPLAY "gerardo"
--g-DISPLAY 'l_folio   ',l_folio
--g-DISPLAY 'v_folio    ',v_folio
--g-DISPLAY 'l_id_referencai ',l_id_referencia
--g-DISPLAY 'v_importe ',v_importe
--g-DISPLAY 'v_respuesta.importePago ',v_respuesta.importePago   
--g-
   
   LET v_respuesta.fechaPago = v_fecha_pago USING "YYYYMMDD" 
   
   LET v_fecha = TODAY
   LET v_respuesta.fechaEntradaTRM = TODAY USING "YYYYMMDD"
   LET v_respuesta.fechaProceso    = v_respuesta.fechaEntradaTRM

   LET v_respuesta.ICodigo        = "00"
   LET v_respuesta.IDescripcion   = "Exito"
   LET v_respuesta.idConceptoPago = NULL
   LET v_respuesta.noDocCtaCorrienteContractual = NULL

   RETURN v_respuesta.*,v_folio

END FUNCTION

FUNCTION fn_ingresa_datos(v_origen,v_folio,v_sua)

   DEFINE v_origen     LIKE pag_extractor_preca.origen_cod

   DEFINE reg_preca RECORD 
     nss                  char(11),      
     nrp                  varchar(20,0), 
     folio_sua            decimal(6,0),  
     f_pago               date,          
     f_entrada            date,          
     f_proceso            date,          
     id_codigo            varchar(20,0), 
     id_descripcion       varchar(50,0), 
     id_concepto_pago     varchar(20,0), 
     imp_pago             decimal(12,2), 
     no_doc_cta_corriente_contractual   varchar(20,0), 
     periodo_pago         char(6),       
     cod_estado           smallint,      
     origen_cod           smallint,      
     folio                decimal(12,0), 
     f_consulta           DATE  
   END RECORD        
   
   DEFINE v_f_pago CHAR(08)
   DEFINE v_folio  decimal(12,0)   --LIKE cta_his_pagos.folio
   DEFINE v_sua    LIKE cta_his_pagos.folio_sua

   DEFINE v_mes CHAR(02),
          v_dia CHAR(02),
          v_ano CHAR(04)

   LET reg_preca.origen_cod = v_origen
   LET reg_preca.nss        = v_respuesta.nss
   LET reg_preca.nrp        = v_respuesta.nrp              
--   LET reg_preca.folio_sua  = v_respuesta.folioSUA
   
   LET v_f_pago = v_respuesta.fechaPago
   LET v_mes = v_f_pago[5,6]
   LET v_dia = v_f_pago[7,8]
   LET v_ano = v_f_pago[1,4]
   LET reg_preca.f_pago = mdy(v_mes,v_dia,v_ano) 

   LET v_f_pago = v_respuesta.fechaEntradaTRM
   LET v_mes = v_f_pago[5,6]
   LET v_dia = v_f_pago[7,8]
   LET v_ano = v_f_pago[1,4]
   LET reg_preca.f_entrada = mdy(v_mes,v_dia,v_ano)

   LET v_f_pago = v_respuesta.fechaProceso
   LET v_mes = v_f_pago[5,6]
   LET v_dia = v_f_pago[7,8]
   LET v_ano = v_f_pago[1,4]
   LET reg_preca.f_proceso = mdy(v_mes,v_dia,v_ano)
  
   LET reg_preca.id_codigo                        = v_respuesta.ICodigo     
   LET reg_preca.id_descripcion                   = v_respuesta.IDescripcion   
   LET reg_preca.id_concepto_pago                 = v_respuesta.idConceptoPago
   LET reg_preca.imp_pago                         = v_respuesta.importePago         
   LET reg_preca.no_doc_cta_corriente_contractual = v_respuesta.noDocCtaCorrienteContractual
   LET reg_preca.periodo_pago                     = v_respuesta.periodoPago    
   LET reg_preca.cod_estado                       = 2 -- 1=para generar archivo

   LET reg_preca.folio = v_folio
   LET reg_preca.f_consulta = TODAY

   IF v_origen = 1 OR v_origen = 0 THEN  -- origen SACI
      IF v_sua = 0 THEN  -- SUA = 0 --> SOLO INFONAVIT
         LET reg_preca.folio_sua = v_folio
      ELSE 
         LET reg_preca.folio_sua  = v_respuesta.folioSUA   
      END IF
   ELSE
      LET reg_preca.folio_sua  = v_respuesta.folioSUA      
   END IF 
   
--   LET reg_preca.folio_sua  = v_respuesta.folioSUA 
   
  INSERT INTO pag_extractor_preca VALUES (reg_preca.*)

END FUNCTION

FUNCTION fn_ingresa_datos2(v_origen,v_folio,v_sua,v_nss,v_nrp,v_folio_sua,v_fecha_pago,v_bim_pago)

   DEFINE v_origen     LIKE pag_extractor_preca.origen_cod

   DEFINE reg_preca RECORD LIKE pag_extractor_preca.*
   
   DEFINE v_f_pago CHAR(08)
   DEFINE v_folio  DECIMAL(12,0)   --LIKE cta_his_pagos.folio
   DEFINE v_sua    LIKE cta_his_pagos.folio_sua

   DEFINE v_mes CHAR(02),
          v_dia CHAR(02),
          v_ano CHAR(04),
          v_existe SMALLINT

   DEFINE v_nss          LIKE afi_derechohabiente.nss, 
          v_nrp          LIKE cta_his_pagos.nrp,
          v_folio_sua    LIKE cta_his_pagos.folio_sua,
          v_fecha_pago   LIKE cta_his_pagos.f_pago,
          v_bim_pago     LIKE cta_his_pagos.periodo_pago

   LET reg_preca.origen_cod = v_origen
   LET reg_preca.nss        = v_respuesta.nss
   LET reg_preca.nrp        = v_respuesta.nrp              
--   LET reg_preca.folio_sua  = v_respuesta.folioSUA
   

   LET reg_preca.nss = v_nss
   LET reg_preca.nrp = v_nrp
   LET reg_preca.folio_sua = v_folio_sua
   LET reg_preca.f_pago = v_fecha_pago
   LET reg_preca.periodo_pago = v_bim_pago 
   
   LET v_f_pago = v_respuesta.fechaEntradaTRM
   LET v_mes = v_f_pago[5,6]
   LET v_dia = v_f_pago[7,8]
   LET v_ano = v_f_pago[1,4]
   LET reg_preca.f_entrada = mdy(v_mes,v_dia,v_ano)

   LET v_f_pago = v_respuesta.fechaProceso
   LET v_mes = v_f_pago[5,6]
   LET v_dia = v_f_pago[7,8]
   LET v_ano = v_f_pago[1,4]
   LET reg_preca.f_proceso = mdy(v_mes,v_dia,v_ano)
  
   LET reg_preca.id_codigo                        = v_respuesta.ICodigo     
   LET reg_preca.id_descripcion                   = v_respuesta.IDescripcion   
   LET reg_preca.id_concepto_pago                 = v_respuesta.idConceptoPago
   LET reg_preca.imp_pago                         = v_respuesta.importePago         
   LET reg_preca.no_doc_cta_corriente_contractual = v_respuesta.noDocCtaCorrienteContractual
 
   LET reg_preca.cod_estado                       = 2 -- 1=para generar archivo

   LET reg_preca.folio = v_folio
   LET reg_preca.f_consulta = TODAY

   LET v_existe = 0 

   SELECT COUNT(*) 
     INTO v_existe
     FROM pag_extractor_preca
    WHERE nss          = reg_preca.nss
      AND nrp          = reg_preca.nrp
      AND folio_sua    = reg_preca.folio_sua
      AND f_pago       = reg_preca.f_pago
      AND periodo_pago = reg_preca.periodo_pago

   IF v_existe = 0 OR 
      v_existe IS NULL THEN 
      INSERT INTO pag_extractor_preca VALUES (reg_preca.*)
   ELSE
      UPDATE pag_extractor_preca
         SET cod_estado = reg_preca.cod_estado,
             f_consulta = TODAY
       WHERE nss          = reg_preca.nss
         AND nrp          = reg_preca.nrp
         AND folio_sua    = reg_preca.folio_sua
         AND f_pago       = reg_preca.f_pago
         AND periodo_pago = reg_preca.periodo_pago
   END IF
   
END FUNCTION
