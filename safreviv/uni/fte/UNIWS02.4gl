#-------------------------------------------------------------------------------
# File: UNIWS02Service.4gl
# GENERATED BY fglwsdl 
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------
#export FGLAPPSERVER=9122
#FORMA DE EJECUTAR http://172.16.16.204:9122/ConsultaUnificacionCreditoServices?wsdl
#http://10.90.8.132/UnificacionCredito/ws/r/ConsultaUnificacionCreditoServices?wsdl
#fastcgidispatch -s -f ConsultaUnificacionCreditoServices.xcf 

IMPORT FGL WSHelper

IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "UNIWS02.inc"

#-------------------------------------------------------------------------------
# Service: ConsultaUnificacionCreditoServicesService
# Port:    ConsultaUnificacionCreditoServices
#-------------------------------------------------------------------------------
#
# FUNCTION CreateUNIWS02 Service
MAIN
   DEFINE servicio     INTEGER
   DEFINE respuesta    INTEGER 
   
   CALL CreateConsultaUnificacionCreditoServices()
        RETURNING servicio --- funcion propia

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      #
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      #
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      
      CASE respuesta
         WHEN 0
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

      IF int_flag <> 0 THEN
         LET int_flag = 0
         EXIT WHILE
      END IF     
   END WHILE
END MAIN
#
FUNCTION CreateConsultaUnificacionCreditoServices()
   DEFINE service      com.WebService
   DEFINE operation    com.WebOperation
   
   TRY
     # Create Web Service
     LET service = com.WebService.CreateWebService("ConsultaUnificacionCreditoServices",
                                                   "http://consultaUnificacionCredito.efp.com") --- nombre de servicio como se publica
   
     #
     # Operation: consultarMovimientos
     #
   
     # Publish Operation : consultar Unificacion de cuentas - Informacion Creditos
     LET operation = com.WebOperation.CreateDOCStyle("consultarUnificacionCreditos" ,       --- nombre de la funcion que se publica
                                                     "consultarUnificacionCreditos",        --- nombre de funcion que se implementa
                                                     ns2unificacionCreditosRequest,         --- variable de parametros de entrada
                                                     ns2consultarUnificacionCreditosReturn) -- variable de salida
                                                     
     CALL service.publishOperation(operation,"")
   
   
     #
     # Register Service
     #
     CALL com.WebServiceEngine.RegisterService(service)
     RETURN 0
   
   CATCH
     RETURN STATUS
   END TRY

END FUNCTION
#
FUNCTION consultarUnificacionCreditos()

   DEFINE v_query_nss     STRING 
   DEFINE v_nss           CHAR(11)
   DEFINE v_query_con     STRING 
   DEFINE v_query_con1    STRING 

   DEFINE v_id_derechohabiente DECIMAL(9,0),
          v_credito            DECIMAL(10,0),
          v_estado_credito     CHAR(30),
          v_tipo_unificacion   CHAR(30),
          v_marca              SMALLINT,
          v_marca_activa       SMALLINT,
          v_count_marca        SMALLINT,
          v_count_marca_his    SMALLINT,
          v_marca_his          SMALLINT,
          v_cont               SMALLINT,
          v_fecha_marca        CHAR(10)
    
   LET v_query_nss="SELECT nss FROM afi_derechohabiente WHERE nss = ? "
   
   PREPARE prp_nss FROM v_query_nss
   EXECUTE prp_nss USING ns2unificacionCreditosRequest.nss INTO v_nss
   
   INITIALIZE ns2consultarUnificacionCreditosReturn.* TO NULL
   
   --Se evalua que exista NSS
   IF v_nss IS NULL THEN --No existe NSS
      
      LET ns2consultarUnificacionCreditosReturn.nss_unificador  = ns2unificacionCreditosRequest.nss            
      LET ns2consultarUnificacionCreditosReturn.marca_activa    = 0
      LET ns2consultarUnificacionCreditosReturn.marca_historica = 0 
      LET ns2consultarUnificacionCreditosReturn.fecha_marca     = ""
      
      LET ns2consultarUnificacionCreditosReturn.nombre_dor  = "" 
      LET ns2consultarUnificacionCreditosReturn.paterno_dor = "" 
      LET ns2consultarUnificacionCreditosReturn.materno_dor = "" 
      LET ns2consultarUnificacionCreditosReturn.curp_dor    = ""    
      LET ns2consultarUnificacionCreditosReturn.rfc_dor     = ""    

      LET ns2consultarUnificacionCreditosReturn.num_credito     = 0
      LET ns2consultarUnificacionCreditosReturn.estado_credito  = 0
      LET ns2consultarUnificacionCreditosReturn.codigoRespuesta ="1"
      
   ELSE --Existe NSS
   
      SELECT "DOR IMSS"
      INTO    v_tipo_unificacion
      FROM    uni_det_unificador
      WHERE   nss_unificador = ns2unificacionCreditosRequest.nss
      AND     estado_familia = 1
      UNION 
      SELECT "DOR Infonavit"
      INTO    v_tipo_unificacion
      FROM    uni_inf_unificador
      WHERE   nss = ns2unificacionCreditosRequest.nss
      AND     estado_familia = 1
      UNION
      SELECT "ADO IMSS"
      INTO    v_tipo_unificacion
      FROM    uni_det_unificado c, uni_det_unificador d
      WHERE   c.nsscta1 = ns2unificacionCreditosRequest.nss
      AND     d.id_unificador = c.id_unificador
      AND     d.estado_familia = 1
      UNION 
      SELECT "ADO Infonavit"
      INTO    v_tipo_unificacion
      FROM    uni_inf_unificado e, uni_inf_unificador f
      WHERE   e.nss     = ns2unificacionCreditosRequest.nss
      AND     f.id_inf_unificador = e.id_unificador
      AND     f.estado_familia = 1
      GROUP BY 1

      IF v_tipo_unificacion IS NULL THEN --Unificacion Rechazada
         LET ns2consultarUnificacionCreditosReturn.nss_unificador  = ns2unificacionCreditosRequest.nss            
         LET ns2consultarUnificacionCreditosReturn.marca_activa    = 0
         LET ns2consultarUnificacionCreditosReturn.marca_historica = 0 
         LET ns2consultarUnificacionCreditosReturn.fecha_marca     = ""
         
         LET ns2consultarUnificacionCreditosReturn.nombre_dor  = "" 
         LET ns2consultarUnificacionCreditosReturn.paterno_dor = "" 
         LET ns2consultarUnificacionCreditosReturn.materno_dor = "" 
         LET ns2consultarUnificacionCreditosReturn.curp_dor    = ""    
         LET ns2consultarUnificacionCreditosReturn.rfc_dor     = ""    
         
         LET ns2consultarUnificacionCreditosReturn.num_credito     = 0
         LET ns2consultarUnificacionCreditosReturn.estado_credito  = 0
         LET ns2consultarUnificacionCreditosReturn.codigoRespuesta ="2"
      ELSE      
         IF v_tipo_unificacion = "DOR IMSS" OR
            v_tipo_unificacion = "ADO IMSS" THEN
            
            LET v_query_con=" SELECT a.nss_unificador,
                                     c.nombre_af ,
                                     c.ap_paterno_af,
                                     c.ap_materno_af,
                                     c.curp,
                                     c.rfc
                              FROM   uni_det_unificador a
                                     INNER JOIN uni_det_unificado b
                                        ON b.id_unificador = a.id_unificador
                                     INNER JOIN afi_derechohabiente c
                                        ON c.id_derechohabiente = a.id_derechohabiente "
         
            IF v_tipo_unificacion = "DOR IMSS" THEN
               LET v_query_con1= "WHERE  a.nss_unificador = ?
                                  AND    a.estado_familia = 1
                                  GROUP BY 1,2,3,4,5,6 "
            END IF
            
            IF v_tipo_unificacion = "ADO IMSS" THEN
               LET v_query_con1= "WHERE  b.nsscta1 = ?
                                  AND    a.estado_familia = 1
                                  GROUP BY 1,2,3,4,5,6 "
            END IF
         END IF
         
         IF v_tipo_unificacion = "DOR Infonavit" OR
            v_tipo_unificacion = "ADO Infonavit" THEN
         
            LET v_query_con=" SELECT a.nss,
                                     c.nombre_af ,
                                     c.ap_paterno_af,
                                     c.ap_materno_af,
                                     c.curp,
                                     c.rfc
                              FROM   uni_inf_unificador a
                                     INNER JOIN uni_inf_unificado b
                                        ON b.id_unificador = a.id_inf_unificador
                                     INNER JOIN afi_derechohabiente c
                                        ON c.id_derechohabiente = a.id_derechohabiente "
                              
            
            IF v_tipo_unificacion = "DOR Infonavit" THEN
               LET v_query_con1= "WHERE  a.nss = ?
                                  AND    a.estado_familia = 1
                                  GROUP BY 1,2,3,4,5,6 "
            END IF
         
            IF v_tipo_unificacion = "ADO Infonavit" THEN
               LET v_query_con1= "WHERE  b.nss = ?
                                  AND    a.estado_familia = 1
                                  GROUP BY 1,2,3,4,5,6 "
            END IF
         END IF
         
         
         LET v_query_con = v_query_con||v_query_con1
         
         PREPARE prp_query from v_query_con
         DECLARE cur_info CURSOR FOR prp_query
         
         FOREACH cur_info USING ns2unificacionCreditosRequest.nss 
                           INTO ns2consultarUnificacionCreditosReturn.nss_unificador,
                                ns2consultarUnificacionCreditosReturn.nombre_dor,
                                ns2consultarUnificacionCreditosReturn.paterno_dor,
                                ns2consultarUnificacionCreditosReturn.materno_dor,
                                ns2consultarUnificacionCreditosReturn.curp_dor,
                                ns2consultarUnificacionCreditosReturn.rfc_dor 
         
            SELECT id_derechohabiente 
            INTO   v_id_derechohabiente
            FROM   afi_derechohabiente 
            WHERE  nss = ns2consultarUnificacionCreditosReturn.nss_unificador
            
            CALL fn_consulta_credito(v_id_derechohabiente)
                 RETURNING v_credito,
                           v_estado_credito
            
            LET ns2consultarUnificacionCreditosReturn.num_credito     = v_credito
            LET ns2consultarUnificacionCreditosReturn.estado_credito  = v_estado_credito
            
            SELECT NVL(MAX(a.f_inicio),"12-31-1899")
            INTO   v_fecha_marca
            FROM   sfr_marca_activa a
            WHERE  a.id_derechohabiente = v_id_derechohabiente
            AND    a.marca IN (501,503)
            
            IF v_fecha_marca = "12-31-1899" THEN
               LET v_marca_activa = 2
            ELSE
               LET v_marca_activa = 1
            END IF
            
            IF v_marca_activa = 2 THEN
               SELECT NVL(MAX(a.f_fin),"12-31-1899")
               INTO   v_fecha_marca
               FROM   sfr_marca_historica a
               WHERE  a.id_derechohabiente = v_id_derechohabiente
               AND    a.marca IN (501,503)
               AND    a.estado_marca  = 0
               
               IF v_fecha_marca = "12-31-1899" THEN
                  LET v_marca_his = 4
               ELSE
                  LET v_marca_his = 3
               END IF
            END IF 
            
            LET ns2consultarUnificacionCreditosReturn.marca_activa    = v_marca_activa
            LET ns2consultarUnificacionCreditosReturn.marca_historica = v_marca_his
            LET ns2consultarUnificacionCreditosReturn.fecha_marca     = v_fecha_marca
         
            IF v_tipo_unificacion = "DOR IMSS" OR
               v_tipo_unificacion = "ADO IMSS" THEN
         
               LET v_query_con=" SELECT b.nsscta1,
                                        c.nombre_af ,
                                        c.ap_paterno_af,
                                        c.ap_materno_af,
                                        c.curp,
                                        c.rfc
                                 FROM   uni_det_unificador a
                                        INNER JOIN uni_det_unificado b
                                           ON b.id_unificador = a.id_unificador
                                        INNER JOIN afi_derechohabiente c
                                           ON c.id_derechohabiente = a.id_derechohabiente
                                 WHERE  a.nss_unificador = ?
                                 AND    a.estado_familia = 1
                                 GROUP BY 1,2,3,4,5,6 "
            END IF
            
            IF v_tipo_unificacion = "DOR Infonavit" OR
               v_tipo_unificacion = "ADO Infonavit" THEN
               LET v_query_con=" SELECT b.nss,
                                        c.nombre_af ,
                                        c.ap_paterno_af,
                                        c.ap_materno_af,
                                        c.curp,
                                        c.rfc
                                 FROM   uni_inf_unificador a
                                        INNER JOIN uni_inf_unificado b
                                           ON b.id_unificador = a.id_inf_unificador
                                        INNER JOIN afi_derechohabiente c
                                           ON c.id_derechohabiente = a.id_derechohabiente
                                 WHERE  a.nss = ?    
                                 AND    a.estado_familia = 1
                                 GROUP BY 1,2,3,4,5,6 "                  
            END IF
         
            PREPARE prp_query_ado from v_query_con
            DECLARE cur_info_ado CURSOR FOR prp_query_ado
            
            CALL ns2consultarUnificacionCreditosReturn.unificados.item.clear()	
         
            LET v_cont = 1					
           
            FOREACH cur_info_ado USING ns2consultarUnificacionCreditosReturn.nss_unificador
                                 INTO ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].nss_unificado,
                                      ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].nombre_ado,
                                      ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].paterno_ado,
                                      ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].materno_ado,
                                      ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].curp_ado,
                                      ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].rfc_ado 
           
               SELECT id_derechohabiente 
               INTO   v_id_derechohabiente
               FROM   afi_derechohabiente 
               WHERE  nss = ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].nss_unificado
               
               CALL fn_consulta_credito(v_id_derechohabiente)
                    RETURNING v_credito,
                              v_estado_credito
               
               LET ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].num_credito_ado     = v_credito
               LET ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].estado_credito_ado  = v_estado_credito
               
               SELECT NVL(MAX(a.f_inicio),"12-31-1899")
               INTO   v_fecha_marca
               FROM   sfr_marca_activa a
               WHERE  a.id_derechohabiente = v_id_derechohabiente
               AND    a.marca IN (502,504)
               
               IF v_fecha_marca = "12-31-1899" THEN
                  LET v_marca_activa = 2
               ELSE
                  LET v_marca_activa = 1
               END IF
               
               IF v_marca_activa = 2 THEN
                  SELECT NVL(MAX(a.f_fin),"12-31-1899")
                  INTO   v_fecha_marca
                  FROM   sfr_marca_historica a
                  WHERE  a.id_derechohabiente = v_id_derechohabiente
                  AND    a.marca IN (502,504)
                  AND    a.estado_marca  = 0
                  
                  IF v_fecha_marca = "12-31-1899" THEN
                     LET v_marca_his = 4
                  ELSE
                     LET v_marca_his = 3
                  END IF
               END IF 
               
               LET ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].marca_activa_ado    = v_marca_activa
               LET ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].marca_historica_ado = v_marca_his
               LET ns2consultarUnificacionCreditosReturn.unificados.item[v_cont].fecha_marca_ado     = v_fecha_marca
               
               LET v_cont = v_cont + 1
            END FOREACH 
         
            CALL ns2consultarUnificacionCreditosReturn.unificados.item.deleteElement(v_cont)       
         END FOREACH 
         
         LET ns2consultarUnificacionCreditosReturn.codigoRespuesta = "0"
      END IF
   END IF        
END FUNCTION 
#
FUNCTION fn_consulta_credito(p_id_derechohabiente)

   DEFINE v_QryTxt CHAR(200),
          p_id_derechohabiente DECIMAL(9,0),
          v_tipo_consulta_cre  SMALLINT

   DEFINE v_resultado           SMALLINT,
          v_tpo_originacion     SMALLINT,
          v_tpo_credito         SMALLINT,
          v_num_credito         DECIMAL(10,0),
          v_f_otorga            DATE,
          v_f_liquida           DATE,
          v_estado_credito      CHAR(30)

   LET v_tipo_consulta_cre = 1
   
   LET v_QryTxt = "EXECUTE FUNCTION fn_credito_vivienda(?,?)"
   
   PREPARE prp_cred_viv_ado FROM v_QryTxt CLIPPED
   EXECUTE prp_cred_viv_ado USING p_id_derechohabiente,  
                                  v_tipo_consulta_cre
                            INTO  v_resultado,
                                  v_tpo_originacion,
                                  v_tpo_credito,
                                  v_num_credito,
                                  v_f_otorga, 
                                  v_f_liquida;

    CASE v_resultado
       WHEN 0 LET v_estado_credito = "CREDITO VIGENTE"
       WHEN 1 LET v_estado_credito = "NO TIENE CREDITO"
       WHEN 2 LET v_estado_credito = "CREDITO LIQUIDADO"
    END CASE

    IF v_resultado = 1 THEN
       LET v_num_credito = 0
    END IF
    
   RETURN v_num_credito, v_estado_credito
END FUNCTION
#