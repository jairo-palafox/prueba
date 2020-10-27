################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 22/08/2017                                        #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CTA                                                      #
#Programa          => CTAWS15                                                  #
#Objetivo          => Servicio web para la publicacion del detalle de          #
#                     movimientos                                              #
#Fecha inicio      => 22/08/2017                                               #
################################################################################
IMPORT FGL WSHelper

IMPORT com
IMPORT XML
DATABASE safre_viv

GLOBALS "CTAWS15.inc"

PRIVATE DEFINE v_ruta_pdf    STRING 
PRIVATE DEFINE v_arr_mov     DYNAMIC ARRAY OF movimientos

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER 
   
   CALL CreateConsultaMovimientosServices() RETURNING servicio

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

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF     
   END WHILE
END MAIN

#
#-------------------------------------------------------------------------------
# Service: ConsultaMovimientosServicesService
# Port:    ConsultaMovimientosServices
#-------------------------------------------------------------------------------
#
# FUNCTION CreateConsultaMovimientosServices
#   RETURNING soapstatus
FUNCTION CreateConsultaMovimientosServices()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("ConsultaMovimientosServices","http://consultaMovimientos.efp.com")

    #
    # Operation: consultarMovimientos
    #

    # Publish Operation : consultarMovimientos
    LET operation = com.WebOperation.CreateDOCStyle("consultarMovimientos","consultarMovimientos",ns2movimientosRequest,ns2consultarMovimientosReturn)
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

FUNCTION consultarMovimientos()
   DEFINE v_query         STRING 
   DEFINE v_trabajador    trabajador

   --Consulta que busca el nss recibido
   INITIALIZE v_trabajador.* TO NULL
   LET v_query =  "SELECT ",
                     "id_derechohabiente, ",
                     "nss, ",
                     "rfc, ",
                     "curp, ",
                     "trim(nombre_af)|| ' ' ||trim(NVL(ap_paterno_af,' '))|| ' ' ||trim(NVL(ap_materno_af,' ')) ",
                  "FROM afi_derechohabiente ",
                  "WHERE nss = ? "
   PREPARE exe_consulta_trabajador FROM v_query
   EXECUTE exe_consulta_trabajador USING ns2movimientosRequest.nss 
                                   INTO v_trabajador.*

   INITIALIZE ns2consultarMovimientosReturn.* TO NULL
 
 --Se evalua que exista NSS
   IF v_trabajador.id_derechohabiente IS NULL THEN --No existe NSS
      LET ns2consultarMovimientosReturn.codigoRespuesta="000"
   ELSE --Existe NSS
      CALL fn_consulta(v_trabajador.*)--Funcion que reaiza la consulta y carga el record a retornar
   END IF 
   
END FUNCTION 


FUNCTION fn_consulta(p_trabajador)--Funcion que carga el record que serà regreasado por el servicio
   DEFINE p_trabajador             trabajador
   DEFINE v_query                  STRING 
   DEFINE v_cont                   INTEGER 
   DEFINE v_rfc10                  CHAR(10)


   LET v_query = "execute procedure sp_gen_his_mov(?)" --Procedimiento que genera una tabla temporal con todos los movimientos realizados
   PREPARE exe_consulta_movimientos FROM v_query               --por el derechohabiente
   EXECUTE exe_consulta_movimientos USING p_trabajador.id_derechohabiente

   LET v_query = "SELECT ", 
                        "mov.f_liquida, ",
                        "mov.movimiento, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "spre.id_subcuenta, ",
                        "subc.subcuenta_desc, ",
                        "SUM(mov.monto_pesos) ",
                     "FROM tmp_movimiento mov ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento ",
                     "INNER JOIN cat_subcuenta_preca spre ON spre.subcuenta = mov.subcuenta ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = spre.id_subcuenta ",
                     "WHERE mov.id_derechohabiente = ? ",
                     "AND mov.subcuenta IN (4,8,42,44,55) ",
                     "AND mov.movimiento IN (SELECT mmov.movimiento FROM cat_muestra_mov mmov) ",
                     "AND mov.f_liquida >= MDY(11,1,2012) ",
                     "GROUP BY 1,2,3,4,5,6,7 ",
                     "UNION ",
                     "SELECT ",
                        "f_pago, ", 
                        "estado, ",
                        "'', ",
                        "-1, ",
                        "CASE estado WHEN 30 THEN 'Pago pendiente de realizar por parte del empleador' ",
                        "         when 70 then 'Pago cancelado, no realizado por el empleador' ",
                        "END CASE, ",
                        "72, ",		--Valor fijo para identificar los adelantos,
                        "'VIVIENDA 97 AMORTIZACION', ",
                        "(monto_aportacion + monto_amortizacion) ",
                     "FROM dis_det_avance_pago ",
                     "WHERE id_derechohabiente = ? ",
                     "AND estado IN (30,70) ",
                     "AND f_pago > MDY(9,1,2005) ",
                     "ORDER BY 1 DESC "
                
      PREPARE exe_consulta FROM v_query
		DECLARE cur_consulta CURSOR FOR exe_consulta

      LET v_cont = 1
      INITIALIZE v_arr_mov TO NULL
						
      FOREACH cur_consulta USING p_trabajador.id_derechohabiente,
                                 p_trabajador.id_derechohabiente 
                           INTO  v_arr_mov[v_cont].*
         LET v_cont = v_cont + 1  
      END FOREACH 

      CLOSE cur_consulta
      FREE cur_consulta
      
      #Se agregan los registros del historico de movimientos TRM

      LET v_query =  "SELECT ",
                        "trm.f_pago, ",
                        "trm.cve_aportacion, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "69, ",                    #Valor fijo para identificar los movimientos de TRM
                        "subc.subcuenta_desc, ",
                        "trm.aportacion ",
                     "FROM safre_his@vivht_tcp:his_pag_bim_trm trm ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                     "WHERE trm.nss = ? ",
                     "AND trm.tpo_aclaracion = 99 ",
                     "AND trm.f_pago > MDY(9,1,2005) ",
                     "AND trm.f_pago <= MDY(10,31,2012) ",
                     "ORDER BY trm.f_pago DESC "

      PREPARE exe_consulta_trm FROM v_query
      DECLARE cur_consulta_trm CURSOR FOR exe_consulta_trm

      FOREACH cur_consulta_trm USING p_trabajador.nss INTO v_arr_mov[v_cont].*
         LET v_cont = v_cont + 1  
      END FOREACH 

      LET v_rfc10 = p_trabajador.rfc
      #Se agregan los registros del historico de movimientos de Pensiones ADS 1997 - 2005
      LET v_query =  "SELECT ",
                        "pen.f_pago, ",
                        "pen.clave, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "70, ",         --Valor fijo para identificar los movimientos de pensiones
                        "subc.subcuenta_desc, ",
                        "pen.aportacion ",
                     "FROM safre_his@vivht_tcp:his_pag_pen_ads pen ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 4 ",
                     "WHERE (pen.nss = '",p_trabajador.nss CLIPPED, "' OR pen.rfc[1,10] = '",v_rfc10 CLIPPED,"' OR pen.rfc = '",p_trabajador.rfc CLIPPED,"') ",
                     "AND pen.clave IN ('105','106','115','116','125','126','135','136','155','156') ",
                     "AND pen.f_pago >= MDY(9,1,1997) ",
                     "AND pen.f_pago <= MDY(8,31,2005) ",
                     "ORDER BY pen.f_pago DESC "
      PREPARE exe_consulta_pen FROM v_query
      DECLARE cur_consulta_pen CURSOR FOR exe_consulta_pen
      FOREACH cur_consulta_pen INTO  v_arr_mov[v_cont].*
         LET v_cont = v_cont + 1  
      END FOREACH 

      #Se agregan los registros del historico de movimientos de ADS 1992 - 1997
      LET v_query =  "SELECT ",
                        "ads.f_pago, ",
                        "ads.clave, ",
                        "cat.movimiento_desc, ",
                        "cat.tipo, ",
                        "cat.desc_ciudadana, ",
                        "71, ",                    --Valor fijo para identificar los movimientos de ADS Bimestral
                        "subc.subcuenta_desc, ",
                        "ads.importe ",
                     "FROM safre_his@vivht_tcp:his_pag_bim_ads ads ",
                     "INNER JOIN cat_movimiento cat ON cat.movimiento = 1 ",
                     "INNER JOIN cat_subcuenta subc ON subc.subcuenta = 8 ",
                     "WHERE (ads.nss = '",p_trabajador.nss CLIPPED, "' OR ads.rfc[1,10] = '",v_rfc10 CLIPPED,"' OR ads.rfc = '",p_trabajador.rfc CLIPPED,"') ",
                     "AND ads.clave IN ('105','115','125','135','155') ",
                     "AND ads.f_pago >= MDY(5,1,1992) ",
                     "AND ads.f_pago <= MDY(8,31,1997) ",
                     "ORDER BY ads.f_pago DESC "
      #DISPLAY v_query
      PREPARE exe_consulta_ads FROM v_query
      DECLARE cur_consulta_ads CURSOR FOR exe_consulta_ads
      FOREACH cur_consulta_ads INTO  v_arr_mov[v_cont].*
         LET v_cont = v_cont + 1  
      END FOREACH 
                
      CALL fn_load_out()--Función que carga el arreglo de salida 
      LET ns2consultarMovimientosReturn.nss = p_trabajador.nss

      IF v_cont= 1 THEN --Evaluaciones para determinar como es el resultado de la consulta, se modifica el 
         LET ns2consultarMovimientosReturn.codigoRespuesta = "003"--código de respuesta deacuerdo a las evaluaciones
         DISPLAY "No se encontraron movimientos"
      END IF 

      IF v_cont > 1 THEN
         DISPLAY "La consulta encontro movimientos"
         LET ns2consultarMovimientosReturn.codigoRespuesta = "002"

         CALL fn_genera_reporte(p_trabajador.*)
         CALL fn_load_pdf(v_ruta_pdf)--Se crea, se envia y se borra el reporte.pdf
      END IF 

      DROP TABLE tmp_movimiento 
    
END FUNCTION 

FUNCTION fn_load_out()--Función que carga el arreglo de salida evaluando el campo desc_ciudadana 
                      --si desc_ciudadana es null se utilizara el campo movimiento_desc la salida
    DEFINE v_cont             SMALLINT 
    DEFINE v_indice           SMALLINT
    DEFINE v_cargo_mov_desc   VARCHAR(60)
    DEFINE v_cargo_mov_desc_pen VARCHAR(60)
    
    CALL ns2consultarMovimientosReturn.movimientos.item.clear()

    #Se consulta la descripcion del cargo informativo para los movimientos del historico TRM
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO 

    #Se consulta la descripcion del cargo informativo para los movimientos del historico PENSIONES
    SELECT
      desc_ciudadana
    INTO
      v_cargo_mov_desc_pen
    FROM cat_movimiento
    WHERE movimiento = CARGO_INFORMATIVO_PEN 

    LET v_indice = 1
    FOR v_cont=1 TO v_arr_mov.getLength()
      IF v_arr_mov[v_cont].f_liquida IS NOT NULL THEN
         IF v_arr_mov[v_cont].desc_ciudadana IS NULL THEN 
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         ELSE 
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento_desc = v_arr_mov[v_cont].desc_ciudadana
         END IF
         
         LET ns2consultarMovimientosReturn.movimientos.item[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
         LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento = v_arr_mov[v_cont].movimiento USING '##&&'
         #LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento_desc = v_arr_mov[v_cont].movimiento_desc
         LET ns2consultarMovimientosReturn.movimientos.item[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc 

         IF v_arr_mov[v_cont].tipo < 0 THEN
            IF v_arr_mov[v_cont].monto_pesos < 0 THEN
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].cargo = v_arr_mov[v_cont].monto_pesos * -1
            ELSE
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].cargo = v_arr_mov[v_cont].monto_pesos
            END IF
            
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice].abono = NULL
         ELSE
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice].abono = v_arr_mov[v_cont].monto_pesos
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice].cargo = NULL
         END IF
         LET v_indice = v_indice + 1

         #Tratamiento especial para los movimientos historicos TRM
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_TRM THEN
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 1 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].cargo = NULL 
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].abono = v_arr_mov[v_cont].monto_pesos
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento = '01' USING '##&&'
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento_desc = ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].cargo = v_arr_mov[v_cont].monto_pesos 
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].abono = NULL
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento = CARGO_INFORMATIVO USING '##&&'
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento_desc = v_cargo_mov_desc
               
               LET v_indice = v_indice + 1
            END IF
         END IF

         #Tratamiento especial para los movimientos historicos PENSIONES
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_PEN THEN
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento = '01' USING '##&&'
            IF v_arr_mov[v_cont].movimiento = 106 
            OR v_arr_mov[v_cont].movimiento = 116
            OR v_arr_mov[v_cont].movimiento = 126
            OR v_arr_mov[v_cont].movimiento = 136
            OR v_arr_mov[v_cont].movimiento = 156 THEN
               #Se genera un movimiento por el mismo monto de la aportacion
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].cargo = NULL 
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].abono = v_arr_mov[v_cont].monto_pesos
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].subcuenta = v_arr_mov[v_cont].subcuenta_desc
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].fLiquidacion = v_arr_mov[v_cont].f_liquida
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento = '01' USING '##&&'
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice].movimiento_desc = ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento_desc

               --Se actualiza el primer movimiento para poner el cargo informativo en la posicion anterior de la lista
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].cargo = v_arr_mov[v_cont].monto_pesos 
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].abono = NULL
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento = CARGO_INFORMATIVO_PEN USING '##&&'
               LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento_desc = v_cargo_mov_desc_pen
               
               LET v_indice = v_indice + 1
            END IF
         END IF
         #Tratamiento especial para los movimientos historicos BIMESTRAL ADS
         IF v_arr_mov[v_cont].subcuenta = SUB_CTA_BIM_ADS THEN
            LET ns2consultarMovimientosReturn.movimientos.item[v_indice - 1].movimiento = '01' USING '##&&'
         END IF
      END IF
    END FOR 
END FUNCTION 

PUBLIC FUNCTION fn_load_pdf(v_ruta_reporte)
   DEFINE archivo           BYTE
   DEFINE v_ruta_reporte    STRING 
   DEFINE v_comando         STRING 

   LOCATE archivo IN MEMORY
   #DISPLAY v_ruta_reporte
   CALL archivo.readFile(v_ruta_reporte)
   LET ns2consultarMovimientosReturn.archivoPDF = archivo
   LET v_comando="rm "||v_ruta_reporte
   RUN v_comando 
   
END FUNCTION

PRIVATE  FUNCTION fn_genera_reporte(p_trabajador)
   DEFINE p_trabajador       trabajador
    DEFINE reporte          om.SaxDocumentHandler
    DEFINE i                SMALLINT
    DEFINE v_reporte        STRING
    DEFINE v_ruta_listados  CHAR(40)
    DEFINE v_ruta_reporte   STRING

   LET v_reporte= "CTAWS151.4rp"

    SELECT ruta_listados
    INTO v_ruta_listados
    FrOM seg_modulo
    WHERE modulo_cod = "cta"
    
    LET v_ruta_reporte = v_ruta_listados CLIPPED , "/" ,
                       ns2movimientosRequest.nss CLIPPED,"_" 
                       ||YEAR(TODAY) CLIPPED
                       ||MONTH(TODAY) CLIPPED||DAY(TODAY) CLIPPED,".pdf" 

    LET v_ruta_pdf = v_ruta_reporte
   
   IF fgl_report_loadCurrentSettings(v_reporte) THEN
      CALL fgl_report_selectDevice("PDF")# PDF, XLS, HTML
      CALL fgl_report_selectPreview(FALSE)
      CALL fgl_report_setoutputfilename(v_ruta_reporte)
      LET reporte = fgl_report_commitCurrentSettings()
      DISPLAY ns2consultarMovimientosReturn.movimientos.item.getLength()
      IF reporte IS NOT NULL THEN
         START REPORT movimientos TO XML HANDLER reporte
            FOR i = 1 TO ns2consultarMovimientosReturn.movimientos.item.getLength()
               OUTPUT TO REPORT movimientos(ns2consultarMovimientosReturn.movimientos.item[i].*,p_trabajador.*)
            END FOR 
         FINISH REPORT movimientos
      END IF
   END IF
  
END FUNCTION 

REPORT movimientos(p_movimientos, p_trabajador) 

   DEFINE p_movimientos    tns1Movimiento
   DEFINE p_trabajador     trabajador
   DEFINE v_fecha          SMALLINT
   DEFINE v_ffin           STRING
   DEFINE v_mes            VARCHAR(15)
   
   FORMAT

   FIRST PAGE HEADER

      LET v_fecha = MONTH(TODAY)
      SELECT LOWER(mes_desc) INTO v_mes FROM cat_mes WHERE mes = v_fecha
      LET v_ffin = DAY(TODAY), ' de ', v_mes CLIPPED, YEAR(TODAY)
      PRINTX v_ffin
      PRINTX p_trabajador.nss
      PRINTX p_trabajador.nombre
      PRINTX p_trabajador.rfc
      PRINTX p_trabajador.curp

   ON EVERY ROW

      PRINTX p_movimientos.fLiquidacion USING 'dd-mm-yyyy'
      PRINTX p_movimientos.movimiento
      PRINTX p_movimientos.movimiento_desc
      PRINTX p_movimientos.subcuenta
      PRINTX p_movimientos.cargo
      PRINTX p_movimientos.abono

END REPORT
