--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/01/2014
-- 21/01/2014 - Se modificaron las consultas a cta_movimiento AG
--===============================================================
 
-- Archivo : UNIG01.4gl
-- funciones, variales y constantes globales del modulo de unificación de cuentas
 
DATABASE safre_viv
GLOBALS
   CONSTANT  g_proceso_cod_uni_IMSS           SMALLINT = 2301 -- IMSS
   CONSTANT  g_proceso_cod_uni_infonavit      SMALLINT = 2302 -- INFONAVIT
   CONSTANT  g_proceso_cod_uni_complementario SMALLINT = 2303 -- COMPLEMENTARIO
   CONSTANT  g_proceso_cod_uni_operacion22    SMALLINT = 2304 -- OPERACION 22
   CONSTANT  g_proceso_cod_uni_resp_sistemas  SMALLINT = 2305 -- RESPUESTA SISTEMAS IMSS
   CONSTANT  g_proceso_cod_uni_resp_sist_inf  SMALLINT = 2306 -- RESPUESTA SISTEMAS INFONAVIT
   
   CONSTANT  g_opera_cod_uni_carga                 SMALLINT = 1,  --  carga de archivo
             g_opera_cod_uni_batch                 SMALLINT = 1,  --  ejecucion en batch
             g_opera_cod_uni_integracion           SMALLINT = 2,  --  integracion
             g_opera_cod_uni_carga_confronta       SMALLINT = 3,  --  carga de archivo confronta
             g_opera_cod_uni_integracion_confronta SMALLINT = 4,  --  integracion confronta
             g_opera_cod_uni_preliquidacion        SMALLINT = 5,  --  preliquidacion
             g_opera_cod_uni_liquidacion           SMALLINT = 6,  -- liquidacion
             g_opera_cod_uni_indicadores           SMALLINT = 7,  -- Indicadores de credito
             g_opera_cod_uni_generacion            SMALLINT = 8,  -- Generación de archivo
             g_opera_cod_uni_cargaop22             SMALLINT = 1,  -- carga op22
             g_opera_cod_uni_integraop22           SMALLINT = 2,  -- integra op22
             g_opera_cod_uni_carga_resp_imss       SMALLINT = 1,  -- carga respuesta sistemas IMSS
             g_opera_cod_uni_integra_resp_imss     SMALLINT = 2,   -- integra respuesta sistemas IMSS
             g_opera_cod_uni_carga_resp_inf        SMALLINT = 1,  -- carga respuesta sistemas INFONAVIT
             g_opera_cod_uni_integra_resp_inf      SMALLINT = 2   -- integra respuesta sistemas INFONAVIT
             
DEFINE v_arr_liq_unificador DYNAMIC ARRAY OF RECORD    
          unificador	    CHAR(11),
          origen			CHAR(30),
          subcuenta		    CHAR(30),
          siefore			CHAR(30),
          monto_en_accion	DECIMAL(20,2),
          monto_en_pesos 	DECIMAL(20,2),
          fecha_liquidacion	DATE
       END RECORD,
       v_arr_liq_unificado DYNAMIC ARRAY OF RECORD    
          unificado	          CHAR(11),
          origen1			        CHAR(30),
          subcuenta1		      CHAR(30),
          siefore1			      CHAR(30),
          monto_en_accion1	  DECIMAL(20,2),
          monto_en_pesos1 	  DECIMAL(20,2),
          fecha_liquidacion1	DATE
       END RECORD  
       DEFINE r_detmov DYNAMIC ARRAY OF RECORD
          fecha_mov           DATE,
          tpo_mov             INTEGER,
          desc_mov            CHAR(80),
          fondo               SMALLINT,
          pesos               DECIMAL(16,6),
          acciones            DECIMAL(16,6),
          f_valor             DATE,
          folio               DECIMAL(10,0),
          origen              CHAR(20)
       END RECORD,
       w ui.Window,
       f ui.Form ,
        v_si_indice2               SMALLINT,
        v_s_reporte                STRING,
        r_ruta_bin         LIKE seg_modulo.ruta_bin,
        r_ruta_listados    LIKE seg_modulo.ruta_listados,
        p_usuario_cod    LIKE seg_usuario.usuario_cod,-- clave del usuario firmado
        v_nom_reporte      VARCHAR(80) -- nombre del reporte        
END GLOBALS

FUNCTION Consulta_Uni_Cuentas_Unificador(v_foliounificador,  v_nssunificador)
DEFINE 
        v_arr_unificador RECORD                               
        folio_proceso            DECIMAL(9,0),      
        folio_liquidacion        DECIMAL(9,0),                                           
        nss_unificador           char(11),          
        nombre_unificador        CHAR(100),                      
        paterno_unificador       CHAR(100),         
        materno_unificador       CHAR(100),         
        rfc                      CHAR(13),          
        curp                     CHAR(18),          
        nombre_imss              CHAR(100),         
        sexo                     CHAR(10),          
        tipo_registro            CHAR(30),          
        status_convoca           CHAR(30),                  
        fecha_notificacion_op22  DATE ,                                               
        afore_receptora          CHAR(13),           
        afore_origen             CHAR(13),            
        fecha_unificacion        DATE ,             
        tipo_movimiento          CHAR(35),
        fecha_notificacion_op21  DATE
     END RECORD ,  
        nombre                   CHAR(100),
        v_sql_txt                   STRING,
        v_sql_txtop21                   STRING,
        v_foliounificador           DECIMAL(9,0),
        v_nssunificador             CHAR (11),
        v_aux_afore_receptora      SMALLINT,
        v_aux_afore_origen         SMALLINT,
        v_fecha_reporte            STRING
       INITIALIZE v_arr_unificador   TO  NULL 

        LET v_sql_txt=  "\n SELECT folio_unificacion,",
                        "\n        folio_liquidacion ,",
                        "\n        nss_unificador,",
                        "\n        nombre_unificador,",
                        "\n        paterno_unificador,",
                        "\n        materno_unificador,",                                              
                        "\n        rfc_unificador,",
                        "\n        curp_unificador,",
                        "\n        nombre_imssunificador,",
                        "\n        sexo_unificador,",
                        "\n        tipo_registro,",
                        "\n        estatus_convocatoria,",
                        "\n        f_notificacion,", -- fecha_notificacion_op22
                        "\n        clave_afore_receptora ||'-'|| ca.afore_desc as RECEPTORA,",
                        "\n        cve_afore_aclaracion||'-'|| ca.afore_desc as ORIGEN,",
                        "\n        f_liquidacion,",   --   fecha_unificacion
                        "\n        ident_movimiento", 
                        "\n     FROM uni_det_unificador, cat_afore ca",
                        "\n        WHERE folio_unificacion = ",v_foliounificador,  
                        "\n          AND nss_unificador = ",v_nssunificador,
                        "\n          AND clave_afore_receptora = ca.afore_cod"

     --display "consulta UNIFICADORRRR:", v_sql_txt ,"\n folios:",  v_foliounificador,", ",  v_nssunificador
     PREPARE c_consulta_unificador FROM  v_sql_txt 
     --EXECUTE c_consulta_unificador USING v_foliounificador,  v_nssunificador
     EXECUTE c_consulta_unificador INTO   v_arr_unificador.folio_proceso,            
                                          v_arr_unificador.folio_liquidacion,        
                                          v_arr_unificador.nss_unificador,                      
                                          v_arr_unificador.nombre_unificador ,       
                                          v_arr_unificador.paterno_unificador,       
                                          v_arr_unificador.materno_unificador,       
                                          v_arr_unificador.rfc,                      
                                          v_arr_unificador.curp,                     
                                          v_arr_unificador.nombre_imss,              
                                          v_arr_unificador.sexo,                     
                                          v_arr_unificador.tipo_registro,            
                                          v_arr_unificador.status_convoca,           
                                          v_arr_unificador.fecha_notificacion_op22,
                                          v_arr_unificador.afore_receptora,
                                          v_arr_unificador.afore_origen,
                                          --v_aux_afore_receptora ,                       
                                          --v_aux_afore_origen ,                                                                                   
                                          v_arr_unificador.fecha_unificacion       ,             
                                          v_arr_unificador.tipo_movimiento    

       LET nombre=v_arr_unificador.nombre_unificador CLIPPED," " ,v_arr_unificador.paterno_unificador CLIPPED," ",v_arr_unificador.materno_unificador CLIPPED

       --Valida si AFORE no tiene valor limpia la variable  
       IF v_arr_unificador.afore_origen[1,3] < 0 THEN 
          LET v_arr_unificador.afore_origen = ""
       END IF
       
       CASE v_arr_unificador.sexo
       	WHEN 1 LET v_arr_unificador.sexo="MASCULINO"
       	WHEN 2 LET v_arr_unificador.sexo="FEMENINO"
       END CASE     
--SE CAMBIA UNIQUE POR MAX EN CONSULTA DE FECHA 
       LET v_sql_txtop21="\n SELECT  MAX  (f_actualiza) ",
                         "\n FROM  glo_folio f, uni_det_unificador u ",
                         "\n WHERE  f.folio= u.folio_unificacion "
--DISPLAY v_sql_txtop21
        PREPARE c_op21 FROM  v_sql_txtop21 
        EXECUTE c_op21 INTO v_arr_unificador.fecha_notificacion_op21
      --DISPLAY "folio_proceso: ",v_arr_unificador.folio_proceso
       {SELECT  f_presentacion
       	INTO   v_arr_unificador.fecha_notificacion_op22
       		FROM uni_cza_unificacion
       			WHERE folio_unificacion= v_arr_unificador.folio_proceso }
                     
       OPEN WINDOW w_cuenta_unificador WITH FORM "UNIC012_1" ATTRIBUTE(STYLE="dialog")
    
       	CALL FGL_SETTITLE("Consulta unificación de cuentas  del Unificador")

        	DISPLAY BY NAME v_arr_unificador.folio_proceso,            
                          v_arr_unificador.folio_liquidacion,        
                          v_arr_unificador.nss_unificador,                      
                          nombre,      
                          v_arr_unificador.rfc,                      
                          v_arr_unificador.curp,                     
                          v_arr_unificador.nombre_imss,              
                          v_arr_unificador.sexo,                     
                          v_arr_unificador.tipo_registro,            
                          v_arr_unificador.status_convoca,           
                          v_arr_unificador.fecha_notificacion_op21,  
                          v_arr_unificador.afore_origen,
                          v_arr_unificador.fecha_unificacion,             
                          v_arr_unificador.afore_receptora ,
                          v_arr_unificador.tipo_movimiento ,
                          v_arr_unificador.fecha_notificacion_op22
			MENU 
				ON ACTION  Regresar 
				EXIT MENU 
				
				ON ACTION  Imprimir 
                    LET v_fecha_reporte = CURRENT HOUR TO SECOND 
					--Genera el reporte de Unificador
					# Recupera la ruta de listados en el que se enviara el archivo
					CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
					# Se indica que el reporte usara la plantilla creada
					IF fgl_report_loadCurrentSettings("UNIG01.4rp") THEN
						CALL fgl_report_selectDevice("PDF")
						LET v_nom_reporte = "safreviv" CLIPPED || "-UNIG01-","00000","-","00000","-","00000"||".pdf"
						CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)					
						LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>", v_nom_reporte CLIPPED,"</a>"
					    CALL genera_reporte_consulta_unificador(v_arr_unificador.*, nombre) RETURNING v_si_indice2                       
						IF(v_si_indice2 = 0)THEN
							CALL fn_mensaje("Atención","No se pudo generar el reporte","information")
						END IF
					ELSE
						DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
					END IF
					DISPLAY v_s_reporte TO v_s_reporte
			END MENU 
  		 CLOSE WINDOW w_cuenta_unificador
END FUNCTION 

#OBJETIVO: Consultar los detalles del UNIFICADO en la consulta de diagnostico 
FUNCTION Consulta_Uni_Cuentas_Unificado(v_foliounificado,  v_nssunificado)
      DEFINE 
          v_arr_unificado RECORD 
	          folio_proceso            DECIMAL(9,0),     
	          folio_liquidacion        DECIMAL(9,0),     
	          nss_unificado            CHAR(11),       
	          nombre_unificado         CHAR(100),
	          paterno_unificado        CHAR(100),
	          materno_unificado        CHAR(100),
	          rfc                      CHAR(13), 
	          curp                     CHAR(18), 
	          nombre_imss              CHAR(100),
	          sexo                     CHAR(10), 
	          tipo_movimiento          CHAR(35),
	          tipo_registro            CHAR(30),
	          status_convoca           CHAR(30),
	          fecha_notificacion_op21  DATE ,   
	          fecha_unificacion        DATE ,  
	          fecha_notificacion_op22  DATE,    
	          clave_afore_receptora    CHAR(13),           
	          afore_origen             CHAR(13)
        END RECORD ,                             
        nombre                     CHAR(100),      
        v_sql_txt                  STRING,
        v_sql_txtop21              STRING,
        v_foliounificado           DECIMAL(9,0),
        v_nssunificado             CHAR (11),
        v_aux_afore_origen_uni     CHAR (4)   
       INITIALIZE v_arr_unificado   TO  NULL 
        LET v_sql_txt=  "\n SELECT du.folio_unificacion,",
                        "\n        max(ur.folio_liquidacion) ,",
                        "\n        du.nsscta1,",
                        "\n        du.nombrecta1,",
                        "\n        du.paternocta1,",
                        "\n        du.maternocta1,",
                        "\n        du.rfccta1,",
                        "\n        du.curpcta1,",
                        "\n        du.nombre_imsscta1,",
                        "\n        du.sexocta1,",
                        "\n        du.tipo_registro,",
                        "\n        du.estatus_convocatoria,",
                     -- "\n        du.f_notificacion,",
                     -- "\n        du.clave_afore_receptora,",
                        "\n        du.cve_entidadcta1,",
                        "\n        du.diagnostico_uni",
                        "\n    FROM uni_det_unificado du, uni_det_unificador ur",
                        "\n       WHERE du.folio_unificacion = ",v_foliounificado,
                        "\n       AND du.folio_unificacion = ur.folio_unificacion",
                        "\n       AND du.nsscta1 = ",v_nssunificado,
                        "\n       GROUP BY 1,3,4,5,6,7,8,9,10,11,12,13,14"
                        
      --DISPLAY "v_sql_txt UNIFICADOOOOO: ", v_sql_txt CLIPPED
      PREPARE c_consulta_unificado FROM  v_sql_txt 
      EXECUTE c_consulta_unificado INTO v_arr_unificado.folio_proceso,
                                        v_arr_unificado.folio_liquidacion,
                                        v_arr_unificado.nss_unificado,
                                        v_arr_unificado.nombre_unificado,
                                        v_arr_unificado.paterno_unificado,       
                                        v_arr_unificado.materno_unificado,
                                        v_arr_unificado.rfc,
                                        v_arr_unificado.curp,
                                        v_arr_unificado.nombre_imss,
                                        v_arr_unificado.sexo,
                                        v_arr_unificado.tipo_registro,
                                        v_arr_unificado.status_convoca, 
                                        v_aux_afore_origen_uni ,
                                        v_arr_unificado.tipo_movimiento
                                       
      --DISPLAY "v_aux_afore_origen_uni: ",v_aux_afore_origen_uni CLIPPED
      CALL  fn_obtiene_nombre_afore( v_aux_afore_origen_uni ) RETURNING   v_arr_unificado.afore_origen  
      LET v_arr_unificado.afore_origen   =  v_aux_afore_origen_uni||"-"||v_arr_unificado.afore_origen
      CASE v_arr_unificado.sexo
	      WHEN 1 LET v_arr_unificado.sexo="MASCULINO"
	      WHEN 2 LET v_arr_unificado.sexo="FEMENINO"
      END CASE 
--SE CAMBIO CONSULTA DE UNIQUE, por MAX
             LET v_sql_txtop21="\n SELECT  MAX  (f_actualiza) ",
                         "\n FROM  glo_folio f, uni_det_unificador u ",
                         "\n WHERE  f.folio= u.folio_unificacion "
--DISPLAY v_sql_txtop21                         
        PREPARE c_op_21 FROM  v_sql_txtop21 
        EXECUTE c_op_21 INTO v_arr_unificado.fecha_notificacion_op21
      LET nombre=v_arr_unificado.nombre_unificado CLIPPED," " ,v_arr_unificado.paterno_unificado CLIPPED," ",v_arr_unificado.materno_unificado CLIPPED
                          
      OPEN WINDOW w_cuenta_unificado WITH FORM "UNIC013_1" ATTRIBUTE(STYLE="dialog")
	      CALL FGL_SETTITLE("Consulta unificación de cuentas  del Unificado")

         DISPLAY BY NAME v_arr_unificado.folio_proceso,
                         v_arr_unificado.folio_liquidacion,
                         v_arr_unificado.nss_unificado,
                         nombre,
                         v_arr_unificado.rfc,
                         v_arr_unificado.curp,
                         v_arr_unificado.nombre_imss,
                         v_arr_unificado.sexo,
                         v_arr_unificado.tipo_registro,
                         v_arr_unificado.status_convoca, 
                         v_arr_unificado.afore_origen ,
                         v_arr_unificado.tipo_movimiento,       
                         v_arr_unificado.fecha_notificacion_op21
        MENU 
     
           ON ACTION  Regresar 
              EXIT MENU 
      
           ON ACTION  Imprimir 
           --Genera el reporte de Unificado IMSS
               # Recupera la ruta de listados en el que se enviara el archivo
               CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
               # Se indica que el reporte usara la plantilla creada
               IF fgl_report_loadCurrentSettings("UNIG011.4rp") THEN
                  CALL fgl_report_selectDevice("PDF")
                  LET v_nom_reporte = "safreviv" CLIPPED || "-UNIG011-","00000","-","00000","-","00000"||".pdf"
                  CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)                  
                  LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>", v_nom_reporte CLIPPED,"</a>"
                  CALL genera_reporte_consulta_unificado(v_arr_unificado.*, nombre) RETURNING v_si_indice2               
                 IF(v_si_indice2 = 0)THEN
                    CALL fn_mensaje("Atención","No se pudo generar el reporte", "information")
                 END IF
               ELSE
                  DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
               END IF
               DISPLAY v_s_reporte TO v_s_reporte
        END MENU
     
   CLOSE WINDOW w_cuenta_unificado
END FUNCTION 
-------------------------------------------------------
#OBJETIVO: Consultar los detalles de la liquidación del UNIFOCADOR 
FUNCTION consulta_liquidados(v_folio, v_nss)
	DEFINE v_folio      DECIMAL(9,0),
       	 v_valor      SMALLINT,
         v_nss      LIKE afi_derechohabiente.nss 

   OPEN WINDOW w_liq_unificador WITH FORM "UNIC014" ATTRIBUTE(STYLE="dialog")
        CALL FGL_SETTITLE("Consulta liquidación de cuentas  del Unificador")        
        DISPLAY v_folio, v_nss
        CALL  llena_consulta_liquidados_unificador(v_folio, v_nss) RETURNING v_valor
        --DISPLAY "v_arr_liq_unificador: ", v_arr_liq_unificador[2].*
			DIALOG ATTRIBUTES (UNBUFFERED)
				DISPLAY ARRAY v_arr_liq_unificador  TO tb_liq_unificador.*
					BEFORE  ROW
					--DISPLAY "posición :",arr_curr() 

					CALL llena_consulta_liquidados_unificado(v_arr_liq_unificador[ARR_CURR()].unificador, v_folio,v_nss)
					BEFORE DISPLAY
					IF v_valor = 1 THEN
						EXIT DIALOG	
					END IF
					END DISPLAY
			
				DISPLAY ARRAY v_arr_liq_unificado  TO tb_liq_unificado.*                                    
				END DISPLAY

				ON ACTION CLOSE
					EXIT DIALOG 
			END DIALOG
         
   CLOSE WINDOW w_liq_unificador

END FUNCTION 

#OBJETIVO: Llenar los datos de los liquidados UNIFICADOR
FUNCTION llena_consulta_liquidados_unificador(v_folio,v_nss)
DEFINE v_folio              DECIMAL(9,0),
			 v_sqltxt             STRING,
			 indx                 INTEGER ,
       v_nss                LIKE afi_derechohabiente.nss ,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_consulta_ctamov    STRING,
       v_tabla_cta_movimientoX VARCHAR(20),
       p_ind_criterio       SMALLINT,
       p_folio_ctamov       DECIMAL(9,0),
       p_fecha_ctamov       DATE
       

   SELECT id_derechohabiente
   INTO v_id_derechohabiente
   FROM afi_derechohabiente
   WHERE nss = v_nss
   
   LET indx =1 
   
   IF(v_folio IS NOT NULL AND v_id_derechohabiente IS NOT NULL )THEN         

      --Se consulta tabla de movimientos en base al folio de liquidación    
      LET p_ind_criterio = 0
      LET p_folio_ctamov = v_folio
      LET p_fecha_ctamov = NULL

      LET v_consulta_ctamov = "EXECUTE FUNCTION fn_tab_movimiento (?, ?, ?) "
      PREPARE prp_consulta_ctamov FROM v_consulta_ctamov 
      EXECUTE prp_consulta_ctamov USING p_ind_criterio, 
                                        p_folio_ctamov, 
                                        p_fecha_ctamov  
                                   INTO v_tabla_cta_movimientoX
   
      LET v_sqltxt= "\n SELECT b.nss_unificador,",
                    "\n        a.origen,",
                    "\n        a.subcuenta ||' - '|| cs.subcuenta_desc,",  
                    "\n        a.fondo_inversion ||' - '|| cf.razon_social,",
                    "\n        a.monto_acciones,",
                    "\n        a.monto_pesos,",
                    "\n        a.f_liquida",
                    "\n   FROM ", v_tabla_cta_movimientoX ," a, ",  
                    "\n        uni_det_unificador b, ", 
                    "\n        cat_subcuenta cs,",
                    "\n        cat_fondo_local cf ",
                    "\n  WHERE a.folio_liquida      = b.folio_liquidacion",
                    "\n    AND a.id_referencia      = b.id_unificador",
                    "\n    AND a.subcuenta          = cs.subcuenta",
                    "\n    AND a.fondo_inversion    = cf.fondo",
                    "\n    AND a.folio_liquida      = ", v_folio,
                    "\n    AND a.id_derechohabiente = ", v_id_derechohabiente  

      PREPARE c_liqui_unificador FROM  v_sqltxt   
      DECLARE  c_liq_unificador CURSOR FOR c_liqui_unificador 

      FOREACH  c_liq_unificador INTO v_arr_liq_unificador[indx].*      
         LET indx = indx + 1 
      END FOREACH

      CALL v_arr_liq_unificador.deleteElement(v_arr_liq_unificador.getLength())
   END IF

   IF(indx = 1)THEN
      CALL fn_mensaje("Atención", "No existen Información con los criterios dados","info")
      RETURN 1
   END IF 

   RETURN 0      
END FUNCTION   

#OBJETIVO: Llenar los datos de los liquidados UNIFICADO
FUNCTION llena_consulta_liquidados_unificado (p_nss_unificador, p_folio,v_nss)
DEFINE p_nss_unificador     CHAR(11),
       p_folio              DECIMAL(9,0),
       v_sqltxt             STRING,
       indx1                INTEGER ,
       v_nss                LIKE afi_derechohabiente.nss ,
       v_nsscta2            LIKE  uni_det_unificado.nsscta2,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_consulta_ctamov    STRING,
       v_tabla_cta_movimientoX VARCHAR(20),
       p_ind_criterio       SMALLINT,
       p_folio_ctamov       DECIMAL(9,0),
       p_fecha_ctamov       DATE

   SELECT id_derechohabiente
   INTO   v_id_derechohabiente
   FROM   afi_derechohabiente
   WHERE  nss = v_nss

   --Se consulta tabla de movimientos en base al folio de liquidación    
   LET p_ind_criterio = 0
   LET p_folio_ctamov = p_folio
   LET p_fecha_ctamov = NULL

   LET v_consulta_ctamov = "EXECUTE FUNCTION fn_tab_movimiento (?, ?, ?) "
   PREPARE prp_consulta_ctamov_ado FROM v_consulta_ctamov 
   EXECUTE prp_consulta_ctamov_ado USING p_ind_criterio, 
                                         p_folio_ctamov, 
                                         p_fecha_ctamov  
                                    INTO v_tabla_cta_movimientoX

   LET v_sqltxt= "\n SELECT a.nsscta1,",
                 "\n        origen,",
                 "\n        b.subcuenta ||' - '|| cs.subcuenta_desc,",
                 "\n        b.fondo_inversion ||' - '|| cf.razon_social,",
                 "\n        monto_acciones,",
                 "\n        monto_pesos,",
                 "\n        c.f_liquidacion",
                 "\n FROM   uni_det_unificado a, ",
                 "\n ",     v_tabla_cta_movimientoX, " b,",
                 "\n        uni_det_unificador c, ",
                 "\n        cat_subcuenta cs,",
                 "\n        cat_fondo_local cf ",
                 "\n WHERE  a.id_derechohabiente = b.id_derechohabiente",
                 "\n AND    a.id_unificador      = c.id_unificador",
                 "\n AND    b.id_referencia      = a.id_unificado",
                 "\n AND    b.subcuenta          = cs.subcuenta",
                 "\n AND    b.fondo_inversion    = cf.fondo",
                 "\n AND    c.id_derechohabiente =  ",v_id_derechohabiente

   LET indx1=1
--DISPLAY v_sqltxt
   DECLARE c_liq_unificado CURSOR FROM v_sqltxt
   FOREACH c_liq_unificado INTO v_arr_liq_unificado[indx1].*
      LET indx1= indx1+1
   END FOREACH     
   CALL v_arr_liq_unificado.deleteElement(v_arr_liq_unificado.getLength())   
END FUNCTION 

#OBJETIVO: Obtiene la descripción de la afore
FUNCTION fn_obtiene_nombre_afore(v_id_afore)
DEFINE v_id_afore     SMALLINT,
       v_nombre_afore CHAR(18),
       v_sql_txt      STRING 

   IF(v_id_afore IS NOT NULL)THEN
      LET v_sql_txt=" SELECT  afore_desc  FROM  cat_afore WHERE afore_cod=",v_id_afore
      
      PREPARE c_nombre_afore FROM v_sql_txt
      EXECUTE c_nombre_afore INTO v_nombre_afore
   ELSE
      LET v_nombre_afore = ''
   END IF

   RETURN v_nombre_afore
END FUNCTION

#OBJETIVO: Consultar los registros líquidados de solo INFONAVIT
FUNCTION consulta_liquidados_INFONAVIT(v_folio,id_derechohabiente,p_id_inf_unificador)
DEFINE v_folio            DECIMAL(9,0),
		 id_derechohabiente INTEGER ,
 		 v_valor            SMALLINT,
         p_id_inf_unificador DECIMAL(9,0)
	OPEN WINDOW w_liq_unificador_infonavit WITH FORM "UNIC014.4fd"    
		CALL FGL_SETTITLE("Consulta liquidación de cuentas  del Unificador")

--DISPLAY "enviados >>", v_folio , id_derechohabiente        
		CALL llena_consulta_liquidados_unificador_INFONAVIT(v_folio,id_derechohabiente)
           RETURNING v_valor
        
		DIALOG ATTRIBUTES (UNBUFFERED)
			DISPLAY ARRAY v_arr_liq_unificador  TO tb_liq_unificador.*            
				BEFORE  ROW  
					IF v_arr_liq_unificador.getLength()>0 THEN                     
						CALL llena_consulta_liquidados_unificado_INFONAVIT(v_folio,id_derechohabiente,p_id_inf_unificador)            	       
					END IF  
				BEFORE DISPLAY
					IF v_valor = 1 THEN
						EXIT DIALOG	
					END IF			
			END DISPLAY			
			DISPLAY ARRAY v_arr_liq_unificado  TO tb_liq_unificado.*                                    
			END DISPLAY
			
			ON ACTION CLOSE
				EXIT DIALOG 
		END DIALOG  		
	CLOSE WINDOW w_liq_unificador_infonavit
END FUNCTION 

#OBJETIVO: Llenar la consulta de liquidados UNIFICADOR INFONAVIT 
FUNCTION llena_consulta_liquidados_unificador_INFONAVIT (v_folio,v_id_derechohabiente)
DEFINE v_folio              DECIMAL(9,0),
       v_sqltxt             STRING,
       indx                 INTEGER ,
       v_id_derechohabiente DECIMAL(9,0),
       v_consulta_ctamov    STRING,
       v_tabla_cta_movimientoX VARCHAR(20),
       p_ind_criterio       SMALLINT,
       p_folio_ctamov       DECIMAL(9,0),
       p_fecha_ctamov       DATE

   --Se consulta tabla de movimientos en base al folio de liquidación    
   LET p_ind_criterio = 0
   LET p_folio_ctamov = v_folio
   LET p_fecha_ctamov = NULL

   LET v_consulta_ctamov = "EXECUTE FUNCTION fn_tab_movimiento (?, ?, ?) "
   PREPARE prp_consulta_ctamov_dori FROM v_consulta_ctamov 
   EXECUTE prp_consulta_ctamov_dori USING p_ind_criterio, 
                                          p_folio_ctamov, 
                                          p_fecha_ctamov  
                                     INTO v_tabla_cta_movimientoX

   LET v_sqltxt= "\n SELECT b.nss,",
                 "\n        a.origen,",
                 "\n        a.subcuenta,",  
                 "\n        a.fondo_inversion,",
                 "\n        a.monto_acciones,",
                 "\n        a.monto_pesos,",
                 "\n        a.f_liquida",
                 "\n 	FROM ", v_tabla_cta_movimientoX, " a, ", 
                 "\n        uni_inf_unificador b",
                 "\n  WHERE a.folio_liquida = b.folio_liquidacion",
                 "\n  AND   a.id_referencia = b.id_inf_unificador",                     
                 "\n  AND   a.folio_liquida = ",v_folio,
                 "\n  AND   b.id_derechohabiente=", v_id_derechohabiente

   LET indx =1
   
   DECLARE  c_liq_unificador_infonavit CURSOR FROM v_sqltxt 
   FOREACH  c_liq_unificador_infonavit INTO v_arr_liq_unificador[indx].*         
      SELECT subcuenta_desc
      INTO   v_arr_liq_unificador[indx].subcuenta
      FROM   cat_subcuenta
      WHERE  subcuenta = v_arr_liq_unificador[indx].subcuenta
      LET indx = indx + 1 
   END FOREACH
   CALL v_arr_liq_unificador.deleteElement(v_arr_liq_unificador.getLength())
   IF(indx = 1)THEN
      CALL fn_mensaje("Atención",
              "No existen Información con los criterios dados",
              "info")
         RETURN 1
   ELSE 
      RETURN 0      
   END IF

END FUNCTION

#OBJETIVO: Llenar la consulta de liquidados UNIFICADO INFONAVIT 
FUNCTION llena_consulta_liquidados_unificado_INFONAVIT(v_folio,v_id_derechohabiente, p_id_inf_unificador)
DEFINE v_nss_unificador CHAR(11),
       v_folio      DECIMAL(9,0),
       v_sqltxt     STRING,
       indx1         INTEGER  ,
       v_id_derechohabiente DECIMAL(9,0),
       p_id_inf_unificador DECIMAL(9,0),
       v_consulta_ctamov    STRING,
       v_tabla_cta_movimientoX VARCHAR(20),
       p_ind_criterio       SMALLINT,
       p_folio_ctamov       DECIMAL(9,0),
       p_fecha_ctamov       DATE

   --Se consulta tabla de movimientos en base al folio de liquidación    
   LET p_ind_criterio = 0
   LET p_folio_ctamov = v_folio
   LET p_fecha_ctamov = NULL

   LET v_consulta_ctamov = "EXECUTE FUNCTION fn_tab_movimiento (?, ?, ?) "
   PREPARE prp_consulta_ctamov_adoi FROM v_consulta_ctamov 
   EXECUTE prp_consulta_ctamov_adoi USING p_ind_criterio, 
                                          p_folio_ctamov, 
                                          p_fecha_ctamov  
                                     INTO v_tabla_cta_movimientoX

   LET v_sqltxt= "\n SELECT a.nss,",   
                 "\n     		b.origen,",          
                 "\n     		b.subcuenta,",       
                 "\n     		b.fondo_inversion,", 
                 "\n     		b.monto_acciones,", 
                 "\n     		b.monto_pesos,",    
                 "\n     		b.f_liquida",       
                 "\n FROM   uni_inf_unificado a, ",
                 "\n",     	v_tabla_cta_movimientoX , " b",
                 "\n WHERE  a.id_derechohabiente=b.id_derechohabiente",
                 "\n AND    a.id_unificador=", p_id_inf_unificador,
                 "\n AND    b.folio_liquida = ", v_folio,
                 "\n GROUP BY 1,2,3,4,5,6,7"

   CALL v_arr_liq_unificado.clear()        
   LET indx1=1            
   DECLARE  c_liq_unificado_infonavit CURSOR FROM   v_sqltxt 
   FOREACH  c_liq_unificado_infonavit INTO v_arr_liq_unificado[indx1].*
      SELECT subcuenta_desc
      INTO   v_arr_liq_unificado[indx1].subcuenta1
      FROM   cat_subcuenta
      WHERE  subcuenta = v_arr_liq_unificado[indx1].subcuenta1
      
      LET indx1= indx1+1                          
   END FOREACH

   CALL v_arr_liq_unificado.deleteElement(v_arr_liq_unificado.getLength())

END FUNCTION    

FUNCTION Consulta_Uni_Cuentas_Unificado_INFONAVIT(v_foliounificado, v_id_inf_unificado, v_nssunificado)
   

      DEFINE 
          v_arr_unificado RECORD 
          id_inf_unificado         DECIMAL(9,0),  
          folio_proceso            DECIMAL(9,0),     
          folio_liquidacion        DECIMAL(9,0),     
          nss_unificado            char(11),       
          nombre_unificado         CHAR(100),
          paterno_unificado        CHAR(100),
          materno_unificado        CHAR(100),
          rfc                      CHAR(13), 
          curp                     CHAR(18), 
          nombre_imss              CHAR(100),
          sexo                     CHAR(10), 
          tipo_movimiento          CHAR(35),
          tipo_registro            CHAR(30),
          status_convoca           CHAR(30),
          fecha_notificacion_op21  DATE ,   
          fecha_unificacion        DATE ,  
          fecha_notificacion_op22  DATE,    
          clave_afore_receptora    CHAR(13),           
          afore_origen             CHAR(13)
        END RECORD ,                             
        nombre                       CHAR(100),      
        v_sql_txt                   STRING,
        v_foliounificado           DECIMAL(9,0),
        v_id_inf_unificado         DECIMAL(9,0),
        v_nssunificado             CHAR (11)

        INITIALIZE v_arr_unificado   TO  NULL 
        LET v_sql_txt=  "\n SELECT",
                        "\n a.folio_unificacion,",
                        "\n b.folio_liquidacion,",
                        "\n a.nss, ",            
                        "\n a.nombre,",          
                        "\n b.curp,",
                        "\n b.rfc, ",            
                        "\n b.sexo,",            
                    	"\n a.diagnostico, ",    
                        "\n a.tipo_registro,",   
                        "\n b.f_notificacion ,",
                        "\n b.f_movimiento", 
                        "\n FROM  uni_inf_unificado a, OUTER uni_inf_unificador b",
                        "\n WHERE a.id_unificador = b.id_inf_unificador",
                        "\n AND  a.folio_unificacion = ?",
                        "\n AND  a.id_inf_unificado = ?",
                        "\n AND  a.nss = ?"
                        -- "\n AND  b.folio_liquidacion=?",                       
      PREPARE c_consulta_unificadoINFONAVIT FROM  v_sql_txt 
      EXECUTE c_consulta_unificadoINFONAVIT USING v_foliounificado,v_id_inf_unificado,v_nssunificado
                                INTO    v_arr_unificado.folio_proceso,
                                        v_arr_unificado.folio_liquidacion,
                                        v_arr_unificado.nss_unificado,
                                        v_arr_unificado.nombre_imss,
                                        v_arr_unificado.curp,
                                        v_arr_unificado.rfc,
                                        v_arr_unificado.sexo,
                                        v_arr_unificado.tipo_movimiento,
                                        v_arr_unificado.tipo_registro,
                                        v_arr_unificado.fecha_notificacion_op21,
                                        v_arr_unificado.fecha_unificacion  
                                        --v_arr_unificado.nombre_unificado,
                                        --v_arr_unificado.paterno_unificado,  
                                        
                                       
   --   CALL  fn_obtiene_nombre_afore( v_aux_afore_origen_uni ) RETURNING   v_arr_unificado.afore_origen  
    --   LET v_arr_unificado.afore_origen   =  v_aux_afore_origen_uni||"-"||v_arr_unificado.afore_origen
      CASE v_arr_unificado.sexo
      WHEN 1 LET v_arr_unificado.sexo="Masculino"
      WHEN 2 LET v_arr_unificado.sexo="Femenino"
      END CASE 
      
      LET nombre=v_arr_unificado.nombre_unificado CLIPPED," " ,v_arr_unificado.paterno_unificado CLIPPED," ",v_arr_unificado.materno_unificado CLIPPED
                          
      OPEN WINDOW w_cuenta_unificado_INFONAVIT WITH FORM "UNIC013" ATTRIBUTE(STYLE="dialog")
        CALL FGL_SETTITLE("Consulta unificación de cuentas  del Unificado")
        --DISPLAY 'op21: ',v_arr_unificado.fecha_notificacion_op21,'UNI: ', v_arr_unificado.fecha_unificacion

        DISPLAY BY NAME v_arr_unificado.folio_proceso,
                        v_arr_unificado.nss_unificado,
                        nombre,
                        v_arr_unificado.rfc,
                        v_arr_unificado.curp,
                        v_arr_unificado.nombre_imss,
                        v_arr_unificado.sexo,
                        v_arr_unificado.tipo_registro,
                        v_arr_unificado.status_convoca, 
                        v_arr_unificado.afore_origen ,
                        v_arr_unificado.fecha_unificacion ,
                        v_arr_unificado.tipo_movimiento,
                        v_arr_unificado.fecha_notificacion_op21       
        MENU 
      
               
     
           ON ACTION  Regresar 
              EXIT MENU 
      
           ON ACTION  Imprimir 
           --Genera el reporte de Unificado infonavit
               # Recupera la ruta de listados en el que se enviara el archivo
               CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
               # Se indica que el reporte usara la plantilla creada
               IF fgl_report_loadCurrentSettings("UNIG013.4rp") THEN
                  CALL fgl_report_selectDevice("PDF")
                  LET v_nom_reporte = "safreviv" CLIPPED || "-UNIG013-","00000","-","00000","-","00000"||".pdf"
                  CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
                  
                  LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>",
                                                                            v_nom_reporte CLIPPED,"</a>"
                  CALL genera_reporte_consulta_unificado_INFONAVIT(v_arr_unificado.*) RETURNING v_si_indice2
               
                 IF(v_si_indice2 = 0)THEN
                    CALL fn_mensaje("Atención",
                                    "No se pudo generar el reporte",
                                    "information")
                 END IF
               ELSE
                  DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
               END IF
                DISPLAY v_s_reporte TO v_s_reporte
        END MENU
       
   CLOSE WINDOW w_cuenta_unificado_INFONAVIT
END FUNCTION 

FUNCTION Consulta_Uni_Cuentas_Unificador_INFONAVIT(v_foliounificador,  v_nssunificador)
DEFINE 
        v_arr_unificador RECORD                               
        folio_proceso            DECIMAL(9,0),      
        folio_liquidacion        DECIMAL(9,0),                                           
        nss_unificador           char(11),          
        nombre_unificador        CHAR(100),                      
        paterno_unificador       CHAR(100),         
        materno_unificador       CHAR(100),         
        rfc                      CHAR(13),          
        curp                     CHAR(18),          
        nombre_imss              CHAR(100),         
        sexo                     CHAR(10),          
        tipo_movimiento          CHAR(35),                    
        tipo_registro            CHAR(30),          
        status_convoca           CHAR(30),                  
        fecha_notificacion_op21  DATE ,                                               
        fecha_unificacion        DATE ,             
        fecha_notificacion_op22  DATE,              
        afore_receptora    CHAR(13),           
        afore_origen     CHAR(13)            
     END RECORD ,  
        nombre                   CHAR(100),
        v_sql_txt                   STRING,
        v_foliounificador           DECIMAL(9,0),
        v_nssunificador             CHAR (11),
        v_aux_afore_receptora      SMALLINT,
        v_aux_afore_origen         SMALLINT 
         INITIALIZE v_arr_unificador   TO  NULL 
        LET v_sql_txt=  "\n SELECT folio_unificacion,",
                        "\n        folio_liquidacion ,",
                        "\n        nss,",
                        "\n        nombre,",                                             
                        "\n        rfc,",
                        "\n        curp,",
                        "\n        sexo,",
                        "\n        tipo_registro,",
                        "\n        f_notificacion,",
                        "\n        f_movimiento",
                        "\n     FROM uni_inf_unificador",
                        "\n        WHERE folio_unificacion=?", 
                        "\n          AND nss=?"

     PREPARE c_consulta_unificador_INFONAVIT FROM  v_sql_txt 
     EXECUTE c_consulta_unificador_INFONAVIT USING v_foliounificador,  v_nssunificador  
                                INTO    v_arr_unificador.folio_proceso,            
                                        v_arr_unificador.folio_liquidacion,        
                                        v_arr_unificador.nss_unificador,                      
                                        v_arr_unificador.nombre_unificador ,       
                                        v_arr_unificador.rfc,                      
                                        v_arr_unificador.curp,                     
                                        v_arr_unificador.sexo,                     
                                        v_arr_unificador.tipo_registro,            
                                        v_arr_unificador.fecha_notificacion_op21,
                                        v_arr_unificador.fecha_unificacion  
 -- v_arr_unificador.nombre_imss,   
    -- CALL  fn_obtiene_nombre_afore( v_aux_afore_receptora) returning  v_arr_unificador.afore_receptora 
    --   LET v_arr_unificador.afore_receptora =v_aux_afore_receptora||"-"||v_arr_unificador.afore_receptora 
    --  CALL  fn_obtiene_nombre_afore( v_aux_afore_origen   ) returning  v_arr_unificador.afore_origen     
    --   LET v_arr_unificador.afore_origen=v_aux_afore_origen||"-"||v_arr_unificador.afore_origen
      LET nombre=v_arr_unificador.nombre_unificador CLIPPED," " ,v_arr_unificador.paterno_unificador CLIPPED," ",v_arr_unificador.materno_unificador CLIPPED
       CASE v_arr_unificador.sexo
      WHEN 1 LET v_arr_unificador.sexo="Masculino"
      WHEN 2 LET v_arr_unificador.sexo="Femenino"
      END CASE                    
      OPEN WINDOW w_cuenta_unificador_INFONAVIT WITH FORM "UNIC012" ATTRIBUTE(STYLE="dialog")
    
        CALL FGL_SETTITLE("Consulta unificación de cuentas  del Unificador")

       --DISPLAY "opp21: ", v_arr_unificador.fecha_notificacion_op21
       --DISPLAY "uni: ",v_arr_unificador.fecha_unificacion
        DISPLAY BY NAME v_arr_unificador.folio_proceso,            
                        v_arr_unificador.folio_liquidacion,        
                        v_arr_unificador.nss_unificador,                      
                        nombre,      
                        v_arr_unificador.rfc,                      
                        v_arr_unificador.curp,                     
                        v_arr_unificador.nombre_imss,              
                        v_arr_unificador.sexo,                     
                        v_arr_unificador.tipo_registro,            
                        v_arr_unificador.status_convoca,           
                        v_arr_unificador.fecha_notificacion_op21,  
                        v_arr_unificador.afore_origen,    
                        v_arr_unificador.afore_receptora,
                        v_arr_unificador.fecha_unificacion       
                 
        MENU 
    
         ON ACTION  Regresar 
             EXIT MENU 
      
         ON ACTION  Imprimir 
         --Genera el reporte de Unificador INFONAVIT
               # Recupera la ruta de listados en el que se enviara el archivo
               CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
               # Se indica que el reporte usara la plantilla creada
               IF fgl_report_loadCurrentSettings("UNIG012.4rp") THEN
                  CALL fgl_report_selectDevice("PDF")
                  LET v_nom_reporte = "safreviv" CLIPPED || "-UNIG012-","00000","-","00000","-","00000"||".pdf"
                  CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
                                  
                  LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>",
                                                                            v_nom_reporte CLIPPED,"</a>"
                  CALL genera_reporte_consulta_unificador_INFONAVIT(v_arr_unificador.*, nombre) RETURNING v_si_indice2
               
                 IF(v_si_indice2 = 0)THEN
                    CALL fn_mensaje("Atención",
                                    "No se pudo generar el reporte",
                                    "information")
                 END IF
               ELSE
                  DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
               END IF
                DISPLAY v_s_reporte TO v_s_reporte

       
       END MENU 
   CLOSE WINDOW w_cuenta_unificador_INFONAVIT


END FUNCTION 

-- Funcion que muestra información del derechohabiente
FUNCTION fn_preconsulta(p_nss,p_usuario_cod,
                                p_tipo_ejecucion, p_s_titulo)
DEFINE p_nss   CHAR(11),
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo CHAR(25),
       lc_qry  STRING,
       cont    INTEGER,
       x_flg   SMALLINT,
       arr_busqueda DYNAMIC ARRAY OF RECORD
          nss             CHAR(11),
          rfc             CHAR(13),
          curp            CHAR(18),
          ap_paterno_af   CHAR(40),
          ap_materno_af   CHAR(40),
          nombres_af      CHAR(40)
       END RECORD,
       v_c_nss CHAR(11)

   LET x_flg = 0
   CALL arr_busqueda.clear()
   
   LET lc_qry = "SELECT a.nss,",
                " a.rfc,",
                " a.curp,",
                " a.ap_paterno_af,",
                " a.ap_materno_af,",
                " a.nombre_af",
                " FROM afi_derechohabiente a ",
                " WHERE nss = ","'",p_nss CLIPPED,"'"
            --DISPLAY "CONSULTA 1:",lc_qry
            PREPARE prp_pre_busqueda FROM lc_qry
            DECLARE cur_pre_busqueda CURSOR FOR prp_pre_busqueda
            
            LET cont= 1

            FOREACH cur_pre_busqueda INTO arr_busqueda[cont].*
                LET cont = 1 + cont

                IF cont > 32767 THEN
                    CALL fn_mensaje("Aviso","SE SOBREPASÓ LA CAPACIDAD MÁXIMA DEL ARREGLO","exclamation")
                    LET INT_FLAG = TRUE
                END IF
            END FOREACH
            
            IF cont = 1 THEN
                CALL fn_mensaje("Aviso","NO EXISTEN REGISTROS CON LOS DATOS ESPECIFICADOS","exclamation")
                LET INT_FLAG = TRUE
            ELSE
               CALL fn_consulta_afi(p_nss,p_usuario_cod,
                                p_tipo_ejecucion, p_s_titulo)
            END IF
            
            --CALL set_count(cont-1)
   
   --OPEN WINDOW InfoDerecho WITH FORM "UNIC014" ATTRIBUTE(STYLE="dialog")
   --   IF (cont-1) >= 1 THEN
   --      
   --      DISPLAY ARRAY arr_busqueda TO tb2.*
   --
   --         ON ACTION ACCEPT
   --            LET v_c_nss = arr_busqueda[ARR_CURR()].nss
   --            LET INT_FLAG = FALSE
   --            EXIT DISPLAY
   --
   --         ON ACTION cancel
   --            LET INT_FLAG = TRUE
   --            EXIT DISPLAY
   --
   --      END DISPLAY
   --
   --      IF NOT INT_FLAG THEN
   --          LET x_flg = 1
   --          LET INT_FLAG = FALSE
   --      ELSE
   --          LET x_flg = 0
   --      END IF
   --   END IF
   --   CLEAR FORM
   --   
   --   IF x_flg THEN
   --      CALL fn_consulta_afi(p_nss,p_usuario_cod,
                                --p_tipo_ejecucion, p_s_titulo)
   --   END IF
   --
   --CLOSE WINDOW InfoDerecho

END FUNCTION

#función para desplegar pantalla de saldos del asignado
FUNCTION fn_eje_consulta_saldo(p_nss)
DEFINE p_nss   CHAR(11),
       v_query STRING,
       v_count SMALLINT

   LET v_query = "SELECT COUNT(*)",
                 "\n FROM afi_derechohabiente",
                 "\n WHERE nss = '",p_nss,"'"

      PREPARE prp_consulta_saldo FROM v_query
      EXECUTE prp_consulta_saldo INTO v_count
      
      IF v_count <> 0 THEN
         CALL fn_saldo_id_derechohabiente(p_nss)	
      ELSE	
            CALL fn_mensaje("Consulta Saldo",
                            "No existen datos con el criterio dado",
                            "about")
      END IF

END FUNCTION

#función que despliega la información del asignado elegido
FUNCTION fn_consulta_afi(p_nss,p_usuario_cod,
                                p_tipo_ejecucion, p_s_titulo)
DEFINE reg_derechohabiente RECORD
          nss              CHAR(11),
          rfc              CHAR(13),
          curp             CHAR(18),
          f_nacimiento     DATE,
          ap_paterno_af    CHAR(40),
          ap_materno_af    CHAR(40),
          nombre_af        CHAR(40),
          nombre_imss      CHAR(50),
          desc_tipo_trab   CHAR(20),
          desc_origen      CHAR(20),
          desc_ind_credito CHAR(30),
          num_credito      DECIMAL(10,0),
          desc_tpo_credito CHAR(30)
       END RECORD,
       domicilio_1 DYNAMIC ARRAY OF RECORD
          dom_cod            SMALLINT,
          dom_desc           CHAR(15),
          envio_desc         CHAR(15)
       END RECORD,
       domicilio_2 DYNAMIC ARRAY OF RECORD
          id_domicilio       INTEGER,
          ind_envio          CHAR(1)
       END RECORD,
       correo_1 DYNAMIC ARRAY OF RECORD
          correo_elect       CHAR(50)
       END RECORD,
       tel_1 DYNAMIC ARRAY OF RECORD
          tel_cod            SMALLINT,
          tel_desc           CHAR(15),
          cve_lada           CHAR(3),
          telefono           CHAR(40),
          extension          CHAR(5),
          pais_cod           CHAR(3)
       END RECORD,
       v_id_derechohabiente       DECIMAL(9,0),
       p_nss                      CHAR(11),
       lc_qry                     STRING,
       i, ii, iii                 SMALLINT,
       cur_row                    SMALLINT,
       scr_row                    SMALLINT,
       p_usuario_cod              LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion           SMALLINT,
       p_s_titulo                 CHAR(25),
       v_id_derechohabiente_saldo DECIMAL(9,0)


   OPEN WINDOW DespliInfo WITH FORM ("UNIC015") ATTRIBUTE(STYLE="dialog")
      LET w = ui.window.getcurrent()
      LET w = ui.window.forName("DespliInfo")
      LET INT_FLAG = TRUE 
      
      CALL fgl_settitle("INFORMACIÓN DE DERECHOHABIENTES")
      
      LET lc_qry = " SELECT afi.id_derechohabiente,",
                           " afi.nss,",
                           " afi.rfc,",
                           " afi.curp,",
                           " afi.f_nacimiento,",
                           " afi.ap_paterno_af,",
                           " afi.ap_materno_af,",
                           " afi.nombre_af,",
                           " afi.nombre_imss,",
                           " DECODE(afi.tipo_trabajador,'S','SOLO INFONAVIT','I','IMSS'),",
                           " DECODE(afi.origen_afiliacion,'R','RECAUDACIÓN','U','UNIFICACIÓN','A','REGISTRO','S','SEPARACIÓN', 'I', 'REGISTRO INICIAL'),",
                           " DECODE(afi.id_credito,'0','SIN CRÉDITO','CON CRÉDITO'),",
                           " cre.num_credito,",
                           " cat.desc_credito",
                      " FROM afi_derechohabiente afi",
                      " LEFT JOIN cta_credito cre ON afi.id_derechohabiente = cre.id_derechohabiente",
                      " LEFT JOIN cat_tipo_credito cat on cre.tpo_credito = cat.tpo_credito",
                     " WHERE afi.nss = '", p_nss CLIPPED,"'"
    --DISPLAY "QUERY 2",lc_qry
    PREPARE qry_asig FROM lc_qry
    EXECUTE qry_asig INTO v_id_derechohabiente, reg_derechohabiente.*
    
    DISPLAY BY NAME reg_derechohabiente.*
    
    DECLARE cur_dom CURSOR FOR
    SELECT dom1.tpo_domicilio,
           dom1.id_domicilio,
           DECODE(tpo_domicilio,1,"PARTICULAR","OTRO"),
           dom1.ind_envio
    FROM   afi_domicilio dom1
    WHERE  dom1.id_derechohabiente = v_id_derechohabiente
    ORDER BY dom1.ind_envio DESC
    
    LET i = 1
    
    FOREACH cur_dom INTO domicilio_1[i].dom_cod,
                         domicilio_2[i].id_domicilio,
                         domicilio_1[i].dom_desc,
                         domicilio_2[i].ind_envio
    
       IF domicilio_2[i].ind_envio = "1" THEN
          LET domicilio_1[i].envio_desc = "CORRESPONDENCIA"
       ELSE
          LET domicilio_1[i].envio_desc = ""
       END IF
    
       LET i = i + 1
    END FOREACH
    
    DECLARE cur_correo CURSOR FOR
      SELECT corr1.valor
        FROM afi_contacto_electronico corr1
       WHERE corr1.id_derechohabiente = v_id_derechohabiente
    ORDER BY corr1.valor
    
    LET iii = 1
    FOREACH cur_correo INTO correo_1[iii].*
       LET iii = iii + 1
    END FOREACH
    
    IF (i) >= 1 THEN
    
       CALL SET_COUNT(i-1)
    
       CALL domicilio_1.deleteelement(domicilio_1.getlength())
       CALL tel_1.deleteelement(tel_1.getlength())
       CALL correo_1.deleteelement(correo_1.getlength())
    
       DIALOG ATTRIBUTES(UNBUFFERED,FIELD ORDER FORM)
          DISPLAY ARRAY domicilio_1 TO tb5.* 
    
          BEFORE DISPLAY
             CALL DIALOG.setactionhidden("close",1)
                MENU "Consulta"
                   BEFORE MENU
                      CALL ui.Interface.refresh()
                   ON ACTION saldo
                      --CALL fn_saldo_id_derechohabiente(p_nss)
                      SELECT id_derechohabiente
                        INTO v_id_derechohabiente_saldo
                        FROM afi_derechohabiente
                        WHERE nss = p_nss
            
                            CALL fn_eje_consulta(1,p_usuario_cod,v_id_derechohabiente_saldo, 
                                                 p_tipo_ejecucion, p_s_titulo)
                   ON ACTION regresar
                      CALL ui.Interface.refresh()
                      EXIT MENU
                END MENU
          BEFORE ROW
             LET cur_row = ARR_CURR()
             LET scr_row = SCR_LINE()
    
             CALL despliega_domicilio(v_id_derechohabiente,
                                      domicilio_2[cur_row].id_domicilio)
    
             ON ACTION cancel
                EXIT DIALOG
          END DISPLAY
    
            {DISPLAY ARRAY tel_1 TO tb15.*
               BEFORE DISPLAY
                  CALL DIALOG.setactionhidden("close",1)
               BEFORE ROW
                  LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()
               ON ACTION close
                  EXIT DIALOG
            END DISPLAY}
    
            DISPLAY ARRAY correo_1 TO tb7.*
               BEFORE DISPLAY
               BEFORE ROW
                  LET cur_row = ARR_CURR()
                  LET scr_row = SCR_LINE()
               ON ACTION cancel
                  EXIT DIALOG
            END DISPLAY
    
       END DIALOG
    ELSE
       ERROR "DERECHOHABIENTE NO TIENE DOMICILIO REGISTRADO"
    END IF
    
    --CLOSE WINDOW w1
    
    CLOSE WINDOW DespliInfo

END FUNCTION

#función para desplegar pantalla de saldos del asignado
FUNCTION fn_saldo_id_derechohabiente(p_nss)
DEFINE p_nss  CHAR(11),
       v_r_datos_generales RECORD
          v_nss              CHAR(11),
          v_rfc              CHAR(13),
          v_curp             CHAR(18),
          nombre_completo    CHAR(120),
          id_derechohabiente DECIMAL(9,0)
       END RECORD,
       arr_arbol DYNAMIC ARRAY OF RECORD
          subcuenta_desc     CHAR(60),
          siefore            SMALLINT,
          monto_pesos        DECIMAL(16,6),
          monto_acciones     DECIMAL(16,6),
          subcuenta          SMALLINT,
          movimiento         SMALLINT,
          padre_id           CHAR(40),
          id                 CHAR(40),
          nivel              SMALLINT
       END RECORD,
       arr_precios  DYNAMIC ARRAY OF RECORD
          precio_cod          SMALLINT,
          precio_sie          CHAR(30),
          precio_al_dia       DECIMAL(19,14)
       END RECORD,
       v_query          STRING,
       v_hoy            DATE,
       i                SMALLINT,
       vsdo_fin         DECIMAL(22,2),
       vsdo_ini         DECIMAL(22,2),
       vrendimiento     DECIMAL(22,2),
       v_subcuenta      SMALLINT,
       v_siefore        SMALLINT,
       v_movimiento     SMALLINT,
       resp_visualiza   SMALLINT,
       v_pos            SMALLINT,
       v_exe_edo_cuenta STRING --Variable para establecer la instruccion que genera el Estado de cuenta

   -- inicializa variables y crea tablas temporales para el procesos
   CALL fn_inicializa()
   
   -- Obtiene los datos personales del id derechohabiente
   SELECT nss,
          rfc,
          curp,
          TRIM(nombre_af)||' '||
          TRIM(ap_paterno_af)||' '||
          TRIM(ap_materno_af),
          id_derechohabiente
     INTO v_r_datos_generales.*
     FROM afi_derechohabiente
    WHERE nss = p_nss
    
    PREPARE prp_proc FROM "EXECUTE PROCEDURE sp_consulta_saldo(?)"
    EXECUTE prp_proc USING v_r_datos_generales.id_derechohabiente
    
    LET v_hoy = TODAY
    LET vsdo_fin = 0
    
    LET v_query = "SELECT gvf.fondo,",
                  " TRIM(cfl.razon_social),",
                  " gvf.precio_fondo",
                  " FROM glo_valor_fondo gvf, ",
                  " cat_fondo_local cfl ",
                  " WHERE gvf.f_valuacion = ? ",
                  " AND cfl.fondo       = gvf.fondo "  
   
      PREPARE prp_precio FROM v_query
      DECLARE cur_precio CURSOR FOR prp_precio
      LET i = 1
      FOREACH cur_precio USING v_hoy INTO arr_precios[i].*
         LET i = i + 1
      END FOREACH
      CLOSE cur_precio
      FREE cur_precio
      
      CALL arr_precios.deleteElement(i)
      	
      PREPARE prp_arbol FROM "SELECT * FROM tmp_arbol_saldo ORDER BY id "
      DECLARE cur_arbol CURSOR FOR prp_arbol
      
      LET i = 1
      FOREACH cur_arbol INTO arr_arbol[i].*
         IF arr_arbol[i].monto_pesos IS NULL THEN
            LET arr_arbol[i].monto_pesos = 0
         END IF
         
         IF arr_arbol[i].monto_acciones IS NULL THEN
            LET arr_arbol[i].monto_acciones = 0
         END IF
         
         IF arr_arbol[i].subcuenta = 0 THEN
            LET vsdo_fin   = vsdo_fin + arr_arbol[i].monto_pesos
         END IF
         
         LET i = i + 1
      END FOREACH
      CLOSE cur_arbol
      FREE cur_arbol
      
      CALL arr_arbol.deleteElement(i)
      LET i = i - 1 
      
   OPEN WINDOW saldo_id_derechohabiente WITH FORM "UNIC016" ATTRIBUTE(STYLE="dialog")
      LET w = ui.Window.forName("saldo_id_derechohabiente")
      LET f = w.getForm()
      LET INT_FLAG = TRUE 
      CALL fgl_settitle("CONSULTA DE SALDO")
      CALL ui.Interface.refresh()
      
      DISPLAY BY NAME v_r_datos_generales.v_nss,
                      v_r_datos_generales.v_rfc,
                      v_r_datos_generales.v_curp,
                      v_r_datos_generales.nombre_completo
      
      DIALOG ATTRIBUTES(UNBUFFERED)
         DISPLAY ARRAY arr_arbol TO tree.*
         BEFORE DISPLAY
            CALL DIALOG.setactionhidden("close",1)
         BEFORE ROW
            LET v_pos = ARR_CURR() 
               IF arr_arbol[v_pos].nivel > 1 THEN
                  LET v_subcuenta = arr_arbol[v_pos].subcuenta
                  LET v_siefore   = arr_arbol[v_pos].siefore
                  LET v_movimiento= arr_arbol[v_pos].movimiento 
               
                  CALL despliega_detalle_mov(v_subcuenta,v_siefore, v_movimiento)
                       RETURNING resp_visualiza
               
                  IF NOT resp_visualiza THEN
                     CALL r_detmov.clear()
                  ELSE
                     CALL r_detmov.deleteelement(r_detmov.getlength())
                  END IF
               ELSE
                  CALL r_detmov.clear()
               END IF
               
               IF vsdo_fin IS NULL THEN
                  LET vsdo_fin = 0
               END IF

               LET vrendimiento = vsdo_fin - vsdo_ini

               DISPLAY vsdo_fin TO sdo_fin
               
            #Boton que invoca la generacion del estado de cuenta
         ON ACTION edo_cuenta
            LET v_exe_edo_cuenta = "cd /ds/safreviv/srv/bin/;fglrun SRVP09.42r '", v_r_datos_generales.v_nss,"'"
            RUN v_exe_edo_cuenta
            
         ON ACTION CANCEL
            EXIT DIALOG      
         
         END DISPLAY
         
         DISPLAY ARRAY r_detmov TO detalle.*
            ON ACTION ACCEPT
               EXIT DIALOG
            
            ON ACTION CANCEL
               EXIT DIALOG
         
         END DISPLAY
         
         DISPLAY ARRAY arr_precios TO precios.*
           ON ACTION CLOSE
              EXIT DIALOG
         
         END DISPLAY

      END DIALOG   

   CLOSE WINDOW saldo_id_derechohabiente   

END FUNCTION
FUNCTION despliega_domicilio(p_id_derechohabiente, p_id_domicilio)
#función para desplegar los domicilios del derechohabiente

   DEFINE p_id_derechohabiente DECIMAL(9,0)
   DEFINE p_id_domicilio       SMALLINT
   DEFINE domicilio RECORD
             calle              CHAR(60),
             num_ext            CHAR(25),
             num_int            CHAR(25),
             colonia_desc       CHAR(50),
             cp                 CHAR(5),
             delegacion_desc    CHAR(50),
             ciudad_desc        CHAR(50),
             estado_desc        CHAR(50)
          END RECORD

   SELECT dom.calle,
          TRIM(dom.num_exterior),
          TRIM(dom.num_interior),
          colonia.colonia_desc,
          dom.cp,
          munic.municipio_desc,
          ciudad.ciudad_desc,
          estado.entidad_desc_larga
     INTO domicilio.calle,
          domicilio.num_ext,
          domicilio.num_int,
          domicilio.colonia_desc,
          domicilio.cp,
          domicilio.delegacion_desc,
          domicilio.ciudad_desc,
          domicilio.estado_desc
     FROM afi_domicilio dom
    INNER JOIN cat_colonia colonia ON colonia.colonia = dom.colonia
    INNER JOIN cat_cp codigo ON codigo.cp = dom.cp
    INNER JOIN cat_municipio munic ON munic.municipio = codigo.municipio
    INNER JOIN cat_ciudad ciudad ON ciudad.ciudad = codigo.ciudad
    INNER JOIN cat_entidad_federativa estado ON estado.entidad_federativa = codigo.entidad_federativa
    WHERE dom.id_derechohabiente = p_id_derechohabiente
      AND dom.id_domicilio = p_id_domicilio

   DISPLAY domicilio.calle,
           domicilio.num_ext,
           domicilio.num_int,
           domicilio.colonia_desc,
           domicilio.cp,
           domicilio.delegacion_desc,
           domicilio.ciudad_desc,
           domicilio.estado_desc
        TO
           calle,
           num_ext,
           num_int,
           colonia_desc,
           codpos,
           delegacion_desc,
           ciudad_desc,
           estado_desc

END FUNCTION
FUNCTION fn_inicializa()
   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_movimientos_saldo
      DROP TABLE tmp_arbol_saldo
   WHENEVER ERROR STOP

   CREATE TEMP TABLE tmp_movimientos_saldo
    (id_derechohabiente DECIMAL(9,0),
     subcuenta          SMALLINT,
     fondo_inversion    SMALLINT,
     f_liquida          DATE, 
     tipo_movimiento    INTEGER,
     monto_pesos        DECIMAL(16,6),
     monto_acciones     DECIMAL(16,6),
     f_valor            DATE,
     folio_liquida      DECIMAL(10,0),
     origen             CHAR(20))

   CREATE TEMP TABLE tmp_arbol_saldo
    (subcuenta_desc     CHAR(70),
     siefore            SMALLINT,
     monto_pesos        DECIMAL(22,2),
     monto_acciones     DECIMAL(22,2),
     subcuenta          SMALLINT,
     movimiento         SMALLINT,
     padre_id           CHAR(40),
     id                 CHAR(40),
     nivel              SMALLINT)

END FUNCTION
FUNCTION despliega_detalle_mov(p_subcuenta,p_fondo, p_movimiento)
DEFINE i            INTEGER,
       p_subcuenta  SMALLINT,
       p_fondo      SMALLINT,
       p_movimiento SMALLINT,
       v_query      STRING

    LET v_query = " SELECT t.f_liquida, ",
                         " t.tipo_movimiento, ",
                         " c.movimiento_desc, ",
                         " t.fondo_inversion, ",
                         " t.monto_pesos, ",
                         " t.monto_acciones, ",
                         " t.f_valor, ",
                         " t.folio_liquida, ",
                         " t.origen ",
                    " FROM tmp_movimientos_saldo t, ",
                         " cat_movimiento c ",
                   " WHERE t.subcuenta       = ", p_subcuenta,
                     " AND t.fondo_inversion = ", p_fondo
                     
   IF p_movimiento > 0 THEN
      LET v_query = v_query , " AND t.tipo_movimiento = ",p_movimiento 
       
   END IF

   LET v_query = v_query , " AND c.movimiento      = t.tipo_movimiento ",
                         " ORDER BY f_liquida DESC "
 
    CALL r_detmov.clear()

    PREPARE prp_mov FROM v_query
    DECLARE cur_mov CURSOR FOR prp_mov

    LET i = 1

    FOREACH cur_mov INTO r_detmov[i].*
        LET i = i + 1
    END FOREACH
    CLOSE cur_mov

    IF i = 1 THEN
        RETURN 0
    ELSE
        RETURN 1
    END IF
END FUNCTION

FUNCTION genera_reporte_consulta_unificador(v_arr_datos_unificador, p_nombre_completo)
--Define las variables para la generación del reporte
DEFINE 
      v_ind_rep1        SMALLINT, --índice para el reporte
      v_ind_rep2        SMALLINT, --índice para el reporte
      v_indx            SMALLINT, --índice para consulta
      v_manejador_rpt   om.SaxDocumentHandler,
      v_QryTxt          STRING, --Complemento de una consulta
      v_cond_reporte    SMALLINT, --Indicador para manejar la impresión por grupos del reporte
      v_nom_reporte     VARCHAR(80), -- nombre del reporte
      p_nombre_completo CHAR(60),
      v_s_reporte       STRING
      

DEFINE  v_arr_datos_unificador RECORD
	        v_folio_proceso            DECIMAL(9,0),
	        v_folio_liquidacion        DECIMAL(9,0),
	        v_nss_unificador           CHAR(11),
	        v_nombre_unificador        CHAR(100),
	        v_paterno_unificador       CHAR(100),
	        v_materno_unificador       CHAR(100),
	        v_rfc                      CHAR(13),
	        v_curp                     CHAR(18),
	        v_nombre_imss              CHAR(100),
	        v_sexo                     CHAR(10),
	        v_tipo_registro            CHAR(30),
	        v_status_convoca           CHAR(30),
	        v_fecha_notificacion_op22  DATE,
	        v_afore_receptora          CHAR(13),
	        v_afore_origen             CHAR(13),
	        v_fecha_unificacion        DATE,
            v_tipo_movimiento          CHAR(35), 
            v_fecha_notificacion_op21  DATE
     END RECORD

 display "Al entrar en  funcion de  reporte:",v_arr_datos_unificador.*
      LET v_indx = TRUE  --Se asume que no hay error
      LET v_ind_rep1 = 1
      LET v_ind_rep2 = 1
      CALL fgl_report_selectPreview(0)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() 
      START REPORT rp_consulta_unificador TO XML HANDLER v_manejador_rpt
         
       --  FOR  v_ind_rep2 = 1 TO v_datos_unificador_unificado.getLength()
            DISPLAY "Registro --------------- ", v_ind_rep2
            DISPLAY "\n ",v_arr_datos_unificador.v_folio_proceso
            DISPLAY "\n ",v_arr_datos_unificador.v_nss_unificador
            OUTPUT TO REPORT rp_consulta_unificador(v_arr_datos_unificador.*,"safreviv", p_nombre_completo )
       --  END FOR
        
      FINISH REPORT rp_consulta_unificador
      --FREE curr_datos_unificador_unificado

      RETURN v_ind_rep2
END FUNCTION 
REPORT rp_consulta_unificador(v_arr_datos_unificador,p_usuario_cod, p_nombre_completo)
DEFINE  v_arr_datos_unificador RECORD             
	        v_folio_proceso            DECIMAL(9,0),
	        v_folio_liquidacion        DECIMAL(9,0),
	        v_nss_unificador           CHAR(11),    
	        v_nombre_unificador        CHAR(100),   
	        v_paterno_unificador       CHAR(100),   
	        v_materno_unificador       CHAR(100),   
	        v_rfc                      CHAR(13),    
	        v_curp                     CHAR(18),    
	        v_nombre_imss              CHAR(100),   
	        v_sexo                     CHAR(10),    
	        v_tipo_registro            CHAR(30),    
	        v_status_convoca           CHAR(30),    
	        v_fecha_notificacion_op22  DATE,        
	        v_afore_receptora          CHAR(13),    
	        v_afore_origen             CHAR(13),    
	        v_fecha_unificacion        DATE,     
            v_tipo_movimiento          CHAR(35),    
            v_fecha_notificacion_op21  DATE         
     END RECORD                                   

      
   DEFINE 
         --Define variables del encabezado
         r_fecha_reporte    DATE,
         p_usuario_cod      CHAR(15), -- clave del usuario firmado
         p_nombre_completo  CHAR(60) 
   FORMAT 
      FIRST PAGE HEADER

            DISPLAY "\n ",v_arr_datos_unificador.v_folio_proceso
            DISPLAY "\n ",v_arr_datos_unificador.v_nss_unificador
            
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
         PRINTX v_arr_datos_unificador.v_folio_proceso          
         PRINTX v_arr_datos_unificador.v_folio_liquidacion      
         PRINTX v_arr_datos_unificador.v_nss_unificador 
         PRINTX p_nombre_completo              
         PRINTX v_arr_datos_unificador.v_rfc                    
         PRINTX v_arr_datos_unificador.v_curp                   
         PRINTX v_arr_datos_unificador.v_nombre_imss            
         PRINTX v_arr_datos_unificador.v_sexo                   
         PRINTX v_arr_datos_unificador.v_tipo_registro          
         PRINTX v_arr_datos_unificador.v_status_convoca         
         PRINTX v_arr_datos_unificador.v_fecha_notificacion_op22  USING "dd-mm-yyyy"
         PRINTX v_arr_datos_unificador.v_afore_receptora        
         PRINTX v_arr_datos_unificador.v_afore_origen           
         PRINTX v_arr_datos_unificador.v_fecha_unificacion        USING "dd-mm-yyyy"
         PRINTX v_arr_datos_unificador.v_tipo_movimiento        
         PRINTX v_arr_datos_unificador.v_fecha_notificacion_op21  USING "dd-mm-yyyy"
END REPORT 
###############################################################################
#Reporte Unificado IMSSS                                                       #
###############################################################################
FUNCTION genera_reporte_consulta_unificado(v_arr_datos_unificado, p_nombre_completo)
   --Define las variables para la generación del reporte
DEFINE 
      v_ind_rep1        SMALLINT, --índice para el reporte
     v_ind_rep2         SMALLINT, --índice para el reporte
      v_indx            SMALLINT, --índice para consulta
      v_manejador_rpt   om.SaxDocumentHandler,
      v_QryTxt          STRING, --Complemento de una consulta
      v_cond_reporte    SMALLINT, --Indicador para manejar la impresión por grupos del reporte
      p_nombre_completo CHAR(60)
     -- v_nom_reporte      VARCHAR(80), -- nombre del reporte
      --v_s_reporte        STRING
      

DEFINE  v_arr_datos_unificado        RECORD                               
              v_folio_proceso            DECIMAL(9,0),     
              v_folio_liquidacion        DECIMAL(9,0),     
              v_nss_unificado            char(11),       
              v_nombre_unificado         CHAR(100),
              v_paterno_unificado        CHAR(100),
              v_materno_unificado        CHAR(100),
              v_rfc                      CHAR(13), 
              v_curp                     CHAR(18), 
              v_nombre_imss              CHAR(100),
              v_sexo                     CHAR(10), 
              v_tipo_movimiento          CHAR(35),
              v_tipo_registro            CHAR(30),
              v_status_convoca           CHAR(30),
              v_fecha_notificacion_op21  DATE ,   
              v_fecha_unificacion        DATE ,  
              v_fecha_notificacion_op22  DATE,    
              v_clave_afore_receptora    CHAR(13),           
              v_afore_origen             CHAR(13)
        END RECORD                      
      

     display "Al entrar en  funcion de  reporte:",v_arr_datos_unificado.*
      LET v_indx = TRUE  --Se asume que no hay error
      LET v_ind_rep1 = 1
      LET v_ind_rep2 = 1
      CALL fgl_report_selectPreview(0)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() 
      START REPORT rp_consulta_unificado TO XML HANDLER v_manejador_rpt
         
       --  FOR  v_ind_rep2 = 1 TO v_datos_unificador_unificado.getLength()
            DISPLAY "Registro --------------- ", v_ind_rep2
            DISPLAY "\n ",v_arr_datos_unificado.v_folio_proceso
            DISPLAY "\n ",v_arr_datos_unificado.v_nss_unificado
            OUTPUT TO REPORT rp_consulta_unificado(v_arr_datos_unificado.*,"safreviv", p_nombre_completo)
       --  END FOR
        
      FINISH REPORT rp_consulta_unificado
      --FREE curr_datos_unificador_unificado

     
      RETURN v_ind_rep2
END FUNCTION 
REPORT rp_consulta_unificado(v_arr_datos_unificado,p_usuario_cod, p_nombre_completo)
   DEFINE  v_arr_datos_unificado RECORD                               
	           v_folio_proceso            DECIMAL(9,0),     
              v_folio_liquidacion        DECIMAL(9,0),     
              v_nss_unificado            char(11),       
              v_nombre_unificado         CHAR(100),
              v_paterno_unificado        CHAR(100),
              v_materno_unificado        CHAR(100),
              v_rfc                      CHAR(13), 
              v_curp                     CHAR(18), 
              v_nombre_imss              CHAR(100),
              v_sexo                     CHAR(10), 
              v_tipo_movimiento          CHAR(35),
              v_tipo_registro            CHAR(30),
              v_status_convoca           CHAR(30),
              v_fecha_notificacion_op21  DATE ,   
              v_fecha_unificacion        DATE ,  
              v_fecha_notificacion_op22  DATE,    
              v_clave_afore_receptora    CHAR(13),           
              v_afore_origen             CHAR(13)
        END RECORD                      
      
   DEFINE 
         --Define variables del encabezado
         r_fecha_reporte      DATE,
         p_usuario_cod        CHAR(15), -- clave del usuario firmado
         p_nombre_completo    CHAR(60)
         
   FORMAT 
      FIRST PAGE HEADER
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
       --  PRINTX   v_arr_datos_unificado.*   
         PRINTX   v_arr_datos_unificado.v_folio_proceso           
         PRINTX   v_arr_datos_unificado.v_folio_liquidacion       
         PRINTX   v_arr_datos_unificado.v_nss_unificado     
         PRINTX   p_nombre_completo      
         PRINTX   v_arr_datos_unificado.v_nombre_unificado        
         PRINTX   v_arr_datos_unificado.v_paterno_unificado       
         PRINTX   v_arr_datos_unificado.v_materno_unificado       
         PRINTX   v_arr_datos_unificado.v_rfc                     
         PRINTX   v_arr_datos_unificado.v_curp                    
         PRINTX   v_arr_datos_unificado.v_nombre_imss             
         PRINTX   v_arr_datos_unificado.v_sexo                    
         PRINTX   v_arr_datos_unificado.v_tipo_movimiento         
         PRINTX   v_arr_datos_unificado.v_tipo_registro           
         PRINTX   v_arr_datos_unificado.v_status_convoca          
         PRINTX   v_arr_datos_unificado.v_fecha_notificacion_op21  USING "dd-mm-yyyy"
         PRINTX   v_arr_datos_unificado.v_fecha_unificacion        USING "dd-mm-yyyy"
         PRINTX   v_arr_datos_unificado.v_fecha_notificacion_op22  USING "dd-mm-yyyy"
         PRINTX   v_arr_datos_unificado.v_clave_afore_receptora   
         PRINTX   v_arr_datos_unificado.v_afore_origen            

         
         

   
END REPORT 

##################################################################
# Reportes INFONAVIT                                             #
##################################################################

FUNCTION genera_reporte_consulta_unificador_INFONAVIT(v_arr_datos_unificador_INFONAVIT, p_nombre_completo)
   --Define las variables para la generación del reporte
DEFINE 
      v_ind_rep1        SMALLINT, --índice para el reporte
      v_ind_rep2        SMALLINT, --índice para el reporte
      v_indx            SMALLINT, --índice para consulta
      v_manejador_rpt   om.SaxDocumentHandler,
      v_QryTxt          STRING, --Complemento de una consulta
      v_cond_reporte    SMALLINT, --Indicador para manejar la impresión por grupos del reporte
      v_nom_reporte     VARCHAR(80), -- nombre del reporte
      p_nombre_completo CHAR(60),
      v_s_reporte       STRING
      

DEFINE  v_arr_datos_unificador_INFONAVIT RECORD                               
	        v_folio_proceso            DECIMAL(9,0),      
	        v_folio_liquidacion        DECIMAL(9,0),                                           
	        v_nss_unificador           char(11),          
	        v_nombre_unificador        CHAR(100),                      
	        v_paterno_unificador       CHAR(100),         
	        v_materno_unificador       CHAR(100),         
	        v_rfc                      CHAR(13),          
	        v_curp                     CHAR(18),          
	        v_nombre_imss              CHAR(100),         
	        v_sexo                     CHAR(10),          
	        v_tipo_movimiento          CHAR(35),                    
	        v_tipo_registro            CHAR(30),          
	        v_status_convoca           CHAR(30),                  
	        v_fecha_notificacion_op21  DATE ,                                               
	        v_fecha_unificacion        DATE ,             
	        v_fecha_notificacion_op22  DATE,              
	        v_afore_receptora    CHAR(13),           
	        v_afore_origen     CHAR(13)            
     END RECORD

 
      LET v_indx = TRUE  --Se asume que no hay error
      LET v_ind_rep1 = 1
      LET v_ind_rep2 = 1
      CALL fgl_report_selectPreview(0)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() 
      START REPORT rp_consulta_unificador_INFONAVIT TO XML HANDLER v_manejador_rpt
         
       --  FOR  v_ind_rep2 = 1 TO v_datos_unificador_unificado.getLength()
            DISPLAY "Registro --------------- ", v_ind_rep2
            DISPLAY "\n ",v_arr_datos_unificador_INFONAVIT.v_folio_proceso
            DISPLAY "\n ",v_arr_datos_unificador_INFONAVIT.v_nss_unificador
                                    
            OUTPUT TO REPORT rp_consulta_unificador_INFONAVIT(v_arr_datos_unificador_INFONAVIT.*,"safreviv", p_nombre_completo)
       --  END FOR
        
      FINISH REPORT rp_consulta_unificador_INFONAVIT
      --FREE curr_datos_unificador_unificado

      
      RETURN v_ind_rep2
END FUNCTION 
REPORT rp_consulta_unificador_INFONAVIT(v_arr_datos_unificador_INFONAVIT,p_usuario_cod, p_nombre_completo)
   DEFINE  v_arr_datos_unificador_INFONAVIT RECORD                               
	         v_folio_proceso            DECIMAL(9,0),      
	         v_folio_liquidacion        DECIMAL(9,0),                                           
	         v_nss_unificador           char(11),          
	         v_nombre_unificador        CHAR(100),                      
	         v_paterno_unificador       CHAR(100),         
	         v_materno_unificador       CHAR(100),         
	         v_rfc                      CHAR(13),          
	         v_curp                     CHAR(18),          
	         v_nombre_imss              CHAR(100),         
	         v_sexo                     CHAR(10),          
	         v_tipo_movimiento          CHAR(35),                    
	         v_tipo_registro            CHAR(30),          
	         v_status_convoca           CHAR(30),                  
	         v_fecha_notificacion_op21  DATE ,                                               
	         v_fecha_unificacion        DATE ,             
	         v_fecha_notificacion_op22  DATE,              
	         v_afore_receptora    CHAR(13),           
	         v_afore_origen     CHAR(13)            
     END RECORD
      
   DEFINE 
         --Define variables del encabezado
         r_fecha_reporte      DATE,
         p_usuario_cod        CHAR(15), -- clave del usuario firmado
         p_nombre_completo    CHAR(60)
         
   FORMAT 
      FIRST PAGE HEADER
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
      --   PRINTX   v_arr_datos_unificador_INFONAVIT.* 
        -- PRINTX   v_arr_datos_unificador_INFONAVIT.v_arr_datos_unificador_INF
         
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_folio_proceso           
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_folio_liquidacion       
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_nss_unificador     
         PRINTX   p_nombre_completo     
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_nombre_unificador       
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_paterno_unificador      
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_materno_unificador      
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_rfc                     
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_curp                    
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_nombre_imss             
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_sexo                    
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_tipo_movimiento         
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_tipo_registro           
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_status_convoca          
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_fecha_notificacion_op21 USING "dd-mm-yyyy"
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_fecha_unificacion       USING "dd-mm-yyyy"    
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_fecha_notificacion_op22 USING "dd-mm-yyyy"
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_afore_receptora    
         PRINTX   v_arr_datos_unificador_INFONAVIT.v_afore_origen     
         

   
END REPORT 
###############################################################################
#Reporte Unificado INFONAVIT                                                       #
###############################################################################
FUNCTION genera_reporte_consulta_unificado_INFONAVIT(v_arr_datos_unificado_INFONAVIT)
   --Define las variables para la generación del reporte
DEFINE 
      v_ind_rep1         SMALLINT, --índice para el reporte
     v_ind_rep2        SMALLINT, --índice para el reporte
      v_indx            SMALLINT, --índice para consulta
      v_manejador_rpt   om.SaxDocumentHandler,
      v_QryTxt            STRING, --Complemento de una consulta
      v_cond_reporte    SMALLINT --Indicador para manejar la impresión por grupos del reporte
     -- v_nom_reporte      VARCHAR(80), -- nombre del reporte
      --v_s_reporte        STRING
      

DEFINE  v_arr_datos_unificado_INFONAVIT        RECORD                               
              v_id_inf_unificado         DECIMAL(9,0),  
              v_folio_proceso            DECIMAL(9,0),     
              v_folio_liquidacion        DECIMAL(9,0),     
              v_nss_unificado            char(11),       
              v_nombre_unificado         CHAR(100),
              v_paterno_unificado        CHAR(100),
              v_materno_unificado        CHAR(100),
              v_rfc                      CHAR(13), 
              v_curp                     CHAR(18), 
              v_nombre_imss              CHAR(100),
              v_sexo                     CHAR(10), 
              v_tipo_movimiento          CHAR(35),
              v_tipo_registro            CHAR(30),
              v_status_convoca           CHAR(30),
              v_fecha_notificacion_op21  DATE ,   
              v_fecha_unificacion        DATE ,  
              v_fecha_notificacion_op22  DATE,    
              v_clave_afore_receptora    CHAR(13),           
              v_afore_origen             CHAR(13)
        END RECORD                      
      

     display "Al entrar en  funcion de  reporte:",v_arr_datos_unificado_INFONAVIT.*
      LET v_indx = TRUE  --Se asume que no hay error
      LET v_ind_rep1 = 1
      LET v_ind_rep2 = 1
      CALL fgl_report_selectPreview(0)
      LET v_manejador_rpt = fgl_report_commitCurrentSettings() 
      START REPORT rp_consulta_unificado_INFONAVIT TO XML HANDLER v_manejador_rpt
         
       --  FOR  v_ind_rep2 = 1 TO v_datos_unificador_unificado.getLength()
            DISPLAY "Registro --------------- ", v_ind_rep2
            DISPLAY "\n ",v_arr_datos_unificado_INFONAVIT.v_folio_proceso
            DISPLAY "\n ",v_arr_datos_unificado_INFONAVIT.v_nss_unificado
            OUTPUT TO REPORT rp_consulta_unificado_INFONAVIT(v_arr_datos_unificado_INFONAVIT.*,"safreviv")
       --  END FOR
        
      FINISH REPORT rp_consulta_unificado_INFONAVIT
      --FREE curr_datos_unificador_unificado

     
      RETURN v_ind_rep2
END FUNCTION 
REPORT rp_consulta_unificado_INFONAVIT(v_arr_datos_unificado_INFONAVIT,p_usuario_cod)
   DEFINE  v_arr_datos_unificado_INFONAVIT RECORD                               
              v_id_inf_unificado         DECIMAL(9,0),  
              v_folio_proceso            DECIMAL(9,0),     
              v_folio_liquidacion        DECIMAL(9,0),     
              v_nss_unificado            char(11),       
              v_nombre_unificado         CHAR(100),
              v_paterno_unificado        CHAR(100),
              v_materno_unificado        CHAR(100),
              v_rfc                      CHAR(13), 
              v_curp                     CHAR(18), 
              v_nombre_imss              CHAR(100),
              v_sexo                     CHAR(10), 
              v_tipo_movimiento          CHAR(35),
              v_tipo_registro            CHAR(30),
              v_status_convoca           CHAR(30),
              v_fecha_notificacion_op21  DATE ,   
              v_fecha_unificacion        DATE ,  
              v_fecha_notificacion_op22  DATE,    
              v_clave_afore_receptora    CHAR(13),           
              v_afore_origen             CHAR(13)
        END RECORD                      
      
   DEFINE 
         --Define variables del encabezado
         r_fecha_reporte      DATE,
         p_usuario_cod        CHAR(15) -- clave del usuario firmado

   FORMAT 
      FIRST PAGE HEADER
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
         --PRINTX   v_arr_datos_unificado_INFONAVIT.*  
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_folio_proceso           
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_folio_liquidacion       
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_nss_unificado           
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_nombre_unificado        
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_paterno_unificado       
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_materno_unificado       
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_rfc                     
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_curp                    
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_nombre_imss             
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_sexo                    
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_tipo_movimiento         
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_tipo_registro           
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_status_convoca          
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_fecha_notificacion_op21 USING "dd-mm-yyyy"
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_fecha_unificacion       USING "dd-mm-yyyy"
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_fecha_notificacion_op22 USING "dd-mm-yyyy"
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_clave_afore_receptora   
         PRINTX    v_arr_datos_unificado_INFONAVIT.v_afore_origen            
  

   
END REPORT

FUNCTION fn_eje_consulta(p_pgm,p_usuario_cod,p_id_derechohabiente, p_tipo_ejecucion, p_s_titulo)
#función para desplegar pantalla de saldos del asignado

    DEFINE p_pgm                SMALLINT,
           p_usuario_cod        LIKE seg_usuario.usuario_cod,
           p_id_derechohabiente DECIMAL(9,0),
           p_tipo_ejecucion SMALLINT,
           p_s_titulo CHAR(25),
           comma STRING

    DEFINE v_pgm           CHAR(6)
    DEFINE l_ruta_bin      CHAR(40)

    INITIALIZE comma TO NULL

    SELECT ct.ruta_bin
    INTO   l_ruta_bin
    FROM   seg_modulo ct
    WHERE  modulo_cod = 'cta'

    IF p_pgm = 1 THEN
        LET v_pgm = 'CTAC01'
    END IF

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," '",p_usuario_cod,
                "' '",p_tipo_ejecucion, "' '",p_s_titulo, "' '",p_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION