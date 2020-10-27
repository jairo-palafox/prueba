--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            => RET                                                                                             
#Programa         => RETS11                                                                                       
#Objetivo          => Genera el archivo de respuesta a PROCESAR de Retiros de Fondo de Ahorro Contingente
#                          que incluye registros aceptados y rechazados                         
#Modificado                                                                                                             
# Esteban Sánchez 12/sep/2013  - Se retoma el código para modificar el layout de salida      
#                                 en consideración del nuevo layout de FA
#Ivan Vega        06mar2014    - Se agrega el NSS y RFC a la tabla historica para poder reconstruir el archivo
#                                que se recibio y enviarlo con su respectiva respuesta
##############################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl" --Archivo que almacena las variables globales del modulo
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod      LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
END GLOBALS

MAIN
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- nombre del usuario        
       p_folio             LIKE glo_folio.folio, -- numero de folio
       p_es_previo         SMALLINT -- booleana. 1-es archivo previo. 0 es final

   -- se reciben los parametros del programa
   LET p_folio            = ARG_VAL(1)
   LET p_usuario_cod      = ARG_VAL(2)
   LET p_es_previo        = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS110.log")
   
   -- Llamado a función que genera el archivo de salida
   CALL fn_archivo_salida( p_folio, p_usuario_cod, p_es_previo)
END MAIN

-- Funcion que se encarga de crear el archivo de salida
-- recibe como parametro la fecha inicial y final para tomar el rango de salida
FUNCTION fn_archivo_salida( p_folio, p_usuario_cod, p_es_previo)
DEFINE p_folio                      LIKE glo_folio.folio,
       p_usuario_cod                LIKE seg_usuario.usuario_cod,      
       v_v_nom_archivo              STRING, -- nombre del archivo de salida
       v_v_nom_archivo_rechazo      STRING, --nombre del archivo de rechazados
       v_c_ruta_env_acr             LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
       p_es_previo                  SMALLINT
       
DEFINE  v_i_contador_registros   INTEGER,
        v_archivo_original       LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo original
        v_fecha_formateada       CHAR(6) -- fecha en formato AAMMDD 


   LET v_i_contador_registros = 0 

   --DISPLAY "folio =  ",p_folio
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'ret'

   -- se obtiene el nombre del archivo original
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod_ret_fondo_ahorro_arch
   AND    folio = p_folio
  
   LET v_fecha_formateada = TODAY USING "yymmdd"
   
   -- si es previo
   IF ( p_es_previo ) THEN
      LET v_v_nom_archivo = "/PREVIO_"
      LET v_v_nom_archivo_rechazo="PREVIO_"
   ELSE
      LET v_v_nom_archivo = "/"
      LET v_v_nom_archivo_rechazo="/"
   END IF
   
      --Se crean los nombres de los archivos
      LET v_v_nom_archivo         = v_v_nom_archivo, v_archivo_original[1,8], "liq.retfa"
      LET v_v_nom_archivo_rechazo = v_v_nom_archivo_rechazo,v_archivo_original[1,8], "rech.retfa"

      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_fondo_ahorro
      WHERE  cod_rechazo = 0
      AND    folio = p_folio
      
      -- si no hay aceptados, no se genera el archivo y se finaliza la ejecucion
      IF ( v_i_contador_registros IS NULL OR v_i_contador_registros < 1 ) THEN
         DISPLAY "____________________________________________________________"
         DISPLAY "   GENERACION DE ARCHIVO DE SALIDA DE REGISTROS ACEPTADOS"
         DISPLAY ""
         DISPLAY "   A T E N C I O N: No se tienen registros aceptados para el"
         DISPLAY "                    folio ", p_folio
         DISPLAY "                    No se generó archivo de salida."
         DISPLAY "\n____________________________________________________________"
         --EXIT PROGRAM
     
      ELSE
      	-- Se invoca la función de generacion de archivos de salida (1) aceptados
      	CALL fn_genera_archivo_salida(p_folio,v_v_nom_archivo,v_c_ruta_env_acr, 1)
   	
      END IF
      
      --Reinicia variable de conteo     
      LET v_i_contador_registros = 0
      --Verifica que existan registros de rechazo
      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_fondo_ahorro
      WHERE  cod_rechazo <> 0
      AND    folio = p_folio
      
      --Valida si se obtuvieron registros de rechazo
      IF ( v_i_contador_registros IS NULL OR v_i_contador_registros < 1 ) THEN
      	
      	DISPLAY "____________________________________________________________"
         DISPLAY "   GENERACION DE ARCHIVO DE SALIDA DE REGISTROS RECHAZADOS"
         DISPLAY ""
         DISPLAY "   A T E N C I O N: No se tienen registros rechazados para el"
         DISPLAY "                    folio ", p_folio
         DISPLAY "                    No se generó archivo de salida."
         DISPLAY "\n____________________________________________________________"
       
      ELSE
      	--Se invoca la generacion de archivos de rechazo (0) rechazados
      	CALL fn_genera_archivo_salida(p_folio,v_v_nom_archivo_rechazo,v_c_ruta_env_acr, 0)
      	
      END IF 	
      
      
END FUNCTION --fn_archivo_salida

#Objetivo. Generar el archivo de aceptados para los retiros del fondo de ahorro
FUNCTION fn_genera_archivo_salida(p_folio, v_v_nom_arc_aceptados,v_c_ruta_env_arch, p_registros)
DEFINE   p_folio                  LIKE glo_folio.folio,
         v_v_nom_arc_aceptados    STRING,   --Nombre del archivo
         v_c_ruta_env_arch        LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
         p_registros              SMALLINT, -- indica si se requieren aceptados o rechazados
         v_s_encabezado_acp       STRING,
         v_s_detalle_acp          STRING,
         v_s_sumario_acp          STRING,
         v_v_ruta_arc_aceptados   STRING, --Ruta del archivo de aceptados
         v_ch_arc_aceptados       base.channel, --Canal para escribir el archivo de salida
         ls_sql                   STRING, --Variable usada para generar las instrucciones sql
         v_id_derechohabiente     LIKE ret_fondo_ahorro.id_derechohabiente,
         v_sum_total_pesos        DECIMAL(12,2),
         v_c_suma_total           CHAR(14),
         v_c_fec_solicitud        CHAR(8),
         v_c_imp_viv72            CHAR(14),
         v_c_tanto_imp_viv72      CHAR(8),
         v_c_entidad              CHAR(6),
         v_c_causal_retiro        CHAR(6),
         v_c_caso_adai            CHAR(10),
         v_cod_rechazo            LIKE ret_fondo_ahorro.cod_rechazo
DEFINE  r_tmp_ret_encabezado  RECORD
 	        v_tpo_registro     CHAR(2),
 	        v_id_servicio      CHAR(2),
 	        v_id_operacion     CHAR(2),
 	        f_operacion        CHAR(8),
 	        v_res_operacion    CHAR(2),
 	        v_mot_rechazo1     CHAR(3),
 	        v_mot_rechazo2     CHAR(3)	
         END RECORD
         
DEFINE  r_tmp_ret_detalle RECORD
 	       v_tpo_registro         CHAR(2),      
          v_id_servicio          CHAR(2),      
          v_id_operacion         CHAR(2),      
          v_nss                  CHAR(11),     
          v_f_solicitud          DATE,         
          v_f_liquidacion	     DATE,         
          v_cve_refer            CHAR(13),     
          v_imp_viv72            DECIMAL(12,2),
          v_tanto_imp_viv72      DECIMAL(12,2), ---
          v_entidad              DECIMAL(6,0), 
          v_causal_retiro        DECIMAL(6,0), 
          v_caso_adai            DECIMAL(10,0), 
          v_RFC_trabajador       CHAR(13),     
          v_id_traspaso         CHAR(1),      
          v_resultado_operacion  CHAR(2),      
          v_motivo_rech_1        CHAR(3),      
          v_motivo_rech_2        CHAR(3)       
         END RECORD
         
 	--se inicializan las variables 
   LET v_s_encabezado_acp = NULL
   LET v_s_detalle_acp    = NULL
   LET v_s_sumario_acp    = NULL
   LET v_sum_total_pesos  = 0.00
   LET v_v_ruta_arc_aceptados = v_c_ruta_env_arch CLIPPED || v_v_nom_arc_aceptados
  
   --Muestra la ruta donde quedara el archivo
   IF ( p_registros = 1 ) THEN
      DISPLAY "\n   Archivo [ACEPTADOS] : ", v_v_ruta_arc_aceptados
   ELSE
      DISPLAY "\n   Archivo [RECHAZADOS] : ", v_v_ruta_arc_aceptados
   END IF
   
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_aceptados = base.Channel.create()     
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_aceptados.openFile(v_v_ruta_arc_aceptados, "w" )
   
   --Asignación de las variables del encabezado
   LET r_tmp_ret_encabezado.v_tpo_registro  = "01"
   LET r_tmp_ret_encabezado.v_id_servicio   = "04"
   LET r_tmp_ret_encabezado.v_id_operacion  = "19"
   LET r_tmp_ret_encabezado.f_operacion     = TODAY USING "yyyymmdd"
   LET r_tmp_ret_encabezado.v_res_operacion = "01"
   LET r_tmp_ret_encabezado.v_mot_rechazo1  = "000"
   LET r_tmp_ret_encabezado.v_mot_rechazo2  = "000"
   
   --Asignación del encabezado
   LET v_s_encabezado_acp = r_tmp_ret_encabezado.v_tpo_registro  ,
                            r_tmp_ret_encabezado.v_id_servicio   ,
                            r_tmp_ret_encabezado.v_id_operacion  ,
                            r_tmp_ret_encabezado.f_operacion     ,
                            r_tmp_ret_encabezado.v_res_operacion ,
                            r_tmp_ret_encabezado.v_mot_rechazo1  ,
                            r_tmp_ret_encabezado.v_mot_rechazo2  
   
    --Se escribe el encabezado en el archivo            
    CALL v_ch_arc_aceptados.writeLine(v_s_encabezado_acp)
    
    DISPLAY "Obteniendo solicitudes aceptadas"
    
    --Query para la selección de los campos   
    LET ls_sql = "\n SELECT id_derechohabiente,",
                 "\n        nss               ,",
                 "\n        rfc               ,",
                 "\n        f_solicitud       ,",
	 			 "\n        f_liquidacion     ,",
				 "\n        id_traspaso       ,",
                 "\n        cve_referencia    ,",
                 "\n        saldo_viv72       ,",
                 "\n        tanto_adicional   ,",
                 "\n        entidad_federativa,",
                 "\n        causal_retiro     ,",
                 "\n        caso_adai         ,",
                 "\n        cod_rechazo        ",
                 "\n FROM   ret_fondo_ahorro   ",
                 "\n WHERE  folio       = ", p_folio
    -- si se pidieron aceptados
    IF ( p_registros = 1 ) THEN
       LET ls_sql = ls_sql , "\n AND    cod_rechazo = 0"
    ELSE
       -- rechazados
       LET ls_sql = ls_sql , "\n AND    cod_rechazo <> 0"
    END IF
    
    PREPARE stm_detalle_fondo_ahorro FROM ls_sql        
    DECLARE cur_detalle_fondo_ahorro CURSOR FOR stm_detalle_fondo_ahorro
   
    --Se asignan valores por defecto en el detalle          
    LET r_tmp_ret_detalle.v_tpo_registro        = "03"
    LET r_tmp_ret_detalle.v_id_servicio         = "04"
    LET r_tmp_ret_detalle.v_id_operacion        = "19"
   
    DISPLAY "Foreach de lectura de solicitudes..."
    FOREACH cur_detalle_fondo_ahorro
    INTO v_id_derechohabiente               ,
         r_tmp_ret_detalle.v_nss            ,
         r_tmp_ret_detalle.v_RFC_trabajador ,
         r_tmp_ret_detalle.v_f_solicitud    ,
	 	 r_tmp_ret_detalle.v_f_liquidacion  ,
		 r_tmp_ret_detalle.v_id_traspaso    ,
         r_tmp_ret_detalle.v_cve_refer      ,
         r_tmp_ret_detalle.v_imp_viv72      ,
         r_tmp_ret_detalle.v_tanto_imp_viv72,
         r_tmp_ret_detalle.v_entidad        ,
         r_tmp_ret_detalle.v_causal_retiro  ,
         r_tmp_ret_detalle.v_caso_adai      ,
         v_cod_rechazo
         
	 	DISPLAY "ID_DER: ", v_id_derechohabiente
	 	DISPLAY "NSS: ", r_tmp_ret_detalle.v_nss
	 	DISPLAY "RFC: ", r_tmp_ret_detalle.v_rfc_trabajador
                
          --Suma el total de los pesos (importe + tanto adicional)
          LET v_sum_total_pesos = v_sum_total_pesos + r_tmp_ret_detalle.v_imp_viv72 + r_tmp_ret_detalle.v_tanto_imp_viv72
         
         --Formatea la fecha de solicitud
         LET v_c_fec_solicitud = r_tmp_ret_detalle.v_f_solicitud USING "yyyymmdd"
   
         --Formatea la entidad
         LET v_c_entidad = r_tmp_ret_detalle.v_entidad USING "&&&&&&"
         
         --Formatea los importes
         LET v_c_imp_viv72        = fn_elimina_punto(r_tmp_ret_detalle.v_imp_viv72)
         LET v_c_imp_viv72        = v_c_imp_viv72      USING "&&&&&&&&&&&&&&"        
         LET v_c_tanto_imp_viv72  = fn_elimina_punto(r_tmp_ret_detalle.v_tanto_imp_viv72)
         LET v_c_tanto_imp_viv72  = v_c_tanto_imp_viv72 USING "&&&&&&&&"
         LET v_c_causal_retiro    = r_tmp_ret_detalle.v_causal_retiro   USING "&&&&&&"
         LET v_c_caso_adai        = r_tmp_ret_detalle.v_caso_adai       USING "&&&&&&&&&&" 

         -- se formatea el diagnostico y codigos de rechazo
         IF ( p_registros = 1 ) THEN
           LET r_tmp_ret_detalle.v_resultado_operacion = "01"
           LET r_tmp_ret_detalle.v_motivo_rech_1       = "000"
           LET r_tmp_ret_detalle.v_motivo_rech_2       = "000"
         ELSE
           LET r_tmp_ret_detalle.v_resultado_operacion = "02"
           LET r_tmp_ret_detalle.v_motivo_rech_1       = v_cod_rechazo USING "&&&"
           LET r_tmp_ret_detalle.v_motivo_rech_2       = "000"
         END IF

         
	    DISPLAY "Escribiendo registro de detalle"
        -- Escribe en el archivo
        LET v_s_detalle_acp = r_tmp_ret_detalle.v_tpo_registro       ,      
                              r_tmp_ret_detalle.v_id_servicio        ,      
                              r_tmp_ret_detalle.v_id_operacion       ,      
                              r_tmp_ret_detalle.v_nss                ,     
                              v_c_fec_solicitud                      ,         
                              r_tmp_ret_detalle.v_f_liquidacion USING "yyyymmdd",         
                              r_tmp_ret_detalle.v_cve_refer          ,     
                              v_c_imp_viv72                          ,
                              v_c_tanto_imp_viv72                    , 
                              v_c_entidad                            , 
                              v_c_causal_retiro                      , 
                              v_c_caso_adai                          , 
                              r_tmp_ret_detalle.v_rfc_trabajador     ,     
                              r_tmp_ret_detalle.v_id_traspaso       ,      
                              r_tmp_ret_detalle.v_resultado_operacion,      
                              r_tmp_ret_detalle.v_motivo_rech_1      ,      
                              r_tmp_ret_detalle.v_motivo_rech_2
                            
         --Se escribe el detalle en el archivo            
         CALL v_ch_arc_aceptados.writeLine(v_s_detalle_acp)                    
   
   END FOREACH

   --Elimina el punto decimal
   LET v_c_suma_total = fn_elimina_punto(v_sum_total_pesos)

   --Asigna el formato
   LET v_c_suma_total = v_c_suma_total USING "&&&&&&&&&&&&&&"

   --Se asigna el valor del  tipo de registro para sumario
   LET r_tmp_ret_detalle.v_tpo_registro = "09"
   DISPLAY "Escribiendo sumario"
   --Sumario   
   LET v_s_sumario_acp = r_tmp_ret_detalle.v_tpo_registro,
                         r_tmp_ret_detalle.v_id_servicio ,
                         r_tmp_ret_detalle.v_id_operacion,
                         v_c_suma_total
                        
                        
   --Se escribe el encabezado en el archivo            
   CALL v_ch_arc_aceptados.writeLine(v_s_sumario_acp)
   
   --se cierra el archivo   
   CALL v_ch_arc_aceptados.CLOSE()
                                            
                        
END FUNCTION 

#Objetivo. Generar el archivo de rehazos para los retiros del fondo de ahorro
FUNCTION fn_genera_archivo_rechazos(p_folio, v_v_nom_arc_rechazados,v_c_ruta_env_arch)
DEFINE  p_folio                  LIKE glo_folio.folio,
        v_v_nom_arc_rechazados   STRING,   --Nombre del archivo
        v_c_ruta_env_arch        LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
        v_s_encabezado_rec       STRING,
        v_s_detalle_rec          STRING,
        v_s_sumario_rec          STRING,
        v_v_ruta_arc_rechazados  STRING, --Ruta del archivo de aceptados
        v_ch_arc_rechazados       base.channel, --Canal para escribir el archivo de salida
        ls_sql                   STRING, --Variable usada para generar las instrucciones sql
        v_id_derechohabiente     LIKE ret_fondo_ahorro.id_derechohabiente,
        v_sum_total_pesos        DECIMAL(12,6),
        v_c_fec_solicitud        CHAR(8),
        v_c_suma_total           CHAR(14),
        v_c_imp_viv72            CHAR(14),
        v_c_tanto_imp_viv72      CHAR(8),
        v_c_entidad              CHAR(6),
        v_c_causal_retiro        CHAR(6),
        v_c_caso_adai            CHAR(10)
 DEFINE r_tmp_ret_encabezado  RECORD
 	        v_tpo_registro     CHAR(2),
 	        v_id_servicio      CHAR(2),
 	        v_id_operacion     CHAR(2),
 	        f_operacion        CHAR(8),
 	        v_res_operacion    CHAR(2),
 	        v_mot_rechazo1     CHAR(3),
 	        v_mot_rechazo2     CHAR(3)	
         END RECORD
         
 DEFINE r_tmp_ret_detalle RECORD
 	       v_tpo_registro         CHAR(2),      
          v_id_servicio          CHAR(2),      
          v_id_operacion         CHAR(2),      
          v_nss                  CHAR(11),     
          v_f_solicitud          DATE,         
          v_f_liquidacion	     DATE,         
          v_cve_refer            CHAR(13),     
          v_imp_viv72            DECIMAL(12,2),
          v_tanto_imp_viv72      DECIMAL(12,2), 
          v_entidad              DECIMAL(6,0), 
          v_causal_retiro        DECIMAL(6,0), 
          v_caso_adai            DECIMAL(10,0), 
          v_RFC_trabajador       CHAR(13),     
          v_id_traspaso         CHAR(1),      
          v_resultado_operacion  CHAR(2),      
          v_motivo_rech_1        CHAR(3),      
          v_motivo_rech_2        CHAR(3)       
         END RECORD
         
 	--se inicializan las variables 
   LET v_s_encabezado_rec = NULL
   LET v_s_detalle_rec    = NULL
   LET v_s_sumario_rec    = NULL
   LET v_sum_total_pesos  = 0.00
   LET v_v_ruta_arc_rechazados = v_c_ruta_env_arch CLIPPED || v_v_nom_arc_rechazados
  
   --Muestra la ruta donde quedará el archivo  
   DISPLAY "\n   Archivo[RECHAZADOS]: ", v_v_ruta_arc_rechazados 
   
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_rechazados = base.Channel .create() 
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_rechazados.openFile(v_v_ruta_arc_rechazados, "w" )
   
   --Asignación de las variables del encabezado
   LET r_tmp_ret_encabezado.v_tpo_registro    ="01"
   LET r_tmp_ret_encabezado.v_id_servicio     ="04"
   LET r_tmp_ret_encabezado.v_id_operacion    ="19"
   LET r_tmp_ret_encabezado.f_operacion       = TODAY USING "yyyymmdd"
   LET r_tmp_ret_encabezado.v_res_operacion   ="02"
   LET r_tmp_ret_encabezado.v_mot_rechazo1    ="000"
   LET r_tmp_ret_encabezado.v_mot_rechazo2    ="000"
   
   --Asignación del encabezado
   LET v_s_encabezado_rec = r_tmp_ret_encabezado.v_tpo_registro  ,
                            r_tmp_ret_encabezado.v_id_servicio   ,
                            r_tmp_ret_encabezado.v_id_operacion  ,
                            r_tmp_ret_encabezado.f_operacion     ,
                            r_tmp_ret_encabezado.v_res_operacion ,
                            r_tmp_ret_encabezado.v_mot_rechazo1  ,
                            r_tmp_ret_encabezado.v_mot_rechazo2  
   
    --Se escribe el encabezado en el archivo            
    CALL v_ch_arc_rechazados.writeLine(v_s_encabezado_rec)
    
    --Query para la selección de los campos   
    LET ls_sql = "\n SELECT id_derechohabiente,",
                 "\n        f_solicitud       ,",
                 "\n        f_liquidacion     ,",
				 "\n        id_traspaso       ,",
                 "\n        cve_referencia    ,",
                 "\n        saldo_viv72       ,",
                 "\n        tanto_adicional   ,",
                 "\n        entidad_federativa,",
                 "\n        causal_retiro     ,",
                 "\n        caso_adai         ,",
                 "\n        cod_rechazo       ,",
                 "\n FROM   ret_fondo_ahorro   ",
                 "\n WHERE  folio = ",p_folio,
                 "\n AND    cod_rechazo <> 0 "
    
    PREPARE stm_det_fondo_ahorro_rec FROM ls_sql        
    DECLARE cur_det_fondo_ahorro_rec CURSOR FOR stm_det_fondo_ahorro_rec
    
    --Se asignan valores por defecto en el detalle          
    LET r_tmp_ret_detalle.v_tpo_registro        = "03"
    LET r_tmp_ret_detalle.v_id_servicio         = "04"
    LET r_tmp_ret_detalle.v_id_operacion        = "19"
    LET r_tmp_ret_detalle.v_resultado_operacion = "02"
    LET r_tmp_ret_detalle.v_motivo_rech_2       = "000"
    	
    FOREACH cur_det_fondo_ahorro_rec
    INTO v_id_derechohabiente               ,
         r_tmp_ret_detalle.v_f_solicitud    ,
	 	 r_tmp_ret_detalle.v_f_liquidacion  ,
		 r_tmp_ret_detalle.v_id_traspaso   ,
         r_tmp_ret_detalle.v_cve_refer      ,
         r_tmp_ret_detalle.v_imp_viv72      ,
         r_tmp_ret_detalle.v_tanto_imp_viv72,
         r_tmp_ret_detalle.v_entidad        ,
         r_tmp_ret_detalle.v_causal_retiro  ,
         r_tmp_ret_detalle.v_caso_adai      ,
         r_tmp_ret_detalle.v_motivo_rech_1  
         
         -- Asigna NSS y RFC
         SELECT nss, rfc 
         INTO   r_tmp_ret_detalle.v_nss,
                r_tmp_ret_detalle.v_rfc_trabajador
         FROM   afi_fondo72
         WHERE  id_derechohabiente = v_id_derechohabiente
	 	
	 	-- si no esta, entonces se busca en la tabla de id_derechohabientes nuevos de fondo72
	 	IF ( r_tmp_ret_detalle.v_nss IS NULL AND r_tmp_ret_detalle.v_rfc_trabajador IS NULL ) THEN
            SELECT nss, rfc
            INTO   r_tmp_ret_detalle.v_nss,
	 	          r_tmp_ret_detalle.v_rfc_trabajador
            FROM   afi_fondo72_d
            WHERE  id_derechohabiente = v_id_derechohabiente
	 	END IF
         
         --Asigna máscara al motivo de rechazo
         LET r_tmp_ret_detalle.v_motivo_rech_1 = r_tmp_ret_detalle.v_motivo_rech_1 USING "&&&"
        
          -- Suma el total de los pesos (monto + tanto adicional)
          LET v_sum_total_pesos = v_sum_total_pesos + r_tmp_ret_detalle.v_imp_viv72 + r_tmp_ret_detalle.v_tanto_imp_viv72
         
         -- Formatea la fecha
         LET v_c_fec_solicitud = r_tmp_ret_detalle.v_f_solicitud USING "yyyymmdd"
   
         --Formatea la entidad
         LET v_c_entidad = r_tmp_ret_detalle.v_entidad USING "&&&&&&"
         
         --Formatea los importes
         LET v_c_imp_viv72       = fn_elimina_punto(r_tmp_ret_detalle.v_imp_viv72)
         LET v_c_imp_viv72       = v_c_imp_viv72      USING "&&&&&&&&&&&&&&"        
         LET v_c_tanto_imp_viv72 = fn_elimina_punto(r_tmp_ret_detalle.v_tanto_imp_viv72)
         LET v_c_tanto_imp_viv72 = v_c_tanto_imp_viv72 USING "&&&&&&&&"
         LET v_c_causal_retiro   = r_tmp_ret_detalle.v_causal_retiro   USING "&&&&&&"
         LET v_c_caso_adai       = r_tmp_ret_detalle.v_caso_adai       USING "&&&&&&&&&&" 
    
        --Escribe en el archivo
        LET v_s_detalle_rec = r_tmp_ret_detalle.v_tpo_registro        ,      
                              r_tmp_ret_detalle.v_id_servicio         ,      
                              r_tmp_ret_detalle.v_id_operacion        ,      
                              r_tmp_ret_detalle.v_nss                 ,     
                              v_c_fec_solicitud                       ,         
                              r_tmp_ret_detalle.v_f_liquidacion USING "yyyymmdd",         
                              r_tmp_ret_detalle.v_cve_refer           ,     
                              v_c_imp_viv72                           ,
                              v_c_tanto_imp_viv72                     , 
                              v_c_entidad                             , 
                              v_c_causal_retiro                       , 
                              v_c_caso_adai                           , 
                              r_tmp_ret_detalle.v_RFC_trabajador      ,     
                              r_tmp_ret_detalle.v_id_traspaso         ,      
                              r_tmp_ret_detalle.v_resultado_operacion ,      
                              r_tmp_ret_detalle.v_motivo_rech_1       ,      
                              r_tmp_ret_detalle.v_motivo_rech_2
                            
         --Se escribe el detalle en el archivo            
         CALL v_ch_arc_rechazados.writeLine(v_s_detalle_rec)                                               
    
   END FOREACH

   --Elimina punto decimal y asigna valor
   LET v_c_suma_total = fn_elimina_punto(v_sum_total_pesos)
   
   --Asigna máscara a la suma del total
   LET v_c_suma_total = v_c_suma_total USING "&&&&&&&&&&&&&&"

   --Se asigna el valor del  tipo de registro para sumario
   LET r_tmp_ret_detalle.v_tpo_registro = "09"
   
   --Asigna sumario     
   LET v_s_sumario_rec =r_tmp_ret_detalle.v_tpo_registro,
                        r_tmp_ret_detalle.v_id_servicio ,
                        r_tmp_ret_detalle.v_id_operacion,
                        v_c_suma_total
                        
                        
   --Se escribe el encabezado en el archivo            
   CALL v_ch_arc_rechazados.writeLine(v_s_sumario_rec)
   
   --se cierra el archivo   
   CALL v_ch_arc_rechazados.CLOSE()                    
                                      

END FUNCTION
#Objetivo. Elimina el punto decimal de las cifras
FUNCTION fn_elimina_punto(v_c_cifra)
DEFINE   v_c_cifra              CHAR(14),
         v_t_token              base.StringTokenizer,
         v_c_cifra_caracter     CHAR(14),
         v_c_cifra_enteros      CHAR(12),
         v_c_cifra_decimal      CHAR(2),
         v_s_indice             SMALLINT
         
   --Crea el tokenizer para obtener las subcadenas
   LET v_t_token = base.StringTokenizer.create(v_c_cifra,".")
   
   --Inicializa el índice
   LET v_s_indice = 1
   
   -- Iteración de las cadenas
   WHILE v_t_token.hasMoreTokens()
   
      IF ( v_s_indice = 1 ) THEN
   
          --Se asignan los enteros de la cifra
         LET v_c_cifra_enteros = v_t_token.nextToken()
         LET v_s_indice = v_s_indice + 1 
      ELSE
        --Se asignan los decimales
        LET v_c_cifra_decimal = v_t_token.nextToken()
      END IF  
   
   END WHILE
   
   --Se concatena la cadena
   LET v_c_cifra_caracter = v_c_cifra_enteros CLIPPED || v_c_cifra_decimal
   
   --Regresa la cadena formada  
   RETURN v_c_cifra_caracter
END FUNCTION