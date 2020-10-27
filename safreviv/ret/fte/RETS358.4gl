--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo           => RET                                                                     #                        
#Programa         => RETS358                                                                 #                      
#Objetivo         => Genera archivo de rechazos de Aclaraciones del Fondo de Ahorro          #
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS358.log")
   
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
   WHERE  proceso_cod = g_proceso_aclara_fa
   AND    folio = p_folio
  
   LET v_fecha_formateada = TODAY USING "yymmdd"
   
      LET v_v_nom_archivo_rechazo="/"
   
      --Se crea el nombre del archivo
      LET v_v_nom_archivo_rechazo = v_v_nom_archivo_rechazo,v_archivo_original[1,8], "rech.aclfa"

      --Inicializa variable de conteo     
      LET v_i_contador_registros = 0

      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_det_aclara_fondo_ahorro
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
         v_c_fec_aclara           CHAR(8),
         v_sum_tot_registros      CHAR(8),
         v_f_opera_cza            DATE,
         v_c_imp_movto            CHAR(13),
         v_c_tanto_imp_viv72      CHAR(8),
         v_c_entidad              CHAR(6),
         v_c_causal_retiro        CHAR(6),
         v_c_caso_adai            CHAR(10),
         v_cod_rechazo            LIKE ret_fondo_ahorro.cod_rechazo
DEFINE  r_tmp_ret_encabezado  RECORD
 	        v_tpo_registro     CHAR(2),
 	        v_id_servicio      CHAR(2),
 	        v_id_operacion     CHAR(2),
 	        f_operacion        CHAR(8)
         END RECORD
         
DEFINE  r_tmp_ret_detalle RECORD
 	      v_tpo_registro         CHAR(2),      
          v_id_servicio          CHAR(2),      
          v_id_operacion         CHAR(2),      
          v_rfc_saci             CHAR(13),     
          v_folio_aclara         DECIMAL(9,0),         
          v_f_aclara             DATE,         
          v_rfc_nuevo            CHAR(13),     
          v_nss_aclarado         CHAR(11),
          v_nombre_aclarado      CHAR(40), ---
          v_tpo_movto            CHAR(2), 
          v_imp_movto            DECIMAL(13,2), 
          v_estatus              CHAR(1), 
          v_cod_rechazo          CHAR(3)       
         END RECORD
         
 	--se inicializan las variables 
   LET v_s_encabezado_acp = NULL
   LET v_s_detalle_acp    = NULL
   LET v_s_sumario_acp    = NULL
   LET v_sum_total_pesos  = 0.00
   LET v_v_ruta_arc_aceptados = v_c_ruta_env_arch CLIPPED || v_v_nom_arc_aceptados
   DISPLAY "\n   Archivo [RECHAZADOS] : ", v_v_ruta_arc_aceptados
   
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_aceptados = base.Channel.create()     
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_aceptados.openFile(v_v_ruta_arc_aceptados, "w" )

   SELECT f_operacion, tot_registros
     INTO v_f_opera_cza, v_sum_tot_registros
     FROM ret_cza_aclara_fondo_ahorro
    WHERE folio = p_folio;
   
   --Asignación de las variables del encabezado
   LET r_tmp_ret_encabezado.v_tpo_registro  = "01"
   LET r_tmp_ret_encabezado.v_id_servicio   = "04"
   LET r_tmp_ret_encabezado.v_id_operacion  = "20"
   LET r_tmp_ret_encabezado.f_operacion     = v_f_opera_cza USING "yyyymmdd"
   
   --Asignación del encabezado
   LET v_s_encabezado_acp = r_tmp_ret_encabezado.v_tpo_registro  ,
                            r_tmp_ret_encabezado.v_id_servicio   ,
                            r_tmp_ret_encabezado.v_id_operacion  ,
                            r_tmp_ret_encabezado.f_operacion     
   
    --Se escribe el encabezado en el archivo            
    CALL v_ch_arc_aceptados.writeLine(v_s_encabezado_acp)
    
    DISPLAY "Obteniendo solicitudes rechazadas"
    
    --Query para la selección de los campos   
    LET ls_sql = "\n SELECT rfc_saci                     ,",
                 "\n        folio_aclara                 ,",
                 "\n        f_aclara                     ,",
                 "\n        rfc_nuevo                    ,",
	 			 "\n        nss_aclarado                 ,",
				 "\n        nombre_aclarado              ,",
                 "\n        tpo_movto                    ,",
                 "\n        imp_movto                    ,",
                 "\n        estatus                      ,",
                 "\n        cod_rechazo                   ",
                 "\n FROM   ret_det_aclara_fondo_ahorro   ",
                 "\n WHERE  folio       = ", p_folio,
                 "\n AND    cod_rechazo <> 0 "
    
    PREPARE stm_detalle_fondo_ahorro FROM ls_sql        
    DECLARE cur_detalle_fondo_ahorro CURSOR FOR stm_detalle_fondo_ahorro
   
    --Se asignan valores por defecto en el detalle          
    LET r_tmp_ret_detalle.v_tpo_registro        = "03"
    LET r_tmp_ret_detalle.v_id_servicio         = "04"
    LET r_tmp_ret_detalle.v_id_operacion        = "20"
   
    DISPLAY "Foreach de lectura de solicitudes..."
    FOREACH cur_detalle_fondo_ahorro
    INTO r_tmp_ret_detalle.v_rfc_saci            ,
         r_tmp_ret_detalle.v_folio_aclara ,
         r_tmp_ret_detalle.v_f_aclara   ,
	 	 r_tmp_ret_detalle.v_rfc_nuevo  ,
		 r_tmp_ret_detalle.v_nss_aclarado    ,
         r_tmp_ret_detalle.v_nombre_aclarado     ,
         r_tmp_ret_detalle.v_tpo_movto     ,
         r_tmp_ret_detalle.v_imp_movto,
         r_tmp_ret_detalle.v_estatus        ,
         r_tmp_ret_detalle.v_cod_rechazo 
         
	 	DISPLAY "ID_DER: ", v_id_derechohabiente
	 	DISPLAY "NSS: ", r_tmp_ret_detalle.v_rfc_saci
	 	DISPLAY "RFC: ", r_tmp_ret_detalle.v_rfc_nuevo
                
         
         --Formatea la fecha de solicitud
         LET v_c_fec_aclara = r_tmp_ret_detalle.v_f_aclara USING "yyyymmdd"
   
         --Formatea el importe
         LET v_c_imp_movto        = r_tmp_ret_detalle.v_imp_movto USING "&&&&&&&&&&.&&"        

         
	    DISPLAY "Escribiendo registro de detalle"
        -- Escribe en el archivo
        LET v_s_detalle_acp = r_tmp_ret_detalle.v_tpo_registro       ,      
                              r_tmp_ret_detalle.v_id_servicio        ,      
                              r_tmp_ret_detalle.v_id_operacion       ,
                              r_tmp_ret_detalle.v_rfc_saci           ,            
                              r_tmp_ret_detalle.v_folio_aclara       ,
                              v_c_fec_aclara                         ,     
                              r_tmp_ret_detalle.v_nss_aclarado       ,
                              r_tmp_ret_detalle.v_nombre_aclarado    ,
                              r_tmp_ret_detalle.v_tpo_movto          ,
                              v_c_imp_movto                          ,
                              r_tmp_ret_detalle.v_estatus            ,
                              r_tmp_ret_detalle.v_cod_rechazo                            
         --Se escribe el detalle en el archivo            
         CALL v_ch_arc_aceptados.writeLine(v_s_detalle_acp)                    
   
   END FOREACH

   --Asigna el formato
   LET v_c_suma_total = v_sum_tot_registros USING "&&&&&&&&&&&&&&"

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