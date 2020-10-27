--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
##############################################################################################
#Modulo            => RET                                                                                             
#Programa         => RETS438                                                                                       
#Objetivo          => Genera el archivo de respuesta a Con los saldos del Infonavit del proceso
#                     de Devoluciones SSV Pensionados
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".RETS438.log")
   
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
        v_fecha_formateada       CHAR(8) -- fecha en formato AAMMDD 


   LET v_i_contador_registros = 0 

   --DISPLAY "folio =  ",p_folio
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

 
   LET v_fecha_formateada = TODAY USING "ddmmyyyy"
   
      LET v_v_nom_archivo = "/"
   
      --Se crean los nombres de los archivos
      LET v_v_nom_archivo         = v_v_nom_archivo, "SSVPensionados_", v_fecha_formateada, ".ssvp"

      SELECT COUNT(*)
      INTO   v_i_contador_registros
      FROM   ret_dif_ssv_pensionado
      WHERE  folio = p_folio
      
      -- si no hay aceptados, no se genera el archivo y se finaliza la ejecucion
      IF ( v_i_contador_registros IS NULL OR v_i_contador_registros < 1 ) THEN
         DISPLAY "____________________________________________________________"
         DISPLAY "   GENERACION DE ARCHIVO DE SALIDA DE REGISTROS "
         DISPLAY ""
         DISPLAY "   A T E N C I O N: No existen registros para el"
         DISPLAY "                    folio ", p_folio
         DISPLAY "                    No se generó archivo de salida."
         DISPLAY "\n____________________________________________________________"
         --EXIT PROGRAM
     
      ELSE
      	-- Se invoca la función de generacion de archivos de salida (1) aceptados
      	CALL fn_genera_archivo_salida(p_folio,v_v_nom_archivo,v_c_ruta_env_acr, 1)
   	
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
         nss                   CHAR(11),
         marca_ws              SMALLINT,
         marca_credito         CHAR(3),
         marca_operativa       CHAR(3),
         aivs_viv92_procesar   DECIMAL(18,2),
         aivs_viv97_procesar   DECIMAL(18,2),
         ape_paterno           CHAR(40),
         ape_materno           CHAR(40),
         nombre                CHAR(40),
         aivs_viv92_saci       DECIMAL(18,2),
         aivs_viv97_saci       DECIMAL(18,2) 
         END RECORD
         
DEFINE  r_detalle RECORD
         nss                   CHAR(11),
         marca_ws              CHAR(2),
         marca_credito         CHAR(3),
         marca_operativa       CHAR(3),
         aivs_viv92_procesar   CHAR(11),
         aivs_viv97_procesar   CHAR(11),
         ape_paterno           CHAR(40),
         ape_materno           CHAR(40),
         nombre                CHAR(40),
         aivs_viv92_saci       CHAR(11),
         aivs_viv97_saci       CHAR(11) 
    END RECORD

         LET v_v_ruta_arc_aceptados = v_c_ruta_env_arch CLIPPED || v_v_nom_arc_aceptados
  
   DISPLAY "\n   Archivo [ACEPTADOS] : ", v_v_ruta_arc_aceptados
   
   --Creación del canal para la escritura del archivo
   LET v_ch_arc_aceptados = base.Channel.create()     
   
   --Se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arc_aceptados.openFile(v_v_ruta_arc_aceptados, "w" )
    
    DISPLAY "Obteniendo solicitudes aceptadas"
    
    --Query para la selección de los campos   
    LET ls_sql = "\n SELECT ",
                 "\n        nss               ,",
                 "\n        marca_ws               ,",
                 "\n        marca_credito,",
	 			     "\n        marca_operativa,",
				     "\n        aivs_viv92_procesar       ,",
                 "\n        aivs_viv97_procesar,",
                 "\n        ape_paterno,",
                 "\n        ape_materno,",
                 "\n        nombre,",
                 "\n        aivs_viv92_saci,",
                 "\n        aivs_viv97_saci    ",
                 "\n FROM   ret_dif_ssv_pensionado   ",
                 "\n WHERE  folio       = ", p_folio
    -- si se pidieron aceptados
    PREPARE stm_detalle_diferencias FROM ls_sql        
    DECLARE cur_detalle_diferencias CURSOR FOR stm_detalle_diferencias
   
  
    DISPLAY "Foreach de lectura de solicitudes..."
    FOREACH cur_detalle_diferencias
    INTO 
         r_tmp_ret_detalle.nss                 ,
         r_tmp_ret_detalle.marca_ws            ,
         r_tmp_ret_detalle.marca_credito       ,
	 	   r_tmp_ret_detalle.marca_operativa     ,
		   r_tmp_ret_detalle.aivs_viv92_procesar ,
         r_tmp_ret_detalle.aivs_viv97_procesar ,
         r_tmp_ret_detalle.ape_paterno         ,
         r_tmp_ret_detalle.ape_materno         ,
         r_tmp_ret_detalle.nombre              ,
         r_tmp_ret_detalle.aivs_viv92_saci     ,
         r_tmp_ret_detalle.aivs_viv97_saci
         

         LET r_detalle.nss                 = r_tmp_ret_detalle.nss                 USING "&&&&&&&&&&&"
         LET r_detalle.marca_ws            = r_tmp_ret_detalle.marca_ws
         LET r_detalle.marca_credito       = r_tmp_ret_detalle.marca_credito
	 	   LET r_detalle.marca_operativa     = r_tmp_ret_detalle.marca_operativa
		   LET r_detalle.aivs_viv92_procesar = r_tmp_ret_detalle.aivs_viv92_procesar*100 USING "##########&"
         LET r_detalle.aivs_viv97_procesar = r_tmp_ret_detalle.aivs_viv97_procesar*100 USING "##########&"
         LET r_detalle.ape_paterno         = r_tmp_ret_detalle.ape_paterno
         LET r_detalle.ape_materno         = r_tmp_ret_detalle.ape_materno
         LET r_detalle.nombre              = r_tmp_ret_detalle.nombre
         LET r_detalle.aivs_viv92_saci     = r_tmp_ret_detalle.aivs_viv92_saci*100     USING "##########&"
         LET r_detalle.aivs_viv97_saci     = r_tmp_ret_detalle.aivs_viv97_saci*100     USING "##########&"
         
	    DISPLAY "Escribiendo registro de detalle"
        -- Escribe en el archivo
        LET v_s_detalle_acp = r_detalle.nss                  , "|",      
                              r_detalle.marca_ws             , "|",     
                              r_detalle.marca_credito        , "|",     
                              r_detalle.marca_operativa      , "|",    
                              r_detalle.aivs_viv92_procesar  , "|",        
                              r_detalle.aivs_viv97_procesar  , "|",        
                              r_detalle.ape_paterno          , "|",    
                              r_detalle.ape_materno          , "|", 
                              r_detalle.nombre               , "|", 
                              r_detalle.aivs_viv92_saci      , "|",
                              r_detalle.aivs_viv97_saci      , "|"
                            
         --Se escribe el detalle en el archivo            
         CALL v_ch_arc_aceptados.writeLine(v_s_detalle_acp)                    
   
   END FOREACH

   
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