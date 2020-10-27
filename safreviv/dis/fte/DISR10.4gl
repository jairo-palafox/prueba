################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 13/08/2013                                    #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR10                                                        #
#Objetivo     => Programa que ejecuta el reverso de aportaciones subsecuentes  #
#                especiales                                                    #
#Fecha inicio => 13/08/2013                                                    #
################################################################################

DATABASE safre_viv

MAIN
  --Sección de Variables Locales
  DEFINE 
     v_folio_carga_ap_sub    LIKE dis_ctr_ap_subsecuente.folio,--Folio de Carga de Archivo de Aportaciones Subsecuentes
     v_des_error             CHAR(150)-- Recibe la descripción del error 
     
  --Sección de Parámetros
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING,   -- Titulo de la ventana
    p_pid                    LIKE bat_ctr_proceso.pid, -- ID del proceso
    p_proceso_cod            LIKE cat_proceso.proceso_cod, -- Código del proceso
    p_opera_cod              LIKE cat_operacion.opera_cod -- Código de operacion
    
  --Seccion de Variables de Retorno
  DEFINE  
    r_folio_valido           DECIMAL(9,0),--LIKE dis_ctr_ap_subsecuente.folio,
    r_bandera                SMALLINT,
    des_bandera              CHAR(100),
    r_existe_ef              SMALLINT  --Variable que almacena el valor de la validación si exiten registros en ef
  
  --Arreglo que almacena el detalle
   DEFINE arr_det_ef         DYNAMIC ARRAY OF RECORD
  	 r_folio_det_ef           DECIMAL(9,0),
  	 r_num_regs               DECIMAL(9,0),   
     r_tot_imp_apo_pat        DECIMAL(22,6),
     r_tot_imp_apo_aiv        DECIMAL(22,6)
  END RECORD
  
  --Define los objetos de tipo ventanas
  DEFINE 
    f_ventana                ui.Window,  -- Define las propìedades de la Ventana
    f_forma                  ui.Form     -- Define las propiedades de la forma
 
 --Define la ruta de los ejecutables 
  DEFINE 
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin, -- Ruta del ejecutable
    v_ruta_listados          LIKE seg_modulo.ruta_listados, -- Rut del log
    v_s_mensaje              STRING,
    r_b_valida               INTEGER,
    l_comando                STRING 
       
  LET p_proceso_cod = 908
  LET p_opera_cod   = 2

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  
  -- si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
      CALL ui.Interface.setText(p_s_titulo)
  END IF
  
     --Obtiene las rutas ejecutable
   SELECT ruta_bin INTO v_ruta_ejecutable
   FROM seg_modulo 
   WHERE modulo_cod = 'dis'

   --Obtiene ruta listados
   SELECT ruta_listados INTO v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'bat'
   
  CLOSE WINDOW SCREEN
  
  OPEN WINDOW vtn_reverso_apo_sub WITH FORM "DISR101"   
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_carga_ap_sub
      FROM  f_folio_carga_ap_sub
    
        --Oculta secciones de la forma
        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma  = f_ventana.getForm()
          CALL DIALOG.setActionHidden("reverso",1)
          CALL f_forma.setElementHidden("gr_detalle",1)
   
      END INPUT

      ON ACTION ACCEPT
         IF v_folio_carga_ap_sub IS NULL THEN
            CALL fn_mensaje("ATENCIÓN",
                            "No ha capturado ningun criterio de busqueda",
                            "about")
            NEXT FIELD f_folio_carga_ap_sub
         ELSE
          
            --Valida que el folio capturado exista en la tabla de dis_interface_ef
            LET r_folio_valido=v_folio_carga_ap_sub
            CALL fn_valida_existe_interface_ef(r_folio_valido)
                  RETURNING r_existe_ef
            -- Si el folio no existe la tabla de dis_interface_ef  se envía menaje de error
            IF NOT r_existe_ef  THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "No hay datos con los parametros capturados",
                               "about")
               NEXT FIELD f_folio_carga_ap_sub
            ELSE               
                --Ejecuta función de consulta de valores de dis_interface_ef
               CALL fn_consulta_cifras_ef(r_folio_valido)
               RETURNING arr_det_ef

            END IF
         END IF
       
      DISPLAY ARRAY arr_det_ef TO scr_detalle.*   END DISPLAY
      CALL DIALOG.setActionHidden("accept",1)
      CALL DIALOG.setActionHidden("reverso",0)
      CALL f_forma.setElementHidden("gr_detalle",0)

      ON ACTION reverso    
          

         LET v_folio_carga_ap_sub=arr_det_ef[ARR_CURR()].r_folio_det_ef

         --Se obtiene pid de la operación de acuerdo al folio
         SELECT DISTINCT pid 
         INTO p_pid
         FROM bat_ctr_operacion
         WHERE folio = v_folio_carga_ap_sub

         --Si no existe el pid de acuerdo al folio, traerá el último
         IF p_pid IS NULL OR p_pid = 0 THEN
            CALL fn_max_pid(p_proceso_cod, 1) RETURNING p_pid 
         END IF 
      
         -- Llama la función para validar la ejecucución del reverso
         CALL fn_valida_reverso(p_pid, p_proceso_cod, 2) RETURNING r_bandera
         
         SELECT descripcion 
         INTO   des_bandera
         FROM   cat_bat_parametro_salida
         WHERE  cod_salida = r_bandera             
         DISPLAY "BANDERA REVERSO ",des_bandera

         -- Si el reverso no tiene ningún bloqueo
         IF r_bandera = 0 THEN   
            --Mensaje para confirmar(0) o cancelar(1) el reverso 
            CALL fn_ventana_confirma ("REVERSO","¿Desea ejecutar el reverso?",
                                      "question") RETURNING r_bandera

            IF r_bandera = 1 THEN
               --CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
            
                    LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,
                                    "/DISR101.42r ",p_usuario_cod," ",p_pid," ",
                                    p_proceso_cod," ",p_opera_cod," ",v_folio_carga_ap_sub," ",
                                    " '","NA","' 1>", v_ruta_listados CLIPPED ,
                                    "/nohup:",p_pid USING "&&&&&",":",
                                              p_proceso_cod USING "&&&&&",":",
                                              p_opera_cod USING "&&&&&" ," 2>&1 &"
                    RUN l_comando
                                  
                    LET v_s_mensaje = "Se ha enviado el Reverso de Aportaciones Subsecuentes Especiales con el pid: ",
                        p_pid CLIPPED,
                       ".\nPuede revisar el estado del proceso en el monitor de ejecución de procesos."
                    
                    CALL fn_mensaje("Reverso",v_s_mensaje,"information")
                    
                    EXIT PROGRAM 
              ELSE
                 CALL fn_mensaje("REVERSO","Se ha cancelado el reverso","about")
            END IF
         ELSE-- Si se detecta algún bloqueo
            --Se realiza consulta de descripción en base al codigo de bloqueo
            SELECT descripcion 
            INTO   v_des_error 
            FROM   cat_bat_parametro_salida
            WHERE  cod_salida = r_bandera

            CALL fn_mensaje("REVERSO", v_des_error, "information")
         END IF

         EXIT DIALOG

      ON ACTION cancelar
         EXIT DIALOG

    END DIALOG 

  CLOSE WINDOW vtn_reverso_apo_sub 

END MAIN

#OBJETIVO: Consultar que la información a reversar exista en la tabla de
#          interface de entidades financieras
FUNCTION fn_valida_existe_interface_ef(p_folio)
   DEFINE 
    p_folio                  LIKE dis_interface_ef.folio_liquida,
    r_folio_valido           DECIMAL(9,0),  --LIKE dis_ctr_ap_subsecuente.folio,
    v_qry_txt                STRING,        --Query para consultar sobre dis_interface_ef
    v_existe_int_ef          DECIMAL(9,0),  --Variable para conocer si existen registros en dis_interface_ef
    v_existe_ac_liq          DECIMAL(9,0),  --Variable para conocer si se han ejecutado acciones de conciliación    
    v_bnd_existe_ef          SMALLINT       --Bandera para indicar si existe información con los criterios indicados 
    
   --Se inicializa las variables para conocer si existe folio
   LET v_existe_int_ef = 0
   LET v_existe_ac_liq = 0
   LET v_bnd_existe_ef = TRUE
    
  --Se consulta para conocer si hay registros en dis_interface_ef
  LET v_qry_txt="SELECT COUNT(*)\n ",
                "FROM dis_interface_ef \n",
                 "WHERE folio_liquida=",p_folio,"\n"

  
  PREPARE stm_count_interface_ef FROM v_qry_txt
  EXECUTE stm_count_interface_ef INTO v_existe_int_ef
  
  --Se valida que exista información para el folio indicado
  IF v_existe_int_ef > 0 THEN
  	
  	  --Existen datos y se ejecuta query de validación sobre la existencia de registros conciliados
     LET v_qry_txt="SELECT COUNT(*)\n",
                   "FROM dis_interface_ef \n",
                    "WHERE folio_liquida=",p_folio,"\n",
                    "AND ind_liquidacion <> 0"
     PREPARE stm_count_ind_liq FROM v_qry_txt
     EXECUTE stm_count_ind_liq INTO v_existe_ac_liq
     
     --Se valida la existencia de registros con acciones de conciliación
     IF v_existe_ac_liq > 0 THEN
     	--Existe al menos un registro con acciones de conciliación por lo tanto no se puede eliminar
     	LET v_bnd_existe_ef = FALSE   
     	
     END IF
  	
  ELSE
    	--No existen registros para el folio
    	LET v_bnd_existe_ef = FALSE
    	
  END IF
     
     --Regresa validación de existencia de información y sin registros con acciones de liquidación
     RETURN v_bnd_existe_ef                 
                 
END FUNCTION
	
#Objetivo cargar el arreglo de dis interface_ef
FUNCTION fn_consulta_cifras_ef(p_folio)
  DEFINE 
    p_folio                  LIKE dis_interface_ef.folio_liquida, --Folio de consulta
    v_indice_arr             INTEGER, --Indice del arreglo
    v_qry_int_ef             STRING   --Cadena para armar el query de consulta
  DEFINE arr_det_ef         DYNAMIC ARRAY OF RECORD
  	 r_folio_det_ef           DECIMAL(9,0),
  	 r_num_regs               DECIMAL(9,0),   
     r_tot_imp_apo_pat        DECIMAL(22,6),
     r_tot_imp_apo_aiv        DECIMAL(22,6) 
    
  END RECORD

  LET v_indice_arr  = 1
  
  --Consulta para obtener los totales
   LET v_qry_int_ef="SELECT folio_liquida,COUNT(*),SUM(imp_ap_pat),SUM(aiv_ap_pat)\n",
                    "FROM dis_interface_ef\n",
                    "WHERE folio_liquida=",p_folio,"\n",
                    "GROUP BY 1"
   --Preparación del query
   PREPARE stm_int_ef FROM v_qry_int_ef
   DECLARE cur_int_ef CURSOR FOR stm_int_ef
  
  --Ejecución del query
   FOREACH cur_int_ef INTO arr_det_ef[v_indice_arr].*
   	
   END FOREACH
   	
   RETURN 	arr_det_ef
                    
END FUNCTION