--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23/09/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIP22                                                        #
#Objetivo     => Lanzado indicador de crédito y desmarca                       #
#Fecha inicio => 23/09/2015                                                    #
################################################################################

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid            LIKE bat_ctr_proceso.pid,            --  ID del proceso
       g_proceso_cod    LIKE cat_proceso.proceso_cod,        -- codigo del proceso
       p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod,  -- codigo del proceso
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre dle archivo
       v_folio_lote     DECIMAL(9,0),                        -- Folio de proceso
       g_opera_cod      LIKE cat_operacion.opera_cod         -- codigo de operacion,
END GLOBALS
###############################################################################
MAIN
   DEFINE p_pid               LIKE bat_ctr_operacion.pid,        -- PID del proceso      
          p_opera_cod         LIKE bat_ctr_operacion.opera_cod,  -- codigo de la operacion
          p_usuario_cod       LIKE seg_usuario.usuario_cod,      -- clave del usuario firmado
          v_folio_unificacion LIKE deo_preliquida.folio_liquida, -- folio del lote
          p_folio_liquidacion LIKE deo_preliquida.folio_liquida, -- folio de la liquidación
          v_si_resultado      INTEGER,                           -- resultado del proceso
          v_si_resultado_ind  INTEGER,                           -- resultado del proceso
          p_titulo            STRING,                            -- titulo del mensaje enviado en el correo
          p_mensaje           STRING,                            -- cuerpo del mensaje enviado
          v_layout            LIKE cat_operacion.layout_cod,
          v_ruta_rescate      STRING,
          v_usuario           LIKE seg_modulo.usuario,
          v_proceso_desc      LIKE cat_proceso.proceso_desc,
          v_extension         LIKE cat_operacion.extension,
          v_opera_desc        LIKE cat_operacion.opera_desc,
          v_ruta_listados     LIKE seg_modulo.ruta_listados

   LET p_usuario_cod       = ARG_VAL(1)
   LET p_pid               = ARG_VAL(2)
   LET p_proceso_cod       = ARG_VAL(3)
   LET p_opera_cod         = ARG_VAL(4)
   LET p_folio_liquidacion = ARG_VAL(5)
   LET p_nombre_archivo    = ARG_VAL(6)
   
   CALL fn_recupera_inf_proceso(p_proceso_cod,
                                p_opera_cod) 
        RETURNING v_proceso_desc,
                  v_extension, 
                  v_opera_desc,
                  v_layout, 
                  v_ruta_rescate,
                  v_ruta_listados,
                  v_usuario
      
   -- se asigna proceso y operacion
   LET g_pid         = p_pid      
   LET g_proceso_cod = p_proceso_cod -- nuevo modelo unificacion
   LET g_opera_cod   = p_opera_cod   -- indicadores

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod    
   
    --Actualización de Indicadores
   CALL actualiza_indicadores_IMSS(p_nombre_archivo,
                                   p_usuario_cod,
                                   p_folio_liquidacion)      
  
   LET v_si_resultado     = 0
   LET v_si_resultado_ind = 0
            
   LET p_titulo = "Finalización de proceso - " || v_proceso_desc CLIPPED || " - ACTUALIZACIÓN DE ÍNDICADORES Y DESMARCAS"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)

END MAIN
###############################################################################
#OBJETIVO: Obtener la descripcion del error
#          Obtiene la descripcion del error de la validacion y
#          la muestra en mensaje para suario.
###############################################################################
FUNCTION fn_mues_desc_valida(p_resultado_opera)
   DEFINE p_resultado_opera SMALLINT,
          v_descripcion     LIKE cat_bat_parametro_salida.descripcion

   -- Obtiene la descripción resultado de la validacion
   SELECT descripcion
   INTO   v_descripcion
   FROM   cat_bat_parametro_salida
   WHERE  cod_salida = p_resultado_opera

   -- Muestra el mensaje encontrado
   DISPLAY "Atención ",v_descripcion CLIPPED
   
   RETURN v_descripcion CLIPPED
END FUNCTION -- fn_mues_desc_valida

#OBJETIVO:  Actualizar los indicadores de créditos y desmarcar las cuentas
FUNCTION  actualiza_indicadores_IMSS(p_nombre_archivo,
                                     p_usuario_cod,
                                     p_folio_liquidacion)
                                     
   DEFINE p_nombre_archivo     CHAR(40),
          p_usuario_cod        CHAR(20),
          p_folio_liquidacion  DECIMAL(9,0),
          v_folio_unificacion  DECIMAL(9,0),
          v_cadena             STRING,
          v_f_actualiza        DATE,
          r_bnd_fin_oper       SMALLINT,
          p_mensaje            STRING,  -- cuerpo del mensaje enviado
          
          v_si_resultado_ind   INTEGER, -- resultado del proceso de indicadores de credito
          isam_err_ind         INTEGER,
          v_c_msj_ind          CHAR(200),
          v_num_registros      INTEGER,
          v_c_id_unificador    DECIMAL(9,0),
          
          v_si_resultado       INTEGER, -- resultado del proceso de desmarca e inhabilitacion
          isam_err             INTEGER,
          v_c_msj              CHAR(200),
          
          v_sqltxt             STRING,
          v_indx               INTEGER,
          bandera              INTEGER,
          bandera_uni          INTEGER,
          v_s_sql              STRING,
          v_seq_cre_archivo    BIGINT,
          v_ctr_folio_archivo  DECIMAL(9,0),
          v_num                SMALLINT,
          v_total_creditos     INTEGER

   DEFINE v_arr_unificador  DYNAMIC ARRAY OF RECORD -- registro 
    	 	  v_id_det_unificador         LIKE uni_det_unificador.id_unificador,
    	 	  v_id_unificado          LIKE uni_det_unificado.id_unificado,
    	 	  v_folio_unificacion     LIKE uni_det_unificador.folio_unificacion,
    	 	  v_id_derecho_unificador LIKE uni_det_unificador.id_derechohabiente,
    	 	  v_id_derecho_unificado  LIKE uni_det_unificado.id_derechohabiente,
    	 	  nsscta1                 CHAR(11)
   END RECORD 
   
   DEFINE v_resultado_operacion_acep_ind   SMALLINT,
          v_resultado_operacion_rech_ind   SMALLINT,
          v_resultado_operacion_acep_marca SMALLINT,
          v_resultado_operacion_rech_marca SMALLINT
       
   LET bandera     = 0
   LET bandera_uni = 0
   LET v_num       = 22

   LET v_resultado_operacion_acep_ind   = 0
   LET v_resultado_operacion_rech_ind   = 0
   LET v_resultado_operacion_acep_marca = 0
   LET v_resultado_operacion_rech_marca = 0

   --Recupera folio de unificación
   SELECT folio_referencia
   INTO   v_folio_unificacion
   FROM   glo_folio
   WHERE  folio = p_folio_liquidacion
   
   SELECT seq_cre_archivo.NEXTVAL
   INTO   v_seq_cre_archivo
   FROM   systables
   WHERE  tabname = "cre_ctr_archivo"
   
   LET v_s_sql = "EXECUTE FUNCTION fn_genera_folio(?, ?, ?)"
   
   PREPARE sid_genera_folio FROM v_s_sql
   EXECUTE sid_genera_folio USING g_proceso_cod,
                                  v_num,
                                  p_usuario_cod
           INTO v_ctr_folio_archivo
           
   LET v_sqltxt = "  SELECT a.id_unificador,     ",
                "\n         b.id_unificado,      ",
                "\n         a.folio_unificacion, ",
                "\n         a.id_derechohabiente,",
                "\n         b.id_derechohabiente,",
                "\n         b.nsscta1            ",
                "\n  FROM   uni_det_unificador a,",
                "\n         uni_det_unificado b",
                "\n  WHERE  a.id_unificador     = b.id_unificador",
                "\n  AND    a.estado_familia    = 1",
                "\n  AND    a.folio_unificacion = ",v_folio_unificacion,
                "\n  AND    a.ind_procedencia   = 1 ",
                "\n  AND    a.diagnostico       = 4 ",
                "\n  UNION ",
                "\n  SELECT a.id_unificador,     ",
                "\n         b.id_unificado,      ",
                "\n         a.folio_unificacion, ",
                "\n         a.id_derechohabiente,",
                "\n         b.id_derechohabiente,",
                "\n         b.nsscta1            ",
                "\n  FROM   uni_det_unificador a,",
                "\n         uni_det_unificado b",
                "\n  WHERE  a.id_unificador     = b.id_unificador",
                "\n  AND    a.estado_familia    = 1",
                "\n  AND    a.folio_unificacion IN (SELECT folio_unificacion ", 
                "\n                                 FROM   uni_det_procedencia ", 
                "\n                                 WHERE  folio_resp_confronta = " ,v_folio_unificacion,
                "\n                                 AND    ind_procedencia      = 1)",
                "\n  AND    a.ind_procedencia   = 1 ",
                "\n  AND    a.diagnostico       = 4 ",
                "\n  GROUP BY 1,2,3,4,5,6 "

   LET v_indx = 1

   PREPARE prp_consulta_ids FROM v_sqltxt
   DECLARE cur_consulta_ids CURSOR FOR prp_consulta_ids 

   DISPLAY "#       Cifras de Indicadores de credito y Desmarca de Unificación          #"
   DISPLAY "#                  Indicadores de credito    |   Desmarca de Unificación    #"
   DISPLAY "#NSS unificado |   Aceptados  |     Error    |   Aceptados  |     Error     #"

   FOREACH cur_consulta_ids INTO v_arr_unificador[v_indx].*
      SELECT f_actualiza 
      INTO   v_f_actualiza
      FROM   glo_folio
      WHERE  folio = v_arr_unificador[v_indx].v_folio_unificacion

      CALL fn_total_creditos (v_arr_unificador[v_indx].v_id_det_unificador)
      RETURNING v_total_creditos

      IF v_total_creditos = 0 OR v_total_creditos = 1 THEN
         LET v_s_sql = "EXECUTE FUNCTION fn_unifica_credito(?,?,?,?,?,?,?,?,?,?,?)"

         PREPARE Prpr_dtos_credito FROM v_s_sql CLIPPED
         EXECUTE Prpr_dtos_credito  USING v_arr_unificador[v_indx].v_id_det_unificador,    --id_unificador
	                                      v_arr_unificador[v_indx].v_id_unificado,         --id_unificado
                                          v_arr_unificador[v_indx].v_id_derecho_unificado, --id_derechohabiente unificado
                                          v_arr_unificador[v_indx].v_id_derecho_unificador,--id_derechohabiente unificador                                          
                                          v_arr_unificador[v_indx].v_folio_unificacion,    --folio_unificacion
                                          v_f_actualiza,                                   --f_actualiza del folio de unifcacion
                                          g_proceso_cod,                                   --codigo de proceso
                                          p_nombre_archivo,                                --nombre de archivo de credito pasa en nulo
                                          p_usuario_cod,                                   --usuario_cod
                                          v_seq_cre_archivo,                               --secuencia de archivo de credito en 0
                                          v_ctr_folio_archivo                              --folio de archivo de credito
                 INTO v_si_resultado_ind, 
                      isam_err_ind,       
                      v_c_msj_ind,
                      v_num_registros, 
                      v_c_id_unificador

         IF v_si_resultado_ind < 0 THEN
            DISPLAY "# # # # # # # # # # # #"
            DISPLAY "# Error en Proceso de unificación de indicadores de crédito "
            DISPLAY "# Res: ",v_si_resultado_ind
            DISPLAY "# Msg: ",v_c_msj_ind
            DISPLAY "# Isa: ",isam_err_ind
            DISPLAY "# Id : ",v_c_id_unificador
            DISPLAY "# # # # # # # # # # # #"
         
            LET v_resultado_operacion_rech_ind = 1          
         ELSE
            LET v_resultado_operacion_rech_ind = 0
         
            IF v_num_registros > 0 THEN
               LET v_resultado_operacion_acep_ind = 1
            ELSE
               LET v_resultado_operacion_acep_ind = 0
            END IF
         END IF

      -- ejecucion de funcion de desmarca e inhabilitacion de unificacion
      LET v_s_sql = "EXECUTE FUNCTION fn_uni_posliquida_recurrente(?, ?, ?, ?, ?)"         

      PREPARE sid_indicadores FROM v_s_sql
      EXECUTE sid_indicadores USING p_usuario_cod,                                   --usuario_cod
                                    v_arr_unificador[v_indx].v_folio_unificacion,    --folio_unificacion
                                    p_proceso_cod,                                   --codigo de proceso
                                    v_arr_unificador[v_indx].v_id_derecho_unificado, --id_derechohabiente unificado 
                                    v_arr_unificador[v_indx].v_id_derecho_unificador --id_derechohabiente unificador
              INTO v_si_resultado,
                   isam_err,
                   v_c_msj 
      
         IF v_si_resultado = 0 THEN
            UPDATE uni_det_unificador
               SET diagnostico = 5 -- indicadores
            WHERE  diagnostico = 4 -- liquidados
            AND    folio_unificacion = v_arr_unificador[v_indx].v_folio_unificacion
            AND    id_unificador = v_arr_unificador[v_indx].v_id_det_unificador                 
         
            UPDATE uni_det_unificado
               SET diagnostico = 5 -- Indicadores
            WHERE  diagnostico = 4 -- Liquidados
            AND    id_unificador = v_arr_unificador[v_indx].v_id_det_unificador                 
            AND    id_unificado = v_arr_unificador[v_indx].v_id_unificado
                    
            LET v_resultado_operacion_acep_marca = 1
         ELSE
            -- Se finaliza operacion aunque no se termine correctamente el error
            DISPLAY "# Error en Proceso de Desmarca del Derechohabiente :",v_arr_unificador[v_indx].v_id_derecho_unificador
            DISPLAY "# # # # # # # # # # # #"
            DISPLAY "# Res :",v_si_resultado
            DISPLAY "# Msg: ",v_c_msj
            DISPLAY "# Isa: ",isam_err
            DISPLAY "# # # # # # # # # # # #"
         
            LET v_resultado_operacion_rech_marca = 1            
         END IF
      END IF 	          

      LET v_total_creditos = 0;
      
      DISPLAY "  ",
              v_arr_unificador[v_indx].nsscta1 USING "&&&&&&&&&&&",
              "      ",
              v_resultado_operacion_acep_ind   USING "####&",
              "           ",
              v_resultado_operacion_rech_ind   USING "####&",
              "           ",
              
              v_resultado_operacion_acep_marca USING "####&",
              "          ",
              v_resultado_operacion_rech_marca USING "####&",
              "      "
              
      LET v_indx = v_indx + 1

      LET v_resultado_operacion_acep_ind   = 0
      LET v_resultado_operacion_rech_ind   = 0
      LET v_resultado_operacion_acep_marca = 0
      LET v_resultado_operacion_rech_marca = 0
   END FOREACH

   IF v_indx = 1 THEN 
      DISPLAY "# NO EXISTE INFORMACIÓN DE DETALLES ",v_indx
      
      {CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
          RETURNING r_bnd_fin_oper}

      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                                  g_proceso_cod, --- Clave del proceso
                                  g_opera_cod)   --- Clave de la operación
           RETURNING r_bnd_fin_oper
   ELSE
      LET v_indx = v_indx - 1
      DISPLAY "# TOTAL DE REGISTROS ",v_indx
   END IF 
   
   IF v_si_resultado_ind = 0 THEN    
      INSERT INTO cre_ctr_archivo (id_cre_ctr_archivo,
                                   folio_archivo,
                                   lote, 
                                   f_lote, 
                                   id_proceso, 
                                   operacion,
                                   nom_archivo,
                                   tot_registros,
                                   tot_aceptados,
                                   tot_rechazados, 
                                   tot_sin_origen, 
                                   estado, 
                                   f_proceso,
                                   usuario)
             VALUES (v_seq_cre_archivo,
                     v_ctr_folio_archivo,
                     1,
                     v_f_actualiza,
                     g_proceso_cod,
                     22,
                     p_nombre_archivo,
                     v_num_registros,
                     v_num_registros,
                     0,
                     0,
                     20,
                     TODAY,
                     p_usuario_cod
                     );

   ELSE      
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
           RETURNING r_bnd_fin_oper
   END IF
   
   IF v_si_resultado = 0 THEN

      -- se invoca la finalizacion de la operacion
      CALL fn_actualiza_opera_fin(g_pid,         --- Identificador del proceso
                                  g_proceso_cod, --- Clave del proceso
                                  g_opera_cod)   --- Clave de la operación
           RETURNING r_bnd_fin_oper
      
      DISPLAY "---------------------------------------------------------------------------------------"
      DISPLAY "# Total de registros como unificados: ", v_indx USING "####&",
                                            " Folio: ", v_folio_unificacion USING "<<<<<<<<<",
                                           " Bandera:", r_bnd_fin_oper USING "##&"
      DISPLAY "                                                                                       "                                           
      
      LET p_mensaje = "# # # # # # # # # # # # # # # # # # # # # # # # # # \n",
                      "# \n",
                      "#    Unificación de indicadores de crédito y desmarca de cuentas terminó completamente. \n",
                      "# \n",
                      "#    El folio Lote: "||v_folio_unificacion,"\n",
                      "# \n",
                      "# # # # # # # # # # # # # # # # # # # # # # # # # #"
   ELSE
      CALL fn_error_opera(g_pid,
                          g_proceso_cod,
                          g_opera_cod)
           RETURNING r_bnd_fin_oper
         
      DISPLAY fn_mues_desc_valida(r_bnd_fin_oper)
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      DISPLAY "#    "
      DISPLAY "#    Error al procesar la unificación de indicadores créditos o desmracas"
      DISPLAY "#    El status de resultado indicadores crédito es      : ", v_si_resultado
      DISPLAY "#    El status de resultado desmarca e inhabilitacion es: ", v_si_resultado
      DISPLAY "#    "
      DISPLAY "# # # # # # # # # # # # # # # # # # # # # # # # # #"
      
      LET p_mensaje = " --- ERROR ---\n",
                      " El proceso de Unificación de Indicadores credito o desmarca no terminó correctamente.\n",
                      " Código de error : ", r_bnd_fin_oper,"\n ",
                      " FECHA           : ",TODAY,"\n",
                      " HORA            : ",CURRENT HOUR TO SECOND,"\n"        
   END IF                 

   CALL v_arr_unificador.deleteElement(v_arr_unificador.getLength())

END FUNCTION

#OBJETIVO: Calcular el total de créditos
FUNCTION fn_total_creditos(p_id_unificador)
DEFINE p_id_unificador       DECIMAL (9,0),
       v_QryTxt              CHAR(70),
       v_tipo_consulta       SMALLINT,
       v_resultado           SMALLINT,
       v_tpo_originacion     SMALLINT,
       v_tpo_credito         SMALLINT,
       v_num_credito         DECIMAL(10,0),
       v_f_otorga            DATE,
       v_f_liquida           DATE,
       v_resultado_dor       SMALLINT,
       v_tpo_originacion_dor SMALLINT,
       v_tpo_credito_dor     SMALLINT,
       v_num_credito_dor     DECIMAL(10,0),
       v_f_otorga_dor        DATE,
       v_f_liquida_dor       DATE,  
       v_id_dh_dor           DECIMAL (9,0),
       v_total_creditos  INTEGER,
       i                 INTEGER 
DEFINE arr_derechohabientes DYNAMIC ARRAY OF RECORD 
       v_id_dh_ado DECIMAL(9,0),
       v_id_dh_dor DECIMAL(9,0)
END RECORD
       
   LET v_tipo_consulta = 0
   LET v_total_creditos = 0

   LET i = 1;

   DECLARE cur_id_ado CURSOR FOR SELECT a.id_derechohabiente 
                                 FROM   uni_det_unificado a
                                 WHERE  a.id_unificador = p_id_unificador
                                 GROUP BY 1 

                                 
   FOREACH cur_id_ado INTO arr_derechohabientes[i].v_id_dh_ado
      LET v_resultado = 2;
      
      LET v_QryTxt = "EXECUTE FUNCTION fn_credito_vivienda(?,?)"
      PREPARE prp_cred_viv_ado FROM v_QryTxt CLIPPED
      EXECUTE prp_cred_viv_ado USING arr_derechohabientes[i].v_id_dh_ado,  
                                     v_tipo_consulta
                               INTO  v_resultado,
                                     v_tpo_originacion,
                                     v_tpo_credito,
                                     v_num_credito,
                                     v_f_otorga, 
                                     v_f_liquida;

      IF v_resultado = 0 THEN 
         LET v_total_creditos = v_total_creditos + 1;
      END IF 

      LET i = i + 1;
   END FOREACH

   SELECT b.id_derechohabiente
   INTO   v_id_dh_dor
   FROM   uni_det_unificador b
   WHERE  b.id_unificador = p_id_unificador
   GROUP BY 1

   LET v_resultado_dor = 2;
   
   LET v_QryTxt = "EXECUTE FUNCTION fn_credito_vivienda(?,?)"
   PREPARE prp_cred_viv_dor FROM v_QryTxt CLIPPED
   EXECUTE prp_cred_viv_dor USING v_id_dh_dor,
                                  v_tipo_consulta
                            INTO  v_resultado_dor,
                                  v_tpo_originacion_dor,
                                  v_tpo_credito_dor,
                                  v_num_credito_dor,
                                  v_f_otorga_dor, 
                                  v_f_liquida_dor;

   IF v_resultado_dor = 0 THEN 
      LET v_total_creditos = v_total_creditos + 1;
   END IF
      
   RETURN v_total_creditos

END FUNCTION 