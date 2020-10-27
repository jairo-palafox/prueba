################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR35                                                        #
#Objetivo     => Lanzado reverso de indicadores de crédito y desmarca de       #
#                unificación recurrente                                        #
#Fecha inicio => Septiembre 02, 2014                                           #
################################################################################
--Lanzador: UNIL57

--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 02/09/2014
-- Fecha ultima modificacion: 17/10/2014  HMH
--===============================================================

GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
   DEFINE p_usuario_cod                  LIKE seg_usuario.usuario_cod,         -- clave del usuario firmado
          p_tipo_ejecucion               SMALLINT,                             -- forma como ejecutara el programa
          p_s_titulo                     STRING,                               -- titulo de la ventana
          p_operacion                    SMALLINT,
          p_i_folio                      LIKE dis_preliquida.folio_liquida,
          p_nombre_archivo               LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo

          r_bandera                      SMALLINT,
          v_s_qry                        STRING,
          v_r_reverso_id_derechohabiente DECIMAL(9,0),
          v_r_reverso_id_referencia      DECIMAL(9,0),
          v_r_reverso_marca              SMALLINT,
          v_r_reverso_folio              DECIMAL(9,0),
          v_cadena                       STRING,
          bn_reverso_desmarca            SMALLINT,
          v_folio                        LIKE dis_preliquida.folio_liquida,
          p_titulo                       STRING,                              -- titulo del mensaje enviado en el correo
          p_mensaje                      STRING,                              -- cuerpo del mensaje enviado
          v_id_derechohabiente           DECIMAL(9,0),
          v_id_pre_unificado             DECIMAL(9,0)

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR35.log")
      
   LET v_cadena = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"
   PREPARE prp_reverso_desmarca FROM v_cadena
   
   DISPLAY "FOLIO:",p_i_folio USING "<<<<<<<<<",
           " PROCESO:",g_proceso_cod
   
   DECLARE Curr_reverso_folio CURSOR FOR
   SELECT a.folio_lote,
          a.id_derechohabiente
   FROM   uni_pre_unificador a,
          uni_det_unificador b
   WHERE  a.id_derechohabiente = b.id_derechohabiente
   AND    b.diagnostico = 5
   AND    b.estado_familia = 1
   AND    b.folio_liquidacion = p_i_folio
     
   FOREACH  Curr_reverso_folio INTO v_folio,
    	                               v_id_derechohabiente
     
      
      DECLARE cur_reverso_desmarca_dor CURSOR FOR
      SELECT  id_derechohabiente,
              marca,
              n_referencia,
              folio
      FROM    sfr_marca_historica
      WHERE   folio = v_folio
      AND     id_derechohabiente = v_id_derechohabiente
      AND     marca = 501
         
      FOREACH cur_reverso_desmarca_dor INTO v_r_reverso_id_derechohabiente,
                                            v_r_reverso_marca,
                                            v_r_reverso_id_referencia,
                                            v_r_reverso_folio
     	  
     	  WHENEVER ERROR CONTINUE
     	  
     	  EXECUTE prp_reverso_desmarca USING v_r_reverso_id_derechohabiente,
     	                                     v_r_reverso_marca, --Marca
     	                                     v_r_reverso_id_referencia,
     	                                     v_r_reverso_folio
     	  
     	  IF SQLCA.SQLCODE < 0 THEN
     	  	LET bn_reverso_desmarca = SQLCA.SQLCODE
     	  END IF
     	  
     	  WHENEVER ERROR STOP
     
      END FOREACH
   END FOREACH  
    
   IF bn_reverso_desmarca < 0 THEN
      DISPLAY "El reverso desmarca Unificador con error: ", bn_reverso_desmarca USING "##&",
              " Id Derechohabiente : ",v_r_reverso_id_derechohabiente
   END IF
    
    -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0
   LET v_folio = 0
   LET v_id_derechohabiente = 0
   
   DECLARE Curr_reverso_folio_unificado CURSOR FOR
   SELECT b.id_derechohabiente,
          b.id_pre_unificado
   FROM   uni_pre_unificado b   
   WHERE  b.id_derechohabiente IN (SELECT a.id_derechohabiente
                                   FROM   uni_det_unificado a, uni_det_unificador c
                                   WHERE  a.id_unificador = c.id_unificador
                                   AND    c.estado_familia = 1
                                   AND    c.diagnostico = 5
                                   AND    c.folio_liquidacion = p_i_folio
                                   )
   AND b.estado =  1
   AND b.diagnostico = 1
   
   FOREACH Curr_reverso_folio_unificado INTO v_id_derechohabiente,
                                             v_id_pre_unificado
   
   
      DECLARE cur_reverso_desmarca_do CURSOR FOR
      SELECT id_derechohabiente,
             marca,
             n_referencia,
             folio
      FROM   sfr_marca_historica
      WHERE  n_referencia = v_id_pre_unificado
      AND    id_derechohabiente = v_id_derechohabiente
      AND    marca = 502

      FOREACH cur_reverso_desmarca_do INTO v_r_reverso_id_derechohabiente,
                                           v_r_reverso_marca,
                                           v_r_reverso_id_referencia,
                                           v_r_reverso_folio
       	  
       	 WHENEVER ERROR CONTINUE
       	 
       	 EXECUTE prp_reverso_desmarca USING v_r_reverso_id_derechohabiente,
       	                                    v_r_reverso_marca, --Marca
       	                                    v_r_reverso_id_referencia,
       	                                    v_r_reverso_folio
       	 
       	 IF SQLCA.SQLCODE < 0 THEN
       	 	  LET bn_reverso_desmarca = SQLCA.SQLCODE
       	 END IF
       	 
       	 WHENEVER ERROR STOP
       
      END FOREACH
   END FOREACH  
    
   IF bn_reverso_desmarca < 0 THEN
      DISPLAY "El reverso desmarca Unificado con error: ", bn_reverso_desmarca USING "##&",
              " Id Derechohabiente : ",v_r_reverso_id_derechohabiente
   END IF
   
   CALL fn_reverso_indicadores(g_proceso_cod,p_i_folio)
    
    --  Se reversan los diagnosticos
   UPDATE uni_det_unificador
      SET diagnostico = 4
   WHERE  folio_liquidacion = p_i_folio
   AND    diagnostico = 5
              
   UPDATE uni_det_unificado
      SET diagnostico = 4
   WHERE  folio_unificacion IN (SELECT UNIQUE folio_unificacion
                                FROM   uni_det_unificador 
                                WHERE  folio_liquidacion = p_i_folio)
   AND    diagnostico = 5   
   
      -- Reversa operación
   LET r_bandera = 0
   
   CALL fn_reversa_operacion(g_pid,
                             g_proceso_cod,
                             g_opera_cod)
        RETURNING r_bandera
         
   IF r_bandera = 0 THEN      
      DISPLAY "Operación lista para volver a generarse."
   ELSE
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO INDICADORES"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INDICADORES","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)
   
END MAIN
################################################################################
FUNCTION fn_reverso_indicadores(v_id_proceso_cod, v_folio_liquidacion)
   DEFINE v_id_proceso_cod        SMALLINT,
          v_folio_liquidacion     DECIMAL(9,0)

   DEFINE v_sqltxt                STRING,
          v_s_sql                 STRING,
          v_arr_unificador        DYNAMIC ARRAY OF RECORD
   			  v_id_unificador         LIKE uni_det_unificador.id_unificador,
   			  v_id_unificado          LIKE uni_det_unificado.id_unificado,
   			  v_folio_unificacion     LIKE uni_det_unificador.folio_unificacion,
   			  nss_unificador          CHAR(11),
   			  v_id_derecho_unificador LIKE uni_det_unificador.id_derechohabiente,
   			  nsscta1                 CHAR(11),
   			  v_id_derecho_unificado  LIKE uni_det_unificado.id_derechohabiente
   END RECORD
   
   DEFINE v_ctr_reverso           SMALLINT,
   			  v_indx                  INTEGER,
   			  v_cont_reverso          SMALLINT

   LET v_s_sql = "EXECUTE FUNCTION fn_reverso_unifica_credito(?,?,?)"
       
   PREPARE sid_reverso_indicadores FROM v_s_sql
     
   LET v_sqltxt="SELECT a.id_unificador, ",
             "\n        b.id_unificado,",
             "\n        a.folio_unificacion,",
             "\n        a.nss_unificador,",
             "\n        a.id_derechohabiente,",
             "\n        b.nsscta1,",
             "\n        b.id_derechohabiente",
             "\n FROM   uni_det_unificador a,",
             "\n        uni_det_unificado b",
             "\n WHERE  a.id_unificador = b.id_unificador",
             "\n AND    a.estado_unificacion = 1",
             "\n AND    a.diagnostico = 5",
             "\n AND    a.folio_liquidacion = ",v_folio_liquidacion

   LET v_indx = 1
   
   PREPARE Pr_unificador_imss FROM v_sqltxt
   DECLARE Curr_unificador_IMSS CURSOR FOR Pr_unificador_imss                         
   FOREACH Curr_unificador_IMSS INTO v_arr_unificador[v_indx].*
      
      LET v_cont_reverso = 0
      
      SELECT COUNT(*)
      INTO   v_cont_reverso
      FROM   cre_acreditado
   	  WHERE  id_derechohabiente = v_arr_unificador[v_indx].v_id_derecho_unificado
   	  AND    estado = 230
            
      IF v_cont_reverso > 0 THEN
         ---- se ejecuta el stored procedure
         DISPLAY "Unificación credito NSS unificador :",v_arr_unificador[v_indx].nss_unificador ,
                                 " NSS Unificado :",v_arr_unificador[v_indx].nsscta1 
      DISPLAY "----------------------------------------------------------------------"
      
         EXECUTE sid_reverso_indicadores USING v_id_proceso_cod,
                                               v_arr_unificador[v_indx].v_id_derecho_unificado,
                                               v_arr_unificador[v_indx].v_id_derecho_unificador
                 INTO v_ctr_reverso
      END IF           
   END FOREACH
END FUNCTION