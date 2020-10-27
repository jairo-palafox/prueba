






CREATE FUNCTION "safreviv".fn_sep_integra_diagnostico_op27(p_folio DECIMAL(10,0), p_archivo CHAR(40), p_usuario CHAR(20), p_proceso_cod SMALLINT, p_pid DECIMAL(9,0))
RETURNING INTEGER,INTEGER,INTEGER,CHAR(200);

DEFINE v_cza_f_proceso              DATE;
DEFINE v_cza_nombre_archivo         CHAR(40);
DEFINE v_cza_tipo_registro          CHAR(2);
DEFINE v_cza_id_servicio            CHAR(2); 
DEFINE v_cza_id_operacion           CHAR(2);  
DEFINE v_cza_tipo_ent_origen        CHAR(2);
DEFINE v_cza_cve_ent_origen         CHAR(3); 
DEFINE v_cza_tipo_ent_destino       CHAR(2);  
DEFINE v_cza_cve_ent_destino        CHAR(3);
DEFINE v_cza_f_trans_lote           CHAR(8); 
DEFINE v_cza_consecutivo_dia        SMALLINT; 
DEFINE v_cza_consecutivo_cad        CHAR(3);
DEFINE v_cza_resultado_operacion    CHAR(2); 
DEFINE v_cza_diagnostico1           CHAR(3);  
DEFINE v_cza_diagnostico2           CHAR(3);
DEFINE v_cza_diagnostico3           CHAR(3); 
DEFINE v_tipo_registro              CHAR(2);       
DEFINE v_contador_registro          CHAR(12); 
DEFINE v_contador_servicio          CHAR(10);
DEFINE v_contador_servicio_02       DECIMAL(10,0); 
DEFINE v_invadido                   CHAR(11); 
DEFINE v_invadido_verifica          CHAR(11);
DEFINE v_rfc                        CHAR(13);      
DEFINE v_curp                       CHAR(18); 
DEFINE v_tipo_entidad_admon         CHAR(2);
DEFINE v_cve_entidad_admon          CHAR(3);       
DEFINE v_paterno                    CHAR(40); 
DEFINE v_materno                    CHAR(40);
DEFINE v_nombre                     CHAR(40);      
DEFINE v_f_nacimiento               CHAR(8);  
DEFINE v_entidad_nacimiento         CHAR(2);
DEFINE v_sexo                       CHAR(1);       
DEFINE v_nombre_procanase           CHAR(50); 
DEFINE v_f_registro                 CHAR(8);
DEFINE v_f_marca_infosar            CHAR(8);       
DEFINE v_diag_confronta             CHAR(2);  
DEFINE v_clasifica_separacion       CHAR(1);
DEFINE v_credito_infonavit          CHAR(1);       
DEFINE v_resultado_operacion        CHAR(2);  
DEFINE v_diagnostico1               CHAR(3);
DEFINE v_diagnostico2               CHAR(3);       
DEFINE v_diagnostico3               CHAR(3);  
DEFINE v_traspaso_previo            CHAR(2);
DEFINE v_ind_cambio_clasificacion   CHAR(1);       
DEFINE v_clasifica_separacion_ant   CHAR(1);
DEFINE v_tipo_registro_det03        CHAR(2);  
DEFINE v_contador_registro_det03    CHAR(12); 
DEFINE v_contador_servicio_det03    CHAR(10);
DEFINE v_asociado_det03              CHAR(11); 
DEFINE v_tipo_entidad_asociado_det03 CHAR(2);  
DEFINE v_cve_entidad_asociado_det03  CHAR(3);
DEFINE v_resultado_operacion_det03   CHAR(2);  
DEFINE v_diagnostico1_det03          CHAR(3);  
DEFINE v_diagnostico2_det03          CHAR(3);
DEFINE v_diagnostico3_det03          CHAR(3);

DEFINE v_id_det_02_op27 DECIMAL(9);
DEFINE v_id_derechohabiente_invadido CHAR(11); 
DEFINE v_id_derechohabiente_asociado CHAR(11);
DEFINE v_f_proceso  DATE;
DEFINE v_estado     SMALLINT;
--Detalle 03
DEFINE v_id_det_03_op27       DECIMAL(9,0);
DEFINE v_contador_servicio_03 CHAR(10);
DEFINE v_sum_tipo_registro        CHAR(2);  
DEFINE v_contador_registro_sum    CHAR(12);
DEFINE v_sum_total_registro_det2  CHAR(10); 
DEFINE v_sum_total_registro_det3  CHAR(10);
DEFINE v_sum_total_registros      CHAR(10);
DEFINE v_cadena_encabezado  CHAR(38); 
DEFINE v_cadena_detalle     CHAR(274); 
DEFINE v_cadena_detalle03   CHAR(274);
DEFINE v_cadena_sumario     CHAR(32);
DEFINE v_consulta           VARCHAR(100);   
DEFINE r_cod_rechazo        SMALLINT;     
DEFINE v_var_aux            INTEGER;
DEFINE v_total_procesados   INTEGER;   
DEFINE v_total_marcados     INTEGER;
DEFINE v_estado_destino SMALLINT;    
DEFINE v_ind            SMALLINT;          
DEFINE v_diag           CHAR(3);
DEFINE v_variable       CHAR(2);     
DEFINE v_sql_error      INTEGER;           
DEFINE v_isam_error     SMALLINT;
DEFINE v_msg_error      CHAR(200);   
DEFINE v_folio          DECIMAL(10,0);     
DEFINE r_valida_1e      SMALLINT;
DEFINE v_valida_diag    SMALLINT;
DEFINE v_id_cat_dato    INTEGER;   
DEFINE v_id_his_op27    INTEGER;      
DEFINE v_f_proceso_ant  CHAR(40);
DEFINE v_folio_ant      CHAR(40);   
DEFINE v_referencia     DECIMAL(9,0);   
DEFINE v_hay_rechazos   SMALLINT;

   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_total_procesados = 0; LET v_total_marcados = 0;
      RETURN v_total_procesados,v_total_marcados,v_sql_error,v_msg_error;
   END EXCEPTION WITH RESUME;

--SET DEBUG FILE TO "/tmp/fn_sep_integra_diagnostico_op27.trace";
--TRACE ON;

   LET v_sql_error = 0;
   LET v_msg_error = NULL;

   UPDATE glo_ctr_archivo SET estado = 2, folio = p_folio WHERE nombre_archivo = p_archivo;

   UPDATE glo_folio SET status = 1 WHERE folio = p_folio;

   UPDATE bat_ctr_operacion SET folio = p_folio
    WHERE pid = p_pid AND proceso_cod = p_proceso_cod AND nom_archivo = p_archivo; 
      
   UPDATE bat_ctr_proceso SET folio = p_folio
    WHERE pid = p_pid AND proceso_cod = p_proceso_cod;
      
   -- Almacena Encabezado de OP27
   FOREACH SELECT tipo_registro, id_servicio, id_operacion, tipo_ent_origen,
                  cve_ent_origen, tipo_ent_destino, cve_ent_destino,
                  f_trans_lote, consecutivo_dia, resultado_operacion,
                  diagnostico1, diagnostico2, diagnostico3
             INTO v_cza_tipo_registro, v_cza_id_servicio,
                  v_cza_id_operacion, v_cza_tipo_ent_origen,
                  v_cza_cve_ent_origen, v_cza_tipo_ent_destino,
                  v_cza_cve_ent_destino, v_cza_f_trans_lote,
                  v_cza_consecutivo_cad, v_cza_resultado_operacion,
                  v_cza_diagnostico1, v_cza_diagnostico2,
                  v_cza_diagnostico3 
             FROM safre_tmp:tmp_sep_cza_op27_diag

      LET v_f_proceso = TODAY;      

	  LET v_cza_consecutivo_dia = v_cza_consecutivo_cad;

	  -- Genera el registro de encabezado de archivo de rechazos
	  LET v_cadena_encabezado = v_cza_tipo_registro|| v_cza_id_servicio|| v_cza_id_operacion||v_cza_tipo_ent_origen||v_cza_cve_ent_origen||v_cza_tipo_ent_destino||v_cza_cve_ent_destino||v_cza_f_trans_lote||v_cza_consecutivo_cad||v_cza_resultado_operacion||v_cza_diagnostico1||v_cza_diagnostico2||v_cza_diagnostico3;								
      LET v_cza_f_trans_lote = v_cza_f_trans_lote[5,6]||v_cza_f_trans_lote[7,8]||v_cza_f_trans_lote[1,4];

      INSERT INTO sep_cza_op27(folio, f_proceso, nombre_archivo, tipo_registro, id_servicio,
                               id_operacion, tipo_ent_origen, cve_ent_origen, tipo_ent_destino,
                               cve_ent_destino, f_trans_lote, consecutivo_dia, resultado_operacion,
                               diagnostico1, diagnostico2, diagnostico3)
                        VALUES(p_folio, v_f_proceso, p_archivo, v_cza_tipo_registro, v_cza_id_servicio,
                               v_cza_id_operacion, v_cza_tipo_ent_origen, v_cza_cve_ent_origen,
                               v_cza_tipo_ent_destino, v_cza_cve_ent_destino, v_cza_f_trans_lote,
                               v_cza_consecutivo_dia, v_cza_resultado_operacion,
                               v_cza_diagnostico1, v_cza_diagnostico2, v_cza_diagnostico3);
   END FOREACH

   -- Tabla temporal para almacenar regitros para reporte
   DELETE FROM safre_tmp:tmp_sep_integrados;
   LET v_total_procesados = 0; LET v_total_marcados = 0; LET v_hay_rechazos = 0;
  
   FOREACH SELECT tipo_registro, contador_registro, contador_servicio, invadido, rfc,
                  curp, tipo_entidad_admon, cve_entidad_admon, paterno, materno,
                  nombre, f_nacimiento, entidad_nacimiento, sexo, nombre_procanase,
                  f_registro, f_marca_infosar, diag_confronta, clasifica_separacion, 
                  credito_infonavit, resultado_operacion, diagnostico1, diagnostico2,
                  diagnostico3, traspaso_previo, ind_cambio_clasificacion
             INTO v_tipo_registro, v_contador_registro, v_contador_servicio, v_invadido,
                  v_rfc, v_curp, v_tipo_entidad_admon, v_cve_entidad_admon,
                  v_paterno, v_materno, v_nombre, v_f_nacimiento, v_entidad_nacimiento,
                  v_sexo, v_nombre_procanase, v_f_registro,
                  v_f_marca_infosar, v_diag_confronta, v_clasifica_separacion,
                  v_credito_infonavit, v_resultado_operacion, v_diagnostico1,
                  v_diagnostico2, v_diagnostico3, v_traspaso_previo, v_ind_cambio_clasificacion
             FROM safre_tmp:tmp_sep_det02_op27_diag

      LET v_total_procesados = v_total_procesados + 1;
      -- la búsqueda de registros entre 02 y 03 se realiza por medio del campo contador registro
      LET v_var_aux = v_contador_registro;
      
      -- Recupera el detalle 03 tmp
      SELECT tipo_registro, contador_servicio, contador_registro, asociado,
             tipo_entidad_asociado, cve_entidad_asociado,
             resultado_operacion, diagnostico1,
             diagnostico2, diagnostico3
        INTO v_tipo_registro_det03, v_contador_servicio_det03, v_contador_registro_det03,
             v_asociado_det03, v_tipo_entidad_asociado_det03, v_cve_entidad_asociado_det03,
             v_resultado_operacion_det03, v_diagnostico1_det03,
             v_diagnostico2_det03, v_diagnostico3_det03
        FROM safre_tmp:tmp_sep_det_03_op27_diag
       WHERE contador_registro = v_var_aux + 1; -- contador de registro 02 = contador de registro 02 + 1 en detalle 03
      
      -- se almacena en el tipo de dato final
      LET v_contador_servicio_02 = v_var_aux; LET v_referencia = 0;
      --Llenando cadenas detalle
       LET v_cadena_detalle = v_tipo_registro|| v_contador_servicio|| v_invadido|| v_rfc|| v_curp|| v_tipo_entidad_admon|| v_cve_entidad_admon|| v_paterno|| v_materno|| v_nombre||v_f_nacimiento|| v_entidad_nacimiento|| v_sexo||v_nombre_procanase|| v_f_registro|| v_f_marca_infosar|| v_diag_confronta|| v_clasifica_separacion|| v_credito_infonavit|| v_resultado_operacion|| v_diagnostico1|| v_diagnostico2|| v_diagnostico3|| v_traspaso_previo||v_ind_cambio_clasificacion;
       LET v_cadena_detalle03 = v_tipo_registro_det03|| v_contador_servicio_det03|| v_asociado_det03|| v_tipo_entidad_asociado_det03|| v_cve_entidad_asociado_det03||v_resultado_operacion_det03|| v_diagnostico1_det03|| v_diagnostico2_det03||v_diagnostico3_det03;
       -- verificar la existencia del invadido y asociado
       SELECT id_derechohabiente INTO v_id_derechohabiente_asociado
       FROM afi_derechohabiente WHERE nss = v_asociado_det03;
       SELECT a.id_derechohabiente INTO v_id_derechohabiente_invadido
       FROM   afi_derechohabiente a WHERE  a.nss = v_invadido;
    
       IF DBINFO('SQLCA.SQLERRD2') = 0 THEN 
             -- si no se encuentra invadido, rechazada y se incluye en archivo de no encontrados
             LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF
             -- Registro 02 y 03 rechazado  
	     INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
	     INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);
             CONTINUE FOREACH;
        END IF
        
     -- VALIDACION PREVIA PARA 1E
     IF v_clasifica_separacion = "E" THEN
        EXECUTE PROCEDURE sp_sep_valida_1e(v_id_derechohabiente_invadido,v_invadido,v_asociado_det03,p_folio,v_diag_confronta,v_resultado_operacion,p_usuario) INTO r_valida_1e;
        IF r_valida_1e <> 9 THEN    -- distinto de aceptado se registra rechazo en reporte;
           LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF
             SELECT max(a.id_det_02_op27) INTO   v_id_det_02_op27 FROM sep_det_02_op27 a WHERE a.invadido = v_invadido AND a.id_derechohabiente_invadido = v_id_derechohabiente_invadido ;
             IF DBINFO('SQLCA.SQLERRD2') = 0 THEN LET v_id_det_02_op27 = 0; END IF
                           -- Registro 02 y 03 rechazado
                           INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
                           INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);
                           INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,
                                                                    diag_confronta, clasifica_separacion_ant,
                                                                    clasifica_separacion_nva)
                                                             VALUES(p_folio, v_id_det_02_op27, r_valida_1e, "",
                                                                    v_clasifica_separacion, v_ind_cambio_clasificacion);
                           CONTINUE FOREACH;
       END IF
     END IF     
	 --VALIDACION DIAGNÓSTICO 05 (VD05)
	 IF v_resultado_operacion = "01" AND v_diag_confronta = "05" THEN --- VD05
	    EXECUTE FUNCTION fn_sep_valida_integra_separacion_por_unificacion(p_usuario,
																		  v_id_derechohabiente_invadido,
                                                                          v_asociado_det03,
																		  v_invadido)
		   INTO v_valida_diag;
		   IF v_valida_diag <> 1 THEN
		      SELECT max(a.id_det_02_op27) 
                 INTO   v_id_det_02_op27 
                 FROM sep_det_02_op27 a 
                 WHERE a.invadido = v_invadido 
                 AND a.id_derechohabiente_invadido = v_id_derechohabiente_invadido;
              IF v_id_det_02_op27 IS NULL AND v_valida_diag <> 9 THEN 
                 LET v_id_det_02_op27 = seq_sep_det_02_op27.NEXTVAL + 1;  
              END IF;
			  INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,
                                         diag_confronta, clasifica_separacion_ant,
                                         clasifica_separacion_nva)
                                         VALUES(p_folio, v_id_det_02_op27,v_valida_diag, "05",
                                         v_clasifica_separacion, v_ind_cambio_clasificacion); --Ocurrencia 9 No pertenece a la misma familia
              IF v_valida_diag == 9 THEN
                 LET v_hay_rechazos = v_hay_rechazos + 1;
                 IF v_hay_rechazos = 1 THEN 
			        INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); 
			     END IF;  
                 INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);              
			     INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);
                 CONTINUE FOREACH;
              END IF;
           END IF;		   
	 END IF; --VD05

     SELECT NVL(MAX(a.n_referencia),0)
        INTO v_referencia
        FROM sfr_marca_activa    a ,
             afi_derechohabiente b
       WHERE a.id_derechohabiente = b.id_derechohabiente
         AND b.nss = v_invadido
         AND a.marca = 280; -- cuenta marcada en proceso de separación de cuentas

      IF v_referencia = 0 THEN -- 1
         -- si no esta marcado se verifica que se encuentre rechazada la marca 
         LET v_referencia = 0;
    
         SELECT NVL(MAX(a.n_referencia),0)
           INTO v_referencia 
           FROM sfr_marca_historica  a,
                afi_derechohabiente  b
          WHERE b.nss                = v_invadido 
            AND a.id_derechohabiente = b.id_derechohabiente 
            AND a.marca = 280 -- SEP invadido
            AND a.estado_marca = 20
            AND a.proceso_marca in (2201,2204) -- Operación 27
            AND a.f_fin IS NOT NULL;

          IF v_referencia = 0 THEN  -- 2 si no la en historico
-- sincronizacion del registro no encontrado
               LET v_invadido_verifica = "";
               SELECT unique a.invadido 
               INTO   v_invadido_verifica
               FROM   sep_v_op27 a
               WHERE  a.invadido = v_invadido;
               -- si no se encuentra registro
               IF DBINFO('SQLCA.SQLERRD2') = 0 THEN -- 4
                    -- se inserta nuevo registro de operación 27
                    LET v_id_det_02_op27 = seq_sep_det_02_op27.NEXTVAL;
                    LET v_f_proceso = TODAY;
                    INSERT INTO sep_det_02_op27 VALUES   (v_id_det_02_op27 ,
                           p_folio , v_f_proceso , v_tipo_registro , v_contador_servicio ,
                           v_invadido , v_id_derechohabiente_invadido  , v_rfc , v_curp  , 
                           v_tipo_entidad_admon , v_cve_entidad_admon , v_paterno , v_materno ,
                           v_nombre , mdy(v_f_nacimiento[5,6],v_f_nacimiento[7,8],v_f_nacimiento[1,4]),
                           v_entidad_nacimiento , v_sexo , v_nombre_procanase , mdy(v_f_registro[5,6],v_f_registro[7,8],v_f_registro[1,4]) ,
                           mdy(v_f_marca_infosar[5,6],v_f_marca_infosar[7,8],v_f_marca_infosar[1,4]) , v_diag_confronta ,
                           v_clasifica_separacion , v_credito_infonavit , v_resultado_operacion , v_diagnostico1 ,
                           v_diagnostico2 , v_diagnostico3 , v_traspaso_previo , v_ind_cambio_clasificacion ,
                           ""  , "1" , "0" , "30") ; 
  
                    LET v_id_det_03_op27 = seq_sep_det_03_op27.NEXTVAL; 
                    LET v_id_derechohabiente_asociado = 0;
  
                    SELECT id_derechohabiente
                    INTO v_id_derechohabiente_asociado
                    FROM afi_derechohabiente
                    WHERE nss = v_asociado_det03;
                    DELETE FROM sep_det_03_op27 WHERE id_det_02_op27 = v_id_det_02_op27; 
                    INSERT INTO sep_det_03_op27 VALUES  (v_id_det_03_op27              ,
                                                         v_id_det_02_op27              ,
                                                         p_folio                       ,
                                                         v_tipo_registro_det03         ,
                                                         v_contador_servicio_det03     ,
                                                         v_asociado_det03              ,
                                                         v_id_derechohabiente_asociado ,
                                                         v_tipo_entidad_asociado_det03 ,
                                                         v_cve_entidad_asociado_det03  ,
                                                         v_resultado_operacion_det03   ,
                                                         v_diagnostico1_det03          ,
                                                         v_diagnostico2_det03          ,
                                                         v_diagnostico3_det03          ,
                                                         "")                           ;

                    IF v_resultado_operacion = "01" and (v_diag_confronta = "01" OR v_diag_confronta = "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05") THEN   -- 3 

                     -- marcar cuenta
                       LET v_total_marcados = v_total_marcados + 1   ;
                      IF v_clasifica_separacion <> "E" THEN
                       EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, -- Marca separacion de cuentas asociado
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                             INTO r_cod_rechazo;
                      END IF
                       EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_invadido,
                                                        280, -- Marca separacion de cuentas invadido
                                                        v_id_det_02_op27,
                                                        p_folio,
                                                        0, 
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                       INTO r_cod_rechazo;

                       IF r_cod_rechazo = 0 THEN
                             INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,       --e
                                                                      diag_confronta, clasifica_separacion_ant,
                                                                      clasifica_separacion_nva)
                                                               VALUES(p_folio, v_id_det_02_op27, 4, r_cod_rechazo,
                                                                      v_clasifica_separacion, v_ind_cambio_clasificacion);

                       ELSE
                           -- si no se encuentra rechazada se incluye en archivo de no encontrados
                           LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF  
			               INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
			               INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);

                           UPDATE sep_Det_02_op27  SET ind_marca_safre = 0 , diag_marca = r_cod_rechazo WHERE id_det_02_op27 = v_id_det_02_op27;
               
                           CONTINUE FOREACH;
                       END IF
                    
                     ELSE -- 3 
                       INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,    --e
                                                                diag_confronta, clasifica_separacion_ant,
                                                                clasifica_separacion_nva)
                                   VALUES(p_folio, v_id_det_02_op27, 4, 0,
                                          v_clasifica_separacion, v_ind_cambio_clasificacion);

                     END IF; -- 3

              ELIF v_resultado_operacion = "01" and (v_diag_confronta = "01" OR v_diag_confronta= "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05") THEN --  4 si encuentra el registro en op27

                 LET v_invadido_verifica = "";               

                 SELECT UNIQUE invadido 
                 INTO   v_invadido_verifica
                 FROM   sep_v_op27 
                 WHERE  invadido = v_invadido
                 AND    asociado = v_asociado_det03
                 AND    estado   in (30,35);

                 IF DBINFO('SQLCA.SQLERRD2') = 0 THEN -- 5 si no lo encuentra como procedente

                    -- se inserta nuevo registro de operación 27
                    LET v_id_det_02_op27 = seq_sep_det_02_op27.NEXTVAL;
                    LET v_f_proceso = TODAY;
                    INSERT INTO sep_det_02_op27 VALUES   (v_id_det_02_op27 , p_folio , v_f_proceso , v_tipo_registro , v_contador_servicio , v_invadido , v_id_derechohabiente_invadido  ,
                                                          v_rfc , v_curp , v_tipo_entidad_admon , v_cve_entidad_admon , v_paterno , v_materno , v_nombre , mdy(v_f_nacimiento[5,6],v_f_nacimiento[7,8],v_f_nacimiento[1,4])    ,
                                                          v_entidad_nacimiento , v_sexo , v_nombre_procanase , mdy(v_f_registro[5,6],v_f_registro[7,8],v_f_registro[1,4]) ,
                                                          mdy(v_f_marca_infosar[5,6],v_f_marca_infosar[7,8],v_f_marca_infosar[1,4]) , v_diag_confronta , v_clasifica_separacion , v_credito_infonavit , v_resultado_operacion ,
                                                          v_diagnostico1 , v_diagnostico2 , v_diagnostico3 , v_traspaso_previo , v_ind_cambio_clasificacion , ""  , "1" , "0" , "30" ); 
  
                    LET v_id_det_03_op27 = seq_sep_det_03_op27.NEXTVAL;
                    LET v_id_derechohabiente_asociado = 0;
  
                    SELECT id_derechohabiente
                    INTO v_id_derechohabiente_asociado
                    FROM afi_derechohabiente
                    WHERE nss = v_asociado_det03;
                    DELETE FROM sep_det_03_op27 WHERE id_det_02_op27 = v_id_det_02_op27; 
                    INSERT INTO sep_det_03_op27 VALUES  (v_id_det_03_op27 , v_id_det_02_op27 , p_folio , v_tipo_registro_det03 , v_contador_servicio_det03 ,
                                                         v_asociado_det03 , v_id_derechohabiente_asociado , v_tipo_entidad_asociado_det03 ,
                                                         v_cve_entidad_asociado_det03 , v_resultado_operacion_det03 ,
                                                         v_diagnostico1_det03 , v_diagnostico2_det03 , v_diagnostico3_det03 , "") ; 

                    LET v_total_marcados = v_total_marcados + 1   ;   -- marcar la cuenta
                   IF v_clasifica_separacion <> "E" THEN
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                             INTO r_cod_rechazo;
                   END IF
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_invadido,
                                                     280, 
                                                     v_id_det_02_op27,
                                                     p_folio,
                                                     0,  
                                                     0, 
                                                     NULL, 
                                                     NULL, 
                                                     p_usuario,
                                                     p_proceso_cod)
                      INTO r_cod_rechazo;

                          IF r_cod_rechazo = 0 THEN
                             INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                                               VALUES(p_folio, v_id_det_02_op27, 4, r_cod_rechazo, v_clasifica_separacion, v_ind_cambio_clasificacion);

                          ELSE                               -- si no se encuentra rechazada se incluye en archivo de no encontrados
                              LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF
			                  INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
			                  INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);
   
                              UPDATE sep_Det_02_op27  SET ind_marca_safre = 0 , diag_marca = r_cod_rechazo WHERE id_det_02_op27 = v_id_det_02_op27;
                  
                              CONTINUE FOREACH;
                          END IF

                 ELSE -- 5 Estado 30 - 35 solo se marca y se registra cambio historico

                    SELECT max(id_det_02_op27)
                    INTO  v_id_det_02_op27
                    FROM sep_v_op27 
                    WHERE invadido = v_invadido AND   asociado = v_asociado_det03 AND   estado   IN (30,35); -- ACEPTADO SIN MARCAR

                    SELECT a.id_det_03_op27,a.id_derechohabiente_asociado
                    INTO   v_id_det_03_op27,v_id_derechohabiente_asociado
                    FROM sep_det_03_op27 a
                    WHERE a.id_det_02_op27 = v_id_det_02_op27;

                    LET v_total_marcados = v_total_marcados + 1   ;
                   IF v_clasifica_separacion <> "E" THEN
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702,
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0,
                                                        NULL,
                                                        NULL,
                                                        p_usuario,
                                                        p_proceso_cod)
                       INTO r_cod_rechazo;
                   END IF
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_invadido,
                                                     280, 
                                                     v_id_det_02_op27,
                                                     p_folio,
                                                     0,  
                                                     0, 
                                                     NULL,
                                                     NULL,
                                                     p_usuario,
                                                     p_proceso_cod)
                      INTO r_cod_rechazo;

                       IF r_cod_rechazo = 0 THEN

                             INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,
                                                                      diag_confronta, clasifica_separacion_ant,
                                                                      clasifica_separacion_nva)
                                                               VALUES(p_folio, v_id_det_02_op27, 4, r_cod_rechazo,  --marcado
                                                                      v_clasifica_separacion, "");

                              EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det02_op27_diag', -- Tabla de la que se toma el nuevo valor
                                                                         'sep_det_02_op27',         -- tabla que tiene el valor antiguo
                                                                         p_usuario,                 -- usuario que realiza la integracion
                                                                         'sep_his_det_02_op27',     -- Tabla en la que se registra el historico
                                                                         v_contador_servicio_02,    -- identificador tabla nuevo valor
                                                                         v_id_det_02_op27,          -- identificador tabla antiguo valor
                                                                         --'contador_servicio',       -- campo indentificador tabla nuevo valor
                                                                         'contador_registro',       -- campo indentificador tabla nuevo valor
                                                                         'id_det_02_op27')         -- campo indentificador tabla antiguo valor
                                             INTO v_ind,v_diag,v_sql_error;


                               EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det_03_op27_diag',
                                                                  'sep_det_03_op27',         
                                                                  p_usuario,                 
                                                                  'sep_his_det_03_op27',     
                                                                  v_contador_registro_det03, 
                                                                  v_id_det_03_op27,          
                                                                  --'contador_servicio',       
                                                                  'contador_registro',       
                                                                  'id_det_03_op27')         
                                 INTO v_ind,v_diag,v_sql_error;

                       ELSE -- si no, se encuentra rechazada y se incluye en archivo de no encontrados
                           LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF
			   -- Registro 02 y 03 rechazado  
			   INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
			   INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);

                           UPDATE sep_Det_02_op27  SET ind_marca_safre = 0 , diag_marca = r_cod_rechazo WHERE id_det_02_op27 = v_id_det_02_op27;
               
                           CONTINUE FOREACH;
                       END IF

                END IF;   --- 5    
          END IF; -- 4
          
          -- registrar rechazos basura
          IF NOT (v_resultado_operacion = "01" AND (v_diag_confronta = "01" OR v_diag_confronta = "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05")) THEN  -- 7

             -- si no se encuentra rechazada se incluye en archivo de no encontrados
             LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF  
			 INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
			 INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);

             CONTINUE FOREACH;
           END IF -- 7
          ELSE  -- 2 si encuentra la referencia en el historico de marcas

             LET v_id_det_02_op27 = v_referencia;  -- referencia de la marca historica

             SELECT estado
             INTO v_estado
             FROM sep_det_02_op27
             WHERE id_det_02_op27 = v_referencia;

             IF v_resultado_operacion = "01" and (v_diag_confronta = "01" or v_diag_confronta = "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05") and v_estado <> 15 and v_estado <> 20 THEN 
                    -- marcar la cuenta
                    LET v_total_marcados = v_total_marcados + 1   ;
                   IF v_clasifica_separacion <> "E" THEN
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                             INTO r_cod_rechazo;
                   END IF
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_invadido,
                                                     280, 
                                                     v_id_det_02_op27,
                                                     p_folio,
                                                     0,  
                                                     0, 
                                                     NULL, 
                                                     NULL, 
                                                     p_usuario,
                                                     p_proceso_cod)
                      INTO r_cod_rechazo;

                       IF r_cod_rechazo = 0 THEN
                             INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                                               VALUES(p_folio, v_id_det_02_op27, 4, r_cod_rechazo, v_clasifica_separacion, v_ind_cambio_clasificacion);

                       ELSE
                           -- si no se encuentra rechazada se incluye en archivo de no encontrados
                           LET v_hay_rechazos = v_hay_rechazos + 1; IF v_hay_rechazos = 1 THEN INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado); END IF
			               INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
			               INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle03);

                           UPDATE sep_Det_02_op27  SET ind_marca_safre = 0 , diag_marca = r_cod_rechazo WHERE id_det_02_op27 = v_id_det_02_op27;
               
                           CONTINUE FOREACH;
                       END IF
             END IF
          END IF -- 2
       ELSE  -- 1
          LET v_id_det_02_op27 = v_referencia; -- se asigna la ref de la marca activa al id de det 02
      END IF -- 1

      SELECT estado
        INTO v_estado
        FROM sep_det_02_op27
       WHERE id_det_02_op27 = v_id_det_02_op27;

       IF DBINFO('SQLCA.SQLERRD2') = 0 THEN  -- si no encuentra la solicitud det02 se inserta nuevo registro de operación 27
                    LET v_f_proceso = TODAY;
                    INSERT INTO sep_det_02_op27 VALUES   (v_id_det_02_op27               , p_folio                        , v_f_proceso                    ,
                                                          v_tipo_registro                , v_contador_servicio            , v_invadido                     ,
                                                          v_id_derechohabiente_invadido  , v_rfc                          , v_curp                         ,
                                                          v_tipo_entidad_admon           , v_cve_entidad_admon            , v_paterno                      ,
                                                          v_materno                      , v_nombre                       ,
                                                          mdy(v_f_nacimiento[5,6],v_f_nacimiento[7,8],v_f_nacimiento[1,4])          ,
                                                          v_entidad_nacimiento           , v_sexo                         , v_nombre_procanase             ,
                                                          mdy(v_f_registro[5,6],v_f_registro[7,8],v_f_registro[1,4])                ,
                                                          mdy(v_f_marca_infosar[5,6],v_f_marca_infosar[7,8],v_f_marca_infosar[1,4]) ,
                                                          v_diag_confronta               , v_clasifica_separacion         , v_credito_infonavit            ,
                                                          v_resultado_operacion          , v_diagnostico1                 , v_diagnostico2                 ,
                                                          v_diagnostico3                 , v_traspaso_previo              , v_ind_cambio_clasificacion     ,
                                                          ""                             , 
                                                          "1"                            , 
                                                          "0"                            , 
                                                          "15"                           ); 
  
                    LET v_id_det_03_op27 = seq_sep_det_03_op27.NEXTVAL; 
                    LET v_id_derechohabiente_asociado = 0;
  
                    SELECT id_derechohabiente
                    INTO v_id_derechohabiente_asociado
                    FROM afi_derechohabiente
                    WHERE nss = v_asociado_det03;
                    DELETE FROM sep_det_03_op27 WHERE id_det_02_op27 = v_id_det_02_op27; 
                    INSERT INTO sep_det_03_op27 VALUES  (v_id_det_03_op27              , v_id_det_02_op27              , p_folio                       ,
                                                         v_tipo_registro_det03         , v_contador_servicio_det03     , v_asociado_det03              ,
                                                         v_id_derechohabiente_asociado , v_tipo_entidad_asociado_det03 , v_cve_entidad_asociado_det03  ,
                                                         v_resultado_operacion_det03   , v_diagnostico1_det03          , v_diagnostico2_det03          ,
                                                         v_diagnostico3_det03          , ""); -- id_expediente
                    -- marcar la cuenta
                    LET v_total_marcados = v_total_marcados + 1   ;
                   IF v_clasifica_separacion <> "E" THEN
                    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                    INTO r_cod_rechazo;                                     
                   END IF
       ELIF v_resultado_operacion = "01" and (v_diag_confronta = "01" OR v_diag_confronta = "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05") and v_estado <> 15 and v_estado <> 20 THEN   --  4 si encuentra el registro en op27 THEN

               EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det02_op27_diag', 
                                                          'sep_det_02_op27',         
                                                          p_usuario,                 
                                                          'sep_his_det_02_op27',     
                                                          v_contador_servicio_02,    
                                                          v_id_det_02_op27,          
                                                          --'contador_servicio',       
                                                          'contador_registro',       
                                                          'id_det_02_op27')         
                              INTO v_ind,v_diag,v_sql_error;

                    SELECT a.id_det_03_op27,a.id_derechohabiente_asociado
                    INTO   v_id_det_03_op27,v_id_derechohabiente_asociado
                    FROM sep_det_03_op27 a
                    WHERE a.id_det_02_op27 = v_id_det_02_op27 ;

               EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det_03_op27_diag', 
                                                          'sep_det_03_op27',         
                                                          p_usuario,                 
                                                          'sep_his_det_03_op27',     
                                                          v_contador_registro_det03, 
                                                          v_id_det_03_op27,          
                                                          --'contador_servicio',     
                                                          'contador_registro',       
                                                          'id_det_03_op27')         
                             INTO v_ind,v_diag,v_sql_error;
                                   -- marcar la cuenta
                LET v_total_marcados = v_total_marcados + 1   ;
               IF v_clasifica_separacion <> "E" THEN
                EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                    INTO r_cod_rechazo; 
                END IF
      END IF

      IF( v_id_det_02_op27 IS NOT NULL )THEN  -- guarda el valor anterior para el reporte
         IF v_ind_cambio_clasificacion = "1" THEN
            LET v_clasifica_separacion_ant = " ";
            SELECT a.clasifica_separacion 
              INTO v_clasifica_separacion_ant
              FROM sep_det_02_op27 a
             WHERE a.id_det_02_op27 = v_id_det_02_op27;
         END IF

         EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det02_op27_diag', 
                                                    'sep_det_02_op27',         
                                                    p_usuario,                 
                                                    'sep_his_det_02_op27',     
                                                    v_contador_servicio_02,    
                                                    v_id_det_02_op27,          
                                                    --'contador_servicio',       
                                                    'contador_registro',       
                                                    'id_det_02_op27')        
                        INTO v_ind,v_diag,v_sql_error;

         LET v_f_proceso = TODAY;        -- se rescata folio anterior
         SELECT a.folio     ,
                a.f_proceso 
           INTO v_folio_ant ,
                v_f_proceso_ant   
           FROM sep_det_02_op27 a
          WHERE id_det_02_op27 = v_id_det_02_op27;
         
         -- Se actualizan registros de la tabla sep_det_02_op27
         UPDATE sep_det_02_op27
            SET folio = p_folio,
                f_proceso = v_f_proceso,
                id_derechohabiente_invadido = v_id_derechohabiente_invadido
          WHERE id_det_02_op27 = v_id_det_02_op27;
 
         SELECT NVL(MAX(a.id_cat_dato_actualizado),0)
           INTO v_id_cat_dato
           FROM sep_cat_dato_actualizado a ,
                sep_cat_entidad_historico b
          WHERE a.cve_natural = "folio"
            AND a.id_cat_entidad_historico = b.id_cat_entidad_historico
            AND b.entidad_cod = "sep_his_det_02_op27";

         SELECT MAX(id_his_op27 + 1)
           INTO v_id_his_op27 
           FROM sep_his_det_02_op27 ;

         LET v_folio_ant = '';

         IF v_id_cat_dato <> 0 THEN             -- insertar historico de folio y fecha proceso
            INSERT INTO sep_his_det_02_op27 VALUES (v_id_his_op27    , v_id_det_02_op27 , v_id_cat_dato    , v_f_proceso        , 
                                                    v_folio_ant      , p_folio          , p_usuario        ) ;
         END IF

         SELECT NVL(MAX(a.id_cat_dato_actualizado),0)
           INTO v_id_cat_dato 
           FROM sep_cat_dato_actualizado a ,
                sep_cat_entidad_historico b
          WHERE a.cve_natural = "f_proceso"
            AND a.id_cat_entidad_historico = b.id_cat_entidad_historico
            AND b.entidad_cod = "sep_his_det_02_op27";

         SELECT MAX(id_his_op27 + 1)
           INTO v_id_his_op27 
           FROM sep_his_det_02_op27;

         IF v_id_cat_dato <> 0 THEN
            INSERT INTO sep_his_det_02_op27 VALUES (v_id_his_op27    , v_id_det_02_op27 , v_id_cat_dato    , v_f_proceso      ,
                                                    v_f_proceso_ant  , v_f_proceso      , p_usuario        );
         END IF
         -- REGISTRAR DATOS PARA REPORTE -- identifica si es cambio de diagnostico
         IF(v_ind_cambio_clasificacion = '1')THEN
           IF v_clasifica_separacion = "E" THEN LET v_clasifica_separacion_ant = ""; END IF
            INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,
                                                     diag_confronta, clasifica_separacion_ant,
                                                     clasifica_separacion_nva)
            VALUES(p_folio, v_id_det_02_op27, 1, v_diag_confronta, v_clasifica_separacion_ant,
                   v_clasifica_separacion);
            -- 1 = cambio de diagnóstico

         END IF
         -- inicializa el estado
         LET v_estado_destino = v_estado;
         
         -- no se usa ELSE, No se tiene conocimiento de otro resultado
         IF(v_resultado_operacion = "01")THEN
            
            IF(v_diag_confronta = "01" OR v_diag_confronta = "03" OR v_diag_confronta = "04" OR v_diag_confronta = "05") THEN

               IF(v_estado = 15)THEN -- marca 27 integrada
                  -- Recupera el estado al que se va actualizar
                  EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',25,15)
                             INTO v_ind,
                                  v_diag,
                                  v_estado_destino;
               END IF

               IF(v_estado = 20)THEN -- marca 27 rechazada
                  -- Recupera el estado al que se va actualizar
                  EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',25,20)
                             INTO v_ind,
                                  v_diag,
                                  v_estado_destino;
               END IF
            END IF
            
            IF(v_diag_confronta = "02")THEN

               IF(v_estado = 15)THEN -- marca 27 integrada
                  
                  -- Desmarca cuenta por diagnóstico improcedente
                  EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_asociado,
                                                                702, 
                                                                v_id_det_03_op27,
                                                                40, 
                                                                702, 
                                                                p_usuario,
                                                                p_proceso_cod)
                                   INTO r_cod_rechazo;
                  EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_invadido,
                                                                280, -- Marca separacion de cuentas invadido
                                                                v_id_det_02_op27,
                                                                40, -- Desmarca cuenta por diagnóstico improcedente
                                                                280, -- Marca separacion de cuentas invadido
                                                                p_usuario,
                                                                p_proceso_cod)
                                   INTO r_cod_rechazo;
                  -- Recupera el estado al que se va actualizar
                  EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',20,15)
                            INTO v_ind,
                                 v_diag,
                                 v_estado_destino;

                  INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia,
                                                           diag_confronta, clasifica_separacion_ant,
                                                           clasifica_separacion_nva)
                                       VALUES(p_folio, v_id_det_02_op27, 3, v_diag_confronta,
                                              v_clasifica_separacion, v_ind_cambio_clasificacion);
                  -- 3 = desmarcado por diagnóstico improcedente

               END IF

               IF(v_estado = 20)THEN -- marca 27 rechazada
                  -- Desmarca cuenta por diagnóstico improcedente
                  EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_asociado,
                                                                702,
                                                                v_id_det_03_op27,
                                                                40, 
                                                                702,
                                                                p_usuario,
                                                                p_proceso_cod)
                                   INTO r_cod_rechazo;
                  EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_invadido,
                                                                280,
                                                                v_id_det_02_op27,
                                                                40, 
                                                                280,
                                                                p_usuario,
                                                                p_proceso_cod)
                                  INTO r_cod_rechazo;

                  -- Recupera el estado al que se va actualizar
                  EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',20,20)
                              INTO v_ind,
                                   v_diag,
                                   v_estado_destino;

                  INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                       VALUES(p_folio, v_id_det_02_op27, 3, v_diag_confronta, v_clasifica_separacion, v_ind_cambio_clasificacion);
                  -- 3 = desmarcado por diagnóstico improcedente
               END IF
            END IF
         END IF

         IF(v_resultado_operacion = "02") AND (v_diag_confronta = "02") THEN

            IF(v_estado = 15)THEN -- marca 27 integrada
               -- Desmarca cuenta por rechazo procesar
               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_asociado,
                                                                702,
                                                                v_id_det_03_op27,
                                                                40,
                                                                702,
                                                                p_usuario,
                                                                p_proceso_cod)
                                   INTO r_cod_rechazo;
               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_invadido,
                                                   280, 
                                                   v_id_det_02_op27,
                                                   30, 
                                                   280,
                                                   p_usuario,
                                                   p_proceso_cod)
               INTO r_cod_rechazo;

               -- Recupera el estado al que se va actualizar
               EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',15,15)
                          INTO v_ind, v_diag, v_estado_destino;

               INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                    VALUES(p_folio, v_id_det_02_op27, 2, v_diag_confronta, v_clasifica_separacion, v_ind_cambio_clasificacion);
               -- 2 = desmarcado por rechazo procesar

            END IF

            IF(v_estado = 20)THEN -- marca 27 rechazada
               -- Desmarca cuenta por rechazo procesar
               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_asociado,
                                                                702, 
                                                                v_id_det_03_op27,
                                                                40, 
                                                                702,
                                                                p_usuario,
                                                                p_proceso_cod)
                                   INTO r_cod_rechazo;
               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_invadido,
                                                             280, 
                                                             v_id_det_02_op27,
                                                             30, 
                                                             280,
                                                             p_usuario,
                                                             p_proceso_cod)
                          INTO r_cod_rechazo;

               -- Recupera el estado al que se va actualizar
               EXECUTE FUNCTION fn_maquinaria('maq_sep_op27',15,20)
                          INTO v_ind, v_diag, v_estado_destino;
               INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                    VALUES(p_folio, v_id_det_02_op27, 2, v_diag_confronta, v_clasifica_separacion, v_ind_cambio_clasificacion);
               -- 2 = desmarcado por rechazo procesar

            END IF
         -- se modifica para no desmarcar cuando diagnostico distinto de 02
         ELIF (v_resultado_operacion = '02') AND (v_diag_confronta <> '02') THEN 
               INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                    VALUES(p_folio, v_id_det_02_op27, 2, v_diag_confronta, v_clasifica_separacion, v_ind_cambio_clasificacion);
         END IF

         -- Actualiza el estado de op27 según la maquinaria
         UPDATE sep_det_02_op27
            SET estado = v_estado_destino
          WHERE id_det_02_op27 = v_id_det_02_op27;

         LET v_variable = NULL;
         -- se busca si la cuenta esta en proceso de separacion         
         SELECT 'si'
           INTO v_variable
           FROM sfr_marca_activa
          WHERE id_derechohabiente = v_id_derechohabiente_invadido
            AND marca = 280;

         -- si no existe el registro con Marca de separacion de cuentas se marca la cuenta en separacion de cuentas
         IF(v_variable IS NULL AND v_diag_confronta = "01" AND v_resultado_operacion = "01" )THEN

            LET v_total_marcados = v_total_marcados + 1;
           IF v_clasifica_separacion <> "E" THEN
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                       INTO r_cod_rechazo;
            END IF
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_invadido,
                                                       280, 
                                                       v_id_det_02_op27,
                                                       p_folio,
                                                       0,  
                                                       0, 
                                                       NULL,
                                                       NULL,
                                                       p_usuario,
                                                       p_proceso_cod)
            INTO r_cod_rechazo;
            IF r_cod_rechazo = 0 THEN
               INSERT INTO safre_tmp:tmp_sep_integrados(folio, id_det_02_op27, tipo_ocurrencia, diag_confronta, clasifica_separacion_ant, clasifica_separacion_nva)
                                    VALUES(p_folio, v_id_det_02_op27, 4, r_cod_rechazo, v_clasifica_separacion, v_ind_cambio_clasificacion);
            -- 4 = vuletas a marcar
            END IF
         END IF
         LET v_id_det_03_op27 = NULL;
         -- datos para insertar en detalle 03
         SELECT id_det_03_op27, 
                contador_servicio
           INTO v_id_det_03_op27,
                v_contador_servicio_03
           FROM sep_det_03_op27
          WHERE id_det_02_op27 = v_id_det_02_op27;

         -- Verificar si existe la pareja de sep_det_02_op27 en sep_det_03_op27
         IF( v_id_det_03_op27 IS NOT NULL )THEN
            EXECUTE PROCEDURE sp_sep_registra_historia('tmp_sep_det_03_op27_diag', 
                                                       'sep_det_03_op27',         
                                                       p_usuario,                 
                                                       'sep_his_det_03_op27',     
                                                       v_contador_servicio_03,       
                                                       v_id_det_03_op27,          
                                                       --'contador_servicio',       
                                                       'contador_registro',       
                                                       'id_det_03_op27')         
                      INTO v_ind,v_diag,v_sql_error;
         ELSE

            SELECT id_derechohabiente
              INTO v_id_derechohabiente_asociado
              FROM afi_derechohabiente
             WHERE nss = v_asociado_det03;
            
             -- En el caso de que no exista el registro en sep_det_02_op27, se registra a partir de la tabla temporal y se marca la cuenta
             DELETE FROM sep_det_03_op27 WHERE id_det_02_op27 = v_id_det_02_op27;
             LET v_id_det_03_op27 = safre_viv:seq_sep_det_03_op27.NEXTVAL;
             INSERT INTO sep_det_03_op27(id_det_03_op27, id_det_02_op27, folio, tipo_registro, contador_servicio, asociado, id_derechohabiente_asociado, tipo_entidad_asociado,
                                         cve_entidad_asociado, resultado_operacion, diagnostico1, diagnostico2, diagnostico3)
                                  VALUES(v_id_det_03_op27, v_id_det_02_op27, p_folio, v_tipo_registro_det03,
                                         v_contador_servicio_det03, v_asociado_det03, v_id_derechohabiente_asociado, v_tipo_entidad_asociado_det03,
                                         v_cve_entidad_asociado_det03, v_resultado_operacion_det03, v_diagnostico1_det03, v_diagnostico2_det03, v_diagnostico3_det03);
             LET v_total_marcados = v_total_marcados + 1   ;
           IF v_clasifica_separacion <> "E" THEN
             EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_asociado,
                                                        702, 
                                                        v_id_det_03_op27,
                                                        p_folio,
                                                        0,  
                                                        0, 
                                                        NULL, 
                                                        NULL, 
                                                        p_usuario,
                                                        p_proceso_cod)
                INTO r_cod_rechazo; 
           END IF
         END IF
      END IF
   END FOREACH
   
   FOREACH SELECT tipo_registro, contador_registro, total_registro_det2, total_registro_det3, total_registros
             INTO v_sum_tipo_registro, v_contador_registro_sum, v_sum_total_registro_det2, v_sum_total_registro_det3, v_sum_total_registros
             FROM safre_tmp:tmp_sep_sum_op27_diag
      INSERT INTO sep_sum_op27(folio, tipo_registro, total_registro_det2, total_registro_det3, total_registros)
                       VALUES (p_folio, v_sum_tipo_registro, v_sum_total_registro_det2, v_sum_total_registro_det3, v_sum_total_registros);
   END FOREACH

   IF v_hay_rechazos >= 1 THEN
      --SYSTEM "sh /safreviv_int/sep/rescate/.sepUbicaNEdop27 "||"0"||" "||"0"||" "||p_archivo||"9";

	  LET v_sum_total_registro_det2 = v_hay_rechazos;
	  LET v_sum_total_registro_det3 = v_hay_rechazos;
	  LET v_sum_total_registros = v_hay_rechazos + v_hay_rechazos;
	  LET v_cadena_sumario = v_sum_tipo_registro|| v_sum_total_registro_det2|| v_sum_total_registro_det3|| v_sum_total_registros;
	  
	  INSERT INTO tmp_sep_rechazados VALUES(v_cadena_sumario);
   END IF
   UPDATE STATISTICS FOR TABLE sep_cza_op27; UPDATE STATISTICS FOR TABLE sep_det_02_op27; UPDATE STATISTICS FOR TABLE sep_det_03_op27;
   UPDATE STATISTICS FOR TABLE sep_sum_op27;
   RETURN v_total_procesados, v_total_marcados, v_sql_error, v_msg_error;
END FUNCTION;


