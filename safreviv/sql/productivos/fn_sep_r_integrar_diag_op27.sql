






CREATE FUNCTION "safreviv".fn_sep_r_integrar_diag_op27( p_folio DECIMAL(9,0)  , -- folio a reversar
                                             p_pid   INTEGER       ) -- del que se va a reversar
                       
RETURNING SMALLINT  , -- v_ind 
          CHAR(003) , -- v_diag 
          INTEGER   , -- sql_error
          INTEGER   , --isam_error
          CHAR(100) , --msg_error
          INTEGER   ; -- total reverdados

-- variables de control
DEFINE v_ind                    SMALLINT   ; 
DEFINE v_diag                   CHAR(003)  ;
DEFINE v_sql_error              INTEGER    ; 
DEFINE v_isam_error             INTEGER    ;
DEFINE v_msg_error              CHAR(100)  ;
DEFINE v_total_reversados       INTEGER    ;

-- variables de proceso
DEFINE v_id_det_02_op27         DECIMAL(9,0) ;
DEFINE v_id_det_03_op27         DECIMAL(9,0) ;
DEFINE v_nombre_archivo         CHAR(40)     ;
DEFINE v_id_derechohabiente     DECIMAL(9,0) ;
DEFINE v_id_derechohabiente_aso DECIMAL(9,0) ;
DEFINE v_clasifica_separacion   CHAR(1);
DEFINE v_ind_cambio_clasificacion CHAR(1);
DEFINE v_contador_servicio      DECIMAL(10,0) ;
DEFINE v_cve_natural            CHAR(40);
DEFINE v_valor_modificado       CHAR(40);
DEFINE v_consulta               CHAR(1000);
DEFINE v_f_proceso              DATE;
DEFINE v_id_his                 DECIMAL(9,0);
DEFINE v_hay_his_03             INTEGER;
DEFINE v_estado_destino         SMALLINT;
DEFINE v_ind_maq                SMALLINT   ; 
DEFINE v_diag_maq               CHAR(003)  ;
DEFINE v_diag_confronta         CHAR(2);
--Variables para marca 150 Separacion por Unificacion
DEFINE v_m_n_referencia         DECIMAL(9,0);
DEFINE v_m_f_inicio             DATE;
DEFINE v_m_h_inicio             DATETIME HOUR TO SECOND; 
DEFINE v_m_folio                DECIMAL(9,0);
DEFINE v_m_marca_causa          SMALLINT;
DEFINE v_m_f_marca_causa        DATE;
DEFINE v_m_usuario_marca        CHAR(20);
DEFINE v_m_proceso_marca        SMALLINT;
DEFINE v_m_f_vigencia           DATE;
DEFINE v_m_marca_03             SMALLINT;
DEFINE v_m_marca_02             SMALLINT;

   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_r_integrar_diag_op27.trace';
   --SET DEBUG FILE TO '/safreviv_int/BD/fn_sep_r_integrar_diag_op27.trace';
   --TRACE ON;
   LET v_ind              =     0 ;
   LET v_diag             = "000" ;
   LET v_sql_error        =     0 ;
   LET v_total_reversados = 0     ;
   LET v_nombre_archivo   = ''    ;
   LET v_isam_error       = 0     ;
   LET v_msg_error        = ""    ;

-- se elimina la historia de la integracion en sep_his_det_02_op28

   SELECT NVL(COUNT(*),0) 
     INTO v_total_reversados 
     FROM sep_det_03_op27 
    WHERE folio = p_folio ;
   
   SELECT NVL(a.nombre_archivo,"x")
     INTO v_nombre_archivo 
     FROM sep_cza_op27 a
    WHERE folio = p_folio ;

   IF(v_nombre_archivo  = "x")THEN
      LET v_ind  = 1     ;
      LET v_diag = "002" ; -- nombre de archivo no encontrado
      --LET v_msg_error = "nombre de archivo no encontrado";
      
      RETURN v_ind       ,
             v_diag      ,
             v_sql_error ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;
   END IF
   
   FOREACH SELECT id_det_02_op27               ,
                  id_derechohabiente_invadido  ,
                  contador_servicio            ,
                  f_proceso,
                  clasifica_separacion,
                  ind_cambio_clasificacion,
                  diag_confronta
             INTO v_id_det_02_op27,
                  v_id_derechohabiente,
                  v_contador_servicio ,
                  v_f_proceso,
                  v_clasifica_separacion,
                  v_ind_cambio_clasificacion,
                  v_diag_confronta
             FROM sep_det_02_op27 
            WHERE folio = p_folio 
            
      -- recupera id det 03 con id det 02 y la marca 
      SELECT a.id_det_03_op27,
         a.id_derechohabiente_asociado,
         b.marca
         INTO v_id_det_03_op27,
              v_id_derechohabiente_aso,
              v_m_marca_03
         FROM sep_det_03_op27 a, sfr_marca_activa b
         WHERE a.id_det_02_op27 = v_id_det_02_op27
         AND a.id_derechohabiente_asociado = b.id_derechohabiente
         AND b.n_referencia = a.id_det_03_op27
         AND b.marca = 702;

      --VALIDANDO SI EL FOLIO A REVERSAR FUE QUIEN MARCO LA CUENTA
      SELECT 
         COUNT(*)
         INTO v_m_marca_02
         FROM sfr_marca_activa
         WHERE id_derechohabiente = v_id_derechohabiente
         AND marca = 280
         AND folio = p_folio; 
      IF v_m_marca_02 = 0 THEN  
         EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                            280,            -- marca entra
                                            v_id_det_02_op27, -- n_referencia
                                            p_folio)      ; -- folio
      ELSE
         EXECUTE PROCEDURE sp_reversa_marca(v_id_derechohabiente,
                                         280,            -- marca entra
                                         v_id_det_02_op27, -- n_referencia
                                         p_folio)      ; -- folio
         --SI EL FOLIO A REVERSAR MARCO LA CUENTA TAMBIEN AÑADE NUEVO REGISTRO A SEP_DET_02_OP27 por lo que se elimina
         DELETE 
            FROM sep_det_02_op27
            WHERE id_derechohabiente_invadido = v_id_derechohabiente
            AND folio = p_folio;
            
      END IF
	  -- se elimina la historia de la integracion en sep_his_det_02_op28
      FOREACH SELECT his.id_his_op27, cat.cve_natural, his.valor_modificado
                INTO v_id_his, v_cve_natural, v_valor_modificado
                FROM sep_det_02_op27 op27 JOIN sep_his_det_02_op27 his
                  ON his.id_det_02_op27 = op27.id_det_02_op27
                     JOIN sep_cat_dato_actualizado cat
                  ON cat.id_cat_dato_actualizado = his.id_cat_dato_actualizado
               WHERE op27.id_det_02_op27 = v_id_det_02_op27
               AND   his.f_modificacion  = v_f_proceso
               
         LET v_consulta = " UPDATE sep_det_02_op27"||
                          "    SET "||TRIM(v_cve_natural)||" = '"||v_valor_modificado||"'"||
                          "  WHERE id_det_02_op27 = "||v_id_det_02_op27;

         EXECUTE IMMEDIATE v_consulta;

         DELETE FROM sep_his_det_02_op27 WHERE id_his_op27 = v_id_his; 

      END FOREACH

      --REALIZA REVERSO DESMARCA O REVERSO DE MARCA SEGUN SEA EL CASO PARA DETALLE 03 CON MARCA 702
      IF v_m_marca_03 <> 702 THEN --Si existe marca es por que la marco, si no existe la desmarcó por diag improcedente
         EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente_aso,
                                            702,            -- marca entra
                                            v_id_det_03_op27, -- n_referencia
                                            p_folio)      ; -- folio
      ELSE 
         EXECUTE PROCEDURE sp_reversa_marca(v_id_derechohabiente_aso,
                                         702,            -- marca entra
                                         v_id_det_03_op27, -- n_referencia
                                         p_folio)      ; -- folio
      END IF
      
      -- Elimina la cuenta virtual para los clasificados E y sus marcas
      IF( v_clasifica_separacion = 'E')THEN
         -- Reversa las maca de la cuenta virtual
         EXECUTE PROCEDURE sp_reversa_marca(v_id_derechohabiente,
                                            160,            -- marca entra
                                            v_id_det_02_op27, -- n_referencia
                                            p_folio)      ;
         EXECUTE PROCEDURE sp_reversa_marca(v_id_derechohabiente,
                                            161,            -- marca entra
                                            v_id_det_02_op27, -- n_referencia
                                            p_folio)      ;
         -- Elimina la cuenta virtual
         DELETE 
           FROM afi_derechohabiente
          WHERE id_derechohabiente = v_id_derechohabiente_aso;
          
      END IF
      -- elimina la historia de las relacionadas al registro de diag op 27
      -- verifica si hay historia de cambios en det 03 op 27
      SELECT NVL(COUNT(*),0) 
      INTO   v_hay_his_03
      FROM   sep_his_det_03_op27 
      WHERE  id_det_03_op27 = v_id_det_03_op27;
      
      IF v_hay_his_03 = 0 THEN

         -- si no hay cambios borra el registro de det 03 op 27
         DELETE FROM sep_det_03_op27 
         WHERE  id_det_02_op27 = v_id_det_02_op27; 

      ELSE
         -- deshace historia de det 03 op 27   
         FOREACH SELECT his.id_his_03_op27, cat.cve_natural, his.valor_modificado
                   INTO v_id_his, v_cve_natural, v_valor_modificado
                   FROM sep_det_03_op27 op27 JOIN sep_his_det_03_op27 his
                     ON his.id_his_det_03_op27 = op27.id_det_03_op27
                        JOIN sep_cat_dato_actualizado cat
                     ON cat.id_cat_dato_actualizado = his.id_cat_dato_actualizado
                  WHERE op27.id_det_03_op27 = v_id_det_03_op27
                  AND   his.f_modificacion  = v_f_proceso
   
   
                  LET v_consulta = " UPDATE sep_det_03_op27"||
                                   "    SET "||TRIM(v_cve_natural)||" = '"||v_valor_modificado||"'"||
                                   "  WHERE id_det_03_op27 = "||v_id_det_03_op27;
         
                  EXECUTE IMMEDIATE v_consulta;
         
                  DELETE FROM sep_his_det_03_op27 WHERE id_his_03_op27 = v_id_his; 
         
         END FOREACH
      END IF

     --SI EN SEPARACION POR UNIFICACION SE REHABILITÓ UNA CUENTA, SE DESHABILITARA DE NUEVO (VD05)
     IF v_diag_confronta = "05" THEN
        --Obtener Marca 150 del historico de marcas
        SELECT n_referencia,
               folio,
               f_inicio,
               h_inicio,
               marca_causa,
               f_marca_causa,
               usuario_marca,
               proceso_marca,
               f_vigencia
           INTO v_m_n_referencia,
                v_m_folio,
                v_m_f_inicio,
                v_m_h_inicio,
                v_m_marca_causa,
                v_m_f_marca_causa,
                v_m_usuario_marca,
                v_m_proceso_marca,
                v_m_f_vigencia
           FROM sfr_marca_historica
           WHERE id_derechohabiente = v_id_derechohabiente_aso
           AND marca = 150
           AND proceso_desmarca = 2204;

        IF DBINFO('SQLCA.SQLERRD2') = 1 THEN
           --Si se encuentra en el historico, se elimina la marca reactivacion 110 del historial
           DELETE FROM sfr_marca_historica
              WHERE id_derechohabiente = v_id_derechohabiente_aso
              AND n_referencia = v_m_n_referencia
              AND marca = 110
              AND proceso_marca = 2204;
           --Posteriormente se pasa a marcar la cuenta nuevamente con 150, del historico de marcas
           --Se inserta directamente para evitar dejar datos en el historial
           INSERT INTO sfr_marca_activa VALUES(v_id_derechohabiente_aso,
                                               150,
                                               v_m_n_referencia,
                                               v_m_f_inicio,
                                               v_m_h_inicio,
                                               v_m_folio,
                                               v_m_proceso_marca,
                                               v_m_marca_causa,
                                               v_m_f_marca_causa,
                                               v_m_f_vigencia,
                                               v_m_usuario_marca);
           --AHORA SE PROCEDE A ACTUALIZAR LA MARCA HISTORICA PARA ELIMINAR f_fin y proceso desmarca
           UPDATE sfr_marca_historica
              SET f_fin = "",
                  proceso_desmarca = ""
              WHERE id_derechohabiente = v_id_derechohabiente_aso
              AND marca = 150
              AND proceso_desmarca = 2204
              AND n_referencia = v_m_n_referencia
              AND f_marca_causa = v_m_f_marca_causa;

        END IF
     END IF --(VD05)
      
      -- Si tuvo cambio de clasificación no reversa el estado del diagnóstico, continúa como integrado
      -- ind_cambio_clasificacion = 1 --> cambió de clasificación
      IF( v_ind_cambio_clasificacion = '1')THEN
         EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_op27",v_id_det_02_op27,"id_det_02_op27",70,"REVERSO")
             INTO v_ind_maq, v_diag_maq, v_estado_destino;
      END IF
      
   END FOREACH;
   
   -- se elimina la integracion con funcion general de reverso de integracion
   EXECUTE FUNCTION fn_reversa_integracion(p_folio,2204) INTO v_sql_error ; 
   
   IF(v_sql_error <> 0)THEN 
   
      LET v_ind  = -1    ;   
      LET v_diag = "003" ; -- error en funcion general de rev de integracion
      LET v_msg_error = "error en funcion general de reverso de integracion";
   
      RETURN v_ind       ,
             v_diag      ,
             v_sql_error ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;
   END IF

   -- se reversa el proceso bat para operacion 27 integrar
   EXECUTE FUNCTION fn_reversa_operacion(p_pid ,2204, 2) INTO v_ind;
   -- si ocurre error al reversar el proceso bat
   --IF(v_ind <> 0)THEN
      --LET v_diag = "004" ; -- error al actualizar operacion
      --LET v_msg_error = "";
  -- 
      --RETURN v_ind       ,
             --v_diag      ,
             ----v_sql_error ,
             --v_isam_error ,
             --v_msg_error  ,
             --v_total_reversados ;
   --END IF

  -- se actualiza el control del archivo a 1 cargado
  UPDATE glo_ctr_archivo 
     SET estado         = 1  
   WHERE folio          = p_folio 
     AND nombre_archivo = v_nombre_archivo;
     
  UPDATE STATISTICS FOR TABLE sep_cza_op27;
  UPDATE STATISTICS FOR TABLE sep_sum_op27;
  UPDATE STATISTICS FOR TABLE sep_his_det_03_op27;
  UPDATE STATISTICS FOR TABLE sep_det_03_op27;

RETURN v_ind         ,
       v_diag        ,
       v_sql_error   ,
       v_isam_error ,
       v_msg_error  ,
       v_total_reversados ;

END FUNCTION;


