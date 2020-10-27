






CREATE FUNCTION "safreviv".fn_mdt_rev_val_recurrente(p_folio SMALLINT, p_f_proceso  DATE)
RETURNING SMALLINT,SMALLINT,INTEGER,INTEGER,INTEGER,INTEGER,INTEGER;

DEFINE v_ind_rev   SMALLINT;
DEFINE v1          SMALLINT;
DEFINE v2          SMALLINT;
DEFINE v_valida    SMALLINT;
DEFINE v_pid       SMALLINT;
DEFINE v_proceso_cod SMALLINT;
DEFINE v_opera_cod   SMALLINT;
DEFINE v_nom_archivo CHAR(200);
DEFINE v_diag      SMALLINT;
DEFINE v_tot_rev_a INTEGER;
DEFINE v_tot_rev_m INTEGER;
DEFINE v_tot_rev_r INTEGER;
DEFINE v_tot_rev_b INTEGER;
DEFINE v_tot_rev   INTEGER;
DEFINE v_estado    SMALLINT;
DEFINE v_conteo    INTEGER;
DEFINE v_consulta  CHAR(1024);
DEFINE v_id_det_ctr_mandato DECIMAL(9,0);
DEFINE v_valor_modificado   CHAR(40);
DEFINE v_cve_natural        CHAR(40);
DEFINE v_id_his_mandato     DECIMAL(9,0);
DEFINE v_sql_error          SMALLINT;

   ON EXCEPTION SET v_sql_error
      LET v_tot_rev_a = 0;
      LET v_tot_rev_m = 0;
      LET v_tot_rev_r = 0;
      LET v_tot_rev_b = 0;
      LET v_tot_rev   = 0;
      LET v_ind_rev = 1;
      LET v_diag    = 4; -- error en consulta
      --TRACE 'SQL ERROR: '||v_sql_error;
      RETURN v_ind_rev, v_diag, v_tot_rev_a, v_tot_rev_m, v_tot_rev_r, v_tot_rev_b, v_tot_rev;
   END EXCEPTION WITH RESUME
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_rev_val_recurrente.trace';
   
   LET v_tot_rev_a = 0;
   LET v_tot_rev_m = 0;
   LET v_tot_rev_r = 0;
   LET v_tot_rev_b = 0;
   LET v_tot_rev   = 0;
   
    --Validacion 1   Se valida que el estado para lote y f_lote en mdt_lote_mandato sea 103
   --TRACE 'Se valida que el estado para lote y f_lote en mdt_lote_mandato sea 103';
   SELECT FIRST 1 estado
     INTO v_estado
     FROM safre_viv:mdt_lote_mandato
    --WHERE lote = p_lote
    --AND f_procesof_mdt = p_f_lote;
    WHERE folio = p_folio;
      --AND f_proceso = p_f_proceso;
      
   IF (v_estado <> 103 AND v_estado <> 102 AND v_estado <> 101)  THEN
      LET v_ind_rev = 1;
      LET v_diag    = 1;
      RETURN v_ind_rev, v_diag, v_tot_rev_a, v_tot_rev_m, v_tot_rev_r, v_tot_rev_b, v_tot_rev;
   END IF;
   
   -- Validacion 2   Se verifica que el pid corresponda a al proceso_cod 46 y tenga estado FINALIZADO
   -- YA NO SE CONSIDERA ESTA TAREA, POR CAUSA DEL PID   
   
   --TRACE 'Se verifica que no exista un registro en mdt_ctr_aplica_mandato con una fecha igual o mayor a la fecha del reverso';
   -- Validacion 3   Se verifica que no exista un registro en mdt_ctr_aplica_mandato con una fecha igual o mayor a la fecha del reverso
   LET v_conteo = 0;
   SELECT FIRST 1 NVL(1,0)
     INTO v_conteo
     FROM safre_viv:mdt_ctr_aplica_mandato
    WHERE f_liquida_pago >= p_f_proceso;
   IF(v_conteo = 1)THEN
      LET v_ind_rev = 1;
      LET v_diag    = 3;
      RETURN v_ind_rev, v_diag, v_tot_rev_a, v_tot_rev_m, v_tot_rev_r, v_tot_rev_b, v_tot_rev;
   END IF
   --TRACE 'Reverso de altas aceptadas   ';
   
   -- 1 Reverso de altas aceptadas   
   --TRACE 'elimina registro de mdt_det_ctr_mandato';
   -- a. elimina registro de mdt_det_ctr_mandato
   FOREACH SELECT his.id_det_ctr_mandato
             INTO v_id_det_ctr_mandato
             FROM safre_viv:mdt_solicitud_mandato sol JOIN safre_viv:mdt_his_mandato his
               ON his.id_solicitud_mandato = sol.id_solicitud_mandato
                  JOIN safre_viv:mdt_cat_dato_actualizado dto
               ON dto.id_cat_dato_actualizado = his.id_cat_dato_actualizado
            WHERE sol.folio = p_folio
              --AND sol.f_lote = p_f_lote
              AND sol.id_origen = 1  -- origen recurrente
              AND sol.estado = 102 -- Aceptadas
              AND sol.tipo_operacion = 'A' -- altas
              
              
      DELETE
        FROM safre_viv:mdt_det_ctr_mandato
       WHERE id_det_ctr_mandato = v_id_det_ctr_mandato;

      DELETE 
        FROM safre_viv:mdt_his_mandato
       WHERE id_det_ctr_mandato = v_id_det_ctr_mandato;
              
      LET v_tot_rev_a = v_tot_rev_a + 1;
   END FOREACH
   --TRACE 'verificar si quedaron regisrtos en mdt_det_ctr_mandato.';
   -- b. verificar si quedaron regisrtos en mdt_det_ctr_mandato.
   LET v_conteo = 0;
   SELECT COUNT(det.id_det_ctr_mandato)
     INTO v_conteo
     FROM safre_viv:mdt_det_ctr_mandato det JOIN safre_viv:mdt_ctr_mandato ctr
       ON det.id_ctr_mandato = ctr.id_ctr_mandato
    WHERE ctr.lote = p_folio;
      --AND ctr.f_lote = p_f_lote;
   --TRACE 'si quedaron no quedaron registros en mdt_det_ctr_mandato para el lote y f_lote, se elimina el registro de mdt_ctr_mandato';
   -- c. si quedaron no quedaron registros en mdt_det_ctr_mandato para el lote y f_lote, se elimina el registro de 
   -- mdt_ctr_mandato
   IF(v_conteo = 0)THEN
      DELETE 
        FROM safre_viv:mdt_ctr_mandato
       WHERE lote = p_folio;
         --AND f_lote = p_f_lote;
   END IF
   
   -- 2	Reverso de modificaciones, reactivaciones, bajas
   --TRACE 'Reverso de modificaciones, reactivaciones, bajas';
   --TRACE 'PROCESA MODIFICACIÓNES';
   -- a. Recuperar los registros históricos registrados en mdt_his_mandato y actualizar la tabla mdt_det_ctr_mandato 
   FOREACH SELECT his.id_det_ctr_mandato, his.id_mdt_his_mandato, his.valor_modificado, dto.cve_natural
             INTO v_id_det_ctr_mandato, v_id_his_mandato, v_valor_modificado, v_cve_natural
             FROM safre_viv:mdt_solicitud_mandato sol JOIN safre_viv:mdt_his_mandato his
               ON his.id_solicitud_mandato = sol.id_solicitud_mandato
                  JOIN safre_viv:mdt_cat_dato_actualizado dto
               ON dto.id_cat_dato_actualizado = his.id_cat_dato_actualizado
            WHERE sol.folio = p_folio
              --AND sol.f_lote = p_f_lote
              AND sol.id_origen = 1 
              AND sol.estado = 102  
              AND sol.tipo_operacion = 'M'
      
      LET v_consulta = "UPDATE safre_viv:mdt_det_ctr_mandato"||
                       "   SET "||TRIM(v_cve_natural)||" = '"||TRIM(v_valor_modificado)||"'"||
                       " WHERE id_det_ctr_mandato = "||v_id_det_ctr_mandato||";";
      EXECUTE IMMEDIATE v_consulta;
      IF(SQLCODE = 0)THEN      
         -- c. Una vez realizado el reverso eliminar el registro histórico de la tabla mdt_his_mandato
         DELETE 
           FROM safre_viv:mdt_his_mandato 
          WHERE id_mdt_his_mandato = v_id_his_mandato;
         LET v_tot_rev_m = v_tot_rev_m + 1;
      END IF
   END FOREACH;
   
   --TRACE 'PROCESA REACTIVACIÓN';
   -- b. Realizar el reverso de modificaciones, reactivaciones y bajas en ese mismo órden
   FOREACH SELECT his.id_det_ctr_mandato, his.id_mdt_his_mandato, his.valor_modificado, dto.cve_natural
             INTO v_id_det_ctr_mandato, v_id_his_mandato, v_valor_modificado, v_cve_natural
             FROM safre_viv:mdt_solicitud_mandato sol JOIN safre_viv:mdt_his_mandato his
               ON his.id_solicitud_mandato = sol.id_solicitud_mandato
                  JOIN safre_viv:mdt_cat_dato_actualizado dto
               ON dto.id_cat_dato_actualizado = his.id_cat_dato_actualizado
            WHERE sol.folio = p_folio
              --AND sol.f_lote = p_f_lote
              AND sol.id_origen = 1  -- origen recurrente
              AND sol.estado = 102 -- Aceptadas
              AND sol.tipo_operacion = 'R' -- Reactivacion
      
      LET v_consulta = "UPDATE safre_viv:mdt_det_ctr_mandato"||
                       "   SET "||TRIM(v_cve_natural)||" = '"||TRIM(v_valor_modificado)||"'"||
                       " WHERE id_det_ctr_mandato = "||v_id_det_ctr_mandato||";";
      EXECUTE IMMEDIATE v_consulta;
      IF(SQLCODE = 0)THEN
         -- c. Una vez realizado el reverso eliminar el registro histórico de la tabla mdt_his_mandato
         DELETE 
           FROM safre_viv:mdt_his_mandato 
          WHERE id_mdt_his_mandato = v_id_his_mandato;
         LET v_tot_rev_r = v_tot_rev_r + 1;
      END IF
   END FOREACH;   
   --TRACE 'PROCESA BAJAS';
   FOREACH SELECT his.id_det_ctr_mandato, his.id_mdt_his_mandato, his.valor_modificado, dto.cve_natural
             INTO v_id_det_ctr_mandato, v_id_his_mandato, v_valor_modificado, v_cve_natural
             FROM safre_viv:mdt_solicitud_mandato sol JOIN safre_viv:mdt_his_mandato his
               ON his.id_solicitud_mandato = sol.id_solicitud_mandato
                  JOIN safre_viv:mdt_cat_dato_actualizado dto
               ON dto.id_cat_dato_actualizado = his.id_cat_dato_actualizado
            WHERE sol.folio = p_folio
              --AND sol.f_lote = p_f_lote
              AND sol.id_origen = 1  -- origen recurrente
              AND sol.estado = 102 -- Aceptadas
              AND sol.tipo_operacion = 'B' -- Bajas
      
      LET v_consulta = "UPDATE safre_viv:mdt_det_ctr_mandato"||
                       "   SET "||TRIM(v_cve_natural)||" = '"||TRIM(v_valor_modificado)||"',"||
                       "       estado = 103"||
                       " WHERE id_det_ctr_mandato = "||v_id_det_ctr_mandato||";";
      EXECUTE IMMEDIATE v_consulta;
      IF(SQLCODE = 0)THEN
         -- c. Una vez realizado el reverso eliminar el registro histórico de la tabla mdt_his_mandato
         DELETE 
           FROM safre_viv:mdt_his_mandato 
          WHERE id_mdt_his_mandato = v_id_his_mandato;
         LET v_tot_rev_b = v_tot_rev_b + 1;
      END IF
   END FOREACH;
   
   -- 3 Actualizar el estado y diagnóstico de la tabla mdt_solicitud_mandato: estado = 101 y diagnóstico = “000” para los registros de lote y f_lote
   --TRACE 'Actualizar el estado y diagnóstico de la tabla mdt_solicitud_mandato: estado = 101 y diagnóstico = “000” para los registros de lote y f_lote';
   UPDATE safre_viv:mdt_solicitud_mandato
      SET estado = 101,
          diagnostico = '000'
    WHERE folio = p_folio;
      --AND f_lote = p_f_lote;
   --TRACE 'Actualizar estado del registro en mdt_lote_mandato a 102 para el lote, f_lote';
   -- 4 Actualizar estado del registro en mdt_lote_mandato a 102 para el lote, f_lote
   UPDATE safre_viv:mdt_lote_mandato
      SET estado = 101 -- se modifica de 102 por independiencia de rec acred.
    WHERE folio = p_folio;
      --AND f_lote = p_f_lote;
   --TRACE 'FINALIZA REVERSO DE VALIDACIÓN DE RECURRENTES';
   -- 5 Eliminar el registro del monitor de proceso para el identificador de proceso recibido (pid) eliminar registros de bat_ctr_operacion y bat_ctr_proceso
   -- YA NO SE CONSIDERA ESTA TAREA, POR CAUSA DEL PID
  
     --EXECUTE PROCEDURE fn_mdt_rev_inserta_inst(p_folio) 
     --INTO    v1,v2;

     SELECT a.pid         ,
            a.proceso_cod ,
            a.opera_cod   , 
            a.nom_archivo 
     INTO   v_pid         ,
            v_proceso_cod ,
            v_opera_cod   ,
            v_nom_archivo
     FROM   bat_ctr_operacion a
     WHERE  a.folio = p_folio
     AND    a.proceso_cod = 1303 -- validacion de solicitudes
     AND    a.opera_cod = 3 ; -- validacion de solicitudes


     EXECUTE FUNCTION fn_reversa_operacion(v_pid, v_proceso_cod, 4) -- rechazos
     INTO    v_valida;

     EXECUTE FUNCTION fn_reversa_operacion(v_pid, v_proceso_cod, 3) -- validacion
     INTO    v_valida;

   --DELETE FROM glo_ctr_archivo 
   --WHERE  nombre_archivo = v_nom_archivo 
   --AND    proceso_Cod = 1303 
   --AND    opera_cod   = 1;  -- se elimina la carga del archivo

   LET v_ind_rev = 0;
   LET v_diag    = 0;
   IF v_tot_rev_m > 0 THEN
      LET v_tot_rev_m = v_tot_rev_m / 2; -- por el estado no cuenta como rev
   END IF
   LET v_tot_rev = v_tot_rev_a + v_tot_rev_m + v_tot_rev_r + v_tot_rev_b;
   RETURN v_ind_rev, v_diag, v_tot_rev_a, v_tot_rev_m, v_tot_rev_r, v_tot_rev_b, v_tot_rev;
END FUNCTION
;


