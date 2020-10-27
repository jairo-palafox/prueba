






CREATE FUNCTION "safreviv".fn_sep_r_integrar_marca_op27( p_folio DECIMAL(9,0)  , -- folio a reversar
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
DEFINE v_nombre_archivo         CHAR(40)     ;
DEFINE v_id_derechohabiente     DECIMAL(9,0) ;
DEFINE v_f_proceso              DATE;
DEFINE v_estado                 SMALLINT      ;



   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error

      RETURN v_ind        , 
             v_diag       ,
             v_sql_error  ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;

   END EXCEPTION --WITH RESUME

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_r_integrar_marca_op27.trace';

   LET v_ind              =     0 ;
   LET v_diag             = "000" ;
   LET v_sql_error        =     0 ;
   LET v_total_reversados = 0     ;
   LET v_nombre_archivo   = ''    ;
   LET v_isam_error       = 0     ;
   LET v_msg_error        = ""    ;


-- se elimina la historia de la integracion en sep_his_det_02_op27

   SELECT NVL(COUNT(*),0) 
     INTO v_total_reversados 
     FROM sep_det_02_op27 
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
   
   -- recupera la fecha del proceso del folio, para eliminar los registros que procesó det03 por medio de la fecha y
   -- que ademas folio esta en null
   -- folio es null ya que rev03 lo actualizó asi
   SELECT f_proceso
     INTO v_f_proceso
     FROM sep_cza_op27
    WHERE folio = p_folio;
   
    
   FOREACH SELECT id_det_02_op27,
                  id_derechohabiente_invadido ,
                  estado
             INTO v_id_det_02_op27,
                  v_id_derechohabiente,
                  v_estado  
             FROM sep_det_02_op27 
            WHERE folio = p_folio
               OR (folio IS NULL AND f_proceso = v_f_proceso) -- los filtros consideran tanto los que procesó y no proceso det03
      -- para marca op 27, se eliminan las marcas de los nss
      IF v_estado = 15  THEN -- integrado
      EXECUTE PROCEDURE sp_reversa_marca(v_id_derechohabiente,
                                         280,            -- marca entra
                                         v_id_det_02_op27, -- n_referencia
                                         p_folio)      ; -- folio
      ELSE 

          DELETE FROM sfr_marca_historica
          WHERE  id_derechohabiente = v_id_derechohabiente 
          AND    marca  = 280
          AND    n_referencia = v_id_det_02_op27
          AND    folio = p_folio;
      END IF
                                         
      -- elimina los registros de det03                               
      DELETE
        FROM sep_det_03_op27
       WHERE id_det_02_op27 = v_id_det_02_op27;
      -- elimina la historia de las relacionadas al registro de det02 op 27
      DELETE 
        FROM sep_his_det_02_op27
       WHERE id_det_02_op27 = v_id_det_02_op27;
   END FOREACH;
   
   -- elimina los registros de det02 que fueron procesados por det03
   DELETE
     FROM sep_det_02_op27
    WHERE folio IS NULL 
      AND f_proceso = v_f_proceso;
   
   
   -- se elimina la integracion con funcion general de reverso de integracion
   EXECUTE FUNCTION fn_reversa_integracion(p_folio,2201) INTO v_sql_error ; 
   
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
   EXECUTE FUNCTION fn_reversa_operacion(p_pid ,2201, 2) INTO v_ind;

   -- si ocurre error al reversar el proceso bat
   IF(v_ind <> 0)THEN
      LET v_diag = "004" ; -- error al actualizar operacion
      LET v_msg_error = "";
   
      RETURN v_ind       ,
             v_diag      ,
             v_sql_error ,
             v_isam_error ,
             v_msg_error  ,
             v_total_reversados ;
   END IF

  -- se actualiza el control del archivo a 1 cargado
  UPDATE glo_ctr_archivo 
     SET estado         = 1  
   WHERE folio          = p_folio 
     AND nombre_archivo = v_nombre_archivo;
     
  UPDATE STATISTICS FOR TABLE sep_cza_op27;
  UPDATE STATISTICS FOR TABLE sep_sum_op27;
  UPDATE STATISTICS FOR TABLE sep_his_det_02_op27;
  UPDATE STATISTICS FOR TABLE sep_det_02_op27;

RETURN v_ind         ,
       v_diag        ,
       v_sql_error   ,
       v_isam_error ,
       v_msg_error  ,
       v_total_reversados ;

END FUNCTION;


