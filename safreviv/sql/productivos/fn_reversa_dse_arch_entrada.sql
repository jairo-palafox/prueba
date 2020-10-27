






CREATE FUNCTION "safreviv".fn_reversa_dse_arch_entrada(p_d_folio DECIMAL(9,0),
                                            p_c_op_arch_ent VARCHAR(5))
   RETURNING SMALLINT;
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_id_derechohabiente    DECIMAL(9,0);
   DEFINE v_sql_error             SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO 'dbg_reverso_dse_archEnt.trace';

   -- se asume que no ocurrirá ningún error
   LET v_sql_error = 0;
   
   -- se verifica si el proceso a reversar se trata de Devolución de Saldos (opt1)
   IF p_c_op_arch_ent = "opt1" THEN
      -- se eliminan los registros de devolución
      DELETE FROM safre_viv:dse_devolucion
       WHERE folio_referencia = p_d_folio;
   END IF;
   
   -- se verifica si el proceso a reversar se trata de Recepción Rechazo Devolución (opt2) o
   -- Recepción Confirmación Devolución (opt3)
   IF p_c_op_arch_ent = "opt2" OR p_c_op_arch_ent = "opt3" THEN
      FOREACH
         -- se procesan los registros de his decolución para el folio dado
         SELECT UNIQUE id_dse_grp_devolucion
           INTO v_id_dse_grp_devolucion
           FROM safre_viv:dse_his_devolucion
          WHERE folio = p_d_folio

         -- se actualiza el edo_procesar de agrupa devolucion
         UPDATE safre_viv:dse_agrupa_devolucion
            SET edo_procesar = 80
          WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion;
      END FOREACH;

      -- eliminan los registros de his devolucion en proceso
      DELETE
        FROM safre_viv:dse_his_devolucion
       WHERE folio = p_d_folio;
   END IF

   -- se verifica si el proceso a reversar se trata de Fondo De Ahorro 72 (opt8)
   IF p_c_op_arch_ent = "opt8" THEN
      -- se eliminan los registros de fondo de ahorro
      DELETE FROM safre_viv:dse_restitucion_fondo72
       WHERE folio = p_d_folio;
   END IF;

   -- se actualiza el estado del registro de control de archivos a 70-Reversado
   DELETE
     FROM safre_viv:dse_ctr_archivo
    WHERE folio = p_d_folio;

   RETURN v_sql_error;
END FUNCTION
;


