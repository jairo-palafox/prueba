






CREATE FUNCTION "safreviv".fn_genera_tramite(  p_cod_proceso_bus    CHAR(3),
                                    p_cod_opera_bus      CHAR (4),
                                    p_folio_procesar     CHAR(50),
                                    p_ind_origen         SMALLINT
                                 )
RETURNING DECIMAL(9,0), DECIMAL(9,0);

   DEFINE v_id_bus_tramite          DECIMAL(9,0);

   --Variables para el manejo de los catalogos
   DEFINE v_id_cat_bus_proceso      DECIMAL(9,0);
   DEFINE v_id_cat_bus_operacion    DECIMAL(9,0);
   DEFINE v_id_cat_bus_contrato     DECIMAL(9,0);

   --Primero se valida que el proceso y operacion esten dados de alta en el sistema
   SELECT   proc.id_cat_bus_proceso,
            opera.id_cat_bus_operacion,
            contr.id_cat_bus_contrato
   INTO     v_id_cat_bus_proceso,
            v_id_cat_bus_operacion,
            v_id_cat_bus_contrato
   FROM cat_bus_proceso proc
   INNER JOIN cat_bus_operacion opera ON opera.id_cat_bus_proceso = proc.id_cat_bus_proceso
   INNER JOIN cat_bus_contrato contr ON contr.id_cat_bus_operacion = opera.id_cat_bus_operacion
   WHERE TO_NUMBER(proc.cod_proceso_bus) = TO_NUMBER(p_cod_proceso_bus)
   AND TO_NUMBER(opera.cod_opera_bus) = TO_NUMBER(p_cod_opera_bus)
   AND contr.ind_vigencia = 1;

   IF (v_id_cat_bus_contrato IS NULL ) THEN
      LET v_id_bus_tramite = -1;
      LET v_id_cat_bus_contrato = -1;
   ELSE
      --Se se encuentra el identificador del contrato significa que es un tramite valido
      INSERT INTO bus_tramite VALUES(seq_bus_tramite.NEXTVAL,
                                     v_id_cat_bus_contrato,
                                     p_folio_procesar,
                                     p_ind_origen);
                                     
      SELECT seq_bus_tramite.CURRVAL
      INTO v_id_bus_tramite
      FROM cat_bus_proceso
      WHERE id_cat_bus_proceso = v_id_cat_bus_proceso ;
   END IF

   RETURN v_id_bus_tramite, v_id_cat_bus_contrato;
END FUNCTION;


