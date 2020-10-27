






CREATE PROCEDURE "safreviv".sp_cnt_transaccion2(p_folio_cnt DECIMAL(9,0),
                                                                         p_folio_liquida DECIMAL(9,0),
                                                                         p_f_liquida DATE ,
                                     p_nombre_arch CHAR(25),
                                     p_f_emision CHAR(8),
                                     p_id_funcion SMALLINT)

DEFINE v_num_poliza      DECIMAL(9,0);
DEFINE v_f_respuesta     DATE;
DEFINE v_folio_reversado DECIMAL(9,0);
DEFINE v_f_reverso       DATE;
DEFINE v_estado          SMALLINT;
DEFINE v_ejercicio       CHAR(4);

LET v_num_poliza     = NULL;
LET v_f_respuesta    = NULL;
LET v_folio_reversado= NULL;
LET v_f_reverso      = NULL;
LET v_ejercicio = NULL;

   --Identifica el proceso que invoca la transaccion
   IF p_id_funcion = 1 THEN
      LET v_estado         = 20;
   ELSE
      LET v_estado         = 70;
   END IF

   --Se insertan datos en la tabla de control
   INSERT INTO safre_viv:cnt_ctr_proceso
        VALUES (p_folio_cnt, p_folio_liquida, p_f_liquida, p_nombre_arch, p_f_emision,
                v_num_poliza, v_ejercicio, v_f_respuesta, v_folio_reversado,
                v_f_reverso, v_estado);

END PROCEDURE;


