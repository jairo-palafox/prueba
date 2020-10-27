






CREATE FUNCTION "safreviv".fn_desmarca_masiva_posible_unificacion()
RETURNING INTEGER,
          INTEGER,
          CHAR(100);

DEFINE v_proceso_cod         SMALLINT;
DEFINE v_marca               SMALLINT;
DEFINE v_referencia          DECIMAL(9,0);
DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_res_desmarca        SMALLINT;
DEFINE v_sql_error           INTEGER;
DEFINE v_isam_error          INTEGER;
DEFINE v_msg_error           CHAR(100);

ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
   RETURN v_sql_error  ,
          v_isam_error ,
          v_msg_error  ;
END EXCEPTION ;

LET v_proceso_cod = 2318;
LET v_sql_error   = 0;
LET v_isam_error  = 0;
LET v_msg_error   = "Desmarca Exitosa";

   FOREACH 
   --   SELECT b.nss,
   --          a.id_derechohabiente, 
   --          a.marca, 
   --          a.f_inicio
   --   FROM sfr_marca_activa a
   --   INNER JOIN afi_derechohabiente b
   --   ON a.id_derechohabiente = b.id_derechohabiente
   --   WHERE a.marca IN (511, 512)
   --   AND a.f_inicio <= "12/31/2014"
   --   ORDER BY f_inicio
   
      SELECT marca,
             n_referencia,
             id_derechohabiente
      INTO   v_marca,
             v_referencia,
             v_id_derechohabiente
      FROM   sfr_marca_activa 
      WHERE  marca IN (511, 512)
      AND    f_inicio <= "12/31/2014"

      EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                           v_marca,
                                           v_referencia,
                                           30,
                                           v_marca,
                                           "SAFREVIV",
                                           v_proceso_cod)
      INTO v_res_desmarca;
   END FOREACH
   
   RETURN v_sql_error  ,
          v_isam_error ,
          v_msg_error  ;             
END FUNCTION;


