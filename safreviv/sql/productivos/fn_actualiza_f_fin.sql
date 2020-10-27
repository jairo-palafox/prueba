






CREATE FUNCTION "safreviv".fn_actualiza_f_fin()

   RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE v_f_inicio           DATE;
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_n_referencia       DECIMAL(9,0);

DEFINE sql_err              INTEGER  ;
DEFINE v_resultado          INTEGER  ;
DEFINE isam_err             INTEGER  ;
DEFINE err_txt              CHAR(200);
DEFINE v_msj                CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      
      RETURN v_resultado, isam_err, err_txt;
   END EXCEPTION

LET v_f_inicio           = "";
LET v_id_derechohabiente = 0 ;
LET v_n_referencia       = 0 ; 
LET v_resultado          = 0 ;
LET isam_err             = 0 ;
LET v_msj                = "Actualización Exitosa";

   FOREACH   
      SELECT f_inicio,
             id_derechohabiente, 
             n_referencia
      INTO   v_f_inicio,
             v_id_derechohabiente,
             v_n_referencia
      FROM   sfr_marca_activa
      WHERE  marca = 150
      AND    id_derechohabiente IN (SELECT id_derechohabiente
                                    FROM   sfr_marca_historica
                                    WHERE  marca = 502
                                    AND    f_fin IS NULL)
 
      UPDATE sfr_marca_historica
      SET    f_fin              = v_f_inicio
      WHERE  marca              = 502
      AND    id_derechohabiente = v_id_derechohabiente
      AND    n_referencia       = v_n_referencia   
      ;
   END FOREACH

RETURN v_resultado, isam_err, v_msj;

END FUNCTION ;


