






CREATE FUNCTION "safreviv".fn_actualiza_f_fin_unificador()

   RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE v_f_inicio           DATE;
DEFINE v_id_dh_unificado    DECIMAL(9,0);
DEFINE v_id_dh_unificador   DECIMAL(9,0);
DEFINE v_id_unificador      DECIMAL(9,0);
DEFINE v_n_referencia_dor   DECIMAL(9,0);
DEFINE v_n_referencia_ado   DECIMAL(9,0);

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
LET v_id_dh_unificado    = 0 ;
LET v_id_dh_unificador   = 0 ;
LET v_n_referencia_ado   = 0 ; 
LET v_n_referencia_dor   = 0 ; 
LET v_resultado          = 0 ;
LET isam_err             = 0 ;
LET v_msj                = "Actualización Exitosa";

   FOREACH   
      --Recupera UNIFICADOS que ya se INHABILITARON
      SELECT id_derechohabiente,
             n_referencia,
             f_inicio
      INTO   v_id_dh_unificado,
             v_n_referencia_ado,
             v_f_inicio
      FROM   sfr_marca_activa
      WHERE  marca = 150

      FOREACH
         --Recupera valores del UNIFICADOR
         SELECT a.id_unificador, 
                b.id_derechohabiente
         INTO   v_id_unificador,
                v_id_dh_unificador
         FROM   uni_det_unificado a,
                uni_det_unificador b
         WHERE  id_unificado = v_n_referencia_ado
         AND    a.id_unificador = b.id_unificador
         GROUP  BY 1,2

         UPDATE sfr_marca_historica
         SET    f_fin              = v_f_inicio
         WHERE  id_derechohabiente = v_id_dh_unificador
         AND    marca              = 501
         AND    n_referencia       = v_id_unificador;
      END FOREACH
   END FOREACH

RETURN v_resultado, isam_err, v_msj;

END FUNCTION 




;


