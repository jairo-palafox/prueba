






CREATE FUNCTION "safreviv".fn_dae_ajusta_cuenta_individual()
RETURNING INTEGER,  
          INTEGER,  
          CHAR(200);

DEFINE v_secuencia_ajuste    DECIMAL(9,0);
DEFINE v_secuencia_aceptados DECIMAL(9,0);
DEFINE v_bnd_desmarca        SMALLINT;
-- Control de Excepciones                 
DEFINE sql_err               INTEGER;  
DEFINE isam_err              INTEGER;  
DEFINE err_txt               CHAR(200);

ON EXCEPTION SET sql_err, isam_err, err_txt

   RETURN sql_err,
          isam_err, 
          err_txt;
END EXCEPTION

LET v_secuencia_ajuste    = 0;
LET v_secuencia_aceptados = 0;
LET v_bnd_desmarca        = 0;
LET sql_err               = 0;
LET isam_err              = 0;
LET err_txt               = "Ajuste existoso";

   SELECT seq_dae_det_ajuste.NEXTVAL
   INTO   v_secuencia_ajuste
   FROM systables
   WHERE tabid = 1;

   SELECT seq_dae_aceptados_ajuste.NEXTVAL
   INTO   v_secuencia_aceptados
   FROM   systables
   WHERE  tabid = 1;

   EXECUTE FUNCTION fn_desmarca_cuenta( 35887235,
                                        403,
                                        14046,
                                        0,
                                        0,
                                        "SAFREVIV",
                                        2403)
   INTO v_bnd_desmarca
   ;
   UPDATE dae_det_solicitud
   SET    status_retiro = 2,
          folio_ajuste  = 44961
   WHERE  id_dae_referencia  = 2304325
   AND    id_derechohabiente = 35887235
   AND    folio              = 37305
   ;
   INSERT INTO dae_det_ajuste 
        VALUES (
                v_secuencia_ajuste,
                2304325,
                35887235,
                "01977834348",
                44961,
                1,
                13
                )
   ;
   INSERT INTO dae_aceptados_ajuste
   VALUES (
           v_secuencia_aceptados,
           v_secuencia_ajuste,
           2304325,
           44961,
           TODAY,
           294410.350000,
           149684.702090,
           44961,
           "04/27/2015"
           )
   ;
   INSERT INTO cta_movimiento
   VALUES (TODAY
           ,35887235
           ,46
           ,11
           ,521
           ,44961
           ,2304325
           ,-149684.702090
           ,-294410.35
           ,"04/27/2015"
           ,TODAY
           ,CURRENT HOUR TO SECOND 
           ,"ROP-1308-C"
           )
   ;
   
   RETURN sql_err,
          isam_err, 
          err_txt;

END FUNCTION;


