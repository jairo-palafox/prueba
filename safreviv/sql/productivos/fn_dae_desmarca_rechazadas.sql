






CREATE FUNCTION "safreviv".fn_dae_desmarca_rechazadas()
   RETURNING SMALLINT, 
             INTEGER, 
             CHAR(200),
             INTEGER

DEFINE v_id_dae_ref_ajuste  DECIMAL(9,0);
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_si_resultado       SMALLINT;
DEFINE v_usuario_cod        CHAR(20);
DEFINE v_proceso_cod        SMALLINT;
-- Control de Excepciones
DEFINE sql_err              INTEGER;
DEFINE isam_err             INTEGER;
DEFINE err_txt              CHAR(200);
DEFINE v_c_msj              CHAR(200);
DEFINE v_tot_desmarca       INTEGER;

    ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt, v_tot_desmarca;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/safreviv_int/BD/desmarca_rechazadas.txt";
 --TRACE ON;
 
 LET v_si_resultado = 0;
 LET isam_err       = 0;
 LET v_tot_desmarca = 0;
 LET v_usuario_cod = "safreviv";
 LET v_proceso_cod = 2403;

 FOREACH
   SELECT a.id_derechohabiente, a.id_dae_ref_ajuste
   INTO   v_id_derechohabiente,
          v_id_dae_ref_ajuste   
   FROM   dae_det_ajuste a
   INNER JOIN sfr_marca_activa b
   ON     a.id_dae_ref_ajuste = b.n_referencia
   WHERE  a.folio_lote = b.folio
   AND    a.resul_operacion = 2
   AND    b.marca = 403

   EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                        403,
                                        v_id_dae_ref_ajuste,
                                        30,
                                        403,
                                        v_usuario_cod,
                                        v_proceso_cod)
   INTO v_si_resultado;

   LET v_tot_desmarca = v_tot_desmarca + 1 ;
       
 END FOREACH;

 LET v_c_msj = 'Desmarca finalizada exitosamente';
 
 RETURN v_si_resultado, 
        isam_err, 
        v_c_msj,
        v_tot_desmarca;
END FUNCTION
;


