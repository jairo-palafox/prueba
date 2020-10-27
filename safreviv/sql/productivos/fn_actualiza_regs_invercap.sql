






CREATE FUNCTION "safreviv".fn_actualiza_regs_invercap()

   RETURNING SMALLINT, INTEGER, CHAR(50)

DEFINE sql_err                         INTEGER  ;
DEFINE v_resultado                     INTEGER  ;
DEFINE isam_err                        INTEGER  ;
DEFINE err_txt                         CHAR(200);
DEFINE v_msj                           CHAR(200);
DEFINE v_tmp_unificador                CHAR(11);
DEFINE v_tmp_unificado                 CHAR(11);
DEFINE v_total_regs                    INTEGER;
DEFINE v_id_pre_unificador             DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_resultado = sql_err;
      
      RETURN v_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_actualiza_invercap.txt";
   --TRACE ON;

LET v_resultado                      = 0 ;
LET isam_err                         = 0 ;
LET v_msj                            = "Actualización Exitosa";
LET v_tmp_unificador                 = "";
LET v_tmp_unificado                  = "";
LET v_total_regs                     = 0;
LET v_id_pre_unificador              = 0;
LET v_id_derechohabiente_unificador  = 0;
LET v_id_derechohabiente_unificado   = 0;

   FOREACH   
      SELECT unificador,                   
             unificado                     
      INTO   v_tmp_unificador,             
             v_tmp_unificado               
      FROM   safre_tmp:tmp_reg_invercap_uni
               
      FOREACH                                        
         SELECT id_pre_unificador,
                id_derechohabiente
         INTO   v_id_pre_unificador,
                v_id_derechohabiente_unificador
         FROM   uni_pre_unificador
         WHERE  nss_correcto = v_tmp_unificador
      
         DELETE
         FROM   uni_pre_unificado
         WHERE  id_pre_unificador = v_id_pre_unificador
         ;
         
         SELECT id_derechohabiente 
         INTO   v_id_derechohabiente_unificado
         FROM   afi_derechohabiente 
         WHERE  nss = v_tmp_unificado
         ;
         
         DELETE
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente_unificador
         AND    marca = 501
         ;
         
         DELETE
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente_unificado
         AND    marca = 502
         ;

      END FOREACH
      
      DELETE
      FROM   uni_pre_unificador
      WHERE  nss_correcto = v_tmp_unificador;
      
   END FOREACH;

RETURN v_resultado, isam_err, v_msj;

END FUNCTION 
;


