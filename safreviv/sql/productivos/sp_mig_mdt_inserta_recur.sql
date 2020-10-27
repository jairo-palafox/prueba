






CREATE FUNCTION "safreviv".sp_mig_mdt_inserta_recur(
                               p_folio DECIMAL(9,0),
                               p_usuario CHAR(20))
RETURNING INTEGER, INTEGER, CHAR(200);

DEFINE v_id_origen               SMALLINT    ;
DEFINE v_id_lote                 integer     ;
DEFINE v_f_inicio_mandato        date        ;
DEFINE v_f_culmina_mandato       date        ;
DEFINE v_scta_origen_descuento   smallint    ;
DEFINE v_estado                  smallint    ;
DEFINE v_f_canales               date        ;
DEFINE v_id_canales              decimal(8,0);
DEFINE v_tipo_operacion          char(1)     ;
DEFINE v_diagnostico             char(3)     ;
DEFINE v_valor_descuento_mandato decimal(12,2); -- cambio precision
DEFINE v_modalidad_aplicacion    SMALLINT    ;
DEFINE v_dec_val_descuento       DECIMAL(10,0)   ;

DEFINE v_id_lote_mandato         DECIMAL(9,0);
DEFINE v_total_procesados        INTEGER;
DEFINE v_fec_actual              DATE;

DEFINE v_tpo_registro            CHAR(2) ;
DEFINE v_nss                     CHAR(11);
DEFINE v_num_credito             CHAR(10);
DEFINE v_tpo_credito             CHAR(3) ;
DEFINE v_edo_credito             CHAR(3) ;
DEFINE v_fec_ini_mandato         CHAR(8) ;
DEFINE v_fec_fin_mandato         CHAR(8) ;
DEFINE v_id_mandato              CHAR(7) ;
DEFINE v_tpo_descuento           CHAR(1) ;
DEFINE v_val_descuento           CHAR(8) ;
DEFINE v_referencia              CHAR(40);
DEFINE v_cve_mandato             CHAR(18);  


DEFINE v_f_inicio                DATE;
DEFINE v_f_culmina               DATE;
DEFINE v_id_derechohabiente      DECIMAL(9,0);

DEFINE v_sql_error               INTEGER;
DEFINE v_isam_error              SMALLINT;
DEFINE v_msg_error               CHAR(200);

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      -- Imprime el codigo de error
      --TRACE 'Ocurrio el error:'||v_sql_error;
      --TRACE 'Error ISAM:'||v_isam_error;
      --TRACE 'MSG Error:'||v_msg_error;      
      LET v_total_procesados = 0;
      RETURN v_total_procesados,v_sql_error,v_msg_error;
   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_mdt_inserta_inst_recurrente.trace';
   --TRACE '1';
   
   LET v_sql_error = 0;
   LET v_msg_error = '';
   LET v_total_procesados = 0;
   LET v_fec_actual = TODAY;
   LET v_id_origen = 1;

   INSERT INTO mdt_lote_mandato
         (folio          ,
          id_origen      ,
          f_proceso      ,
          estado         )
   VALUES(p_folio       ,
          v_id_origen   ,
          v_fec_actual  ,
          101          ); 
          
   --TRACE '2';
             
   FOREACH SELECT tpo_registro   ,
                  nss            ,
                  num_credito    ,
                  tpo_credito    ,
                  edo_credito    ,
                  fec_ini_mandato,
                  fec_fin_mandato,
                  id_mandato     ,
                  tpo_descuento  ,
                  val_descuento  ,
                  referencia     ,
                  cve_mandato    
             INTO v_tpo_registro   ,
                  v_nss            ,
                  v_num_credito    ,
                  v_tpo_credito    ,
                  v_edo_credito    ,
                  v_fec_ini_mandato,
                  v_fec_fin_mandato,
                  v_id_mandato     ,
                  v_tpo_descuento  ,
                  v_dec_val_descuento  ,
                  v_referencia     ,
                  v_cve_mandato           
             FROM safre_mig:tmp_mdt_det_recur_trans
            WHERE 1 = 1
            
      --TRACE '4';
  
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_nss;
     
      --TRACE  v_id_derechohabiente ;
   
   
      IF(v_id_derechohabiente > 0 )THEN
         --TRACE '6';

         --LET v_f_inicio  = v_fec_ini_mandato;
         --LET v_f_culmina = v_fec_fin_mandato;
      
         EXECUTE PROCEDURE sp_cambia_formato_fecha(v_fec_ini_mandato) 
                     INTO v_f_inicio;
         --TRACE '7';
         EXECUTE PROCEDURE sp_cambia_formato_fecha(v_fec_fin_mandato) 
                     INTO v_f_culmina;
         --TRACE '8';
         --EXECUTE PROCEDURE sp_cambia_formato_fecha(v_fec_actual) 
         --         INTO v_fec_actual;
         --TRACE '9';
         LET v_id_origen     = 1;
         LET v_valor_descuento_mandato = v_dec_val_descuento / 100;
         LET v_estado = 101;
         LET v_f_canales = '';
         LET v_id_canales = '';
         LET v_diagnostico = '000';
         
         IF v_edo_credito = '015' THEN LET v_tipo_operacion = 'A';
           ELIF v_edo_credito = '016' THEN LET v_tipo_operacion = 'B';
             ELIF v_edo_credito = '017' or v_edo_credito = '018'
                  THEN LET v_tipo_operacion = 'M';
                ELSE LET v_tipo_operacion = 'D';
         END IF;
         IF v_nss[1,2] = '77' THEN
            LET v_scta_origen_descuento = 43;
         ELSE
            LET v_scta_origen_descuento = 41;
         END IF;
         
         LET v_modalidad_aplicacion = 1;
         INSERT INTO safre_viv:mdt_solicitud_mandato 
               (id_solicitud_mandato    ,
                id_derechohabiente      ,
                id_origen               ,
                folio                   ,
                nss                     ,
                id_credito              ,
                tpo_descuento_mandato   ,
                valor_descuento_mandato ,
                f_inicio_mandato        ,
                f_culmina_mandato       ,
                referencia              ,
                cve_mandato             ,
                scta_origen_descuento   ,
                modalidad_aplicacion    ,
                usuario                 ,
                estado                  ,
                f_canales               ,
                id_canales              ,
                tipo_operacion          ,
                diagnostico)      
         VALUES(safre_viv:seq_mdt_solicitud_mandato.NEXTVAL ,
                v_id_derechohabiente,
                v_id_origen,
                p_folio, -- se ingresa el folio en lugar del lote, debio al cambio de recepscion de acreditados
                v_nss,
                v_num_credito,             
                v_tpo_descuento,
                v_valor_descuento_mandato,
                v_f_inicio,
                v_f_culmina,
                v_referencia,
                v_cve_mandato,
                v_scta_origen_descuento,
                v_modalidad_aplicacion,
                p_usuario,
                v_estado,
                v_f_canales   ,
                v_id_canales  ,
                v_tipo_operacion  ,
                v_diagnostico);
                
      ELSE
        --TRACE 'id_derechohabiente nulo';
      END IF
                
      --TRACE '10';
            
      LET v_total_procesados = v_total_procesados + 1;
      
      
   END FOREACH
   --TRACE '11';
   
   RETURN v_total_procesados,v_sql_error,v_msg_error;

END FUNCTION;


