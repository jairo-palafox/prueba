






CREATE PROCEDURE "safreviv".sp_status_retiro_fondo_ahorro_acti_manual(v_causa_rec   SMALLINT --causa de rechazo
                                                          ,v_estatus     SMALLINT  --estatus de solicitud
                                                          ,v_usuario_cod VARCHAR(20)
                                                          ,v_mod_marca   CHAR(1)
                                                          ,v_folio       DECIMAL(9,0)
                                                        ) 
   DEFINE v_id_solicitud        DECIMAL(9,0);   
   DEFINE v_i_estado_marca      INTEGER;
   DEFINE v_nss                 CHAR(11);
   DEFINE v_id_afi_fondo72      DECIMAL(9,0);
   DEFINE v_marca_fondo_ahorro  INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_proceso_cod         SMALLINT;
   DEFINE v_operacion           SMALLINT;
   

   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_i_estado_marca     = 0;
   LET v_id_afi_fondo72     = 0;
   LET v_id_solicitud       = 0;
   LET v_nss                = "" ;
   LEt v_proceso_cod        = 1518;
   LEt v_operacion          = 1;      --proceso anterior


  --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_status_retiro_fondo_ahorro_acti_arch.log';

   IF v_mod_marca = 'D' THEN
   --toma el ultimo folio activo y modifica su estatus

     SELECT UNIQUE(max(folio_liquida))
       INTO v_folio
       FROM ret_preliquida72  a ,
            glo_folio b
      WHERE a.folio_liquida = b.folio
        AND b.proceso_cod   = v_proceso_cod --100
        --AND b.opera_cod     = v_operacion
        AND b.status        = 2; --liquidado
  END IF

     IF v_mod_marca = 'R' THEN
   --toma el ultimo folio activo y modifica su estatus
     SELECT UNIQUE(max(folio_liquida))
       INTO v_folio
       FROM ret_preliquida72  a ,
            glo_folio b
      WHERE a.folio_liquida = b.folio
        AND b.proceso_cod   = v_proceso_cod --100
        --AND b.opera_cod     = v_operacion
        AND b.status        = 1;
  END IF
       --TRACE ('------------------------------------\n');
       --TRACE ('v_folio');
       --TRACE (v_folio);
       --TRACE ('v_marca_fondo_ahorro');
       --TRACE (v_marca_fondo_ahorro);
       --TRACE ('v_id_solicitud');
       --TRACE (v_id_solicitud);
       --TRACE ('v_usuario_cod');
       --TRACE (v_usuario_cod);

   --se busca solicitud para realizar cambio e estatus
  FOREACH cu_status_fondo_ahorro FOR SELECT id_referencia,id_afi_fondo72
                                              INTO  v_id_solicitud,v_id_afi_fondo72
                                              FROM ret_preliquida72
                                             WHERE folio_liquida = v_folio

       --TRACE ('dentro de el for ');
       --TRACE ('v_folio');
       --TRACE (v_folio);
       --TRACE ('v_marca_fondo_ahorro');
       --TRACE (v_marca_fondo_ahorro);
       --TRACE ('v_id_solicitud');
       --TRACE (v_id_solicitud);
       --TRACE ('v_usuario_cod');
       --TRACE (v_usuario_cod);

       SELECT nss
         INTO v_nss
         FROM afi_fondo72
        WHERE id_afi_fondo72 = v_id_afi_fondo72;

   --cambio de estatuis tabla ret_fondo_ahorro
   UPDATE  ret_fondo_ahorro_manual SET estado_solicitud   = v_estatus,
                                       cod_rechazo        = v_causa_rec,
                                       folio              = v_folio
                                 WHERE id_solicitud       = v_id_solicitud;

       --TRACE ('------------------------------------\n');
       --TRACE ('v_id_afi_fondo72');
       --TRACE (v_id_afi_fondo72);
       --TRACE ('v_marca_fondo_ahorro');
       --TRACE (v_marca_fondo_ahorro);
       --TRACE ('v_id_solicitud');
       --TRACE (v_id_solicitud);
       --TRACE ('v_usuario_cod');
       --TRACE (v_usuario_cod);

    IF v_mod_marca = 'D' THEN
    -- se desmarca la cuenta
       EXECUTE FUNCTION fn_desmarca_cuenta(
                        v_id_afi_fondo72
                       ,v_marca_fondo_ahorro -- desmarca de fondo ahorro
                       ,v_id_solicitud -- identificador de registro de archivo o lote
                       ,0 -- estado marca
                       ,0 -- marca de la causa
                       ,v_usuario_cod
                       ,v_proceso_cod )           --proceso_cod 
                   INTO v_i_estado_marca;
    END IF

     IF v_mod_marca = 'R' THEN
       
      EXECUTE PROCEDURE sp_reversa_desmarca(
                              v_id_afi_fondo72
                             ,v_marca_fondo_ahorro    -- marca de disposicion
                             ,v_id_solicitud
                             ,v_folio);
    END IF
  END FOREACH;

END PROCEDURE;


