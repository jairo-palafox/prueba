






CREATE PROCEDURE "safreviv".sp_status_retiro_fondo_ahorro_acti_arch(v_causa_rec   SMALLINT --causa de rechazo
                                                        ,v_estatus     SMALLINT  --estatus de solicitud
                                                        ,v_usuario_cod VARCHAR(20)
                                                        ,v_mod_marca   CHAR(1)
                                                        ,v_folio       DECIMAL(9,0)
                                                        ) 
   DEFINE v_id_solicitud        DECIMAL(9,0);   
   DEFINE v_i_estado_marca      INTEGER;
   DEFINE v_id_afi_fondo72      DECIMAL(9,0);
   DEFINE v_marca_fondo_ahorro  INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_proceso_cod         SMALLINT;
   DEFINE v_operacion           SMALLINT;
   

   LET v_marca_fondo_ahorro = 802; -- marca para fondo ahorro
   LET v_i_estado_marca     = 0;
   LET v_id_afi_fondo72     = 0;
   LET v_id_solicitud       = 0;
   LEt v_proceso_cod        = 1515;
   LEt v_operacion          = 3;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/1515_cambio_status.trace';

  -- se busca solicitud para realizar cambio de estatus
  FOREACH 
  SELECT DISTINCT
         a.id_referencia ,
         b.id_derechohabiente
  INTO   v_id_solicitud  ,
         v_id_afi_fondo72
  FROM   ret_preliquida72 a,
         ret_fondo_ahorro b
  WHERE  a.folio_liquida = v_folio
  AND    a.folio_liquida = b.folio
  AND    a.id_referencia = b.id_solicitud

     -- se cambia el estatus de la solicitud segun parametros
     UPDATE  ret_fondo_ahorro 
     SET     estado_solicitud   = v_estatus,
             cod_rechazo        = v_causa_rec
     WHERE   id_solicitud       = v_id_solicitud
     AND     folio              = v_folio;

     -- si es una desmarca
     IF ( v_mod_marca = 'D' ) THEN
        -- se desmarca la cuenta
        EXECUTE FUNCTION fn_desmarca_cuenta( v_id_afi_fondo72
                                            ,v_marca_fondo_ahorro -- desmarca de fondo ahorro
                                            ,v_id_solicitud -- identificador de registro de archivo o lote
                                            ,0 -- estado marca
                                            ,0 -- marca de la causa
                                            ,v_usuario_cod
                                            ,v_proceso_cod )           --proceso_cod 
        INTO v_i_estado_marca;
/*        
        trace ("Desmarcando cuenta");
        trace("v_id_afi_fondo72    : " || v_id_afi_fondo72    );
        trace("v_marca_fondo_ahorro: " || v_marca_fondo_ahorro);
        trace("v_id_solicitud      : " || v_id_solicitud      );
        trace("v_i_estado_marca    : " || v_i_estado_marca    );
*/
        
    END IF

    -- si es un reverso de desmarca
    IF ( v_mod_marca = 'R' ) THEN
       
       EXECUTE PROCEDURE sp_reversa_desmarca( v_id_afi_fondo72
                                             ,v_marca_fondo_ahorro    -- marca de disposicion
                                             ,v_id_solicitud
                                             ,v_folio);
    END IF
  END FOREACH;

END PROCEDURE;


