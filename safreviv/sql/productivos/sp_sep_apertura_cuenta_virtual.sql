






CREATE PROCEDURE "safreviv".sp_sep_apertura_cuenta_virtual(p_id_det_02_op27 DECIMAL(9,0),p_folio integer, p_usuario CHAR(20),p_asociado char(11))
RETURNING INTEGER   ,
          INTEGER   ,
          CHAR(254) ;

DEFINE v_error_sql                INTEGER;
DEFINE v_isam_error               INTEGER;
DEFINE v_msg_error                CHAR(254);

DEFINE v_asociado                 CHAR(011);
DEFINE v_id_derechohabiente       DECIMAL(9,0);
DEFINE r_cod_rechazo              INTEGER;


   ON EXCEPTION SET v_error_sql  ,
                    v_isam_error ,
                    v_msg_error


      RETURN v_error_sql  ,
             v_isam_error , 
             v_msg_error  ;

   END EXCEPTION


--SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_apertura_cuenta_virtual.trace';
--TRACE ON;



   LET v_error_sql  = 0 ;
   LET v_isam_error = 0 ; 
   LET v_msg_error  = "CUENTA APERTURADA Y MARCADA"; 
   LET r_cod_rechazo = 0;


  LET v_asociado = p_asociado; 

   EXECUTE FUNCTION fn_apertura_cuenta_afi(v_asociado     ,     -- nss
                                           ""             ,     -- curp
                                           ""             ,     -- rfc 
                                           ""             ,     -- ind_nrp 
                                           ""             ,     -- nombre_af
                                           "V"            ,     -- tipo_trabajador
                                           "0"            ,     -- id_credito
                                           p_folio        ,     -- folio_lote
                                           "S"            )    -- origen_afiliacion

    INTO v_id_derechohabiente;
 
    IF v_id_derechohabiente <= 0 THEN 

       LET v_error_sql = v_id_derechohabiente ; 
       lET v_msg_error = "ERROR AL APERTURAR CUENTA EN afi_derechohabiente: "||v_asociado;

       RETURN v_error_sql  ,
              v_isam_error ,
              v_msg_error  ;

    END IF;
       
    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente ,  --id_derechohabiente 
                                     160                  ,  --marca cuena con nss virtual
                                     p_id_det_02_op27     ,  --n_referencia id de op27
                                     p_folio              ,  --folio_lote
                                     0                    ,  -- estado_marca 
                                     0                    ,  --cod_rechazo
                                     280                  ,  --marca_Causa
                                     today                ,  --fecha_causa
                                     p_usuario            ,  -- usuario 
                                     2204                 )  --proceso_cod 2204 diagnostico operacion 27
                                     
    INTO r_cod_rechazo;

    EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente ,  --id_derechohabiente 
                                     161                  ,  --marca cuena con nss virtual
                                     p_id_det_02_op27     ,  --n_referencia id de op27
                                     p_folio              ,  --folio_lote
                                     0                    ,  -- estado_marca 
                                     0                    ,  --cod_rechazo
                                     280                  ,  --marca_Causa
                                     today                ,  --fecha_causa
                                     p_usuario            ,  -- usuario 
                                     2204                 )  --proceso_cod 2204 diagnostico operacion 27
                                     
    INTO r_cod_rechazo;

    -- si no no convive la marca 
    IF r_cod_rechazo > 0 THEN 

       LET v_error_sql = r_cod_rechazo ;
       LET v_msg_error = "MARCA NO CONVIVE";

       RETURN v_error_sql  ,
              v_isam_error ,
              v_msg_error  ;

    ELIF r_cod_rechazo < 0 THEN 

       LET v_error_sql = r_cod_rechazo ;
       LET v_msg_error = "ERROR AL MARCAR CUENTA VIRTUAL ID: "||p_id_det_02_op27||" nss: "||v_asociado;

       RETURN v_error_sql  ,
              v_isam_error ,
              v_msg_error  ;

    END IF;

RETURN v_error_sql  ,
       v_isam_error ,
       v_msg_error ;

END PROCEDURE;


