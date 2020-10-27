






CREATE PROCEDURE "safreviv".sp_insert_solicitud_retiro_solo_infonavit(
                                            v_id_derechohabiente DECIMAL(9,0),
                                            v_id_solicitud       SMALLINT ,
                                            v_tipo_r             SMALLINT  ,
                                            v_causa_rec          SMALLINT,
                                            v_estatus            SMALLINT,
                                            v_acc_acciones       DECIMAL(18,6),
                                            v_acc_pesos          DECIMAL(20,2),
                                            v_clabe              char(18) ,
                                            p_cve_banco          SMALLINT,
                                            p_entidad            SMALLINT,
                                            p_causal_retiro      SMALLINT,
                                            p_ident              SMALLINT,     -- Identificador de beneficiario (si aplica)
                                            p_nombre             CHAR(18),     -- Nombre del beneficiario
                                            p_ape_pat            CHAR(18),     -- Apellido paterno
                                            p_ape_mat            CHAR(18),     -- Apellido materno
                                            v_modulo             CHAR(1));

   DEFINE v_folio                 DECIMAL(9,0);
   DEFINE v_ret_solicitd          INTEGER;
   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_solo_infonavit  INTEGER; -- 801 de acuerdo a catalogo
   DEFINE v_proceso_cod           SMALLINT; 

   -- se inician las variables para marca
   LET v_marca_solo_infonavit = 801; -- marca para disposicion de recursos
   LET v_i_estado_marca       = 0;
   LET v_folio                = 0 ;
   LET v_ret_solicitd         = 0;
   LET v_proceso_cod          = 1501 ;

--SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_insert_solicitud_retiro_solo_infonavit.trace';

IF v_modulo = 'A' THEN

   LET v_ret_solicitd = seq_ret_solicitud.NEXTVAL;
   INSERT INTO ret_solo_infonavit VALUES(v_ret_solicitd ,
                                                   v_id_derechohabiente   ,
                                                   TODAY                  ,
                                                   v_estatus              ,
                                                   v_folio                ,
                                                   v_acc_acciones         ,
                                                   v_acc_pesos            ,
                                                   v_clabe                ,
                                                   -------------------------
                                                   p_cve_banco            ,
                                                   p_entidad              ,
                                                   p_causal_retiro        ,
                                                   -------------------------
                                                   --v_tipo_r             ,
                                                   TODAY                  ,
                                                   TODAY                  ,
                                                   CURRENT HOUR TO SECOND  ,
                                                   USER                   ,
                                                   v_causa_rec
                                                   ) ;

   INSERT INTO ret_beneficiario VALUES(v_ret_solicitd ,
                                                 p_ident ,                            --tpo_beneficiario     smallint
                                                 p_nombre,                            --nombre_beneficiario   char(40)
                                                 p_ape_pat,                           --paterno_beneficiario   char(40)
                                                 p_ape_mat,                           --materno_beneficiario   char(40)
                                                 today);

      --IF v_estatus = 10 THEN 
        -- se marca la cuenta
        LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_marca_cuenta(
                 v_id_derechohabiente
                ,v_marca_solo_infonavit -- marca de solo infonavitt
                ,v_ret_solicitd
                ,0     -- folio se asignara en la solicitd
                ,0     -- estado marca
                ,0     -- codigo de rechazo
                ,0     -- marca de la causa
                ,NULL  -- fecha de la causa
                ,USER  --usuario
                ,v_proceso_cod) --proceso_cod
            INTO v_i_estado_marca;
       --END IF
         -- se desmarca la cuenta
        IF v_estatus = 100 THEN  
          EXECUTE FUNCTION fn_desmarca_cuenta(
                v_id_derechohabiente
               ,v_marca_solo_infonavit -- desmarca de solo infonavit
               ,v_ret_solicitd -- identificador de registro de archivo o lote
               ,40 -- estado marca
               ,v_marca_solo_infonavit -- marca de la causa
               ,USER
               ,v_proceso_cod) --proceso_cod
          INTO v_i_estado_marca; 
      END IF  
END IF;

IF v_modulo = 'M' THEN
   UPDATE  ret_solo_infonavit SET estado_solicitud   = v_estatus,
                                  cod_rechazo        = v_causa_rec
                                  --f_captura          = TODAY
                            WHERE id_solicitud       = v_id_solicitud;
END IF;

END PROCEDURE;


