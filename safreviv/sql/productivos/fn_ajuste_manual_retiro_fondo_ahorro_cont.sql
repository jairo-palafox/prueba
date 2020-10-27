






CREATE FUNCTION "safreviv".fn_ajuste_manual_retiro_fondo_ahorro_cont(v_id_afi_fondo72        DECIMAL(10,0)
                                                         ,v_id_solicitud          DECIMAL(9,0)
                                                         ,v_monto_ajustado        DECIMAL(14,2)
                                                         ,v_monto_ajustado_tanto  DECIMAL(14,2)                                                         
                                                         ,v_tipo_ajuste           CHAR(1) 
                                                         -- tipo de ajste a capturar D = detalle - S = solicitud       
                                                         )
      RETURNING INTEGER, INTEGER, VARCHAR(250)

   DEFINE  v_b_paso               SMALLINT;
   --DEFINE  v_id_afi_fondo72       DECIMAL(9,0);
   DEFINE  v_id_derechohabiente   DECIMAL(9,0);   
   DEFINE  r_acc_viv97            DECIMAL(18,6);
   DEFINE  r_pes_viv72            DECIMAL(14,2);
   DEFINE  v_movimiento           SMALLINT;
   DEFINE  v_movimiento_apo       SMALLINT;
   DEFINE  v_valor_mov            SMALLINT;
   DEFINE  v_origen               CHAR(20);
   DEFINE  v_subcuenta            SMALLINT;
   DEFINE  v_saldo_97_aivs        DECIMAL(18,6);
   DEFINE  v_saldo_97_pesos       DECIMAL(14,2);
   DEFINE  v_resultado_consulta   SMALLINT;
   DEFINE  v_count                SMALLINT;
   define  v_monto_ajustado_sgiro DECIMAL(14,2);

   DEFINE v_i_estado_marca        INTEGER;
   DEFINE v_marca_fondo_ahorro    INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_bnd_preli             SMALLINT;
   DEFINE v_nss                   CHAR(12);
   DEFINE r_tanto_adicional       DECIMAL(14,2);
   DEFINE v_estado_solicitud      SMALLINT;
   DEFINE v_estado_solicitud_cambio SMALLINT;

   --record de tabla ret_fondo_ahorro para la asignacion de tabla manual
   DEFINE r_fondo_ahorro_id_solicitud         decimal(9,0);
   DEFINE r_fondo_ahorro_id_derechohabiente   decimal(9,0);
   DEFINE r_fondo_ahorro_f_solicitud          date;
   DEFINE r_fondo_ahorro_estado_solicitud     smallint;
   DEFINE r_fondo_ahorro_causal_retiro        smallint;
   DEFINE r_fondo_ahorro_id_datamart          decimal(9,0);
   DEFINE r_fondo_ahorro_folio                decimal(9,0);
   DEFINE r_fondo_ahorro_cve_referencia       varchar(20,0);
   DEFINE r_fondo_ahorro_saldo_viv72          decimal(14,2);
   DEFINE r_fondo_ahorro_tanto_adicional      decimal(14,2);
   DEFINE r_fondo_ahorro_caso_adai            integer;
   DEFINE r_fondo_ahorro_entidad_federativa   smallint;
   DEFINE r_fondo_ahorro_f_captura            date;
   DEFINE r_fondo_ahorro_h_captura            datetime hour to second;
   DEFINE r_fondo_ahorro_usuario              VARCHAR(20,0);
   DEFINE r_fondo_ahorro_cod_rechazo          SMALLINT;

   --record de tabla ret_det_fondo72 para la asignacion de tabla manual
   DEFINE r_det_fondo72_id_afi_fondo72       decimal(9,0);
   DEFINE r_det_fondo72_id_solicitud         decimal(9,0);
   DEFINE r_det_fondo72_saldo_viv72          decimal(12,2);
   DEFINE r_det_fondo72_tanto_adicional      decimal(12,2);   
   DEFINE r_det_fondo72_id_datamart          decimal(9,0);
   DEFINE r_det_fondo72_f_saldo              date;
   DEFINE r_det_fondo72_h_saldo              datetime hour to second;
   DEFINE r_det_fondo72_estado_detalle       smallint;
   DEFINE r_det_fondo72_cod_rechazo          smallint;
   

      -- Control de Excepciones
   DEFINE v_si_resultado                      SMALLINT;
   DEFINE sql_err                             INTEGER;
   DEFINE isam_err                            INTEGER;
   DEFINE err_txt                             VARCHAR(250);
   DEFINE v_c_msj                             VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro = 802; -- marca para disposicion de fondo ahorro
   LET v_i_estado_marca     = 0;
   LET v_count              = 0;
   LET v_bnd_preli          = 0;

   LET v_b_paso             = 0;
   --LET v_id_afi_fondo72     = 0;
   LET v_id_derechohabiente = 0;
   
   LET r_acc_viv97          = 0;
   LET r_pes_viv72          = 0;
   LET v_movimiento         = 182;
   LET v_movimiento_apo     = 422;
   LET v_origen             = "RETIRO W";
   LET v_subcuenta          = 40;
   LET v_resultado_consulta = 0;
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_97_pesos     = 0;
   LET v_valor_mov          = 1;
   LET v_estado_solicitud   = 18;
   LET v_estado_solicitud_cambio   = 19;

    --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ajuste_manual_retiro_fondo_ahorro_cont.log';
         --TRACE ("v_id_solicitud");
         --TRACE (v_id_solicitud);
         --TRACE ("v_id_afi_fondo72");
         --TRACE (v_id_afi_fondo72);
         --TRACE ("v_tipo_ajuste");
         --TRACE (v_tipo_ajuste);
   
   IF v_tipo_ajuste = 'D' THEN 
      FOREACH         SELECT * 
                            INTO r_det_fondo72_id_afi_fondo72
                                 ,r_det_fondo72_id_solicitud
                                 ,r_det_fondo72_saldo_viv72
                                 ,r_det_fondo72_tanto_adicional
                                 ,r_det_fondo72_id_datamart
                                 ,r_det_fondo72_f_saldo
                                 ,r_det_fondo72_h_saldo
                                 ,r_det_fondo72_estado_detalle
                                 ,r_det_fondo72_cod_rechazo
                           FROM ret_det_fondo72  
                          WHERE id_solicitud     = v_id_solicitud
                            AND id_afi_fondo72   = v_id_afi_fondo72 
                            AND estado_detalle   = 1  --estado 1 = activo 2 = inactivo 
          LET v_monto_ajustado_sgiro = 0;
          LET v_count = v_count + 1;  
          IF r_det_fondo72_saldo_viv72 < v_monto_ajustado THEN
             LET v_monto_ajustado_sgiro =   v_monto_ajustado - r_det_fondo72_saldo_viv72;
          END IF

          if r_det_fondo72_tanto_adicional > 0 then
            LET r_det_fondo72_tanto_adicional = r_det_fondo72_saldo_viv72;
          end if 

         --TRACE("v_monto_ajustado");
         --TRACE(v_monto_ajustado);
         --TRACE("r_det_fondo72_saldo_viv72");
         --TRACE(r_det_fondo72_saldo_viv72);
         --TRACE("v_monto_ajustado_sgiro");
         --TRACE(v_monto_ajustado_sgiro);

         INSERT INTO ret_det_fondo72_manual VALUES(r_det_fondo72_id_afi_fondo72
                                 ,r_det_fondo72_id_solicitud
                                 ,r_det_fondo72_saldo_viv72
                                 ,v_monto_ajustado_tanto
                                 ,v_monto_ajustado_sgiro
                                 ,r_det_fondo72_id_datamart
                                 ,r_det_fondo72_f_saldo
                                 ,r_det_fondo72_h_saldo
                                 ,r_det_fondo72_estado_detalle
                                 ,r_det_fondo72_cod_rechazo);

      END FOREACH;
    END IF 
   IF v_tipo_ajuste = 'S' THEN 
      FOREACH                SELECT *
                              INTO r_fondo_ahorro_id_solicitud
                                   ,r_fondo_ahorro_id_derechohabiente
                                   ,r_fondo_ahorro_f_solicitud
                                   ,r_fondo_ahorro_estado_solicitud
                                   ,r_fondo_ahorro_causal_retiro
                                   ,r_fondo_ahorro_id_datamart
                                   ,r_fondo_ahorro_folio
                                   ,r_fondo_ahorro_cve_referencia
                                   ,r_fondo_ahorro_saldo_viv72
                                   ,r_fondo_ahorro_tanto_adicional
                                   ,r_fondo_ahorro_caso_adai
                                   ,r_fondo_ahorro_entidad_federativa
                                   ,r_fondo_ahorro_f_captura
                                   ,r_fondo_ahorro_h_captura
                                   ,r_fondo_ahorro_usuario
                                   ,r_fondo_ahorro_cod_rechazo 
                              FROM ret_fondo_ahorro
                             WHERE estado_solicitud = v_estado_solicitud 
                               AND id_solicitud = v_id_solicitud
                               
              LET v_count = v_count + 1;              
              LET v_monto_ajustado_sgiro = 0;  
              IF r_fondo_ahorro_saldo_viv72 < v_monto_ajustado THEN
                LET v_monto_ajustado_sgiro =   v_monto_ajustado - r_fondo_ahorro_saldo_viv72;
              END IF    

              --TRACE("v_monto_ajustado");
              --TRACE(v_monto_ajustado);
              --TRACE("r_fondo_ahorro_saldo_viv72");
              --TRACE(r_fondo_ahorro_saldo_viv72);
              --TRACE("v_monto_ajustado_sgiro");
              --TRACE(v_monto_ajustado_sgiro);
          
              INSERT INTO ret_fondo_ahorro_manual values(r_fondo_ahorro_id_solicitud
                                   ,r_fondo_ahorro_id_derechohabiente
                                   ,r_fondo_ahorro_f_solicitud
                                   ,r_fondo_ahorro_estado_solicitud
                                   ,r_fondo_ahorro_causal_retiro
                                   ,r_fondo_ahorro_id_datamart
                                   ,r_fondo_ahorro_folio
                                   ,r_fondo_ahorro_cve_referencia
                                   ,r_fondo_ahorro_saldo_viv72
                                   ,v_monto_ajustado_tanto
                                   ,v_monto_ajustado_sgiro
                                   ,r_fondo_ahorro_caso_adai
                                   ,r_fondo_ahorro_entidad_federativa
                                   ,r_fondo_ahorro_f_captura
                                   ,r_fondo_ahorro_h_captura
                                   ,r_fondo_ahorro_usuario
                                   ,r_fondo_ahorro_cod_rechazo );

          --cambia estatus de campo ingresado a para que no se ajuste para que ya no se ajuste el movimiento 
          UPDATE  ret_fondo_ahorro SET estado_solicitud   = 19
                                 WHERE id_solicitud        = v_id_solicitud;
      END FOREACH;
   END IF
   
   IF  v_count <= 0 THEN
      LET v_si_resultado = 1;
      LET v_c_msj        = "No se encontraron registros a cargar";
   END IF
   
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


