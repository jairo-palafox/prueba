






CREATE FUNCTION "safreviv".sp_sep_integra_op40(p_folio          DECIMAL(9,0),
                                    p_nombre_archivo CHAR(40), 
                                    p_usuario        CHAR(20),
                                    p_proceso_cod    SMALLINT)
                                    
RETURNING INTEGER,INTEGER,CHAR(254),INTEGER, INTEGER, INTEGER, INTEGER;

-----------------------------------------------------------------------
--Descripcion: Funcion para validar archivo de correcion de saldos 
--             rechazados de op28
--Creaada: 15 abr 2018
--
-----------------------------------------------------------------------

DEFINE v_total_desmarcados INTEGER;
DEFINE v_total_rechazados  INTEGER;
DEFINE v_total_integrados  INTEGER;
DEFINE v_total_aceptadas   INTEGER;
DEFINE v_valida_env_previo SMALLINT;
DEFINE v_insert_det_extra  SMALLINT;
DEFINE f_ind_conciliar     SMALLINT;
DEFINE v_existe_op28       SMALLINT;
-- Retorno de función valida envío previo
DEFINE resultado_valida   SMALLINT;
DEFINE v_sql_err          INTEGER; 
DEFINE isam_err           INTEGER;
DEFINE err_txt            VARCHAR(255);

-- encabezado op28
DEFINE v_cza_f_proceso           DATE;
DEFINE v_cza_tipo_registro       CHAR(2);
DEFINE v_cza_id_servicio         CHAR(2);
DEFINE v_cza_id_operacion        CHAR(2);
DEFINE v_cza_tipo_ent_origen     CHAR(2);
DEFINE v_cza_cve_ent_origen      CHAR(3);
DEFINE v_cza_tipo_ent_destino    CHAR(2);
DEFINE v_cza_cve_ent_destino     CHAR(3);
DEFINE v_cza_f_trans_lote        CHAR(8);
DEFINE v_cza_consecutivo_dia     CHAR(3);
DEFINE v_cza_resultado_operacion CHAR(2);
DEFINE v_cza_diagnostico         CHAR(9);

-- Registro tmp detalle 02 op28
DEFINE v_t_tipo_registro        CHAR(2) ;
DEFINE v_t_contador_servicio    CHAR(10);
DEFINE v_t_invadido             CHAR(11);
DEFINE v_t_ind_marca            CHAR(1) ;
DEFINE v_t_saldo_viv_92         CHAR(15);
DEFINE v_t_saldo_viv_97         CHAR(15);
DEFINE v_t_resultado_operacion  CHAR(2) ;
DEFINE v_t_diagnostico1         CHAR(3) ;
DEFINE v_t_diagnostico2         CHAR(3) ;
DEFINE v_t_diagnostico3         CHAR(3) ;
DEFINE v_t_clasifica_separacion CHAR(1) ;
DEFINE v_t_desmarca_pantalla    CHAR(1) ;
DEFINE v_t_tipo_movimiento      CHAR(1) ;
DEFINE v_t_ind_desmarca         CHAR(1) ;

-- Registro detalle 02 op28
DEFINE v_m_id_det_02_op28          DECIMAL(9)   ;
DEFINE v_m_folio                   DECIMAL(9)   ;
DEFINE v_m_f_proceso               DATE         ;
DEFINE v_m_tipo_registro           CHAR(2)      ;
DEFINE v_m_contador_servicio       INTEGER      ;
DEFINE v_m_invadido                CHAR(11)     ;
DEFINE v_m_id_derechohabiente      DECIMAL(9,0) ;
DEFINE v_m_id_derechohabiente_tipo CHAR(001);
DEFINE v_m_ind_marca               CHAR(1)      ;
DEFINE v_m_saldo_viv_92            DECIMAL(16,6);
DEFINE v_m_saldo_viv_97            DECIMAL(16,6);
DEFINE v_m_resultado_operacion     CHAR(2)      ;
DEFINE v_m_diagnostico1            CHAR(3)      ;
DEFINE v_m_diagnostico2            CHAR(3)      ;
DEFINE v_m_diagnostico3            CHAR(3)      ;
DEFINE v_m_clasifica_separacion    CHAR(1)      ;
DEFINE v_m_id_det_op27             DECIMAL(9)   ;
DEFINE v_m_estado                  SMALLINT     ;
DEFINE v_m_ind_conciliar           SMALLINT     ;
DEFINE v_m_desmarca_pantalla       CHAR(1)      ;
DEFINE v_m_tipo_movimiento         SMALLINT      ;
DEFINE v_m_ind_desmarca            SMALLINT      ;

-- Registro tmp detalle 03 op28
DEFINE v_t_det03_tipo_registro       CHAR(2) ;
DEFINE v_t_det03_contador_servicio   CHAR(10);
DEFINE v_t_det03_asociado            CHAR(11);
DEFINE v_t_det03_ind_marca           CHAR(1) ;
DEFINE v_t_det03_saldo_viv_92        CHAR(15);
DEFINE v_t_det03_saldo_viv_97        CHAR(15);
DEFINE v_t_det03_resultado_operacion CHAR(2) ;
DEFINE v_t_det03_diagnostico1        CHAR(3) ;
DEFINE v_t_det03_diagnostico2        CHAR(3) ;
DEFINE v_t_det03_diagnostico3        CHAR(3) ;
DEFINE v_t_det03_desmarca_pantalla   CHAR(1) ;
DEFINE v_t_det03_tipo_movimiento     CHAR(1) ;
DEFINE v_t_det03_ind_desmarca        CHAR(1) ;

-- Registro detalle 03 op28
DEFINE v_m_det03_id_det_02_op28       DECIMAL(9)   ;
DEFINE v_m_det03_folio                DECIMAL(9)   ;
DEFINE v_m_det03_f_proceso            DATE         ;
DEFINE v_m_det03_tipo_registro        CHAR(2)      ;
DEFINE v_m_det03_contador_servicio    INTEGER      ;
DEFINE v_m_det03_asociado             CHAR(11)     ;
DEFINE v_m_det03_id_derechohabiente   DECIMAL(9,0) ;
DEFINE v_m_det03_id_derechohabiente_tipo  CHAR(001)  ;
DEFINE v_m_det03_ind_marca            CHAR(1)      ;
DEFINE v_m_det03_saldo_viv_92         DECIMAL(16,6);
DEFINE v_m_det03_saldo_viv_97         DECIMAL(16,6);
DEFINE v_m_det03_resultado_operacion  CHAR(2)      ;
DEFINE v_m_det03_diagnostico1         CHAR(3)      ;
DEFINE v_m_det03_diagnostico2         CHAR(3)      ;
DEFINE v_m_det03_diagnostico3         CHAR(3)      ;
DEFINE v_m_det03_desmarca_pantalla    CHAR(1)      ;
DEFINE v_m_det03_tipo_movimiento      SMALLINT;
DEFINE v_m_det03_ind_desmarca        SMALLINT;


DEFINE v_sum_tipo_registro       CHAR(2);
DEFINE v_sum_total_registro_det2 CHAR(10);
DEFINE v_sum_total_registro_det3 CHAR(10);
DEFINE v_sum_total_registros     CHAR(10);

-- Variables generales
DEFINE v_marca              SMALLINT     ;
DEFINE v_senal              INTEGER      ;
DEFINE v_fn_marca_cuenta    CHAR(100)    ;   
DEFINE v_cod_rch            INTEGER      ;
DEFINE v_cod_marca          INTEGER      ;
DEFINE v_m_diag_marca       CHAR(3)      ;
DEFINE v_si_dia             SMALLINT     ;
DEFINE v_si_mes             SMALLINT     ;
DEFINE v_si_ano             SMALLINT     ;
DEFINE v_estado             SMALLINT     ;
DEFINE v_estado_des         SMALLINT     ;
DEFINE v_cont_det03         SMALLINT     ;
DEFINE v_tipo_verificacion  SMALLINT     ;
DEFINE v_existe             SMALLINT     ;
DEFINE v_cons_ind           SMALLINT     ;        
DEFINE v_cons_id_expediente DECIMAL(9,0) ;
DEFINE v_id_nss_expediente  DECIMAL(9,0);
DEFINE v_rechazos           SMALLINT;

DEFINE v_cadena_encabezado CHAR(38);
DEFINE v_cadena_detalle    CHAR(100);
DEFINE v_cadena_sumario    CHAR(32);

-- variables para el control de excepciones
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

   ON EXCEPTION SET v_sql_error, 
                    v_isam_error, 
                    v_msg_error
      LET v_total_desmarcados = 0;
      LET v_total_rechazados  = 0;
      LET v_total_integrados  = 0;
      LET v_total_aceptadas   = 0;
      
      RETURN v_sql_error,
             v_isam_error,
             v_msg_error,
             v_total_desmarcados,
             v_total_rechazados,
             v_total_integrados,
             v_total_aceptadas;
   END EXCEPTION 
   
   SET DEBUG FILE TO '/safreviv_int/BD/fn_sep_int_op28.trace';
   TRACE 'Inicia SPL ';
   TRACE ON;

   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "";
   LET v_total_desmarcados = 0;
   LET v_total_rechazados  = 0;
   LET v_total_integrados  = 0;
   LET v_total_aceptadas   = 0;
   LET v_rechazos          = 0;
   LET v_insert_det_extra  = 0;
   LET v_valida_env_previo = 0;
   LET resultado_valida    = 0;
   LET f_ind_conciliar     = 0;
   LET v_m_ind_conciliar   = 0;
   
   EXECUTE FUNCTION fn_maquinaria('maq_sep_ctr_op28', -- maquinaria
                                  0                , -- (señal)  Recibir
                                  0                ) -- (Estado) inicial
                             INTO v_cod_marca       ,
                                  v_m_diag_marca    ,
                                  v_estado_des      ;

   -- almacena histórico de encabezado op28 se ingresa como "integrado" par
   -- facilitar el reverso 

   EXECUTE FUNCTION fn_maquinaria('maq_sep_ctr_op28', -- maquinaria
                                  5                 , -- (señal)  Integrar Archivo
                                  v_estado_des      ) -- (Estado 5) Recibido
                             INTO v_cod_marca       , 
                                  v_m_diag_marca    , 
                                  v_estado          ;

   FOREACH SELECT tipo_registro       ,
                  id_servicio         ,
                  id_operacion        ,
                  tipo_ent_origen     ,
                  f_trans_lote        ,
                  consecutivo_dia     
             INTO v_cza_tipo_registro       ,
                  v_cza_id_servicio         ,
                  v_cza_id_operacion        ,
                  v_cza_tipo_ent_origen     ,
                  v_cza_f_trans_lote        ,
                  v_cza_consecutivo_dia     
             FROM safre_tmp:tmp_sep_cza_op40
  
      LET v_cadena_encabezado = v_cza_tipo_registro||
                                v_cza_id_servicio||
                                v_cza_id_operacion||
                                v_cza_tipo_ent_origen||
                                v_cza_f_trans_lote||
                                v_cza_consecutivo_dia;
                                
      LET v_cza_f_proceso = TODAY;
      INSERT INTO sep_cza_op40
       (folio,
        f_proceso,
        nombre_archivo,
        tipo_registro,
        id_servicio,
        id_operacion,
        tipo_ent_origen,
        f_trans_lote,
        consecutivo_dia,
        estado)
      VALUES
       (p_folio,
        v_cza_f_proceso,
        p_nombre_archivo,
        v_cza_tipo_registro,
        v_cza_id_servicio,
        v_cza_id_operacion,
        v_cza_tipo_ent_origen,
        sp_cambia_formato_fecha(v_cza_f_trans_lote),
        v_cza_consecutivo_dia,
        v_estado);
        
   END FOREACH
  
   FOREACH SELECT tipo_registro        ,
                  contador_servicio    ,
                  invadido             ,
                  saldo_viv_92         ,
                  saldo_viv_97         ,
                  tipo_movimiento      ,
                  ind_desmarca 
             INTO v_t_tipo_registro       ,
                  v_t_contador_servicio   ,
                  v_t_invadido            ,
                  v_t_saldo_viv_92        ,
                  v_t_saldo_viv_97        ,
                  v_t_tipo_movimiento     ,
                  v_t_ind_desmarca
             FROM safre_tmp:tmp_sep_det02_op40

      SELECT tipo_registro      ,
             contador_servicio  ,
             asociado           ,
             saldo_viv_92       ,
             saldo_viv_97       ,
             tipo_movimiento    ,
             ind_desmarca
        INTO v_t_det03_tipo_registro      ,
             v_t_det03_contador_servicio  ,
             v_t_det03_asociado           ,
             v_t_det03_saldo_viv_92       ,
             v_t_det03_saldo_viv_97       ,
             v_t_det03_tipo_movimiento    ,
             v_t_det03_ind_desmarca
        FROM safre_tmp:tmp_sep_det03_op40
       WHERE contador_servicio = v_t_contador_servicio + 1;
      
      SELECT id_derechohabiente,
             tipo_trabajador
        INTO v_m_id_derechohabiente,
             v_m_id_derechohabiente_tipo
        FROM afi_derechohabiente
       WHERE nss = v_t_invadido;
      
      SELECT id_derechohabiente,
             tipo_trabajador
        INTO v_m_det03_id_derechohabiente, 
             v_m_det03_id_derechohabiente_tipo
        FROM afi_derechohabiente
       WHERE nss = v_t_det03_asociado;
    
       LET v_existe = 0;
 
       If v_m_id_derechohabiente is not null and 
          v_m_det03_id_derechohabiente is not null then 

          LET v_existe_op28 = 0;

          SELECT count(*) 
          INTO   v_existe_op28 
          FROM   sep_det_02_op28 a ,
                 sep_det_03_op28 b
          WHERE  (        a.id_derechohabiente_invadido = v_m_id_derechohabiente
                  AND     a.id_det_02_op28 = b.id_det_02_op28
                  AND     b.id_derechohabiente_asociado = v_m_det03_id_derechohabiente)

          OR
                 (        a.invadido = v_t_invadido
                  AND     a.id_det_02_op28 = b.id_det_02_op28
                  AND     b.asociado = v_t_det03_asociado);

          IF v_existe_op28 > 0 THEN 
             LET v_existe = 1;
             LET v_m_ind_conciliar = 1; --si existe se inicializa en 1 aceptada
          ELSE 
             LET v_existe = 0;
             LET v_m_ind_conciliar = 0; --si no existe se pone en 0 
          END IF
       END IF

       -- validar saldos en el archivo antes de insertar
       IF v_existe = 1 AND v_m_ind_conciliar = 1 THEN
          --primera validacion monto invadido vs asociado deben ser iguales abono vs cargo
          IF v_t_tipo_movimiento = 1 THEN
             IF v_t_det03_tipo_movimiento  <> 2 THEN
                LET v_m_ind_conciliar = 2; -- movimiento de abono requerido en det03
             ELIF v_t_det03_tipo_movimiento = 2 THEN
                 IF (v_t_saldo_viv_92 - v_t_det03_saldo_viv_92) <> 0 THEN
                     LET v_m_ind_conciliar = 3; -- mov viv 92 en archivo invadido y asociado diferentes
                 ELIF (v_t_saldo_viv_97 - v_t_det03_saldo_viv_97) <> 0 THEN
                    LET v_m_ind_conciliar = 4; -- mov viv 97 en archivo invadido y asociado diferentes
                 END IF
             END IF
          ELIF v_t_tipo_movimiento = 2 THEN
             IF v_t_det03_tipo_movimiento  <> 1 THEN
                LET v_m_ind_conciliar = 5; -- movimiento de cargo requerido en det03
             ELIF v_t_det03_tipo_movimiento = 1 THEN
                 IF (v_t_saldo_viv_92 - v_t_det03_saldo_viv_92) <> 0 THEN
                     LET v_m_ind_conciliar = 3; -- mov viv 92 en archivo invadido y asociado diferentes
                 ELIF (v_t_saldo_viv_97 - v_t_det03_saldo_viv_97) <> 0 THEN
                    LET v_m_ind_conciliar = 4; -- mov viv 97 en archivo invadido y asociado diferentes
                 END IF
             END IF
          END IF
       END IF


      IF v_existe = 1 AND v_m_ind_conciliar = 1 THEN
         -- Parejas encontradas
         EXECUTE FUNCTION fn_maquinaria('maq_sep_det_op28',
                                        5, -- (señal)  Integrar Archivo Op 28
                                        5) -- (Estado) Validado
                                   INTO v_cod_marca, 
                                        v_m_diag_marca, 
                                        v_m_estado;
         --LET v_m_ind_conciliar = 1; -- pareja encontrada


      ELSE
         -- Pareja no encontrada
         EXECUTE FUNCTION fn_maquinaria('maq_sep_det_op28',
                                        25, -- (señal)  Rechazar reg Op 28
                                        5)  -- (Estado) Validado
                                   INTO v_cod_marca, 
                                        v_m_diag_marca, 
                                        v_m_estado;

         --LET v_m_ind_conciliar = 0; -- pareja no enconatrada

         -- Registra rechazos, para envio de archivo de rechazos
         LET v_rechazos = v_rechazos + 1;
         
         IF v_rechazos = 1 THEN
            INSERT INTO tmp_sep_rechazados VALUES(v_cadena_encabezado);
         END IF
         
         LET v_cadena_detalle = v_t_tipo_registro||
                                v_t_contador_servicio||
                                v_t_invadido||
                                v_t_saldo_viv_92||
                                v_t_saldo_viv_97||
                                v_t_tipo_movimiento||
                                v_t_ind_desmarca; --||
                                --v_t_resultado_operacion||
                                --v_t_diagnostico1||
                                --v_t_diagnostico2||
                                --v_t_diagnostico3||
				--v_t_desmarca_pantalla;
         -- Registro 02 rechazado  
         INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
        
         LET v_cadena_detalle = v_t_det03_tipo_registro||
                                v_t_det03_contador_servicio||
                                v_t_det03_asociado||
                                v_t_det03_saldo_viv_92||
                                v_t_det03_saldo_viv_97||
                                v_t_det03_tipo_movimiento||
                                v_t_det03_ind_desmarca; --||
                                --v_t_det03_resultado_operacion||
                                --v_t_det03_diagnostico1||
                                --v_t_det03_diagnostico2||
                                --v_t_det03_diagnostico3||
				--v_t_det03_desmarca_pantalla;
         -- Registro 03 rechazado
         INSERT INTO tmp_sep_rechazados VALUES(v_cadena_detalle);
      
      END IF	  


      --TRACE 'Insertando movimientos Det02 y Det03 de OP28';
      --v_m_id_det_02_op28 = secuencia

      LET v_m_folio                   = p_folio;
      LET v_m_f_proceso               = TODAY;
      LET v_m_tipo_registro           = v_t_tipo_registro       ;
      LET v_m_contador_servicio       = v_t_contador_servicio   ;
      LET v_m_invadido                = v_t_invadido            ;
      LET v_m_saldo_viv_92            = v_t_saldo_viv_92/100;
      LET v_m_saldo_viv_97            = v_t_saldo_viv_97/100;
      LET v_m_tipo_movimiento         = v_t_tipo_movimiento;
      LET v_m_ind_desmarca            = v_t_ind_desmarca;
      
      -- Detalle 03
      LET v_m_det03_folio               = p_folio;
      LET v_m_det03_f_proceso           = TODAY;
      LET v_m_det03_tipo_registro       = v_t_det03_tipo_registro      ;
      LET v_m_det03_contador_servicio   = v_t_det03_contador_servicio  ;
      LET v_m_det03_asociado            = v_t_det03_asociado           ;
      LET v_m_det03_saldo_viv_92        = v_t_det03_saldo_viv_92/100;
      LET v_m_det03_saldo_viv_97        = v_t_det03_saldo_viv_97/100;
      LET v_m_det03_tipo_movimiento     = v_t_det03_tipo_movimiento;
      LET v_m_det03_ind_desmarca        = v_t_det03_ind_desmarca;

         --TRACE 'inserta det 02 para tipo imss';         
         INSERT INTO sep_det_02_op40 (id_det_02_op40      ,
                                      folio               ,
                                      f_proceso           ,
                                      tipo_registro       ,
                                      contador_servicio   ,
                                      invadido            ,
                                      id_derechohabiente_invadido  ,
                                      saldo_viv_92        ,
                                      saldo_viv_97        ,
                                      ind_conciliar       ,
                                      estado              ,
                                      tipo_movimiento     ,
                                      ind_desmarca) 
                              VALUES (seq_sep_det_02_op28.NEXTVAL,
                                      v_m_folio               ,
                                      v_m_f_proceso           ,
                                      v_m_tipo_registro       ,
                                      v_m_contador_servicio   ,
                                      v_m_invadido            ,
                                      v_m_id_derechohabiente  ,
                                      v_m_saldo_viv_92        ,
                                      v_m_saldo_viv_97        ,
                                      v_m_ind_conciliar       ,
                                      v_m_estado              ,
                                      v_m_tipo_movimiento     ,
                                      v_m_ind_desmarca    );
            
         LET v_total_aceptadas = v_total_aceptadas + 1;


      INSERT INTO sep_det_03_op40 (id_det_02_op40     ,
                                   folio              ,
                                   f_proceso          ,
                                   tipo_registro      ,
                                   contador_servicio  ,
                                   asociado           ,
                                   id_derechohabiente_asociado ,
                                   saldo_viv_92       ,
                                   saldo_viv_97       ,
                                   tipo_movimiento  ,
                                   ind_desmarca     
                                   ) 
                           --VALUES (safre_viv:seq_sep_det_03_op28.NEXTVAL,  Revisar si es esto ó lo siguiente
                           VALUES (seq_sep_det_02_op28.CURRVAL,
                                   v_m_det03_folio              ,
                                   v_m_det03_f_proceso          ,
                                   v_m_det03_tipo_registro      ,
                                   v_m_det03_contador_servicio  ,
                                   v_m_det03_asociado           ,
                                   v_m_det03_id_derechohabiente ,
                                   v_m_det03_saldo_viv_92       ,
                                   v_m_det03_saldo_viv_97       ,
                                   v_m_det03_tipo_movimiento    ,
                                   v_m_det03_ind_desmarca  );

               LET v_total_integrados = v_total_integrados + 1;

   END FOREACH;
   
   FOREACH SELECT tipo_registro,
                  total_registro_det2,
                  total_registro_det3,
                  total_registros
             INTO v_sum_tipo_registro,
                  v_sum_total_registro_det2,
                  v_sum_total_registro_det3,
                  v_sum_total_registros
             FROM safre_tmp:tmp_sep_sum_op40
    
      INSERT INTO sep_sum_op40
       (folio,
        tipo_registro,
        total_registro_det2,
        total_registro_det3,
        total_registros)
      VALUES
       (p_folio,
        v_sum_tipo_registro,
        v_sum_total_registro_det2,
        v_sum_total_registro_det3,
        v_sum_total_registros);
   END FOREACH;
   
   IF v_rechazos >= 1 THEN  
      LET v_total_rechazados = v_rechazos;
      LET v_sum_total_registro_det2 = v_rechazos;
      LET v_sum_total_registro_det3 = v_rechazos;
      LET v_sum_total_registros = v_rechazos + v_rechazos;
      LET v_cadena_sumario = v_sum_tipo_registro|| -- tipo registro
                             v_sum_total_registro_det2|| -- Total registros 02
                             v_sum_total_registro_det3|| -- Total registros 03
                             v_sum_total_registros; -- Toral registros
      INSERT INTO tmp_sep_rechazados VALUES(v_cadena_sumario);
   
   END IF
  
   -- se actualiza control de archivo y folio solo 
   -- si la integracion es exitosa
 
   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio = p_folio
    WHERE nombre_archivo = p_nombre_archivo;
    
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
    
   --TRACE 'Fin de SPL';
   
   RETURN v_sql_error,
          v_isam_error,
          v_msg_error,
          v_total_integrados, 
          v_total_aceptadas, 
          v_total_rechazados, 
          v_total_desmarcados;

END FUNCTION;


