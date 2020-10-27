






CREATE FUNCTION "safreviv".fn_sep_preliquidar_fondo72(p_folio       DECIMAL(9,0),
                                            p_pid         DECIMAL(9,0),
                                            p_proceso_cod SMALLINT,
                                            p_opera_cod   SMALLINT,
                                            p_id_expediente_fondo72 DECIMAL(9,0))
                                            

RETURNING INTEGER,
          DECIMAL(16,6),
          DECIMAL(16,6),
          INTEGER,
          CHAR(200);

DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(200);

DEFINE v_afi_id_afi_fondo72              DECIMAL(9,0) ;
DEFINE v_afi_id_afi_fondo72_asociado     DECIMAL(9,0) ;
DEFINE v_afi_rfc                CHAR(13)     ;
DEFINE v_afi_nss                CHAR(11)     ;
DEFINE v_afi_id_derechohabiente DECIMAL(9,0) ;
DEFINE v_afi_nombre             CHAR(60)     ;
DEFINE v_afi_f_apertura         DATE         ;
DEFINE v_afi_ind_estado_cuenta  SMALLINT     ;
DEFINE v_afi_f_estado_cuenta    DATE         ;

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);

DEFINE v_total_expedientes INTEGER;
DEFINE v_total_cargo       DECIMAL(16,6);
DEFINE v_total_abono       DECIMAL(16,6);
DEFINE v_empresa           CHAR(20);


-- registros de saldos de cargo y abono por expediente
DEFINE v_id_expediente     DECIMAL(9,0);
DEFINE v_id_sep_movimiento_invadido_fondo72     DECIMAL(9,0);
DEFINE v_rfc_invadido      CHAR(13);
DEFINE v_rfc_asociado      CHAR(13);
DEFINE v_subcuenta         SMALLINT;
DEFINE v_aivs              DECIMAL(16,6);
DEFINE v_pesos             DECIMAL(12,2);
-- datos para preliquidación
DEFINE v_fecha_actual           DATE;
DEFINE v_id_derechohabiente_inv DECIMAL(9,0);
DEFINE v_id_derechohabiente_asc DECIMAL(9,0);
--DEFINE v_subcuenta_preliq       SMALLINT;
DEFINE v_fondo_inversion        SMALLINT;
DEFINE v_movimiento_abono       SMALLINT;
DEFINE v_movimiento_cargo       SMALLINT;
--DEFINE v_id_sep_movimiento_invadido DECIMAL(9,0);
DEFINE v_h_registro             DATETIME HOUR TO SECOND;
DEFINE v_valor_accion           DECIMAL(28,6);


   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
      LET v_total_expedientes = 0;
      LET v_total_cargo = 0;
      LET v_total_abono = 0;
      RETURN v_total_expedientes,
             v_total_cargo,
             v_total_abono,
             v_sql_error,
             v_msg_error;
   END EXCEPTION ;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_preliquidar_fondo72.trace';

   LET v_total_expedientes = 0;
   LET v_total_cargo = 0;
   LET v_total_abono = 0;
   LET v_sql_error = 0;
   LET v_msg_error = NULL;

   --TRACE "1.- actualiza folio";
   -- Actualiza estado del folio a preliquidado
   UPDATE glo_folio
      SET status = 2
    WHERE folio = p_folio
      AND proceso_cod  = p_proceso_cod
      AND opera_cod = p_opera_cod;

   -- información de registros de preliquidación
   -- LET v_subcuenta_preliq = 44;--viv 97 sólo inf
   LET v_fondo_inversion  = 11;
   LET v_movimiento_abono = 381;   LET v_movimiento_cargo = 382;   --TRACE "2.- ciclo de expedientes";

   FOREACH SELECT id_expediente_fondo72   ,
                  id_sep_movimiento_invadido_fondo72 ,
                  id_afi_fondo72_invadido ,
                  rfc_invadido            ,
                  rfc_asociado            ,
                  --SUM(aivs)               ,
                  --SUM(pesos)
                  aivs                    ,
                  pesos                   ,
                  empresa
             INTO v_id_expediente         ,
                  v_id_sep_movimiento_invadido_fondo72 ,
                  v_afi_id_afi_fondo72    ,
                  v_rfc_invadido          ,
                  v_rfc_asociado          ,
                  v_aivs                  ,
                  v_pesos                 ,
                  v_empresa 
             FROM TABLE (MULTISET(
                             SELECT exp.id_expediente_fondo72,
                                    mov.id_sep_movimiento_invadido_fondo72,
                                    exp.id_afi_fondo72_invadido,
                                    exp.rfc_invadido,
                                    exp.rfc_asociado,
                                    --SUM(mov.importe) aivs,
                                    --SUM(mov.importe) pesos
                                    mov.importe aivs,
                                    mov.importe pesos,
                                    mov.empresa
                               FROM sep_expediente_fondo72 exp JOIN sep_movimiento_invadido_fondo72 mov
                                 ON mov.id_expediente_fondo72 = exp.id_expediente_fondo72
                              WHERE exp.estado           = 75               -- expediente previo confirmado
                                AND mov.ind_mov_asociado = 1      -- monto a separar
                                AND exp.id_expediente_fondo72 = p_id_expediente_fondo72 )) -- Expediente de separacion
                              --GROUP BY exp.id_expediente_fondo72    ,
                                       --exp.id_afi_fondo72_invadido  ,
                                       --exp.rfc_invadido             ,
                                       --exp.rfc_asociado            ))
           WHERE 1=1
           --GROUP BY  id_expediente_fondo72   ,
                     --id_afi_fondo72_invadido ,
                     --rfc_invadido            ,
                     --rfc_asociado


      LET v_subcuenta = 40;

      LET v_total_cargo = v_total_cargo + v_aivs;
      LET v_total_abono = v_total_abono + v_aivs;
--TRACE "3.- foreach";
--TRACE "id_expediente "||v_id_expediente;

      LET v_afi_id_afi_fondo72_asociado = 0;

      SELECT a.*
      INTO   v_afi_id_afi_fondo72     ,
             v_afi_rfc                ,
             v_afi_nss                ,
             v_afi_id_derechohabiente ,
             v_afi_nombre             ,
             v_afi_f_apertura         ,
             v_afi_ind_estado_cuenta  ,
             v_afi_f_estado_cuenta
      FROM afi_fondo72 a
      WHERE a.id_afi_fondo72 = v_afi_id_afi_fondo72;

      LET v_afi_id_afi_fondo72_asociado = NULL;
      -- Valida si la cuenta es nueva, si en sep_expediente_fondo72 no existe id_afi_fondo72_asociado la cuenta es nueva
      SELECT id_afi_fondo72_asociado
        INTO v_afi_id_afi_fondo72_asociado
        FROM sep_expediente_fondo72
       WHERE id_expediente_fondo72 = p_id_expediente_fondo72;
       
      -- Se crea la cuenta asociada y se actualiza en sep_expediente_fondo72 
      IF(v_afi_id_afi_fondo72_asociado IS NULL)THEN
       
         LET v_afi_id_afi_fondo72_asociado = seq_afi_fondo72.NEXTVAL;

         INSERT INTO afi_fondo72 VALUES (v_afi_id_afi_fondo72_asociado   ,
                                         v_rfc_asociado                 ,
                                         "00000000000"                  ,
                                         ""                             ,
                                         v_afi_nombre                   ,
                                         v_afi_f_apertura               ,
                                         0                              , --v_afi_ind_estado_cuenta        ,
                                         v_afi_f_estado_cuenta          );
         
         UPDATE sep_expediente_fondo72 
         SET    id_afi_fondo72_asociado = v_afi_id_afi_fondo72_asociado
         WHERE  id_expediente_fondo72 = v_id_expediente;
         
      END IF

--TRACE "4.- insertar en afi_fondo72";

      LET v_fecha_actual = TODAY;
      LET v_h_registro   = CURRENT HOUR TO SECOND;

      -- cálcula el valor en persos de las acciones para la fecha

      LET v_pesos = v_aivs;
      -------------------------------
      -- Movimientos para el invadido
      --INSERT INTO safre_viv:cta_fondo72
      INSERT INTO safre_viv:sep_preliquida_fondo72
                               VALUES (v_afi_id_afi_fondo72   ,
                                       v_fecha_actual         ,
                                       v_subcuenta            ,
                                       "382"                  ,
                                       p_folio                ,
                                       --v_id_expediente        ,
                                       v_id_sep_movimiento_invadido_fondo72 ,
                                       -v_pesos               ,
                                       " "                    ,
                                       v_fecha_actual         ,
                                       v_h_registro           ,
                                       --"ACLARA X SEP"         );
                                       v_empresa              );

      -------------------------------
      -- Movimientos para el asociado

      --INSERT INTO safre_viv:cta_fondo72
      INSERT INTO safre_viv:sep_preliquida_fondo72
                               VALUES (v_afi_id_afi_fondo72_asociado   ,
                                       v_fecha_actual         ,
                                       v_subcuenta            ,
                                       "381"                  ,
                                       p_folio                ,
                                       --v_id_expediente        ,
                                       v_id_sep_movimiento_invadido_fondo72 ,
                                       v_pesos                ,
                                       " "                    ,
                                       v_fecha_actual         ,
                                       v_h_registro           ,
                                       --"ACLARA X SEP"         );
                                       v_empresa         );

      --TRACE "5.- avanza maquinaria";


      -- realiza el conte de expedientes procesados
      LET v_total_expedientes = v_total_expedientes + 1;
   END FOREACH

      UPDATE sep_expediente_fondo72
      SET    estado = 80 -- preliquidado
      WHERE  id_expediente_fondo72 = v_id_expediente;

   RETURN v_total_expedientes,
          v_total_cargo,
          v_total_abono,
          v_sql_error,
          v_msg_error;
END FUNCTION;


