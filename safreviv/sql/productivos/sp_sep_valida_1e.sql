






CREATE FUNCTION "safreviv".sp_sep_valida_1e(p_id_derechohabiente_invadido DECIMAL(9,0),
                                 p_invadido char(011),
                                 p_asociado char(11),
                                 p_folio decimal(9,0),
                                 p_diag_confronta char(2),
                                 p_resultado_operacion char(2),
                                 p_usuario char(20))
RETURNING SMALLINT;

DEFINE r_valida_1e                        SMALLINT;
DEFINE v_id_derechohabiente_asociado      DECIMAL(9,0);
DEFINE v_id_det_02_op27                   SMALLINT;
DEFINE v_error_sql                        SMALLINT;
DEFINE v_isam_error                       SMALLINT;
DEFINE v_msg_error                        CHAR(40);
DEFINE v_diagnostico                      CHAR(02);

--SET DEBUG FILE TO "/safreviv_int/BD/sp_sep_valida_1e.trace";
--TRACE ON;
LET r_valida_1e = 0;

       -- se valida que el diagnostico del registro sea 01
       IF p_resultado_operacion <> "01" THEN 
           LET r_valida_1e = 8; -- rechazdo por distitno de 01
           RETURN r_valida_1e; 
       END IF
          

        SELECT FIRST 1 a.id_derechohabiente
        INTO   v_id_derechohabiente_asociado
        FROM   afi_derechohabiente a 
        WHERE  a.nss = p_asociado;

        IF DBINFO('SQLCA.SQLERRD2') > 0 THEN 
           LET r_valida_1e = 5; -- rechazdo por existencia de asociado virutal
           RETURN r_valida_1e; 
        END IF

        -- se busca op27 procedente
        SELECT a.id_det_02_op27
        INTO   v_id_det_02_op27
        FROM sep_det_02_op27 a
        WHERE a.invadido                    = p_invadido 
        AND   a.id_derechohabiente_invadido = p_id_derechohabiente_invadido
        AND   a.diag_confronta in ("03","04")  -- solo 03 o 04)
        AND   a.estado = 30  -- solo se aceptan procedentes sin ligar a expediente
        GROUP BY 1;

        IF DBINFO('SQLCA.SQLERRD2') = 0  THEN 
        -- si no se encuentra se rechaza por falta de 03 y 04
           LET r_valida_1e = 6; -- rechazado por no existir 03 04 procedente
           RETURN r_valida_1e;      
        ELSE 
        -- si se encuentra se acepta el 1-e y se da de alta el virtual
           IF p_resultado_operacion = "01" THEN -- Solo se apertura si el resultado operacion = 01
           EXECUTE PROCEDURE sp_sep_apertura_cuenta_virtual(v_id_det_02_op27,p_folio,p_usuario,p_asociado) INTO v_error_sql,v_isam_error,v_msg_error;
              IF v_error_sql = 0 THEN

                 LET r_valida_1e = 9; -- aceptado

                 SELECT a.id_derechohabiente 
                 INTO  v_id_derechohabiente_asociado 
                 FROM   afi_derechohabiente a
                 WHERE  a.nss = p_asociado;

                 UPDATE sep_Det_03_op27
                 SET    asociado = p_asociado ,
                        id_derechohabiente_asociado = v_id_derechohabiente_asociado
                 WHERE  id_det_02_op27 = v_id_det_02_op27;
              ELSE 
                 LET r_valida_1e = 7; -- rechazado error al aperturar o marcar la cuenta
              END IF
           ELSE
             LET r_valida_1e = 9; -- aceptado sin aperturar cuenta 
           END IF
       END IF
RETURN r_valida_1e;
END FUNCTION;


