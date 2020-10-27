






CREATE PROCEDURE "safreviv".fn_ret_perfil_pensionado_hana(v_nss char(11))
RETURNING SMALLINT,  SMALLINT,  SMALLINT,  SMALLINT,  SMALLINT;

DEFINE v_resultado           SMALLINT;
DEFINE v_credito_vigente     SMALLINT;
DEFINE v_credito_cancelado   SMALLINT;
DEFINE v_credito_liquidado   SMALLINT;
DEFINE v_sin_tramite         SMALLINT;


   LET v_resultado         = 0 ;
   LET v_credito_vigente   = 0 ;
   LET v_credito_cancelado = 0 ;
   LET v_credito_liquidado = 0 ;
   LET v_sin_tramite       = 0 ;


   --CALL fn_val_nss_hana(v_nss) RETURNING v_resultado ; no es necesario
   --IF v_resultado = 0 THEN no es necesario
      CALL fn_bus_reso_hana(v_nss) RETURNING v_resultado ;

      IF v_resultado = 0 THEN

         CALL fn_busca_info_cre_hana(v_nss) RETURNING v_resultado,         v_credito_vigente,
                                                      v_credito_cancelado, v_credito_liquidado,
                                                      v_sin_tramite;
      END IF
   --END IF

   RETURN v_resultado, v_credito_vigente, v_credito_cancelado, v_credito_liquidado, v_sin_tramite;
END PROCEDURE
;


