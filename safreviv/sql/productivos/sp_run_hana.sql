






CREATE PROCEDURE "safreviv".sp_run_hana()

DEFINE v_nss                 CHAR(11);
DEFINE v_resultado           SMALLINT;
DEFINE v_credito_vigente     SMALLINT;
DEFINE v_credito_cancelado   SMALLINT;
DEFINE v_credito_liquidado   SMALLINT;
DEFINE v_sin_tramite         SMALLINT;


   PREPARE c_stmt FROM "SELECT nss FROM afi_derechohabiente WHERE ind_estado_cuenta = 0 ; " ;

   DECLARE cur_credito CURSOR FOR c_stmt ;
   OPEN cur_credito ;

   WHILE ( 1 = 1)
      FETCH cur_credito INTO v_nss ;
      IF (SQLCODE != 100) THEN
          LET v_resultado         = 0;

          CALL fn_ret_perfil_pensionado_hana(v_nss) RETURNING v_resultado,         v_credito_vigente,
                                                              v_credito_cancelado, v_credito_liquidado,
                                                              v_sin_tramite ;


          INSERT INTO safre_sdo@vivws_tcp:nss_hana VALUES ( v_nss,  v_resultado, 
                                        v_credito_vigente, v_credito_cancelado,
                                        v_credito_liquidado, v_sin_tramite ) ;

      ELSE
         EXIT ;
      END IF
   END WHILE ;
END PROCEDURE
;


