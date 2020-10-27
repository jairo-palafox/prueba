






CREATE PROCEDURE "safreviv".sp_valida_modif_rfc()
   RETURNING INTEGER, INTEGER

   DEFINE v_nss            CHAR(11);
   DEFINE v_rfc            CHAR(13);
   DEFINE v_ax_nss         CHAR(11);
   DEFINE v_cnt_rechazos   INTEGER ;
   DEFINE v_cnt_aceptados  INTEGER ;
   DEFINE v_estado_cuenta  SMALLINT;
   DEFINE v_cod_error      CHAR(30);

   LET v_cnt_aceptados = 0;
   LET v_cnt_rechazos  = 0;
   LET v_estado_cuenta = 0;

   FOREACH
      SELECT nss, rfc
        INTO v_nss,v_rfc
        FROM safre_tmp:tmp_modificacion_rfc

      SELECT nss,
             ind_estado_cuenta
        INTO v_ax_nss,
             v_estado_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      -- Se valida que la cuenta este activa
      IF v_estado_cuenta = 0 OR v_estado_cuenta IS NULL THEN
         --Si el nss aux es nulo, no existe el nss en afi_derechohabiente
         IF v_ax_nss IS NULL OR v_ax_nss = 0 THEN
            -- Se inserta en la tabla de rechazos
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
                 VALUES(v_nss,v_rfc,"NSS INEXISTENTE O NULO");

            -- Se elimina de la tabla
            DELETE
              FROM safre_tmp:tmp_modificacion_rfc
             WHERE nss = v_nss
                OR rfc = v_rfc ;

            -- se incrementa el contador de rechazos
            LET v_cnt_rechazos = v_cnt_rechazos + 1;

            CONTINUE FOREACH;
         ELSE
            LET v_cnt_aceptados = v_cnt_aceptados + 1 ;
         END IF;
      ELSE
         -- Se inserta en la tabla de rechazos
         INSERT INTO safre_tmp:tmp_modificacion_rechazo_rfc
              VALUES(v_nss,v_rfc,"CUENTA INACTIVA");

         -- Se elimina de la tabla
         DELETE
           FROM safre_tmp:tmp_modificacion_rfc
          WHERE nss = v_nss
             OR rfc = v_rfc ;

         -- se rechaza
         LET v_cnt_rechazos = v_cnt_rechazos + 1 ;
         CONTINUE FOREACH;
      END IF;
   END FOREACH;

   RETURN v_cnt_aceptados,v_cnt_rechazos;
END PROCEDURE;


