






CREATE PROCEDURE "safreviv".sp_valida_mod_f_nacimiento()
   RETURNING SMALLINT, INTEGER, INTEGER

   DEFINE v_nss            CHAR(11);
   DEFINE v_f_nacimiento   CHAR(8);
   DEFINE v_ax_nss         CHAR(11);
   DEFINE v_cnt_rechazos   INTEGER ;
   DEFINE v_cnt_aceptados  INTEGER ;
   DEFINE v_estado_cuenta  SMALLINT;
   DEFINE v_cod_error      CHAR(30);
   DEFINE v_formato_fecha  CHAR(8);
   DEFINE v_fecha_valida   SMALLINT;
   DEFINE v_c_fecha_salida DATE;
   DEFINE v_bandera        SMALLINT;
   DEFINE v_fec_dia        DATE;

   LET v_cnt_aceptados = 0;
   LET v_cnt_rechazos  = 0;
   LET v_estado_cuenta = 0;
   LET v_formato_fecha = "ddmmyyyy";
   LET v_bandera       = 1;
   LET v_fec_dia       = TODAY;
   
   FOREACH
      SELECT nss, f_nacimiento
        INTO v_nss,v_f_nacimiento
        FROM safre_tmp:tmp_modificacion_f_nacimiento

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
            INSERT INTO safre_tmp:tmp_mod_rch_f_nacimiento
                 VALUES(v_nss,v_f_nacimiento,"NSS INEXISTENTE O NULO");

            -- Se elimina de la tabla
            DELETE
              FROM safre_tmp:tmp_modificacion_f_nacimiento
             WHERE nss = v_nss ;

            -- se incrementa el contador de rechazos
            LET v_cnt_rechazos = v_cnt_rechazos + 1;

            CONTINUE FOREACH;
         ELSE
            LET v_fecha_valida = NULL;
            LET v_c_fecha_salida = NULL;
            
            EXECUTE FUNCTION fn_verifica_fecha(v_f_nacimiento,v_formato_fecha)
            INTO v_fecha_valida, v_c_fecha_salida;
            
            IF v_c_fecha_salida > v_fec_dia THEN
               LET v_fecha_valida = 0;
            END IF
            
            IF v_fecha_valida = 0 THEN
               INSERT INTO safre_tmp:tmp_mod_rch_f_nacimiento
               VALUES(v_nss,v_f_nacimiento,"FECHA NO VALIDA");
               
               -- Se elimina de la tabla
               DELETE
                 FROM safre_tmp:tmp_modificacion_f_nacimiento
                WHERE nss = v_nss ;
                
               LET v_cnt_rechazos = v_cnt_rechazos + 1;
            END IF 

            LET v_cnt_aceptados = v_cnt_aceptados + 1 ;
         END IF;
      ELSE
         -- Se inserta en la tabla de rechazos
         INSERT INTO safre_tmp:tmp_mod_rch_f_nacimiento
              VALUES(v_nss,v_f_nacimiento,"CUENTA INACTIVA");

         -- Se elimina de la tabla
         DELETE
           FROM safre_tmp:tmp_modificacion_f_nacimiento
          WHERE nss = v_nss ;

         -- se rechaza
         LET v_cnt_rechazos = v_cnt_rechazos + 1 ;
         CONTINUE FOREACH;
      END IF;
   END FOREACH;

   RETURN v_bandera, v_cnt_aceptados,v_cnt_rechazos;
END PROCEDURE;


