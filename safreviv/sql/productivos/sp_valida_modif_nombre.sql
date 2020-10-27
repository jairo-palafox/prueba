






CREATE PROCEDURE "safreviv".sp_valida_modif_nombre()
   RETURNING INTEGER, INTEGER

   DEFINE v_nss                 CHAR(11);
   DEFINE v_ap_paterno_af       CHAR(40);
   DEFINE v_ap_materno_af       CHAR(40);
   DEFINE v_nombre_af           CHAR(40);
   DEFINE v_ax_nss              CHAR(11);
   DEFINE v_cnt_rechazos        INTEGER ;
   DEFINE v_cnt_aceptados       INTEGER ;
   DEFINE v_estado_cuenta       SMALLINT;

   LET v_cnt_aceptados = 0;
   LET v_cnt_rechazos  = 0;
   LET v_estado_cuenta = 0;
   LET v_ap_paterno_af = NULL;
   LET v_ap_materno_af = NULL;
   LET v_nombre_af     = NULL;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/sp_valida_modif_nombre.trace";
   --TRACE ON;                                                           

{   FOREACH
      SELECT nss,
             ap_paterno_af,
             ap_materno_af,
             nombre_af
        INTO v_nss,
             v_ap_paterno_af,
             v_ap_materno_af,
             v_nombre_af
        FROM safre_tmp:tmp_modificacion_nombre

      SELECT nss,
             ind_estado_cuenta
        INTO v_ax_nss,
             v_estado_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      -- Se valida que la cuenta este activa
      IF v_estado_cuenta = 0 THEN
         --Si el nss aux es nulo, no existe el nss en afi_derechohabiente 
         IF v_ax_nss IS NULL OR v_ax_nss = 0 THEN
            -- Se inserta en la tabla de rechazos
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                 VALUES(v_nss,v_ap_paterno_af, v_ap_materno_af, v_nombre_af);

            -- Se elimina de la tabla
            DELETE 
              FROM safre_tmp:tmp_modificacion_nombre
             WHERE nss = v_nss
                OR ap_paterno_af = v_ap_paterno_af
                OR ap_materno_af = v_ap_materno_af
                OR nombre_af     = v_nombre_af;

            -- se incrementa el contador de rechazos 
            LET v_cnt_rechazos = v_cnt_rechazos + 1;

            CONTINUE FOREACH;
         ELSE
            LET v_cnt_aceptados = v_cnt_aceptados + 1 ;
            CONTINUE FOREACH;
         END IF;
      ELSE
         -- Se inserta en la tabla de rechazos
         INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
              VALUES(v_nss,v_ap_paterno_af, v_ap_materno_af, v_nombre_af);

         -- Se elimina de la tabla
         DELETE
           FROM safre_tmp:tmp_modificacion_nombre
          WHERE nss = v_nss
             OR ap_paterno_af = v_ap_paterno_af
             OR ap_materno_af = v_ap_materno_af
             OR nombre_af     = v_nombre_af;

         -- se rechaza
         LET v_cnt_rechazos = v_cnt_rechazos + 1 ;
         CONTINUE FOREACH;
      END IF;
   END FOREACH;}

   FOREACH
      SELECT nss,
             ap_paterno_af,
             ap_materno_af,
             nombre_af
        INTO v_nss,
             v_ap_paterno_af,
             v_ap_materno_af,
             v_nombre_af
        FROM safre_tmp:tmp_modificacion_nombre

      SELECT nss,
             ind_estado_cuenta
        INTO v_ax_nss,
             v_estado_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

      IF v_nss IS NULL OR v_nss[1] = " " OR
         v_ap_paterno_af IS NULL OR v_ap_paterno_af[1] = " " OR
         v_ap_materno_af IS NULL OR v_ap_materno_af[1] = " " OR
         v_nombre_af IS NULL OR v_nombre_af[1] = " " THEN 

         INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
              VALUES(v_nss,v_ap_paterno_af, v_ap_materno_af, v_nombre_af);

         -- Se elimina de la tabla
         DELETE 
           FROM safre_tmp:tmp_modificacion_nombre
          WHERE nss = v_nss;

         -- se incrementa el contador de rechazos 
         LET v_cnt_rechazos = v_cnt_rechazos + 1;
         CONTINUE FOREACH;
      END IF ;
      --Si el nss aux es nulo, no existe el nss en afi_derechohabiente 
      IF v_ax_nss IS NULL OR v_ax_nss = 0 THEN
         -- Se inserta en la tabla de rechazos
         INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
              VALUES(v_nss,v_ap_paterno_af, v_ap_materno_af, v_nombre_af);

         -- Se elimina de la tabla
         DELETE 
           FROM safre_tmp:tmp_modificacion_nombre
          WHERE nss = v_nss;
             --OR ap_paterno_af = v_ap_paterno_af
             --OR ap_materno_af = v_ap_materno_af
             --OR nombre_af     = v_nombre_af;

         -- se incrementa el contador de rechazos 
         LET v_cnt_rechazos = v_cnt_rechazos + 1;
         CONTINUE FOREACH;
      ELSE
         -- Se valida que la cuenta este activa
         IF v_estado_cuenta = 0 THEN
            LET v_cnt_aceptados = v_cnt_aceptados + 1 ;

         ELSE
            -- Se inserta en la tabla de rechazos
            INSERT INTO safre_tmp:tmp_modificacion_rechazo_nombre
                 VALUES(v_nss,v_ap_paterno_af, v_ap_materno_af, v_nombre_af);

            -- Se elimina de la tabla
            DELETE
              FROM safre_tmp:tmp_modificacion_nombre
             WHERE nss = v_nss;
                --OR ap_paterno_af = v_ap_paterno_af
                --OR ap_materno_af = v_ap_materno_af
                --OR nombre_af     = v_nombre_af;

            -- se rechaza
            LET v_cnt_rechazos = v_cnt_rechazos + 1 ;
            CONTINUE FOREACH;
         END IF;
      END IF;
   END FOREACH;

   --Se eliminan los nss nulos que no se borraron de la tabla 
   DELETE
     FROM safre_tmp:tmp_modificacion_nombre
    WHERE nss IS NULL
       OR nss[1] = " ";

   RETURN v_cnt_aceptados,v_cnt_rechazos;
END PROCEDURE;


