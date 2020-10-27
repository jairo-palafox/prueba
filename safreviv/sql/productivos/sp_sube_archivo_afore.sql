






CREATE PROCEDURE "safreviv".sp_sube_archivo_afore();

   DEFINE v_nss      CHAR(11);
   DEFINE v_afore    SMALLINT;
   DEFINE v_ax_afore SMALLINT;
   DEFINE v_error    SMALLINT;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/sp_sube_archivo_afore.trace";
   --TRACE ON;

   LET v_nss         = "";
   LET v_afore       = 0;
   LET v_ax_afore    = 0;
   LET v_error       = 0;

   DELETE
     FROM safre_tmp:tmp_dh_afore
    WHERE nss   IS NULL
       OR nss   = "";

   DELETE
     FROM safre_tmp:tmp_dh_afore
    WHERE afore IS NULL
       OR afore = ""
       OR afore = 0;

   FOREACH
      SELECT nss,afore
        INTO v_nss, v_afore
        FROM safre_tmp:tmp_dh_afore

      IF v_nss IS NULL OR v_nss[1] = " " OR v_afore IS NULL THEN
         CONTINUE FOREACH;
      ELSE
         SELECT afore
           INTO v_ax_afore
           FROM safre_tmp:tmp_afi_afore
          WHERE nss = v_nss;

         IF v_ax_afore = 0 OR v_ax_afore IS NULL THEN
            INSERT INTO safre_tmp:tmp_afi_afore VALUES (v_nss,v_afore);
            CONTINUE FOREACH;
         ELIF v_ax_afore <> v_afore THEN

            LET v_error = 17;

            INSERT INTO safre_tmp:tmp_afi_afore_nss VALUES(v_nss,v_afore,v_error);

            DELETE
              FROM safre_tmp:tmp_afi_afore
             WHERE nss = v_nss;
         
            CONTINUE FOREACH;
         END IF;
      END IF;

   END FOREACH;

   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_afi_afore;
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_afi_afore_nss;

END PROCEDURE;


