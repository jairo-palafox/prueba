






CREATE PROCEDURE "safreviv".sp_actualiza_nombres()

   DEFINE v_nss   CHAR(11);
   DEFINE v_rfc                  CHAR(13);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_ap_paterno           CHAR(40);
   DEFINE v_ap_materno           CHAR(40);
   DEFINE v_nombre               CHAR(40);

	SET PDQPRIORITY HIGH;

	FOREACH
      SELECT safre_nss,
	     pau_rfc,
	     pau_curp,
	     pau_ap_paterno,
	     pau_ap_materno,
	     pau_nombre
      INTO  v_nss,
	    v_rfc,
	    v_curp,
            v_ap_paterno,
            v_ap_materno,
            v_nombre
      FROM afi_dif_nombre

		UPDATE afi_derechohabiente 
		SET rfc = v_rfc,
                    curp = v_curp,
		    ap_paterno_af = v_ap_paterno,
		    ap_materno_af = v_ap_materno,
		    nombre_af = v_nombre
		WHERE nss = v_nss;

		UPDATE cta_saldo_preca 
		SET rfc = v_rfc,
                    curp = v_curp,
		    ap_paterno = v_ap_paterno,
		    ap_materno = v_ap_materno,
		    nombre = v_nombre
		WHERE nss = v_nss;
	END FOREACH;

END PROCEDURE;


