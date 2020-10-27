






CREATE PROCEDURE "safreviv".sp_concilia_nombres()

   DEFINE v_rfc                  CHAR(13);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_curp                 CHAR(18);
   DEFINE v_ap_paterno           CHAR(40);
   DEFINE v_ap_materno           CHAR(40);
   DEFINE v_nombre               CHAR(40);

   DEFINE v_bd_rfc	         CHAR(13);
   DEFINE v_bd_curp		 CHAR(18);
   DEFINE v_bd_ap_paterno        CHAR(50);
   DEFINE v_bd_ap_materno        CHAR(50);
   DEFINE v_bd_nombre            CHAR(50);

	DROP TABLE IF EXISTS afi_dif_nombre CASCADE ;
   CREATE TABLE afi_dif_nombre
   (
      safre_nss           		CHAR(11),
      safre_rfc           		CHAR(13),
      safre_curp           		CHAR(18),
      safre_ap_paterno     		CHAR(40),
      safre_ap_materno     		CHAR(40),
      safre_nombre         		CHAR(40),
      pau_rfc           		CHAR(13),
      pau_curp           		CHAR(18),
      pau_ap_paterno     		CHAR(40),
      pau_ap_materno     		CHAR(40),
      pau_nombre         		CHAR(40)
   )in afi_2_dbs;

	SET PDQPRIORITY HIGH;

	UPDATE STATISTICS FOR TABLE safre_tmp:tmp_cbd_bdnsviv2;

	--Se lee cada uno de los derechohabientes
   FOREACH
      SELECT 
				afi.ap_paterno_af,
				nvl(afi.ap_materno_af,' '),
				afi.nombre_af,
				nvl(afi.rfc,' '),
				nvl(afi.curp,' '),
				afi.nss,
				bdnsviv.rfc,
				bdnsviv.curp,
				bdnsviv.ap_paterno,
				bdnsviv.ap_materno,
				bdnsviv.nombre
      INTO 
            v_ap_paterno,
            v_ap_materno,
            v_nombre,
	    v_rfc,
	    v_curp,
	    v_nss,
	    v_bd_rfc,
	    v_bd_curp,
            v_bd_ap_paterno,
            v_bd_ap_materno,
            v_bd_nombre
      FROM afi_derechohabiente afi
      INNER JOIN afi_77 bdnsviv ON bdnsviv.nss = afi.nss

		IF (v_curp <> v_bd_curp
			OR v_rfc <> v_bd_rfc
			OR v_ap_paterno <> v_bd_ap_paterno
			OR v_ap_materno <> v_bd_ap_materno
			OR v_nombre <> v_bd_nombre) THEN

			INSERT INTO afi_dif_nombre VALUES (
							   v_nss,
							   v_rfc,
							   v_curp,
                                                           v_ap_paterno,
                                                           v_ap_materno,
                                                           v_nombre ,
							   v_bd_rfc,
							   v_bd_curp,
                                                           v_bd_ap_paterno,
                                                           v_bd_ap_materno,
                                                           v_bd_nombre );
		END IF

	END FOREACH;

	UPDATE STATISTICS FOR TABLE afi_dif_nombre;

END PROCEDURE;


