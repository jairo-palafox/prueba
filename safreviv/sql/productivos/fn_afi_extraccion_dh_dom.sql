






CREATE FUNCTION "safreviv".fn_afi_extraccion_dh_dom()
   RETURNING SMALLINT, INTEGER, VARCHAR(250);

   -- Registro del acreditado para la tabla temporal
   DEFINE v_rfc              CHAR(13);
   DEFINE v_curp             CHAR(18);
   DEFINE v_nombre           CHAR(120);
   DEFINE v_nss              CHAR(11);
   DEFINE v_domicilio        CHAR(90);
   DEFINE v_colonia          CHAR(50);
   DEFINE v_ciudad           CHAR(50);
   DEFINE v_localidad        CHAR(100);
   DEFINE v_estado           CHAR(50);
   DEFINE v_municipio        CHAR(50);
   DEFINE v_cp               CHAR(5);

   DEFINE v_cod_error        SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err         INTEGER;
   DEFINE v_c_msj            VARCHAR(250);
   DEFINE v_precio           DECIMAL(15,6);

   ON EXCEPTION SET v_cod_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_cod_error, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/afi_dh_dom.trace';
   --TRACE ON;

   -- se establece la prioridad
   SET PDQPRIORITY HIGH;

   -- se inicializa el contador de registros
   LET v_cod_error = 0;
   LET v_isam_err = 0;
   LET v_c_msj = 'El proceso finalizó correctamente';

   FOREACH
      SELECT af.rfc,
             af.curp,
             TRIM(af.ap_paterno_af)||" "||TRIM(af.ap_materno_af)||" "||TRIM(af.nombre_af),
             af.nss,
             TRIM(ad.calle)||" "||TRIM(ad.num_exterior)||" "||TRIM(ad.num_interior),
             TRIM(ad.colonia),
             TRIM(cd.ciudad_desc),
             ce.entidad_desc_larga,
             cm.municipio_desc,
             cp.cp
        INTO v_rfc,
             v_curp,
             v_nombre,
             v_nss,
             v_domicilio,
             v_colonia,
             v_ciudad,
             v_estado,
             v_municipio,
             v_cp
        FROM afi_derechohabiente af,
             afi_domicilio ad, cat_cp cp,
             cat_ciudad cd,
             cat_entidad_federativa ce,
             cat_municipio cm
       WHERE af.id_derechohabiente = ad.id_derechohabiente
         AND ad.cp = cp.cp
         AND cp.ciudad = cd.ciudad
         AND cp.entidad_federativa = ce.entidad_federativa
         AND cp.municipio = cm.municipio

      IF v_colonia IS NOT NULL AND v_ciudad IS NOT NULL THEN
         LET v_localidad = TRIM(v_colonia)||", "||TRIM(v_ciudad);
      ELSE
         IF v_colonia IS NULL AND v_ciudad IS NULL THEN
            LET v_localidad = " ";
         ELSE
            IF v_colonia IS NULL AND v_ciudad IS NOT NULL THEN
               LET v_localidad = v_ciudad;
            ELSE
               IF v_colonia IS NOT NULL AND v_ciudad IS NULL THEN
                  LET v_localidad = v_colonia;
               END IF
            END IF
         END IF
      END IF

      -- se inserta el registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_dh_dom (
                  rfc,
                  curp,
                  nombre,
                  nss,
                  domicilio,
                  localidad,
                  estado,
                  municipio,
                  cp)
          VALUES (v_rfc,
                  v_curp,
                  v_nombre,
                  v_nss,
                  v_domicilio,
                  v_localidad,
                  v_estado,
                  v_municipio,
                  v_cp);
   END FOREACH;

   -- se regresa la prioridad
   SET PDQPRIORITY DEFAULT;

   RETURN v_cod_error, v_isam_err, v_c_msj;

END FUNCTION;


