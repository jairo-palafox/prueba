






CREATE PROCEDURE "informix".sp_carga_inicial_contacto_elect(p_folio DECIMAL(9,0), 
                                                 p_fecha_proceso DATE)

   DEFINE v_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_correo_electronico   CHAR(40);
   DEFINE v_tipo_correo          SMALLINT;
   DEFINE v_id_contacto          SMALLINT;
   DEFINE v_usuario              CHAR(20);
   
   LET v_usuario = USER;

   FOREACH
   SELECT a.id_derechohabiente,
          b.correo_electronico,
          b.tipo_correo
     INTO v_id_derechohabiente,
          v_correo_electronico,
          v_tipo_correo
     FROM safre_mig:tmp_det_contacto_elect b, 
          afi_derechohabiente a
    WHERE a.nss = b.nss


   SELECT MAX(id_contacto_electronico)+1
     INTO v_id_contacto
     FROM afi_contacto_electronico
    WHERE id_derechohabiente = v_id_derechohabiente;

   IF v_id_contacto IS NULL THEN
     LET v_id_contacto = 1;
   END IF

   INSERT INTO afi_contacto_electronico VALUES(
          v_id_derechohabiente,
          v_id_contacto,
          v_tipo_correo,
          v_correo_electronico,
          p_folio,
          p_fecha_proceso,
          v_usuario);
   END FOREACH
END PROCEDURE
;


