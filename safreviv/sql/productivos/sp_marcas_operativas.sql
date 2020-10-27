






CREATE PROCEDURE "safreviv".sp_marcas_operativas(p_nss CHAR(11))

      SELECT '20' as clave,
             'NSS correcto' as clave_desc,
             afi.nss,
             s.marca,
             d.descripcion_marca
        FROM afi_derechohabiente afi,
             sfr_marca_activa s,
             sfr_marca d,
             cta_verifica_marca t
       WHERE afi.nss = p_nss
         AND s.id_derechohabiente = afi.id_derechohabiente
         AND s.marca = d.marca
         AND s.marca = t.marca
        INTO TEMP tmp_marcas_operativas;

END PROCEDURE
;


