






CREATE PROCEDURE "safreviv".sp_sep_extraccion_marcas_separacion()
RETURNING CHAR(11)  AS nss_invadido,
          CHAR(3)   AS afore_nss,
          CHAR(8)   AS fecha_de_marca,
          CHAR(40)  AS apellido_paterno,
          CHAR(40)  AS apellido_materno,
          CHAR(40)  AS nombre,
          CHAR(18)  AS curp_nss_invadido,
          CHAR(2)   AS diagnostico,
          CHAR(1)   AS clasificacion,
          CHAR(1)   AS credito_infonavit,
          CHAR(11)  AS nss_asociado,
          CHAR(10)  AS estado;

DEFINE v_nss_invadido      CHAR(11);
DEFINE v_afore_nss         CHAR(3);
DEFINE v_fecha_de_marca    CHAR(8);
DEFINE v_apellido_paterno  CHAR(40);
DEFINE v_apellido_materno  CHAR(40);
DEFINE v_nombre            CHAR(40);
DEFINE v_curp_nss_invadido CHAR(18);
DEFINE v_diagnostico       CHAR(2);
DEFINE v_clasificacion     CHAR(1);
DEFINE v_credito_infonavit CHAR(1);
DEFINE v_nss_asociado      CHAR(11);
DEFINE v_estado            CHAR(10);

--v_nss_invadido,v_afore_nss,v_fecha_de_marca,v_apellido_paterno,v_apellido_materno,
--v_nombre,v_curp_nss_invadido,v_diagnostico,v_clasificacion,v_credito_infonavit,
--v_nss_asociado,v_estado_num

--REALIZANDO EL QUERY
FOREACH
    SELECT  a.nss,
            b.cve_entidad_admon,
            TO_CHAR(c.f_inicio, "%Y%m%d"),
            --c.f_inicio,
            a.ap_paterno_af,
            a.ap_materno_af,
            a.nombre_af,
            a.curp,
            b.diag_confronta,
            b.clasifica_separacion,
            b.credito_infonavit,
            d.asociado,
            CASE b.estado
                    WHEN 5  THEN "aceptado"
                    WHEN 15 THEN "aceptado"
                    WHEN 30 THEN "aceptado"
                    WHEN 10 THEN "rechazado"
                    WHEN 20 THEN "rechazado"
                    WHEN 25 THEN "rechazado"
                    WHEN 35 THEN "ligado_exp"
                    ELSE ""
            END
    INTO    v_nss_invadido,
            v_afore_nss,
            v_fecha_de_marca,
            v_apellido_paterno,
            v_apellido_materno,
            v_nombre,
            v_curp_nss_invadido,
            v_diagnostico,
            v_clasificacion,
            v_credito_infonavit,
            v_nss_asociado,
            v_estado
    FROM    afi_derechohabiente a, 
            sep_det_02_op27 b, 
            sfr_marca_activa c, 
            sep_det_03_op27 d
    WHERE   a.id_derechohabiente = c.id_derechohabiente
    AND     a.id_derechohabiente = b.id_derechohabiente_invadido
    AND     b.id_det_02_op27 = d.id_det_02_op27
    AND     c.marca = 280
    
    RETURN  v_nss_invadido,
            v_afore_nss,
            v_fecha_de_marca,
            v_apellido_paterno,
            v_apellido_materno,
            v_nombre,
            v_curp_nss_invadido,
            v_diagnostico,
            v_clasificacion,
            v_credito_infonavit,
            v_nss_asociado,
            v_estado
    WITH RESUME;
END FOREACH;

END PROCEDURE;


