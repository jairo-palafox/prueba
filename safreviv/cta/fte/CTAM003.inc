GLOBALS

    TYPE t_datos_aclara RECORD
        aclaracion_cod              LIKE pag_tpo_aclaracion.aclaracion_cod,
        aclaracion_descripcion      LIKE pag_tpo_aclaracion.aclaracion_descripcion,
        ind_visible                 SMALLINT
    END RECORD

END GLOBALS