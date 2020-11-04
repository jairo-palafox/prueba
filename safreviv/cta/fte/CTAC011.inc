-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAC011
-- Funcionalidad => Consulta catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------
GLOBALS

    CONSTANT MAX_REGISTROS     SMALLINT = 200

    TYPE t_datos_aclara RECORD
        aclaracion_cod              LIKE pag_tpo_aclaracion.aclaracion_cod,
        aclaracion_descripcion      LIKE pag_tpo_aclaracion.aclaracion_descripcion,
        ind_visible                 SMALLINT,
        f_modifica                  LIKE pag_tpo_aclaracion.f_actualiza,
        usuario                     LIKE pag_tpo_aclaracion.usuario,
        tipo_modifica               VARCHAR(60)
    END RECORD

    TYPE t_lista_aclara DYNAMIC ARRAY OF t_datos_aclara

END GLOBALS