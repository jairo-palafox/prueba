






CREATE PROCEDURE "safreviv".fn_rev_dif_apo_sub(v_folio_carga_ap_sub DECIMAL (9,0))  --Folio de Carga de Archivo de Aportaciones Subsecuentes
RETURNING SMALLINT;

--Última modificación 14122017
--Declaración de variables
DEFINE v_bnd_proceso            SMALLINT;
DEFINE v_folio_liquida          DECIMAL(9,0);	
DEFINE v_id_dis_interface_ef    DECIMAL(9,0);
   
  --Inicialización de variables
  LET v_bnd_proceso         = 0; --0 es correcto; 1 es incorrecto
  LET v_id_dis_interface_ef = 0;
  LET v_folio_liquida       = 0;

  --Extrae los campos de la tabla dis_ap_subsecuente 
  --donde el campo folio es igual al folio de carga de archivo de 
  --Aportaciones Subsecuentes
  FOREACH
	SELECT	A.folio_liquida, A.id_dis_interface_ef
	INTO 	v_folio_liquida,v_id_dis_interface_ef
	FROM 	dis_ap_subsecuente A
	WHERE 	A.folio = v_folio_carga_ap_sub

    --Actualiza la tabla dis_interface_ef con el valor del 
    --campo ind_liquidacion = 0 (Liquidado)
	UPDATE dis_interface_ef
	SET    ind_liquidacion                      = 0
	WHERE  dis_interface_ef.folio_liquida       = v_folio_liquida
	AND    dis_interface_ef.id_dis_interface_ef = v_id_dis_interface_ef;
  END FOREACH;

  --Elimina informacion de la tabla de aportaciones subsecuentes
  DELETE
  FROM   safre_viv:dis_ap_subsecuente
  WHERE  folio = v_folio_carga_ap_sub;

  --Elimina información de la tabla de cargos duplicados de aportaciones
  --subsecuente
  DELETE
  FROM   safre_viv:dis_ap_cargo_dup
  WHERE  folio = v_folio_carga_ap_sub;

  --Actualiza el campo estado a 40
  --(Archivo Reversado, Aportaciones Subsecuentes)
  UPDATE safre_viv:dis_ctr_ap_subsecuente
  SET    estado  = 40
  WHERE  folio   = v_folio_carga_ap_sub;

  RETURN v_bnd_proceso; 

END PROCEDURE;


