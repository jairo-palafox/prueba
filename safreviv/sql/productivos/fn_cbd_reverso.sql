






CREATE FUNCTION "safreviv".fn_cbd_reverso(p_folio DECIMAL(9,0), p_usuario_cod  VARCHAR(30))
RETURNING VARCHAR(50);

   DEFINE v_nombre_archivo       VARCHAR(50);
   DEFINE v_result               SMALLINT;

   SET PDQPRIORITY HIGH;

   --Se eliminan los posibles datos de conciliacion
   DELETE FROM  safre_viv:cbd_cifras_concilia WHERE folio = p_folio;

   --Se eliminan las cifras globales del archivo
   DELETE FROM  safre_viv:cbd_cifras_concilia_global WHERE folio = p_folio;

   --Se elimina el sumario
   DELETE FROM  safre_viv:cbd_sum_bdnsviv WHERE folio = p_folio;

   --Se eliminan todos los detalles del archivo
   DROP TABLE IF EXISTS safre_viv:cbd_detalle_bdnsviv CASCADE ;
   --DELETE FROM  safre_viv:cbd_detalle_bdnsviv;

   --Se elimina el encabezado del archivo
   DELETE FROM  safre_viv:cbd_cza_bdnsviv WHERE folio = p_folio;

   --Se busca el nombre del archivo
   SELECT 
      nombre_archivo
   INTO v_nombre_archivo
   FROM safre_viv:glo_ctr_archivo 
   WHERE proceso_cod = 2101
   AND folio = p_folio;

   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(v_nombre_archivo,
                                                 p_folio,
                                                 3,               --Estado de reversado
                                                 p_usuario_cod)
                                                INTO v_result; 

   SET PDQPRIORITY DEFAULT;
   RETURN v_nombre_archivo;

END FUNCTION;


