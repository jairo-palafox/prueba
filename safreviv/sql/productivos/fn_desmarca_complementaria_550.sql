






CREATE FUNCTION "safreviv".fn_desmarca_complementaria_550()

RETURNING SMALLINT;


DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_n_referencia       SMALLINT;
DEFINE v_error              INTEGER;
DEFINE v_bnd_marca          SMALLINT;

   ON EXCEPTION SET v_error
      RETURN v_error; -- ERROR  
      
   END EXCEPTION WITH RESUME;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_desmarca_complementaria_550.trace';
   --TRACE ON;

   FOREACH 
      SELECT id_derechohabiente, 
             n_referencia
      INTO   v_id_derechohabiente, 
             v_n_referencia
      FROM   sfr_marca_activa
      WHERE  marca = 550
      
      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,550,v_n_referencia,0,'',"safreviv",2317)
      INTO v_bnd_marca;
      
   END FOREACH;
   
   RETURN 0; 
END FUNCTION;


