






CREATE FUNCTION "safreviv".fn_estadisticas_saldo_especial()
RETURNING SMALLINT;

   --Primero eliminamos el indice de la tabla de saldo mensual para utilizar el espacio
   DROP INDEX IF EXISTS xpkcta_saldo_especial;

   SET PDQPRIORITY HIGH;
   
   create unique index xpkcta_saldo_especial on cta_saldo_especial(id_derechohabiente,subcuenta,fondo_inversion)
   using btree  in tmp_bdn_4_dbs;

   UPDATE STATISTICS FOR TABLE cta_saldo_especial;

   SET PDQPRIORITY DEFAULT;
   RETURN 0;      --cero significa que la funcion se ejecuto correctamente
END FUNCTION;


