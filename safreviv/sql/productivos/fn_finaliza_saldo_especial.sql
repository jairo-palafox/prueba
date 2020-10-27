






CREATE FUNCTION "safreviv".fn_finaliza_saldo_especial()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(80);

   LET v_resultado = 1;    --Significa que el proceso se ejecuto correctamente
   LET v_mensaje = "La tabla de saldo especial se finalizó correctamente";
   
   --Se eliminna la tabla de saldo especial
   --DROP TABLE IF EXISTS cta_saldo_especial CASCADE ;

   --Crea el indice para la tabla de saldo mensual y actualiza las estadisticas
   --create unique index xpkcta_saldo_mensual on cta_saldo_mensual(id_derechohabiente,subcuenta,fondo_inversion)
   --using btree  in saldo_2ix_dbs;

   --UPDATE STATISTICS FOR TABLE cta_saldo_mensual;
   

   RETURN v_resultado, v_mensaje;      --uno significa que la funcion se ejecuto correctamente
END FUNCTION;


