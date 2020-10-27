






CREATE FUNCTION "safreviv".fn_inicializa_saldo_especial()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(80);

   LET v_resultado = 1;    --Significa que el proceso se ejecuto correctamente
   LET v_mensaje = "La tabla de saldo especial se creo correctamente";
   
   --Se eliminna la tabla de saldo mensual
   DROP TABLE IF EXISTS cta_saldo_especial CASCADE ;
   CREATE TABLE cta_saldo_especial
     (
       id_derechohabiente decimal(9,0) not null ,
       subcuenta smallint not null ,
       fondo_inversion smallint not null ,
       monto_acciones decimal(20,6),
       monto_pesos decimal(20,2),
       f_saldo date
     )
  fragment by round robin in tmp_bdn_1_dbs,tmp_bdn_2_dbs,tmp_bdn_3_dbs,tmp_bdn_4_dbs
  extent size 8000 next size 2000 lock mode row;

   RETURN v_resultado, v_mensaje;      --uno significa que la funcion se ejecuto correctamente
END FUNCTION;


