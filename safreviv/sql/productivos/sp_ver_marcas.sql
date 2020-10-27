






CREATE PROCEDURE "safreviv".sp_ver_marcas()

   DEFINE v_nss     CHAR(11);
   DEFINE v_marca   CHAR(3);
   DEFINE v_fecha   DATE;
   DEFINE v_marca1  CHAR(3);
   DEFINE v_fecha1  DATE;
   DEFINE v_marca2  CHAR(3);
   DEFINE v_fecha2  DATE;
   DEFINE v_ind_cta CHAR(1);
   DEFINE v_id      decimal(9,0);

let v_nss    = "";
let v_marca  = "";
let v_fecha  = "";
let v_marca1 = "";
let v_fecha1 = "";
let v_marca2 = "";
let v_fecha2 = "";
let v_ind_cta= "";
let v_id     = "";

   FOREACH
      SELECT a.nss, a.id_derechohabiente, a.ind_estado_cuenta
        INTO v_nss, v_id, v_ind_cta
        FROM tmp_afi a

      foreach
          select marca, f_inicio
           into v_marca, v_fecha
           from sfr_marca_activa
          WHERE id_derechohabiente = v_id

          if v_marca1 is null then
             let v_marca1 = v_marca;
             let v_fecha1 = v_fecha;
          else
             let v_marca2 = v_marca;
             let v_fecha2 = v_fecha;
          end if
      end foreach;

      insert into tmp_marcas_2 values(
         v_nss,
         v_marca1,
         v_fecha1,
         v_marca2,
         v_fecha2,
         v_ind_cta);

let v_nss    = "";
let v_marca  = "";
let v_fecha  = "";
let v_marca1 = "";
let v_fecha1 = "";
let v_marca2 = "";
let v_fecha2 = "";
let v_ind_cta= "";
let v_id     = "";

   END FOREACH;

END PROCEDURE;


