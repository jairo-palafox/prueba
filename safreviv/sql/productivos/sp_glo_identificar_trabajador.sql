






CREATE PROCEDURE "safreviv".sp_glo_identificar_trabajador( p_id_glo_consulta_trabajador decimal(9,0),
                                                p_nss char(11),
                                                p_rfc char(13),
                                                p_curp char(18) )
RETURNING INTEGER    ,  -- indicador de proceso
          INTEGER    ,  -- error sql
          INTEGER    ,  -- error esam
          CHAR(250)  ,  -- msg error
          CHAR(005)  ,  -- estatus_consulta
          INTEGER    ;  -- total registros
         -- CHAR(500)  ;  -- query
----------------------------------------------------------------
--
-- CREACION    : 3 dic 2015
-- AUTOR       : Jesus David Y¦¦ez Moreno
-- SISTEMA     : SACI-SAFRE
--
-- DESCIRPCION : Procedimiento para localizar trabajadores
-- registrados en SACI, regimen actual y fondo de ahorro
-- el store es consumido por un servicio que a su vez es
-- consumido por CRM, devuelve la localizacion de trabajadores
-- y si tienen una marca activa ya sea de separacion de cuentas
-- o unificacion.
----------------------------------------------------------------

DEFINE             v_err_sql         INTEGER;
DEFINE             v_err_isam        INTEGER;
DEFINE             v_err_msg         CHAR(500);
DEFINE             v_r_actual        CHAR(500);
DEFINE             v_r_anterior      CHAR(500);
DEFINE             v_criterio        CHAR(100);
DEFINE             v_criterio2       CHAR(100);
DEFINE             v_ind             SMALLINT;
DEFINE             v_tipo_nss        SMALLINT; -- 1 viv97 , 2 fondo72
DEFINE             v_diag            SMALLINT;

define             v_id_derechohabiente decimal(9,0);
DEFINE             v_nss               char(11);
DEFINE             v_rfc               char(13);
define             v_curp              char(18);
define             v_ap_paterno_af     char(40);
define             v_ap_materno_af     char(40);
define             v_nombre_af         char(40);
define             v_nombre_r_anterior char(120);
define             v_marca             smallint;
define             v_marca_crm         char(003); --codigo informado a crm
define             v_unificador        char(011);

define             v_n_referencia     decimal(9,0);
define             v_folio             decimal(9,0);
define             v_estatus_consulta  char(003);
define             v_total_registros   integer;

 ON EXCEPTION SET v_err_sql,v_err_isam,v_err_msg
   LET v_ind = 2; --excepcion de sql
   LET v_estatus_consulta = "400"; -- excepcion de negocio

   --INSERT INTO glo_det_consulta_trabajador VALUES (seq_glo_det_consulta_trabajador.NEXTVAL, p_id_glo_consulta_trabajador,v_tipo_nss,v_nss,
                                                   --v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_r_anterior, v_rfc ,
                                                   --v_curp, v_marca_crm, v_unificador);

   RETURN v_ind                   ,
          v_err_sql               ,
          v_err_isam              ,
          v_err_msg               ,
          v_estatus_consulta      ,
          v_total_registros       ;
          --v_r_actual              WITH RESUME  ;
   END EXCEPTION;

  --SET DEBUG FILE TO '/safreviv_int/BD/sp_glo_identificar_trabajador.trace';
  --TRACE ON;
LET v_ind = 0;
LET v_err_sql  = 0 ;
LET v_err_isam = 0 ;
LET v_err_msg  = "TRABAJADOR NO LOCALIZADO";
LET v_criterio = "";
LET v_criterio2 = "";
let v_nss = "";
let v_rfc = "";
let v_curp = "";
let v_ap_paterno_af = "";
let v_ap_materno_af = "";
let v_nombre_af = "";
let v_nombre_r_anterior = "";
let v_marca  = 0;
let v_marca_crm = "000";
let v_unificador = "";
let v_r_actual = "";
let v_r_anterior = "";
let v_tipo_nss = 0;
let v_n_referencia = 0;
let v_folio = 0;
let v_estatus_consulta = "000";
let v_total_registros = 0;

IF p_nss IS NULL OR p_nss = "" THEN LET p_nss = NULL; END IF;
IF p_rfc IS NULL OR p_rfc = "" THEN LET p_rfc = NULL; END IF;
IF p_curp IS NULL OR p_curp = "" THEN LET p_curp = NULL; END IF;

IF p_nss IS NOT NULL  AND p_rfc IS NOT NULL AND p_curp IS NOT NULL THEN

   LET v_criterio = " nss = '"||p_nss||"' AND rfc MATCHES '"||TRIM(p_rfc)||"*' AND curp = '"||p_curp||"'";
   LET v_criterio2 = " nss = '"||p_nss||"' AND rfc MATCHES '"||TRIM(p_rfc)||"*'";

ELIF  p_nss IS NOT NULL AND p_rfc IS NOT NULL AND p_curp IS NULL THEN

   LET v_criterio = " nss = '"||p_nss||"' AND rfc MATCHES '"||TRIM(p_rfc)||"*'";
   LET v_criterio2 = " nss = '"||p_nss||"' AND rfc MATCHES '"||TRIM(p_rfc)||"*'";

ELIF p_nss IS NOT NULL AND p_rfc IS NULL AND p_curp IS NULL THEN

   LET v_criterio = " nss = '"||p_nss||"'";
   LET v_criterio2 = " nss = '"||p_nss||"'";

ELIF p_nss IS NULL AND p_rfc IS NOT NULL AND p_curp IS NOT NULL THEN

   LET v_criterio = " rfc MATCHES '"||TRIM(p_rfc) ||"*' AND curp = '"||p_curp||"'";
   LET v_criterio2 = " rfc MATCHES '"||TRIM(p_rfc) ||"*'";

ELIF p_nss IS NULL AND p_rfc IS NOT NULL AND p_curp IS NULL THEN

   LET v_criterio = " rfc MATCHES '"||TRIM(p_rfc)||"*'";
   LET v_criterio2 = " rfc MATCHES '"||TRIM(p_rfc)||"*'";

ELIF p_nss IS NOT NULL AND p_rfc IS NULL AND p_curp IS NOT NULL THEN

   LET v_criterio = " nss = '"||p_nss||"' AND curp = '"||p_curp||"'";
   LET v_criterio2 = " nss = '"||p_nss||"'";

ELIF p_nss IS NULL AND p_rfc IS NULL AND p_curp IS NOT NULL THEN

   LET v_criterio = " curp = '"||p_curp||"'";

ELIF p_nss IS NULL AND p_rfc IS NULL AND p_curp IS NULL THEN

     LET v_ind = 3; -- excepcion campos nulos no se puede realizar busqueda

        --INSERT INTO glo_det_consulta_trabajador VALUES (seq_glo_det_consulta_trabajador.NEXTVAL, p_id_glo_consulta_trabajador,v_tipo_nss,v_nss,
         --                                          v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_r_anterior, v_rfc ,
         --                                          v_curp, v_marca_crm, v_unificador);
         LET v_estatus_consulta = "400";
         RETURN v_ind                ,
                v_err_sql            ,
                v_err_isam           ,
                v_err_msg            ,
                v_estatus_consulta   ,
                v_total_registros    ;
                --v_r_actual           ;
END IF

LET v_r_actual = "SELECT id_derechohabiente, nss, rfc, curp, ap_paterno_af, ap_materno_af, nombre_af FROM afi_derechohabiente "||
                 "WHERE "||TRIM(v_criterio);

   PREPARE prp_identifica_regimen_actual FROM v_r_actual;
   DECLARE cur_identifica_regimen_actual CURSOR FOR prp_identifica_regimen_actual;
   OPEN cur_identifica_regimen_actual;


   WHILE(SQLCODE == 0)

      FETCH cur_identifica_regimen_actual INTO  v_id_derechohabiente ,
                                                v_nss                ,
                                                v_rfc                ,
                                                v_curp               ,
                                                v_ap_paterno_af      ,
                                                v_ap_materno_af      ,
                                                v_nombre_af          ;

      IF (SQLCODE == 0) THEN
         LET v_estatus_consulta = "100"; -- localizado regimen actual
         LET v_err_msg = "TRABAJADOR LOCALIZADO";
         LET v_tipo_nss = 1; -- es de viv97
         LET v_total_registros = v_total_registros + 1;

         -- se busca marca de separacion de cuentas

         LET v_marca             = 0;
         LET v_marca_crm         = "000";
         LET v_n_referencia  = 0;
         LET v_folio         = 0;

        SELECT FIRST 1 a.marca,
                       CASE a.marca
                        WHEN 280 THEN "012"
                        WHEN 551 THEN "012"
                        WHEN 150 THEN "014"
                        WHEN 151 THEN "014"
                        WHEN 501 THEN "015"
                        WHEN 502 THEN "013"
                        WHEN 503 THEN "015"
                        WHEN 503 THEN "015"
                        WHEN 504 THEN "013"
                       END,
               a.n_referencia   ,
               a.folio
          INTO v_marca ,
               v_marca_crm          ,
               v_n_referencia   ,
               v_folio
          FROM sfr_marca_activa a
         WHERE a.id_derechohabiente =  v_id_derechohabiente
           AND a.marca in (280,551,501,502,503,504,150,151);

         IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
            LET v_marca = 0;
            LET v_marca_crm = "000";
            LET v_unificador = "";
         END IF

         IF (v_marca = 280 ) THEN    -- separacion de cuentas

             LET v_unificador = "";

           ELIF (v_marca = 501 OR v_marca = 503)  THEN -- unificacion (unificador)

             LET v_unificador = "";

           ELIF (v_marca = 502 ) THEN -- unificacion (unificado)

             LET v_unificador = "";

             SELECT b.nss_unificador
             INTO   v_unificador
             FROM   uni_det_unificado  a ,
                    uni_det_unificador b
             WHERE  a.id_unificado      = v_n_referencia
               AND  a.folio_unificacion = v_folio
               AND  a.id_unificador     = b.id_unificador
               AND  a.folio_unificacion = b.folio_unificacion ;

           ELIF (v_marca = 150) THEN -- unificacion (unificado)

             SELECT b.nss_unificador
             INTO   v_unificador
             FROM   uni_det_unificado  a ,
                    uni_det_unificador b
             WHERE  a.id_derechohabiente = v_id_derechohabiente
               AND  a.estado_unificacion = 1
               AND  a.diagnostico        in (5,6)
               AND  a.id_unificador     = b.id_unificador
               AND  a.folio_unificacion = b.folio_unificacion ;

           ELIF (v_marca = 504 OR v_marca = 151) THEN -- unificacion (unificado solo infonavit)

             LET v_unificador = "";

             SELECT b.nss
             INTO   v_unificador
             FROM   uni_inf_unificado  a ,
                    uni_inf_unificador b
             WHERE  a.id_inf_unificado  = v_n_referencia
               AND  a.folio_unificacion = v_folio
               AND  a.id_unificador     = b.id_inf_unificador
               AND  a.folio_unificacion = b.folio_unificacion ;

            END IF


             INSERT INTO glo_det_consulta_trabajador VALUES (seq_glo_det_consulta_trabajador.NEXTVAL, p_id_glo_consulta_trabajador,v_tipo_nss,v_nss,
                                                   v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_r_anterior, v_rfc ,
                                                   v_curp, v_marca_crm, v_unificador);
      END IF

   END WHILE;

   CLOSE cur_identifica_regimen_actual;
   FREE cur_identifica_regimen_actual;
   FREE prp_identifica_regimen_actual;


 IF (v_estatus_consulta = "100" AND p_rfc IS NOT NULL ) OR (v_estatus_consulta = "000") THEN  -- if 4


   IF (p_nss IS NOT NULL AND p_rfc IS NOT NULL ) OR
      (p_nss IS NULL     AND p_rfc IS NOT NULL ) OR
      (p_nss IS NOT NULL AND p_rfc IS NULL     ) THEN -- if 5

      LET v_r_anterior = "SELECT nss, rfc, nombre FROM afi_fondo72 "||
                         "WHERE "||TRIM(v_criterio2);

      PREPARE prp_identifica_regimen_anterior FROM v_r_anterior;
      DECLARE cur_identifica_regimen_anterior CURSOR FOR prp_identifica_regimen_anterior;
      OPEN cur_identifica_regimen_anterior;

      WHILE(SQLCODE == 0)

         FETCH cur_identifica_regimen_anterior INTO  v_nss ,
                                                     v_rfc ,
                                                     v_nombre_r_anterior;

         IF (SQLCODE == 0) THEN
            IF (v_estatus_consulta = "100" OR v_estatus_consulta = "300") THEN LET v_estatus_consulta = "300";  --localizado en viv97 y fondo anterior
               ELSE LET  v_estatus_consulta = "200"; --localizado solo fondo anterior
            END IF

            LET v_err_msg = "TRABAJADOR LOCALIZADO";
            LET v_tipo_nss = 2; -- es de fondo 72
            LET v_total_registros = v_total_registros + 1;
            LET v_ap_paterno_af = ""; LET v_ap_materno_af = ""; LET v_nombre_af = "";
            INSERT INTO glo_det_consulta_trabajador VALUES (seq_glo_det_consulta_trabajador.NEXTVAL, p_id_glo_consulta_trabajador,v_tipo_nss,v_nss,
                                                   v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_r_anterior, v_rfc ,
                                                   v_curp, v_marca_crm, v_unificador);

         END IF

      END WHILE;

      CLOSE cur_identifica_regimen_anterior;
      FREE cur_identifica_regimen_anterior;
      FREE prp_identifica_regimen_anterior;

    END IF  -- fin if 5

   END IF  -- fin if 4

    IF v_ind = 0  AND v_estatus_consulta = "000" THEN -- ejecucion exitosa y no localizado

            --INSERT INTO glo_det_consulta_trabajador VALUES (seq_glo_det_consulta_trabajador.NEXTVAL, p_id_glo_consulta_trabajador,v_tipo_nss,v_nss,
                                                   --v_ap_paterno_af, v_ap_materno_af, v_nombre_af, v_nombre_r_anterior, v_rfc ,
                                                   --v_curp, v_marca_crm, v_unificador);
            LET v_estatus_consulta  = "000";

            RETURN v_ind                ,
                   v_err_sql            ,
                   v_err_isam           ,
                   v_err_msg            ,
                   v_estatus_consulta   ,
                   v_total_registros    ;

    ELIF v_ind = 0 AND (v_estatus_consulta = "100" OR
                        v_estatus_consulta = "200" OR
                        v_estatus_consulta = "300"    ) THEN -- ejecucion exitosa y registros encontrados

            RETURN v_ind                ,
                   v_err_sql            ,
                   v_err_isam           ,
                   v_err_msg            ,
                   v_estatus_consulta   ,
                   v_total_registros    ;

    END IF

END PROCEDURE;


