






CREATE FUNCTION "safreviv".fn_desmarcar_no_procedentes_648()

RETURNING DECIMAL(9,0),
          SMALLINT,
          CHAR(30),
          INTEGER ,
          INTEGER ;

DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_nss_unificador         CHAR(11);
DEFINE v_id_unificador          DECIMAL(9,0);
DEFINE v_total_desmarca_dor     INTEGER;
DEFINE v_total_desmarca_ado     INTEGER;
DEFINE v_error                  INTEGER;
DEFINE v_bnd_marca              SMALLINT;
DEFINE v_folio                  DECIMAL(9,0);
DEFINE v_id_derechohabiente_ado DECIMAL(9,0);
DEFINE v_nss_unificado          CHAR(11);    
DEFINE v_id_unificado           DECIMAL(9,0);
DEFINE v_estado_desmarca        SMALLINT;
DEFINE isam_err                 INTEGER;
DEFINE err_txt                  CHAR(30);
DEFINE v_id_dh_marca            DECIMAL(9,0);
DEFINE v_n_referencia           DECIMAL(9,0);
DEFINE v_id_dh_marca_ado        DECIMAL(9,0);
DEFINE v_n_referencia_ado       DECIMAL(9,0);

   ON EXCEPTION SET v_error, isam_err, err_txt   
      RETURN v_folio,
             v_error,
             err_txt,
             v_total_desmarca_dor,
             v_total_desmarca_ado;
   END EXCEPTION WITH RESUME;

LET v_id_derechohabiente  = 0;
LET v_total_desmarca_dor  = 0;
LET v_total_desmarca_ado = 0;
LET v_nss_unificador     = "";
LET v_error              = 0;
LET v_folio              = 0;
LET v_estado_desmarca    = 0;
LET v_id_dh_marca        = 0;
LET v_n_referencia       = 0;
LET v_id_dh_marca_ado    = 0;
LET v_n_referencia_ado   = 0;
LET err_txt              = "Desmarca exitosa";

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_desmarcar_no_procedentes_648.trace';
   --TRACE ON;

   FOREACH
      SELECT nss_unificador
      INTO   v_nss_unificador
      FROM   safre_tmp:prodinf648_no_procedentes
      GROUP BY 1

      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_unificador;

      --Si el unificador existe
      IF v_id_derechohabiente IS NOT NULL THEN
         SELECT id_derechohabiente,
                n_referencia
         INTO   v_id_dh_marca,
                v_n_referencia 
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    marca = 501;
         
         IF v_n_referencia IS NOT NULL THEN
            --Desmarca al unificador
            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh_marca,501,v_n_referencia,0,0,"safreviv",2301)
                        INTO v_estado_desmarca;

            LET v_total_desmarca_dor = v_total_desmarca_dor + 1;
         END IF
      END IF
   END FOREACH;      
   
   FOREACH
      SELECT nss_unificado
      INTO   v_nss_unificado
      FROM   safre_tmp:prodinf648_no_procedentes
      GROUP BY 1
   
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_ado
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_unificado;
   
      IF v_id_derechohabiente_ado IS NOT NULL THEN
         SELECT id_derechohabiente,
                n_referencia
         INTO   v_id_dh_marca_ado,
                v_n_referencia_ado 
         FROM   sfr_marca_activa
         WHERE  id_derechohabiente = v_id_derechohabiente_ado
         AND    marca = 502;

         IF v_n_referencia_ado IS NOT NULL THEN 
            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh_marca_ado,502,v_n_referencia_ado,0,0,"safreviv",2301)
                        INTO v_estado_desmarca;

            LET v_total_desmarca_ado = v_total_desmarca_ado + 1;
         END IF   
      END IF
   END FOREACH;

   RETURN v_folio,
          v_error,
          err_txt,
          v_total_desmarca_dor,
          v_total_desmarca_ado;
END FUNCTION;


