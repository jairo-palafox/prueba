






CREATE FUNCTION "safreviv".fn_marcar_procedentes_648()

RETURNING DECIMAL(9,0),
          SMALLINT,
          CHAR(30),
          INTEGER ,
          INTEGER ;


DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_nss_unificador         CHAR(11);
DEFINE v_id_unificador          DECIMAL(9,0);
DEFINE v_total_marcas_dor       INTEGER;
DEFINE v_total_marcas_ado       INTEGER;
DEFINE v_n_referencia           INTEGER;
DEFINE v_error                  INTEGER;
DEFINE v_bnd_marca              SMALLINT;
DEFINE v_folio                  DECIMAL(9,0);
DEFINE v_nombre_imss            CHAR(50);
DEFINE v_id_derechohabiente_ado DECIMAL(9,0);
DEFINE v_nss_unificado          CHAR(11);    
DEFINE v_nombre_imss_ado        CHAR(50);
DEFINE v_id_unificado           DECIMAL(9,0);
DEFINE v_estado_marca           SMALLINT;
DEFINE isam_err                 INTEGER;
DEFINE err_txt                  CHAR(30);
DEFINE v_estado_desmarca        SMALLINT;
DEFINE v_seq_pre_unificador     DECIMAL(9,0);
DEFINE v_seq_pre_unificado      DECIMAL(9,0);

   ON EXCEPTION SET v_error, isam_err, err_txt   
      RETURN v_folio,
             v_error,
             err_txt,
             v_total_marcas_dor,
             v_total_marcas_ado;
   END EXCEPTION WITH RESUME;

LET v_total_marcas_dor   = 0;
LET v_total_marcas_ado   = 0;
LET v_id_derechohabiente = 0;
LET v_nss_unificador     = "";
LET v_nombre_imss        = "";
LET v_n_referencia       = 0;
LET v_error              = 0;
LET v_folio              = 0;
LET v_estado_desmarca    = 0;
LET v_seq_pre_unificador = 0;
LET v_seq_pre_unificado  = 0;
LET err_txt              = "Marca exitosa";

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_marcar_procedentes_648.trace';
   --TRACE ON;

   EXECUTE FUNCTION fn_genera_folio (2301, 2, "safreviv")
               INTO v_folio;

   FOREACH
      SELECT nss_unificado
      INTO   v_nss_unificado
      FROM   safre_tmp:prodinf648_procedentes
      GROUP BY 1

      SELECT id_derechohabiente,
             nombre_imss
      INTO   v_id_derechohabiente_ado,
             v_nombre_imss_ado
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_unificado;

      IF v_id_derechohabiente_ado IS NOT NULL THEN
         SELECT seq_uni_pre_unificado.NEXTVAL
         INTO   v_seq_pre_unificado
         FROM   systables 
         WHERE  tabname = "seq_uni_pre_unificado";

         INSERT INTO uni_pre_unificado
              VALUES (
                      v_seq_pre_unificado,
                      --v_seq_pre_unificador,
                      v_folio,
                      v_id_derechohabiente_ado,
                      NULL,
                      v_nss_unificado,
                      v_nombre_imss_ado,
                      1,
                      1
                      );

         EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_ado,502,v_seq_pre_unificado,v_folio,0,0,0,NULL,"safreviv",2301)
                     INTO v_estado_marca;

         IF v_estado_marca <> 0 THEN
            UPDATE uni_pre_unificado
            SET    estado      = 2,
                   diagnostico = 11 -- UNIFICADO NO SE MARCO
            WHERE  id_pre_unificado = v_seq_pre_unificado;
            
            INSERT INTO safre_tmp:uni_rch_marca 
            VALUES (v_nss_unificado,
                    11,
                    NULL); --No existe DH
         ELSE
            LET v_total_marcas_ado = v_total_marcas_ado + 1;
         END IF
      ELSE
         INSERT INTO safre_tmp:uni_rch_marca 
         VALUES (v_nss_unificado,
                 12,
                 NULL); --No existe DH
      END IF 
   END FOREACH
   
   FOREACH
      SELECT nss_unificador
      INTO   v_nss_unificador
      FROM   safre_tmp:prodinf648_procedentes
      GROUP BY 1
      
      SELECT id_derechohabiente,
             nombre_imss
      INTO   v_id_derechohabiente,
             v_nombre_imss
      FROM   afi_derechohabiente
      WHERE  nss = v_nss_unificador;

      --Si el unificador existe
      IF v_id_derechohabiente IS NOT NULL THEN
         SELECT seq_uni_pre_unificador.NEXTVAL
         INTO   v_seq_pre_unificador
         FROM   systables 
         WHERE  tabname = "seq_uni_pre_unificador";
            
         INSERT INTO uni_pre_unificador 
              VALUES (           
                      v_seq_pre_unificador,
                      v_folio,
                      v_id_derechohabiente,
                      0,
                      0,
                      v_nss_unificador,
                      v_nombre_imss,
                      1,
                      1,
                      1,
                      TODAY,
                      TODAY
                     );

         --Marca al unificador
         EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,501,v_seq_pre_unificador,v_folio,0,0,0,NULL,"safreviv",2301)
                     INTO v_estado_marca;

         IF v_estado_marca <> 0 THEN 
            UPDATE uni_pre_unificador
            SET    estado_familia = 2,
                   diagnostico    = 10 -- Rechaza familia unificador no se marcó
            WHERE  id_pre_unificador = v_seq_pre_unificador;
            
            INSERT INTO safre_tmp:uni_rch_marca 
            VALUES (v_nss_unificado,
                    10,
                    NULL); --Rechazo de marca
         ELSE   
            LET v_total_marcas_dor = v_total_marcas_dor + 1;
         END IF
      ELSE
         INSERT INTO safre_tmp:uni_rch_marca 
         VALUES (v_nss_unificado,
                 9, --No existe DH
                 NULL);
      END IF
   END FOREACH;

   RETURN v_folio,
          v_error,
          err_txt,
          v_total_marcas_dor,
          v_total_marcas_ado;
END FUNCTION;


