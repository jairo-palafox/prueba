






CREATE FUNCTION "safreviv".fn_cre_integra_homologacion(p_id_cre_ctr_arh DECIMAL(9,0),
                                            p_folio DECIMAL(9,0),
                                            p_arch_proceso CHAR(100),
                                            p_usuario CHAR(20))
   RETURNING SMALLINT, CHAR(11), SMALLINT, INTEGER, INTEGER, INTEGER, INTEGER

   -- REGISTRO de trabajo
   DEFINE v_nss                     CHAR(11);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_edo_credito             SMALLINT;
   DEFINE v_tpo_credito1            DECIMAL(9,0);
   DEFINE v_num_credito1            DECIMAL(10,0);
   DEFINE v_edo_credito1            SMALLINT;
   DEFINE v_edo_homologa            SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE v_entidad                 SMALLINT;

   -- Variables auxiliares
   DEFINE v_ax_total                INTEGER;   -- total registros en archivo
   DEFINE v_ax_rechazado            INTEGER;   -- total registros no catalogados
   DEFINE v_ax_procesado            INTEGER;   -- total registros a actualizar
   DEFINE v_ax_sin_orig             INTEGER;   -- total registros sin origen o sin crédito igual
   DEFINE v_ax_aceptado             INTEGER;   -- total registros mismo crédito

   -- Totales para homologación
   DEFINE v_ax_estado               INTEGER;   -- total diferente estado
   DEFINE v_ax_tipo                 INTEGER;   -- total diferente tipo
   DEFINE v_ax_numero               INTEGER;   -- total diferente número
   DEFINE v_ax_tp_edo               INTEGER;   -- total diferente tipo y estado

   DEFINE v_ax_id_dh                SMALLINT;  -- indica si el nss existe en catálogo
   DEFINE v_ax_sin                  SMALLINT;  -- indica si el nss no tiene crédito originado
   DEFINE v_ax_dif                  SMALLINT;  -- indica si el nss tiene crédito diferente
   DEFINE v_ax_nci                  SMALLINT;  -- indica si el número de crédito es diferente
   DEFINE v_ax_tpo                  SMALLINT;  -- indica si el tipo de crédito es diferente
   DEFINE v_ax_edo                  SMALLINT;  -- indica si el estado de crédito es diferente
   DEFINE v_ax_cre                  SMALLINT;  -- indica si el nss tiene el mismo crédito
   DEFINE v_ax_tps                  SMALLINT;  -- indica si el nss tiene mismo nci y diferente
   DEFINE v_ax_hm                   SMALLINT;  -- indica si el registro se va a homologar

   DEFINE v_error                   SMALLINT;  -- en caso de error contiene el código

   DEFINE v_i_estado                SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE r_ax_bandera              SMALLINT; -- valor de regreso de la actualización

   DEFINE v_txt                     CHAR(45);  -- para ihnabilitación / habilitación de índices
   DEFINE v_qry_ins                 CHAR(500); -- para inserción en tabla histórica

   ON EXCEPTION SET v_error
      LET v_ax_estado = 0;
      LET v_ax_tipo   = 0;
      LET v_ax_numero = 0;
      LET v_ax_tp_edo = 0;

      -- Devolverá el codigo de error cuando ocurra una excepción diferente a -239
      RETURN v_error, v_nss, v_edo_credito, v_ax_estado, v_ax_tipo, v_ax_numero, v_ax_tp_edo;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/creIntegHomologa.trace';
   --TRACE ON;

   -- se establece la prioridad
   SET PDQPRIORITY HIGH;

   LET v_txt = "SET INDEXES FOR cre_homologa_trm DISABLED";
   EXECUTE IMMEDIATE v_txt;

   -- se inicializa variables
   LET v_error              = 0;

   LET v_i_estado           = 2; -- estado Integrado
   LET v_nss                = NULL;
   LET v_id_derechohabiente = 0;
   LET v_id_cre_acreditado  = 0;
   LET v_tpo_credito        = 0;
   LET v_num_credito        = 0;
   LET v_edo_credito        = 0;
   LET v_tpo_credito1       = 0;
   LET v_num_credito1       = 0;
   LET v_edo_credito1       = 0;
   LET v_edo_homologa       = 0;
   LET v_f_proceso          = TODAY;
   LET v_ax_total           = 0;
   LET v_ax_rechazado       = 0;
   LET v_ax_procesado       = 0;
   LET v_ax_sin_orig        = 0;
   LET v_ax_aceptado        = 0;
   LET v_ax_id_dh           = 11;    -- Derechohabiente no existe en maestro
   LET v_ax_sin             = 26;    -- Registro sin originación de crédito
   LET v_ax_dif             = 14;    -- Registro con crédito diferente a vigente ó liquidado
   LET v_ax_nci             = 22;    -- Registro con diferente número de crédito
   LET v_ax_tpo             = 16;    -- Registro con diferente tipo de crédito
   LET v_ax_edo             = 15;    -- Registro con diferente estado de crédito
   LET v_ax_tps             = 29;    -- Registro con mismo número de crédito diferente tipo y diferente estado
   LET v_ax_cre             = 0;     -- Registro con mismo crédito
   LET v_ax_estado          = 0;
   LET v_ax_tipo            = 0;
   LET v_ax_numero          = 0;
   LET v_ax_tp_edo          = 0;
   LET v_ax_hm              = 0;

   --------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS INEXISTENTES EN CATÁLOGO --
   --------------------------------------------------------
   FOREACH
    SELECT t.nss,
           t.tpo_credito,
           t.num_credito,
           c.entidad
      INTO v_nss,
           v_tpo_credito,
           v_num_credito,
           v_edo_credito
      FROM safre_tmp:tmp_homologa_trm t,
           cat_estado_trm c
     WHERE t.nss NOT IN (
           SELECT afi.nss
             FROM afi_derechohabiente afi
            INNER JOIN safre_tmp:tmp_homologa_trm t1
               ON afi.nss = t1.nss)
       AND t.edo_credito = c.edo_trm

      -- se asigna el estatud de error
      LET v_edo_homologa = v_ax_id_dh;

      -- se incrementa el número de registros rechazados
      LET v_ax_rechazado = v_ax_rechazado + 1;

      -- se inserta registro
      INSERT INTO safre_tmp:tmp_reg_homologa (
                  nss               ,
                  tpo_credito       ,
                  num_credito       ,
                  edo_credito       ,
                  tpo_credito1      ,
                  num_credito1      ,
                  edo_credito1      ,
                  edo_homologa      ,
                  id_cre_acreditado)
          VALUES (v_nss               ,
                  v_tpo_credito       ,
                  v_num_credito       ,
                  v_edo_credito       ,
                  v_tpo_credito1      ,
                  v_num_credito1      ,
                  v_edo_credito1      ,
                  v_edo_homologa      ,
                  v_id_cre_acreditado);
   END FOREACH;

   -----------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE HOMOLOGACIÓN --
   -----------------------------------------------
   -- se obtienen los datos de la tabla temporal

   FOREACH
    SELECT t.nss,
           t.tpo_credito,
           t.num_credito,
           c.entidad,
           a.id_derechohabiente
      INTO v_nss,
           v_tpo_credito,
           v_num_credito,
           v_edo_credito,
           v_id_derechohabiente
      FROM safre_tmp:tmp_homologa_trm t,
           afi_derechohabiente a,
           cat_estado_trm c
     WHERE t.nss = a.nss
       AND t.edo_credito = c.edo_trm
       AND t.nss IN (
           SELECT afi.nss
             FROM afi_derechohabiente afi
            INNER JOIN safre_tmp:tmp_homologa_trm t1
               ON afi.nss = t1.nss)

      -- se consulta en acreditados
      FOREACH
         SELECT c.tpo_credito,
                c.num_credito,
                m.entidad,
                c.id_cre_acreditado
           INTO v_tpo_credito1,
                v_num_credito1,
                v_edo_credito1,
                v_id_cre_acreditado
           FROM cre_acreditado c,
                cat_maq_credito m
          WHERE c.id_derechohabiente = v_id_derechohabiente
            AND c.estado = m.estado

         IF v_id_cre_acreditado IS NULL THEN
             LET v_edo_homologa = v_ax_sin;
             LET v_ax_sin_orig  = v_ax_sin_orig + 1;

             CONTINUE FOREACH;
         ELIF
            v_tpo_credito = v_tpo_credito1 AND
            v_num_credito = v_num_credito1 AND
            v_edo_credito = v_edo_credito1 THEN    --- mismo registro
             LET v_edo_homologa = v_ax_cre;
             LET v_ax_aceptado  = v_ax_aceptado + 1;

             -- se inserta registro
             INSERT INTO safre_tmp:tmp_reg_homologa (
                         nss               ,
                         tpo_credito       ,
                         num_credito       ,
                         edo_credito       ,
                         tpo_credito1      ,
                         num_credito1      ,
                         edo_credito1      ,
                         edo_homologa      ,
                         id_cre_acreditado)
                 VALUES (v_nss               ,
                         v_tpo_credito       ,
                         v_num_credito       ,
                         v_edo_credito       ,
                         v_tpo_credito1      ,
                         v_num_credito1      ,
                         v_edo_credito1      ,
                         v_edo_homologa      ,
                         v_id_cre_acreditado);

             CONTINUE FOREACH;
         ELIF
            v_tpo_credito = v_tpo_credito1 AND
            v_num_credito <> v_num_credito1 AND
            v_edo_credito = v_edo_credito1 THEN    --- mismo tipo, diferente número, mismo estado
             LET v_edo_homologa = v_ax_nci;
             LET v_ax_procesado = v_ax_procesado + 1;
             LET v_ax_numero    = v_ax_numero + 1;
         ELIF
            v_tpo_credito <> v_tpo_credito1 AND
            v_num_credito = v_num_credito1 AND
            v_edo_credito = v_edo_credito1 THEN    --- diferente tipo, mismo número, mismo estado
             LET v_edo_homologa = v_ax_tpo;
             LET v_ax_procesado = v_ax_procesado + 1;
             LET v_ax_tipo      = v_ax_tipo + 1;
             LET v_ax_hm        = 1;
         ELIF
            v_tpo_credito = v_tpo_credito1 AND
            v_num_credito = v_num_credito1 AND
            v_edo_credito <> v_edo_credito1 THEN    --- mismo tipo, mismo número, diferente estado
             LET v_edo_homologa = v_ax_edo;
             LET v_ax_procesado = v_ax_procesado + 1;
             LET v_ax_estado    = v_ax_estado + 1;
             LET v_ax_hm        = 1;
         ELIF
            v_tpo_credito <> v_tpo_credito1 AND
            v_num_credito = v_num_credito1 AND
            v_edo_credito <> v_edo_credito1 THEN    --- diferente tipo, mismo número, diferente estado
             LET v_edo_homologa = v_ax_tps;
             LET v_ax_procesado = v_ax_procesado + 1;
             LET v_ax_tp_edo    = v_ax_tp_edo + 1;
             LET v_ax_hm        = 1;
         ELSE
             LET v_edo_homologa = v_ax_sin;
             LET v_ax_sin_orig  = v_ax_sin_orig + 1;

             CONTINUE FOREACH;
         END IF

         IF v_ax_hm = 1 THEN
               -- se inserta registro
               INSERT INTO safre_tmp:tmp_reg_homologa (
                           nss               ,
                           tpo_credito       ,
                           num_credito       ,
                           edo_credito       ,
                           tpo_credito1      ,
                           num_credito1      ,
                           edo_credito1      ,
                           edo_homologa      ,
                           id_cre_acreditado)
                   VALUES (v_nss               ,
                           v_tpo_credito       ,
                           v_num_credito       ,
                           v_edo_credito       ,
                           v_tpo_credito1      ,
                           v_num_credito1      ,
                           v_edo_credito1      ,
                           v_edo_homologa      ,
                           v_id_cre_acreditado);
         END IF

         LET v_id_derechohabiente = 0;
         LET v_id_cre_acreditado  = 0;
         LET v_tpo_credito        = 0;
         LET v_num_credito        = 0;
         LET v_edo_credito        = 0;
         LET v_tpo_credito1       = 0;
         LET v_num_credito1       = 0;
         LET v_edo_credito1       = 0;
         LET v_edo_homologa       = 0;
         LET v_ax_hm              = 0;
      END FOREACH;
   END FOREACH;

   LET v_qry_ins = "INSERT INTO cre_homologa_trm "||
                   " SELECT id_cre_acreditado   ,"||
                          " tpo_credito         ,"||
                          " num_credito         ,"||
                          " edo_credito         ,"||
                          " tpo_credito1        ,"||
                          " num_credito1        ,"||
                          " edo_credito1        ,"||
                          " edo_homologa        ,"||
                            p_id_cre_ctr_arh||
                          " ,0,'',"||
                          "'"||v_f_proceso||"'"||
                   "   FROM safre_tmp:tmp_reg_homologa";

   EXECUTE IMMEDIATE v_qry_ins;

   LET v_txt = "SET INDEXES FOR cre_homologa_trm ENABLED";
   EXECUTE IMMEDIATE v_txt;

   LET v_ax_sin_orig = v_ax_sin_orig + v_ax_aceptado;

   -- actualiza estadisticas a la tabla histórica
   UPDATE STATISTICS FOR TABLE cre_homologa_trm;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_arch_proceso, p_folio, v_i_estado, p_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_folio, v_ax_procesado, v_ax_rechazado, v_ax_sin_orig, p_id_cre_ctr_arh);

   UPDATE STATISTICS FOR TABLE cre_ctr_archivo;

   -- se regresa la prioridad
   SET PDQPRIORITY DEFAULT;

   RETURN v_error, v_nss, v_edo_credito, v_ax_estado, v_ax_tipo, v_ax_numero, v_ax_tp_edo;

END FUNCTION
;


