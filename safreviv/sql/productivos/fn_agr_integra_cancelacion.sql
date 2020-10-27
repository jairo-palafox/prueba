






CREATE FUNCTION "safreviv".fn_agr_integra_cancelacion( p_usuario    LIKE seg_usuario.usuario_cod,
                                            p_folio      LIKE glo_ctr_archivo.folio,
                                            p_tipo_opera CHAR(1) )
   RETURNING SMALLINT, INTEGER, INTEGER
   -- p_usuario            Parámetro del usuario que recibe la función
   -- p_folio              Segundo parámetro que recibe (folio)
   -- p_tipo_opera         Parámetro que define el tipo de operación 

   --variables que recuperan información de las temporales
   DEFINE v_nss                       CHAR(11);
   DEFINE v_num_credito               DECIMAL(10,0);
   DEFINE v_tpo_credito               CHAR(2);
   DEFINE v_tpo_registro              CHAR(2);
   DEFINE v_ssv_92_97                 DECIMAL(8,0);
   DEFINE v_fec_otorgamiento          DATE;
   DEFINE v_fec_culminacion           DATE;
   DEFINE v_sts_credito               SMALLINT;
   DEFINE v_tpo_descuento             SMALLINT;
   DEFINE v_val_descuento             DECIMAL(8,0);
   DEFINE v_nrp                       CHAR(11);
   DEFINE v_fec_ini_oblig_patron      DATE;
   DEFINE v_nss_liberado              CHAR(11);
   DEFINE v_fec_proceso               DATE;
   DEFINE v_sdo_credito               DECIMAL(8,0);
   DEFINE v_fec_prox_liquidar         DATE;
   DEFINE v_fec_dsd_avis_desast       DATE;
   DEFINE v_fec_hst_avis_desast       DATE;
   DEFINE v_tpo_rechazo               DECIMAL(2,0);

   DEFINE v_id_derechohabiente        LIKE afi_derechohabiente.id_derechohabiente;
   DEFINE v_id_cre_acreditado         LIKE cre_acreditado.id_cre_acreditado;
   DEFINE v_entidad                   LIKE cat_maq_credito.entidad;
   DEFINE v_estado                    LIKE cre_acreditado.estado;
   DEFINE v_edo_procesar              LIKE cre_acreditado.edo_procesar;
   DEFINE v_f_otorga                  LIKE cre_acreditado.f_otorga;
   DEFINE v_tpo_originacion           LIKE cre_acreditado.tpo_originacion;
   DEFINE v_tpo_originacion_procesar  CHAR(4);
   DEFINE v_marca_inf                 LIKE cat_tipo_credito.marca_inf;
   DEFINE v_marca_prc                 LIKE cat_tipo_credito.marca_prc;
   DEFINE v_n_referencia              LIKE sfr_marca_activa.n_referencia;
   DEFINE v_n_referencia_procesar     LIKE sfr_marca_activa.n_referencia;
   DEFINE v_id_cre_ctr_archivo        LIKE cre_ctr_archivo.id_cre_ctr_archivo;
   DEFINE v_f_proceso                 LIKE cre_ctr_archivo.f_proceso;
   DEFINE v_ax_tpo_transferencia      CHAR(2);
   DEFINE v_cnt_cta_credito           SMALLINT;
   DEFINE v_error                     SMALLINT;
   DEFINE v_ax_cod_error              SMALLINT; --variable del codigo de error de fn_desmarca_cuenta

   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo      DECIMAL(9,0);
   DEFINE rch_nss                     CHAR(11);
   DEFINE rch_tpo_originacion         SMALLINT;
   DEFINE rch_tpo_credito             SMALLINT;
   DEFINE rch_tpo_registro            CHAR(2);
   DEFINE rch_num_credito             DECIMAL(10,0);
   DEFINE rch_sdo_deudor              DECIMAL(12,2);
   DEFINE rch_f_otorga                DATE;
   DEFINE rch_f_culmina               DATE;
   DEFINE rch_edo_credito             SMALLINT;
   DEFINE rch_tpo_dscto               SMALLINT;
   DEFINE rch_valor_dscto             DECIMAL(12,4);
   DEFINE rch_nrp                     CHAR(11);
   DEFINE rch_f_ini_dscto             DATE;
   DEFINE rch_nss_liberado            CHAR(11);
   DEFINE rch_f_gen_arh               DATE;
   DEFINE rch_sdo_credito             DECIMAL(12,2);
   DEFINE rch_f_prox_liq              DATE;
   DEFINE rch_f_desde                 DATE;
   DEFINE rch_f_hasta                 DATE;
   DEFINE rch_tpo_rch                 SMALLINT;
   DEFINE rch_estado                  SMALLINT;

   DEFINE v_cnt_rech                  SMALLINT;     -- contador para los rechazados
   DEFINE v_cnt_acept                 SMALLINT;     -- contador para los aceptados

   -- Campos desmarca 225
   DEFINE v_dm_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_dm_marca_entra          SMALLINT;
   DEFINE v_dm_n_referencia         INTEGER;
   DEFINE v_dm_estado_marca         SMALLINT;
   DEFINE v_dm_marca_causa          SMALLINT;
   DEFINE v_dm_usuario              CHAR(12);
   DEFINE v_dm_proceso_cod          SMALLINT;

   ON EXCEPTION SET v_error
      -- LET v_cnt_rech   = 0;
      -- LET v_cnt_acept  = 0;

      RETURN v_error, v_cnt_acept, v_cnt_rech;
   END EXCEPTION;

   ---SET DEBUG FILE TO "/safreviv_int/archivos/agrIntegraCancelacion.trace";
   ---TRACE ON;

   --Se inicializan los contadores en cero.
   LET v_cnt_rech             = 0;
   LET v_cnt_acept            = 0;
   LET v_error                = 0;
   LET v_ax_tpo_transferencia = "43";
   LET v_ax_cod_error         = "";

   LET v_dm_estado_marca      = 0;
   LET v_dm_marca_causa       = 0;
   LET v_dm_usuario           = p_usuario;
   LET v_dm_proceso_cod       = 308;

   --Recupera identificador del archivo
   SELECT id_cre_ctr_archivo,
          f_proceso
     INTO v_id_cre_ctr_archivo,
          v_f_proceso
     FROM cre_ctr_archivo
    WHERE folio_archivo = p_folio;

   IF p_tipo_opera = 'M' THEN 
      FOREACH
         --verifica los registros a cancelar, toma los registros de la tabla tmp_cre_acred_agr_20,en esta tabla sólo se guardan los registros con tipo registro 20l,
         --además se toman solo los registros que tengan sts_credito = 9
         SELECT tpo_registro,
                nss,
                num_credito,
                ssv_92_97,
                fec_otorgamiento,
                fec_culminacion,
                tpo_credito,
                sts_credito,
                tpo_descuento,
                val_descuento,
                nrp,
                fec_ini_oblig_patron,
                nss_liberado,
                fec_proceso,
                sdo_credito,
                fec_prox_liquidar,
                fec_dsd_avis_desast,
                fec_hst_avis_desast,
                tpo_rechazo
           INTO v_tpo_registro,
                v_nss,
                v_num_credito,
                v_ssv_92_97,
                v_fec_otorgamiento,
                v_fec_culminacion,
                v_tpo_credito,
                v_sts_credito,
                v_tpo_descuento,
                v_val_descuento,
                v_nrp,
                v_fec_ini_oblig_patron,
                v_nss_liberado,
                v_fec_proceso,
                v_sdo_credito,
                v_fec_prox_liquidar,
                v_fec_dsd_avis_desast,
                v_fec_hst_avis_desast,
                v_tpo_rechazo
           FROM safre_tmp:tmp_cre_acred_agr_20
          WHERE tpo_registro = "20"
            AND sts_credito = 9

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = v_id_cre_ctr_archivo;
         LET rch_nss                = v_nss;
         LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
         LET rch_tpo_credito        = v_tpo_credito; 
         LET rch_tpo_registro       = v_tpo_registro;
         LET rch_num_credito        = v_num_credito;
         LET rch_sdo_deudor         = v_ssv_92_97/100;
         LET rch_f_otorga           = v_fec_otorgamiento;
         LET rch_f_culmina          = v_fec_culminacion;
         LET rch_edo_credito        = v_sts_credito;
         LET rch_tpo_dscto          = v_tpo_descuento;
         LET rch_valor_dscto        = v_val_descuento/10000; 
         LET rch_nrp                = v_nrp;
         LET rch_f_ini_dscto        = v_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_nss_liberado;
         LET rch_f_gen_arh          = v_fec_proceso;
         LET rch_sdo_credito        = v_sdo_credito/100;
         LET rch_f_prox_liq         = v_fec_prox_liquidar;
         LET rch_f_desde            = v_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_tpo_rechazo;
         LET rch_estado             = 0;            -- Inicializado como procedente

         --Buscar el id_derechohabiente con el NSS,
         SELECT id_derechohabiente 
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;
         --sí no existe el derechohabiente en afi_derechohabiente se rechaza y sigue con el foreach 

         IF v_id_derechohabiente IS NULL THEN 
            LET rch_estado = 11;  --TRABAJADOR NO EXISTE

            -- se inserta en la tabla de rechazos
            INSERT INTO cre_rch_acreditado (
                        id_cre_ctr_archivo,
                        nss,
                        tpo_originacion,
                        tpo_credito,
                        tpo_registro,
                        num_credito,
                        sdo_deudor,
                        f_otorga,
                        f_culmina,
                        edo_credito,
                        tpo_dscto,
                        valor_dscto,
                        nrp,
                        f_ini_dscto,
                        nss_liberado,
                        f_gen_arh,
                        sdo_credito,
                        f_prox_liq,
                        f_desde,
                        f_hasta,
                        tpo_rch,
                        estado)
                VALUES (rch_id_cre_ctr_archivo,
                        rch_nss,
                        rch_tpo_originacion,
                        rch_tpo_credito,
                        rch_tpo_registro,
                        rch_num_credito,
                        rch_sdo_deudor,
                        rch_f_otorga,
                        rch_f_culmina,
                        rch_edo_credito,
                        rch_tpo_dscto,
                        rch_valor_dscto,
                        rch_nrp,
                        rch_f_ini_dscto,
                        rch_nss_liberado,
                        rch_f_gen_arh,
                        rch_sdo_credito,
                        rch_f_prox_liq,
                        rch_f_desde,
                        rch_f_hasta,
                        rch_tpo_rch,
                        rch_estado);

            --Se rechaza la cancelacion y continua con el foreach
            LET v_cnt_rech = v_cnt_rech + 1;
            CONTINUE FOREACH;
         ELSE
            --LET v_cnt_acept = v_cnt_acept + 1
            SELECT cre.id_cre_acreditado,
                   ent.entidad,
                   cre.estado,
                   cre.edo_procesar,
                   cre.f_otorga,
                   DECODE(cre.tpo_originacion,1,"03",2,"16",4,"43"),
                   DECODE(cre.tpo_originacion,1,"01",2,"02",4,"04"),
                   mrc.marca_inf,
                   mrc.marca_prc
              INTO v_id_cre_acreditado,
                   v_entidad,
                   v_estado,
                   v_edo_procesar,
                   v_f_otorga,
                   v_tpo_originacion,
                   v_tpo_originacion_procesar,
                   v_marca_inf,
                   v_marca_prc
              FROM cre_acreditado cre,       -- tabla principal acreditados
                   cat_maq_credito ent,      -- maquinaria de estados
                   cat_tipo_credito mrc      -- catálogo de tipos de crédito
             WHERE cre.id_derechohabiente = v_id_derechohabiente
               AND cre.tpo_credito        = v_tpo_credito
               AND cre.num_credito        = v_num_credito
               AND cre.estado             = ent.estado
               AND cre.tpo_originacion    = mrc.tpo_originacion
               AND cre.tpo_credito        = mrc.tpo_credito  ;

            IF  v_id_cre_acreditado IS NULL THEN 
               LET rch_estado = 13;  -- NO EXISTE MARCA CRÉDITO VIGENTE

               -- se inserta en la tabla de rechazos
               INSERT INTO cre_rch_acreditado (
                           id_cre_ctr_archivo,
                           nss,
                           tpo_originacion,
                           tpo_credito,
                           tpo_registro,
                           num_credito,
                           sdo_deudor,
                           f_otorga,
                           f_culmina,
                           edo_credito,
                           tpo_dscto,
                           valor_dscto,
                           nrp,
                           f_ini_dscto,
                           nss_liberado,
                           f_gen_arh,
                           sdo_credito,
                           f_prox_liq,
                           f_desde,
                           f_hasta,
                           tpo_rch,
                           estado)
                   VALUES (rch_id_cre_ctr_archivo,
                           rch_nss,
                           rch_tpo_originacion,
                           rch_tpo_credito,
                           rch_tpo_registro,
                           rch_num_credito,
                           rch_sdo_deudor,
                           rch_f_otorga,
                           rch_f_culmina,
                           rch_edo_credito,
                           rch_tpo_dscto,
                           rch_valor_dscto,
                           rch_nrp,
                           rch_f_ini_dscto,
                           rch_nss_liberado,
                           rch_f_gen_arh,
                           rch_sdo_credito,
                           rch_f_prox_liq,
                           rch_f_desde,
                           rch_f_hasta,
                           rch_tpo_rch,
                           rch_estado);

               --si no existe se rechaza
               LET v_cnt_rech = v_cnt_rech + 1;
               CONTINUE FOREACH;
            ELSE 
               IF v_entidad = 1 THEN
                  SELECT id_cre_ctr_archivo,
                         f_proceso
                    INTO v_id_cre_ctr_archivo,
                         v_f_proceso
                    FROM cre_ctr_archivo
                   WHERE folio_archivo = p_folio;

                  -- se inserta en cre_his_acreditado
                  INSERT INTO cre_his_acreditado ( id_cre_acreditado,
                                                   id_cre_ctr_archivo,
                                                   tpo_transferencia,
                                                   edo_procesar,
                                                   estado,
                                                   f_proceso)
                                           VALUES( v_id_cre_acreditado,
                                                   v_id_cre_ctr_archivo,
                                                   v_ax_tpo_transferencia,
                                                   v_edo_procesar,
                                                   174,
                                                   v_f_proceso);

                  UPDATE cre_acreditado
                    SET estado = 174
                  WHERE id_cre_acreditado = v_id_cre_acreditado;

                  --Busca en cta_credito si hay información para el id_derechohabiente, 
                  --el tipo de crédito y el número de crédito, si hay se debe borrar el registro
                  SELECT COUNT(*)
                    INTO v_cnt_cta_credito
                    FROM cta_credito
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND tpo_credito = v_tpo_credito
                     AND num_credito = v_num_credito;

                  IF v_cnt_cta_credito >= 1 THEN
                      INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                           301,
                                                           v_tpo_credito,
                                                           v_num_credito,
                                                           v_f_otorga,
                                                           5,
                                                           TODAY );
                      DELETE
                        FROM cta_credito
                       WHERE id_derechohabiente = v_id_derechohabiente
                         AND tpo_credito = v_tpo_credito
                         AND num_credito = v_num_credito;
                  ELSE
                      INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                           301,
                                                           v_tpo_credito,
                                                           v_num_credito,
                                                           v_f_otorga,
                                                           5,
                                                           TODAY );
                  END IF;

                  FOREACH
                     --Se verifica que exista una marca interna para el id_derechohabiente y la marca que le corresponda
                     SELECT n_referencia
                       INTO v_n_referencia
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND marca = v_marca_inf
                        
                     --si la marca existe, se desmarca llamando a la funcion fn_desmarca_cuenta
                     IF v_n_referencia IS NOT NULL THEN
                        EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                            v_marca_inf,v_n_referencia,
                                                            0,
                                                            0,
                                                            p_usuario,
                                                            301) 
                                                       INTO v_ax_cod_error;
                     END IF;
                  END FOREACH;

                  DELETE
                    FROM cta_marca_ws
                   WHERE id_derechohabiente =  v_id_derechohabiente
                     AND id_origen          =  v_id_cre_acreditado
                     AND modulo_cod         =  v_tpo_originacion
                     AND tpo_Credito        =  v_tpo_credito  ;

                  INSERT INTO cta_marca_ws VALUES(v_id_derechohabiente,
                                                  v_id_cre_acreditado,
                                                  v_tpo_originacion,
                                                  v_tpo_credito,
                                                  v_marca_prc,
                                                  TODAY,
                                                  1,
                                                  "",
                                                  "",
                                                  0,
                                                  v_num_credito,
                                                  v_f_otorga,
                                                  v_tpo_originacion_procesar,
                                                  p_folio,
                                                  p_usuario
                                                  );
               ELSE
                  LET rch_estado = 15;  -- ESTADO DEL CRÉDITO NO VÁLIDO

                  -- se inserta en la tabla de rechazos
                  INSERT INTO cre_rch_acreditado (
                              id_cre_ctr_archivo,
                              nss,
                              tpo_originacion,
                              tpo_credito,
                              tpo_registro,
                              num_credito,
                              sdo_deudor,
                              f_otorga,
                              f_culmina,
                              edo_credito,
                              tpo_dscto,
                              valor_dscto,
                              nrp,
                              f_ini_dscto,
                              nss_liberado,
                              f_gen_arh,
                              sdo_credito,
                              f_prox_liq,
                              f_desde,
                              f_hasta,
                              tpo_rch,
                              estado)
                      VALUES (rch_id_cre_ctr_archivo,
                              rch_nss,
                              rch_tpo_originacion,
                              rch_tpo_credito,
                              rch_tpo_registro,
                              rch_num_credito,
                              rch_sdo_deudor,
                              rch_f_otorga,
                              rch_f_culmina,
                              rch_edo_credito,
                              rch_tpo_dscto,
                              rch_valor_dscto,
                              rch_nrp,
                              rch_f_ini_dscto,
                              rch_nss_liberado,
                              rch_f_gen_arh,
                              rch_sdo_credito,
                              rch_f_prox_liq,
                              rch_f_desde,
                              rch_f_hasta,
                              rch_tpo_rch,
                              rch_estado);

                  LET v_cnt_rech = v_cnt_rech + 1;
                  CONTINUE FOREACH;
               END IF;
            END IF;

            LET v_cnt_acept = v_cnt_acept + 1;

            FOREACH
               SELECT id_derechohabiente,
                      marca,
                      n_referencia
                 INTO v_dm_id_derechohabiente,
                      v_dm_marca_entra,
                      v_dm_n_referencia
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND marca              = 225

               -- se invoca la función de desmarca
               EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                                   v_dm_marca_entra,
                                                   v_dm_n_referencia,
                                                   v_dm_estado_marca,
                                                   v_dm_marca_causa,
                                                   v_dm_usuario,
                                                   v_dm_proceso_cod)
                                              INTO v_ax_cod_error;
            END FOREACH;
         END IF;
      END FOREACH;

   ELIF p_tipo_opera = 'A' THEN 

      FOREACH
         --verifica los registros a cancelar, toma los registros de la tabla tmp_desmarca_det_agr_20
         --además se toman solo los registros que tengan sts_credito = 9
          SELECT tpo_registro,
                nss,
                num_credito,
                ssv_92_97,
                fec_otorgamiento,
                fec_culminacion,
                tpo_credito,
                sts_credito,
                tpo_descuento,
                val_descuento,
                nrp,
                fec_ini_oblig_patron,
                nss_liberado,
                fec_proceso,
                sdo_credito,
                fec_prox_liquidar,
                fec_dsd_avis_desast,
                fec_hst_avis_desast,
                tpo_rechazo
           INTO v_tpo_registro,
                v_nss,
                v_num_credito,
                v_ssv_92_97,
                v_fec_otorgamiento,
                v_fec_culminacion,
                v_tpo_credito,
                v_sts_credito,
                v_tpo_descuento,
                v_val_descuento,
                v_nrp,
                v_fec_ini_oblig_patron,
                v_nss_liberado,
                v_fec_proceso,
                v_sdo_credito,
                v_fec_prox_liquidar,
                v_fec_dsd_avis_desast,
                v_fec_hst_avis_desast,
                v_tpo_rechazo
           FROM safre_tmp:tmp_desmarca_det_agr_20
          WHERE tpo_registro = "20"
            AND sts_credito = 9

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = v_id_cre_ctr_archivo;
         LET rch_nss                = v_nss;
         LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
         LET rch_tpo_credito        = v_tpo_credito; 
         LET rch_tpo_registro       = v_tpo_registro;
         LET rch_num_credito        = v_num_credito;
         LET rch_sdo_deudor         = v_ssv_92_97/100;
         LET rch_f_otorga           = v_fec_otorgamiento;
         LET rch_f_culmina          = v_fec_culminacion;
         LET rch_edo_credito        = v_sts_credito;
         LET rch_tpo_dscto          = v_tpo_descuento;
         LET rch_valor_dscto        = v_val_descuento/10000; 
         LET rch_nrp                = v_nrp;
         LET rch_f_ini_dscto        = v_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_nss_liberado;
         LET rch_f_gen_arh          = v_fec_proceso;
         LET rch_sdo_credito        = v_sdo_credito/100;
         LET rch_f_prox_liq         = v_fec_prox_liquidar;
         LET rch_f_desde            = v_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_tpo_rechazo;
         LET rch_estado             = 0;             -- Inicializado como procedente

         --Buscar el id_derechohabiente con el NSS,
         SELECT id_derechohabiente 
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         --sí no existe el derechohabiente en afi_derechohabiente se rechaza y sigue con el foreach 
         IF v_id_derechohabiente IS NULL THEN 
            LET rch_estado = 11;  --TRABAJADOR NO EXISTE

            -- se inserta en la tabla de rechazos
            INSERT INTO cre_rch_acreditado (
                        id_cre_ctr_archivo,
                        nss,
                        tpo_originacion,
                        tpo_credito,
                        tpo_registro,
                        num_credito,
                        sdo_deudor,
                        f_otorga,
                        f_culmina,
                        edo_credito,
                        tpo_dscto,
                        valor_dscto,
                        nrp,
                        f_ini_dscto,
                        nss_liberado,
                        f_gen_arh,
                        sdo_credito,
                        f_prox_liq,
                        f_desde,
                        f_hasta,
                        tpo_rch,
                        estado)
                VALUES (rch_id_cre_ctr_archivo,
                        rch_nss,
                        rch_tpo_originacion,
                        rch_tpo_credito,
                        rch_tpo_registro,
                        rch_num_credito,
                        rch_sdo_deudor,
                        rch_f_otorga,
                        rch_f_culmina,
                        rch_edo_credito,
                        rch_tpo_dscto,
                        rch_valor_dscto,
                        rch_nrp,
                        rch_f_ini_dscto,
                        rch_nss_liberado,
                        rch_f_gen_arh,
                        rch_sdo_credito,
                        rch_f_prox_liq,
                        rch_f_desde,
                        rch_f_hasta,
                        rch_tpo_rch,
                        rch_estado);

            --Se rechaza la cancelación y continua con el foreach
            LET v_cnt_rech = v_cnt_rech + 1;
            CONTINUE FOREACH;
         ELSE
            --LET v_cnt_acept = v_cnt_acept + 1
            SELECT cre.id_cre_acreditado,
                   ent.entidad,
                   cre.estado,
                   cre.edo_procesar,
                   cre.f_otorga,
                   DECODE(cre.tpo_originacion,1,"03",2,"16",4,"43"),
                   DECODE(cre.tpo_originacion,1,"01",2,"02",4,"04"),
                   mrc.marca_inf,
                   mrc.marca_prc
              INTO v_id_cre_acreditado,
                   v_entidad,
                   v_estado,
                   v_edo_procesar,
                   v_f_otorga,
                   v_tpo_originacion,
                   v_tpo_originacion_procesar,
                   v_marca_inf,
                   v_marca_prc
              FROM cre_acreditado cre,       -- tabla principal acreditados
                   cat_maq_credito ent,      -- maquinaria de estados
                   cat_tipo_credito mrc      -- catálogo de tipos de crédito
             WHERE cre.id_derechohabiente = v_id_derechohabiente
               AND cre.tpo_credito        = v_tpo_credito
               AND cre.num_credito        = v_num_credito
               AND cre.estado             = ent.estado
               AND cre.tpo_originacion    = mrc.tpo_originacion
               AND cre.tpo_credito        = mrc.tpo_credito  ;

            IF  v_id_cre_acreditado IS NULL THEN
               LET rch_estado = 13;  -- NO EXISTE MARCA CRÉDITO VIGENTE

               -- se inserta en la tabla de rechazos
               INSERT INTO cre_rch_acreditado (
                           id_cre_ctr_archivo,
                           nss,
                           tpo_originacion,
                           tpo_credito,
                           tpo_registro,
                           num_credito,
                           sdo_deudor,
                           f_otorga,
                           f_culmina,
                           edo_credito,
                           tpo_dscto,
                           valor_dscto,
                           nrp,
                           f_ini_dscto,
                           nss_liberado,
                           f_gen_arh,
                           sdo_credito,
                           f_prox_liq,
                           f_desde,
                           f_hasta,
                           tpo_rch,
                           estado)
                   VALUES (rch_id_cre_ctr_archivo,
                           rch_nss,
                           rch_tpo_originacion,
                           rch_tpo_credito,
                           rch_tpo_registro,
                           rch_num_credito,
                           rch_sdo_deudor,
                           rch_f_otorga,
                           rch_f_culmina,
                           rch_edo_credito,
                           rch_tpo_dscto,
                           rch_valor_dscto,
                           rch_nrp,
                           rch_f_ini_dscto,
                           rch_nss_liberado,
                           rch_f_gen_arh,
                           rch_sdo_credito,
                           rch_f_prox_liq,
                           rch_f_desde,
                           rch_f_hasta,
                           rch_tpo_rch,
                           rch_estado);

               --si no existe se rechaza
               LET v_cnt_rech = v_cnt_rech + 1;
               CONTINUE FOREACH;
            ELSE
               IF v_entidad = 1 THEN
                  SELECT id_cre_ctr_archivo,
                         f_proceso
                    INTO v_id_cre_ctr_archivo,
                         v_f_proceso
                    FROM cre_ctr_archivo
                   WHERE folio_archivo = p_folio;

                  -- se inserta en cre_his_acreditado
                  INSERT INTO cre_his_acreditado ( id_cre_acreditado,
                                                   id_cre_ctr_archivo,
                                                   tpo_transferencia,
                                                   edo_procesar,
                                                   estado,
                                                   f_proceso)
                                           VALUES( v_id_cre_acreditado,
                                                   v_id_cre_ctr_archivo,
                                                   v_ax_tpo_transferencia,
                                                   v_edo_procesar,
                                                   174,
                                                   v_f_proceso);

                  UPDATE cre_acreditado
                     SET estado = 174
                  WHERE id_cre_acreditado = v_id_cre_acreditado;

                  --Busca en cta_credito si hay información para el id_derechohabiente, 
                  --el tipo de crédito y el número de crédito, si hay se debe borrar el registro
                  SELECT COUNT(*)
                    INTO v_cnt_cta_credito
                    FROM cta_credito
                   WHERE id_derechohabiente = v_id_derechohabiente
                     AND tpo_credito = v_tpo_credito
                     AND num_credito = v_num_credito;

                  IF v_cnt_cta_credito >= 1 THEN
                      INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                           301,
                                                           v_tpo_credito,
                                                           v_num_credito,
                                                           v_f_otorga,
                                                           5,
                                                           TODAY );

                      DELETE
                        FROM cta_credito
                       WHERE id_derechohabiente = v_id_derechohabiente
                         AND tpo_credito = v_tpo_credito
                         AND num_credito = v_num_credito;
                  ELSE
                      INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                           301,
                                                           v_tpo_credito,
                                                           v_num_credito,
                                                           v_f_otorga,
                                                           5,
                                                           TODAY );
                  END IF;

                  FOREACH
                     --Se verifica que exista una marca interna para el id_derechohabiente y la marca que le corresponda
                     SELECT n_referencia
                       INTO v_n_referencia
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND marca = v_marca_inf

                     --si la marca existe, se desmarca llamando a la funcion fn_desmarca_cuenta
                     IF v_n_referencia IS NOT NULL THEN
                        EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                            v_marca_inf,v_n_referencia,
                                                            0,
                                                            0,
                                                            p_usuario,
                                                            301) 
                                                       INTO v_ax_cod_error;
                     END IF;
                  END FOREACH;

                  DELETE
                    FROM cta_marca_ws
                   WHERE id_derechohabiente =  v_id_derechohabiente
                     AND id_origen          =  v_id_cre_acreditado
                     AND modulo_cod         =  v_tpo_originacion
                     AND tpo_Credito        =  v_tpo_credito  ;

                  INSERT INTO cta_marca_ws VALUES(v_id_derechohabiente,
                                                  v_id_cre_acreditado,
                                                  v_tpo_originacion,
                                                  v_tpo_credito,
                                                  v_marca_prc,
                                                  TODAY,
                                                  1,
                                                  "",
                                                  "",
                                                  0,
                                                  v_num_credito,
                                                  v_f_otorga,
                                                  v_tpo_originacion_procesar,
                                                  p_folio,
                                                  p_usuario
                                                  );
               ELSE
                  LET rch_estado = 15;  -- ESTADO DEL CRÉDITO NO VÁLIDO

                  -- se inserta en la tabla de rechazos
                  INSERT INTO cre_rch_acreditado (
                              id_cre_ctr_archivo,
                              nss,
                              tpo_originacion,
                              tpo_credito,
                              tpo_registro,
                              num_credito,
                              sdo_deudor,
                              f_otorga,
                              f_culmina,
                              edo_credito,
                              tpo_dscto,
                              valor_dscto,
                              nrp,
                              f_ini_dscto,
                              nss_liberado,
                              f_gen_arh,
                              sdo_credito,
                              f_prox_liq,
                              f_desde,
                              f_hasta,
                              tpo_rch,
                              estado)
                      VALUES (rch_id_cre_ctr_archivo,
                              rch_nss,
                              rch_tpo_originacion,
                              rch_tpo_credito,
                              rch_tpo_registro,
                              rch_num_credito,
                              rch_sdo_deudor,
                              rch_f_otorga,
                              rch_f_culmina,
                              rch_edo_credito,
                              rch_tpo_dscto,
                              rch_valor_dscto,
                              rch_nrp,
                              rch_f_ini_dscto,
                              rch_nss_liberado,
                              rch_f_gen_arh,
                              rch_sdo_credito,
                              rch_f_prox_liq,
                              rch_f_desde,
                              rch_f_hasta,
                              rch_tpo_rch,
                              rch_estado);

                  LET v_cnt_rech = v_cnt_rech + 1;
                  CONTINUE FOREACH;
               END IF;
            END IF;

            LET v_cnt_acept = v_cnt_acept + 1;

            FOREACH
               SELECT id_derechohabiente,
                      marca,
                      n_referencia
                 INTO v_dm_id_derechohabiente,
                      v_dm_marca_entra,
                      v_dm_n_referencia
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND marca              = 225

               -- se invoca la función de desmarca
               EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                                   v_dm_marca_entra,
                                                   v_dm_n_referencia,
                                                   v_dm_estado_marca,
                                                   v_dm_marca_causa,
                                                   v_dm_usuario,
                                                   v_dm_proceso_cod)
                                              INTO v_ax_cod_error;
            END FOREACH;
         END IF;
      END FOREACH;

   ELSE

      --  Segundo foreach para la validacion de los estado 5 y 0
      FOREACH
         SELECT tpo_registro,
                nss,
                num_credito,
                ssv_92_97,
                fec_otorgamiento,
                fec_culminacion,
                tpo_credito,
                sts_credito,
                tpo_descuento,
                val_descuento,
                nrp,
                fec_ini_oblig_patron,
                nss_liberado,
                fec_proceso,
                sdo_credito,
                fec_prox_liquidar,
                fec_dsd_avis_desast,
                fec_hst_avis_desast,
                tpo_rechazo
           INTO v_tpo_registro,
                v_nss,
                v_num_credito,
                v_ssv_92_97,
                v_fec_otorgamiento,
                v_fec_culminacion,
                v_tpo_credito,
                v_sts_credito,
                v_tpo_descuento,
                v_val_descuento,
                v_nrp,
                v_fec_ini_oblig_patron,
                v_nss_liberado,
                v_fec_proceso,
                v_sdo_credito,
                v_fec_prox_liquidar,
                v_fec_dsd_avis_desast,
                v_fec_hst_avis_desast,
                v_tpo_rechazo
           FROM safre_tmp:tmp_desmarca_det_agr_05
          WHERE tpo_registro = "05"
            AND sts_credito  = 0

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = v_id_cre_ctr_archivo;
         LET rch_nss                = v_nss;
         LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
         LET rch_tpo_credito        = v_tpo_credito; 
         LET rch_tpo_registro       = v_tpo_registro;
         LET rch_num_credito        = v_num_credito;
         LET rch_sdo_deudor         = v_ssv_92_97/100;
         LET rch_f_otorga           = v_fec_otorgamiento;
         LET rch_f_culmina          = v_fec_culminacion;
         LET rch_edo_credito        = v_sts_credito;
         LET rch_tpo_dscto          = v_tpo_descuento;
         LET rch_valor_dscto        = v_val_descuento/10000; 
         LET rch_nrp                = v_nrp;
         LET rch_f_ini_dscto        = v_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_nss_liberado;
         LET rch_f_gen_arh          = v_fec_proceso;
         LET rch_sdo_credito        = v_sdo_credito/100;
         LET rch_f_prox_liq         = v_fec_prox_liquidar;
         LET rch_f_desde            = v_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_tpo_rechazo;
         LET rch_estado             = 0;             -- Inicializado como procedente

         --Buscar el id_derechohabiente con el NSS,
         SELECT id_derechohabiente 
           INTO v_id_derechohabiente
           FROM afi_derechohabiente
          WHERE nss = v_nss;

         --sí no existe el derechohabiente en afi_derechohabiente se rechaza y sigue con el foreach 
         IF v_id_derechohabiente IS NULL THEN 
            LET rch_estado = 11;  --TRABAJADOR NO EXISTE

            INSERT INTO safre_tmp:tmp_nss_desmarcados_agr(
                        nss,
                        tpo_credito,
                        estado)
                VALUES (v_nss,
                        v_tpo_credito,
                        3);

            -- se inserta en la tabla de rechazos
            INSERT INTO cre_rch_acreditado (
                        id_cre_ctr_archivo,
                        nss,
                        tpo_originacion,
                        tpo_credito,
                        tpo_registro,
                        num_credito,
                        sdo_deudor,
                        f_otorga,
                        f_culmina,
                        edo_credito,
                        tpo_dscto,
                        valor_dscto,
                        nrp,
                        f_ini_dscto,
                        nss_liberado,
                        f_gen_arh,
                        sdo_credito,
                        f_prox_liq,
                        f_desde,
                        f_hasta,
                        tpo_rch,
                        estado)
                VALUES (rch_id_cre_ctr_archivo,
                        rch_nss,
                        rch_tpo_originacion,
                        rch_tpo_credito,
                        rch_tpo_registro,
                        rch_num_credito,
                        rch_sdo_deudor,
                        rch_f_otorga,
                        rch_f_culmina,
                        rch_edo_credito,
                        rch_tpo_dscto,
                        rch_valor_dscto,
                        rch_nrp,
                        rch_f_ini_dscto,
                        rch_nss_liberado,
                        rch_f_gen_arh,
                        rch_sdo_credito,
                        rch_f_prox_liq,
                        rch_f_desde,
                        rch_f_hasta,
                        rch_tpo_rch,
                        rch_estado);

            --Se rechaza la cancelacion y continua con el foreach
            LET v_cnt_rech = v_cnt_rech + 1;
            CONTINUE FOREACH;
         ELSE
            IF NOT EXISTS(SELECT cre.id_derechohabiente
                            FROM cre_acreditado cre,
                                 cat_maq_credito ent,
                                 cat_tipo_credito mrc
                           WHERE cre.id_derechohabiente = v_id_derechohabiente
                             AND cre.tpo_credito        = v_tpo_credito
                             AND cre.num_credito        = v_num_credito
                             AND cre.estado             = ent.estado
                             AND ent.entidad IN(1,3)
                             AND cre.tpo_originacion    = mrc.tpo_originacion
                             AND cre.tpo_credito        = mrc.tpo_credito) THEN

               LET rch_estado = 11;  --TRABAJADOR NO EXISTE

               INSERT INTO safre_tmp:tmp_nss_desmarcados_agr(
                           nss,
                           tpo_credito,
                           estado)
                   VALUES (v_nss,
                           v_tpo_credito,
                           3);

               -- se inserta en la tabla de rechazos
               INSERT INTO cre_rch_acreditado (
                           id_cre_ctr_archivo,
                           nss,
                           tpo_originacion,
                           tpo_credito,
                           tpo_registro,
                           num_credito,
                           sdo_deudor,
                           f_otorga,
                           f_culmina,
                           edo_credito,
                           tpo_dscto,
                           valor_dscto,
                           nrp,
                           f_ini_dscto,
                           nss_liberado,
                           f_gen_arh,
                           sdo_credito,
                           f_prox_liq,
                           f_desde,
                           f_hasta,
                           tpo_rch,
                           estado)
                   VALUES (rch_id_cre_ctr_archivo,
                           rch_nss,
                           rch_tpo_originacion,
                           rch_tpo_credito,
                           rch_tpo_registro,
                           rch_num_credito,
                           rch_sdo_deudor,
                           rch_f_otorga,
                           rch_f_culmina,
                           rch_edo_credito,
                           rch_tpo_dscto,
                           rch_valor_dscto,
                           rch_nrp,
                           rch_f_ini_dscto,
                           rch_nss_liberado,
                           rch_f_gen_arh,
                           rch_sdo_credito,
                           rch_f_prox_liq,
                           rch_f_desde,
                           rch_f_hasta,
                           rch_tpo_rch,
                           rch_estado);

               --Se rechaza la cancelacion y continua con el foreach
               LET v_cnt_rech = v_cnt_rech + 1;
               CONTINUE FOREACH;
            ELSE
               FOREACH
                  -- Modifica: EAS Fecha: 24/06/2019
                  -- Debido a que el dh tiene más de un crédito vigente con el mismo número de crédito por originación en Mricroflujo.
                  SELECT cre.id_cre_acreditado,
                         ent.entidad,
                         cre.estado,
                         cre.edo_procesar,
                         cre.f_otorga,
                         DECODE(cre.tpo_originacion,1,"03",2,"16",4,"43"),
                         DECODE(cre.tpo_originacion,1,"01",2,"02",4,"04"),
                         mrc.marca_inf,
                         mrc.marca_prc
                    INTO v_id_cre_acreditado,
                         v_entidad,
                         v_estado,
                         v_edo_procesar,
                         v_f_otorga,
                         v_tpo_originacion,
                         v_tpo_originacion_procesar,
                         v_marca_inf,
                         v_marca_prc
                    FROM cre_acreditado cre,       -- tabla principal acreditados
                         cat_maq_credito ent,      -- maquinaria de estados
                         cat_tipo_credito mrc      -- catálogo de tipos de crédito
                   WHERE cre.id_derechohabiente = v_id_derechohabiente
                     AND cre.tpo_credito        = v_tpo_credito
                     AND cre.num_credito        = v_num_credito
                     AND cre.estado             = ent.estado
                     AND ent.entidad IN(1,3)
                     AND cre.tpo_originacion    = mrc.tpo_originacion
                     AND cre.tpo_credito        = mrc.tpo_credito

                   SELECT id_cre_ctr_archivo,
                          f_proceso
                     INTO v_id_cre_ctr_archivo,
                          v_f_proceso
                     FROM cre_ctr_archivo
                    WHERE folio_archivo = p_folio;

                   -- se inserta en cre_his_acreditado
                   INSERT INTO cre_his_acreditado ( id_cre_acreditado,
                                                    id_cre_ctr_archivo,
                                                    tpo_transferencia,
                                                    edo_procesar,
                                                    estado,
                                                    f_proceso)
                                            VALUES( v_id_cre_acreditado,
                                                    v_id_cre_ctr_archivo,
                                                    v_ax_tpo_transferencia,
                                                    v_edo_procesar,
                                                    174,
                                                    v_f_proceso);

                   UPDATE cre_acreditado
                     SET estado = 174
                   WHERE id_cre_acreditado = v_id_cre_acreditado;

                   --Busca en cta_credito si hay información para el id_derechohabiente,
                   --el tipo de crédito y el número de crédito, si hay se debe borrar el registro
                   SELECT COUNT(*)
                     INTO v_cnt_cta_credito
                     FROM cta_credito
                    WHERE id_derechohabiente = v_id_derechohabiente
                      AND tpo_credito = v_tpo_credito
                      AND num_credito = v_num_credito;

                   IF v_cnt_cta_credito >= 1 THEN
                       INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                            301,
                                                            v_tpo_credito,
                                                            v_num_credito,
                                                            v_f_otorga,
                                                            5,
                                                            TODAY );

                       DELETE
                         FROM cta_credito
                        WHERE id_derechohabiente = v_id_derechohabiente
                          AND tpo_credito = v_tpo_credito
                          AND num_credito = v_num_credito;
                   ELSE
                       INSERT INTO cta_his_credito VALUES ( v_id_derechohabiente,
                                                            301,
                                                            v_tpo_credito,
                                                            v_num_credito,
                                                            v_f_otorga,
                                                            5,
                                                            TODAY );
                   END IF;

                   FOREACH
                      --Se verifica que exista una marca interna para el id_derechohabiente y la marca que le corresponda
                      SELECT n_referencia
                        INTO v_n_referencia
                        FROM sfr_marca_activa
                       WHERE id_derechohabiente = v_id_derechohabiente
                         AND marca = v_marca_inf

                      --si la marca existe, se desmarca llamando a la funcion fn_desmarca_cuenta
                      IF v_n_referencia IS NOT NULL THEN
                         EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                             v_marca_inf,v_n_referencia,
                                                             0,
                                                             0,
                                                             p_usuario,
                                                             308) 
                                                        INTO v_ax_cod_error;
                      END IF;
                   END FOREACH;

                   DELETE
                     FROM cta_marca_ws
                    WHERE id_derechohabiente =  v_id_derechohabiente
                      AND id_origen          =  v_id_cre_acreditado
                      AND modulo_cod         =  v_tpo_originacion
                      AND tpo_Credito        =  v_tpo_credito  ;

                   INSERT INTO cta_marca_ws VALUES(v_id_derechohabiente,
                                                   v_id_cre_acreditado,
                                                   v_tpo_originacion,
                                                   v_tpo_credito,
                                                   v_marca_prc,
                                                   TODAY,
                                                   1,
                                                   "",
                                                   "",
                                                   0,
                                                   v_num_credito,
                                                   v_f_otorga,
                                                   v_tpo_originacion_procesar,
                                                   p_folio,
                                                   p_usuario
                                                   );

                  -- Desmarca UG
                  FOREACH
                     SELECT id_derechohabiente,
                            marca,
                            n_referencia
                       INTO v_dm_id_derechohabiente,
                            v_dm_marca_entra,
                            v_dm_n_referencia
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND marca              = 225

                     -- se invoca la función de desmarca
                     EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                                         v_dm_marca_entra,
                                                         v_dm_n_referencia,
                                                         v_dm_estado_marca,
                                                         v_dm_marca_causa,
                                                         v_dm_usuario,
                                                         v_dm_proceso_cod)
                                                    INTO v_ax_cod_error;
                  END FOREACH;

               END FOREACH  -- Recupera créditos vigentes y en trámite

               -- Incrementa contador para aceptados
               LET v_cnt_acept = v_cnt_acept + 1;

            END IF
         END IF;
      END FOREACH;   -- Cancelaciones tipo operación = "D"
   END IF;

   RETURN v_error, v_cnt_acept, v_cnt_rech;

END FUNCTION;


