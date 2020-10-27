






CREATE PROCEDURE "safreviv".sp_inserta_dse(p_nss               CHAR(11),
                                p_tpo_transferencia CHAR(2),
                                p_num_credito       DECIMAL(10,0),
                                p_folio             INTEGER,
                                p_pesos97           DECIMAL(12,2),
                                p_aivs97            DECIMAL (19,2),
                                p_pesos92           DECIMAL(12,2),
                                p_aivs92            DECIMAL (19,2))

   --Table: dse_agrupa_devolucion
   DEFINE v_agrupa_id_dse_grp_devolucion  DECIMAL(9,0);
   DEFINE v_agrupa_id_derechohabiente     DECIMAL(9,0);
   DEFINE v_agrupa_num_credito            DECIMAL(10,0);
   DEFINE v_agrupa_tpo_transferencia      CHAR(2);
   DEFINE v_agrupa_origen_devolucion      CHAR(2);
   DEFINE v_agrupa_f_movimiento           DATE;
   DEFINE v_agrupa_folio_liquida          DECIMAL(9,0);
   DEFINE v_agrupa_aivs97                 DECIMAL(16,6);
   DEFINE v_agrupa_pesos97                DECIMAL(12,2);
   DEFINE v_agrupa_aivs92                 DECIMAL(16,6);
   DEFINE v_agrupa_pesos92                DECIMAL(12,2);
   DEFINE v_agrupa_monto_aportacion       DECIMAL(12,2);
   DEFINE v_agrupa_aivs_aportacion        DECIMAL(16,6);
   DEFINE v_agrupa_nss_separacion         CHAR(11);
   DEFINE v_agrupa_edo_procesar           SMALLINT;
   DEFINE v_agrupa_estado                 SMALLINT;

   --Variables para la tabla: afi_derechohabiente
   DEFINE v_afi_curp                      CHAR(18);
   DEFINE v_afi_id_derechohabiente        DECIMAL(9,0);
   DEFINE v_afi_rfc                       CHAR(13);
   DEFINE v_afi_ap_paterno_af             CHAR(40);
   DEFINE v_afi_ap_materno_af             CHAR(40);
   DEFINE v_afi_nombre_af                 CHAR(40);
   DEFINE v_afi_nombre_imss               CHAR(50);
   DEFINE v_id_grp                        DECIMAL(9,0);
   DEFINE v_bnd                           SMALLINT;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseinserta.trace';
   ---SET DEBUG FILE TO '/safreviv_int/archivos/dseinserta.trace';
   ---TRACE ON;

   SELECT curp,
          id_derechohabiente,
          rfc,
          ap_paterno_af,
          ap_materno_af,
          nombre_af,
          nombre_imss
     INTO v_afi_curp,
          v_afi_id_derechohabiente,
          v_afi_rfc,
          v_afi_ap_paterno_af,
          v_afi_ap_materno_af,
          v_afi_nombre_af,
          v_afi_nombre_imss
     FROM afi_derechohabiente
    WHERE nss = p_nss;

   LET v_id_grp = seq_dse_grp_devolucion.NEXTVAL;

   --se inserta registro con datos originales en tabla histórica
   INSERT INTO dse_his_devolucion VALUES ( v_id_grp,
                                           v_afi_id_derechohabiente,
                                           p_tpo_transferencia,
                                           1,--lote
                                           1,--id_lote
                                           TODAY,--f_presentacion
                                           v_afi_ap_paterno_af,
                                           v_afi_ap_materno_af,
                                           v_afi_nombre_af,
                                           v_afi_nombre_imss,
                                           p_aivs97,
                                           p_pesos97,
                                           p_aivs92,
                                           p_pesos92,
                                           0,
                                           0,
                                           "",
                                           20,
                                           "",--diagnostico
                                           142,
                                           TODAY);

   INSERT INTO dse_agrupa_devolucion VALUES ( v_id_grp, -- consecutivo para id_d                                                                                                                                                             se de unificador
                                              v_afi_id_derechohabiente,
                                              p_num_credito,
                                              p_tpo_transferencia,
                                              "02",
                                              TODAY,
                                              p_folio,
                                              p_aivs97,
                                              p_pesos97,
                                              p_aivs92,
                                              p_pesos92,
                                              0,
                                              0,
                                              "",
                                              20,
                                              142);
END PROCEDURE;


