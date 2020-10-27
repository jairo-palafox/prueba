






CREATE PROCEDURE "safreviv".sp_inserta_his_dev(dseh_id_dse_grp_devolucion DECIMAL(9,0),
                                       dseh_folio                 DECIMAL(9,0),
                                       dseh_tpo_transferencia     CHAR(2),
                                       dseh_lote                  SMALLINT,
                                       dseh_id_lote               DECIMAL(9,0),
                                       dseh_f_presentacion        DATE,
                                       dseh_paterno_afore         CHAR(40),
                                       dseh_materno_afore         CHAR(40),
                                       dseh_nombre_afore          CHAR(40),
                                       dseh_nom_imss              CHAR(50),
                                       dseh_aivs97                DECIMAL(16,6),
                                       dseh_pesos97               DECIMAL(12,2),
                                       dseh_aivs92                DECIMAL(16,6),
                                       dseh_pesos92               DECIMAL(12,2),
                                       dseh_monto_aportacion      DECIMAL(12,2),
                                       dseh_aivs_aportacion       DECIMAL(16,6),
                                       dseh_nss_separacion        CHAR(11),
                                       dseh_edo_procesar          SMALLINT,
                                       dseh_diagnostico           CHAR(3),
                                       dseh_estado                SMALLINT,
                                       dseh_f_proceso             DATE)

   INSERT INTO dse_his_devolucion (
               id_dse_grp_devolucion,
               folio,
               tpo_transferencia,
               lote,
               id_lote,
               f_presentacion,
               paterno_afore,
               materno_afore,
               nombre_afore,
               nom_imss,
               aivs97,
               pesos97,
               aivs92,
               pesos92,
               monto_aportacion,
               aivs_aportacion,
               nss_separacion,
               edo_procesar,
               diagnostico,
               estado,
               f_proceso)
       VALUES (dseh_id_dse_grp_devolucion,
               dseh_folio,
               dseh_tpo_transferencia,
               dseh_lote,
               dseh_id_lote,
               dseh_f_presentacion,
               dseh_paterno_afore,
               dseh_materno_afore,
               dseh_nombre_afore,
               dseh_nom_imss,
               dseh_aivs97,
               dseh_pesos97,
               dseh_aivs92,
               dseh_pesos92,
               dseh_monto_aportacion,
               dseh_aivs_aportacion,
               dseh_nss_separacion,
               dseh_edo_procesar,
               dseh_diagnostico,
               dseh_estado,
               dseh_f_proceso);

END PROCEDURE;


