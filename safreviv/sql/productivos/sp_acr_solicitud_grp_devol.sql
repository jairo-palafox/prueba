






CREATE PROCEDURE "safreviv".sp_acr_solicitud_grp_devol()

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
   DEFINE v_afi_nss                       CHAR(11);
   DEFINE v_afi_rfc                       CHAR(13);
   DEFINE v_afi_ap_paterno_af             CHAR(40);
   DEFINE v_afi_ap_materno_af             CHAR(40);
   DEFINE v_afi_nombre_af                 CHAR(40);
   DEFINE v_afi_nombre_imss               CHAR(50);

   --Variables para la tabla: tmp_rep_devolucion y tmp_ctr_devolucion
   DEFINE v_tmp_curp                      CHAR(18);
   DEFINE v_tmp_nss                       CHAR(11);
   DEFINE v_tmp_rfc                       CHAR(13);
   DEFINE v_tmp_ap_paterno_af             CHAR(40);
   DEFINE v_tmp_ap_materno_af             CHAR(40);
   DEFINE v_tmp_nombre_af                 CHAR(40);
   DEFINE v_tmp_aivs97                    DECIMAL(16,6);
   DEFINE v_tmp_pesos97                   DECIMAL(12,2);
   DEFINE v_tmp_nss_separacion            CHAR(11);
   DEFINE v_tmp_origen_devolucion         CHAR(2);
   DEFINE v_tmp_monto_aportacion          DECIMAL(12,2);
   DEFINE v_tmp_aivs_aportacion           DECIMAL(16,6);
   DEFINE v_tmp_aivs92                    DECIMAL(16,6);
   DEFINE v_tmp_pesos92                   DECIMAL(12,2);
   DEFINE v_tmp_nom_imss                  CHAR(50);
   DEFINE v_tmp_num_credito               DECIMAL(10,0);
   DEFINE v_tmp_edo_procesar              SMALLINT;

   --Variables auxiliares
   DEFINE v_valor_fondo                   DECIMAL(19,14);
   DEFINE v_edo_procesar_aux              SMALLINT;
   DEFINE v_f_solic_saldos_aux            DATE;

   --Variables provenientes de la tabla: tmp_rep_devolucion
   DEFINE v_tmp_curp_r                   CHAR(18);
   DEFINE v_tmp_nss_r                    CHAR(11);
   DEFINE v_tmp_rfc_r                    CHAR(13);
   DEFINE v_tmp_ap_paterno_af_r          CHAR(40);
   DEFINE v_tmp_ap_materno_af_r          CHAR(40);
   DEFINE v_tmp_nombre_af_r              CHAR(40);
   DEFINE v_tmp_aivs97_r                 DECIMAL(16,6);
   DEFINE v_tmp_pesos97_r                DECIMAL(12,2);
   DEFINE v_tmp_nss_separacion_r         CHAR(11);
   DEFINE v_tmp_origen_devolucion_r      CHAR(2);
   DEFINE v_tmp_monto_aportacion_r       DECIMAL(12,2);
   DEFINE v_tmp_aivs_aportacion_r        DECIMAL(16,6);
   DEFINE v_tmp_aivs92_r                 DECIMAL(16,6);
   DEFINE v_tmp_pesos92_r                DECIMAL(12,2);
   DEFINE v_tmp_nom_imss_r               CHAR(50);
   DEFINE v_tmp_num_credito_r            DECIMAL(10,0);
   DEFINE v_tmp_edo_procesar_r           SMALLINT;

   --Variables agregadas despues del cambio para validacion de marca 150 23/10/2015 
   DEFINE v_nss_unificador               CHAR(11);
   DEFINE v_id_dh_unificador             DECIMAL(9,0);
   DEFINE p_nss                          CHAR(11);
   DEFINE vcodigo_op                     CHAR(1);
   DEFINE vfecha_tramite                 DATE;
   DEFINE p_marca                        DECIMAL;
   DEFINE p_id_dh                        DECIMAL(9,0);
   DEFINE v_id_grp_dev_unificador        DECIMAL(9,0);
   DEFINE v_bnd                          SMALLINT;
   DEFINE v_diag                         SMALLINT;

   --Variables para tabla tmp_rpt_devolucion
   DEFINE v_rpt_curp                      CHAR(18);
   DEFINE v_rpt_nss                       CHAR(11);
   DEFINE v_rpt_rfc                       CHAR(13);
   DEFINE v_rpt_ap_paterno_af             CHAR(40);
   DEFINE v_rpt_ap_materno_af             CHAR(40);
   DEFINE v_rpt_nombre_af                 CHAR(40);
   DEFINE v_rpt_aivs97                    DECIMAL(16,6);
   DEFINE v_rpt_pesos97                   DECIMAL(12,2);
   DEFINE v_rpt_nss_separacion            CHAR(11);
   DEFINE v_rpt_origen_devolucion         CHAR(2);
   DEFINE v_rpt_monto_aportacion          DECIMAL(12,2);
   DEFINE v_rpt_aivs_aportacion           DECIMAL(16,6);
   DEFINE v_rpt_aivs92                    DECIMAL(16,6);
   DEFINE v_rpt_pesos92                   DECIMAL(12,2);
   DEFINE v_rpt_nom_imss                  CHAR(50);
   DEFINE v_rpt_num_credito               DECIMAL(10,0);
   DEFINE v_rpt_edo_procesar              SMALLINT;
   DEFINE qry_string                      CHAR(200);

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseSolicDevol.trace';
   --SET DEBUG FILE TO '/safreviv/acr/sql/dseSolicGrpDevol.trace';
   --TRACE ON;

   LET v_agrupa_id_derechohabiente = "";
   LET v_rpt_curp                  = "";
   LET v_rpt_nss                   = "";
   LET v_rpt_rfc                   = "";
   LET v_rpt_ap_paterno_af         = "";
   LET v_rpt_ap_materno_af         = "";
   LET v_rpt_nombre_af             = "";
   LET v_rpt_aivs97                = 0;
   LET v_rpt_pesos97               = 0;
   LET v_rpt_nss_separacion        = "";
   LET v_rpt_origen_devolucion     = "";
   LET v_rpt_monto_aportacion      = 0;
   LET v_rpt_aivs_aportacion       = 0;
   LET v_rpt_aivs92                = 0;
   LET v_rpt_pesos92               = 0;
   LET v_rpt_nom_imss              = "";
   LET v_rpt_num_credito           = 0;
   LET v_rpt_edo_procesar          = 0;
   LET v_tmp_aivs97                = 0;
   LET v_tmp_pesos97               = 0;
   LET v_tmp_monto_aportacion      = 0;
   LET v_tmp_aivs_aportacion       = 0;
   LET v_tmp_aivs92                = 0;
   LET v_tmp_pesos92               = 0;
   LET v_tmp_nss_separacion        = "";
   LET v_tmp_origen_devolucion     = "";
   LET v_tmp_num_credito           = "";

   LET qry_string = " SELECT UNIQUE nss "||
                      " FROM safre_tmp:tmp_ctr_devolucion "||
                      " INTO TEMP tmp_ctr_nss";

   EXECUTE IMMEDIATE qry_string;

   FOREACH
      SELECT nss
        INTO v_rpt_nss
        FROM tmp_ctr_nss

      FOREACH
        SELECT curp             ,
               nss              ,
               rfc              ,
               ap_paterno_af    ,
               ap_materno_af    ,
               nombre_af        ,
               aivs97           ,
               pesos97          ,
               nss_separacion   ,
               origen_devolucion,
               monto_aportacion ,
               aivs_aportacion  ,
               aivs92           ,
               pesos92          ,
               nom_imss         ,
               num_credito      ,
               edo_procesar 
          INTO v_rpt_curp             ,
               v_rpt_nss              ,
               v_rpt_rfc              ,
               v_rpt_ap_paterno_af    ,
               v_rpt_ap_materno_af    ,
               v_rpt_nombre_af        ,
               v_rpt_aivs97           ,
               v_rpt_pesos97          ,
               v_rpt_nss_separacion   ,
               v_rpt_origen_devolucion,
               v_rpt_monto_aportacion ,
               v_rpt_aivs_aportacion  ,
               v_rpt_aivs92           ,
               v_rpt_pesos92          ,
               v_rpt_nom_imss         ,
               v_rpt_num_credito      ,
               v_rpt_edo_procesar
          FROM safre_tmp:tmp_ctr_devolucion
         WHERE nss = v_rpt_nss

         LET v_tmp_aivs97            = v_tmp_aivs97 + v_rpt_aivs97;
         LET v_tmp_pesos97           = v_tmp_pesos97 + v_rpt_pesos97;
         LET v_tmp_monto_aportacion  = v_tmp_monto_aportacion + v_rpt_monto_aportacion;
         LET v_tmp_aivs_aportacion   = v_tmp_aivs_aportacion + v_rpt_aivs_aportacion;
         LET v_tmp_aivs92            = v_tmp_aivs92 + v_rpt_aivs92;
         LET v_tmp_pesos92           = v_tmp_pesos92 + v_rpt_pesos92;

         IF v_rpt_nss_separacion IS NOT NULL THEN
            LET v_tmp_nss_separacion  = v_rpt_nss_separacion;
         END IF

         IF v_rpt_origen_devolucion IS NOT NULL THEN
            LET v_tmp_origen_devolucion = v_rpt_origen_devolucion;
         END IF

         IF v_rpt_num_credito IS NOT NULL THEN
            LET v_tmp_num_credito  = v_rpt_num_credito;
         END IF

      END FOREACH;

      IF v_tmp_monto_aportacion IS NULL THEN
         LET v_tmp_monto_aportacion = 0;
      END IF

      --Se inserta el detalle del registro en la tabla temporal
      INSERT INTO safre_tmp:tmp_rep_devolucion VALUES ( v_rpt_curp             ,
                                                        v_rpt_nss              ,
                                                        v_rpt_rfc              ,
                                                        v_rpt_ap_paterno_af    ,
                                                        v_rpt_ap_materno_af    ,
                                                        v_rpt_nombre_af        ,
                                                        v_tmp_aivs97           ,
                                                        v_tmp_pesos97          ,
                                                        v_tmp_nss_separacion   ,
                                                        v_tmp_origen_devolucion,
                                                        v_tmp_monto_aportacion ,
                                                        v_tmp_aivs_aportacion  ,
                                                        v_tmp_aivs92           ,
                                                        v_tmp_pesos92          ,
                                                        v_rpt_nom_imss         ,
                                                        v_rpt_num_credito      ,
                                                        v_rpt_edo_procesar     );

      LET v_agrupa_id_derechohabiente = "";
      LET v_rpt_curp                  = "";
      LET v_rpt_nss                   = "";
      LET v_rpt_rfc                   = "";
      LET v_rpt_ap_paterno_af         = "";
      LET v_rpt_ap_materno_af         = "";
      LET v_rpt_nombre_af             = "";
      LET v_rpt_aivs97                = 0;
      LET v_rpt_pesos97               = 0;
      LET v_rpt_nss_separacion        = "";
      LET v_rpt_origen_devolucion     = "";
      LET v_rpt_monto_aportacion      = 0;
      LET v_rpt_aivs_aportacion       = 0;
      LET v_rpt_aivs92                = 0;
      LET v_rpt_pesos92               = 0;
      LET v_rpt_nom_imss              = "";
      LET v_rpt_num_credito           = 0;
      LET v_rpt_edo_procesar          = 0;
      LET v_tmp_aivs97                = 0;
      LET v_tmp_pesos97               = 0;
      LET v_tmp_monto_aportacion      = 0;
      LET v_tmp_aivs_aportacion       = 0;
      LET v_tmp_aivs92                = 0;
      LET v_tmp_pesos92               = 0;
      LET v_tmp_nss_separacion        = "";
      LET v_tmp_origen_devolucion     = "";
      LET v_tmp_num_credito           = "";

   END FOREACH;

END PROCEDURE;


