






CREATE FUNCTION "safreviv".fn_prepara_bdnsviv_plus()
RETURNING SMALLINT,VARCHAR(200);

   --Variables de salida
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(200);

   --Variables para manipular los datos
   DEFINE v_id_derechohabiente                     decimal(9,0);
   DEFINE v_nss                                    char(11);
   DEFINE v_rfc                                    char(13);
   DEFINE v_curp                                   char(18);
   DEFINE v_ape_paterno                            char(40);
   DEFINE v_ape_materno                            char(40);
   DEFINE v_nombre                                 char(40);
   DEFINE v_nombre_imss                            char(50);
   DEFINE v_saldo_viv97                            decimal(12,2);
   DEFINE v_saldo_viv92                            decimal(12,2);
   DEFINE v_aivs_97                                decimal(12,2);
   DEFINE v_aivs_92                                decimal(12,2);
   DEFINE v_saldo_viv97_inf                        decimal(12,2);
   DEFINE v_saldo_viv92_inf                        decimal(12,2);
   DEFINE v_aivs_97_inf                            decimal(12,2);
   DEFINE v_aivs_92_inf                            decimal(12,2);
   DEFINE v_fcredito                               char(6);
   DEFINE v_temp_f_credito                         date;
   DEFINE v_id_credito                             smallint;
   DEFINE v_ind_imss                               smallint;
   DEFINE v_ind_inf                                smallint;

   DEFINE v_subcuenta                              smallint;
   DEFINE v_fondo_inversion                        smallint;
   DEFINE v_monto_acciones                         decimal(22,6);
   DEFINE v_monto_pesos                            decimal(22,2);
   
   SET PDQPRIORITY HIGH;

   DROP TABLE IF EXISTS cbd_prepara_saldo CASCADE ;
   CREATE TABLE cbd_prepara_saldo
   (
      nss                   char(11)   NOT NULL,
      rfc                   char(13),
      curp                  char(18),
      ape_paterno           char(40),
      ape_materno           char(40),
      nombre                char(40),
      nombre_imss           char(50),
      saldo_viv97           decimal(22,2),
      saldo_viv92           decimal(22,2),
      aivs_97               decimal(26,6),
      aivs_92               decimal(26,6),
      f_credito             char(6),
      ind_imss              smallint
   ) 
   fragment by round robin in bdprc_1_dbs , bdprc_2_dbs , bdprc_3_dbs , bdprc_4_dbs
   extent size 128000 next size 16 lock mode page;


   --Se inicializan las variables de respuesta
   LET v_resultado = 0;
   LET v_mensaje = "El archivo se preparo correctamente";

   --Se consulta cada derechihabiente de la tabla de afi_derechohabiente
   FOREACH
      SELECT
         id_derechohabiente,
         nss,
         rfc,
         curp,
         ap_paterno_af,
         ap_materno_af,
         nombre_af,
         nombre_imss,
         f_credito,
         id_credito
      INTO
         v_id_derechohabiente,
         v_nss,
         v_rfc,
         v_curp,
         v_ape_paterno,
         v_ape_materno,
         v_nombre,
         v_nombre_imss,
         v_temp_f_credito,
         v_id_credito
      FROM afi_derechohabiente

      IF (trim(v_nombre) = 'S/N') THEN
         LET v_nombre = ' ';
      END IF

      IF (v_id_credito <> 0 AND v_temp_f_credito IS NOT NULL) THEN
         LET v_fcredito = to_char(v_temp_f_credito,'%y%m%d');
      ELSE
         LET v_fcredito = ' ';
      END IF

      --Se inicializan las variables
      LET v_ind_imss = 0;
      LET v_ind_inf = 0;
      LET v_saldo_viv97 = 0;
      LET v_saldo_viv92 = 0;
      LET v_aivs_97 = 0;
      LET v_aivs_92 = 0;
      LET v_saldo_viv97_inf = 0;
      LET v_saldo_viv92_inf = 0;
      LET v_aivs_97_inf = 0;
      LET v_aivs_92_inf = 0;

      FOREACH
         SELECT
            subcuenta,
            fondo_inversion,
            monto_acciones,
            monto_pesos
         INTO
            v_subcuenta,
            v_fondo_inversion,
            v_monto_acciones,
            v_monto_pesos
         FROM safre_sdo@vivws_tcp:cta_saldo_mensual
         WHERE id_derechohabiente = v_id_derechohabiente
         AND (subcuenta = 8 OR subcuenta = 4 OR subcuenta = 44 OR subcuenta = 42)
         AND fondo_inversion = 11

         IF v_monto_acciones IS NOT NULL THEN
            IF (v_monto_acciones < 0) THEN
               LET v_monto_acciones  = (v_monto_acciones * -1);
               LET v_monto_pesos  = (v_monto_pesos * -1);
            END IF

            --Vivienda 97
            IF (v_subcuenta = 4 AND v_fondo_inversion = 11) THEN
               LET v_saldo_viv97 = v_monto_pesos;
               LET v_aivs_97 = v_monto_acciones;
               LET v_ind_imss = 1;
            END IF   --FIN Vivienda 97

            --Vivienda 92
            IF (v_subcuenta = 8 AND v_fondo_inversion = 11) THEN
               LET v_saldo_viv92 = v_monto_pesos;
               LET v_aivs_92 = v_monto_acciones;
               LET v_ind_imss = 1;
            END IF   --FIN Vivienda 92

            --Vivienda 97 solo infonavit
            IF (v_subcuenta = 44 AND v_fondo_inversion = 11) THEN
               LET v_saldo_viv97_inf = v_monto_pesos;
               LET v_aivs_97_inf = v_monto_acciones;
               LET v_ind_inf = 1;
            END IF   --FIN Vivienda 97 solo infonavit

            --Vivienda 92 solo infonavit
            IF (v_subcuenta = 42 AND v_fondo_inversion = 11) THEN
               LET v_saldo_viv92_inf = v_monto_pesos;
               LET v_aivs_92_inf = v_monto_acciones;
               LET v_ind_inf = 1;
            END IF   --FIN Vivienda 92 solo infonavit
         END IF
      END FOREACH;   --FIN ciclo de Saldo

      --Este caso es para los nss's que no tienen saldo
      IF (v_ind_imss = 0 AND v_ind_inf = 0) THEN
         --Se valida si el NSS que no tiene saldo corresponde a un derechohabiente imss o solo infonavit
         IF (v_nss[1,2] = '77') THEN
            --Para los NSS's solo infonavit se activa el indicador
            LET v_ind_inf = 1;
         ELSE
            --Para los NSS's imss solo se activa el indicador
            LET v_ind_imss = 1;
         END IF
      END IF

      IF (v_ind_imss = 1) THEN
         INSERT INTO cbd_prepara_saldo VALUES(
            v_nss,
            v_rfc,
            v_curp,
            v_ape_paterno,
            v_ape_materno,
            v_nombre,
            v_nombre_imss,
            v_saldo_viv97,
            v_saldo_viv92,
            v_aivs_97,
            v_aivs_92,
            v_fcredito,
            1
         );
      END IF

      IF (v_ind_inf = 1) THEN
         INSERT INTO cbd_prepara_saldo VALUES(
            v_nss,
            v_rfc,
            v_curp,
            v_ape_paterno,
            v_ape_materno,
            v_nombre,
            v_nombre_imss,
            v_saldo_viv97_inf,
            v_saldo_viv92_inf,
            v_aivs_97_inf,
            v_aivs_92_inf,
            v_fcredito,
            0
         );
      END IF
      
   END FOREACH;

   CREATE INDEX XPKcbd_prepara_saldo ON cbd_prepara_saldo(nss ASC)IN bdprc_ix_dbs;
   UPDATE statistics FOR TABLE cbd_prepara_saldo;

   SET PDQPRIORITY DEFAULT;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


