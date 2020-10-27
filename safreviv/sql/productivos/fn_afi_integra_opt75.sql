






CREATE FUNCTION "safreviv".fn_afi_integra_opt75(p_usuario_cod CHAR(20),
                                     p_folio DECIMAL(10),
                                     p_nombre_archivo CHAR(40),
                                     p_pid DECIMAL(9,0),
                                     p_proceso_cod SMALLINT)

   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER , INTEGER

-- variables de la tabla del encabezado
   DEFINE v_cza_tmp_tpo_registro                 CHAR(2);
   DEFINE v_cza_tmp_id_servicio                  CHAR(2);
   DEFINE v_cza_tmp_id_operacion                 CHAR(2);
   DEFINE v_cza_tmp_tpo_entidad_origen           CHAR(2);
   DEFINE v_cza_tmp_cve_entidad_origen           CHAR(3);
   DEFINE v_cza_tmp_tpo_entidad_destino          CHAR(2);
   DEFINE v_cza_tmp_cve_entidad_destino          CHAR(3);
   DEFINE v_cza_tmp_cve_entidad_genera           CHAR(3);
   DEFINE v_cza_tmp_f_transferencia              DATE;
   DEFINE v_cza_tmp_consecutivo                  DECIMAL(3,0);
   DEFINE v_cza_tmp_filler                       CHAR(370);

   -- variables de la tabla del detalle
   DEFINE v_det_tmp_tpo_registro                 CHAR(2);
   DEFINE v_det_tmp_id_operacion                 CHAR(2);
   DEFINE v_det_tmp_nss                          CHAR(11);
   DEFINE v_det_tmp_rfc                          CHAR(13);
   DEFINE v_det_tmp_curp                         CHAR(18);
   DEFINE v_det_tmp_apaterno                     CHAR(40);
   DEFINE v_det_tmp_amaterno                     CHAR(40);
   DEFINE v_det_tmp_nombre                       CHAR(40);
   DEFINE v_det_tmp_f_nacimiento                 DATE;
   DEFINE v_det_tmp_sexo                         CHAR(1);
   DEFINE v_det_tmp_entidad_nacimiento           CHAR(2);
   DEFINE v_det_tmp_estatus_trabajador           CHAR(1);
   DEFINE v_det_tmp_filler                       CHAR(222);

   -- variables de la tabla del sumario
   DEFINE v_sum_tmp_tpo_registro                 CHAR(2);
   DEFINE v_sum_tmp_tpo_entidad_origen           CHAR(2);
   DEFINE v_sum_tmp_cve_entidad_origen           CHAR(3);
   DEFINE v_sum_tmp_tpo_entidad_destino          CHAR(2);
   DEFINE v_sum_tmp_cve_entidad_destino          CHAR(3);
   DEFINE v_sum_tmp_id_servicio                  CHAR(2);
   DEFINE v_sum_tmp_id_operacion                 CHAR(2);
   DEFINE v_sum_tmp_f_transferencia              DATE;
   DEFINE v_sum_tmp_consecutivo                  DECIMAL(3,0);
   DEFINE v_sum_tmp_num_registros_entrada        DECIMAL(9,0);
   DEFINE v_sum_tmp_num_registros_salida         DECIMAL(9,0);
   DEFINE v_sum_tmp_filler                       CHAR(359);

   -- constantes para códigos de error
   DEFINE v_error_nss_vacio                       SMALLINT;
   DEFINE v_error_nombre_vacio                    SMALLINT;
   DEFINE v_error_nss_inconsistente               SMALLINT;

   -- campos de la tabla de rechazos afi_rch_afiliatorio
   DEFINE afi_rch_afiliatorio_tpo_movimiento      CHAR(2);
   DEFINE afi_rch_afiliatorio_espacios            CHAR(2);
   DEFINE afi_rch_afiliatorio_nrp                 CHAR(11);
   DEFINE afi_rch_afiliatorio_f_movimiento        CHAR(8);
   DEFINE afi_rch_afiliatorio_rfc                 CHAR(13);
   DEFINE afi_rch_afiliatorio_curp                CHAR(18);
   DEFINE afi_rch_afiliatorio_t_trabajador        DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss                 CHAR(11);
   DEFINE afi_rch_afiliatorio_nombre              CHAR(50);
   DEFINE afi_rch_afiliatorio_presentacion_extemp DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_jornada_semana      DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_sdi                 DECIMAL(6,0);
   DEFINE afi_rch_afiliatorio_sexo                DECIMAL(1,0);
   DEFINE afi_rch_afiliatorio_nss_correcto        CHAR(11);
   DEFINE afi_rch_afiliatorio_nombre_correcto     CHAR(50);
   DEFINE afi_rch_afiliatorio_cod_rechazo         SMALLINT;
   DEFINE v_registro_rechazado                    SMALLINT; -- booleana para verificar un registro rechazado
   DEFINE v_rch                                   smallint;

   --variables para el historico de afi_derechohabiente
   DEFINE v_afi_dere_id_derechohabiente           DECIMAL(9,0) ;
   DEFINE v_afi_dere_f_modifica                   DATE;
   DEFINE v_afi_dere_folio_lote_modifica          DECIMAL(9,0) ;
   DEFINE v_afi_dere_ind_modifica                 CHAR(18);
   DEFINE v_afi_dere_curp                         CHAR(18);
   DEFINE v_afi_dere_rfc                          CHAR(13);
   DEFINE v_afi_dere_ind_nrp                      CHAR(1);
   DEFINE v_afi_dere_f_nacimiento                 DATE;
   DEFINE v_afi_dere_nombre_imss                  CHAR(50);
   DEFINE v_afi_dere_nombre_af                    CHAR(40);
   DEFINE v_afi_dere_ap_paterno_af                CHAR(40);
   DEFINE v_afi_dere_ap_materno_af                CHAR(40);
   DEFINE v_num_cambios_realizados                INTEGER;
   DEFINE v_num_altas_realizados                  INTEGER;
   DEFINE v_afi_sexo                              CHAR(1);
   DEFINE v_sexo_curp                             CHAR(1);
   DEFINE v_bnd_sexo                              SMALLINT;
   DEFINE v_bnd_rfc                               SMALLINT;
   DEFINE v_nombre_imss                           CHAR(50);
   DEFINE bnd_nom                                 SMALLINT;
   DEFINE bnd_curp                                SMALLINT;
   DEFINE bnd_rfc                                 SMALLINT;

   --variable para validad la exixtenci ade registron en afi_derechohabiente
   DEFINE v_i_contador_registros                  INTEGER;

   -- Control de Excepciones
   DEFINE v_i_resultado                           SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(255);
   DEFINE tmp_riss_imss                           SMALLINT;

   --variable para contar los rechazos
   DEFINE v_i_cnt_rechazos                        INTEGER;

   -- variables para parametros de retorno de función que verifica si ya tuvo un crédito
   DEFINE v_resultado         SMALLINT;
   DEFINE v_tpo_originacion   SMALLINT;
   DEFINE v_tpo_credito       SMALLINT;
   DEFINE v_num_credito       DECIMAL(10,0);
   DEFINE v_f_otorga          DATE;
   DEFINE v_f_liquida         DATE;
   DEFINE v_tpo_desc          SMALLINT;

   -- se define el comportamiento en la ocurrencia de una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN sql_err, isam_err, err_txt, v_num_cambios_realizados,v_i_cnt_rechazos;
   END EXCEPTION

   LET v_i_contador_registros   = 0;
   LET tmp_riss_imss            = NULL;

   -- contador de cambios realizados
   LET v_num_cambios_realizados = 0;
   LET v_i_cnt_rechazos         = 0;
   LET v_rch                    = 0;

   SET DEBUG FILE TO "/safreviv_int/BD/fn_afi_prevalida_integracion_opt75.trace";
   TRACE ON;

   LET v_det_tmp_rfc                  = "";
   LET v_det_tmp_curp                 = "";
   LET v_det_tmp_apaterno             = "";
   LET v_det_tmp_amaterno             = "";
   LET v_det_tmp_nombre               = "";
   LET v_det_tmp_f_nacimiento         = "";
   LET v_det_tmp_sexo                 = "";

   LET v_afi_dere_id_derechohabiente  = 0;
   LET v_afi_dere_folio_lote_modifica = 0;
   LET v_afi_dere_curp                = "";
   LET v_afi_dere_rfc                 = "";
   LET v_afi_dere_ind_nrp             = "";
   LET v_afi_dere_f_nacimiento        = "";
   LET v_afi_dere_nombre_imss         = "";
   LET v_afi_dere_nombre_af           = "";
   LET v_afi_dere_ap_paterno_af       = "";
   LET v_afi_dere_ap_materno_af       = "";
   LET v_afi_sexo                     = "";
   LET v_sexo_curp                    = "";
   LET v_bnd_sexo                     = 0;
   LET v_bnd_rfc                      = 0;
   LET v_registro_rechazado           = 0;
   LET v_num_altas_realizados         = 0;

   -- se definen las constantes de código de error
   LET v_error_nombre_vacio             = 7;
   LET v_error_nss_vacio                = 8;
   LET v_error_nss_inconsistente        = 14;

   LET bnd_nom                          = 0;
   LET bnd_curp                         = 0;
   LET bnd_rfc                          = 0;

   --Se hace ciclo de registros historicos
   FOREACH
      SELECT tpo_registro                ,
             id_operacion                ,
             nss                         ,
             TRIM(rfc)                   ,
             TRIM(curp)                  ,
             TRIM(apaterno)              ,
             TRIM(amaterno)              ,
             TRIM(nombre)                ,
             f_nacimiento                ,
             sexo                        ,
             entidad_nacimiento          ,
             estatus_trabajador          ,
             DECODE(curp[11],"H","1","M","2","0")
        INTO v_det_tmp_tpo_registro       ,
             v_det_tmp_id_operacion       ,
             v_det_tmp_nss                ,
             v_det_tmp_rfc                ,
             v_det_tmp_curp               ,
             v_det_tmp_apaterno           ,
             v_det_tmp_amaterno           ,
             v_det_tmp_nombre             ,
             v_det_tmp_f_nacimiento       ,
             v_det_tmp_sexo               ,
             v_det_tmp_entidad_nacimiento ,
             v_det_tmp_estatus_trabajador ,
             v_sexo_curp
        FROM safre_tmp:tmp_afi_det_op75

-- se limpian variables cuando traen espacios
      IF v_det_tmp_apaterno = " " THEN
         LET v_det_tmp_apaterno = NULL;
      END IF

      IF v_det_tmp_amaterno = " " THEN
         LET v_det_tmp_amaterno = NULL;
      END IF

      IF v_det_tmp_nombre = " " THEN
         LET v_det_tmp_nombre = NULL;
      ENd IF

      IF v_det_tmp_rfc = " " THEN
         LET v_det_tmp_rfc = NULL;
      END IF

      IF v_det_tmp_curp = " " THEN
         LET v_det_tmp_curp = NULL;
      END IF

--trace "busca datos del nss: " || v_det_tmp_nss;
     -- se selecciona el registro de afi_derechohabieNte
      SELECT id_derechohabiente  ,
             folio_lote          ,
             TRIM(curp)          ,
             TRIM(rfc)           ,
             ind_nrp             ,
             f_nacimiento        ,
             TRIM(nombre_imss)   ,
             TRIM(nombre_af)     ,
             TRIM(ap_paterno_af) ,
             TRIM(ap_materno_af) ,
             DECODE(sexo,"1","1","2","2","0")
        INTO v_afi_dere_id_derechohabiente  ,
             v_afi_dere_folio_lote_modifica ,
             v_afi_dere_curp                ,
             v_afi_dere_rfc                 ,
             v_afi_dere_ind_nrp             ,
             v_afi_dere_f_nacimiento        ,
             v_afi_dere_nombre_imss         ,
             v_afi_dere_nombre_af           ,
             v_afi_dere_ap_paterno_af       ,
             v_afi_dere_ap_materno_af       ,
             v_afi_sexo
        FROM afi_derechohabiente
       WHERE nss = v_det_tmp_nss;

      IF v_afi_dere_rfc = " " THEN
         LET v_afi_dere_rfc = NULL;
      END IF

      IF v_afi_dere_curp = " " THEN
         LET v_afi_dere_curp = NULL;
      END IF


      -- si se encontró el derechohabiente
      IF ( v_afi_dere_id_derechohabiente IS NOT NULL ) THEN
        --se asume que se actualizaran los campos se registar el histórico
        --trace "inserta en his_derechohabiente";

         INSERT INTO afi_his_derechohabiente
                    (id_derechohabiente  ,
                     f_modifica          ,
                     folio_lote_modifica ,
                     ind_modifica        ,
                     curp                ,
                     rfc                 ,
                     ind_nrp             ,
                     f_nacimiento        ,
                     nombre_imss         ,
                     nombre_af           ,
                     ap_paterno_af       ,
                     ap_materno_af)
             VALUES (v_afi_dere_id_derechohabiente  ,
                     TODAY                          ,
                     p_folio                        ,
                     6                              ,  --cambio nombre afore
                     v_afi_dere_curp                ,
                     v_afi_dere_rfc                 ,
                     v_afi_dere_ind_nrp             ,
                     v_afi_dere_f_nacimiento        ,
                     v_afi_dere_nombre_imss         ,
                     v_afi_dere_nombre_af           ,
                     v_afi_dere_ap_paterno_af       ,
                     v_afi_dere_ap_materno_af);

         ---se verifica si se puede actualizar el dato sexo
         IF v_sexo_curp <> "0" THEN
            IF (v_afi_sexo = "0") OR
               (v_afi_sexo <> v_sexo_curp) THEN
               LET v_bnd_sexo = 1;
            ELSE
               LET v_sexo_curp = v_afi_sexo;
            END IF
         ELSE
            LET v_sexo_curp = v_afi_sexo;
         END IF

         IF (v_afi_dere_rfc IS NULL OR LENGTH(v_afi_dere_rfc) < 13) AND (LENGTH(v_det_tmp_rfc) = 13) THEN
            LET v_bnd_rfc = 1;
         ELSE
            LET v_det_tmp_rfc = v_afi_dere_rfc;
         END IF


       --*******************************************************
       -- se verifica si se debe actualizar el campo curp

         IF v_afi_dere_curp = '' OR
               v_afi_dere_curp = ' ' THEN
               LET v_afi_dere_curp = NULL;
            END IF

           -- IF (v_afi_dere_curp) IS NULL  THEN
               IF (LENGTH(v_det_tmp_curp) = 18) THEN

                  --********************************
                  --validando posiciones de curp
                  IF (v_det_tmp_curp[1]  NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[2]  NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[3]  NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[4]  NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[5]  NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[6]  NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[7]  NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[8]  NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[9]  NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[10] NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[11] NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[12] NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[13] NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[14] NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[15] NOT MATCHES '[A-Z]*') OR
                     (v_det_tmp_curp[16] NOT MATCHES '[A-Z]*') OR
                     --(v_det_tmp_curp[17] NOT MATCHES '[A-Z]*') OR
                     --(v_det_tmp_curp[17] NOT MATCHES '[0-9]*') OR
                     (v_det_tmp_curp[18] NOT MATCHES '[0-9]*') THEN

                     --IF (v_det_tmp_curp[17] NOT MATCHES '[A-Z]*') THEN
                   --     IF (v_det_tmp_curp[17] NOT MATCHES '[0-9]*') THEN
                 --          LET v_det_tmp_curp = v_afi_dere_curp;
               --         END IF
             --        END IF
                    LET v_det_tmp_curp = v_afi_dere_curp; 
                  ELSE
                  --********************************
                     LET v_det_tmp_curp = v_det_tmp_curp;
                     LET bnd_curp = 1;
                  END IF
               ELSE
                  LET v_det_tmp_curp = v_afi_dere_curp;
               END IF
            --ELSE
              --  LET v_det_tmp_curp = v_afi_dere_curp;
            --END IF

         -- se verifica si se debe actualizar el campo RFC

          EXECUTE FUNCTION fn_edo_cred_viv (v_afi_dere_id_derechohabiente,1)
                                               INTO v_resultado,
                                                    v_tpo_originacion,
                                                    v_tpo_credito,
                                                    v_num_credito,
                                                    v_f_otorga,
                                                    v_f_liquida,
                                                    v_tpo_desc;

         --TRACE "resultado fn cred : "||v_resultado;

         IF (v_resultado = 0) OR
            (v_resultado = 2) THEN
            LET v_det_tmp_rfc = v_afi_dere_rfc;
         ELSE

         -- se verifica si se debe actualizar el campo RFC

            IF (v_afi_dere_rfc Is NULL) OR
               (LENGTH(v_afi_dere_rfc) <> 13 ) THEN
               IF (LENGTH(v_det_tmp_rfc) = 13 )THEN
                  LET v_det_tmp_rfc = v_det_tmp_rfc;
                  LET bnd_rfc = 1;
               ELSE
                  LET v_det_tmp_rfc = v_afi_dere_rfc;
               END IF
            END IF

         END IF

         IF (v_det_tmp_apaterno IS NULL) OR
            (v_det_tmp_apaterno = ' ')   AND
            (v_det_tmp_nombre IS NULL )  OR
            (v_det_tmp_nombre = ' ' )    THEN

            LET bnd_nom = 0;

            LET v_det_tmp_nombre   = v_afi_dere_nombre_af;
            LET v_det_tmp_apaterno = v_afi_dere_ap_paterno_af;
            LET v_det_tmp_amaterno  = v_afi_dere_ap_materno_af;
         ELSE
            LET bnd_nom = 1;
         END IF
         --***************************************************

         IF (bnd_nom  = 1) OR
            (bnd_curp = 1) OR
            (bnd_rfc  = 1) THEN

            UPDATE afi_derechohabiente
               SET nombre_af     = v_det_tmp_nombre   ,
                   ap_paterno_af = v_det_tmp_apaterno ,
                   ap_materno_af = v_det_tmp_amaterno ,
                   curp          = v_det_tmp_curp     ,
                   rfc           = v_det_tmp_rfc      ,
                   sexo          = v_sexo_curp
             WHERE nss = v_det_tmp_nss ;

         -- se cuenta un cambio
            LET v_num_cambios_realizados = v_num_cambios_realizados + 1;
         END IF

      ELSE   ----Si no existe el nss en la base de derechohabientes
         IF v_det_tmp_nss IS NULL OR v_det_tmp_nss[1] = " " THEN
            -- se marca el rechazo
            LET v_registro_rechazado            = 1;
            LET v_det_tmp_nss                   = "NULO";
            LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_vacio; -- NSS vacío
         ELSE
            IF (v_det_tmp_nss[1]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[2]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[3]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[4]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[5]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[6]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[7]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[8]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[9]  NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[10] NOT MATCHES '[0-9]*') OR
               (v_det_tmp_nss[11] NOT MATCHES '[0-9]*') THEN
               LET v_registro_rechazado            = 1;
               LET afi_rch_afiliatorio_cod_rechazo = v_error_nss_inconsistente;
            END IF
         END IF

         IF v_registro_rechazado = 0 THEN
            IF (v_det_tmp_apaterno IS NULL) OR (v_det_tmp_apaterno = " ") AND
               (v_det_tmp_amaterno IS NULL) OR (v_det_tmp_amaterno = " ") AND
               (v_det_tmp_nombre   IS NULL) OR (v_det_tmp_nombre = " ") THEN
               LET v_rch            = 1;
               LET afi_rch_afiliatorio_cod_rechazo = v_error_nombre_vacio;
               LET afi_rch_afiliatorio_nombre      = NULL;
            END IF
         END IF

         TRACE "v_registro_rechazado : "||v_registro_rechazado;
         TRACE "v_rch : "||v_rch;

         IF (v_rch = 1) OR (v_registro_rechazado = 1) THEN
            -- se asignan los campos al registro de rechazo
            LET afi_rch_afiliatorio_tpo_movimiento = 75 ;
            LET afi_rch_afiliatorio_nrp            = NULL;
            LET afi_rch_afiliatorio_f_movimiento   = DAY(TODAY) || MONTH(TODAY) || YEAR(TODAY) ;
            LET afi_rch_afiliatorio_rfc            = v_det_tmp_rfc;
            LET afi_rch_afiliatorio_curp           = v_det_tmp_curp;
            LET afi_rch_afiliatorio_t_trabajador   = NULL ;
            LET afi_rch_afiliatorio_nss            = v_det_tmp_nss;

            IF( v_det_tmp_apaterno IS NOT NULL) AND (v_det_tmp_amaterno IS NOT NULL) AND (v_det_tmp_nombre IS NOT NULL) THEN
               LET afi_rch_afiliatorio_nombre = v_det_tmp_amaterno||"$"||v_det_tmp_nombre ;
            END IF

            IF( v_det_tmp_apaterno IS NULL) AND (v_det_tmp_amaterno IS NOT NULL AND v_det_tmp_nombre IS NOT NULL) THEN
               LET afi_rch_afiliatorio_nombre = v_det_tmp_amaterno||"$"||v_det_tmp_nombre ;
            END IF

            IF( v_det_tmp_amaterno IS NULL) AND (v_det_tmp_apaterno IS NOT NULL AND v_det_tmp_nombre IS NOT NULL) THEN
               LET afi_rch_afiliatorio_nombre = v_det_tmp_apaterno||"$"||v_det_tmp_nombre ;
            END IF

            IF( v_det_tmp_nombre IS NULL) AND (v_det_tmp_apaterno IS NOT NULL AND v_det_tmp_amaterno IS NOT NULL) THEN
               LET afi_rch_afiliatorio_nombre = v_det_tmp_apaterno||"$"||v_det_tmp_amaterno ;
            END IF

            LET afi_rch_afiliatorio_presentacion_extemp = NULL ;
            LET afi_rch_afiliatorio_jornada_semana      = NULL ;
            LET afi_rch_afiliatorio_sdi                 = NULL ;
            LET afi_rch_afiliatorio_sexo                = v_det_tmp_sexo;
            LET afi_rch_afiliatorio_nss_correcto        = NULL ;
            LET afi_rch_afiliatorio_nombre_correcto     = NULL ;
            LET afi_rch_afiliatorio_cod_rechazo         = 1 ;
            INSERT INTO afi_rch_afiliatorio
                      ( tpo_movimiento      ,
                        nrp                 ,
                        f_movimiento        ,
                        curp_rfc            ,
                        t_trabajador        ,
                        nss                 ,
                        nombre              ,
                        presentacion_extemp ,
                        jornada_semana      ,
                        sdi                 ,
                        sexo                ,
                        nss_correcto        ,
                        nombre_correcto     ,
                        riss_imss           ,
                        cod_rechazo         ,
                        folio_lote )
               VALUES ( afi_rch_afiliatorio_tpo_movimiento      ,
                        afi_rch_afiliatorio_nrp                 ,
                        afi_rch_afiliatorio_f_movimiento        ,
                        afi_rch_afiliatorio_curp                ,
                        afi_rch_afiliatorio_t_trabajador        ,
                        afi_rch_afiliatorio_nss                 ,
                        afi_rch_afiliatorio_nombre              ,
                        afi_rch_afiliatorio_presentacion_extemp ,
                        afi_rch_afiliatorio_jornada_semana      ,
                        afi_rch_afiliatorio_sdi                 ,
                        afi_rch_afiliatorio_sexo                ,
                        afi_rch_afiliatorio_nss_correcto        ,
                        afi_rch_afiliatorio_nombre_correcto     ,
                        tmp_riss_imss                           ,
                        afi_rch_afiliatorio_cod_rechazo         ,
                        p_folio);

            LET v_i_cnt_rechazos = v_i_cnt_rechazos + 1;

            CONTINUE FOREACH;
         ELSE
            LET v_nombre_imss = TRIM(v_det_tmp_apaterno)||"$"||TRIM(v_det_tmp_amaterno)||"$"||TRIM(v_det_tmp_nombre);

            IF LENGTH(v_det_tmp_rfc) <> 13 THEN
               LET v_det_tmp_rfc = "";
            END IF

            LET v_afi_dere_ind_nrp = " ";
            EXECUTE FUNCTION fn_apertura_cuenta_afi(v_det_tmp_nss               ,-- NSS
                                                    v_det_tmp_curp              ,-- CURP
                                                    v_det_tmp_rfc               ,-- RFC
                                                    v_afi_dere_ind_nrp          ,-- Ind relacion laboral
                                                    v_nombre_imss               ,-- Nombre
                                                    "I"                         ,-- tipo trabajador (IMSS)
                                                    0                           ,-- id_credito (sin credito)
                                                    p_folio                     ,-- folio
                                                    "P"                         )-- origen de afiliacion
                                               INTO v_afi_dere_id_derechohabiente;

            LET v_num_altas_realizados = v_num_altas_realizados + 1;
         END IF
      END IF

      LET v_det_tmp_rfc                  = "";
      LET v_det_tmp_curp                 = "";
      LET v_det_tmp_apaterno             = "";
      LET v_det_tmp_amaterno             = "";
      LET v_det_tmp_nombre               = "";
      LET v_det_tmp_f_nacimiento         = "";
      LET v_det_tmp_sexo                 = "";

      LET v_afi_dere_id_derechohabiente  = 0;
      LET v_afi_dere_folio_lote_modifica = 0;
      LET v_afi_dere_curp                = "";
      LET v_afi_dere_rfc                 = "";
      LET v_afi_dere_ind_nrp             = "";
      LET v_afi_dere_f_nacimiento        = "";
      LET v_afi_dere_nombre_imss         = "";
      LET v_afi_dere_nombre_af           = "";
      LET v_afi_dere_ap_paterno_af       = "";
      LET v_afi_dere_ap_materno_af       = "";
      LET v_afi_sexo                     = "";
      LET v_sexo_curp                    = "";
      LET v_bnd_sexo                     = 0;
      LET v_bnd_rfc                      = 0;
      LET v_registro_rechazado           = 0;
   END FOREACH ;
   
   INSERT INTO tmp_cnt_reg
        VALUES (v_num_altas_realizados,
                v_num_cambios_realizados,
                v_i_cnt_rechazos,
                today,
                p_usuario_cod );

--trace "actualiza tablas de control";

   UPDATE bat_ctr_operacion
      SET folio = p_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod = 2;

   -- se cambia el estatus del archivo a integrado
   UPDATE glo_ctr_archivo
      SET estado = 2, -- integrado
          folio = p_folio
    WHERE proceso_cod = p_proceso_cod
      AND nombre_archivo = p_nombre_archivo;

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado                         = 0;
   LET sql_err                               = 0;
   LET isam_err                              = 0;
   LET err_txt = "Cambios Operación 75 realizados: " ||v_num_cambios_realizados||", Altas realizadas: "||v_num_altas_realizados;

   RETURN sql_err, isam_err, err_txt, v_num_cambios_realizados,v_i_cnt_rechazos;

END FUNCTION
;


