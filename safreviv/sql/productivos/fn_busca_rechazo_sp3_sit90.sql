






CREATE FUNCTION "safreviv".fn_busca_rechazo_sp3_sit90(p_id_ocg_ctr_arch DECIMAL(9,0))

   RETURNING SMALLINT

   DEFINE v_error              SMALLINT;
   DEFINE v_id_ocg_solic_ug    DECIMAL(9,0);
   DEFINE v_id_ocg_detalle     DECIMAL(9,0);
   DEFINE v_inconsistencia     smallint    ;
   DEFINE v_bnd_rl             smallint    ;
   DEFINE v_bnd_inconsistencia smallint    ;
   DEFINE v_f_venc_imp_solic   DATE        ;
   DEFINE v_f_alta_rl          DATE        ;
   DEFINE v_f_alta             CHAR(10)        ;
   DEFINE v_f_baja_rl          DATE        ;
   DEFINE v_nss_rl             CHAR(11)    ;
   DEFINE v_nss                CHAR(11)    ;
   DEFINE v_id_dh              DECIMAL(9,0);
   DEFINE v_f_proceso          DATE        ;

    ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_busca_rechazo_sp3.sql.trace';
   TRACE ON;

   LET v_error              = 0;
   LET v_id_ocg_solic_ug     = "";
   LET v_id_ocg_detalle      = "";
   LET v_inconsistencia      = "";
   LET v_bnd_rl              = 0;
   LET v_bnd_inconsistencia  = 0;
   LET v_f_venc_imp_solic    = "";
   LET v_f_alta_rl           = "";
   LET v_f_alta              = "";
   LET v_f_baja_rl           = "";
   LET v_nss_rl              = "";
   LET v_nss                 = "";
   LET v_id_dh               = "";
   LET v_f_proceso           = "";

   FOREACH
      SELECT g.id_ocg_solicitud_ug,
             g.id_ocg_detalle,
             g.id_derechohabiente,
             g.f_vencimiento,
             d.nss,
             d.f_proceso
        INTO v_id_ocg_solic_ug,
             v_id_ocg_detalle,
             v_id_dh,
             v_f_venc_imp_solic,
             v_nss,
             v_f_proceso
        from ocg_solicitud_uso_garantia g,
             ocg_detalle d
       where g.id_ocg_detalle = d.id_ocg_detalle
         and d.f_proceso   >= "12/01/2017"
         and g.situacion   = 90
         and d.subproceso  = 3
         and g.diagnostico = 1


      FOREACH
         SELECT nss,
                (lpad(month(f_alta)+1,2,0))||
                "/01/"||
                (year(f_alta)),
                (f_baja-(day(f_baja)))
           INTO v_nss_rl,
                v_f_alta,
                v_f_baja_rl
           FROM ocg_relacion_laboral
          WHERE nss = v_nss

         IF v_f_alta[1,2] = 13 THEN
            LET v_f_alta = "01"||"/"||"01"||"/"||((v_f_alta[7,10])+1);
            LET v_f_alta_rl = v_f_alta;
         ELSE
            LET v_f_alta_rl = v_f_alta;
         END IF

         -- Si tiene relaci¦n laboral se prente una bandera
         IF v_f_venc_imp_solic BETWEEN v_f_alta_rl AND v_f_baja_rl THEN
            LET v_bnd_rl = 1;
            EXIT FOREACH;
         END IF
      END FOREACH;

      IF v_bnd_rl = 1 THEN
         LET v_inconsistencia = "44";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO safre_tmp:ocg_sp003_sit90
              VALUES (
                     v_id_ocg_solic_ug,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_venc_imp_solic,
                     v_nss,
                     v_f_proceso);

      END IF;

      LET v_id_ocg_solic_ug     = "";
      LET v_id_ocg_detalle      = "";
      LET v_inconsistencia      = "";
      LET v_bnd_rl              = 0;
      LET v_bnd_inconsistencia  = 0;
      LET v_f_venc_imp_solic    = "";
      LET v_f_alta_rl           = "";
      LET v_f_alta              = "";
      LET v_f_baja_rl           = "";
      LET v_nss_rl              = "";
      LET v_nss                 = "";
      LET v_id_dh               = "";
      LET v_f_proceso           = "";

   END FOREACH

   RETURN v_error;

END FUNCTION;


