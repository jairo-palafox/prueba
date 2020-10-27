#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRL54                                        #
#Objetivo          => Lanzador para la ejecución del proceso de     #
#                      actualización de marcas                      #
#Autor             => Hector Fabián Jiménez Lara                    #
#Fecha inicio      => 28 de Agosto del 2015                         #
#####################################################################
DATABASE safre_viv

GLOBALS
   DEFINE g_proceso_cod            INTEGER
   DEFINE g_usuario                CHAR (20)
   DEFINE p_tpo_ejecucion          SMALLINT
   DEFINE p_s_titulo               STRING        -- Título de la ventana
   DEFINE v_cnt                    INTEGER
   DEFINE v_s_qry                  STRING
   DEFINE i                        INTEGER
   DEFINE j                        INTEGER
   DEFINE v_cadena                 STRING
   DEFINE v_cod_rechazo            INTEGER
   DEFINE v_arr_marca DYNAMIC ARRAY OF RECORD
      nss                          LIKE afi_derechohabiente.nss,
      num_credito                  LIKE cre_acreditado.num_credito,
      tpo_credito                  LIKE cre_acreditado.tpo_credito,
      estado                       LIKE cre_acreditado.estado,
      edo_procesar                 LIKE cre_acreditado.edo_procesar,
      marca                        LIKE sfr_marca_activa.marca,
      situacion                    SMALLINT,
      tpo                          SMALLINT,
      liq_deudor                   SMALLINT,
      int_prc                      SMALLINT,
      checkbox                     SMALLINT
   END RECORD
END GLOBALS

MAIN
   DEFINE v_ax_arr_desm DYNAMIC ARRAY OF RECORD
      nss                         LIKE afi_derechohabiente.nss,
      num_credito                 LIKE cre_acreditado.num_credito,
      tpo_credito                 LIKE cre_acreditado.tpo_credito,
      estado                      LIKE cre_acreditado.estado,
      edo_procesar                LIKE cre_acreditado.edo_procesar,
      marca                       LIKE sfr_marca_activa.marca,
      situacion                   SMALLINT,
      tpo                         SMALLINT,
      liq_deudor                  SMALLINT,
      int_prc                     SMALLINT,
      checkbox                    SMALLINT
   END RECORD

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   CALL fn_llena_arreglo()

   OPEN WINDOW w1 WITH FORM "AGRL561"

      INPUT ARRAY v_arr_marca FROM r_marca.* ATTRIBUTE( WITHOUT DEFAULTS  ,
                                                        APPEND ROW = FALSE,
                                                        DELETE ROW = FALSE,
                                                        INSERT ROW = FALSE,
                                                        ACCEPT = FALSE    ,
                                                        UNBUFFERED        )

         ON ACTION Proceder
            CALL v_ax_arr_desm.clear()
            DISPLAY " Se procesa lo siguiente"
            LET j = 1
            FOR i = 1 TO v_arr_marca.getLength()
               -- Si el checkbox esta prendido
               -- se llena el arr auxiliar con las seleccionadas
               IF v_arr_marca[i].checkbox = 1 THEN
                  LET v_ax_arr_desm[j].* = v_arr_marca[i].*
                  LET j = j + 1
               END IF
            END FOR

            -- Se invoca a la función que realiza las validaciones
            CALL fn_valida_desm(v_ax_arr_desm)

            CALL fn_mensaje("Atención","Finaliza Actualización","stop")
            
            CALL fn_llena_arreglo()

            EXIT INPUT 

         ON ACTION CANCEL
            EXIT INPUT

      END INPUT
   CLOSE WINDOW w1

END MAIN

FUNCTION fn_valida_desm(p_arr_desm)
   DEFINE p_arr_desm DYNAMIC ARRAY OF RECORD
      nss                         LIKE afi_derechohabiente.nss,
      num_credito                 LIKE cre_acreditado.num_credito,
      tpo_credito                 LIKE cre_acreditado.tpo_credito,
      estado                      LIKE cre_acreditado.estado,
      edo_procesar                LIKE cre_acreditado.edo_procesar,
      marca                       LIKE sfr_marca_activa.marca,
      situacion                   SMALLINT,
      tpo                         SMALLINT,                                     -- identificador marcas(0=interna, 1=procesar)
      liq_deudor                  SMALLINT,
      int_prc                     SMALLINT,
      checkbox                    SMALLINT
   END RECORD
   DEFINE v_id_dh                 LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_id_cre_acre           LIKE cre_acreditado.id_cre_acreditado
   DEFINE v_tpo_orig              LIKE cre_acreditado.tpo_originacion
   DEFINE v_cta_id_dh             LIKE cre_acreditado.id_derechohabiente
   DEFINE v_f_credito             DATE
   DEFINE v_f_otorga              DATE
   DEFINE v_folio                 LIKE cre_ctr_archivo.folio_archivo
   DEFINE v_modulo_cod            LIKE cta_marca_ws.modulo_cod
   DEFINE v_ax_marca_procesar     LIKE cta_marca_ws.marca_procesar
   DEFINE v_ax_marca_int          LIKE sfr_marca_activa.marca
   DEFINE v_rec_ax_cre_edo RECORD
      id_cre_acreditado           LIKE cre_acreditado.id_cre_acreditado,
      edo_procesar                LIKE cre_acreditado.edo_procesar
   END RECORD
   DEFINE v_rec_ax_edos RECORD
      estado                      LIKE cre_Acreditado.estado,
      edo_procesar                LIKE cre_acreditado.edo_procesar
   END RECORD
   DEFINE v_bnd_borrar            SMALLINT
   DEFINE v_existe_marca          SMALLINT

   LET v_bnd_borrar = 1
   LET v_modulo_cod = "0"

   FOR i = 1 TO p_arr_desm.getLength()
      DISPLAY "MARCA   : ",p_arr_desm[i].marca
      CASE
         WHEN p_arr_desm[i].marca = 231
             LET v_modulo_cod = "03"

         WHEN p_arr_desm[i].marca = 232
               LET v_modulo_cod = "16"

         WHEN p_arr_desm[i].marca = 234
               LET v_modulo_cod = "43"
      END CASE

      -- Se obtiene el id_derechohabiente
      SELECT UNIQUE a.id_derechohabiente,
             c.id_cre_acreditado,
             c.tpo_originacion,
             c.f_otorga,
             ct.folio_archivo
        INTO v_id_dh,
             v_id_cre_acre,
             v_tpo_orig,
             v_f_otorga,
             v_folio
        FROM afi_derechohabiente a,
             cre_acreditado c,
             cat_tipo_credito t,
             cre_ctr_archivo ct
       WHERE a.id_derechohabiente = c.id_derechohabiente
         AND c.tpo_originacion = t.tpo_originacion
         AND c.id_cre_ctr_archivo = ct.id_cre_ctr_archivo
         AND nss = p_arr_desm[i].nss
         AND num_credito = p_arr_desm[i].num_credito

      -- Se extrae el tipo de proceso
      IF v_tpo_orig = 1 OR v_tpo_orig = 2 THEN
         LET g_proceso_cod = 308
      ELSE
         LET g_proceso_cod = 1208
      END IF

      -- Marca interna
      IF p_arr_desm[i].tpo = 0 THEN
         -- Se valida si es marca o desmarca (situación, 0=desmarca , 2=marca)
         IF p_arr_desm[i].situacion = 0 THEN
            LET v_s_qry = "SELECT id_cre_acreditado,
                                  edo_procesar
                             FROM cre_acreditado
                            WHERE id_derechohabiente = "|| v_id_dh ||
                             "AND tpo_credito = " || p_arr_desm[i].tpo_credito ||
                             "AND num_credito = " || p_arr_desm[i].num_credito ||
                             "AND estado = 140"

            PREPARE prp_cons_c_ac FROM v_s_qry
            DECLARE cur_cons_c_ac CURSOR FOR prp_cons_c_ac
            LET v_cnt = 1
            FOREACH cur_cons_c_ac INTO v_rec_ax_cre_edo.*
               IF v_rec_ax_cre_edo.edo_procesar = 70 OR
                  v_rec_ax_cre_edo.edo_procesar = 80 OR
                  v_rec_ax_cre_edo.edo_procesar = 85 THEN

                  LET v_cadena="Registro no se puede desmarcar\n
                                porque hay una solicitud de saldo que no ha respondido Procesar\n
                                NSS : "|| p_arr_desm[i].nss || "\n
                                Número de Crédito : " || p_arr_desm[i].num_credito

                  CALL fn_mensaje("Aviso",v_cadena,"stop")
                  EXIT FOREACH
               END IF
               LET v_bnd_borrar = 0
            END FOREACH

            DISPLAY "Derechohabiente antes del qry: ",v_id_dh
            DISPLAY "Modulo cod antes del qry     : ",v_modulo_cod
            
            LET v_s_qry = "SELECT estado,
                                  edo_procesar
                             FROM cre_uso_garantia
                            WHERE id_derechohabiente = " || v_id_dh ||
                             "AND tpo_transferencia  = '" || v_modulo_cod || "' " ||
                             "AND id_cre_ctr_archivo IN (
                                  SELECT id_cre_ctr_archivo
                                    FROM cre_ctr_archivo
                                   WHERE operacion = 43)
                              AND estado = 140"

            PREPARE prp_edos_pr_b FROM v_s_qry
            DECLARE cur_edos_pr_b CURSOR FOR prp_edos_pr_b
            FOREACH cur_edos_pr_b INTO v_rec_ax_edos.*
               IF v_rec_ax_cre_edo.edo_procesar = 70 OR
                  v_rec_ax_cre_edo.edo_procesar = 80 OR
                  v_rec_ax_cre_edo.edo_procesar = 85 THEN

                  LET v_cadena="Registro no se puede desmarcar\n
                                porque hay una solicitud de saldo que no ha respondido Procesar\n
                                NSS : "|| p_arr_desm[i].nss || "\n
                                Número de Crédito : " || p_arr_desm[i].num_credito

                  CALL fn_mensaje("Aviso",v_cadena,"stop")
                  EXIT FOREACH
               END IF
               LET v_bnd_borrar = 0
            END FOREACH

            SELECT UNIQUE id_derechohabiente,
                   f_credito
              INTO v_cta_id_dh,
                   v_f_credito
              FROM cta_credito
             WHERE id_derechohabiente = v_id_dh 
               AND num_credito = p_arr_desm[i].num_credito

            IF v_cta_id_dh > 0 OR v_cta_id_dh IS NOT NULL THEN
               -- Se inserta en cta_his_credito
               LET v_s_qry = "INSERT INTO cta_his_credito
                                   VALUES(?,?,?,?,?,4,TODAY)"

               PREPARE prp_ins_his_cr FROM v_s_qry
               EXECUTE prp_ins_his_cr USING v_id_dh,
                                            g_proceso_cod,
                                            p_arr_desm[i].tpo_credito,
                                            p_arr_desm[i].num_credito,
                                            v_f_credito

               -- Se elimina de cta_credito
               LET v_s_qry = "DELETE 
                                FROM cta_credito
                              WHERE id_derechohabiente = ?
                                AND f_credito          = ?"

               PREPARE prp_del_cta FROM v_s_qry
               EXECUTE prp_del_cta USING v_cta_id_dh,
                                         v_f_credito

            END IF

            -- Se ejecuta la función que desmarca la cuenta
            LET v_s_qry = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,0,0,?,?)"

            PREPARE prp_exe_desmarca_interna FROM v_s_qry

            EXECUTE prp_exe_desmarca_interna USING v_id_dh            ,
                                                   p_arr_desm[i].marca,
                                                   v_id_cre_acre      ,
                                                   g_usuario          ,
                                                   g_proceso_cod
                                              INTO v_cod_rechazo

            IF v_cod_rechazo <> 0 THEN
               LET v_cadena = "Ocurrió un error al desmarcar\n Id Derechohabiente :" || v_id_dh ||
                              "\n Marca : " || p_arr_desm[i].marca

               CALL fn_mensaje("Error",v_cadena,"stop")
            END IF

            -- Se actualiza el estado
            -- Si la marca interna es de procesar
            IF p_arr_desm[i].marca = 231 OR
               p_arr_desm[i].marca = 232 OR
               p_arr_desm[i].marca = 234 THEN

               UPDATE cre_acreditado
                  SET edo_procesar = 210
                WHERE id_derechohabiente = v_id_dh
                  AND id_cre_acreditado  = v_id_cre_acre

            ELSE

               UPDATE cre_acreditado
                  SET estado = 170
                WHERE id_derechohabiente = v_id_dh
                  AND id_cre_acreditado  = v_id_cre_acre

            END IF

         -- situacion 2 (marca)
         ELSE
            -- Si es una marca interna, y tiene una marca interna Procesar
            IF p_arr_desm[i].marca <> 231 OR
               p_arr_desm[i].marca <> 232 OR
               p_arr_desm[i].marca <> 234 THEN

               LET v_s_qry = " SELECT s.marca
                                 FROM cre_acreditado c,
                                      sfr_marca_activa s,
                                      afi_Derechohabiente a
                                WHERE a.nss = " || p_arr_desm[i].nss ||
                                 "AND a.id_derechohabiente = c.id_derechohabiente
                                  AND a.id_derechohabiente = s.id_derechohabiente
                                  AND c.id_Cre_acreditado = s.n_referencia
                                  AND (s.marca IN(SELECT marca_inf
                                                    FROM cat_tipo_credito)
                                      OR s.marca IN(SELECT marca_prc
                                                      FROM cat_tipo_credito))"

               PREPARE prp_cons_mar_proc FROM v_s_qry
               DECLARE cur_mar_proc CURSOR FOR prp_cons_mar_proc
               LET v_cnt = 1
               FOREACH cur_mar_proc INTO v_ax_marca_int
                  IF v_ax_marca_int = 231 OR
                     v_ax_marca_int = 232 OR
                     v_ax_marca_int = 234 THEN

                     LET v_s_qry = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,0,0,?,?)"
                     PREPARE prp_desmacaA FROM v_s_qry
                     EXECUTE prp_desmacaA USING v_id_dh       ,
                                                v_ax_marca_int,
                                                v_id_cre_acre ,
                                                g_usuario     ,
                                                g_proceso_cod 
                                           INTO v_cod_rechazo

                     EXIT FOREACH
                  END IF
                  LET v_cnt = v_cnt + 1
               END FOREACH
            END IF

            -- Verificación de registro en cta_credito
            SELECT UNIQUE id_derechohabiente,
                   f_credito
              INTO v_cta_id_dh,
                   v_f_credito
              FROM cta_credito
             WHERE id_derechohabiente = v_id_dh 
               AND num_credito = p_arr_desm[i].num_credito

            IF v_cta_id_dh = 0 OR v_cta_id_dh IS NULL THEN
               LET v_s_qry = "INSERT INTO cta_credito
                                   VALUES(?,?,?,?,?)"

               PREPARE prp_ins_cta_cr FROM v_s_qry
               EXECUTE prp_ins_cta_cr USING v_id_dh,
                                            g_proceso_cod,
                                            p_arr_desm[i].tpo_credito,
                                            p_arr_desm[i].num_credito,
                                            v_f_otorga

            END IF

            LET v_s_qry = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,0,0,TODAY,?,?)"

            PREPARE prp_exe_marca_int FROM v_s_qry
            EXECUTE prp_exe_marca_int USING v_id_dh,
                                            p_arr_desm[i].marca,
                                            v_id_cre_acre,
                                            v_folio,
                                            p_arr_desm[i].estado,
                                            g_usuario,
                                            g_proceso_cod
                                       INTO v_cod_rechazo

            DISPLAY "MARCA : ", p_arr_desm[i].marca
            IF p_arr_desm[i].marca = 231 OR
               p_arr_desm[i].marca = 232 OR
               p_arr_desm[i].marca = 234 THEN

               LET v_s_qry = "UPDATE cre_acreditado
                                 SET estado = 120
                               WHERE id_derechohabiente = ?
                                 AND id_cre_acreditado  = ?"

               PREPARE prp_upd_cre_acA FROM v_s_qry

               EXECUTE prp_upd_cre_acA USING v_id_dh,
                                             v_id_cre_acre

               -- Se realiza el reverso de la desmarca cuando es una marca procesar interna
               LET v_s_qry = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"

               PREPARE prp_rev_marca FROM v_s_qry

               EXECUTE prp_rev_marca USING v_id_dh,
                                           v_ax_marca_int,
                                           v_id_cre_acre,
                                           v_folio
                                             
            ELSE

               LET v_s_qry = "UPDATE cre_acreditado
                                 SET estado = 140
                               WHERE id_derechohabiente = ?
                                 AND id_cre_acreditado  = ?"

               PREPARE prp_upd_cre_ac FROM v_s_qry

               EXECUTE prp_upd_cre_ac USING v_id_dh,
                                            v_id_cre_acre

            END IF

            IF p_arr_desm[i].liq_deudor = 1 THEN
               UPDATE cre_acreditado
                  SET estado = 20
                WHERE id_derechohabiente = v_id_dh
                  AND id_cre_acreditado  = v_id_cre_acre
            ELSE
               UPDATE cre_acreditado
                  SET estado = 140
                WHERE id_derechohabiente = v_id_dh
                  AND id_cre_acreditado = v_id_cre_acre
            END IF

         END IF

       -- Marca procesar tipo 1
      ELSE
         CASE
            WHEN p_arr_desm[i].marca = 231
               LET v_modulo_cod        = "03"
               LET v_ax_marca_procesar = "01"

            WHEN p_arr_desm[i].marca = 232
               LET v_modulo_cod        = "16"
               LET v_ax_marca_procesar = "02"

            WHEN p_arr_desm[i].marca = 234
               LET v_modulo_cod        = "43"
               LET v_ax_marca_procesar = "04"

         END CASE

         -- Se valida si es marca o desmarca
         IF p_arr_desm[i].situacion = 0 THEN
            LET v_s_qry = "INSERT INTO cta_marca_ws
                                VALUES(?,?,?,?,?,TODAY,1,null,null,?,?,?,?,?,?)"

            PREPARE prp_ins_cta_ws FROM v_s_qry

            EXECUTE prp_ins_cta_ws USING v_id_dh,
                                         v_id_cre_acre,
                                         v_modulo_cod,
                                         p_arr_desm[i].tpo_credito,
                                         p_arr_desm[i].marca,
                                         p_arr_desm[i].situacion,
                                         p_arr_desm[i].num_credito,
                                         v_f_otorga,
                                         v_ax_marca_procesar,
                                         v_folio,
                                         g_usuario

         ELSE -- situación 2 (marca)
            LET v_s_qry = "INSERT INTO cta_marca_ws
                                VALUES(?,?,?,?,?,TODAY,1,null,null,?,?,?,?,?,?)"

            PREPARE prp_ins_marca_ws FROM v_s_qry

            EXECUTE prp_ins_marca_ws USING v_id_dh,
                                           v_id_cre_acre,
                                           v_modulo_cod,
                                           p_arr_desm[i].tpo_credito,
                                           p_arr_desm[i].marca,
                                           p_arr_desm[i].situacion,
                                           p_arr_desm[i].num_credito,
                                           v_f_otorga,
                                           v_ax_marca_procesar,
                                           v_folio,
                                           g_usuario

         END IF
      END IF

      -- Se borra de la tabla de paso dependiendo de la bandera
      IF v_bnd_borrar = 1 THEN
         LET v_s_qry = " DELETE
                           FROM act_marca_desm_ag
                          WHERE nss          = ?
                            AND num_credito  = ?
                            AND estado       = ?
                            AND edo_procesar = ?
                            AND marca        = ? "

         PREPARE prp_del_paso FROM v_s_qry

         EXECUTE prp_del_paso USING p_arr_desm[i].nss,
                                    p_arr_desm[i].num_credito,
                                    p_arr_desm[i].estado,
                                    p_arr_desm[i].edo_procesar,
                                    p_arr_desm[i].marca
      END IF
   END FOR
END FUNCTION

FUNCTION fn_llena_arreglo()
   CALL v_arr_marca.clear()

   LET v_s_qry = "SELECT *
                    FROM act_marca_desm_ag"

   PREPARE prp_cons FROM v_s_qry
   DECLARE cur_cons CURSOR FOR prp_cons

   LET v_cnt = 1
   FOREACH cur_cons INTO v_arr_marca[v_cnt].*
      LET v_cnt = v_cnt + 1
   END FOREACH
   CALL v_arr_marca.deleteElement(v_arr_marca.getLength())
END FUNCTION