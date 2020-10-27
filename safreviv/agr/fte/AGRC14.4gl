DATABASE safre_viv

GLOBALS
DEFINE arr_creditos DYNAMIC ARRAY OF RECORD
         v_id_cre_tramite DECIMAL(9,0),
         v_id_derechohabiente DECIMAL(9,0),
         v_num_credito DECIMAL(10,0),
         v_f_vigencia DATE,
         v_estado SMALLINT,
         v_id_cre_acreditado DECIMAL(9,0),
         v_f_proceso DATE
END RECORD 

DEFINE arr_creditos_real DYNAMIC ARRAY OF RECORD 
         v_id_cre_tramite    DECIMAL(10,0),
         v_id_cre_acreditado DECIMAL(10,0),
         v_num_credito       DECIMAL(10,0),
         v_f_vigencia        DATE,
         v_f_proceso         DATETIME YEAR TO SECOND,
         v_f_cancela         DATETIME YEAR TO SECOND,
         v_seleccion         SMALLINT
END RECORD 
END GLOBALS

MAIN
DEFINE v_nss CHAR(11),
       v_id_derechohabiente DECIMAL(10,0),
       v_tot_creditos SMALLINT,
       v_confirma SMALLINT,
       v_seleccion SMALLINT,
       f_ventana ui.Window,
       f_forma ui.Form,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_s_titulo STRING 

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRC14.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   OPEN WINDOW v_creditos WITH FORM "AGRC140"
   DIALOG ATTRIBUTES(UNBUFFERED) 

      INPUT BY NAME v_nss

         BEFORE INPUT
            LET f_ventana = ui.Window.getCurrent()
            LET f_forma = f_ventana.getForm()

            CALL f_forma.setElementHidden("gr_detalles", 1)
      END INPUT

         ON ACTION ACCEPT
            {IF getLength(v_nss) <> 11 THEN
               CALL fn_mensaje ("Atención", "Debe capturar 11 dígitos", "stop")
               NEXT FIELD v_nss
            ELSE}
               SELECT id_derechohabiente
               INTO   v_id_derechohabiente
               FROM   afi_derechohabiente
               WHERE  nss = v_nss

               IF v_id_derechohabiente IS NULL THEN
                  CALL fn_mensaje ("Atención", "No existe el NSS capturado", "stop")
                  NEXT FIELD v_nss
               ELSE
                  CALL f_forma.setElementHidden("gr_detalles", 0)

                  CALL fn_consulta_credito(v_id_derechohabiente)
                  RETURNING v_tot_creditos

                  IF v_tot_creditos > 1 THEN
                     INPUT ARRAY arr_creditos_real WITHOUT DEFAULTS
                     FROM rec_creditos.*
                     ATTRIBUTES (INSERT ROW = FALSE, 
                                 APPEND ROW = FALSE, 
                                 DELETE ROW = FALSE,
                                 ACCEPT = FALSE,
                                 CANCEL = FALSE)


                        ON CHANGE v_seleccion
                           LET v_seleccion = 1 

                        ON ACTION cancelacion

                           IF v_seleccion = 0 THEN
                              CALL fn_mensaje ("Atención","Debe seleccionar \n un crédito en trámite","")
                           ELSE

                              CALL fn_ventana_confirma ("Atención", "¿Desea cancelar por desistimiento \n el crédito en trámite elegido?", "stop")
                              RETURNING v_confirma -- 1-confirma, 0-Cancela

                              IF v_confirma = 1 THEN
                                 CALL fn_cancela_credito(arr_creditos_real[ARR_CURR()].v_id_cre_acreditado,
                                                         arr_creditos_real[ARR_CURR()].v_id_cre_tramite,
                                                         arr_creditos_real[ARR_CURR()].v_num_credito,
                                                         v_id_derechohabiente,
                                                         p_usuario_cod)
                                 CALL fn_mensaje ("Atención","Se ha enviado la cancelación \n del crédito en trámite","") 

                                 EXIT INPUT
                                 EXIT DIALOG
                              END IF
                          END IF
                        ON ACTION salir
                           CALL f_forma.setElementHidden("gr_detalles", 1)
                           EXIT INPUT
                     END INPUT
                  ELSE
                     LET v_nss = ""
                     DISPLAY BY NAME v_nss
                     CALL f_forma.setElementHidden("gr_detalles", 1)
                     CALL fn_mensaje ("Atención","No existe un crédito en trámite para el NSS capturado","") 
                  END IF
               END IF
            --END IF

         ON ACTION salir
            EXIT DIALOG

   END DIALOG
   CLOSE WINDOW v_creditos

END MAIN

#OBJETIVO: Consultar los créditos en trámite para un NSS determinado
FUNCTION fn_consulta_credito(p_id_derechohabiente)
DEFINE p_id_derechohabiente DECIMAL(10,0),
       i INTEGER

   DECLARE cur_creditos CURSOR FOR {SELECT t.*, c.id_cre_acreditado
                                   FROM cre_tramite t, cre_acreditado c
                                   WHERE t.id_derechohabiente = c.id_derechohabiente
                                   AND t.estado = 18
                                   AND t.num_credito = c.num_credito
                                   AND t.estado = c.estado
                                   AND t.id_derechohabiente = ?}
                                   SELECT UNIQUE t.id_cre_tramite ict,
                                          t.id_derechohabiente id,
                                          t.num_credito nci,
                                          t.f_vigencia  f_vig,
                                          t.estado edo,
                                          c.id_cre_acreditado ic
                                   FROM   cre_tramite t, cre_acreditado c, cre_his_tramite h
                                   WHERE  t.id_cre_tramite = h.id_cre_tramite
                                   AND    t.id_derechohabiente = c.id_derechohabiente
                                   AND    t.estado = 18
                                   AND    h.num_credito = c.num_credito
                                   AND    t.estado = c.estado
                                   AND    t.id_derechohabiente = ?

   LET i = 1

   FOREACH cur_creditos USING p_id_derechohabiente
                        INTO arr_creditos[i].v_id_cre_tramite,
                             arr_creditos[i].v_id_derechohabiente,
                             arr_creditos[i].v_num_credito,
                             arr_creditos[i].v_f_vigencia,
                             arr_creditos[i].v_estado,
                             arr_creditos[i].v_id_cre_acreditado

      SELECT MIN(h.f_proceso)
      INTO arr_creditos[i].v_f_proceso
      FROM cre_his_tramite h
      WHERE h.id_cre_tramite = arr_creditos[i].v_id_cre_tramite

      LET arr_creditos_real[i].v_id_cre_acreditado = arr_creditos[i].v_id_cre_acreditado 
      LET arr_creditos_real[i].v_id_cre_tramite = arr_creditos[i].v_id_cre_tramite 
      LET arr_creditos_real[i].v_num_credito = arr_creditos[i].v_num_credito 
      LET arr_creditos_real[i].v_f_vigencia = arr_creditos[i].v_f_vigencia
      LET arr_creditos_real[i].v_f_proceso =  arr_creditos[i].v_f_proceso
      LET arr_creditos_real[i].v_f_cancela = CURRENT YEAR TO SECOND

      LET i = i + 1
   END FOREACH

   RETURN i
END FUNCTION

#OBJETIVO: Cancelar el crédito en trámite para el NSS solicitado
FUNCTION fn_cancela_credito(p_id_cre_acreditado,p_id_cre_tramite, p_num_credito, p_id_derechohabiente, p_usuario_cod)
DEFINE p_id_cre_tramite     DECIMAL(10,0),
       p_id_cre_acreditado  DECIMAL(10,0),
       p_num_credito        DECIMAL(10,0), 
       p_id_derechohabiente DECIMAL (9,0),
       p_usuario            CHAR(20),
       v_desmarca           STRING,
       v_n_refrerencia      DECIMAL (9,0),
       v_folio_marca        DECIMAL (9,0),
       p_usuario_cod        CHAR(20),
       v_res_desmarca       SMALLINT

DISPLAY "DATOS PARA CANCELACIÓN DE CRÉDITO : ", p_id_cre_tramite, "\n", 
p_id_cre_acreditado, "\n", 
p_num_credito, "\n", 
p_id_derechohabiente, "\n", 
p_usuario_cod, "\n"
       
       UPDATE cre_his_tramite
       SET    estado = 340
       WHERE  id_cre_tramite = p_id_cre_tramite

       UPDATE cre_tramite
       SET estado = 340
       WHERE id_cre_tramite = p_id_cre_tramite

       UPDATE cre_acreditado
       SET estado = 340
       WHERE id_cre_acreditado = p_id_cre_acreditado
       AND num_credito IN(SELECT num_credito
                          FROM cre_his_tramite
                          WHERE id_cre_tramite = p_id_cre_tramite);

       SELECT n_referencia, folio
       INTO   v_n_refrerencia,
              v_folio_marca
       FROM   sfr_marca_activa
       WHERE  id_derechohabiente = p_id_derechohabiente
       AND    marca = 213;

       LET  v_desmarca = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"

       PREPARE prp_demarca FROM v_desmarca
       EXECUTE prp_demarca USING p_id_derechohabiente,
                                 "213",
                                 v_n_refrerencia,
                                 "40",
                                 "0",
                                 p_usuario_cod,
                                 "352"
                           INTO  v_res_desmarca;

       DELETE
       FROM  cta_marca_ws
       WHERE id_derechohabiente = p_id_derechohabiente
       AND   situacion in(0,2)
       AND   marca_procesar = "04";

       DELETE
         FROM cta_marca_ws
        WHERE id_derechohabiente = p_id_derechohabiente
          AND situacion = 0

       INSERT INTO cta_marca_ws  SELECT c.id_derechohabiente,
                                        c.id_cre_acreditado,
                                        '43',
                                        c.tpo_credito,
                                        '234',
                                        TODAY,
                                        1,
                                        "",
                                        "",
                                        '0',
                                        c.num_credito,
                                        c.f_otorga,
                                        '04',
                                        r.folio_archivo,
                                        "infonavit"
                                 FROM   cre_acreditado c, cre_ctr_archivo r
                                 WHERE  id_cre_acreditado = p_id_cre_acreditado
                                 AND    c.id_cre_ctr_archivo = r.id_cre_ctr_archivo
                                 ;

       INSERT INTO cre_tramite_desistimiento(id_cre_tramite,
                                             f_actualiza,
                                             usuario)
                                      VALUES(p_id_cre_tramite,
                                             TODAY,
                                             p_usuario);

END FUNCTION