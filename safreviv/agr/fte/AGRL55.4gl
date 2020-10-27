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
   DEFINE g_usuario                 CHAR (20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_s_titulo                STRING        -- Título de la ventana
   DEFINE v_s_qry                   STRING
   DEFINE v_bandera                 SMALLINT
   DEFINE v_cnt                     INTEGER
   DEFINE v_pos                     INTEGER
   DEFINE j                         INTEGER
   DEFINE i                         INTEGER   
   DEFINE v_arr_desmarca DYNAMIC ARRAY OF RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      checkbox                      SMALLINT,
      ck_procesar                   SMALLINT 
   END RECORD
   DEFINE v_arr_marca DYNAMIC ARRAY OF RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      marca_prc                     LIKE cat_tipo_credito.marca_prc,
      ck_liq                        SMALLINT,
      checkbox                      SMALLINT,
      ck_int_pr                     SMALLINT,
      ck_procesar                   SMALLINT
   END RECORD
END GLOBALS

MAIN
   DEFINE v_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_situacion               SMALLINT

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   OPEN WINDOW w_m_marca WITH FORM "AGRL551"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE, CANCEL=FALSE)
         ON ACTION Marca
            -- Situacion 2=Marca 
            LET v_situacion = 2
            -- Se valida el nss
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca  la función que actualiza la marca
               CALL fn_act_marca(v_nss,v_situacion)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF

         ON ACTION Desmarca
            -- situacion 0=desmarca
            LET v_situacion = 0
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca a la función que actualiza la desmarca
               CALL fn_act_desmarca(v_nss,v_situacion)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF

         ON ACTION Cancelar
            EXIT INPUT

      END INPUT
   CLOSE WINDOW w_m_marca
END MAIN


FUNCTION fn_act_marca(p_nss,p_situacion)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE p_situacion               SMALLINT
   DEFINE v_ax_arr_marca DYNAMIC ARRAY OF RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      marca_prc                     LIKE cat_tipo_credito.marca_prc,
      ck_liq                        SMALLINT,
      checkbox                      SMALLINT,
      ck_int_pr                     SMALLINT,
      ck_procesar                   SMALLINT
   END RECORD
   DEFINE v_rec_marca RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      marca_prc                     LIKE cat_tipo_credito.marca_prc,
      ck_liq                        SMALLINT,
      checkbox                      SMALLINT,
      ck_int_pr                     SMALLINT,
      ck_procesar                   SMALLINT
   END RECORD
   DEFINE v_bandera_tpo             SMALLINT

   CALL v_arr_marca.clear()

   LET v_s_qry = "SELECT c.num_credito,
                         c.tpo_credito,
                         c.estado,
                         c.edo_procesar,
                         ct.marca_inf,
                         ct.marca_prc
                    FROM cre_acreditado c,
                         cat_tipo_credito ct,
                   OUTER(sfr_marca_activa  s) ,
                         afi_Derechohabiente a
                   WHERE a.nss = '" || p_nss || "'" ||
                    "AND a.id_derechohabiente = c.id_derechohabiente
                     AND a.id_derechohabiente = s.id_derechohabiente
                     AND c.id_Cre_acreditado  = s.n_referencia
                     AND c.tpo_originacion    = ct.tpo_originacion
                     AND c.tpo_credito        = ct.tpo_credito
                     AND (s.marca IN(SELECT marca_inf
                                       FROM cat_tipo_credito)
                         OR s.marca IN(SELECT marca_prc
                                         FROM cat_tipo_credito)) "


   PREPARE prp_extrae_marcas FROM v_s_qry

   DECLARE cur_mar CURSOR FOR prp_extrae_marcas

   LET v_cnt = 1

   FOREACH cur_mar INTO v_arr_marca[v_cnt].num_credito,
                        v_arr_marca[v_cnt].tpo_credito,
                        v_arr_marca[v_cnt].estado,
                        v_arr_marca[v_cnt].edo_procesar,
                        v_arr_marca[v_cnt].marca,
                        v_arr_marca[v_cnt].marca_prc

      LET v_arr_marca[v_cnt].checkbox = 0
      LET v_cnt = v_cnt + 1
   END FOREACH

   CALL v_arr_marca.deleteElement(v_arr_marca.getLength())

   OPEN WINDOW w_marcas WITH FORM "AGRL553"

      -- Se inicializan los combobox a cero
      FOR i = 1 TO v_arr_marca.getLength()
         LET v_arr_marca[i].checkbox    = 0
         LET v_arr_marca[i].ck_procesar = 0
         LET v_arr_marca[i].ck_liq      = 0
         LET v_arr_marca[i].ck_int_pr   = 0
      END FOR

      DISPLAY p_nss TO v_nss

      INPUT ARRAY v_arr_marca FROM r_marca.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                        APPEND ROW = FALSE,
                                                        DELETE ROW = FALSE,
                                                        INSERT ROW = FALSE,
                                                        UNBUFFERED )

         BEFORE ROW
            LET v_pos = arr_curr()

         ON CHANGE checkbox
            -- Se valida que solo puedan ingresar una casilla
            IF v_arr_marca[v_pos].checkbox = 1 THEN
               FOR j = 1 TO v_arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET v_arr_marca[j].checkbox = 0
                  END IF
                  -- Si se elige marca interna, bloquea las marcas procesar
                  LET v_arr_marca[j].ck_procesar = 0
               END FOR
            END IF

            -- Se revisan los checkbox de liquidación de deudor
            FOR i = 1 TO v_arr_marca.getLength()
               IF v_arr_marca[v_pos].ck_liq = 1 THEN
                  CONTINUE FOR
               ELSE
                  LET v_arr_marca[i].ck_liq = 0 
               END IF
            END FOR

         ON CHANGE ck_procesar
{            -- Se valida que no sean marcas internas
            IF v_arr_marca[v_pos].marca != 231 OR
               v_arr_marca[v_pos].marca != 232 OR
               v_arr_marca[v_pos].marca != 234 THEN

               LET v_arr_marca[v_pos].marca = v_arr_marca[v_pos].marca_prc

            END IF
}
            -- Valida que solo puedan seleccionar una casilla
            IF v_arr_marca[v_pos].ck_procesar = 1 THEN
               FOR j = 1 TO v_arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET v_arr_marca[j].ck_procesar = 0
                  END IF
               END FOR
            END IF

            FOR j = 1 TO v_arr_marca.getLength()
               -- Si se elige marca Procesar, bloquea las marcas internas
               LET v_arr_marca[j].checkbox = 0
               LET v_arr_marca[j].ck_liq   = 0
            END FOR

         ON CHANGE ck_liq
            -- Se valida que solo puedan ingresar una casilla
            IF v_arr_marca[v_pos].ck_liq = 1 THEN
               FOR j = 1 TO v_arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET v_arr_marca[j].ck_liq = 0
                  END IF
               END FOR
            END IF

            -- Se valida que sea la misma que el checkbox de las marcas
            FOR i = 1 TO v_arr_marca.getLength()
               IF v_arr_marca[i].checkbox = 1 THEN
                  LET v_arr_marca[i].ck_liq = 1
               END IF
               IF v_arr_marca[i].ck_procesar = 1 THEN
                  LET v_arr_marca[v_pos].ck_liq = 0 
               END IF
            END FOR

         ON ACTION ACCEPT
            CALL fn_ventana_confirma("Alterta","¿Esta seguro que desea continuar?","stop")
            RETURNING v_bandera

            IF v_bandera = 1 THEN
               -- Se pasan los datos a un arreglo auxiliar
               FOR i = 1 TO v_arr_marca.getLength()
                  IF v_arr_marca[i].checkbox = 1 THEN
                     FOR j = 1 TO v_arr_marca.getLength()
                        LET v_ax_arr_marca[j].* = v_arr_marca[j].*
                     END FOR
                     -- bandera de marca interna
                     LET v_bandera_tpo = 0
                  END IF

                  IF v_arr_marca[i].ck_int_pr = 1 THEN
                     FOR j = 1 TO v_arr_marca.getLength()
                        LET v_ax_arr_marca[j].* = v_arr_marca[j].*
                     END FOR
                     -- bandera de marca interna
                     LET v_bandera_tpo = 0
                  END IF

                  IF v_arr_marca[i].ck_procesar = 1 THEN
                     FOR j = 1 TO v_arr_marca.getLength()
                        LET v_ax_arr_marca[j].* = v_arr_marca[j].*
                        LET v_ax_arr_marca[j].marca = v_arr_marca[j].marca_prc
                     END FOR
                     -- bandera de marca Procesar
                     LET v_bandera_tpo = 1
                  END IF
               END FOR

               -- Se pasa a un record para la invocación  la función
               FOR i = 1 TO v_ax_arr_marca.getLength()
                  -- si el check box de marca interna esta prendito
                  IF v_ax_arr_marca[i].checkbox = 1 THEN    -- Si el checkbox esta prendido
                     LET v_rec_marca.* = v_ax_arr_marca[i].*
                  END IF

                  IF v_ax_arr_marca[i].ck_procesar = 1 THEN
                     LET v_rec_marca.* = v_ax_arr_marca[i].*
                  END IF

                  IF v_ax_arr_marca[i].ck_int_pr = 1 THEN
                     LET v_rec_marca.* = v_ax_arr_marca[i].*
                  END IF
               END FOR

               CALL v_ax_arr_marca.clear()   -- Se limpia el arreglo
               CALL fn_ins_mov_tabla_marca(p_nss,v_rec_marca.*,p_situacion,v_bandera_tpo)
               EXIT INPUT 
            END IF

      END INPUT

   CLOSE WINDOW w_marcas

END FUNCTION



FUNCTION fn_act_desmarca(p_nss,p_situacion)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE p_situacion               SMALLINT
   DEFINE v_ax_arr_desmarca DYNAMIC ARRAY OF RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      checkbox                      SMALLINT,
      ck_procesar                   SMALLINT
   END RECORD
   DEFINE v_rec_desmarca RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      checkbox                      SMALLINT,
      ck_procesar                   SMALLINT 
   END RECORD
   DEFINE v_bandera_tpo             SMALLINT

   OPEN WINDOW w_d_arreglo WITH FORM "AGRL552"
      CALL fn_llena_arreglo(p_nss)

      -- Se inicializan los combobox a cero
      FOR i = 1 TO v_arr_desmarca.getLength()
         LET v_arr_desmarca[i].checkbox    = 0
         LET v_arr_desmarca[i].ck_procesar = 0
      END FOR

      DISPLAY p_nss TO v_nss

      DIALOG ATTRIBUTES(UNBUFFERED)

         INPUT ARRAY v_arr_desmarca FROM r_desmarca.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                                 APPEND ROW = FALSE,
                                                                 DELETE ROW = FALSE,
                                                                 INSERT ROW = FALSE)

            BEFORE ROW
               LET v_pos = arr_curr()

            ON CHANGE checkbox
               -- Se valida que solo puedan ingresar una casilla
               IF v_arr_desmarca[v_pos].checkbox = 1 THEN
                  FOR j = 1 TO v_arr_desmarca.getLength()
                     IF j <> v_pos THEN
                        LET v_arr_desmarca[j].checkbox = 0
                     END IF
                  END FOR
               END IF

               -- Si se elige marca interna, bloquea las marcas procesar
               FOR j = 1 TO v_arr_desmarca.getLength()
                  LET v_arr_desmarca[j].ck_procesar = 0
               END FOR

            ON CHANGE ck_procesar
               -- Se valida que no sean marcas internas
               IF v_arr_desmarca[v_pos].marca = 231 OR
                  v_arr_desmarca[v_pos].marca = 232 OR
                  v_arr_desmarca[v_pos].marca = 234 THEN

                  LET v_arr_desmarca[v_pos].ck_procesar = 1
               ELSE
                  LET v_arr_desmarca[v_pos].ck_procesar = 0
               END IF

               -- Valida que solo puedan seleccionar una casilla
               IF v_arr_desmarca[v_pos].ck_procesar = 1 THEN
                  FOR j = 1 TO v_arr_desmarca.getLength()
                     IF j <> v_pos THEN
                        LET v_arr_desmarca[j].ck_procesar = 0
                     END IF
                  END FOR
               END IF

               -- Si se elige marca Procesar, bloquea las marcas internas
               FOR j = 1 TO v_arr_desmarca.getLength()
                  LET v_arr_desmarca[j].checkbox = 0
               END FOR

            ON ACTION ACCEPT
               CALL fn_ventana_confirma("Alterta","¿Está seguro que desea continuar?","stop")
               RETURNING v_bandera

               IF v_bandera = 1 THEN
                  FOR i = 1 TO v_arr_desmarca.getLength()
                     IF v_arr_desmarca[i].checkbox = 1 THEN
                        FOR j = 1 TO v_arr_desmarca.getLength()
                           LET v_ax_arr_desmarca[j].* = v_arr_desmarca[j].*
                        END FOR
                        --LET i = i + 1
                        LET v_bandera_tpo = 0
                     END IF

                     IF v_arr_desmarca[i].ck_procesar = 1 THEN
                        FOR j = 1 TO v_arr_desmarca.getLength()
                           LET v_ax_arr_desmarca[j].* = v_arr_desmarca[j].*
                        END FOR
                        LET v_bandera_tpo = 1
                     END IF
                  END FOR

                  FOR i = 1 TO v_ax_arr_desmarca.getLength()
                     IF v_ax_arr_desmarca[i].checkbox = 1 THEN    -- Si el checkbox esta prendido
                        --DISPLAY "Se procesa: ",v_ax_arr_desmarca[i].*
                        --DiSPLAY "En la posición: ",i
                        LET v_rec_desmarca.* = v_ax_arr_desmarca[i].*
                     END IF

                     IF v_ax_arr_desmarca[i].ck_procesar = 1 THEN
                        LET v_rec_desmarca.* = v_ax_arr_desmarca[i].*
                     END IF
                  END FOR

                  CALL v_ax_arr_desmarca.clear()   -- Se limpia el arreglo

                  CALL fn_ins_mov_tabla(p_nss,v_rec_desmarca.*,p_situacion,v_bandera_tpo)

                  EXIT DIALOG
               END IF

         END INPUT

         ON ACTION CANCEL
            EXIT DIALOG

      END DIALOG
   CLOSE WINDOW w_d_arreglo
END FUNCTION


FUNCTION fn_ins_mov_tabla_marca(p_nss,p_rec_marca,p_situacion,p_bandera)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE p_rec_marca RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      marca_prc                     LIKE cat_tipo_credito.marca_prc,
      ck_liq                        SMALLINT,
      checkbox                      SMALLINT,
      ck_int_pr                     SMALLINT,
      ck_procesar                   SMALLINT
   END RECORD
   DEFINE p_situacion               SMALLINT
   DEFINE p_bandera                 SMALLINT

   LET v_s_qry = "INSERT INTO act_marca_desm_ag
                       VALUES(?,?,?,?,?,?,?,?,?,?)"

   PREPARE prp_ins_tb_marca FROM v_s_qry
   EXECUTE prp_ins_tb_marca USING p_nss,
                                  p_rec_marca.num_credito,
                                  p_rec_marca.tpo_credito,
                                  p_rec_marca.estado,
                                  p_rec_marca.edo_procesar,
                                  p_rec_marca.marca,
                                  p_situacion,
                                  p_bandera,
                                  p_rec_marca.ck_liq,
                                  p_rec_marca.ck_int_pr

   IF sqlca.sqlcode < 0 THEN
      CALL fn_mensaje("Alterta","Error al insertar registro","stop")
   ELSE
      CALL fn_mensaje("Alterta","Registro procesado correctamente","")
   END IF
END FUNCTION



FUNCTION fn_ins_mov_tabla(p_nss,p_rec_marca,p_situacion,p_bandera_tpo)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE p_rec_marca RECORD
      num_credito                   LIKE cre_acreditado.num_credito,
      tpo_credito                   LIKE cre_acreditado.tpo_credito,
      estado                        LIKE cre_acreditado.estado,
      edo_procesar                  LIKE cre_acreditado.edo_procesar,
      marca                         LIKE sfr_marca_activa.marca,
      checkbox                      SMALLINT,
      ck_procesar                   SMALLINT 
   END RECORD
   DEFINE p_situacion               SMALLINT
   DEFINE p_bandera_tpo             SMALLINT

   LET v_s_qry = "INSERT INTO act_marca_desm_ag
                       VALUES(?,?,?,?,?,?,?,?,0)"

   PREPARE prp_ins_tb FROM v_s_qry
   EXECUTE prp_ins_tb USING p_nss,
                            p_rec_marca.num_credito,
                            p_rec_marca.tpo_credito,
                            p_rec_marca.estado,
                            p_rec_marca.edo_procesar,
                            p_rec_marca.marca,
                            p_situacion,
                            p_bandera_tpo

   IF sqlca.sqlcode < 0 THEN
      CALL fn_mensaje("Alterta","Error al insertar registro","stop")
   ELSE
      CALL fn_mensaje("Alterta","Registro insertado correctamente","")
   END IF

END FUNCTION



FUNCTION fn_llena_arreglo(p_nss)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss

   CALL v_arr_desmarca.clear()

   LET v_s_qry = "SELECT c.num_credito,
                         c.tpo_credito,
                         c.estado,
                         c.edo_procesar,
                         s.marca
                    FROM cre_acreditado c,
                         sfr_marca_activa s,
                         afi_Derechohabiente a
                   WHERE a.nss = " || p_nss ||
                    "AND a.id_derechohabiente = c.id_derechohabiente
                     AND a.id_derechohabiente = s.id_derechohabiente
                     AND c.id_Cre_acreditado = s.n_referencia
                     AND (s.marca IN(SELECT marca_inf
                                       FROM cat_tipo_credito)
                         OR s.marca IN(SELECT marca_prc
                                         FROM cat_tipo_credito)) "

   PREPARE prp_extrae_desmarcas FROM v_s_qry
   DECLARE cur_desmar CURSOR FOR prp_extrae_desmarcas
   LET v_cnt = 1
   FOREACH cur_desmar INTO v_arr_desmarca[v_cnt].*
      LET v_arr_desmarca[v_cnt].checkbox = 0
      LET v_cnt = v_cnt + 1
   END FOREACH
   CALL v_arr_desmarca.deleteElement(v_arr_desmarca.getLength())
END FUNCTION



FUNCTION fn_existe_nss(p_nss)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_ax_nss                  LIKE afi_derechohabiente.nss

   LET v_s_qry = " SELECT nss
                     FROM afi_derechohabiente
                    WHERE nss = ? "

   PREPARE prp_exi_nss FROM v_s_qry
   EXECUTE prp_exi_nss USING p_nss INTO v_ax_nss

   -- no existe el nss
   IF v_ax_nss IS NULL THEN
      LET v_bandera = 0
   ELSE
   --si existe
      LET v_bandera = 1
   END IF

   RETURN v_bandera
END FUNCTION