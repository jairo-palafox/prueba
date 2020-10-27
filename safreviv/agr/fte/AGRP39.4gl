#########################################################################
#Modulo            => AGR                                               #
#Programa          => AGRP39                                            #
#Objetivo          => actualización de marcas uso anualidad y garantia  #
#Autor             => Jose Edaurdo Ventura                              #
#Fecha inicio      => 27 JUNIO 2016                                     #
#Autor modifica    => Emilio Abarca,EFP.                                #
#Fecha modofica    => 21 JUNIO 2018.                                    #
#Modificación      => Se incluye marca 221 para la actualización.       #
#########################################################################

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
   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_tpo_proceso             SMALLINT

END GLOBALS

MAIN
   DEFINE v_nss                     CHAR(11)
   DEFINE v_situacion               SMALLINT

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP39.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW AGRP391 WITH FORM "AGRP391"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE, CANCEL=FALSE)
         ON ACTION Marcar
            LET v_tpo_proceso = 1
            -- Se valida el nss
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca  la función que actualiza la marca
               CALL fn_act_marca(v_nss,v_tpo_proceso)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF

         ON ACTION Desmarcar
            let v_tpo_proceso = 2
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca a la función que actualiza la desmarca
               CALL fn_act_marca(v_nss,v_tpo_proceso)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF

         ON ACTION Cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW AGRP391

END MAIN

FUNCTION fn_act_marca(p_nss,v_tpo_proceso)

   DEFINE p_nss                     LIKE afi_derechohabiente.nss

   DEFINE v_tpo_proceso             SMALLINT

   DEFINE arr_credito DYNAMIC ARRAY OF RECORD
      id_derechohabiente            DECIMAL(9,0),
      id_cre_uso_garantia           DECIMAL(9,0),
      num_credito                   LIKE cre_uso_garantia.num_credito,
      tpo_uso                       LIKE cre_uso_garantia.tpo_uso,
      estado                        LIKE cre_uso_garantia.estado,
      edo_procesar                  LIKE cre_uso_garantia.edo_procesar,
      monto                         LIKE cre_uso_garantia.importe_v97,
      fecha                         DATE,
      marca                         LIKE sfr_marca_activa.marca,
      v_box1                        SMALLINT,
      v_box2                        SMALLINT,
      v_box3                        SMALLINT 
   END RECORD

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD
      nss          CHAR(11),
      num_credito  LIKE cre_uso_garantia.num_credito,
      tpo_uso      LIKE cre_uso_garantia.tpo_uso,
      estado       LIKE cre_uso_garantia.estado,
      edo_procesar LIKE cre_uso_garantia.edo_procesar,
      monto        LIKE cre_uso_garantia.importe_v97,
      fecha        DATE,
      marca        LIKE sfr_marca_activa.marca,
      situacion    SMALLINT,
      v_box1       SMALLINT,
      v_box2       SMALLINT,
      v_box3       SMALLINT 
   END RECORD

   DEFINE v_folio_liquida  DECIMAL(9,0)
   DEFINE v_qry_mov        STRING 
   DEFINE v_tabla          CHAR(20)
   DEFINE v_id_dh_autoriza DECIMAL(9,0)

   CALL arr_marca.clear()
   CALL arr_credito.clear()

   LET v_folio_liquida = 0

   #Tipo de originación 1 es para Transferencia de acreditados
   LET v_s_qry = " SELECT cre.id_derechohabiente,
                           cre.id_cre_acreditado,
                           cre.folio_liquida,
                           cre.num_credito,
                           cre.tpo_originacion,
                           cre.estado,
                           cre.edo_procesar
                     FROM cre_acreditado cre,
                          cat_tipo_credito cat
                    WHERE id_derechohabiente  = ?
                      AND cre.tpo_originacion = 1
                      AND cre.tpo_originacion = cat.tpo_originacion
                      AND cre.tpo_credito     = cat.tpo_credito;"

   PREPARE prp_extrae_ta FROM v_s_qry
   DECLARE crs_ta CURSOR FOR prp_extrae_ta

   LET v_cnt = 1

   FOREACH crs_ta USING v_id_derechohabiente 
                   INTO arr_credito[v_cnt].id_derechohabiente,
                        arr_credito[v_cnt].id_cre_uso_garantia,
                        v_folio_liquida,
                        arr_credito[v_cnt].num_credito,
                        arr_credito[v_cnt].tpo_uso,
                        arr_credito[v_cnt].estado,
                        arr_credito[v_cnt].edo_procesar

      IF(v_folio_liquida <> 0) THEN
         #Búsca la tabla de movimientos para el saldo
         LET v_qry_mov = "EXECUTE procedure fn_tab_movimiento(0,",v_folio_liquida,",NULL) "

         PREPARE tab_mov FROM v_qry_mov
         EXECUTE tab_mov INTO v_tabla

         LET v_qry_mov = " SELECT SUM(monto_pesos) ",
                               " FROM ",v_tabla,
                              " WHERE folio_liquida = ",v_folio_liquida,
                                " AND id_derechohabiente = ",arr_credito[v_cnt].id_derechohabiente,
                                " AND subcuenta IN (4,8)",
                                " AND fondo_inversion = 11;"

         PREPARE monto_liq FROM v_qry_mov
         EXECUTE monto_liq INTO arr_credito[v_cnt].monto
         
      ELSE 
         --El monto a solicitar se recupera todo lo que tiene en la subcuenta 92 y 97
         SELECT SUM (monto_pesos)
           INTO arr_credito[v_cnt].monto
           FROM cta_movimiento
          WHERE id_derechohabiente = arr_credito[v_cnt].id_derechohabiente
            AND subcuenta IN (4,8)
            AND fondo_inversion = 11;
      END IF

      IF(arr_credito[v_cnt].monto IS NULL) THEN
         LET arr_credito[v_cnt].monto = 0
      END IF

      --Busca marca
      SELECT marca,
             f_inicio
        INTO arr_credito[v_cnt].marca,
             arr_credito[v_cnt].fecha
        FROM sfr_marca_activa
       WHERE id_derechohabiente = arr_credito[v_cnt].id_derechohabiente
         AND n_referencia       = arr_credito[v_cnt].id_cre_uso_garantia
         AND marca = 221;

      LET arr_credito[v_cnt].v_box1 = 0
      LET arr_credito[v_cnt].v_box2 = 0
      LET arr_credito[v_cnt].v_box3 = 0 

      --1 ->MARCAR
      IF v_tpo_proceso = 1 THEN
         --Si encuentra una marca activa la elimina del arreglo para no volverla a Marcar
         IF arr_credito[v_cnt].marca IS NOT NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      --2 -->DESMARCAR
      IF v_tpo_proceso = 2 THEN
         --Si la marca es nula no se contempla para el arreglo, ya que se necesita que exista para Desmarcarla
         IF arr_credito[v_cnt].marca IS NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      LET v_cnt = v_cnt + 1

   END FOREACH 

 --("43" es para uso de anualidad de ag , "18" y "48" para uso de garantía 43 bis)
   LET v_s_qry ="SELECT id_derechohabiente,
                        id_cre_uso_garantia,
                        num_credito,
                        tpo_transferencia,
                        estado,
                        edo_procesar,
                        importe_v97
                   FROM cre_uso_garantia
                  WHERE id_cre_uso_garantia > 0
                    AND id_cre_ctr_archivo IN (
                 SELECT id_cre_ctr_archivo
                   FROM cre_ctr_archivo
                  WHERE operacion NOT IN (1,6,9,14))
                    anD id_derechohabiente = ?
                    AND tpo_transferencia = '43'"
                    --and estado in(20,140)
                    --AND edo_procesar IN (5, 60, 70, 75, 80, 85)

   PREPARE prp_extrae_anualidad FROM v_s_qry

   DECLARE cur_anualidad CURSOR FOR prp_extrae_anualidad

   --DISPLAY v_s_qry

   --LET v_cnt = 1

   FOREACH cur_anualidad USING v_id_derechohabiente 
      INTO arr_credito[v_cnt].id_derechohabiente,
                        arr_credito[v_cnt].id_cre_uso_garantia,
                        arr_credito[v_cnt].num_credito,
                        arr_credito[v_cnt].tpo_uso,
                        arr_credito[v_cnt].estado,
                        arr_credito[v_cnt].edo_procesar,
                        arr_credito[v_cnt].monto

      SELECT marca,
             f_inicio
        INTO arr_credito[v_cnt].marca,
             arr_credito[v_cnt].fecha
        FROM sfr_marca_activa
       WHERE id_derechohabiente = arr_credito[v_cnt].id_derechohabiente
         AND n_referencia = arr_credito[v_cnt].id_cre_uso_garantia
         AND marca = 225;

      LET arr_credito[v_cnt].v_box1 = 0
      LET arr_credito[v_cnt].v_box2 = 0
      LET arr_credito[v_cnt].v_box3 = 0 

      IF v_tpo_proceso = 1 THEN
         IF arr_credito[v_cnt].marca IS NOT NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      IF v_tpo_proceso = 2 THEN
         IF arr_credito[v_cnt].marca IS NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      LET v_cnt = v_cnt + 1
   END FOREACH

--   CALL arr_credito.deleteElement(arr_marca.getLength())

   -- Generan deudor AG
   -- Modifica EAS 21/06/2019
   LET v_s_qry = " SELECT cre.id_derechohabiente,
                          cre.id_cre_acreditado,
                          cre.num_credito,
                          cre.tpo_originacion,
                          cre.estado,
                          cre.edo_procesar,
                          cre.sdo_deudor,
                          sfr.marca,
                          sfr.f_inicio
                     FROM cre_acreditado cre,
                    OUTER sfr_marca_activa sfr,
                          cat_tipo_credito cat
                    WHERE cre.id_derechohabiente  = ?
                      AND cre.tpo_originacion = 4
                      AND cre.id_derechohabiente = sfr.id_derechohabiente
                      AND sfr.marca = 225
                      AND cre.id_cre_acreditado = sfr.n_referencia
                      AND cre.tpo_originacion   = cat.tpo_originacion
                      AND cre.tpo_credito       = cat.tpo_credito;"

   PREPARE prp_extrae_sdo_ag FROM v_s_qry
   DECLARE cur_sdo_ag CURSOR FOR prp_extrae_sdo_ag

   FOREACH cur_sdo_ag USING v_id_derechohabiente 
                       INTO arr_credito[v_cnt].id_derechohabiente,
                            arr_credito[v_cnt].id_cre_uso_garantia,
                            arr_credito[v_cnt].num_credito,
                            arr_credito[v_cnt].tpo_uso,
                            arr_credito[v_cnt].estado,
                            arr_credito[v_cnt].edo_procesar,
                            arr_credito[v_cnt].monto,
                            arr_credito[v_cnt].marca,
                            arr_credito[v_cnt].fecha

      LET arr_credito[v_cnt].v_box1 = 0
      LET arr_credito[v_cnt].v_box2 = 0
      LET arr_credito[v_cnt].v_box3 = 0 

      IF v_tpo_proceso = 1 THEN
         IF arr_credito[v_cnt].marca IS NOT NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      IF v_tpo_proceso = 2 THEN
         IF arr_credito[v_cnt].marca IS NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      LET v_cnt = v_cnt + 1

   END FOREACH

--   CALL arr_credito.deleteElement(arr_marca.getLength())

   -- Uso de garantía 43BIS   
   LET v_s_qry ="SELECT id_derechohabiente,
                        id_cre_uso_garantia,
                        num_credito,
                        tpo_transferencia,
                        estado,
                        edo_procesar,
                        importe_v97
                   FROM cre_uso_garantia
                  WHERE id_cre_uso_garantia > 0
                    AND id_cre_ctr_archivo IN (
                 SELECT id_cre_ctr_archivo
                   FROM cre_ctr_archivo
                  WHERE operacion NOT IN (1,6,9,14))
                    anD id_derechohabiente = ?
                    AND tpo_transferencia in ('18','48')" 
                    --and estado in(20,140)
                    --AND edo_procesar IN (5, 60, 70, 75, 80, 85)

   PREPARE prp_extrae_garantia FROM v_s_qry

   DECLARE cur_garantia CURSOR FOR prp_extrae_garantia

 --  LET v_cnt = 1

   FOREACH cur_garantia USING v_id_derechohabiente 
      INTO arr_credito[v_cnt].id_derechohabiente,
                        arr_credito[v_cnt].id_cre_uso_garantia,
                        arr_credito[v_cnt].num_credito,
                        arr_credito[v_cnt].tpo_uso,
                        arr_credito[v_cnt].estado,
                        arr_credito[v_cnt].edo_procesar,
                        arr_credito[v_cnt].monto

      SELECT marca,
             f_inicio
        INTO arr_credito[v_cnt].marca,
             arr_credito[v_cnt].fecha
        FROM sfr_marca_activa
       WHERE id_derechohabiente = arr_credito[v_cnt].id_derechohabiente
         AND n_referencia = arr_credito[v_cnt].id_cre_uso_garantia
         AND   marca = 223;

      LET arr_credito[v_cnt].v_box1 = 0
      LET arr_credito[v_cnt].v_box2 = 0
      LET arr_credito[v_cnt].v_box3 = 0 

            IF v_tpo_proceso = 1 THEN
         IF arr_credito[v_cnt].marca IS NOT NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF

      IF v_tpo_proceso = 2 THEN
         IF arr_credito[v_cnt].marca IS NULL THEN
            CALL arr_credito.deleteElement(v_cnt)
            LET v_cnt = v_cnt -1
         END IF
      END IF
--DISPLAY v_cnt
      LET v_cnt = v_cnt + 1
   END FOREACH

   CALL arr_credito.deleteElement(arr_credito.getLength())

   OPEN WINDOW AGRP392 WITH FORM "AGRP392"

   IF arr_credito.getLength() >= 1 THEN

      -- Se inicializan los checkbox a cero
      FOR i = 1 TO arr_credito.getLength()
         LET arr_credito[i].v_box1 = 0
         LET arr_credito[i].v_box2 = 0
      END FOR

      --DISPLAY p_nss TO v_nss

      FOR v_cnt = 1 TO arr_credito.getLength()
         LET arr_marca[v_cnt].nss         = p_nss
         LET arr_marca[v_cnt].num_credito = arr_credito[v_cnt].num_credito
         LET arr_marca[v_cnt].tpo_uso     = arr_credito[v_cnt].tpo_uso
         LET arr_marca[v_cnt].estado      = arr_credito[v_cnt].estado
         LET arr_marca[v_cnt].edo_procesar= arr_credito[v_cnt].edo_procesar
         LET arr_marca[v_cnt].monto       = arr_credito[v_cnt].monto
         LET arr_marca[v_cnt].fecha       = arr_credito[v_cnt].fecha
         LET arr_marca[v_cnt].marca       = arr_credito[v_cnt].marca
         LET arr_marca[v_cnt].situacion   = ""
         LET arr_marca[v_cnt].v_box1      = arr_credito[v_cnt].v_box1
         LET arr_marca[v_cnt].v_box2      = arr_credito[v_cnt].v_box2
         LET arr_marca[v_cnt].v_box3      = arr_credito[v_cnt].v_box3
      END FOR
      END IF

      IF arr_marca.getLength() >= 1 THEN

      INPUT ARRAY arr_marca FROM record1.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                        APPEND ROW = FALSE,
                                                        DELETE ROW = FALSE,
                                                        INSERT ROW = FALSE,
                                                         UNBUFFERED)

         BEFORE ROW
            LET v_pos = arr_curr()

         ON CHANGE v_box1
               --Marcar
               IF v_tpo_proceso = 1 THEN
                  IF (arr_marca[v_pos].tpo_uso = 43) OR 
                     (arr_marca[v_pos].tpo_uso = 1) THEN
                     IF arr_marca[v_pos].v_box1 = 1 THEN
                         CALL fn_mensaje("Alerta","No es posible procesar esta marca para tipo de transferencia 43","stop")
                        LET arr_marca[v_pos].v_box1 = 0
                     END IF
                  END IF

                  IF arr_marca[v_pos].marca = 223 THEN
                     IF arr_marca[v_pos].v_box1 = 1 THEN
                        CALL fn_mensaje("Alerta","El registro seleccionado ya se encuentra marcado","stop")
                        LET arr_marca[v_pos].v_box1 = 0
                     END IF
                  END IF
               END IF

               --Desmarcar
               IF v_tpo_proceso = 2 THEN
                  IF arr_marca[v_pos].marca = 225 OR 
                     arr_marca[v_pos].marca = 221 THEN 
                     IF arr_marca[v_pos].v_box1 = 1 THEN
                        CALL fn_mensaje("Alerta","No es posible solicitar esta desmarca para el registro seleccionado ","stop")
                        LET arr_marca[v_pos].v_box1 = 0
                     END IF
                  END IF
               END IF
            
            -- Se valida que solo puedan ingresar una casilla
            IF arr_marca[v_pos].v_box1 = 1 THEN
               FOR j = 1 TO arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET arr_marca[j].v_box1 = 0
                  END IF
                  -- Si se elige marca interna, bloquea las marcas procesar
                  LET arr_marca[v_pos].v_box2 = 0
               END FOR
            END IF

         ON CHANGE v_box2

            IF v_tpo_proceso = 1 THEN
               IF (arr_marca[v_pos].tpo_uso = 18) OR
                  (arr_marca[v_pos].tpo_uso = 48) OR 
                   (arr_marca[v_pos].tpo_uso = 1) THEN
                  IF arr_marca[v_pos].v_box2 = 1 THEN
                     CALL fn_mensaje("Alerta","No es posible procesar esta marca para tipo de transferencia 1,18,48","stop")
                     LET arr_marca[v_pos].v_box2 = 0
                  END IF
               END IF

               IF arr_marca[v_pos].marca = 225 THEN
                  IF arr_marca[v_pos].v_box2 = 1 THEN
                     CALL fn_mensaje("Alerta","El registro seleccionado ya se encuentra marcado","stop")
                     LET arr_marca[v_pos].v_box2 = 0
                  END IF
               END IF
            END IF

            IF v_tpo_proceso = 2 THEN
                  IF arr_marca[v_pos].marca = 223 OR 
                     arr_marca[v_pos].marca = 221 THEN
                     IF arr_marca[v_pos].v_box2 = 1 THEN
                        CALL fn_mensaje("Alerta","No es posible solicitar esta desmarca para el registro seleccionado ","stop")
                        LET arr_marca[v_pos].v_box2 = 0
                     END IF
                  END IF
               END IF

            -- Valida que solo puedan seleccionar una casilla
            IF arr_marca[v_pos].v_box2 = 1 THEN
               FOR j = 1 TO arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET arr_marca[j].v_box2 = 0
                  END IF
                  -- Si se elige marca interna, bloquea las marcas procesar
                  LET arr_marca[v_pos].v_box1 = 0
               END FOR
            END IF

         ON CHANGE v_box3

            IF v_tpo_proceso = 1 THEN
               IF (arr_marca[v_pos].tpo_uso = 18) OR
                  (arr_marca[v_pos].tpo_uso = 48) OR 
                  (arr_marca[v_pos].tpo_uso = 43) THEN
                  IF arr_marca[v_pos].v_box3 = 1 THEN
                     CALL fn_mensaje("Alerta","No es posible procesar esta marca para tipo de originacion 1","stop")
                     LET arr_marca[v_pos].v_box3 = 0
                  END IF
               END IF

               IF arr_marca[v_pos].marca = 221 THEN
                  IF arr_marca[v_pos].v_box3 = 1 THEN
                     CALL fn_mensaje("Alerta","El registro seleccionado ya se encuentra marcado","stop")
                     LET arr_marca[v_pos].v_box3 = 0
                  END IF
               END IF
            END IF

            IF v_tpo_proceso = 2 THEN
               IF arr_marca[v_pos].marca = 223 OR 
                  arr_marca[v_pos].marca = 225 THEN
                     IF arr_marca[v_pos].v_box3 = 1 THEN
                        CALL fn_mensaje("Alerta","No es posible solicitar esta desmarca para el registro seleccionado ","stop")
                        LET arr_marca[v_pos].v_box3 = 0
                     END IF
                  END IF
             END IF

            -- Valida que solo puedan seleccionar una casilla
            IF arr_marca[v_pos].v_box3 = 1 THEN
               FOR j = 1 TO arr_marca.getLength()
                  IF j <> v_pos THEN
                     LET arr_marca[j].v_box3 = 0
                  END IF
                  -- Si se elige marca interna, bloquea las marcas procesar
                  LET arr_marca[v_pos].v_box1 = 0
                  LET arr_marca[v_pos].v_box2 = 0
               END FOR
            END IF

         ON ACTION ACCEPT
            {FOR j = 1 TO arr_marca.getLength()
               IF (arr_marca[j].v_box1 = 1) OR
                  (arr_marca[j].v_box2 = 1) THEN

               END IF
            END FOR}
            CALL fn_ventana_confirma("Alterta","¿Esta seguro que desea continuar?","stop")
            RETURNING v_bandera

            IF v_bandera = 1 THEN
               FOR j = 1 TO arr_marca.getLength()
                  IF (arr_marca[j].v_box1 = 1) OR
                     (arr_marca[j].v_box2 = 1) OR 
                     (arr_marca[j].v_box3 = 1) THEN

                     IF (arr_marca[j].marca IS NULL) THEN
                        IF arr_marca[j].v_box1 = 1 THEN
                           LET arr_marca[j].marca = 223
                        END IF

                        IF arr_marca[j].v_box2 = 1 THEN
                           LET arr_marca[j].marca = 225
                        END IF

                        IF arr_marca[j].v_box3 = 1 THEN
                           LET arr_marca[j].marca = 221
                        END IF
                     END IF

                     -- Verifica que no exista la solicitud en autorización
                     SELECT MAX(id_derechohabiente)
                        INTO v_id_dh_autoriza
                        FROM agr_act_marca_uso
                       WHERE id_derechohabiente  = arr_credito[j].id_derechohabiente
                         AND id_cre_uso_garantia = arr_credito[j].id_cre_uso_garantia
                         AND marca       = arr_marca[j].marca
                         AND f_ini_marca = arr_marca[j].fecha

                     IF v_id_dh_autoriza IS NULL THEN
                        INSERT INTO agr_act_marca_uso
                             VALUES (arr_credito[j].id_derechohabiente,
                                     arr_credito[j].id_cre_uso_garantia,
                                     arr_marca[j].nss,
                                     arr_marca[j].num_credito,
                                     arr_marca[j].marca,
                                     arr_marca[j].fecha,
                                     arr_marca[j].tpo_uso,
                                     arr_marca[j].estado,
                                     arr_marca[j].edo_procesar,
                                     arr_marca[j].v_box1,
                                     arr_marca[j].v_box2,
                                     arr_marca[j].v_box3,
                                     v_tpo_proceso,
                                     g_usuario,
                                     TODAY);
                     END IF
                  END IF
               END FOR

               IF v_tpo_proceso = 1 THEN
                  CALL fn_mensaje("Alerta","Se procesó solicitud de marca y está en espera de autorización","stop")
               END IF

               IF v_tpo_proceso = 2 THEN
                  CALL fn_mensaje("Alerta","Se procesó solicitud de desmarca y está en espera de autorización","stop")
               END IF
               
               EXIT INPUT
            END IF

         END INPUT
      ELSE
          CALL fn_mensaje("Error","No se encontraron registros para NSS ingresado","stop")
      END IF

      CLOSE WINDOW AGRP392

END FUNCTION

FUNCTION fn_existe_nss(p_nss)

   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_ax_nss                  LIKE afi_derechohabiente.nss

   LET v_s_qry = " SELECT nss,id_derechohabiente
                     FROM afi_derechohabiente
                    WHERE nss = ? "

   PREPARE prp_exi_nss FROM v_s_qry
   EXECUTE prp_exi_nss USING p_nss INTO v_ax_nss,v_id_derechohabiente

   -- no existe el nss
   IF v_ax_nss IS NULL THEN
      LET v_bandera = 0
   ELSE
   --si existe
      LET v_bandera = 1
   END IF

   RETURN v_bandera

END FUNCTION