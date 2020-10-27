
-------------------------------------------------------------------------------
-- Modulo        => AGR
-- Componente    => AGRP47
-- Descripción   => Reactivación de marcas 225 223 y 221
-- Autor         => Gerardo Alfonso Vega Paredes.
-- Fecha         => 21 de Nomvienbre de 2017.
-- Requerimiento => SACI2017-1-11
-------------------------------------------------------------------------------
-- Autor Mod.   => Emilio Abarca Sánchez
-- Fec Mod.     => 13 Junio del 2018
-- Modificación => Se agrega reactivación para registros con marca 221
-------------------------------------------------------------------------------

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

   DEFINE arr_reactiva DYNAMIC ARRAY OF RECORD
      nss                 CHAR(11),
      id_derechohabiente  DECIMAL(9,0),
      id_cre_acreditado   DECIMAL(9,0),
      folio_liquida       DECIMAL(9,0),
      num_credito         LIKE cre_uso_garantia.num_credito,
      tpo_uso             LIKE cre_uso_garantia.tpo_uso,
      periodo_pago        CHAR(6),
      aux_estado          SMALLINT,
      estado              CHAR(50),
      edo_procesar        CHAR(50),
      monto               LIKE cre_uso_garantia.importe_v97,
      f_inicio           DATE,
      fecha              DATE,
      marca              LIKE sfr_marca_activa.marca,
      folio              LIKE sfr_marca_activa.folio,      
      v_box1             SMALLINT
   END RECORD

END GLOBALS

MAIN
   DEFINE v_nss CHAR(11)

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP47.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   OPEN WINDOW AGRP471 WITH FORM "AGRP471"
      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE, CANCEL=FALSE)
         ON ACTION Reactivacion
            let v_tpo_proceso = 1
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
   CLOSE WINDOW AGRP471
END MAIN

FUNCTION fn_act_marca(p_nss,v_tpo_proceso)
   DEFINE p_nss           LIKE afi_derechohabiente.nss
   DEFINE v_tpo_proceso   SMALLINT
   DEFINE v_cnt_true      SMALLINT,
           v_cont2         SMALLINT
   DEFINE v_aux_estado    SMALLINT
   DEFINE v_aux_edo_prcr  SMALLINT
   DEFINE v_qry_mov       STRING 
   DEFINE v_tabla         CHAR(20)
   DEFINE v_importe_v97   DECIMAL(12,2)

   CALL arr_reactiva.clear()

   #Búsca inf. para marca 221 TA o Acreditados
   LET v_s_qry ="SELECT afi.nss,
                        sfr.id_derechohabiente,
                        sfr.n_referencia,
                        cre.folio_liquida,
                        cre.num_credito,
                        cre.tpo_originacion,
                        '',
                        cre.estado,
                        cre.edo_procesar,
                        0,
                        sfr.f_inicio,
                        sfr.f_fin,
                        sfr.marca,
                        sfr.folio
                 FROM sfr_marca_historica sfr,  
                      cre_acreditado cre,
                      afi_derechohabiente afi
                WHERE sfr.id_derechohabiente = ?
                  AND sfr.marca              = 221
                  AND sfr.id_derechohabiente = cre.id_derechohabiente
                  AND sfr.n_referencia       = cre.id_cre_acreditado
                  AND sfr.id_derechohabiente = afi.id_derechohabiente
                  AND sfr.f_fin IS NOT NULL
                  ORDER BY sfr.f_fin DESC;"
                  
   PREPARE prp_extrae_ta FROM v_s_qry
   DECLARE cur_ta CURSOR FOR prp_extrae_ta 

   LET v_cnt = 1 --Inicializa contador

   FOREACH cur_ta USING v_id_derechohabiente 
                   INTO arr_reactiva[v_cnt].nss,
                        arr_reactiva[v_cnt].id_derechohabiente,
                        arr_reactiva[v_cnt].id_cre_acreditado,
                        arr_reactiva[v_cnt].folio_liquida,
                        arr_reactiva[v_cnt].num_credito,
                        arr_reactiva[v_cnt].tpo_uso,
                        arr_reactiva[v_cnt].periodo_pago,
                        v_aux_estado,
                        v_aux_edo_prcr,
                        arr_reactiva[v_cnt].monto,
                        arr_reactiva[v_cnt].f_inicio,
                        arr_reactiva[v_cnt].fecha,
                        arr_reactiva[v_cnt].marca,
                        arr_reactiva[v_cnt].folio

      LET arr_reactiva[v_cnt].v_box1 = 0

      LET arr_reactiva[v_cnt].aux_estado = v_aux_estado

      --descripción estado
      SELECT estado ||"-"||estado_desc
        INTO arr_reactiva[v_cnt].estado
        FROM cat_maq_credito
       WHERE estado = v_aux_estado
       
      --descripción estado procesar
      SELECT estado ||"-"||estado_desc
        INTO  arr_reactiva[v_cnt].edo_procesar
        FROM cat_maq_credito
       WHERE estado = v_aux_edo_prcr

      IF(arr_reactiva[v_cnt].folio_liquida <> 0) THEN
         
         #Búsca la tabla de movimientos para el saldo
         LET v_qry_mov = "EXECUTE procedure fn_tab_movimiento(0,",arr_reactiva[v_cnt].folio_liquida,",NULL) "

         PREPARE tab_mov FROM v_qry_mov
         EXECUTE tab_mov INTO v_tabla

         LET v_qry_mov = " SELECT SUM(monto_pesos) ",
                               " FROM ",v_tabla,
                              " WHERE folio_liquida = ",arr_reactiva[v_cnt].folio_liquida,
                                " AND id_derechohabiente = ",arr_reactiva[v_cnt].id_derechohabiente,
                                " AND subcuenta IN (4,8)",
                                " AND fondo_inversion = 11;"

         PREPARE monto_liq FROM v_qry_mov
         EXECUTE monto_liq INTO arr_reactiva[v_cnt].monto
         
      ELSE 
          --El monto a solicitar se recupera todo lo que tiene en la subcuenta 92 y 97
         SELECT SUM (monto_pesos)
           INTO arr_reactiva[v_cnt].monto
           FROM cta_movimiento
          WHERE id_derechohabiente = arr_reactiva[v_cnt].id_derechohabiente
            AND subcuenta IN (4,8)
            AND fondo_inversion = 11;
      END IF 
      
      IF(arr_reactiva[v_cnt].monto IS NULL) THEN
         LET arr_reactiva[v_cnt].monto = 0
      END IF

      LET v_cnt = v_cnt + 1
      
   END FOREACH

   #Búsca inf. para marcas de AG 223 y 225
   --(43 es para uso de anualidad de ag y 18 y 48 para uso de garantía 43 bis)

   LET v_s_qry ="SELECT afi.nss,
                        sfr.id_derechohabiente,
                        sfr.n_referencia,
                        cre.folio_liquida,
                        cre.num_credito,
                        cre.tpo_transferencia,
                        cre.periodo_pago,
                        cre.estado,
                        cre.edo_procesar,
                        cre.importe_v97,
                        sfr.f_inicio,
                        sfr.f_fin,
                        sfr.marca,
                        sfr.folio
                 FROM sfr_marca_historica sfr,  
                      cre_uso_garantia cre,
                      afi_derechohabiente afi
                WHERE sfr.id_derechohabiente = ?
                  AND sfr.marca IN (223,225)
                  AND sfr.id_derechohabiente = cre.id_derechohabiente
                  AND sfr.n_referencia       = cre.id_cre_uso_garantia
                  AND sfr.id_derechohabiente = afi.id_derechohabiente
                  AND sfr.f_fin IS NOT NULL
                  ORDER BY sfr.f_fin DESC;"
   

   PREPARE prp_extrae_ag FROM v_s_qry
   DECLARE cur_anualidad CURSOR FOR prp_extrae_ag

   LET v_importe_v97   = 0

   FOREACH cur_anualidad USING v_id_derechohabiente 
                          INTO arr_reactiva[v_cnt].nss,
                               arr_reactiva[v_cnt].id_derechohabiente,
                               arr_reactiva[v_cnt].id_cre_acreditado,
                               arr_reactiva[v_cnt].folio_liquida,
                               arr_reactiva[v_cnt].num_credito,
                               arr_reactiva[v_cnt].tpo_uso,
                               arr_reactiva[v_cnt].periodo_pago,
                               v_aux_estado,
                               v_aux_edo_prcr,
                               v_importe_v97,
                               arr_reactiva[v_cnt].f_inicio,
                               arr_reactiva[v_cnt].fecha,
                               arr_reactiva[v_cnt].marca,
                               arr_reactiva[v_cnt].folio


      LET arr_reactiva[v_cnt].v_box1 = 0
      LET arr_reactiva[v_cnt].aux_estado = v_aux_estado

      --descripción estado
      SELECT estado ||"-"||estado_desc
        INTO arr_reactiva[v_cnt].estado
        FROM cat_maq_credito
       WHERE estado = v_aux_estado
       
      --descripción estado procesar
      SELECT estado ||"-"||estado_desc
        INTO arr_reactiva[v_cnt].edo_procesar
        FROM cat_maq_credito
       WHERE estado = v_aux_edo_prcr

      IF(arr_reactiva[v_cnt].folio_liquida <> 0) THEN
      
         #Búsca la tabla de movimientos para el saldo
         LET v_qry_mov = "EXECUTE procedure fn_tab_movimiento(0,",arr_reactiva[v_cnt].folio_liquida,",NULL) "

         PREPARE tab_mov2 FROM v_qry_mov
         EXECUTE tab_mov2 INTO v_tabla

         LET v_qry_mov = " SELECT SUM(monto_pesos) ",
                               " FROM ",v_tabla,
                              " WHERE folio_liquida = ",arr_reactiva[v_cnt].folio_liquida,
                                " AND id_derechohabiente = ",arr_reactiva[v_cnt].id_derechohabiente,
                                " AND subcuenta IN (4,8)",
                                " AND fondo_inversion = 11;"

         PREPARE monto_liq2 FROM v_qry_mov
         EXECUTE monto_liq2 INTO arr_reactiva[v_cnt].monto
         
      ELSE
         LET arr_reactiva[v_cnt].monto = v_importe_v97
      END IF 

      IF(arr_reactiva[v_cnt].monto IS NULL ) THEN 
         LET arr_reactiva[v_cnt].monto = 0
      END IF 
 
      LET v_cnt = v_cnt + 1
      
   END FOREACH

   CALL arr_reactiva.deleteElement(arr_reactiva.getLength())

   OPEN WINDOW AGRP472 WITH FORM "AGRP472"

   IF(v_cnt > 1) THEN -- Si hay registros en el arreglo

      INPUT ARRAY arr_reactiva FROM tab_credito.* ATTRIBUTE(WITHOUT DEFAULTS,APPEND ROW = FALSE,
                                                               DELETE ROW = FALSE,INSERT ROW = FALSE,UNBUFFERED)

         BEFORE ROW
            LET v_pos = arr_curr()

         ON ACTION ACCEPT
            --Verifica si hay más de un registro marcado
            LET v_cnt_true = 0 
            FOR j = 1 TO arr_reactiva.getLength()
               IF arr_reactiva[j].v_box1 <> 0 THEN
                  LET v_cnt_true = v_cnt_true + 1
               END IF
            END FOR
            
            IF v_cnt_true = 0 THEN --AND v_cont2 = 0 THEN
               CALL fn_mensaje("Alerta","Para continuar debe seleccionar un registro","stop")
               NEXT FIELD v_box1
            END IF

            #Selecciona la marca PROCESAR
            CALL confirma_marca_procesar()
            
      END INPUT
      ELSE
          CALL fn_mensaje(" ","No se encontraron registros para reactivar"," ")
      END IF

      CLOSE WINDOW AGRP472

END FUNCTION

FUNCTION confirma_marca_procesar()

   DEFINE check_prc01 SMALLINT
   DEFINE check_prc02 SMALLINT
   DEFINE check_prc04 SMALLINT
   DEFINE v_marca_prc SMALLINT 
   
   OPEN WINDOW vtn_confirma WITH FORM "AGRP473" ATTRIBUTE(STYLE="dialog")

      LET check_prc01 = 0
      LET check_prc02 = 0
      LET check_prc04 = 0
      
      INPUT BY NAME check_prc01,check_prc02,check_prc04 ATTRIBUTES(UNBUFFERED,WITHOUT DEFAULTS)
          ON ACTION ACCEPT 
            --Valida que sólo se seleccione una marca PROCESAR
            IF(check_prc01  = 0) AND 
               (check_prc02 = 0) AND 
               (check_prc04 = 0) THEN
               CALL fn_mensaje("","Selecciona la marca","")
               CONTINUE INPUT 
            END IF 
            IF(check_prc01  = 1) AND 
               (check_prc02 = 1) AND 
               (check_prc04 = 1) THEN
               CALL fn_mensaje("","Debe seleccionar sólo una marca","")
               CONTINUE INPUT 
            END IF 
            IF(check_prc01  = 0) AND 
               (check_prc02 = 1) AND 
               (check_prc04 = 1) THEN
               CALL fn_mensaje("","Debe seleccionar sólo una marca","")
               CONTINUE INPUT 
            END IF
            IF(check_prc01  = 1) AND 
               (check_prc02 = 0) AND 
               (check_prc04 = 1) THEN
               CALL fn_mensaje("","Debe seleccionar sólo una marca","")
               CONTINUE INPUT 
            END IF
            IF(check_prc01  = 1) AND 
               (check_prc02 = 1) AND 
               (check_prc04 = 0) THEN
               CALL fn_mensaje("","Debe seleccionar sólo una marca","")
               CONTINUE INPUT 
            END IF 

            #Al pasar la validación se obtiene la marca seleccionada
            CASE 
               WHEN check_prc01 = 1
                  LET v_marca_prc = 1
               WHEN check_prc02= 1
                  LET v_marca_prc = 2
               WHEN check_prc04 = 1
                  LET v_marca_prc = 4
            END CASE 

            CALL fn_ventana_confirma("Alterta","Esta seguro que desea continuar?","stop")
            RETURNING v_bandera

            IF(v_bandera = 1) THEN

               FOR j = 1 TO arr_reactiva.getLength()
                  IF (arr_reactiva[j].v_box1 = 1) THEN
                     #Reversa desmarca
                     LET v_s_qry = "EXECUTE PROCEDURE sp_reversa_desmarca(?,?,?,?)"
                     PREPARE prp_desmarca FROM v_s_qry
                     EXECUTE prp_desmarca USING arr_reactiva[j].id_derechohabiente,
                                                 arr_reactiva[j].marca,
                                                 arr_reactiva[j].id_cre_acreditado,
                                                 arr_reactiva[j].folio

                     CALL act_marca_corresponde(arr_reactiva[j].marca,
                                                arr_reactiva[j].f_inicio,
                                                v_marca_prc,
                                                arr_reactiva[j].id_derechohabiente,
                                                arr_reactiva[j].folio_liquida,
                                                arr_reactiva[j].periodo_pago,
                                                arr_reactiva[j].aux_estado,
                                                arr_reactiva[j].folio,
                                                arr_reactiva[j].id_cre_acreditado)
                  END IF
               END FOR

               #Inicia la reactivación
               CALL fn_mensaje("","Se procesó solicitud de reactivación","")
               EXIT PROGRAM  --Se sale del programa para volver a consultar las marcas
            ELSE
               CALL fn_mensaje("","Se ha cancelado la operación","")
               EXIT INPUT 
            END IF 
           
              
                       
          ON ACTION CANCEL
             EXIT INPUT 
       END INPUT 
   CLOSE WINDOW vtn_confirma 
END FUNCTION 

FUNCTION act_marca_corresponde(v_marca,  --Marca original activa
                               p_f_inicio, --fecha de inicio marca
                               p_marca_prcr, --Marca seleccionada para conciliar
                               v_id_derecho, --id_derechohabiente
                               p_folio_liquida, --folio de liquidación 
                               p_periodo_pago,  --periodo_pago
                               p_estado,     --Estado de la solicitud
                               v_folio,     --folio de la marca en sfr_marca_Activa
                               v_referencia) --referencia

   DEFINE v_marca        SMALLINT,
          p_f_inicio      DATE,
          p_marca_prcr    SMALLINT ,
          v_id_derecho    DECIMAL(9,0),
          p_folio_liquida DECIMAL(9,0),
          p_periodo_pago  CHAR(6),
          p_estado        SMALLINT,
          v_folio         DECIMAL(9,0),
          v_referencia    DECIMAL(9,0),
          v_marca_cam     SMALLINT,
          v_solic         SMALLINT
   DEFINE v_fecha         CHAR(8)
   DEFINE v_periodo_pago  CHAR(6) 

   LET v_marca_cam = v_marca

   LET v_fecha = TODAY USING "ddmmyyyy" 
   LET v_periodo_pago = v_fecha[5,8],v_fecha[3,4]

   #En caso de que la marca PROCESAR no corresponda con la marca en conciliación
   #actualizar de la siguiente manera:

   --Si tiene marca 223 -Uso de garantía 43 Bis y seleccionó la opción 4 (Marcar como 225 -Uso de anualidad)
   IF v_marca = 223 AND p_marca_prcr = 4 THEN

      UPDATE sfr_marca_activa
      SET    marca = 225
      WHERE  folio              = v_folio
      AND    id_derechohabiente = v_id_derecho
      AND    n_referencia       = v_referencia
      AND    marca = 223;

      UPDATE sfr_marca_historica
      SET    marca = 225
      WHERE  folio              = v_folio
      AND    id_derechohabiente = v_id_derecho
      AND    n_referencia       = v_referencia
      AND    marca = 223;      

      --Actualiza tabla maestra para dejarla como una solicitud 225
      UPDATE cre_uso_garantia
         SET tpo_transferencia   = 43,
             periodo_pago        = v_periodo_pago
       WHERE id_cre_uso_garantia = v_referencia
         AND id_derechohabiente  = v_id_derecho;

      --Guarda historia del cambio de marca
      INSERT INTO cre_cambio_marca(
                     id_referencia,
                     id_derechohabiente,
                     folio_liquida,
                     periodo_pago,
                     marca_origen,
                     marca_final,
                     estado,
                     f_proceso)
             VALUES (v_referencia,
                     v_id_derecho,
                     p_folio_liquida,
                     p_periodo_pago,
                     223,
                     225,
                     p_estado,
                     TODAY);
         
      LET v_marca_cam = 225

   END IF 

   --Si tiene marca 225 -Uso de Anualidad y seleccionó la opción 2 (Marcar como 223 -Uso de garantía 43bis)
   IF(v_marca = 225) AND (p_marca_prcr = 2) THEN 
   
      UPDATE sfr_marca_activa
      SET    marca = 223
      WHERE  folio              = v_folio
      AND    id_derechohabiente = v_id_derecho
      AND    n_referencia       = v_referencia
      AND    marca = 225;
   
      UPDATE sfr_marca_historica
      SET    marca = 223
      WHERE  folio              = v_folio
      AND    id_derechohabiente = v_id_derecho
      AND    n_referencia       = v_referencia
      AND    marca = 225;

      --Actualiza tabla maestra para dejarla como una solicitud nueva 223
      UPDATE cre_uso_garantia
         SET  tpo_transferencia   = 18,
              num_credito         = 0,
              periodo_pago        = v_periodo_pago
       WHERE id_cre_uso_garantia = v_referencia
         AND id_derechohabiente  = v_id_derecho;

        --Guarda historia del cambio de marca
      INSERT INTO cre_cambio_marca(
                     id_referencia,
                     id_derechohabiente,
                     folio_liquida,
                     periodo_pago,
                     marca_origen,
                     marca_final,
                     estado,
                     f_proceso)
             VALUES (v_referencia,
                     v_id_derecho,
                     p_folio_liquida,
                     p_periodo_pago,
                     225,
                     223,
                     p_estado,
                     TODAY); 
                     
      LET v_marca_cam = 223
      
   END IF 

   #Actualiza para petición de saldo a PROCESAR

   CASE 
      WHEN v_marca = 221

         --Verifica si es un registro de adelantos
         IF(p_estado = 140) THEN 
            UPDATE cre_acreditado
               SET edo_procesar = 70
            WHERE id_cre_acreditado   = v_referencia
              AND id_derechohabiente  = v_id_derecho
              AND estado = 140;
         ELSE 
            UPDATE cre_acreditado
               SET estado = 20,
                   edo_procesar = 70
            WHERE id_cre_acreditado   = v_referencia
              AND id_derechohabiente  = v_id_derecho;
         
         END IF 

      WHEN v_marca = 223 OR v_marca = 225
         --Registro con adelanto (140- LIQUIDADA) --Adelantos
         IF(p_estado = 140) THEN 
            UPDATE cre_uso_garantia
               SET edo_procesar = 70
            WHERE id_cre_uso_garantia = v_referencia
              AND id_derechohabiente  = v_id_derecho
              AND estado = 140;
         ELSE 
            UPDATE cre_uso_garantia
               SET estado = 20,
                   edo_procesar = 70
            WHERE id_cre_uso_garantia = v_referencia
              AND id_derechohabiente  = v_id_derecho;
         
         END IF 
      
   END CASE 

   LET v_solic = 2;      --tipo solic 1=marca 2=desmarca
   
   INSERT INTO cre_sol_marca_esp VALUES (
      v_referencia,
      v_id_derecho,
      v_marca_cam,
      p_f_inicio,
      v_solic,  
      5,     --motivo
      TODAY,
      g_usuario)
   
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