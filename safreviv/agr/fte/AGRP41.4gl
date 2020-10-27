###################################################################
#Modulo            => AGR                                         #
#Programa          => AGRP41                                      #
#Objetivo          => Actulaización solicitud de saldos           #
#Autor             => Jose Edaurdo Ventura                        #
#Fecha inicio      => 17 NOVIEMBRE 2016                           #
#Autor modifica    => Emilio Abarca,EFP.                          #
#Fecha modifica    => 03 OCTUBRE 2017.                            #
###################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_usuario                 CHAR (20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_s_titulo                STRING        -- Título de la ventana
   DEFINE v_s_qry                   STRING
   DEFINE v_nss                     CHAR(11)
   DEFINE v_id_dh                   DECIMAL(9,0)
END GLOBALS

MAIN
   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP41.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW AGRP411 WITH FORM "AGRP411"

      INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

      ON ACTION ACCEPT
         CALL fn_procesa()
         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT
   CLOSE WINDOW AGRP411
END MAIN

FUNCTION fn_procesa()

   DEFINE v_pos        INTEGER
   DEFINE v_qry2       STRING
   DEFINE v_tabla      CHAR(30)
   DEFINE v_criterio   SMALLINT
   --contadores
   DEFINE i            INTEGER
   DEFINE c            INTEGER
   DEFINE k            INTEGER
   DEFINE z            INTEGER 

   DEFINE arr_cons_cre DYNAMIC ARRAY OF RECORD
      id_cre_referencia DECIMAL(9,0),
      tpo_originacion   SMALLINT,  
      tpo_credito       SMALLINT,
      tpo_proceso       CHAR(15),
      num_credito       CHAR(10),
      f_otorga          DATE,
      sdo_deudor        DECIMAL(12,2),
      estado            SMALLINT,
      edo_procesar      SMALLINT,
      folio             DECIMAL(9,0),
      monto             DECIMAL(12,2),
      f_liquida         DATE,
      v_box             SMALLINT
   END RECORD

   DEFINE r_datos RECORD
      id_derechohabiente DECIMAL(9,0),
      id_cre_referencia  DECIMAL(9,0),
      tpo_proceso        SMALLINT
   END RECORD
   
   DEFINE v_tot_solic   INTEGER
   DEFINE v_id_deudor   SMALLINT
   DEFINE v_ind_marca   SMALLINT 
   
   DEFINE arr_mov DYNAMIC ARRAY OF RECORD
      folio          DECIMAL(9,0),
      movimiento     SMALLINT,
      f_movimiento   DATE,
      monto_pesos    DECIMAL(12,2)
   END RECORD
   
   IF v_nss IS NULL THEN
      CALL fn_mensaje("","Debe ingresar un NSS para continuar","")
   ELSE
      SELECT id_derechohabiente
        INTO v_id_dh
        FROM afi_derechohabiente
       WHERE nss = v_nss

       -- Obtiene registros de cre_Acreditado con tipo originacion 1 y 4
       LET v_s_qry = "SELECT c.id_cre_acreditado, \n
                             c.tpo_originacion, \n
                             c.tpo_credito,     \n
                             c.num_credito,     \n
                             c.f_otorga,        \n
                             c.sdo_deudor,      \n
                             c.estado,          \n
                             c.edo_procesar,    \n
                             c.folio_liquida,'',''\n
                        FROM cre_acreditado c,   \n
                             cat_maq_credito m  \n
                       WHERE c.id_derechohabiente = ",v_id_dh,"
                         AND c.edo_procesar IN (10,70,80,85,120) \n
                         AND c.tpo_originacion IN (1,4) \n
                         AND c.estado = m.estado \n
                         AND m.entidad IN (1,2)  \n
                         ORDER BY f_otorga desc;"

       PREPARE prp_cons_cre FROM v_s_qry
       DECLARE cur_cons_cre CURSOR FOR prp_cons_cre 

       CALL arr_cons_cre.clear()
       LET i = 1
       
       FOREACH cur_cons_cre INTO arr_cons_cre[i].id_cre_referencia,
                                  arr_cons_cre[i].tpo_originacion,
                                  arr_cons_cre[i].tpo_credito,
                                  arr_cons_cre[i].num_credito,
                                  arr_cons_cre[i].f_otorga,
                                  arr_cons_cre[i].sdo_deudor,
                                  arr_cons_cre[i].estado,
                                  arr_cons_cre[i].edo_procesar,
                                  arr_cons_cre[i].folio

          LET arr_cons_cre[i].tpo_proceso = "1-Deudor"

          # Obtiene monto liquidado y f_liquida
          LET v_tabla = NULL
          LET v_criterio = 0
             
          -- Función para obtener tabla de mocimiento de acuerdo al folio
          LET v_qry2 = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"
          PREPARE prp_obt_mov FROM v_qry2
          EXECUTE prp_obt_mov USING v_criterio,
                                     arr_cons_cre[i].folio,    -- Folio de Liquidación
                                     " "                        -- En este caso no es necesario la fecha
                                     INTO v_tabla

           IF (v_tabla IS NOT NULL) THEN
              LET v_qry2 = "SELECT f_liquida,
                                   SUM(monto_pesos)
                              FROM ",v_tabla," 
                             WHERE folio_liquida = ",arr_cons_cre[i].folio,"
                               AND id_derechohabiente = ",v_id_dh,
                             " GROUP BY 1" 

              PREPARE prp_monto_deudor FROM v_qry2
              EXECUTE prp_monto_deudor INTO arr_cons_cre[i].f_liquida,
                                             arr_cons_cre[i].monto
                                        
           END IF 

          -- Por default deja como desmarcado el Checkbox
          LET arr_cons_cre[i].v_box = 0

          -- Incrementa contador
          LET i = i + 1
          
       END FOREACH

       -- Busca solicitudes de saldo en cre_uso_garantía
       LET v_s_qry = "SELECT id_cre_uso_garantia,\n
                             DECODE(tpo_transferencia,'43',4,2),\n
                             tpo_transferencia,\n
                             num_credito,\n
                             f_proceso,\n
                             importe_v97,\n
                             estado,\n
                             edo_procesar,\n
                             folio_liquida \n
                        FROM cre_uso_garantia \n
                       WHERE id_cre_uso_garantia > 0 \n
                         AND id_cre_ctr_archivo IN ( \n
                       SELECT id_cre_ctr_archivo \n
                         FROM cre_ctr_archivo \n
                        WHERE operacion NOT IN (1,6,9,14) \n
                          AND id_derechohabiente = ",v_id_dh,
                        " AND estado IN (20,140) \n
                          AND edo_procesar IN (5,60,70,75,80,85,120));"

       PREPARE prp_cons_ug FROM v_s_qry
       DECLARE cur_cons_ug CURSOR FOR prp_cons_ug

       FOREACH cur_cons_ug INTO arr_cons_cre[i].id_cre_referencia,
                                  arr_cons_cre[i].tpo_originacion,
                                  arr_cons_cre[i].tpo_credito,
                                  arr_cons_cre[i].num_credito,
                                  arr_cons_cre[i].f_otorga,
                                  arr_cons_cre[i].sdo_deudor,
                                  arr_cons_cre[i].estado,
                                  arr_cons_cre[i].edo_procesar,
                                  arr_cons_cre[i].folio

          LET arr_cons_cre[i].tpo_proceso = "2-Garantía"
           
          # Obtiene monto liquidado y f_liquida
          LET v_tabla = NULL
          LET v_criterio = 0

              
          -- Función para obtener tabla de mocimiento de acuerdo al folio
          LET v_qry2 = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"
          PREPARE prp_tab_mov FROM v_qry2
          EXECUTE prp_tab_mov USING v_criterio,
                                     arr_cons_cre[i].folio,    -- Folio de Liquidación
                                     " "                        -- En este caso no es necesario la fecha
                                     INTO v_tabla

           IF (v_tabla IS NOT NULL) THEN
              LET v_qry2 = "SELECT f_liquida,
                                   SUM(monto_pesos)
                              FROM ",v_tabla," 
                             WHERE folio_liquida = ",arr_cons_cre[i].folio,"
                               AND id_derechohabiente = ",v_id_dh,
                             " GROUP BY 1"

              PREPARE prp_monto_ug FROM v_qry2
              EXECUTE prp_monto_ug INTO arr_cons_cre[i].f_liquida,
                                         arr_cons_cre[i].monto
                                      
           END IF 

           -- Por default deja como desmarcado el Checkbox
           LET arr_cons_cre[i].v_box = 0

           -- Incrementa contador
           LET i = i + 1
          
       END FOREACH 
       
       -- Elimina la última fila en blanco
       IF (arr_cons_cre[arr_cons_cre.getLength()].id_cre_referencia IS NULL) AND 
          (arr_cons_cre[arr_cons_cre.getLength()].tpo_originacion IS NULL) THEN
          CALL arr_cons_cre.deleteElement(arr_cons_cre.getLength()) 
       END IF 

       OPEN WINDOW AGRP412 WITH FORM "AGRP412"

       DIALOG ATTRIBUTE (UNBUFFERED)

       INPUT ARRAY arr_cons_cre FROM v_arr_cons_cre.*  ATTRIBUTE (WITHOUT DEFAULTS,
                                                                    APPEND ROW = FALSE,
                                                                    DELETE ROW = FALSE,
                                                                    INSERT ROW = FALSE)

        BEFORE ROW
            LET v_pos = arr_curr() -- Posición del registro seleccionado

            {
            DISPLAY ""            
            DISPLAY "id_dh: ",v_id_dh
            DISPLAY "id_cre_referencia: ",arr_cons_cre[v_pos].id_cre_referencia
            DISPLAY "tpo proceso: ",arr_cons_cre[v_pos].tpo_proceso
            DISPLAY "folio liquida: ",arr_cons_cre[v_pos].folio
            }
            -- Obtiene movimientos por registro seleccionado
            IF (arr_cons_cre[v_pos].tpo_proceso = "1-Deudor") THEN
            
               DECLARE crs_mov_acre CURSOR FOR 
                  SELECT folio_referencia,
                          movimiento ,
                          f_movimiento,
                          monto_pesos
                    FROM cre_saldo_deudor
                   WHERE id_cre_acreditado = arr_cons_cre[v_pos].id_cre_referencia
                   ORDER BY f_movimiento DESC

               -- llena arreglo de movimientos
               LET z = 1
               CALL arr_mov.clear()
               
               FOREACH crs_mov_acre INTO arr_mov[z].folio,
                                          arr_mov[z].movimiento,
                                          arr_mov[z].f_movimiento,
                                          arr_mov[z].monto_pesos
                  LET z = z + 1
                  
               END FOREACH

               -- Elimina fila en blanco
               IF (arr_mov[arr_mov.getLength()].folio IS NULL) AND 
                  (arr_mov[arr_mov.getLength()].movimiento IS NULL) THEN
                  CALL arr_mov.deleteElement(arr_mov.getLength()) 
               END IF 
            
            END IF 

            IF (arr_cons_cre[v_pos].tpo_proceso = "2-Garantía") THEN

               LET v_tabla = NULL 
               LET v_qry2  = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

               PREPARE prp_tab_mov_ug FROM v_qry2
               EXECUTE prp_tab_mov_ug USING v_criterio,
                                             arr_cons_cre[v_pos].folio,    -- Folio de Liquidación
                                             " "                            -- En este caso no es necesario la fecha
                                             INTO v_tabla

               IF (v_tabla IS NOT NULL) THEN

                  LET v_qry2 = "SELECT folio_liquida,
                                        movimiento,
                                        f_liquida,
                                        monto_pesos
                                   FROM ",v_tabla,
                                " WHERE id_derechohabiente = ", v_id_dh,
                                  " AND folio_liquida = ",arr_cons_cre[v_pos].folio,
                                  "ORDER BY f_liquida desc;"

                  PREPARE prp_mov_ug FROM v_qry2
                  DECLARE crs_mov_ug CURSOR FOR prp_mov_ug

                  LET z = 1
                  CALL arr_mov.clear()

                  FOREACH crs_mov_ug INTO arr_mov[z].folio,
                                           arr_mov[z].movimiento,
                                           arr_mov[z].f_movimiento,
                                           arr_mov[z].monto_pesos
                     LET z = z + 1
                  
                  END FOREACH

                   -- Elimina fila en blanco
                  IF (arr_mov[arr_mov.getLength()].folio IS NULL) AND 
                     (arr_mov[arr_mov.getLength()].movimiento IS NULL) THEN
                     CALL arr_mov.deleteElement(arr_mov.getLength()) 
                  END IF 

               END IF 
            END IF 

            ON CHANGE v_box
               # Código para sólo seleccionar el checkbox de un registro
               IF arr_cons_cre[v_pos].v_box = 1 THEN
                  FOR c = 1 TO arr_cons_cre.getLength()
                     IF c <> v_pos THEN
                        LET arr_cons_cre[c].v_box = 0
                     END IF
                  END FOR
               END IF
            
         END INPUT

         DISPLAY ARRAY arr_mov TO v_arr_deudor.*  
         END DISPLAY

       ON ACTION ACCEPT
           INITIALIZE r_datos.* TO NULL
           
           # Recorre arreglo para saber si se marcó el checkbox de un registro
           FOR k = 1 TO arr_cons_cre.getLength()
           
              IF (arr_cons_cre[k].v_box = 1) THEN
              
                 LET v_ind_marca = 1   -- Registro marcado
                 
                 -- Asigna valores 
                 LET r_datos.id_derechohabiente = v_id_dh
                 LET r_datos.id_cre_referencia  = arr_cons_cre[k].id_cre_referencia

                 IF (arr_cons_cre[k].tpo_proceso = "1-Deudor") THEN
                    LET r_datos.tpo_proceso = 1
                 ELSE 
                    IF (arr_cons_cre[k].tpo_proceso = "2-Garantía") THEN
                       LET r_datos.tpo_proceso = 2
                    END IF 
                 END IF 
                 
                 EXIT FOR
                 
              ELSE 
                 LET v_ind_marca = 0
              END IF
              
           END FOR 

           IF (v_ind_marca = 0) THEN
              CALL fn_mensaje("","Para poder continuar debe marcar un registro","")
              CONTINUE DIALOG
           ELSE              
             -- Si el registro marcado de cre_Acreditado tipo de originacion "1" y "4"
             -- valida que sea un crédito liquida deudor.
             IF (r_datos.tpo_proceso = 1) THEN 
             
                SELECT tpo.id_deudor			
                   INTO v_id_deudor			
                   FROM cre_acreditado cre, cat_tipo_credito tpo			
                  WHERE cre.id_cre_acreditado = r_datos.id_cre_referencia		
                   AND cre.tpo_originacion = tpo.tpo_originacion			
                   AND cre.tpo_credito = tpo.tpo_credito	
 
                IF (v_id_deudor <> 1) THEN
                   CALL fn_mensaje("","Tipo de crédito no liquida deudor","") 
                   CONTINUE DIALOG 
                END IF 
                
             END IF 

             -- Valida que no se haya reaizado una solcitud previamente
              LET v_tot_solic = 0
              
              SELECT COUNT(*)
                 INTO v_tot_solic
                 FROM cre_reenvio_procesar
                WHERE id_derechohabiente = r_datos.id_derechohabiente
                  AND id_referencia = r_datos.id_cre_referencia
                  AND estado = 10

              IF (v_tot_solic >= 1) THEN
                 CALL fn_mensaje("","Este registro ya ha sido solicitado previamente","")
              ELSE 
                -- Hace la solicitud de reenvío a procesar.
                INSERT INTO cre_reenvio_procesar (
                                 id_derechohabiente,
                                 id_referencia,
                                 id_proceso,
                                 estado,
                                 f_proceso,
                                 usuario)
                        VALUES (r_datos.id_derechohabiente,
                                 r_datos.id_cre_referencia,
                                 r_datos.tpo_proceso,
                                 '10',
                                 TODAY,
                                 g_usuario);

                 CALL fn_mensaje("","Datos almacenados en espera de autorización","")
                 CONTINUE DIALOG 
              END IF 
           END IF 
           
       ON ACTION CLOSE 
          EXIT DIALOG
          
      END DIALOG
   END IF

END FUNCTION