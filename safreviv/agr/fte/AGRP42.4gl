#################################################################################
#Modulo            => AGR                                                       #
#Programa          => AGRP41                                                    #
#Objetivo          => Autoriza actulaización solicitud de saldos                #
#Autor             => Jose Edaurdo Ventura                                      #
#Fecha inicio      => 17 NOVIEMBRE 2016                                         #
#Autor modifica    => Emilio Abarca,EFP.                                        #
#Fecha modifica    => 05 OCTUBRE 2017.                                          #
#################################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_usuario                 CHAR (20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_s_titulo                STRING        -- Título de la ventana
END GLOBALS

MAIN
   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AGRP42.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   CALL fn_autoriza()

END MAIN

FUNCTION fn_autoriza()

   DEFINE i               INTEGER
   DEFINE v_qry_acre      STRING 
   DEFINE v_qry_ug        STRING 
   DEFINE v_pos           INTEGER
   DEFINE v_c_marca       INTEGER 
   DEFINE v_titulo_correo STRING
   DEFINE v_mens_correo   STRING 
   DEFINE p_proceso_cod   SMALLINT 
   DEFINE p_opera_cod     SMALLINT  
   
   DEFINE arr_cons_cre DYNAMIC ARRAY OF RECORD
      nss                  CHAR(11),
      id_cre_acreditado    DECIMAL(9,0),
      tipo_proceso         CHAR(15),
      tpo_credito          SMALLINT,
      num_credito          DECIMAL(10,0),
      f_otorga             DATE,
      sdo_deudor           DECIMAL(12,2),
      estado               SMALLINT,
      edo_procesar         SMALLINT,
      v_box                SMALLINT,
      v_box1               SMALLINT
   END RECORD

   LET p_proceso_cod   = 342
   LET p_opera_cod     = 1
   LET v_titulo_correo = "PROCESO: AUTORIZA SOLICITUD DE SALDO PROCESAR"
   LET v_mens_correo   = "PROCESO: AUTORIZA SOLICITUD SALDOS \n",
                         "DESCRIPCIÓN: Se ha realizado la autorización de saldos\n
                                       a PROCESAR."

   -- Query deudores a liquidar
   LET v_qry_acre = "SELECT a.nss, \n
                            r.id_referencia,\n
                            c.tpo_credito,  \n
                            c.num_credito,  \n
                            c.f_otorga,     \n
                            c.sdo_deudor,   \n
                            c.estado,       \n
                            c.edo_procesar  \n
                       FROM cre_reenvio_procesar r,\n
                            cre_acreditado c,      \n
                            afi_derechohabiente a, \n 
                            cat_maq_credito m      \n
                      WHERE r.id_referencia = c.id_cre_acreditado       \n
                        AND c.id_derechohabiente = a.id_derechohabiente \n
                        AND c.estado = m.estado \n
                        AND m.entidad in (1,2)  \n
                        AND r.estado     = 10    \n
                        AND r.id_proceso = 1     \n
                        ORDER BY f_otorga DESC"

   -- Query para las solicitudes usos de garantía (UG)
   LET v_qry_ug = "SELECT a.nss, \n
                           r.id_referencia,\n
                           DECODE(u.tpo_transferencia,'43',4,2),\n
                           u.num_credito,\n
                           u.f_proceso,  \n
                           u.importe_v97,\n
                           u.estado,     \n
                           u.edo_procesar\n
                      FROM cre_reenvio_procesar r,
                           cre_uso_garantia u,
                           afi_derechohabiente a
                     WHERE r.id_referencia      = u.id_cre_uso_garantia
                       AND u.id_derechohabiente = a.id_derechohabiente
                       AND r.estado     = 10
                       AND r.id_proceso = 2
                       ORDER BY f_proceso DESC"
                          
   PREPARE prp_cons_deudor FROM v_qry_acre
   DECLARE crs_deudor CURSOR FOR prp_cons_deudor

   LET i = 1
   CALL arr_cons_cre.clear()  -- Limpia arreglo antes de cargarlo.

   -- Deudores
   FOREACH crs_deudor INTO arr_cons_cre[i].nss,
                            arr_cons_cre[i].id_cre_acreditado,
                            arr_cons_cre[i].tpo_credito,
                            arr_cons_cre[i].num_credito,
                            arr_cons_cre[i].f_otorga,
                            arr_cons_cre[i].sdo_deudor,
                            arr_cons_cre[i].estado,
                            arr_cons_cre[i].edo_procesar

      LET arr_cons_cre[i].tipo_proceso = "1-Deudor"

       -- Por default inhabilita los CheckBoxs
       LET arr_cons_cre[i].v_box  = 0
       LET arr_cons_cre[i].v_box1 = 0

      LET i = i + 1 -- Incremente contador

   END FOREACH 

   -- Solicitudes uso de garantía
   PREPARE prp_cons_ug FROM v_qry_ug
   DECLARE crs_ug CURSOR FOR prp_cons_ug

   FOREACH crs_ug INTO arr_cons_cre[i].nss,
                        arr_cons_cre[i].id_cre_acreditado,
                        arr_cons_cre[i].tpo_credito,
                        arr_cons_cre[i].num_credito,
                        arr_cons_cre[i].f_otorga,
                        arr_cons_cre[i].sdo_deudor,
                        arr_cons_cre[i].estado,
                        arr_cons_cre[i].edo_procesar

      LET arr_cons_cre[i].tipo_proceso = "2-Garantía"

      -- Por default inhabilita los CheckBoxs
      LET arr_cons_cre[i].v_box  = 0
      LET arr_cons_cre[i].v_box1 = 0

      LET i = i + 1
      
   END FOREACH 

    -- Elimina fila en blanco del arreglo
   IF (arr_cons_cre[arr_cons_cre.getLength()].nss IS NULL) AND 
      (arr_cons_cre[arr_cons_cre.getLength()].id_cre_acreditado IS NULL) THEN
      CALL arr_cons_cre.deleteElement(arr_cons_cre.getLength()) 
   END IF 

   OPEN WINDOW AGRP421 WITH FORM "AGRP421"

      IF i > 1 THEN

         INPUT ARRAY arr_cons_cre FROM v_arr_cons_cre.*  ATTRIBUTE (UNBUFFERED,WITHOUT DEFAULTS,
                                                                      APPEND ROW = FALSE,
                                                                      DELETE ROW = FALSE,
                                                                      INSERT ROW = FALSE)
            BEFORE ROW
               LET v_pos = arr_curr()

            ON CHANGE v_box
               IF (arr_cons_cre[v_pos].v_box = 1)THEN
                  LET arr_cons_cre[v_pos].v_box1 = 0
               END IF

            ON CHANGE v_box1
               IF (arr_cons_cre[v_pos].v_box1 = 1)THEN
                  LET arr_cons_cre[v_pos].v_box = 0
               END IF

            ON ACTION ACCEPT

               LET v_c_marca = 0 -- contador de regisros marcados en el checkBox
               
               FOR i = 1 TO arr_cons_cre.getLength()
               
                  IF arr_cons_cre[i].v_box = 1 THEN

                     LET v_c_marca = v_c_marca + 1

                     UPDATE cre_reenvio_procesar
                        SET estado = 20
                      WHERE id_referencia = arr_cons_cre[i].id_cre_acreditado

                     IF(arr_cons_cre[i].tipo_proceso = "1-Deudor") THEN
                     
                        UPDATE cre_acreditado
                           SET estado      = 140,
                               edo_procesar = 70
                        WHERE id_cre_acreditado = arr_cons_cre[i].id_cre_acreditado
                     ELSE 
                        IF(arr_cons_cre[i].tipo_proceso = "2-Garantía") THEN
                        
                           UPDATE cre_uso_garantia
                              SET estado = 140,
                                  edo_procesar = 70 
                           WHERE id_cre_uso_garantia = arr_cons_cre[i].id_cre_acreditado
                           
                        END IF 
                     END IF 
                     
                  END IF

                  IF arr_cons_cre[i].v_box1 = 1 THEN
                  
                     LET v_c_marca = v_c_marca + 1
                     
                     UPDATE cre_reenvio_procesar
                        SET estado = 30
                      WHERE id_referencia = arr_cons_cre[i].id_cre_acreditado
                      
                  END IF
                  
               END FOR

               IF(v_c_marca = 0) THEN
                  CALL fn_mensaje("","Para poder continuar debe marcar uno o varios registros","")
               ELSE
                  CALL fn_mensaje("","Las autorizaciones se relizaron de forma correcta","")

                  -- Ejecuta función que envía correo
                  CALL fn_correo_proceso(0,
                                         p_proceso_cod,
                                         p_opera_cod,
                                         NULL,
                                         v_titulo_correo,
                                         v_mens_correo)
                  EXIT INPUT 
               END IF 

            ON ACTION CANCEL
               EXIT INPUT
               
         END INPUT

      ELSE
         CALL fn_mensaje("","No hay datos pendientes por autorizar","")
      END IF

   CLOSE WINDOW AGRP421

END FUNCTION