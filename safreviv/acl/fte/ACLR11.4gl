--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLR11                                                                 #
#Objetivo     => Programa que realiza el reverso de preliquidacion de ACl Salida manual #
#Fecha inicio => Abril 18, 2012                                                         #
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion          SMALLINT,
       p_cad_ventana            STRING,
       v_proceso_cod            LIKE cat_proceso.proceso_cod,
       v_opera_cod              LIKE cat_operacion.opera_cod,
       v_sql_procedure          STRING,
       v_correcto               BOOLEAN,
       v_folio                  DECIMAL(9,0), -- folio
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_query                  STRING,
       v_id_derechohabiente     DECIMAL(9,0),
       v_lote_cod               SMALLINT,
       v_pid                    decimal(9,0),
       v_bnd_continuar          SMALLINT,
       v_estatus                SMALLINT,
       v_folio_original         DECIMAL(9,0),
       v_id_referencia_original LIKE cta_his_pagos.id_referencia,
       v_r_cta_his_pagos_nuevo  RECORD LIKE cta_his_pagos.* -- registro de cta_his_pagos con el folio de preliquidacion

   -- se recuperan los parámetros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod   = 105 -- salida manual
   LET v_opera_cod     = 2 -- preliquidacion
   LET v_lote_cod      = 1

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

    OPEN WINDOW w_consulta_folio WITH FORM "ACLR111"
   #combo de folio
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   INPUT v_folio
   --, v_id_referencia 
   WITHOUT DEFAULTS
      FROM cmb_folio
      --, cmb_id_referencia    
      ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio = NULL
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.folio
         FROM   glo_folio a
         WHERE  a.status = 1
         AND    proceso_cod = v_proceso_cod
{
         SELECT *
         FROM   acl_ctr_lote 
         WHERE  lote_cod = v_lote_cod
         }
         
         FOREACH cur_folios INTO v_folio
            CALL v_cbx_folios.addItem(v_folio, v_folio)
         END FOREACH

         FREE cur_folios

         LET v_folio = NULL
         
      ON ACTION ACCEPT
         DISPLAY "v_folio:", v_folio

         -- se debio elegir almenos un folio y/o una referencia
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Reverso",
                            "Es necesario elegir un folio",
                            "stop")
            CONTINUE INPUT
         END IF
         
         -- se envian los parametros de consulta
         IF ( v_folio IS NOT NULL ) THEN
            LET v_correcto = FALSE
            --Obtenemos el pid del folio de la integración
            LET v_query = "\n SELECT MAX(pid)                        "
                          ,"\n FROM   bat_ctr_operacion               "
                          ,"\n WHERE  folio = ?                       "
                          ,"\n AND    proceso_cod = ?                 "
            PREPARE prp_obtiene_pid_int FROM v_query
            --Se obtiene el pid del folio
            EXECUTE prp_obtiene_pid_int USING v_folio, v_proceso_cod
                                        INTO  v_pid
            DISPLAY "PID obtenido:",v_pid
            CALL fn_ventana_confirma("Atención"
                                    ,"Se realizará el reverso para el folio "||v_folio
                                    ||"\n¿Desea continuar?" ,"")
                                    RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF ( v_bnd_continuar = 1 ) THEN

               --Se valida si es posible realizar el reverso
               CALL fn_valida_reverso(v_pid, v_proceso_cod, v_opera_cod  )
                     RETURNING v_estatus
                     --DISPLAY "v_estatus: ",v_estatus
                     
               IF ( v_estatus = 0 ) THEN

                    -- se obtiene el registro preliquidado
                    SELECT *
                    INTO   v_r_cta_his_pagos_nuevo.*
                    FROM   cta_his_pagos
                    WHERE  folio = v_folio
               
                    -- modificar el estatus de los registros
                    SELECT id_derechohabiente, folio_referencia
                    INTO   v_id_derechohabiente, v_folio_original
                    FROM   cta_his_pagos
                    WHERE  folio = v_folio

                    DISPLAY "Folio: ", v_folio
                    DISPLAY "Folio referencia: ", v_folio_original

                    -- se obtiene el id_referencia original
                    SELECT id_referencia
                    INTO   v_id_referencia_original
                    FROM   cta_his_pagos
                    WHERE  folio = v_folio_original
                    AND    id_derechohabiente = v_id_derechohabiente
                    AND    nrp          = v_r_cta_his_pagos_nuevo.nrp
                    AND    periodo_pago = v_r_cta_his_pagos_nuevo.periodo_pago
                    AND    folio_sua    = v_r_cta_his_pagos_nuevo.folio_sua
                    AND    imp_am_cre   = v_r_cta_his_pagos_nuevo.imp_am_cre
                    AND    imp_ap_pat   = v_r_cta_his_pagos_nuevo.imp_ap_pat
                    AND    imp_ren_viv_pgo_ext = v_r_cta_his_pagos_nuevo.imp_ren_viv_pgo_ext

                    -- se actualiza el folio original a ind_liquidacion = 1
                    UPDATE cta_his_pagos
                    SET    ind_liquidacion = 1
                    WHERE  ind_liquidacion = 6
                    AND    folio = v_folio_original
                    AND    id_referencia = v_id_referencia_original

                    -- se actualiza el estado de pago del folio original
                    UPDATE pag_ctr_pago
                    SET    estado_pago = 20
                    WHERE  estado_pago = 80
                    AND    folio = v_folio_original
                    AND    id_referencia = v_id_referencia_original
                    
                    -- se borran los registro generados en la preliquidacion
                    DELETE FROM cta_his_pagos
                    WHERE  folio = v_folio

                    DELETE FROM cta_pag_complemento
                    WHERE  folio = v_folio

                    DELETE FROM pag_ctr_pago
                    WHERE  folio = v_folio
                    
                    -- se borran los registros creados en la preliquidacion
                    DELETE FROM acl_preliquida
                    WHERE  folio_liquida = v_folio
                    
                    -- se borran los registros en excepciom
                    DELETE FROM pag_excep_preliquida
                    WHERE  folio = v_folio

                    -- se borra el registro de control del lote
                    DELETE FROM acl_ctr_lote
                    WHERE  folio = v_folio

                    -- se cambia el estatus del folio a reversado
                    LET v_sql_procedure = "UPDATE glo_folio SET status = -1 WHERE folio = ?"
                    PREPARE sid_updt_folio FROM v_sql_procedure
                    EXECUTE sid_updt_folio USING v_folio
                    
                    --S e actualiza el estatus de la operacion como reversada en el monitor
                    CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod)
                    RETURNING v_estatus

                    -- Se actualiza el estatus de la operacion como reversada en el monitor
                    LET v_opera_cod = 1 
                    CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod)
                    RETURNING v_estatus

                    CALL fn_mensaje("Revero","Se realizó el reverso exitosamente","information")
                    EXIT INPUT

                ELSE
                  --Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la carga
                  DISPLAY "No se puedo ejecutar el reverso"
                  CALL fn_muestra_inc_operacion(v_estatus)
                  CONTINUE INPUT
                END IF
            END IF
         ELSE
            CONTINUE INPUT
         END IF
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   
   
END MAIN
