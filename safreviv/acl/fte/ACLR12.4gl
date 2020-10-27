--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGR15                                                                 #
#Objetivo     => Programa que realiza el reverso de liquidación para SALIDA MANUAL      #
#Fecha inicio => Enero 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion SMALLINT,
       p_cad_ventana   STRING,
       v_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod     LIKE cat_operacion.opera_cod,
       v_folio                  DECIMAL(9,0), -- folio
       v_id_referencia          DECIMAL(9,0),
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_cbx_id_referencia      ui.ComboBox, -- combo de afores
       v_r_glo_folio            RECORD LIKE acl_ctr_lote.*,
       v_r_glo_id_referencia    RECORD LIKE acl_ctr_lote.*,
       v_s_cadena               STRING, -- cadena de texto
       v_query                  STRING,
       v_lote_cod               SMALLINT,
       v_error                 SMALLINT,
       v_pid                   decimal(9,0),
       v_bnd_continuar         SMALLINT,
       r_sql_code              SMALLINT,
       v_estatus               SMALLINT

DEFINE pid                 DECIMAL(9,0)
DEFINE p_opera_cod         SMALLINT
DEFINE p_tabla             CHAR(40)

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = 105 
   LET v_opera_cod = 3
   LET v_lote_cod = 1
   LET pid = 0
   LET p_tabla = "acl_preliquida"
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF


    OPEN WINDOW w_consulta_folio WITH FORM "ACLR121"
   #combo de folio
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
  
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   #combo de id_referencia
   -- se le asigna el apuntado del combo a la variable
   --LET v_cbx_id_referencia = ui.ComboBox.forName("formonly.cmb_id_referencia")
  
   -- se inicia el combobox en blanco
   --CALL v_cbx_id_referencia.clear()
   
   
   INPUT v_folio
   --,v_id_referencia 
   WITHOUT DEFAULTS
      FROM cmb_folio
      --, cmb_id_referencia    
      ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         -- se asignan los valores por omision
         LET v_folio = NULL
         --LET v_id_referencia = NULL
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.folio
         FROM   glo_folio a
         WHERE  a.proceso_cod = v_proceso_cod
         AND    a.status = 2   

         {SELECT DISTINCT a.*
         FROM acl_ctr_lote as a , acl_preliquida as p
         WHERE a.folio = p.folio_liquida
         AND   a.id_referencia = p.id_referencia
         --AND   a.nombre_archivo is not NULL
         AND   a.lote_cod = v_lote_cod
         AND EXISTS (
         SELECT c.* FROM cta_movimiento as c
         WHERE p.folio_liquida = c.folio_liquida
         AND   p.id_referencia = c.id_referencia)
        }
         FOREACH cur_folios INTO v_folio
            CALL v_cbx_folios.addItem(v_folio, v_folio)
         END FOREACH

         FREE cur_folios

         LET v_folio = NULL

    {AFTER FIELD cmb_folio
        CALL v_cbx_id_referencia.clear()
        -- se asignan los valores por omision
         LET v_id_referencia = NULL
         
         -- se llena el arreglo de id referencias
         DECLARE cur_id_referencias CURSOR FOR
         SELECT DISTINCT a.*
        FROM acl_ctr_lote as a , acl_preliquida as p
        WHERE a.folio = p.folio_liquida
        AND   a.id_referencia = p.id_referencia
        --AND   a.nombre_archivo is not NULL
        AND   a.lote_cod = v_lote_cod
        AND   a.folio = v_folio
        AND  EXISTS (
        SELECT c.* FROM cta_movimiento as c
        WHERE p.folio_liquida = c.folio_liquida
        AND   p.id_referencia = c.id_referencia
        AND   p.folio_liquida = v_folio)
  
         
         FOREACH cur_id_referencias INTO v_r_glo_id_referencia.*
            LET v_s_cadena = v_r_glo_id_referencia.id_referencia
            CALL v_cbx_id_referencia.addItem(v_r_glo_id_referencia.id_referencia, v_s_cadena)
         END FOREACH

         FREE cur_id_referencias
         }
         
      ON ACTION ACCEPT
         --DISPLAY "v_folio:", v_folio
         --DISPLAY "v_id_referencia:", v_id_referencia

         -- se debio elegir almenos un folio y/o una referencia
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Reverso",
                            "Debe elegir un folio",
                            "stop")
            CONTINUE INPUT
         END IF
         
         -- se envian los parametros de consulta
         IF ( v_folio IS NOT NULL AND v_folio >= 0 ) THEN
            
            ## CODIGO PARA mandar llamar el proceso de  la liquodación
            LET v_error = FALSE
            ##
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
            CALL fn_ventana_confirma("Reverso de la generación del archivo"
                                    ,"Se realizara el reverso para el folio "||v_folio
                                    ||"\n¿Desea continuar?" ,"")
                                    RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF ( v_bnd_continuar = 1 ) THEN

               -- se verifica si la poliza contable ya se hizo
               CALL fn_valida_poliza_cnt(v_folio, v_proceso_cod)
               RETURNING v_estatus

               IF ( v_estatus <> 0 ) THEN
                  CALL fn_mensaje("Atención","No se puede realizar el reverso debido a que la póliza contable ya ha sido generada","stop")
                  EXIT INPUT
               END IF
               
               --Se valida si es posible realizar el reverso
               CALL fn_valida_reverso(v_pid, v_proceso_cod, v_opera_cod  )
                     RETURNING v_estatus
                     --DISPLAY "v_estatus: ",v_estatus
                     
               IF ( v_estatus = 0 ) THEN
                  -- funcion que borra el registro contable
                  CALL  fn_reverso_reg_cnt(v_folio) RETURNING r_sql_code                

                  -- Se realiza el reverso de la LIQUIDACIÓN
                  CALL fn_reverso_liquidacion_manual_acl(v_folio)
                                 RETURNING r_sql_code

                  IF ( r_sql_code <> 0 ) THEN 
                     -- Si es diferente de cero indica que ocurrio error en el reverso
                     LET v_error = TRUE
                     CALL fn_mensaje("Error # "||r_sql_code
                                    ,"Ocurrió un error al realizar\n el reverso de la Liquidación"
                                    ,"stop")
                     CONTINUE INPUT
                  ELSE
                     --Se actualiza el estatus de la operacion como reversada en el monitor
                     CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod)
                                               RETURNING v_estatus
                  END IF
               ELSE
                  -- Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la LIQUIDACIÓN
                  DISPLAY "No se puedo ejecutar el reverso del Liquidacion"
                  CALL fn_muestra_inc_operacion(v_estatus)
                  EXIT INPUT
               END IF

               
               IF ( v_error = FALSE ) THEN 
                  CALL fn_mensaje("Proceso de reverso", "El reverso se realizó con éxito."
                                 ,"info")
                                 -- Se actualiza el estatus de la operacion como reversada en el monitor
                  CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod)
                  RETURNING v_estatus
                  EXIT INPUT
               END IF
            END IF
        ELSE
            CONTINUE INPUT
        END IF

            
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
END MAIN

#Objetivo: Realiza el reverso de la liquidación
FUNCTION fn_reverso_liquidacion_manual_acl(p_folio)
DEFINE p_folio          DECIMAL(9,0)
      ,r_sql_reverso    SMALLINT

   LET r_sql_reverso = 0
   # Ejecuta el SP que realiza el reverso
   PREPARE prp_sp_reverso_liquidacion_manual_aclara FROM "EXECUTE PROCEDURE sp_reverso_liquidacion_manual_aclara(?)"
   EXECUTE prp_sp_reverso_liquidacion_manual_aclara USING p_folio
                                                    INTO r_sql_reverso 
   RETURN r_sql_reverso 
END FUNCTION