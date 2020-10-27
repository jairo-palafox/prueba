--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGR15                                                                 #
#Objetivo     => Programa que realiza el reverso de liquidación para SAR 92             #
#Fecha inicio => Enero 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion SMALLINT,
       p_cad_ventana   STRING,
       v_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod LIKE cat_operacion.opera_cod,
       v_sql_procedure   STRING,
       v_correcto      BOOLEAN,
       v_folio                  DECIMAL(9,0), -- folio
       v_id_referencia          DECIMAL(9,0),
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_cbx_id_referencia      ui.ComboBox, -- combo de afores
       v_r_glo_folio            RECORD LIKE acl_ctr_lote.*,
       v_r_glo_id_referencia    RECORD LIKE acl_ctr_lote.*,
       v_s_cadena               STRING, -- cadena de texto
       v_query                  STRING,
       g_ruta_envio             LIKE seg_modulo.ruta_envio,
       g_nom_archivo            STRING,
       v_lote_cod               SMALLINT,
       v_pid                    decimal(9,0),
       v_bnd_continuar          SMALLINT,
       v_estatus                SMALLINT,
       v_existen_registros      SMALLINT    
       
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)
   LET v_proceso_cod = 105 # SAR92
   LET v_opera_cod  = 4 # SAR92
   LET v_lote_cod = 1

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_cad_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_cad_ventana)
   END IF

   
   --LET v_opera_cod_liquidacion =  #Liquidacion

   --ejecuta el store procedure (con el folio y el id secuencia del fulanito

   -- valida si se ejecuto bie
    --si se ejecuta mensaje de reverso
    --otro caso mensaje de error

    SELECT COUNT(*)
    INTO  v_existen_registros
    FROM   acl_ctr_lote b --acl_sum_sc_nss
    --WHERE  nombre_archivo IS NOT NULL
    WHERE    lote_cod = v_lote_cod
   
   IF v_existen_registros IS NULL OR v_existen_registros = 0 THEN
     CALL fn_mensaje("Atención","No existen archivosa a reversar","information")
     EXIT PROGRAM
   END IF

    OPEN WINDOW w_consulta_folio WITH FORM "ACLR101"
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
         LET v_id_referencia = NULL
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT *
         FROM   acl_ctr_lote b --acl_sum_sc_nss
         WHERE  nombre_archivo IS NOT NULL
         AND  lote_cod = v_lote_cod
         --AND    a.folio = b.f olio
         --AND
         -- estado = 2 -- integrado

         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios

    {AFTER FIELD cmb_folio
        CALL v_cbx_id_referencia.clear()
        -- se asignan los valores por omision
         LET v_id_referencia = NULL
         
         -- se llena el arreglo de folios
         DECLARE cur_id_referencias CURSOR FOR
         SELECT *
         FROM   acl_ctr_lote --acl_sum_sc_nss
         WHERE  lote_cod = v_lote_cod
         AND    folio = v_folio
         --AND
         -- estado = 2 -- integrado

         FOREACH cur_id_referencias INTO v_r_glo_id_referencia.*
            LET v_s_cadena = v_r_glo_id_referencia.id_referencia
            CALL v_cbx_id_referencia.addItem(v_r_glo_id_referencia.id_referencia, v_s_cadena)
         END FOREACH

         FREE cur_id_referencias
         }
      ON ACTION ACCEPT
         DISPLAY "v_folio:", v_folio
         DISPLAY "v_id_referencia:", v_id_referencia

         -- se debio elegir almenos un folio y/o una referencia
         IF ( v_folio IS NULL ) THEN
            CALL fn_mensaje("Reverso",
                            "Es necesario elegir un folio",
                            "stop")
            CONTINUE INPUT
         END IF
         
         -- se envian los parametros de consulta
         IF ( v_folio IS NOT NULL AND v_folio >= 0 ) THEN
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
                                    ,"Se realizará el reverso para el folio "||v_folio
                                    ||"\n¿Desea continuar?" ,"")
                                    RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF ( v_bnd_continuar = 1 ) THEN

               --Se valida si es posible realizar el reverso
               CALL fn_valida_reverso(v_pid, v_proceso_cod, v_opera_cod  )
                     RETURNING v_estatus
                     DISPLAY "v_estatus: ",v_estatus
                     
               IF v_estatus = 0 THEN
            ##
                    UPDATE acl_ctr_lote
                    SET    nombre_archivo = NULL
                    WHERE  folio = v_folio
                    --AND    id_referencia = v_id_referencia
                    AND    lote_cod = v_lote_cod

                   IF SQLCA.SQLCODE = 0 THEN
                      --Se obtiene la ruta de envio de los archivos
                      LET v_query = "\n SELECT ruta_envio         "
                                   ,"\n FROM   seg_modulo         "
                                   ,"\n WHERE  modulo_cod = 'acl' "
                      PREPARE prp_ruta_archivo FROM v_query
                      EXECUTE prp_ruta_archivo INTO g_ruta_envio
                      LET g_nom_archivo =  v_r_glo_id_referencia.nombre_archivo
                      RUN "rm "||g_ruta_envio CLIPPED||"/"||g_nom_archivo

                      --Se actualiza el estatus de la operacion como reversada en el monitor
                      CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod)
                      RETURNING v_estatus
                        
                      CALL fn_mensaje("Reverso",
                                    "Se realizó en reverso del archivo",
                                    "about")
                      EXIT INPUT
                   ELSE
                      DISPLAY "\nError en eliminar registro (Codigo):",SQLCA.SQLCODE
                      DISPLAY "Error en eliminar registro (Codigo):",SQLCA.SQLERRM,"\n"
                      CALL fn_mensaje("Reverso",
                                    "Ocurrió un Error al realizar el reverso",
                                    "about")
                      EXIT INPUT
                   END IF                   
                ELSE
                   --Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la carga
                   DISPLAY "No se puedo ejecutar el reverso de la Carga"
                   CALL fn_muestra_inc_operacion(v_estatus)
                   CONTINUE INPUT
                END IF
            ELSE
               CONTINUE INPUT
            END IF
         END IF
        
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   
   
END MAIN
