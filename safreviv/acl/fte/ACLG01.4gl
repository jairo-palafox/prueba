DATABASE safre_viv
{ ==========================================================================
Clave:  fn_w_reverso_preliquidacion_sin_cambio
Nombre: fn_w_reverso_preliquidacion_sin_cambio
Fecha creacion: 30 de Enero de 2012
Autor: Francisco López 
Narrativa del proceso que realiza:
 Esta función captura/valida el folio a reversar para la preliquidacion y la carga de los archivos
Parametros de Entrada:
 -
Parámetros de salida:
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

============================================================================}
FUNCTION fn_w_reverso_preliquidacion_acl(p_usuario_cod, p_cad_ventana, v_proceso_cod, v_opera_cod_preliquidacion)
DEFINE p_usuario_cod           char(20),
       p_cad_ventana           STRING,
       v_folio                 decimal(9,0),
       v_tmp_folio             decimal(9,0),
       v_proceso_cod           SMALLINT,
       v_opera_cod_preliquidacion SMALLINT,
       r_sql_code              SMALLINT,
       v_error                 BOOLEAN,
       v_bnd_continuar         SMALLINT,
       v_query                 STRING,
       v_pid                   decimal(9,0),
       v_estatus               SMALLINT,
       r_ruta_bin              STRING,
       r_ruta_listados         STRING,
       v_sql                   STRING, -- cadena con enunciado SQL
       v_cmb_combofolios       ui.combobox -- apuntador para el combobox con los folios

   LET v_error = FALSE

   --Obtenemos el pid del folio de la integración
   LET v_query = "\n SELECT MAX(pid)                        "
                ,"\n FROM   bat_ctr_operacion               "
                ,"\n WHERE  folio = ?                       "
                ,"\n AND    proceso_cod = ?                 "
   PREPARE prp_obtiene_pid_preliq FROM v_query
   --Se obtiene las rutas
   CALL fn_rutas("glo") RETURNING r_ruta_bin,r_ruta_listados
   DISPLAY "r_ruta_bin:",r_ruta_bin
   OPEN WINDOW w_reversaIntegracion_2 WITH FORM r_ruta_bin.trim()||"/GLOG012"

   -- se llena el combo con los folios del proceso
   LET v_sql =          "SELECT folio\n"
   LET v_sql = v_sql || "FROM glo_folio\n"
   LET v_sql = v_sql || "WHERE\n"
   LET v_sql = v_sql || "proceso_cod = " || v_proceso_cod
   LET v_sql = v_sql || "AND\n"
   LET v_sql = v_sql || "status = 1\n" -- folios liquidados estan en estatus 2
   LET v_sql = v_sql || "ORDER BY folio DESC" -- folios liquidados estan en estatus 2
   
   -- se crea el combobox
   LET v_cmb_combofolios = ui.ComboBox.forName("formonly.v_folio")
   
   CALL v_cmb_combofolios.clear()
   
   -- se ejecuta la consulta con los folios
   PREPARE sid_foliosreverso FROM v_sql
   DECLARE cur_foliosreverso CURSOR FOR sid_foliosreverso
   
   FOREACH cur_foliosreverso INTO v_folio
      CALL v_cmb_combofolios.addItem(v_folio, v_folio)
   END FOREACH
   
   LET v_folio = NULL


      INPUT BY NAME v_folio 
      WITHOUT DEFAULTS
      ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE)
      
         BEFORE INPUT
            CALL ui.Interface.setText(p_cad_ventana)
            --Se despliaga la descripción del proceso
            CALL fn_despliega_desc(v_proceso_cod, v_opera_cod_preliquidacion) 
         ON ACTION reverso
            LET v_folio = GET_FLDBUF(v_folio) CLIPPED
            IF(v_folio IS NULL)THEN
               ERROR "Capture folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF
            IF NOT(v_folio > 0)THEN
               ERROR "Capture folio válido" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF

            --Query para verificar si el folio existe
            SELECT folio
              INTO v_tmp_folio
              FROM glo_folio
             WHERE folio = v_folio
             AND   proceso_cod = v_proceso_cod
            --Verificamos el codigo sql para determinar si encontro el folio
            IF( SQLCA.SQLCODE <> 0)THEN
               ERROR "No existe folio" ATTRIBUTE(REVERSE)
               NEXT FIELD v_folio 
            END IF

            --Se obtiene el pid del folio
            EXECUTE prp_obtiene_pid_preliq USING v_folio, v_proceso_cod
                                           INTO  v_pid
            DISPLAY "PID obtenido:",v_pid
            CALL fn_ventana_confirma("Reverso de la Preliquidación"
                                    ,"Se realizara el reverso para el folio "||v_folio
                                    ||"\n¿Desea continuar?" ,"")
                                    RETURNING v_bnd_continuar
            # v_bnd_continuar = 1 --> Aceptar
            # v_bnd_continuar = 2 --> Cancelar
            IF(v_bnd_continuar = 1)THEN

               --Se valida si es posible realizar el reverso de la PRELIQUIDACIÓN
               CALL fn_valida_reverso(v_pid, v_proceso_cod, v_opera_cod_preliquidacion)
                     RETURNING v_estatus
               IF v_estatus = 0 THEN
               	
               	  --Se realiza el reverso de la PRELIQUIDACIÓN
                  CALL fn_reversa_ind_liquidacion(v_folio,v_proceso_cod,v_opera_cod_preliquidacion)
                       RETURNING r_sql_code
               	
                  --Se realiza el reverso de la PRELIQUIDACIÓN
                  CALL fn_reversa_preliquidacion(v_folio,v_proceso_cod,v_opera_cod_preliquidacion)
                       RETURNING r_sql_code
                  IF r_sql_code <> 0 THEN 
                     --Si es diferente de cero indica que ocurrio error en el reverso
                     LET v_error = TRUE
                     CALL fn_mensaje("Error # "||r_sql_code
                                    ,"Ocurrió un error al realizar\n el reverso de la preliquidación"
                                    ,"error")
                     CONTINUE INPUT
                  END IF
                  --Se actualiza el estatus de la operacion como reversada en el monitor
                  CALL fn_reversa_operacion(v_pid, v_proceso_cod, v_opera_cod_preliquidacion)
                        RETURNING v_estatus
                        
                  -- se actualiza el estatus del folio a integrado
                  LET v_sql = "UPDATE glo_folio\n"
                  LET v_sql = v_sql || "SET status = 0\n" -- integrado
                  LET v_sql = v_sql || "WHERE folio = ?"
                  PREPARE prp_actualiza_folio FROM v_sql

                  EXECUTE prp_actualiza_folio USING v_folio
               ELSE
                  --Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la PRELIQUIDACIÓN
                  DISPLAY "No se puedo ejecutar el reverso del Preliquidacion"
                  CALL fn_muestra_inc_operacion(v_estatus)
                  CONTINUE INPUT
               END IF

               
               IF v_error = FALSE THEN 
                  CALL fn_mensaje("Proceso de reverso"
                                 ,"Se realizo con éxito el reverso."
                                 ,"info")
                  EXIT INPUT
               END IF
            END IF
            
         ON ACTION cancel
            EXIT INPUT

      END INPUT

   CLOSE WINDOW w_reversaIntegracion_2

END FUNCTION


{ ==========================================================================
Clave: fn_reversa_ind_liquidacion
Nombre: fn_reversa_ind_liquidacion
Fecha creacion: 05 de Julio de 2012
Autor: Rubén Haro Castro
Narrativa del proceso que realiza: 
Parametros de Entrada:
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
--Se realiza el reverso de la del ind_liquidacion
FUNCTION fn_reversa_ind_liquidacion(p_folio,p_proceso_cod,p_opera_cod)
DEFINE p_folio         LIKE glo_folio.folio,
       p_proceso_cod   SMALLINT,
       p_opera_cod     SMALLINT,      
       v_error_sql     SMALLINT,
       v_error_isam    SMALLINT,
       v_mensaje_error VARCHAR(255)

    -- Ejecuta el SP que realiza el reverso
   PREPARE prp_reversa_preliquidacion FROM "EXECUTE FUNCTION fn_reversa_ind_liquidacion(?,?,?)"
   EXECUTE prp_reversa_preliquidacion USING p_folio, p_proceso_cod, p_opera_cod
   INTO v_error_sql,v_error_isam,v_mensaje_error

   IF(v_error_sql = 0)THEN                                                           
      RETURN FALSE
   ELSE
      DISPLAY "\nError ejecucion sp_preliquida_sin_cambio_nss (Codigo): ",v_error_sql
      DISPLAY "Error en sp_preliquida_sin_cambio_nss (Mensaje):",v_mensaje_error,"\n"
      DISPLAY "Error en sp_preliquida_sin_cambio_nss (Mensaje):",v_error_isam,"\n"   
      RETURN TRUE
   END IF

END FUNCTION    
   