#####################################################################
#Modulo            => PRT                                           #
#Programa          => PRTL27                                        #
#Objetivo          => Programa que pide al usuario la seleccion del #
#                     proceso a reversar                            #
#Autor             => Héctor F. Jiménez Lara                        #
#Fecha inicio      => 07 Agosto 2015                                #
#####################################################################

DATABASE safre_viv

   DEFINE p_v_usuario         LIKE seg_usuario.usuario             -- usuario firmado al sistema
   DEFINE m_c_programa_cod    LIKE bat_ctr_operacion.programa_cod  -- nombre del programa
   DEFINE m_i_proceso_cod     LIKE cat_proceso.proceso_cod         -- proceso que llama las funciones
   DEFINE m_i_opera_cod       LIKE cat_operacion.opera_cod         -- operación que llama la funcion
   DEFINE v_s_comando         STRING

MAIN
   DEFINE p_b_tipo_carga      SMALLINT                      -- tipo de carga (1 - modo en linea y 2 - modo batch)
   DEFINE p_v_nom_prog        VARCHAR(30)                   -- nombre del programa
   DEFINE v_c_op_arch_ent     VARCHAR(5)                    -- selección del modulo a reversar (Archivos entrada)
   DEFINE v_c_op_arch_sal     VARCHAR(5)                    -- selección del modulo a reversar (Archivos salida)
   DEFINE v_ci_op_arch_tran   VARCHAR(5)                    -- selección del modulo a reversar (Transacciones)
   DEFINE v_d_pid             DECIMAL(9,0)                  -- identificador del proceso
   DEFINE v_d_folio           LIKE bat_ctr_operacion.folio  -- folio
   DEFINE r_b_valida          SMALLINT                      -- booleana que indica si el proceso se puede ejecutar o no

   -- recupera los parametros que vienen del principal
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG (p_v_usuario CLIPPED|| ".PRTL27.log")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   -- se inicializan variables
   LET m_i_proceso_cod     = 2812   -- reverso portabilidad
   LET m_i_opera_cod       = 1      -- reverso portabilidad
   LET m_c_programa_cod    = "PRTL27"
   LET v_d_pid             = 0
   LET v_d_folio           = 0
   LET v_c_op_arch_ent     = NULL
   LET v_c_op_arch_sal     = NULL
   LET v_ci_op_arch_tran   = NULL

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(v_d_pid,m_i_proceso_cod,m_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   CLOSE WINDOW SCREEN 
   OPEN WINDOW w_reverso WITH FORM "PRTL271"

   INPUT v_ci_op_arch_tran WITHOUT DEFAULTS
    FROM gtransacciones ATTRIBUTES(UNBUFFERED)

      ON ACTION ACCEPT
         -- se valida la operacion seleccionada
         IF v_ci_op_arch_tran IS NULL THEN
            CALL fn_mensaje("Reverso","Debe seleccionar un módulo a reversar para continuar","stop")
            CONTINUE INPUT
         END IF

         -- varifica si el usuario selecciono la opción Transacciones a reversar
         IF v_ci_op_arch_tran IS NOT NULL THEN
            CALL fn_reversa_transacciones(v_ci_op_arch_tran)
         END IF

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w_reverso
END MAIN

#Objetivo: Reverso de Transacciones, permite la captura de la transaccion a reversar
FUNCTION fn_reversa_transacciones(p_c_op_arch_tran)
   DEFINE p_c_op_arch_tran      VARCHAR(5)                         -- contiene la opcion a reversar transacciones
   DEFINE v_d_folio             INTEGER                            -- numero de folio
   DEFINE v_c_ruta_list_bat     LIKE seg_modulo.ruta_listados      -- ruta listados de bat
   DEFINE v_c_ruta_bin          LIKE seg_modulo.ruta_bin           -- ruta del bin del módulo
   DEFINE v_d_pid               LIKE bat_ctr_operacion.pid         -- identificador del proceso
   DEFINE v_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo -- nombre del archivo
   DEFINE v_s_qryTxt            STRING                             -- se asigna una sentencia sql a ejecutar
   DEFINE v_s_mensaje           STRING                             -- mensaje a mostrar a usuario
   DEFINE v_b_continua          SMALLINT                           -- booleana que indica si el proceso continua
   DEFINE v_i_cta_reg           SMALLINT                           --contador de registros
   DEFINE r_b_valida            SMALLINT                           -- booleana que indica si el proceso se puede ejecutar o no

   -- se inicializan variables
   LET v_d_pid         = 0
   LET v_d_folio       = NULL
   LET v_v_nom_archivo = NULL

   -- se obtienen las rutas de control del modulo
   LET v_s_qryTxt = " SELECT ruta_bin\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'prt'"

   PREPARE prp_slc_ruta_bin2 FROM v_s_qryTxt
   EXECUTE prp_slc_ruta_bin2 INTO v_c_ruta_bin

   -- se obtienen la ruta de listados para el modulo bat
   LET v_s_qryTxt = " SELECT ruta_listados\n",
                    "   FROM seg_modulo\n",
                    "  WHERE modulo_cod = 'bat'"

   PREPARE prp_slc_rutaListadosBat4 FROM v_s_qryTxt
   EXECUTE prp_slc_rutaListadosBat4 INTO v_c_ruta_list_bat

   OPEN WINDOW w_arch_transaccion WITH FORM "PRTL272"

   INPUT v_d_folio WITHOUT DEFAULTS FROM folio ATTRIBUTES(UNBUFFERED)
      AFTER FIELD folio
         NEXT FIELD folio

      ON ACTION ACCEPT
         IF v_d_folio IS NULL THEN
            CALL fn_mensaje("Reverso de Transacciones","Debe ingresar el folio para continuar","stop")
            CONTINUE INPUT
         END IF

         -- se inicializa el contador de registros
         LET v_i_cta_reg = 0

         -- se verifica la opción seleccionada para validar el reverso
         CASE p_c_op_arch_tran
            WHEN "opt8" -- Preliquidación
               CALL fn_valida_preliquidacion(v_d_folio) RETURNING v_b_continua

            WHEN "opt9" -- Liquidación
               CALL fn_valida_liquidacion(v_d_folio) RETURNING v_b_continua
            OTHERWISE
               CALL fn_mensaje("Reverso","Opción no reconocida","stop")
               CONTINUE INPUT 
         END CASE

         -- si la validacion no fue aceptada no continua con el reverso
         IF NOT v_b_continua THEN
            CONTINUE INPUT
         END IF

         -- se invoca la funcion que genera el PID
         CALL fn_genera_pid(m_i_proceso_cod, m_i_opera_cod, p_v_usuario) RETURNING v_d_pid

         -- se invoca la funcion que inicializa el proceso
         LET r_b_valida = fn_inicializa_proceso(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                v_d_folio, m_c_programa_cod,
                                                v_v_nom_archivo, p_v_usuario)

         IF r_b_valida = 0 THEN
            -- se invoca la función que deja la operación en estado Procesando
            LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, m_i_proceso_cod, m_i_opera_cod,
                                                    v_d_folio, m_c_programa_cod,
                                                    v_v_nom_archivo, p_v_usuario)

            -- se verifica si fue posible inicializar la operacion
            IF r_b_valida = 0 THEN
               -- se asigna el nombre del archivo
               LET v_v_nom_archivo = "N/A"


               -- se crea el comando que ejecuta el proceso de reverso de transacciones
               LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin CLIPPED,"/PRTR01 ",
                                                       p_v_usuario         , " ",
                                                       v_d_pid             , " ",
                                                       m_i_proceso_cod     , " ",
                                                       m_i_opera_cod       , " ",
                                                       v_d_folio           , " ",
                                                       v_v_nom_archivo     , " ",
                                                       p_c_op_arch_tran    , " 1> ",
                                                       v_c_ruta_list_bat CLIPPED,
                                                       "/nohup:",v_d_pid USING "&&&&&",":",
                                                       m_i_proceso_cod USING "&&&&&",":",
                                                       m_i_opera_cod USING "&&&&&",
                                                       " 2>&1 &"

               RUN v_s_comando

               -- se asigna el mensaje a mostrar al usuario
               LET v_s_mensaje = "Se ha enviado el reverso con PID: ",v_d_pid CLIPPED,
                                 ".\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
               CALL fn_mensaje("Reverso",v_s_mensaje,"information")
            ELSE
               -- en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_b_valida)
               CONTINUE INPUT
            END IF
         ELSE
            -- en caso de error se muestra un mensaje a usuario y no continua
            CALL fn_muestra_inc_operacion(r_b_valida)
            CONTINUE INPUT
         END IF

         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

   END INPUT
   CLOSE WINDOW w_arch_transaccion

END FUNCTION

FUNCTION fn_valida_preliquidacion(p_d_folio)
   DEFINE p_d_folio    LIKE bat_ctr_operacion.folio -- folio
   DEFINE v_i_cta_reg  INTEGER                      -- contador de registros
   DEFINE v_b_valida   SMALLINT                     -- variable que indica si la validación fue correcta o no
   DEFINE v_s_qryTxt   STRING                       -- se asigna la sentencia sql a ejecutar
   DEFINE v_cnt_movs            INTEGER

   -- se inicilizan variables
   LET v_b_valida = TRUE

   -- se verifica que no este liquidado
   LET v_s_qryTxt = " SELECT COUNT(*)
                        FROM cta_movimiento
                       WHERE folio_liquida = ?"
   PREPARE prp_cnt_movs FROM v_s_qryTxt
   EXECUTE prp_cnt_movs INTO v_cnt_movs USING p_d_folio

   DISPLAY "CONTADOR DE MOVS : ",v_cnt_movs
   IF v_cnt_movs > 0 THEN
      CALL fn_mensaje("Alerta","El folio ya fue liquidado","stop")
      --EXIT PROGRAM
      LET v_b_valida = FALSE

      RETURN v_b_valida
   END IF


   -- consulta si existe el identificador en la tabla de preliquidación
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM prt_preliq_portabilidad\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_prtPreliquida FROM v_s_qryTxt
   EXECUTE prp_count_prtPreliquida INTO v_i_cta_reg

   -- verifica si se entontraron registro en la consulta
   IF v_i_cta_reg = 0 THEN
      -- se valida que exista el folio
      LET v_s_qryTxt = " SELECT COUNT(*)\n",
                       "   FROM glo_folio\n",
                       "  WHERE folio = ",p_d_folio

      PREPARE prp_count_gloFolio FROM v_s_qryTxt
      EXECUTE prp_count_gloFolio INTO v_i_cta_reg

      IF v_i_cta_reg = 0 THEN
         -- no se encontro el folio se muestra mensaje de error
         CALL fn_mensaje("Reverso","Folio a reversar no localizado","stop")

         -- se indica que el reverso no procede ya que no existen registros en tabla maestro
         LET v_b_valida = FALSE

         RETURN v_b_valida
      ELSE
         -- se indica que el reverso procede ya que existen folio
         LET v_b_valida = TRUE

         RETURN v_b_valida
      END IF
   END IF

   RETURN v_b_valida
END FUNCTION

# Objetivo: Se valida el proceso de reverso de Liquidación
FUNCTION fn_valida_liquidacion(p_d_folio)
   DEFINE p_d_folio           LIKE bat_ctr_operacion.folio -- folio
   DEFINE v_i_edo_transaccion LIKE cnt_transaccion.estado  -- estado transacción
   DEFINE v_i_cta_reg         INTEGER                      -- contador de registros
   DEFINE v_b_valida          SMALLINT                     -- variable que indica si la validación fue correcta o no
   DEFINE v_s_qryTxt          STRING                       -- se asigna la sentencia sql a ejecutar

   -- se asume que la transacción se podrá realizar
   LET v_b_valida = TRUE

   -- se consulta si existe el folio a reversar
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cta_movimiento\n",
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_ctaMovim FROM v_s_qryTxt
   EXECUTE prp_count_ctaMovim INTO v_i_cta_reg

   -- si no existe el folio se muestra mensaje
   IF v_i_cta_reg = 0 THEN
      -- no se encontro el folio se muestra mensaje de error
      CALL fn_mensaje("Reverso","Folio a reversar no localizado","stop")

      -- se indica que el reverso no procede ya que no existen registros en tabla maestro
      LET v_b_valida = FALSE

      RETURN v_b_valida
   END IF

   -- se consulta si existen registros en cnt transacción
   LET v_s_qryTxt = " SELECT COUNT(*)\n",
                    "   FROM cnt_transaccion\n",                
                    "  WHERE folio_liquida = ",p_d_folio

   PREPARE prp_count_cntTrans FROM v_s_qryTxt
   EXECUTE prp_count_cntTrans INTO v_i_cta_reg

   -- si no existen registros se muestra mensaje
   IF v_i_cta_reg > 0 THEN
      -- se consulta el estado en cnt transaccion
      LET v_s_qryTxt = " SELECT UNIQUE estado\n",
                       "   FROM cnt_transaccion\n",
                       "  WHERE folio_liquida = ",p_d_folio

      PREPARE prp_uniq_cntTrans FROM v_s_qryTxt
      EXECUTE prp_uniq_cntTrans INTO v_i_edo_transaccion

      -- si el estado es diferente a 10 no se realiza el reverso
      IF v_i_edo_transaccion <> 10 THEN
         CALL fn_mensaje("Reverso de Transacciones","Folio ya contabilizado","stop")

         -- se indica que el reverso no procede ya que no existen registros en tabla maestro
         LET v_b_valida = FALSE

         RETURN v_b_valida
      END IF
   END IF

   RETURN v_b_valida
END FUNCTION
