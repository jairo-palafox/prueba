--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRR02                                             #
#Objetivo          =>Programa que ejecuta los reversos de               #
#                    transacciones (preliquidacion y liquidacion)       #
#                    para el módulo de Anualidades Garantizadas         #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>05 Junio 2012                                      #
#Actualización     =>Héctor Jiménez                                     #
#Fecha Act.        =>06 Abril 2015                                      #
#Descripcion       =>Se elimina la especifificacion de la BD(safre_viv) #
#                    en la ejecución de las funciones,se modifica la    #
#                    definición de variables                            #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario            -- usuario firmado al sistema
   DEFINE p_d_pid              DECIMAL(9,0)                        -- identificador del proceso
   DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod        -- proceso que llama las funciones
   DEFINE p_i_opera_cod        LIKE cat_operacion.opera_cod        -- operación que llama la funcion
   DEFINE p_d_folio            LIKE glo_ctr_archivo.folio          -- numero de folio
   DEFINE p_v_nom_archivo      LIKE bat_ctr_operacion.nom_archivo  -- nombre del archivo
   DEFINE p_c_op_arch_tran     VARCHAR(5)                          -- contiene la opcion a reversar transacciones
   DEFINE p_i_tpo_originacion  LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE r_codigo_sql         SMALLINT                            -- codigo de error
   DEFINE r_b_valida           SMALLINT                            -- indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario          = ARG_VAL(1)
   LET p_d_pid              = ARG_VAL(2)
   LET p_i_proceso_cod      = ARG_VAL(3)
   LET p_i_opera_cod        = ARG_VAL(4)
   LET p_d_folio            = ARG_VAL(5)
   LET p_v_nom_archivo      = ARG_VAL(6)
   LET p_c_op_arch_tran     = ARG_VAL(7)
   LET p_i_tpo_originacion  = ARG_VAL(8)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRR02.log")

   DISPLAY "=INICIA AGRR02="
   DISPLAY " USUARIO        : ",p_v_usuario
   DISPLAY " PID            : ",p_d_pid
   DISPLAY " FOLIO          : ",p_d_folio USING "#########&"
   DISPLAY " TPO ORIGINACION: ",p_i_tpo_originacion

   -- se verifica la opción seleccionada para validar el reverso
   CASE p_c_op_arch_tran
      WHEN "opt8" -- Preliquidación
         CALL fn_reversa_preliquidacion_agr(p_d_folio, p_i_tpo_originacion) RETURNING r_codigo_sql

      WHEN "opt9" -- Liquidación
         CALL fn_reverso_liquidacion_agr(p_d_folio, p_i_tpo_originacion, p_v_usuario) RETURNING r_codigo_sql

      WHEN "opt10" -- Saldos Remanentes
         CALL fn_reversa_saldos_remanentes_agr(p_d_folio, p_i_tpo_originacion) RETURNING r_codigo_sql

      OTHERWISE
         DISPLAY "REVERSA TRANSACCIONES - OPCIÓN NO RECONOCIDA"
   END CASE

   -- se verifica si ocurrió algun error con el reverso
   IF r_codigo_sql <> 0 THEN
      -- se marca el proceso como rechazado
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que realiza los reversos del proceso
   CALL fn_reversa_tbls_gen(p_c_op_arch_tran)

   -- se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF

   DISPLAY "=FIN="
END MAIN

#Objetivo: Realiza el reverso de la pre liquidación AGR
FUNCTION fn_reversa_preliquidacion_agr(p_folio, p_i_tpo_originacion)
   DEFINE p_folio             LIKE glo_folio.folio                -- folio
   DEFINE p_i_tpo_originacion LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE r_sql_reverso       SMALLINT                            -- codigo de error
      
   -- se inicializan variables
   LET r_sql_reverso = 0

   DISPLAY " REVERSA PRELIQUIDACIÓN"
   -- ejecuta el SP que realiza el reverso de la preliquidación
   PREPARE prp_fn_reverso_preliquidacion FROM "EXECUTE FUNCTION fn_agr_reversa_preliquidacion(?,?)"
   EXECUTE prp_fn_reverso_preliquidacion USING p_folio, p_i_tpo_originacion
                                          INTO r_sql_reverso

   -- se verifica si ocurrió algun error con el reverso
   IF r_sql_reverso <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL REALIZAR EL REVERSO (CÓDIGO):",r_sql_reverso
   END IF

   RETURN r_sql_reverso
END FUNCTION

#Objetivo: Realiza el reverso de la liquidación AGR
FUNCTION fn_reverso_liquidacion_agr(p_folio, p_i_tpo_originacion, p_v_usuario)
   DEFINE p_folio             LIKE glo_folio.folio                -- folio
   DEFINE p_i_tpo_originacion LIKE cre_acreditado.tpo_originacion -- tipo de originación
   DEFINE p_v_usuario         LIKE seg_usuario.usuario            -- usuario firmado al sistema
   DEFINE r_sts_rev_cont      SMALLINT                            -- estatus del registro contable
   DEFINE r_sql_reverso       SMALLINT                            -- codigo de error

   -- se inicializan variables
   LET r_sql_reverso = 0

   DISPLAY "REVERSA OPERACIONES COMPLEMENTARIAS"

   UPDATE bat_ctr_operacion
      SET folio = ""
    WHERE pid IN(SELECT pid
                   FROM bat_ctr_proceso
                  WHERE folio = p_folio)
      AND opera_cod > 3

   DISPLAY " REVERSA REGISTRO CONTABLE"
   -- se invoca la funcion que reversa el proceso de contabilidad
   CALL fn_reverso_reg_cnt(p_folio) RETURNING r_sts_rev_cont

   -- se verifica si ocurrió algun error con el reverso
   IF r_sts_rev_cont > 1 THEN
      DISPLAY " ERROR: No es posible continuar con el reverso debido a que ya fue generada la\n",
              " póliza contable, o bien, la fecha de reverso es posterior a la de liquidación"
      LET r_sql_reverso = r_sts_rev_cont

      RETURN r_sql_reverso
   END IF

   DISPLAY " REVERSA LIQUIDACIÓN"
   -- ejecuta el SP que realiza el reverso de la liquidación
   PREPARE prp_reverso_liquidacion FROM "EXECUTE FUNCTION fn_agr_reversa_liquidacion(?,?)"
   EXECUTE prp_reverso_liquidacion USING p_folio, p_i_tpo_originacion
                                    INTO r_sql_reverso

   -- se verifica si ocurrió algun error con el reverso
   IF r_sql_reverso <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL REALIZAR EL REVERSO (CÓDIGO):",r_sql_reverso

      RETURN r_sql_reverso
   END IF

   RETURN r_sql_reverso
END FUNCTION

#Objetivo: Realiza el reverso de saldos remanentes agr
FUNCTION fn_reversa_saldos_remanentes_agr(p_folio, p_i_tpo_originacion)
   DEFINE p_folio             LIKE glo_folio.folio                 -- folio
   DEFINE p_i_tpo_originacion LIKE cre_acreditado.tpo_originacion  -- tipo de originación
   DEFINE r_sql_reverso       SMALLINT                             -- código de error

   -- se inicializan variables
   LET r_sql_reverso = 0

   DISPLAY " REVERSA SALDOS REMANENTES"
   -- ejecuta el SP que realiza el reverso de saldos remanentes
   PREPARE prp_fn_reverso_remanentes_agr FROM "EXECUTE FUNCTION fn_reversa_saldo_remanente_acr(?)"
   EXECUTE prp_fn_reverso_remanentes_agr USING p_i_tpo_originacion
                                          INTO r_sql_reverso

   -- se verifica si ocurrió algun error con el reverso
   IF r_sql_reverso <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL REALIZAR EL REVERSO (CÓDIGO):",r_sql_reverso
   END IF

   RETURN r_sql_reverso
END FUNCTION

#Objetivo: Función que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen(p_c_op_arch_tran)
   DEFINE p_c_op_arch_tran    VARCHAR(5)                   -- contiene la opcion a reversar transacciones
   DEFINE v_d_pid_rev         LIKE bat_ctr_proceso.pid     -- pid
   DEFINE v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod -- proceso a reversar
   DEFINE v_i_opera_cod_rev   LIKE cat_operacion.opera_cod -- operacion a reversar

   -- se verifica la opción seleccionada para asigna el proceso y operacion correspondiente
   CASE p_c_op_arch_tran
      WHEN "opt8" -- Preliquidación
         LET v_i_proceso_cod_rev = g_proc_cod_agr_liquidacion
         LET v_i_opera_cod_rev = 1
      WHEN "opt9" -- Liquidación
         LET v_i_proceso_cod_rev = g_proc_cod_agr_liquidacion
         LET v_i_opera_cod_rev = 2
      WHEN "opt10" -- Saldos Remanentes
         LET v_i_proceso_cod_rev = g_proc_cod_agr_sdos_reman
         LET v_i_opera_cod_rev = 1
      OTHERWISE
         DISPLAY "REVERSA TRANSACCIONES - OPCIÓN NO RECONOCIDA"
   END CASE

   -- se obtiene el del proceso y operación a reversar
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- verifica si se trata de la primera operacion del proceso
   IF v_i_opera_cod_rev = 1 THEN
      -- se eliminan el procesos y las operaciones para el proceso_cod correspondiente
      DELETE
        FROM bat_ctr_proceso
       WHERE pid = v_d_pid_rev
         AND proceso_cod = v_i_proceso_cod_rev

      -- se eliminan las operaciones para el proceso_cod correspondiente
      DELETE
        FROM bat_ctr_operacion
       WHERE pid = v_d_pid_rev
         AND proceso_cod = v_i_proceso_cod_rev
   ELSE
      -- se eliminan las operaciones para el proceso_cod correspondiente
      UPDATE bat_ctr_proceso
         SET estado_cod = 2,
             fecha_fin  = NULL
       WHERE pid = v_d_pid_rev
         AND proceso_cod = v_i_proceso_cod_rev

      -- se actualiza la operación
      UPDATE bat_ctr_operacion
         SET estado_cod = 1,
             fecha_ini = NULL,
             fecha_fin  = NULL,
             nom_archivo = NULL
       WHERE pid = v_d_pid_rev
         AND proceso_cod = v_i_proceso_cod_rev
         AND opera_cod >= v_i_opera_cod_rev
   END IF

END FUNCTION