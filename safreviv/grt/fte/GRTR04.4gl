--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTR04                                        #
#Objetivo          =>Programa que ejecuta el reverso de la         #
#                    transacción (preliquidacion o liquidacion)    #
#                    seleccionada por el usuario para el módulo de #
#                    Uso de Garantía 43 bis                        #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>22 Mayo 2012                                  #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

MAIN
   DEFINE p_v_usuario           LIKE seg_usuario.usuario, -- usuario firmado al sistema
          p_d_pid               DECIMAL(9,0), -- identificador del proceso
          p_i_proceso_cod       LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          p_i_opera_cod         LIKE cat_operacion.opera_cod, -- operación que llama la funcion
          p_d_folio             LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_nom_archivo       LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          p_c_op_arch_tran      VARCHAR(5), -- contiene la opcion a reversar transacciones
          r_codigo_sql          SMALLINT, -- codigo de error
          r_b_valida            SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario            = ARG_VAL(1)
   LET p_d_pid                = ARG_VAL(2)
   LET p_i_proceso_cod        = ARG_VAL(3)
   LET p_i_opera_cod          = ARG_VAL(4)
   LET p_d_folio              = ARG_VAL(5)
   LET p_v_nom_archivo        = ARG_VAL(6)
   LET p_c_op_arch_tran       = ARG_VAL(7)
   
   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTR04.log")

   DISPLAY "=INICIA GRTR04="
   DISPLAY " USUARIO        : ",p_v_usuario
   DISPLAY " PID            : ",p_d_pid
   DISPLAY " FOLIO          : ",p_d_folio USING "#########&"

   -- se verifica la opción seleccionada para validar el reverso
   CASE p_c_op_arch_tran
      WHEN "opt8" -- Preliquidación
         CALL fn_reversa_preliquidacion_usogrt(p_d_folio) RETURNING r_codigo_sql

      WHEN "opt9" -- Liquidación
         CALL fn_reverso_liquidacion_usogrt(p_d_folio) RETURNING r_codigo_sql

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

#Objetivo: Realiza el reverso de la preliquidación
FUNCTION fn_reversa_preliquidacion_usogrt(p_folio)
   DEFINE p_folio               LIKE glo_folio.folio, -- folio
          r_sql_reverso         SMALLINT -- codigo de error
      
   -- se inicializan variables
   LET r_sql_reverso = 0

   DISPLAY " REVERSA PRELIQUIDACIÓN"
   -- ejecuta el SP que realiza el reverso de la preliquidación
   PREPARE prp_reverso_preliquidacion_uso FROM "EXECUTE FUNCTION safre_viv:fn_uso_reversa_preliquida(?)"
   EXECUTE prp_reverso_preliquidacion_uso USING p_folio
                                           INTO r_sql_reverso

   -- se verifica si ocurrió algun error con el reverso
   IF r_sql_reverso <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL REALIZAR EL REVERSO (CÓDIGO):",r_sql_reverso
   END IF

   RETURN r_sql_reverso
END FUNCTION

#Objetivo: Realiza el reverso de la liquidación
FUNCTION fn_reverso_liquidacion_usogrt(p_folio)
   DEFINE p_folio               LIKE glo_folio.folio, -- folio
          r_sts_rev_cont        SMALLINT, -- estatus del registro contable
          r_sql_reverso         SMALLINT -- codigo de error

   -- se inicializan variables
   LET r_sql_reverso = 0

   DISPLAY " REVERSA REGISTRO CONTABLE"
   -- se invoca la funcion que reversa el proceso de contabilidad
   CALL fn_reverso_reg_cnt(p_folio) RETURNING r_sts_rev_cont

   -- se verifica si ocurrió algun error con el reverso
   --IF r_sts_rev_cont = 1 THEN
   IF r_sts_rev_cont > 1 THEN
      DISPLAY " ERROR: No es posible continuar con el reverso debido a que ya fue generada la\n",
              " póliza contable, o bien, la fecha de reverso es posterior a la de liquidación"
      LET r_sql_reverso = r_sts_rev_cont

      RETURN r_sql_reverso
   END IF

   DISPLAY " REVERSA LIQUIDACIÓN"
   -- ejecuta el SP que realiza el reverso de la liquidación
   PREPARE prp_reverso_liquidacion_uso FROM "EXECUTE FUNCTION safre_viv:fn_uso_reversa_liquidacion(?)"
   EXECUTE prp_reverso_liquidacion_uso USING p_folio
                                        INTO r_sql_reverso

   -- se verifica si ocurrió algun error con el reverso
   IF r_sql_reverso <> 0 THEN
      DISPLAY "OCURRIÓ UN ERROR AL REALIZAR EL REVERSO (CÓDIGO):",r_sql_reverso

      RETURN r_sql_reverso
   END IF

   RETURN r_sql_reverso
END FUNCTION

#Objetivo: Función que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen(p_c_op_arch_tran)
   DEFINE p_c_op_arch_tran    VARCHAR(5), -- contiene la opcion a reversar transacciones
          v_d_pid_rev         LIKE bat_ctr_proceso.pid, -- PID
          v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev   LIKE cat_operacion.opera_cod -- operacion a reversar

   -- se verifica la opción seleccionada para asigna el proceso y operacion correspondiente
   CASE p_c_op_arch_tran
      WHEN "opt8" -- Preliquidación
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_liquida -- liquidación uso garantía 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt9" -- Liquidación
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_liquida -- liquidación uso garantía 43bis
         LET v_i_opera_cod_rev = 2
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