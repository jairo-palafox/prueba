--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRR04                                             #
#Objetivo          =>Programa que ejecuta el reverso de la  validación  #
#                    e Integración del proceso seleccionado por el      #
#                    usuario (la opción viene como parámetro) para el   #
#                    módulo de DSE Anualidades Garantizadas             #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>07 Junio 2012                                      #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid             LIKE bat_ctr_proceso.pid, -- pid de la operación
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso    VARCHAR(100), -- nombre del archivo a integrar
          p_d_pid_rev         LIKE bat_ctr_proceso.pid, -- pid del reverso
          p_c_op_arch_ent     VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
          v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev   LIKE cat_operacion.opera_cod, -- operación a reversar
          v_s_qryTxt          STRING, -- contiene una sentencia sql a ejecutar
          v_i_sql_error       SMALLINT, -- contiene el estatus del reverso
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario      = ARG_VAL(1) -- nombre de usuario
   LET p_d_pid          = ARG_VAL(2) -- pid del proceso
   LET p_i_proceso_cod  = ARG_VAL(3) -- codigo del proceso
   LET p_i_opera_cod    = ARG_VAL(4) -- codigo de operación del proceso
   LET p_d_folio        = ARG_VAL(5) -- folio del reverso
   LET p_v_arch_proceso = ARG_VAL(6) -- nombre del archivo del proceso
   LET p_d_pid_rev      = ARG_VAL(7) -- pid del reverso
   LET p_c_op_arch_ent  = ARG_VAL(8) -- opción a reversar elegida

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRR04.log")

   DISPLAY "=INICIA AGRR04="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " OPCIÓN        : "

   -- se verifica la opción seleccionada para asigna el proceso y operacion correspondiente
   CASE p_c_op_arch_ent
      WHEN "opt1" -- Devolución de Saldos
         DISPLAY " DEVOLUCIÓN DE SALDOS"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_dse
         LET v_i_opera_cod_rev = 2
      WHEN "opt2" -- Recepción Rechazo Devolución
         DISPLAY " RECEPCIÓN RECHAZO DEVOLUCIÓN"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_rech_dse
         LET v_i_opera_cod_rev = 2
      WHEN "opt3" -- Recepción Confirmación Devolución
         DISPLAY " RECEPCIÓN CONFIRMACIÓN DEVOLUCIÓN"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_conf_dse
         LET v_i_opera_cod_rev = 2
   END CASE

   DISPLAY " EJECUTA PROCESO DE REVERSO"
   -- se ejecuta la función que reversa el proceso de archivo entrada
   LET v_s_qryTxt = " EXECUTE FUNCTION safre_viv:fn_reversa_dse_arch_entrada(?,?)"

   PREPARE prp_reversa_arch_ent FROM v_s_qryTxt
   EXECUTE prp_reversa_arch_ent USING p_d_folio, p_c_op_arch_ent INTO v_i_sql_error

   -- se valida el codigo de error
   IF v_i_sql_error <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      DISPLAY "ERROR EN EL PROCESO DE REVERSO: ",v_i_sql_error

      -- se invoca la función que deja la operación en estado ERRONEA
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se ejecuta la funcion que ejecuta el reverso de la operacion
   LET r_b_valida = fn_reversa_operacion(p_d_pid_rev, v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- se verifica si fue posible reversar la operación
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se asigna la operación de la validación del archivo
   LET v_i_opera_cod_rev = v_i_opera_cod_rev - 1

   -- se ejecuta la funcion que ejecuta el reverso de la operacion
   LET r_b_valida = fn_reversa_operacion(p_d_pid_rev, v_i_proceso_cod_rev, v_i_opera_cod_rev)

   -- se verifica si fue posible reversar la operación
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se invoca la función que actualiza el registro de control a estatus 3-Reversado
   LET r_b_valida = fn_act_edo_archivo(p_v_arch_proceso, p_d_folio,3,p_v_usuario)

   DISPLAY " REVERSA EDO ARCHIVO: ",r_b_valida
   -- se verifica si fue posible actualiza el estado del archivo
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

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
