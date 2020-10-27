--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRR01                                             #
#Objetivo          =>Programa lanzado que ejecuta el reverso del archivo#
#                    de entrada seleccionado por el usuario (el cual    #
#                    viene como parámetro) para el módulo de Anualudades#
#                    Garantizadas                                       #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>05 Junio 2012                                      #
#Actualización     =>Héctor Jiménez                                     #
#Fecha Act.        =>06 Abril 2015                                      #
#Descripcion       =>Se elimina la especifificacion de la BD en la      #
#                    ejecución de las funciones                         #
#########################################################################

DATABASE safre_viv
GLOBALS "AGRG01.4gl"

DEFINE p_v_usuario          LIKE seg_usuario.usuario      -- nombre del usuario
DEFINE p_d_pid              LIKE bat_ctr_proceso.pid      -- pid de la operación
DEFINE p_i_proceso_cod      LIKE cat_proceso.proceso_cod  -- codigo del proceso
DEFINE p_i_opera_cod        LIKE cat_operacion.opera_cod  -- codigo de la operacion de la etapa
DEFINE p_d_folio_rev        LIKE glo_ctr_archivo.folio    -- numero de folio
DEFINE p_v_arch_proceso     VARCHAR(100)                  -- nombre del archivo a reversar
DEFINE p_d_pid_rev          LIKE bat_ctr_proceso.pid      -- pid del reverso
DEFINE p_c_op_arch_ent      VARCHAR(5)                    -- contiene la opcion a reversar de Archivos entrada
DEFINE p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo

MAIN
   DEFINE v_s_qryTxt        STRING   -- contiene una sentencia sql a ejecutar
   DEFINE v_i_sql_error     SMALLINT -- contiene el estatus del reverso
   DEFINE r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- Se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario          = ARG_VAL(1)
   LET p_d_pid              = ARG_VAL(2) -- pid del proceso
   LET p_i_proceso_cod      = ARG_VAL(3)
   LET p_i_opera_cod        = ARG_VAL(4)
   LET p_d_folio_rev        = ARG_VAL(5) -- folio del reverso
   LET p_v_arch_proceso     = ARG_VAL(6)
   LET p_d_pid_rev          = ARG_VAL(7) -- pid del reverso
   LET p_c_op_arch_ent      = ARG_VAL(8)
   LET p_id_cre_ctr_archivo = ARG_VAL(9)

   -- Se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRR01.log")

   DISPLAY "=INICIA AGRR01="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO REV     : ",p_d_folio_rev USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " PID REV       : ",p_d_pid_rev
   DISPLAY " ID CTR ARCH   : ",p_id_cre_ctr_archivo
   DISPLAY " OPCIÓN        : ",p_c_op_arch_ent

   -- Se valida que exista el identificador del archivo
   IF p_id_cre_ctr_archivo <> 0 THEN
      DISPLAY "  EJECUTA PROCESO DE REVERSO"
      -- se ejecuta la función que reversa el proceso de archivo entrada
      LET v_s_qryTxt = " EXECUTE FUNCTION fn_agr_reversa_arch_entrada(?,?,?)"

      PREPARE prp_reversa_arch_ent FROM v_s_qryTxt
      EXECUTE prp_reversa_arch_ent USING p_d_folio_rev, p_c_op_arch_ent, p_id_cre_ctr_archivo
                                    INTO v_i_sql_error
      -- Verifica si ocurrió algun error
      IF v_i_sql_error <> 0 THEN
         DISPLAY " ERROR: Ocurrió un error en el Reverso: ",v_i_sql_error
         -- En caso de ocurrir un error en el reverso se marca el proceso como ERRONEO
         CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING r_b_valida

         -- Se valida el estatus de retorno
         IF r_b_valida <> 0 THEN
            -- En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
         END IF

         EXIT PROGRAM
      END IF
   END IF

   -- Se invoca la funcion que realiza los reversos del proceso
   CALL fn_reversa_tbls_gen()
{
   DISPLAY " ACTUALIZA ESTADO REVERSADO"
   -- Se invoca la función que actualiza el registro de control a estatus 3-Reversado
   LET r_b_valida = fn_act_edo_archivo(p_v_arch_proceso, p_d_folio_rev,3,p_v_usuario)

   -- Se verifica si fue posible actualiza el estado del archivo
   IF r_b_valida <> 0 THEN
      DISPLAY "ERROR EN ACTUALIZACIÓN EDO ARCHIVO:"
      -- En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF
}
   -- Se invoca la función que deja la operación en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- Se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      DISPLAY "ERROR EN ACTUALIZACIÓN OPERA FIN"

      -- En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN

#Objetivo: Función que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen()
   DEFINE v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod  -- Proceso a reversar
   DEFINE v_i_opera_cod_rev   LIKE cat_operacion.opera_cod  -- Operación a reversar

   -- Se verifica la opción seleccionada para asigna el proceso y operación correspondiente
   CASE p_c_op_arch_ent
      WHEN "opt1" -- Recurrente
         DISPLAY " RECURRENTE"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_recurrente
         LET v_i_opera_cod_rev = 1
      WHEN "opt2" -- Rechazo de Saldos
         DISPLAY " RECHAZO DE SALDOS"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_rech_saldos -- recepcion rechazo de saldos agr
         LET v_i_opera_cod_rev = 1
      WHEN "opt3" -- Saldos Transferidos
         DISPLAY " SALDOS TRANSFERIDOS"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_sdos_transf -- recepción saldos transferidos agr
         LET v_i_opera_cod_rev = 1
      WHEN "opt4" -- Solicitudes Devueltas
         DISPLAY " SOLICITUDES DEVUELTAS"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_devol_solic
         LET v_i_opera_cod_rev = 1
      WHEN "opt5" -- Solicitudes no Atendidas
         DISPLAY " SOLICITUDES NO ATENDIDAS"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_no_atendidas
         LET v_i_opera_cod_rev = 1
      WHEN "opt6" -- Solicitudes de Desmarca
         DISPLAY " SOLICITUDES DE DESMARCA"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_solic_desmarca
         LET v_i_opera_cod_rev = 1
      WHEN "opt11" -- Uso Anualidad o Garantia
         DISPLAY " USO ANUALIDAD O GARANTÍA"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_uso_anualid
         LET v_i_opera_cod_rev = 1
      WHEN "opt12" -- Solicitudes de Marca/Desmarca
         DISPLAY " SOLICITUDES DE MARCA/DESMARCA"
         LET v_i_proceso_cod_rev = g_proc_cod_agr_recurr_marca
         LET v_i_opera_cod_rev = 1
      OTHERWISE -- Opción no reconocida
         DISPLAY " OPCIÓN NO RECONOCIDA: ",p_c_op_arch_ent
         LET v_i_proceso_cod_rev = 0
         LET v_i_opera_cod_rev = 0
   END CASE

   -- Se eliminan los procesos y las operaciones para el proceso_cod correspondiente
   DELETE
     FROM bat_ctr_proceso
    WHERE pid = p_d_pid_rev
      AND proceso_cod = v_i_proceso_cod_rev;

   DELETE
     FROM bat_ctr_operacion
    WHERE pid = p_d_pid_rev
      AND proceso_cod = v_i_proceso_cod_rev;

   DELETE
     FROM glo_ctr_archivo
    WHERE proceso_cod = v_i_proceso_cod_rev
      AND opera_cod = v_i_opera_cod_rev
      AND nombre_archivo = p_v_arch_proceso;
END FUNCTION
