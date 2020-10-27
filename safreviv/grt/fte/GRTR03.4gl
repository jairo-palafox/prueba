--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>GRT                                                #
#Programa          =>GRTR03                                             #
#Objetivo          =>Programa lanzado que ejecuta el reverso del archivo#
#                    de entrada seleccionado por el usuario (el cual    #
#                    viene como par�metro) para el m�dulo de Uso de     #
#                    garant�a 43 bis                                    #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>22 Mayo 2012                                       #
#########################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- nombre del usuario
       p_d_pid              LIKE bat_ctr_proceso.pid, -- pid de la operaci�n
       p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       p_i_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
       p_d_folio_rev        LIKE glo_ctr_archivo.folio, -- numero de folio
       p_v_arch_proceso     VARCHAR(100), -- nombre del archivo a reversar
       p_d_pid_rev          LIKE bat_ctr_proceso.pid, -- pid del reverso
       p_c_op_arch_ent      VARCHAR(5), -- contiene la opcion a reversar de Archivos entrada
       p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo
          
MAIN
   DEFINE v_s_qryTxt        STRING, -- contiene una sentencia sql a ejecutar
          v_i_sql_error     SMALLINT, -- contiene el estatus del reverso
          r_b_valida        SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario          = ARG_VAL(1)
   LET p_d_pid              = ARG_VAL(2) -- pid del proceso
   LET p_i_proceso_cod      = ARG_VAL(3)
   LET p_i_opera_cod        = ARG_VAL(4)
   LET p_d_folio_rev        = ARG_VAL(5) -- folio del reverso
   LET p_v_arch_proceso     = ARG_VAL(6)
   LET p_d_pid_rev          = ARG_VAL(7) -- pid del reverso
   LET p_c_op_arch_ent      = ARG_VAL(8)
   LET p_id_cre_ctr_archivo = ARG_VAL(9)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTR03.log")

   DISPLAY "=INICIA GRTR03="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso
   DISPLAY " OPCI�N        : ",p_c_op_arch_ent
   DISPLAY " FOLIO REV     : ",p_d_folio_rev USING "#########&"
   DISPLAY " PID REV       : ",p_d_pid_rev
   DISPLAY " ID ARCHIVO    : ",p_id_cre_ctr_archivo

   IF p_id_cre_ctr_archivo <> 0 THEN
      DISPLAY "  EJECUTA PROCESO DE REVERSO"
      -- se ejecuta la funci�n que reversa el proceso de archivo entrada
      LET v_s_qryTxt = " EXECUTE FUNCTION safre_viv:fn_uso_reversa_arch_entrada(?,?,?)"

      PREPARE prp_reversa_arch_ent FROM v_s_qryTxt
      EXECUTE prp_reversa_arch_ent USING p_d_folio_rev, p_c_op_arch_ent, p_id_cre_ctr_archivo
                                    INTO v_i_sql_error
      IF v_i_sql_error <> 0 THEN
         -- en caso de ocurrir un error en el reverso se marca el proceso como ERRONEO
         CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) RETURNING v_i_sql_error
      END IF
   END IF

   -- se invoca la funcion que realiza los reversos del proceso
   CALL fn_reversa_tbls_gen()

   DISPLAY " ACTUALIZA ESTADO REVERSADO"
   -- se invoca la funci�n que actualiza el registro de control a estatus 3-Reversado
   LET r_b_valida = fn_act_edo_archivo(p_v_arch_proceso, p_d_folio_rev,3,p_v_usuario)

   -- se verifica si fue posible actualiza el estado del archivo
   IF r_b_valida <> 0 THEN
      DISPLAY "ERROR EN ACTUALIZACI�N EDO ARCHIVO:"
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)
   END IF

   -- se invoca la funci�n que deja la operaci�n en estado Finalizado
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      DISPLAY "ERROR EN ACTUALIZACI�N OPERA FIN"
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_desplega_inc_operacion(r_b_valida)

      --EXIT PROGRAM
   END IF
   DISPLAY "=FIN="
END MAIN

#Objetivo: Funci�n que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen()
   DEFINE v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev   LIKE cat_operacion.opera_cod -- operacion a reversar

   -- se verifica la opci�n seleccionada para asigna el proceso y operacion correspondiente
   CASE p_c_op_arch_ent
      WHEN "opt1" -- Uso Garant�a
         DISPLAY " USO GARANT�A"
         LET v_i_proceso_cod_rev =  g_proc_cod_grt_uso_garantia -- recepci�n uso de garant�a 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt2" -- Rechazo de Saldos
         DISPLAY " RECHAZO DE SALDOS"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_rech_saldos -- recepci�n rechazo de saldos uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt3" -- Saldos Transferidos
         DISPLAY " SALDOS TRANSFERIDOS"
         LET v_i_proceso_cod_rev =  g_proc_cod_grt_uso_sdos_transf -- recepci�n saldos transferidos uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt4" -- Solicitudes Devueltas
         DISPLAY " SOLICITUDES DEVUELTAS"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_devol_solic -- recepci�n devol solicitudes uso 43bis
         LET v_i_opera_cod_rev = 1
      WHEN "opt5" -- Solicitudes no Atendidas
         DISPLAY " SOLICITUDES NO ATENDIDAS"
         LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_no_atendidas -- recepci�n solic no atendidas uso 43bis
         LET v_i_opera_cod_rev = 1
   END CASE

   -- se eliminan los procesos y las operaciones para el proceso_cod correspondiente
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
