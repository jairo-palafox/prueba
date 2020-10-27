--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTR05                                        #
#Objetivo          =>Programa que ejecuta el reverso de archivos   #
#                    de salida (Solicitud de saldos) para el       #
#                    módulo de Uso de Garantía 43 bis              #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>22 Mayo 2012                                  #
####################################################################

DATABASE safre_viv
GLOBALS "GRTG01.4gl"

MAIN
   DEFINE p_v_usuario          LIKE seg_usuario.usuario, -- usuario firmado al sistema
          p_d_pid              DECIMAL(9,0), -- identificador del proceso
          p_i_proceso_cod      LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          p_i_opera_cod        LIKE cat_operacion.opera_cod, -- operación que llama la funcion 
          p_v_nom_archivo      LIKE bat_ctr_operacion.nom_archivo, -- nombre del archivo
          p_d_folio            LIKE bat_ctr_operacion.folio, -- folio
          v_s_qryTxt           STRING, -- contiene una sentencia sql a ejecutar
          p_id_cre_ctr_archivo LIKE cre_ctr_archivo.id_cre_ctr_archivo, -- identificador del archivo
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
          
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario          = ARG_VAL(1)
   LET p_d_pid              = ARG_VAL(2)
   LET p_i_proceso_cod      = ARG_VAL(3)
   LET p_i_opera_cod        = ARG_VAL(4)
   LET p_d_folio            = ARG_VAL(5)
   LET p_v_nom_archivo      = ARG_VAL(6)
   LET p_id_cre_ctr_archivo = ARG_VAL(7)
   
   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".GRTR05.log")

   DISPLAY "=INICIA GRTR05="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_nom_archivo

   DISPLAY " EJECUTA REVERSO SOLICITUD DE SALDOS"
   LET v_s_qryTxt = "EXECUTE FUNCTION safre_viv:fn_uso_reversa_arch_salida(?)"
   PREPARE prp_reversa_arch_sal FROM v_s_qryTxt
   EXECUTE prp_reversa_arch_sal USING p_id_cre_ctr_archivo
                                 INTO r_b_valida

   -- valida el codigo de regreso de la función del reverso
   IF r_b_valida <> 0 THEN
      DISPLAY " OCURRIÓ UN ERROR EN EL PROCESO DE REVERSO: ",r_b_valida

      -- se ejecuta la función que actualiza el proceso como erroneo
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF

      EXIT PROGRAM
   END IF

   -- se invoca la funcion que realiza los reversos del proceso
   CALL fn_reversa_tbls_gen()

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

#Objetivo: Función que actualiza el proceso a reversado
FUNCTION fn_reversa_tbls_gen()
   DEFINE v_d_pid_rev         LIKE bat_ctr_proceso.pid, -- PID
          v_i_proceso_cod_rev LIKE cat_proceso.proceso_cod, -- proceso a reversar
          v_i_opera_cod_rev   LIKE cat_operacion.opera_cod -- operacion a reversar

   -- se asigna el proceso y operacion del proceso a reversar
   LET v_i_proceso_cod_rev = g_proc_cod_grt_uso_arch_solic -- generación archivo solic sdos uso 43bis
   LET v_i_opera_cod_rev = 1

   -- se obtiene el del proceso y operación a reversar
   LET v_d_pid_rev = fn_max_pid(v_i_proceso_cod_rev, v_i_opera_cod_rev)

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
      AND opera_cod = v_i_opera_cod_rev
END FUNCTION