--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################
#Modulo            =>AGR                                                #
#Programa          =>AGRP25                                             #
#Objetivo          =>Programa que ejecuta el store procedure de         #
#                    de saldos remanentes del módulo de Anualidades G.  #
#Autor             =>Daniel Buendia, EFP                                #
#Fecha inicio      =>02 Julio 2012                                      #
#########################################################################

DATABASE safre_viv

MAIN
   DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid             LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio           LIKE glo_ctr_archivo.folio, -- numero de folio
          p_v_arch_proceso    VARCHAR(100), -- nombre del archivo
          v_i_tpo_originacion LIKE cre_acreditado.tpo_originacion, -- tipo de originación
          v_s_qryTxt          STRING, -- guarda una sentencia SQL a ejecutar
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no

   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario         = ARG_VAL(1)
   LET p_d_pid             = ARG_VAL(2)
   LET p_i_proceso_cod     = ARG_VAL(3)
   LET p_i_opera_cod       = ARG_VAL(4)
   LET p_d_folio           = ARG_VAL(5)
   LET p_v_arch_proceso    = ARG_VAL(6)

   -- se crea el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".AGRP25.log")

   DISPLAY "=INICIA AGRP25="
   DISPLAY " USUARIO       : ",p_v_usuario
   DISPLAY " PID           : ",p_d_pid
   DISPLAY " FOLIO         : ",p_d_folio USING "#########&"
   DISPLAY " ARCHIVO       : ",p_v_arch_proceso

   -- se inicializan variables
   LET v_i_tpo_originacion = 4 -- Anualidades Garantizadas

   DISPLAY " EJECUTA SALDOS REMANENTES: ",p_v_arch_proceso
   -- se invoca la funcion que ejecuta el procedimiento de saldos remanentes
   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:fn_agr_saldos_remanentes(?,?)"

   PREPARE prp_procd_saldos_reman FROM v_s_qryTxt
   EXECUTE prp_procd_saldos_reman USING p_v_usuario, v_i_tpo_originacion
                                   INTO r_b_valida

   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      DISPLAY " ERROR: Ocurrió un error en Saldos Remanentes: ",r_b_valida
      -- En caso de error se marca el proceso como Erroneo
      LET r_b_valida = fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod)

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

