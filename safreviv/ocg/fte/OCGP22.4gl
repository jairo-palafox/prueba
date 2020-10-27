#####################################################################
#Modulo            => OCG                                           #
#Programa          => OCGP22                                        #
#Objetivo          => Lanzado para la ejecución de desmarca por     #
#                     crédito en trámite vencido                    #
#Autor             => Mauro Muñiz Caballero                         #
#Fecha inicio      => 3 de febrero de 2017                          #
#####################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_proceso_cod             INTEGER
   DEFINE p_opera_cod               INTEGER
   DEFINE p_pid                     DECIMAL(9,0)
   DEFINE v_s_comando               STRING
   DEFINE v_total                   INTEGER

END GLOBALS

MAIN

   DEFINE p_usuario_cod             CHAR(20)
   DEFINE v_si_error                SMALLINT  -- variable para error en la función (0=no hay error, 1=Error)
   DEFINE v_cod_error               INTEGER
   DEFINE v_estado                  SMALLINT

   -- se asignan los valores que vienen como parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)

   IF p_pid IS NULL THEN
      LET p_pid = 0
   END IF

   IF p_proceso_cod IS NULL THEN
    LET p_proceso_cod = 3921
   END IF

   IF p_opera_cod IS NULL THEN
      LET p_opera_cod = 1
   END IF

   DISPLAY "= Inicia OCGP22 ="
   DISPLAY " Usuario : ",p_usuario_cod
   DISPLAY " Proceso : ",p_proceso_cod
   DISPLAY " Pid     : ",p_pid

   DISPLAY ""
   DISPLAY " Inicio proceso de vencimiento por fecha de vigencia"

   LET v_total = 0

   LET v_s_comando = "EXECUTE FUNCTION fn_act_ocg_tramite()"

   PREPARE prp_exe_fn FROM v_s_comando
   EXECUTE prp_exe_fn INTO v_si_error,
                           v_cod_error,
                           v_total

   DISPLAY ""
   DISPLAY " Total regitros desmarcados: ",v_total
   DISPLAY " Fin de proceso de vencimiento por fecha de vigencia"

   -- En caso de haber existido un error se despliega el código
   IF v_si_error <> 0 THEN
      DISPLAY "Ocurrió un error al ejecutar la función : ",v_cod_error

      CALL fn_error_opera( p_pid,
                           p_proceso_cod,
                           p_opera_cod )
                           RETURNING v_estado
   ELSE
      CALL fn_actualiza_opera_fin( p_pid,
                                   p_proceso_cod,
                                   p_opera_cod)
                                   RETURNING v_estado
   END IF

   DISPLAY "= FIN ="

END MAIN
