#####################################################################
#Modulo            => AGR                                           #
#Programa          => AGRP35                                        #
#Objetivo          => (Homologación) Lanzado  para la ejecución de  #
#                      cancelacion y desmarca por vencimiento       #
#Autor             => Hector Fabián Jiménez Lara                    #
#Fecha inicio      => 15 de Julio de 2015                           #
#####################################################################
DATABASE safre_viv

GLOBALS
   DEFINE g_proceso_cod            INTEGER
   DEFINE g_opera_cod              INTEGER
   DEFINE p_pid                    DECIMAL(9,0)
   DEFINE v_s_comando              STRING
END GLOBALS

MAIN
   DEFINE p_usuario_cod           LIKE seg_usuario.usuario_cod
   DEFINE v_si_error              SMALLINT  -- variable para error en la función (0=no hay error, 1=Error)
   DEFINE v_cod_error             INTEGER
   DEFINE v_estado                SMALLINT

   -- se asignan los valores que vienen como parametros
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET g_proceso_cod   = ARG_VAL(3)
   LET g_opera_cod     = ARG_VAL(4)

   DISPLAY "= Inicia AGRP35 ="
   DISPLAY " Usuario : ",p_usuario_cod
   DISPLAY " Proceso : ",g_proceso_cod
   DISPLAY " Pid     : ",p_pid

   LET v_s_comando = "EXECUTE FUNCTION fn_act_cre_tramite()"

   PREPARE prp_exe_fn FROM v_s_comando
   EXECUTE prp_exe_fn INTO v_si_error,
                           v_cod_error

   -- En caso de haber existido un error se despliega el código
   IF v_si_error <> 0 THEN
      DISPLAY "\n Ocurrió un error al ejecutar la función : ",v_cod_error

      CALL fn_error_opera( p_pid,
                           g_proceso_cod,
                           g_opera_cod )
                           RETURNING v_estado
   ELSE
      CALL fn_actualiza_opera_fin( p_pid,
                                   g_proceso_cod,
                                   g_opera_cod)
                                   RETURNING v_estado
   END IF

   DISPLAY "= FIN ="
END MAIN
