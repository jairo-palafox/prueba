DATABASE safre_viv

GLOBALS "RETG01.4gl"
GLOBALS
  DEFINE g_proceso_cod            LIKE cat_proceso.proceso_cod  --codigo del proceso
  DEFINE g_opera_cod              LIKE cat_operacion.opera_cod  --codigo de operacion
END GLOBALS 


MAIN
  DEFINE v_usuario                CHAR(20)  --Almacena al usuario
  DEFINE v_tipo_proceso           SMALLINT  --Forma como ejecutara el programa
  DEFINE v_nom_prog               CHAR(40)  --Almacena opción del menú

  -- se asignan los parametros que vienen del fglrun
   LET v_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   LET g_proceso_cod  = g_proceso_archivos_respuesta_fico_ssv
   LET g_opera_cod    = g_opera_cod_valida_respuesta_fico_dap

  -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   CALL fn_transfiere_archivo(g_proceso_cod,g_opera_cod,v_usuario)
END MAIN

