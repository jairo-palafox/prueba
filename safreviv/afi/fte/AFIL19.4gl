DATABASE safre_viv

GLOBALS

   DEFINE g_pid           DECIMAL(9,0)
   DEFINE g_proceso_cod   SMALLINT
   DEFINE g_opera_cod     SMALLINT

   DEFINE g_usuario_cod   CHAR(20)
   DEFINE g_tpo_ejecucion SMALLINT
   DEFINE g_nom_ventana   STRING

END GLOBALS

MAIN

   LET g_usuario_cod   = ARG_VAL(1)
   LET g_tpo_ejecucion = ARG_VAL(2)
   LET g_nom_ventana   = ARG_VAL(3)

   CALL fn_archivo_datos_contacto()

END MAIN

FUNCTION fn_archivo_datos_contacto()

   DEFINE r_vandera     SMALLINT

   LET g_pid         = 0
   LET g_proceso_cod = 1815
   LET g_opera_cod   = 1

   CALL ui.Interface.setName("Validar")
   CALL ui.Interface.setText("Validar")

   CALL fn_carga_archivo(g_pid,
                         g_proceso_cod,
                         g_opera_cod,
                         2,       -- indicador carga batch
                         "AFIL19",-- programa lanzador
                         "",      -- programa para validar
                         g_usuario_cod,--usuario
                         1)       -- indicador genera pid

   RETURNING r_vandera
END FUNCTION
