################################################################################
# Version: 1.0.0                                                               #
# Fecha ultima modificacion: 15 JULIO 2013                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETM01                                                   #
#Objetivo          => Programa lanzador para validar archivos                  #
#Fecha inicio      => 15 JULIO 2013                                            #
################################################################################
DATABASE safre_viv

GLOBALS "RETM01.inc"

PRIVATE DEFINE g_pid             LIKE bat_ctr_proceso.pid      -- ID del proceso
PRIVATE DEFINE g_proceso_cod     LIKE cat_proceso.proceso_cod  -- codigo del proceso
PRIVATE DEFINE g_opera_cod       LIKE cat_operacion.opera_cod  -- codigo de operacion

MAIN
   DEFINE p_usuario              VARCHAR(30)                   -- Almacena al usuario
   DEFINE p_tipo_ejecucion       SMALLINT                      -- Forma como ejecutara el programa 
   DEFINE p_titulo               VARCHAR(30)                   -- Almacena opción del menú
   DEFINE v_result_carga         SMALLINT                      -- Bandera de carga de archivo


   -- se asignan los parametros que vienen del fglrun
   LET p_usuario = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   LET g_proceso_cod = PROC_CUENTAS_CLABE
   LET g_opera_cod = OP_VALIDA_ARCHIVO

   -- Valida operacion para verificar si se puede continuar.
   IF (fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
      CALL fn_carga_archivo(  g_pid,g_proceso_cod,
                              g_opera_cod,
                              2,
                              "RETM01",
                              "",
                              p_usuario,
                              TRUE)
                              RETURNING v_result_carga

      IF v_result_carga = FALSE THEN
         CALL fn_mensaje("Atención","Carga Cancelada","about")
      END IF
      --END IF
   ELSE
     CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod,
                                                       g_opera_cod))
   END IF

END MAIN