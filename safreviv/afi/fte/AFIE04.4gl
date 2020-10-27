#########################################################################################
#Modulo       => AFI                                                                    #
#Programa     => AFIE04                                                                 #
#Objetivo     => Invoca la función para la carga de archivos desde un equipo local      #
#                de la operación 75 de afiliación                                       #
#Fecha inicio => 10 de noviembre de 2014                                                #
#########################################################################################

DATABASE safre_viv

GLOBALS "AFIG01.4gl"

GLOBALS

   DEFINE g_pid                    LIKE bat_ctr_proceso.pid      -- ID del proceso
   DEFINE g_proceso_cod            LIKE cat_proceso.proceso_cod  -- código del proceso
   DEFINE g_opera_cod              LIKE cat_operacion.opera_cod  -- código de operaciÓn

END GLOBALS

MAIN

   DEFINE v_usuario                VARCHAR(30)                    -- Almacena al usuario
   DEFINE v_tipo_proceso           SMALLINT                       -- Forma como ejecutara el programa
   DEFINE v_nom_prog               VARCHAR(30)                    -- Almacena opción del menú
   DEFINE v_ruta_ejecutable        LIKE seg_modulo.ruta_bin       -- Ruta del ejecutable
   DEFINE v_ruta_listados          LIKE seg_modulo.ruta_listados  -- Ruta del log
   DEFINE r_bnd_carga              SMALLINT                       -- Bandera de carga de archivo

   -- se asignan los parámetros que vienen del fglrun
   LET v_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   LET g_proceso_cod  = g_proceso_cod_afi_movimientos_opt75
   LET g_opera_cod    = 1

  -- se asigna el título del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   CALL STARTLOG(v_usuario CLIPPED|| ".AFIE04.log")
   CALL fn_transfiere_archivo(g_proceso_cod,g_opera_cod,"SAFREVIV")

END MAIN


