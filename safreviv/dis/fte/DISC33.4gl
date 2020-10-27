###############################################################################
#Version                    => 1.0.0                                          #
#Fecha ultima modificacion  => 31/03/2016                                     #
###############################################################################
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISC33                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACIÓN DE APORTACIONES SUBSECUENTES #
#                     SIN CONCILIAR                                           #
#Fecha Inicio      => 31/03/2016                                              #
###############################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                          LIKE bat_ctr_proceso.pid,     --id del proceso
    g_proceso_cod                  LIKE cat_proceso.proceso_cod, --código del proceso
    g_opera_cod                    LIKE cat_operacion.opera_cod  --código de operación
END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod                  LIKE seg_usuario.usuario_cod, --clave del usuario firmado
    p_tipo_ejecucion               SMALLINT, --forma como ejecutara el programa
    p_s_titulo                     STRING    --título de la ventana

  -- Se recupera la clave de usuario desde parametro 
  -- argumento con índice 1
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  -- Si se obtuvo el título, se pone como título de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  -- Validación que NO se tenga Preliquidación de Aportaciones Subsecuentes ejecutándose
  IF f_existe_proceso_operacion_ejecutando(933, 4) THEN
     MENU "No se puede ejecutar"
       ATTRIBUTES ( STYLE="dialog", COMMENT="Preliquidación de Aportaciones Subsecuentes Sin Conciliar ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF

  -- Se asignan las variables de control 
  LET g_proceso_cod = 933
  LET g_opera_cod   = 4

  -- Se invoca la funcion general de consulta de preliquidación
  -- es necesario cargar en cat_preliquida el proceso y el nombre de la tabla
  -- que contiene los datos de la preliquidación
  CALL fn_consulta_preliq(p_usuario_cod, g_proceso_cod, g_opera_cod)
END MAIN
