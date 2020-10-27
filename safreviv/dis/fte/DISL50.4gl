################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>  18/11/2018                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISL50                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                liquidacion para aportaciones subsecuentes (Sin Adelanto)     #
#Fecha inicio => 18/11/2015                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --Id del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operación
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE
    p_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave usuario firmado
    p_tipo_ejecucion         SMALLINT, --Forma como ejecutara el programa
    p_s_titulo               STRING,   --Título de la ventana
    p_operacion              SMALLINT,
    v_ruta_ejecutable        LIKE seg_modulo.ruta_bin,
    v_ruta_listados          LIKE seg_modulo.ruta_listados,
    l_comando                STRING,
    v_folio_liquida          LIKE dis_preliquida.folio_liquida

  --Se recupera la clave de usuario desde parametro 
  --argumento con indice 1
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET g_proceso_cod    = 932 --Dispersión
  LET g_opera_cod      = 4   --Liquidación
  LET p_operacion      = 4   --Ejecutar liquidación

  --Obtiene las rutas de modulo a ejecutar
  SELECT ruta_bin, ruta_listados
  INTO   v_ruta_ejecutable, v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  CALL STARTLOG (p_usuario_cod CLIPPED|| ".DISL50.log")

  --Se invoca la funcion para enviar la liquidacion
  CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

END MAIN