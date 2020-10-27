###############################################################################
#Version                    => 1.0.0                                          #
#Fecha ultima modificacion  => 21/03/2018                                     #
###############################################################################
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => DIS                                                     #
#Programa          => DISC03                                                  #
#Objetivo          => CONSULTA DE    LIQUIDACION DE DISPERSION                #
#Fecha Inicio      =>                                                         #
###############################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose

DATABASE safre_viv
GLOBALS
DEFINE g_pid                 LIKE bat_ctr_proceso.pid,     --ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod, --codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod, --codigo de operacion
       g_sql_txt             STRING,
       v_proc_entra          SMALLINT,
       v_proc_val            SMALLINT,
       v_cod_conv            SMALLINT,
       v_desc_proc_val       CHAR(40),
       v_mensaje_val         STRING
END GLOBALS

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, --clave del usuario firmado
       p_tipo_ejecucion      SMALLINT, --forma como ejecutara el programa
       p_s_titulo            STRING,   --titulo de la ventana
       p_operacion           SMALLINT  --operacion 1: ejecuta, 2: consulta

  -- se recupera la clave de usuario desde parametro 
  -- argumento con indice 1
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  -- si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  LET g_proceso_cod = 901 -- dispersion
  LET g_opera_cod   = 2   -- liquidacion
  LET p_operacion   = 1   -- consultar la liquidacion

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

  -- se invoca la funcion general de consulta de liquidacion
  CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
  
END MAIN