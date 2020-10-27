################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>  20/03/2018                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISL04                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                liquidacion para dispersion                                   #
#Fecha inicio => Enero 30, 2012                                                #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid                 LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod         LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod           LIKE cat_operacion.opera_cod -- codigo de operacion

DEFINE 
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING

END GLOBALS

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave usuario firmado
       p_tipo_ejecucion      SMALLINT, -- forma como ejecutara el programa
       p_s_titulo            STRING, -- titulo de la ventana
       p_operacion           SMALLINT,
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_ruta_listados       LIKE seg_modulo.ruta_listados,
       l_comando             STRING,
       v_folio_liquida       LIKE dis_preliquida.folio_liquida
       --p_programa_cod        VARCHAR(10)

  -- se recupera la clave de usuario desde parametro 
  -- argumento con indice 1
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  --Obtiene las rutas de modulo a ejecutar
  SELECT ruta_bin,ruta_listados
  INTO   v_ruta_ejecutable,v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  -- si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  CALL STARTLOG (p_usuario_cod CLIPPED|| ".DISL04.log")

  LET g_proceso_cod = 901 -- dispersion
  LET g_opera_cod   = 2 -- liquidacion
  LET p_operacion   = 2 -- ejecutar liquidacion

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

  -- se invoca la funcion para enviar la liquidacion
  CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)


  {--Obtenemos el ultimo folio de preliquidación
  SELECT folio_liquida INTO v_folio_liquida
  FROM dis_preliquida
  GROUP BY folio_liquida

  DISPLAY "folio liquida -- ",v_folio_liquida}

  {-- Generar el archivo o interface a Hipotecaria Social Para Amortización de
  -- Crédito y/o Cargo a Capital)
  LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS01.42r ",v_folio_liquida
  DISPLAY "v_comando = \n",l_comando
  RUN l_comando}

  --Obtiene Max Pid
  --CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING g_pid
   
  {--Generar el archivo o interface de Pago REAL HS
  LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS01.42r ",
                  v_folio_liquida,
                  " 1>>",v_ruta_listados CLIPPED,"/nohup:",
                  g_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod USING "&&&&&" ," 2>&1 &"
  RUN l_comando

  --Generar el archivo o interface a las Entidades Financieras y/o Servicios
  LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS03.42r ",
                  v_folio_liquida,
                  " 1>>",v_ruta_listados CLIPPED,"/nohup:",
                  g_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod USING "&&&&&" ," 2>&1 &"
  RUN l_comando

  --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
  LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS04.42r ",
                  v_folio_liquida,
                  " 1>>",v_ruta_listados CLIPPED,"/nohup:",
                  g_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod USING "&&&&&" ," 2>&1 &"
  RUN l_comando

  --Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
  LET l_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/DISS05.42r ",
                  v_folio_liquida,
                  " 1>>",v_ruta_listados CLIPPED,"/nohup:",
                  g_pid USING "&&&&&",":",
                  g_proceso_cod USING "&&&&&",":",
                  g_opera_cod USING "&&&&&" ," 2>&1 &"
  RUN l_comando}

  {-- Generar el archivo o interface a las Entidades Financieras y/o Servicios
  LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS03.42r ",v_folio_liquida
  DISPLAY "v_comando = \n",l_comando
  RUN l_comando

  -- Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
  LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS04.42r ",v_folio_liquida
  DISPLAY "v_comando = \n",l_comando
  RUN l_comando

  -- Generar el archivo o interface a las diferencias negativas por avance (Abono para HS)
  LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISS05.42r ",v_folio_liquida
  DISPLAY "v_comando = \n",l_comando
  RUN l_comando}
   
END MAIN