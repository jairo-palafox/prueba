################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  =>  19/02/2016                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISL55                                                        #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la       #
#                liquidacion para aportaciones subsecuentes sin Conciliar.     #
#Fecha inicio => 19/02/2016                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                          LIKE bat_ctr_proceso.pid,     -- id del proceso
    g_proceso_cod                  LIKE cat_proceso.proceso_cod, -- codigo del proceso
    g_opera_cod                    LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod                  LIKE seg_usuario.usuario_cod, -- clave usuario firmado
    p_tipo_ejecucion               SMALLINT, -- forma como ejecutara el programa
    p_s_titulo                     STRING,   -- titulo de la ventana
    p_operacion                    SMALLINT,
    v_ruta_ejecutable              LIKE seg_modulo.ruta_bin,
    v_ruta_listados                LIKE seg_modulo.ruta_listados

  --Se recupera la clave de usuario desde parametro 
  --argumento con indice 1
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  --Obtiene las rutas de modulo a ejecutar
  SELECT ruta_bin,ruta_listados
  INTO   v_ruta_ejecutable,v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'dis'

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  CALL STARTLOG (p_usuario_cod CLIPPED|| ".DISL55.log")

  LET g_proceso_cod = 933 -- dispersion
  LET g_opera_cod   = 5 -- liquidacion
  LET p_operacion   = 2 -- ejecutar liquidacion

  --Se invoca la funcion para enviar la liquidacion
  CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

  UPDATE dis_as_sin_conciliar
     SET ind_concilia = 4
   WHERE ind_concilia = 3; 
  
END MAIN
