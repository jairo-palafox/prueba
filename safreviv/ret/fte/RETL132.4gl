################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 19/06/2012                                     #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => RET                                                           #
#Programa     => RETL132                                                        #
#Objetivo     => Programa que ejecuta la liquidacion para retiros              #
#                trámite judicial                                              #
#Fecha inicio => 19/06/2012                                                    #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave usuario firmado
       p_tipo_ejecucion  SMALLINT, -- forma como ejecutara el programa
       p_nombre_menu     STRING, -- titulo de la ventana
       p_operacion       SMALLINT,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       --l_comando         STRING,
       v_folio_liquida   LIKE ret_preliquida.folio_liquida
       --p_programa_cod    VARCHAR(10)

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_nombre_menu    = ARG_VAL(3)

   --Obtiene las rutas de modulo a ejecutar
   SELECT ruta_bin,ruta_listados
     INTO v_ruta_ejecutable,v_ruta_listados
   FROM seg_modulo 
   WHERE modulo_cod = 'ret'
   --DISPLAY "RUTA EXE", v_ruta_ejecutable, "RUTA LST",v_ruta_listados
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_nombre_menu IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_nombre_menu)
   END IF
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETL132.log")

   LET g_proceso_cod = g_proceso_cod_ret_Ley73_arch       --1516 -- retiros ley 73   
   LET g_opera_cod   = g_opera_cod_ret_ley73_liquidacion  --4 -- liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

   -- se obtiene el folio del ultimo proceso ejecutado
   SELECT folio
   INTO   v_folio_liquida
   FROM   bat_ctr_operacion
   WHERE  opera_cod   = 4
     AND  pid = (SELECT MAX(pid)
                 FROM   bat_ctr_operacion
                 WHERE  proceso_cod = 1516)


   DISPLAY "Folio a notificar:  ",v_folio_liquida

   IF v_folio_liquida THEN
      -- se invoca el proceso que obtendra la informacion que se notificara
      CALL fn_lanza_proceso_notificacion(p_usuario_cod,v_folio_liquida)
   END IF

END MAIN


#Funcion que realiza el proceso de notificacion para el ultimo folio liquidado
FUNCTION fn_lanza_proceso_notificacion(p_usuario_cod,p_folio)

    DEFINE p_folio       LIKE glo_folio.folio
    DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod

    DEFINE v_s_comando       STRING
    DEFINE v_c_ruta_bin_ret  LIKE seg_modulo.ruta_bin -- ruta del bin de acr
    DEFINE v_c_ruta_list_bat LIKE seg_modulo.ruta_listados -- ruta listados de bat
    DEFINE v_ruta_vacia      STRING

    -- Se obtienen las rutas de los ejecutables
    CALL fn_rutas("ret") RETURNING v_c_ruta_bin_ret, v_ruta_vacia
    CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat

    LET v_s_comando = "nohup fglrun "
                    ,v_c_ruta_bin_ret CLIPPED
                    ,"/RETL416.42r "
                    ,p_usuario_cod, " "
                    ,g_notificacion_ley73_contigente," "
                    ,g_opera_notificacion_ley73_contigente," "
                    ,p_folio
                    ," 1>", v_c_ruta_list_bat CLIPPED
                    ,"/nohup:",g_notificacion_ley73_contigente USING "&&&&&",":"
                    ,g_opera_notificacion_ley73_contigente USING "&&&&&"
                    ," 2>&1 &"

    DISPLAY v_s_comando
    RUN v_s_comando

END FUNCTION