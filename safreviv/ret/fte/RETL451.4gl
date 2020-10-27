--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETL451                                                                #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                de la carga de las Excepciones de la Devolición del SSV                #
#Fecha inicio => Septiembre 14, 2017                                                    #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion     SMALLINT, -- forma como ejecutara el programa
       p_s_titulo           STRING, -- titulo de la ventana
       p_operacion          SMALLINT,
       v_id_derechohabiente DECIMAL(9,0),
       v_id_solicitud       DECIMAL(9,0),
       v_query              STRING,
       v_marca_excep_ssv    SMALLINT,
       v_estado_marca       SMALLINT,
       v_marca_causa        SMALLINT,
       v_i_estado_marca     SMALLINT
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETL451.log")

   LET g_proceso_cod = g_proceso_excep_devol_ssv -- Exepciones de la Devolución del SSV
   LET g_opera_cod   = g_opera_excep_devol_ssv_liquida -- liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion
   LET v_estado_marca = 0
   LET v_marca_causa = 0
   LET v_marca_excep_ssv = 820

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

   DISPLAY "regresa de la liquidacion"

   --- Se solicita que la desmarca se haga en la confirmación del pago RETP448   
   -- Desmarca las cuentas liquidadas
   LET v_query = " SELECT a.id_derechohabiente, b.id_solicitud ",
                 " FROM   afi_derechohabiente a, ret_excep_devol_ssv b ",
                 " WHERE  a.nss = b.nss ",
                 " AND    b.estado_solicitud = 50 "

   PREPARE prp_solicitudes FROM v_query
   DECLARE cur_solicitudes CURSOR FOR prp_solicitudes

   FOREACH cur_solicitudes INTO v_id_derechohabiente, v_id_solicitud

--      LET v_query = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,?,?,?,?)"
--      PREPARE prp_desmarca FROM v_query
--      EXECUTE prp_desmarca USING 
--                    v_id_derechohabiente
--                   ,v_marca_excep_ssv
--                   ,v_id_solicitud
--                   ,v_estado_marca
--                   ,v_marca_causa
--                   ,p_usuario_cod
--                   ,g_proceso_cod
--               INTO v_i_estado_marca;
               
      -- Actualiza las solicitudes liquidadas

      UPDATE ret_excep_devol_ssv
      SET    estado_solicitud = 60
      WHERE  id_solicitud     = v_id_solicitud
      AND    estado_solicitud = 50;
      
   END FOREACH   
 
END MAIN