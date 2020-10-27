################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL385                                                      #
#Objetivo      => Programa que realiza la liquidación del archivo SIAFF de     #
#                 cargos.                                                      #
#Fecha inicio  => Agosto 2015.                                                 #
#Requerimiento => 878                                                          #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       p_operacion      SMALLINT

    DEFINE v_folio              DECIMAL(9,0)
    DEFINE v_id_solicitud       DECIMAL(9,0)
    DEFINE v_id_derechohabiente DECIMAL(9,0)
    DEFINE v_nss                CHAR(11)

    DEFINE v_rest_valida    SMALLINT,
           v_mensaje        STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETL385.log")

   LET g_proceso_cod = 1569
   LET g_opera_cod   = 4
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- Valida operacion para verificar si se puede continuar.
    CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
    DISPLAY ""
    DISPLAY ""
    DISPLAY v_rest_valida
    DISPLAY ""
    
    IF ( v_rest_valida != 0 ) THEN

      CALL fn_recupera_inconsis_opera(v_rest_valida) RETURNING v_mensaje

      CALL fn_mensaje("Atención", v_mensaje, "stop")
      MENU
         COMMAND "Cerrar"
            EXIT MENU
      END MENU
      RETURN
    END IF

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)

   SELECT UNIQUE(max(folio_liquida))
    INTO   v_folio
    FROM   ret_preliquida  a , glo_folio b
    WHERE  a.folio_liquida = b.folio
    AND    b.proceso_cod   = 1569
    AND    b.opera_cod     = 3
    AND    b.status        = 2

    DISPLAY "Folio:     ",v_folio

    DECLARE cur_solicitudes CURSOR FOR SELECT id_referencia,id_derechohabiente FROM ret_preliquida
                                                     WHERE folio_liquida = v_folio

    FOREACH cur_solicitudes INTO v_id_solicitud,v_id_derechohabiente

        SELECT nss
        INTO   v_nss
		FROM   afi_derechohabiente
		WHERE  id_derechohabiente = v_id_derechohabiente;
    
        UPDATE ret_his_respuesta_siaff
		SET    estado_solicitud = 60 
		WHERE  id_solicitud     = v_id_solicitud
        AND    nss              = v_nss
        
    END FOREACH

END MAIN