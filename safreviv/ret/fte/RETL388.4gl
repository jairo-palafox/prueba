################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL388                                                      #
#Objetivo      => Programa lanzador para realizar la liquidación de los montos #
#                 obtenidos de la tabla ret_parcial_generico y ret_ctr_parcial #
#Fecha inicio  => Agosto 2015.                                                 #
#Requerimiento => 867                                                          #
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

    DEFINE v_rest_valida       SMALLINT,
           v_mensaje           STRING,
           r_resultado_opera   SMALLINT
    DEFINE v_folio             DECIMAL(9,0)
    DEFINE v_id_solicitud      DECIMAL(9,0)
    DEFINE v_num_pago          SMALLINT
    DEFINE v_num_parcialidades SMALLINT 

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETL387.log")

   LET g_proceso_cod = 1563 -- Retiro Parcial Generico
   LET g_opera_cod   = 2 -- Liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se obtiene el PID del proceso
   SELECT MAX(pid)
   INTO   g_pid
   FROM   bat_ctr_proceso
   WHERE  proceso_cod = g_proceso_cod

   -- Valida operacion para verificar si se puede continuar.
    CALL fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod)
                            RETURNING v_rest_valida
    
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
    AND    b.proceso_cod   = 1563
    AND    b.opera_cod     = 1
    AND    b.status        = 2

    DISPLAY "Folio:     ",v_folio

    DECLARE cur_solicitudes CURSOR FOR SELECT id_referencia FROM ret_preliquida
                                                     WHERE folio_liquida = v_folio

    FOREACH cur_solicitudes INTO v_id_solicitud

        DISPLAY "Solicitud: ",v_id_solicitud
    
        SELECT num_pago,num_parcialidades
        INTO   v_num_pago,v_num_parcialidades
        FROM   ret_ctr_parcial c, ret_parcial_generico p
		WHERE  c.id_solicitud         = p.id_solicitud   
        AND    c.id_solicitud         = v_id_solicitud
		AND    c.modalidad_retiro     = 11          
		AND    c.estado_solicitud     = 50          
		AND    c.cod_rechazo          = 0

        DISPLAY "Num  pago:  ",v_num_pago,". Num parcialidades: ",v_num_parcialidades

        UPDATE ret_ctr_parcial
		SET    estado_solicitud = 60     ,
		       f_liquida		= TODAY    
		WHERE  id_solicitud     = v_id_solicitud
		  AND  modalidad_retiro = 11
          AND  folio_liquida    = v_folio
		  AND  num_pago         = v_num_pago

        IF v_num_pago = v_num_parcialidades THEN

            UPDATE ret_parcial_generico
			SET    estado_solicitud = 60
			WHERE  id_solicitud     = v_id_solicitud
			  AND  modalidad_retiro = 11;
			-- se actualiza la tabla de control de retiro generico
			UPDATE ret_solicitud_generico
			SET    estado_solicitud = 60, -- preliquidada
			       folio            = v_folio       
			WHERE  id_solicitud     = v_id_solicitud;

        END IF
        
    END FOREACH

END MAIN