################################################################################
#Proyecto         => SAFRE VIVIENDA                                            #
#Propietario      => E.F.P.                                                    #
#Modulo           => DAE                                                       #
#Programa         => DAER13                                                    #
#Objetivo         => Lanzado del reverso de Preliquidación Ajuste Ind DAE      #
#Fecha inicio     => 18/Abr/2016                                               #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--==============================================================================
DATABASE safre_viv

GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo          STRING -- titulo de la ventana
       ,p_folio_liquida           LIKE glo_folio.folio
       ,v_folio_integracion LIKE glo_folio.folio
       ,p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera           SMALLINT
       ,v_s_qry             STRING
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado
       ,v_folio_lote        DECIMAL(9,0)
       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio_liquida  = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DAER13.log")

   LET g_proceso_cod = 2406 -- DPE
   LET g_opera_cod   = 3    -- preliquidacion

   -- Se invoca rutina para reversar la preliquidación.
   --Solamente borra de dae_preliquida
   CALL fn_reversa_preliquidacion(p_folio_liquida, g_proceso_cod, g_opera_cod)
   RETURNING r_bandera
      
   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"

      --Actualiza estado a reversado en bat_ctr_proceso y operacion
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera

      --Recuperar folio de integración
      SELECT folio_referencia
      INTO   v_folio_lote 
      FROM   glo_folio 
      WHERE  folio = p_folio_liquida
      AND    proceso_cod = g_proceso_cod;

      --Actualiza status_retiro
      UPDATE dae_det_solicitud
      SET    status_retiro = 1, 
             folio_ajuste = NULL 
      WHERE  folio_ajuste = v_folio_lote;
      -- Se actualiza el folio referencia
      UPDATE glo_folio                   
      SET    folio_referencia = NULL
      WHERE  folio_referencia = p_folio_liquida
      AND    proceso_cod = g_proceso_cod;

      --Borra el folio de liquidación   
      DELETE FROM glo_folio WHERE folio = p_folio_liquida
      
      DISPLAY "Folio de liquidación reversado : ", p_folio_liquida 

      DISPLAY "Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   LET p_titulo = "Finalización de la operación de Preliquidación"

   LET p_mensaje = "   Finalización de la operación de Preliquidación","\n",
                   "   Folio: "||p_folio_liquida,"\n",
                   "   Fecha de inicio: "||TODAY,"\n",
                   "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)   
END MAIN