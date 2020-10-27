--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27/05/2015
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER21                                                        #
#Objetivo     => Programa lanzador para reversar la preliquidación del proceso #
#Fecha inicio => 30/08/2012                                                    #
################################################################################

GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,v_s_qry          STRING
       ,p_titulo         STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje        STRING -- cuerpo del mensaje enviado

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER21.log")

   LET g_proceso_cod = g_proceso_cod_dpe_credito  -- DPE Créditos
   LET g_opera_cod   = g_opera_cod_dpe_preliquidacion -- preliquidacion

   -- Se invoca rutina para reversar la preliquidación.
   CALL fn_reversa_preliquidacion(p_d_folio, g_proceso_cod, g_opera_cod)
      RETURNING r_bandera
   IF(r_bandera = 0)THEN
      DISPLAY " El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   
      LET v_s_qry = 
           "UPDATE glo_folio "
           ,"\n   SET status = 0"
           ,"\n WHERE proceso_cod = ?"
           ,"\n   AND status = 1"
           ,"\n   AND folio = ",p_d_folio
      PREPARE Prpr_ActuGloFolio FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActuGloFolio USING g_proceso_cod
     
      LET v_s_qry = 
           "UPDATE glo_ctr_archivo"
           ,"\n   SET estado = 2 "-- Integrado
           ,"\n WHERE proceso_cod = ?"
           ,"\n   AND folio = ",p_d_folio
           ,"\n   AND estado = 3" -- integrado
       PREPARE Prpr_ActuGloArchivo FROM v_s_qry CLIPPED
       EXECUTE Prpr_ActuGloArchivo USING g_proceso_cod
       
       -- Actualiza el estado de la solicitud de preliquidado a integrado
      UPDATE dpe_sol_creditos
      SET    estado_solicitud = 1
      WHERE  folio = p_d_folio
      AND    estado_solicitud = 3;

      UPDATE bat_ctr_operacion 
      SET    folio = NULL
      WHERE  pid = g_pid
      AND    opera_cod = g_opera_cod;
         
      
      -- Retaura el status de folio para volver a usarlo
   IF(r_bandera = 0 )THEN
      DISPLAY " Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = " Finalización de proceso - REVERSO PRELIQUIDACION"
   
   LET p_mensaje = " Finalización de proceso - REVERSO PRELIQUIDACION","\n",
                   "\n",
                   " Folio reversado: "||p_d_folio,"\n",
                   "\n",
                   " Fecha de inicio: "||TODAY,"\n",
                   " Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN
