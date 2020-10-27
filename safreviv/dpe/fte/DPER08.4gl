--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/11/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER08                                                        #
#Objetivo     => Programa lanzado de preliquidación devolución pagos en exceso #
#Fecha inicio => Noviembre 28, 2016.                                           #
################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_tipo_ejecucion    SMALLINT -- forma como ejecutara el programa
       ,p_s_titulo          STRING -- titulo de la ventana
       ,p_operacion         SMALLINT
       ,p_d_folio           LIKE glo_folio.folio
       ,v_folio_integracion LIKE glo_folio.folio
       ,p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera           SMALLINT
       ,v_s_qry             STRING
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPEL08.log")

   -- Se invoca rutina para reversar la preliquidación.
   CALL fn_reversa_preliquidacion(p_d_folio, g_proceso_cod, g_opera_cod)
        RETURNING r_bandera
      
   IF(r_bandera = 0)THEN
      DISPLAY ""
      DISPLAY "   El reverso se realizó con éxito"
      DISPLAY ""
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
          RETURNING r_bandera

     SELECT folio_referencia 
     INTO   v_folio_integracion
     FROM   glo_folio 
     WHERE  folio = p_d_folio

     UPDATE glo_ctr_archivo
     SET    folio = v_folio_integracion
     WHERE  folio = p_d_folio
          
     DELETE FROM glo_folio
     WHERE folio = p_d_folio
     AND   proceso_cod = g_proceso_cod
     AND   opera_cod = 5

     --  Se reversan los diagnosticos
     UPDATE safre_viv:dpe_sol_trabajador
     SET    diagnostico = 2,
            diag_procesa = NULL,
            folio_liquida = NULL
     WHERE  folio  = v_folio_integracion
     --AND    diagnostico = 4

      -- Retaura el status de folio para volver a usarlo
   IF(r_bandera = 0 )THEN
      DISPLAY "Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO PRELIQUIDACION"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO PRELIQUIDACION","\n",
                   "   Folio: "||p_d_folio,"\n",
                   "   Fecha de inicio: "||TODAY,"\n",
                   "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,"",p_titulo,p_mensaje)
END MAIN
