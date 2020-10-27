--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:25/Ene/2016
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR46                                                        #
#Objetivo     => Lanzado reverso liquidación                                   #
#Fecha inicio => 25/Ene/2016                                                   #
################################################################################
GLOBALS "UNIG01.4gl"
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
       ,p_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera                      SMALLINT
       ,v_s_qry                        STRING
       ,v_r_reverso_id_derechohabiente DECIMAL(9,0)
       ,v_r_reverso_id_referencia      DECIMAL(9,0)
       ,v_r_reverso_marca              SMALLINT
       ,v_r_reverso_folio              DECIMAL(9,0)
       ,v_cadena                       STRING
       ,bn_reverso_desmarca            SMALLINT
       ,v_folio_lote                   LIKE dis_preliquida.folio_liquida
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR46.log")
         
   -- Se invoca rutina para reversar la liquidación.
   CALL fn_reverso_liquidacion(p_folio)   
      RETURNING r_bandera

   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera
           
      UPDATE glo_folio 
      SET    status = 1,
             folio_referencia = NULL
      WHERE  folio = p_folio;

      UPDATE uni_det_complementario
      SET    diagnostico        = 3,
             folio_liquidacion  = NULL,
             fecha_liquida      = NULL
      WHERE  folio_liquidacion = p_folio
      AND    estado             = 1
      AND    diagnostico        = 4;

      DELETE FROM cta_movimiento
      WHERE folio_liquida =  p_folio

   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF      

   LET p_titulo = "Finalización de proceso - REVERSO LIQUIDACION"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO LIQUIDACION","\n",
                   "\n",
                   "   Folio: "||p_folio,"\n",
                   "\n",
                   "   Fecha de inicio: "||TODAY,"\n",
                   "   Hora           : ",CURRENT HOUR TO SECOND,"\n"
END MAIN