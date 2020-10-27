################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR36                                                        #
#Objetivo     => Lanzado reverso generar archivo salida unificación recurrente #
#Fecha inicio => Septiembre 02, 2014                                           #
################################################################################
--Lanzador: UNIL58

--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15/01/2014
-- Fecha      -- Modificación
-- 12-11-2014 Se corrige sección de comentarios.
--==============================================================================

GLOBALS "UNIG01.4gl"
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
       ,p_i_folio           LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera           SMALLINT
       ,v_s_qry             STRING
       ,v_cadena            STRING
       ,bn_reverso_desmarca SMALLINT
       ,v_folio             LIKE dis_preliquida.folio_liquida
       ,p_titulo            STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje           STRING -- cuerpo del mensaje enviado

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)

   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR36.log")

   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) 
   RETURNING r_bandera

   IF r_bandera = 0 THEN
      -- Reversa operación
      LET r_bandera = 0
      
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera         

      --  Se reversan los diagnosticos
      UPDATE uni_det_unificador
      SET    diagnostico = 5
      WHERE  folio_liquidacion = p_i_folio
      AND    diagnostico = 6

      --Se recuepera el folio de unificación      
      LET v_s_qry = "SELECT folio_referencia", 
                    "\n  FROM glo_folio",
                    "\n WHERE proceso_cod = ",g_proceso_cod,
                    "\n AND folio = ",p_i_folio

      PREPARE prp_folio_unificacion FROM v_s_qry
      EXECUTE prp_folio_unificacion INTO v_folio                               
         
      UPDATE  uni_det_unificado
      SET     diagnostico = 5
      WHERE   folio_unificacion = v_folio
      AND     diagnostico = 6   

      IF(r_bandera = 0)THEN      
         DISPLAY "Operación lista para volver a generarse."      
      ELSE
         -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
   ELSE
      DISPLAY fn_recupera_inconsis_opera(r_bandera)	
   END IF

   LET p_titulo = "Finalización de proceso - REVERSO GENERAR ARCHIVO SALIDA"

   LET p_mensaje = "Finalización de proceso - REVERSO GENERAR ARCHIVO SALIDA","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"
  
   CALL fn_correo_proceso(g_pid,
                          g_proceso_cod,
                          g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
END MAIN