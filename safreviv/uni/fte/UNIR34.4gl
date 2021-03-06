--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 02/09/2014
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR34                                                        #
#Objetivo     => Lanzado reverso liquidación unificación recurrente            #
#Fecha inicio => Septiembre 02, 2014                                           #
################################################################################
--Lanzador: UNIL56

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
       ,p_i_folio        LIKE dis_preliquida.folio_liquida
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
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
   
   -- Bandera para comprobar que se ejecute correctamente el reverso de la desmarca
   LET bn_reverso_desmarca = 0

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR34.log")

   -- se asigna proceso y operacion
   LET g_proceso_cod = 2314 -- Unificación Recurrente
   LET g_opera_cod   = 4    -- Liquidación
   

   -- Se invoca rutina para reversar la liquidación.
   CALL fn_reverso_liquidacion(p_i_folio)
      RETURNING r_bandera
   IF(r_bandera = 0)THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
      
   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bandera
   
   IF r_bandera = 0 THEN
   -- Reversa operación
      LET r_bandera = 0
        CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bandera
         LET v_s_qry = 
             "UPDATE glo_folio" 
             ,"\n   SET status = 1"
             ,"\n WHERE proceso_cod = ",g_proceso_cod
             ,"\n   AND status = 2"
             ,"\n   AND folio = ",p_i_folio
      
         --DISPLAY "UPDATE glo_folio: ", v_s_qry
         PREPARE Prpr_ActGlofolio FROM v_s_qry CLIPPED
         EXECUTE Prpr_ActGlofolio 
      
         LET v_s_qry = 
         "UPDATE glo_ctr_archivo" 
         ,"\n   SET estado = 1,"
         ,"\n       opera_cod = 3"
         ,"\n WHERE proceso_cod = ",g_proceso_cod
         ,"\n   AND estado = 2"
         ,"\n   AND opera_cod = 4"
         ,"\n   AND folio = ",p_i_folio
      
         --DISPLAY "UPDATE glo_ctr_archivo: ", v_s_qry
         PREPARE Prpr_ActGgloCtrArchivo FROM v_s_qry CLIPPED
         EXECUTE Prpr_ActGgloCtrArchivo
         
         --  Se reversan los diagnosticos
         UPDATE uni_det_unificador
            SET diagnostico = 3,
                f_liquidacion = NULL,
                folio_liquidacion = NULL
          WHERE folio_liquidacion = p_i_folio
            AND diagnostico = 4

         LET v_s_qry = "SELECT folio_referencia", 
                          "\n  FROM glo_folio",
                          "\n WHERE proceso_cod = ",g_proceso_cod,
                          "\n AND status = 1",
                          "\n AND folio = ", p_i_folio

         PREPARE prp_folio_unificacion FROM v_s_qry
         EXECUTE prp_folio_unificacion INTO v_folio_lote
         
         UPDATE uni_det_unificado
            SET diagnostico = 3
          WHERE folio_unificacion = v_folio_lote
            AND diagnostico = 4   
           
      IF(r_bandera = 0)THEN      
         DISPLAY "Operación lista para volver a generarse."
      
      ELSE
       -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
   ELSE
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO LIQUIDACION"
   
   LET p_mensaje = "Finalización de proceso - REVERSO LIQUIDACION","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

END MAIN
