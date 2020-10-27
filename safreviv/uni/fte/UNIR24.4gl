--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR24                                                        #
#Objetivo     => Programa que ejecuta el rutna de reverso de liquidación       #
#                operación 22                                                  #
#Fecha inicio => Junio 05, 2012                                                #
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
       ,p_i_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera                      SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado

DEFINE arr_referencias_op22 DYNAMIC ARRAY OF RECORD
          v_id_unificador_dor DECIMAL(9,0), 
          v_id_unificador_ado DECIMAL(9,0), 
          v_id_referencia_dor DECIMAL(9,0), 
          v_id_referencia_ado DECIMAL(9,0)  
END RECORD   

DEFINE v_i_referencias INTEGER

       
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR24.log")

   LET g_proceso_cod = g_proceso_cod_uni_operacion22 -- Operacion 22   
   LET g_opera_cod   = g_opera_cod_uni_integraop22 -- integra op22

   CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod) RETURNING r_bandera
   
   IF r_bandera = 0 THEN
   -- Reversa operación
      LET r_bandera = 0
        CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
            RETURNING r_bandera
            
      IF(r_bandera = 0)THEN
           
         DECLARE cur_referencias CURSOR FOR 
                 SELECT dor.id_unificador, 
                        ado.id_unificador,
                        dor.id_referencia,
                        ado.id_referencia_ado
                   FROM uni_resp_op22_unificador dor, 
                        uni_resp_op22_unificado ado  
                  WHERE folio = p_i_folio
                   AND  dor.id_unificador = ado.id_unificador

         LET v_i_referencias = 1          

         FOREACH cur_referencias INTO arr_referencias_op22[v_i_referencias ].*
         
            --Borra de UNIFICADO
            DELETE FROM uni_resp_op22_unificado
             WHERE id_unificador = arr_referencias_op22[v_i_referencias ].v_id_unificador_ado
            --DISPLAY "SE ELIMINO INFORMACION DEL UNIFICADO"

            --Borra de UNIFICADOR
            DELETE FROM uni_resp_op22_unificador
             WHERE id_unificador = arr_referencias_op22[v_i_referencias ].v_id_unificador_dor
               AND folio = p_i_folio
            --DISPLAY "SE ELIMINO INFORMACION DEL UNIFICADOR"

            IF arr_referencias_op22[v_i_referencias ].v_id_referencia_dor <> 0 OR 
               arr_referencias_op22[v_i_referencias ].v_id_referencia_ado <> 0 THEN 
               --Actualiza fechas de uni_det_unificador 
               UPDATE uni_det_unificador 
                  SET f_notificacion = NULL,
                      f_aplicacion   = NULL
                WHERE id_unificador  = arr_referencias_op22[v_i_referencias ].v_id_unificador_dor
              -- DISPLAY "SE ACTUALIZARON FECHAS"
            END IF        
            LET v_i_referencias = v_i_referencias  + 1
         END FOREACH

         UPDATE glo_ctr_archivo
            SET estado      = 1,  -- archivo cargado
                folio       = NULL 
          WHERE folio       = p_i_folio
            AND estado      = 2
            AND opera_cod   = 1

         UPDATE bat_ctr_operacion 
            SET folio       = NULL,
                nom_archivo = NULL
          WHERE proceso_cod = g_proceso_cod
            AND opera_cod   = g_opera_cod
            AND pid         = g_pid;  

         DISPLAY "\n El reverso se realizó con éxito"         
         DISPLAY "\n Operación lista para volver a generarse. \n "
      
      ELSE
       -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
    ELSE
       DISPLAY fn_recupera_inconsis_opera(r_bandera)
    END IF  
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN OP22"
   
   LET p_mensaje = "Finalización de proceso - REVERSO INTEGRACIÓN OP22","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   
   --CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
   --                       "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
   --                       p_titulo,
   --                       p_mensaje)
   
END MAIN