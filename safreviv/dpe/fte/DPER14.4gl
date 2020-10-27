--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/05/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPER14                                                                 #
#Objetivo     => Programa que ejecuta el rutna de reverso de generacion de archivo      #
#                para la devolucion por pagos indevidos o en exceso solo INFONAVIT.     #
#Fecha inicio => 04/05/2012                                                             #
#########################################################################################
IMPORT os
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
       ,v_folio                        LIKE dis_preliquida.folio_liquida
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado
       ,v_c_ruta_env_acr               LIKE seg_modulo.ruta_envio -- ruta donde se coloca el archivo
       ,v_v_nom_archivo                CHAR(40) -- nombre del archivo de salida
       ,v_v_ruta_nomarch               VARCHAR(100) -- ruta y nombre del archivo de salida
       ,v_res                          SMALLINT

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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPEL22.log")

   LET g_proceso_cod = g_proceso_cod_dpe_infonavit-- devolucion de operaciones
   LET g_opera_cod   = 5 -- generacion de archivo INFONAVIT
   LET INT_FLAG = FALSE

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
         
   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."
      
      UPDATE glo_folio
      SET status = 2
      WHERE folio = p_i_folio
      AND status = 3
      
      UPDATE safre_viv:dpe_sol_soloinfonavit
         SET estado_solicitud = 4
       WHERE folio = p_i_folio
         AND estado_solicitud = 5
         
         -- se obtienen la ruta envio del modulo
      SELECT ruta_envio 
      INTO v_c_ruta_env_acr
      FROM seg_modulo
      WHERE modulo_cod = 'dpe'
     
       -- se crea el nombre del archivo y posteriormente se concatena con la ruta
   
      LET v_v_nom_archivo = "/S" ||"prueba_arch_salida_INFONAVIT.dpe"
      LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED || v_v_nom_archivo
      CALL os.Path.delete(v_c_ruta_env_acr CLIPPED || v_v_nom_archivo CLIPPED) RETURNING v_res
      
      IF v_res THEN
         DISPLAY "Archivo eliminado"
      ELSE
         DISPLAY "Archivo no eliminado"
      END IF
         
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - GENERACION DE ARCHIVO"
   
   LET p_mensaje = "Finalización de proceso - GENERACION DE ARCHIVO","\n",
                  "#\n",
                  "# Folio: "||p_i_folio,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   
END MAIN