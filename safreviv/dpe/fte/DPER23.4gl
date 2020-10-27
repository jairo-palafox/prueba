--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11/06/2015
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER23                                                        #
#Objetivo     => Programa lanzado para reversar el archivo de salida créditos  #
#Fecha inicio => 30/08/2012                                                    #
################################################################################

IMPORT os
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod
       ,p_tipo_ejecucion SMALLINT
       ,p_s_titulo       STRING
       ,p_i_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       ,r_bandera        SMALLINT
       ,p_titulo         STRING
       ,p_mensaje        STRING

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

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER23.log")

   LET g_proceso_cod = g_proceso_cod_dpe_credito -- DPE Créditos
   LET g_opera_cod   = 5 -- generacion de archivo INFONAVIT
   LET INT_FLAG = FALSE

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
         
   IF(r_bandera = 0)THEN      
      DISPLAY "  Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
      UPDATE glo_folio
      SET    status = 2
      WHERE  folio = p_i_folio
      AND    status = 3

      UPDATE dpe_sol_creditos
      SET    estado_solicitud = 4
      WHERE  folio = p_i_folio
      AND    estado_solicitud = 5

      UPDATE bat_ctr_operacion 
      SET    folio = NULL
      WHERE  pid = g_pid
      AND    opera_cod = g_opera_cod

   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   LET p_titulo = " Finalización de proceso - GENERACION DE ARCHIVO"
   
   LET p_mensaje = " Finalización de proceso - GENERACION DE ARCHIVO","\n",
                   "\n",
                   " Folio Reversado: "||p_i_folio,"\n",
                   "\n",
                   " Fecha de inicio: "||TODAY,"\n",
                   " Hora           : ",CURRENT HOUR TO SECOND,"\n"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)   
END MAIN