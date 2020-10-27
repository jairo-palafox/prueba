--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/08/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPER16                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de carga de archivo PROCESAR #
#Fecha inicio => Agosto 06, 2012                                                        #
#########################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
       ,bnd_reverso   SMALLINT
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,p_titulo         STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje        STRING -- cuerpo del mensaje enviado

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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER24.log")

   LET g_proceso_cod = g_proceso_cod_dpe_procesar_gen -- PROCESAR
   LET g_opera_cod   = 3  -- carga

   -- Restaura datos afectados por carga de archivo
   CALL fn_dpe_corrige_reg_carga_archivo(p_nombre_archivo)
   RETURNING bnd_reverso
   IF bnd_reverso = 1 THEN 
      DISPLAY "El reverso se realiz� con �xito"
         -- Reversa operaci�n
    CALL fn_reversa_operacion(g_pid,g_proceso_cod,3)
      RETURNING r_bandera

--    CALL fn_reversa_operacion(g_pid,g_proceso_cod,1)
--      RETURNING r_bandera      
   ELSE 
      DISPLAY "El reverso no se ha realizado"   
   END IF 


   IF(r_bandera = 0)THEN      
      DISPLAY "Operaci�n lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalizaci�n de proceso - REVERSO 1� VALIDACI�N ACUSE PROCESAR"
   
   LET p_mensaje = "Finalizaci�n de proceso - REVERSO 1� VALIDACI�N ACUSE PROCESAR","\n",
                  "#\n",
                  "# Nombre del archivo: "||p_nombre_archivo,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "/ds/safreviv/ret/bin/correo.txt", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   

END MAIN

{
   Funcion : fn_dpe_corrige_reg_carga_archivo
   Fecha   : Marzo 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
   Aturo   : Felipe Nava
}
FUNCTION fn_dpe_corrige_reg_carga_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   ,v_s_qry          STRING

   LET v_s_qry =
     "DELETE FROM glo_ctr_archivo"
    ,"\n WHERE nombre_archivo =", "'",p_nombre_archivo CLIPPED,"'"
    ,"\n   AND proceso_cod = ", g_proceso_cod
    ,"\n   AND estado = 1"
   
   PREPARE Prpr_dpe_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_dpe_LimpiaCtrArchvo 
   LET bnd_reverso = 1

   DISPLAY "PID ",g_pid, " - PROCESO ",g_proceso_cod, " - OPERA ",g_opera_cod 
   
   UPDATE bat_ctr_operacion 
      SET fecha_ini   = NULL,
          fecha_fin   = NULL,
          nom_archivo = "",
          estado_cod  = 1
    WHERE pid         = g_pid
      AND proceso_cod = g_proceso_cod
      AND opera_cod   = g_opera_cod 
     
   RETURN bnd_reverso
   
END FUNCTION -- fn_dpe_corrige_reg_carga_archivo