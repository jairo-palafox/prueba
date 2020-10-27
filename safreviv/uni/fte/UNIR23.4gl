--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 31/07/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR23                                                        #
#Objetivo     => Programa lanzado del reverso de validación de archivo         #
#                operación 22                                                  #
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR23.log")

   -- se asigna proceso y operacion
   LET g_proceso_cod = g_proceso_cod_uni_operacion22 
   LET g_opera_cod   = g_opera_cod_uni_cargaop22

   -- Restaura datos afectados por carga de archivo
   CALL fn_dpe_corrige_reg_carga_archivo(p_nombre_archivo)
   DISPLAY "El reverso se realizó con éxito"

   -- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN
      
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO CARGA DE ARCHIVO "
   
   LET p_mensaje = "Finalización de proceso - REVERSO CARGA DE ARCHIVO ","\n",
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
   --    
   ,v_s_qry          STRING

   LET v_s_qry =
     "DELETE FROM glo_ctr_archivo"
    ,"\n WHERE nombre_archivo = ?"
    ,"\n   AND proceso_cod = ?"
    ,"\n   AND estado = 1"
   DISPLAY v_s_qry 
   PREPARE Prpr_dpe_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_dpe_LimpiaCtrArchvo USING  p_nombre_archivo, g_proceso_cod

END FUNCTION -- fn_dpe_corrige_reg_carga_archivo
