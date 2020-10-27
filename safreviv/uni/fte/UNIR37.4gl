--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12/Nov/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR37                                                        #
#Objetivo     => Programa que ejecuta el reverso de carga de archivo           #
#Fecha inicio => 12/Nov/2015                                                   #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
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
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR37.log")
   
   --Elimina archivo 
   CALL fn_uni_reversa_archivo(p_nombre_archivo)
        RETURNING r_bandera

   IF r_bandera = 0 THEN 
      --Marca operación como reversada
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera

      IF(r_bandera = 0)THEN
         DISPLAY "El reverso se realizó con éxito"   
         DISPLAY "Operación lista para volver a generarse."

         EXIT PROGRAM 
      ELSE
         -- Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
   ELSE 
      DISPLAY "El reverso no se pudo completar "   
      DISPLAY "Ha ocurrido un error : ", r_bandera
      
      EXIT PROGRAM
      
   END IF 

   LET p_titulo = "Finalización de proceso - REVERSO CARGA DE ARCHIVO CONFRONTADO"
   
   LET p_mensaje = "Finalización de proceso - REVERSO CARGA DE ARCHIVO CONFRONTADO","\n",
                  "#\n",
                  "# Nombre del archivo: "||p_nombre_archivo,"\n",
                  "#\n",
                  "# Fecha de inicio: "||TODAY,"\n",
                  "# Hora           : ",CURRENT HOUR TO SECOND,"\n",
                  "# # # # # # # # # # # # # # # # # # # # # # # #"

   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
END MAIN

#OBJETIVO: Corrige datos adicionales de reverso de integracion
FUNCTION fn_uni_reversa_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo,
   v_QryTxt          STRING,
   r_retorno         SMALLINT

   LET v_QryTxt = " DELETE FROM glo_ctr_archivo"
                 ,"\n WHERE nombre_archivo = ", '"', p_nombre_archivo,'"'
                 ,"\n   AND proceso_cod = ", g_proceso_cod
                 ,"\n   AND estado = 1"

   PREPARE Prpr_dpe_LimpiaCtrArchvo FROM v_QryTxt 
   EXECUTE Prpr_dpe_LimpiaCtrArchvo 

   IF(SQLCA.SQLCODE = 0) THEN
      LET r_retorno = 0
   ELSE
      LET r_retorno = (SQLCA.SQLCODE)  
   END IF

   RETURN r_retorno
END FUNCTION
