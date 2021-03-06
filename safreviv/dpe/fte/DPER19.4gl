--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER19                                                        #
#Objetivo     => Programa lanzado de reverso de carga de archivo cr�ditos      #
#Fecha inicio => 30/08/2012                                                    #
################################################################################
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
   --LET p_tipo_ejecucion = ARG_VAL(7)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPEL17.log")

   LET g_proceso_cod = g_proceso_cod_dpe_credito -- dpe Cr�ditos
   LET g_opera_cod   = g_opera_cod_dpe_carga  -- carga


   -- Reversa operaci�n
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   IF(r_bandera = 0)THEN

      -- Restaura datos afectados por carga de archivo
      DELETE FROM glo_ctr_archivo
      WHERE nombre_archivo = p_nombre_archivo
      AND proceso_cod =  g_proceso_cod
      AND estado = 1
      
      DISPLAY "El reverso se realiz� con �xito"

      DISPLAY "Operaci�n lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalizaci�n de proceso - REVERSO CARGA DE ARCHIVO"
   
   LET p_mensaje = "Finalizaci�n de proceso - REVERSO CARGA DE ARCHIVO","\n",
                  "\n",
                  " Nombre del archivo: "||p_nombre_archivo,"\n",
                  "\n",
                  " Fecha de inicio: "||TODAY,"\n",
                  " Hora           : ",CURRENT HOUR TO SECOND,"\n"

   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "", -- no lleva archivo adjunto
                          p_titulo,
                          p_mensaje)
   

END MAIN