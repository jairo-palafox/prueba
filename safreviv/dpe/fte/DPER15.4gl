--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/11/2016
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPER15                                                        #
#Objetivo     => Programa lanzador del reverso de la generacion del archivo    #
#Fecha inicio => Noviembre 28, 2016.                                           #
################################################################################

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
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER15.log")

   -- Reversa operación
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
        RETURNING r_bandera
         
   IF(r_bandera = 0)THEN      
      DISPLAY ""
      DISPLAY "   Operación lista para volver a generarse."
      DISPLAY ""

      UPDATE safre_viv:dpe_sol_trabajador
      SET    diagnostico = 6
      WHERE  folio_liquida = p_i_folio
      AND    diagnostico = 8
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   LET p_titulo = "Finalización de proceso - REVERSO GENERACION DE ARCHIVO"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO GENERACION DE ARCHIVO","\n",
                   "   Folio Liquidación : "||p_i_folio,"\n",
                   "   Fecha de inicio   : "||TODAY,"\n",
                   "   Hora              : ",CURRENT HOUR TO SECOND,"\n"

   DISPLAY p_mensaje
END MAIN