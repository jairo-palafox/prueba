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
       --,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_d_folio        VARCHAR(100)
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER16.log")

   CALL fn_dpe_reversa_carga_prc(p_nombre_archivo)
        RETURNING bnd_reverso 

   IF bnd_reverso = 1 THEN 
      DISPLAY "El reverso se realizó con éxito"
         -- Reversa operación
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera
   ELSE 
      DISPLAY "El reverso no se ha realizado"   
   END IF 

   IF(r_bandera = 0)THEN      
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   
   LET p_titulo = "Finalización de proceso - REVERSO CARGA DE ARCHIVO PROCESAR"
   
   LET p_mensaje = "  Finalización de proceso - REVERSO CARGA DE ARCHIVO PROCESAR","\n",
                   "  Nombre del archivo: "||p_nombre_archivo,"\n",
                   "  Fecha de inicio: "||TODAY,"\n",
                   "  Hora           : ",CURRENT HOUR TO SECOND,"\n"

   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,"",p_titulo,p_mensaje)
END MAIN

#OBJETIVO: Borrar el registro del archivo de la respuesta de PROCESAR.
FUNCTION fn_dpe_reversa_carga_prc(p_nombre_archivo)
DEFINE p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_folio_lote     DECIMAL (9,0)

   SELECT folio 
   INTO   v_folio_lote
   FROM   glo_ctr_archivo
   WHERE  nombre_archivo = p_nombre_archivo
   AND    proceso_cod = g_proceso_cod
   AND    estado = 1
   ;

   DELETE FROM glo_ctr_archivo
   WHERE  nombre_archivo = p_nombre_archivo
   AND    proceso_cod = g_proceso_cod
   AND    estado = 1
   ;

   UPDATE glo_folio 
   SET    status = 0
   WHERE  folio = v_folio_lote
   ;

   LET bnd_reverso = 1

   RETURN bnd_reverso
END FUNCTION -- fn_dpe_reversa_carga_prc