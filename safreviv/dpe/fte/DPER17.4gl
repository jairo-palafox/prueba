--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/11/2016 
--===============================================================

################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEP05                                                        #
#Objetivo     => Programa lanzado del reverso de la integracion de la respuesta#
#                 PROCESAR para Devolucion de Pagos Indebidos o en Exceso      #
#Fecha inicio => Noviembre 28, 2016.                                           #
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
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       --
       ,r_bandera        SMALLINT
       ,p_titulo                       STRING -- titulo del mensaje enviado en el correo
       ,p_mensaje                      STRING -- cuerpo del mensaje enviado

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_tipo_ejecucion = ARG_VAL(7)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPER17.log")

   CALL fn_dpe_reversa_integracion_prc(p_d_folio)
      RETURNING r_bandera

   IF(r_bandera = 0)THEN

      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera

      DISPLAY "El reverso se realizó con éxito"
      DISPLAY " "
      DISPLAY "Operación lista para volver a generarse."
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN PROCESAR"

   LET p_mensaje = "   Finalización de proceso - REVERSO INTEGRACIÓN PROCESAR","\n",
                   "   Folio: "||p_d_folio,"\n",
                   "   Fecha de inicio: "||TODAY,"\n",
                   "   Hora           : ",CURRENT HOUR TO SECOND,""
   DISPLAY p_mensaje
END MAIN

#OBJETIVO: Ejecutar el reverso de la integración de la respuesta de PROCESAR
FUNCTION fn_dpe_reversa_integracion_prc(p_d_folio)
DEFINE p_d_folio   LIKE dis_preliquida.folio_liquida,
       bnd_reverso SMALLINT

   --Actualiza archivo a cargadodpe_resp_procesar
   UPDATE glo_ctr_archivo
   SET    folio = NULL, 
          estado = 1
   WHERE  proceso_cod    = 1001
   AND    opera_cod      = 3
   AND    estado         = 2;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio = NULL,
          nom_archivo = ""
   WHERE pid         = g_pid
   AND   proceso_cod = 1001
   AND   opera_cod   = 4;

   --Actualiza Integrar PROCESAR a GENERAR ARCHIVO PROCESAR
   UPDATE dpe_sol_trabajador
   SET    diagnostico     = 0,
          resul_op        = NULL,
          diag_procesa    = NULL,
          folio_respuesta = NULL
   WHERE  folio = p_d_folio;
   --AND    diagnostico = 2;

   --Elimina de la tabla de respuesta    
   DELETE 
   FROM   dpe_resp_procesar
   WHERE  folio = p_d_folio

   LET bnd_reverso = 0

   RETURN bnd_reverso 
   
END FUNCTION -- fn_dpe_reversa_integracion_prc
