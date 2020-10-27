--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 25/Ene/2016
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR44                                                        #
#Objetivo     => Lanzado reverso integración                                   #
#Fecha inicio => 25/Ene/2016                                                  #
################################################################################
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_s_titulo       STRING,
       p_folio          LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       p_titulo             STRING,
       p_mensaje            STRING

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR44.log")

   CALL fn_uni_rev_integracion(p_folio, p_nombre_archivo) 
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO INTEGRACIÓN","\n",
                   "\n",
                   "   Folio complementario: "||p_folio,"\n",
                   "\n",
                   "   Fecha de inicio     : "||TODAY,"\n",
                   "   Hora                : ",CURRENT HOUR TO SECOND,"\n"

   DISPLAY p_mensaje
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)

END MAIN

#OBJETIVO: Corregir datos adicionales de reverso de integracion
FUNCTION fn_uni_rev_integracion(p_folio, p_nombre_archivo)
DEFINE p_folio          LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo,
       r_bandera        SMALLINT

   DELETE FROM glo_folio
   WHERE proceso_cod = g_proceso_cod
   AND   folio = p_folio
   ;
   UPDATE glo_ctr_archivo
   SET    estado = 1,
          folio = NULL
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_folio
   AND    estado = 2
   ;
   DELETE FROM uni_det_complementario
   WHERE  folio_complentario = p_folio
   ;

   IF(SQLCA.SQLCODE = 0)THEN    
      DISPLAY "Reverso de integración realizado con éxito."
      DISPLAY "Operación lista para volver a generarse."

      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera
   ELSE
      DISPLAY "   --- ERROR --- "
      DISPLAY "   Ocurrió un error al procesar el reverso de la integración"  
      DISPLAY "   Resultado : " , SQLCA.SQLCODE
      DISPLAY "   Error     : " , SQLERRMESSAGE
   END IF
END FUNCTION -- fn_uni_rev_integracion
