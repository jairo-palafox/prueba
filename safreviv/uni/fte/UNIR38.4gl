--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:12/Nov/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIR38                                                        #
#Objetivo     => Lanzado reverso integración                                   #
#Fecha inicio => 12/Nov/2015                                                   #
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
       r_bandera            SMALLINT,
       p_titulo             STRING,
       p_mensaje            STRING,
       v_QryTxt             STRING,
       v_resultado          SMALLINT,
       err_txt              CHAR(200),
       v_total_unificadores INTEGER,
       v_total_unificados   INTEGER,
       p_status             SMALLINT 

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   LET p_status         = ARG_VAL(7)
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".UNIR38.log")

   LET v_QryTxt = "EXECUTE FUNCTION fn_uni_reversa_int_nvo_modelo(?,?)"
   DISPLAY v_QryTxt 
   PREPARE prp_reverso_int FROM v_QryTxt
   EXECUTE prp_reverso_int USING p_folio, 
                                 g_proceso_cod
                            INTO v_resultado,
                                 err_txt,
                                 v_total_unificadores,
                                 v_total_unificados

   IF v_resultado = 0 THEN                                  
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
           RETURNING r_bandera

      IF(r_bandera = 0)THEN    
         DISPLAY "Reverso de integración realizado con éxito."
         DISPLAY "Operación lista para volver a generarse."
      ELSE
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
   ELSE 
      DISPLAY "   --- ERROR --- "
      DISPLAY "   Ocurrió un error al procesar el reverso de la integración"  
      DISPLAY "   Resultado : " , v_resultado
      DISPLAY "   Error     : " , err_txt
   END IF    
   
   
   LET p_titulo = "Finalización de proceso - REVERSO INTEGRACIÓN"
   
   LET p_mensaje = "   Finalización de proceso - REVERSO INTEGRACIÓN","\n",
                   "\n",
                   "   Folio confrontación: "||p_folio,"\n",
                   "\n",
                   "   Fecha de inicio: "||TODAY,"\n",
                   "   Hora           : ",CURRENT HOUR TO SECOND,"\n"

   DISPLAY p_mensaje
   
   CALL fn_correo_proceso(g_pid,g_proceso_cod,g_opera_cod,
                          "",
                          p_titulo,
                          p_mensaje)

END MAIN

#OBJETIVO: Corregir datos adicionales de reverso de integracion
FUNCTION fn_uni_rev_integracion(p_folio, p_nombre_archivo)
DEFINE p_folio        LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo

DEFINE rec_id_procedencia RECORD 
   id_unificador           DECIMAL(9,0),
   id_derechohabiente_dor  DECIMAL(9,0),
   id_unificado            DECIMAL(9,0),
   id_derechohabiente_ado  DECIMAL(9,0)
END RECORD    
   --

   DECLARE cur_act_procedencia CURSOR FOR SELECT id_unificador
                                          FROM   uni_det_procedencia
                                          WHERE  folio_resp_confronta = p_folio

   FOREACH cur_act_procedencia INTO rec_id_procedencia.id_unificador
      UPDATE uni_det_unificador 
      SET    ind_procedencia = 0, 
             diagnostico     = 1   
      WHERE  id_unificador = rec_id_procedencia.id_unificador;

      UPDATE uni_det_unificado 
      SET    diagnostico     = 1   
      WHERE  id_unificador = rec_id_procedencia.id_unificador;
   END FOREACH 
                                             
   DELETE FROM  uni_cza_unificacion
   WHERE  folio_unificacion = p_folio
   ;
   DELETE FROM uni_det_unificador
   WHERE folio_unificacion = p_folio
   ;
   DELETE FROM uni_det_unificado
   WHERE folio_unificacion = p_folio
   ;
   DELETE FROM uni_sum_unificacion
   WHERE  folio_unificacion = p_folio
   ;
   DELETE FROM sfr_marca_activa 
   WHERE folio = p_folio
   ;
   DELETE FROM sfr_marca_historica
   WHERE folio = p_folio
   ;
   DELETE FROM uni_det_rechazos
   WHERE folio_unificacion = p_folio
   ;
   DELETE FROM uni_det_procedencia
   WHERE folio_unificacion = p_folio
   ;
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
   UPDATE uni_det_procedencia
   SET    folio_resp_confronta = NULL,
          ind_procedencia      = 0 
   WHERE  folio_resp_confronta = p_folio
   ;
   UPDATE uni_pre_unificador 
   SET    estado =  10
   WHERE  id_derechohabiente = v_id_derechohabiente_unificador
   AND    estado =  1;   
END FUNCTION -- fn_uni_rev_integracion
