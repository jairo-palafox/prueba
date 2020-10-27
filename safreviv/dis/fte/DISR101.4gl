################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR101                                                       #
#Objetivo     => Programa que ejecuta el reverso de aportaciones subsecuentes  #
#                especiales                                                    #
#Fecha inicio => 13/08/2013                                                    #
################################################################################


DATABASE safre_viv

GLOBALS
 DEFINE p_usuario       VARCHAR(30), -- usuario
        p_proceso_cod  LIKE cat_proceso.proceso_cod, -- código de proceso
        p_pid          LIKE glo_pid.pid,
        p_opera_cod    LIKE cat_operacion.opera_cod, -- código de operación
        p_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        p_archivo      VARCHAR(100),
        v_aux_folio    STRING


END GLOBALS 


MAIN

   --Asigna variables generales
   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_folio        = ARG_VAL(5)
   LET p_archivo      = ARG_VAL(6)
   --Invoca la función de reversa de las aportaciones subsecuentes especiales
   CALL fn_reversa_aportaciones_subsecuentes_esp()
 

END MAIN



FUNCTION fn_reversa_aportaciones_subsecuentes_esp()

   DEFINE
      r_bandera            SMALLINT 

   DEFINE 
      v_QryTxt          STRING,
      r_bnd_status      SMALLINT,
      r_bnd_oera_error  SMALLINT
      

   LET r_bnd_status = 0

   WHENEVER ERROR CONTINUE 
      LET v_QryTxt = "EXECUTE PROCEDURE fn_rev_dif_apo_sub_esp(?)"
                           
      PREPARE prp_reverso_dif FROM v_QryTxt
      EXECUTE prp_reverso_dif USING p_folio INTO r_bnd_status
      
      
      
   WHENEVER ERROR STOP 

   
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF      
   
   --En caso que se haya recibido que el proceso no fué correcto  se emite un mensaje y  sale de
   --la ejecución del programa
   IF r_bnd_status <> 0 THEN
      DISPLAY "Ocurrió una inconsistencia en la ejecución del reverso, bandera",r_bnd_status
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   
   END IF 
   
   --Actualiza el estado del archivo a reversado
   UPDATE glo_ctr_archivo
   SET estado = 3
   WHERE folio = p_folio

  LET v_aux_folio=p_folio

   DISPLAY " ###############REVERSO DE APORTACIONES SUBSECUENTES ESPECIALES############## "
   DISPLAY "Reverso de Aportaciones Subsecuentes Especiales concluido con éxito"
   DISPLAY "Folio reversado: ",v_aux_folio.trim()

   -- Función para finalizar la operacion
     {CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
     RETURNING r_bnd_status}
   
   -
   -Reversa Operación 2
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
   RETURNING r_bandera

   --Reversa operación 1
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
   RETURNING r_bandera



     --Si la operacin no se finaliza, envia mensaje de error
     {IF r_bnd_status <> 0 THEN
        CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
        RETURNING r_bnd_oera_error
      

     END IF}
      


END FUNCTION 
