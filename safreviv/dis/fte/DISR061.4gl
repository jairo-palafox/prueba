################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR061                                                       #
#Objetivo     => Programa que ejecuta el reverso del rechazo de avance         #
#                de pagos en batch                                             #
#Fecha inicio => 25/09/2012                                                    #
################################################################################


DATABASE safre_viv

GLOBALS
 DEFINE p_usuario       VARCHAR(30), -- usuario
        p_proceso_cod  LIKE cat_proceso.proceso_cod, -- código de proceso
        p_pid          LIKE glo_pid.pid,
        p_opera_cod    LIKE cat_operacion.opera_cod, -- código de operación
        p_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        p_archivo      VARCHAR(100)


END GLOBALS 


MAIN

   --Asigna variables generales
   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_folio        = ARG_VAL(5)
   LET p_archivo      = ARG_VAL(6)



   CALL fn_reversa_rechazo_avance_pagos()
 

END MAIN



FUNCTION fn_reversa_rechazo_avance_pagos()

   DEFINE
      v_cuenta_rechazos    DECIMAL (10,0),
      r_bandera            SMALLINT 

   CALL fn_cancela_avance_por_rechazo()
      

   WHENEVER ERROR CONTINUE 
      PREPARE prp_reverso_rch_avpag 
      FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago5(?)"
      EXECUTE prp_reverso_rch_avpag USING p_folio
      WHENEVER ERROR STOP 

      IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                RETURNING r_bandera
         EXIT PROGRAM
      END IF

      --Reversa operación 2
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
         RETURNING r_bandera

      --Reversa operacion 1
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
         RETURNING r_bandera 

      --Actualiza el estado del archivo a reversado
      UPDATE glo_ctr_archivo
      SET estado = 3
      WHERE proceso_cod = 905
      AND folio = p_folio
         
      DISPLAY " ############################################ "
      DISPLAY "\n  El reverso del Rechazo de Avance de Pagos ha concluido satisfactoriamente.  "

END FUNCTION 



#Objetivo: Función para la cancelación de avance de pago por rechazo
FUNCTION fn_cancela_avance_por_rechazo()


   DEFINE 
      v_i_derechohabiente LIKE dis_det_avance_pago.id_derechohabiente,
      v_i_periodo_pago    LIKE dis_det_avance_pago.periodo_pago,
      v_i_nrp             LIKE dis_det_avance_pago.nrp,
      v_estado            LIKE dis_det_avance_pago.estado,
      v_QryTxt            STRING 

      LET v_QryTxt = "\n SELECT id_derechohabiente,periodo_pago,nrp, estado",
                     "\n   FROM dis_det_avance_pago",
                     "\n  WHERE folio = ", p_folio

      --Cancelación Avances de Pago por Rechazo
      PREPARE prp_cancela_avance_pago_rch FROM v_QryTxt
      DECLARE cur_cancela_avance_pago_rch CURSOR FOR prp_cancela_avance_pago_rch

      FOREACH cur_cancela_avance_pago_rch INTO v_i_derechohabiente,
                                               v_i_periodo_pago,
                                               v_i_nrp,
                                               v_estado                                            
         UPDATE dis_det_avance_pago
         SET estado = 30
         WHERE id_derechohabiente = v_i_derechohabiente
         AND periodo_pago       = v_i_periodo_pago
         AND nrp                = v_i_nrp
         AND estado = 41

         
   END FOREACH

END FUNCTION