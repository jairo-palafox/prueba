################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => CNT                                                           #
#Programa     => CNTR011                                                       #
#Objetivo     => Programa que ejecuta el reverso de la póliza contable en batch#
#Fecha inicio => 29/10/2012                                                    #
################################################################################



DATABASE safre_viv

GLOBALS
 DEFINE p_usuario       VARCHAR(30), -- usuario
        p_proceso_cod  LIKE cat_proceso.proceso_cod, -- código de proceso
        p_pid          LIKE glo_pid.pid,
        p_opera_cod    LIKE cat_operacion.opera_cod, -- código de operación
        p_folio        LIKE dis_det_avance_pago.folio, -- Folio generado
        p_folio_ant    LIKE dis_det_avance_pago.folio, -- Folio a reversar
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
   



   CALL fn_reversa_poliza_cnt()
 

END MAIN



FUNCTION fn_reversa_poliza_cnt()

   DEFINE
      r_bandera            SMALLINT 

   DEFINE 
      v_QryTxt          STRING,
      r_bnd_status      SMALLINT,
      r_bnd_error       SMALLINT,
      r_mensaje         CHAR (70)
      

   LET r_bnd_status = 0
   LET r_bnd_error = 0


   --Validar lógica de negocio

   WHENEVER ERROR CONTINUE
   
      --Regresa al estado normal de los registros contables
      UPDATE cnt_transaccion
      SET   estado = 10,
            folio_cnt = 0
            --tpo_transaccion = 1 --reversado
      WHERE folio_cnt = p_folio;

      --Elimina información de control
      {UPDATE cnt_ctr_proceso
      SET   estado = 70,
            f_reverso = TODAY,
            folio_reversado = p_folio
      WHERE folio_cnt = p_folio}
      
      DELETE FROM cnt_ctr_proceso
      WHERE folio_cnt = p_folio;

   WHENEVER ERROR STOP 

   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",r_mensaje
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF  
   
   --Reversa operación 1
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
   RETURNING r_bandera

      
   DISPLAY " ############################################ "
   DISPLAY "\n  El reverso de la Póliza Contable se ha realizado exitosamente"

END FUNCTION 


