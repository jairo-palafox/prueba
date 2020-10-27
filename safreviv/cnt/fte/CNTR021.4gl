################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => CNT                                                           #
#Programa     => CNTR021                                                       #
#Objetivo     => Programa que ejecuta el reverso de la confirmación de la      #
#                póliza contable en batch                                      #
#Fecha inicio => 29/10/2012                                                    #
################################################################################



DATABASE safre_viv

GLOBALS
 DEFINE p_usuario              VARCHAR(30), -- usuario
        p_proceso_cod          LIKE cat_proceso.proceso_cod, -- código de proceso
        p_pid                  LIKE glo_pid.pid,
        p_opera_cod            LIKE cat_operacion.opera_cod, -- código de operación
        p_folio                LIKE dis_det_avance_pago.folio, -- Folio generado
        p_folio_liquida        LIKE dis_det_avance_pago.folio, -- Folio a reversar
        p_archivo              VARCHAR(100),
        f_folio_confirmacion   LIKE dis_det_avance_pago.folio -- Folio confirmado


END GLOBALS 


MAIN

   --Asigna variables generales
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_archivo        = ARG_VAL(6)
   



   CALL fn_reversa_confirmacion_poliza_cnt()
 

END MAIN



FUNCTION fn_reversa_confirmacion_poliza_cnt()

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
   
   --Reversa la confirmación de la póliza
   UPDATE   cnt_ctr_proceso
   SET      num_poliza = "",
            ejercicio = "",
            f_respuesta = "",
            estado = 70  --Póliza generada
   WHERE    folio_cnt = p_folio

   DELETE FROM cnt_error_poliza
   WHERE folio_cnt = p_folio;

   UPDATE cnt_transaccion
   SET estado = 70
   WHERE folio_cnt = p_folio
   


   --Se obtiene folio de referencia
   SELECT folio_referencia
   INTO f_folio_confirmacion
   FROM glo_folio
   WHERE folio = p_folio;

   --Actualiza estado del archivo a reversado
   UPDATE glo_ctr_archivo
   SET estado = 3 --Reversado
   WHERE folio = f_folio_confirmacion;

    WHENEVER ERROR STOP


    IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",r_mensaje
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF   
{   WHENEVER ERROR CONTINUE 
      LET v_QryTxt = "EXECUTE PROCEDURE sp_rev_dis_avances_pago_dif(?)"
                           
      PREPARE prp_reverso_dif FROM v_QryTxt
      EXECUTE prp_reverso_dif USING p_folio INTO r_bnd_status,r_bnd_error,r_mensaje
   WHENEVER ERROR STOP 

   
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",r_mensaje
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF      
}

   --Reversa operación de carga de archivo
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
   RETURNING r_bandera

   --Reversa operación de integración
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
   RETURNING r_bandera

      
   DISPLAY " ############################################ "
   DISPLAY "\n  El reverso de la Confirmación de la Póliza Contable se ha realizado exitosamente"

END FUNCTION 


