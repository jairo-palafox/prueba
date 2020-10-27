################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR071                                                       #
#Objetivo     => Programa que ejecuta el reverso de la cancelaci�n por         #
#                depuraci�n (Avances de pago) en batch                         #
#Fecha inicio => 25/09/2012                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    p_usuario                VARCHAR(30),                  --Usuario
    p_proceso_cod            LIKE cat_proceso.proceso_cod, --C�digo de proceso
    p_pid                    LIKE glo_pid.pid,
    p_opera_cod              LIKE cat_operacion.opera_cod, --C�digo de operaci�n
    p_folio                  LIKE dis_det_avance_pago.folio,--Folio generado
    p_archivo                VARCHAR(100)
END GLOBALS 

MAIN
  --Asigna variables generales
  LET p_usuario      = ARG_VAL(1)
  LET p_pid          = ARG_VAL(2)
  LET p_proceso_cod  = ARG_VAL(3)
  LET p_opera_cod    = ARG_VAL(4)
  LET p_folio        = ARG_VAL(5)
  LET p_archivo      = ARG_VAL(6)

  CALL fn_reversa_cancelacion_avance_pagos()
END MAIN

FUNCTION fn_reversa_cancelacion_avance_pagos()
  DEFINE
    r_bandera                SMALLINT 

  DEFINE 
    v_QryTxt                 STRING,
    r_bnd_status             SMALLINT,
    r_bnd_error              SMALLINT,
    r_mensaje                CHAR(70)
      
  LET r_bnd_status = 0
  LET r_bnd_error  = 0

  WHENEVER ERROR CONTINUE 
    LET v_QryTxt = "EXECUTE PROCEDURE sp_rev_dis_avances_pago_can(?)"
                           
    PREPARE prp_reverso_dif FROM v_QryTxt
    EXECUTE prp_reverso_dif USING p_folio INTO r_bnd_status,r_bnd_error,r_mensaje
  WHENEVER ERROR STOP 

  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "C�digo de ERROR SQL: ",SQLCA.sqlcode
     DISPLAY "Mensaje: ",r_mensaje
     --Funci�n para finalizar la operaci�n en error
     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
     RETURNING r_bandera
      EXIT PROGRAM
  END IF      

  --Actualiza el estado del archivo a reversado
  UPDATE glo_ctr_archivo
  SET    estado      = 3
  WHERE  proceso_cod = 903
  AND    folio       = p_folio

  --Reversa Operaci�n 2
  CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
  RETURNING r_bandera

  --Reversa operacio�n 1
  CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
  RETURNING r_bandera
      
  DISPLAY " ############################################ "
  DISPLAY "\n  El reverso de la Cancelaci�n por Depuraci�n (Avance de Pagos) ha concluido satisfactoriamente.  "

END FUNCTION