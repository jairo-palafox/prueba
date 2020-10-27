################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 06/05/2019                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR291                                                       #
#Objetivo     => Programa que ejecuta el reverso de la carga del archivo de    #
#                actualización del indicador de adelanto de liquidación de     #
#                dispersión de pagos.                                          #
#Fecha inicio => 06/05/2019                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
 DEFINE 
   p_usuario                 VARCHAR(30),                    --Usuario
   p_proceso_cod             LIKE cat_proceso.proceso_cod,   --Código de proceso
   p_pid                     LIKE glo_pid.pid,
   p_opera_cod               LIKE cat_operacion.opera_cod, --Código de operación
   p_folio                   LIKE dis_det_avance_pago.folio, --Folio generado
   p_archivo                 VARCHAR(100)
END GLOBALS 

MAIN
  --Asigna variables generales
  LET p_usuario      = ARG_VAL(1)
  LET p_pid          = ARG_VAL(2)
  LET p_proceso_cod  = ARG_VAL(3)
  LET p_opera_cod    = ARG_VAL(4)
  LET p_folio        = ARG_VAL(5)
  LET p_archivo      = ARG_VAL(6)

  CALL fn_reversa_aiad()
END MAIN

FUNCTION fn_reversa_aiad()
  DEFINE
    r_bandera                SMALLINT, 
    v_QryTxt                 STRING,
    r_bnd_status             SMALLINT
      
  LET r_bnd_status = 0

  WHENEVER ERROR CONTINUE
    LET v_QryTxt = "EXECUTE PROCEDURE sp_rev_dis_act_ind_ade(?)"
    PREPARE prp_reverso_dif FROM v_QryTxt
    EXECUTE prp_reverso_dif USING p_folio INTO r_bnd_status
  WHENEVER ERROR STOP

  DISPLAY "p_folio      : ", p_folio
  DISPLAY "p_pid        : ", p_pid
  DISPLAY "p_proceso_cod: ", p_proceso_cod
  DISPLAY "p_opera_cod  : ", p_opera_cod
   
  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
     --Función para finalizar la operación en error
     CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
     RETURNING r_bandera
     --EXIT PROGRAM
  END IF      

  --Actualiza el estado del archivo a reversado
  UPDATE glo_ctr_archivo
  SET    estado = 3
  WHERE  folio  = p_folio

  --Reversa Operación 2
  CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
  RETURNING r_bandera

  --Reversa Operación 1
  CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
  RETURNING r_bandera
      
  DISPLAY " ############################################ "
  DISPLAY "\n  El reverso de la Actualización del Indicador de adelanto de liquidación DIS ha concluido satisfactoriamente.  "

END FUNCTION