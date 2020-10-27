################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR141                                                       #
#Objetivo     => Programa que ejecuta el reverso de la carga de archivo        #
#                de créditos ceros.                                            #
#Fecha inicio => 06/03/2015                                                    #
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

   CALL STARTLOG(p_usuario CLIPPED||".DISR141.log")
   CALL fn_reversa_carga_archivo_cred_cero()
 
END MAIN


FUNCTION fn_reversa_carga_archivo_cred_cero()
   DEFINE r_bandera            SMALLINT 

   DEFINE v_QryTxt          STRING,
          r_bnd_status      SMALLINT,
          r_bnd_error       SMALLINT,
          r_mensaje         CHAR (70)
      

   LET r_bnd_status = 0
   LET r_bnd_error = 0

   WHENEVER ERROR CONTINUE 
      LET v_QryTxt = "EXECUTE PROCEDURE sp_dis_rev_cred_cero_carga(?)"
                           
      PREPARE prp_reverso_carga_cred_cero FROM v_QryTxt
      EXECUTE prp_reverso_carga_cred_cero USING p_folio INTO r_bnd_status,r_bnd_error,r_mensaje
   WHENEVER ERROR STOP 

   DISPLAY ""
   DISPLAY ""
   DISPLAY ("##############################################################")
   DISPLAY "Inicio del Reverso de Carga de Archivo Dispersión Créditos Ceros"
   DISPLAY "Folio:"   ,p_folio

   
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      DISPLAY "Mensaje: ",r_mensaje
       DISPLAY "______________________________________________________________________________"
      DISPLAY "\nNo se pudo realiza el Reverso de la Carga de Archivo Dispersión Créditos Ceros"
      
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF      

   
   --Actualiza el estado del archivo a reversado
   UPDATE glo_ctr_archivo
   SET estado = 3
   WHERE proceso_cod = 920
   AND folio = p_folio

   --Reversa Operación 2
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
   RETURNING r_bandera

   --Reversa operacioón 1
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
   RETURNING r_bandera

      
   DISPLAY " ####################################################################################"
   DISPLAY "\n El reverso de la carga de archivo de Créditos Ceros ha concluido satisfactoriamente.  "

END FUNCTION 




