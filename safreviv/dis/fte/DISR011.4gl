################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 24/09/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR011                                                       #
#Objetivo     => Programa que ejecuta el reverso de avance de pagos en batch   #
#Fecha inicio => 24/09/2012                                                    #
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



   CALL fn_reversa_avance_pagos()
 

END MAIN



FUNCTION fn_reversa_avance_pagos()

   DEFINE
      v_cuenta_rechazos    DECIMAL (10,0),
      r_bandera            SMALLINT 

   --Valida que existan registros con rechazo
   SELECT COUNT (*)
   INTO v_cuenta_rechazos
   FROM dis_rch_avance_pago
   WHERE folio = p_folio

   IF v_cuenta_rechazos > 0 THEN  

      --Si hay rechazos reversa operaciones 2 y 1
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
         RETURNING r_bandera

      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
         RETURNING r_bandera

      --Ejecuta el SP que elimina los registros en las tablas sum y det

      WHENEVER ERROR CONTINUE 
      PREPARE prp_reverso_rch_avpag 
      FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago4(?)"
      EXECUTE prp_reverso_rch_avpag USING p_folio
      WHENEVER ERROR STOP 

      IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                RETURNING r_bandera
         EXIT PROGRAM
      END IF

         
      DISPLAY " ############################################ "
      DISPLAY "\n  El reverso de Avance de Pagos ha concluido satisfactoriamente.  "
      

   ELSE

      --Si no hay registros con avance cerrado 
      --SE EJECUTA REVERSO PARA OPERACIONES REGISTRADAS
      -- Llama la función para actualizar edos y fechas de tablas control de procesos
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 3) 
      RETURNING r_bandera
   
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
      RETURNING r_bandera

      CALL fn_reversa_operacion(p_pid, p_proceso_cod, 1) 
      RETURNING r_bandera  

      WHENEVER ERROR CONTINUE 
      --Ejecuta el SP que elimina los registros en las tablas sum y det
      PREPARE prp_reverso_reg_avpag 
      FROM "EXECUTE PROCEDURE safre_viv:sp_dis_avances_pago4(?)"
      EXECUTE prp_reverso_reg_avpag USING p_folio
      WHENEVER ERROR STOP 

      IF SQLCA.sqlcode < 0 THEN
         DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
         -- Función para finalizar la operación en error
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                RETURNING r_bandera
         EXIT PROGRAM
      END IF
   
      DISPLAY " ############################################ "
      DISPLAY "\n  El reverso de Avance de Pagos ha concluido satisfactoriamente.  "
      
   END IF 

END FUNCTION 