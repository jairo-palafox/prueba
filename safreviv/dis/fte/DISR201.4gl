################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR201                                                       #
#Objetivo     => Programa que ejecuta el reverso de aportaciones subsecuentes  #
#                sin conciliar.                                                #
#Fecha inicio => 28/03/2016                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
 DEFINE p_usuario      VARCHAR(30),                    -- usuario
        p_proceso_cod  LIKE cat_proceso.proceso_cod,   -- código de proceso
        p_pid          LIKE glo_pid.pid,
        p_opera_cod    LIKE cat_operacion.opera_cod,   -- código de operación
        p_folio        DECIMAL(9,0), -- Folio generado
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

   CALL fn_reversa_aportaciones_subsecuentes()
END MAIN

FUNCTION fn_reversa_aportaciones_subsecuentes()
   DEFINE
      r_bandera         SMALLINT 

   DEFINE 
      v_QryTxt          STRING,
      r_bnd_status      SMALLINT
      
   LET r_bnd_status = 0
   
   LET v_QryTxt = " UPDATE dis_as_sin_conciliar ",
                  "    SET ind_concilia = 1 ",
	              "  WHERE folio_ap_subs = ? ", 
	              "    AND ind_concilia = 2 "
                           
   PREPARE prp_reverso_dif FROM v_QryTxt
   EXECUTE prp_reverso_dif USING p_folio    
   
   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Código de ERROR SQL: ",SQLCA.sqlcode
      -- Función para finalizar la operación en error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING r_bandera
      EXIT PROGRAM
   END IF      

   --Actualiza el estado del archivo a reversado
   UPDATE glo_ctr_archivo
   SET    estado = 3
   WHERE  folio  = p_folio

   --Reversa Operación 3
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 3) 
   RETURNING r_bandera

   --Reversa Operación 2
   CALL fn_reversa_operacion(p_pid, p_proceso_cod, 2) 
   RETURNING r_bandera
      
   DISPLAY " ############################################ "
   DISPLAY "\n  El reverso de Aportaciones Subsecuentes Sin Conciliar ha concluido satisfactoriamente.  "

END FUNCTION 
