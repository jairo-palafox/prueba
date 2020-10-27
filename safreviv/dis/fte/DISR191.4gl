#############################################################################
#Módulo          => DIS                                                     #
#Programa        => DISR191.4gl                                             #
#Objetivo        => Programa de Reverso Confirmación (OCG).                 #
#Fecha Inicio    => 24/02/2016                                              #
#############################################################################
DATABASE safre_viv

DEFINE g_folio_transaccion       LIKE glo_folio.folio,
       g_pid                     LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod             LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_usuario_cod             LIKE seg_usuario.usuario_cod, -- Clave de usuario
       g_opera_cod               LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nom_archivo             STRING

MAIN
   #Si se ha recibido parámetros se continua    
   IF (NUM_ARGS() > 0)THEN
      LET g_usuario_cod              = ARG_VAL(1)
      LET g_pid                      = ARG_VAL(2)
      LET g_proceso_cod              = ARG_VAL(3)
      LET g_opera_cod                = ARG_VAL(4)
      LET g_folio_transaccion        = ARG_VAL(5)
      LET g_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR191.log")
      CALL fn_reverso()
   END IF
END MAIN

FUNCTION fn_reverso()
   DEFINE r_sql_code             SMALLINT
   DEFINE r_bandera              INTEGER
   DEFINE v_QryTxt               STRING

   DISPLAY ""
   DISPLAY ""
   DISPLAY ("##############################################################")
   DISPLAY "Inicio del Reverso de Confirmación"
   DISPLAY "Folio:"   ,g_folio_transaccion

   CALL fn_reverso_confirmacion_apo_sub(g_folio_transaccion) RETURNING r_sql_code

   IF r_sql_code = 0 THEN
      CALL fn_reversa_operacion(g_pid, g_proceso_cod, 1) 
      RETURNING r_bandera

      LET v_QryTxt = "\n UPDATE glo_folio",
                     "\n SET    status      = 0",
                     "\n WHERE  folio       = ",g_folio_transaccion,
                     "\n AND    proceso_cod = ",g_proceso_cod,
                     "\n AND    opera_cod   = 1"
 
      PREPARE prp_update_estatus_folio FROM v_QryTxt
      EXECUTE prp_update_estatus_folio
      DISPLAY "Reverso de Confirmación (OCG) Finalizado"
      DISPLAY ""
      DISPLAY ("##############################################################")
   ELSE
      DISPLAY ("##############################################################")
      DISPLAY ""
      DISPLAY("Ocurrio un error al aplicar el Reverso de Confirmación (OCG)")
      DISPLAY ("##############################################################")
   END IF 

END FUNCTION

FUNCTION fn_reverso_confirmacion_apo_sub(p_folio_transaccion)
  DEFINE p_folio_transaccion     DECIMAL(9,0)
  DEFINE v_bnd_reverso           SMALLINT

  LET v_bnd_reverso = 0
  PREPARE ps_reverso_confirmacion_apo_sub FROM "EXECUTE FUNCTION fn_reverso_confirma_apo_sub(?)"
  EXECUTE ps_reverso_confirmacion_apo_sub USING p_folio_transaccion INTO v_bnd_reverso

 RETURN v_bnd_reverso
END FUNCTION