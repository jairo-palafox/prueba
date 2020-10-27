#############################################################################
#Módulo          => DIS                                                     #
#Programa        => DISR121.4gl                                             #
#Objetivo        => Programa de Reverso de los Casos de Excepción a         #
#                   dispersar.                                              #
#Fecha Inicio    => 26-06-2014                                              #
#############################################################################
DATABASE safre_viv
  DEFINE 
    g_folio                  LIKE glo_folio.folio,
    g_pid                    LIKE bat_ctr_proceso.pid,     --ID del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Codigo del proceso
    g_usuario_cod            LIKE seg_usuario.usuario_cod, --Clave de usuario
    g_opera_cod              LIKE cat_operacion.opera_cod, --Codigo de operacion
    g_nom_archivo            STRING

MAIN
  #Si se ha recibido parámetros se continua    
  IF (NUM_ARGS() > 0)THEN
      LET g_usuario_cod   = ARG_VAL(1)
      LET g_pid           = ARG_VAL(2)
      LET g_proceso_cod   = ARG_VAL(3)
      LET g_opera_cod     = ARG_VAL(4)
      LET g_folio         = ARG_VAL(5)
      LET g_nom_archivo   = ARG_VAL(6)

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR121.log")
      CALL fn_reverso()
  END IF
END MAIN

FUNCTION fn_reverso()
  DEFINE 
    v_folio_pagos            DECIMAL(9,0)
       
  DEFINE 
    v_bnd_rev_ope            SMALLINT, --Bandera reversa operacion
    v_QryTxt                 STRING

  SELECT folio_referencia 
  INTO   v_folio_pagos
  FROM   glo_folio
  WHERE  folio = g_folio

  -- Ejecuta el SP que realiza el reverso de los casos de excepción a dispersar
  PREPARE prp_reversa_casos_excep
  FROM "EXECUTE PROCEDURE safre_viv:sp_dis_rev_casos_excep(?)"
  EXECUTE prp_reversa_casos_excep USING g_folio
   
  DISPLAY ""
  DISPLAY "Inicio de Reverso Casos de Excepción a Dispersar. "
  DISPLAY "Folio : ", g_folio
  EXECUTE prp_reversa_casos_excep USING g_folio
  DISPLAY "Fin de Reverso Casos de Excepción a Dispersar. "
  DISPLAY ""
        
  LET v_QryTxt = "\n UPDATE glo_folio",
                 "\n SET    status      = 0",
                 "\n WHERE  folio       = ", g_folio,
                 "\n AND    proceso_cod = ", g_proceso_cod,
                 "\n AND    opera_cod   = 1"
   
  DISPLAY "Reversa Control de Folios: ", g_folio
                        
  PREPARE prp_update_estatus_folio FROM v_QryTxt
  EXECUTE prp_update_estatus_folio

  --Reversa operación 2
  CALL fn_reversa_operacion(g_pid, g_proceso_cod, g_opera_cod)
  RETURNING v_bnd_rev_ope

  --Reversa operación 1
  CALL fn_reversa_operacion(g_pid, g_proceso_cod, 1)
  RETURNING v_bnd_rev_ope

  --Actualiza el estado del archivo a reversar
  UPDATE glo_ctr_archivo
  SET    estado      = 3
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = g_folio
  
  DISPLAY ""
  DISPLAY "Reverso Casos de Excepción a Disperar finalizado. "
  DISPLAY ""
  DISPLAY ("##############################################################")

END FUNCTION