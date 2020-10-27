#############################################################################
#Módulo          => DIS                                                     #
#Programa        => DISR111.4gl                                             #
#Objetivo        => Programa de Reverso del Cierre de Avances Abiertos      #
#Fecha Inicio    => 13-05-2014                                              #
#############################################################################
DATABASE safre_viv
  DEFINE 
    g_folio_liquida          LIKE glo_folio.folio,
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
      LET g_folio_liquida = ARG_VAL(5)
      LET g_nom_archivo   = ARG_VAL(6)

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR111.log")
      CALL fn_reverso()
  END IF
END MAIN

FUNCTION fn_reverso()
  DEFINE 
    v_folio_pagos            DECIMAL(9,0),
    v_rev_cnt                SMALLINT  --Bandera reverso registro contable
       
  DEFINE 
    v_bnd_rev_ope            SMALLINT, --Bandera reversa operacion
    v_QryTxt                 STRING

  SELECT folio_referencia 
  INTO   v_folio_pagos
  FROM   glo_folio
  WHERE  folio = g_folio_liquida

  -- Ejecuta el SP que realiza el reverso contable
  CALL fn_reverso_reg_cnt(g_folio_liquida) 
  RETURNING v_rev_cnt

  IF (v_rev_cnt = 0) THEN
     -- Ejecuta el SP que realiza el reverso del cierre de avances
     PREPARE prp_reversa_compensa_avance
     FROM "EXECUTE PROCEDURE safre_viv:sp_dis_rev_cierra_ava(?)"
     EXECUTE prp_reversa_compensa_avance USING g_folio_liquida
   
     DISPLAY ""
     DISPLAY "Inicio de Reverso Cierre Avance de Pagos. "
     DISPLAY "Folio : ", g_folio_liquida
     EXECUTE prp_reversa_compensa_avance USING g_folio_liquida
     DISPLAY "Fin de Reverso Cierre Avance de Pagos. "
     DISPLAY ""
        
     LET v_QryTxt = "\n UPDATE glo_folio",
                    "\n SET    status      = 0",
                    "\n WHERE  folio       = ", g_folio_liquida,
                    "\n AND    proceso_cod = ", g_proceso_cod,
                    "\n AND    opera_cod   = 1"
   
     DISPLAY "Reversa Control de Folios: ", g_folio_liquida
                        
     PREPARE prp_update_estatus_folio FROM v_QryTxt
     EXECUTE prp_update_estatus_folio
   
     CALL fn_reversa_operacion(g_pid, g_proceso_cod, g_opera_cod)
     RETURNING v_bnd_rev_ope
                         
     DISPLAY ""
     DISPLAY "Reverso Cierre de Avances de Pago finalizado. "
     DISPLAY ""
     DISPLAY ("##############################################################")
  ELSE
     -- no se puede realizar el reverso porque la poliza contable ya fue enviada
     -- se marca error al reversar
     DISPLAY "============================================================"
     DISPLAY " REVERSO DEL REGISTRO DE LA PÓLIZA CONTABLE                 "
     DISPLAY "____________________________________________________________"
     DISPLAY "\nNo se pudo realiza el reverso del registro de la póliza contable debido a que"
     -- se indica en consola que fue lo que paso
     CASE v_rev_cnt
       WHEN 1 -- No existe poliza contable
         DISPLAY " la póliza contable no existe."

       WHEN 2 --Fecha de emision distinta a la actual
         DISPLAY " la fecha de emisión de la misma no corresponde a la fecha actual."
         DISPLAY ""
         DISPLAY " Debe ejecutar el proceso de Reversos Operativos."

       WHEN 3 --La poliza ya se genero
         DISPLAY " la póliza ya fue emitida y contabilizada."

       WHEN 4 --El periodo contable ya fue cerrado
         DISPLAY " periodo contable cerrado."

       WHEN 5 --La póliza contable no ha sido confirmada
         DISPLAY " la póliza no ha sido contabilizada."
     END CASE

     DISPLAY " Para el proceso identificado por el folio: ", g_folio_liquida
  END IF
END FUNCTION