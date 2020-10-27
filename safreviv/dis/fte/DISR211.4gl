#############################################################################
#Módulo          => DIS                                                     #
#Programa        => DISR211.4gl                                             #
#Objetivo        => Programa de Reverso Preliquidación de Aportaciones      #
#                   Subsecuentes Sin Conciliar                              #
#Fecha Inicio    => 23/03/2016                                              #
#############################################################################
DATABASE safre_viv
DEFINE g_folio_liquida             LIKE glo_folio.folio,
       g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
       g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_nom_archivo               STRING

MAIN
   #Si se ha recibido parámetros se continua    
   IF (NUM_ARGS() > 0)THEN
      LET g_usuario_cod              = ARG_VAL(1)
      LET g_pid                      = ARG_VAL(2)
      LET g_proceso_cod              = ARG_VAL(3)
      LET g_opera_cod                = ARG_VAL(4)
      LET g_folio_liquida            = ARG_VAL(5)
      LET g_nom_archivo              = ARG_VAL(6)

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR211.log")
      CALL fn_reverso()
   END IF
END MAIN

FUNCTION fn_reverso()
   DEFINE v_folio_pagos    LIKE glo_folio.folio_referencia,
          v_b_rev_pre      SMALLINT  --Bandera reversa preliquidación
       
   DEFINE v_bnd_rev_ope    SMALLINT, --Bandera reversa operacion
          v_QryTxt         STRING

   SELECT folio_referencia 
   INTO   v_folio_pagos
   FROM   glo_folio
   WHERE  folio = g_folio_liquida

   PREPARE prp_reversa_compensa_avance
   FROM "EXECUTE PROCEDURE safre_viv:fn_rev_pre_apo_sub(?)"

   DISPLAY ""
   DISPLAY "Inicio de Reverso Preliquidación Aportaciones Subsecuentes Sin Conciliar "
   DISPLAY "Folio : ",g_folio_liquida
   EXECUTE prp_reversa_compensa_avance USING g_folio_liquida
   DISPLAY "Fin de Reverso Preliquidación Aportaciones Subsecuentes Sin Conciliar "
   DISPLAY ""
   
   WHENEVER ERROR CONTINUE 
     CALL fn_reversa_preliquidacion(g_folio_liquida,
                                    g_proceso_cod,g_opera_cod)
     RETURNING v_b_rev_pre
   WHENEVER ERROR STOP

   IF SQLCA.sqlcode < 0 THEN
      DISPLAY "Error en el reverso de preliquidación ",
      SQLCA.sqlcode
   END IF
      
   LET v_QryTxt = "\n UPDATE glo_folio",
                  "\n SET    status      = 0",
                  "\n WHERE  folio       = ",g_folio_liquida,
                  "\n AND    proceso_cod = ",g_proceso_cod,
                  "\n AND    opera_cod   = 3"

   DISPLAY "Reversa Control de Folios: ",g_folio_liquida
                     
   PREPARE prp_update_estatus_folio FROM v_QryTxt
   EXECUTE prp_update_estatus_folio

   LET v_QryTxt = "\n UPDATE dis_as_sin_conciliar",
                  "\n SET    ind_concilia = 2 ",
                  "\n WHERE  ind_concilia = 3 ",
                  "\n   AND  folio_ap_subs = ",g_folio_liquida                  

   DISPLAY "Reversa tabla de control"
                     
   PREPARE prp_upd_dasc FROM v_QryTxt
   EXECUTE prp_upd_dasc


   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING v_bnd_rev_ope
                      
   DISPLAY ""
   DISPLAY "Reverso Preliquidación finalizado "
   DISPLAY ""
   DISPLAY ("##############################################################")

END FUNCTION
