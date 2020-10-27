################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 30/09/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR151                                                       #
#Objetivo     => Programa de Reverso de la DISPERSIÓN DE LAS                   #
#                APORTACIONES SUBSECUENTES PORTABILIDAD receptora.             #
#                                                                              #
#Fecha inicio => 30/09/2015                                                    #
################################################################################
DATABASE safre_viv
   DEFINE g_folio                  LIKE glo_folio.folio,
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

      CALL STARTLOG(g_usuario_cod CLIPPED||".DISR151.log")
      CALL fn_reverso()
   END IF
END MAIN


FUNCTION fn_reverso()
   DEFINE v_folio_pagos          DECIMAL(9,0)
       
   DEFINE v_bnd_rev_ope          SMALLINT, --Bandera reversa operacion
          v_QryTxt               STRING

   DEFINE r_bnd_reverso_prt   SMALLINT, 
          v_status_err           SMALLINT,
          v_desc_err             VARCHAR(200)

   DEFINE bnd_reverso_liq_ok     SMALLINT

   SELECT folio_referencia 
   INTO   v_folio_pagos
   FROM   glo_folio
   WHERE  folio = g_folio

   LET bnd_reverso_liq_ok = 0
   
   --Reversa la Liquidacion de Portabilidad
   CALL fn_reverso_poliza_cnt_prt() RETURNING bnd_reverso_liq_ok

   DISPLAY ""
   DISPLAY "____________________________________________________________________"
   DISPLAY "============================================================"
   DISPLAY " REVERSO DE LA LIQUIDACION DE PORTABILIDAD                 "
   DISPLAY ""

   IF bnd_reverso_liq_ok = 0 THEN
      -- Ejecuta el SP que realiza el reverso de la Portabilidad
      PREPARE prp_reversa_prt
      FROM "EXECUTE PROCEDURE safre_viv:sp_dis_rev_prt(?)"
      --EXECUTE prp_reversa_prt USING g_folio
      
      DISPLAY ""
      DISPLAY ""
      DISPLAY ("##############################################################")
      DISPLAY "Inicio del Reverso de Liquidación de Portabilidad"
      DISPLAY "Folio:"   ,g_folio
      
      EXECUTE prp_reversa_prt INTO r_bnd_reverso_prt, v_status_err, v_desc_err
                                    USING g_folio

      DISPLAY  r_bnd_reverso_prt, v_status_err, v_desc_err

      {
      PREPARE prp_reversa_prt_preliq
      FROM "EXECUTE PROCEDURE safre_viv:sp_dis_rev_compensa_avance(?)"

      EXECUTE prp_reversa_prt_preliq USING g_folio
}

      
      DISPLAY "Fin de Reverso Preliquidación de Portabilidad. "
      DISPLAY ""
           
      LET v_QryTxt = "\n UPDATE glo_folio",
                     "\n SET    status      = 0",
                     "\n WHERE  folio       = ", g_folio,
                     "\n AND    proceso_cod = ", g_proceso_cod,
                     "\n AND    opera_cod   = 1"
      
      DISPLAY "Reversa Control de Folios: ", g_folio
                           
      PREPARE prp_update_estatus_folio FROM v_QryTxt
      EXECUTE prp_update_estatus_folio

      {--Reversa operación 2
      CALL fn_reversa_operacion(g_pid, g_proceso_cod, g_opera_cod)
      RETURNING v_bnd_rev_ope}

      --Reversa operación 1
      CALL fn_reversa_operacion(g_pid, g_proceso_cod, 1)
      RETURNING v_bnd_rev_ope

      --Actualiza el estado del archivo a reversar
      UPDATE glo_ctr_archivo
      SET    estado      = 3
      WHERE  proceso_cod = g_proceso_cod
      AND    folio       = g_folio
     
      DISPLAY ""
      DISPLAY "Reverso de la Preliquidación Portabilidad finalizado. "
      DISPLAY ""
      DISPLAY ("##############################################################")

   ELSE
      DISPLAY "____________________________________________________________"
      DISPLAY "\nNo se pudo realizar el Reverso de la Preliquidación Portabilidad"
   END IF

END FUNCTION


FUNCTION fn_reverso_poliza_cnt_prt()
   DEFINE r_sql_code SMALLINT
   DEFINE r_bandera  INTEGER
   DEFINE v_QryTxt   STRING
   DEFINE r_rev_cnt  SMALLINT

   LET r_rev_cnt = 0

   DISPLAY ""
   DISPLAY ""
   DISPLAY ("##############################################################")
   DISPLAY "Inicio del Reverso de la Poliza Contable"
   DISPLAY "Folio:"   ,g_folio

   CALL fn_reverso_reg_cnt(g_folio) RETURNING r_rev_cnt
   LET r_sql_code = r_rev_cnt
   
   DISPLAY "r_sql_code:"   ,r_sql_code
   IF r_sql_code = 0 THEN

   
      CALL fn_reversa_operacion(g_pid, g_proceso_cod, 2) 
      RETURNING r_bandera

      LET v_QryTxt = "\n UPDATE glo_folio",
                     "\n    SET status = 1",
                     "\n WHERE folio = ",g_folio,
                     "\n   AND proceso_cod = ",g_proceso_cod,
                     "\n   AND opera_cod   = 1"
                              
      PREPARE ps_update_estatus_folio_1 FROM v_QryTxt
      EXECUTE ps_update_estatus_folio_1
      DISPLAY "Reverso de la Poliza Contable Finalizado"
      DISPLAY ""
      DISPLAY ("##############################################################")
   ELSE
      DISPLAY "============================================================"
      DISPLAY " REVERSO DEL REGISTRO DE LA PÓLIZA CONTABLE                 "
      DISPLAY "____________________________________________________________"
      DISPLAY "\nNo se pudo realizar el reverso del registro de la póliza contable debido a que"
      -- se indica en consola que fue lo que paso
      CASE r_sql_code
         WHEN 1 -- No existe poliza contable
            DISPLAY "la póliza contable no existe"
         
         WHEN 2 -- fecha de emision distinta a la actual
            DISPLAY "la fecha de emisión de la misma no corresponde a la fecha actual"
            DISPLAY ""
            DISPLAY "Debe ejecutar el proceso de Reversos Operativos."
         
         WHEN 3 -- la poliza ya se genero
            DISPLAY "la póliza ya fue emitida y contabilizada"       

         WHEN 4 -- el periodo contable ya fue cerrado
            DISPLAY "periodo contable cerrado"       

         WHEN 5 -- la póliza contable no ha sido confirmada
            DISPLAY "la póliza no ha sido contabilizada"
      END CASE
      
      DISPLAY "Para el proceso identificado por el folio: ", g_folio   
   END IF 
   
 RETURN r_sql_code
END FUNCTION