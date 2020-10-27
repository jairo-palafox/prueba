--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLR02                                                                 #
#Objetivo     => Programa que realiza el reverso total(carga/preliquidación)            #
#                ACLARATORIOS SIN CAMBIO                                                #
#Fecha inicio => Enero 26, 2012                                                         #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen las variables#
#                correspondientes; hilda rivas                                          #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera        SMALLINT
       ,v_sql            STRING
       ,v_error_isam     INTEGER
       ,v_mensaje        VARCHAR(255)       

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLR02.log")

      --LET v_s_qry = " EXECUTE PROCEDURE sp_ret_reverso_preliquidacion_disposicion(?,?)"
      --PREPARE prp_exec_rev_prel_tran FROM v_s_qry
      --EXECUTE prp_exec_rev_prel_tran USING p_d_folio, g_proceso_cod


{
   -- se cambian los estados de pago
   DELETE FROM pag_ctr_pago
   WHERE folio = p_d_folio

   -- se regresan a estatus 1 los registros que fueron cambiados de estatus
   UPDATE cta_his_pagos   
   SET    result_operacion = 1
   WHERE  folio = p_d_folio
   AND    id_derechohabiente IN ( SELECT id_derechohabiente FROM acl_preliquida WHERE folio_liquida = p_d_folio)
   
   -- se regresan a su ind_liquidacion original los registros que fueron modificados por verificar
   -- la existencia del pago
   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 1,
          folio_referencia = NULL
   WHERE  folio_referencia = p_d_folio
   AND    ind_liquidacion IN (2,3,5)

   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 3,
          folio_referencia = NULL
   WHERE  folio_referencia = p_d_folio
   AND    ind_liquidacion = 4
   
   UPDATE cta_his_pagos
   SET    ind_liquidacion = 1
   WHERE  folio           = p_d_folio
   AND    ind_liquidacion = 4  
   
   -- se borran los datos
   DELETE FROM acl_preliquida
   WHERE folio_liquida = p_d_folio

   -- se actualiza el estatus del folio
   LET v_sql = "UPDATE glo_folio SET status = 0 WHERE folio = ?"
   PREPARE sid_folio FROM v_sql
   EXECUTE sid_folio USING p_d_folio
}
      
   -- Se invoca rutina para reversar la preliquidación.
   CALL fn_reversa_preliquidacion(p_d_folio, g_proceso_cod, g_opera_cod)
      RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN

      -- Reversa operación
      LET r_bandera = 0

      LET v_sql = " EXECUTE PROCEDURE sp_acl_sc_rev_preliquidacion(?,?)"
      PREPARE prp_exec_rev_prel_tran FROM v_sql
      EXECUTE prp_exec_rev_prel_tran INTO r_bandera, v_error_isam, v_mensaje 
      USING p_d_folio, g_proceso_cod

      DISPLAY "Resultado: ", r_bandera
      DISPLAY "Mensaje: ", v_mensaje  
    
      
      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera

   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF 
   
END MAIN
