--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RETIROS                                                 #
#Programa          => RETR02                                                  #
#Objetivo          => REVERSO LIQUIDACION DE RETIRO SOLO INFONAVIT            #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
DEFINE --p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
        p_s_titulo            STRING -- titulo de la ventana
       ,p_pid                LIKE bat_ctr_operacion.pid         -- PID del proceso
       ,p_proceso_cod        LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod          LIKE bat_ctr_operacion.opera_cod   -- codigo de la operacion
       ,p_usuario_cod        LIKE seg_usuario.usuario_cod       -- clave del usuario firmado
       ,p_folio              LIKE ret_preliquida.folio_liquida
       ,p_doc_cod            VARCHAR(20) 
       ,r_sql_code           SMALLINT
       ,v_error              SMALLINT 
       ,v_estatus            SMALLINT 
       ,p_opera_cod_liquidacion LIKE cat_operacion.opera_cod -- codigo de operacion
       ,v_s_sql              CHAR(500)
       ,v_id_derechohabiente DECIMAL(9,0)
       ,v_id_solicitud       DECIMAL(9,0)


   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 
   LET p_opera_cod_liquidacion = p_opera_cod

   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR02.log")
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   --*****************
   CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod_liquidacion)
        RETURNING v_estatus
        
   IF v_estatus = 0 THEN
      --Se realiza el reverso de la LIQUIDACIÓN      
      CALL fn_reverso_liquidacion(p_folio)
           RETURNING r_sql_code
     
      IF r_sql_code <> 0 THEN 
         --Si es diferente de cero indica que ocurrio error en el reverso
         LET v_error = TRUE
         DISPLAY "Error # "||r_sql_code
                        ,"Ocurrió un error al realizar\n el reverso de la Liquidación"         
      ELSE
         --Se cambia el estatus de la tabla glo_folio para que permita volver a realizar el folio
         PREPARE prp_actualiza_folio_status FROM "\n UPDATE glo_folio    "
                                               ||"\n SET    status = 1   "
                                               ||"\n WHERE folio = ?     "
         
         EXECUTE prp_actualiza_folio_status USING p_folio
         --Se actualiza el estatus de la operacion como reversada en el monitor
         CALL fn_reversa_operacion(p_pid, p_proceso_cod, p_opera_cod_liquidacion)
             RETURNING v_estatus
      END IF
   ELSE
      --Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la LIQUIDACIÓN
      DISPLAY "No se puedo ejecutar el reverso del Liquidacion"
      CALL fn_desplega_inc_operacion(v_estatus)      
   END IF

   IF v_estatus = 0 THEN  
      --  CALL fn_w_reverso_liquidacion(p_usuario_cod,p_s_titulo, p_proceso_cod, p_opera_cod)
      PREPARE prp_cam_stat_liquidacion FROM "EXECUTE PROCEDURE sp_status_retiro_solo_info_acti( ?, ?, ?, ?)"
      EXECUTE prp_cam_stat_liquidacion USING  '0' , '50' , p_usuario_cod , 'R' -- rechazo, --estatus liquidacion
      
   END IF 
   IF v_error = FALSE THEN 
                 DISPLAY "Se realizo con éxito el reverso"
   END IF
END MAIN