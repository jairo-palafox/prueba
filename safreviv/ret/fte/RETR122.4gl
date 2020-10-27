 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RET                                                     #
#Programa          => RETR122                                                 #
#Objetivo          => REVERSO LIQUIDACION DE RETIRO FONDO DE AHORRO           #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
DEFINE  p_s_titulo       STRING -- titulo de la ventana
       ,p_pid            LIKE bat_ctr_operacion.pid         -- PID del proceso
       ,p_proceso_cod    LIKE bat_ctr_operacion.proceso_cod -- codigo del proceso
       ,p_opera_cod      LIKE bat_ctr_operacion.opera_cod   -- codigo de la operacion
       ,p_usuario_cod    LIKE seg_usuario.usuario_cod       -- clave del usuario firmado
       ,p_folio          LIKE ret_preliquida.folio_liquida
       ,p_doc_cod        VARCHAR(20) 
       ,r_sql_code       SMALLINT
       ,v_error          SMALLINT 
       ,v_estatus        SMALLINT
       ,v_sql            STRING
       ,p_opera_cod_liquidacion LIKE cat_operacion.opera_cod -- codigo de operacion


   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 
   --DISPLAY p_proceso_cod , p_opera_cod
   LET p_opera_cod_liquidacion = p_opera_cod

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR122.log")
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   DISPLAY "Ejecutando rutina de reverso de liquidación..."

   -- reverso contable
   CALL fn_reverso_reg_cnt(p_folio) RETURNING v_error

   -- si el registro contable se reverso correctamente
   IF ( v_error = 0 ) THEN
      # Ejecuta el SP que realiza el reverso
      PREPARE prp_fn_reverso_liquidacion FROM "EXECUTE FUNCTION fn_reverso_liquidacion_fondo72(?)"
      EXECUTE prp_fn_reverso_liquidacion USING p_folio
                                         INTO r_sql_code
   
      -- si el reverso de liqudiacion se ejecuto correctamente
      IF ( r_sql_code = 0 ) THEN
         -- se desmarcan los derechohabientes
         PREPARE prp_cam_stat_liquidacion FROM "EXECUTE PROCEDURE sp_status_retiro_fondo_ahorro_acti_arch( ?, ?, ?, ?, ?)"
         EXECUTE prp_cam_stat_liquidacion USING  '0' , '50', p_usuario_cod , 'R' ,p_folio -- rechazo, --estatus liquidacion
                                             --v_causa_rec, v_estatus usuario ,v_mod_marca,v_folio  
		 
         -- Se cambia el estatus de la tabla glo_folio para que permita volver a realizar el folio
         PREPARE prp_actualiza_folio_status FROM "\n UPDATE glo_folio    "
                                               ||"\n SET    status = 1   "
                                               ||"\n WHERE folio = ?     "
         
         EXECUTE prp_actualiza_folio_status USING p_folio
         --Se actualiza el estatus de la operacion como reversada en el monitor
         CALL fn_reversa_operacion(p_pid, p_proceso_cod, p_opera_cod_liquidacion)
              RETURNING v_estatus

         DISPLAY "El reverso de Liquidación ha finalizado..." 
      ELSE
         DISPLAY "Ocurrio un error al realizar el reverso de la liquidación"
         DISPLAY "Error (SQL): ", r_sql_code
      END IF
   ELSE
      -- Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la LIQUIDACIÓN
      DISPLAY "No se puede ejecutar el reverso del Liquidacion"      
      CALL fn_desplega_inc_operacion(v_estatus)      
   END IF

END MAIN