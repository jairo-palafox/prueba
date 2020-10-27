 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => RETR02                                                  #
#Programa          =>                                                         #
#Objetivo          => REVERSO LIQUIDACION DE RETIRO FONDO DE AHORRO WS        #
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
       ,p_doc_cod       varchar(20) 
       ,r_sql_code      SMALLINT
       ,v_error         SMALLINT 
       ,v_estatus       SMALLINT 
       ,p_opera_cod_liquidacion LIKE cat_operacion.opera_cod -- codigo de operacion


   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_pid           = ARG_VAL(2)
   LET p_proceso_cod   = ARG_VAL(3)
   LET p_opera_cod     = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_doc_cod       = ARG_VAL(6) 

   LET p_opera_cod_liquidacion = p_opera_cod

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR04.log")
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se indica que inicia el proceso
   DISPLAY "Iniciando rutina de reverso de liquidación de Retiros por Fondo de Ahorro WS"

   --CALL fn_valida_reverso_fondo72(p_pid, p_proceso_cod, p_opera_cod_liquidacion)
   CALL fn_valida_reverso(p_pid, p_proceso_cod, p_opera_cod)
        RETURNING v_estatus
        --LET v_estatus = 0
        --DISPLAY "v_estatus", v_estatus
        
   IF ( v_estatus = 0 ) THEN 
      -- se ejecuta la rutina de reverso
      PREPARE sid_revliquidacioni FROM "\n EXECUTE FUNCTION fn_ret_rev_liquidacion_fondo_ahorro_ws(?)"
      
      EXECUTE sid_revliquidacioni USING p_folio INTO v_error
      --Se actualiza el estatus de la operacion como reversada en el monitor
      CALL fn_reversa_operacion(p_pid, p_proceso_cod, p_opera_cod_liquidacion)
          RETURNING v_estatus
             
      DISPLAY "El reverso se realizó exitosamente."

   ELSE
      --Se muestra un mensaje en dado caso que no se pueda efectuar el reverso de la LIQUIDACIÓN
      DISPLAY "No se pudo realizar el reverso."
   END IF

END MAIN