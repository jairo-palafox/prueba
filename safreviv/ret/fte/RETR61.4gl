--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR61                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de preliquidación            #
#                de retiros tipo N                                                      #
#Fecha inicio => Marzo 06, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_s_titulo       STRING -- titulo de la ventana
       ,p_operacion      SMALLINT
       ,p_d_folio        LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera        SMALLINT
       ,v_s_sql           STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR61.log")


   -- Se invoca rutina para reversar la preliquidación.
   CALL fn_reversa_preliquidacion(p_d_folio, g_proceso_cod, g_opera_cod)
      RETURNING r_bandera
      
   IF ( r_bandera = 0 ) THEN
     
     WHENEVER SQLERROR STOP
     --WHENEVER SQLERROR CONTINUE

      LET v_s_sql = " EXECUTE PROCEDURE sp_ret_reverso_preliquidacion_tipo_n(?,?)"
      PREPARE prp_exec_sp_estatus FROM v_s_sql
      EXECUTE prp_exec_sp_estatus USING p_d_folio,g_proceso_cod

      --regresa el valor del error sql y el error en texto
      --INTO v_i_cod_err_sql,v_c_cod_er_sql

      WHENEVER SQLERROR STOP

      CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
      RETURNING r_bandera   
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

   {-- Reversa operación
   LET r_bandera = 0
     CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
         RETURNING r_bandera
   
      LET v_s_qry = 
           "UPDATE glo_folio "
           ,"\n   SET status = 0"
           ,"\n WHERE proceso_cod = ", g_proceso_cod
           ,"\n   AND status = 1"
           ,"\n   AND folio = ",p_d_folio
      PREPARE Prpr_ActuGloFolio FROM v_s_qry CLIPPED
      EXECUTE Prpr_ActuGloFolio 
     
      LET v_s_qry = 
           "UPDATE glo_ctr_archivo"
           ,"\n   SET estado = 2 "-- Integrado
           ,"\n WHERE proceso_cod = ", g_proceso_cod
           ,"\n   AND folio = ",p_d_folio
           ,"\n   AND estado = 3" -- integrado
           
       PREPARE Prpr_ActuGloArchivo FROM v_s_qry CLIPPED
       EXECUTE Prpr_ActuGloArchivo

   -- se cambian las solicitudes a estatus 30 integrada/recibida procesar
   UPDATE ret_tipo_n
      SET estado_solicitud = 30 -- integrada/recibida procesar
    WHERE folio  = p_d_folio
      AND estado_solicitud IN (50, -- preliquidada
                               101 -- rechazada por insuficiencia de saldo
                              )
       
      -- Retaura el status de folio para volver a usarlo
   IF(r_bandera = 0 )THEN
      DISPLAY "Operación lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF}


   
END MAIN
