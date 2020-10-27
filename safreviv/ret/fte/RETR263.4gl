--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => RET                                                                    #
#Programa     => RETR20                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de Integración de archivo    #
#                para SPESS
#Fecha inicio => Mayo 05, 2014                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_d_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT,
       v_ls_SqlQry        STRING
       ,p_marca           SMALLINT   

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)
   
   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETR263.log")

   LET g_proceso_cod   = g_proceso_cod_ret_carga_spes -- carga inicial de retiros SPESS
   LET g_opera_cod     = g_opera_cod_ret_integra_spes_carga -- integracion

   DISPLAY g_proceso_cod
   DISPLAY g_opera_cod
   DISPLAY p_d_folio
   
   -- se ejecuta el store para el reverso
   LET v_ls_SqlQry = "EXECUTE PROCEDURE sp_ret_rev_int_spess(?)"
   PREPARE ex_desmarc_cuenta FROM v_ls_SqlQry
   EXECUTE ex_desmarc_cuenta USING p_d_folio
        
   
   -- Se invoca rutina para reversar la integración.
   CALL fn_reversa_integracion(p_d_folio, g_proceso_cod)
      RETURNING r_bandera
      
   IF ( r_bandera = 0 )THEN
      DISPLAY "El reverso se realizó con éxito"
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
   -- Reversa operación
   LET r_bandera = 0

   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
       RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN

      UPDATE glo_ctr_archivo
         SET estado = 1, folio = NULL -- cargado
       WHERE folio = p_d_folio
         AND estado = 2; -- integrado
      
      DISPLAY "Operación lista para volver a generarse."
      -- Retaura el status de folio para volver a usarlo

   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

END MAIN
