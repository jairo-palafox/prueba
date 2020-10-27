--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL76                                                        #
#Objetivo     => Programa que ejecuta la rutina de reverso de preliquidaci�n   #
#                de registro de pagos de garant�a de estados y muncipios       #
#Fecha inicio => 06 Junio de 2013                                              #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"

DEFINE g_pid         LIKE bat_ctr_proceso.pid,     #  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  # codigo de operacion


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,        # clave del usuario firmado
       p_d_folio        LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       r_bandera        SMALLINT,
       v_consulta       STRING,
       v_error_isam     INTEGER,
       v_mensaje        VARCHAR(255)
   
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   DISPLAY "Ejecutando rutina de reverso..."
   
   LET v_consulta = " EXECUTE FUNCTION sp_pag_gem_rev_preliquidacion(?,?)"
   PREPARE prp_exec_rev_prel_tran FROM v_consulta
   EXECUTE prp_exec_rev_prel_tran USING p_d_folio, 
                                        g_proceso_cod 
                                   INTO r_bandera, 
                                        v_error_isam, 
                                        v_mensaje

   IF(r_bandera <> 0)THEN
      DISPLAY "C�digo: "||r_bandera
      DISPLAY "ISAM: "||v_error_isam
      DISPLAY "Mensaje: "||v_mensaje
   ELSE
      # se reversa la operacion en el monitor de procesos
      CALL fn_reversa_operacion(g_pid,
                                g_proceso_cod,
                                g_opera_cod) RETURNING r_bandera
   
      IF( r_bandera = 0 )THEN
         DISPLAY "El reverso se realiz� con �xito"   
      ELSE
         # Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
   END IF
END MAIN
