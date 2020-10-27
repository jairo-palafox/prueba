--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGR77                                                        #
#Objetivo     => Programa que ejecuta el rutna de reverso de liquidación       #
#                de registro de pagos garantía de estados y municipios         #
#Fecha inicio => 06 Junio 2013                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"

DEFINE g_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  # codigo de operacion

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,        # clave del usuario firmado
       p_folio          LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       r_bandera        INTEGER,
       v_error_isam     INTEGER,
       v_mensaje        VARCHAR(255),
       v_s_sql          STRING
       
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_folio          = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    
      
   CALL  fn_reverso_reg_cnt(p_folio) RETURNING r_bandera

   # Reversa operación
   LET r_bandera = 0

   DISPLAY "Ejecutando rutina de reverso..."
   
   # se ejecuta el SP de reverso
   LET v_s_sql = " EXECUTE PROCEDURE sp_pag_gem_rev_liquidacion(?,?)"
   PREPARE prp_reverso_liquidacion_vol FROM v_s_sql
   EXECUTE prp_reverso_liquidacion_vol USING p_folio,
                                             g_proceso_cod 
                                        INTO r_bandera, 
                                             v_error_isam, 
                                             v_mensaje

   IF(r_bandera <> 0)THEN
      DISPLAY "Código: ", r_bandera
      DISPLAY "ISAM: ", v_error_isam
      DISPLAY "Mensaje: ", v_mensaje
   ELSE
      CALL fn_reversa_operacion(g_pid,
                                g_proceso_cod,
                                g_opera_cod) RETURNING r_bandera

      IF( r_bandera = 0 )THEN
         # se termino el reverso
         DISPLAY "El reverso se realizó con éxito"
      ELSE
         # Muestra el error ocurrido
         DISPLAY fn_recupera_inconsis_opera(r_bandera)
      END IF
      
   END IF
 
END MAIN