--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/04/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGR65                                                        #
#Objetivo     => Programa que ejecuta la rutina de reverso de Integración y    #
#                carga de archivo de registro de pagos aportaciones voluntarias#
#Fecha inicio => 17 Abril del 2013                                             #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"

DEFINE g_pid          LIKE bat_ctr_proceso.pid,     #  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, # codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod  # codigo de operacion

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,        # clave del usuario firmado
       p_d_folio        LIKE dis_preliquida.folio_liquida,
       p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       r_bandera        SMALLINT,
       v_error_isam     INTEGER,
       v_mensaje        VARCHAR(255),
       v_consulta       STRING

   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   DISPLAY "Ejecutando rutina de reverso..."
   
   # se ejecuta el SP de reverso de integracion de aporataciones voluntarias  
   LET v_consulta = "EXECUTE PROCEDURE sp_pag_vol_rev_integracion(?,?)"
   PREPARE prp_reverso_integracion_vol FROM v_consulta
   EXECUTE prp_reverso_integracion_vol USING p_d_folio,
                                             g_proceso_cod
                                        INTO r_bandera, 
                                             v_error_isam, 
                                             v_mensaje
      
   IF( r_bandera = 0 )THEN
      DISPLAY v_mensaje
   ELSE
      DISPLAY "Ocurrió un error al reversar la carga e integración"
      DISPLAY "Código: ", r_bandera
      DISPLAY "ISAM: ",v_error_isam
      DISPLAY "Mensaje :", v_mensaje
      EXIT PROGRAM
   END IF
   
   CALL fn_reversa_operacion(g_pid,
                             g_proceso_cod,
                             g_opera_cod) RETURNING r_bandera
   
   IF( r_bandera = 0 )THEN
   
      # se reversa la carga
      CALL fn_corrige_reg_carga_archivo(p_nombre_archivo)

      # Reversa operacion de carga
      CALL fn_reversa_operacion(g_pid,
                                g_proceso_cod_pag_registro_pagos_av,
                                g_opera_cod_pag_carga) RETURNING r_bandera
                                
      DISPLAY "Operación lista para volver a generarse."
   ELSE
      # Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

END MAIN

{
================================================================================
Clave: 
Nombre: fn_corrige_reg_carga_archivo
Fecha creacion: 17 Abril 2013
Autor: Hugo Ramírez
Narrativa del proceso que realiza:
corrige datos adicionales de reverso de integracion

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

================================================================================
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo)
DEFINE p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo
       v_consulta          STRING

   LET v_consulta = "\n DELETE FROM glo_ctr_archivo",
                    "\n  WHERE nombre_archivo = ?",
                    "\n    AND proceso_cod = ?",
                    "\n    AND estado = 1"
   
   PREPARE prp_limpia_ctr_archvo FROM v_consulta 
   EXECUTE prp_limpia_ctr_archvo USING p_nombre_archivo,
                                      g_proceso_cod

END FUNCTION 