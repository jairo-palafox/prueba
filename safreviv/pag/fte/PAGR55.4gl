--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09 Agosto 2012
--=============================================================================
#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGR55                                                                 #
#Objetivo     => Programa que ejecuta la rutina de reverso de Integraci�n y carga de    #
#                archivo de registro de pagos fondo anterior                            #
#Fecha inicio => Febrero 01, 2013                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_d_folio         LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera         SMALLINT
       ,v_error_isam      INTEGER
       ,v_mensaje         VARCHAR(255)
       ,v_ls_SqlQry       STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGR55.log")

   DISPLAY "Ejecutando rutina de reverso..."
   
   -- se ejecuta el SP de reverso de integracion de FC  
   LET v_ls_SqlQry = "EXECUTE PROCEDURE sp_pag_fa_rev_integracion(?)"
   PREPARE sid_reverso FROM v_ls_SqlQry
   EXECUTE sid_reverso USING p_d_folio
      
   IF ( r_bandera = 0 ) THEN
      DISPLAY v_mensaje
   ELSE
      DISPLAY "Ocurri� un error al reversar la carga e integraci�n"
      DISPLAY "C�digo: ", r_bandera
      DISPLAY "Mensaje :", v_mensaje
      EXIT PROGRAM
   END IF
   
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
       RETURNING r_bandera

   IF ( r_bandera = 0 ) THEN

      UPDATE glo_ctr_archivo
         SET estado = 1, folio = NULL -- cargado
       WHERE folio  = p_d_folio
         AND estado = 2; -- integrado
      
      -- se reversa la carga
      CALL fn_corrige_reg_carga_archivo(p_nombre_archivo)

      -- Reversa operacion de carga
      CALL fn_reversa_operacion(g_pid,g_proceso_cod_pag_registro_pagos_fa,g_opera_cod_pag_carga)
                                RETURNING r_bandera
                                
      DISPLAY "Operaci�n lista para volver a generarse."
   ELSE
    -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF

END MAIN

{
   Funcion : fn_corrige_reg_carga_archivo
   Fecha   : febrero 21, 2012
   Descrip : corrige datos adicionales de reverso de integracion
}
FUNCTION fn_corrige_reg_carga_archivo(p_nombre_archivo)
  DEFINE 
   p_nombre_archivo  LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
   --    
   ,v_s_qry          STRING

   LET v_s_qry =
     "DELETE FROM glo_ctr_archivo"
    ,"\n WHERE nombre_archivo = ?"
    ,"\n   AND proceso_cod = ", g_proceso_cod
    ,"\n   AND estado = 1"
   --DISPLAY v_s_qry 
   PREPARE Prpr_LimpiaCtrArchvo FROM v_s_qry 
   EXECUTE Prpr_LimpiaCtrArchvo USING  p_nombre_archivo

END FUNCTION -- fn_corrige_reg_carga_archivo