-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGC91
-- Objetivo      => Consulta de preliquidación CambiaVit
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 28 de Agosto de 2018
-- Requerimiento =>
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fehca        =>
-- Autor        =>
-- Clave cambio =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
   DEFINE g_pid LIKE bat_ctr_proceso.pid --  ID del proceso
END GLOBALS

MAIN
   DEFINE 
      p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
      p_tipo_ejecucion SMALLINT,                     -- forma como ejecutara el programa
      p_s_titulo       STRING                        -- titulo de la ventana


   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF p_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CALL fn_consulta_preliq(p_usuario_cod, g_proceso_reg_pag_svt, g_opera_cod_pag_preliquidacion)

END MAIN