-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGL93
-- Objetivo      => Progama que ejecuta el SP que realiza la liquidación CambiVit
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 31 de Mayo de 2018
-- Requerimiento => plasgac-43
-----------------------------------------------------------------------------------------
-- Modificación => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"

GLOBALS
   DEFINE g_pid LIKE bat_ctr_proceso.pid,  --  ID del proceso
          g_operacion SMALLINT             -- 1 CONSULTA; 2 LIQUIDA
          
END GLOBALS

MAIN
   DEFINE 
      p_usuario_cod    LIKE seg_usuario.usuario_cod,
      p_tipo_ejecucion SMALLINT,     -- forma como ejecutara el programa
      p_s_titulo       STRING        -- titulo de la ventana
  
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF p_s_titulo IS NOT NULL THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGL93.log")

   LET g_operacion   = 2 -- liquidar
   
   CALL fn_liquida(p_usuario_cod,g_proceso_reg_pag_svt, g_opera_cod_pag_liquidacion, g_operacion)
   
END MAIN