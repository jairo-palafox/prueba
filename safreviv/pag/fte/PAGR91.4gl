-----------------------------------------------------------------------------------------
-- Modulo        => PAG
-- Programa      => PAGR91
-- Objetivo      => Reverso de preliquidaci�n
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 28 de Agosto de 2018
-- Requerimiento => 
-----------------------------------------------------------------------------------------
-- Modificaci�n => 
-- Fehca        => 
-- Autor        => 
-- Clave cambio => 
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"

GLOBALS

   DEFINE 
      g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
      g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
      g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN

   DEFINE 
     p_usuario_cod    LIKE seg_usuario.usuario_cod,        -- clave del usuario firmado
     p_d_folio        LIKE dis_preliquida.folio_liquida,
     p_nombre_archivo LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo
     r_bandera        SMALLINT,
     v_s_qry          STRING

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_d_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   -- se inicia el log
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGR91.log")

   LET v_s_qry = " EXECUTE PROCEDURE sp_rev_preliquidacion_cvt(?,?)"
   PREPARE prp_exec_rev_prel_tran FROM v_s_qry
   EXECUTE prp_exec_rev_prel_tran USING p_d_folio, g_proceso_cod
   
   -- se reversa la operacion en el monitor de procesos
   CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
   RETURNING r_bandera
   
   IF r_bandera = 0 THEN
      DISPLAY "El reverso se realiz� con �xito"   
   ELSE
      -- Muestra el error ocurrido
      DISPLAY fn_recupera_inconsis_opera(r_bandera)
   END IF
END MAIN
