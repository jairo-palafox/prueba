-- funciones, variales y constantes globales del modulo de retiros
DATABASE safre_viv
GLOBALS
   -- Proceso y operaciones de Devolucion por errores operativos DEO
   CONSTANT  g_proceso_cod_deo                SMALLINT = 801, -- Devolucion por errores operativos DEO
             g_opera_cod_deo_carga            SMALLINT = 1, -- carga
             g_opera_cod_deo_integracion      SMALLINT = 2, -- integracion
             g_opera_cod_deo_preliquidacion   SMALLINT = 3, -- preliquidacion
             g_opera_cod_deo_liquidacion      SMALLINT = 4  -- liquidacion
   
END GLOBALS