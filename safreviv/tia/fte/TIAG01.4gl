-- funciones, variales y constantes globales del modulo de traspasos INFONAVIT-AFORE
DATABASE safre_viv
GLOBALS
   CONSTANT  g_proceso_cod_tia                       SMALLINT = 1701  -- traspaso
   CONSTANT  g_proceso_cod_reportes_decreto          SMALLINT = 1702  -- traspaso  
   
   -- etapas de los procesos
   CONSTANT  g_opera_cod_tia_carga                   SMALLINT = 1, -- carga de archivo de tia
             g_opera_cod_tia_integracion             SMALLINT = 2, -- integracion de de tia
             g_opera_cod_tia_preliquidacion          SMALLINT = 3, -- preliquidacion de tia
             g_opera_cod_tia_liquidacion             SMALLINT = 4, -- liquidacion de tia
             g_opera_cod_tia_genera_reportes_decreto SMALLINT = 1  -- Genéra reportes de decretos

END GLOBALS