################################################################################
#Modulo        => PAG
#Programa      => PAGG01
#Objetivo      => Definicion de variables y constantes globales para el modulo DE PAGOS 
#Fecha Inicio  => 19 JULIO 2012
#Autor         => Rubén Haro Castro
################################################################################

DATABASE safre_viv

GLOBALS
   -- codigos de los procesos aclaratorios
   CONSTANT 
      g_proceso_cod_pag_registro_pagos_LQINFO SMALLINT = 1401, -- registro de pagos LQINFO      
      g_proceso_cod_pag_registro_pagos_sar92  SMALLINT = 1402, -- registro pagos pagos_sar92
      g_proceso_cod_pag_registro_pagos_SINF   SMALLINT = 1403, -- registro pagos Sólo Infonavit
      g_proceso_cod_pag_registro_pagos_fc     SMALLINT = 1405, -- FORTALECIMIENTO DE CRÉDITO 
      g_proceso_cod_pag_registro_pagos_fa     SMALLINT = 1406, -- FONDO ANTERIOR
      g_proceso_cod_pag_registro_pagos_av     SMALLINT = 1407, -- Aportaciones voluntarias 
      g_proceso_cod_pag_registro_pagos_gem    SMALLINT = 1408, -- Garantia de entidades y municipios (GEM)
      g_proceso_cod_pag_registro_pagos_extrac SMALLINT = 1409, -- Extractor de pagos para la precalificación
      g_proceso_reg_pag_svt                   SMALLINT = 1416, -- Registro de pagos cambio de casa CambiaVit
      g_proceso_reg_pag_cifras_lqinfo         SMALLINT = 1417, -- Cifras globales lqinfo batch
      g_proceso_reg_pag_causal_sin_liq        SMALLINT = 1419, -- Causales sin liquidar batch

     -- etapas de las operaciones de los diferentes procesos
     g_opera_cod_pag_carga                       SMALLINT = 1 , -- valida 
     g_opera_cod_pag_integracion                 SMALLINT = 2 , -- integracion
     g_opera_cod_pag_preliquidacion              SMALLINT = 3 , -- preliquidacion
     g_opera_cod_pag_liquidacion                 SMALLINT = 4   -- liquidacion 
                                                             
END GLOBALS                                                    
      
      
