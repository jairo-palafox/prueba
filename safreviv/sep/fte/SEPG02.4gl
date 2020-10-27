GLOBALS
# Compensación deudor
CONSTANT v_proc_compensacion_deudor          INTEGER = 2229
CONSTANT v_opera_carga_compensacion_deudor   INTEGER = 1
CONSTANT v_opera_integra_compensacion_deudor INTEGER = 2

# Reverso de integración de Compensación deudor
CONSTANT v_proc_reverso_int_compensacion_deudor  INTEGER = 2233
CONSTANT v_opera_reverso_int_compensacion_deudor INTEGER = 1

# Reverso de validación de Compensación deudor
CONSTANT v_proc_reverso_val_compensacion_deudor  INTEGER = 2234
CONSTANT v_opera_reverso_val_compensacion_deudor INTEGER = 1

# Sepraración de montos de expedientes sólo infonavit
CONSTANT v_proc_expedientes_solo_infonavit         INTEGER = 2230        
CONSTANT v_opera_preliq_expedientes_solo_infonavit INTEGER = 1
CONSTANT v_opera_liq_expedientes_solo_infonavit    INTEGER = 2

# Reverso de preliquidación de expediente sólo infonavit
CONSTANT v_proc_reverso_preliq_exp_solo_infonavit  INTEGER = 2231        
CONSTANT v_opera_reverso_preliq_exp_solo_infonavit INTEGER = 1

# Reverso de liquidación de expediente sólo infonavit
CONSTANT v_proc_reverso_liq_exp_solo_infonavit  INTEGER = 2232        
CONSTANT v_opera_reverso_liq_exp_solo_infonavit INTEGER = 1


END GLOBALS