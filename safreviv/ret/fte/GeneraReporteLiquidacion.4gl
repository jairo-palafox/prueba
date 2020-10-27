
################################################################################
#Modulo            => Retiro                                                   #
#Programa          => GeneraReporteLiquidacion                                 #
#Objetivo          => Programa lanzador para la Generacion del Reporte de      #
#                     Liquidacion                                              #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

MAIN
    DEFINE p_folio                 INTEGER
    DEFINE p_tabla                 STRING   -- nombre de la tabla en cadena
    DEFINE p_usuario_reporte       STRING   -- usuario que emite el reporte
    DEFINE p_pid                   DECIMAL(9,0) -- pid del proceso
    DEFINE p_proceso_cod           SMALLINT
    DEFINE p_opera_cod             SMALLINT
    DEFINE p_programa_cod          VARCHAR(10)

    LET p_folio            = ARG_VAL(1)
    LET p_tabla            = ARG_VAL(2)
    LET p_usuario_reporte  = ARG_VAL(3)
    LET p_pid              = ARG_VAL(4)
    LET p_proceso_cod      = ARG_VAL(5)
    LET p_opera_cod        = ARG_VAL(6)
    LET p_programa_cod     = ARG_VAL(7)

    # se invoca la funcion para generar el reporte de la liquidacion
    CALL fn_reporte_liquidacion(p_folio, p_tabla, p_usuario_reporte, p_pid, p_proceso_cod, p_opera_cod, p_programa_cod, FALSE)
  
END MAIN
