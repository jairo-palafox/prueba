--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/07/2012
--===============================================================

################################################################################
#Modulo       => DAE                                                           #
#Programa     => DPEG01                                                        #
#Objetivo     => Ejecutar funciones, variales y constantes globales del modulo #
#                de devolucion de amortizaciones excedentes                    #
#Fecha inicio => 05/04/2012                                                    #
################################################################################

DATABASE safre_viv
GLOBALS
   CONSTANT  g_proceso_cod_dae        SMALLINT = 2400
   CONSTANT  g_proceso_cod_genera_dae SMALLINT = 2401

   CONSTANT  g_opera_cod_dae_carga            SMALLINT = 1,  -- carga de archivo
             g_opera_cod_dae_integracion      SMALLINT = 2,  -- integracion de
             g_opera_cod_dae_preliquidacion   SMALLINT = 3,  -- preliquidacion
             g_opera_cod_dae_liquidacion      SMALLINT = 4,  -- liquidacion
             g_opera_cod_dae_genera_archivo   SMALLINT = 1   -- genera archivo

DEFINE w ui.Window,
       f ui.Form

END GLOBALS

#OBJETIVO: Función para desplegar pantalla de saldos del asignado              
FUNCTION fn_eje_consulta(p_pgm,p_usuario_cod,p_id_derechohabiente, p_tipo_ejecucion, p_s_titulo)
DEFINE p_pgm                SMALLINT,
       p_usuario_cod        LIKE seg_usuario.usuario_cod,
       p_id_derechohabiente DECIMAL(9,0),
       p_tipo_ejecucion SMALLINT,
       p_s_titulo CHAR(25),
       comma STRING

    DEFINE v_pgm           CHAR(6)
    DEFINE l_ruta_bin      CHAR(40)

    INITIALIZE comma TO NULL

    SELECT ct.ruta_bin
      INTO l_ruta_bin
      FROM seg_modulo ct
     WHERE modulo_cod = 'cta'

    IF p_pgm = 1 THEN
        LET v_pgm = 'CTAC01'
    END IF

    LET comma = "cd ",l_ruta_bin CLIPPED,"/; fglrun ", v_pgm," '",p_usuario_cod,
                "' '",p_tipo_ejecucion, "' '",p_s_titulo, "' '",p_id_derechohabiente,"'"

    CALL ui.interface.refresh()

    LET comma = comma CLIPPED
    RUN comma

END FUNCTION