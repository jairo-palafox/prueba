################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 03/05/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTG01                                                   #
#Objetivo          => Declarar las variables globales y constantes para el     #
#                     modulo de contabilidad                                   #
#Fecha inicio      => 03/05/2012                                               #
################################################################################
DATABASE safre_viv
GLOBALS
---Sección de variables
DEFINE cb             ui.ComboBox
DEFINE g_usuario      LIKE seg_usuario.usuario_cod 
DEFINE g_tipo_proceso SMALLINT
DEFINE g_nom_prog     VARCHAR(30)
DEFINE g_pid          LIKE bat_ctr_proceso.pid -- ID del proceso
DEFINE v_cmb_proceso  LIKE cat_proceso_cnt.cod_proceso_cnt
DEFINE v_cmb_proceso_cod LIKE cat_proceso.proceso_cod
DEFINE v_cmb_sub_cta  LIKE cat_subcuenta.subcuenta
DEFINE f_cmb_periodo  LIKE cat_proceso_cnt.cod_periodo
DEFINE v_cmb_tpo_crd  LIKE cat_tipo_credito.tpo_credito
DEFINE v_fecha        LIKE cat_proceso_cnt.f_actualiza

---Sección arreglos


---Sección de constantes
CONSTANT g_proceso_cod SMALLINT = 601
CONSTANT g_proceso_cod2 SMALLINT = 602
CONSTANT g_opera_cod1  SMALLINT = 1
CONSTANT g_opera_cod2  SMALLINT = 2
END GLOBALS