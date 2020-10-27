################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDL02                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 24/05/2012                                               #
################################################################################
DATABASE safre_viv

GLOBALS
   #Constantes del proceso de integracion
   CONSTANT g_proceso_cod        SMALLINT = 2101
   CONSTANT g_opera_cod          SMALLINT = 2

   #Variables globales
   DEFINE g_usuario              VARCHAR(30)
   DEFINE g_folio                SMALLINT
   DEFINE g_pid                  LIKE bat_ctr_proceso.pid   #  ID del proceso

   DEFINE g_reg_modulo   RECORD
      ruta_exp         VARCHAR(40),
      ruta_rescate     VARCHAR(40),
      ruta_listados    VARCHAR(40)
   END RECORD
   
END GLOBALS