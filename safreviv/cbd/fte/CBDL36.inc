#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#Modulo            => CBD                                                      #
#Programa          => CBDL36                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha Inicio      => 16-AGOSTO-2017                                            #
################################################################################
GLOBALS
   #Constantes del proceso de integracion
   CONSTANT PROC_AJUSTE_SALDO         SMALLINT = 2116

   CONSTANT OP_VALIDA_ARCHIVO         SMALLINT = 1
   CONSTANT OP_INTEGRA_ARCHIVO        SMALLINT = 2

   CONSTANT EDO_ARCHIVO_VALIDADO      SMALLINT = 1
   CONSTANT EDO_ARCHIVO_INTEGRADO     SMALLINT = 2
   CONSTANT EDO_ARCHIVO_ERROR         SMALLINT = 3

   CONSTANT MOD_CONCILIACION          CHAR(3) = 'cbd'
   CONSTANT MOD_BATCH                 CHAR(3) = 'bat'

   CONSTANT v_nombre_pendientes       = "lista_pendientes" -- Tabla de archivos pendientes
   CONSTANT v_nombre_listos           = "lista_listos"--Tabla de archivos a integra 


   TYPE rutas RECORD
      ruta_bin              VARCHAR(40),
      ruta_rescate          VARCHAR(40),
      ruta_listados_proceso VARCHAR(40),
      ruta_listados_batch   VARCHAR(40)
   END RECORD

   TYPE datos_previos RECORD
      nom_archivo             VARCHAR(100),
      acciones              decimal(26,6),
      pesos                 decimal(22,2),
      registros             INTEGER
   END RECORD

END GLOBALS
