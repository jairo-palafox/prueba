#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CNT                                                     #
#Programa          => CNTM04                                                  #
#Objetivo          => PROGRAMA DE MANTENIMIENTO PARA EL CATALOGO DE PROCESOS  #
#                     CONTABLES                                               #
#Fecha Inicio      => 13-SEPTIEMBRE-2012                                      #
###############################################################################

GLOBALS

   CONSTANT INSERTA     SMALLINT = 1
   CONSTANT ACTUALIZA   SMALLINT = 2
   
   TYPE cat_proceso RECORD
      cod_proceso_cnt         SMALLINT,
      desc_proceso_cnt        CHAR(65),
      referencia_cnt          CHAR(4),
      desc_proc_corta_cnt     CHAR(30),
      naturaleza              CHAR(5),
      cod_naturaleza_mov      SMALLINT,
      periodo                 CHAR(40),
      cod_periodo             SMALLINT
   END RECORD

   TYPE catalogo RECORD
      clave                   SMALLINT,
      descripcion             VARCHAR(60)
   END RECORD
   
END GLOBALS