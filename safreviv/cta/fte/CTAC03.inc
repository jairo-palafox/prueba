###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC03                                                  #
#Objetivo          => CONSULTA DE DECRETO                                     #
#Fecha Inicio      => 02-JULIO-2012                                           #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 50
   
   TYPE datos_generales RECORD
      id_decreto               DECIMAL(9,0),
      consec_cuenta            DECIMAL(11,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_completo          CHAR(120),
      cve_icefa                CHAR(3),
      nombre_patron            CHAR(120)
   END RECORD

   TYPE movimientos_decreto RECORD
      id_decreto                 DECIMAL(9,0),
      f_liquida                  DATE,
      folio_liquida              DECIMAL(9,0),
      movimiento                 VARCHAR(100),
      origen                     VARCHAR(30),
      monto_acciones             DECIMAL(22,2),
      monto_pesos                DECIMAL(22,2),
      fondo_inversion            SMALLINT
   END RECORD
   
END GLOBALS