###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC03                                                  #
#Objetivo          => CONSULTA DE SALDO                                       #
#Fecha Inicio      => 25-JULIO-2012                                           #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 50

   CONSTANT PERFILES_EDO_CTA  STRING = "'INTEPE'"
   CONSTANT PERFIL_ADMIN      INTEGER = 110
   
   TYPE datos_generales RECORD
      id_derechohabiente       DECIMAL(9,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_completo          CHAR(120)
   END RECORD

   TYPE historico RECORD
      f_liquidacion     DATE,
      monto_acciones    DECIMAL(16,6),
      monto_pesos       DECIMAL(12,2),
      precio_accion     DECIMAL(16,6),
      fondo             VARCHAR(40),
      subcuenta         VARCHAR(40),
      tipo_movimiento   VARCHAR(40),
      folio             DECIMAL(9,0),
      id_origen         VARCHAR(20)
   END RECORD
   
END GLOBALS