###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC07                                                  #
#Objetivo          => CONSULTA DE SALDO HISTORICO                             #
#Fecha Inicio      => 06-ABRIL-2015                                           #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 50
   
   TYPE datos_generales RECORD
      id_derechohabiente       DECIMAL(9,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_completo          CHAR(120)
   END RECORD

   TYPE arbol RECORD
      subcuenta_desc     CHAR(60),
      siefore            SMALLINT,
      monto_pesos        DECIMAL(16,6),
      monto_acciones     DECIMAL(16,6),
      subcuenta          SMALLINT,
      movimiento         SMALLINT,
      padre_id           CHAR(40),
      id                 CHAR(40),
      nivel              SMALLINT
   END RECORD

   TYPE precios RECORD
      precio_cod          SMALLINT,
      precio_sie          CHAR(30),
      precio_al_dia       DECIMAL(19,14)
   END RECORD

   TYPE movimientos RECORD
      fecha_mov                DATE,
      tpo_mov                  INTEGER,
      desc_mov                 CHAR(80),
      fondo                    SMALLINT,
      pesos                    DECIMAL(16,6),
      acciones                 DECIMAL(16,6),
      f_valor                  DATE,
      folio                    DECIMAL(10,0),
      origen                   CHAR(20),
      referencia               DECIMAL(9,0),
      modulo                   CHAR(3)
   END RECORD
   
END GLOBALS