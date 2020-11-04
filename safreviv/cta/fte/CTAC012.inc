
GLOBALS

    CONSTANT SUB_CTA_TRM       SMALLINT = 69
    CONSTANT CARGO_INFORMATIVO SMALLINT = 2182
    CONSTANT CARGO_AMORTIZACION SMALLINT = 52

    CONSTANT SUB_CTA_TRM_AMOR       SMALLINT = 73

    CONSTANT SUB_CTA_PEN_APOR       SMALLINT = 70
    CONSTANT CARGO_INFORMATIVO_PEN SMALLINT = 2202
    CONSTANT CARGO_AMORTIZACION_PEN SMALLINT = 52

    CONSTANT SUB_CTA_PEN_AMOR       SMALLINT = 72

    CONSTANT SUB_CTA_BIM_ADS   SMALLINT = 71
    CONSTANT DESC_AMORTIZACION VARCHAR(12) = "AMORTIZACION"

    DEFINE g_nss       VARCHAR(11)
    DEFINE g_finicio   DATE
    DEFINE g_ffin      DATE
    DEFINE g_opcion    SMALLINT

    TYPE trabajador RECORD
       id_derechohabiente   DECIMAL(9,0),
       nss                  CHAR(11),
       rfc                  CHAR(13),
       curp                 CHAR(18),
       nombre               VARCHAR(40),
       a_paterno            VARCHAR(40),
       a_materno            VARCHAR(40)
    END RECORD


    TYPE movimientos RECORD
       f_liquida         DATE,
       movimiento        SMALLINT,
       movimiento_desc   VARCHAR(40),
       tipo              SMALLINT,
       desc_ciudadana    VARCHAR(60),
       subcuenta         SMALLINT,
       subcuenta_desc    VARCHAR(40),
       monto_pesos       DECIMAL(12,2),
       bimestre          VARCHAR (7),
       nrp               VARCHAR(11),
       nombre_patron     VARCHAR(160),
       id_movimiento     INTEGER
    END RECORD


    TYPE movimientos_ads RECORD
       nss               CHAR(11),
       f_liquida         DATE,
       movimiento        SMALLINT,
       movimiento_desc   VARCHAR(40),
       tipo              SMALLINT,
       desc_ciudadana    VARCHAR(60),
       subcuenta         SMALLINT,
       subcuenta_desc    VARCHAR(40),
       monto_pesos       DECIMAL(12,2),
       bimestre          VARCHAR (6),
       nrp               VARCHAR(11),
       nombre_patron     VARCHAR(160),
       id_movimiento     INTEGER
    END RECORD

    TYPE movimiento_otros RECORD
      fLiquidacion DATE,
      bimestre   CHAR(6),
      movimiento CHAR(5),
      movimiento_desc STRING,
      subcuenta STRING,
      abono DECIMAL(16,2),
      cargo DECIMAL(16,2),
      no_movimiento SMALLINT,
      id_movimiento INTEGER
    END RECORD

    TYPE movimiento_final RECORD
      fLiquidacion DATE,
      movimiento CHAR(5),
      movimiento_desc STRING,
      subcuenta STRING,
      abono DECIMAL(16,2),
      cargo DECIMAL(16,2)
    END RECORD

END GLOBALS
