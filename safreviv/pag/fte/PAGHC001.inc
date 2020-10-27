###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Módulo            => AFILIACIÓN                                              #
#Programa          => PAFHC001.inc                                            #
#Objetivo          => Declaración de variables y constantes globales          #
###############################################################################

GLOBALS

   TYPE datos_generales RECORD
      id_derechohabiente       DECIMAL(9,0),
      nss                      CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_completo          CHAR(120)
   END RECORD

   TYPE datos_consulta RECORD
      id_derechohabiente       DECIMAL(9,0),
      nss                      CHAR(11),
      nss2                     CHAR(11),
      nss3                     CHAR(11),
      nss4                     CHAR(11),
      rfc                      CHAR(13),
      curp                     CHAR(18),
      nombre_completo          CHAR(120)
   END RECORD

   TYPE historico_trm RECORD
      nss                     CHAR(11),
      nrp                     CHAR(11),
      f_pago                  DATE,
      periodo_pago            CHAR(6),
      folio_sua               CHAR(6),
      cve_aportacion          CHAR(1),
      aportacion              DECIMAL(15,2),
      cve_amortizacion        CHAR(1),
      amortizacion            DECIMAL(15,2),
      tpo_aclaracion          CHAR(2)
   END RECORD

   TYPE historico_anual RECORD
      nss_an                  CHAR(11),
      rfc_an                  CHAR(13),
      nombre_trabajador       VARCHAR(120),
      ano                     CHAR(4),
      clave                   VARCHAR(3),
      patron                  VARCHAR(120),
      bimestres               VARCHAR(3),
      importe                 DECIMAL(16,2)
   END RECORD

   TYPE historico_ads RECORD
      nss_ads               CHAR(11),
      rfc_ads               CHAR(13),
      nombre                VARCHAR(120)  ,
      ano_pago              CHAR(4)  ,
      bimestre              CHAR(3)  ,
      f_pago_ads            DATE,
      clave_ap              CHAR(3)  ,
      patron_ads            CHAR(120)  ,
      importe_ads           DECIMAL(16,2)
   END RECORD

   TYPE historico_pensiones RECORD
      nss_pen              CHAR(11),
      rfc_pen              CHAR(13),
      nombre_pen           VARCHAR(60),
      sdi                  CHAR(7),
      bimestre_pen         CHAR(6),
      f_pago_pen           DATE,
      clave_pen            CHAR(3),
      patron_pen           VARCHAR(60),
      aportacion_pen       DECIMAL(16,2),
      amortizacion_pen     DECIMAL(16,2),
      folio_sua_pen        CHAR(6)
   END RECORD

END GLOBALS
