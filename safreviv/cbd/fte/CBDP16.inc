####################################################################
#Modulo            =>CBD                                           #
#Programa          =>CBDP16.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>15/07/2014                                    #
####################################################################

GLOBALS

   CONSTANT VIVIENDA_97                      SMALLINT = 4
   CONSTANT VIVIENDA_92                      SMALLINT = 8

   TYPE detalle RECORD
      nss                                    CHAR(11),
      f_liquidacion                          DATE,
      modulo                                 CHAR(3),
      subcuenta                              CHAR(1),
      aivs                                   DECIMAL(22,6)
   END RECORD

   TYPE detalle_dis RECORD
      nss                                    CHAR(11),
      f_liquidacion                          DATE,
      modulo                                 CHAR(3),
      subcuenta                              CHAR(1),
      aivs                                   DECIMAL(18,2),
      folio_sua                              CHAR(6),
      periodo_pago                           CHAR(6),
      proceso_pag                            SMALLINT
   END RECORD

   TYPE detalle_pag RECORD
      nss                                    CHAR(11),
      f_liquidacion                          DATE,
      modulo                                 CHAR(3),
      subcuenta                              CHAR(1),
      aivs                                   DECIMAL(18,2),
      nrp                                    CHAR(11),
      f_pago                                 DATE,
      periodo_pago                           CHAR(6),
      folio_sua                              CHAR(6),
      imp_ap_pat                             DECIMAL(11,2),
      aiv_ap_pat                             DECIMAL(11,2),
      imp_am_cre                             DECIMAL(11,2),
      folio                                  DECIMAL(6,0),
      tpo_aclaracion                         CHAR(3),
      int_gen_pgo_ext                        DECIMAL(12,2)
   END RECORD

END GLOBALS