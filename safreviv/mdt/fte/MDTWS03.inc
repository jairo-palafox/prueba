####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTWS03.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>05 MARZO 2012                                 #
####################################################################

DATABASE safre_viv

GLOBALS

   #
   # TYPE : solicitaMandato
   #
   TYPE solicitaMandato RECORD
      id_origen                  LIKE mdt_solicitud_mandato.id_origen,
      nss                        LIKE mdt_solicitud_mandato.nss,
      id_credito                 LIKE mdt_solicitud_mandato.id_credito,
      --id_mandato                 LIKE mdt_solicitud_mandato.cve_mandato,
      cve_mandato                 LIKE mdt_solicitud_mandato.cve_mandato,
      tpo_descuento_mandato      LIKE mdt_solicitud_mandato.tpo_descuento_mandato,
      valor_descuento_mandato    LIKE mdt_solicitud_mandato.valor_descuento_mandato,
      f_canales                  LIKE mdt_solicitud_mandato.f_canales,
      f_inicio_mandato           LIKE mdt_solicitud_mandato.f_inicio_mandato,
      f_culmina_mandato          LIKE mdt_solicitud_mandato.f_culmina_mandato,
      referencia                 LIKE mdt_solicitud_mandato.referencia,
      id_canales                 LIKE mdt_solicitud_mandato.id_canales,
      tipo_operacion             LIKE mdt_solicitud_mandato.tipo_operacion
   END RECORD


END GLOBALS
