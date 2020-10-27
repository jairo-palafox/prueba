####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTW01.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>09 MARZO 2012                                 #
####################################################################
DATABASE safre_viv

GLOBALS

   #
   # TYPE : notificaMandato
   #
   TYPE notificaMandato RECORD
      id_origen                     LIKE mdt_solicitud_mandato.id_origen,
      nss                           LIKE mdt_solicitud_mandato.nss,
      id_credito                    LIKE mdt_solicitud_mandato.id_credito,
      --id_mandato                    LIKE mdt_solicitud_mandato.id_solicitud_mandato,
      cve_mandato                    LIKE mdt_solicitud_mandato.cve_mandato,
      tpo_descuento                 LIKE mdt_solicitud_mandato.tpo_descuento_mandato,
      valor_descuento               LIKE mdt_solicitud_mandato.valor_descuento_mandato,
      f_canales                     LIKE mdt_solicitud_mandato.f_canales,
      f_inicio_mandato              LIKE mdt_solicitud_mandato.f_inicio_mandato,
      f_culmina_mandato             LIKE mdt_solicitud_mandato.f_culmina_mandato,
      referencia                    LIKE mdt_solicitud_mandato.referencia,
      id_canales                    LIKE mdt_solicitud_mandato.id_canales,
      tipo_operacion                LIKE mdt_solicitud_mandato.tipo_operacion,
      resultado_operacion           STRING,
      diagnostico                   LIKE mdt_solicitud_mandato.diagnostico
   END RECORD

   #
   # TYPE : notificaMandato
   #
   TYPE respuestaMandato RECORD
      id_origen                     LIKE mdt_solicitud_mandato.id_origen,
      nss                           LIKE mdt_solicitud_mandato.nss,
      id_credito                    LIKE mdt_solicitud_mandato.id_credito,
      --id_mandato                    LIKE mdt_solicitud_mandato.id_solicitud_mandato,
      cve_mandato                   LIKE mdt_solicitud_mandato.cve_mandato,
      tpo_descuento                 LIKE mdt_solicitud_mandato.tpo_descuento_mandato,
      valor_descuento               LIKE mdt_solicitud_mandato.valor_descuento_mandato,
      f_canales                     LIKE mdt_solicitud_mandato.f_canales,
      f_inicio_mandato              LIKE mdt_solicitud_mandato.f_inicio_mandato,
      f_culmina_mandato             LIKE mdt_solicitud_mandato.f_culmina_mandato,
      referencia                    LIKE mdt_solicitud_mandato.referencia,
      id_canales                    LIKE mdt_solicitud_mandato.id_canales,
      tipo_operacion                LIKE mdt_solicitud_mandato.tipo_operacion,
      resultado_operacion           STRING,
      diagnostico                   LIKE mdt_solicitud_mandato.diagnostico,
      diag_notifica                 STRING
   END RECORD

   #Variable para almacenar la fecha del dia
   DEFINE g_f_dia DATE

   CONSTANT WS_INSTRUCCION          CHAR(5) = 'mdt_1'

END GLOBALS