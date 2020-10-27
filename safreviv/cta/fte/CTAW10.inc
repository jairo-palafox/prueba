####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAW10.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################
DATABASE safre_viv

GLOBALS

   #
   # TYPE : solicitaMarca
   #
   TYPE solicitaMarca RECORD
      id_cre_ctr_archivo      LIKE cre_acreditado.id_cre_ctr_archivo,
      id_derechohabiente		LIKE cta_marca_ws.id_derechohabiente,
      id_origen               LIKE cta_marca_ws.id_origen,
      modulo_cod              LIKE cta_marca_ws.modulo_cod,
      tpo_credito             LIKE cta_marca_ws.tpo_credito,
      marca                   LIKE cta_marca_ws.marca,
      f_solicita              LIKE cta_marca_ws.f_solicita,
      intento                 LIKE cta_marca_ws.intento,
      cod_result_op           LIKE cta_marca_ws.cod_result_op,
      diagnostico             LIKE cta_marca_ws.diagnostico,
      situacion               LIKE cta_marca_ws.situacion,
      num_credito             LIKE cta_marca_ws.num_credito,
      f_infonavit             LIKE cta_marca_ws.f_infonavit,
      marca_procesar          LIKE cta_marca_ws.marca_procesar,
      folio_archivo           LIKE cta_marca_ws.folio_archivo,
      usuario                 LIKE cta_marca_ws.usuario,
      nss                     LIKE afi_derechohabiente.nss,
      ap_paterno              LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno              LIKE afi_derechohabiente.ap_materno_af,
      nombre                  LIKE afi_derechohabiente.nombre_af,
      rfc                     LIKE afi_derechohabiente.rfc
   END RECORD

   #Variable para almacenar la fecha del dia
   DEFINE g_f_dia DATE

   CONSTANT WS_MARCA          CHAR(5) = 'cre_1'

END GLOBALS