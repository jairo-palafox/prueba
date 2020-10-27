####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS07.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>26 FEBRERO 2012                               #
####################################################################
DATABASE safre_viv

GLOBALS

   TYPE credito RECORD
      id_derechohabiente   LIKE cre_acreditado.id_derechohabiente,
      num_credito          LIKE cre_acreditado.num_credito,
      tipo_credito         LIKE cat_tipo_credito.desc_credito,
      estatus              LIKE cre_acreditado.edo_credito,
      fecha_otorgamiento   LIKE cre_acreditado.f_otorga,
      fecha_culminacion    LIKE cre_acreditado.f_culmina
   END RECORD

   TYPE detalle RECORD
      nrp                  LIKE cta_his_pagos.nrp,
      empresa              VARCHAR(50),
      fecha_pago           LIKE cta_his_pagos.f_pago,
      aportacion           LIKE cta_his_pagos.imp_ap_pat,
      amortizacion         LIKE cta_his_pagos.imp_am_cre,
      bimestre             LIKE cta_his_pagos.periodo_pago
   END RECORD
   
END GLOBALS