####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSWS02.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    consulta de aclaratorio                       #
#Fecha inicio      =>01 JUNIO 2015                                 #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml

GLOBALS "NOTWS01.inc"
GLOBALS "NOTWS02.inc"

FUNCTION fn_prepara_consulta()
   DEFINE v_query STRING

   DATABASE safre_viv

   LET v_query = "SELECT id_derechohabiente FROM afi_derechohabiente WHERE nss = ?"
   PREPARE exe_consulta_nss FROM v_query

   LET v_query = "SELECT FIRST 30 ",
                     "cta.folio, ",
                     "cta.nrp, ",
                     "fn_bimestre_pago(cta.periodo_pago), ",
                     "cta.folio_sua, ",
                     "cta.f_pago, ",
                     "cat_ind.desc_ind_liquidacion ind_liquidacion, ",
                     "cta.tpo_aclaracion || ' - ' || cat_acl.aclaracion_descripcion, ",
                     "cta.imp_ap_pat, ",
                     "cta.imp_am_cre ",
                  "FROM cta_his_pagos cta ",
                  "INNER JOIN cta_pag_complemento com ", 
                     "ON (com.id_derechohabiente = cta.id_derechohabiente AND com.id_referencia = cta.id_referencia) ",
                  "INNER JOIN pag_ind_liquidacion cat_ind ON cat_ind.ind_liquidacion = cta.ind_liquidacion ",
                  "INNER JOIN pag_tpo_aclaracion cat_acl on cat_acl.aclaracion_cod = TO_NUMBER(cta.tpo_aclaracion) ",
                  "WHERE cta.id_derechohabiente = ? ",
                  "AND cta.origen_archivo = 1 ",               --Archivo de LQ
                  "AND cta.ind_liquidacion = 1 ",              --IN (1,2,3,5,6) ",   --Indicador de pagos en Aclaracion
                  "AND TO_NUMBER(cta.tpo_aclaracion) <= 26 ",
                  "AND cta.tpo_aclaracion NOT IN ('08','09','13','17','25') ",
                  "ORDER BY cta.folio DESC, cta.f_pago DESC "
   PREPARE exe_consulta_aclaratorio FROM v_query
END FUNCTION


FUNCTION fn_consultar_aclaratorio()
   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE i                         SMALLINT

   #Se ejecuta la funcion que prepara las consultas a ejecutar
   CALL fn_prepara_consulta()

   EXECUTE exe_consulta_nss USING consultaRequest.nss INTO v_id_derechohabiente

   LET consultaReturn.nss = consultaRequest.nss
   
   IF v_id_derechohabiente IS NULL OR v_id_derechohabiente <=0 THEN
      LET consultaReturn.codigoRespuesta = NSS_NO_ENCONTRADO
   ELSE
      DECLARE cur_consulta_aclaratorio CURSOR FOR exe_consulta_aclaratorio

      LET i = 1
      FOREACH cur_consulta_aclaratorio USING v_id_derechohabiente INTO consultaReturn.movimientos[i].*
         LET i = i + 1
      END FOREACH

      IF i <> 1 THEN
         LET consultaReturn.codigoRespuesta = NSS_CON_ACLARACION
         CALL consultaReturn.movimientos.deleteElement(consultaReturn.movimientos.getLength())
      ELSE
         LET consultaReturn.codigoRespuesta = NSS_SIN_ACLARACION
         INITIALIZE consultaReturn.movimientos TO NULL
      END IF
      
   END IF
END FUNCTION