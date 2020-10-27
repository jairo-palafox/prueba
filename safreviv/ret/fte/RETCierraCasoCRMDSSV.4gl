--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            => RET                                                     #
#PROGRAMA          => RETWS19                                                 #
#OBJETIVO          => WS GENERACION DE SOLICITUD DE RETIRO PARA EL FLUJO DE   #
#                     RETIRO DE LA DEVOLUCIÓN AUTOMÁTICA DE AMORTIZACIONES    #
#                     EXCEDENTES A TRAVÉS DE LA FIEL                          #
#FECHA INICIO      => 30-NOV-2017                                             #
# Autor           Fecha      Modificación                                     #
###############################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT SECURITY
  
DATABASE safre_viv 
# 
# USER GLOBALS VARIABLES
#
GLOBALS "RETG01.4gl"
PRIVATE DEFINE v_ruta_pdf    STRING
PRIVATE DEFINE v_archivo_pdf STRING 
GLOBALS

END GLOBALS

#
# MAIN
#
MAIN
DEFINE v_resultado       INTEGER, -- recibe el resultado de la ejecucion del servicio 
       v_ruta_log        STRING,
       v_cadena          STRING,
       v_ruta_ejecutable VARCHAR(40),
       v_nss             CHAR(11),
       v_codigo          INTEGER 
DEFINE v_reg_acuse RECORD 
           nss                  CHAR(11),
           estado_solicitud     SMALLINT,
           cod_rechazo          CHAR(03),
           desc_rechazo         CHAR(30),
           caso_crm             CHAR(10)
           
   END RECORD 

   LET v_nss = ARG_VAL(1)

   IF v_nss IS NOT NULL AND v_nss <> '           ' AND v_nss <> '03907255461' THEN  
      DECLARE cur_acuses CURSOR FOR
         SELECT a.nss, a.estado_solicitud, '030', '',a.caso_adai
         FROM   ret_solicitud_generico a,
                ret_sol_medio_entrega b 
         WHERE  a.id_solicitud = b.id_solicitud
         AND    a.nss IN (
                          '24765300322','41785800834','14695310236','24735307134',
                          '12815804955','83785704640','11715324791','52694711400',
                          '78795708854','32755717512','19745728204','23845708884',
                          '17845804644','67935850346','82885302511','10815808505',
                          '01644672600','01755037577','65795822991','11765886004',
                          '11775811752','62875803454','19735343923','62785815655',
                          '33705511476','37937522581','43705251072','41725509370',
                          '24815601133')
         AND    a.modalidad_retiro = 3
         AND    a.estado_solicitud = 214

      FOREACH cur_acuses INTO v_reg_acuse.*
         CALL fn_confirma_pago_crm(v_reg_acuse.nss,v_reg_acuse.caso_crm, 1 , 0, 
                                   v_reg_acuse.estado_solicitud,v_reg_acuse.cod_rechazo, 
                                   v_reg_acuse.desc_rechazo) RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios
      END FOREACH
   ELSE 
      IF v_nss = '03907255461' THEN 
         CALL fn_confirma_pago_crm(v_nss,'501699799', 1 , 0, 
                                   100,'030','') RETURNING v_resultado, v_codigo  -- Se envia 1 mientras se definen los beneficiarios
      ELSE 
         DISPLAY "Debe ingresar un nss"
      END IF 
   END IF 

END MAIN

