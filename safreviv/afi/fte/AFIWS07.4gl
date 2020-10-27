IMPORT FGL WSHelper
IMPORT com
IMPORT XML

GLOBALS "AFIWS05.inc"

FUNCTION fn_consulta_afore()
   DEFINE v_nss           CHAR(11)
   DEFINE v_cta           SMALLINT
   DEFINE v_afore_desc    CHAR(40)
   DEFINE v_id_dh         DECIMAL(9,0)
   DEFINE v_idx           SMALLINT
   DEFINE v_ax_nss        STRING 
   DEFINE v_bnd_resp      SMALLINT 
   
   LET v_nss    = v_rec_in.nss_in
   LET v_ax_nss = v_nss
   LET v_ax_nss = v_ax_nss CLIPPED 
   LET bnd_dato = 1

   DISPLAY "NSS : ",v_nss
   DISPLAY "tamaño nss : " ,LENGTH(v_nss)
   
   IF LENGTH(v_nss) <> 11 THEN
      LET resp_out.CodResp = "10 NSS Inválido o Incorrecto" CLIPPED
      DISPLAY "diferente a 11"
      LET bnd_dato = 0
   ELSE
      FOR v_idx = 1 TO v_ax_nss.getLength()
         --IF v_ax_nss.getCharAt(a) NOT MATCHES '[0-9]*' THEN
         IF v_ax_nss.getCharAt(v_idx) NOT MATCHES '[0-9]' THEN
            LET resp_out.CodResp = "10 NSS Inválido o Incorrecto" CLIPPED
            LET bnd_dato = 0
            DISPLAY "Vaida numeros"
            EXIT FOR
         END IF 
      END FOR 

      IF bnd_dato = 1 THEN
         -- Se valida que exista el nss en la base de datos
         SELECT COUNT(*)
           INTO v_cta
           FROM afi_derechohabiente
          WHERE nss = v_nss

         IF v_cta >= 1 THEN
            LET resp_out.CodResp = "20 NSS Correcto" CLIPPED
            LET v_bnd_resp = 1 
         ELSE
            LET resp_out.CodResp = "11 NSS no existe en Base de Datos" CLIPPED
         END IF
      END IF 
   END IF

   --IF resp_out.CodResp = "20 NSS Correcto" THEN
   IF v_bnd_resp = 1 THEN
      LET resp_out.nss = v_nss

      -- Se obtiene el id_derechohabiente 
      LET v_s_qry = "SELECT FIRST 1 id_derechohabiente
                       FROM afi_derechohabiente
                      WHERE nss = ? "

      PREPARE prp_c_iddh FROM v_s_qry
      EXECUTE prp_c_iddh USING v_nss 
                          INTO v_id_dh

      LET v_s_qry = " SELECT FIRST 1 a.afore_cod || ' - ' || b.afore_desc
                        FROM afi_afore a, 
                             cat_afore b
                       WHERE id_derechohabiente = ?
                         AND a.afore_cod = b.afore_cod"

      PREPARE prp_cons_afore FROM v_s_qry
      EXECUTE prp_cons_afore USING v_id_dh 
                              INTO v_afore_desc

      IF v_afore_desc[1,3] = 531 OR
         v_afore_desc IS NULL    OR
         v_afore_desc = " "      OR
         v_afore_desc = ""       THEN

         LET resp_out.afore = "000 - SIN AFORE REGISTRADA"
      ELSE
         LET resp_out.afore = v_afore_desc
      END IF
   END IF
END FUNCTION
