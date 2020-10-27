######################################################################
#Modulo            => AFI                                            #
#Programa          => AFIWS09                                        #
#Objetivo          => Libreria que se encarga de ejecutar el negocio #
#                     para la consulta de notificaciones registradas #
#                     por SMS o via correo electrónico               #
#                     por derechohabiente                            #
#Fecha inicio      => 10 Noviembre 2015                              #
######################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT XML
DATABASE safre_viv
GLOBALS "AFIWS08.inc"


FUNCTION fn_consulta_notificaciones()
   DEFINE v_nss                  CHAR(11)
   DEFINE v_cta                  SMALLINT
   DEFINE v_id_dh                DECIMAL(9,0)
   DEFINE v_idx                  SMALLINT
   DEFINE v_ax_nss               STRING 
   DEFINE v_bnd_resp             SMALLINT
   DEFINE v_tpo_notificacion     LIKE afi_ind_notifica.tpo_notificacion
   DEFINE v_ax_rec_notifica  RECORD
      sms                        SMALLINT,
      correo                     SMALLINT
   END RECORD 
   
   LET v_nss    = request.nss
   LET v_ax_nss = v_nss
   LET v_ax_nss = v_ax_nss CLIPPED 
   LET bnd_dato = 1

   DISPLAY "NSS : ",v_nss
   IF LENGTH(v_nss) <> 11 THEN
      LET response.CodResp = "10 NSS Inválido o Incorrecto" CLIPPED
      DISPLAY "diferente a 11"
      LET bnd_dato = 0
   ELSE
      FOR v_idx = 1 TO v_ax_nss.getLength()
         IF v_ax_nss.getCharAt(v_idx) NOT MATCHES '[0-9]' THEN
            LET response.CodResp = "10 NSS Inválido o Incorrecto" CLIPPED
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
            LET response.CodResp = "20 NSS Correcto"  CLIPPED
            LET v_bnd_resp = 1 
         ELSE
            LET response.CodResp = "11 NSS no existe en Base de Datos" CLIPPED 
         END IF
      END IF 
   END IF

   IF v_bnd_resp = 1 THEN
      LET response.nss = v_nss

      -- Se obtiene el id_derechohabiente 
      LET v_s_qry = "SELECT FIRST 1 id_derechohabiente
                       FROM afi_derechohabiente
                      WHERE nss = ? "

      PREPARE prp_c_iddh FROM v_s_qry
      EXECUTE prp_c_iddh USING v_nss 
                          INTO v_id_dh

      DISPLAY "ID_DH :",v_id_dh
      LET v_s_qry = " SELECT tpo_notificacion 
                        FROM afi_ind_notifica
                       WHERE id_derechohabiente = ? "

      PREPARE prp_cons_notifica FROM v_s_qry
      DECLARE cur_notifica CURSOR FOR prp_cons_notifica

      FOREACH cur_notifica USING v_id_dh INTO v_tpo_notificacion
         IF v_tpo_notificacion IS NULL THEN
            LET response.CodResp = "000 - SIN NOTIFICACIONES REGISTRADAS"
            LET response.sms    = 0
            LET response.correo = 0
            EXIT FOREACH
         END IF

         DISPLAY "tipo notificacion : ",v_tpo_notificacion
         CASE v_tpo_notificacion
            WHEN 1
               LET v_ax_rec_notifica.sms    = 1
            WHEN 2
               LET v_ax_rec_notifica.correo = 1
{            WHEN 3
               LET v_ax_rec_notifica.sms    = 0
            WHEN 4
               LET v_ax_rec_notifica.correo = 0} 
         END CASE
      END FOREACH

      LET response.sms    = v_ax_rec_notifica.sms
      LET response.correo = v_ax_rec_notifica.correo

   END IF
END FUNCTION
