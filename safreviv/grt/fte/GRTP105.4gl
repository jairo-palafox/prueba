#######################################################################
#Modulo              => GRT                                           #
#Programa            => GRTC107                                       #
#Objetivo            => VALIDACIONES DE ESTRUCTURA PARA               #
#                       DATOS DE ENTRADA ARCHIVO 43 BIS               #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 04 de FEBRERO 2016                            #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario       CHAR(20)
   DEFINE p_tpo_ejecucion SMALLINT
   DEFINE p_s_titulo      CHAR(20)

   DEFINE z INTEGER
   DEFINE v_dato CHAR(1)
   DEFINE bnd_dato SMALLINT
   DEFINE bnd_nss SMALLINT

   DEFINE arr_tmp DYNAMIC ARRAY OF RECORD
tpo_registro         char(1) ,
subproceso           char(3) ,
tpo_envio            char(1) ,
f_envio              char(8) ,
cve_ent_financiera   char(3) ,
nss                  char(11),
num_ctrl_ef          char(18),
rfc                  char(13),
curp                 char(18),
ap_paterno_af        char(40),
ap_materno_af        char(40),
nombre_af            char(40),
num_bimestres        char(3) ,
viv92                char(8) ,
viv97                char(8) ,
f_corte_subcuenta    char(8) ,
diagnostico          char(2) ,
inconsistencias      char(40),
num_escritura        char(8) ,
num_notario          char(4) ,
ent_fed_notario      char(2) ,
municipio_notario    char(3) ,
num_rpp              char(15),
folio_real           char(8) ,
partida              char(6) ,
foja                 char(8) ,
volumen              char(6) ,
libro                char(6) ,
tomo                 char(6) ,
seccion              char(6) ,
ent_fed_inmueble     char(2) ,
domicilio_inmueble   char(30),
valor_avaluo         char(15),
monto_credito        char(15),
plazo_credito        char(5) ,
tpo_moneda           char(2) ,
tasa_base            char(20),
margen               char(20),
f_otorga_cred_ef     char(8) ,
imp_solic_uti_grt    char(15),
f_venc_imp_solic     char(8) ,
imp_utilizado_grt    char(15),
imp_aport_subsec     char(15),
bim_apor_subsec      char(6) ,
imp_subsec_dev       char(15),
f_libera_garantia    char(8) ,
imp_grt_devuelto     char(15),
causa_liquidacion    char(1) ,
f_deposito           char(8) ,
cred_convenidos      char(1) ,
solic_saldo          char(1) ,
f_vigencia           char(8) ,
filler               char(64) 
   END RECORD

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".GRTC107.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   CALL fn_valida_datos()

END MAIN

FUNCTION fn_valida_datos()

   DEFINE v_qry_tmp STRING
   DEFINE a INTEGER

   LET v_qry_tmp = "SELECT * FROM safre_tmp:tmp_rec_det_grt43"

   PREPARE prp_tmp FROM v_qry_tmp
   DECLARE cur_tmp CURSOR FOR prp_tmp

   LET a=1
   FOREACH cur_tmp INTO arr_tmp[a].*
      LET a=a+1
   END FOREACH

   CALL arr_tmp.deleteElement(a)

   FOR a = 1 TO arr_tmp.getLength()
      CASE
         WHEN arr_tmp[a].subproceso = 001
            CALL fn_valida_tramite(a)
         WHEN arr_tmp[a].subproceso = 002
            CALL fn_valida_formalizacion(a)
         WHEN arr_tmp[a].subproceso = 003
            CALL fn_valida_garantia(a)
         WHEN arr_tmp[a].subproceso = 005
            CALL fn_valida_liquidacion(a)
      END CASE
   END FOR

END FUNCTION

FUNCTION fn_valida_tramite(a)

   DEFINE a INTEGER

END FUNCTION

FUNCTION fn_valida_formalizacion(a)

   DEFINE a INTEGER
   DEFINE bnd_cve_ent_fin     SMALLINT
   DEFINE bnd_ent_fed_notario SMALLINT
   DEFINE bnd_folio_real      SMALLINT
   DEFINE bnd_seccion         SMALLINT
   DEFINE bnd_f_otorga        SMALLINT
   DEFINE bnd_f_vigencia      SMALLINT

   LET bnd_nss             = 0
   LET bnd_cve_ent_fin     = 0
   LET bnd_ent_fed_notario = 0
   LET bnd_folio_real      = 0
   LET bnd_seccion         = 0
   LET bnd_f_otorga        = 0
   LET bnd_f_vigencia      = 0

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].nss)
      LET v_dato = arr_tmp[a].nss[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_nss = 1
         EXIT FOR
      ELSE
         LET bnd_nss = 0
      END IF
   END FOR

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].cve_ent_financiera)
      LET v_dato = arr_tmp[a].cve_ent_financiera[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_cve_ent_fin = 1
         EXIT FOR
      ELSE
         LET bnd_cve_ent_fin = 0
      END IF
   END FOR

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].ent_fed_notario)
      LET v_dato = arr_tmp[a].ent_fed_notario[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_ent_fed_notario = 1
         EXIT FOR
      ELSE
         LET bnd_ent_fed_notario = 0
      END IF
   END FOR

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].folio_real)
      LET v_dato = arr_tmp[a].folio_real[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_folio_real = 1
         EXIT FOR
      ELSE
         LET bnd_folio_real = 0
      END IF
   END FOR

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].seccion)
      LET v_dato = arr_tmp[a].seccion[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_seccion = 1
         EXIT FOR
      ELSE
         LET bnd_seccion = 0
      END IF
   END FOR

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].f_otorga_cred_ef)
      LET v_dato = arr_tmp[a].f_otorga_cred_ef[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_f_otorga = 1
         EXIT FOR
      ELSE
         LET bnd_f_otorga = 0
      END IF
   END FOR

   IF bnd_f_otorga = 0 THEN
      IF arr_tmp[a].f_otorga_cred_ef[5,6] <= 12 THEN
         LET bnd_f_otorga = 0
      ELSE
         LET bnd_f_otorga = 1
      END IF

      IF arr_tmp[a].f_otorga_cred_ef[7,8] <= 31 THEN
         LET bnd_f_otorga = 0
      ELSE
         LET bnd_f_otorga = 1
      END IF
   END IF

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO  LENGTH (arr_tmp[a].f_vigencia)
      LET v_dato = arr_tmp[a].f_vigencia[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_f_vigencia = 1
         EXIT FOR
      ELSE
         LET bnd_f_vigencia = 0
      END IF
   END FOR

   IF bnd_f_otorga = 0 THEN
      IF arr_tmp[a].f_vigencia[5,6] <= 12 THEN
         LET bnd_f_vigencia = 0
      ELSE
         LET bnd_f_vigencia = 1
      END IF

      IF arr_tmp[a].f_vigencia[7,8] <= 31 THEN
         LET bnd_f_vigencia = 0
      ELSE
         LET bnd_f_vigencia = 1
      END IF
   END IF

   IF (bnd_nss             = 0) AND
      (bnd_cve_ent_fin     = 0) AND
      (bnd_ent_fed_notario = 0) AND
      (bnd_folio_real      = 0) AND
      (bnd_seccion         = 0) AND
      (bnd_f_otorga        = 0) AND
      (bnd_f_vigencia      = 0) THEN

      

   END IF

END FUNCTION

FUNCTION fn_valida_garantia(a)

   DEFINE a INTEGER
END FUNCTION

FUNCTION fn_valida_liquidacion(a)

   DEFINE a INTEGER
END FUNCTION

FUNCTION fn_valida_numero(v_dato)

   DEFINE v_dato CHAR(1)

   LET bnd_dato = 0
   IF v_dato MATCHES '[0-9]*' THEN
      LET bnd_dato = 0
   ELSE
      LET bnd_dato = 1
   END IF

END FUNCTION