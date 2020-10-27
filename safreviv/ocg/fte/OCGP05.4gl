#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGP05                                        #
#Objetivo            => VALIDACIONES DE ESTRUCTURA PARA               #
#                       DATOS DE ENTRADA ARCHIVO 43 BIS               #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 04 de FEBRERO 2016                            #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario             CHAR(20)
   DEFINE p_tpo_ejecucion       SMALLINT
   DEFINE p_s_titulo            CHAR(20)
   DEFINE z                     INTEGER
   DEFINE v_dato                CHAR(1)
   DEFINE bnd_dato              SMALLINT
   DEFINE bnd_nss               SMALLINT
   DEFINE bnd_cve_ent_fin       SMALLINT
   DEFINE bnd_ent_fed_notario   SMALLINT
   DEFINE bnd_folio_real        SMALLINT
   DEFINE bnd_seccion           SMALLINT
   DEFINE bnd_f_otorga          SMALLINT
   DEFINE bnd_f_vigencia        SMALLINT
   DEFINE bnd_imp_solic_uti_grt SMALLINT
   DEFINE bnd_f_venc_imp_solic  SMALLINT
   DEFINE bnd_solic_saldo       SMALLINT
   DEFINE bnd_f_libera_garantia SMALLINT
   DEFINE bnd_f_deposito        SMALLINT
   DEFINE bnd_bim_apor_subsec   SMALLINT
   DEFINE bnd_fecha             SMALLINT
   DEFINE v_fecha               CHAR(8)
   DEFINE bnd_f_liberacion      SMALLINT
   DEFINE bnd_bimestre          SMALLINT
   DEFINE v_seq                 DECIMAL(9,0)
   DEFINE v_id_ocg_ctr_archivo  DECIMAL(9,0)
   DEFINE bnd_imp_grt           SMALLINT
   DEFINE bnd_imp_subsec        SMALLINT
   DEFINE bnd_ld                SMALLINT
   DEFINE bnd_monto             SMALLINT
   DEFINE bnd_avaluo            SMALLINT
   DEFINE bnd_num_notario       SMALLINT
   DEFINE bnd_ent_inmueble      SMALLINT
   DEFINE bnd_plazo_cred        SMALLINT
   DEFINE bnd_tpo_moneda        SMALLINT


   DEFINE arr_tmp DYNAMIC ARRAY OF RECORD
          id_tmp_det           DECIMAL(9,0),
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

   DEFINE arr_rechazo DYNAMIC ARRAY OF RECORD
          id_tmp_det           DECIMAL(9,0),
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

   --record para cifras control de conteo
   DEFINE r_cifras_control RECORD
      tot_reg_arh INTEGER,
      tot_sp1_arh INTEGER, 
      tot_sp2_arh INTEGER,
      tot_sp3_arh INTEGER,
      tot_sp5_arh INTEGER,
      tot_sp1_rch_val INTEGER,
      tot_sp2_rch_val INTEGER, 
      tot_sp3_rch_val INTEGER,
      tot_sp5_rch_val INTEGER   
   END RECORD
      
END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parametro 
   LET p_usuario            = ARG_VAL(1)
   LET p_tpo_ejecucion      = ARG_VAL(2)
   LET p_s_titulo           = ARG_VAL(3)
   LET v_id_ocg_ctr_archivo = ARG_VAL(4)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP05.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   IF v_id_ocg_ctr_archivo IS NULL THEN
      LET v_id_ocg_ctr_archivo = 0
   END IF

   LET bnd_fecha = 1
   CALL fn_valida_datos()
   --LET v_fecha = "01132016"
   --CALL fn_valida_fecha(v_fecha)

END MAIN

FUNCTION fn_valida_datos()

   DEFINE v_qry_tmp STRING
   DEFINE a INTEGER
   DEFINE v_nss CHAR(11)

   --Crea tabla temporal para cifras control
   CALL fn_crea_temporal()

   DATABASE safre_tmp

   ALTER TABLE tmp_rec_det_ocg43 ADD ( id_tmp_det DECIMAL(9,0) BEFORE tpo_registro)

   -- Total registros cargados originalmente
   LET v_qry_tmp = "SELECT COUNT(*)
                     FROM tmp_rec_det_ocg43"

   PREPARE prp_tot_reg FROM v_qry_tmp
   EXECUTE prp_tot_reg INTO r_cifras_control.tot_reg_arh
   
   -- Recupera totales por cada subproceso cargado
   LET v_qry_tmp = "SELECT COUNT(*)
                     FROM tmp_rec_det_ocg43
                    WHERE subproceso = 1;"

   PREPARE prp_sp1 FROM v_qry_tmp
   EXECUTE prp_sp1 INTO r_cifras_control.tot_sp1_arh
 
   LET v_qry_tmp = "SELECT COUNT(*)
                     FROM tmp_rec_det_ocg43
                    WHERE subproceso = 2;"

   PREPARE prp_sp2 FROM v_qry_tmp
   EXECUTE prp_sp2 INTO r_cifras_control.tot_sp2_arh

   LET v_qry_tmp = "SELECT COUNT(*)
                     FROM tmp_rec_det_ocg43
                    WHERE subproceso = 3;"

   PREPARE prp_sp3 FROM v_qry_tmp
   EXECUTE prp_sp3 INTO r_cifras_control.tot_sp3_arh

   LET v_qry_tmp = "SELECT COUNT(*)
                     FROM tmp_rec_det_ocg43
                    WHERE subproceso = 5;"

   PREPARE prp_sp5 FROM v_qry_tmp
   EXECUTE prp_sp5 INTO r_cifras_control.tot_sp5_arh

   DATABASE safre_viv

   LET v_qry_tmp = "SELECT * FROM safre_tmp:tmp_rec_det_ocg43 for update;"

   PREPARE prp_tmp FROM v_qry_tmp
   DECLARE cur_tmp CURSOR FOR prp_tmp

   LET a=1
   FOREACH cur_tmp INTO arr_tmp[a].*

      IF arr_tmp[a].id_tmp_det          = " " THEN 
         LET  arr_tmp[a].id_tmp_det          = NULL 
      END IF
  
      IF arr_tmp[a].tpo_registro        = " " THEN 
         LET  arr_tmp[a].tpo_registro        = NULL 
      END IF
  
      IF arr_tmp[a].subproceso          = " " THEN 
         LET  arr_tmp[a].subproceso          = NULL 
      END IF
  
      IF arr_tmp[a].tpo_envio           = " " THEN 
         LET  arr_tmp[a].tpo_envio           = NULL 
      END IF
  
      IF arr_tmp[a].f_envio             = " " OR 
         arr_tmp[a].f_envio             = "00000000" THEN 
         LET  arr_tmp[a].f_envio             = NULL 
      END IF
  
      IF arr_tmp[a].cve_ent_financiera  = " " THEN 
         LET  arr_tmp[a].cve_ent_financiera  = NULL 
      END IF
  
      IF arr_tmp[a].nss                 = " " THEN 
         LET  arr_tmp[a].nss                 = NULL 
      END IF
  
      IF arr_tmp[a].num_ctrl_ef         = " " THEN 
         LET  arr_tmp[a].num_ctrl_ef         = NULL 
      END IF
  
      IF arr_tmp[a].rfc                 = " " THEN 
         LET  arr_tmp[a].rfc                 = NULL 
      END IF
  
      IF arr_tmp[a].curp                = " " THEN 
         LET  arr_tmp[a].curp                = NULL 
      END IF
  
      IF arr_tmp[a].ap_paterno_af       = " " THEN 
         LET  arr_tmp[a].ap_paterno_af       = NULL 
      END IF
  
      IF arr_tmp[a].ap_materno_af       = " " THEN 
         LET  arr_tmp[a].ap_materno_af       = NULL 
      END IF
  
      IF arr_tmp[a].nombre_af           = " " THEN 
         LET  arr_tmp[a].nombre_af           = NULL 
      END IF
  
      IF arr_tmp[a].num_bimestres       = " " THEN 
         LET  arr_tmp[a].num_bimestres       = NULL 
      END IF
  
      IF arr_tmp[a].viv92               = " " THEN 
         LET  arr_tmp[a].viv92               = NULL 
      END IF
  
      IF arr_tmp[a].viv97               = " " THEN 
         LET  arr_tmp[a].viv97               = NULL 
      END IF
  
      IF arr_tmp[a].f_corte_subcuenta   = " "  OR 
         arr_tmp[a].f_corte_subcuenta   = "00000000" THEN 
         LET  arr_tmp[a].f_corte_subcuenta   = NULL 
      END IF
  
      IF arr_tmp[a].diagnostico         = " " THEN 
         LET  arr_tmp[a].diagnostico         = NULL 
      END IF
  
      IF arr_tmp[a].inconsistencias     = " " THEN 
         LET  arr_tmp[a].inconsistencias     = NULL 
      END IF
  
      IF arr_tmp[a].num_escritura       = " " THEN 
         LET  arr_tmp[a].num_escritura       = NULL 
      END IF
  
      IF arr_tmp[a].num_notario         = " " THEN 
         LET  arr_tmp[a].num_notario         = NULL 
      END IF
  
      IF arr_tmp[a].ent_fed_notario     = " " THEN 
         LET  arr_tmp[a].ent_fed_notario     = NULL 
      END IF
  
      IF arr_tmp[a].municipio_notario   = " " THEN 
         LET  arr_tmp[a].municipio_notario   = NULL
      END IF
       
      IF arr_tmp[a].num_rpp             = " " THEN 
         LET  arr_tmp[a].num_rpp             = NULL 
      END IF
  
      IF arr_tmp[a].folio_real          = " " THEN 
         LET  arr_tmp[a].folio_real          = NULL 
      END IF
  
      IF arr_tmp[a].partida             = " " THEN 
         LET  arr_tmp[a].partida             = NULL 
      END IF
  
      IF arr_tmp[a].foja                = " " THEN 
         LET  arr_tmp[a].foja                = NULL 
      END IF
  
      IF arr_tmp[a].volumen             = " " THEN 
         LET  arr_tmp[a].volumen             = NULL
      END IF
  
      IF arr_tmp[a].libro               = " " THEN 
         LET  arr_tmp[a].libro               = NULL 
      END IF
  
      IF arr_tmp[a].tomo                = " " THEN 
         LET  arr_tmp[a].tomo                = NULL 
      END IF
  
      IF arr_tmp[a].seccion             = " " THEN 
         LET  arr_tmp[a].seccion             = NULL 
      END IF
  
      IF arr_tmp[a].ent_fed_inmueble    = " " THEN 
         LET  arr_tmp[a].ent_fed_inmueble    = NULL 
      END IF
  
      IF arr_tmp[a].domicilio_inmueble  = " " THEN 
         LET  arr_tmp[a].domicilio_inmueble  = NULL 
      END IF
  
      IF arr_tmp[a].valor_avaluo        = " " THEN 
         LET  arr_tmp[a].valor_avaluo        = NULL 
      END IF
  
      IF arr_tmp[a].monto_credito       = " " THEN 
         LET  arr_tmp[a].monto_credito       = NULL 
      END IF
  
      IF arr_tmp[a].plazo_credito       = " " THEN 
        LET  arr_tmp[a].plazo_credito       = NULL 
      END IF
  
      IF arr_tmp[a].tpo_moneda          = " " THEN 
         LET  arr_tmp[a].tpo_moneda          = NULL 
      END IF
  
      IF arr_tmp[a].tasa_base           = " " THEN 
         LET  arr_tmp[a].tasa_base           = NULL 
      END IF
  
      IF arr_tmp[a].margen              = " " THEN 
         LET  arr_tmp[a].margen              = NULL 
      END IF
  
      IF arr_tmp[a].f_otorga_cred_ef    = " " OR 
         arr_tmp[a].f_otorga_cred_ef    = "00000000" THEN 
         LET  arr_tmp[a].f_otorga_cred_ef    = NULL 
      END IF
  
      IF arr_tmp[a].imp_solic_uti_grt   = " " THEN 
         LET  arr_tmp[a].imp_solic_uti_grt   = NULL 
      END IF
  
      IF arr_tmp[a].f_venc_imp_solic    = " " OR 
         arr_tmp[a].f_venc_imp_solic    = "00000000" THEN 
         LET  arr_tmp[a].f_venc_imp_solic    = NULL 
      END IF
  
      IF arr_tmp[a].imp_utilizado_grt   = " " THEN 
         LET  arr_tmp[a].imp_utilizado_grt   = NULL 
      END IF
  
      IF arr_tmp[a].imp_aport_subsec    = " " THEN 
         LET  arr_tmp[a].imp_aport_subsec    = NULL 
      END IF
  
      IF arr_tmp[a].bim_apor_subsec     = " " THEN 
         LET  arr_tmp[a].bim_apor_subsec     = NULL 
      END IF
  
      IF arr_tmp[a].imp_subsec_dev      = " " THEN 
         LET  arr_tmp[a].imp_subsec_dev      = NULL 
      END IF
  
      IF arr_tmp[a].f_libera_garantia   = " " OR 
         arr_tmp[a].f_libera_garantia   = "00000000" THEN 
         LET  arr_tmp[a].f_libera_garantia   = NULL 
      END IF
  
      IF arr_tmp[a].imp_grt_devuelto    = " " THEN 
         LET  arr_tmp[a].imp_grt_devuelto    = NULL 
      END IF
  
      IF arr_tmp[a].causa_liquidacion   = " " THEN 
         LET  arr_tmp[a].causa_liquidacion   = NULL 
      END IF
  
      IF arr_tmp[a].f_deposito          = " "  OR 
         arr_tmp[a].f_deposito          = "00000000" THEN 
         LET  arr_tmp[a].f_deposito          = NULL 
      END IF
  
      IF arr_tmp[a].cred_convenidos     = " " THEN 
         LET  arr_tmp[a].cred_convenidos     = NULL 
      END IF
  
      IF arr_tmp[a].solic_saldo         = " " THEN 
         LET  arr_tmp[a].solic_saldo         = NULL 
      END IF
  
      IF arr_tmp[a].f_vigencia          = " "  OR 
         arr_tmp[a].f_vigencia          = "00000000" THEN 
         LET  arr_tmp[a].f_vigencia          = NULL 
      END IF
  
      IF arr_tmp[a].filler              = " " THEN 
         LET  arr_tmp[a].filler              = NULL 
      END IF

      SELECT FIRST 1 nss,
             seq_ocg_rechazo.NEXTVAL
      INTO v_nss,
           v_seq
        FROM safre_tmp:tmp_rec_det_ocg43

        --DISPLAY "id_secuencia : ",v_seq

      UPDATE safre_tmp:tmp_rec_det_ocg43
        SET id_tmp_det = v_seq
        WHERE CURRENT OF cur_tmp

      LET arr_tmp[a].id_tmp_det = v_seq
        
      LET a=a+1
   END FOREACH

   --DATABASE safre_viv

   -- Inicializa contadores de rechazos en validación
   LET r_cifras_control.tot_sp1_rch_val = 0
   LET r_cifras_control.tot_sp2_rch_val = 0
   LET r_cifras_control.tot_sp3_rch_val = 0
   LET r_cifras_control.tot_sp5_rch_val = 0

   CALL arr_tmp.deleteElement(a)

   FOR a = 1 TO arr_tmp.getLength()

   --DISPLAY "subproceso : ",arr_tmp[a].subproceso

      LET bnd_cve_ent_fin       = 0
      LET bnd_ent_fed_notario   = 0
      LET bnd_folio_real        = 0
      LET bnd_seccion           = 0
      LET bnd_f_otorga          = 0
      LET bnd_f_vigencia        = 0
      LET bnd_imp_solic_uti_grt = 0
      LET bnd_f_venc_imp_solic  = 0

      LET bnd_fecha = 0
      CASE
         WHEN arr_tmp[a].subproceso = "001"
            CALL fn_valida_tramite(a)
         WHEN arr_tmp[a].subproceso = "002"
            CALL fn_valida_formalizacion(a)
         WHEN arr_tmp[a].subproceso = "003"
            CALL fn_valida_garantia(a)
         WHEN arr_tmp[a].subproceso = "005"
            CALL fn_valida_liquidacion(a)
      END CASE
   END FOR

   -- Guarda cifras control
   INSERT INTO safre_tmp:tmp_ocg_cifras (
                             tot_reg_archivo,
                             tot_sp1_arh,
                             tot_sp2_arh,
                             tot_sp3_arh,
                             tot_sp5_arh,
                             tot_sp1_rch_val,
                             tot_sp2_rch_val,
                             tot_sp3_rch_Val,
                             tot_sp5_rch_val)
                    VALUES (r_cifras_control.tot_reg_arh,
                             r_cifras_control.tot_sp1_arh,
                             r_cifras_control.tot_sp2_arh,
                             r_cifras_control.tot_sp3_arh,
                             r_cifras_control.tot_sp5_arh,
                             r_cifras_control.tot_sp1_rch_val,
                             r_cifras_control.tot_sp2_rch_val,
                             r_cifras_control.tot_sp3_rch_val,
                             r_cifras_control.tot_sp5_rch_val); 
END FUNCTION

FUNCTION fn_valida_tramite(a)

   DEFINE a INTEGER

   LET bnd_nss             = 0
   LET bnd_cve_ent_fin     = 0

   CALL fn_valida_nss(a)
   CALL fn_cve_ent_fin(a)

   IF (bnd_nss = 1) OR
      (bnd_cve_ent_fin = 1) THEN
      CALL fn_borra_registro(a)
   END IF

END FUNCTION

FUNCTION fn_valida_formalizacion(a)

   DEFINE a                   INTEGER

   LET bnd_nss             = 0
   LET bnd_cve_ent_fin     = 0
   LET bnd_ent_fed_notario = 0
   LET bnd_folio_real      = 0
   LET bnd_seccion         = 0
   LET bnd_f_otorga        = 0
   LET bnd_f_vigencia      = 0
   LET bnd_avaluo          = 0
   LET bnd_num_notario     = 0
   LET bnd_ent_inmueble    = 0
   LET bnd_plazo_cred      = 0
   LET bnd_tpo_moneda      = 0

   CALL fn_valida_nss(a)
   CALL fn_cve_ent_fin(a)

   IF (bnd_nss = 0) AND
      (bnd_cve_ent_fin = 0) THEN

      IF arr_tmp[a].f_otorga_cred_ef is not null THEN
         LET v_fecha = arr_tmp[a].f_otorga_cred_ef[5,6],
                       arr_tmp[a].f_otorga_cred_ef[7,8],
                       arr_tmp[a].f_otorga_cred_ef[1,4]
         CALL fn_valida_fecha(v_fecha)
         IF bnd_fecha = 0 THEN
            LET bnd_f_otorga = 0
         ELSE
            LET bnd_f_otorga = 1
         END IF
      END IF
   END IF

   CALL fn_valida_monto(arr_tmp[a].valor_avaluo)

   --DISPLAY "nss  : ",arr_tmp[a].nss

      --DISPLAY "bnd_monto : ",bnd_monto
      IF bnd_monto = 0 THEN
         LET bnd_avaluo = 0
      ELSE
         LET bnd_avaluo = 1
      END IF

   LET bnd_monto = 0

   CALL fn_valida_monto(arr_tmp[a].monto_credito)

   FOR z = 1 TO  LENGTH (arr_tmp[a].num_notario)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].num_notario[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
         LET bnd_num_notario = 0
         CONTINUE FOR
      ELSE
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 1 THEN
            LET bnd_num_notario = 1
            EXIT FOR
         ELSE
            LET bnd_num_notario = 0
         END IF
      END IF
   END FOR

   FOR z = 1 TO  LENGTH (arr_tmp[a].municipio_notario)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].municipio_notario[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
         LET bnd_ent_fed_notario = 0
         CONTINUE FOR
      ELSE
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 1 THEN
            LET bnd_ent_fed_notario = 1
            EXIT FOR
         ELSE
            LET bnd_ent_fed_notario = 0
         END IF
      END IF
   END FOR

   FOR z = 1 TO  LENGTH (arr_tmp[a].ent_fed_inmueble)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].ent_fed_inmueble[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
         LET bnd_ent_inmueble = 0
         CONTINUE FOR
      ELSE
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 1 THEN
            LET bnd_ent_inmueble = 1
            EXIT FOR
         ELSE
            LET bnd_ent_inmueble = 0
         END IF
      END IF
   END FOR

   FOR z = 1 TO  LENGTH (arr_tmp[a].plazo_credito)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].plazo_credito[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
         LET bnd_plazo_cred = 0
         CONTINUE FOR
      ELSE
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 1 THEN
            LET bnd_plazo_cred = 1
            EXIT FOR
         ELSE
            LET bnd_plazo_cred = 0
         END IF
      END IF
   END FOR

   FOR z = 1 TO  LENGTH (arr_tmp[a].tpo_moneda)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].tpo_moneda[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
         LET bnd_tpo_moneda = 0
         CONTINUE FOR
      ELSE
         CALL fn_valida_numero(v_dato)
         IF bnd_dato = 1 THEN
            LET bnd_tpo_moneda = 1
            EXIT FOR
         ELSE
            LET bnd_tpo_moneda = 0
         END IF
      END IF
   END FOR


   --DISPLAY "bnd_avaluo : ",bnd_avaluo
   --DISPLAY "bnd_monto  : ",bnd_monto
      
   IF (bnd_nss             = 0) AND
      (bnd_cve_ent_fin     = 0) AND
      (bnd_ent_fed_notario = 0) AND
      (bnd_folio_real      = 0) AND
      (bnd_seccion         = 0) AND
      (bnd_monto           = 0) AND
      (bnd_avaluo          = 0) AND
      (bnd_fecha           = 0) AND
      (bnd_num_notario     = 0) AND
      (bnd_ent_inmueble    = 0) AND
      (bnd_plazo_cred      = 0) AND
      (bnd_tpo_moneda      = 0) THEN
   ELSE
      CALL fn_borra_registro(a)
   END IF

END FUNCTION

FUNCTION fn_valida_garantia(a)

   DEFINE a INTEGER

   LET bnd_nss                = 0
   LET bnd_cve_ent_fin        = 0
   LET bnd_imp_solic_uti_grt  = 0
   LET bnd_f_venc_imp_solic   = 0

   --DISPLAY arr_tmp[a].f_venc_imp_solic
   --DISPLAY arr_tmp[a].solic_saldo
   --DISPLAY arr_tmp[a].imp_solic_uti_grt

   CALL fn_valida_nss(a)
   CALL fn_cve_ent_fin(a)

   IF (bnd_nss = 0) AND
      (bnd_cve_ent_fin = 0)THEN

      IF (bnd_nss = 0) AND
         (bnd_cve_ent_fin = 0) THEN
         LET v_fecha = arr_tmp[a].f_venc_imp_solic[5,6],
                       arr_tmp[a].f_venc_imp_solic[7,8],
                       arr_tmp[a].f_venc_imp_solic[1,4]
         CALL fn_valida_fecha(v_fecha)
         IF bnd_fecha = 0 THEN
            LET bnd_f_venc_imp_solic = 0
         ELSE
            LET bnd_f_venc_imp_solic = 1
         END IF
      END IF
   END IF

   IF bnd_fecha = 0 THEN
      IF (arr_tmp[a].solic_saldo = 1) OR
         (arr_tmp[a].solic_saldo = 2) THEN
         LET bnd_solic_saldo = 0
      ELSE
       --  LET bnd_solic_saldo = 1
      END IF
   END IF

   LET bnd_dato = 0
   LET z = 1
   --DISPLAY arr_tmp[a].*
  -- DISPLAY "importe solicitado : ",arr_tmp[a].imp_solic_uti_grt
   FOR z = 1 TO  LENGTH (arr_tmp[a].imp_solic_uti_grt)
      LET v_dato = NULL
      LET v_dato = arr_tmp[a].imp_solic_uti_grt[z,z]
      IF (v_dato IS NULL) OR
         (v_dato = " ") THEN
          LET bnd_imp_solic_uti_grt = 0
          CONTINUE FOR
          ELSE
          CALL fn_valida_numero(v_dato)
          IF bnd_dato = 1 THEN
             LET bnd_imp_solic_uti_grt = 1
              EXIT FOR
          ELSE
              LET bnd_imp_solic_uti_grt = 0
          END IF
      END IF
      IF arr_tmp[a].imp_solic_uti_grt > "99999999999999" THEN 
         LET bnd_imp_solic_uti_grt = 1
      END IF
   END FOR

   IF (bnd_nss               = 0) AND
      (bnd_cve_ent_fin       = 0) AND
      (bnd_solic_saldo       = 0) AND
      (bnd_imp_solic_uti_grt = 0) AND
      (bnd_fecha             = 0) THEN
   ELSE
      CALL fn_borra_registro(a)
   END IF

END FUNCTION

FUNCTION fn_valida_liquidacion(a)

   DEFINE a                INTEGER
   DEFINE bnd_dev          SMALLINT
   DEFINE bnd_liq          SMALLINT

   LET bnd_nss             = 0
   LET bnd_cve_ent_fin     = 0
   LET bnd_fecha           = 0
   LET bnd_imp_grt         = 0
   LET bnd_imp_subsec      = 0
   LET bnd_f_liberacion    = 0
   LET bnd_ld              = 0
   LET bnd_liq             = 0
   LET bnd_dev             = 0
   LET bnd_fecha           = 0
   LET bnd_f_deposito      = 0
   LET bnd_bimestre        = 0


   CALL fn_valida_nss(a)
   CALL fn_cve_ent_fin(a)

      
      IF (bnd_nss = 0) AND
         (bnd_cve_ent_fin = 0) THEN

         IF arr_tmp[a].f_deposito = " " THEN
            LET arr_tmp[a].f_deposito = NULL
         END IF
         
         --IF arr_tmp[a].f_libera_garantia IS NOT NULL AND
            --arr_tmp[a].f_deposito IS NOT NULL THEN

            --DISPLAY "f_libera null, f_dep null"
            --LET bnd_ld = 1
         --ELSE 

         --DISPLAY "f_dep    : ", arr_tmp[a].f_deposito
         --DISPLAY "f_libera : ", arr_tmp[a].f_libera_garantia
         IF arr_tmp[a].f_libera_garantia IS NOT NULL THEN
            LET bnd_liq = 1
         ELSE
            LET bnd_dev = 1
            --DISPLAY "f_dep null"
         END IF

         IF arr_tmp[a].f_libera_garantia IS NOT NULL AND 
            arr_tmp[a].f_deposito IS NOT NULL THEN
            LET bnd_liq = 1
            LET bnd_dev = 1
         END IF

         --DISPLAY "bnd_liq : ",bnd_liq
         --DISPLAY "bnd_dev : ",bnd_dev

         --END IF
         --*****
      END IF

   IF bnd_liq = 1 THEN
      --DISPLAY "f_garantía tabla : ",arr_tmp[a].f_libera_garantia
      LET v_fecha = arr_tmp[a].f_libera_garantia[5,6],
                    arr_tmp[a].f_libera_garantia[7,8],
                    arr_tmp[a].f_libera_garantia[1,4]
      --DISPLAY "fecha:  ",v_fecha
      CALL fn_valida_fecha(v_fecha)
      --DISPLAY "Bandera fecha : ",bnd_fecha
      IF bnd_fecha = 0 THEN
         LET bnd_f_liberacion = 0
      ELSE
         LET bnd_f_liberacion = 1
      END IF
   END IF

   IF bnd_dev = 1 THEN
      LET v_fecha = arr_tmp[a].f_deposito[5,6],
                    arr_tmp[a].f_deposito[7,8],
                    arr_tmp[a].f_deposito[1,4]
      CALL fn_valida_fecha(v_fecha)
      IF bnd_fecha = 0 THEN
         LET bnd_f_deposito = 0
      ELSE
         LET bnd_f_deposito = 1
      END IF
   END IF

   IF (bnd_fecha = 0) AND (bnd_dev = 1) THEN
      LET v_fecha = arr_tmp[a].bim_apor_subsec[5,6],
                    '01',
                    arr_tmp[a].bim_apor_subsec[1,4]
      CALL fn_valida_fecha(v_fecha)
      IF bnd_fecha = 0 THEN
         LET bnd_bimestre = 0
      ELSE
         LET bnd_bimestre = 1
      END IF

      IF bnd_bimestre = 0 THEN

         LET arr_tmp[a].imp_subsec_dev    = arr_tmp[a].imp_subsec_dev CLIPPED
         LET arr_tmp[a].imp_grt_devuelto  = arr_tmp[a].imp_grt_devuelto CLIPPED

         --DISPLAY "imp subsec : ",arr_tmp[a].imp_subsec_dev
         --DISPLAY "imp grt    : ",arr_tmp[a].imp_grt_devuelto

            LET bnd_dato = 0
            LET z = 1
            FOR z = 1 TO  LENGTH (arr_tmp[a].imp_grt_devuelto)
               LET v_dato = NULL
               LET v_dato = arr_tmp[a].imp_grt_devuelto[z,z]
               IF (v_dato IS NULL) OR
                  (v_dato = " ") THEN
                  LET bnd_imp_grt = 0
                  CONTINUE FOR
               ELSE
                  CALL fn_valida_numero(v_dato)
                  IF bnd_dato = 1 THEN
                     LET bnd_imp_grt = 1
                     EXIT FOR
                  ELSE
                     LET bnd_imp_grt = 0
                  END IF
               END IF
            END FOR

            LET bnd_dato = 0
            LET z = 1
            FOR z = 1 TO  LENGTH (arr_tmp[a].imp_subsec_dev)
               LET v_dato = NULL
               LET v_dato = arr_tmp[a].imp_subsec_dev[z,z]
               IF (v_dato IS NULL) OR
                  (v_dato = " ") THEN
                  LET bnd_imp_subsec = 0
                  CONTINUE FOR
               ELSE
                  CALL fn_valida_numero(v_dato)
                  IF bnd_dato = 1 THEN
                     LET bnd_imp_subsec = 1
                     EXIT FOR
                  ELSE
                     LET bnd_imp_subsec = 0
                  END IF
               END IF
            END FOR

      END IF
   END IF

   IF (bnd_nss               = 0) AND
      (bnd_cve_ent_fin       = 0) AND
      (bnd_fecha             = 0) AND
      (bnd_imp_subsec        = 0) AND
      (bnd_imp_grt           = 0) AND 
      (bnd_f_liberacion      = 0) AND 
      (bnd_f_deposito        = 0) AND 
      (bnd_bimestre          = 0) THEN
   ELSE
      CALL fn_borra_registro(a)
   END IF

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

FUNCTION fn_valida_nss(a)

DEFINE a INTEGER


   LET bnd_dato = 0
   LET z = 1

   -- Valida que el nss no contenga sólo "0"
   IF(arr_tmp[a].nss = "00000000000") THEN
      --Prende la bandera para rechazar  el registro
      LET bnd_nss = 1
   ELSE 
      -- Recorre toda la cadena
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
      
   END IF 
   
END FUNCTION

FUNCTION fn_cve_ent_fin(a)

   DEFINE a INTEGER

   LET bnd_dato = 0
   LET z = 1
   FOR z = 1 TO LENGTH (arr_tmp[a].cve_ent_financiera)
      LET v_dato = arr_tmp[a].cve_ent_financiera[z,z]
      CALL fn_valida_numero(v_dato)
      IF bnd_dato = 1 THEN
         LET bnd_cve_ent_fin = 1
         EXIT FOR
      ELSE
         LET bnd_cve_ent_fin = 0
      END IF
   END FOR

END FUNCTION

FUNCTION fn_valida_monto(v_monto)
   DEFINE v_monto        CHAR(15)

   --DISPLAY "monto : ",v_monto
   LET z = 1
   FOR z = 1 TO LENGTH(v_monto)
      LET v_dato = v_monto[z,z]
      CALL fn_valida_numero(v_dato)

      IF bnd_dato = 0 THEN
         LET bnd_monto = 0
      ELSE
         LET bnd_monto = 1
         EXIT FOR
      END IF

   END FOR

END FUNCTION

FUNCTION fn_borra_registro(a)

   DEFINE a                INTEGER
   DEFINE v_inconsistencia CHAR(2)
   DEFINE v_s_qry          STRING
   DEFINE x                INTEGER

   IF bnd_num_notario      = 1  THEN 
      LET v_inconsistencia = 31 -- Numero de notario erroneo
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_tpo_moneda       = 1  THEN 
      LET v_inconsistencia = 38 -- tipo moneda erroneo  
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_ent_inmueble     = 1  THEN 
      LET v_inconsistencia = 33 -- entidada inmueble erroneo
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_plazo_cred       = 1  THEN 
      LET v_inconsistencia = 37 -- plazo crédito erroneo  
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_avaluo = 1 THEN
      LET v_inconsistencia = 35  -- valor avaluo erroneo
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_monto = 1 THEN
      LET v_inconsistencia = 36  --monto crédito erroneo
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_imp_subsec     = 1 THEN
      LET v_inconsistencia = 52 -- importe subsecuente erroeno
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_imp_grt     = 1 THEN
      LET v_inconsistencia = 56 -- importe garantia erroeno
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_bimestre     = 1 THEN
      LET v_inconsistencia = 55 -- solicitud rechazada
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_f_liberacion     = 1 THEN
      LET v_inconsistencia = 51 --trabajador no afiliado en infonavit 
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

      IF bnd_f_deposito    = 1 THEN
      LET v_inconsistencia = 28 --solicitud rechazada 
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_nss              = 1 THEN
      LET v_inconsistencia = 02 --trabajador no afiliado en infonavit 
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_cve_ent_fin      = 1  THEN 
      LET v_inconsistencia = 26 --entidad financiera diferente   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
             arr_tmp[a].subproceso,
             v_inconsistencia,
             TODAY)
   END IF

   IF bnd_ent_fed_notario  = 1  THEN 
      LET v_inconsistencia = 32 --entidad o municipio de notario erroneo   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_folio_real       = 1  THEN 
      LET v_inconsistencia = 28 -- solicitud rechazada    
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_seccion          = 1  THEN 
      LET v_inconsistencia = 28 -- solicitud rechazada   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_f_otorga         = 1  THEN 
      LET v_inconsistencia = 41 -- fecha de otorgamiento erronea  
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_f_vigencia       = 1  THEN 
      LET v_inconsistencia = 46 -- fecha de vencimiento invalida   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_imp_solic_uti_grt= 1  THEN 
      LET v_inconsistencia = 45 -- importe solicitado invalido   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   IF bnd_f_venc_imp_solic = 1  THEN 
      LET v_inconsistencia = 46 -- fecha de vencimiento invalida   
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF
   
   IF bnd_ld = 1 THEN
      LET v_inconsistencia = 58 -- fecha deposito  
      INSERT INTO ocg_inconsistencia_rch
      VALUES (arr_tmp[a].id_tmp_det,
              arr_tmp[a].subproceso,
              v_inconsistencia,
              TODAY)
   END IF

   --INSERT INTO safre_tmp:tmp_rechazo
   LET v_s_qry = "
   SELECT *
     FROM safre_tmp:tmp_rec_det_ocg43
    WHERE id_tmp_det = ",arr_tmp[a].id_tmp_det

    CALL arr_rechazo.clear()

   PREPARE prp_rechazo FROM v_s_qry
   DECLARE cur_rechazo CURSOR FOR prp_rechazo

   LET x=1
   FOREACH cur_rechazo INTO arr_rechazo[x].*
      LET arr_rechazo[x].diagnostico = "02"
   END FOREACH

   --CALL arr_rechazo.deleteElement(x)
   
  -- DISPLAY "cuenta arreglo : ",x

   --INSERT INTO safre_tmp:tmp_rechazo
   INSERT INTO ocg_rechazo_ent
       VALUES (v_id_ocg_ctr_archivo,
               arr_rechazo[x].*,
               20,
               TODAY)

   DELETE FROM safre_tmp:tmp_rec_det_ocg43
    WHERE id_tmp_det = arr_tmp[a].id_tmp_det

    -- Conteo para los rechazos
    IF(arr_rechazo[x].subproceso = "001") THEN
       --Aumenta contador de rechazos para el sp1
       LET r_cifras_control.tot_sp1_rch_val = r_cifras_control.tot_sp1_rch_val + 1
    END IF 

    IF(arr_rechazo[x].subproceso = "002") THEN
       --Aumenta contador de rechazos para el sp1
       LET r_cifras_control.tot_sp2_rch_val = r_cifras_control.tot_sp2_rch_val + 1
    END IF 

    IF(arr_rechazo[x].subproceso = "003") THEN
       --Aumenta contador de rechazos para el sp1
       LET r_cifras_control.tot_sp3_rch_val =  r_cifras_control.tot_sp3_rch_val + 1
    END IF
 
    IF(arr_rechazo[x].subproceso = "005") THEN
       --Aumenta contador de rechazos para el sp1
       LET r_cifras_control.tot_sp5_rch_val = r_cifras_control.tot_sp5_rch_val + 1
    END IF
   

END FUNCTION

FUNCTION fn_valida_fecha(v_fecha)

   DEFINE v_fecha CHAR(8)
   DEFINE v_f_valida DATE
   
   LET v_f_valida = MDY(v_fecha[1,2],v_fecha[3,4],v_fecha[5,8])

   IF v_f_valida IS NULL THEN
      --DISPLAY "fecha incorrecta"
      LET bnd_fecha = 1
   ELSE
      LET bnd_fecha = 0
   END IF

   --DISPLAY "fecha : ",v_f_valida
END FUNCTION

FUNCTION fn_crea_temporal()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_ocg_cifras

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_ocg_cifras (tot_reg_archivo INTEGER,
                                    tot_sp1_arh INTEGER,
                                    tot_sp2_arh INTEGER,
                                    tot_sp3_arh INTEGER,
                                    tot_sp5_arh INTEGER,
                                    tot_sp1_rch_val INTEGER,
                                    tot_sp2_rch_val INTEGER,
                                    tot_sp3_rch_Val INTEGER,
                                    tot_sp5_rch_val INTEGER);
      
END FUNCTION 
