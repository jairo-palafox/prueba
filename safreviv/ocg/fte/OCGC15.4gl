#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC15                                        #
#Objetivo            => PANTALLAS DE CONSULTA DE RECAHAZOS .Ent       #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 25 de ENERO de 2017                           #
#######################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_usuario        CHAR(20)
   DEFINE p_tpo_ejecucion  SMALLINT
   DEFINE p_s_titulo       CHAR(20)
   DEFINE v_cadena         STRING

   DEFINE v_nss                 CHAR(11)
   DEFINE v_subproceso          CHAR(3)
   DEFINE v_ef                  CHAR(3)
   DEFINE v_f_carga             CHAR(10)
   DEFINE v_f_formaliza         CHAR(10)
   DEFINE v_f_proceso           DATE
   DEFINE v_estado              CHAR(3)
   DEFINE v_producto            CHAR(3)

     DEFINE v_s_qry     STRING
   DEFINE b           SMALLINT
   DEFINE v_pos       SMALLINT

   DEFINE arr_cons_rch DYNAMIC ARRAY OF RECORD
      id_tmp_det                    DECIMAL(9,0),
      nss                           CHAR(11),
      estado                        CHAR(3),
      subproceso                    CHAR(3),
      tpo_credito                   CHAR(3),
      cve_ent_financiera            char(3),
      diagnostico                   char(2),
      f_proceso                     DATE,
      rfc                           CHAR(13),
      nombre                        VARCHAR(80)
   END RECORD

DEFINE v_id_archivo             CHAR(9)
DEFINE v_id_tmp_detalle         CHAR(9)
DEFINE v_tpo_registro           CHAR(3)
DEFINE v_subproceso             CHAR(3)
DEFINE v_tpo_envio              CHAR(3)
DEFINE v_f_envio                CHAR(10)
DEFINE v_nss                    CHAR(11)
DEFINE v_control_ef             CHAR(3)
DEFINE v_rfc                    CHAR(13)
DEFINE v_curp                   CHAR(18)
DEFINE v_ap_paterno             CHAR(20)
DEFINE v_ap_materno             CHAR(20)
DEFINE v_nombre                 CHAR(20)
DEFINE v_num_bimestres          CHAR(3)
DEFINE v_viv_92                 CHAR(15)
DEFINE v_sdo_97                 CHAR(15)
DEFINE v_f_subcta               CHAR(10)
DEFINE v_diagnostico            CHAR(3)
DEFINE v_inconsistencia         CHAR(40)
DEFINE v_num_escritura          CHAR(15)
DEFINE v_notario                CHAR(15)
DEFINE v_ent_fed_notario        CHAR(15)
DEFINE v_mcpio_notario          CHAR(15)
DEFINE v_num_rpp                CHAR(15)
DEFINE v_folio_real             CHAR(15)
DEFINE v_partida                CHAR(15)
DEFINE v_foja                   CHAR(15)
DEFINE v_volumen                CHAR(15)
DEFINE v_libro                  CHAR(15)
DEFINE v_tomo                   CHAR(15)
DEFINE v_seccion                CHAR(15)
DEFINE v_ent_fed_inmueble       CHAR(15)
DEFINE v_domicilio_inmueble     CHAR(15)
DEFINE v_valor_avaluo1          CHAR(15)
DEFINE v_monto_credito          CHAR(15)
DEFINE v_plazo_credito          CHAR(15)
DEFINE v_tpo_moneda             CHAR(15)
DEFINE v_tasa_base              CHAR(15)
DEFINE v_margen                 CHAR(15)
DEFINE v_f_otorga_ent_fin       CHAR(10)
DEFINE v_importe_solicitado     CHAR(15)
DEFINE v_f_vencimiento          CHAR(10)
DEFINE v_importe_utilizado      CHAR(15)
DEFINE v_imp_pago               CHAR(15)
DEFINE v_bim_ap                 CHAR(10)
DEFINE v_imp_ap_subsec          CHAR(15)
DEFINE v_f_liberacion           CHAR(10)
DEFINE v_imp_devuelto           CHAR(15)
DEFINE v_causa_liquida          CHAR(2)
DEFINE v_f_deposito             CHAR(8)
DEFINE v_producto               CHAR(3)
DEFINE v_solic_saldo            CHAR(3)
DEFINE v_f_vigencia             CHAR(10)
DEFINE v_filler                 CHAR(40)
DEFINE v_estado                 CHAR(3)
DEFINE v_periodo_pago           CHAR(10)
DEFINE v_situacion              CHAR(3)
DEFINE v_carga                  CHAR(10)

END GLOBALS

MAIN

      -- se recupera la clave de usuario desde parámetro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC15.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGC15 WITH FORM "OCGC151"

   INPUT BY NAME v_nss,
                 v_ef,
                 v_f_proceso ATTRIBUTES (UNBUFFERED)

      ON ACTION ACCEPT

      LET v_cadena = " "

      IF v_nss IS NOT NULL THEN
         LET v_cadena = v_cadena, " AND nss = '",v_nss,"'"
      END IF

      IF v_f_proceso IS NOT NULL THEN
         --LET v_f_proceso = v_f_proceso[4,5],v_f_proceso[1,2],v_f_proceso[7,10]
         LET v_cadena = v_cadena," AND f_proceso = '",v_f_proceso,"'"
      END IF

      IF v_ef IS NOT NULL THEN
         LET v_cadena = v_cadena,"AND cve_ent_financiera = '",v_ef,"'"
      END IF

      CALL fn_cons()

      ON ACTION CANCEL
         CALL arr_cons_rch.clear()
         EXIT INPUT

   END INPUT
   CLOSE WINDOW OCGC15

END MAIN

FUNCTION fn_cons()

   DEFINE arr_inconsistencia DYNAMIC ARRAY OF RECORD
   inconsistencia            CHAR(3),
   descripcion               CHAR(40)
   END RECORD

   DEFINE a                  SMALLINT

   LET v_s_qry ="
   SELECT id_tmp_det,
          nss,
          estado,
          subproceso,
          cred_convenidos,
          cve_ent_financiera,
          '2',
          f_proceso,
          rfc,
          TRIM(ap_paterno_af)||' '||TRIM(ap_materno_af)||' '||TRIM(nombre_af)
     FROM ocg_rechazo_ent 
    WHERE 1 = 1 ",v_cadena


     PREPARE prp_rechazo FROM v_s_qry
   DECLARE cur_rechazo CURSOR FOR prp_rechazo

   LET b = 1
   FOREACH cur_rechazo INTO arr_cons_rch[b].*
      LET b = b+1
   END FOREACH

   CALL arr_cons_rch.deleteElement(arr_cons_rch.getLength())

  IF arr_cons_rch.getLength() >= 1 THEN
      OPEN WINDOW rechazos WITH FORM "OCGC152"

      DIALOG ATTRIBUTES(UNBUFFERED)

         DISPLAY ARRAY arr_cons_rch TO tab_rechazo.* --ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
         ON ACTION ACCEPT
            BEFORE ROW
            LET v_pos = ARR_CURR()

--******************************************************************************
            LET v_s_qry = "SELECT i.inconsistencia,
                                  c.incons_desc
                             FROM ocg_inconsistencia_rch i,
                                  cat_inconsistencia c
                            WHERE i.inconsistencia = c.inconsistencia
                              AND i.id_ocg_referencia = ",arr_cons_rch[v_pos].id_tmp_det

            CALL arr_inconsistencia.clear()

PREPARE prp_incons FROM v_s_qry
DECLARE cur_incons CURSOR FOR prp_incons

LET a = 1
FOREACH cur_incons INTO arr_inconsistencia[a].*
   LET a = a + 1
END FOREACH

IF arr_inconsistencia.getLength() >= 1 THEN

DISPLAY ARRAY arr_inconsistencia TO tab_inconsistencia.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

BEFORE DISPLAY
EXIT DISPLAY
END DISPLAY

END IF

--******************************************************************************
            --CALL fn_datos(v_pos)

   ON ACTION atras

         EXIT DIALOG
         END DISPLAY

         END DIALOG
         
      CLOSE WINDOW rechazos 
      CALL arr_cons_rch.clear()
   ELSE
      CALL fn_mensaje("Alerta","No se encontraron registros rechazados para datos ingresados","stop")
   END IF

END FUNCTION
