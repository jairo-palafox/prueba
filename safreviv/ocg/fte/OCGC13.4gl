#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC13                                                  #
#Objetivo            => PANTALLA DE CONSULTA PARA MIGRACIÓN DE DETALLES         #
#                       HISTÓRICOS 43BIS                                        #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 15 de OCTUBRE del 2016                                  #
#################################################################################

DATABASE safre_viv

   DEFINE p_usuario            CHAR(20)
   DEFINE p_s_titulo           CHAR(20)
   DEFINE p_tpo_ejecucion      SMALLINT
   DEFINE v_nss                CHAR(11)
   DEFINE v_cve_ef             SMALLINT
   DEFINE v_f_proceso          DATE
   DEFINE v_f_carga            DATE
   DEFINE v_situacion          CHAR(1)
   DEFINE v_tpo_credito        CHAR(2)
   DEFINE v_subproceso         SMALLINT
   DEFINE v_cadena             STRING

   DEFINE v_control_ef         CHAR(18)
   DEFINE v_clave_nss          CHAR(1)
   DEFINE v_cve_nss_asociado   CHAR(11)
   DEFINE v_ap_paterno         CHAR(40)
   DEFINE v_ap_materno         CHAR(40)
   DEFINE v_nombre             CHAR(40)
   DEFINE v_rfc                CHAR(13)
   DEFINE v_curp               CHAR(18)
   DEFINE v_genero             CHAR(1)
   DEFINE v_estado_desc        CHAR(50)
   DEFINE v_cve_ent_financiera SMALLINT
   DEFINE v_f_envio            DATE
   DEFINE v_producto           CHAR(1)
   DEFINE v_sdo_97             DECIMAL(15,2)
   DEFINE v_f_subcta           DATE
   DEFINE v_f_liquida          DATE
   DEFINE v_f_liquida_cofi     DATE
   DEFINE v_f_respuesta        DATE
   DEFINE v_f_formaliza        DATE
   DEFINE v_f_vigencia         DATE
   DEFINE v_diagnostico        CHAR(1)

   
   DEFINE arr_mig_tramite DYNAMIC ARRAY OF RECORD
      situacion           CHAR(1),
      subproceso          CHAR(3),
      tpo_credito         CHAR(1),
      cve_ent_financiera  CHAR(3),
      diagnostico         CHAR(2),
      f_proceso           DATE,
      nss                 CHAR(11),
      rfc                 CHAR(13),
      paterno             CHAR(40),
      materno             CHAR(40),
      nombre              CHAR(40),
      viv97               DECIMAL(15,2)
   END RECORD

   DEFINE arr_general DYNAMIC ARRAY OF RECORD
      situacion           CHAR(1),
      subproceso          CHAR(3),
      tpo_credito         CHAR(1),
      cve_ent_financiera  CHAR(3),
      diagnostico         CHAR(2),
      f_proceso           DATE,
      nss                 CHAR(11),
      rfc                 CHAR(13),
      nombre              CHAR(150)
   END RECORD

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC13.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW busqueda WITH FORM "OCGC131"

   INPUT BY NAME v_nss,
                 v_subproceso,
                 v_cve_ef,
               --  v_f_proceso,
               --  v_f_carga,
               --  v_f_formaliza
                 v_situacion,
                 v_tpo_credito  ATTRIBUTES(UNBUFFERED)


   ON ACTION ACCEPT

   LET v_cadena = " "

   IF v_nss IS NOT NULL THEN
      LET v_cadena = v_cadena," AND nss = '",v_nss,"'"
   END IF

   IF v_subproceso IS NOT NULL THEN
      LET v_cadena = v_cadena," AND subproceso  = ",v_subproceso
   END IF

   IF v_cve_ef IS NOT NULL THEN
      LET v_cadena = v_cadena," AND ent_financiera = ",v_cve_ef
   END IF

   IF v_f_proceso IS NOT NULL THEN
      LET v_cadena = v_cadena," AND f_proceso   = ",v_f_proceso
   END IF

   IF v_situacion IS NOT NULL THEN
      LET v_cadena = v_cadena," AND situacion   =",v_situacion
   END IF

   IF v_tpo_credito IS NOT NULL THEN
      LET v_cadena = v_cadena," AND tpo_credito = ",v_tpo_credito
   END IF

      --CALL fn_consulta(v_cadena)
      --CALL fn_det_tramite()

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW busqueda

END MAIN
