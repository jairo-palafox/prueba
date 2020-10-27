#######################################################################
#Modulo              => OCG                                           #
#Programa            => OCGC07                                        #
#Objetivo            => PANTALLAS DE CONSULTA DE RECHAZOS CARTERA     #
#Autor               => JOSÉ EDUARDO VENTURA                          #
#Fecha inicio        => 25 de ENERO de 2016                           #
#######################################################################

DATABASE safre_viv

   DEFINE v_cadena          STRING

MAIN

   DEFINE p_usuario         CHAR(20)
   DEFINE p_tpo_ejecucion   SMALLINT
   DEFINE p_s_titulo        CHAR(20)
   DEFINE v_nss             CHAR(11)
   DEFINE v_ent_financiera  SMALLINT
   DEFINE v_f_rechazo       DATE
   DEFINE v_tpo_rechazo     SMALLINT

      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC08.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW ent_cons_rch WITH FORM "OCGC082"

   INPUT BY NAME v_nss,
                 v_ent_financiera,
                 v_f_rechazo,
                 v_tpo_rechazo ATTRIBUTES(UNBUFFERED)

   ON ACTION ACCEPT

   LET v_cadena = " "

      IF v_nss IS NOT NULL THEN
         LET v_cadena = v_cadena," AND nss = ","'",v_nss,"'" 
      END IF

      IF v_ent_financiera IS NOT NULL THEN
         LET v_cadena = v_cadena," AND cve_ent_financiera = ",v_ent_financiera
      END IF

      IF v_f_rechazo IS NOT NULL THEN
         LET v_cadena = v_cadena," AND f_proceso = ","'",v_f_rechazo,"'"
      END IF

      IF v_tpo_rechazo IS NOT NULL THEN
         IF v_f_rechazo IS NOT NULL THEN

         LET v_cadena = "AND id_ocg_liq_cofi in 
                        (select id_ocg_liq_cofi
                           from ocg_rechazo_cartera
                          WHERE rechazo_cod = ",v_tpo_rechazo,"
                            AND f_proceso = ",v_f_rechazo,")"
         ELSE
            LET v_cadena = "AND id_ocg_liq_cofi in 
                        (select id_ocg_liq_cofi
                           from ocg_rechazo_cartera
                          WHERE rechazo_cod = ",v_tpo_rechazo,")"
         END IF
      END IF
      CALL fn_consulta_rch()
      LET v_nss            = ""
      LET v_ent_financiera = ""
      LET v_f_rechazo      = ""
      LET v_tpo_rechazo    = ""
      NEXT FIELD v_nss
   --EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   
   CLOSE WINDOW ent_cons_rch

END MAIN

FUNCTION fn_consulta_rch()

   DEFINE v_qry                 STRING
   DEFINE a                     INTEGER

   DEFINE arr_rch DYNAMIC ARRAY OF RECORD
      id                  DECIMAL(9,0),
      nss                 CHAR(11),
      cve_ent_financiera  SMALLINT,
      f_rechazo           DATE,
      rechazo_desc        CHAR(50)
   END RECORD

   LET v_qry = "SELECT id_ocg_liq_cofi,
                       nss,
                       cve_ent_financiera,
                       f_proceso
                  FROM ocg_liquidacion_cofi
                 WHERE diagnostico in (1,2)
                   AND situacion = 190
                   ANd estado = 60  ",v_cadena,"
                       order by f_proceso desc"

   --DISPLAY v_qry

   PREPARE prp_rch_cartera FROM v_qry
   DECLARE cur_rch_cartera CURSOR FOR prp_rch_cartera

   LET a = 1

   FOREACH cur_rch_cartera INTO arr_rch[a].id,
                                arr_rch[a].nss,
                                arr_rch[a].cve_ent_financiera,
                                arr_rch[a].f_rechazo
      SELECT c.rechazo_desc
        INTO arr_rch[a].rechazo_desc
        FROM cat_rechazo_cartera c,
             ocg_rechazo_cartera b
       WHERE b.id_ocg_liq_cofi = arr_rch[a].id
         AND b.rechazo_cod = c.rechazo_cod

      LET a = a+1
   END FOREACH

   CALL arr_rch.deleteElement(arr_rch.getLength())
{
   LET v_qry = "SELECT id_ocg_liq_cofi,
                       nss,
                       cve_ent_financiera,
                       f_proceso
                  FROM ocg_liquidacion_cofi
                 WHERE diagnostico = 1 ",v_cadena,"
                       order by f_proceso desc"

   PREPARE prp_rch_info FROM v_qry
   DECLARE cur_rch_info CURSOR FOR prp_rch_info

   LET a = 1

   FOREACH cur_rch_cartera INTO arr_rch[a].id,
                                arr_rch[a].nss,
                                arr_rch[a].cve_ent_financiera,
                                arr_rch[a].f_rechazo
      SELECT c.rechazo_desc
        INTO arr_rch[a].rechazo_desc
        FROM cat_rechazo_cartera c,
             ocg_rechazo_cartera b
       WHERE b.id_ocg_liq_cofi = arr_rch[a].id
         AND b.rechazo_cod = c.rechazo_cod

      LET a = a+1
   END FOREACH

   CALL arr_rch.deleteElement(arr_rch.getLength())
}
   OPEN WINDOW cons_rch WITH FORM "OCGC081"

   DISPLAY ARRAY arr_rch TO tab_rechazo.* ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

   ON ACTION salir
      EXIT DISPLAY

   END DISPLAY
   CLOSE WINDOW cons_rch
END FUNCTION