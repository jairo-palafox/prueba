#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC09                                                  #
#Objetivo            => PANTALLAS DE CONSULTA PARA PROSPECTOS DE DEVOLUCIÓN     #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 20 de Junio de 2016                                     #
#################################################################################

DATABASE safre_viv

   DEFINE v_qry             STRING
   DEFINE v_cadena          STRING
   DEFINE p_usuario         CHAR(20)
   DEFINE p_tpo_ejecucion   SMALLINT
   DEFINE p_s_titulo        CHAR(20)
   DEFINE v_nss             CHAR(11)
   DEFINE v_f_proceso       DATE
   DEFINE v_f_liquida       DATE
   DEFINE v_cve_ef          SMALLINT

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC09.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW OCGC091 WITH FORM "OCGC091"

   INPUT BY NAME v_nss,
                 v_cve_ef,
                 v_f_liquida,
                 v_f_proceso ATTRIBUTES(UNBUFFERED)

   ON ACTION ACCEPT

   LET v_cadena = " "

      IF v_nss IS NOT NULL THEN
         LET v_cadena = v_cadena," AND d.nss = ","'",v_nss,"'" 
      END IF

      IF v_f_proceso IS NOT NULL THEN
         LET v_cadena = v_cadena," AND d.f_proceso = ","'",v_f_proceso,"'"
      END IF

      IF v_f_liquida IS NOT NULL THEN
         LET v_cadena = v_cadena," AND l.f_liberacion_gtia = ","'",v_f_liquida,"'"
      END IF

      IF v_cve_ef IS NOT NULL THEN
         LET v_cadena = v_cadena," AND d.cve_ent_financiera = ",v_cve_ef
      END IF


      CALL fn_consulta_dev()
      NEXT FIELD v_nss
   --EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   
   CLOSE WINDOW OCGC091

END MAIN

FUNCTION fn_consulta_dev()

   DEFINE a          INTEGER

   DEFINE arr_id DYNAMIC ARRAY OF RECORD
      id_ocg_ctr_transaccion DECIMAL(9,0),
      id_ocg_formalizacion   DECIMAL(9,0)
   END RECORD

   DEFINE arr_transaccion DYNAMIC ARRAY OF RECORD
      nss                    CHAR(11),
      cve_ent_financiera     SMALLINT,
      num_ctr_int            CHAR(18),
      periodo_pago           CHAR(6),
      vivienda97             DECIMAL(12,2),
      aivs                   DECIMAL(12,5),
      f_transaccion          DATE,
      f_pago                 DATE,
      f_liquidacion          DATE
   END RECORD

   LET v_qry = "SELECT id_ocg_ctr_transaccion,
                       id_ocg_formalizacion,
                       nss,
                       cve_ent_financiera,
                       num_ctr_int_ef,
                       periodo_pago,
                       vivienda_97,
                       f_transaccion,
                       f_pago
                  FROM ocg_ctr_transaccion
                 WHERE id_derechohabiente in (
                SELECT l.id_derechohabiente
                  FROM ocg_liquidacion l,ocg_detalle d
                 WHERE l.id_derechohabiente = d.id_derechohabiente
                   AND d.subproceso = 5 ",v_cadena,")
                   AND estado = 40"

   PREPARE prp_transaccion FROM v_qry
   DECLARE cur_transaccion CURSOR FOR prp_transaccion

   LET a = 1

   FOREACH cur_transaccion INTO arr_id[a].id_ocg_ctr_transaccion,
                                arr_id[a].id_ocg_formalizacion,
                                arr_transaccion[a].nss,
                                arr_transaccion[a].cve_ent_financiera,
                                arr_transaccion[a].num_ctr_int,
                                arr_transaccion[a].periodo_pago,
                                arr_transaccion[a].vivienda97,
                                arr_transaccion[a].f_transaccion,
                                arr_transaccion[a].f_pago

DISPLAY "arreglo",arr_transaccion[a].*

DISPLAY "id_ocg_formalizacion",arr_id[a].id_ocg_formalizacion

      SELECT f_liberacion_gtia
        INTO arr_transaccion[a].f_liquidacion
        FROM ocg_liquidacion
       WHERE id_ocg_formalizacion = arr_id[a].id_ocg_formalizacion
         AND diagnostico = 1

      SELECT precio_fondo
        INTO arr_transaccion[a].aivs
        FROM glo_valor_fondo
       WHERE f_valuacion = arr_transaccion[a].f_transaccion
         AND fondo = 11

      LET a = a+1
   END FOREACH

   CALL arr_transaccion.deleteElement(arr_transaccion.getLength())
   CALL arr_id.deleteElement(arr_id.getLength())

   IF arr_transaccion.getLength() >= 1 THEN

      OPEN WINDOW OCGC092 WITH FORM "OCGC092"
         DISPLAY ARRAY arr_transaccion TO tab_pros_dev.* ATTRIBUTES (UNBUFFERED, CANCEL = FALSE)
         ON ACTION ACCEPT
         EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW OCGC092
   ELSE
      CALL fn_mensaje ("Archivo","No se encontró información para parámetro de búsqueda ingresado","information")
   END IF

   CALL arr_transaccion.clear()
   CALL arr_id.clear()

   LET v_nss      = NULL
   LET v_cve_ef   = NULL
   LET v_f_liquida= NULL
   LET v_f_proceso= NULL

END FUNCTION