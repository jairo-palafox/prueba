#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC10                                                  #
#Objetivo            => PANTALLA DE CONSULTA PARA TRANSACCIONES                 #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 20 de JULIO del 2016                                    #
#################################################################################

DATABASE safre_viv

   DEFINE v_qry             STRING
   DEFINE v_cadena          STRING
   DEFINE p_usuario         CHAR(20)
   DEFINE p_tpo_ejecucion   SMALLINT
   DEFINE p_s_titulo        CHAR(20)
   DEFINE v_nss             CHAR(11)
   DEFINE v_f_proceso       DATE
   DEFINE v_f_transaccion   DATE
   DEFINE v_cve_ef          SMALLINT

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC10.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW OCGC101 WITH FORM "OCGC101"

   INPUT BY NAME v_nss,
                 v_cve_ef,
                 v_f_transaccion,
                 v_f_proceso ATTRIBUTES(UNBUFFERED)

   ON ACTION ACCEPT

   LET v_cadena = " "

      IF v_nss IS NOT NULL THEN
         LET v_cadena = v_cadena," AND t.nss = ","'",v_nss,"'" 
      END IF

      IF v_f_proceso IS NOT NULL THEN
         LET v_cadena = v_cadena," AND t.f_proceso = ","'",v_f_proceso,"'"
      END IF

      IF v_f_transaccion IS NOT NULL THEN
         LET v_cadena = v_cadena," AND t.f_transaccion = ","'",v_f_transaccion,"'"
      END IF

      IF v_cve_ef IS NOT NULL THEN
         LET v_cadena = v_cadena," AND t.cve_ent_financiera = ",v_cve_ef
      END IF


      CALL fn_consulta_dev()
      NEXT FIELD v_nss
   --EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   
   CLOSE WINDOW OCGC101

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
      f_pago                 DATE,
      f_transaccion          DATE,
      concepto               SMALLINT,
      estado                 SMALLINT
   END RECORD

   LET v_qry = "SELECT a.id_ocg_ctr_transaccion,
                       b.id_ocg_formalizacion,
                       b.nss,
                       b.cve_ent_financiera,
                       b.num_ctr_int_ef,
                       b.periodo_pago,
                       b.vivienda_97,
                       b.f_transaccion,
                       b.f_pago,
                       b.concepto,
                       a.estado
                  FROM ocg_transaccion_pago a, ocg_ctr_transaccion b
                 WHERE a.id_ocg_ctr_transaccion = b.id_ocg_ctr_transaccion"

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
                                arr_transaccion[a].f_pago,
                                arr_transaccion[a].concepto,
                                arr_transaccion[a].estado

--DISPLAY "arreglo",arr_transaccion[a].*

--DISPLAY "id_ocg_formalizacion",arr_id[a].id_ocg_formalizacion

      LET a = a+1
   END FOREACH

   CALL arr_transaccion.deleteElement(arr_transaccion.getLength())
   CALL arr_id.deleteElement(arr_id.getLength())

   IF arr_transaccion.getLength() >= 1 THEN

      OPEN WINDOW OCGC102 WITH FORM "OCGC102"
         DISPLAY ARRAY arr_transaccion TO tab_transaccion.* ATTRIBUTES (UNBUFFERED, CANCEL = FALSE)
         ON ACTION ACCEPT
         EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW OCGC092
   ELSE
      CALL fn_mensaje ("Archivo","No se encontró información para parámetro de búsqueda ingresado","information")
   END IF

   CALL arr_transaccion.clear()
   CALL arr_id.clear()

   LET v_nss           = NULL
   LET v_cve_ef        = NULL
   LET v_f_transaccion = NULL
   LET v_f_proceso     = NULL

END FUNCTION