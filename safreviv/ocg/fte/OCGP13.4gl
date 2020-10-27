##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP13                                             #
#Objetivo          => Programa que permite recuperar devoluciones por    #
#                     crédito liquidado                                  #
#Autor             => José Eduardo Venntura                              #
#Fecha inicio      => 26 Septiembre 2016                                 #
##########################################################################

DATABASE safre_viv

   --Definiciòn de variables globales, parametros enviados del menu
   DEFINE g_usuario      CHAR(20)
   DEFINE g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING
   DEFINE v_cnt_dev      INTEGER
   DEFINE v_cnt_reg      INTEGER
   DEFINE v_estado       SMALLINT
   DEFINE v_pid          INTEGER
   DEFINE p_proceso_cod  INTEGER
   DEFINE p_opera_cod    SMALLINT

MAIN 
   -- se incorpora como parametros enviados desde el menu el proceso y codigo de la operaciòn
   LET g_usuario      = ARG_VAL(1)
   LET v_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET g_tipo_proceso = ARG_VAL(5)
   LET g_nom_ventana  = ARG_VAL(6)

   IF p_proceso_cod IS NULL THEN
      LET p_proceso_cod = 3916
   END IF

   IF p_opera_cod IS NULL THEN
      LET p_opera_cod = 1
   END IF

   -- se genera el archivo log en caso de Error
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP13.log")

    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   --CLOSE WINDOW SCREEN

   DISPLAY ""
   DISPLAY "Inicia proceso para recuperación de devoluciones por liquidación : "
   DISPLAY ""

   SELECT COUNT(*)
     INTO v_cnt_reg
     FROM ocg_ctr_transaccion
    WHERE concepto IN (108,308,408,808)
     AND estado = 30

   IF v_cnt_reg >= 1 THEN
      CALL fn_dev_cred_liq()
      DISPLAY "Total de devoluciones por liquidación : ",v_cnt_reg
      LET v_estado = 0
   ELSE
      DISPLAY "No se encontraros nuevas devoluciones por liquidación para recuerar"
      LET v_estado = 0
   END IF

   IF v_estado = 0 THEN

      CALL fn_actualiza_opera_fin(v_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(v_pid, p_proceso_cod, p_opera_cod)  RETURNING v_estado
   END IF

   DISPLAY ""
   DISPLAY "Fin de proceso"
   DISPLAY ""

END MAIN

FUNCTION fn_dev_cred_liq()

   DEFINE v_qry               STRING
   DEFINE a                   INTEGER
   DEFINE v_id_ocg_devolucion DECIMAL(9,0)

   DEFINE arr_dev_cred_liq DYNAMIC ARRAY OF RECORD
      id_ocg_ctr_transaccion  DECIMAL (9,0),
      id_ocg_devolucion       DECIMAL (9,0),
      id_ocg_detalle          DECIMAL (9,0),
      id_ocg_formalizacion    DECIMAL (9,0),
      id_ocg_tramite          DECIMAL (9,0),
      id_derechohabiente      DECIMAL (9,0),
      cve_ent_financiera      SMALLINT,
      num_ctr_int_ef          CHAR (18),
      periodo_pago            CHAR(8),
      importe_subsec_dev      DECIMAL (10,2),
      importe_ocg_dev         DECIMAL(10,2),
      importe_pendiente       DECIMAL(10,2),
      f_deposito              DATE,
      tpo_credito             CHAR(1),
      diagnostico             SMALLINT,
      estado                  SMALLINT,
      situacion               SMALLINT,
      edo_registro            SMALLINT,
      f_proceso               DATE
   END RECORD

LET v_qry =
"  SELECT id_ocg_ctr_transaccion,
          seq_ocg_devolucion.nextval,
          '',
          id_ocg_formalizacion,
          '',
          id_derechohabiente,
          cve_ent_financiera,
          num_ctr_int_ef,
          periodo_pago,
          vivienda_97,
          '',
          '',
          f_pago,
          '',
          '',
          estado,
          '',
          '',
          TODAY
     FROM ocg_ctr_transaccion
    WHERE concepto IN (108,308,408,808)
     AND estado = 30"

   -- el registro no tiene id_ocg_detalle por que no entra el registro a devolucion mediante algún sub-proceso
   PREPARE prp_devolucion FROM v_qry
   DECLARE cur_devolucion CURSOR FOR prp_devolucion

   LET a = 1
   FOREACH cur_devolucion INTO arr_dev_cred_liq[a].*

      SELECT id_ocg_tramite
             tpo_credito
        INTO arr_dev_cred_liq[a].id_ocg_tramite,
             arr_dev_cred_liq[a].tpo_credito
        FROM ocg_formalizacion
       WHERE id_ocg_formalizacion = arr_dev_cred_liq[a].id_ocg_formalizacion

      LET arr_dev_cred_liq[a].edo_registro = 200

      LET a = a+1
   END FOREACH

   CALL arr_dev_cred_liq.deleteElement(a)

   FOR a = 1 TO arr_dev_cred_liq.getLength()
      INSERT INTO ocg_devolucion
           VALUES (arr_dev_cred_liq[a].id_ocg_devolucion,
                   arr_dev_cred_liq[a].id_ocg_detalle,
                   arr_dev_cred_liq[a].id_ocg_formalizacion,
                   arr_dev_cred_liq[a].id_ocg_tramite,
                   arr_dev_cred_liq[a].id_derechohabiente,
                   arr_dev_cred_liq[a].cve_ent_financiera,
                   arr_dev_cred_liq[a].num_ctr_int_ef,
                   arr_dev_cred_liq[a].periodo_pago,
                   arr_dev_cred_liq[a].importe_subsec_dev,
                   arr_dev_cred_liq[a].importe_ocg_dev,
                   arr_dev_cred_liq[a].importe_pendiente,
                   arr_dev_cred_liq[a].f_deposito,
                   arr_dev_cred_liq[a].tpo_credito,
                   arr_dev_cred_liq[a].diagnostico,
                   arr_dev_cred_liq[a].estado,
                   arr_dev_cred_liq[a].situacion,
                   arr_dev_cred_liq[a].edo_registro,
                   arr_dev_cred_liq[a].f_proceso)
   END FOR

END FUNCTION