###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGP08                                                  #
#Objetivo          => Programa que realiza la conciliación para la devolución #
#                     de saldos 43bis                                         #
#Autor             => José Eduardo Ventura                                    #
#Fecha inicio      => 23 JUNIO 2016                                           #
###############################################################################
DATABASE safre_viv


   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE v_qry                     STRING
   DEFINE v_cadena                  STRING
   DEFINE v_cb_lote                 CHAR(11)
   DEFINE v_ef                      CHAR(3)
   DEFINE v_f_dev                   DATE
   DEFINE v_f_proceso               DATE
   DEFINE cb                        ui.ComboBox
   DEFINE w                         ui.Window
   DEFINE f                         ui.Form

   DEFINE arr_devolucion  DYNAMIC ARRAY OF RECORD
      lote                CHAR (11),
      cve_ent_financiera  CHAR (3),
      monto_devuelto      DECIMAL(12,2),
      monto_devolucion    DECIMAL(12,2),
      diferencia          DECIMAL(12,2),
      f_dev               DATE,
      f_proceso           DATE,
      v_box               SMALLINT
   END RECORD

MAIN

   DEFINE p_tipo_ejecucion          SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo                STRING   -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGP08.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la función de conciliación
   CALL fn_filtro()
   --CALL fn_concilia()

END MAIN

FUNCTION fn_filtro()

   DEFINE a        INTEGER

   DEFINE arr_lote DYNAMIC ARRAY OF RECORD
      lote       CHAR(11),
      lote_desc  CHAR(11)
   END RECORD

   CLOSE WINDOW SCREEN
   OPEN WINDOW OCGP081 WITH FORM  "OCGP081"

   LET cb = ui.ComboBox.forName("v_cb_lote")

   INPUT BY NAME v_cb_lote,
                 v_ef,
                 v_f_dev,
                 v_f_proceso ATTRIBUTES (UNBUFFERED)

   BEFORE INPUT

      LET v_qry = "SELECT lote,
                          lote
                     FROM ocg_devolucion_ef"

      PREPARE prp_lote FROM v_qry
      DECLARE cur_lote CURSOR FOR prp_lote

      LET a = 1 
      FOREACH cur_lote INTO arr_lote[a].*
         CALL cb.addItem(arr_lote[a].lote, arr_lote[a].lote_desc)
         LET a = a+1
      END FOREACH

      ON ACTION ACCEPT

         LET v_cadena = " "

         IF v_cb_lote IS NOT NULL THEN
            LET v_cadena = v_cadena," AND lote = ","'",v_cb_lote,"'"
         END IF

         IF v_ef IS NOT NULL THEN
            LET v_cadena = v_cadena," AND cve_ent_financiera = ",v_ef
         END IF

         IF v_f_dev IS NOT NULL THEN
            LET v_cadena = v_cadena," AND f_devolucion = ","'",v_f_dev,"'"
         END IF

         IF v_f_proceso IS NOT NULL THEN
            LET v_cadena = v_cadena," AND f_proceso = ","'",v_f_proceso,"'"
         END IF

         CALL fn_concilia()
         EXIT INPUT

      ON ACTION CANCEL
         EXIT INPUT

      END INPUT
      CLOSE WINDOW OCGP081
END FUNCTION

FUNCTION fn_concilia()

   DEFINE b             SMALLINT
   DEFINE v_bnd_box     SMALLINT
   DEFINE v_id_ocg_dev  DECIMAL(9,0)
   DEFINE v_imp_ap      DECIMAL(12,5)
   DEFINE v_imp_dev     DECIMAL(12,5)
   DEFINE v_monto_devuelto DECIMAL(12,5)
   DEFINE v_disponible  DECIMAL(12,5)
   DEFINE v_imp_sol     DECIMAL(12,5)

   --CALL fn_mensaje("Alerta", "Iniciando proceso de conciliación","stop")

   LET v_qry = "SELECT lote,
                       cve_ent_financiera,
                       monto_devuelto,
                       monto_devolucion,
                       '',
                       f_devolucion,
                       f_proceso
                  FROM ocg_devolucion_ef
                 WHERE estado in (110,150) ",v_cadena

   PREPARE prp_devolucion FROM v_qry
   DECLARE cur_devolucion CURSOR FOR prp_devolucion

   LET b = 1
   FOREACH cur_devolucion INTO arr_devolucion[b].*
      LET arr_devolucion[b].v_box = 0
      LET arr_devolucion[b].diferencia = (arr_devolucion[b].monto_devolucion - arr_devolucion[b].monto_devuelto)
      IF arr_devolucion[b].diferencia IS NULL THEN
         LET arr_devolucion[b].diferencia = 0
      END IF
      LET b = b+1
   END FOREACH

   CALL arr_devolucion.deleteElement(arr_devolucion.getLength())

   IF arr_devolucion.getLength() >= 1 THEN

      OPEN WINDOW OCGP082 WITH FORM "OCGP082"
         INPUT ARRAY arr_devolucion WITHOUT DEFAULTS FROM tab_devolucion.*
            ATTRIBUTES (UNBUFFERED, INSERT ROW = FALSE,DELETE ROW = FALSE, APPEND ROW = FALSE )

         ON ACTION ACCEPT
            FOR b = 1 TO arr_devolucion.getLength()
               IF arr_devolucion[b].v_box = 1 THEN

                  LET v_qry = "
                  SELECT id_ocg_devolucion,
                         importe_subsec_devuelto,
                         importe_ocg_devuelto
                    FROM ocg_devolucion
                   WHERE cve_ent_financiera = ",arr_devolucion[b].cve_ent_financiera,"
                     AND f_deposito         = '",arr_devolucion[b].f_dev,"'",
                     " AND edo_registro = 160"
                    -- AND importe_subsec_devuelto = ",arr_devolucion[b].monto_dev,"
                     -- OR importe_ocg_devuelto = ",arr_devolucion[b].monto_dev

                 -- DISPLAY v_qry

                  PREPARE prp_concilia_dev FROM v_qry
                  DECLARE cur_concilia_dev CURSOR FOR prp_concilia_dev

                  FOREACH cur_concilia_dev INTO v_id_ocg_dev,
                                                v_imp_ap,
                                                v_imp_dev

                      IF v_imp_ap IS NOT NULL THEN
                         LET v_imp_sol = v_imp_ap
                      END IF

                      IF v_imp_dev IS NOT NULL THEN
                         LET v_imp_sol = v_imp_dev
                      END IF

                      DISPLAY v_imp_sol
                      DISPLAY arr_devolucion[b].diferencia

                         IF v_imp_sol <= arr_devolucion[b].diferencia THEN
                            UPDATE ocg_devolucion
                               SET edo_registro = 170,
                                   importe_pendiente = 0
                             WHERE id_ocg_devolucion = v_id_ocg_dev

                           IF arr_devolucion[b].monto_devuelto IS NULL THEN
                              LET arr_devolucion[b].monto_devuelto = 0
                           END IF

                           LET v_monto_devuelto = arr_devolucion[b].monto_devuelto + v_imp_sol

                           DISPLAY v_monto_devuelto

                            UPDATE ocg_devolucion_ef
                               SET monto_devuelto     = v_monto_devuelto
                             WHERE cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                               AND f_devolucion       = arr_devolucion[b].f_dev

                            LET arr_devolucion[b].monto_devuelto = v_monto_devuelto
            
                         ELSE
                            LET v_disponible = (arr_devolucion[b].diferencia - v_imp_sol)
                            UPDATE ocg_devolucion
                               SET edo_registro = 180,
                                   importe_pendiente = v_disponible

                            IF arr_devolucion[b].monto_devuelto IS NULL THEN
                              LET arr_devolucion[b].monto_devuelto = 0
                            END IF

                            LET v_monto_devuelto = arr_devolucion[b].monto_devuelto + v_imp_sol

                            DISPLAY v_monto_devuelto

                            
                            UPDATE ocg_devolucion_ef
                               SET monto_devuelto     = v_monto_devuelto
                             WHERE cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                               AND f_devolucion       = arr_devolucion[b].f_dev

                            LET arr_devolucion[b].monto_devuelto = v_monto_devuelto

                         END IF

                  END FOREACH

                  LET v_bnd_box = 1
               END IF
               IF arr_devolucion[b].monto_devolucion = v_monto_devuelto THEN
                  UPDATE ocg_devolucion_ef
                     SET estado = 140
                   WHERE cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                     AND f_devolucion       = arr_devolucion[b].f_dev
                  ELSE
                     UPDATE ocg_devolucion_ef
                     SET estado = 150
                   WHERE cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                     AND f_devolucion       = arr_devolucion[b].f_dev
               END IF
            END FOR
            IF v_bnd_box = 1 THEN
                CALL fn_mensaje("Alerta", "Proceso de conciliación realizado para registros seleccionados","stop")
            END IF
            EXIT INPUT

         END INPUT
      CLOSE WINDOW OCGP082
   ELSE
      CALL fn_mensaje("Alerta", "No exísten fichas autorizadas para conciliar","stop")
   END IF

END FUNCTION