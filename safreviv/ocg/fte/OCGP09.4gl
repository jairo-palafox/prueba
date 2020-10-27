###############################################################################
#Modulo            => OCG                                                     #
#Programa          => OCGP08                                                  #
#Objetivo          => Programa que realiza la autorización para la devolución #
#                     de saldos 43bis                                         #
#Autor             => José Eduardo Ventura                                    #
#Fecha inicio      => 06 JULIO 2016                                           #
###############################################################################
DATABASE safre_viv


   DEFINE p_usuario_cod             LIKE seg_usuario.usuario_cod
   DEFINE v_qry                     STRING
   DEFINE a                         INTEGER
   DEFINE v_bnd                     SMALLINT
   DEFINE b                         SMALLINT
   DEFINE w ui.Window
   DEFINE f ui.Form

   DEFINE arr_devolucion  DYNAMIC ARRAY OF RECORD
      lote                CHAR (11),
      cve_ent_financiera  CHAR (3),
      ent_financiera      CHAR(40),
      monto_dev           DECIMAL(12,2),
      f_devolucion        DATE,
      f_proceso           DATE,
      usuario             CHAR(20),
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

   -- se invoca la función que despliega tabla para autorizar el monto
   CALL fn_devolucion()

END MAIN

FUNCTION fn_devolucion()

   LET v_qry = "SELECT a.lote,
                       a.cve_ent_financiera,
                       b.ent_financiera_desc,
                       a.monto_devolucion,
                       a.f_devolucion,
                       a.f_proceso,
                       a.usuario
                  FROM ocg_devolucion_ef a,
                       cat_entidad_financiera b
                 WHERE a.estado = 100
                   AND a.cve_ent_financiera = b.cve_ent_financiera"

   PREPARE prp_devolucion FROM v_qry
   DECLARE cur_devolucion CURSOR FOR prp_devolucion

   LET b = 1
   FOREACH cur_devolucion INTO arr_devolucion[b].*
      LET arr_devolucion[b].v_box = 0
      LET b = b+1
   END FOREACH

   CALL arr_devolucion.deleteElement(arr_devolucion.getLength())

   IF arr_devolucion.getLength() >= 1 THEN

   MENU
      ON ACTION Autorizar
         LET v_bnd = 1
         CALL fn_autorizar()
         EXIT MENU
      ON ACTION Actualizar
         LET v_bnd = 2
         CALL fn_actualizar()
         EXIT MENU
      ON ACTION Eliminar
         LET v_bnd = 3
         CALL fn_eliminar()
         EXIT MENU
      ON ACTION CANCEL
         EXIT MENU
   END MENU

   ELSE
      CALL fn_mensaje("Alerta", "No exísten montos pendientes","stop")
   END IF

END FUNCTION

FUNCTION fn_autorizar()

   DEFINE v_pos          INTEGER
   DEFINE v_bandera      SMALLINT
   DEFINE v_ef           CHAR(50)
   DEFINE v_f_dev        DATE
   DEFINE v_monto_dev    DECIMAL(12,5)
   DEFINE bnd_cancela    SMALLINT
   DEFINE v_f_nueva      DATE
   DEFINE v_monto_nuevo  DECIMAL(12,5)
   DEFINE v_cadena       STRING

   LET bnd_cancela = 0

      OPEN WINDOW OCGP091 WITH FORM "OCGP091"
         LET w = ui.Window.getCurrent()
         LET f = w.getForm()
         CALL f.setElementHidden("table2",1)
         CALL f.setElementHidden("table3",1)

         INPUT ARRAY arr_devolucion WITHOUT DEFAULTS FROM  tab_autorizar.*
         ATTRIBUTES (UNBUFFERED, INSERT ROW = FALSE,DELETE ROW = FALSE, APPEND ROW = FALSE )

         BEFORE ROW
            ON CHANGE v_box


      ON ACTION ACCEPT

      IF v_bnd = 1 THEN
         LET a = 0
         FOR b = 1 TO arr_devolucion.getLength()
            IF arr_devolucion[b].v_box = 1 THEN

               LET a = a+1

               --DISPLAY "arreglo : ", arr_devolucion[b].*

               UPDATE ocg_devolucion_ef
                  SET estado             = 110,
                      f_autorizacion     = TODAY,
                      usuario            = p_usuario_cod
                WHERE lote               = arr_devolucion[b].lote
                  AND cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                  AND monto_devolucion   = arr_devolucion[b].monto_dev
                  AND f_devolucion       = arr_devolucion[b].f_devolucion
                  AND estado = 100

            END IF
         END FOR
      END IF

         CALL fn_mensaje("Alerta", "Proceso realizado de forma correcta para registros seleccionados","stop")
         EXIT INPUT

         END INPUT
      CLOSE WINDOW OCGP091
END FUNCTION

FUNCTION fn_actualizar()

   DEFINE v_pos          INTEGER
   DEFINE v_bandera      SMALLINT
   DEFINE v_ef           CHAR(50)
   DEFINE v_f_dev        DATE
   DEFINE v_monto_dev    DECIMAL(12,5)
   DEFINE bnd_cancela    SMALLINT
   DEFINE v_f_nueva      DATE
   DEFINE v_monto_nuevo  DECIMAL(12,5)
   DEFINE v_cadena       STRING

   LET bnd_cancela = 0

      OPEN WINDOW OCGP091 WITH FORM "OCGP091"
         LET w = ui.Window.getCurrent()
         LET f = w.getForm()
         CALL f.setElementHidden("table1",1)
         CALL f.setElementHidden("table2",1)
         INPUT ARRAY arr_devolucion WITHOUT DEFAULTS FROM  tab_actualizar.*
         ATTRIBUTES (UNBUFFERED, INSERT ROW = FALSE,DELETE ROW = FALSE, APPEND ROW = FALSE )

         BEFORE ROW
            ON CHANGE v_box2

         IF v_bnd = 2 THEN
            LET a = 0
            LET v_pos = ARR_CURR()

            IF arr_devolucion[v_pos].v_box = 1 THEN

               FOR b = 1 TO arr_devolucion.getLength()
                  IF b <> v_pos THEN
                     LET arr_devolucion[b].v_box = 0
                     LET a = a+1
                  END IF
               END FOR
            END IF
         END IF


      ON ACTION ACCEPT

      IF (v_bnd = 2) AND
         (v_pos > 0) THEN
         OPEN WINDOW OCGP092 WITH FORM "OCGP092"

            --DISPLAY arr_devolucion[v_pos].cve_ent_financiera TO v_ef
            DISPLAY arr_devolucion[v_pos].f_devolucion       TO v_f_dev
            DISPLAY arr_devolucion[v_pos].ent_financiera     TO v_ef

            --INPUT BY NAME v_monto_dev ATTRIBUTES (UNBUFFERED)
            DISPLAY arr_devolucion[v_pos].monto_dev    TO v_monto_dev
            DISPLAY arr_devolucion[v_pos].f_devolucion TO v_f_dev

            INPUT BY NAME v_f_nueva,v_monto_nuevo ATTRIBUTES (UNBUFFERED)

            ON ACTION ACCEPT
               IF (v_f_nueva IS NULL) AND
                  (v_monto_nuevo IS NULL) THEN
                  CALL fn_mensaje("Alerta", "Debe ingresar un monto","stop")
               ELSE
                  IF v_f_nueva IS NULL THEN
                     LET v_f_nueva = v_f_dev
                  END IF
                  IF v_monto_nuevo IS NULL THEN
                     LET v_monto_nuevo = v_monto_dev
                  END IF

                  UPDATE ocg_devolucion_ef
                     SET monto_devolucion   = v_monto_nuevo,
                         f_devolucion       = v_f_nueva,
                         f_autorizacion     = TODAY
                   WHERE lote               = arr_devolucion[v_pos].lote
                     AND cve_ent_financiera = arr_devolucion[v_pos].cve_ent_financiera
                     AND monto_devolucion   = arr_devolucion[v_pos].monto_dev
                     AND f_devolucion       = arr_devolucion[v_pos].f_devolucion
                     AND estado = 100
                   --CALL fn_mensaje("Alerta", "Monto actualizado correctamente","stop")
                   EXIT INPUT
               END IF

            ON ACTION CANCEL
               LET bnd_cancela = 1
               EXIT INPUT
            END INPUT

      END IF

         IF (v_bnd       =  2) AND
            (bnd_cancela <> 1) AND
            (v_pos        > 0)  THEN
            CALL fn_mensaje("Alerta", "Monto actualizado de forma correcta","stop")
            EXIT INPUT
         ELSE
            IF bnd_cancela = 1 THEN
               CALL fn_mensaje("Alerta", "Actualización fué cancelada","stop")
            ELSE
               CALL fn_mensaje("Alerta", "Ningún registro fué seleccionado","stop")
            END IF
            EXIT INPUT
         END IF

         END INPUT
      CLOSE WINDOW OCGP091
END FUNCTION

FUNCTION fn_eliminar()

   DEFINE v_pos          INTEGER
   DEFINE v_bandera      SMALLINT
   DEFINE v_ef           CHAR(50)
   DEFINE v_f_dev        DATE
   DEFINE v_monto_dev    DECIMAL(12,5)
   DEFINE bnd_cancela    SMALLINT
   DEFINE v_f_nueva      DATE
   DEFINE v_monto_nuevo  DECIMAL(12,5)
   DEFINE v_cadena       STRING

   LET bnd_cancela = 0

      OPEN WINDOW OCGP091 WITH FORM "OCGP091"
         LET w = ui.Window.getCurrent()
         LET f = w.getForm()
         CALL f.setElementHidden("table1",1)
         CALL f.setElementHidden("table3",1)
         INPUT ARRAY arr_devolucion WITHOUT DEFAULTS FROM  tab_eliminar.*
         ATTRIBUTES (UNBUFFERED, INSERT ROW = FALSE,DELETE ROW = FALSE, APPEND ROW = FALSE )

         BEFORE ROW
            ON CHANGE v_box1




      ON ACTION ACCEPT

      IF v_bnd = 3 THEN
         CALL fn_ventana_confirma("Alterta","¿Esta seguro que desea continuar?","stop")
         RETURNING v_bandera

         IF v_bandera = 1 THEN
            LET a = 0
               FOR b = 1 TO arr_devolucion.getLength()
                  IF arr_devolucion[b].v_box = 1 THEN

                     LET a = a+1 

                     UPDATE ocg_devolucion_ef
                        SET estado             = 130,
                            f_autorizacion     = TODAY
                      WHERE lote               = arr_devolucion[b].lote
                        AND cve_ent_financiera = arr_devolucion[b].cve_ent_financiera
                        AND monto_devolucion   = arr_devolucion[b].monto_dev
                        AND f_devolucion       = arr_devolucion[b].f_devolucion
                        AND estado = 100
               END IF
           END FOR
         END IF
      END IF
      

         CALL fn_mensaje("Alerta", "Proceso realizado de forma correcta para registros seleccionados","stop")
         EXIT INPUT

         END INPUT
      CLOSE WINDOW OCGP091
END FUNCTION