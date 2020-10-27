#################################################################################
#Modulo              => OCG                                                     #
#Programa            => OCGC10                                                  #
#Objetivo            => PANTALLA DE CONSULTA PARA FICHAS DE DEPOSITO            #
#                       DE DEVOLUCIONES                                         #
#Autor               => JOSÉ EDUARDO VENTURA                                    #
#Fecha inicio        => 20 de JULIO del 2016                                    #
#################################################################################

DATABASE safre_viv

   DEFINE v_qry             STRING
   DEFINE v_cadena          STRING
   DEFINE p_usuario         CHAR(20)
   DEFINE p_tpo_ejecucion   SMALLINT
   DEFINE p_s_titulo        CHAR(20)
--   DEFINE v_nss            CHAR(11)
--  DEFINE v_f_proceso       DATE
   DEFINE v_f_devolucion    DATE
   DEFINE v_cve_ef          SMALLINT
   DEFINE v_lote            CHAR(11)
   DEFINE cb                ui.ComboBox

MAIN
      -- se recupera la clave de usuario desde parametro 
   LET p_usuario       = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_s_titulo      = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGC11.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN

   OPEN WINDOW OCGC111 WITH FORM "OCGC111"

   INPUT BY NAME v_cve_ef,
                 v_f_devolucion,
                 v_lote ATTRIBUTES(UNBUFFERED)

   BEFORE INPUT
      LET cb = ui.ComboBox.forName("v_cve_ef")
       CALL fn_llena_combo()

   ON ACTION ACCEPT

   LET v_cadena = " "

   IF v_cve_ef IS NOT NULL THEN
      LET v_cadena = v_cadena," AND a.cve_ent_financiera = ",v_cve_ef
   END IF

   IF v_f_devolucion IS NOT NULL THEN
      LET v_cadena =v_cadena, " AND a.f_devolucion = ","'",v_f_devolucion,"'"
   END IF

   IF v_lote IS NOT NULL THEN
      LET v_cadena = v_cadena," AND a.lote = ",v_lote
   END IF

      CALL fn_consulta_dev()
   --EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   
   CLOSE WINDOW OCGC111

END MAIN

FUNCTION fn_consulta_dev()

   DEFINE a          INTEGER
   DEFINE v_detalle  STRING
   DEFINE ch         base.Channel
   DEFINE v_nom_arh  STRING
   DEFINE v_ruta_envio CHAR(50)
   DEFINE v_mensaje  STRING

   DEFINE arr_cons_ficha DYNAMIC ARRAY OF RECORD
      lote                   CHAR(20),
      ent_financiera         CHAR(40),
      monto_devolucion       DECIMAL(12,2),
      monto_devuelto         DECIMAL(12,2),
      monto_pendiente        DECIMAL(12,2),
      f_devolucion           DATE,
      f_autorizacion         DATE,
      f_proceso              DATE,
      estado                 CHAR(50),
      usuario                CHAR(20)
   END RECORD

   LET v_qry = "SELECT a.lote,
                       b.ent_financiera_desc,
                       a.monto_devolucion,
                       a.monto_devuelto,
                       '',
                       a.f_devolucion,
                       a.f_autorizacion,
                       a.f_proceso,
                       c.edo_devolucion_desc,
                       a.usuario
                  FROM ocg_devolucion_ef a,
                       cat_entidad_financiera b,
                       cat_edo_devolucion c
                 WHERE a.cve_ent_financiera = b.cve_ent_financiera
                   AND a.estado = c.edo_devolucion ",v_cadena

   DISPLAY " qry : ",v_qry

   PREPARE prp_ficha FROM v_qry
   DECLARE cur_ficha CURSOR FOR prp_ficha

   LET a = 1

   FOREACH cur_ficha INTO arr_cons_ficha[a].*

      IF arr_cons_ficha[a].monto_devuelto IS NULL THEN
         LET arr_cons_ficha[a].monto_devuelto = 0
      END IF

      LET arr_cons_ficha[a].monto_pendiente = arr_cons_ficha[a].monto_devolucion - arr_cons_ficha[a].monto_devuelto

      IF arr_cons_ficha[a].monto_pendiente IS NULL THEN
         LET arr_cons_ficha[a].monto_pendiente = 0
      END IF

      LET a = a+1

   END FOREACH

   CALL arr_cons_ficha.deleteElement(arr_cons_ficha.getLength())

   IF arr_cons_ficha.getLength() >= 1 THEN

      OPEN WINDOW OCGC112 WITH FORM "OCGC112"
         DISPLAY ARRAY arr_cons_ficha TO tab_cons_ficha.* ATTRIBUTES (UNBUFFERED, CANCEL = FALSE)

         ON ACTION ACCEPT
         EXIT DISPLAY

         ON ACTION archivo

            SELECT ruta_envio
              INTO v_ruta_envio
              FROM seg_modulo
             WHERE modulo_cod = 'ocg'

            LET ch = base.Channel.create()
            LET v_nom_arh = v_ruta_envio CLIPPED,"/Extractor_fichas.txt"

            CALL ch.openFile(v_nom_arh,"w" )
            CALL ch.setDelimiter(NULL)

            FOR a = 1 TO arr_cons_ficha.getLength()
            
               LET v_detalle = arr_cons_ficha[a].ent_financiera CLIPPED,"|",
                               arr_cons_ficha[a].monto_devolucion CLIPPED,"|",
                               arr_cons_ficha[a].monto_devuelto CLIPPED,"|",
                               arr_cons_ficha[a].monto_pendiente CLIPPED,"|",
                               arr_cons_ficha[a].f_devolucion CLIPPED USING "yyyymmdd","|",
                               arr_cons_ficha[a].f_autorizacion CLIPPED USING "yyyymmdd","|",
                               arr_cons_ficha[a].f_proceso CLIPPED USING "yyyymmdd","|",
                               arr_cons_ficha[a].estado CLIPPED

               CALL ch.writeLine([v_detalle])

            END FOR

               CALL ch.close()

               LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
               CALL fn_mensaje("Alerta", v_mensaje,"stop")

      END DISPLAY
      CLOSE WINDOW OCGC112
   ELSE
      CALL fn_mensaje ("Archivo","No se encontró información para parámetro de búsqueda ingresado","information")
   END IF

   CALL arr_cons_ficha.clear()

   LET v_cve_ef        = NULL
   LET v_f_devolucion  = NULL
   LET v_lote          = NULL

END FUNCTION

FUNCTION fn_llena_combo()

   DEFINE v_qry    STRING
   DEFINE v_cadena STRING
   DEFINE v_cnt    SMALLINT
   DEFINE arr_cb_entidad DYNAMIC ARRAY OF RECORD
      entidad       SMALLINT,
      entidad_desc  CHAR(40)
   END RECORD

   LET v_qry = "SELECT cve_ent_financiera,
                       cve_ent_financiera||'-'||ent_financiera_desc
                  FROM cat_entidad_financiera
                 WHERE estado_ef = 10 ORDER BY cve_ent_financiera asc"

   PREPARE prp_combo FROM v_qry
   DECLARE cur_combo CURSOR FOR prp_combo

   LET v_cnt = 1

   FOREACH cur_combo INTO arr_cb_entidad[v_cnt].*
      CALL cb.addItem(arr_cb_entidad[v_cnt].entidad, arr_cb_entidad[v_cnt].entidad_desc)
      LET v_cnt = v_cnt + 1
   END FOREACH

END FUNCTION