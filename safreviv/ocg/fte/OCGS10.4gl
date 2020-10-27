--===============================================================
-- Versi�n: 1.0.0
-- Fecha �ltima modificaci�n:
--===============================================================

#####################################################################
#Modulo            => OCG                                           #
#Programa          => OCGS10                                        #
#Objetivo          => Extractor con desglose de pagos SP003 y SP004 #
#Autor             => Jos� Eduardo Ventura                          #
#Fecha inicio      => 09 septiembre 2017                            #
#####################################################################

DATABASE safre_viv

GLOBALS
   DEFINE p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
   DEFINE p_s_titulo            STRING   -- titulo de la ventana
   DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod

   DEFINE v_nss                 CHAR(11)
   DEFINE v_f_desde             DATE
   DEFINE v_f_hasta             DATE
   DEFINE v_entidad             SMALLINT
   DEFINE v_cb_concepto         SMALLINT
   DEFINE cb                    ui.ComboBox
   DEFINE cb_c                  ui.ComboBox
   DEFINE v_ug                  SMALLINT
   DEFINE v_ap                  SMALLINT
   DEFINE ch                    base.Channel
   DEFINE v_cadena              STRING
   DEFINE v_dev_ug              SMALLINT
   DEFINE v_dev_as              SMALLINT
   DEFINE v_dev                 SMALLINT

   DEFINE arr_cb_entidad DYNAMIC ARRAY OF RECORD
      entidad            CHAR(3),
      entidad_desc       CHAR(40)
   END RECORD

   DEFINE arr_transaccion DYNAMIC ARRAY OF RECORD
      cve_ent             CHAR(3),
      entidad             CHAR(80),
      nss                 CHAR(11),
      concepto            CHAR(50),
      viv_97              DECIMAL(12,2),
      periodo             CHAR(6),
      f_pago              DATE
   END RECORD

END GLOBALS

MAIN

   DEFINE v_msj                     STRING

--CLOSE WINDOW SCREEN

    -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CALL STARTLOG(p_usuario_cod CLIPPED|| ".OCGS10.log")

   CALL fn_consulta_1()

END MAIN

FUNCTION fn_consulta_1()

   DEFINE v_qry_t      STRING
   DEFINE a            SMALLINT
   DEFINE v_concepto   STRING
   DEFINE bnd_consulta SMALLINT

   OPEN WINDOW OCGGS10 WITH FORM "OCGS101"

   LET cb   = ui.ComboBox.forName("v_entidad")
   LET cb_c = ui.ComboBox.forName("v_cb_concepto")

   INPUT BY NAME v_nss,
                 v_f_desde,
                 v_f_hasta,
                 v_entidad,
                 v_cb_concepto ATTRIBUTES(UNBUFFERED)
   BEFORE INPUT
      CALL fn_combo()

      ON ACTION ACCEPT

         IF v_cb_concepto = 1 THEN
            LET v_ap = 1
         END IF

         IF v_cb_concepto = 2 THEN
            LET v_ug = 1
         END IF

         IF v_cb_concepto = 3 THEN
            LET v_dev_ug = 1
         END IF

         IF v_cb_concepto = 4 THEN
            LET v_dev_as = 1
         END IF

         IF v_cb_concepto = 5 THEN
            LET v_dev = 1
         END IF

         IF v_cb_concepto = 6 THEN
            LET v_ap      = 1 
            LET v_ug      = 1
            LET v_dev     = 1
         END IF

         IF v_cb_concepto IS NULL THEN
            CALL fn_mensaje("Alerta", "Debe ingresar concepto","stop")
            LET v_nss      = NULL
            LET v_f_desde  = NULL
            LET v_f_hasta  = NULL
            LET v_entidad  = NULL
            LET v_concepto = NULL
            LET bnd_consulta = 0
         ELSE

            IF v_f_desde IS NULL AND
               v_f_hasta IS NULL THEN
               CALL fn_mensaje("Alerta", "Es necesario ingresar un rango de fechas","stop")
               LET v_nss      = NULL
               LET v_f_desde  = NULL
               LET v_f_hasta  = NULL
               LET v_entidad  = NULL
               LET v_concepto = NULL
               LET bnd_consulta = 0
            ELSE
               LET bnd_consulta = 1
            END IF
         END IF

         IF bnd_consulta = 1 THEN

         LET v_cadena = " "

         --IF v_ap IS NULL AND
         --   v_ug IS NULL THEN
            
         --   LET v_ap = 0
         --   LET v_ug = 0
         --END IF

         IF v_nss IS NOT NULL THEN
            LET v_cadena = v_cadena," AND nss = ",v_nss
            --DISPLAY "cadena 0 : ",v_cadena
         END IF

         IF v_f_hasta IS NOT NULL THEN
            LET v_f_hasta = v_f_hasta
         ELSE
            LET v_f_hasta = TODAY
         END IF

         IF v_f_desde IS NOT NULL THEN
            LET v_cadena = v_cadena, " AND f_pago BETWEEN '",v_f_desde, "' AND '",v_f_hasta,"'"
             --DISPLAY "cadena 1 : ",v_cadena
         END IF

         IF v_entidad IS NOT NULL THEN
            --DISPLAY "entidad : ",v_entidad
            IF v_entidad <> "00" THEN
               LET v_cadena = v_cadena," AND t.cve_ent_financiera in ( ",v_entidad," )"
               --DISPLAY "cadena 2 : ",v_cadena
            END IF
         END IF

         LET v_concepto = NULL

         IF v_ug = 1 THEN
            LET v_concepto = " 407,417 "
         END IF

         IF v_ap = 1 THEN
            IF v_concepto IS NULL THEN
               LET v_concepto = " 807,107,307,817,117,317 "
            ELSE
               LET v_concepto = v_concepto,",807,107,307,817,117,317"
            END IF
         END IF

         IF v_ap = 0 AND
            v_ug = 0 THEN
            LET v_concepto = " 407,417,807,107,307,817,117,317 "
         END IF

         IF v_dev_ug = 1 THEN
            LET v_concepto = " 608 "
         END IF

         IF v_dev_as = 1 THEN
            LET v_concepto = " 508 "
         END IF

         IF v_dev = 1 THEN 
            IF v_ap = 1 OR
               v_ug = 1 THEN
               LET v_concepto = v_concepto, " ,508,608 "
            ELSE
               LET v_concepto = " 508,608 "
            END IF
         END IF

         LET v_concepto = "(",v_concepto,")"

         LET v_cadena = v_cadena," AND concepto IN ",v_concepto

         --DISPLAY "cadena 3 : ",v_cadena
         
         IF v_cadena IS NOT NULL OR
            v_cadena <> " " THEN

            LET v_qry_t = "SELECT first 32000 c.cve_ent_financiera,
                                  c.ent_financiera_desc,
                                  t.nss,
                                  t.concepto,
                                  t.vivienda_97,
                                  periodo_pago,
                                  t.f_pago
                             FROM ocg_ctr_transaccion t,
                                  cat_entidad_financiera c
                            WHERE 1 = 1
                              AND t.cve_ent_financiera = c.cve_ent_financiera ",v_cadena

            PREPARE prp_transaccion FROM v_qry_t
            DECLARE cur_transaccion CURSOR FOR prp_transaccion

            LET a = 1
            FOREACH cur_transaccion INTO arr_transaccion[a].*

               IF arr_transaccion[a].concepto = "407" OR
                  arr_transaccion[a].concepto = "417" THEN
                  LET arr_transaccion[a].concepto = "Uso de Garant�a"
               ELSE
                  IF arr_transaccion[a].concepto = "508" THEN
                     LET arr_transaccion[a].concepto = "Devoluci�n AS"
                  ELSE
                     IF arr_transaccion[a].concepto = "608" THEN
                        LET arr_transaccion[a].concepto = "Devoluci�n UG"
                     ELSE
                        LET arr_transaccion[a].concepto = "Aportaci�n Subsecuente"
                     END IF
                  END IF
               END IF

               LET a = a +1
            END FOREACH

            CALL arr_transaccion.deleteElement(a)
         END IF

         IF arr_transaccion.getLength() <= 1 OR
            v_cadena IS NULL THEN
            CALL fn_mensaje("Alerta", "No ex�sten registros con los datos ingresados","stop")
            EXIT INPUT
         END IF

      --EXIT INPUT

         OPEN WINDOW OCGS102 WITH FORM "OCGS102"

         DISPLAY ARRAY arr_transaccion TO tab_transaccion.*

           ON ACTION ACCEPT
              EXIT DISPLAY

           ON ACTION archivo
              --CALL fn_archivo()
             CALL fn_archivo_full()

           ON ACTION CANCEL
              LET v_ap          = 0
              LET v_ug          = 0
              LET v_dev         = 0
              LET v_dev_ug      = 0
              LET v_dev_as      = 0
              LET bnd_consulta  = 0
              LET v_cb_concepto = NULL
              LET v_nss         = NULL
              LET v_f_desde     = NULL
              LET v_f_hasta     = NULL
              LET v_entidad     = NULL
              CALL arr_transaccion.clear()
              CALL arr_cb_entidad.clear()
              EXIT DISPLAY

         END DISPLAY

         CLOSE WINDOW OCGS102

         END IF

      ON ACTION CANCEL
      EXIT INPUT

   END INPUT

END FUNCTION

FUNCTION fn_combo()

   DEFINE v_qry  STRING
   DEFINE a      SMALLINT

   LET v_qry = "SELECT cve_ent_financiera,
                       cve_ent_financiera||'-'||
                       ent_financiera_desc 
                  FROM cat_entidad_financiera "

   PREPARE prp_combo FROM v_qry
   DECLARE cur_combo CURSOR FOR prp_combo

   LET a = 1

   FOREACH cur_combo INTO arr_cb_entidad[a].*
      LET a = a + 1 
   END FOREACH

   CALL arr_cb_entidad.deleteElement(a)

   FOR a = 1 TO arr_cb_entidad.getLength()
      CALL cb.addItem(arr_cb_entidad[a].entidad,arr_cb_entidad[a].entidad_desc)
   END FOR

   LET a = a+1

   CALL cb.addItem("00","TODAS")

   CALL cb_c.addItem("1","Aportaciones Subsecuentes")
   CALL cb_c.addItem("2","Uso de Garant�a")
   CALL cb_c.addItem("3","Devoluciones UG")
   CALL cb_c.addItem("4","Devoluciones AS")
   CALL cb_c.addItem("5","Devoluciones")
   CALL cb_c.addItem("6","Todos")

END FUNCTION

FUNCTION fn_archivo()

   DEFINE b             INTEGER
   DEFINE v_detalle     STRING
   DEFINE v_nom_arh     STRING
   DEFINE v_ruta_envio  CHAR(50)
   DEFINE v_mensaje     STRING

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

    DISPLAY "ruta : ",v_ruta_envio

   LET ch = base.Channel.create()

   LET v_nom_arh = v_ruta_envio CLIPPED,"/Extractor_transacciones.txt"

   DISPLAY "v_nom_arch = ", v_nom_arh

   CALL ch.openFile(v_nom_arh,"w" )
   CALL ch.setDelimiter(NULL)

      FOR b = 1 TO arr_transaccion.getLength()

         LET v_detalle = arr_transaccion[b].cve_ent CLIPPED,"|",
                         arr_transaccion[b].entidad CLIPPED,"|",
                         arr_transaccion[b].nss CLIPPED,"|",
                         arr_transaccion[b].concepto CLIPPED,"|",
                         arr_transaccion[b].viv_97 CLIPPED,"|",
                         arr_transaccion[b].periodo CLIPPED,"|",
                         arr_transaccion[b].f_pago CLIPPED USING "yyyymmdd"

         --DISPLAY "detalle : ",v_detalle

         CALL ch.writeLine([v_detalle])

      END FOR

   CALL ch.close()

   LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
   CALL fn_mensaje("Alerta", v_mensaje,"stop")

END FUNCTION

FUNCTION fn_archivo_full()

   DEFINE b             INTEGER
   DEFINE v_detalle     STRING
   DEFINE v_nom_arh     STRING
   DEFINE v_ruta_envio  CHAR(50)
   DEFINE v_mensaje     STRING
   DEFINE v_qry_t       STRING

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   DISPLAY "ruta : ",v_ruta_envio

   LET ch = base.Channel.create()

   LET v_nom_arh = v_ruta_envio CLIPPED,"/Extractor_transacciones.txt"

   LET v_qry_t = "
         SELECT c.cve_ent_financiera,
                c.ent_financiera_desc,
                t.nss,
                t.concepto,
                con.desc_concepto_ocg,
                t.vivienda_97,
                periodo_pago,
                YEAR (t.f_pago)||
                lpad(MONTH(t.f_pago),2,0)||
                lpad(DAY(t.f_pago),2,0) as f_pago
           FROM ocg_ctr_transaccion t,
                cat_entidad_financiera c,
                cat_concepto_ocg con
          WHERE 1 = 1
            AND t.concepto = con.cod_concepto_ocg
            AND t.cve_ent_financiera = c.cve_ent_financiera ",v_cadena, " INTO temp tmp_det_ex "

      PREPARE prp_qry_t FROM v_qry_t
      EXECUTE prp_qry_t

      UPDATE tmp_det_ex
         SET desc_concepto_ocg = "Aportaci�n Subsecuente"
       WHERE concepto IN (807,107,307,817,117,317)

      UPDATE tmp_det_ex
         SET desc_concepto_ocg = "Uso Garant�a"
       WHERE concepto IN (407,417)

      UPDATE tmp_det_ex
         SET desc_concepto_ocg = "Devoluci�n AS"
       WHERE concepto IN (508)

      UPDATE tmp_det_ex
         SET desc_concepto_ocg = "Devoluci�n UG"
       WHERE concepto IN (608)

      UNLOAD TO '/safreviv_int/ocg/envio/Extractor_transacciones.txt'
       select *  from tmp_det_ex

   LET v_mensaje = "Archivo generado de forma correcta en : ",v_nom_arh
   CALL fn_mensaje("Alerta", v_mensaje,"stop")

 
END FUNCTION
