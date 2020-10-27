#########################################################################
#Modulo            => AFI                                               #
#Programa          => AFIP24                                            #
#Objetivo          => actualización de marcas operativas                #
#                     para atencion de aclaraciones                     #
#Autor             => Jose Edaurdo Ventura                              #
#Fecha inicio      => 04 JUIO 2016                                      #
#########################################################################
DATABASE safre_viv

GLOBALS
   DEFINE g_usuario                 CHAR (20)
   DEFINE p_tpo_ejecucion           SMALLINT
   DEFINE p_s_titulo                STRING        -- Título de la ventana
   DEFINE v_s_qry                   STRING
   DEFINE v_bandera                 SMALLINT
   DEFINE v_cnt                     INTEGER
   DEFINE v_pos                     INTEGER
   DEFINE j                         INTEGER
   DEFINE i                         INTEGER
   DEFINE v_id_derechohabiente      DECIMAL(9,0)
   DEFINE v_tpo_proceso             SMALLINT
   DEFINE g_proceso_cod             SMALLINT
   DEFINE v_cod_rechazo             SMALLINT
   DEFINE v_cadena                  STRING
   DEFINE v_rch_desc                CHAR(50)
   DEFINE cb                        ui.ComboBox
   DEFINE w ui.Window
   DEFINE f ui.Form

END GLOBALS

MAIN
   DEFINE v_nss                     CHAR(11)
   DEFINE v_situacion               SMALLINT

   LET g_usuario       = ARG_VAL (1)
   LET p_tpo_ejecucion = ARG_VAL (2)
   LET p_s_titulo      = ARG_VAL (3)

   -- Creación de log
   CALL STARTLOG(g_usuario CLIPPED|| ".AFIP24.log")

   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   
   OPEN WINDOW AFIP241 WITH FORM "AFIP241"

      INPUT BY NAME v_nss ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE, CANCEL=FALSE)
         ON ACTION Marcar
            LET v_tpo_proceso = 1
            -- Se valida el nss
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca  la función que actualiza la marca
               CALL fn_act_marca(v_nss,v_tpo_proceso)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF
            EXIT INPUT

         ON ACTION Desmarcar
            let v_tpo_proceso = 2
            CALL fn_existe_nss(v_nss) RETURNING v_bandera
            IF v_bandera = 1 THEN
               -- Se invoca a la función que actualiza la desmarca
               CALL fn_act_desmarca(v_nss,v_tpo_proceso)
            ELSE
               CALL fn_mensaje("Alerta","El NSS ingresado no existe","stop")
            END IF
            EXIT INPUT
            
         ON ACTION Cancelar
            EXIT INPUT

      END INPUT
   CLOSE WINDOW AFIP241
END MAIN


FUNCTION fn_act_desmarca(p_nss,v_tpo_proceso)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss

   DEFINE v_tpo_proceso             SMALLINT
   DEFINE arr_desmarca DYNAMIC ARRAY OF RECORD
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      marca              SMALLINT,
      proceso_marca      SMALLINT,
      n_referencia       INTEGER,
      v_box1             SMALLINT
   END RECORD

   DEFINE v_bandera_tpo             SMALLINT

   CALL arr_desmarca.clear()

   IF v_tpo_proceso = 2 THEN

   OPEN WINDOW AFIP242 WITH FORM "AFIP242"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setElementHidden("table2",1)

   INPUT ARRAY arr_desmarca FROM tab_desmarca.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                           APPEND ROW = FALSE,
                                                           DELETE ROW = FALSE,
                                                           INSERT ROW = FALSE,
                                                           UNBUFFERED)

   BEFORE INPUT

   LET v_s_qry ="SELECT sfr.id_derechohabiente,
                        afi.nss,
                        sfr.marca,
                        sfr.proceso_marca,
                        sfr.n_referencia
                   FROM sfr_marca_activa sfr,afi_derechohabiente afi
                  WHERE sfr.id_derechohabiente = afi.id_derechohabiente
                    AND sfr.marca in (select marca from sfr_actualiza_marca
                                       where tpo_actualiza = 2)
                    and sfr.id_derechohabiente = ?"

   PREPARE prp_extrae_unificacion FROM v_s_qry

   DECLARE cur_unificacion CURSOR FOR prp_extrae_unificacion

   DISPLAY "consulta: ",v_s_qry

   LET v_cnt = 1

   FOREACH cur_unificacion USING v_id_derechohabiente
      INTO arr_desmarca[v_cnt].*
      DISPLAY "arreglo :",arr_desmarca[v_cnt].*
      DISPLAY "cuenta : ",v_cnt
      LET arr_desmarca[v_cnt].v_box1 = 0
      LET v_cnt = v_cnt + 1
   END FOREACH

   CALL arr_desmarca.deleteElement(arr_desmarca.getLength())

   ON ACTION ACCEPT

   FOR v_cnt = 1 TO arr_desmarca.getLength()
      IF arr_desmarca[v_cnt].v_box1 = 1 THEN
         LET v_s_qry = "EXECUTE FUNCTION fn_desmarca_cuenta(?,?,?,0,0,?,?)"

               PREPARE prp_exe_desmarca_interna FROM v_s_qry
               EXECUTE prp_exe_desmarca_interna USING arr_desmarca[v_cnt].id_derechohabiente ,
                                                      arr_desmarca[v_cnt].marca              ,
                                                      arr_desmarca[v_cnt].n_referencia       ,
                                                      g_usuario                              ,
                                                      arr_desmarca[v_cnt].proceso_marca
                                                 INTO v_cod_rechazo

               DISPLAY "La funcion de desmarca regresa : ", v_cod_rechazo

               IF v_cod_rechazo <> 0 THEN
                     SELECT rch_desc
                       INTO v_rch_desc
                       FROM cat_rch_marca
                      WHERE rch_cod = v_cod_rechazo

                  LET v_cadena = "Error de convivencia para \n NSS :" || arr_desmarca[v_cnt].nss ||
                                    "\n Marca : " || arr_desmarca[v_cnt].marca||" \n"||v_rch_desc

                  CALL fn_mensaje("Error",v_cadena,"stop")
                  DISPLAY v_cadena
              END IF
      END IF
   END FOR
   CALL fn_mensaje("Alerta","Se ejecutó proceso de desmarca para registros seleccionados","stop")
   EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW AFIP242

   END IF

END FUNCTION

FUNCTION fn_act_marca(p_nss,v_tpo_proceso)

   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_tpo_proceso             SMALLINT
   DEFINE v_bnd                     SMALLINT
   DEFINE v_cnt_marca               INTEGER
   DEFINE v_proceso                 SMALLINT

   DEFINE arr_marca DYNAMIC ARRAY OF RECORD
      id_derechohabiente DECIMAL(9,0),
      nss                CHAR(11),
      marca              CHAR(40),
     -- proceso_marca      SMALLINT,
      v_box2             SMALLINT
   END RECORD

   DEFINE v_bandera_tpo             SMALLINT

   IF v_tpo_proceso = 1 THEN

   OPEN WINDOW AFIP242 WITH FORM "AFIP242"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setElementHidden("table1",1)

   INPUT ARRAY arr_marca FROM tab_marca.* ATTRIBUTE( WITHOUT DEFAULTS,
                                                           APPEND ROW = FALSE,
                                                           DELETE ROW = FALSE,
                                                           INSERT ROW = FALSE,
                                                           UNBUFFERED)

   BEFORE INPUT
      CALL arr_marca.clear()
   
   LET cb = ui.ComboBox.forName("v_cb_marca")

   LET v_s_qry ="SELECT afi.id_derechohabiente,
                        afi.nss,
                        ''
                   FROM afi_derechohabiente afi
                  WHERE afi.id_derechohabiente = ?"

   PREPARE prp_marca FROM v_s_qry

   DECLARE cur_marca CURSOR FOR prp_marca

   CALL fn_llena_combo(1)

   LET v_cnt = 1

   FOREACH cur_marca USING v_id_derechohabiente
      INTO arr_marca[v_cnt].*

         --CALL fn_llena_combo()

      LET arr_marca[v_cnt].v_box2 = 0
      LET v_cnt = v_cnt + 1
   END FOREACH

   CALL arr_marca.deleteElement(arr_marca.getLength())

   ON ACTION ACCEPT

      LET v_bnd = 0

   FOR v_cnt = 1 TO arr_marca.getLength()
      IF arr_marca[v_cnt].v_box2 = 1 THEN
         DISPLAY "marca : ", arr_marca[v_cnt].marca

         SELECT COUNT(*)
           INTO v_cnt_marca
           FROM sfr_marca_activa
          WHERE id_derechohabiente = arr_marca[v_cnt].id_derechohabiente
            AND marca = arr_marca[v_cnt].marca

         DISPLAY "cuenta marca :",v_cnt_marca

         IF v_cnt_marca >= 1 THEN
            CALL fn_mensaje("Alerta","Ya exíste marca activa para registro seleccionado","stop")
            LET v_bnd = 1
         END IF

         IF(v_bnd = 0) AND
           (arr_marca[v_cnt].v_box2 = 1) THEN

           LET v_s_qry = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,0,0,TODAY,?,?)"

              SELECT proceso_cod
                INTO v_proceso
               FROM sfr_actualiza_marca
              WHERE marca = arr_marca[v_cnt].marca
                AND tpo_actualiza = 2

               PREPARE prp_exe_marca_int FROM v_s_qry
               EXECUTE prp_exe_marca_int USING arr_marca[v_cnt].id_derechohabiente,
                                               arr_marca[v_cnt].marca,
                                               arr_marca[v_cnt].id_derechohabiente,
                                               "0",
                                               "",
                                               g_usuario,
                                               v_proceso
                                       INTO v_cod_rechazo

               DISPLAY "La funcion de marca regresa : ", v_cod_rechazo

               IF v_cod_rechazo <> 0 THEN
                  IF v_cod_rechazo = "-239" THEN
                     LET v_cadena = "Marca :",arr_marca[v_cnt].marca CLIPPED," solicitada con anterioridad "," \n Favor de verificar"
                  ELSE
                     SELECT rch_desc
                       INTO v_rch_desc
                       FROM cat_rch_marca
                      WHERE rch_cod = v_cod_rechazo

                     LET v_cadena = "Error de convivencia para \n NSS :" || arr_marca[v_cnt].nss ||
                                    "\n Marca : " || arr_marca[v_cnt].marca||" \n"||v_rch_desc
                  END IF
                  CALL fn_mensaje("Error",v_cadena,"stop")
                  DISPLAY v_cadena
                  EXIT INPUT
                  CALL arr_marca.clear()
               END IF

               IF v_cod_rechazo = 0 THEN
                  CALL fn_mensaje("Alerta","Se ejecutó proceso de marca para registros seleccionados","stop")
                  EXIT INPUT
               END IF
         END IF

      ELSE
         CALL fn_mensaje("Alerta","Es necesario seleccionar y aplicar una marca para continuar","stop")
      END IF
   END FOR

   ON ACTION CANCEL
      EXIT INPUT 
   END INPUT

   CLOSE WINDOW AFIP242

   END IF

END FUNCTION

FUNCTION fn_llena_combo(v_bnd_cb)

   DEFINE v_qry    STRING
   DEFINE v_bnd_cb SMALLINT
   DEFINE v_cadena STRING
   DEFINE arr_cb_marca DYNAMIC ARRAY OF RECORD
      marca       SMALLINT,
      marca_desc  CHAR(40)
   END RECORD

   IF v_bnd_cb = 1 THEN
      LET v_cadena = "AND sfr.marca <> 213"
   ELSE
      LET v_cadena = " "
   END IF

   LET v_qry = "SELECT sfr.marca,
                       sfr.marca||'-'||cat.proceso_desc
                  FROM sfr_actualiza_marca sfr,cat_proceso cat
                 WHERE sfr.proceso_cod = cat.proceso_cod
                   AND tpo_actualiza = 2 ",v_cadena

   PREPARE prp_combo FROM v_qry
   DECLARE cur_combo CURSOR FOR prp_combo

   LET v_cnt = 1

   FOREACH cur_combo INTO arr_cb_marca[v_cnt].*
      CALL cb.addItem(arr_cb_marca[v_cnt].marca, arr_cb_marca[v_cnt].marca_desc)
      LET v_cnt = v_cnt + 1
   END FOREACH

END FUNCTION

FUNCTION fn_existe_nss(p_nss)
   DEFINE p_nss                     LIKE afi_derechohabiente.nss
   DEFINE v_ax_nss                  LIKE afi_derechohabiente.nss

   LET v_s_qry = " SELECT nss,id_derechohabiente
                     FROM afi_derechohabiente
                    WHERE nss = ? "

   PREPARE prp_exi_nss FROM v_s_qry
   EXECUTE prp_exi_nss USING p_nss INTO v_ax_nss,v_id_derechohabiente

   -- no existe el nss
   IF v_ax_nss IS NULL THEN
      LET v_bandera = 0
   ELSE
   --si existe
      LET v_bandera = 1
   END IF

   RETURN v_bandera
END FUNCTION