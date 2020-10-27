##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM07                                             #
#Objetivo          => Mantenimiento a catálogo "Municipios INEGI"        #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => 23/Noviembre/2014                                  #
##########################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_titulo                 STRING    -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE g_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa
   DEFINE v_cb_entidad             INTEGER
   DEFINE arr_entidad              DYNAMIC ARRAY OF RECORD LIKE cat_entidad_federativa.*
   DEFINE cb                       ui.ComboBox
   DEFINE a                        INTEGER
   DEFINE v_qry                    STRING
   DEFINE f                        ui.Form
   DEFINE w                        ui.Window

   DEFINE arr_consulta DYNAMIC ARRAY OF RECORD
          municipio CHAR(50)
   END RECORD
END GLOBALS

MAIN

   LET g_usuario          =   ARG_VAL  (1)
   LET g_tipo_ejecucion   =   ARG_VAL  (2)
   LET g_titulo           =   ARG_VAL  (3)

   -- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM12.log")
   
--*********************************************************************************
--Menú de selección de acciones para ALTA,BAJA o CONSULTA en cat_municipio_inegi  *
--*********************************************************************************
   MENU --"SELECCIONE UNA ACCIÓN A REALIZAR"

      ON ACTION agregar
         CALL fn_alta()

      ON ACTION eliminar
         CALL fn_baja()

      ON ACTION consulta
        CALL fn_consulta_gral()

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU
   END MENU

END MAIN

FUNCTION fn_consulta_gral()

   DEFINE v_cuenta             INTEGER  -- Variable que valida que existan registros en tabla cat_municipio_inegi
   DEFINE v_pos                INTEGER

   OPEN WINDOW datos_generales WITH FORM "CATM121"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   CALL f.setFieldHidden  ("v_mun",1)
   CALL f.setFieldHidden  ("v_mun_desc",1)
   CALL f.setElementHidden("lb_clave",1)
   CALL f.setElementHidden("lb_municipio",1)

      CALL fn_cb_entidad()
      DISPLAY "v_entidad",v_cb_entidad

   INPUT BY NAME v_cb_entidad ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS, ACCEPT = FALSE, CANCEL=FALSE)

   BEFORE INPUT
      DISPLAY BY NAME v_cb_entidad

   ON ACTION consultar
      CALL arr_consulta.clear()
      IF (v_cb_entidad IS NOT NULL) THEN

      SELECT COUNT (*)
        INTO v_cuenta
        FROM cat_municipio_inegi

      DISPLAY "cuenta municipio : ",v_cuenta

      IF v_cuenta = 0 THEN
         CALL fn_mensaje("Atención","No se encontraron registros para esta consulta", "stop")

      ELSE
         --CALL f.setElementHidden("tab_municipios",0)
         LET v_qry = "select municipio||' '||' '||municipio_desc
                        from cat_municipio_inegi
                       where entidad_federativa = (select entidad_federativa
                                                     from cat_entidad_federativa
                                                    where entidad_federativa = ","'",v_cb_entidad CLIPPED,"'",")
                         and estado = 1"

         PREPARE prp_consulta FROM v_qry
         DECLARE cur_consulta CURSOR FOR prp_consulta

         LET a = 1

         FOREACH cur_consulta INTO arr_consulta[a].*
            LET a = a +1
         END FOREACH

         IF arr_consulta[arr_consulta.getLength()].municipio IS NULL AND
            a > 1 THEN
            CALL arr_consulta.deleteElement(arr_consulta.getLength())
         END IF

         DISPLAY "arr municipio : ", arr_consulta[1].municipio

         IF arr_consulta[1].municipio IS NOT NULL THEN
            DISPLAY ARRAY arr_consulta TO tab_municipios.* ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)

            ON ACTION editar
               LET v_pos = arr_curr()
               CALL fn_editar(v_pos)
               EXIT DISPLAY

            ON ACTION salir
               EXIT DISPLAY

               END DISPLAY
               EXIT INPUT
            CALL DIALOG.setActionHidden( "consultar", 0 )
         ELSE
            CALL fn_mensaje ("Atención","No se encontraron registros para esta consulta", "stop")
            EXIT INPUT
         END IF
      END IF
   END IF

   ON ACTION salir
   EXIT INPUT
   END INPUT
   CLOSE WINDOW datos_generales

END FUNCTION

FUNCTION fn_alta()
   DEFINE v_mun       INTEGER
   DEFINE v_mun_desc  CHAR(50)
   DEFINE v_bnd1      SMALLINT
   DEFINE v_cta_clave SMALLINT
   DEFINE v_cta_mun   SMALLINT
   DEFINE v_bnd2      SMALLINT
   DEFINE r_confirma  SMALLINT

   OPEN WINDOW alta WITH FORM "CATM121"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   --CALL f.setFieldHidden  ("v_cb_entidad",1)
   CALL f.setElementHidden("tab_municipios",1)


   LET v_cb_entidad = ""

   CALL fn_cb_entidad()
   DISPLAY BY NAME v_cb_entidad
   LET v_mun = ""
   LET v_mun_desc = ""

   INPUT BY NAME v_cb_entidad,v_mun,v_mun_desc ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS)

   ON ACTION ACCEPT

      LET v_mun_desc = v_mun_desc 
      IF v_mun IS NULL THEN
         CALL fn_mensaje ("Atención","Debe ingresar una clave de municipio válida", "stop")
      ELSE

         LET v_cta_clave = 0

         SELECT COUNT(*)
           INTO v_cta_clave
           FROM cat_municipio_inegi
          WHERE municipio = v_mun
            AND entidad_federativa = v_cb_entidad

         IF v_cta_clave > 0 THEN
            LET v_bnd1 = 0
            CALL fn_ventana_confirma ("Atención","Ya exíste un municipio con la clave ingresada \n ¿ Desea reactivarla ?", "stop") RETURNING r_confirma
               IF r_confirma = 1 THEN
                  UPDATE cat_municipio_inegi
                     SET estado = 1
                   WHERE municipio = v_mun
                  CALL fn_mensaje ("Atención","Reactivación exitosa", "stop")
               ELSE
                  CALL fn_mensaje ("Atención","Reactivación de municipio cancelada", "stop")
               END IF
               --NEXT FIELD v_mun
               EXIT INPUT
         ELSE
            LET v_bnd1 = 1
         END IF
      END IF

      IF (v_bnd1 = 1 )THEN
         IF (v_mun_desc IS NULL) THEN
            CALL fn_mensaje ("Atención","Debe ingresar nombre o descripción de municipio", "stop")
            NEXT FIELD v_mun_desc
         ELSE

            LET v_cta_mun = 0

            SELECT COUNT(*)
              INTO v_cta_mun
              FROM cat_municipio_inegi
             WHERE municipio_desc = v_mun_desc
               AND municipio = v_mun
               AND entidad_federativa = v_cb_entidad

            IF v_cta_mun > 0 THEN
               LET v_bnd2 = 0
               CALL fn_mensaje ("Atención","Ya exíste un municipio con la descripción ingresada", "stop")
               NEXT FIELD v_mun_desc
            ELSE
               LET v_bnd2 = 1
            END IF
         END IF
      END IF

      IF (v_mun IS NOT NULL)      AND 
         (v_mun_desc IS NOT NULL) AND 
         (v_bnd1 =1)              AND 
         (v_bnd2 = 1)             THEN

         INSERT INTO cat_municipio_inegi VALUES ( v_mun,
                                                  v_cb_entidad,
                                                  v_mun_desc,
                                                  "1",
                                                  TODAY,
                                                  g_usuario)

         CALL fn_mensaje ("Atención","Alta de municipio se realizó de forma correcta", "stop")
         EXIT INPUT
      END IF

      ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   CLOSE WINDOW alta

END FUNCTION

FUNCTION fn_baja()

   DEFINE v_municipio CHAR(50)
   DEFINE v_pos       CHAR (50)

   OPEN WINDOW baja WITH FORM "CATM121"

   LET v_cb_entidad = ""

   CALL fn_cb_entidad()
   DISPLAY BY NAME v_cb_entidad

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   --CALL f.setElementHidden("tab_municipios",1)
   CALL f.setFieldHidden  ("v_mun",1)
   CALL f.setFieldHidden  ("v_mun_desc",1)
   CALL f.setElementHidden("lb_clave",1)
   CALL f.setElementHidden("lb_municipio",1)

   INPUT BY NAME v_cb_entidad ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS)

   ON ACTION ACCEPT

   LET v_qry = "select municipio||' '||' '||municipio_desc
                        from cat_municipio_inegi
                       where entidad_federativa = (select entidad_federativa
                                                     from cat_entidad_federativa
                                                    where entidad_federativa = ","'",v_cb_entidad CLIPPED,"'",")
                         and estado = 1"

         PREPARE prp_municipio FROM v_qry
         DECLARE cur_municipio CURSOR FOR prp_municipio

         LET a = 1

         FOREACH cur_municipio INTO arr_consulta[a].*
            LET a = a +1
         END FOREACH

         IF arr_consulta[arr_consulta.getLength()].municipio IS NULL AND
            a > 1 THEN
            CALL arr_consulta.deleteElement(arr_consulta.getLength())
         END IF

         IF arr_consulta[1].municipio IS NOT NULL THEN
            DISPLAY ARRAY arr_consulta TO tab_municipios.* ATTRIBUTES (CANCEL=FALSE)

            ON ACTION ACCEPT

               LET v_municipio = arr_consulta [ARR_CURR()].municipio

               UPDATE cat_municipio_inegi
                  SET estado = 2
                WHERE municipio||' '||' '||municipio_desc = v_municipio


               CALL fn_mensaje ("Atención","Baja lógica de municipio se realizó de forma correcta", "stop")
               EXIT DISPLAY
               EXIT INPUT

            ON ACTION salir
               EXIT DISPLAY
               END DISPLAY
               EXIT INPUT
            --CALL DIALOG.setActionHidden( "consultar", 0 )
         ELSE
            CALL fn_mensaje ("Atención","No se encontraron registros para esta consulta", "stop")
            EXIT INPUT
         END IF

{
      DELETE 
        FROM cat_municipio_inegi
       WHERE municipio_desc = v_cb_entidad
}
      

      EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT

   END INPUT
   CLOSE WINDOW baja

END FUNCTION

FUNCTION fn_cb_entidad()
   LET cb = ui.ComboBox.forName("v_cb_entidad")

      LET v_qry = "SELECT entidad_federativa,
                          entidad_federativa||' '||' '||entidad_desc_larga
                     FROM cat_entidad_federativa
                    WHERE entidad_federativa BETWEEN 1 AND 32
                 ORDER BY entidad_federativa"

      PREPARE prp_combo FROM v_qry
      DECLARE cur_entidad CURSOR FOR prp_combo
      LET a = 1
      FOREACH cur_entidad INTO arr_entidad[a].entidad_federativa,arr_entidad[a].entidad_desc_larga
         CALL cb.addItem(arr_entidad[a].entidad_federativa,arr_entidad[a].entidad_desc_larga)
         LET a = a+1
      END FOREACH

      IF arr_entidad[arr_entidad.getLength()].entidad_federativa IS NULL AND
         a > 1 THEN
         CALL arr_entidad.deleteElement(arr_entidad.getLength())
      END IF

      LET v_cb_entidad = arr_entidad[1].entidad_federativa
END FUNCTION

FUNCTION fn_editar(v_pos)

   DEFINE v_cve                INTEGER  -- Variable que valida que existan registros en tabla cat_municipio_inegi
   DEFINE a                    INTEGER
   DEFINE v_qry                STRING
   DEFINE v_municipio          CHAR(60)
   DEFINE v_pos                INTEGER


   OPEN WINDOW editar WITH FORM "CATM122"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()

   LET v_cve =""
   LET v_municipio =""

   INPUT BY NAME v_municipio ATTRIBUTES(UNBUFFERED)

   BEFORE INPUT 

   LET v_cve = arr_consulta[v_pos].municipio[1,5]
   LET v_municipio = arr_consulta[v_pos].municipio[7,50]

   DISPLAY BY NAME v_cve
   DISPLAY BY NAME v_municipio

   --DISPLAY "pos de arreglo :",v_pos
   --DISPLAY "v_cve :", arr_consulta[v_pos].clave
   --DISPLAY "v_entidad :",arr_consulta[v_pos].entidad

   ON ACTION ACCEPT

   IF (v_cve IS NULL) OR
      (v_municipio IS NULL) THEN
      CALL fn_mensaje("Atención","No se puede actualizar información con datos en blanco", "stop")
   ELSE
      UPDATE cat_municipio_inegi
         SET municipio      = v_cve,
             municipio_desc = v_municipio,
             f_actualiza    = TODAY
       WHERE municipio      = v_cve
       CALL fn_mensaje("Atención","Los datos se actualizaron de forma correcta", "stop")
       EXIT INPUT
   END IF

   ON ACTION CANCEL
      EXIT INPUT
   END INPUT

   CLOSE WINDOW editar
END FUNCTION