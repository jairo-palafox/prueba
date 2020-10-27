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
   CALL STARTLOG(g_usuario CLIPPED|| ".GRTM01.log")
   
--*********************************************************************************
--Menú de selección de acciones para ALTA,BAJA o CONSULTA en cat_municipio_inegi  *
--*********************************************************************************
   MENU --"SELECCIONE UNA ACCIÓN A REALIZAR"

      ON ACTION alta
         CALL fn_alta()

      ON ACTION baja
         --CALL fn_baja()

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

   DEFINE arr_consulta DYNAMIC ARRAY OF RECORD
          municipio CHAR(50)
   END RECORD

   OPEN WINDOW datos_generales WITH FORM "GRTM01"

      CALL fn_cb_entidad()
      DISPLAY "v_entidad",v_cb_entidad

   INPUT BY NAME v_cb_entidad ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS, ACCEPT = FALSE, CANCEL=FALSE)

   BEFORE INPUT
      DISPLAY BY NAME v_cb_entidad

   ON ACTION consultar
      IF (v_cb_entidad IS NOT NULL) THEN

      SELECT COUNT (*)
        INTO v_cuenta
        FROM cat_municipio_inegi

      IF v_cuenta = 0 THEN
         CALL fn_mensaje("Atención","No se encontraron registros para esta consulta", "stop")

      ELSE
         LET v_qry = "select municipio||' '||' '||municipio_desc
                        from cat_municipio_inegi
                       where entidad_federativa = (select entidad_federativa
                                                     from cat_entidad_federativa
                                                    where entidad_federativa = ","'",v_cb_entidad CLIPPED,"'",")"

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

         IF arr_consulta[1].municipio IS NOT NULL THEN
            DISPLAY ARRAY arr_consulta TO tab_municipios.* ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)
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

   OPEN WINDOW alta WITH FORM "GRTM01"

   LET v_cb_entidad = ""

   CALL fn_cb_entidad()
   DISPLAY BY NAME v_cb_entidad
   LET v_mun = ""
   LET v_mun_desc = ""

   INPUT BY NAME v_cb_entidad,v_mun,v_mun_desc ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS)

  -- BEFORE INPUT
 --     DISPLAY BY NAME v_cb_entidad

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

         IF v_cta_clave > 0 THEN
            LET v_bnd1 = 0
            CALL fn_mensaje ("Atención","Ya exíste un municipio con la clave ingresada", "stop")
            NEXT FIELD v_mun
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

            IF v_cta_mun > 0 THEN
               LET v_bnd2 = 0
               CALL fn_mensaje ("Atención","Ya exíste un municipio con la descripción ingresada", "stop")
               NEXT FIELD v_mun_desc
            ELSE
               LET v_bnd2 = 1
            END IF
         END IF
      END IF

      DISPLAY "bnd 1 ",v_bnd1
      DISPLAY "bnd 2 ",v_bnd2
      DISPLAY "cta mun ",v_cta_clave
      DISPLAY "cta des ",v_cta_mun
      DISPLAY ""
      DISPLAY ""

      IF (v_mun IS NOT NULL)      AND 
         (v_mun_desc IS NOT NULL) AND 
         (v_bnd1 =1)              AND 
         (v_bnd2 = 1)             THEN

         {INSERT INTO cat_municipio_inegi VALUES ( v_mun,
                                                  v_ent_fed,
                                                  v_mun_desc,
                                                  TODAY,
                                                  v_usuario)}
         CALL fn_mensaje ("Atención","Alta de municipio se realizó de forma correcta", "stop")
         EXIT INPUT
      END IF

      ON ACTION CANCEL
      EXIT INPUT
   END INPUT
   CLOSE WINDOW alta

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