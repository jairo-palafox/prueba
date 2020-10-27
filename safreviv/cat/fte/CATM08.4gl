##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM08                                             #
#Objetivo          => Mantenimiento a catálogo "cat_categoria_roja"      #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => Noviembre 2014                                     #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_titulo                 STRING   -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20) -- Variable para recuperar nombre de usuario
   DEFINE p_tipo_ejecucion         SMALLINT -- Forma como ejecutará el programa
   DEFINE cb                       ui.ComboBox 
   DEFINE v_categoria              CHAR(3)
END GLOBALS

MAIN

   LET g_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET g_titulo           =   ARG_VAL  (3)

-- Se asigna el título de la ventana
   IF ( g_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM08.log")

--CLOSE WINDOW SCREEN

--OPEN WINDOW opciones WITH FORM "CATM084"
--********************************************************************************
--Menú de seleccion de acciones para ALTA,BAJA o CONSULTA en cat_categoria_roja  *
--********************************************************************************
   MENU --"MANTENIMIENTO A CATEGORÍA ROJA"

      ON ACTION alta
         CALL fn_alta_categoria()

      ON ACTION baja
         CALL fn_baja_categoria()

      ON ACTION consulta
         CALL fn_consulta_categoria()

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE 
         EXIT MENU 

   END MENU

--CLOSE WINDOW opciones

END MAIN

--*******************************************************
--Función para alta de categoría en cat_categoria_roja  *
--*******************************************************
FUNCTION fn_alta_categoria()

   DEFINE v_descripcion            CHAR(60)
   DEFINE v_codigo                 CHAR(3)
   DEFINE v_max_categoria          SMALLINT
   DEFINE v_cta_cat                SMALLINT -- cuenta si existe categoría en tabla

   OPEN WINDOW alta WITH FORM "CATM081"

   INPUT BY NAME v_descripcion ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

   LET v_descripcion = UPSHIFT (v_descripcion)

      IF v_descripcion IS NULL THEN
         CALL fn_mensaje("Atención","Debe agregar una descripción", "stop")
      END IF

         SELECT MAX (categoria)
           INTO v_max_categoria
           FROM cat_categoria_roja

         IF v_max_categoria IS NULL THEN 
            LET v_max_categoria = 0
         END IF
         
         LET v_codigo = (v_max_categoria + 1)

      IF v_descripcion IS NOT NULL THEN

         SELECT COUNT (*)
           INTO v_cta_cat
           FROM cat_categoria_roja a
          WHERE a.categoria_desc = v_descripcion

         IF v_cta_cat = 0 THEN

         INSERT INTO cat_categoria_roja VALUES (v_codigo,
                                                v_descripcion,
                                                TODAY,
                                                g_usuario)

         CALL fn_mensaje("Atención", "Se realizó alta correctamente", "stop")

         EXIT INPUT

         ELSE
        
         CALL fn_mensaje("Atención", "Ya exíste categoría con la descripción ingresada ", "stop")
         NEXT FIELD v_descripcion
         END IF
      END IF

   ON ACTION CANCEL
      EXIT INPUT 
      END INPUT 
      CLOSE WINDOW alta

END FUNCTION

--*******************************************************
--Función para baja de categoría en cat_categoria_roja  *
--*******************************************************
FUNCTION fn_baja_categoria()

   DEFINE rec_categoria RECORD LIKE cat_categoria_roja.*

   DEFINE v_cuenta                 INTEGER
   DEFINE v_cat_desc               CHAR(60)

   OPEN WINDOW baja WITH FORM "CATM082"

   LET cb = ui.ComboBox.forName("v_categoria")

   DECLARE cur_categoria CURSOR FOR SELECT * FROM cat_categoria_roja
   FOREACH cur_categoria INTO rec_categoria.*
      CALL cb.addItem(rec_categoria.categoria, rec_categoria.categoria_desc)
   END FOREACH

   INPUT BY NAME v_categoria ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      IF v_categoria IS NULL THEN
         CALL fn_mensaje("Atención","Debe agregar categoría para proceder con la baja", "stop")

      ELSE

        { SELECT COUNT (*)
           INTO v_cuenta
           FROM cat_categoria_roja c
          WHERE c.categoria = v_categoria}

         SELECT categoria_desc
           INTO v_cat_desc
           FROM cat_categoria_roja c
          WHERE c.categoria = v_categoria

      --IF v_cuenta <> 0 THEN

         CALL fn_mensaje("Atención","Se borraran datos de Categoría :"||v_cat_desc,"stop")
         DELETE FROM cat_categoria_roja 
               WHERE categoria = v_categoria

         CALL fn_mensaje("Atención","Se borraron datos relacionados con Categoría:"||v_cat_desc, "stop")

      --ELSE

      --CALL fn_mensaje("Atención","No existen datos relacionados a Categoría"||v_cat_desc, "stop")
      END IF

      EXIT INPUT
   --END IF 

   ON ACTION CANCEL 
      EXIT INPUT 
      END INPUT 
      CLOSE WINDOW baja

END FUNCTION

--********************************************************
--Función para consulta de catalogo  cat_categoria_roja  *
--********************************************************
FUNCTION fn_consulta_categoria()

   MENU "CONSULTA"
      ON ACTION Categoria
         CALL fn_categoria()

      ON ACTION General
         CALL fn_todo()

      ON ACTION CLOSE 
         EXIT MENU

      ON ACTION CANCEL
         EXIT MENU

   END MENU

END FUNCTION

--***********************************************************
--Función para consulta en cat_categoria_roja por categoría *
--***********************************************************
FUNCTION fn_categoria()

   DEFINE r_categoria RECORD LIKE cat_categoria_roja.*

   DEFINE v_query                  STRING
   DEFINE j                        SMALLINT

   DEFINE arr_consulta  DYNAMIC ARRAY OF RECORD 
           categoria               CHAR(3),
           categoria_desc          CHAR(60),
           f_actualiza             DATE
   END RECORD

   OPEN WINDOW consulta WITH FORM "CATM083"

   LET cb = ui.ComboBox.forName("v_categoria")

   DECLARE c_categoria CURSOR FOR SELECT * FROM cat_categoria_roja
   FOREACH c_categoria INTO r_categoria.*
      CALL cb.addItem(r_categoria.categoria, r_categoria.categoria_desc)
   END FOREACH
      
      INPUT BY NAME v_categoria ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      LET v_query = "SELECT categoria,
                            categoria_desc,
                            f_actualiza
                       FROM cat_categoria_roja
                      WHERE categoria =", v_categoria

      PREPARE prp_consulta FROM v_query 
      DECLARE cur_consulta CURSOR FOR prp_consulta

      LET j=1

      FOREACH cur_consulta INTO arr_consulta[j].*
         LET j= j+1
      END FOREACH

      IF arr_consulta[arr_consulta.getLength()].categoria_desc IS NULL AND
         j > 1 THEN
         CALL arr_consulta.deleteElement(arr_consulta.getLength())
      END IF
      
      DISPLAY ARRAY arr_consulta TO tab_consulta.*
      --END DISPLAY

   ON ACTION CLOSE
      EXIT INPUT
      END DISPLAY
      END INPUT
      CLOSE WINDOW consulta

END FUNCTION

--***********************************************************
--Función para consulta general en cat_categoria_roja       *
--***********************************************************
FUNCTION fn_todo()

   DEFINE v_query                  STRING
   DEFINE j                        SMALLINT

   DEFINE arr_consulta  DYNAMIC ARRAY OF RECORD 
          categoria                CHAR(3),
          categoria_desc           CHAR(20),
          f_actualiza              DATE
   END RECORD

   OPEN WINDOW consulta WITH FORM "CATM083"

   LET v_query = "SELECT categoria,
                         categoria_desc,
                         f_actualiza
                    FROM cat_categoria_roja"

   PREPARE prp_todo FROM v_query 
   DECLARE cur_todo CURSOR FOR prp_todo

   LET j=1

   FOREACH cur_todo INTO arr_consulta[j].*
      LET j= j+1
   END FOREACH

   IF arr_consulta[arr_consulta.getLength()].categoria_desc IS NULL AND
         j > 1 THEN
         CALL arr_consulta.deleteElement(arr_consulta.getLength())
      END IF
   
   DISPLAY ARRAY arr_consulta TO tab_consulta.*   ATTRIBUTES (ACCEPT = FALSE)

   ON ACTION CANCEL 
      EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW consulta

END FUNCTION