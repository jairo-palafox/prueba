##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM09                                             #
#Objetivo          => Mantenimiento a catálogo "cat_estado_rojo"         #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => Noviembre 2014                                     #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE g_titulo                 STRING    -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE p_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa
   DEFINE v_estado                 CHAR(3)
   DEFINE cb                       ui.ComboBox 

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
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM09.log")

--CLOSE WINDOW SCREEN

--OPEN WINDOW opciones WITH FORM "CATM094"
--********************************************************************************
--Menú de seleccion de acciones para ALTA,BAJA o CONSULTA en cat_categoria_roja  *
--********************************************************************************
   MENU   --"MANTENIMIENTO A ESTADO ROJO"

      ON ACTION alta
         CALL fn_alta_estado()

      ON ACTION baja
         CALL fn_baja_estado()

      ON ACTION consulta
         CALL fn_consulta_estado()

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU
   END MENU

--CLOSE WINDOW opciones

END MAIN

--***********************************
--Función para alta de estado rojo  *
--***********************************
FUNCTION fn_alta_estado()

   DEFINE v_descripcion            CHAR(60)
   DEFINE v_codigo                 CHAR(3)
   DEFINE v_max_estado             SMALLINT
   DEFINE v_cta_edo                SMALLINT -- cuenta si existe categoría en tabla

   OPEN WINDOW alta WITH FORM "CATM091"

   INPUT BY NAME v_descripcion  ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

   LET v_descripcion = UPSHIFT (v_descripcion)

      IF v_descripcion IS NULL THEN
         CALL fn_mensaje("Atención","Debe agregar una descripción", "stop")
      END IF

         SELECT MAX (estado_rojo)
           INTO v_max_estado
           FROM cat_estado_rojo

           IF v_max_estado IS NULL THEN 
            LET v_max_estado = 0
         END IF

         LET v_codigo = (v_max_estado + 1)


      IF v_descripcion IS NOT NULL THEN

         SELECT COUNT (*)
           INTO v_cta_edo
           FROM cat_estado_rojo e
          WHERE e.estado_rojo_desc = v_descripcion

         IF v_cta_edo = 0 THEN

            INSERT INTO cat_estado_rojo VALUES (v_codigo,
                                                v_descripcion,
                                                TODAY,
                                                g_usuario)

            CALL fn_mensaje("Atención", "Se realizó alta de estado correctamente", "stop")

            EXIT INPUT

         ELSE
        
            CALL fn_mensaje("Atención", "Ya exíste estado con la descripción ingresada ", "stop")
            NEXT FIELD v_descripcion
         END IF
      END IF

   ON ACTION CANCEL
      EXIT INPUT
      END INPUT
      CLOSE WINDOW alta

END FUNCTION

--***********************************
--Función para baja de estado rojo  *
--***********************************
FUNCTION fn_baja_estado()

   DEFINE v_cuenta            INTEGER
   DEFINE v_estado_desc       CHAR(60)
   DEFINE rec_estado          RECORD LIKE cat_estado_rojo.*


   OPEN WINDOW baja WITH FORM "CATM092"

   LET cb = ui.ComboBox.forName("v_estado")

   DECLARE cur_baja CURSOR FOR SELECT * FROM cat_estado_rojo
   FOREACH cur_baja INTO rec_estado.*
      CALL cb.addItem(rec_estado.estado_rojo, rec_estado.estado_rojo_desc)
   END FOREACH

   INPUT BY NAME v_estado ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      IF v_estado IS NULL THEN
         CALL fn_mensaje("Atención","Debe agregar identificador de estado para proceder con la baja", "stop")

      ELSE

         SELECT COUNT (*)
           INTO v_cuenta
           FROM cat_estado_rojo e
          WHERE e.estado_rojo = v_estado

         SELECT estado_rojo_desc
           INTO v_estado_desc
           FROM cat_estado_rojo e
          WHERE e.estado_rojo = v_estado


         IF v_cuenta <> 0 THEN

            CALL fn_mensaje("Atención","Se borraran datos de estado rojo \n"||v_estado_desc,"stop")
            DELETE FROM cat_estado_rojo
                  WHERE estado_rojo = v_estado

            CALL fn_mensaje("Atención","Los datos relacionados con estado rojo fueron borrados", "stop")

        ELSE

           CALL fn_mensaje("Atención","No se encontraron datos relacionados con estado "||v_estado_desc, "stop")
         END IF

      EXIT INPUT

   END IF

   ON ACTION CANCEL
      EXIT INPUT
      END INPUT
      CLOSE WINDOW baja

END FUNCTION

--***************************************
--Función para consulta de estado rojo  *
--***************************************
FUNCTION fn_consulta_estado()

   MENU
   ON ACTION estado
      CALL fn_estado()

   ON ACTION general
      CALL fn_todo()

   ON ACTION CLOSE
      EXIT MENU

   ON ACTION CANCEL
      EXIT MENU

   END MENU
END FUNCTION

--*************************************************
--Función para consulta de estado rojo por estado *
--*************************************************
FUNCTION fn_estado()

   DEFINE v_query                  STRING
   DEFINE j                        SMALLINT

      DEFINE rec_estado RECORD LIKE cat_estado_rojo.*

      DEFINE arr_consulta DYNAMIC ARRAY OF RECORD
             estado               CHAR (3),
             estado_desc          CHAR (60),
             fecha                DATE
      END RECORD

   OPEN WINDOW consulta WITH FORM "CATM093"

     LET cb = ui.ComboBox.forName("v_estado")

   DECLARE cur_estado CURSOR FOR SELECT * FROM cat_estado_rojo
   FOREACH cur_estado INTO rec_estado.*
      CALL cb.addItem(rec_estado.estado_rojo, rec_estado.estado_rojo_desc)
   END FOREACH

      INPUT BY NAME v_estado ATTRIBUTES (UNBUFFERED)

   ON ACTION ACCEPT

      LET v_query = "SELECT estado_rojo,
                            estado_rojo_desc,
                            f_actualiza
                       FROM cat_estado_rojo
                      WHERE estado_rojo =",v_estado
    
      PREPARE prp_consulta FROM v_query
      DECLARE cur_consulta CURSOR FOR prp_consulta

      LET j=1

      FOREACH cur_consulta INTO arr_consulta[j].*
         LET j= j+1
      END FOREACH

      DISPLAY ARRAY arr_consulta TO tab_consulta.*
      END DISPLAY

      ON ACTION CLOSE
         EXIT INPUT
         END INPUT
         CLOSE WINDOW consulta

END FUNCTION

--**********************************************
--Función para consulta general de estado rojo *
--**********************************************
FUNCTION fn_todo()

   DEFINE v_query                  STRING
   DEFINE j                        SMALLINT

   DEFINE arr_consulta  DYNAMIC ARRAY OF RECORD
          estado                   CHAR(3),
          estado_desc              CHAR(20),
          f_actualiza              DATE
   END RECORD

   OPEN WINDOW consulta WITH FORM "CATM093"

   LET v_query = "SELECT estado_rojo,
                         estado_rojo_desc,
                         f_actualiza
                    FROM cat_estado_rojo"

   PREPARE prp_todo FROM v_query
   DECLARE cur_todo CURSOR FOR prp_todo

   LET j=1

   FOREACH cur_todo INTO arr_consulta[j].*
      LET j= j+1
   END FOREACH

   DISPLAY ARRAY arr_consulta TO tab_consulta.* ATTRIBUTES ( ACCEPT = FALSE )


   ON ACTION CANCEL
      EXIT DISPLAY
      END DISPLAY
      CLOSE WINDOW consulta

END FUNCTION