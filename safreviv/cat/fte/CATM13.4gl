##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM13                                             #
#Objetivo          => Mantenimiento a catálogo "Entidades Federativas"   #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => 01/DICIEMBRE/2015                                  #
##########################################################################

DATABASE safre_viv

GLOBALS
   DEFINE g_titulo                 STRING    -- Variable para título de ventana
   DEFINE g_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE g_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa

   DEFINE arr_consulta DYNAMIC ARRAY OF RECORD
          clave   SMALLINT,
          entidad CHAR(50)
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
   CALL STARTLOG(g_usuario CLIPPED|| ".CATM13.log")
   
--***********************************************************
--Menú de selección de acciones para CONSULTA de entidades  *
--***********************************************************
   MENU --"SELECCIONE UNA ACCIÓN A REALIZAR"

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
   DEFINE a                    INTEGER
   DEFINE v_qry                STRING

   OPEN WINDOW datos_generales WITH FORM "CATM131"

   SELECT COUNT (*)
     INTO v_cuenta
     FROM cat_entidad_federativa

   IF v_cuenta = 0 THEN
      CALL fn_mensaje("Atención","No se encontraron registros para esta consulta", "stop")

   ELSE

      LET v_qry = "select entidad_federativa,entidad_desc_larga
                        from cat_entidad_federativa
                        where entidad_federativa between  1 and 32"

      PREPARE prp_consulta FROM v_qry
      DECLARE cur_consulta CURSOR FOR prp_consulta

      LET a = 1

      FOREACH cur_consulta INTO arr_consulta[a].*
         LET a = a +1
      END FOREACH

      IF arr_consulta[arr_consulta.getLength()].clave IS NULL AND
         a > 1 THEN
         CALL arr_consulta.deleteElement(arr_consulta.getLength())
      END IF

      IF arr_consulta[1].clave IS NOT NULL THEN
         DISPLAY ARRAY arr_consulta TO tab_entidad.* ATTRIBUTES (ACCEPT = FALSE, CANCEL=FALSE)
         ON ACTION salir
            EXIT DISPLAY
            END DISPLAY
      ELSE
         CALL fn_mensaje ("Atención","No se encontraron registros para esta consulta", "stop")
      END IF
   END IF
   CLOSE WINDOW datos_generales

END FUNCTION