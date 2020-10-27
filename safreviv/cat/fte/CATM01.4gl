{
SEGM02
Mantenimiento del catalogo de programas del sistema
}
DATABASE safre_viv
GLOBALS
DEFINE g_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       arr_cat_proceso    DYNAMIC ARRAY OF RECORD LIKE cat_proceso.*, -- arreglo de seg_modulo
       arr_cat_operacion  DYNAMIC ARRAY OF RECORD LIKE cat_operacion.* -- arreglo de seg_programa
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       p_modulo_cod     LIKE seg_modulo.modulo_cod,
       r_proceso_cod    LIKE cat_operacion.proceso_cod,
       r_opera_cod      LIKE cat_operacion.opera_cod

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET p_modulo_cod = ""
   
   -- se crear el archivo log
   CALL STARTLOG (g_usuario_cod CLIPPED|| ".CATM01.log")

   -- se abre la ventana principal del catalogo
   CALL fn_mantenimiento_cat_proceso(p_modulo_cod,g_usuario_cod)
           RETURNING r_proceso_cod,r_opera_cod
END MAIN

{FUNCTION fn_mantenimiento_cat_proceso()
   DEFINE r_cat_proceso     RECORD LIKE cat_proceso.*, -- registro de la tabla seg_modulo
          r_cat_operacion   RECORD LIKE cat_operacion.*, -- registro de seg_programa
          v_contador       SMALLINT -- contador

   -- se cargan los Procesos
   CALL fn_cargar_procesos()
          
   -- se abre la ventana que muestra el contenido del catalogo
   OPEN WINDOW w_catprocesooperacion WITH FORM "CATM012"
   DIALOG
   ATTRIBUTES (UNBUFFERED)

      -- ======================================================================================
      -- ======================================================================================
      -- ======================================================================================
      -- SE MUESTRAN LOS MODULOS REGISTRADOS
      -- ======================================================================================
      DISPLAY ARRAY arr_cat_proceso TO tbl_cat_proceso.*

         BEFORE DISPLAY
            -- se muestran los datos del primer renglon (si es que se tiene)
            IF ( arr_cat_proceso.getLength() > 0 ) THEN
               LET r_cat_proceso.* = arr_cat_proceso[1].*
               --DISPLAY BY NAME r_cat_proceso.*

               -- se cargan los programas del modulo por omision
               CALL fn_cargar_operaciones(r_cat_proceso.*)
            END IF
            
         -- al cambiar el renglon se muestran los datos del modulo en cuestion
         BEFORE ROW
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_cat_proceso.* = arr_cat_proceso[v_contador].*
            --DISPLAY BY NAME r_cat_proceso.*
            -- se cargan los programas del modulo por omision
            CALL fn_cargar_operaciones(r_cat_proceso.*)
            CALL ui.interface.refresh()

         ON ACTION Alta
            -- se invoca la funcion de alta de programa
            CALL fn_alta_cat_operacion(r_cat_proceso.*)
            -- se recargan los programas
            CALL fn_cargar_operaciones(r_cat_proceso.*)

            
      END DISPLAY

      -- ======================================================================================
      -- ======================================================================================
      -- ======================================================================================
      -- SE MUESTRAN LOS PROGRAMAS DEL MODULO QUE SE ENCUENTRA ELEGIDO
      -- ======================================================================================
      DISPLAY ARRAY arr_cat_operacion TO tbl_cat_operacion.*

         ON ACTION Alta
            -- se invoca la funcion de alta de programa
            CALL fn_alta_cat_operacion(r_cat_proceso.*)
            -- se recargan los programas
            CALL fn_cargar_operaciones(r_cat_proceso.*)

         ON ACTION baja
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_cat_operacion.* = arr_cat_operacion[v_contador].*

            -- se invoca la baja del registro
            CALL fn_baja_cat_operacion(r_cat_proceso.*, r_cat_operacion.*)
            CALL fn_cargar_operaciones(r_cat_proceso.*)

         ON ACTION modificar
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_cat_operacion.* = arr_cat_operacion[v_contador].*

            -- se invoca la baja del registro
            CALL fn_modificacion_cat_operacion(r_cat_proceso.*, r_cat_operacion.*)
            CALL fn_cargar_operaciones(r_cat_proceso.*)
      
      END DISPLAY

      ON ACTION ACCEPT
         EXIT DIALOG

      ON ACTION Cancelar
         EXIT DIALOG
      
   END DIALOG
   
   CLOSE WINDOW w_catprocesooperacion
END FUNCTION}

{FUNCTION fn_cargar_procesos()
   DEFINE v_contador    SMALLINT, -- contador de registros
          r_cat_proceso  RECORD LIKE cat_proceso.*

   -- se limpia el arreglo
   CALL arr_cat_proceso.clear()
   
   -- se leen los registros de la tabla seg_modulo
   DECLARE cur_catproceso CURSOR FOR
   SELECT *
   FROM cat_proceso
   ORDER BY modulo_cod,proceso_cod

   -- se inicia el contador
   LET v_contador = 1

   FOREACH cur_catproceso INTO r_cat_proceso.*
      -- se transfieren los datos al arreglo de despliegue
      LET arr_cat_proceso[v_contador].* = r_cat_proceso.*

      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH 
END FUNCTION}

{FUNCTION fn_cargar_operaciones(r_cat_proceso)
   DEFINE v_contador      SMALLINT, -- contador de registros
          r_cat_proceso    RECORD LIKE cat_proceso.*,
          r_cat_operacion  RECORD LIKE cat_operacion.*

   -- se limpia el arreglo
   CALL arr_cat_operacion.clear()
   
   -- se leen los registros de la tabla seg_modulo
   DECLARE cur_catoperacion CURSOR FOR
   SELECT *
   FROM cat_operacion
   WHERE proceso_cod = r_cat_proceso.proceso_cod
   ORDER BY opera_cod

   -- se inicia el contador
   LET v_contador = 1

   FOREACH cur_catoperacion INTO r_cat_operacion.*
      -- se transfieren los datos al arreglo de despliegue
      LET arr_cat_operacion[v_contador].* = r_cat_operacion.*

      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH 
END FUNCTION}

{-- OBJETIVO: crear un nuevo registro de programa asociado a un modulo
FUNCTION fn_alta_cat_operacion(r_cat_proceso)
DEFINE r_cat_proceso   RECORD LIKE cat_proceso.*,
       r_cat_operacion RECORD LIKE cat_operacion.*
           
   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_alta_cat_operacion WITH FORM "CATM011"

   -- se asignan los datos de usuario y fecha
   LET r_cat_operacion.proceso_cod    = r_cat_proceso.proceso_cod
   --LET r_cat_operacion.usuario       = g_usuario_cod
   --LET r_cat_operacion.f_actualiza   = TODAY
   -- el nombre del programa comienza siempre con las mismas letras del modulo
   LET r_cat_operacion.proceso_cod  = UPSHIFT(r_cat_proceso.proceso_cod) CLIPPED
   
   -- se capturan los datos
   INPUT BY NAME 
      r_cat_operacion.opera_cod   ,
      r_cat_operacion.opera_desc  
   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se despliega el nombre del usuario que hace la alta y la fecha
         DISPLAY BY NAME r_cat_operacion.opera_cod, r_cat_operacion.opera_desc
                         --,r_cat_operacion.usuario, r_cat_operacion.f_actualiza

      ON ACTION ACCEPT
         -- se continua si es que hay error en sql
         WHENEVER SQLERROR CONTINUE

         -- se inserta el registro en base de datos
         INSERT INTO seg_programa VALUES (r_cat_operacion.*)
         -- se indica al usuario que la insercion se realizo con exito

         -- si hubo error (duplicado)
         IF ( SQLCA.sqlcode <> 0 ) THEN
            CASE SQLCA.sqlcode
               WHEN -239 -- duplicado
                  CALL fn_mensaje("Atención", "Ya existe un registro con el mismo código de programa","stop")
                  CONTINUE INPUT
            END CASE
         END IF
         
         CALL fn_mensaje("Alta de registro","Satisfactoria la alta", "information")
         EXIT INPUT
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_alta_cat_operacion
END FUNCTION}

{-- OBJETIVO: Dar de baja un registro de la tabla seg_programa
FUNCTION fn_baja_cat_operacion(r_cat_proceso, r_cat_operacion)
   DEFINE  r_cat_proceso    RECORD LIKE cat_proceso.*,
           r_cat_operacion  RECORD LIKE cat_operacion.*

   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_baja_cat_operacion WITH FORM "CATM011"
  
   -- se capturan los datos
   DISPLAY BY NAME r_cat_operacion.opera_desc,
                   r_cat_operacion.* 

   MENU
   ATTRIBUTES ( COMMENT = "Confirma baja" )

      -- se acepta la eliminiacion
      COMMAND "Eliminar"
         MENU "Confirmar eliminación"
         ATTRIBUTES ( COMMENT = "Confirma baja", STYLE = "dialog" )
            COMMAND "Aceptar"
               DELETE FROM cat_operacion
               WHERE
                  opera_cod = r_cat_operacion.opera_cod
                  -- se indica al usuario que la insercion se realizo con exito
                  CALL fn_mensaje("Baja de registro", "Baja satisfactoria", "information")
                  EXIT MENU
            COMMAND "Cancelar"
               EXIT MENU
         END MENU   
         EXIT MENU

      -- cancela el borrado
      COMMAND "Cancelar"
         EXIT MENU
   END MENU

   CLOSE WINDOW w_baja_cat_operacion
END FUNCTION}



{FUNCTION fn_modificacion_cat_operacion(r_cat_proceso, r_cat_operacion)
   DEFINE  r_cat_proceso    RECORD LIKE cat_proceso.*,
           r_cat_operacion  RECORD LIKE cat_operacion.*
           
   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_modifica_cat_operacion WITH FORM "CATM011"

   -- se asignan los datos de usuario y fecha
   --LET r_seg_programa.usuario     = g_usuario_cod
   --LET r_seg_programa.f_actualiza = TODAY
  
   -- se capturan los datos
   INPUT BY NAME 
      r_cat_operacion.opera_desc  

   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se despliega el nombre del usuario que hace la alta y la fecha
         DISPLAY BY NAME r_cat_operacion.opera_desc,
                         r_cat_operacion.*

      ON ACTION ACCEPT
         -- se continua si es que hay error en sql
         WHENEVER SQLERROR CONTINUE

         -- se inserta el registro en base de datos
         UPDATE seg_programa
         SET
            programa_desc   = r_seg_programa.programa_desc
         WHERE
            programa_cod = r_seg_programa.programa_cod
            
         -- se indica al usuario que la actualizacion se realizo con exito
         CALL fn_mensaje("Modificación de registro", "Se modifico el registro", "information")
         EXIT INPUT
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_modifica_cat_operacion
END FUNCTION}

