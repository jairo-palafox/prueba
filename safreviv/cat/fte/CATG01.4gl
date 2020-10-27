################################################################################
# Modulo: cat                                                                  #
# Programa: CATG01                                                             #
# Descripcion: Funcion general para consulta, altas, bajas y modificación del  #
#              catalogo de procesos y operaciones                              #
# Fecha:21/02/2012                                                             #
################################################################################
DATABASE
    safre_viv
GLOBALS
DEFINE arr_cat_operacion  DYNAMIC ARRAY OF RECORD LIKE cat_operacion.*,
                          --arreglo de cat_operacion
       arr_cat_proceso    DYNAMIC ARRAY OF RECORD LIKE cat_proceso.*,
                          --arreglo de cat_proceso
       g_qry_txt          STRING --Preparar consultas
END GLOBALS
#Objetivo: Muestra los registros por default del catalogo de procesos y sus
#          operaciones
FUNCTION fn_mantenimiento_cat_proceso(p_modulo_cod,v_usuario)
DEFINE r_cat_proceso     RECORD LIKE cat_proceso.*, -- registro cat_proceso
       r_cat_operacion   RECORD LIKE cat_operacion.*, -- registro cat_operacion
       v_contador        SMALLINT, -- contador
       p_modulo_cod      LIKE seg_modulo.modulo_cod, --Modulo a filtar
       v_usuario         LIKE seg_usuario.usuario --Usuario del proceso

   -- se cargan los Procesos por default
   CALL fn_cargar_procesos(p_modulo_cod)
          
   -- se abre la ventana que muestra el contenido del catalogo
   OPEN WINDOW w_catprocesooperacion WITH FORM "CATG012"
   DIALOG
   ATTRIBUTES (UNBUFFERED)

      {======================================================================
                       SE MUESTRAN LOS PPROCESOS REGISTRADOS
       ======================================================================}
      DISPLAY ARRAY arr_cat_proceso TO tbl_cat_proceso.*

         BEFORE DISPLAY
            --Se ocultan botones para consulta de registros
            IF length(p_modulo_cod) > 0 THEN
               CALL DIALOG.setActionHidden("alta",1)
               CALL DIALOG.setActionHidden("baja",1)
               CALL DIALOG.setActionHidden("modificar",1)
               CALL DIALOG.setActionHidden("cancelar",1)
            END IF
            -- se muestran los datos del primer renglon (si es que se tiene)
            IF ( arr_cat_proceso.getLength() > 0 ) THEN
               LET r_cat_proceso.* = arr_cat_proceso[1].*
               -- se cargan los procesos y operaciones por omision
               CALL fn_cargar_operaciones(r_cat_proceso.*)
            ELSE
               -- Si no hay procesos para el modulo solicitado
               CALL fn_mensaje("Atención",
                               "No existen procesos para el módulo,
                          \n Ir a catálogo de procesos y darlo de alta","about")
               EXIT DIALOG      
               CLOSE WINDOW w_catprocesooperacion
               RETURN 0,0
            END IF
            
         -- al cambiar el renglon se muestran los datos del modulo en cuestion
         BEFORE ROW
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_cat_proceso.* = arr_cat_proceso[v_contador].*
            -- se cargan los programas del modulo por omision
            CALL fn_cargar_operaciones(r_cat_proceso.*)
            CALL ui.interface.refresh()

         ON ACTION modificar
            -- Baja de catalogo de operaciones
            CALL fn_modifica_cat_proceso(r_cat_proceso.*)
            -- Se cargan los procesos por omision
            CALL fn_cargar_procesos(p_modulo_cod)

         ON ACTION baja
            -- Baja de catalogo de procesos
            CALL fn_baja_cat_proceso(r_cat_proceso.*)
            -- se cargan los procesos por omision
            CALL fn_cargar_procesos(p_modulo_cod)

         ON ACTION alta
            -- se invoca la funcion de alta de procesos
            CALL fn_alta_proceso(v_usuario)
            -- se cargan los procesos por omision
            CALL fn_cargar_procesos(p_modulo_cod)
            -- se cargann las operaciones por omision
            CALL fn_cargar_operaciones(r_cat_proceso.*)

      END DISPLAY
      {======================================================================
          SE MUESTRAN LAS OPERACIONES DEL PROCESO QUE SE ENCUENTRA ELEGIDO
       ======================================================================}
      DISPLAY ARRAY arr_cat_operacion TO tbl_cat_operacion.*

         BEFORE DISPLAY
            --Se ocultan botones para consulta de registros
            IF length(p_modulo_cod) > 0 THEN
               CALL DIALOG.setActionHidden("alta",1)
               CALL DIALOG.setActionHidden("baja",1)
               CALL DIALOG.setActionHidden("modificar",1)
               CALL DIALOG.setActionHidden("cancelar",1)
            END IF

         ON ACTION alta
            -- se invoca la funcion de alta de operaciones
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
            CALL fn_modificacion_cat_operacion(r_cat_proceso.*,r_cat_operacion.*)
            CALL fn_cargar_operaciones(r_cat_proceso.*)
      
      END DISPLAY

      ON ACTION ACCEPT
         -- se obtiene el indice del renglon en cuestion
         LET v_contador = ARR_CURR()
         -- se muestran los datos del registro
         LET r_cat_operacion.* = arr_cat_operacion[v_contador].*
         EXIT DIALOG

      ON ACTION Cancelar
         EXIT DIALOG
      
   END DIALOG

   CLOSE WINDOW w_catprocesooperacion
   RETURN r_cat_operacion.proceso_cod,r_cat_operacion.opera_cod

END FUNCTION
#Objetivo: Consulta para cargar los procesos del catalogo
FUNCTION fn_cargar_procesos(v_modulo_cod)
   DEFINE v_contador    SMALLINT, -- contador de registros
          r_cat_proceso RECORD LIKE cat_proceso.*, --Registros de los proceso
          v_modulo_cod  LIKE seg_modulo.modulo_cod --El modulo a cosultar

   -- se limpia el arreglo
   CALL arr_cat_proceso.clear()

   LET g_qry_txt = "\n SELECT *",
                   "\n FROM cat_proceso",
                   "\n  WHERE 1 = 1"
       IF LENGTH(v_modulo_cod) > 0 THEN
          LET g_qry_txt = g_qry_txt||"\n AND modulo_cod = '",v_modulo_cod,"'"
       END IF
          LET g_qry_txt = g_qry_txt||"\n ORDER BY modulo_cod,proceso_cod"
          
    PREPARE prp_consulta_modulo_proceso FROM g_qry_txt
   
   -- se leen los registros de la tabla cat_proceso
   DECLARE cur_catproceso CURSOR FOR prp_consulta_modulo_proceso
   -- se inicia el contador
   LET v_contador = 1

   FOREACH cur_catproceso INTO r_cat_proceso.*
      -- se transfieren los datos al arreglo de despliegue
      LET arr_cat_proceso[v_contador].* = r_cat_proceso.*

      -- se incrementa el contador
      LET v_contador = v_contador + 1
   END FOREACH 
END FUNCTION
#Objetivo: Carga catalogo de operaciones
FUNCTION fn_cargar_operaciones(r_cat_proceso)
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
END FUNCTION
-- OBJETIVO: crear un nuevo registro de programa asociado a un modulo
FUNCTION fn_alta_cat_operacion(r_cat_proceso)
DEFINE r_cat_proceso   RECORD LIKE cat_proceso.*,
       r_cat_operacion RECORD LIKE cat_operacion.*,
       v_opera_cod_act LIKE cat_operacion.opera_cod,
       v_opera_cod_ant_act LIKE cat_operacion.opera_cod_ant,
       v_opera_cod_post_act LIKE cat_operacion.opera_cod_post,
       v_layout_cod         LIKE cat_operacion.layout_cod

   LET v_opera_cod_act = 0
   --Obtiene el ultimo registro del catalogo y generr el nuevo
   SELECT MAX(opera_cod),MAX(opera_cod_ant),MAX(opera_cod_post),MAX(layout_cod)
     INTO v_opera_cod_act,v_opera_cod_ant_act,v_opera_cod_post_act,v_layout_cod
   FROM cat_operacion
   WHERE proceso_cod = r_cat_proceso.proceso_cod

   IF v_opera_cod_act = 0 OR v_opera_cod_act IS null THEN
      LET r_cat_operacion.opera_cod = 1
      LET r_cat_operacion.opera_cod_ant = 0
   ELSE 
      LET r_cat_operacion.opera_cod = v_opera_cod_act + 1
      LET r_cat_operacion.opera_cod_ant = v_opera_cod_act
   END IF
   LET r_cat_operacion.layout_cod = v_layout_cod
   LET r_cat_operacion.opera_cod_post = 0
   LET v_opera_cod_post_act = 0

   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_alta_cat_operacion WITH FORM "CATG011"

   -- se capturan los datos
   INPUT BY NAME 
      r_cat_operacion.opera_desc,r_cat_operacion.programa_cod,
      r_cat_operacion.layout_cod,r_cat_operacion.ejecuta_cod,
      r_cat_operacion.extension
   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

      BEFORE INPUT
         -- se despliegan los datos que no es necesario que el usuario capture
         -- valores por omisión
         DISPLAY BY NAME
                 r_cat_proceso.proceso_cod, r_cat_proceso.proceso_desc,
                 r_cat_operacion.opera_cod, r_cat_operacion.layout_cod,
                 r_cat_operacion.opera_cod_ant,r_cat_operacion.opera_cod_post
         DISPLAY v_opera_cod_act TO opera_cod_act
         DISPLAY v_opera_cod_ant_act TO opera_cod_ant_act
         DISPLAY v_opera_cod_post_act TO opera_cod_post_act

      ON ACTION buscar
         --Lostado de los programas por modulo
         CALL fn_consulta_programa(r_cat_proceso.modulo_cod)
                         RETURNING r_cat_operacion.programa_cod

      ON ACTION ACCEPT
         -- se continua si es que hay error en sql
         WHENEVER SQLERROR CONTINUE

         -- se inserta el registro en base de datos
         LET r_cat_operacion.proceso_cod = r_cat_proceso.proceso_cod
         INSERT INTO cat_operacion VALUES (r_cat_operacion.*)
         -- se actualizan operaciones anteriores
         UPDATE cat_operacion
            SET opera_cod_post = r_cat_operacion.opera_cod
         WHERE proceso_cod = r_cat_operacion.proceso_cod
           AND opera_cod   = r_cat_operacion.opera_cod - 1

         -- si hubo error (duplicado)
         IF ( SQLCA.sqlcode <> 0 ) THEN
            CASE SQLCA.sqlcode
               WHEN -239 -- duplicado
                  CALL fn_mensaje("Atención",
                                  "Ya existe registro con el misma operación",
                                  "stop")
                  CONTINUE INPUT
            END CASE
         END IF
         
         CALL fn_mensaje("Registros de Operación",
                         "Alta de registro satisfactorio","information")
         EXIT INPUT
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_alta_cat_operacion
END FUNCTION
# OBJETIVO: Dar de baja un registro de la tabla seg_programa
FUNCTION fn_baja_cat_operacion(r_cat_proceso, r_cat_operacion)
   DEFINE  r_cat_proceso    RECORD LIKE cat_proceso.*,
           r_cat_operacion  RECORD LIKE cat_operacion.*,
           v_opera_cod_act  LIKE cat_operacion.opera_cod,
           v_opera_cod_ant  LIKE cat_operacion.opera_cod_ant,
           v_row_id         INTEGER

   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_baja_cat_operacion WITH FORM "CATG011"
  
   -- se muestra los datos
   DISPLAY BY NAME r_cat_operacion.proceso_cod,r_cat_proceso.proceso_desc,
                   r_cat_operacion.opera_cod,r_cat_operacion.opera_desc,
                   r_cat_operacion.programa_cod,r_cat_operacion.layout_cod,
                   r_cat_operacion.ejecuta_cod,r_cat_operacion.extension,
                   r_cat_operacion.opera_cod_ant,r_cat_operacion.opera_cod_post
   DISPLAY r_cat_operacion.opera_cod TO opera_cod_act
   DISPLAY r_cat_operacion.opera_cod_ant TO opera_cod_ant_act
   DISPLAY r_cat_operacion.opera_cod_post TO opera_cod_post_act

   MENU
   ATTRIBUTES ( COMMENT = "Confirma baja" )

      -- se acepta la eliminiacion
      COMMAND "Eliminar"
         MENU "Confirmar eliminación"
         ATTRIBUTES ( COMMENT = "Confirmación de baja", STYLE = "dialog" )
            COMMAND "Aceptar"
               DELETE FROM cat_operacion
               WHERE proceso_cod = r_cat_operacion.proceso_cod
                 AND opera_cod = r_cat_operacion.opera_cod

               LET g_qry_txt = "\n SELECT ROWID",
                       "\n FROM cat_operacion",
                       "\n WHERE proceso_cod = ",r_cat_operacion.proceso_cod,"",
                       "\n ORDER BY opera_cod"

               PREPARE prp_consulta_actualiza_operaciones FROM g_qry_txt
               LET v_opera_cod_act = 1
               LET v_opera_cod_ant = 0
               DECLARE cur_actualiza_operaciones CURSOR FOR
                                              prp_consulta_actualiza_operaciones
               FOREACH cur_actualiza_operaciones INTO v_row_id
                   -- se actualiza opera_cod, opera_cod_ant y opera_cod_post del
                   -- la operacion aactual
                   LET g_qry_txt = "\n UPDATE cat_operacion",
                       "\n    SET opera_cod = ",v_opera_cod_act,",",
                       "\n        opera_cod_ant = ",v_opera_cod_ant,",",
                       "\n        opera_cod_post = 0",
                       "\n WHERE proceso_cod = ",r_cat_operacion.proceso_cod,"",
                       "\n   AND ROWID = ",v_row_id
                   PREPARE prp_update_ant_post FROM  g_qry_txt
                   EXECUTE prp_update_ant_post

                   -- se actualiza opera_cod_pos del la operacion anterior
                   UPDATE cat_operacion
                      SET opera_cod_post = v_opera_cod_act
                   WHERE proceso_cod = r_cat_operacion.proceso_cod
                     AND opera_cod   = v_opera_cod_act - 1

                   LET v_opera_cod_act = v_opera_cod_act + 1
                   LET v_opera_cod_ant = v_opera_cod_ant + 1

               END FOREACH

               CALL fn_mensaje("Registros de Operación",
                                  "Baja de registro satisfactorio",
                                  "information")
                  EXIT MENU
            COMMAND "Cancelar"
               EXIT MENU
         END MENU   
         EXIT MENU

      COMMAND "Cancelar"
         EXIT MENU
   END MENU

   CLOSE WINDOW w_baja_cat_operacion
END FUNCTION
#Objetivo: Actualizacion de las operaciones
FUNCTION fn_modificacion_cat_operacion(r_cat_proceso, r_cat_operacion)
   DEFINE  r_cat_proceso    RECORD LIKE cat_proceso.*,
           r_cat_operacion  RECORD LIKE cat_operacion.*
           
   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_modifica_cat_operacion WITH FORM "CATG011"

   -- se capturan los datos
   INPUT BY NAME 
      r_cat_operacion.opera_desc,r_cat_operacion.programa_cod,
      r_cat_operacion.layout_cod,r_cat_operacion.ejecuta_cod,
      r_cat_operacion.extension

   WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
         -- Se despliegan los datos de la operación
         DISPLAY BY NAME r_cat_operacion.proceso_cod,r_cat_proceso.proceso_desc,
                   r_cat_operacion.opera_cod,r_cat_operacion.opera_desc,
                   r_cat_operacion.programa_cod,r_cat_operacion.layout_cod,
                   r_cat_operacion.ejecuta_cod,r_cat_operacion.extension,
                   r_cat_operacion.opera_cod_ant,r_cat_operacion.opera_cod_post
         DISPLAY r_cat_operacion.opera_cod TO opera_cod_act
         DISPLAY r_cat_operacion.opera_cod_ant TO opera_cod_ant_act
         DISPLAY r_cat_operacion.opera_cod_post TO opera_cod_post_act

      ON ACTION buscar
         CALL fn_consulta_programa(r_cat_proceso.modulo_cod)
                         RETURNING r_cat_operacion.programa_cod

      ON ACTION ACCEPT
         -- se continua si es que hay error en sql
         WHENEVER SQLERROR CONTINUE

         -- se inserta el registro en base de datos
         LET g_qry_txt = "\n UPDATE cat_operacion",
                "\n    SET opera_desc     = '",r_cat_operacion.opera_desc,"',",
               "\n        programa_cod   = '",r_cat_operacion.programa_cod,"',",
                "\n        layout_cod     = '",r_cat_operacion.layout_cod,"',",
                "\n        ejecuta_cod    = '",r_cat_operacion.ejecuta_cod,"',",
                "\n        extension      = '",r_cat_operacion.extension,"',",
              "\n        opera_cod_ant  = '",r_cat_operacion.opera_cod_ant,"',",
              "\n        opera_cod_post = '",r_cat_operacion.opera_cod_post,"'",
                "\n WHERE proceso_cod     = '",r_cat_operacion.proceso_cod,"'",
                "\n   and opera_cod       = '",r_cat_operacion.opera_cod,"'"
         PREPARE prp_actualiza_operacion FROM g_qry_txt
         EXECUTE prp_actualiza_operacion
            
         -- se indica al usuario que la actualizacion se realizo con exito
         CALL fn_mensaje("Registros de Operación",
                         "Modificación de registro satisfactorio","information")
         EXIT INPUT
         
      ON ACTION CANCEL
         EXIT INPUT
   
   END INPUT
   
   CLOSE WINDOW w_modifica_cat_operacion
END FUNCTION
#Objetivo: Alta de nuevo proceso
FUNCTION fn_alta_proceso(v_usuario)
DEFINE p_modulo_cod  LIKE seg_modulo.modulo_cod,
       r_cat_proceso RECORD LIKE cat_proceso.*,
       v_usuario     LIKE seg_usuario.usuario
DEFINE f_ventana  ui.Window,   -- Define las propìedades de la Ventana
       f_forma    ui.Form     -- Define las propiedades de la forma

   SELECT MAX(proceso_cod)
     INTO r_cat_proceso.proceso_cod
   FROM cat_proceso
   LET r_cat_proceso.proceso_cod = r_cat_proceso.proceso_cod + 1

   OPEN WINDOW vtn_alta_proceso WITH FORM "CATG013"
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()
      CALL f_forma.setElementHidden("gr_modulo_desc",1) 
      INPUT BY NAME p_modulo_cod,r_cat_proceso.proceso_desc{,
                                 r_cat_proceso.proceso_ext}
         WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)

         BEFORE INPUT
            CALL fn_cmb_modulo() RETURNING p_modulo_cod 
            LET r_cat_proceso.f_actualiza = TODAY
            LET r_cat_proceso.usuario = v_usuario
            DISPLAY r_cat_proceso.proceso_cod TO proceso_cod
            DISPLAY r_cat_proceso.f_actualiza TO f_actualiza
            DISPLAY r_cat_proceso.usuario TO usuario

         AFTER FIELD p_modulo_cod
            DISPLAY p_modulo_cod TO proceso_ext

         ON ACTION ACCEPT
            LET r_cat_proceso.modulo_cod = p_modulo_cod
            LET r_cat_proceso.proceso_ext = p_modulo_cod
            --DISPLAY p_modulo_cod TO proceso_desc
            WHENEVER SQLERROR CONTINUE
               INSERT INTO cat_proceso VALUES (r_cat_proceso.*)

               DISPLAY "Codigo de error ",SQLCA.sqlcode
               -- si hubo error (duplicado)
               IF ( SQLCA.sqlcode < 0 ) THEN
                  CASE SQLCA.sqlcode
                     WHEN -239 -- duplicado
                        CALL fn_mensaje("Atención",
                                      "Ya existe registro con el mismo proceso",
                                      "stop")
                        CONTINUE INPUT
                  END CASE
               END IF
         
               CALL fn_mensaje("Registros de Operación",
                               "Alta de registro satisfactorio","information")
               EXIT INPUT

         ON ACTION CANCEL
            EXIT INPUT
            END INPUT
   
   CLOSE WINDOW vtn_alta_proceso

END FUNCTION
#Objetivo: Llena combo para modulo_cod y modulo_desc de seg_modulo
FUNCTION fn_cmb_modulo()
DEFINE p_modulo_cod LIKE seg_modulo.modulo_cod,
       v_modulo_cod LIKE seg_modulo.modulo_cod,
       v_modulo_des LIKE seg_modulo.modulo_desc,
       cb           ui.ComboBox -- Variable de Combobox

   LET cb = ui.ComboBox.forName("p_modulo_cod") --Asigna combo a la forma
   -- Validación si el combo es nulo 
   IF cb IS NULL THEN
      CALL fn_mensaje("","Form field not found in current form","about")
      EXIT PROGRAM
   END IF

   -- Limpia el combo
   CALL cb.clear()

   LET g_qry_txt = "\n SELECT modulo_cod,modulo_desc",
                   "\n FROM seg_modulo",
                   "\n ORDER BY modulo_cod"
      PREPARE prp_consulta_modulo FROM g_qry_txt

   DECLARE cur_llena_combo_modulos CURSOR FOR prp_consulta_modulo
      FOREACH cur_llena_combo_modulos INTO v_modulo_cod,v_modulo_des
         CALL cb.addItem(v_modulo_cod,v_modulo_des)
      END FOREACH

   RETURN p_modulo_cod
END FUNCTION
# OBJETIVO: Dar de baja un registro de la tabla cat_proceso
FUNCTION fn_baja_cat_proceso(r_cat_proceso)
DEFINE r_cat_proceso  RECORD LIKE cat_proceso.*,
       v_modulo_desc  LIKE seg_modulo.modulo_desc,
       v_existe_operacion SMALLINT
DEFINE f_ventana  ui.Window,   -- Define las propìedades de la Ventana
       f_forma    ui.Form     -- Define las propiedades de la forma

   -- se abre la ventana para dar de alta el registo
   OPEN WINDOW w_baja_cat_proceso WITH FORM "CATG013"

   LET f_ventana = ui.Window.getCurrent()
   LET f_forma = f_ventana.getForm()
   CALL f_forma.setElementHidden("gr_modulo_cod",1)
   
      SELECT modulo_desc INTO v_modulo_desc
      FROM seg_modulo
      WHERE modulo_cod = r_cat_proceso.modulo_cod

       -- se muestra los datos
      DISPLAY v_modulo_desc TO f_modulo_desc
      DISPLAY r_cat_proceso.proceso_cod TO proceso_cod
      DISPLAY r_cat_proceso.proceso_desc TO proceso_desc
      DISPLAY r_cat_proceso.proceso_ext TO proceso_ext
      DISPLAY r_cat_proceso.f_actualiza TO f_actualiza
      DISPLAY r_cat_proceso.usuario TO usuario

   MENU
   ATTRIBUTES ( COMMENT = "Confirma baja" )

      -- se acepta la eliminiacion
      COMMAND "Eliminar"
         MENU "Confirmar eliminación"
         ATTRIBUTES ( COMMENT = "Confirma baja", STYLE = "dialog" )
            COMMAND "Aceptar"

               SELECT COUNT (*)
                 INTO v_existe_operacion
               FROM cat_operacion
               WHERE proceso_cod = r_cat_proceso.proceso_cod

               IF v_existe_operacion > 0 THEN
                  CALL fn_mensaje("Catálogo de proceso",
                                  "Existen operaciones asignada al proceso\n
                                   no se puede eliminar el registro",
                                  "about")
                  EXIT MENU
               ELSE
                  DELETE FROM cat_proceso
                  WHERE modulo_cod = r_cat_proceso.modulo_cod
                    AND proceso_cod = r_cat_proceso.proceso_cod
                  CALL fn_mensaje("Registros de Operación",
                               "Baja de registro satisfactorio",
                               "information")
                  EXIT MENU
               END if
            COMMAND "Cancelar"
               EXIT MENU
         END MENU   
         EXIT MENU

      -- cancela el borrado
      COMMAND "Cancelar"
         EXIT MENU
   END MENU

   CLOSE WINDOW w_baja_cat_proceso
END FUNCTION
#Objetivo: Modificaciones al catalogo de procesos, campo descripción
FUNCTION fn_modifica_cat_proceso(r_cat_proceso)
DEFINE r_cat_proceso  RECORD LIKE cat_proceso.*,
       v_modulo_desc  LIKE seg_modulo.modulo_desc
DEFINE f_ventana  ui.Window,   -- Define las propìedades de la Ventana
       f_forma    ui.Form     -- Define las propiedades de la forma

   OPEN WINDOW vtn_modifica_desc_proceso WITH FORM "CATG013"
      LET f_ventana = ui.Window.getCurrent()
      LET f_forma = f_ventana.getForm()
      CALL f_forma.setElementHidden("gr_modulo_cod",1)

      INPUT BY NAME r_cat_proceso.proceso_desc
                    WITHOUT DEFAULTS ATTRIBUTES (UNBUFFERED)
         BEFORE INPUT
            SELECT modulo_desc INTO v_modulo_desc
            FROM seg_modulo
            WHERE modulo_cod = r_cat_proceso.modulo_cod

            --despliega los datos del proceso seleccionado
            DISPLAY v_modulo_desc TO f_modulo_desc
            DISPLAY r_cat_proceso.proceso_cod TO proceso_cod
            DISPLAY r_cat_proceso.proceso_desc TO proceso_desc
            DISPLAY r_cat_proceso.proceso_ext TO proceso_ext
            DISPLAY r_cat_proceso.f_actualiza TO f_actualiza
            DISPLAY r_cat_proceso.usuario TO usuario

         ON ACTION ACCEPT
            -- Continua si es que hay error en sql
            WHENEVER SQLERROR CONTINUE

            -- se inserta el registro en base de datos
            LET g_qry_txt = "\n UPDATE cat_proceso",
                    "\n    SET proceso_desc = '",r_cat_proceso.proceso_desc,"'",
                    "\n WHERE proceso_cod   = '",r_cat_proceso.proceso_cod,"'",
                    "\n   AND modulo_cod    = '",r_cat_proceso.modulo_cod,"'"

            PREPARE prp_actualiza_proceso FROM g_qry_txt
            EXECUTE prp_actualiza_proceso
            
            -- se indica al usuario que la actualizacion se realizo con exito
            CALL fn_mensaje("Registros de Procesos",
                         "Modificación de registro satisfactorio","information")
            EXIT INPUT
         
         ON ACTION CANCEL
            EXIT INPUT
   
      END INPUT
          
   CLOSE WINDOW vtn_modifica_desc_proceso

END FUNCTION
#objetivo: consulta codigos de programa sobre seg_programa
FUNCTION fn_consulta_programa(p_modulo_cod)
   DEFINE p_modulo_cod   LIKE seg_programa.modulo_cod
   DEFINE marr_programa  DYNAMIC ARRAY OF RECORD
             programa_cod  LIKE seg_programa.programa_cod ,
             programa_desc LIKE seg_programa.programa_desc
          END RECORD
   DEFINE v_cont         INTEGER
   DEFINE v_pos_sel      INTEGER
   DEFINE v_sql          STRING
   
   IF LENGTH(p_modulo_cod CLIPPED) = 0 THEN
      CALL fn_mensaje("Advertencia","EL MODULO ES REQUERIDO PARA ESTA CONSULTA,
                                  \n VERIFICARLO CON EL ADMINISTRADOR",
                                    "exclamation")
      RETURN NULL
   END IF
   
   LET v_sql = "SELECT programa_cod, programa_desc",
               "  FROM seg_programa",
               " WHERE modulo_cod = '",p_modulo_cod,"'",
               " ORDER BY 1"
   
   PREPARE EnuConProg FROM v_sql
   DECLARE CurConProg CURSOR FOR EnuConProg
   
   LET v_cont    = 1   
   LET v_pos_sel = 0
   FOREACH CurConProg INTO marr_programa[v_cont].*
      LET v_cont = v_cont + 1
   END FOREACH

   IF(marr_programa[marr_programa.getLength()].programa_cod IS NULL OR 
      marr_programa[marr_programa.getLength()].programa_desc IS NULL)THEN
      CALL marr_programa.deleteElement(marr_programa.getLength())
   END IF
   
   IF v_cont > 1 THEN
      
      OPEN WINDOW vtn_con_programa WITH FORM "CATG014"
      
      DISPLAY ARRAY marr_programa TO tbl_programa.*
         ATTRIBUTES (UNBUFFERED)
         
         ON ACTION accept
            LET v_pos_sel = ARR_CURR()
            EXIT DISPLAY
         ON ACTION cancel
            EXIT DISPLAY
      END DISPLAY
      
      CLOSE WINDOW vtn_con_programa
      
   END IF
   
   IF v_pos_sel > 0 THEN
      RETURN marr_programa[v_pos_sel].programa_cod
   ELSE
      RETURN NULL
   END IF
   
END FUNCTION

