##########################################################################
#Modulo            => CAT                                                #
#Programa          => CATM10                                             #
#Objetivo          => Mantenimiento a catálogo de usuarios rojos         #
#Autor             => Jose Eduardo Ventura                               #
#Fecha inicio      => 08 Abril 2015                                     #
##########################################################################

DATABASE safre_viv

GLOBALS

   DEFINE p_titulo                 STRING    -- Variable para título de ventana
   DEFINE p_usuario                CHAR(20)  -- Variable para recuperar nombre de usuario
   DEFINE p_tipo_ejecucion         SMALLINT  -- Forma como ejecutará el programa
   DEFINE v_estado                 CHAR(3)
   DEFINE v_nom_usuario            CHAR (50)
   DEFINE v_perfil_corta           CHAR (8)
   DEFINE v_perfil_corta_aux       CHAR (20)
   DEFINE v_edo                    INTEGER
   DEFINE v_item                   STRING
   DEFINE v_cod                    STRING

   DEFINE w ui.Window              
   DEFINE f ui.Form
   DEFINE cb                       ui.ComboBox
   
   DEFINE rec_usuario          RECORD LIKE seg_perfil.*

END GLOBALS

MAIN

   LET p_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET p_titulo           =   ARG_VAL  (3)

-- Se asigna el título de la ventana
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

-- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".CATM10.log")

--********************************************************************************
--Menú de seleccion de acciones para ALTA,BAJA o CONSULTA de usuarios rojos  *
--********************************************************************************
   MENU   "Perfil Consultas Rojas"

      ON ACTION alta
         CALL fn_alta_usuario()

      ON ACTION baja
         CALL fn_baja_usuario()

      ON ACTION consulta
         CALL fn_consulta_usuario()

      ON ACTION salir
         EXIT MENU

      ON ACTION CLOSE
         EXIT MENU
   END MENU

END MAIN

FUNCTION fn_alta_usuario()

   OPEN WINDOW usuarios WITH FORM "CATM101"
-- se inicializan variables en nulo
   LET v_nom_usuario =""
   LET v_perfil_corta =""
-- se oculta tabla de consulta
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setElementHidden("tab_consulta",1)

   LET cb = ui.ComboBox.forName("v_perfil_corta")
   DECLARE cur_alta CURSOR FOR SELECT * FROM seg_perfil
   FOREACH cur_alta INTO rec_usuario.*
      LET v_item = rec_usuario.perfil_corta CLIPPED
      LET v_cod = rec_usuario.perfil_corta
      CALL cb.addItem(v_item, v_cod)
   END FOREACH
   
   INPUT BY NAME v_perfil_corta ATTRIBUTES (WITHOUT DEFAULTS, UNBUFFERED)
   
   ON CHANGE v_perfil_corta
      LET v_nom_usuario =""
        
      IF v_perfil_corta IS NOT NULL THEN
         SELECT perfil_desc
           INTO v_nom_usuario
           FROM seg_perfil
          WHERE perfil_corta = v_perfil_corta
      END IF
       
       DISPLAY v_perfil_corta
       DISPLAY BY NAME v_nom_usuario


   ON ACTION ACCEPT

      SELECT estado_rojo
        INTO v_edo
        FROM cat_perfil_rojo
       WHERE perfil_corta = v_perfil_corta

       DISPLAY "Código de usuario : ",v_perfil_corta
       DISPLAY "estado: ",v_edo

      CASE 
         WHEN v_edo = 1 
            CALL fn_mensaje ("Archivo","El perfil ya se encuentra activo","information")
         WHEN v_edo > 1 
            UPDATE cat_perfil_rojo SET estado_rojo = 1 WHERE perfil_corta = v_perfil_corta
            CALL fn_mensaje("Archivo","El estado del perfil fue actualizado de forma correcta","information")
         WHEN (v_edo IS NULL) OR (v_edo = 0) 
            INSERT INTO cat_perfil_rojo VALUES (v_perfil_corta,"1",TODAY)
            CALL fn_mensaje("Archivo","Alta de perfil realizada de forma correcta","information")
      END CASE
      EXIT INPUT

   ON ACTION CANCEL
      EXIT INPUT

 
   END INPUT

   CLOSE WINDOW usuarios
    
   
END FUNCTION

FUNCTION fn_baja_usuario()

   DEFINE rec_perfil_rojo  RECORD LIKE cat_perfil_rojo.*
          {perfil_corta CHAR(8),
          estado_rojo SMALLINT,
          f_actualiza DATE
   END RECORD}

    OPEN WINDOW usuarios WITH FORM "CATM101"
-- se inicializan variables en nulo
   LET v_nom_usuario =""
   LET v_perfil_corta =""
-- se oculta tabla de consulta
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setElementHidden("tab_consulta",1)

   LET cb = ui.ComboBox.forName("v_perfil_corta")
    CALL cb.clear()
   DECLARE cur_baja CURSOR FOR SELECT * FROM cat_perfil_rojo WHERE estado_rojo = 1
   
   FOREACH cur_baja INTO rec_perfil_rojo.*
      LET v_item = rec_perfil_rojo.perfil_corta CLIPPED
      LET v_cod = rec_perfil_rojo.perfil_corta
      CALL cb.addItem(v_item, v_cod)
   END FOREACH
   
   INPUT BY NAME v_perfil_corta ATTRIBUTES (WITHOUT DEFAULTS, UNBUFFERED)
   
   ON CHANGE v_perfil_corta
      LET v_nom_usuario =""
      
      IF v_perfil_corta IS NOT NULL THEN
         SELECT perfil_desc
           INTO v_nom_usuario
           FROM seg_perfil
          WHERE perfil_corta = v_perfil_corta
      END IF
       
       DISPLAY v_perfil_corta
       DISPLAY BY NAME v_nom_usuario

   ON ACTION ACCEPT

      UPDATE cat_perfil_rojo SET estado_rojo = 2 WHERE perfil_corta = v_perfil_corta
      CALL fn_mensaje ("Archivo","Baja de perfil realizada de forma correcta","information")
      
      EXIT INPUT
      
   ON ACTION CANCEL
      EXIT INPUT
      
      END INPUT
      CLOSE WINDOW usuarios

END FUNCTION

FUNCTION fn_consulta_usuario()

   DEFINE i          SMALLINT 
   DEFINE arr_usuarios DYNAMIC ARRAY OF RECORD
         perfil_corta CHAR(8),
         usuario_desc CHAR(50),
         estado      CHAR(40),
         f_actualiza DATE
   END RECORD

   OPEN WINDOW usuarios WITH FORM "CATM101"

   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
   CALL f.setFieldHidden("v_perfil_corta",1)
   CALL f.setElementHidden("lb_perfil_corta",1)
   CALL f.setElementHidden("lb_nom_usuario",1)
   CALL f.setFieldHidden("v_nom_usuario",1)

   DECLARE cur_usuarios CURSOR FOR SELECT a.perfil_corta,c.perfil_desc,b.estado_rojo_desc, a.f_actualiza
                                     FROM cat_perfil_rojo a, cat_estado_rojo b,seg_perfil c
                                    WHERE a.estado_rojo = b.estado_rojo
                                      AND a.perfil_corta = c.perfil_corta

   LET i = 1
   FOREACH cur_usuarios INTO arr_usuarios[i].*
   LET i = i+1
   END FOREACH

   IF arr_usuarios[arr_usuarios.getLength()].perfil_corta IS NULL AND
         i > 1 THEN
         CALL arr_usuarios.deleteElement(arr_usuarios.getLength())
   END IF

   DISPLAY ARRAY arr_usuarios TO tab_consulta.*

   ON ACTION ACCEPT
         EXIT DISPLAY
   ON ACTION CANCEL
      EXIT DISPLAY
      END DISPLAY
   CLOSE WINDOW usuarios

END FUNCTION