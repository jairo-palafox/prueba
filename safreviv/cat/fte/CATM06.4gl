####################################################################
#Modulo            =>CAT                                           #
#Programa          =>CATM066                                       #
#Objetivo          =>Administrador de catálogo de afores           #
#Autor             =>Eneas Adan Armas Osorio, EFP                  #
#Fecha inicio      =>12 Noviembre 2013                             #
####################################################################
DATABASE safre_viv

GLOBALS
   DEFINE g_usuario_sys   LIKE cat_proceso.usuario,-- Usuario de acceso al modulo
      g_tipo_carga        SMALLINT,                -- Tipo de carga -- No funcional
      g_nom_prog          VARCHAR(30)              -- Nombre del programa -- No funcional
   DEFINE arbol           DYNAMIC ARRAY OF RECORD
      name                STRING,
      pid                 STRING,
      id                  STRING,
      hasChildren         BOOLEAN,
      marca               SMALLINT,
      expandido           BOOLEAN,
      cod                 STRING,
      des                 STRING
   END RECORD
END GLOBALS

MAIN
   DEFINE id     INTEGER
   ,v_ban_cambio SMALLINT
   ,v_afore_cod  LIKE cat_afore.afore_cod
   ,v_afore_desc LIKE cat_afore.afore_desc

   -- Parametros de entrada al catalogo
   LET g_usuario_sys = ARG_VAL(1)
   LET g_tipo_carga = ARG_VAL(2)
   LET g_nom_prog = ARG_VAL(3)

    CLOSE WINDOW SCREEN 
   --abre ventana con el listado principal de las afores
   OPEN WINDOW CATM061 WITH FORM "CATM061" 

   DISPLAY ARRAY arbol TO sr_arbol.* ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      BEFORE DISPLAY
         --llenado inicial del arbol o arreglo
         CALL fn_expande(DIALOG,0)
         CALL DIALOG.setSelectionMode("sr_arbol",1)
      ON EXPAND(id)
         CALL fn_expande(DIALOG,id)
      ON COLLAPSE(id)
         CALL fn_colapsa(DIALOG,id)
      ON ACTION Nuevo
         CALL fn_nuevo(arbol[arr_curr()].id,arbol[arr_curr()].pid)
            RETURNING v_ban_cambio,v_afore_cod,v_afore_desc
         IF v_ban_cambio <> 0 THEN
            CALL fn_agr_renglon(DIALOG,0,v_afore_cod,v_afore_desc)
            --IF v_nivel == 0 THEN 
               --IF arbol[arr_curr()].expandido THEN
                  --CALL fn_agr_renglon(DIALOG,arr_curr(),v_afore_cod,v_afore_desc)
               --ELSE
                  --LET arbol[arr_curr()].hasChildren = TRUE
                  --LET arbol[arr_curr()].expandido = TRUE
                  --CALL fn_expande(DIALOG,arr_curr())
               --END IF
            --ELSE
            --END IF
         END IF
      ON ACTION Modificar
         --marca en 2, baja logica, no se deben realizar cambios
         IF arbol[arr_curr()].marca = 2 THEN
            CALL fn_mensaje("Error", "No se pueden realizar cambios a afore en baja.", "information")
            CONTINUE DISPLAY
         END IF
         CALL fn_modificacion(arbol[arr_curr()].id, arbol[arr_curr()].pid, arbol[arr_curr()].des)
            RETURNING v_ban_cambio,v_afore_cod,v_afore_desc
         IF v_ban_cambio <> 0 THEN
            --LET arbol[arr_curr()].id = v_afore_cod
            --LET arbol[arr_curr()].cod = v_afore_cod
            LET arbol[arr_curr()].name = v_afore_cod, " - ", v_afore_desc CLIPPED
            LET arbol[arr_curr()].des  = v_afore_desc CLIPPED

            CALL arbol.clear()
            CALL fn_expande(DIALOG,0)
         END IF
      ON ACTION Baja
         --marca en 2, baja logica, no se deben realizar cambios
         IF arbol[arr_curr()].marca = 2 THEN
            CALL fn_mensaje("Error", "No se pueden realizar cambios a afore en baja.", "information")
            CONTINUE DISPLAY
         END IF
         CALL fn_baja(arbol[arr_curr()].id, arbol[arr_curr()].pid, arbol[arr_curr()].des)
            RETURNING v_ban_cambio
         IF v_ban_cambio <> 0 THEN
            --CALL fn_eli_renglon(DIALOG,arr_curr())
            --CALL DIALOG.deleteNode("sr_arbol", arr_curr())
            LET arbol[arr_curr()].marca  = 2
         END IF
      ON ACTION Cancelar
         EXIT DISPLAY 
   END DISPLAY
   CLOSE WINDOW CATM061
END MAIN

#elimina el renglon (cuando se elimino bien el registro)
FUNCTION fn_eli_renglon(p_dialogo,p_indice)
DEFINE p_dialogo ui.Dialog
DEFINE p_indice  INT
   CALL p_dialogo.deleteNode("sr_arbol", p_indice)
END FUNCTION

#agrega los datos del renglon (cuando se guardaron bien)
FUNCTION fn_agr_renglon(p_dialogo,p_indice,p_afore_cod,p_afore_desc)
DEFINE p_dialogo ui.Dialog
   ,p_indice        INT
   ,x               INT
   ,p_afore_cod     STRING
   ,p_afore_desc    LIKE cat_afore.afore_desc

   LET x = p_dialogo.appendNode("sr_arbol", p_indice)
   LET arbol[x].id    = p_afore_cod
   LET arbol[x].name  = p_afore_cod," - " ,p_afore_desc CLIPPED
   LET arbol[x].cod   = p_afore_cod
   LET arbol[x].des   = p_afore_desc
   LET arbol[x].marca = 0
   LET arbol[x].hasChildren = FALSE
END FUNCTION

#colapsa el renglon seleccionado
FUNCTION fn_colapsa(p_dialogo,p_indice)
DEFINE p_dialogo ui.Dialog
DEFINE p_indice  INT
   WHILE p_indice < arbol.getLength()
      IF arbol[p_indice + 1].pid IS NULL THEN EXIT WHILE END IF
      IF arbol[p_indice + 1].pid <> arbol[p_indice].id THEN EXIT WHILE END IF
      CALL p_dialogo.deleteNode("sr_arbol", p_indice + 1)
   END WHILE
END FUNCTION

#expande el renglon seleccionado
FUNCTION fn_expande(p_dialogo,p_indice)
DEFINE p_dialogo ui.Dialog
DEFINE p_indice     INT
   ,x               INT
   ,v_sql           STRING
   ,v_sql2          STRING
   ,v_afore         RECORD 
      afore_cod     LIKE cat_afore.afore_cod,
      afore_desc    LIKE cat_afore.afore_desc,
      marca         LIKE cat_afore.marca
   END RECORD
   ,afore_codH      LIKE cat_afore.afore_cod

   --condicion para consulta de llenado inicial si no, agregar hijos
   IF p_indice = 0 THEN
      LET v_sql = "\n WHERE afore_fusion is null "
      LET x=0
   ELSE
      LET v_sql = "\n WHERE afore_fusion = ",arbol[p_indice].id
   END IF
   
   --consulta para llenar listado principal
   LET v_sql = "\n SELECT afore_cod,afore_desc,marca FROM cat_afore",
               v_sql,
               "\n     AND NOT afore_cod IN (0,590)",
               "\n ORDER BY marca,afore_cod"
   --consulta para ver si tiene dependencias el afore
   LET v_sql2 = "\n select first 1 afore_cod from cat_afore",
                "\n where afore_fusion = ?"
   PREPARE sid_aforeC FROM v_sql
   PREPARE sid_aforeH2 FROM v_sql2
   --se llena el listado con las afores   
   DECLARE cur_aforeC CURSOR FOR sid_aforeC
   FOREACH cur_aforeC INTO v_afore.*
      --llenado inicial si no en modo de agregar
      IF p_indice = 0 THEN
         LET x = x + 1
      ELSE 
         LET x = p_dialogo.appendNode("sr_arbol", p_indice)
      END IF
      --llana datos del afore al arreglo
      LET arbol[x].id = v_afore.afore_cod
      LET arbol[x].name = v_afore.afore_cod || " - " || v_afore.afore_desc CLIPPED
      LET arbol[x].cod = v_afore.afore_cod
      LET arbol[x].des = v_afore.afore_desc CLIPPED
      LET arbol[x].marca = v_afore.marca
      LET afore_codH = NULL

      --por afore se ve si tiene dependientes
      EXECUTE sid_aforeH2 USING v_afore.afore_cod INTO afore_codH
      IF afore_codH IS NULL THEN
         LET arbol[x].hasChildren = FALSE
      ELSE
         LET arbol[x].hasChildren = TRUE
      END if
   END FOREACH
END FUNCTION

#alta de datos de una afore
FUNCTION fn_nuevo(p_id,p_pid)
DEFINE p_id      INTEGER
   ,p_pid        INTEGER
   ,fusion       LIKE cat_afore.afore_fusion
   ,cod          LIKE cat_afore.afore_cod
   ,descr        LIKE cat_afore.afore_desc
   ,v_consulta   STRING
   ,v_hoy        LIKE cat_afore.f_actualiza
   ,v_salida_cod SMALLINT
   ,v_ventana    ui.Window
   ,v_forma      ui.Form
   
   -- se abre ventana para captura de datos
   OPEN WINDOW CATM062 WITH FORM "CATM062"
   INPUT BY NAME cod,descr,fusion
   ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS, ACCEPT = FALSE, CANCEL = FALSE)
   BEFORE INPUT
      LET fusion       = p_id
      LET v_ventana    = ui.Window.getCurrent()
      LET v_forma      = v_ventana.getForm()
      CALL v_forma.setFieldHidden("fusion",1)
      CALL v_forma.setElementHidden("label1",1)
   --ON CHANGE nivel
      --IF nivel == 0 THEN
         --LET fusion = p_id
      --ELSE
         --LET fusion = p_pid
      --END IF
   ON ACTION guardar
      --validaciones para valores nuevos
      IF descr IS NULL THEN
         CALL fn_mensaje("Error", "La descripción es obligatoria.", "information")
         NEXT FIELD descr
      END IF 
      IF cod IS NULL THEN
         CALL fn_mensaje("Error", "El código es obligatorio.", "information")
         NEXT FIELD cod
      END IF 
      IF fn_no_val_afore_cod(cod) THEN
         CALL fn_mensaje("Error", "El código ya existe.", "information")
         NEXT FIELD cod
      END IF 

      --si pasa las validaciones se realiza proceso
      LET v_hoy =  TODAY
      LET fusion = NULL
      LET v_consulta = "insert into cat_afore(afore_cod,afore_desc,marca, afore_fusion,f_actualiza,usuario)",
                             "\n values (?,?,0,?,?,?)"
      PREPARE alta FROM v_consulta
      EXECUTE alta USING cod,descr,fusion,v_hoy,g_usuario_sys
      IF sqlca.sqlcode == 0 THEN
         IF sqlca.sqlerrd[3] > 0 THEN
            CALL fn_mensaje("Alta","El alta fue correcta.","information")
            LET v_salida_cod = 1
            EXIT INPUT
         ELSE
            CALL fn_mensaje("Alta","No se registraron los datos.","stop")
         END IF
      ELSE
         CALL fn_mensaje("Alta","Error al registrar la información.","stop")
      END IF
   ON ACTION Cancelar
      LET v_salida_cod =  0
      EXIT INPUT
   END INPUT
   CLOSE WINDOW CATM062
   RETURN v_salida_cod,cod,descr
END FUNCTION

#registra la modificacion de informacion de una afore
FUNCTION fn_modificacion(p_id,p_pid,p_descr)
DEFINE p_id      INTEGER
   ,p_pid        INTEGER
   ,p_descr      LIKE cat_afore.afore_desc
   ,fusion       LIKE cat_afore.afore_fusion
   ,cod          LIKE cat_afore.afore_cod
   ,descr        LIKE cat_afore.afore_desc
   ,v_hoy        LIKE cat_afore.f_actualiza
   ,v_descr_ant  LIKE cat_afore.afore_desc
   ,v_consulta   STRING
   ,v_salida_cod SMALLINT
   ,v_cod_ant    INTEGER
   ,v_ventana    ui.Window
   ,v_forma      ui.Form
   ,v_cbx        ui.ComboBox

   LET v_salida_cod =  0
  
   --se abre la ventana para la modificacion de afore
   OPEN WINDOW CATM062 WITH FORM "CATM062"
   INPUT BY NAME descr,fusion
   ATTRIBUTES (UNBUFFERED,WITHOUT DEFAULTS, ACCEPT = FALSE, CANCEL = FALSE)
   BEFORE INPUT
      --inicialización de variables
      LET v_ventana   = ui.Window.getCurrent()
      LET v_forma     = v_ventana.getForm()
      CALL v_forma.setFieldStyle("","")
      LET cod         = p_id
      LET fusion      = p_pid
      LET descr       = p_descr
      LET v_cod_ant   = p_id
      LET v_descr_ant = p_descr
      LET v_cbx = ui.ComboBox.forName("fusion")
      DISPLAY BY NAME cod
      CALL fn_llenacombo(v_cod_ant)
   ON ACTION guardar
      --validaciones para valores modificados
      IF descr IS NULL THEN
         CALL fn_mensaje("Error", "La descripción es obligatoria.", "information")
         NEXT FIELD descr
      END IF 

      --si pasa las validaciones se realiza proceso
      LET v_hoy =  TODAY
      IF fusion = 0 THEN
         LET fusion = NULL
      END IF
      LET v_consulta = "UPDATE cat_afore   ",
                    "\n SET afore_desc=?   ",
                    "\n    ,afore_fusion=? ",
                    "\n    ,f_actualiza=?  ",
                    "\n    ,usuario=?      ",
                    "\n WHERE afore_cod=?  "
      PREPARE actualiza FROM v_consulta
      EXECUTE actualiza USING descr,fusion,v_hoy,g_usuario_sys,v_cod_ant
      IF sqlca.sqlcode == 0 THEN
         IF sqlca.sqlerrd[3] > 0 THEN
            CALL fn_mensaje("Actualización","Actualziación correcta.","information")
            LET v_salida_cod = 1
            EXIT INPUT
         ELSE
            CALL fn_mensaje("Actualización","No se registraron los datos.","stop")
         END IF
      ELSE
         CALL fn_mensaje("Actualización","Error al registrar la información.","stop")
      END IF
   ON ACTION cancelar
      EXIT INPUT
   END INPUT
   CLOSE WINDOW CATM062
   RETURN v_salida_cod,v_cod_ant,descr

END FUNCTION

#coloca el estatus de baja en el registro de la afore
FUNCTION fn_baja(p_id,p_pid,p_descr)
DEFINE p_id      INTEGER
   ,p_pid        INTEGER
   ,p_descr      LIKE cat_afore.afore_desc
   ,v_consulta   STRING
   ,v_salida_cod SMALLINT
   ,v_hoy        LIKE cat_afore.f_actualiza
   ,v_confirma   SMALLINT

   LET v_salida_cod = 0

   --validaciones para valores eliminados
   IF fn_val_afore_hija(p_id) THEN
      CALL fn_mensaje("Error", "No se puede dar de baja el afore, tiene dependientes.", "information")
      RETURN v_salida_cod
   END IF 

   --confirmación del proceso de eliminacion
   CALL fn_ventana_confirma("Error", "Está seguro de dar de baja la afore "||p_id||" - "||p_descr, "information")
      RETURNING v_confirma
   IF v_confirma <> 0 THEN
      LET v_hoy =  TODAY

      LET v_consulta = "UPDATE cat_afore   ",
                    "\n SET marca=2        ",
                    "\n    ,f_actualiza=?  ",
                    "\n    ,usuario=?      ",
                    "\n WHERE afore_cod=?  "
      PREPARE baja FROM v_consulta
      EXECUTE baja USING v_hoy,g_usuario_sys,p_id
      IF sqlca.sqlcode == 0 THEN
         IF sqlca.sqlerrd[3] > 0 THEN
            CALL fn_mensaje("Eliminación","El registro a sido dado de baja.","information")
            LET v_salida_cod = 1
         ELSE
            CALL fn_mensaje("Eliminación","No se dio de baja el registro.","stop")
         END IF
      ELSE
         CALL fn_mensaje("Eliminación","Error en el proceso.","stop")
      END IF
   END IF

   RETURN v_salida_cod

END FUNCTION

#valida que el código de la afore no este repetido
FUNCTION fn_no_val_afore_cod(p_afore_cod)
DEFINE p_afore_cod LIKE cat_afore.afore_cod
   ,v_afore_cod     LIKE cat_afore.afore_cod
   ,v_sql           STRING
   LET v_sql = "\n select first 1 afore_cod from cat_afore",
                "\n where afore_cod = ?"
   LET v_afore_cod = NULL
   PREPARE sid_afore_val FROM v_sql
   EXECUTE sid_afore_val USING p_afore_cod INTO v_afore_cod
   IF v_afore_cod IS NULL THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION

#valida que la afore no tenga hijas para ser marcada como borrada
FUNCTION fn_val_afore_hija(p_afore_cod)
DEFINE p_afore_cod LIKE cat_afore.afore_cod
   ,v_afore_cod    LIKE cat_afore.afore_cod
   ,v_sql          STRING
   LET v_sql = "\n select first 1 afore_cod from cat_afore",
               "\n where marca=0 AND afore_fusion = ?"
   LET v_afore_cod = NULL
   PREPARE sid_afore_val_hija FROM v_sql
   EXECUTE sid_afore_val_hija USING p_afore_cod INTO v_afore_cod
   IF v_afore_cod IS NULL THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION

#llena los datos de afore fusión para la modificacion de un afore
FUNCTION fn_llenacombo(v_cod)
    DEFINE cbx      ui.ComboBox
    DEFINE consulta STRING
    DEFINE v_id     SMALLINT
    DEFINE v_desc   varchar(100)
    DEFINE v_cod    LIKE cat_afore.afore_cod
    
    LET cbx = ui.ComboBox.forName("fusion")
    LET consulta = "\n SELECT afore_cod,afore_cod||' - '||afore_desc FROM cat_afore   ",
               "\n WHERE NOT afore_cod IN (0,590,",v_cod,") AND marca in (0,1)",
               "\n  ORDER BY marca,afore_cod"

    PREPARE consulta FROM consulta
    DECLARE cursorabc CURSOR FOR consulta

    CALL cbx.clear()
    FOREACH cursorabc INTO  v_id, v_desc
        CALL cbx.addItem(v_id, v_desc)
    END FOREACH
    FREE cursorabc
    FREE consulta
END FUNCTION

