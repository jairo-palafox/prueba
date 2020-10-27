####################################################
# Modulo => CAT                                    #
# Programa => CATM04                               #
# Objetivo => Mantenimiento al catálogo de layout  #
# Fecha de inicio => 21/02/2012                    #
####################################################

DATABASE
--Selección de la base de datos SAFREVIVIENDA    
    safre_viv
--Declaración de variables globales
GLOBALS
DEFINE marr_CatLayout DYNAMIC ARRAY OF RECORD LIKE  cat_layout.*, --Arreglo para llenar la tabla del CATÁLOGO LAYOUT
       marr_CatCampo DYNAMIC ARRAY OF RECORD LIKE  cat_campo.*, --Arreglo para llenar la tabla del CATÁLOGO CAMPO
       g_usuario_cod LIKE seg_usuario.usuario_cod,
       QryTxt, QryTxt_2 STRING,
       f             ui.Form,  
       w             ui.window
END GLOBALS

--Programa principal MAIN
MAIN
DEFINE lc_error  CHAR(1), -- Regresa estatus de error 
       ls_qry    STRING,  -- Contiene resultado de Construct 
       lc_fin    STRING,
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo STRING -- titulo de la ventana

--Asignación de argumentos para la ejecución del programa   
   LET g_usuario_cod = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo = ARG_VAL(3)
   
--Funcion que obtiene el titulo del formulario
   IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   OPEN WINDOW vtn_layout_principal WITH FORM "CATM042"
   CLOSE WINDOW SCREEN
      LET lc_error = "N"
      LET INT_FLAG = FALSE 
      LET lc_fin = "N"
      
      WHILE (lc_fin = "N")
      
         CALL LLenaArryCatLayout(" 1=1") --Llamada a función que obtiene los registros de la tabla cat_layout
         
         DIALOG ATTRIBUTES (UNBUFFERED)--Se maneja un DIALOG para el manejo de dos records en un solo formulario
         -- ====================================
         -- SE MUESTRAN LOS LAYOUT REGISTRADOS
         -- ====================================
            DISPLAY ARRAY marr_CatLayout TO sr_tab_layout.* --Se despliega los datos de la tabla cat_layout

               BEFORE ROW

               --Cadena de consulta que se envia al seleccionar un registro del record primario
               LET QryTxt = " layout_cod = ", marr_CatLayout[ARR_CURR()].layout_cod,
                            " AND registro = ", marr_CatLayout[ARR_CURR()].registro
               
               LET QryTxt_2 = QryTxt CLIPPED             
               
               
               CALL LLenaArryCatCampo(QryTxt) --Llamada a función que obtiene los registros de la tabla cat_campo
                                              --Segun criterios seleccionados del records principal (Layout)
               ON ACTION Alta_Layout -- Nuevo Registro para la tabla cat_layout
                  CALL fAddCatLayout(marr_CatLayout[ARR_CURR()].*)  -- Abre ventana para ingresar nuevo registro en la tabla layout
                  CALL LLenaArryCatLayout(" 1=1") -- Actualiza el arreglo principal cat_layout
               
               ON ACTION Alta_Registro -- Nuevo Registro para la tabla cat_layout
                  CALL fAddCatRegistro(marr_CatLayout[ARR_CURR()].*)  -- Abre ventana para ingresar nuevo registro en la tabla layout
                  CALL LLenaArryCatLayout(" 1=1") -- Actualiza el arreglo principal cat_layout   
                  
               ON ACTION Agregar_Campo -- Agregar un registro del tipo campo de la tabla cat_campo
                                       -- Lo relaciona con la tabla cat_layout
                  CALL fAddCatCampo(marr_CatLayout[ARR_CURR()].*)  -- Abre ventana para ingresar nuevo registro para cat_layout
                     LET QryTxt = " layout_cod = ", marr_CatLayout[ARR_CURR()].layout_cod,
                                  " AND registro = ", marr_CatLayout[ARR_CURR()].registro
                  CALL LLenaArryCatCampo(QryTxt) -- Actualiza el arreglo principal cat_layout
                  
               ON ACTION Modificar -- Modificar Registro de la tabla cat_layout
                  CALL MModMovimientoCatLayout(marr_CatLayout[ARR_CURR()].*) --Llamada a la función que controla la modificación
                                                                             --de la tabla cat_layout
                  CALL LLenaArryCatLayout(" 1=1") -- Actualiza el arreglo principal cat_layout
               
               ON ACTION Eliminar -- Elimina Registro del CATÁLOGO cat_layout
                  IF(marr_CatLayout.getLength() >=1)THEN 
                     IF (ARR_CURR() <> 0)THEN
                        IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                                  "    Layout: "||marr_CatLayout[ARR_CURR()].layout_cod||"\n"||
                      "    Registro: "||marr_CatLayout[ARR_CURR()].registro||"\n"||
                      "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                           --Llamada a la función que elimina el registro de la tabla cat_layout
                           CALL MEliMovimientoCatLayout(marr_CatLayout[ARR_CURR()].*) RETURNING lc_error
                           IF (lc_error = 'S')THEN
                              LET INT_FLAG = TRUE
                              EXIT DIALOG
                           ELSE -- Eliminar del arreglo de pantalla clave eliminada
                              CALL marr_CatLayout.deleteElement(ARR_CURR())
                              CALL fn_mensaje("Atencion","Clave Eliminada con Exito","info")
                           END IF
                        END IF
                        LET INT_FLAG = FALSE
                     END IF
                  END IF
               
               ON ACTION Consultar -- Consulta del catálaogo cat_layout
                  CALL MConMovimientoCatlayout() RETURNING ls_qry
                  CALL LLenaArryCatLayout(ls_qry) -- Actualiza el arreglo principal cat_layout
                  IF(marr_CatLayout.getLength() = 0) THEN
                     CALL LLenaArryCatLayout(" 1=1")
                  END IF  

                  IF ARR_CURR() > marr_CatLayout.getLength() THEN
                    -- Cadena de consulta que se envia al seleccionar un registro del record primario
                    LET QryTxt = " layout_cod = ", marr_CatLayout[marr_CatLayout.getLength()].layout_cod,
                                 " AND registro = ", marr_CatLayout[marr_CatLayout.getLength()].registro
                  ELSE
                    -- Cadena de consulta que se envia al seleccionar un registro del record primario
                    LET QryTxt = " layout_cod = ", marr_CatLayout[ARR_CURR()].layout_cod,
                                 " AND registro = ", marr_CatLayout[ARR_CURR()].registro
                  END IF
                  
                  CALL LLenaArryCatCampo(QryTxt) -- Actualiza el arreglo principal cat_campo
                  --CONTINUE DIALOG
               
               ON ACTION Salir --Termina el proceso o termina la aplicación
                  LET INT_FLAG = TRUE
                  LET lc_fin= "S"
            EXIT DIALOG
            END DISPLAY
            
            -- ====================================================================
            -- SE MUESTRAN LOS CAMPOS REGISTRADOS Y QUE ESTAN ASOCIADOS A UN LAYOUT
            -- ====================================================================
            
            DISPLAY ARRAY marr_CatCampo TO sr_tab_campo.* -- despliega los registro de la tabla cat_campo
               
               ON ACTION Agregar_Campo -- Agregar un registro del tipo campo de la tabla cat_campo
                                       -- Lo relaciona con la tabla cat_layout
                  CALL fAddCatCampo(marr_CatLayout[ARR_CURR()].*)  -- Abre ventana para ingresar nuevo registro de la tabla cat_campo
                  CALL LLenaArryCatCampo(" 1=1") -- Actualiza el arreglo secundario cat_campo
               
               ON ACTION Modificar -- Modifica el registro seleccionado de la tabla cat_campo
                  --Llamada de función principal para modificar de base de datos cat_campo
                  CALL MModMovimientoCatCampo(marr_CatCampo[ARR_CURR()].*) 
                  CALL LLenaArryCatCampo(QryTxt_2) -- Actualiza el arreglo secundario cat_campo
                  
               ON ACTION Eliminar -- Elimina Registro del CATÁLOGO de la tabla cat_campo
                  IF(marr_CatCampo.getLength() >=1)THEN 
                     IF (ARR_CURR() <> 0)THEN
                        IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                                  "    Layout: "||marr_CatCampo[ARR_CURR()].layout_cod||"\n"||
                      "    Registro: "||marr_CatCampo[ARR_CURR()].registro||"\n"||
                      "    Campo: "||marr_CatCampo[ARR_CURR()].campo_cod||"\n"||
                      "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                           --Llamada a la función que elimina el registro de la tabla cat_campo
                           CALL MEliMovimientoCatCampo(marr_CatCampo[ARR_CURR()].*) RETURNING lc_error
                           IF (lc_error = 'S')THEN
                              LET INT_FLAG = TRUE
                              EXIT DIALOG
                           ELSE -- Eliminar del arreglo de pantalla clave eliminada
                              CALL marr_CatCampo.deleteElement(ARR_CURR())
                              CALL fn_mensaje("Atencion","Clave Eliminada con Exito","info")
                           END IF
                        END IF
                        LET INT_FLAG = FALSE
                     END IF
                  END IF
                
               ON ACTION Consultar -- Consulta un registro de la tabla cat_campo
                 CALL MConMovimientoCatCampo() RETURNING ls_qry
                 CALL LLenaArryCatCampo(ls_qry) -- Actualiza el arreglo secundario cat_campo
               
               ON ACTION Salir --Termina el proceso o termina la aplicación
                  LET INT_FLAG = TRUE
                  LET lc_fin= "S"
            EXIT DIALOG
            END DISPLAY

         END DIALOG --Finaliza el DIALOG  
      END WHILE
   CLOSE WINDOW vtn_layout_principal

END MAIN

# Objetivo: Función que llena pantalla principal de layout cat_layout
FUNCTION LLenaArryCatLayout(ls_qry)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*,
       li_pos       INTEGER,
       ls_qry  STRING
    
   WHENEVER ERROR CONTINUE
     CALL marr_CatLayout.CLEAR()
     LET QryTxT = " SELECT * \n",
                  " FROM   cat_layout \n",
                  " WHERE ",  ls_qry CLIPPED,
                  " ORDER BY layout_cod " 
     PREPARE Prp_ObtRegCatLayout FROM QryTxT CLIPPED
     
     LET li_pos = 0
      DECLARE Crs_ObtRegCatLayout CURSOR FOR Prp_ObtRegCatLayout
         FOREACH Crs_ObtRegCatLayout INTO lref_CatLayout.*
            LET li_pos = li_pos + 1
            LET marr_CatLayout[li_pos].* = lref_CatLayout.* 
         END FOREACH
     
     IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje("ATENCION",
                      "OCURRIO UN ERROR AL CARGAR LOS DATOS DE Layout "||
                      "\n ERROR :"||SQLCA.SQLCODE, "about")
         LET INT_FLAG = TRUE
      END IF
      FREE Prp_ObtRegCatLayout
      
      IF li_pos  = 0  THEN 
         CALL fn_mensaje("ATENCION","SIN REGISTROS EN Layout", "about") 
      ELSE
         IF(marr_CatLayout[marr_CatLayout.getLength()].layout_cod IS NULL OR 
            marr_CatLayout[marr_CatLayout.getLength()].registro CLIPPED = '')THEN
            CALL marr_CatLayout.deleteElement(marr_CatLayout.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP

END FUNCTION

# Objetivo: Abre ventana para ingresar un nuevo registro para la tabla cat_layout
FUNCTION fAddCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*,
       v_existe, v_maxlayout INTEGER,
       v_maxregistro CHAR(2)
       
   LET INT_FLAG = FALSE
   INITIALIZE lref_CatLayout.* TO NULL
   
   --Llamada a función para obtener el valor maximo de la tabla cat_layout
   CALL fn_recumaxlayout() RETURNING v_maxlayout, v_maxregistro
   LET lref_CatLayout.layout_cod = v_maxlayout
   LET lref_CatLayout.registro = v_maxregistro
   
   OPEN WINDOW WAddCatLayout WITH FORM "CATM041"
      
      INPUT lref_CatLayout.layout_cod, lref_CatLayout.registro, lref_CatLayout.descripcion,
            lref_CatLayout.programa, lref_CatLayout.tabla, lref_CatLayout.archivo,
            lref_CatLayout.origen_cod, lref_CatLayout.modulo_cod WITHOUT DEFAULTS 
         FROM f_layout_cod, f_registro, f_descripcion, 
              f_programa, f_tabla, f_archivo,
              f_origen_cod, f_modulo_cod ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         
         CALL DIALOG.setFieldActive("f_layout_cod",FALSE)
         
         AFTER FIELD f_registro
            IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_registro      
   		      END IF
   		      
   		      SELECT COUNT (*) INTO v_existe
             FROM cat_layout
  		        WHERE layout_cod = lref_CatLayout.layout_cod
  		        AND registro = lref_CatLayout.registro
  		      
  		      IF v_existe > 0 THEN
  		         CALL fn_mensaje ("ATENCIÓN", "EL REGISTRO YA EXISTE", "about")
  		         NEXT FIELD f_registro
  		      END IF

   		ON ACTION ACCEPT
   		
   		   IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_layout_cod      
   		   END IF
   		   
   		   IF MAltMovimientoCatLayout(lref_CatLayout.*) THEN
            INITIALIZE lref_CatLayout.* TO NULL
            CLEAR FORM 
            EXIT INPUT
         ELSE 
            CONTINUE INPUT
         END IF
      
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WAddCatLayout    
      
END FUNCTION

# Objetivo: Abre ventana para ingresar un nuevo registro con un layout existente a la tabla cat_layout
FUNCTION fAddCatRegistro(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*,
       v_existe, v_maxlayout INTEGER,
       v_maxregistro CHAR(2)
       
   LET INT_FLAG = FALSE
   LET v_maxlayout = lref_CatLayout.layout_cod
   INITIALIZE lref_CatLayout.* TO NULL
   LET lref_CatLayout.layout_cod = v_maxlayout
      
   OPEN WINDOW WAddCatRegistro WITH FORM "CATM041"
      
      INPUT lref_CatLayout.layout_cod, lref_CatLayout.registro, lref_CatLayout.descripcion,
            lref_CatLayout.programa, lref_CatLayout.tabla, lref_CatLayout.archivo,
            lref_CatLayout.origen_cod, lref_CatLayout.modulo_cod WITHOUT DEFAULTS 
         FROM f_layout_cod, f_registro, f_descripcion, 
              f_programa, f_tabla, f_archivo,
              f_origen_cod, f_modulo_cod ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         
         CALL DIALOG.setFieldActive("f_layout_cod",FALSE)
         
         AFTER FIELD f_registro
            IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_registro      
   		      END IF
   		      
   		      SELECT COUNT (*) INTO v_existe
             FROM cat_layout
  		        WHERE layout_cod = lref_CatLayout.layout_cod
  		        AND registro = lref_CatLayout.registro
  		      
  		      IF v_existe > 0 THEN
  		         CALL fn_mensaje ("ATENCIÓN", "EL REGISTRO YA EXISTE", "about")
  		         NEXT FIELD f_registro
  		      END IF

   		ON ACTION ACCEPT
   		
   		   IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_layout_cod      
   		   END IF
   		   
   		   IF MAltMovimientoCatLayout(lref_CatLayout.*) THEN
            INITIALIZE lref_CatLayout.* TO NULL
            CLEAR FORM 
            EXIT INPUT
         ELSE 
            CONTINUE INPUT
         END IF
      
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WAddCatRegistro    
      
END FUNCTION

#Objetivo: Función que inserta un registro a la tabla cat_layout
FUNCTION MAltMovimientoCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*

   IF SqlInsMovimientoCatLayout(lref_CatLayout.*) THEN
      CALL fn_mensaje("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#Objetivo : Inserta el Registro (SQL) a la tabla cat_layout, validado con codigo de error SQL
FUNCTION SqlInsMovimientoCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*

   WHENEVER ERROR CONTINUE
      INSERT INTO cat_layout VALUES (lref_CatLayout.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#Objetivo : Función modifica un registro de la tabla cat_layout
FUNCTION MModMovimientoCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*,
       v_existe INTEGER

   OPEN WINDOW WModCatLayout WITH FORM "CATM041"
      INPUT lref_CatLayout.layout_cod, lref_CatLayout.registro, lref_CatLayout.descripcion,
            lref_CatLayout.programa, lref_CatLayout.tabla, lref_CatLayout.archivo,
            lref_CatLayout.origen_cod, lref_CatLayout.modulo_cod WITHOUT DEFAULTS 
         FROM f_layout_cod, f_registro, f_descripcion, 
              f_programa, f_tabla, f_archivo,
              f_origen_cod, f_modulo_cod ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         CALL DIALOG.setFieldActive("f_layout_cod",FALSE)
         CALL DIALOG.setFieldActive("f_registro",FALSE)
         AFTER FIELD f_layout_cod
            IF lref_CatLayout.layout_cod IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL LAYOUT",'about')
               NEXT FIELD f_layout_cod      
   		      END IF
  		   
  		   AFTER FIELD f_registro
            IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_registro      
   		      END IF
   		      
   		      SELECT COUNT (*) INTO v_existe
             FROM cat_layout
  		        WHERE layout_cod = lref_CatLayout.layout_cod
  		        AND registro = lref_CatLayout.registro
  		      
  		      IF v_existe > 0 THEN
  		         CALL fn_mensaje ("ATENCIÓN", "EL REGISTRO YA EXISTE", "about")
  		         NEXT FIELD f_registro
  		      END IF
   		  
   		ON ACTION ACCEPT
         IF lref_CatLayout.layout_cod IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL LAYOUT",'about')
               NEXT FIELD f_layout_cod      
   		   END IF
   		   
   		   IF lref_CatLayout.registro IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA EL REGISTRO",'about')
               NEXT FIELD f_layout_cod      
   		   END IF
   		   
   		   IF SqlModMovimientoCatLayout(lref_CatLayout.*) THEN 
            CALL fn_mensaje("Registro Procesado",
                                "Registro Modificado Satisfactorimente",'about')
            INITIALIZE lref_CatLayout.* TO NULL
            CLEAR FORM
            EXIT INPUT
         END IF
      
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WModCatLayout        

END FUNCTION

#Objetivo : Modifica de la base de datos un registro de la tabla cat_layout, con codigo de error SQL
FUNCTION SqlModMovimientoCatLayout(lref_CatLayout)
DEFINE  lref_CatLayout RECORD LIKE cat_layout.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE cat_layout SET ",
                  "\n descripcion = '",lref_CatLayout.descripcion CLIPPED,"',",
                  "\n programa = '",lref_CatLayout.programa CLIPPED,"',",
                  "\n tabla = '",lref_CatLayout.tabla CLIPPED,"',",
                  "\n archivo = '",lref_CatLayout.archivo CLIPPED,"',",
                  "\n origen_cod = ",lref_CatLayout.origen_cod CLIPPED,",",
                  "\n modulo_cod = '",lref_CatLayout.modulo_cod CLIPPED,"'",
                  "\n WHERE layout_cod = ",lref_CatLayout.layout_cod CLIPPED,
                  "\n AND registro = '",lref_CatLayout.registro CLIPPED,"'"
      
      PREPARE EnuModcatlayout FROM QryTxt  
      EXECUTE EnuModcatlayout
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP            

END FUNCTION


#Objetivo: Función que llama a la función que elimina de la base de datos un Registro
#          de la tabla cat_layout            
FUNCTION MEliMovimientoCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*,
       lc_error     CHAR(1)
       
   IF SqlEliMovimientoCatLayout(lref_CatLayout.*) THEN
      INITIALIZE lref_CatLayout.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   
   RETURN lc_error
   
END FUNCTION

#Objetivo: Elimina un registro de la tabla cat_layout, con codigo de error SQL
FUNCTION SqlEliMovimientoCatLayout(lref_CatLayout)
DEFINE lref_CatLayout RECORD LIKE cat_layout.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM cat_layout",
                  " WHERE layout_cod = ? ",
                  " AND registro = ? "
      PREPARE EnuElicatlayout FROM QryTxt
      EXECUTE EnuElicatlayout USING lref_CatLayout.layout_cod, lref_CatLayout.registro
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#OBJETIVO : Genera Qry de búsqueda para la tabla cat_layout
FUNCTION MConMovimientoCatlayout()
DEFINE ls_qry STRING
   
   LET INT_FLAG = FALSE
   LET ls_qry = " 1=1"
   
   OPEN WINDOW w_entQry WITH FORM "CATM041"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      CALL FGL_SETTITLE("CATÁLOGO LAYOUT. BÚSQUEDA")
      CONSTRUCT ls_qry ON layout_cod, registro, descripcion,
                          programa, tabla, archivo,
                          origen_cod, modulo_cod
          FROM f_layout_cod, f_registro, f_descripcion,
               f_programa, f_tabla, f_archivo,
               f_origen_cod, f_modulo_cod

         BEFORE CONSTRUCT

         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            LET ls_qry = " 1=1"
            EXIT CONSTRUCT      
      END CONSTRUCT
   CLOSE WINDOW w_entQry
   
   RETURN ls_qry
   
END FUNCTION

# Objetivo: Función que llena pantalla principal de la tabla cat_campo
FUNCTION LLenaArryCatCampo(ls_qry)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*,
       li_pos       INTEGER,
       ls_qry  STRING
    
   WHENEVER ERROR CONTINUE
     CALL marr_CatCampo.CLEAR()
     LET QryTxT = " SELECT * \n",
                  " FROM   cat_campo \n",
                  " WHERE ",  ls_qry CLIPPED,
                  " ORDER BY layout_cod " 
     PREPARE Prp_ObtRegCatCampo FROM QryTxT CLIPPED
     
     LET li_pos = 0
      DECLARE Crs_ObtRegCatCampo CURSOR FOR Prp_ObtRegCatCampo
         FOREACH Crs_ObtRegCatCampo INTO lref_CatCampo.*
            LET li_pos = li_pos + 1
            LET marr_CatCampo[li_pos].* = lref_CatCampo.* 
         END FOREACH
     
     IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje("ATENCION",
                      "OCURRIO UN ERROR AL CARGAR LOS DATOS DE Campo "||
                      "\n ERROR :"||SQLCA.SQLCODE, "about")
         LET INT_FLAG = TRUE
      END IF
      FREE Prp_ObtRegCatCampo
      
      IF li_pos  = 0  THEN 
         CALL fn_mensaje("ATENCION","SIN REGISTROS EN Campo", "about") 
      ELSE
         IF(marr_CatCampo[marr_CatCampo.getLength()].layout_cod IS NULL OR 
            marr_CatCampo[marr_CatCampo.getLength()].registro CLIPPED = '')THEN
            CALL marr_CatCampo.deleteElement(marr_CatCampo.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP

END FUNCTION

# Objetivo: Abre ventana para ingresar un nuevo registro en la tabla cat_campo
FUNCTION fAddCatCampo(lref_CatLayout)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*,
       lref_CatLayout RECORD LIKE cat_layout.*,
       v_existe, v_maxvalor SMALLINT
       
   LET INT_FLAG = FALSE
   LET lref_CatCampo.layout_cod = lref_CatLayout.layout_cod
   LET lref_CatCampo.registro = lref_CatLayout.registro

   OPEN WINDOW WAddCatCampo WITH FORM "CATM043"
      
      --Llama da función que recupera el maximo de la tabla cat_campo
      CALL fn_recumax(lref_CatCampo.layout_cod, lref_CatCampo.registro) RETURNING v_maxvalor
           LET lref_CatCampo.campo_cod = v_maxvalor 
      
      INPUT lref_CatCampo.layout_cod, lref_CatCampo.registro, lref_CatCampo.campo_cod,
            lref_CatCampo.campo_desc, lref_CatCampo.tipo_dato, lref_CatCampo.longitud,
            lref_CatCampo.precision, lref_CatCampo.pos_inicial, lref_CatCampo.pos_final,
            lref_CatCampo.valor_cte, lref_CatCampo.num_valida WITHOUT DEFAULTS 
         FROM f_layout_cod, f_registro, f_campo_cod,    
              f_campo_desc, f_tipo_dato, f_longitud,
              f_precision, f_pos_inicial, f_pos_final, 
              f_valor_cte, f_num_valida ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         CALL DIALOG.setFieldActive("f_layout_cod",FALSE)
         CALL DIALOG.setFieldActive("f_registro",FALSE)
         CALL DIALOG.setFieldActive("f_campo_cod",FALSE)
   		  
   		ON ACTION ACCEPT

   		   IF MAltMovimientoCatCampo(lref_CatCampo.*) THEN
            INITIALIZE lref_CatCampo.* TO NULL
            CLEAR FORM 
            EXIT INPUT
         ELSE 
            CONTINUE INPUT
         END IF
      
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WAddCatCampo    
      
END FUNCTION

#Objetivo: Función que inserta un registro a la tabla cat_campo
FUNCTION MAltMovimientoCatCampo(lref_CatCampo)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*

   IF SqlInsMovimientoCatCampo(lref_CatCampo.*) THEN
      CALL fn_mensaje("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#Objetivo : Inserta el Registro (SQL) en la tabla cat_campo, con codigo de error SQL
FUNCTION SqlInsMovimientoCatCampo(lref_CatCampo)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*
   
   WHENEVER ERROR CONTINUE
      INSERT INTO cat_campo VALUES (lref_CatCampo.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#Objetivo : Función modifica registro de la tabla cat_campo
FUNCTION MModMovimientoCatCampo(lref_CatCampo)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*,
       v_existe INTEGER

   OPEN WINDOW WModCatCampo WITH FORM "CATM043"
      INPUT lref_CatCampo.layout_cod, lref_CatCampo.registro, lref_CatCampo.campo_cod,
            lref_CatCampo.campo_desc, lref_CatCampo.tipo_dato, lref_CatCampo.longitud,
            lref_CatCampo.precision, lref_CatCampo.pos_inicial, lref_CatCampo.pos_final,
            lref_CatCampo.valor_cte, lref_CatCampo.num_valida WITHOUT DEFAULTS 
         FROM f_layout_cod, f_registro, f_campo_cod,    
              f_campo_desc, f_tipo_dato, f_longitud,
              f_precision, f_pos_inicial, f_pos_final, 
              f_valor_cte, f_num_valida ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         CALL DIALOG.setFieldActive("f_layout_cod",FALSE)
         CALL DIALOG.setFieldActive("f_registro",FALSE)
         CALL DIALOG.setFieldActive("f_campo_cod",FALSE)
   		  
   		   ON ACTION ACCEPT

   		   --Llamada de la función que actualiza un registro de la tabla cat_campo
   		   IF SqlModMovimientoCatCampo(lref_CatCampo.*) THEN 
            CALL fn_mensaje("Registro Procesado",
                                "Registro Modificado Satisfactorimente",'about')
            INITIALIZE lref_CatCampo.* TO NULL
            CLEAR FORM
            EXIT INPUT
         END IF
      
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WModCatCampo        

END FUNCTION

#Objetivo : Modifica un Registro de la tabla cat_campo, con codigo de error SQL
FUNCTION SqlModMovimientoCatCampo(lref_CatCampo)
DEFINE  lref_CatCampo RECORD LIKE cat_campo.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE cat_campo SET ",
                  "\n campo_desc = '",lref_CatCampo.campo_desc CLIPPED,"',",
                  "\n tipo_dato = '",lref_CatCampo.tipo_dato CLIPPED,"',",
                  "\n longitud = '",lref_CatCampo.longitud CLIPPED,"',",
                  "\n precision = '",lref_CatCampo.precision CLIPPED,"',",
                  "\n pos_inicial = '",lref_CatCampo.pos_inicial CLIPPED,"',",
                  "\n pos_final = '",lref_CatCampo.pos_final CLIPPED,"',",
                  "\n valor_cte = '",lref_CatCampo.valor_cte CLIPPED,"',",
                  "\n num_valida = '",lref_CatCampo.num_valida CLIPPED,"'",
                  "\n WHERE layout_cod = ",lref_CatCampo.layout_cod CLIPPED,
                  "\n AND registro = '",lref_CatCampo.registro CLIPPED,"'",
                  "\n AND campo_cod = '",lref_CatCampo.campo_cod CLIPPED,"'"

      PREPARE EnuModcatcampo FROM QryTxt  
      EXECUTE EnuModcatcampo
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP            

END FUNCTION

#Objetivo: Función que elimina un registro de la tabla cat_campom con codigo de error SQL
FUNCTION MEliMovimientoCatCampo(lref_CatCampo)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*,
       lc_error     CHAR(1)
       
   IF SqlEliMovimientoCatCampo(lref_CatCampo.*) THEN
      INITIALIZE lref_CatCampo.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   
   RETURN lc_error
   
END FUNCTION

#Objetivo: Elimina un registro de la tabla cat_campo, con codigo de error SQL
FUNCTION SqlEliMovimientoCatCampo(lref_CatCampo)
DEFINE lref_CatCampo RECORD LIKE cat_campo.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM cat_campo",
                  " WHERE layout_cod = ? ",
                  " AND registro = ? ",
                  " AND campo_cod = ? "
                  
      PREPARE EnuElicatcampo FROM QryTxt
      EXECUTE EnuElicatcampo USING lref_CatCampo.layout_cod, lref_CatCampo.registro, lref_CatCampo.campo_cod
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#OBJETIVO : Genera Qry de búsqueda para la tabla cat_campo
FUNCTION MConMovimientoCatCampo()
DEFINE ls_qry STRING
   
   LET INT_FLAG = FALSE
   LET ls_qry = " 1=1"
   
   OPEN WINDOW w_entQry WITH FORM "CATM043"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      CALL FGL_SETTITLE("CATÁLOGO LAYOUT. BÚSQUEDA")
      CONSTRUCT ls_qry ON layout_cod, registro, campo_cod,
                          campo_desc, tipo_dato, longitud,
                          precision, pos_inicial, pos_final,
                          valor_cte, num_valida
          FROM f_layout_cod, f_registro, f_campo_cod,    
              f_campo_desc, f_tipo_dato, f_longitud,
              f_precision, f_pos_inicial, f_pos_final, 
              f_valor_cte, f_num_valida

         BEFORE CONSTRUCT

         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT      
      END CONSTRUCT
   CLOSE WINDOW w_entQry
   
   RETURN ls_qry
   
END FUNCTION

# Objetivo: recuper el valor maximo de la clave de la tabla cat_campo y lo incrementa en uno
FUNCTION fn_recumax(v_layout_cod, v_registro)
DEFINE v_layout_cod LIKE cat_campo.layout_cod, 
       v_registro LIKE cat_campo.registro,
       v_lmaxvalor SMALLINT
  
   SELECT MAX(campo_cod) INTO v_lmaxvalor
    FROM cat_campo
     WHERE layout_cod = v_layout_cod
     AND registro = v_registro

    IF v_lmaxvalor IS NULL THEN
       LET v_lmaxvalor = 1
    ELSE
       LET v_lmaxvalor = v_lmaxvalor + 1
    END IF
    
    RETURN v_lmaxvalor

END FUNCTION

# Objetivo: recuper el valor maximo de la clave de la tabla cat_layout y lo incrementa en uno
FUNCTION fn_recumaxlayout()
DEFINE v_lmaxvalor_layout, v_lmaxvalor_reg  SMALLINT

   SELECT MAX(layout_cod) INTO v_lmaxvalor_layout
    FROM cat_layout
    
    IF v_lmaxvalor_layout IS NULL THEN
       LET v_lmaxvalor_layout = 1
    ELSE
       LET v_lmaxvalor_layout = v_lmaxvalor_layout + 1
    END IF
    
    SELECT MAX(registro) INTO v_lmaxvalor_reg
     FROM cat_layout
      WHERE layout_cod = v_lmaxvalor_layout
      
      IF v_lmaxvalor_reg IS NULL THEN
         LET v_lmaxvalor_reg = 1
      ELSE
         LET v_lmaxvalor_reg = v_lmaxvalor_reg + 1
      END IF
    
    RETURN v_lmaxvalor_layout, v_lmaxvalor_reg

END FUNCTION