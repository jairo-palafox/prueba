####################################################
# Modulo => CAT                                    #
# Programa => CATM02                               #
# Objetivo => Mantenimiento al catálogo de estado  #
# Fecha de inicio => 21/02/2012                    #
####################################################

DATABASE
    safre_viv
GLOBALS
DEFINE marr_CatBatEdo DYNAMIC ARRAY OF RECORD LIKE  cat_bat_estado.*, --Arreglo para llenar la tabla del CATÁLOGO
       g_usuario_cod LIKE seg_usuario.usuario_cod,
       QryTxt       STRING,
       f             ui.Form,  
       w             ui.window
END GLOBALS
MAIN
DEFINE lc_error  CHAR(1), -- Regresa estatus de error 
       ls_qry    STRING,  -- Contiene resultado de Construct 
       lc_fin    STRING,
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo STRING -- titulo de la ventana

   LET g_usuario_cod = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo = ARG_VAL(3)
   
   IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
   END IF

   OPEN WINDOW vtn_edo_principal WITH FORM "CATM022"
   CLOSE WINDOW SCREEN
      LET lc_error = "N"
      LET INT_FLAG = FALSE 
      LET lc_fin = "N"
      
      WHILE (lc_fin = "N")
         CALL LLenaArryCatBatEdo(" 1=1")
         DISPLAY ARRAY marr_CatBatEdo TO sr_tab_bat_estado.*
            ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
            
            ON ACTION Alta -- Nuevo Registro
               CALL fAddCatBatEdo()  -- Abre ventana para ingresar nuevo registro
               CALL LLenaArryCatBatEdo(" 1=1") -- Actualiza el arreglo
            
            ON ACTION Modificar -- Modificar Registro
               CALL MModMovimientoCatBatEdo(marr_CatBatEdo[ARR_CURR()].*) 
               CALL LLenaArryCatBatEdo(" 1=1") -- Actualiza el arreglo
               
            ON ACTION Eliminar -- Elimina Registro del CATÁLOGO
               IF(marr_CatBatEdo.getLength() >=1)THEN 
                  IF (ARR_CURR() <> 0)THEN
                     IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                               "    Estado: "||marr_CatBatEdo[ARR_CURR()].estado_cod||"\n"||
                   "    Descripción: "||marr_CatBatEdo[ARR_CURR()].estado_descripcion||"\n"||
                   "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                        --DISPLAY marr_CatBatEdo[ARR_CURR()].*
                        CALL MEliMovimientoCatBatEdo(marr_CatBatEdo[ARR_CURR()].*) RETURNING lc_error
                        IF (lc_error = 'S')THEN
                           LET INT_FLAG = TRUE
                           EXIT DISPLAY
                        ELSE                    -- Eliminar del arreglo clave eliminada
                           CALL marr_CatBatEdo.deleteElement(ARR_CURR())
                           CALL fn_mensaje("Atencion","Clave Eliminada con Exito","info")
                        END IF
                     END IF
                     LET INT_FLAG = FALSE
                  END IF
               END IF

            --ON ACTION Consultar -- Consulta
            --   CALL MConMovimientoCatBatEdo() RETURNING ls_qry
            --   CALL LLenaArryCatBatEdo(ls_qry) -- Actualiza el arreglo
               
            ON ACTION Salir
               LET INT_FLAG = TRUE
               LET lc_fin= "S"
         EXIT DISPLAY
         END DISPLAY
      END WHILE
   CLOSE WINDOW vtn_edo_principal            

END MAIN

# Objetivo: Función que llena pantalla principal 
FUNCTION LLenaArryCatBatEdo(ls_qry)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*,
       li_pos       INTEGER,
       ls_qry  STRING

   WHENEVER ERROR CONTINUE
      CALL marr_CatBatEdo.CLEAR()
      LET QryTxT = " SELECT * \n",
                   " FROM   cat_bat_estado \n",
                   " WHERE ",  ls_qry CLIPPED,
                   " ORDER BY estado_cod " 
      PREPARE Prp_ObtRegCatBatEdo FROM QryTxT CLIPPED
      
      LET li_pos = 0
      DECLARE Crs_ObtRegCatBatEdo CURSOR FOR Prp_ObtRegCatBatEdo
         FOREACH Crs_ObtRegCatBatEdo INTO lref_CatBatEdo.*
            LET li_pos = li_pos + 1
            LET marr_CatBatEdo[li_pos].* = lref_CatBatEdo.* 
         END FOREACH
         
      IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje("ATENCION",
                      "OCURRIO UN ERROR AL CARGAR LOS DATOS DE estado "||
                      "\n ERROR :"||SQLCA.SQLCODE, "about")
      LET INT_FLAG = TRUE
      END IF
      FREE Prp_ObtRegCatBatEdo
      
      IF li_pos  = 0  THEN 
         CALL fn_mensaje("ATENCION","SIN REGISTROS EN estado", "about") 
      ELSE
         IF(marr_CatBatEdo[marr_CatBatEdo.getLength()].estado_cod IS NULL OR 
            marr_CatBatEdo[marr_CatBatEdo.getLength()].estado_descripcion CLIPPED = '')THEN
            CALL marr_CatBatEdo.deleteElement(marr_CatBatEdo.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP   

END FUNCTION

# Objetivo: Abre ventana para ingresar un nuevo registro
FUNCTION fAddCatBatEdo()
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*,
       v_maxvalor INTEGER
      
   LET INT_FLAG = FALSE
   
   OPEN WINDOW WAddCatBatEdo WITH FORM "CATM021"

      CALL fn_recumax() RETURNING v_maxvalor
         DISPLAY v_maxvalor TO f_estado_cod
      
      INPUT lref_CatBatEdo.estado_descripcion WITHOUT DEFAULTS 
       FROM f_estado_des ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT

         AFTER FIELD f_estado_des
            IF lref_CatBatEdo.estado_descripcion IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","INTRODUZCA LA DESCRIPCIÓN",'about')
               NEXT FIELD f_estado_des      
   		      END IF
       
      ON ACTION ACCEPT
         IF lref_CatBatEdo.estado_descripcion IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA LA DESCRIPCIÓN",'about')
  			    NEXT FIELD f_estado_des
         END IF
         
         LET lref_CatBatEdo.estado_cod = v_maxvalor
         LET lref_CatBatEdo.estado_descripcion = UPSHIFT(lref_CatBatEdo.estado_descripcion)
         
         IF MAltMovimientoCatBatEdo(lref_CatBatEdo.*) THEN
            INITIALIZE lref_CatBatEdo.* TO NULL
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
   CLOSE WINDOW WAddCatBatEdo 

END FUNCTION

# Objetivo: recuper el valor maximo de la clave de la tabla y lo incrementa en uno
FUNCTION fn_recumax()
DEFINE v_lmaxvalor INTEGER

   SELECT MAX(estado_cod) INTO v_lmaxvalor
    FROM cat_bat_estado
    
    IF v_lmaxvalor IS NULL THEN
       LET v_lmaxvalor = 1
    ELSE
       LET v_lmaxvalor = v_lmaxvalor + 1
    END IF
    
    RETURN v_lmaxvalor

END FUNCTION

#Objetivo: Función que dá de registro  
FUNCTION MAltMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*

   IF SqlInsMovimientoCatBatEdo(lref_CatBatEdo.*) THEN
      CALL fn_mensaje("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#Objetivo : Inserta el Registro (SQL)
FUNCTION SqlInsMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*

   WHENEVER ERROR CONTINUE
      INSERT INTO cat_bat_estado VALUES (lref_CatBatEdo.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#Objetivo : Función modifica registro  
FUNCTION MModMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*
   
   OPEN WINDOW WModCatBatEdo WITH FORM "CATM021"
      INPUT lref_CatBatEdo.estado_cod,lref_CatBatEdo.estado_descripcion WITHOUT DEFAULTS 
      FROM f_estado_cod,f_estado_des ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
         CALL DIALOG.setFieldActive("f_estado_cod",0)
     	  AFTER FIELD f_estado_des
             IF lref_CatBatEdo.estado_descripcion IS NULL THEN 
                CALL fn_mensaje("ATENCIÓN","INTRODUZCA LA DESCRIPCIÓN DEl ESTADO",'about')
                NEXT FIELD f_estado_des
             END IF
      
      ON ACTION ACCEPT
         IF lref_CatBatEdo.estado_descripcion IS NULL THEN 
	 	        CALL fn_mensaje("ATENCIÓN", "INTRODUZCA LA DESCRIPCIÓN DEL ESTADO",'about')
  			    NEXT FIELD f_estado_des
   	     END IF
   	     
   	     IF SqlModMovimientoCatBatEdo(lref_CatBatEdo.*) THEN 
            CALL fn_mensaje("Registro Procesado",
                                "Registro Modificado Satisfactorimente",'about')
            INITIALIZE lref_CatBatEdo.* TO NULL
            CLEAR FORM
            EXIT INPUT
         END IF
         
         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT

         AFTER INPUT    
            CONTINUE INPUT
      END INPUT
   
   CLOSE WINDOW WModCatBatEdo 

END FUNCTION

#Objetivo : Modifica Registro
FUNCTION SqlModMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE  lref_CatBatEdo RECORD LIKE cat_bat_estado.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE cat_bat_estado SET ",
                  "\n estado_descripcion = '",lref_CatBatEdo.estado_descripcion CLIPPED,"'",   
                  "\n WHERE estado_cod = ",lref_CatBatEdo.estado_cod CLIPPED

      PREPARE EnuModcatbatedo FROM QryTxt  
      EXECUTE EnuModcatbatedo 
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#Objetivo: Función elimina registro  
FUNCTION MEliMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*,
       lc_error     CHAR(1)
       
   IF SqlEliMovimientoCatBatEdo(lref_CatBatEdo.*) THEN
      INITIALIZE lref_CatBatEdo.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   
   RETURN lc_error
   
END FUNCTION

#Objetivo: Elimina Registro
FUNCTION SqlEliMovimientoCatBatEdo(lref_CatBatEdo)
DEFINE lref_CatBatEdo RECORD LIKE cat_bat_estado.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM cat_bat_estado",
                  " WHERE estado_cod = ? "
      PREPARE EnuElicatbatedo FROM QryTxt
      EXECUTE EnuElicatbatedo USING lref_CatBatEdo.estado_cod
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP
   
END FUNCTION

#OBJETIVO : Genera Qry de búsqueda
--FUNCTION MConMovimientoCatBatEdo()
--DEFINE ls_qry STRING
--   
--   LET INT_FLAG = FALSE
--   LET ls_qry = " 1=1"
--   
--   OPEN WINDOW w_entQry WITH FORM "CATM021"
--      LET w = ui.Window.getCurrent()
--      LET f = w.getForm()
--      CALL FGL_SETTITLE("CATÁLOGO ESTADOS. BÚSQUEDA")
--      CONSTRUCT ls_qry ON estado_cod,estado_descripcion
--           FROM f_estado_cod,f_estado_des
--
--         BEFORE CONSTRUCT
--
--         ON KEY(INTERRUPT)
--            LET INT_FLAG = TRUE
--            EXIT CONSTRUCT      
--      END CONSTRUCT
--   CLOSE WINDOW w_entQry
--   
--   RETURN ls_qry
--   
--END FUNCTION