##################################################################
# Modulo => RET                                                  #
# Programa => RETM275                                            #
# Objetivo => Mantenimiento al catálogo de Registros Patronales  #
# Fecha de inicio => 16/07/2014                                  #
##################################################################

DATABASE
    safre_viv
GLOBALS
DEFINE marr_RetCatNrp DYNAMIC ARRAY OF RECORD LIKE  ret_cat_nrp.*, --Arreglo para llenar la tabla del CATÁLOGO
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

   OPEN WINDOW vtn_edo_principal WITH FORM "RETM2752"
   CLOSE WINDOW SCREEN
      LET lc_error = "N"
      LET INT_FLAG = FALSE 
      LET lc_fin = "N"
      
      WHILE (lc_fin = "N")
         CALL LLenaArryRetCatNrp(" 1=1")
         DISPLAY ARRAY marr_RetCatNrp TO sr_tab_cat_nrp.*
            ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
            
            ON ACTION Alta -- Nuevo Registro
               CALL fAddRetCatNrp()  -- Abre ventana para ingresar nuevo registro
               CALL LLenaArryRetCatNrp(" 1=1") -- Actualiza el arreglo
            
            ON ACTION Modificar -- Modificar Registro
               CALL MModMovimientoRetCatNrp(marr_RetCatNrp[ARR_CURR()].*) 
               CALL MInsertMovHistRetCatNrp(marr_RetCatNrp[ARR_CURR()].nrp, marr_RetCatNrp[ARR_CURR()].nombre_patron, "A") RETURNING lc_error
               CALL LLenaArryRetCatNrp(" 1=1") -- Actualiza el arreglo
               
            ON ACTION Eliminar -- Elimina Registro del CATÁLOGO
               IF(marr_RetCatNrp.getLength() >=1)THEN 
                  IF (ARR_CURR() <> 0)THEN
                     IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                               "    Estado: "||marr_RetCatNrp[ARR_CURR()].nrp||"\n"||
                   "    Descripción: "||marr_RetCatNrp[ARR_CURR()].nombre_patron||"\n"||
                   "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                        --DISPLAY marr_RetCatNrp[ARR_CURR()].*
                        CALL MEliMovimientoRetCatNrp(marr_RetCatNrp[ARR_CURR()].*) RETURNING lc_error
                        IF (lc_error = 'S')THEN
                           LET INT_FLAG = TRUE
                           EXIT DISPLAY
                        ELSE                    -- Inserta en el historico y Eliminar del arreglo clave eliminada
                           CALL MInsertMovHistRetCatNrp(marr_RetCatNrp[ARR_CURR()].nrp, marr_RetCatNrp[ARR_CURR()].nombre_patron, "E") RETURNING lc_error
                           CALL marr_RetCatNrp.deleteElement(ARR_CURR())
                           CALL fn_mensaje("Atencion","Clave Eliminada con Exito","info")
                        END IF
                     END IF
                     LET INT_FLAG = FALSE
                  END IF
               END IF

            --ON ACTION Consultar -- Consulta
            --   CALL MConMovimientoCatBatEdo() RETURNING ls_qry
            --   CALL LLenaArryRetCatNrp(ls_qry) -- Actualiza el arreglo
               
            ON ACTION Salir
               LET INT_FLAG = TRUE
               LET lc_fin= "S"
         EXIT DISPLAY
         END DISPLAY
      END WHILE
   CLOSE WINDOW vtn_edo_principal            

END MAIN

# Objetivo: Función que llena pantalla principal 
FUNCTION LLenaArryRetCatNrp(ls_qry)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*,
       li_pos       INTEGER,
       ls_qry  STRING

   WHENEVER ERROR CONTINUE
      CALL marr_RetCatNrp.CLEAR()
      LET QryTxT = " SELECT * \n",
                   " FROM   ret_cat_nrp \n",
                   " WHERE ",  ls_qry CLIPPED,
                   " ORDER BY nombre_patron " 
      PREPARE Prp_ObtRegRetCatNrp FROM QryTxT CLIPPED
      
      LET li_pos = 0
      DECLARE Crs_ObtRegRetCatNrp CURSOR FOR Prp_ObtRegRetCatNrp
         FOREACH Crs_ObtRegRetCatNrp INTO lref_RetCatNrp.*
            LET li_pos = li_pos + 1
            LET marr_RetCatNrp[li_pos].* = lref_RetCatNrp.* 
         END FOREACH
         
      IF(SQLCA.SQLCODE < 0)THEN
      CALL fn_mensaje("ATENCION",
                      "OCURRIO UN ERROR AL CARGAR LOS DATOS DE LOS PATRONES "||
                      "\n ERROR :"||SQLCA.SQLCODE, "about")
      LET INT_FLAG = TRUE
      END IF
      FREE Prp_ObtRegRetCatNrp
      
      IF li_pos  = 0  THEN 
         CALL fn_mensaje("ATENCION","SIN REGISTROS EN EL CATALOGO DE PATRONES ", "about") 
      ELSE
         IF(marr_RetCatNrp[marr_RetCatNrp.getLength()].nrp IS NULL OR 
            marr_RetCatNrp[marr_RetCatNrp.getLength()].nombre_patron CLIPPED = '')THEN
            CALL marr_RetCatNrp.deleteElement(marr_RetCatNrp.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP   

END FUNCTION

# Objetivo: Abre ventana para ingresar un nuevo registro
FUNCTION fAddRetCatNrp()
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*,
       v_maxvalor INTEGER
      
   LET INT_FLAG = FALSE
   
   OPEN WINDOW WAddRetCatNrp WITH FORM "RETM2751"

      INPUT lref_RetCatNrp.nrp, lref_RetCatNrp.nombre_patron WITHOUT DEFAULTS 
       FROM f_nrp, f_nombre_patron ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT

         AFTER FIELD f_nombre_patron
            IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","DEDE INGRESAR LA INFORMACIÓN COMPLETA",'about')
               NEXT FIELD f_nrp      
   		      END IF
       
      ON ACTION ACCEPT
         IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
		     CALL fn_mensaje("ERROR","DEDE INGRESAR LA INFORMACIÓN COMPLETA",'about')
  			 NEXT FIELD f_nrp
         END IF
         
         LET lref_RetCatNrp.nrp = UPSHIFT(lref_RetCatNrp.nrp)
         LET lref_RetCatNrp.nombre_patron = UPSHIFT(lref_RetCatNrp.nombre_patron)
         
         IF MAltMovimientoRetCatNrp(lref_RetCatNrp.*) THEN
             INITIALIZE lref_RetCatNrp.* TO NULL
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
   CLOSE WINDOW WAddRetCatNrp 

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

#Objetivo: Función que dá de alta el registro  
FUNCTION MAltMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*

   IF SqlInsMovimientoRetCatNrp(lref_RetCatNrp.*) THEN
      CALL fn_mensaje("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#Objetivo : Inserta el Registro (SQL)
FUNCTION SqlInsMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*

   WHENEVER ERROR CONTINUE
      INSERT INTO ret_cat_nrp VALUES (lref_RetCatNrp.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#Objetivo : Función modifica registro  
FUNCTION MModMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*
       
   OPEN WINDOW WModRetCatNrp WITH FORM "RETM2751"
      INPUT lref_RetCatNrp.nrp,lref_RetCatNrp.nombre_patron WITHOUT DEFAULTS 
      FROM f_nrp,f_nombre_patron ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT
        -- CALL DIALOG.setFieldActive("f_nrp",0)
        -- CALL DIALOG.setFieldActive("f_nombre_patron",0)
     	  AFTER FIELD f_nombre_patron
             IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
                CALL fn_mensaje("ATENCIÓN","INTRODUZCA LA INFORMACIÓN COMPLETA ",'about')
                NEXT FIELD f_nrp
             END IF
      ON ACTION ACCEPT
         IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
	 	        CALL fn_mensaje("ATENCIÓN", "INTRODUZCA LA INFORMACIÓN COMPLETA ",'about')
  			    NEXT FIELD f_nrp
   	     END IF
   	     --DISPLAY "Envia arreglo para modificacion de datos"
   	     IF SqlModMovimientoRetCatNrp(lref_RetCatNrp.*) THEN 
            CALL fn_mensaje("Registro Procesado",
                                "Registro Modificado Satisfactorimente",'about')
            INITIALIZE lref_RetCatNrp.* TO NULL
            CLEAR FORM
            EXIT INPUT
         END IF
         
         ON KEY(INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT INPUT

         AFTER INPUT    
            CONTINUE INPUT
      END INPUT
   
   CLOSE WINDOW WModRetCatNrp 

END FUNCTION

#Objetivo : Modifica Registro
FUNCTION SqlModMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE  lref_RetCatNrp RECORD LIKE ret_cat_nrp.*
   --DISPLAY "Modificando Informacion"
   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE ret_cat_nrp SET ",
                  "\n nombre_patron = '",lref_RetCatNrp.nombre_patron CLIPPED,"'",   
                  "\n WHERE nrp = '", lref_RetCatNrp.nrp CLIPPED, "'"
      --DISPLAY "El Query de Actualizacion >" || QryTxt || "<"
      PREPARE EnuModretcatnrp FROM QryTxt  
      EXECUTE EnuModretcatnrp 
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         --DISPLAY "Modificacion Exitosa"
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#Objetivo: Función elimina registro  
FUNCTION MEliMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*,
       lc_error     CHAR(1)
       
   IF SqlEliMovimientoRetCatNrp(lref_RetCatNrp.*) THEN
      INITIALIZE lref_RetCatNrp.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   
   RETURN lc_error
   
END FUNCTION

#Objetivo: Elimina Registro
FUNCTION SqlEliMovimientoRetCatNrp(lref_RetCatNrp)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*
   
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM ret_cat_nrp",
                  " WHERE nrp = ? "
      PREPARE EnuEliretcatnrp FROM QryTxt
      EXECUTE EnuEliretcatnrp USING lref_RetCatNrp.nrp
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP
   
END FUNCTION


# Objetivo: Registra el cambio en la tabla historica
FUNCTION MInsertMovHistRetCatNrp(p_nrp, p_nombre_patron,p_modif)
DEFINE p_nrp           LIKE ret_cat_nrp.nrp,
       p_nombre_patron LIKE ret_cat_nrp.nombre_patron,
       p_modif         CHAR (1)
   
   WHENEVER ERROR CONTINUE
  	  INSERT INTO ret_cat_nrp_his 
                  (id_cat_nrp, nrp, nombre_patron, f_modif, tpo_modif, usuario)
           VALUES (seq_id_cat_nrp_his.nextval, p_nrp, p_nombre_patron, today, p_modif, g_usuario_cod);
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP
   
END FUNCTION

