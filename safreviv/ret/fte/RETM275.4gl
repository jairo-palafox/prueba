--===============================================================
-- VERSION: 1.0.0
-- FECHA ULTIMA MODIFICACION:
--===============================================================
#########################################################################################
#MODULO       => RET                                                                    #
#PROGRAMA     => RETM275                                                                #
#OBJETIVO     => Mantenimiento al catálogo de Patrones                                  #
#Fecha inicio => 07 Agosto 2014                                                         #
#Modificacion =>                                                                        #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
DEFINE marr_RetCatNrp DYNAMIC ARRAY OF RECORD LIKE  ret_cat_nrp.*
{
======================================================================
Clave: 
Nombre: main
Fecha creacion: 08 de julio de 2014
Autor: Ricardo Perez, EFP
Narrativa del proceso que realiza:
Le da mantenimiento al catálogo de patrones


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
MAIN 
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion    SMALLINT, -- forma como ejecutara el programa
       p_s_titulo          STRING,   -- titulo de la ventana
       v_c_nrp             LIKE ret_cat_nrp.nrp,
       v_c_nombre          LIKE ret_cat_nrp.nombre_patron,
       v_f_alta_ini        DATE,
       v_f_alta_fin        DATE,
       v_c_usuario         LIKE sfr_marca_historica.usuario_desmarca,
       v_i_indice          INTEGER, 
       v_s_qryTxt          STRING,

       v_arr_patrones    DYNAMIC ARRAY OF RECORD -- arreglo que contiene los detalles de los patrones
           patrones_nrp        LIKE ret_cat_nrp.nrp,
           patrones_nombre     LIKE ret_cat_nrp.nombre_patron,
           patrones_f_alta     LIKE sfr_marca_historica.f_inicio,
           patrones_usuario    LIKE sfr_marca_historica.usuario_marca
       END RECORD,


        f_ventana        ui.Window,   -- Define las propìedades de la Ventana
        f_forma          ui.Form     -- Define las propiedades de la forma
       
        DEFINE v_reporte         STRING
        DEFINE report_handler    om.SaxDocumentHandler -- Librería de Genero para la generación de reportes
        DEFINE v_excepcion       SMALLINT
        DEFINE i                 SMALLINT
       
   -- se obtienen los parametros de ejecucion
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".RETM275.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   CLOSE WINDOW SCREEN
   -- se abre la ventana de consulta
   OPEN WINDOW w_parametros WITH FORM "RETM2751"
      -- se capturan los datos de la consulta
        DIALOG ATTRIBUTE(UNBUFFERED)
            INPUT v_c_nrp,
                  v_c_nombre,
                  v_f_alta_ini,
                  v_f_alta_fin,
                  v_c_usuario
            FROM v_nrp,
                 v_nombre,
                 fecha_alta_ini,
                 fecha_alta_fin,
                 v_usuario
               
            BEFORE INPUT   
                LET f_ventana = ui.Window.getCurrent()
                LET f_forma = f_ventana.getForm()
                CALL f_forma.setElementHidden("Grp_Detalle", 1) --Oculta la Sección de Detalles
                CALL f_forma.setElementHidden("Grid3", 1) --Oculta la Sección de Detalles
                CALL f_forma.setElementHidden("tbl_det_marcas", 1) --Oculta la Sección de Detalles
              
            END INPUT    


          
            -- Botón cancel que da salida a la consulta y terminar la captura de los parámetros
            ON ACTION cancelar
                EXIT DIALOG 
            -- Botón aceptar que realiza la consulta en base a folio y fecha
            ON ACTION Alta -- Nuevo Registro
                CALL fAddRetCatNrp(p_usuario_cod)  -- Abre ventana para ingresar nuevo registro
               --CALL LLenaArryRetCatNrp(" 1=1") -- Actualiza el arreglo         
            ON ACTION ACCEPT  

                DISPLAY "Se buscara informacion con los criterios proporcionados"
 
                -- se inicializa el indice del arreglo
                --CALL f_forma.setElementHidden("Grp_detalle_archivo", 0) --Se muestra la Sección de Detalles
                CALL f_forma.setElementHidden("tbl_det_patrones", 0) --Se muestra la Sección de Detalles
                --DISPLAY ">>Se acepto el folio, inicia la consulta de la informacion para el lote " || v_folio_lote || "<<"
                LET v_i_indice = 1

                -- se crea la sentencia que busca los archivos disponibles por integrar

                LET v_s_qryTxt = "SELECT nrp, nombre_patron, ",
                                 "       f_alta, usuario ",
                                 "  FROM ret_cat_nrp ",
                                 " where 1 = 1 "
                IF v_c_nrp IS NOT NULL THEN 
                    
                    LET v_s_qryTxt = v_s_qryTxt.trim() || " AND nrp like '%" || v_c_nrp CLIPPED  || "%'"
                END IF 
                IF v_c_nombre IS NOT NULL THEN 
                    LET v_s_qryTxt = v_s_qryTxt.trim() || " AND nombre_patron like '%" || v_c_nombre CLIPPED || "%'"
                END IF 
                IF v_f_alta_ini IS NOT NULL AND v_f_alta_fin IS NOT NULL THEN 
                    LET v_s_qryTxt = v_s_qryTxt.trim() || " AND f_alta between '" || v_f_alta_ini || "' and '" || v_f_alta_fin || "'" 
                END IF 
                IF v_c_usuario IS NOT NULL THEN 
                    LET v_s_qryTxt = v_s_qryTxt.trim() || " AND usuario like '%" || v_c_usuario CLIPPED || "%'"
                END IF 

                DISPLAY "El query >" || v_s_qryTxt || "<"

                PREPARE prp_detalle_patrones FROM v_s_qryTxt
                DECLARE cur_detalle_patrones CURSOR FOR prp_detalle_patrones
                --DISPLAY v_s_qryTxt
                FOREACH cur_detalle_patrones INTO v_arr_patrones[v_i_indice].*
                    -- se incrementa el indice del arreglo
                    LET v_i_indice = v_i_indice + 1
                END FOREACH

                CALL v_arr_patrones.deleteElement(v_i_indice)
                IF v_i_indice = 1 THEN
                    DISPLAY "No existen registros con estos criterios de búsqueda"
                    CALL fn_mensaje(p_s_titulo,"No existen registros con estos criterios de búsqueda","about")
                    CALL f_forma.setElementHidden("tbl_det_patrones", 1) --Se oculta la Sección de Detalles por no existir informacion
                END IF 


            DISPLAY ARRAY v_arr_patrones TO tbl_det_patrones.* 
                BEFORE DISPLAY 
                CALL DIALOG.setActionHidden("accept",1)
                ON ACTION CANCEL
                   CALL v_arr_patrones.clear()
                   EXIT DISPLAY 
            END DISPLAY   
    END DIALOG 
END MAIN

# Objetivo: Función que llena pantalla principal 
FUNCTION LLenaArryRetCatNrp(ls_qry)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*,
       li_pos         INTEGER,
       ls_qry         STRING,
       QryTxt         STRING 

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
FUNCTION fAddRetCatNrp(p_usuario_cod)
DEFINE lref_RetCatNrp RECORD LIKE ret_cat_nrp.*,
       v_maxvalor INTEGER,
       v_n_cuantos SMALLINT ,
       p_usuario_cod LIKE seg_usuario.usuario
      
   LET INT_FLAG = FALSE
   
   OPEN WINDOW WAddRetCatNrp WITH FORM "RETM2752"

      INPUT lref_RetCatNrp.nrp, lref_RetCatNrp.nombre_patron WITHOUT DEFAULTS 
       FROM f_nrp, f_nombre_patron ATTRIBUTES (UNBUFFERED)
      
      BEFORE INPUT

         AFTER FIELD f_nombre_patron
            IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
               CALL fn_mensaje("ATENCIÓN","DEBE INGRESAR LA INFORMACIÓN COMPLETA",'about')
               NEXT FIELD f_nrp      
   		      END IF
       
      ON ACTION ACCEPT
         IF lref_RetCatNrp.nombre_patron IS NULL OR lref_RetCatNrp.nrp IS NULL THEN 
		     CALL fn_mensaje("ERROR","DEBE INGRESAR LA INFORMACIÓN COMPLETA",'about')
  			 NEXT FIELD f_nrp
         END IF
         IF length(lref_RetCatNrp.nrp) < 10 THEN 
		     CALL fn_mensaje("ERROR","EL NRP DEBE SER DE POR LO MENOS 10 CARACTERES",'about')
  			 NEXT FIELD f_nrp
         END IF
         
         LET lref_RetCatNrp.nrp = UPSHIFT(lref_RetCatNrp.nrp)
         LET lref_RetCatNrp.nombre_patron = UPSHIFT(lref_RetCatNrp.nombre_patron)
         LET lref_RetCatNrp.f_alta = TODAY 
         LET lref_RetCatNrp.usuario = p_usuario_cod

         SELECT count(*)
           INTO v_n_cuantos
           FROM ret_cat_nrp
          WHERE nrp = lref_RetCatNrp.nrp
         IF v_n_cuantos > 0 THEN 
             CALL fn_mensaje("ERROR","EL NRP: " || lref_RetCatNrp.nrp || " YA EXISTE, FAVOR DE VERIFICAR",'about')
             CONTINUE INPUT 
         ELSE  
             IF MAltMovimientoRetCatNrp(lref_RetCatNrp.*) THEN
                 INITIALIZE lref_RetCatNrp.* TO NULL
                 CLEAR FORM 
                 EXIT INPUT
             ELSE 
                 CONTINUE INPUT
             END IF
         END IF       
      ON KEY(INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT

      AFTER INPUT    
         CONTINUE INPUT
      END INPUT
   CLOSE WINDOW WAddRetCatNrp 

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
