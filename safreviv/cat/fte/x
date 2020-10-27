####################################################################
#Modulo            =>CAT                                           #
#Programa          =>CATM03                                        #
#Objetivo          =>Catálogo de preliquidaciones                  #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>21 Febrero 2012                               #
####################################################################
DATABASE safre_viv
-- Definicion de variables globales
GLOBALS
DEFINE marr_CatPre DYNAMIC ARRAY OF RECORD -- Arreglo principal de preliquidaciones
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
       END RECORD,
       QryTxt       STRING,                        -- Cadena para el armado de SQL's
       g_usuario_sys LIKE cat_preliquida.usuario,  -- Usuario de acceso al modulo
       g_tipo_carga  SMALLINT,                     -- Tipo de carga -- No funcional
       g_nom_prog    VARCHAR(30)                   -- Nombre del programa -- No funcional
DEFINE arr_seg_modulo   DYNAMIC ARRAY OF RECORD LIKE seg_modulo.* -- arreglo de modulos de referencia para el catálogo
DEFINE mref_CatPre      RECORD  -- Registro principal de preliquidaciones
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
       END RECORD
DEFINE v_r_seg_modulo   RECORD LIKE seg_modulo.*     -- Registro de modulo seleccionado
DEFINE g_r_opera        RECORD LIKE cat_operacion.*  -- Registro de operacion seleccionada
DEFINE g_r_proceso      RECORD LIKE cat_proceso.*    -- Registro de proceso seleccionado
END GLOBALS


MAIN
   DEFINE lc_error  CHAR(1), -- Regresa estatus de error 
          ls_qry    STRING,  -- Contiene resultado de Construct 
          lc_fin    STRING
   DEFINE v_contador       SMALLINT -- contador
   DEFINE r_seg_modulo     RECORD LIKE seg_modulo.* -- registro de la tabla seg_modulo

   -- Parametros de entrada al catalogo
   LET g_usuario_sys = ARG_VAL(1)
   LET g_tipo_carga = ARG_VAL(2)
   LET g_nom_prog = ARG_VAL(3)

   OPEN WINDOW vtn_usu_principal WITH FORM "CATM032"

   LET INT_FLAG = FALSE 

   -- se cargan los arreglos de los modulos de acuerdo al catalogo
   CALL fn_cargar_modulos()

   -- Manejo multiple de entradas en pantalla
   DIALOG ATTRIBUTES (UNBUFFERED)

      -- Despliegue del arreglo de modulos de acuerdo al catalogo del sistema
      DISPLAY ARRAY arr_seg_modulo TO tbl_seg_modulo.*
   
         BEFORE DISPLAY
            -- se muestran los datos del primer renglon (si es que se tiene)
            IF ( arr_seg_modulo.getLength() > 0 ) THEN
               LET r_seg_modulo.* = arr_seg_modulo[1].*
            END IF
   
         -- al cambiar el renglon se muestran los datos del modulo en cuestion
         BEFORE ROW
            -- Bloqueo de elementos que no intervienen para esta entrada de datos
            CALL DIALOG.setActionHidden("eliminar",TRUE)
            CALL DIALOG.setActionHidden("modificar",TRUE)
            
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()
         
            -- se muestran los datos del registro
            LET r_seg_modulo.* = arr_seg_modulo[v_contador].*
            CALL LLenaArryCatPre(" nombre_tabla matches '*"||r_seg_modulo.modulo_cod||"*'")

            -- Actualizacion de la pantalla del catalogo
            CALL ui.interface.refresh()
         
      END DISPLAY
   
      -- Despliegue del arreglo de preliquidaciones de acuerdo al catalogo del sistema
      DISPLAY ARRAY marr_CatPre TO tbl_preliquida.*
         -- se muestran los datos del primer renglon (si es que se tiene)
         BEFORE ROW
            -- Desbloqueo de elementos que intervienen para esta entrada de datos
            CALL DIALOG.setActionHidden("eliminar",FALSE)
            CALL DIALOG.setActionHidden("modificar",FALSE)
         
      END DISPLAY

      ON ACTION nuevo     -- Nuevo Registro
         -- Abre ventana para ingresar nuevo registro
         CALL fAddCatPre(r_seg_modulo.modulo_cod)   
         -- Actualiza el arreglo para ser visualizado
         CALL LLenaArryCatPre(" nombre_tabla matches '*"||r_seg_modulo.modulo_cod||"*'") 

      ON ACTION modificar -- Modificar Registro
         -- Recupera registro que sera modificado
         LET mref_CatPre.* = marr_CatPre[ARR_CURR()].*
         -- Abre ventana para modificar registro
         CALL MModMovimientoCatPre(marr_CatPre[ARR_CURR()].*) 
         -- Actualiza el arreglo para ser visualizado
         CALL LLenaArryCatPre(" nombre_tabla matches '*"||mref_CatPre.nombre_tabla[1,3]||"*'")

      ON ACTION eliminar -- Elimina Registro del CATÁLOGO
         IF(marr_CatPre.getLength() >=1)THEN 
            IF (ARR_CURR() <> 0)THEN
               IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                         "    Tabla: "||marr_CatPre[ARR_CURR()].nombre_tabla||"\n"||
                         "    Proceso: "||marr_CatPre[ARR_CURR()].proceso_cod||"\n"||
                         "    Operación: "||marr_CatPre[ARR_CURR()].opera_cod||"\n"||
                         "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                  DISPLAY marr_CatPre[ARR_CURR()].*
                  -- Rutina par eliminar registro
                  CALL MEliMovimientoCatPre(marr_CatPre[ARR_CURR()].*) RETURNING lc_error
                  IF (lc_error = 'S')THEN
                     LET INT_FLAG = TRUE
                     EXIT DIALOG
                  ELSE
                     -- Elimina registro del arreglo en pantalla
                     CALL marr_CatPre.deleteElement(ARR_CURR())
                     CALL FGL_WINMESSAGE("Atencion","Clave Eliminada con Exito","info")
                  END IF
               END IF
               LET INT_FLAG = FALSE
            END IF
         END IF
      
      ON ACTION cancelar
         EXIT DIALOG
   
   END DIALOG
      
   CLOSE WINDOW vtn_usu_principal

END MAIN

#############################################################################
# Funcion           => LLenaArryCatPre - Carga arreglo de preliquidaciones  #
#                      en función al modulo seleccionado                    #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => ls_qry - Cadena para el criterio de consulta         #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION LLenaArryCatPre(ls_qry)
DEFINE lref_CatPre RECORD 
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
       END RECORD,
       li_pos       INTEGER,
       ls_qry  STRING

   WHENEVER ERROR CONTINUE
      -- Rutina para vaciar el arreglo de preliquidaciones
      CALL marr_CatPre.CLEAR()
      LET QryTxT = " SELECT p.proceso_cod , proceso_desc, p.opera_cod   ,",
                          " opera_desc, p.nombre_tabla, p.f_actualiza ,p.usuario",
                   " FROM   cat_preliquida p, \n",
                           "cat_proceso    cp,\n",
                           "cat_operacion  co\n",
                   " WHERE ",  ls_qry CLIPPED,
                   "   AND p.proceso_cod = cp.proceso_cod",
                   "   AND p.proceso_cod = co.proceso_cod",
                   "   AND p.opera_cod   = co.opera_cod",
                   " ORDER BY proceso_cod, opera_cod " 

      PREPARE Prp_ObtRegCatPre FROM QryTxT CLIPPED

      LET li_pos = 0
      -- Carga de preliquidaciones localizadas en base de datos
      DECLARE Crs_ObtRegCatPre CURSOR FOR Prp_ObtRegCatPre
         FOREACH Crs_ObtRegCatPre INTO lref_CatPre.*
            LET li_pos = li_pos + 1
            LET marr_CatPre[li_pos].* = lref_CatPre.* 
         END FOREACH
    
      IF(SQLCA.SQLCODE < 0)THEN
         CALL fn_mensaje("ATENCION",
                         "OCURRIO UN ERROR AL CARGAR LOS DATOS DE PRELIQUIDACIONES "||
                         "\n ERROR :"||SQLCA.SQLCODE, "about")
         LET INT_FLAG = TRUE
      END IF  
      FREE Prp_ObtRegCatPre
    
      IF li_pos  = 0  THEN 
      ELSE
         -- Condicion para eliminar registros nulos
         IF(marr_CatPre[marr_CatPre.getLength()].proceso_cod IS NULL OR 
            marr_CatPre[marr_CatPre.getLength()].opera_cod IS NULL)THEN
            CALL marr_CatPre.deleteElement(marr_CatPre.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => fAddCatPre - Alta de registros de preliquidaciones   #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => p_modulo_cod - Modulo seleccionado para el alta de   #
#                      procesos y operaciones                               #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION fAddCatPre(p_modulo_cod)
DEFINE lref_CatPre   RECORD LIKE cat_preliquida.*,
       li_existe     INTEGER
DEFINE p_modulo_cod  LIKE seg_modulo.modulo_cod
      
   LET INT_FLAG = FALSE

   OPEN WINDOW WAddCatPre WITH FORM "CATM031"
      
   MENU ""
      BEFORE MENU
         LET lref_CatPre.nombre_tabla = p_modulo_cod, "_preliquida"
         DISPLAY BY NAME lref_CatPre.nombre_tabla                               
         
      ON ACTION ACCEPT
         IF lref_CatPre.proceso_cod IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA EL PROCESO",'about')
  			    CONTINUE MENU
         END IF

         IF lref_CatPre.opera_cod IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA LA OPERACIÓN",'about')
  			    CONTINUE MENU
         END IF
         
         IF lref_CatPre.proceso_cod = 0 AND lref_CatPre.opera_cod = 0 THEN 
		        CALL fn_mensaje("ERROR","CLAVES INVALIDAS PARA PROCESO Y OPERACION",'about')
  			    CONTINUE MENU
         END IF 
         
         IF lref_CatPre.nombre_tabla IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA LA TABLA",'about')
  			    CONTINUE MENU
         END IF

         SELECT COUNT (*)
           INTO li_existe
         FROM cat_preliquida 
         WHERE proceso_cod  = lref_CatPre.proceso_cod 
           AND opera_cod    = lref_CatPre.opera_cod 
           AND nombre_tabla = lref_CatPre.nombre_tabla 
         
         DISPLAY "lref_CatPre.proceso_cod  - ", lref_CatPre.proceso_cod 
         DISPLAY "lref_CatPre.opera_cod    - ", lref_CatPre.opera_cod   
         DISPLAY "lref_CatPre.nombre_tabla - ", lref_CatPre.nombre_tabla
         DISPLAY "li_existe - ",li_existe
         
         -- Validación de duplicados de preliquidaciones
         IF li_existe > 0 THEN
  		      CALL fn_mensaje ("ATENCIÓN", "LA PRELIQUIDACION YA EXISTE", "about")
  		      CONTINUE MENU
         END IF
         
         -- Rutina para insertar registro capturado de preliquidaciones
         IF NOT MAltMovimientoCatPre(lref_CatPre.*) THEN
            CONTINUE MENU
         ELSE
            EXIT MENU
         END IF

      ON ACTION btn_opera
         -- Rutina para consulta de procesos y operaciones asociados al modulo
         CALL fn_mantenimiento_cat_proceso(lref_CatPre.nombre_tabla[1,3],g_usuario_sys) RETURNING  lref_CatPre.proceso_cod, lref_CatPre.opera_cod
         DISPLAY BY NAME lref_CatPre.proceso_cod, lref_CatPre.opera_cod
         -- Recuperacion y despliegue de descripciones de procesos y operaciones
         CALL fn_rec_proceso(lref_CatPre.nombre_tabla[1,3],lref_CatPre.proceso_cod) RETURNING g_r_proceso.*
         DISPLAY BY NAME g_r_proceso.proceso_desc
         CALL fn_rec_opera(lref_CatPre.proceso_cod, lref_CatPre.opera_cod) RETURNING g_r_opera.*
         DISPLAY BY NAME g_r_opera.opera_desc
         
         CONTINUE MENU
      
      ON ACTION cancelar
         EXIT MENU
         
   END MENU
   CLOSE WINDOW WAddCatPre 

END FUNCTION

#############################################################################
# Funcion           => MAltMovimientoCatPre - Rutina para almacenar en BD   #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION MAltMovimientoCatPre(lref_CatPre)
DEFINE lref_CatPre RECORD LIKE cat_preliquida.*

   -- Rutina para insertar en base de datos la preliquidacion
   IF SqlInsMovimientoCatPre(lref_CatPre.*) THEN
      CALL FGL_WINMESSAGE("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#############################################################################
# Funcion           => SqlInsMovimientoCatPre - Insert del movimietno       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION SqlInsMovimientoCatPre(lref_CatPre)
DEFINE lref_CatPre RECORD LIKE cat_preliquida.*

   WHENEVER ERROR CONTINUE
      INSERT INTO cat_preliquida VALUES (lref_CatPre.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => MModMovimientoCatPre - Modificacion de registros de  #
#                      preliquidaciones                                     #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro a ser modificado              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION MModMovimientoCatPre(lref_CatPre)
DEFINE lref_CatPre RECORD 
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
       END RECORD
DEFINE li_existe   INTEGER

   OPEN WINDOW WModCatPre WITH FORM "CATM031"
      
      MENU ""
         BEFORE MENU
            DISPLAY BY NAME lref_CatPre.proceso_cod, 
                            lref_CatPre.opera_cod, 
                            lref_CatPre.nombre_tabla

            -- Recuperacion y despliegue de descripciones de procesos y operaciones
            CALL fn_rec_proceso(lref_CatPre.nombre_tabla[1,3],lref_CatPre.proceso_cod) RETURNING g_r_proceso.*
            DISPLAY BY NAME g_r_proceso.proceso_desc
            CALL fn_rec_opera(lref_CatPre.proceso_cod, lref_CatPre.opera_cod) RETURNING g_r_opera.*
            DISPLAY BY NAME g_r_opera.opera_desc

      ON ACTION ACCEPT
         IF lref_CatPre.proceso_cod IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA EL PROCESO",'about')
  			    CONTINUE MENU
         END IF

         IF lref_CatPre.opera_cod IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA LA OPERACIÓN",'about')
  			    CONTINUE MENU
         END IF

         IF lref_CatPre.nombre_tabla IS NULL THEN 
		        CALL fn_mensaje("ERROR","INTRODUZCA LA TABLA",'about')
  			    CONTINUE MENU
         END IF

         SELECT COUNT (*)
           INTO li_existe
         FROM cat_preliquida 
         WHERE proceso_cod  = lref_CatPre.proceso_cod 
           AND opera_cod    = lref_CatPre.opera_cod 
           AND nombre_tabla = lref_CatPre.nombre_tabla 

         -- Validación de duplicados de preliquidaciones
         IF li_existe > 0 THEN
            CALL fn_mensaje ("ATENCIÓN", "LA PRELIQUIDACION YA EXISTE", "about")
            CONTINUE MENU
         END IF

         -- Rutina para actualizar el registro de preliquidaciones
         IF SqlModMovimientoCatUsr(lref_CatPre.*) THEN 
            CALL FGL_WINMESSAGE("Registro Procesado",
                                "Registro Modificado Satisfactorimente",'about')
            INITIALIZE lref_CatPre.* TO NULL
            CLEAR FORM
            EXIT MENU
         END IF

      ON ACTION btn_opera
         -- Rutina para consulta de procesos y operaciones asociados al modulo
         CALL fn_mantenimiento_cat_proceso(lref_CatPre.nombre_tabla[1,3],g_usuario_sys) RETURNING  lref_CatPre.proceso_cod, lref_CatPre.opera_cod
         DISPLAY BY NAME lref_CatPre.proceso_cod, lref_CatPre.opera_cod
         -- Recuperacion y despliegue de descripciones de procesos y operaciones
         CALL fn_rec_proceso(lref_CatPre.nombre_tabla[1,3],lref_CatPre.proceso_cod) RETURNING g_r_proceso.*
         DISPLAY BY NAME g_r_proceso.proceso_desc
         CALL fn_rec_opera(lref_CatPre.proceso_cod, lref_CatPre.opera_cod) RETURNING g_r_opera.*
         DISPLAY BY NAME g_r_opera.opera_desc
         CONTINUE MENU
      
      ON ACTION cancelar
         EXIT MENU
      
      END MENU
   CLOSE WINDOW WModCatPre 
  
END FUNCTION

#############################################################################
# Funcion           => SqlInsMovimientoCatPre - Insert del movimietno       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a modificar    #
# Salida:           => True si fue actualizado, falso si hubo error         #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION SqlModMovimientoCatUsr(lref_CatPre)
DEFINE  lref_CatPre RECORD 
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
        END RECORD
   LET lref_CatPre.f_actualiza = TODAY
   LET lref_CatPre.usuario = g_usuario_sys

   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE cat_preliquida SET ",
                  "\n proceso_cod  = ",lref_CatPre.proceso_cod,",", 
                  "\n opera_cod    = ",lref_CatPre.opera_cod,",", 
                  "\n nombre_tabla = '",lref_CatPre.nombre_tabla CLIPPED,"',", 
                  "\n f_actualiza  = '",lref_CatPre.f_actualiza,"',", 
                  "\n usuario      = '",lref_CatPre.usuario CLIPPED,"'", 
                  "\n WHERE proceso_cod  = ", mref_CatPre.proceso_cod  ,
                  "\n   AND opera_cod    = ", mref_CatPre.opera_cod    ,
                  "\n   AND nombre_tabla = '", mref_CatPre.nombre_tabla CLIPPED, "'"

      PREPARE EnuModpreli FROM QryTxt  
      EXECUTE EnuModpreli 
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => fConPreliquida - Rutina que arma las condiciones de  #
#                      consulta de preliquidaciones                         #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => ls_qry - Cadena SQL de condiciones para la consulta  #
#                      de preliquidaciones                                  #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION fConPreliquida()
   DEFINE ls_qry STRING

   LET INT_FLAG = FALSE
   LET ls_qry = " 1=1"    

   OPEN WINDOW w_entQry WITH FORM "CATM031"

   CALL FGL_SETTITLE("CATÁLOGO DE PRELIQUIDACIONES")

   -- Generador de condiciones para la consulta de preliquidaciones
   CONSTRUCT ls_qry ON proceso_cod,opera_cod,nombre_tabla
        FROM proceso_cod,opera_cod,nombre_tabla
      

   IF LENGTH(ls_qry CLIPPED) = 0 THEN
      LET ls_qry = " 1=1 "
   END IF

   CLOSE WINDOW w_entQry 
   RETURN ls_qry

END FUNCTION

#############################################################################
# Funcion           => MEliMovimientoCatPre - Rutina para eliminar en BD    #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a eliminar     #
# Salida:           => 'S' si fue guardado, 'N' si hubo error                #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION MEliMovimientoCatPre(lref_CatPre) 
   DEFINE lref_CatPre RECORD
             proceso_cod   LIKE cat_preliquida.proceso_cod ,
             proceso_desc  LIKE cat_proceso.proceso_desc,
             opera_cod     LIKE cat_preliquida.opera_cod   ,
             opera_desc    LIKE cat_operacion.opera_desc  ,
             nombre_tabla  LIKE cat_preliquida.nombre_tabla,
             f_actualiza   LIKE cat_preliquida.f_actualiza ,
             usuario       LIKE cat_preliquida.usuario     
          END RECORD,
          lc_error     CHAR(1)
   
   -- Rutina para eliminar preliquidaciones seleccionadas
   IF SqlEliMovimientoCatPre(lref_CatPre.*) THEN
      INITIALIZE lref_CatPre.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   RETURN lc_error

END FUNCTION

#############################################################################
# Funcion           => SqlEliMovimientoCatPre - Delete del movimietno      #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION SqlEliMovimientoCatPre(lref_CatPre)
DEFINE lref_CatPre RECORD
          proceso_cod   LIKE cat_preliquida.proceso_cod ,
          proceso_desc  LIKE cat_proceso.proceso_desc,
          opera_cod     LIKE cat_preliquida.opera_cod   ,
          opera_desc    LIKE cat_operacion.opera_desc  ,
          nombre_tabla  LIKE cat_preliquida.nombre_tabla,
          f_actualiza   LIKE cat_preliquida.f_actualiza ,
          usuario       LIKE cat_preliquida.usuario     
       END RECORD     
       
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM cat_preliquida",
                  "\n WHERE proceso_cod  = ", lref_CatPre.proceso_cod  ,
                  "\n   AND opera_cod    = ", lref_CatPre.opera_cod    ,
                  "\n   AND nombre_tabla = '", lref_CatPre.nombre_tabla CLIPPED,"'"
      
      PREPARE EnuEliprestamo FROM QryTxt
      EXECUTE EnuEliprestamo 
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => fn_consulta_seg_modulo - Rutina para consulta de mo- #
#                      dulos (Ya no es funcional, opera didrente)           #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatPre - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
{FUNCTION fn_consulta_seg_modulo()
   DEFINE r_seg_modulo     RECORD LIKE seg_modulo.*, -- registro de la tabla seg_modulo
          v_contador       SMALLINT -- contador
   
   -- se cargan los modulos
   CALL fn_cargar_modulos()
          
   -- se abre la ventana que muestra el contenido del catalogo
   OPEN WINDOW w_selmod WITH FORM "CATM033"

      DISPLAY ARRAY arr_seg_modulo TO tbl_seg_modulo.*
      ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY
            -- se muestran los datos del primer renglon (si es que se tiene)
            IF ( arr_seg_modulo.getLength() > 0 ) THEN
               LET r_seg_modulo.* = arr_seg_modulo[1].*
               DISPLAY BY NAME r_seg_modulo.*
            END IF

         -- al cambiar el renglon se muestran los datos del modulo en cuestion
         BEFORE ROW
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_seg_modulo.* = arr_seg_modulo[v_contador].*
            DISPLAY BY NAME r_seg_modulo.*
            CALL ui.interface.refresh()
         
            
         ON ACTION ACCEPT
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()

            -- se muestran los datos del registro
            LET r_seg_modulo.* = arr_seg_modulo[v_contador].*
            EXIT DISPLAY

         ON ACTION CANCEL
            EXIT DISPLAY

      END DISPLAY
            
   CLOSE WINDOW w_selmod
   
   RETURN r_seg_modulo.*
   
END FUNCTION
}

#############################################################################
# Funcion           => fn_cargar_modulos - Rutina para cargar los arreglos  #
#                      de los modulos                                       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION fn_cargar_modulos()
   DEFINE v_contador    SMALLINT, -- contador de registros
          r_seg_modulo  RECORD LIKE seg_modulo.*

   -- se limpia el arreglo de modulos
   CALL arr_seg_modulo.clear()
   
   -- se leen los registros de la tabla seg_modulo
   DECLARE cur_segmodulo CURSOR FOR SELECT *
                                      FROM seg_modulo
                                     ORDER BY modulo_cod

   LET v_contador = 1

   -- Carga del arreglo de modulos de la base de datos
   FOREACH cur_segmodulo INTO r_seg_modulo.*
      -- se transfieren los datos al arreglo de despliegue
      LET arr_seg_modulo[v_contador].* = r_seg_modulo.*
      LET v_contador = v_contador + 1
   END FOREACH 
END FUNCTION

#############################################################################
# Funcion           => fn_rec_proceso - Rutina para recuperar el registro   #
#                      del proceso seleccionado                             #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => p_modulo - Clave del modulo del proceso a recuperar  #
#                      p_proceso - Clave del proceso a recuperar            #
# Salida:           => v_r_proceso - registro del proceso recuperado        #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION fn_rec_proceso(p_modulo, p_proceso)
   DEFINE p_modulo    LIKE cat_proceso.modulo_cod
   DEFINE p_proceso   LIKE cat_proceso.proceso_cod
   DEFINE v_r_proceso RECORD LIKE cat_proceso.*
   
   SELECT * INTO v_r_proceso.*
     FROM cat_proceso
    WHERE proceso_cod = p_proceso
      AND modulo_cod = p_modulo
    
  RETURN v_r_proceso.*
  
END FUNCTION

#############################################################################
# Funcion           => fn_rec_opera - Rutina para recuperar el registro     #
#                      de la operacion seleccionada                         #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => p_proceso - Clave del proceso de la operacion a recup#
#                      p_opera - Clave de la operacion o a recuperar        #
# Salida:           => v_r_opera - registro de la operacion recuperada      #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 21 Febrero 2012                                      #
#############################################################################
FUNCTION fn_rec_opera(p_proceso, p_opera)
   DEFINE p_proceso   LIKE cat_operacion.proceso_cod
   DEFINE p_opera     LIKE cat_operacion.opera_cod
   DEFINE v_r_opera   RECORD LIKE cat_operacion.*
   
   SELECT * INTO v_r_opera.*
     FROM cat_operacion
    WHERE proceso_cod = p_proceso
      AND opera_cod = p_opera
    
  RETURN v_r_opera.*
  
END FUNCTION