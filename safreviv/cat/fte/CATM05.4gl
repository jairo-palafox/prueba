####################################################################
#Modulo            =>CAT                                           #
#Programa          =>CATM05                                        #
#Objetivo          =>Catálogo de reversos                          #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>28 Febrero 2012                               #
####################################################################
DATABASE safre_viv
-- Definicion de variables globales
GLOBALS
DEFINE marr_CatReverso DYNAMIC ARRAY OF RECORD LIKE cat_tabla_reverso.*,-- Arreglo principal de reversos
       QryTxt       STRING,                        -- Cadena para el armado de SQL's
       g_usuario_sys LIKE cat_proceso.usuario,  -- Usuario de acceso al modulo
       g_tipo_carga  SMALLINT,                     -- Tipo de carga -- No funcional
       g_nom_prog    VARCHAR(30)                   -- Nombre del programa -- No funcional
DEFINE arr_cat_proceso   DYNAMIC ARRAY OF RECORD 
          proceso_cod_m  LIKE cat_proceso.proceso_cod ,
          modulo_cod     LIKE cat_proceso.modulo_cod  , 
          proceso_desc   LIKE cat_proceso.proceso_desc, 
          proceso_ext    LIKE cat_proceso.proceso_ext , 
          f_actualiza_m  LIKE cat_proceso.f_actualiza , 
          usuario_m      LIKE cat_proceso.usuario       -- arreglo de modulos de referencia para el catálogo
       END RECORD
DEFINE mref_CatReverso      RECORD LIKE cat_tabla_reverso.* -- Registro principal de reversos
DEFINE v_r_cat_proceso  RECORD LIKE cat_proceso.*     -- Registro de modulo seleccionado
DEFINE g_r_proceso      RECORD LIKE cat_proceso.*    -- Registro de proceso seleccionado
END GLOBALS


MAIN
   DEFINE lc_error  CHAR(1), -- Regresa estatus de error 
          ls_qry    STRING,  -- Contiene resultado de Construct 
          lc_fin    STRING
   DEFINE v_contador       SMALLINT -- contador
   DEFINE r_cat_proceso    RECORD LIKE cat_proceso.* -- registro de la tabla cat_proceso

   -- Parametros de entrada al catalogo
   LET g_usuario_sys = ARG_VAL(1)
   LET g_tipo_carga = ARG_VAL(2)
   LET g_nom_prog = ARG_VAL(3)

   OPEN WINDOW vtn_usu_principal WITH FORM "CATM052"

   LET INT_FLAG = FALSE 

   -- se cargan los arreglos de los modulos de acuerdo al catalogo
   CALL fn_cargar_modulos()

   -- Manejo multiple de entradas en pantalla
   DIALOG ATTRIBUTES (UNBUFFERED)

      -- Despliegue del arreglo de modulos de acuerdo al catalogo del sistema
      DISPLAY ARRAY arr_cat_proceso TO tbl_proceso.*
   
         BEFORE DISPLAY
            -- se muestran los datos del primer renglon (si es que se tiene)
            IF ( arr_cat_proceso.getLength() > 0 ) THEN
               LET r_cat_proceso.* = arr_cat_proceso[1].*
            END IF
   
         -- al cambiar el renglon se muestran los datos del modulo en cuestion
         BEFORE ROW
            -- Bloqueo de elementos que no intervienen para esta entrada de datos
            CALL DIALOG.setActionHidden("eliminar",TRUE)
            CALL DIALOG.setActionHidden("modificar",TRUE)
            
            -- se obtiene el indice del renglon en cuestion
            LET v_contador = ARR_CURR()
         
            -- se muestran los datos del registro
            LET r_cat_proceso.* = arr_cat_proceso[v_contador].*
            CALL LLenaArryCatReverso(" proceso_cod = "|| r_cat_proceso.proceso_cod)

            -- Actualizacion de la pantalla del catalogo
            CALL ui.interface.refresh()
         
      END DISPLAY
   
      -- Despliegue del arreglo de reversos de acuerdo al catalogo del sistema
      DISPLAY ARRAY marr_CatReverso TO tbl_reverso.*
         -- se muestran los datos del primer renglon (si es que se tiene)
         BEFORE ROW
            -- Desbloqueo de elementos que intervienen para esta entrada de datos
            CALL DIALOG.setActionHidden("eliminar",FALSE)
            CALL DIALOG.setActionHidden("modificar",FALSE)
         
      END DISPLAY

      ON ACTION nuevo     -- Nuevo Registro
         -- Abre ventana para ingresar nuevo registro
         CALL fAddCatRev(r_cat_proceso.proceso_cod)   
         -- Actualiza el arreglo para ser visualizado
         CALL LLenaArryCatReverso(" proceso_cod = "|| r_cat_proceso.proceso_cod)

      ON ACTION modificar -- Modificar Registro
         -- Recupera registro que sera modificado
         LET mref_CatReverso.* = marr_CatReverso[ARR_CURR()].*
         -- Abre ventana para modificar registro
         CALL MModMovimientoCatRev(marr_CatReverso[ARR_CURR()].*) 
         -- Actualiza el arreglo para ser visualizado
         CALL LLenaArryCatReverso(" proceso_cod = "|| r_cat_proceso.proceso_cod)

      ON ACTION eliminar -- Elimina Registro del CATÁLOGO
         IF(marr_CatReverso.getLength() >=1)THEN 
            IF (ARR_CURR() <> 0)THEN
               IF(FGL_WINBUTTON( "Eliminar Registro", "Registro: \n\n"||
                         "    Tabla: "||marr_CatReverso[ARR_CURR()].nombre_tabla||"\n"||
                         "    Proceso: "||marr_CatReverso[ARR_CURR()].proceso_cod||"\n"||
                         "¿Desea eliminar el registro?", "No", "Si|No", "question", 0)="Si")THEN
                  DISPLAY marr_CatReverso[ARR_CURR()].*
                  -- Rutina par eliminar registro
                  CALL MEliMovimientoCatRev(marr_CatReverso[ARR_CURR()].*) RETURNING lc_error
                  IF (lc_error = 'S')THEN
                     LET INT_FLAG = TRUE
                     EXIT DIALOG
                  ELSE
                     -- Elimina registro del arreglo en pantalla
                     CALL marr_CatReverso.deleteElement(ARR_CURR())
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
# Funcion           => LLenaArryCatReverso - Carga arreglo de reversos      #
#                      en función al proceso seleccioando                   #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => ls_qry - Cadena para el criterio de consulta         #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION LLenaArryCatReverso(ls_qry)
DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*,
       li_pos       INTEGER,
       ls_qry  STRING

   WHENEVER ERROR CONTINUE
      -- Rutina para vaciar el arreglo de reversos
      CALL marr_CatReverso.CLEAR()
      LET QryTxT = " SELECT *",
                   " FROM   cat_tabla_reverso\n",
                   " WHERE ",  ls_qry CLIPPED,
                   " ORDER BY proceso_cod" 
      
      PREPARE Prp_ObtRegCatRev FROM QryTxT CLIPPED

      LET li_pos = 0
      -- Carga de reversos localizadas en base de datos
      DECLARE Crs_ObtRegCatRev CURSOR FOR Prp_ObtRegCatRev
         FOREACH Crs_ObtRegCatRev INTO lref_CatReverso.*
            LET li_pos = li_pos + 1
            LET marr_CatReverso[li_pos].* = lref_CatReverso.* 
         END FOREACH
    
      IF(SQLCA.SQLCODE < 0)THEN
         CALL fn_mensaje("ATENCION",
                         "OCURRIO UN ERROR AL CARGAR LOS DATOS DE REVERSOS "||
                         "\n ERROR :"||SQLCA.SQLCODE, "about")
         LET INT_FLAG = TRUE
      END IF  
      FREE Prp_ObtRegCatRev
    
      IF li_pos  = 0  THEN 
      ELSE
         -- Condicion para eliminar registros nulos
         IF(marr_CatReverso[marr_CatReverso.getLength()].proceso_cod IS NULL OR 
            marr_CatReverso[marr_CatReverso.getLength()].tabla_id IS NULL)THEN
            CALL marr_CatReverso.deleteElement(marr_CatReverso.getLength())
         END IF
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => fAddCatRev - Alta de registros de reversos           #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => p_proceso_cod - Procesp seleccionado para el alta de #
#                      reversos                                             #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION fAddCatRev(p_proceso_cod)
DEFINE lref_CatReverso   RECORD LIKE cat_tabla_reverso.*,
       li_existe     INTEGER
DEFINE p_proceso_cod LIKE cat_proceso.proceso_cod
      
   LET INT_FLAG = FALSE

   OPEN WINDOW WAddCatPre WITH FORM "CATM051"
      
   INPUT BY NAME lref_CatReverso.nombre_tabla WITHOUT DEFAULTS
      ATTRIBUTES (CANCEL = FALSE)
      BEFORE INPUT
         
         LET lref_CatReverso.proceso_cod = p_proceso_cod

         SELECT NVL(MAX(tabla_id),0)+1 INTO lref_CatReverso.tabla_id
           FROM cat_tabla_reverso
          WHERE proceso_cod = lref_CatReverso.proceso_cod
         
         DISPLAY BY NAME lref_CatReverso.proceso_cod
         DISPLAY BY NAME lref_CatReverso.tabla_id

         -- Recuperacion y despliegue de descripciones de procesos y operaciones
         CALL fn_rec_proceso(lref_CatReverso.proceso_cod) RETURNING g_r_proceso.*
         DISPLAY BY NAME g_r_proceso.proceso_desc

      AFTER INPUT 
         IF NOT INT_FLAG THEN
            IF lref_CatReverso.proceso_cod IS NULL THEN 
		           CALL fn_mensaje("ERROR","INTRODUZCA EL PROCESO",'about')
  			       CONTINUE INPUT
            END IF
            
            IF lref_CatReverso.nombre_tabla IS NULL THEN 
		           CALL fn_mensaje("ERROR","INTRODUZCA LA TABLA",'about')
  			       CONTINUE INPUT
            END IF
            
            SELECT COUNT (*)
              INTO li_existe
            FROM cat_tabla_reverso 
            WHERE proceso_cod  = lref_CatReverso.proceso_cod 
              AND nombre_tabla = lref_CatReverso.nombre_tabla 
            
            DISPLAY "lref_CatReverso.proceso_cod  - ", lref_CatReverso.proceso_cod 
            DISPLAY "lref_CatReverso.nombre_tabla - ", lref_CatReverso.nombre_tabla
            DISPLAY "li_existe - ",li_existe
            
            -- Validación de duplicados de reversos
            IF li_existe > 0 THEN
  		         CALL fn_mensaje ("ATENCIÓN", "EL REVERSO YA EXISTE", "about")
  		         CONTINUE INPUT
            END IF
            
            -- Rutina para insertar registro capturado de reversos
            IF NOT MAltMovimientoCatRev(lref_CatReverso.*) THEN
               CONTINUE INPUT
            ELSE
               EXIT INPUT
            END IF
         END IF

      ON ACTION cancelar
         EXIT INPUT
         
   END INPUT
   CLOSE WINDOW WAddCatPre 

END FUNCTION

#############################################################################
# Funcion           => MAltMovimientoCatRev - Rutina para almacenar en BD   #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro del movimiento a insertar #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION MAltMovimientoCatRev(lref_CatReverso)
DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*

   -- Rutina para insertar en base de datos el reverso
   IF SqlInsMovimientoCatRev(lref_CatReverso.*) THEN
      CALL FGL_WINMESSAGE("Registro Procesado","Registro Ingresado Satisfactorimente",'info')
      RETURN TRUE
   END IF
   RETURN FALSE

END FUNCTION

#############################################################################
# Funcion           => SqlInsMovimientoCatRev - Insert del movimietno       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION SqlInsMovimientoCatRev(lref_CatReverso)
DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*

   WHENEVER ERROR CONTINUE
      LET lref_CatReverso.usuario = g_usuario_sys
      LET lref_CatReverso.f_actualiza = TODAY
      
      SELECT NVL(MAX(tabla_id),0)+1 INTO lref_CatReverso.tabla_id
        FROM cat_tabla_reverso
       WHERE proceso_cod = lref_CatReverso.proceso_cod
       
      INSERT INTO cat_tabla_reverso VALUES (lref_CatReverso.*)
         IF SQLCA.SQLCODE <> 0 THEN
            RETURN FALSE
         ELSE
            RETURN TRUE
         END IF
  WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => MModMovimientoCatRev - Modificacion de registros de  #
#                      reversos                                             #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro a ser modificado          #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION MModMovimientoCatRev(lref_CatReverso)
DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*
DEFINE li_existe   INTEGER

   OPEN WINDOW WModCatRev WITH FORM "CATM051"
      
   INPUT BY NAME lref_CatReverso.nombre_tabla WITHOUT DEFAULTS
      ATTRIBUTES (CANCEL = FALSE)
      BEFORE INPUT
         DISPLAY BY NAME lref_CatReverso.proceso_cod, 
                         lref_CatReverso.tabla_id

         -- Recuperacion y despliegue de descripciones de procesos y operaciones
         CALL fn_rec_proceso(lref_CatReverso.proceso_cod) RETURNING g_r_proceso.*
         DISPLAY BY NAME g_r_proceso.proceso_desc

      AFTER INPUT
         IF NOT INT_FLAG THEN
            IF lref_CatReverso.proceso_cod IS NULL THEN 
		           CALL fn_mensaje("ERROR","INTRODUZCA EL PROCESO",'about')
  			       CONTINUE INPUT
            END IF
            
            IF lref_CatReverso.nombre_tabla IS NULL THEN 
		           CALL fn_mensaje("ERROR","INTRODUZCA LA TABLA",'about')
  			       CONTINUE INPUT
            END IF
            
            SELECT COUNT (*)
              INTO li_existe
            FROM cat_tabla_reverso 
            WHERE proceso_cod  = lref_CatReverso.proceso_cod 
              AND nombre_tabla = lref_CatReverso.nombre_tabla 
            
            -- Validación de duplicados de reversos
            IF li_existe > 0 THEN
               CALL fn_mensaje ("ATENCIÓN", "EL REVERSO YA EXISTE", "about")
               CONTINUE INPUT
            END IF
            
            -- Rutina para actualizar el registro de reversos
            IF SqlModMovimientoCatRev(lref_CatReverso.*) THEN 
               CALL FGL_WINMESSAGE("Registro Procesado",
                                   "Registro Modificado Satisfactorimente",'about')
               INITIALIZE lref_CatReverso.* TO NULL
               CLEAR FORM
               EXIT INPUT
            END IF
         END IF

      ON ACTION cancelar
         EXIT INPUT
      
   END INPUT
   CLOSE WINDOW WModCatRev 
  
END FUNCTION

#############################################################################
# Funcion           => SqlModMovimientoCatRev - Insert del movimietno       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro del movimiento a modificar    #
# Salida:           => True si fue actualizado, falso si hubo error         #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION SqlModMovimientoCatRev(lref_CatReverso)
DEFINE  lref_CatReverso RECORD LIKE cat_tabla_reverso.*

   LET lref_CatReverso.f_actualiza = TODAY
   LET lref_CatReverso.usuario = g_usuario_sys

   WHENEVER ERROR CONTINUE
      LET QryTxt ="\n UPDATE cat_tabla_reverso SET ",
                  "\n nombre_tabla = '",lref_CatReverso.nombre_tabla CLIPPED,"',", 
                  "\n f_actualiza  = '",lref_CatReverso.f_actualiza,"',", 
                  "\n usuario      = '",lref_CatReverso.usuario CLIPPED,"'", 
                  "\n WHERE proceso_cod  = ", mref_CatReverso.proceso_cod  ,
                  "\n   AND tabla_id = ", mref_CatReverso.tabla_id CLIPPED

      PREPARE EnuModRever FROM QryTxt  
      EXECUTE EnuModRever 
      IF SQLCA.SQLCODE<>0 THEN
         DISPLAY "ERROR"
         RETURN FALSE    
      ELSE
         RETURN TRUE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => MEliMovimientoCatRev - Rutina para eliminar en BD    #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro del movimiento a eliminar #
# Salida:           => 'S' si fue guardado, 'N' si hubo error               #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION MEliMovimientoCatRev(lref_CatReverso) 
   DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*,
          lc_error     CHAR(1)
   
   -- Rutina para eliminar reversos seleccionados
   IF SqlEliMovimientoCatRev(lref_CatReverso.*) THEN
      INITIALIZE lref_CatReverso.* TO NULL
      LET lc_error  = 'S'
   ELSE
      LET  lc_error = 'N'
   END IF
   RETURN lc_error

END FUNCTION

#############################################################################
# Funcion           => SqlEliMovimientoCatRev - Delete del movimietno      #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => lref_CatReverso - Registro del movimiento a insertar     #
# Salida:           => True si fue guardado, falso si hubo error            #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION SqlEliMovimientoCatRev(lref_CatReverso)
DEFINE lref_CatReverso RECORD LIKE cat_tabla_reverso.*
       
   WHENEVER ERROR CONTINUE
      LET QryTxt = "DELETE FROM cat_tabla_reverso",
                  "\n WHERE proceso_cod  = ", lref_CatReverso.proceso_cod  ,
                  "\n   AND tabla_id = ", lref_CatReverso.tabla_id
      
      PREPARE EnuEliReverso FROM QryTxt
      EXECUTE EnuEliReverso 
  													   
      IF SQLCA.SQLCODE <> 0 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   WHENEVER ERROR STOP

END FUNCTION

#############################################################################
# Funcion           => fn_cargar_modulos - Rutina para cargar los arreglos  #
#                      de los modulos                                       #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION fn_cargar_modulos()
   DEFINE v_contador    SMALLINT, -- contador de registros
          r_cat_proceso  RECORD LIKE cat_proceso.*

   -- se limpia el arreglo de modulos
   CALL arr_cat_proceso.clear()
   
   -- se leen los registros de la tabla cat_proceso
   DECLARE cur_catproceso CURSOR FOR SELECT *
                                      FROM cat_proceso
                                     ORDER BY proceso_cod

   LET v_contador = 1

   -- Carga del arreglo de modulos de la base de datos
   FOREACH cur_catproceso INTO r_cat_proceso.*
      -- se transfieren los datos al arreglo de despliegue
      LET arr_cat_proceso[v_contador].* = r_cat_proceso.*
      LET v_contador = v_contador + 1
   END FOREACH 
END FUNCTION

#############################################################################
# Funcion           => fn_rec_proceso - Rutina para recuperar el registro   #
#                      del proceso seleccionado                             #
# Propietario       => E.F.P                                                #
# Sistema           => CAT                                                  #
# Entrada:          => p_proceso - Clave del proceso a recuperar            #
# Salida:           => v_r_proceso - registro del proceso recuperado        #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 28 Febrero 2012                                      #
#############################################################################
FUNCTION fn_rec_proceso(p_proceso)
   DEFINE p_proceso   LIKE cat_proceso.proceso_cod
   DEFINE v_r_proceso RECORD LIKE cat_proceso.*
   
   SELECT * INTO v_r_proceso.*
     FROM cat_proceso
    WHERE proceso_cod = p_proceso
    
  RETURN v_r_proceso.*
  
END FUNCTION
