--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--===============================================================

####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPC81                                        #
#Objetivo          =>Consulta de canales de recepcion              #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>01 Mayo 2012                                  #
####################################################################

DATABASE safre_viv

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod
DEFINE p_tipo_ejecucion SMALLINT
DEFINE p_titulo         STRING
DEFINE v_r_sep_cat_canal_recepcion_exp DYNAMIC ARRAY OF RECORD LIKE sep_cat_canal_recepcion_exp.*   
DEFINE v_reg_sep_cat_canal_recepcion_exp RECORD LIKE sep_cat_canal_recepcion_exp.*   

MAIN 

DEFINE v_SqlQry       STRING
DEFINE v_i_pos        SMALLINT
DEFINE v_i_pos_max    SMALLINT
DEFINE v_ventana      ui.Window

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   RUN "/home/selefp/sep/archivo_01.sh"
   
   OPEN WINDOW w_canal_separa WITH FORM "SEPC811"

   -- Apertura de menú principal del módulo
   DISPLAY ARRAY v_r_sep_cat_canal_recepcion_exp TO tbl_sep_cat_canal_recepcion_exp.*
      ATTRIBUTE (ACCEPT=FALSE , CANCEL=FALSE )
      
      BEFORE DISPLAY
         -- Asignación de titulo de ventana del módulo
         IF p_titulo IS NOT NULL THEN
            CALL ui.Interface.setText(p_titulo)
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText(p_titulo)
         END IF
      
      -- Extracción de datos del catálogo para su visualización
      ON ACTION consulta
         
         CALL v_r_sep_cat_canal_recepcion_exp.clear()
         
         LET v_SqlQry = "select * from sep_cat_canal_recepcion_exp order by 1"
   
         PREPARE EnuCanSep FROM v_SqlQry
         DECLARE CurCanSep CURSOR FOR EnuCanSep

         LET v_i_pos = 1
         
         -- Carga del arreglo global para su visualización y manipulación
         FOREACH CurCanSep INTO v_r_sep_cat_canal_recepcion_exp[v_i_pos].*
            LET v_i_pos = v_i_pos + 1
         END FOREACH
         
         -- Eliminación de registros nulos
         IF LENGTH(v_r_sep_cat_canal_recepcion_exp[v_r_sep_cat_canal_recepcion_exp.getLength()].canal_cod CLIPPED) = 0 THEN
            CALL v_r_sep_cat_canal_recepcion_exp.deleteElement(v_r_sep_cat_canal_recepcion_exp.getLength())
         END IF
         
         -- Despliegue del total de registros encontrados
         DISPLAY v_r_sep_cat_canal_recepcion_exp.getLength() TO v_total

         -- Despliegue en pantalla de los registros encontrados
         DISPLAY ARRAY v_r_sep_cat_canal_recepcion_exp TO tbl_sep_cat_canal_recepcion_exp.*
            BEFORE DISPLAY
               EXIT DISPLAY
         END DISPLAY
            
      ON ACTION modifica
         
         -- Validación de que exista registros consultados antes de poderlos modificar
         IF v_r_sep_cat_canal_recepcion_exp.getLength() = 0 THEN
            CALL fn_mensaje("Aviso","Debe consultar la información antes de modificarla","info")
            CONTINUE DISPLAY
         END IF

         -- Apertura del arreglo para su manipulación
         INPUT ARRAY v_r_sep_cat_canal_recepcion_exp FROM tbl_sep_cat_canal_recepcion_exp.*
            ATTRIBUTE (INSERT ROW = FALSE, APPEND ROW = FALSE ,
                       DELETE ROW = FALSE, AUTO APPEND = FALSE ,
                       ACCEPT=FALSE , CANCEL=FALSE ,
                       WITHOUT DEFAULTS = TRUE, UNBUFFERED)

            -- Confirmación para alamcenar la información en el catálogo
            ON ACTION aceptar
               IF fn_ventana_confirma("Aviso","Desea aceptar las modificaciones?","question") = 1 THEN
                  CALL fn_actualiza_datos()
                  EXIT INPUT
               END IF

            -- Agregar un nuevo elelemento en la pantalla
            ON ACTION agrega
               
               LET v_i_pos = ARR_CURR()
               -- Agrega elemento al final
               CALL v_r_sep_cat_canal_recepcion_exp.appendElement()
               LET v_i_pos_max = v_r_sep_cat_canal_recepcion_exp.getLength()
   
               -- Nueve los elementos a una posición posterior para dejar libre el elemento agregado en la posición seleccionada
               WHILE v_i_pos < v_i_pos_max
                  LET v_r_sep_cat_canal_recepcion_exp[v_i_pos_max].* = v_r_sep_cat_canal_recepcion_exp[v_i_pos_max-1].*
                  LET v_r_sep_cat_canal_recepcion_exp[v_i_pos_max].canal_cod = v_r_sep_cat_canal_recepcion_exp[v_i_pos_max].canal_cod + 1
                  LET v_i_pos_max = v_i_pos_max - 1
               END WHILE
               
               -- Inicializa registro agregado
               INITIALIZE v_r_sep_cat_canal_recepcion_exp[v_i_pos+1].* TO NULL
               LET v_r_sep_cat_canal_recepcion_exp[v_i_pos+1].canal_cod = v_i_pos+1
               
               -- Despliegue en pantalla de los registros
               DISPLAY ARRAY v_r_sep_cat_canal_recepcion_exp TO tbl_sep_cat_canal_recepcion_exp.*
                  BEFORE DISPLAY 
                     EXIT DISPLAY
               END DISPLAY 
               
               -- Despliegue del total de registros encontrados
               DISPLAY v_r_sep_cat_canal_recepcion_exp.getLength() TO v_total
               
            ON ACTION elimina
               -- Elimina el registro seleccionado del arreglo 
               CALL v_r_sep_cat_canal_recepcion_exp.deleteElement(ARR_CURR())
   
               -- Reasignacion de claves en los registros restantes
               FOR v_i_pos = 1 TO v_r_sep_cat_canal_recepcion_exp.getLength()
                  LET v_r_sep_cat_canal_recepcion_exp[v_i_pos].canal_cod = v_i_pos
               END FOR
               
               -- Despliegue en pantalla de los registros restantes
               DISPLAY ARRAY v_r_sep_cat_canal_recepcion_exp TO tbl_sep_cat_canal_recepcion_exp.*
                  BEFORE DISPLAY 
                     EXIT DISPLAY
               END DISPLAY 
               
               -- Despliegue del total de registros encontrados
               DISPLAY v_r_sep_cat_canal_recepcion_exp.getLength() TO v_total

            -- Confirmación de la cancelación de modificación de registros
            ON ACTION cancel
               IF fn_ventana_confirma("Aviso","Desea cancelar modificaciones?","question") = 1 THEN
                  CALL v_r_sep_cat_canal_recepcion_exp.clear()
                  DISPLAY ARRAY v_r_sep_cat_canal_recepcion_exp TO tbl_sep_cat_canal_recepcion_exp.*
                     BEFORE DISPLAY 
                        EXIT DISPLAY
                  END DISPLAY 
                  -- Despliegue del total de registros encontrados
                  DISPLAY v_r_sep_cat_canal_recepcion_exp.getLength() TO v_total
                  EXIT INPUT
               END IF

         END INPUT
               
      -- Salida del modulo
      ON ACTION salir
         EXIT DISPLAY
      
   END DISPLAY
   
   
END MAIN

#############################################################################
# Funcion           => fn_actualiza_datos - Actualiza los datos del catalogo#
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 01 Mayo 2012                                         #
#############################################################################
FUNCTION fn_actualiza_datos()
DEFINE v_pos   SMALLINT  
  
  -- Recorrido de todo el arreglo para realizar la afectación correspondiente
  FOR v_pos = 1 TO v_r_sep_cat_canal_recepcion_exp.getLength()
     
     IF fn_existe_sep_cat_canal(v_r_sep_cat_canal_recepcion_exp[v_pos].canal_cod) THEN
        -- Actualiza los registros existentes con la nueva información
        CALL fn_update_sep_cat_canal(v_r_sep_cat_canal_recepcion_exp[v_pos].*)
     ELSE
        -- Inserta los registros agregados al catalogo
        CALL fn_insert_sep_cat_canal(v_r_sep_cat_canal_recepcion_exp[v_pos].*)
     END IF
     
  END FOR
  
  -- Eliminación de registros excedentes a la última posición
  CALL fn_elimina_excedentes(v_r_sep_cat_canal_recepcion_exp[v_r_sep_cat_canal_recepcion_exp.getLength()].canal_cod)
  
END FUNCTION

#############################################################################
# Funcion           => fn_existe_sep_cat_canal - Verifica la existencia del #
#                      registro de canal                                    #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_canal_cod - clave de canal buscado                 #
# Salida:           => Verdadero si existe, falso en caso contrario         #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 01 Mayo 2012                                         #
#############################################################################
FUNCTION fn_existe_sep_cat_canal(p_canal_cod)
DEFINE p_canal_cod LIKE sep_cat_canal_recepcion_exp.canal_cod
DEFINE v_count     SMALLINT

   SELECT COUNT(*)
     INTO v_count
     FROM sep_cat_canal_recepcion_exp
    WHERE canal_cod = p_canal_cod
    
   IF v_count > 0 THEN
      RETURN 1
   ELSE
      RETURN 0
   END IF 
   
END FUNCTION

#############################################################################
# Funcion           => fn_insert_sep_cat_canal - Inserta regsitro nuevo de  #
#                      canal                                                #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_r_cat_canal - registro de canal a insertar         #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Mayo 2012                                         #
#############################################################################
FUNCTION fn_insert_sep_cat_canal(p_r_cat_canal)
DEFINE p_r_cat_canal   RECORD LIKE sep_cat_canal_recepcion_exp.*

   INSERT INTO sep_cat_canal_recepcion_exp VALUES (p_r_cat_canal.*)
   
   DISPLAY "Inserta ", p_r_cat_canal.canal_cod, " - ",p_r_cat_canal.canal_desc
   
END FUNCTION

#############################################################################
# Funcion           => fn_update_sep_cat_canal - Actuliza regsitro nuevo de #
#                      canal                                                #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_r_cat_canal - registro de canal a actualizar       #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Mayo 2012                                         #
#############################################################################
FUNCTION fn_update_sep_cat_canal(p_r_cat_canal)
DEFINE p_r_cat_canal   RECORD LIKE sep_cat_canal_recepcion_exp.*

   UPDATE sep_cat_canal_recepcion_exp
      SET canal_desc = p_r_cat_canal.canal_desc
    WHERE canal_cod  = p_r_cat_canal.canal_cod
   
   DISPLAY "Actualiza ", p_r_cat_canal.canal_cod, " - ",p_r_cat_canal.canal_desc
    
END FUNCTION

#############################################################################
# Funcion           => fn_elimina_excedentes - Elimina regsitros excedentes #
#                      de canal                                             #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_canal_cod - clave de últino canal válido           #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Mayo 2012                                         #
#############################################################################
FUNCTION fn_elimina_excedentes(p_canal_cod)
DEFINE p_canal_cod LIKE sep_cat_canal_recepcion_exp.canal_cod

   DELETE FROM sep_cat_canal_recepcion_exp
    WHERE canal_cod > p_canal_cod

   DISPLAY "Elimian exc ", p_canal_cod
    
END FUNCTION