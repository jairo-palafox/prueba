--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/03/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTM01                                                     #
#Objetivo        => Catálogo de programas de negocio bus genérico              #
#Fecha Inicio    => 03 Marzo 2015                                              #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod      VARCHAR(20), --LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion    SMALLINT,
       p_titulo_ventana   STRING,
       v_ventana          ui.Window

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)

   CALL fn_inicializa_consultas()
   
   # muestra pantalla para elegir opción
   CALL fn_elige_opcion()
   
END MAIN

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => funcion para inicializar consultas                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

      # almacena datos
   LET v_consulta = " INSERT INTO cat_bus_negocio(id_cat_bus_negocio,",
                    "                             programa,",
                    "                             desc_programa,",
                    "                             origen,",
                    "                             tipo_programa)",
                    " VALUES(seq_cat_bus_negocio.NEXTVAL,?,?,?,?)"
   PREPARE prp_almacena_negocio FROM v_consulta

   LET v_consulta = "\n SELECT id_cat_bus_negocio,",
                    "\n        programa,",
                    "\n        desc_programa,",
                    "\n        origen,",
                    "\n        tipo_programa",
                    "\n   FROM cat_bus_negocio",
                    "\n  WHERE 1 = 1" # Todos los registros
   PREPARE prp_recupera_registros_negocio FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_negocio",
                    "    SET desc_programa = ?,",
                    "        origen        = ?,",
                    "        tipo_programa = ?",
                    "  WHERE id_cat_bus_negocio = ?"
   PREPARE prp_actualiza_negocio FROM v_consulta

   LET v_consulta = "\n SELECT NVL(1,0)",
                    "\n   FROM cat_bus_rechazo",
                    "\n  WHERE cod_rechazo = ?"
   PREPARE prp_verifica_cod_rechazo FROM v_consulta 

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => funcion para elegir opcion de menu a realizar            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elige_opcion()


   OPEN WINDOW vtna_menu WITH FORM "PRTM011"
      # Se asigna el titulo de la ventana
      IF(p_titulo_ventana IS NOT NULL)THEN
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF

      # Menu de acciones a realizar
      MENU ""

         ON ACTION agregar
            CALL fn_captura_datos_negocio()

         ON ACTION consultar
            CALL fn_elige_registro_bus_negocio("C")# Consulta de registro

         ON ACTION modificar
            CALL fn_elige_registro_bus_negocio("M")# Modificación de registros

         ON ACTION cancelar
            EXIT MENU
      
      END MENU
      
   CLOSE WINDOW vtna_menu

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Pantalla de captura de datos de negocio                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_datos_negocio()
DEFINE v_negocio RECORD
         v_programa      LIKE cat_bus_negocio.programa,
         v_descripcion   LIKE cat_bus_negocio.desc_programa,
         v_origen        LIKE cat_bus_negocio.origen,
         v_tipo_programa LIKE cat_bus_negocio.tipo_programa
       END RECORD,
       r_error           BOOLEAN,
       r_confirma        BOOLEAN,
       r_existe_registro BOOLEAN

   INPUT v_negocio.v_programa,
         v_negocio.v_descripcion,
         v_negocio.v_origen,
         v_negocio.v_tipo_programa WITHOUT DEFAULTS 
    FROM programa,
         desc_programa,
         origen,
         tipo_programa ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

      AFTER FIELD programa
         CALL fn_verifica_programa(v_negocio.v_programa)RETURNING r_existe_registro
         IF( r_existe_registro )THEN
            CALL fn_mensaje("AVISO","Ya existe el código de rechazo '"||v_negocio.v_programa CLIPPED||"'","exclamation")
            NEXT FIELD programa
         END IF 

      ON ACTION aceptar
         # valida captura de datos
         IF(v_negocio.v_programa IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture programa","information")
            NEXT FIELD programa
         END IF

         CALL fn_verifica_programa(v_negocio.v_programa)RETURNING r_existe_registro
         IF( r_existe_registro )THEN
            CALL fn_mensaje("AVISO","Ya existe el código de rechazo '"||v_negocio.v_programa CLIPPED||"'","exclamation")
            NEXT FIELD programa
         END IF

         IF(v_negocio.v_origen IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture origen programa","information")
            NEXT FIELD origen
         END IF
         IF(v_negocio.v_tipo_programa IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture tipo programa","information")
            NEXT FIELD tipo_programa
         END IF
         CALL fn_ventana_confirma("AVISO","¿Almacenar registro?","question")RETURNING r_confirma
         IF(r_confirma)THEN
            CALL fn_agrega_negocio(v_negocio.*) RETURNING r_error
            IF(r_error)THEN
               CALL fn_mensaje("AVISO","No se pudieron almacenar los datos","information")
               CONTINUE INPUT
            ELSE
               CALL fn_mensaje("AVISO","Registro agregado correctamente","information")
               INITIALIZE v_negocio.* TO NULL
            END IF
            # si se almacenaron correctamente, regresa a menu de opciones
            EXIT INPUT
         ELSE
            CONTINUE INPUT
         END IF

      ON ACTION cancelar
         EXIT INPUT

   END INPUT

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Almacena datos de negocio                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_agrega_negocio(p_negocio)
DEFINE p_negocio RECORD
         v_programa      LIKE cat_bus_negocio.programa,
         v_descripcion   LIKE cat_bus_negocio.desc_programa,
         v_origen        LIKE cat_bus_negocio.origen,
         v_tipo_programa LIKE cat_bus_negocio.tipo_programa
       END RECORD,
       v_error    BOOLEAN

   LET v_error = FALSE
   EXECUTE prp_almacena_negocio USING p_negocio.v_programa,
                                      p_negocio.v_descripcion,
                                      p_negocio.v_origen,
                                      p_negocio.v_tipo_programa
          
   # Verifica si ocurrió error
   IF(SQLCA.sqlcode <> 0)THEN
      LET v_error = TRUE
   END IF

   RETURN v_error

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Muestra tabla de registros capturados                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elige_registro_bus_negocio(p_tipo_consulta)
DEFINE p_tipo_consulta CHAR(1),
       r_negocio DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio, 
         v_programa           LIKE cat_bus_negocio.programa,
         v_descripcion        LIKE cat_bus_negocio.desc_programa,
         v_origen             LIKE cat_bus_negocio.origen,
         v_tipo_programa      LIKE cat_bus_negocio.tipo_programa
       END RECORD
       
   OPEN WINDOW vtna_tabla_registros WITH FORM "PRTM012"

      DISPLAY ARRAY r_negocio TO sr_bus_negocio.* ATTRIBUTES(UNBUFFERED , ACCEPT = FALSE, CANCEL = FALSE)
      
         BEFORE DISPLAY
            IF(p_titulo_ventana IS NOT NULL)THEN
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo_ventana)
               CALL ui.Interface.setText(p_titulo_ventana)
            END IF
            CALL fn_recupera_registros_bus_negocio() RETURNING r_negocio
            IF(r_negocio.getLength() = 0)THEN
               CALL fn_mensaje(p_titulo_ventana,"No se encontró información","information")
               EXIT DISPLAY
            END IF

         ON ACTION elegir
            # determina si es consulta o modificacion
            IF(p_tipo_consulta = "C")THEN # Consulta
               CALL fn_consulta_registro_bus_negocio(r_negocio[ARR_CURR()].*)
            ELSE # Modificación
               CALL fn_captura_modificacion_bus_negocio(r_negocio[ARR_CURR()].*)
            END IF
            CALL r_negocio.clear()
            CALL fn_recupera_registros_bus_negocio() RETURNING r_negocio

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_tabla_registros

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Recupera los registros de bus generico                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_registros_bus_negocio()
DEFINE v_registros_negocio DYNAMIC ARRAY OF RECORD
         v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio, 
         v_programa      LIKE cat_bus_negocio.programa,
         v_descripcion   LIKE cat_bus_negocio.desc_programa,
         v_origen        LIKE cat_bus_negocio.origen,
         v_tipo_programa LIKE cat_bus_negocio.tipo_programa
       END RECORD,
       v_indice   INTEGER


   CALL v_registros_negocio.clear()
   LET v_indice = 1
   
   DECLARE cur_recupera_registros_negocio CURSOR FOR prp_recupera_registros_negocio
   FOREACH cur_recupera_registros_negocio INTO v_registros_negocio[v_indice].v_id_cat_bus_negocio,
                                               v_registros_negocio[v_indice].v_programa,
                                               v_registros_negocio[v_indice].v_descripcion,
                                               v_registros_negocio[v_indice].v_origen,
                                               v_registros_negocio[v_indice].v_tipo_programa
     LET v_indice = v_indice + 1

   END FOREACH
   # en caso de que el ultimo registro sea nulo, se elimina
   IF(v_registros_negocio[v_registros_negocio.getLength()].v_programa IS NULL)THEN
      CALL v_registros_negocio.deleteElement(v_registros_negocio.getLength())
   END IF

   RETURN v_registros_negocio
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Muestra registro consultado                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_consulta_registro_bus_negocio(p_registro_negocio)
DEFINE p_registro_negocio RECORD
         v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio, 
         v_programa           LIKE cat_bus_negocio.programa,
         v_descripcion        LIKE cat_bus_negocio.desc_programa,
         v_origen             LIKE cat_bus_negocio.origen,
         v_tipo_programa      LIKE cat_bus_negocio.tipo_programa
       END RECORD

   OPEN WINDOW vtna_consulta_registro WITH FORM "PRTM011"
      # sólo muestra el registro a consultar
      MENU ""
         BEFORE MENU
            DISPLAY p_registro_negocio.v_programa      TO programa
            DISPLAY p_registro_negocio.v_descripcion   TO desc_programa
            DISPLAY p_registro_negocio.v_origen        TO origen
            DISPLAY p_registro_negocio.v_tipo_programa TO tipo_programa
            
         ON ACTION cancelar
            INITIALIZE p_registro_negocio.* TO NULL
            EXIT MENU

      END MENU  

   CLOSE WINDOW vtna_consulta_registro

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Muestra registro consultado                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_modificacion_bus_negocio(p_registro_negocio)
DEFINE p_registro_negocio RECORD
         v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio, 
         v_programa           LIKE cat_bus_negocio.programa,
         v_descripcion        LIKE cat_bus_negocio.desc_programa,
         v_origen             LIKE cat_bus_negocio.origen,
         v_tipo_programa      LIKE cat_bus_negocio.tipo_programa
       END RECORD,
       r_confirma  BOOLEAN, # respuesta de ventana de confirmacion
       v_error     BOOLEAN  # en caso de error al actualizar registro

   OPEN WINDOW vtna_modifica_registro WITH FORM "PRTM011"

      INPUT p_registro_negocio.v_descripcion,
            p_registro_negocio.v_origen,
            p_registro_negocio.v_tipo_programa WITHOUT DEFAULTS 
       FROM desc_programa,
            origen,
            tipo_programa ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            DISPLAY p_registro_negocio.v_programa TO programa
            
         ON ACTION aceptar
            CALL fn_ventana_confirma("AVISO","¿Actualizar registro?","question")RETURNING r_confirma
            IF(r_confirma)THEN
               CALL fn_actualiza_registro_bus_negocio(p_registro_negocio.*) RETURNING v_error
               # en caso de error al actualizar
               IF(v_error)THEN
                  CALL fn_mensaje("AVISO","No se puede actualizar el registro","information")
                  CONTINUE INPUT
               END IF
               # termina actualizacion correctamente
               CALL fn_mensaje("AVISO","Registro actualizado correctamente","information")
               EXIT INPUT               
            END IF
            CONTINUE INPUT
            
         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_modifica_registro
   
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Muestra registro consultado                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_actualiza_registro_bus_negocio(p_registro_negocio)
DEFINE p_registro_negocio RECORD
         v_id_cat_bus_negocio LIKE cat_bus_negocio.id_cat_bus_negocio, 
         v_programa           LIKE cat_bus_negocio.programa,
         v_descripcion        LIKE cat_bus_negocio.desc_programa,
         v_origen             LIKE cat_bus_negocio.origen,
         v_tipo_programa      LIKE cat_bus_negocio.tipo_programa
       END RECORD,
       v_error BOOLEAN

   EXECUTE prp_actualiza_negocio USING p_registro_negocio.v_descripcion,
                                       p_registro_negocio.v_origen,
                                       p_registro_negocio.v_tipo_programa,
                                       p_registro_negocio.v_id_cat_bus_negocio

   # Verifica si ocurrió error
   IF(SQLCA.sqlcode <> 0)THEN
      LET v_error = TRUE
   END IF

   RETURN v_error
    
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM01                                                   #
#Descripcion       => Revisa si ya existe el cod_rechazo                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_verifica_programa(p_programa)
DEFINE p_programa        LIKE cat_bus_negocio.programa,
       v_existe_registro BOOLEAN

   LET v_existe_registro =  FALSE
   EXECUTE prp_verifica_cod_rechazo USING p_programa
                                     INTO v_existe_registro
   
   RETURN v_existe_registro
END FUNCTION