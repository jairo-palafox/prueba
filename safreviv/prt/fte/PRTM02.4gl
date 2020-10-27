--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 03/03/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTM02                                                     #
#Objetivo        => Catálogo de códigos de rechazo de bus genérico             #
#Fecha Inicio    => 03 Marzo 2015                                              #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod      VARCHAR(20),--LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion    SMALLINT,
       p_titulo_ventana   STRING,
       v_ventana          ui.Window

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
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
#Programa          => PRTM02                                                   #
#Descripcion       => funcion para inicializar consultas                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   # almacena datos
    --id_cat_bus_rechazo,",
   LET v_consulta = " INSERT INTO cat_bus_rechazo(cod_rechazo,",
                    "                             desc_rechazo,",
                    "                             nombre_campo,",
                    "                             tipo_rechazo,",
                    "                             f_actualiza,",
                    "                             usuario)",
                    " VALUES(?,?,?,?,?,?)"
   PREPARE prp_almacena_rechazo FROM v_consulta

   LET v_consulta = "\n SELECT cod_rechazo,",
                    "\n        nombre_campo,",
                    "\n        desc_rechazo,",
                    "\n        tipo_rechazo,",
                    "\n        f_actualiza,",
                    "\n        usuario",
                    "\n   FROM cat_bus_rechazo",
                    "\n  WHERE 1 = 1" # Todos los registros
   PREPARE prp_recupera_registros_negocio FROM v_consulta

   LET v_consulta = " UPDATE cat_bus_rechazo",
                    "    SET nombre_campo = ?,",
                    "        desc_rechazo = ?,",
                    "        tipo_rechazo = ?,",
                    "        f_actualiza  = ?,",
                    "        usuario      = ?",
                    "  WHERE cod_rechazo = ?"
   PREPARE prp_actualiza_rechazo FROM v_consulta

   LET v_consulta = "\n SELECT NVL(1,0)",
                    "\n   FROM cat_bus_rechazo",
                    "\n  WHERE cod_rechazo = ?"
   PREPARE prp_verifica_cod_rechazo FROM v_consulta

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => funcion para elegir opcion de menu a realizar            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elige_opcion()

   OPEN WINDOW vtna_menu WITH FORM "PRTM021"
      # Se asigna el titulo de la ventana
      IF(p_titulo_ventana IS NOT NULL)THEN
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF

      # Menu de acciones a realizar
      MENU ""

         ON ACTION agregar
            CALL fn_captura_datos_rechazo()

         ON ACTION consultar
            CALL fn_elige_registro_bus_rechazo("C")# Consulta de registro

         ON ACTION modificar
            CALL fn_elige_registro_bus_rechazo("M")# Modificación de registros

         ON ACTION cancelar
            EXIT MENU
      
      END MENU
      
   CLOSE WINDOW vtna_menu

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Pantalla de captura de datos de rechazo                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_datos_rechazo()
DEFINE v_rechazo RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       r_error           BOOLEAN,
       r_confirma        BOOLEAN,
       r_existe_registro BOOLEAN

   INPUT v_rechazo.v_cod_rechazo,
         v_rechazo.v_descripcion,
         v_rechazo.v_nombre_campo,
         v_rechazo.v_tipo_rechazo WITHOUT DEFAULTS 
    FROM cod_rechazo,
         desc_rechazo,
         nombre_campo,
         tipo_rechazo ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         
      AFTER FIELD cod_rechazo 
         CALL fn_verifica_cod_rechazo(v_rechazo.v_cod_rechazo)RETURNING r_existe_registro
         IF( r_existe_registro )THEN
            CALL fn_mensaje("AVISO","Ya existe el código de rechazo '"||v_rechazo.v_cod_rechazo CLIPPED||"'","exclamation")
            NEXT FIELD cod_rechazo
         END IF

      ON ACTION aceptar
         # valida captura de datos
         IF(v_rechazo.v_cod_rechazo IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture código","information")
            NEXT FIELD cod_rechazo
         END IF
         
         CALL fn_verifica_cod_rechazo(v_rechazo.v_cod_rechazo)RETURNING r_existe_registro
         IF( r_existe_registro )THEN
            CALL fn_mensaje("AVISO","Ya existe el código de rechazo '"||v_rechazo.v_cod_rechazo CLIPPED||"'","exclamation")
            NEXT FIELD cod_rechazo
         END IF
         IF(v_rechazo.v_nombre_campo IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture nombre campo","information")
            NEXT FIELD nombre_campo
         END IF
         IF(v_rechazo.v_tipo_rechazo IS NULL)THEN
            CALL fn_mensaje("AVISO","Capture tipo rechazo","information")
            NEXT FIELD tipo_rechazo
         END IF
         CALL fn_ventana_confirma("AVISO","¿Almacenar registro?","question")RETURNING r_confirma
         IF(r_confirma)THEN
            LET v_rechazo.v_f_actualiza = TODAY
            LET v_rechazo.v_usuario     = p_usuario_cod
            
            CALL fn_agrega_rechazo(v_rechazo.*) RETURNING r_error
            IF(r_error)THEN
               CALL fn_mensaje("AVISO","No se pudieron almacenar los datos","information")
               CONTINUE INPUT
            END IF
            INITIALIZE v_rechazo.* TO NULL 
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
#Programa          => PRTM02                                                   #
#Descripcion       => Almacena datos de rechazo                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_agrega_rechazo(p_rechazo)
DEFINE p_rechazo RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       v_error    BOOLEAN

   LET v_error = FALSE

   EXECUTE prp_almacena_rechazo USING p_rechazo.v_cod_rechazo,
                                      p_rechazo.v_descripcion,
                                      p_rechazo.v_nombre_campo,
                                      p_rechazo.v_tipo_rechazo,
                                      p_rechazo.v_f_actualiza,
                                      p_rechazo.v_usuario
          
   # Verifica si ocurrió error
   IF(SQLCA.sqlcode <> 0)THEN
      LET v_error = TRUE
   ELSE
      CALL fn_mensaje(p_titulo_ventana,"Registro almacenado correctamente","information")
   END IF

   RETURN v_error

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Muestra tabla de registros capturados                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_elige_registro_bus_rechazo(p_tipo_consulta)
DEFINE p_tipo_consulta CHAR(1),
       r_rechazos DYNAMIC ARRAY OF RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       v_continua BOOLEAN

   LET v_continua = TRUE
   OPEN WINDOW vtna_tabla_registros WITH FORM "PRTM022"

      WHILE v_continua
         DISPLAY ARRAY r_rechazos TO sr_bus_rechazo.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
      
            BEFORE DISPLAY
               CALL fn_recupera_registros_bus_rechazo() RETURNING r_rechazos
               IF(p_titulo_ventana IS NOT NULL)THEN
                  LET v_ventana = ui.Window.getCurrent()
                  CALL v_ventana.setText(p_titulo_ventana)
                  CALL ui.Interface.setText(p_titulo_ventana)
               END IF

            ON ACTION elegir
               # determina si es consulta o modificacion
               IF(p_tipo_consulta = "C")THEN # Consulta
                  CALL fn_consulta_registro_bus_rechazo(r_rechazos[ARR_CURR()].*)
               ELSE # Modificación
                  CALL fn_captura_modificacion_bus_rechazo(r_rechazos[ARR_CURR()].*)
               END IF
               EXIT DISPLAY              

            ON ACTION cancelar
               LET v_continua = FALSE
               EXIT DISPLAY

         END DISPLAY
     END WHILE 

   CLOSE WINDOW vtna_tabla_registros

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Recupera los registros de bus rechazo                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_recupera_registros_bus_rechazo()
DEFINE v_rechazos DYNAMIC ARRAY OF RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       v_indice   INTEGER

   CALL v_rechazos.clear()
   LET v_indice = 1
   
   DECLARE cur_recupera_registros_negocio CURSOR FOR prp_recupera_registros_negocio
   FOREACH cur_recupera_registros_negocio INTO v_rechazos[v_indice].v_cod_rechazo,
                                               v_rechazos[v_indice].v_nombre_campo,
                                               v_rechazos[v_indice].v_descripcion,
                                               v_rechazos[v_indice].v_tipo_rechazo,
                                               v_rechazos[v_indice].v_f_actualiza,
                                               v_rechazos[v_indice].v_usuario
     LET v_indice = v_indice + 1

   END FOREACH
   # en caso de que el ultimo registro sea nulo, se elimina
   IF(v_rechazos[v_rechazos.getLength()].v_cod_rechazo IS NULL)THEN
      CALL v_rechazos.deleteElement(v_rechazos.getLength())
   END IF

   RETURN v_rechazos
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Muestra registro consultado                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_consulta_registro_bus_rechazo(p_registro_rechazos)
DEFINE p_registro_rechazos RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD

   OPEN WINDOW vtna_consulta_registro WITH FORM "PRTM021"
      # sólo muestra el registro a consultar
      MENU ""
         BEFORE MENU
            DISPLAY p_registro_rechazos.v_cod_rechazo  TO cod_rechazo
            DISPLAY p_registro_rechazos.v_descripcion  TO desc_rechazo
            DISPLAY p_registro_rechazos.v_nombre_campo TO nombre_campo
            DISPLAY p_registro_rechazos.v_tipo_rechazo TO tipo_rechazo
            
         ON ACTION cancelar
            EXIT MENU

      END MENU  

   CLOSE WINDOW vtna_consulta_registro

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Muestra registro consultado                              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_captura_modificacion_bus_rechazo(p_registro_rechazos)
DEFINE p_registro_rechazos RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       r_confirma  BOOLEAN, # respuesta de ventana de confirmacion
       v_error     BOOLEAN  # en caso de error al actualizar registro

   OPEN WINDOW vtna_modifica_registro WITH FORM "PRTM021"

      INPUT p_registro_rechazos.v_descripcion,
            p_registro_rechazos.v_nombre_campo,
            p_registro_rechazos.v_tipo_rechazo WITHOUT DEFAULTS 
       FROM desc_rechazo,
            nombre_campo,
            tipo_rechazo ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            DISPLAY p_registro_rechazos.v_cod_rechazo TO cod_rechazo
            
         ON ACTION aceptar
            IF(p_registro_rechazos.v_nombre_campo IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture nombre campo","information")
               NEXT FIELD nombre_campo
            END IF
            IF(p_registro_rechazos.v_tipo_rechazo IS NULL)THEN
               CALL fn_mensaje("AVISO","Capture tipo rechazo","information")
               NEXT FIELD tipo_rechazo
            END IF
            CALL fn_ventana_confirma("AVISO","¿Actualizar registro?","question")RETURNING r_confirma
            IF(r_confirma)THEN
               # recupera fecha y usuario actual
               LET p_registro_rechazos.v_f_actualiza = TODAY
               LET p_registro_rechazos.v_usuario     = p_usuario_cod 
               CALL fn_actualiza_registro_bus_rechazo(p_registro_rechazos.*) RETURNING v_error
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
#Programa          => PRTM02                                                   #
#Descripcion       => Actualiza el registro                                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_actualiza_registro_bus_rechazo(p_registro_rechazos)
DEFINE p_registro_rechazos RECORD
         v_cod_rechazo   LIKE cat_bus_rechazo.cod_rechazo,
         v_descripcion   LIKE cat_bus_rechazo.desc_rechazo,
         v_nombre_campo  LIKE cat_bus_rechazo.nombre_campo,
         v_tipo_rechazo  LIKE cat_bus_rechazo.tipo_rechazo,
         v_f_actualiza   LIKE cat_bus_rechazo.f_actualiza,
         v_usuario       LIKE cat_bus_rechazo.usuario
       END RECORD,
       v_error BOOLEAN
    
   EXECUTE prp_actualiza_rechazo USING p_registro_rechazos.v_nombre_campo,
                                       p_registro_rechazos.v_descripcion,
                                       p_registro_rechazos.v_tipo_rechazo,
                                       p_registro_rechazos.v_f_actualiza,
                                       p_registro_rechazos.v_usuario,
                                       p_registro_rechazos.v_cod_rechazo

   # Verifica si ocurrió error
   IF(SQLCA.sqlcode <> 0)THEN
      LET v_error = TRUE
   END IF

   RETURN v_error
    
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTM02                                                   #
#Descripcion       => Revisa si ya existe el cod_rechazo                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 03 Marzo 2015                                            #
################################################################################
FUNCTION fn_verifica_cod_rechazo(p_cod_rechazo)
DEFINE p_cod_rechazo     LIKE cat_bus_rechazo.cod_rechazo,
       v_consulta        STRING,
       v_existe_registro BOOLEAN

   LET v_existe_registro =  FALSE

   EXECUTE prp_verifica_cod_rechazo USING p_cod_rechazo
                                     INTO v_existe_registro
   
   RETURN v_existe_registro
END FUNCTION