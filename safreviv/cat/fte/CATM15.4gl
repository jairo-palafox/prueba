--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/09/2016
--===============================================================
#######################################################################
#Modulo       => CAT                                                  #
#Programa     => CATM15                                               #
#Objetivo     => Mantenimiento al catálogo de dias feriados infonavit #
#Autor        => Hugo César Ramírez Gracía                            #
#Fecha inicio => 20 SEPTIEMBRE 2016                                   #
#######################################################################
DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Clave del usuario
       p_tipo_ejecucion  SMALLINT,                     # Forma como ejecutara el programa
       p_titulo_vtna     STRING,                       # Texto que aparece en titulo de ventana
       v_dias_feriados   DYNAMIC ARRAY OF RECORD
         v_seleccion     BOOLEAN,
         v_fecha         LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion   LIKE cat_feriado_infonavit.feriado_desc
       END RECORD,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

       
MAIN
   # Recupera parámetros de programa
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   CALL fn_elije_opcion()
   
END MAIN

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Menu principal del manrenimiento de fechas feriadas      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_elije_opcion()

   # se recupera la ruta ejecutable del módulo
   {SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "glo"}

   #Se asigna el titulo de la ventana
   IF(p_titulo_vtna IS NOT NULL)THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF
   MENU
      BEFORE MENU

      ON ACTION agregar
         CALL fn_agrega_fecha()

      ON ACTION consultar
         CALL fn_consulta_fechas()

      ON ACTION eliminar
         CALL fn_elimina_fechas()

      ON ACTION cancelar
         EXIT MENU

   END MENU
   
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para agregar fechas feriadas                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_agrega_fecha()
DEFINE v_fecha_feriada RECORD
         v_fecha       LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion LIKE cat_feriado_infonavit.feriado_desc
       END RECORD,
       r_ind_fecha     SMALLINT,
       r_confirma      BOOLEAN,
       v_continua      BOOLEAN

   OPEN WINDOW vtna_captura_fecha WITH FORM "CATM151"

      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()
    
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)         
         CALL v_ventana.setText(p_titulo_vtna)
      END IF
      
      LET v_continua = TRUE
      WHILE v_continua  
         DIALOG ATTRIBUTES (UNBUFFERED) 
            INPUT v_fecha_feriada.v_fecha, 
                  v_fecha_feriada.v_celebracion --WITHOUT DEFAULTS
             FROM captura_fecha, 
                  captura_celebracion --ATTRIBUTES(UNBUFFERED)
 
               AFTER FIELD captura_fecha
                  IF (v_fecha_feriada.v_fecha is NULL ) THEN 
                     CALL fn_mensaje("Aviso","Capture la fecha","information")
                     NEXT FIELD captura_fecha
                  ELSE 
                     CALL fn_verifica_fecha(v_fecha_feriada.v_fecha) RETURNING r_ind_fecha
                     CASE r_ind_fecha

                        WHEN 0
                           NEXT FIELD captura_celebracion
   
                        WHEN 1
                           CALL fn_mensaje("Aviso","Se esta capturando fecha anterior","information")
                           NEXT FIELD captura_celebracion

                        WHEN 2
                           CALL fn_mensaje("Aviso","Fecha ya existe","information")
                           NEXT FIELD captura_fecha

                     END CASE
                   END IF   

            END INPUT

            # Arreglo de fechas ya capturadas para el año en curso
            DISPLAY ARRAY v_dias_feriados TO sr_fechas_feriadas.*

            END DISPLAY
         

            BEFORE DIALOG
               CALL fn_recupera_dias_feriados_ejercicio()

            ON ACTION guardar
               IF(v_fecha_feriada.v_fecha IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture fecha","information")
                  NEXT FIELD captura_fecha
               END IF
               IF(v_fecha_feriada.v_celebracion IS NULL)THEN
                  CALL fn_mensaje("Aviso","Capture celebración","information")
                  NEXT FIELD captura_celebracion
               END IF
               CALL fn_ventana_confirma("Confimar","¿Guardar dia feriado?","question") RETURNING r_confirma
               IF(r_confirma)THEN
                  CALL fn_inserta_fecha_festiva(v_fecha_feriada.*)
                  CALL fn_mensaje("Aviso","Registro almacenado correctamente","information")
                  EXIT DIALOG
               ELSE
                  CONTINUE DIALOG
               END IF
               
            ON ACTION cancelar
               LET v_continua = FALSE
               EXIT DIALOG

         END DIALOG
      END WHILE

   CLOSE WINDOW vtna_captura_fecha

END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para consultar fechas feriadas                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_consulta_fechas()

   DEFINE v_criterios_busqueda RECORD
      v_fecha_inicio LIKE cat_feriado_infonavit.feriado_fecha,
      v_fecha_fin    LIKE cat_feriado_infonavit.feriado_fecha,
      v_celebracion  LIKE cat_feriado_infonavit.feriado_desc,
      v_ejercicio    CHAR(4)
   END RECORD,
   v_combo_ejercicio ui.ComboBox,
   v_continua       BOOLEAN

OPEN WINDOW vtna_consulta_fechas WITH FORM "CATM152"

   #Se asigna el titulo de la ventana
   LET v_ventana = ui.Window.getCurrent()
   LET v_forma = v_ventana.getForm()


   ##oculta tabla fechas feriadas
   CALL v_forma.setElementHidden("gpo_dias_inhabiles",1)

   IF(p_titulo_vtna IS NOT NULL)THEN
       CALL ui.Interface.setText(p_titulo_vtna)
       CALL v_ventana.setText(p_titulo_vtna)
    END IF

    INPUT v_criterios_busqueda.v_fecha_inicio,
          v_criterios_busqueda.v_fecha_fin,
          v_criterios_busqueda.v_celebracion,
          v_criterios_busqueda.v_ejercicio
     FROM captura_fecha_inicio,
          captura_fecha_fin,
          captura_celebracion,
          ejercicio ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
         # recupera el combobox del ejercicio
         LET v_combo_ejercicio = ui.ComboBox.forName("formonly.ejercicio")
         CALL fn_recupera_ejercicios_feriados(v_combo_ejercicio)

         AFTER FIELD captura_fecha_fin
            IF(v_criterios_busqueda.v_fecha_inicio > v_criterios_busqueda.v_fecha_fin)THEN
               CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
               NEXT FIELD captura_fecha_fin
            END IF

         ON ACTION consultar
            IF(v_criterios_busqueda.v_fecha_inicio > v_criterios_busqueda.v_fecha_fin)THEN
               CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
               NEXT FIELD captura_fecha_fin
            END IF

            CALL fn_recupera_fechas(v_criterios_busqueda.*) RETURNING v_continua

            IF NOT(v_continua)THEN
               CALL fn_mensaje("Aviso","No se encontraror registros con criterio dado","information")
               CONTINUE INPUT
            ELSE
               DISPLAY ARRAY v_dias_feriados TO sr_fechas_feriadas.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

                  BEFORE DISPLAY
                  # oculta el campo del chek box
                  CALL v_forma.setFieldHidden("seleccion",TRUE)

                  ##pone visible tabla fechas feriadas
                  CALL v_forma.setElementHidden("gpo_dias_inhabiles",0)

                  ## oculta gpo_conceptos_busqueda
                  CALL v_forma.setElementHidden("gpo_conceptos_busqueda",1)

                  ON ACTION CANCEL  
                     ## pone visible gpo_conceptos_busqueda
                     CALL v_forma.setElementHidden("gpo_conceptos_busqueda",0)
                     ##oculta tabla fechas feriadas
                     CALL v_forma.setElementHidden("gpo_dias_inhabiles",1)
                     EXIT DISPLAY 
                     
              END DISPLAY
            END IF

         ON ACTION cancelar
            LET v_continua = FALSE
            EXIT INPUT
      END INPUT

   CLOSE WINDOW vtna_consulta_fechas

END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para eliminar fechas feriadas                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_elimina_fechas()
DEFINE v_criterios_busqueda RECORD
         v_fecha_inicio LIKE cat_feriado_infonavit.feriado_fecha,
         v_fecha_fin    LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion  LIKE cat_feriado_infonavit.feriado_desc,
         v_ejercicio    CHAR(4)
       END RECORD,
       v_combo_ejercicio ui.ComboBox,
       v_continua       BOOLEAN,
       r_confirma       BOOLEAN,
       v_selecciono_registro BOOLEAN
       
OPEN WINDOW vtna_elimina_fechas WITH FORM "CATM152"

      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()

      # oculta el campo del chek box
      CALL v_forma.setFieldHidden("seleccion",FALSE)

      ##oculta tabla fechas feriadas
      CALL v_forma.setElementHidden("gpo_dias_inhabiles",1) 
      
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)         
         CALL v_ventana.setText(p_titulo_vtna)
      END IF

      INPUT v_criterios_busqueda.v_fecha_inicio,
            v_criterios_busqueda.v_fecha_fin,
            v_criterios_busqueda.v_celebracion,
            v_criterios_busqueda.v_ejercicio 
       FROM captura_fecha_inicio,
            captura_fecha_fin,
            captura_celebracion,
            ejercicio ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            # recupera el combobox del ejercicio
            LET v_combo_ejercicio = ui.ComboBox.forName("formonly.ejercicio")
            CALL fn_recupera_ejercicios_feriados(v_combo_ejercicio)

         AFTER FIELD captura_fecha_fin
            IF(v_criterios_busqueda.v_fecha_inicio > v_criterios_busqueda.v_fecha_fin)THEN
               CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
               NEXT FIELD captura_fecha_fin
            END IF

         ON ACTION aceptar
            IF(v_criterios_busqueda.v_fecha_inicio > v_criterios_busqueda.v_fecha_fin)THEN
               CALL fn_mensaje("Aviso","Periodo inicio no puede ser mayor a periodo fin","information")
               NEXT FIELD captura_fecha_fin
            END IF
            
            CALL fn_recupera_fechas(v_criterios_busqueda.*) RETURNING v_continua
            
            IF NOT(v_continua)THEN
               CALL fn_mensaje("Aviso","No se encontraror registros con criterio dado","information")
               CONTINUE INPUT
            ELSE 
               ##pone visible tabla fechas feriadas
               CALL v_forma.setElementHidden("gpo_dias_inhabiles",0)

               ## oculta gpo_conceptos_busqueda
               CALL v_forma.setElementHidden("gpo_conceptos_busqueda",1)
               
               INPUT ARRAY v_dias_feriados WITHOUT DEFAULTS FROM sr_fechas_feriadas.* 
               ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

                  BEFORE INPUT
                  # habilita columna de seleccion
                  CALL v_forma.setFieldHidden("seleccion",FALSE)

                  ON ACTION eliminar
                     LET v_selecciono_registro = FALSE

                     # Valida que se haya seleccionado algun registro
                     CALL fn_valida_seleccion() RETURNING v_selecciono_registro

                     IF NOT(v_selecciono_registro)THEN
                        CALL fn_mensaje("Aviso","Seleccione algún registro","information")
                        CONTINUE INPUT
                     END IF 
                  
                     CALL fn_ventana_confirma("Confimar","¿Eliminar registros seleccionados?","question") RETURNING r_confirma

                     IF(r_confirma)THEN
                        CALL fn_elimina_registros_fechas()
                        CALL fn_mensaje("Aviso","Registros eliminados correctamente","information")

                        ##pone visible tabla fechas feriadas
                        CALL v_forma.setElementHidden("gpo_dias_inhabiles",1)

                        ## oculta gpo_conceptos_busqueda
                        CALL v_forma.setElementHidden("gpo_conceptos_busqueda",0)
                        EXIT INPUT
                        
                     ELSE
                        CONTINUE INPUT
                     END IF

                  ON ACTION cancelar
                     ##pone visible tabla fechas feriadas
                     CALL v_forma.setElementHidden("gpo_dias_inhabiles",1)
                     ## oculta gpo_conceptos_busqueda
                     CALL v_forma.setElementHidden("gpo_conceptos_busqueda",0)
                     EXIT INPUT
               END INPUT
            END IF
            
         ON ACTION cancelar
            LET v_continua = FALSE
            EXIT INPUT
      END INPUT
CLOSE WINDOW vtna_elimina_fechas
   
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para recuperar los dias feriados del ejercicio   #
#                     actual                                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_recupera_dias_feriados_ejercicio()
DEFINE v_consulta      STRING,
       v_fecha_inicio  DATE,
       v_fecha_fin     DATE,
       v_anio_actual   CHAR(4),
       v_indice        INTEGER,
       v_dia_feriado   RECORD
         v_fecha       LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion LIKE cat_feriado_infonavit.feriado_desc
       END RECORD

   # Limpia arreglo
   CALL v_dias_feriados.clear()
   LET v_indice = 1
   # recupera fecha incio y fin
   LET v_anio_actual  = YEAR(TODAY)
   LET v_fecha_inicio = "01/01/"||v_anio_actual # primer dia del año
   LET v_fecha_fin    = "12/31/"||v_anio_actual # ultimo dia del año
   
   LET v_consulta = "\n SELECT feriado_fecha,",
                    "\n        feriado_desc",
                    "\n   FROM cat_feriado_infonavit",
                    "\n  WHERE feriado_fecha BETWEEN ? AND ?",
                    "\n  ORDER by feriado_fecha ASC"
   PREPARE prp_recupera_fechas_feriadas FROM v_consulta
   DECLARE cur_recupera_fechas_feriadas CURSOR FOR prp_recupera_fechas_feriadas
   FOREACH cur_recupera_fechas_feriadas USING v_fecha_inicio,
                                              v_fecha_fin
                                         INTO v_dia_feriado.*
                                         
      LET v_dias_feriados[v_indice].v_fecha       = v_dia_feriado.v_fecha
      LET v_dias_feriados[v_indice].v_celebracion = v_dia_feriado.v_celebracion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_fechas_feriadas
                    
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para verifica y valida la fecha feriada          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_verifica_fecha(p_fecha)
DEFINE p_fecha        DATE,
       v_existe_fecha BOOLEAN,
       v_consulta     STRING,
       v_ind_fecha    SMALLINT

   LET v_ind_fecha    = 0
   LET v_existe_fecha = 0
   
   LET v_consulta = "\n SELECT FIRST 1 1",
                    "\n   FROM cat_feriado_infonavit",
                    "\n  WHERE feriado_fecha = ?"
   PREPARE prp_existe_fecha FROM v_consulta
   EXECUTE prp_existe_fecha USING p_fecha
                             INTO v_existe_fecha

   IF(p_fecha < TODAY)THEN
      LET v_ind_fecha = 1 # se esta capturando fecha anterior
   END IF

   IF(v_existe_fecha)THEN
      LET v_ind_fecha = 2 # ya existe la fecha
   END IF

   RETURN v_ind_fecha
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para verifica y valida la fecha feriada          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_inserta_fecha_festiva(p_dia_feriado)
DEFINE p_dia_feriado   RECORD
         v_fecha       LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion LIKE cat_feriado_infonavit.feriado_desc
       END RECORD,
       v_fecha_actual  DATE

   LET v_fecha_actual = TODAY
   
   INSERT INTO cat_feriado_infonavit(feriado_fecha,
                           feriado_desc,
                           f_proceso,
                           usuario)
   VALUES(p_dia_feriado.v_fecha,
          p_dia_feriado.v_celebracion,
          v_fecha_actual,
          p_usuario_cod)

END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para recuperar los ejercicios registrados con    #
#                     fechas feriadas                                          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_recupera_ejercicios_feriados(p_combo_ejercicio)
DEFINE p_combo_ejercicio ui.ComboBox,
       v_consulta        STRING,
       v_ejercicio       CHAR(4)

   CALL p_combo_ejercicio.clear()

   LET v_consulta = "\n SELECT UNIQUE YEAR(feriado_fecha)",
                    "\n   FROM cat_feriado_infonavit",
                    "\n  WHERE 1 = 1",
                    "\n  ORDER BY 1 ASC"
   PREPARE prp_recupera_ejercicios_feriados FROM v_consulta
   DECLARE cur_recupera_ejercicios_feriados CURSOR FOR prp_recupera_ejercicios_feriados
   FOREACH cur_recupera_ejercicios_feriados INTO v_ejercicio

      CALL p_combo_ejercicio.addItem(v_ejercicio,v_ejercicio)

   END FOREACH
   FREE cur_recupera_ejercicios_feriados
   
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para eliminar fechas feriadas                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_elimina_registros_fechas()
DEFINE v_consulta STRING,
       v_indice   INTEGER

   LET v_consulta = "\n DELETE ",
                    "\n   FROM cat_feriado_infonavit",
                    "\n  WHERE feriado_fecha = ?"
   PREPARE prp_elimina_fechas FROM v_consulta
   
   FOR v_indice = 1 TO v_dias_feriados.getLength()
      # se eliminan solo los registros selecionados
      IF(v_dias_feriados[v_indice].v_seleccion = TRUE)THEN
         EXECUTE prp_elimina_fechas USING v_dias_feriados[v_indice].v_fecha 
      END IF
      
   END FOR
   
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       => Funcion para recuperar fechas feriadas                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_recupera_fechas(p_criterios_busqueda)
DEFINE p_criterios_busqueda RECORD
         v_fecha_inicio LIKE cat_feriado_infonavit.feriado_fecha,
         v_fecha_fin    LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion  LIKE cat_feriado_infonavit.feriado_desc,
         v_ejercicio    CHAR(4)
       END RECORD,
       v_filtro         STRING,
       v_consulta       STRING,
       v_dia_feriado    RECORD
         v_fecha        LIKE cat_feriado_infonavit.feriado_fecha,
         v_celebracion  LIKE cat_feriado_infonavit.feriado_desc
       END RECORD,
       v_indice          INTEGER,
       v_recupero_fechas BOOLEAN

   IF(p_criterios_busqueda.v_fecha_inicio IS NULL AND 
      p_criterios_busqueda.v_fecha_fin    IS NULL AND 
      p_criterios_busqueda.v_celebracion  IS NULL AND 
      p_criterios_busqueda.v_ejercicio    IS NULL)THEN

      LET v_filtro = " 1 = 1 "
   ELSE
      LET v_filtro = " "
      # Periodo
      IF(p_criterios_busqueda.v_fecha_inicio IS NOT NULL)THEN
         IF(p_criterios_busqueda.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," feriado_fecha BETWEEN '",p_criterios_busqueda.v_fecha_inicio,"' AND '",p_criterios_busqueda.v_fecha_fin,"' AND "  
         ELSE
            LET v_filtro = v_filtro," feriado_fecha = '",p_criterios_busqueda.v_fecha_inicio,"' AND "
         END IF
      ELSE
         IF(p_criterios_busqueda.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro," feriado_fecha = '",p_criterios_busqueda.v_fecha_fin,"' AND "
         END IF
      END IF
      # Celebración
      IF(p_criterios_busqueda.v_celebracion IS NOT NULL)THEN
         LET v_filtro = v_filtro," feriado_desc = '",p_criterios_busqueda.v_celebracion,"' AND "
      END IF

      # Ejercicio
      IF(p_criterios_busqueda.v_ejercicio IS NOT NULL)THEN
         LET v_filtro = v_filtro," YEAR(feriado_fecha) = '",p_criterios_busqueda.v_ejercicio,"' AND "
      END IF
      
      # elimina el ultimo AND
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-4)
   END IF
   
   # Limpia arreglo a desplegar
   CALL v_dias_feriados.clear()

   LET v_indice = 1
   
   LET v_consulta = "\n SELECT feriado_fecha,",
                    "\n        feriado_desc",
                    "\n   FROM cat_feriado_infonavit",
                    "\n  WHERE ",v_filtro,
                    "\n  ORDER BY feriado_fecha ASC"
                    
   PREPARE prp_recuper_fechas FROM v_consulta
   DECLARE cur_recuper_fechas CURSOR FOR prp_recuper_fechas
   FOREACH cur_recuper_fechas INTO v_dia_feriado.*
      LET v_dias_feriados[v_indice].v_seleccion       = 0
      LET v_dias_feriados[v_indice].v_fecha       = v_dia_feriado.v_fecha
      LET v_dias_feriados[v_indice].v_celebracion = v_dia_feriado.v_celebracion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recuper_fechas
   IF( v_dias_feriados.getLength() > 0 )THEN
      LET v_recupero_fechas = TRUE
   ELSE
      LET v_recupero_fechas = FALSE
   END IF
   RETURN v_recupero_fechas
END FUNCTION

################################################################################
#Modulo            => CAT                                                      #
#Programa          => CATM15                                                   #
#Descripcion       =>                                                          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 SEPTIEMBRE 2016                                       #
################################################################################
FUNCTION fn_valida_seleccion()
DEFINE v_indice       INTEGER,
       v_seleccionado BOOLEAN

   LET v_seleccionado = FALSE
   FOR v_indice = 1 TO v_dias_feriados.getLength()
      
      IF(v_dias_feriados[v_indice].v_seleccion = TRUE)THEN
         LET v_seleccionado = TRUE
         EXIT FOR
      END IF
      
   END FOR
   
   RETURN v_seleccionado
END FUNCTION