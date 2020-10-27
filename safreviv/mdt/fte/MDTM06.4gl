--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => CATALOGO DE ALTA DE PRELACIÓN (MODIFICACIÓN)            #
#Autor             => Francisco López                                         #
#Fecha Inicio      => 13/02/2012                                              #
###############################################################################
DATABASE safre_viv
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod -- codigo del proceso
      ,g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
      ,p_usuario_cod LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_ruta_bin    LIKE seg_modulo.ruta_bin,
       v_catPrelacion RECORD LIKE mdt_cat_prelacion.*

--Array para la tabla de los tipos de prelacion que se van a modificar
DEFINE v_arr_mod_prelacion DYNAMIC ARRAY OF RECORD
        v_orden            INTEGER,
        tpo_prelacion      LIKE mdt_det_prelacion.tpo_prelacion,
        tpo_mandato        LIKE mdt_det_prelacion.tpo_mandato,
        desc_tpo_mandato   LIKE mdt_tpo_mandato.desc_tpo_mandato,
        prelacion          LIKE mdt_det_prelacion.prelacion
       END RECORD,
       v_ventana         ui.Window

# Objetivo:
MAIN
DEFINE p_tipo_ejecucion  SMALLINT, -- forma como ejecutara el programa
       p_titulo          STRING,   -- titulo de la ventana
       v_tpo_prelacion   LIKE mdt_cat_prelacion.tpo_prelacion,
       v_bandera_tabla   BOOLEAN,
       v_bnd_continuar   BOOLEAN,
       --v_indice          INTEGER,
       cb                ui.ComboBox,  --Tipo Combobox
       v_prelacion       RECORD
        tpo_prelacion    LIKE mdt_det_prelacion.tpo_prelacion,
        tpo_mandato      LIKE mdt_det_prelacion.tpo_mandato,
        desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
        prelacion        LIKE mdt_det_prelacion.prelacion
       END RECORD,
       v_confirma        BOOLEAN,
       v_desc_prelacion  LIKE mdt_cat_prelacion.desc_prelacion
   
   -- se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)
   LET v_tpo_prelacion  = ARG_VAL(4)

   ################################
   # Valor de prueba
   --LET v_tpo_prelacion = 1
   #################################

   IF NOT fn_verifica_privilegios("MDTM06",p_usuario_cod) THEN
      CALL fn_mensaje("Advertencia","No cuenta con privilegios para esta opción","error")
      EXIT PROGRAM
   END IF

   # Recupera la ruta de los archivos ejecutables para mandatos
   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   OPEN WINDOW w_mod_prelacion WITH FORM g_ruta_bin CLIPPED||"/MDTM061"

   IF(p_titulo IS NOT NULL)THEN
      CALL ui.Interface.setText(p_titulo)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_titulo)
   END IF

      
      --INPUT v_tpo_prelacion FROM cb_tipo_prelacion ATTRIBUTES (UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE)
      --   BEFORE INPUT
      --      LET v_bnd_continuar = FALSE
      --      LET cb = ui.ComboBox.forName("formonly.cb_tipo_prelacion")
      --      CALL f_llena_combo_prelacion(cb)
            
      --   ON ACTION Aceptar
      --      IF v_tpo_prelacion IS NOT NULL THEN 
      --         CALL f_llena_tabla_prelacion(v_tpo_prelacion)
      --         IF v_arr_mod_prelacion.getLength() > 0 THEN
      --            #Si se encontraron resulados se realiza el otro input
      --            LET v_bnd_continuar = TRUE
      --            EXIT INPUT
      --         ELSE
      --            CONTINUE INPUT
      --         END IF
      --      ELSE
      --         ERROR "Seleccione Prelación" ATTRIBUTE(REVERSE, BOLD, YELLOW)
      --         CONTINUE INPUT
      --      END IF
      --      EXIT INPUT
            
      --   ON ACTION Cancelar
      --      LET v_bnd_continuar = FALSE
      --      # Regresa el control al menu
      --      EXIT INPUT
            
      --END INPUT
   
      --IF(v_bnd_continuar)THEN
         --Si se activo la bandera para la captura de la tabla se muetsra el input
         DISPLAY ARRAY v_arr_mod_prelacion TO tb_prelacion_mod.*
                                           ATTRIBUTES (UNBUFFERED, CANCEL = FALSE, ACCEPT = FALSE)
            BEFORE DISPLAY

               # recupera la descripcion del tipo de prelacion
               SELECT desc_prelacion
                 INTO v_desc_prelacion
                 FROM mdt_cat_prelacion
                WHERE tpo_prelacion = v_tpo_prelacion
               # desplega el tipo de prelacion
               DISPLAY v_desc_prelacion TO edi_tipo_prelacion
               # Recupera la prelacion del tipo de prelacion
               CALL f_llena_tabla_prelacion(v_tpo_prelacion)
               --LET v_bnd_continuar = FALSE
               --LET v_indice = 0
               LET v_confirma = FALSE 
            
            ON ACTION Subir
               # sube solo si el superior es mayor o igual a 1
               IF((ARR_CURR()-1) >= 1)THEN
                  # se recupera el registro que se bajara
                  LET v_prelacion.desc_tpo_mandato = v_arr_mod_prelacion[ARR_CURR()-1].desc_tpo_mandato
                  LET v_prelacion.tpo_mandato      = v_arr_mod_prelacion[ARR_CURR()-1].tpo_mandato
                  LET v_prelacion.tpo_prelacion    = v_arr_mod_prelacion[ARR_CURR()-1].tpo_prelacion
                  LET v_prelacion.prelacion        = v_arr_mod_prelacion[ARR_CURR()-1].prelacion
                  # se sube el registro actual
                  LET v_arr_mod_prelacion[ARR_CURR()-1].desc_tpo_mandato = v_arr_mod_prelacion[ARR_CURR()].desc_tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()-1].tpo_mandato      = v_arr_mod_prelacion[ARR_CURR()].tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()-1].tpo_prelacion    = v_arr_mod_prelacion[ARR_CURR()].tpo_prelacion                                    
                  LET v_arr_mod_prelacion[ARR_CURR()-1].prelacion        = v_arr_mod_prelacion[ARR_CURR()].prelacion
                  # se baja el registro recuperado
                  LET v_arr_mod_prelacion[ARR_CURR()].desc_tpo_mandato = v_prelacion.desc_tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()].tpo_mandato      = v_prelacion.tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()].tpo_prelacion    = v_prelacion.tpo_prelacion
                  LET v_arr_mod_prelacion[ARR_CURR()].prelacion        = v_prelacion.prelacion

                  CALL FGL_SET_ARR_CURR( ARR_CURR()-1)

                  CONTINUE DISPLAY
               END IF

            ON ACTION Bajar
               # baja solo si el inferior es menor o igual aal tamño del arreglo
               IF((ARR_CURR()+1) <= v_arr_mod_prelacion.getLength())THEN
                  # se recupera el registro que se subira
                  LET v_prelacion.desc_tpo_mandato = v_arr_mod_prelacion[ARR_CURR()+1].desc_tpo_mandato
                  LET v_prelacion.tpo_mandato      = v_arr_mod_prelacion[ARR_CURR()+1].tpo_mandato
                  LET v_prelacion.tpo_prelacion    = v_arr_mod_prelacion[ARR_CURR()+1].tpo_prelacion
                  LET v_prelacion.prelacion        = v_arr_mod_prelacion[ARR_CURR()+1].prelacion
                  # se baja el registro actual
                  LET v_arr_mod_prelacion[ARR_CURR()+1].desc_tpo_mandato = v_arr_mod_prelacion[ARR_CURR()].desc_tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()+1].tpo_mandato      = v_arr_mod_prelacion[ARR_CURR()].tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()+1].tpo_prelacion    = v_arr_mod_prelacion[ARR_CURR()].tpo_prelacion                  
                  LET v_arr_mod_prelacion[ARR_CURR()+1].prelacion        = v_arr_mod_prelacion[ARR_CURR()].prelacion
                  # se sube el registro recuperado
                  LET v_arr_mod_prelacion[ARR_CURR()].desc_tpo_mandato = v_prelacion.desc_tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()].tpo_mandato      = v_prelacion.tpo_mandato
                  LET v_arr_mod_prelacion[ARR_CURR()].tpo_prelacion    = v_prelacion.tpo_prelacion                  
                  LET v_arr_mod_prelacion[ARR_CURR()].prelacion        = v_prelacion.prelacion

                  CALL FGL_SET_ARR_CURR( ARR_CURR()+1)
                  
                  CONTINUE DISPLAY
               END IF

            ON ACTION Aceptar
               CALL fn_ventana_confirma(p_titulo,"¿Desea continuar con la modificación?","about")
                                        RETURNING v_confirma
               IF(v_confirma)THEN
                  CALL f_modifica_orden_prelacion() RETURNING v_confirma
                  IF(v_confirma)THEN
                     CALL fn_mensaje(p_titulo,"Modificación a la Prelación Efectuada","about")
                     LET v_bnd_continuar = FALSE
                     ACCEPT DISPLAY
                  ELSE
                     CALL fn_mensaje(p_titulo,"Ocurrió un error al actualizar la información","about")
                     LET v_bnd_continuar = TRUE
                     EXIT DISPLAY
                  END IF
               ELSE
                  CONTINUE DISPLAY
               END IF
               
            ON ACTION Cancelar
               CALL fn_ventana_confirma(p_titulo,"Se perderan los cambios que no se hayan almacenado\n¿Desea salir?","about")
                                        RETURNING v_confirma
               IF(v_confirma)THEN
                  CALL v_arr_mod_prelacion.clear()
                  # Devuelve el flujo a la seleccion de la prelacion
                  --LET v_bnd_continuar = TRUE
                  EXIT DISPLAY
               ELSE
                  CONTINUE DISPLAY
               END IF
               
         END DISPLAY
         
      --END IF

   CLOSE WINDOW w_mod_prelacion
END MAIN
###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => Llenar el array que se desplegara para ser modificada   #
#                     la info                                                 #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_llena_tabla_prelacion(v_tpo_prelacion)
DEFINE v_tpo_prelacion   LIKE mdt_cat_prelacion.tpo_prelacion,
       v_indice          INTEGER,
       v_prelacion       RECORD
        tpo_prelacion    LIKE mdt_det_prelacion.tpo_prelacion,
        tpo_mandato      LIKE mdt_det_prelacion.tpo_mandato,
        desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
        prelacion        LIKE mdt_det_prelacion.prelacion
       END RECORD      
   
   DECLARE cur_tabla_prelacion CURSOR FOR
   SELECT a.tpo_prelacion, a.tpo_mandato ,b.desc_tpo_mandato, a.prelacion
     FROM mdt_det_prelacion AS a, mdt_tpo_mandato AS b 
    WHERE tpo_prelacion = v_tpo_prelacion 
      AND b.tpo_mandato = a.tpo_mandato
    ORDER BY prelacion

   LET v_indice = 1
   CALL v_arr_mod_prelacion.clear()
   FOREACH cur_tabla_prelacion INTO v_prelacion.*
      # v_orden indica como es que va a quedar el orden
      LET v_arr_mod_prelacion[v_indice].v_orden          = v_indice
      LET v_arr_mod_prelacion[v_indice].tpo_prelacion    = v_prelacion.tpo_prelacion
      LET v_arr_mod_prelacion[v_indice].tpo_mandato      = v_prelacion.tpo_mandato
      LET v_arr_mod_prelacion[v_indice].desc_tpo_mandato = v_prelacion.desc_tpo_mandato
      LET v_arr_mod_prelacion[v_indice].prelacion        = v_prelacion.prelacion
      LET v_indice = v_indice + 1
   END FOREACH

   --Se elimina el ultimo registro si está nulo
   IF v_arr_mod_prelacion[v_arr_mod_prelacion.getLength()].prelacion IS NULL THEN
      CALL v_arr_mod_prelacion.deleteElement(v_arr_mod_prelacion.getLength())
   END IF
   
   FREE cur_tabla_prelacion
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => Llenar el combo que se desplegara seleccionar la        #
#                     prelacion a modificar                                   #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 13/02/2012                                              #
###############################################################################
FUNCTION f_llena_combo_prelacion(cb)
DEFINE cb ui.ComboBox  --Tipo Combobox

   CALL cb.clear()
   # Se llena el combo con los datos   
   DECLARE cur_tipo_mandato CURSOR FOR
   SELECT * FROM mdt_cat_prelacion

   FOREACH cur_tipo_mandato INTO v_catPrelacion.tpo_prelacion, v_catPrelacion.desc_prelacion
      CALL cb.addItem(v_catPrelacion.tpo_prelacion,v_catPrelacion.desc_prelacion)
   END FOREACH
   FREE cur_tipo_mandato
   
END FUNCTION

###############################################################################
#Modulo            => MDT                                                     #
#Programa          => MDTM06                                                  #
#Objetivo          => Modifica el orden de la prelacion                       #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 13/02/2012                                              #
###############################################################################
FUNCTION f_modifica_orden_prelacion()
DEFINE v_indice   INTEGER,
       v_consulta STRING

   WHENEVER ERROR CONTINUE

   LET v_consulta = "\n DELETE safre_viv:mdt_det_prelacion ",
                    "\n  WHERE tpo_prelacion = ?",
                    "\n    AND tpo_mandato = ?",
                    "\n    AND prelacion = ?"
   PREPARE prp_elimina_orden_prelacion FROM v_consulta
   # Actualiza el orden de la prelacion
   FOR v_indice = 1 TO v_arr_mod_prelacion.getLength()
      EXECUTE prp_elimina_orden_prelacion
        USING v_arr_mod_prelacion[v_indice].tpo_prelacion,
              v_arr_mod_prelacion[v_indice].tpo_mandato,
              v_arr_mod_prelacion[v_indice].prelacion       
   END FOR
   LET v_consulta = "\n INSERT INTO safre_viv:mdt_det_prelacion ",
                    "\n    VALUES(?,?,?)"
                    
   PREPARE prp_inserta_orden_prelacion FROM v_consulta
   # Actualiza el orden de la prelacion
   FOR v_indice = 1 TO v_arr_mod_prelacion.getLength()
      EXECUTE prp_inserta_orden_prelacion 
        USING v_arr_mod_prelacion[v_indice].tpo_prelacion, 
              v_arr_mod_prelacion[v_indice].tpo_mandato,
              v_arr_mod_prelacion[v_indice].v_orden
        
   END FOR
   IF(SQLCA.SQLCODE < 0)THEN
      RETURN FALSE
   ELSE
      RETURN TRUE
   END IF
END FUNCTION