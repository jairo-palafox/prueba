--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05-07-2013
--===============================================================

####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSC02                                        #
#Objetivo          =>Consulta de mandatos registrados              #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>09 Febrero 2012                               #
####################################################################
IMPORT os
DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      STRING,             -- nombre del programa
       p_v_tpo_mdt       SMALLINT                 -- tipo de mandato, opcional
DEFINE cmbmdt            LIKE mdt_cat_mandato.id_cat_mandato
DEFINE cmbtpomdt         LIKE mdt_tpo_mandato.tpo_mandato
DEFINE v_arr_mandato     DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          etiqueta          LIKE mdt_cat_gpo_etiqueta.etiqueta,
          valor_etiqueta    LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD
DEFINE v_arr_mandAux     RECORD -- arreglo que contiene los mandatos
          etiqueta          LIKE mdt_cat_gpo_etiqueta.etiqueta,
          valor_etiqueta    LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD
DEFINE v_arr_imagen      DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          nombre_imagen     CHAR(200),
          desc_imagen       CHAR(40)
       END RECORD
DEFINE v_arr_imgAux      RECORD -- arreglo que contiene los mandatos
          nombre_imagen     CHAR(200),
          desc_imagen       CHAR(40)
       END RECORD
DEFINE v_ejecuta         STRING
DEFINE vstatus           SMALLINT
DEFINE tree DYNAMIC ARRAY OF RECORD
          name STRING,
          pid STRING,
          id STRING,
          idx INT,
          expanded BOOLEAN,
          municipio STRING
       END RECORD,
       v_ventana    ui.Window,
       v_forma      ui.Form,

       v_paquetes DYNAMIC ARRAY OF RECORD
         v_num     INTEGER,
         v_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
         v_cve_mandato LIKE mdt_cat_mandato_paquete.cve_mandato
       END RECORD,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

MAIN
DEFINE v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato,
       v_tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
       # variables para el filtrado de mandatos
       v_id_tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
       v_minucipio        LIKE cat_municipio.municipio_desc,
       v_entidad_fed      LIKE cat_entidad_federativa.entidad_desc_larga,
       v_filtro           STRING,
       v_bnd_salir        BOOLEAN,
       v_rec_datos        BOOLEAN,
       v_modifica_mdt     BOOLEAN,
       v_abonos           STRING,
       v_pagos            STRING,
       r_filtro1          STRING,
       r_filtro2          STRING,
       r_filtro3          STRING
       

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)
   LET p_v_tpo_mdt    = ARG_VAL(4)

   SELECT ruta_bin
    INTO v_ruta_ejecutable
    FROM seg_modulo
   WHERE modulo_cod = "hps" 

   LET v_abonos = "313,323,333" # Movimientos para abonos
   LET v_pagos  = "314,324,334" # Movimientos para pagos
   --CALL ui.Interface.loadStyles("mdtstyle")
   
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF
   LET v_modifica_mdt = TRUE

   OPEN WINDOW w_arbol_mandato WITH FORM "HPSC022"

   IF(p_v_nom_prog IS NOT NULL)THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   WHILE v_modifica_mdt 
      DISPLAY ARRAY tree TO scr_tree.*
                    ATTRIBUTES (UNBUFFERED, KEEP CURRENT ROW =TRUE, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE DISPLAY
            LET v_rec_datos = TRUE
            WHILE v_rec_datos 
               # Filtra la consulta de mandatos
               IF(p_v_tpo_mdt IS NULL OR LENGTH(p_v_tpo_mdt CLIPPED) = 0 OR p_v_tpo_mdt = 0)THEN
                  --CALL fn_filtra_consulta_mandatos() RETURNING v_filtro
                  CALL fn_filtra_consulta_mandatos() RETURNING v_id_tpo_mandato,v_minucipio,v_entidad_fed,v_bnd_salir
                  IF(v_bnd_salir)THEN
                     LET v_modifica_mdt = FALSE
                     EXIT DISPLAY
                  END IF
               ELSE
                  LET v_filtro = "1 = 1"
               END IF
               # funcion que recupera información
               CALL fn_carga_arbol(p_v_tpo_mdt,v_id_tpo_mandato,v_minucipio,v_entidad_fed) RETURNING r_filtro1, r_filtro2,r_filtro3
               IF(tree.getLength() > 0)THEN
                  LET v_rec_datos = FALSE
               ELSE
                  CALL fn_mensaje("AVISO","No se encontraron registros con criterio dado","INFORMATION")
               END IF
            END WHILE
            IF p_v_tpo_mdt IS NULL OR LENGTH(p_v_tpo_mdt CLIPPED) = 0 OR p_v_tpo_mdt = 0 THEN
         
               IF fn_verifica_privilegios("MDTM01",p_v_usuario) THEN
                  CALL DIALOG.setActionHidden( "alta", FALSE)
               ELSE
                  CALL DIALOG.setActionHidden( "alta", TRUE)
               END IF
               IF fn_verifica_privilegios("MDTM03",p_v_usuario) THEN
                  CALL DIALOG.setActionHidden( "baja", FALSE)
               ELSE
                  CALL DIALOG.setActionHidden( "baja", TRUE)
               END IF
               IF fn_verifica_privilegios("MDTM04",p_v_usuario) THEN
                  CALL DIALOG.setActionHidden( "modificar", FALSE)
               ELSE
                  CALL DIALOG.setActionHidden( "modificar", TRUE)
               END IF
         
            ELSE
               CALL DIALOG.setActionHidden( "alta", TRUE)
               CALL DIALOG.setActionHidden( "baja", TRUE)
               CALL DIALOG.setActionHidden( "modificar", TRUE)
            END IF

         ON ACTION alta
      
            IF tree[ARR_CURR()].pid IS NULL OR tree[ARR_CURR()].pid = 0 THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el tipo de mandato","error")
            ELSE
               IF tree[ARR_CURR()].pid <> 99 THEN
                  LET v_ejecuta = "fglrun HPSM01 ", p_v_usuario CLIPPED," ",
                                                    p_b_tipo_carga," ",
                                                    "Alta ", 
                                                    tree[ARR_CURR()].pid
                  DISPLAY "EJECUTA Alta: ",v_ejecuta CLIPPED
                  RUN v_ejecuta RETURNING vstatus  
               ELSE
                  IF tree[ARR_CURR()].id IS NULL OR tree[ARR_CURR()].id = 0 THEN
                     CALL fn_mensaje("Advertencia","Es requerido que seleccione el tipo de mandato","error")
                  ELSE
                     LET v_ejecuta = "fglrun HPSM01 ", p_v_usuario CLIPPED," ",
                                                       p_b_tipo_carga," ",
                                                       "Alta ", 
                                                       tree[ARR_CURR()].id
                     DISPLAY "EJECUTA Alta: ",v_ejecuta CLIPPED
                     RUN v_ejecuta RETURNING vstatus
                  END IF
               END IF
            END IF
            CALL fn_carga_arbol(p_v_tpo_mdt,v_id_tpo_mandato,v_minucipio,v_entidad_fed) RETURNING r_filtro1, r_filtro2,r_filtro3
         
         ON ACTION baja
            IF (tree[ARR_CURR()].pid IS NULL OR tree[ARR_CURR()].pid = 0) OR
               (tree[ARR_CURR()].id IS NULL OR tree[ARR_CURR()].id = 0) OR 
               tree[ARR_CURR()].pid = 99 THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el mandato","error")
            ELSE
               LET v_ejecuta = "fglrun HPSM03 ", p_v_usuario CLIPPED," ",
                                                 p_b_tipo_carga," ",
                                                 "Baja ", 
                                                 tree[ARR_CURR()].pid, " ",
                                                 tree[ARR_CURR()].id
               DISPLAY "EJECUTA Baja: ",v_ejecuta CLIPPED
               RUN v_ejecuta RETURNING vstatus  
            
               CALL fn_carga_arbol(p_v_tpo_mdt,v_id_tpo_mandato,v_minucipio,v_entidad_fed) RETURNING r_filtro1, r_filtro2,r_filtro3
            
            END IF

         ON ACTION modificar
            IF (tree[ARR_CURR()].pid IS NULL OR tree[ARR_CURR()].pid = 0) OR
               (tree[ARR_CURR()].id IS NULL OR tree[ARR_CURR()].id = 0) OR 
               tree[ARR_CURR()].pid = 99 THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el mandato","error")
            ELSE
               LET v_ejecuta = "fglrun HPSM04 ", p_v_usuario CLIPPED," ",
                                                 p_b_tipo_carga," ",
                                                 "Modificar ", 
                                                 tree[ARR_CURR()].pid, " ",
                                                 tree[ARR_CURR()].id
               DISPLAY "EJECUTA Modif: ",v_ejecuta CLIPPED
               RUN v_ejecuta RETURNING vstatus  
            END IF
            CALL fn_carga_arbol(p_v_tpo_mdt,v_id_tpo_mandato,v_minucipio,v_entidad_fed) RETURNING r_filtro1, r_filtro2,r_filtro3

         ON ACTION paquete
            INITIALIZE v_desc_tpo_mandato TO NULL
            LET v_tpo_mandato = tree[ARR_CURR()].pid 
            SELECT desc_tpo_mandato 
              INTO v_desc_tpo_mandato
              FROM mdt_tpo_mandato
             WHERE tpo_mandato = v_tpo_mandato 
            IF(v_desc_tpo_mandato = "MANTENIMIENTO" OR
               v_desc_tpo_mandato = "SERVICIOS" )THEN  #se captura paquete solo si es tpo_mandato = 'mantenimiento'
               CALL fn_captura_paquete(tree[ARR_CURR()].id, v_tpo_mandato)
            ELSE
               CALL fn_mensaje("","Solo tipo mantenimiento","information")
            END IF
         
         ON ACTION consultar
            IF (tree[ARR_CURR()].pid IS NULL OR tree[ARR_CURR()].pid = 0) OR
               (tree[ARR_CURR()].id IS NULL OR tree[ARR_CURR()].id = 0) OR 
               tree[ARR_CURR()].pid = 99 THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el mandato","error")
            ELSE
               CALL fn_consulta_detalle(tree[ARR_CURR()].pid, tree[ARR_CURR()].id) 
            END IF

         ON ACTION movimientos
            IF( tree[ARR_CURR()].id = 0 OR tree[ARR_CURR()].id = 99 OR
                tree[ARR_CURR()].id IS NULL )THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el mandato","error")
            ELSE
               IF(tree[ARR_CURR()].pid = 99)THEN # registro seleccionado es un grupo de mandato
                  # se muestra consulta global ya que seleccionó un grupo de mandato
                  CALL fn_consulta_global_mov_mandato(0,tree[ARR_CURR()].id,v_abonos,v_pagos,r_filtro1, r_filtro2,r_filtro3)
               ELSE
                  CALL fn_tipo_consulta_mov(tree[ARR_CURR()].id, tree[ARR_CURR()].pid, v_abonos,v_pagos,r_filtro1, r_filtro2,r_filtro3)
               END IF
            END IF

         ON ACTION recurrencia
            IF( tree[ARR_CURR()].id = 0 OR tree[ARR_CURR()].id = 99 OR
                tree[ARR_CURR()].id IS NULL OR tree[ARR_CURR()].pid = 99)THEN
               CALL fn_mensaje("Advertencia","Es requerido que seleccione el mandato","error")
            ELSE
               CALL fn_configura_ejecucion_mandato(tree[ARR_CURR()].id, tree[ARR_CURR()].pid)
            END IF
      
         ON ACTION CLOSE
            EXIT DISPLAY
      
      END DISPLAY
      CALL tree.clear()
   END WHILE 

   CLOSE WINDOW w_arbol_mandato
   
END MAIN

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para configuracion de ejecución      #
#                     batch de mandatos                            # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_configura_ejecucion_mandato(p_id_cat_mandato,p_tpo_mandato)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_tpo_mandato    LIKE mdt_tpo_mandato.tpo_mandato,
       v_configuracion RECORD
         v_recurrencia    SMALLINT,
         v_tipo           SMALLINT,
         v_descripcion    VARCHAR(10),
         v_tipo_ejecucion SMALLINT
       END RECORD,
       v_cb_recurrencia    ui.ComboBox,
       v_cb_tipo           ui.ComboBox,
       v_cb_descripcion    ui.ComboBox,
       v_cb_tipo_ejecucion ui.ComboBox,
       r_tpo_mandato_desc   VARCHAR(20),
       r_mandato_desc       VARCHAR(40),
       r_confirma           BOOLEAN,
       r_error_sql          BOOLEAN
       

   OPEN WINDOW vtna_configuracion WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC027" ATTRIBUTES(STYLE = "dialog")

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()
      IF(p_v_nom_prog IS NOT NULL)THEN
         CALL ui.Interface.setText(p_v_nom_prog)         
         CALL v_ventana.setText(p_v_nom_prog)
      END IF

      

      INPUT v_configuracion.v_recurrencia,
            v_configuracion.v_tipo,
            v_configuracion.v_descripcion,
            v_configuracion.v_descripcion,
            v_configuracion.v_tipo_ejecucion 
       FROM cb_recurrencia,
            cb_tipo,
            cb_descripcion,
            dtedi_descripcion,
            cb_tipo_ejecucion ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE INPUT
            CALL fn_recupera_descripciones_mandato(p_tpo_mandato, p_id_cat_mandato) RETURNING r_tpo_mandato_desc, r_mandato_desc
            DISPLAY r_tpo_mandato_desc TO tpo_mandato
            DISPLAY r_mandato_desc     TO mandato

            CALL fn_recupera_configuracion(p_id_cat_mandato) RETURNING v_configuracion.*
            
            LET v_cb_recurrencia    = ui.ComboBox.forName("formonly.cb_recurrencia")
            CALL fn_llena_cb_recurrencia(v_cb_recurrencia)
      
            LET v_cb_tipo           = ui.ComboBox.forName("formonly.cb_tipo")
            CALL fn_llena_cb_tipo(v_cb_tipo, v_configuracion.v_recurrencia)
      
            LET v_cb_descripcion    = ui.ComboBox.forName("formonly.cb_descripcion")
            CALL fn_recupera_descripcion(v_configuracion.v_recurrencia, v_configuracion.v_tipo,v_cb_descripcion)
      
            LET v_cb_tipo_ejecucion = ui.ComboBox.forName("formonly.cb_tipo_ejecucion")
            CALL fn_llena_cb_tipo_ejecucion(v_cb_tipo_ejecucion,v_configuracion.v_recurrencia)

            CASE 
               WHEN v_configuracion.v_recurrencia = 1
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)
                  CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)
                  CALL v_forma.setFieldHidden("cb_tipo",TRUE)
                  CALL v_forma.setElementHidden("lbl_tipo",TRUE)
                  
               WHEN v_configuracion.v_recurrencia = 9 OR v_configuracion.v_recurrencia = 10
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",FALSE)
                  CALL v_forma.setFieldHidden("cb_tipo",FALSE)
                  CALL v_forma.setElementHidden("lbl_tipo",FALSE)
                  IF(v_configuracion.v_tipo = 4)THEN # Fecha
                     CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)

                     CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  ELSE
                     CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)

                     CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
                  END IF

               OTHERWISE
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)
                  CALL v_forma.setFieldHidden("cb_tipo",FALSE)
                  CALL v_forma.setElementHidden("lbl_tipo",FALSE)
                  IF(v_configuracion.v_tipo = 4)THEN # Fecha
                     CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)

                     CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  ELSE
                     CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)

                     CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
                  END IF
                  
            END CASE
            
            
         AFTER FIELD cb_recurrencia
            IF(v_configuracion.v_recurrencia IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture recurrencia","about")
               NEXT FIELD cb_recurrencia
            END IF

         {AFTER FIELD cb_tipo
            IF(v_configuracion.v_tipo IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture tipo","about")
               NEXT FIELD cb_tipo
            END IF}

         {AFTER FIELD cb_descripcion
            IF(v_configuracion.v_descripcion IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture descripción","about")
               NEXT FIELD cb_descripcion
            END IF}

         AFTER FIELD cb_tipo_ejecucion
            IF(v_configuracion.v_tipo_ejecucion IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture tipo recurrencia","about")
               NEXT FIELD cb_tipo_ejecucion
            END IF
            
         ON CHANGE cb_recurrencia
            INITIALIZE v_configuracion.v_tipo,v_configuracion.v_descripcion,v_configuracion.v_tipo_ejecucion TO NULL
            CASE 
               WHEN v_configuracion.v_recurrencia = 1
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)
                  CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)
                  CALL v_forma.setFieldHidden("cb_tipo",TRUE)
                  CALL v_forma.setElementHidden("lbl_tipo",TRUE)
                  
               WHEN v_configuracion.v_recurrencia = 9 OR v_configuracion.v_recurrencia = 10
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",FALSE)
                  CALL v_forma.setFieldHidden("cb_tipo",FALSE)
                  CALL v_forma.setElementHidden("lbl_tipo",FALSE)
                  IF(v_configuracion.v_tipo = 4)THEN # Fecha
                     CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)

                     CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  ELSE
                     CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)

                     CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
                  END IF
                  CALL fn_llena_cb_tipo_ejecucion(v_cb_tipo_ejecucion,v_configuracion.v_recurrencia)

               OTHERWISE
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)
                  CALL v_forma.setFieldHidden("cb_tipo",FALSE)
                  CALL v_forma.setElementHidden("lbl_tipo",FALSE)
                  IF(v_configuracion.v_tipo = 4)THEN # Fecha
                     CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)

                     CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  ELSE
                     CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                     CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)

                     CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
                     CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
                  END IF
                  
            END CASE
            {CASE v_configuracion.v_recurrencia
               WHEN 1
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)
                  CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
                  CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
                  CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)
                  CALL v_forma.setFieldHidden("cb_tipo",TRUE)
                  CALL v_forma.setElementHidden("lbl_tipo",TRUE)
               WHEN 9
                  CALL fn_llena_cb_tipo_ejecucion(v_cb_tipo_ejecucion,v_configuracion.v_recurrencia)
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",FALSE)                  
               WHEN 10
                  CALL fn_llena_cb_tipo_ejecucion(v_cb_tipo_ejecucion,v_configuracion.v_recurrencia)
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",FALSE)
               
               OTHERWISE
                  CALL v_forma.setFieldHidden("cb_tipo_ejecucion",TRUE)

                  CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
                  CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
                  CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
                  CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)
                  CALL v_forma.setFieldHidden("cb_tipo",FALSE)
                  CALL v_forma.setElementHidden("lbl_tipo",FALSE)
            END CASE}
            
            CALL fn_llena_cb_tipo(v_cb_tipo, v_configuracion.v_recurrencia)            

         ON CHANGE cb_tipo
            INITIALIZE v_configuracion.v_descripcion TO NULL
            # 4 = fecha
            IF(v_configuracion.v_tipo = 4)THEN
               INITIALIZE v_configuracion.v_descripcion TO NULL
               CALL v_forma.setFieldHidden("cb_descripcion",TRUE)
               CALL v_forma.setElementHidden("lbl_descripcion",TRUE)
               
               CALL v_forma.setFieldHidden("dtedi_descripcion",FALSE)
               CALL v_forma.setElementHidden("lbl_descripcion_dia",FALSE)
            ELSE
               CALL v_forma.setFieldHidden("cb_descripcion",FALSE)
               CALL v_forma.setElementHidden("lbl_descripcion",FALSE)
               
               CALL v_forma.setFieldHidden("dtedi_descripcion",TRUE)
               CALL v_forma.setElementHidden("lbl_descripcion_dia",TRUE)
               CALL fn_recupera_descripcion(v_configuracion.v_recurrencia, v_configuracion.v_tipo,v_cb_descripcion)
            END IF
            
            

         ON ACTION aceptar
            IF(v_configuracion.v_recurrencia IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture recurrencia","about")
               NEXT FIELD cb_recurrencia
            END IF
            IF((v_configuracion.v_recurrencia = 9 OR #bimestral
                v_configuracion.v_recurrencia = 10) AND #semestral
                v_configuracion.v_tipo_ejecucion IS NULL)THEN
               CALL fn_mensaje("Aviso","Capture tipo recurrencia","about")
               NEXT FIELD cb_tipo_ejecucion
            END IF
            IF(v_configuracion.v_tipo IS NULL AND v_configuracion.v_recurrencia <> 1)THEN #automatico
               CALL fn_mensaje("Aviso","Capture tipo","about")
               NEXT FIELD cb_tipo
            END IF
            IF(v_configuracion.v_descripcion IS NULL AND v_configuracion.v_recurrencia <> 1)THEN
               CALL fn_mensaje("Aviso","Capture descripción","about")
               IF(v_configuracion.v_tipo = 4)THEN
                  NEXT FIELD dtedi_descripcion
               ELSE
                  NEXT FIELD cb_descripcion
               END IF
            END IF
            # confirma actualización de operacion
            CALL fn_ventana_confirma("Aviso","¿Almacenar registro?","about")RETURNING r_confirma
            IF(r_confirma)THEN
               # se actualiza el registro
               CALL fn_guarda_configuracion(p_id_cat_mandato,v_configuracion.*)RETURNING r_error_sql
               IF(r_error_sql)THEN
                  # si ha ocurrido un error
                  CALL fn_mensaje("Aviso","Ocurrió un error al almacenar registro","about")
               ELSE
                  CALL fn_mensaje("Aviso","Registro almacenado correctamente","about")
                  ACCEPT INPUT 
               END IF
            END IF

         ON ACTION cancelar
            EXIT INPUT

      END INPUT
      
   CLOSE WINDOW vtna_configuracion
   
END FUNCTION



####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para filtrar los mandatos            # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_filtra_consulta_mandatos()
DEFINE v_filtro         STRING,
       v_tpo_mandato    LIKE mdt_tpo_mandato.tpo_mandato,
       v_minucipio      LIKE cat_municipio.municipio_desc,
       v_entidad_fed    LIKE cat_entidad_federativa.entidad_desc_larga, 
       v_cb_tpo_mandato ui.ComboBox,
       v_cb_municipio   ui.ComboBox,
       v_cb_entidad_fed ui.ComboBox,
       v_bnd_salir      BOOLEAN

   LET v_bnd_salir = TRUE  
   {CONSTRUCT v_filtro ON mdt.tpo_mandato, 
                         ins.valor_etiqueta,
                         ins.valor_etiqueta 
                    FROM cb_tpo_mandato,
                         cb_municipio,
                         cb_entidad_federativa}
                         

   INPUT v_tpo_mandato, 
         v_minucipio ,
         v_entidad_fed WITHOUT DEFAULTS
    FROM cb_tpo_mandato,
         cb_municipio,
         cb_entidad_federativa
      ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)
      --BEFORE CONSTRUCT
      BEFORE INPUT
         LET v_cb_tpo_mandato = ui.ComboBox.forName("formonly.cb_tpo_mandato")
         CALL fn_llena_cb_tpo_mandato(v_cb_tpo_mandato)
         LET v_cb_municipio = ui.ComboBox.forName("formonly.cb_municipio")
         CALL fn_llena_cb_municipio(v_cb_municipio)
         LET v_cb_entidad_fed = ui.ComboBox.forName("formonly.cb_entidad_federativa")
         CALL fn_llena_cb_entidad_federativa(v_cb_entidad_fed)

      ON ACTION aceptar
         LET v_bnd_salir = FALSE
         --ACCEPT CONSTRUCT
         ACCEPT INPUT

      ON ACTION cancelar
         LET v_bnd_salir = TRUE
         --EXIT CONSTRUCT
         EXIT INPUT
          

   --END CONSTRUCT
   END INPUT

  --RETURN v_filtro
  RETURN v_tpo_mandato,v_minucipio,v_entidad_fed,v_bnd_salir

END FUNCTION

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para recuperar los tipos de mandatos # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_llena_cb_tpo_mandato(v_cb_tpo_mandato)
DEFINE v_cb_tpo_mandato   ui.ComboBox,
       v_consulta         STRING,
       v_tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
       v_desc_tpo_mandato LIKE mdt_tpo_mandato.desc_tpo_mandato

   CALL v_cb_tpo_mandato.clear()
   LET v_consulta = "\n SELECT tpo_mandato, desc_tpo_mandato",
                    "\n   FROM mdt_tpo_mandato",
                    "\n  WHERE 1 = 1"
   PREPARE prp_rec_tpo_mandato FROM v_consulta
   DECLARE cur_rec_tpo_mandato CURSOR FOR prp_rec_tpo_mandato 
   FOREACH cur_rec_tpo_mandato INTO v_tpo_mandato,v_desc_tpo_mandato
      CALL v_cb_tpo_mandato.addItem(v_tpo_mandato,v_desc_tpo_mandato CLIPPED)
   END FOREACH
END FUNCTION

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para recuperar los municipios        # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_llena_cb_municipio(v_cb_municipio)
DEFINE v_cb_municipio   ui.ComboBox,
       v_consulta       STRING,
       v_municipio_desc LIKE cat_municipio.municipio_desc

   CALL v_cb_municipio.clear()
   LET v_consulta = "\n SELECT distinct municipio_desc",
                    "\n   FROM cat_municipio",
                    "\n  WHERE 1 = 1",
                    "\n  ORDER BY 1"
   PREPARE prp_rec_municipio FROM v_consulta
   DECLARE cur_rec_municipio CURSOR FOR prp_rec_municipio 
   FOREACH cur_rec_municipio INTO v_municipio_desc
      CALL v_cb_municipio.addItem(v_municipio_desc CLIPPED,v_municipio_desc CLIPPED)
   END FOREACH
END FUNCTION

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para recuperar los tipos de mandatos # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_llena_cb_entidad_federativa(v_cb_entidad_federativa)
DEFINE v_cb_entidad_federativa ui.ComboBox,
       v_consulta              STRING,
       v_entidad_fed           LIKE cat_entidad_federativa.entidad_desc_larga

   CALL v_cb_entidad_federativa.clear()
   LET v_consulta = "\n SELECT entidad_desc_larga",
                    "\n   FROM cat_entidad_federativa",
                    "\n  WHERE 1 = 1",
                    "\n  ORDER BY 1"
   PREPARE prp_rec_entidad_federativa FROM v_consulta
   DECLARE cur_rec_entidad_federativa CURSOR FOR prp_rec_entidad_federativa 
   FOREACH cur_rec_entidad_federativa INTO v_entidad_fed
      CALL v_cb_entidad_federativa.addItem(v_entidad_fed CLIPPED,v_entidad_fed CLIPPED)
   END FOREACH
END FUNCTION

FUNCTION fn_carga_arbol(p_tpo_mdt,p_id_tpo_mandato,p_minucipio,p_entidad_fed)
DEFINE p_tpo_mdt  LIKE mdt_cat_mandato.tpo_mandato,
       p_id_tpo_mandato LIKE mdt_tpo_mandato.tpo_mandato,
       p_minucipio      LIKE cat_municipio.municipio_desc,
       p_entidad_fed    LIKE cat_entidad_federativa.entidad_desc_larga
DEFINE v_s_qry    STRING,
       v_r_mdt    RECORD --LIKE mdt_cat_mandato.*,
         id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
         cve_mandato        LIKE mdt_cat_mandato.cve_mandato,
         desc_mandato       LIKE mdt_cat_mandato.desc_mandato,
         desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         usuario            LIKE mdt_cat_mandato.usuario,
         tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         f_creacion         LIKE mdt_cat_mandato.f_creacion,
         estado             LIKE mdt_cat_mandato.estado
       END RECORD,
       v_etiqueta           LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_valor_etiqueta     LIKE mdt_cat_instancia_mandato.valor_etiqueta,
       v_i_indice     INTEGER,
       v_filtro       STRING,
       v_filtro_param STRING,
       v_filtro_tpo   STRING,
       v_consulta     STRING -- determina si se muestran los tipos de mandatos

   LET v_filtro = " 1 = 1"
   LET v_filtro_tpo = " 1 = 1"
   INITIALIZE v_s_qry TO NULL
   IF(p_id_tpo_mandato IS NOT NULL AND p_id_tpo_mandato <> 0)THEN
      --LET v_filtro = "cat.tpo_mandato = ",p_id_tpo_mandato CLIPPED
      LET v_filtro_tpo = "cat.tpo_mandato = ",p_id_tpo_mandato CLIPPED
                             
   END IF
   IF(v_filtro <> " ")THEN
      IF(p_minucipio IS NOT NULL AND p_entidad_fed IS NOT NULL)THEN
         LET v_filtro = v_filtro,
                        " AND (ins.valor_etiqueta = '",p_minucipio CLIPPED, # revisar esta condicion, ya que si solo uno de los dos es verdadero recupera el registro
                        "'  OR ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"') " # se filtra sin construct, ya que entidad y municipio se filtran por el mismo campo y si se seleccionan los dos, no recupera el registro, aún y esten los dos atributos 
      ELSE
         IF(p_minucipio IS NOT NULL)THEN
            LET v_filtro = v_filtro, " AND ins.valor_etiqueta = '",p_minucipio CLIPPED,"' "
         END IF
         IF(p_entidad_fed IS NOT NULL)THEN
            LET v_filtro = v_filtro, " AND ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"' "
         END IF
      END IF
   ELSE
      IF(p_minucipio IS NOT NULL AND p_entidad_fed IS NOT NULL)THEN
         LET v_filtro = " (ins.valor_etiqueta = '",p_minucipio CLIPPED, # revisar esta condicion, ya que si solo uno de los dos es verdadero recupera el registro
                        "'  OR ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"') " # se filtra sin construct, ya que entidad y municipio se filtran por el mismo campo y si se seleccionan los dos, no recupera el registro, aún y esten los dos atributos 
      ELSE
         IF(p_minucipio IS NOT NULL)THEN
            LET v_filtro = " ins.valor_etiqueta = '",p_minucipio CLIPPED,"' "
         END IF
         IF(p_entidad_fed IS NOT NULL)THEN
            LET v_filtro = " ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"' "
         END IF
      END IF
   END IF
   
   IF p_tpo_mdt IS NULL OR LENGTH(p_tpo_mdt CLIPPED) = 0 OR p_tpo_mdt = 0 THEN
      LET v_filtro_param = " 1=1" 
   ELSE
      LET v_filtro_param = " cat.tpo_mandato = ", p_tpo_mdt 
   END IF
   
   {LET v_s_qry =
      "(SELECT 0 id_mandato, '', desc_tpo_mandato, '', '', tpo_mandato, today, 0",
      "   FROM mdt_tpo_mandato",
      "  WHERE ",v_s_qry CLIPPED,
      "  UNION ALL ",
      " SELECT *",
      "   FROM mdt_cat_mandato ",
      "  WHERE ",v_s_qry CLIPPED,
      "    AND ",v_filtro CLIPPED,
      "    AND estado = 100 ",
      ") ORDER BY 6,1"}

   LET v_s_qry =
      "\n(SELECT DISTINCT 0 id_mandato, '', desc_tpo_mandato, '', '', tpo_mandato, today, 0",
      --"\n   FROM mdt_tpo_mandato tpo",
      "\n   FROM mdt_tpo_mandato cat",
      "\n  WHERE ",v_filtro_param CLIPPED,
      "\n    AND ",v_filtro_tpo CLIPPED,
      {"\n   FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato mdt",
      "\n     ON mdt.tpo_mandato = tpo.tpo_mandato", 
      "\n        JOIN mdt_cat_atributo_nivel nvl",
      "\n     ON nvl.id_cat_mandato = mdt.id_cat_mandato",
      "\n        JOIN mdt_cat_instancia_mandato ins",
      "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
      "\n  WHERE ",v_s_qry CLIPPED,
      "\n    AND ",v_filtro CLIPPED,
      "\n    AND mdt.estado = 100",}
      "\n  UNION ALL ",
      "\n SELECT DISTINCT cat.*",
      "\n   FROM mdt_cat_mandato cat JOIN mdt_cat_atributo_nivel nvl",
      "\n     ON nvl.id_cat_mandato = cat.id_cat_mandato",
      "\n        JOIN mdt_cat_instancia_mandato ins",
      "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
      "\n        JOIN mdt_cat_gpo_etiqueta etq",
      "\n     ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
      "\n  WHERE ",v_filtro_param CLIPPED,
      "\n    AND ",v_filtro CLIPPED,
      "\n    AND ",v_filtro_tpo CLIPPED,
      "\n    AND cat.estado = 100",
      "\n) ORDER BY 6,1"
   --DISPLAY "CONSULTA:",v_s_qry
      
   PREPARE Prpr_ObtenerListaArbol FROM v_s_qry CLIPPED
   DECLARE Curr_ObtenerListaArbol CURSOR FOR Prpr_ObtenerListaArbol
   # consulta para recupeerar el municipio del mandato
   LET v_consulta = "\n SELECT etq.etiqueta,ins.valor_etiqueta",
                    "\n   FROM mdt_cat_mandato mdt JOIN mdt_cat_atributo_nivel nvl",
                    "\n     ON nvl.id_cat_mandato = mdt.id_cat_mandato",
                    "\n        JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n        JOIN mdt_cat_gpo_etiqueta etq",
                    "\n     ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE etq.etiqueta = 'MUNICIPIO'",
                    "\n    AND mdt.id_cat_mandato = ?"
   PREPARE prp_rec_municipio_etiqueta FROM v_consulta 

   LET v_i_indice = 0
   CALL tree.CLEAR()
   FOREACH Curr_ObtenerListaArbol INTO v_r_mdt.*
      LET v_i_indice = v_i_indice + 1
      IF  v_i_indice = 1 THEN
         LET tree[v_i_indice].name = 'TIPOS DE MANDATO'
         LET tree[v_i_indice].id   = 99
         LET tree[v_i_indice].pid  = NULL
         LET tree[v_i_indice].idx  = v_i_indice
         LET tree[v_i_indice].expanded = FALSE
         LET v_i_indice = v_i_indice + 1
      END IF
      LET v_etiqueta = " "
      # recupera municipio
      EXECUTE prp_rec_municipio_etiqueta USING v_r_mdt.id_cat_mandato
                                          INTO v_etiqueta,
                                               v_valor_etiqueta
                                               
      LET tree[v_i_indice].name = v_r_mdt.desc_mandato
      # agrega el valor de la etiqueta al arbol si es que es de tipo municipio
      IF(v_etiqueta CLIPPED = "MUNICIPIO")THEN
         LET tree[v_i_indice].municipio = v_valor_etiqueta
      END IF
      IF v_r_mdt.id_cat_mandato = 0 THEN
         LET tree[v_i_indice].id   = v_r_mdt.tpo_mandato
         LET tree[v_i_indice].pid  = 99
      ELSE
         LET tree[v_i_indice].id   = v_r_mdt.id_cat_mandato
         LET tree[v_i_indice].pid  = v_r_mdt.tpo_mandato
      END IF
      LET tree[v_i_indice].idx  = v_i_indice
      LET tree[v_i_indice].expanded = FALSE

   END FOREACH
   FREE Curr_ObtenerListaArbol
   # en caso de que solo se haya recuperado el tipo de mandato, se limpia el arbol
   # para volver a filtrar
   IF(tree.getLength() <= 2)THEN
      CALL tree.CLEAR()
   END IF
   
   CALL FGL_SET_ARR_CURR( 1 )
      
   RETURN v_filtro_param,v_filtro,v_filtro_tpo
END FUNCTION -- fn_llena_arbol_opciones

FUNCTION fn_consulta_detalle(p_tpo_mdt, p_cat_mdt) 
   DEFINE p_tpo_mdt   LIKE mdt_cat_mandato.tpo_mandato
   DEFINE p_cat_mdt   LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE v_gpo_mandato LIKE mdt_gpo_mandato.id_gpo_mandato
   DEFINE v_ruta_aper   CHAR(100)
   DEFINE v_r_mandato RECORD LIKE mdt_cat_mandato.*

   -- CALL fn_admon_archivo_mdt('0035_11-01-03.pdf',0, 'B') RETURNING vstatus -- AHM TMP
   -- DISPLAY "fn_admon_archivo_mdt status : ",vstatus
   -- RETURN -- AHM TMP

   -- se abre la ventana de consulta
   OPEN WINDOW w_con_mandato WITH FORM "HPSC02"

   IF(p_v_nom_prog IS NOT NULL)THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   
   LET cmbtpomdt = p_tpo_mdt
   LET cmbmdt      = p_cat_mdt
    
   DIALOG ATTRIBUTES (UNBUFFERED)
      
      INPUT BY NAME cmbtpomdt, cmbmdt ATTRIBUTES(WITHOUT DEFAULTS=TRUE)
         BEFORE INPUT
            IF p_v_tpo_mdt IS NULL OR p_v_tpo_mdt = 0 OR LENGTH(p_v_tpo_mdt CLIPPED) = 0 THEN
               CALL DIALOG.setFieldActive("cmbtpomdt",1)
            ELSE
               CALL DIALOG.setFieldActive("cmbtpomdt",0)
               LET cmbtpomdt = p_v_tpo_mdt
               DISPLAY BY NAME cmbtpomdt
            END IF
            CALL fn_rec_mandato(cmbmdt) RETURNING v_r_mandato.*
            DISPLAY BY NAME v_r_mandato.desc_larga_mandato
            
         ON CHANGE cmbtpomdt
            LET cmbmdt = NULL
            DISPLAY BY NAME cmbmdt
            CALL init_combo_mandato(cmbtpomdt)
            CALL v_arr_mandato.clear()
            --DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.*
             --  BEFORE DISPLAY
             --     EXIT DISPLAY
            --END DISPLAY
            CONTINUE DIALOG
            
      END INPUT

      DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.*
      END DISPLAY 

      DISPLAY ARRAY v_arr_imagen TO r_atributos_img.*
      END DISPLAY 

      BEFORE DIALOG
      
         -- Inicializa el combo de mandatos
         CALL inicializa_mandatos()
         
         -- Carga el arrerglo de mandatos-detalle por clave seleccionada
         CALL fn_carga_arr_mdt(cmbtpomdt, cmbmdt)
         CALL fn_carga_arr_img()
      
      --ON ACTION ACCEPT
      --	 -- Carga el arrerglo de mandatos-detalle por clave seleccionada
      --	 CALL fn_carga_arr_mdt(cmbtpomdt, cmbmdt)
      --	 -- Despliega arreglo de mandatos-detalle
      --	 --DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.*
      --	 --   BEFORE DISPLAY
      --	 --      EXIT DISPLAY
      --	 --END DISPLAY 
      --	 CONTINUE DIALOG
      
      ON ACTION CLOSE -- CANCELAR
         EXIT DIALOG

   END DIALOG
   
   CLOSE WINDOW w_con_mandato 

END FUNCTION

#############################################################################
# Funcion           => fn_carga_arr_mdt - Carga detalle del mandato         #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 10 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_mdt(p_tpomandato, p_mandato)
DEFINE p_mandato     LIKE mdt_cat_mandato.id_cat_mandato
DEFINE p_tpomandato  LIKE mdt_cat_mandato.tpo_mandato
DEFINE v_i_count     INTEGER

   IF p_tpomandato IS NULL OR LENGTH(p_tpomandato CLIPPED) = 0 THEN
      RETURN
   END IF

   IF p_mandato IS NULL OR LENGTH(p_mandato CLIPPED) = 0 THEN
      RETURN
   END IF
   
   LET v_s_qryTxt = "SELECT etiqueta, valor_etiqueta",
                    "  FROM mdt_cat_mandato m,",
                    "       mdt_cat_atributo_nivel n,",
                    "       mdt_cat_gpo_etiqueta g,",
                    "       mdt_cat_instancia_mandato i",
                    " WHERE m.id_cat_mandato = n.id_cat_mandato",
                    "   AND n.id_atr_nivel = i.id_atr_nivel",
                    "   AND n.id_gpo_etiqueta = g.id_gpo_etiqueta",
                    "   AND m.id_cat_mandato = ", p_mandato,
                    "   AND m.tpo_mandato = ", p_tpomandato

   PREPARE EnuDetMandato FROM v_s_qryTxt
   DECLARE CurDetMandato CURSOR FOR EnuDetMandato
   
   CALL v_arr_mandato.clear()
   
   LET v_i_count = 1
   FOREACH CurDetMandato INTO v_arr_mandAux.* -- INTO v_arr_mandato[v_i_count].*
      CALL v_arr_mandato.appendElement()
      LET v_arr_mandato[v_arr_mandato.getLength()].* = v_arr_mandAux.*
      LET v_i_count = v_i_count + 1
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => inicializa_mandatos - Definicion e inicializacion    #
#                      de los combos dinamicos del modulo de mandatos       #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 09 Febrero 2012                                      #
#############################################################################
FUNCTION inicializa_mandatos()
 
   -- Inicializa combo de bancos en funcion a su tabla de base de datos
   CALL init_combo_mandato(cmbtpomdt)
   CALL init_combo_tipomandato()

END FUNCTION

#############################################################################
# Funcion           => init_combo_mandato - Inicializa el combo de mandatos #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => p_tpo_mdt - tipo de mandato para el filtro           #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 09 Febrero 2012                                      #
#############################################################################
FUNCTION init_combo_mandato(p_tpo_mdt)

   DEFINE p_tpo_mdt        SMALLINT
   DEFINE v_s_qry          VARCHAR(50)
   DEFINE cb ui.ComboBox
   DEFINE mandato           LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE descripcion       LIKE mdt_cat_mandato.desc_mandato
   DEFINE desc_combo        CHAR(50)

   LET cb      = ui.combobox.forname("cmbmdt")
   CALL cb.clear()
   
   IF p_tpo_mdt IS NULL THEN
      LET v_s_qry = ""
   ELSE
      LET v_s_qry = " AND tpo_mandato = ", p_tpo_mdt
   END IF

   LET v_s_qryTxt = "SELECT tb.id_cat_mandato, tb.desc_mandato",
                    "  FROM mdt_cat_mandato tb",
                    " WHERE estado = 100 ",
                    v_s_qry,
                    " ORDER  BY 1 "

   PREPARE enu_mandato FROM v_s_qryTxt
   DECLARE cur_mandato CURSOR FOR enu_mandato
   
   FOREACH cur_mandato INTO mandato, descripcion
       LET desc_combo= mandato       USING "&&&", " ",
                       descripcion CLIPPED
       CALL cb.additem( mandato, desc_combo )
   END FOREACH

END FUNCTION

#############################################################################
# Funcion           => init_combo_mandato - Inicializa el combo de mandatos #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => p_tpo_mdt - tipo de mandato para el filtro           #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 30 Marzo 2012                                        #
#############################################################################
FUNCTION fn_rec_mandato(p_mdt)

   DEFINE p_mdt        LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE v_s_qry      VARCHAR(50)
   DEFINE v_r_mandato  RECORD LIKE mdt_cat_mandato.*

   LET v_s_qryTxt = "SELECT *",
                    "  FROM mdt_cat_mandato",
                    " WHERE estado = 100 ",
                    " AND id_cat_mandato = ", p_mdt

   DISPLAY "Recupera mdt: ", v_s_qryTxt
   PREPARE enu_mandato_rec FROM v_s_qryTxt
   EXECUTE enu_mandato_rec INTO v_r_mandato.*
   
   IF STATUS = NOTFOUND THEN
      INITIALIZE v_r_mandato TO NULL
   END IF
   
   RETURN v_r_mandato.*
   
END FUNCTION
#############################################################################
# Funcion           => init_combo_tipomandato - Inicializa el combo tpo mdt.#
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 09 Febrero 2012                                      #
#############################################################################
FUNCTION init_combo_tipomandato()

   DEFINE cb ui.ComboBox
   DEFINE mandato           LIKE mdt_tpo_mandato.tpo_mandato
   DEFINE descripcion       LIKE mdt_tpo_mandato.desc_tpo_mandato
   DEFINE desc_combo        CHAR(50)

   LET cb   = ui.combobox.forname("cmbtpomdt")
   CALL cb.clear()
   
   DECLARE cur_tpomandato CURSOR FOR
    SELECT tb.tpo_mandato, tb.desc_tpo_mandato
      FROM mdt_tpo_mandato tb
     ORDER  BY 1
   
   FOREACH cur_tpomandato INTO mandato, descripcion
       LET desc_combo= mandato       USING "&&&", " ",
                       descripcion CLIPPED
       CALL cb.additem( mandato, desc_combo )
   END FOREACH

END FUNCTION

#############################################################################
# Funcion           => fn_carga_arr_img - Carga imagenes del mandato        #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_img()
DEFINE v_i_count     INTEGER
DEFINE p_gpo_mdt     LIKE mdt_imagen_docto.id_gpo_mandato
DEFINE v_ruta_docto  LIKE seg_modulo.ruta_docto
DEFINE p_NomArc      VARCHAR(500)
DEFINE v_cad         VARCHAR(20)
DEFINE v_sus         VARCHAR(20)
DEFINE ret           INTEGER

   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   LET v_s_qryTxt = "SELECT nombre_imagen, desc_imagen",
                    "  FROM mdt_imagen_docto",
                    " WHERE id_gpo_mandato = ?"
   
   PREPARE EnuImgMandato FROM v_s_qryTxt
   DECLARE CurImgMandato CURSOR FOR EnuImgMandato

   LET v_s_qryTxt = "SELECT id_gpo_mandato ",
                    "  FROM mdt_cat_mandato m,",
                    "       mdt_gpo_mandato n",
                    " WHERE m.id_cat_mandato = n.id_cat_mandato",
                    "   AND m.id_cat_mandato = ",cmbmdt,
                    "   AND m.tpo_mandato = ",cmbtpomdt
   
   PREPARE EnuGpoImg FROM v_s_qryTxt
   DECLARE CurGpoImg CURSOR FOR EnuGpoImg
   
   CALL v_arr_imagen.clear()
   LET v_i_count = 1
   
   FOREACH CurGpoImg INTO p_gpo_mdt
      
      FOREACH CurImgMandato USING p_gpo_mdt INTO v_arr_imgAux.* --   INTO v_arr_imagen[v_i_count].*
         --LET v_arr_imagen[v_i_count].nombre_imagen = "<a href='",v_ruta_docto CLIPPED, v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"' target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         CALL v_arr_imagen.appendElement()
         LET v_arr_imagen[v_arr_imagen.getLength()].* = v_arr_imgAux.*
         LET v_arr_imagen[v_i_count].nombre_imagen = "<a gwc:attributes=\"href resourceuri('",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"','mdtdocto')\" target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         --LET v_arr_imagen[v_i_count].nombre_imagen = "<a gwc:attributes=\"href resourceuri('tenencia.pdf','mdtdocto')\" target='nueva'>tene.pdf</a>"
         LET v_i_count = v_i_count + 1
      END FOREACH
   END FOREACH
   
END FUNCTION

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL20                                                        #
#Objetivo     => Captura de paquetes de mandato                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio =>  Junio 2012                                                   # 
#Fecha modif  => 21 Junio 2012                                                 #
################################################################################
FUNCTION fn_captura_paquete(p_id_cat_mandato,p_tpo_mandato)
DEFINE p_id_cat_mandato  LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_tpo_mandato     LIKE mdt_tpo_mandato.tpo_mandato,
       v_indice          INTEGER
       

  
   
   OPEN WINDOW vtna_paquete WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC023"
     ATTRIBUTES(STYLE="dialog") # se abrira como pop-up

      INPUT ARRAY v_paquetes FROM sr_paquetes.*
        ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, APPEND ROW = FALSE)

        BEFORE INPUT
          CALL fn_recuper_paquetes(p_id_cat_mandato)
          IF(v_paquetes.getLength() = 0)THEN
             CALL v_paquetes.appendElement()
             LET v_paquetes[v_paquetes.getLength()].v_num = v_paquetes.getLength()
             LET v_paquetes[v_paquetes.getLength()].v_id_cat_mandato = p_id_cat_mandato
          END IF
          IF(p_v_nom_prog IS NOT NULL)THEN
             CALL ui.Interface.setText(p_v_nom_prog)
             LET v_ventana = ui.Window.getCurrent()
             CALL v_ventana.setText(p_v_nom_prog)
          END IF

        AFTER FIELD paquete
           {FOR v_indice = 1 TO v_paquetes.getLength()
              IF(LENGTH(v_paquetes[v_indice].v_cve_mandato CLIPPED) <> 18)THEN
                CALL fn_mensaje("Aviso","Paquete no valido","information")
                CALL FGL_SET_ARR_CURR(v_indice)
                EXIT FOR
              END IF
              CASE p_tpo_mandato
                 WHEN "02" # tipo mantenimiento
                    IF(v_paquetes[v_indice].v_cve_mandato[1,2] <> "02")THEN
                       CALL fn_mensaje("Aviso","Paquete no corresponde a tipo mantenimiento","information")
                       LET v_paquetes[v_indice].v_cve_mandato = "02" # tipo mantenimiento
                       CALL FGL_SET_ARR_CURR(v_indice)
                       EXIT FOR
                    END IF

                 WHEN "03" # tipo servicios
                    IF(v_paquetes[v_indice].v_cve_mandato[1,2] <> "03")THEN
                       CALL fn_mensaje("Aviso","Paquete no corresponde a tipo servicios","information")
                       LET v_paquetes[v_indice].v_cve_mandato = "03" # tipo servicios
                       CALL FGL_SET_ARR_CURR(v_indice)
                       EXIT FOR
                    END IF
              END CASE
           END FOR}
           
        ON ACTION aceptar
           FOR v_indice = 1 TO v_paquetes.getLength()
              IF(LENGTH(v_paquetes[v_indice].v_cve_mandato CLIPPED) <> 18)THEN
                CALL fn_mensaje("Aviso","Paquete no valido","information")
                CALL FGL_SET_ARR_CURR(v_indice)
                CONTINUE INPUT
              END IF
              CASE p_tpo_mandato
                 WHEN "02" # tipo mantenimiento
                    IF(v_paquetes[v_indice].v_cve_mandato[1,2] <> "02")THEN
                       CALL fn_mensaje("Aviso","Paquete no corresponde a tipo mantenimiento","information")
                       LET v_paquetes[v_indice].v_cve_mandato = "02" # tipo mantenimiento
                       CALL FGL_SET_ARR_CURR(v_indice)
                       CONTINUE INPUT
                    END IF

                 WHEN "03" # tipo servicios
                    IF(v_paquetes[v_indice].v_cve_mandato[1,2] <> "03")THEN
                       CALL fn_mensaje("Aviso","Paquete no corresponde a tipo servicios","information")
                       LET v_paquetes[v_indice].v_cve_mandato = "03" # tipo servicios
                       CALL FGL_SET_ARR_CURR(v_indice)
                       CONTINUE INPUT
                    END IF
              END CASE
           END FOR

           # Se eliminan todos los registros y se vulelven a insertar los que sobrevivan en el arreglo
           DELETE
             FROM mdt_cat_mandato_paquete                
            WHERE id_cat_mandato = p_id_cat_mandato
           # Actualiza mdt_cat_mandato_paquete toda la tabla
           FOR v_indice = 1 TO v_paquetes.getLength()
              DISPLAY (v_paquetes[v_indice].v_cve_mandato IS NOT NULL)
              IF(v_paquetes[v_indice].v_id_cat_mandato IS NOT NULL)THEN
                 INSERT INTO mdt_cat_mandato_paquete( id_cat_mandato,cve_mandato)
                 VALUES(v_paquetes[v_indice].v_id_cat_mandato,v_paquetes[v_indice].v_cve_mandato)
                 IF(SQLCA.SQLCODE <> 0)THEN
                    DISPLAY SQLCA.SQLCODE
                 END IF
              END IF
           END FOR
           EXIT INPUT

        ON ACTION agregar
           CALL v_paquetes.appendElement()
           LET v_paquetes[v_paquetes.getLength()].v_num = v_paquetes.getLength()
           LET v_paquetes[v_paquetes.getLength()].v_id_cat_mandato = p_id_cat_mandato
           IF(p_tpo_mandato = "02")THEN
              # es tipo mantenimiento
              LET v_paquetes[v_paquetes.getLength()].v_cve_mandato = "02"
           ELSE
              # es tipo servicios
              LET v_paquetes[v_paquetes.getLength()].v_cve_mandato = "03"
           END IF
           
           
        ON ACTION eliminar
           CALL v_paquetes.deleteElement(ARR_CURR())

        ON ACTION cancelar
           EXIT INPUT
           
      END INPUT

   CLOSE WINDOW vtna_paquete 

END FUNCTION 

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL20                                                        #
#Objetivo     => Recupera de paquetes de mandato                               #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio =>  Junio 2012                                                   # 
#Fecha modif  => 21 Junio 2012                                                 #
################################################################################
FUNCTION fn_recuper_paquetes(p_id_cat_mandato)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato, 
       v_consulta STRING,
       v_paquetes_aux RECORD
         v_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
         v_cve_mandato LIKE mdt_cat_mandato_paquete.cve_mandato
       END RECORD,
       v_indice INTEGER

  WHENEVER ERROR CONTINUE
  LET v_indice = 1
  LET v_consulta = "\n SELECT id_cat_mandato,cve_mandato",
                   "\n   FROM mdt_cat_mandato_paquete",
                   "\n  WHERE id_cat_mandato= ?"
  PREPARE prp_recupera_paquete FROM v_consulta
  DECLARE cur_recupera_paquete  CURSOR FOR prp_recupera_paquete
  FOREACH cur_recupera_paquete USING p_id_cat_mandato INTO v_paquetes_aux.*
     LET v_paquetes[v_indice].v_num            = v_indice
     --DISPLAY v_paquetes[v_indice].v_num
     LET v_paquetes[v_indice].v_id_cat_mandato = v_paquetes_aux.v_id_cat_mandato
     LET v_paquetes[v_indice].v_cve_mandato    = v_paquetes_aux.v_cve_mandato
     LET v_indice = v_indice + 1
  END FOREACH  
  FREE cur_recupera_paquete

END FUNCTION

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSC02                                                        #
#Objetivo     =>                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 04 Julio 2013                                                 #
################################################################################
FUNCTION fn_tipo_consulta_mov(p_id_cat_mandato,p_tpo_mandato,p_abonos,p_pagos,p_filtro1, p_filtro2, p_filtro3)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_tpo_mandato    LIKE mdt_tpo_mandato.tpo_mandato,
       p_filtro1        STRING,
       p_filtro2        STRING,
       p_filtro3        STRING,
       v_tipo_consulta  SMALLINT,
       p_abonos         STRING,
       p_pagos          STRING

   
   
   OPEN WINDOW vtna_tipo_consulta_mov WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC024" ATTRIBUTES( STYLE = "dialog")

      IF(p_v_nom_prog IS NOT NULL)THEN
         CALL ui.Interface.setText(p_v_nom_prog)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_v_nom_prog)
      END IF
      INPUT v_tipo_consulta FROM tpo_consulta ATTRIBUTES( ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         ON ACTION aceptar            
            # v_tipo_consulta = 1 Consulta detalle
            # v_tipo_consulta = 2 Consulta global
            IF(v_tipo_consulta = 1)THEN
               CALL fn_consulta_movimientos_mandato(p_id_cat_mandato,p_tpo_mandato,p_abonos,p_pagos)
               EXIT INPUT
            ELSE
               IF(p_tpo_mandato = 99)THEN # Registro actual es tipo mandato(grupo)               
                  --CALL fn_consulta_global_mov_mandato(p_id_cat_mandato,p_abonos,p_pagos)
                  --EXIT INPUT
               ELSE # Registro actual es mandato
                  CALL fn_consulta_global_mov_mandato(p_id_cat_mandato,0,p_abonos,p_pagos,p_filtro1, p_filtro2, p_filtro3)
                  EXIT INPUT
               END IF
            END IF            

         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_tipo_consulta_mov

END FUNCTION

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSC02                                                        #
#Objetivo     =>                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 04 Julio 2013                                                 #
################################################################################
FUNCTION fn_consulta_global_mov_mandato(p_id_cat_mandato,p_tpo_mandato,p_abonos,p_pagos,p_filtro1, p_filtro2,p_filtro3)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato,
       p_tpo_mandato    LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_abonos         STRING,
       p_pagos          STRING,
       p_filtro1        STRING,
       p_filtro2        STRING,
       p_filtro3        STRING,
       r_continua       BOOLEAN,
       r_montos_totales RECORD         
         v_abono        LIKE cta_movimiento.monto_pesos,
         v_pago         LIKE cta_movimiento.monto_pesos,
         v_saldo        LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_tpo_mandato_desc LIKE mdt_tpo_mandato.desc_tpo_mandato
       
   OPEN WINDOW vtna_consulta_global WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC026"

      IF(p_v_nom_prog IS NOT NULL)THEN
         CALL ui.Interface.setText(p_v_nom_prog)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_v_nom_prog)
      END IF

      MENU ""

         BEFORE MENU
            SELECT desc_tpo_mandato
              INTO v_tpo_mandato_desc
              FROM mdt_tpo_mandato
             WHERE tpo_mandato = p_tpo_mandato

            DISPLAY v_tpo_mandato_desc TO tpo_mandato
            CALL fn_recupera_mov_globales(p_id_cat_mandato,p_tpo_mandato,p_abonos,p_pagos,p_filtro1, p_filtro2, p_filtro3) RETURNING r_continua,r_montos_totales.v_abono,r_montos_totales.v_pago
            IF(r_continua)THEN
               LET r_montos_totales.v_saldo = r_montos_totales.v_abono + r_montos_totales.v_pago # pago esta con valor negativo, por lo que para obtener el saldo se suma abonos y pagos
               DISPLAY r_montos_totales.v_abono TO total_abonado
               DISPLAY r_montos_totales.v_pago  TO total_pagado
               DISPLAY r_montos_totales.v_saldo TO total_saldo
            ELSE
               CALL fn_mensaje("Aviso","No se encontraror registros con criterio dado","information")
               EXIT MENU
            END IF

         --ON ACTION reporte

         ON ACTION cancelar
            EXIT MENU

      END MENU


   CLOSE WINDOW vtna_consulta_global

END FUNCTION

{===============================================================================
Nombre: fn_recupera_mov_globales
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar montos totales de movimientos del mandato
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_mov_globales(p_id_cat_mandato,p_tpo_mandato, p_abonos,p_pagos,p_filtro1, p_filtro2, p_filtro3)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato,
       p_tpo_mandato    LIKE mdt_cat_mandato.tpo_mandato,
       p_abonos         STRING,
       p_pagos          STRING,
       p_filtro1        STRING,
       p_filtro2        STRING,
       p_filtro3        STRING,
       v_consulta       STRING,
       v_montos_totales RECORD         
         v_abono        LIKE cta_movimiento.monto_pesos,
         v_pago         LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_indice         SMALLINT,
       v_continua       BOOLEAN,
       v_filtro         STRING
   
   LET v_indice   = 1  
   INITIALIZE v_montos_totales TO NULL 

   
   # si p_id_cat_mandato <> 0 entonces p_tpo_mandato = 0
   # si p_id_cat_mandato = 0 entonces p_tpo_mandato <> 0
   IF(p_id_cat_mandato <> 0)THEN
      LET v_filtro = " cat.id_cat_mandato = ",p_id_cat_mandato
   ELSE
      LET v_filtro = " cat.tpo_mandato = ",p_tpo_mandato      
   END IF
   
   # se generan dos sub tablas con los datos de abonos y pagos por ser el mismo campo de pesos pero para diferente movimientos(pagos y abonos)
   # siempre estan los abonos y luego estaran los pagos (tab1 LEFT OUTER JOIN tab2)

LET v_consulta =                     
"\n SELECT NVL(SUM(tab1.monto_pesos),0), ",
       "\n NVL(SUM(tab2.monto_pesos),0) ",
  "\n FROM TABLE(MULTISET(SELECT mto.id_derechohabiente, ",
                             "\n afi.nss, ",
                             "\n SUM(cta.monto_pesos) as monto_pesos ",
                       "\n FROM hps_det_aplica_servicio mto JOIN afi_derechohabiente afi ",
                         "\n ON afi.id_derechohabiente = mto.id_derechohabiente ",
 
                            "\n JOIN TABLE(MULTISET(SELECT f_liquida, ",
                                                       "\n id_referencia, ",
                                                       "\n monto_pesos ",
                                                  "\n FROM cta_movimiento ",
                                                 "\n WHERE movimiento IN (",p_abonos,") ",  #solo abonos
                                                   "\n AND folio_liquida IN (SELECT folio_aplica_servicio FROM hps_ctr_aplica_servicio WHERE estado = 103))) cta  ",   # abonado

                        "\n ON cta.id_referencia = mto.id_det_aplica_servicio ",
                           "\n JOIN mdt_cat_mandato cat ",
                        "\n ON cat.id_cat_mandato = mto.id_cat_mandato ",

 # Solo relaciona que exista el mandato para el atributo seleccionado

                           "\n JOIN TABLE (MULTISET(SELECT id_cat_mandato ",
                                                  "\n FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                    "\n ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                       "\n JOIN mdt_cat_gpo_etiqueta etq ",
                                                    "\n ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                 "\n WHERE ",p_filtro2,  # filtro de atributos
                                                 "\n GROUP BY 1)) nvl ",
                        "\n ON nvl.id_cat_mandato = cat.id_cat_mandato ",

                     "\n WHERE ",v_filtro,  
                       "\n AND ",p_filtro1, 
                       "\n AND ",p_filtro3, 
                     "\n GROUP BY 1,2)) tab1 ",

      "\n LEFT OUTER JOIN ",

      "\n TABLE(MULTISET(SELECT mto.id_derechohabiente, ",
                            "\n afi.nss, ",
                            "\n SUM(cta.monto_pesos) as monto_pesos ",
                       "\n FROM TABLE(MULTISET(SELECT id_derechohabiente, ",
                                                  "\n id_det_aplica_pago_servicio,",
                                                  "\n id_cat_mandato ",
                                             "\n FROM hps_det_aplica_monto ",
                                            "\n WHERE 1 = 1 ",
                                            "\n GROUP BY 1,2,3)) mto ",
                            "\n JOIN afi_derechohabiente afi ",
                         "\n ON afi.id_derechohabiente = mto.id_derechohabiente ",
                            "\n JOIN hps_det_aplica_pago_servicio pag ",
                         "\n ON mto.id_det_aplica_pago_servicio = pag.id_det_aplica_pago_servicio ",
                            "\n JOIN TABLE(MULTISET(SELECT f_liquida, ",
                                                       "\n id_referencia,",
                                                       "\n monto_pesos ",
                                                  "\n FROM cta_movimiento ",
                                                 "\n WHERE movimiento IN (",p_pagos,") ", # solo pagos
                                                   "\n AND folio_liquida IN (SELECT folio_pago_servicio FROM hps_ctr_aplica_pago_servicio WHERE ind_proceso = 1))) cta ",
                         "\n ON cta.id_referencia = pag.id_det_aplica_pago_servicio ",
                           "\n JOIN mdt_cat_mandato cat ",
                        "\n ON cat.id_cat_mandato = mto.id_cat_mandato ",

# Solo relaciona que exista el mandato para el atributo seleccionado

                           "\n JOIN TABLE (MULTISET(SELECT id_cat_mandato ",
                                                  "\n FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                    "\n ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                      "\n  JOIN mdt_cat_gpo_etiqueta etq ",
                                                    "\n ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                 "\n WHERE ",p_filtro2, # filtro de atributos
                                                 "\n GROUP BY 1)) nvl ",
                        "\n ON nvl.id_cat_mandato = cat.id_cat_mandato ",
                    
                     "\n WHERE ",v_filtro,
                       "\n AND ",p_filtro1,
                       "\n AND ",p_filtro3,
                      "\n GROUP BY 1,2)) tab2 ",
    "\n ON tab2.id_derechohabiente = tab1.id_derechohabiente "


   --DISPLAY v_consulta
   PREPARE prp_recupera_mov_globales FROM v_consulta
   EXECUTE prp_recupera_mov_globales --USING p_id_cat_mandato,
                                     --      p_id_cat_mandato 
                                      INTO v_montos_totales.*

   IF(v_montos_totales.v_abono IS NOT NULL)THEN
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
   END IF
   
   RETURN v_continua,v_montos_totales.v_abono,v_montos_totales.v_pago

END FUNCTION

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSC02                                                        #
#Objetivo     =>                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 04 Julio 2013                                                 #
################################################################################
FUNCTION fn_recupera_descripciones_mandato(p_tpo_mandato,p_id_cat_mandato)
DEFINE p_id_cat_mandato   LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_tpo_mandato      LIKE mdt_tpo_mandato.tpo_mandato,
       v_tpo_mandato_desc VARCHAR(20),
       v_mandato_desc     VARCHAR(40)
       
   SELECT desc_tpo_mandato
     INTO v_tpo_mandato_desc
     FROM mdt_tpo_mandato
    WHERE tpo_mandato = p_tpo_mandato

   SELECT desc_mandato
     INTO v_mandato_desc
     FROM mdt_cat_mandato
    WHERE id_cat_mandato = p_id_cat_mandato

    RETURN v_tpo_mandato_desc, v_mandato_desc

END FUNCTION

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSC02                                                        #
#Objetivo     =>                                #
#Autor        => Hugo César Ramírez García                                     #
#Fecha inicio => 04 Julio 2013                                                 #
################################################################################
FUNCTION fn_consulta_movimientos_mandato(p_id_cat_mandato,p_tpo_mandato,p_abonos,p_pagos)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_tpo_mandato    LIKE mdt_tpo_mandato.tpo_mandato,
       p_abonos             STRING,
       p_pagos              STRING,
       r_derechohabientes DYNAMIC ARRAY OF RECORD
         v_num                SMALLINT,
         v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_abono              LIKE cta_movimiento.monto_pesos,
         v_pago               LIKE cta_movimiento.monto_pesos,
         v_saldo              LIKE cta_movimiento.monto_pesos
       END RECORD,
       r_movimientos DYNAMIC ARRAY OF RECORD
         v_num                 SMALLINT,
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado,
         v_boton               CHAR(1)
       END RECORD,
       v_continua           BOOLEAN,
       r_filtro             STRING,       
       r_tpo_mandato_desc   VARCHAR(20),
       r_mandato_desc       VARCHAR(40),
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_total_abonos       LIKE cta_movimiento.monto_pesos,
       v_total_pagos        LIKE cta_movimiento.monto_pesos,
       v_total_saldo        LIKE cta_movimiento.monto_pesos,
       v_indice             SMALLINT

   
   
   OPEN WINDOW vtna_consulta_det_mandato WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC025"

      IF(p_v_nom_prog IS NOT NULL)THEN
         CALL ui.Interface.setText(p_v_nom_prog)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_v_nom_prog)
      END IF

      CALL fn_recupera_descripciones_mandato(p_tpo_mandato, p_id_cat_mandato) RETURNING r_tpo_mandato_desc, r_mandato_desc

      DISPLAY r_tpo_mandato_desc TO tpo_mandato_desc
      DISPLAY r_mandato_desc     TO mandato_des
      
      LET v_continua = TRUE
      WHILE v_continua
         # Funcion para recuperar el filtro de búsqueda
         CALL fn_recupera_filtros() RETURNING v_continua,r_filtro
         IF( v_continua )THEN
            DIALOG ATTRIBUTES(UNBUFFERED)

               DISPLAY ARRAY r_derechohabientes TO sr_derechohabiente.*

                  BEFORE ROW
                     LET v_total_abonos = 0
                     LET v_total_pagos  = 0
                     LET v_total_saldo  = 0
                     FOR v_indice = 1 TO r_derechohabientes.getLength()
                        LET v_total_abonos = v_total_abonos + r_derechohabientes[v_indice].v_abono
                        LET v_total_pagos  = v_total_pagos  + r_derechohabientes[v_indice].v_pago
                        LET v_total_saldo  = v_total_saldo  + r_derechohabientes[v_indice].v_saldo
                     END FOR
                     DISPLAY v_total_abonos TO total_abonado
                     DISPLAY v_total_pagos  TO total_pagado
                     DISPLAY v_total_saldo  TO total_saldo
                     CALL r_movimientos.clear()
                     LET v_id_derechohabiente = r_derechohabientes[ARR_CURR()].v_id_derechohabiente
                     CALL fn_recupera_mandatos(r_filtro,
                                               v_id_derechohabiente,
                                               p_id_cat_mandato,
                                               p_abonos,
                                               p_pagos) RETURNING r_movimientos
                  ON ACTION reporte
                     CALL fn_consulta_individual(r_filtro,v_id_derechohabiente,p_id_cat_mandato,p_abonos,p_pagos,"Mandatos")

               END DISPLAY

               INPUT ARRAY r_movimientos FROM sr_movimientos.* ATTRIBUTES(APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE)

                  ON ACTION buscar
                    IF(r_movimientos[ARR_CURR()].v_id_det_aplica_monto = 0)THEN
                       CALL fn_mensaje("Aviso","No existen datos para el registro","information")
                    ELSE
                       CALL fn_muestra_datos_complementarios(r_movimientos[ARR_CURR()].v_id_det_aplica_monto)
                    END IF

               END INPUT

               BEFORE DIALOG
                  CALL r_derechohabientes.clear()
                  CALL fn_recupera_derechohabientes(p_id_cat_mandato,r_filtro,p_abonos,p_pagos) RETURNING v_continua,r_derechohabientes                  
                  IF NOT( v_continua )THEN
                     CALL fn_mensaje("Aviso","No se encontraron registros con criterio dado","information")
                     LET v_continua = TRUE
                     EXIT DIALOG
                  END IF

               ON ACTION reporte

               ON ACTION cancelar
                  LET v_continua = FALSE
                  EXIT DIALOG

            END DIALOG
          
         END IF
         
      END WHILE

   CLOSE WINDOW vtna_consulta_det_mandato   

END FUNCTION

{===============================================================================
Nombre: fn_recupera_filtros
Fecha creacion: 04 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar el filtro de la consulta
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_filtros()
DEFINE v_filtros RECORD
         v_nss          LIKE afi_derechohabiente.nss,
         v_fecha_inicio DATE,
         v_fecha_fin    DATE
       END RECORD,
       v_continuar      BOOLEAN,
       v_filtro         STRING

   LET v_continuar = TRUE
   LET v_filtro = " "
   
   INPUT v_filtros.* FROM nss,periodo_inicio,periodo_fin ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

      AFTER FIELD periodo_fin
         IF(v_filtros.v_fecha_inicio IS NOT NULL AND v_filtros.v_fecha_fin IS NOT NULL AND v_filtros.v_fecha_inicio > v_filtros.v_fecha_fin)THEN
            CALL fn_mensaje("Aviso","Fecha incio es mayor a fecha fin","information")
            NEXT FIELD periodo_fin
         END IF

      ON ACTION consultar
         IF(v_filtros.v_fecha_inicio IS NOT NULL AND v_filtros.v_fecha_fin IS NOT NULL AND v_filtros.v_fecha_inicio > v_filtros.v_fecha_fin)THEN
            CALL fn_mensaje("Aviso","Fecha incio es mayor a fecha fin","information")
            NEXT FIELD periodo_fin
         END IF
         LET v_continuar = TRUE
         ACCEPT INPUT

      ON ACTION cancelar
         LET v_continuar = FALSE
         EXIT INPUT

   END INPUT

   IF(v_continuar)THEN
      LET v_filtro = " 1 = 1 AND "
      IF(v_filtros.v_nss CLIPPED IS NOT NULL)THEN
         LET v_filtro = "afi.nss = '"||v_filtros.v_nss||"' AND "
      END IF
      IF(v_filtros.v_fecha_inicio IS NOT NULL)THEN
         IF(v_filtros.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro||"cta.f_liquida BETWEEN '"||v_filtros.v_fecha_inicio||"' AND '"||v_filtros.v_fecha_fin||"' AND "
         ELSE
            LET v_filtro = v_filtro||"cta.f_liquida = '"||v_filtros.v_fecha_inicio||"' AND "
         END IF
      ELSE
         IF(v_filtros.v_fecha_fin IS NOT NULL)THEN
            LET v_filtro = v_filtro||"cta.f_liquida = '"||v_filtros.v_fecha_fin||"' AND "
         END IF
      END IF
      LET v_filtro = v_filtro.subString(1,v_filtro.getLength()-4)
   END IF
   
   RETURN v_continuar,v_filtro

END FUNCTION

{===============================================================================
Nombre: fn_recupera_derechohabientes
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los derechohabientes según el filtro
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_derechohabientes(p_id_cat_mandato, p_filtro,p_abonos,p_pagos)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_filtro      STRING,
       p_abonos      STRING,
       p_pagos       STRING,
       v_consulta    STRING,
       v_derechohabiente RECORD
         v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_abono              LIKE cta_movimiento.monto_pesos,
         v_pago               LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_derechohabientes DYNAMIC ARRAY OF RECORD
         v_num                SMALLINT,
         v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                LIKE afi_derechohabiente.nss,
         v_abono              LIKE cta_movimiento.monto_pesos,
         v_pago               LIKE cta_movimiento.monto_pesos,
         v_saldo              LIKE cta_movimiento.monto_pesos
       END RECORD,
       v_indice               SMALLINT,
       v_continua             BOOLEAN
   
   LET v_indice   = 1   
   CALL v_derechohabientes.clear()
   # se generan dos sub tablas con los datos de abonos y pagos por ser el mismo campo de pesos pero para diferente movimientos(pagos y abonos)
   # siempre estan los abonos y luego estaran los pagos (tab1 LEFT OUTER JOIN tab2)
   LET v_consulta = "\n SELECT tab1.id_derechohabiente,",
                    "\n        tab1.nss,",
                    "\n        NVL(tab1.monto_pesos,0),",
                    "\n        NVL(tab2.monto_pesos,0)",
                    "\n   FROM TABLE(MULTISET(SELECT mto.id_derechohabiente,",
                    "\n                              afi.nss,",
                    "\n                              SUM(cta.monto_pesos) as monto_pesos",
                    "\n                         FROM hps_det_aplica_servicio mto JOIN afi_derechohabiente afi",
                    "\n                           ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n                              JOIN TABLE(MULTISET(SELECT f_liquida,",
                    "\n                                                         id_referencia,",
                    "\n                                                         monto_pesos",
                    "\n                                                    FROM cta_movimiento",
                    "\n                                                   WHERE movimiento IN (",p_abonos,")",# solo abonos
                    "\n                                                     AND folio_liquida IN (SELECT folio_aplica_servicio FROM hps_ctr_aplica_servicio WHERE estado = 103))) cta", #abonado
                    "\n                          ON cta.id_referencia = mto.id_det_aplica_servicio",
                    "\n                             JOIN mdt_cat_mandato_paquete paq",
                    "\n                          ON paq.cve_mandato = mto.cve_mandato",
                    "\n                       WHERE ",p_filtro, # filtro de nss y periodo
                    "\n                         AND paq.id_cat_mandato = ?", # mandato seleccionado en el arbol de mandatos( de pantalla principal)
                    "\n                       GROUP BY 1,2)) tab1",
                    "\n        LEFT OUTER JOIN ",
                    "\n        TABLE(MULTISET(SELECT mto.id_derechohabiente,",
                    "\n                              afi.nss,",
                    "\n                              SUM(cta.monto_pesos) as monto_pesos",
                    "\n                         FROM TABLE(MULTISET(SELECT id_derechohabiente,",
                    "\n                                                    id_det_aplica_pago_servicio,",
                    "\n                                                    id_cat_mandato",
                    "\n                                               FROM hps_det_aplica_monto",
                    "\n                                              WHERE 1 = 1",
                    "\n                                              GROUP BY 1,2,3)) mto ",                    
                    "\n                              JOIN afi_derechohabiente afi",
                    "\n                           ON afi.id_derechohabiente = mto.id_derechohabiente",
                    "\n                              JOIN hps_det_aplica_pago_servicio pag",
                    "\n                           ON mto.id_det_aplica_pago_servicio = pag.id_det_aplica_pago_servicio",
                    "\n                              JOIN TABLE(MULTISET(SELECT f_liquida,",
                    "\n                                                         id_referencia,",
                    "\n                                                         monto_pesos",
                    "\n                                                    FROM cta_movimiento",
                    "\n                                                   WHERE movimiento IN (",p_pagos,")", # solo pagos
                    "\n                                                     AND folio_liquida IN (SELECT folio_pago_servicio FROM hps_ctr_aplica_pago_servicio WHERE ind_proceso = 1))) cta",
                    "\n                           ON cta.id_referencia = pag.id_det_aplica_pago_servicio",
                    "\n                        WHERE ",p_filtro, # filtro de nss y periodo
                    "\n                          AND mto.id_cat_mandato = ?", # mandato seleccionado en el arbol de mandatos( de pantalla principal)
                    "\n                        GROUP BY 1,2)) tab2",
                    "\n     ON tab2.id_derechohabiente = tab1.id_derechohabiente"

--DISPLAY v_consulta

                    
   PREPARE prp_recupera_reg_derechohabiente FROM v_consulta
   DECLARE cur_recupera_reg_derechohabiente CURSOR FOR prp_recupera_reg_derechohabiente
   FOREACH cur_recupera_reg_derechohabiente USING p_id_cat_mandato,
                                                  p_id_cat_mandato 
                                             INTO v_derechohabiente.*
      LET v_derechohabientes[v_indice].v_num                = v_indice
      LET v_derechohabientes[v_indice].v_id_derechohabiente = v_derechohabiente.v_id_derechohabiente
      LET v_derechohabientes[v_indice].v_nss                = v_derechohabiente.v_nss
      LET v_derechohabientes[v_indice].v_abono              = v_derechohabiente.v_abono
      LET v_derechohabientes[v_indice].v_pago               = v_derechohabiente.v_pago
      LET v_derechohabientes[v_indice].v_saldo              = v_derechohabiente.v_abono + v_derechohabiente.v_pago # el pago es negativo, sumando se puede obtener el saldo

      LET v_indice = v_indice + 1
   END FOREACH 
   FREE cur_recupera_reg_derechohabiente

   IF(v_derechohabientes.getLength() > 0)THEN
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
   END IF
   
   RETURN v_continua,v_derechohabientes

END FUNCTION

{===============================================================================
Nombre: fn_recupera_mandatos
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los movimientos de mandatos según el filtro
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_mandatos(p_filtro,p_id_derechohabiente,p_id_cat_mandato,p_abonos,p_pagos)
DEFINE p_filtro             STRING,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
       p_abonos             STRING,
       p_pagos              STRING,
       v_consulta           STRING,       
       v_movimiento RECORD
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado
       END RECORD,
       v_movimientos DYNAMIC ARRAY OF RECORD
         v_num                 SMALLINT,
         v_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
         v_f_liquida           LIKE cta_movimiento.f_liquida,
         v_nss                 LIKE afi_derechohabiente.nss,
         v_subcuenta           LIKE cat_subcuenta.subcuenta_desc,
         v_movimiento          LIKE cat_movimiento.movimiento_desc,
         v_folio_liquida       LIKE cta_movimiento.folio_liquida,
         v_monto_pesos         LIKE cta_movimiento.monto_pesos,
         v_origen              LIKE cta_movimiento.origen,
         v_periodo_pago        LIKE mdt_det_aplica_mandato.periodo_pago,
         v_estado              LIKE mdt_det_aplica_monto.estado,
         v_boton               CHAR(1)
       END RECORD,
       v_indice       SMALLINT

   CALL v_movimientos.clear()
   LET v_indice = 1
   LET v_consulta = "\n SELECT mto.id_det_aplica_servicio,",
                    "\n        cta.f_liquida,",
                    "\n        afi.nss,",
                    "\n        sub.subcuenta_desc,",
                    "\n        mov.movimiento_desc,",
                    "\n        cta.folio_liquida,",
                    "\n        cta.monto_pesos,",
                    "\n        cta.origen,",
--                    "\n        apl.periodo_pago,",
                    "\n        '',",
                    "\n        mto.estado",
                    "\n   FROM afi_derechohabiente afi",
                    "\n        JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_derechohabiente,",
                    "\n                              subcuenta,",
                    "\n                              movimiento,",
                    "\n                              id_referencia,",
                    "\n                              folio_liquida,",
                    "\n                              monto_pesos,",
                    "\n                              origen",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_abonos,")",
                    "\n                          AND folio_liquida IN (SELECT folio_aplica_servicio FROM hps_ctr_aplica_servicio WHERE estado = 103))) cta",
                    "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                    "\n        JOIN cat_subcuenta sub",
                    "\n     ON sub.subcuenta = cta.subcuenta",
                    "\n        JOIN cat_movimiento mov",
                    "\n     ON mov.movimiento = cta.movimiento",
                    "\n        JOIN hps_det_aplica_servicio mto",
                    "\n     ON cta.id_referencia = mto.id_det_aplica_servicio",
                    "\n        JOIN hps_det_aplica_servicio apl",
                    "\n     ON apl.id_det_aplica_servicio = mto.id_det_aplica_servicio ",
                    "\n  WHERE ",p_filtro,
                    "\n    AND cta.id_derechohabiente = ?",
                    "\n    AND mto.id_cat_mandato = ?",
                    
                    "\n UNION ALL",
                    
                    "\n SELECT 0,",
                    "\n        cta.f_liquida,",
                    "\n        afi.nss,",
                    "\n        sub.subcuenta_desc,",
                    "\n        mov.movimiento_desc,",
                    "\n        cta.folio_liquida,",
                    "\n        cta.monto_pesos,",
                    "\n        cta.origen,",
                    "\n        '',",
                    "\n        0",
                    "\n   FROM afi_derechohabiente afi",
                    "\n        JOIN TABLE(MULTISET(",
                    "\n                       SELECT f_liquida,",
                    "\n                              id_derechohabiente,",
                    "\n                              subcuenta,",
                    "\n                              movimiento,",
                    "\n                              id_referencia,",
                    "\n                              folio_liquida,",
                    "\n                              monto_pesos,",
                    "\n                              origen",
                    "\n                         FROM cta_movimiento",
                    "\n                        WHERE movimiento IN (",p_pagos,")",
                    "\n                          AND folio_liquida IN (SELECT folio_pago_servicio FROM hps_ctr_aplica_pago_servicio WHERE ind_proceso = 1))) cta",
                    "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                    "\n        JOIN cat_subcuenta sub",
                    "\n     ON sub.subcuenta = cta.subcuenta",
                    "\n        JOIN cat_movimiento mov",
                    "\n     ON mov.movimiento = cta.movimiento",
                    "\n        JOIN hps_det_aplica_pago_servicio pag",
                    "\n     ON cta.id_referencia = pag.id_det_aplica_pago_servicio",
                    "\n        JOIN TABLE(MULTISET(SELECT id_derechohabiente,",
                    "\n                                   id_det_aplica_pago_servicio,",
                    "\n                                   id_cat_mandato",
                    "\n                              FROM hps_det_aplica_monto",
                    "\n                             WHERE 1 = 1",
                    "\n                             GROUP BY 1,2,3)) mto",
                    "\n     ON mto.id_det_aplica_pago_servicio = pag.id_det_aplica_pago_servicio",
                    "\n  WHERE ",p_filtro,
                    "\n    AND cta.id_derechohabiente = ?",
                    "\n    AND mto.id_cat_mandato = ?"

--DISPLAY v_consulta

   PREPARE prp_recupera_movimientos FROM v_consulta
   DECLARE cur_recupera_movimientos CURSOR FOR prp_recupera_movimientos
   FOREACH cur_recupera_movimientos USING p_id_derechohabiente,
                                          p_id_cat_mandato,
                                          p_id_derechohabiente,
                                          p_id_cat_mandato
                                     INTO v_movimiento.*
      LET v_movimientos[v_indice].v_num                 = v_indice
      LET v_movimientos[v_indice].v_id_det_aplica_monto = v_movimiento.v_id_det_aplica_monto
      LET v_movimientos[v_indice].v_f_liquida           = v_movimiento.v_f_liquida
      LET v_movimientos[v_indice].v_nss                 = v_movimiento.v_nss
      LET v_movimientos[v_indice].v_subcuenta           = v_movimiento.v_subcuenta
      LET v_movimientos[v_indice].v_movimiento          = v_movimiento.v_movimiento
      LET v_movimientos[v_indice].v_folio_liquida       = v_movimiento.v_folio_liquida
      LET v_movimientos[v_indice].v_monto_pesos         = v_movimiento.v_monto_pesos
      LET v_movimientos[v_indice].v_origen              = v_movimiento.v_origen
      LET v_movimientos[v_indice].v_periodo_pago        = v_movimiento.v_periodo_pago
      LET v_movimientos[v_indice].v_estado              = v_movimiento.v_estado
      
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_movimientos
   
   RETURN v_movimientos
END FUNCTION 

{===============================================================================
Nombre: fn_muestra_datos_complementarios
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para mostrar los datos complementarios
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_muestra_datos_complementarios(p_id_det_aplica_monto)
DEFINE p_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
       v_ventana   ui.Window,
       v_datos_com DYNAMIC ARRAY OF RECORD
         v_etiqueta VARCHAR(40),
         v_valor    VARCHAR(200)
       END RECORD,
       v_entidad   CHAR(20),
       v_continuar BOOLEAN

   LET v_entidad = "mdt_referencia_abono"
   OPEN WINDOW vtna_datos_complementarios WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC102" ATTRIBUTES(STYLE = "dialog")
      LET v_ventana = ui.Window.getCurrent()
      IF(p_v_nom_prog IS NOT NULL)THEN
         CALL ui.Interface.setText(p_v_nom_prog)         
         CALL v_ventana.setText(p_v_nom_prog)
      END IF
      DISPLAY ARRAY v_datos_com TO sr_complementario.* ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            CALL fn_recupera_datos_complementarios(p_id_det_aplica_monto,v_entidad) RETURNING v_continuar, v_datos_com
            IF NOT( v_continuar )THEN
               CALL fn_mensaje("Aviso","No se encontraror registros con citerio dado","information")
               EXIT DISPLAY
            END IF

         ON ACTION aceptar
            EXIT DISPLAY

      END DISPLAY

   CLOSE WINDOW vtna_datos_complementarios

END FUNCTION

{===============================================================================
Nombre: fn_recupera_datos_complementarios
Fecha creacion: 02 Julio 2013
Autor: Hugo César Ramírez Gracía
Narrativa del proceso que realiza:
 Función para recuperar los datos complementarios
 Parametros de Entrada:
  -
 Parámetros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_datos_complementarios(p_id_det_aplica_monto, p_entidad)
DEFINE p_id_det_aplica_monto LIKE mdt_det_aplica_monto.id_det_aplica_monto,
       p_entidad             CHAR(20),
       v_consulta            STRING,
       v_dato_complementario RECORD
         v_ind        SMALLINT,
         v_diag       CHAR(3),
         v_sql_error  INTEGER,
         v_isam_error INTEGER,
         v_msg_error  VARCHAR(100),
         v_etiqueta   VARCHAR(40),
         v_valor      VARCHAR(200)
       END RECORD,
       v_datos_complementarios DYNAMIC ARRAY OF RECORD
         v_etiqueta   VARCHAR(40),
         v_valor      VARCHAR(200)
       END RECORD,
       v_indice    SMALLINT,
       v_continuar BOOLEAN

   LET v_indice = 1
   CALL v_datos_complementarios.clear()
   LET v_consulta = "EXECUTE FUNCTION fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_recupera_datos FROM v_consulta
   DECLARE cur_recupera_datos CURSOR FOR prp_recupera_datos
   FOREACH cur_recupera_datos USING p_entidad,
                                    p_id_det_aplica_monto
                               INTO v_dato_complementario.*
      LET v_datos_complementarios[v_indice].v_etiqueta = v_dato_complementario.v_etiqueta
      LET v_datos_complementarios[v_indice].v_valor = v_dato_complementario.v_valor
      LET v_indice = v_indice + 1

   END FOREACH
   FREE cur_recupera_datos

   IF(v_datos_complementarios.getLength() > 0)THEN
      LET v_continuar = TRUE 
   ELSE
      LET v_continuar = FALSE
   END IF
   
   RETURN v_continuar, v_datos_complementarios
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATM01                                                  #
#Objetivo          => #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 27 Marzo 2012                                           #
###############################################################################
FUNCTION fn_llena_cb_recurrencia(cb_tipo)
DEFINE cb_tipo         ui.ComboBox,
       v_tipo_operacon RECORD
        v_tipo_cod         LIKE bat_tipo_operacion.tipo_cod,
        v_tipo_descripcion LIKE bat_tipo_operacion.tipo_descripcion
       END RECORD

   WHENEVER ERROR CONTINUE
   # se limpia el combo a desplegar
   CALL cb_tipo.clear()
   # recupera el catalogo de tipo operacion(bat_tipo_operacion)
   DECLARE cur_recupera_tipo_operacion CURSOR FOR SELECT tipo_cod,tipo_descripcion
                                                    FROM bat_tipo_operacion
   FOREACH cur_recupera_tipo_operacion INTO v_tipo_operacon.*
      IF(v_tipo_operacon.v_tipo_cod <> 0 AND v_tipo_operacon.v_tipo_cod <> 2)THEN # no se consideran los tipos MANUAL y DIARIA
         CALL cb_tipo.addItem(v_tipo_operacon.v_tipo_cod,v_tipo_operacon.v_tipo_descripcion)
      END IF
   END FOREACH

   FREE cur_recupera_tipo_operacion

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATM01                                                  #
#Objetivo          => #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 27 Marzo 2012                                           #
###############################################################################
FUNCTION fn_llena_cb_tipo(cb_ejecucion, p_filtro)
DEFINE cb_ejecucion    ui.ComboBox,
       v_tipo_ejecuta  RECORD
        v_ejecuta_cod  LIKE bat_tipo_operacion.tipo_cod,
        v_ejecuta_desc LIKE bat_tipo_operacion.tipo_descripcion
       END RECORD
DEFINE p_filtro        SMALLINT
DEFINE v_qry           STRING
DEFINE v_filtro        STRING

   WHENEVER ERROR CONTINUE
   # se limpia el combo a desplegar
   CALL cb_ejecucion.clear()
   # recupera el catalogo de tipo ejecuta(bat_tipo_ejecuta)
   
   CASE p_filtro
      WHEN 0 -- TODOS
      	 LET v_filtro = " 1 = 1 "
      WHEN 1 -- AUTOMATICO
         LET v_filtro = " 1 = 2 " -- NINGUNO
      WHEN 2 -- DIARIA
         --LET v_filtro = " 1 = 2 " -- NINGUNO
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural
      WHEN 3 -- SEMANAL
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural
      WHEN 4 -- QUINCENAL
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural
      WHEN 5 -- MENSUAL
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural
      WHEN 7 -- ANUAL
         LET v_filtro = " ejecuta_cod IN (1, 2, 4) " # Habil, natural y fecha
      WHEN 9 -- BIMESTRAL
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural
      WHEN 10 -- SEMESTRAL
         LET v_filtro = " ejecuta_cod IN (1, 2) " # Habil y natural

   END CASE
   
   
   LET v_qry = "SELECT ejecuta_cod,ejecuta_desc",
               "  FROM bat_tipo_ejecuta",
               " WHERE ",v_filtro
                           
   PREPARE enu_recupera_tipo_ejecuta FROM v_qry
   DECLARE cur_recupera_tipo_ejecuta CURSOR FOR enu_recupera_tipo_ejecuta

   FOREACH cur_recupera_tipo_ejecuta INTO v_tipo_ejecuta.*
      CALL cb_ejecucion.addItem(v_tipo_ejecuta.v_ejecuta_cod,v_tipo_ejecuta.v_ejecuta_desc)
   END FOREACH

   FREE cur_recupera_tipo_ejecuta

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATM01                                                  #
#Objetivo          => #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 27 Marzo 2012                                           #
###############################################################################
FUNCTION fn_recupera_descripcion(p_tipo_cod, p_tipo_ejecucion,cb_descripcion)
DEFINE p_tipo_cod       LIKE cat_operacion.recurrencia,
       p_tipo_ejecucion LIKE cat_operacion.ejecuta_cod,
       cb_descripcion   ui.ComboBox,
       v_inicio_rango   LIKE bat_rango_operacion_ejecuta.inicio_rango,
       v_fin_rango      LIKE bat_rango_operacion_ejecuta.fin_rango,
       v_indice         SMALLINT,
       r_rango        DYNAMIC ARRAY OF STRING

   WHENEVER ERROR CONTINUE
   
   SELECT inicio_rango,fin_rango
     INTO v_inicio_rango,v_fin_rango
     FROM bat_rango_operacion_ejecuta
    WHERE tipo_cod  = p_tipo_cod
      AND ejecuta_cod = p_tipo_ejecucion
      
   CALL fn_recupera_rango_ejecucion(v_inicio_rango CLIPPED, v_fin_rango CLIPPED) RETURNING r_rango
   
   CALL cb_descripcion.clear()
   FOR v_indice = 1 TO r_rango.getLength()      
      CALL cb_descripcion.addItem(r_rango[v_indice],r_rango[v_indice])
   END FOR
   

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATM01                                                  #
#Objetivo          => #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 27 Marzo 2012                                           #
###############################################################################
FUNCTION fn_recupera_rango_ejecucion(p_rango_inicio, p_rango_fin)
DEFINE p_rango_inicio LIKE bat_rango_operacion_ejecuta.inicio_rango,
       p_rango_fin    LIKE bat_rango_operacion_ejecuta.fin_rango,       
       v_dias         DYNAMIC ARRAY OF STRING,
       v_numerico     SMALLINT,
       v_indice       SMALLINT,
       v_indice_aux   SMALLINT,
       v_almacena     BOOLEAN,
       v_rango        DYNAMIC ARRAY OF STRING
       
   WHENEVER ERROR CONTINUE
   CALL v_rango.clear()
   LET v_dias[1] = "LUNES"
   LET v_dias[2] = "MARTES"
   LET v_dias[3] = "MIERCOLES"
   LET v_dias[4] = "JUEVES"
   LET v_dias[5] = "VIERNES"
   LET v_dias[6] = "SABADO"
   LET v_dias[7] = "DOMINGO"
   # se verifica si es caracter o númerico
   LET v_numerico = p_rango_inicio
   LET v_almacena = FALSE
   IF(p_rango_inicio IS NOT NULL)THEN
      
      # Es caracter, solo se procesan dias
      LET v_indice_aux = 1      
      FOR v_indice = 1 TO 7
         # indica el comienzo para almacenar los dias
         IF(p_rango_inicio = v_dias[v_indice])THEN
            LET v_almacena = TRUE
         END IF
         # indica el fin para almacenar los dias
         IF(v_almacena = TRUE AND p_rango_fin = v_dias[v_indice])THEN
            LET v_rango[v_indice_aux] = v_dias[v_indice]
            LET v_almacena = FALSE
         END IF
         # almacena los dias en arreglo
         IF(v_almacena)THEN
            LET v_rango[v_indice_aux] = v_dias[v_indice]
            LET v_indice_aux = v_indice_aux + 1
         END IF
      END FOR
      IF(v_rango.getLength() = 0)THEN
         # Es númerico
         FOR v_indice = p_rango_inicio TO p_rango_fin
            LET v_rango[v_indice] = v_indice
         END FOR
      END IF
   ELSE
      
   END IF

   RETURN v_rango
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATM01                                                  #
#Objetivo          => #
#Autor             => Hugo César Ramírez García                               #
#Fecha Inicio      => 27 Marzo 2012                                           #
###############################################################################
FUNCTION fn_llena_cb_tipo_ejecucion(p_cb_tipo_ejecucion,p_recurrencia)
DEFINE p_cb_tipo_ejecucion ui.ComboBox,
       p_recurrencia       SMALLINT,
       v_indice            SMALLINT,
       v_descripcion DYNAMIC ARRAY OF RECORD
         v_valor     SMALLINT,
         v_texto     VARCHAR(20)
       END RECORD

   # se limpia el combo a desplegar
   CALL p_cb_tipo_ejecucion.clear()
   CALL v_descripcion.clear()

   CASE p_recurrencia

      WHEN 9 # BIMESTRAL
         LET v_descripcion[1].v_valor = 1
         LET v_descripcion[1].v_texto = "Impar"

         LET v_descripcion[2].v_valor = 2
         LET v_descripcion[2].v_texto = "Par"

      WHEN 10 # SEMESTRAL
         LET v_descripcion[1].v_valor = 1
         LET v_descripcion[1].v_texto = "1"
         LET v_descripcion[2].v_valor = 2
         LET v_descripcion[2].v_texto = "2"
         LET v_descripcion[3].v_valor = 3
         LET v_descripcion[3].v_texto = "3"
         LET v_descripcion[4].v_valor = 4
         LET v_descripcion[4].v_texto = "4"
         LET v_descripcion[5].v_valor = 5
         LET v_descripcion[5].v_texto = "5"
         LET v_descripcion[6].v_valor = 6
         LET v_descripcion[6].v_texto = "6"
   END CASE

   FOR v_indice = 1 TO v_descripcion.getLength()
      CALL p_cb_tipo_ejecucion.addItem(v_descripcion[v_indice].v_valor,v_descripcion[v_indice].v_texto)
   END FOR

END FUNCTION

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para recuperar la configuracion del  #
#                     mandato                                      # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_recupera_configuracion(p_id_cat_mandato)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       v_configuracion RECORD
         v_recurrencia    SMALLINT,
         v_tipo           SMALLINT,
         v_descripcion    VARCHAR(10),
         v_tipo_ejecucion SMALLINT
       END RECORD

   INITIALIZE v_configuracion TO NULL
       
   SELECT recurrencia,
          complemento_recurrencia,
          ejecuta_cod,
          descripcion
     INTO v_configuracion.v_recurrencia,
          v_configuracion.v_tipo_ejecucion,
          v_configuracion.v_tipo,
          v_configuracion.v_descripcion
     FROM mdt_cat_mandato_ejecuta_pago
    WHERE id_cat_mandato = p_id_cat_mandato

    RETURN v_configuracion.*
END FUNCTION

####################################################################
#Modulo            => HPS                                          #
#Programa          => HPSC02                                       #
#Objetivo          => funcion para guardar la configuracion del    #
#                     mandato                                      # 
#Autor             => Hugo César Ramírez García                    #
#Fecha inicio      => 06 Septiembre 2012                           #
####################################################################
FUNCTION fn_guarda_configuracion(p_id_cat_mandato, p_configuracion)
DEFINE p_id_cat_mandato LIKE mdt_cat_mandato_paquete.id_cat_mandato,
       p_configuracion RECORD
         v_recurrencia    SMALLINT,
         v_tipo           SMALLINT,
         v_descripcion    VARCHAR(10),
         v_tipo_ejecucion SMALLINT
       END RECORD,
       v_encontro  BOOLEAN,
       v_error_sql BOOLEAN

   LET v_encontro  = FALSE
   LET v_error_sql = FALSE
   
   SELECT 1
     INTO v_encontro
     FROM mdt_cat_mandato_ejecuta_pago
    WHERE id_cat_mandato = p_id_cat_mandato 

   IF(v_encontro)THEN
      UPDATE mdt_cat_mandato_ejecuta_pago
         SET recurrencia = p_configuracion.v_recurrencia,
             complemento_recurrencia = p_configuracion.v_tipo_ejecucion,
             ejecuta_cod = p_configuracion.v_tipo,
             descripcion = p_configuracion.v_descripcion
       WHERE id_cat_mandato = p_id_cat_mandato 
       IF(SQLCA.SQLCODE <> 0)THEN
          LET v_error_sql = TRUE
       END IF
   ELSE
      INSERT INTO mdt_cat_mandato_ejecuta_pago 
        (id_cat_mandato,
         recurrencia,
         complemento_recurrencia,
         ejecuta_cod,
         descripcion)
      VALUES(p_id_cat_mandato,
             p_configuracion.v_recurrencia,
             p_configuracion.v_tipo_ejecucion,
             p_configuracion.v_tipo,
             p_configuracion.v_descripcion)
      IF(SQLCA.SQLCODE <> 0)THEN
         LET v_error_sql = TRUE
      END IF

   END IF
   
   RETURN v_error_sql
END FUNCTION
