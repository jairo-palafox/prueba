HPSC02.4gl                                                                                          0000777 0000212 0001751 00000346762 13113322760 012314  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
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
               # funcion que recupera informaci�n
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
                  # se muestra consulta global ya que seleccion� un grupo de mandato
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
#Objetivo          => funcion para configuracion de ejecuci�n      #
#                     batch de mandatos                            # 
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
               CALL fn_mensaje("Aviso","Capture descripci�n","about")
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
               CALL fn_mensaje("Aviso","Capture descripci�n","about")
               IF(v_configuracion.v_tipo = 4)THEN
                  NEXT FIELD dtedi_descripcion
               ELSE
                  NEXT FIELD cb_descripcion
               END IF
            END IF
            # confirma actualizaci�n de operacion
            CALL fn_ventana_confirma("Aviso","�Almacenar registro?","about")RETURNING r_confirma
            IF(r_confirma)THEN
               # se actualiza el registro
               CALL fn_guarda_configuracion(p_id_cat_mandato,v_configuracion.*)RETURNING r_error_sql
               IF(r_error_sql)THEN
                  # si ha ocurrido un error
                  CALL fn_mensaje("Aviso","Ocurri� un error al almacenar registro","about")
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
                        "'  OR ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"') " # se filtra sin construct, ya que entidad y municipio se filtran por el mismo campo y si se seleccionan los dos, no recupera el registro, a�n y esten los dos atributos 
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
                        "'  OR ins.valor_etiqueta = '",p_entidad_fed CLIPPED,"') " # se filtra sin construct, ya que entidad y municipio se filtran por el mismo campo y si se seleccionan los dos, no recupera el registro, a�n y esten los dos atributos 
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar montos totales de movimientos del mandato
 Parametros de Entrada:
  -
 Par�metros de salida:
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
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
         # Funcion para recuperar el filtro de b�squeda
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar el filtro de la consulta
 Parametros de Entrada:
  -
 Par�metros de salida:
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar los derechohabientes seg�n el filtro
 Parametros de Entrada:
  -
 Par�metros de salida:
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar los movimientos de mandatos seg�n el filtro
 Parametros de Entrada:
  -
 Par�metros de salida:
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para mostrar los datos complementarios
 Parametros de Entrada:
  -
 Par�metros de salida:
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
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar los datos complementarios
 Parametros de Entrada:
  -
 Par�metros de salida:
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
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
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
   # se verifica si es caracter o n�merico
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
         # Es n�merico
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
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
#Autor             => Hugo C�sar Ram�rez Garc�a                    #
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
              HPSC03.4gl                                                                                          0000777 0000212 0001751 00000016720 13113322760 012301  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-AGO-2015
--===============================================================
####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSC03                                        #
#Objetivo          =>Consulta de solicitudes de cancelaci�n de Hipoteca con Servicios #
#Fecha inicio      =>10 de Agosto de 2015                          #
####################################################################
DATABASE safre_viv

DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      STRING,             -- nombre del programa
       p_v_tpo_mdt       SMALLINT,                 -- tipo de mandato, opcional
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin

DEFINE v_arr_solicitud DYNAMIC ARRAY OF RECORD
v_id_solicitud          DECIMAL(9,0),
v_nss                   CHAR(11),
v_nombre                CHAR(40),
v_resultado_operacion   CHAR(2),
v_diagnostico_interno   CHAR(18),
v_estado                SMALLINT
END RECORD

DEFINE v_arr_restitucion DYNAMIC ARRAY OF RECORD
v_atributo CHAR(40),
v_valor    VARCHAR(200)
END RECORD

DEFINE v_condition   STRING
DEFINE v_nss         CHAR(11)
DEFINE v_estado      SMALLINT
DEFINE f_ini_registro   DATE
DEFINE f_fin_registro   DATE
DEFINE f_ini_solicitado DATE
DEFINE f_fin_solicitado DATE
DEFINE f_ini_restituido DATE
DEFINE f_fin_restituido DATE

MAIN
   
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)
   LET p_v_tpo_mdt    = ARG_VAL(4)

   SELECT ruta_bin
    INTO v_ruta_ejecutable
    FROM seg_modulo
   WHERE modulo_cod = "hps" 

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   OPEN WINDOW vtna_consulta WITH FORM v_ruta_ejecutable CLIPPED||"/HPSC031"

      DIALOG ATTRIBUTES(UNBUFFERED)

         --Leyendo datos de entrada
         INPUT BY NAME v_nss,v_estado,f_ini_registro,f_fin_registro,f_ini_solicitado,f_fin_solicitado,f_ini_restituido,f_fin_restituido
         END INPUT
         
         --Despliega las cuentas con solicitud de cancelaci�n
         DISPLAY ARRAY v_arr_solicitud TO Record2.*
            BEFORE ROW
               CALL fn_despliega_restitucion(v_arr_solicitud[ARR_CURR()].v_id_solicitud)
         END DISPLAY

         --Despliega Solicitud restitucion
         DISPLAY ARRAY v_arr_restitucion TO Record3.*
         END DISPLAY

         ON ACTION aceptar
            CALL fn_arma_cadena()
            CALL v_arr_restitucion.CLEAR()
            CALL fn_despliega_cancelacion(v_condition)

         ON ACTION cancelar
            EXIT DIALOG

      END DIALOG

   CLOSE WINDOW vtna_consulta
   
END MAIN

FUNCTION fn_despliega_cancelacion(p_condition)

   DEFINE p_condition STRING
   DEFINE p_query     STRING
   DEFINE i INTEGER

   CALL v_arr_solicitud.CLEAR()

   DISPLAY p_condition
   LET p_query = "SELECT a.id_hps_solicitud_cancelacion,
                         a.nss,
                         a.nombre,
                         a.resultado_operacion,
                         a.diagnostico_interno,
                         a.estado 
                         FROM hps_solicitud_cancelacion a, hps_restitucion b WHERE a.id_hps_solicitud_cancelacion = b.id_hps_solicitud_cancelacion AND ",p_condition
   PREPARE prp_cancela FROM p_query
   DECLARE cur_cancela CURSOR FOR prp_cancela

   LET i=1
   FOREACH cur_cancela INTO v_arr_solicitud[i].*
      LET i = i+1
   END FOREACH

   CALL v_arr_solicitud.deleteElement(i)

END FUNCTION


FUNCTION fn_despliega_restitucion(p_id_solicitud)

   DEFINE p_id_solicitud DECIMAL(9,0)
   DEFINE p_query_r      STRING 
   DEFINE j              INTEGER
   DEFINE p_entidad      CHAR(40)
   DEFINE p_id_registro_entidad DECIMAL(9,0)

   --RESPUESTAS de la funcion que no se guardan en el record
   DEFINE v_ind SMALLINT
   DEFINE v_diag                CHAR(3)    -- diagnostico de error
   DEFINE v_sql_error           INTEGER 
   DEFINE v_isam_error          INTEGER
   DEFINE v_msg_error           CHAR(100)

   CALL v_arr_restitucion.CLEAR()

   --Obteniendo los datos con los que se llamar� a la funci�n
   LET p_entidad = "hps_restitucion"
   SELECT id_hps_restitucion 
      INTO p_id_registro_entidad
      FROM hps_restitucion
      WHERE id_hps_restitucion = p_id_solicitud
      
   --Ejecutando Query
   LET p_query_r = "CALL fn_glo_recupera_etiquetas(?,?)"
   PREPARE prp_restitucion FROM p_query_r
   DECLARE cur_restitucion CURSOR FOR prp_restitucion
   
   LET j = 1 
   FOREACH cur_restitucion USING p_entidad,p_id_registro_entidad INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_arr_restitucion[j].v_atributo,v_arr_restitucion[j].v_valor
      LET j = j+1
   END FOREACH

   CALL v_arr_restitucion.deleteElement(j)

END FUNCTION

FUNCTION fn_arma_cadena()

   --Arma la cadena segun las condiciones del INPUT
   LET v_condition = " 1=1 "
   --NSS
   IF v_nss IS NOT NULL THEN
      LET v_condition = v_condition," AND a.nss = ",v_nss
   END IF

   --ESTADO
   IF v_estado IS NOT NULL THEN
      LET v_condition = v_condition," AND a.estado = ",v_estado
   END IF

   --FECHA REGISTRO
   --Fecha Registro acotando ambos limites
   IF (f_ini_registro IS NOT NULL) AND (f_fin_registro IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud BETWEEN '",f_ini_registro,"' AND '",f_fin_registro,"'"      
   END IF

   --Fecha Registro acotando anterior
   IF (f_ini_registro IS NOT NULL) AND (f_fin_registro IS NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud >= '",f_ini_registro,"'"      
   END IF

   --Fecha Registro acotando despu�s
   IF (f_ini_registro IS NULL) AND (f_fin_registro IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_registro_solicitud <= '",f_fin_registro,"'"      
   END IF

   
   --FECHA SOLICITADO
   IF (f_ini_solicitado IS NOT NULL) AND (f_fin_solicitado IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion BETWEEN '",f_ini_solicitado,"' AND '",f_fin_solicitado,"'"      
   END IF

   IF (f_ini_solicitado IS NOT NULL) AND (f_fin_solicitado IS NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion >= '",f_ini_solicitado,"'"      
   END IF

   IF (f_ini_solicitado IS NULL) AND (f_fin_solicitado IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_agrupa_restitucion <= '",f_fin_solicitado,"'"      
   END IF

   
   --FECHA RESTITUIDO
   IF (f_ini_restituido IS NOT NULL) AND (f_fin_restituido IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion BETWEEN '",f_ini_restituido,"' AND '",f_fin_restituido,"'"      
   END IF

   IF (f_ini_restituido IS NOT NULL) AND (f_fin_restituido IS NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion >= '",f_ini_restituido,"'"      
   END IF

   IF (f_ini_restituido IS NULL) AND (f_fin_restituido IS NOT NULL) THEN
      LET v_condition = v_condition," AND b.f_liquida_restitucion <= '",f_fin_restituido,"'"      
   END IF

END FUNCTION                                                HPSG01.4gl                                                                                          0000777 0000212 0001751 00000077470 13113322760 012314  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-04-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => MDT                                                     #
#Programa          => MDTG01                                                  #
#Objetivo          => LIBRERIA DE FUNCIONES GENERALES DE MANDATOS             #
#Fecha Inicio      => 08-MAR-2012                                             #
###############################################################################
IMPORT os   
GLOBALS "HPSG02.4gl"
DATABASE safre_viv


################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG02                                                   #
#Descripcion       => Extrae el ultimo token de una cadena                     #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_extrae_ultimo_token(p_cadena)
DEFINE p_cadena     STRING,
       v_cadena_aux STRING,
       v_elemento   STRING
DEFINE v_cad_busqueda    base.StringTokenizer,
       v_cad_reemplazo   base.StringBuffer

   LET v_cad_reemplazo = base.StringBuffer.create()
   CALL v_cad_reemplazo.append(p_cadena CLIPPED)
   # reemplaza los slash por pipes, para  poder hacer busqueda por toquen(para el caso de ruta windows)
   CALL v_cad_reemplazo.replace("\\","|",0)
   LET v_cadena_aux = v_cad_reemplazo.toString()
   # limpia buffer
   CALL v_cad_reemplazo.clear()
   CALL v_cad_reemplazo.append(v_cadena_aux CLIPPED)
   # reemplaza las diagonales por pipes, para  poder hacer busqueda por toquen(para el caso de ruta linux)
   CALL v_cad_reemplazo.replace("/","|",0)
   LET v_cadena_aux = v_cad_reemplazo.toString()
   # divide la cadena segun los pipes encontrados
   LET v_cad_busqueda = base.StringTokenizer.create(v_cadena_aux,"|")
   WHILE v_cad_busqueda.hasMoreTokens()
      # recupera cada elemento hasta el ultimo
      LET v_elemento = v_cad_busqueda.nextToken()
   END WHILE
   # retorna el ultimo elemento encontrado
   RETURN v_elemento
END FUNCTION 

#############################################################################
# Funcion           => fn_transfiere_archivo - Copiar archivos de un equipo #
#                      remoto al servidor                                   #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_r_imagen - Registro que identifica la iamgen       #
#                      v_operacion - Tipo operacion, inoperable actualmente #
#                      v_nombre_original - NOmbre original del archivo,     #
#                      actualmente inoperable                               #
# Salida:           => v_NomArcDepT - nombre del archivo almacenado         #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Marzo 2012                                        #
#############################################################################
FUNCTION fn_transfiere_archivo(p_r_imagen, v_operacion, v_nombre_original)
   DEFINE p_r_imagen     RECORD
             id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
             id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato ,
             id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
             nombre_imagen     STRING, -- TMP AHM Cambiar� tipo dato LIKE mdt_imagen_docto.nombre_imagen  ,
             v_documento_int   STRING,
             v_documento       STRING,
             desc_imagen       LIKE mdt_imagen_docto.desc_imagen
          END RECORD,
          v_operacion       CHAR(1),
          v_nombre_original STRING
            
   DEFINE v_NomArcDep    VARCHAR(500)
   DEFINE v_NomArcDepT   STRING
   DEFINE v_ruta_docto   LIKE seg_modulo.ruta_docto
   DEFINE v_gpo          STRING
            
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
            
   {DISPLAY "NomArc - ",p_r_imagen.nombre_imagen CLIPPED
   CALL os.Path.basename(p_r_imagen.nombre_imagen) RETURNING v_NomArcDep
   DISPLAY "os.Path.basename - ", v_NomArcDep}
   # funcion que extrae el ultimo elemento de una cadena ruta
   CALL fn_extrae_ultimo_token(p_r_imagen.nombre_imagen) RETURNING v_NomArcDep

   SLEEP 5
   
   CALL fn_depura_archivo(v_NomArcDep) RETURNING v_NomArcDep
            
   LET v_gpo = p_r_imagen.id_cat_gpo USING "&&&&"
   LET v_gpo = v_gpo.trim()
   LET v_NomArcDepT = "tmp_", v_gpo, "_", v_NomArcDep
            
   DISPLAY "Origen : ",p_r_imagen.nombre_imagen
   DISPLAY "Destino: ",v_ruta_docto CLIPPED, v_NomArcDepT
   CALL fgl_getfile(p_r_imagen.nombre_imagen, v_ruta_docto CLIPPED||v_NomArcDepT)
            
   RETURN v_NomArcDepT
            
END FUNCTION
            
#############################################################################
# Funcion           => fn_depura_archivo - Depurar nombre de archivo, en    #
#                      especifico los espacios del archivo                  #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_NomArc - Archivo a subir del local al servidor     #
# Salida:           => p_NomArcDep - Archivo depurado con guiones en lugar  #
#                      de espacios                                          #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 02 Marzo 2012                                        #
#############################################################################
FUNCTION fn_depura_archivo(p_NomArc)
  DEFINE p_NomArc      VARCHAR(500)
  DEFINE p_NomArcDep   VARCHAR(500)
  DEFINE v_cadena  STRING
  DEFINE v_longitud INTEGER

            
  CALL fn_sustituye_cadena (p_NomArc," ","_") RETURNING p_NomArcDep
  --LET v_longitud = LENGTH(p_NomArcDep)
  --LET p_NomArcDep = "temp_",p_NomArcDep[5,v_longitud]
  --DISPLAY p_NomArcDep[5,v_longitud]
  --CALL fn_sustituye_cadena (p_NomArcDep,"tmp_","temp_") RETURNING p_NomArcDep
            
  RETURN p_NomArcDep
END FUNCTION
            

#############################################################################
# Funcion           => fn_admon_archivo_mdt - Mantenimiento a los archivos  #
#                      subidos al servidor de forma temporal                #
# Propietario       => E.F.P                                                #
# Sistema           => GLO                                                  #
# Entrada:          => p_archivo - Nombre del archivo a dar mantenimiento   #
#                      p_id_imagen_docto - Clave de imagen para referencia  #
#                      p_tpo_ren - Tipo de mantenimiento:                   #
#                                  'A' renombrar archivo temporal en la alta#
#                                  'B' borrar archivo al cancelar o confir- #
#                                  mar su eliminaci�n                       #
# Salida:           => v_archivo - Nombre del archivo o notificaci�n de man-#
#                                  tenimiento:                              #
#                                  'A' Archivo renombrado de temporal por su#
#                                  nombre definitivo o leyenda NO_MODIFICADO#
#                                  'B' leyenda de ELIMINADO o NO_ELIMINADO  #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 06 Marzo 2012                                        #
#############################################################################
FUNCTION fn_admon_archivo_mdt(p_archivo,p_id_imagen_docto, p_tpo_ren)
   DEFINE p_archivo         STRING,
          p_id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
          p_tpo_ren         CHAR(1),  -- Renombrar para Alta 'A' o modificaci�n 'M'
          v_archivo         STRING,
          v_buf             base.StringBuffer
   DEFINE v_res             SMALLINT
   DEFINE v_ruta_docto      LIKE seg_modulo.ruta_docto
   
   IF LENGTH(p_archivo CLIPPED) = 0 THEN
      DISPLAY "Archivo nulo, no procede el guardado ...."
      RETURN v_archivo
   END IF 
          
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   DISPLAY "fn_admon_archivo_mdt - parametros:"
   DISPLAY "p_archivo         - ", p_archivo        
   DISPLAY "p_id_imagen_docto - ", p_id_imagen_docto
   DISPLAY "p_tpo_ren         - ", p_tpo_ren        
   DISPLAY "Ruta origen  - ",v_ruta_docto CLIPPED||p_archivo
   
   LET v_buf = base.StringBuffer.create()
            
   IF p_tpo_ren = 'A' THEN
      CALL v_buf.append(p_archivo)
      CALL v_buf.replace("tmp", p_id_imagen_docto USING "&&&&", 1)
      LET v_archivo = v_buf.toString()
      DISPLAY "Ruta destino - ",v_ruta_docto CLIPPED||v_archivo CLIPPED
      CALL os.Path.rename(v_ruta_docto CLIPPED||p_archivo CLIPPED, v_ruta_docto CLIPPED||v_archivo CLIPPED) RETURNING v_res
      IF NOT v_res THEN
         LET v_archivo = "NO MODIFICADO"
      END IF 
   ELSE     
      IF p_tpo_ren = 'B' THEN
         CALL os.Path.delete(v_ruta_docto CLIPPED||p_archivo CLIPPED) RETURNING v_res
         DISPLAY "os.Path.delete - '", v_ruta_docto CLIPPED||p_archivo  CLIPPED, "' - resultado: ",v_res
         IF v_res THEN
            LET v_archivo = "ELIMINADO"
         ELSE
            LET v_archivo = "NO ELIMINADO"
         END IF 
      END IF
   END IF   
            
   RETURN v_archivo
END FUNCTION

###############################################################################
# Funcion           => fn_admon_archivo_mdt_modificaciones - Mantenimiento a  #
#                      los archivos subidos al servidor de forma temporal     #
#                      durante la modificaci�n de mandatos                    #
# Propietario       => E.F.P                                                  #
# Sistema           => GLO                                                    #
# Entrada:          => p_archivo_org - Nombre del archivo a dar manto orginal #
#                      p_archivo_nvo - Nombre del archivo a dar manto nuevo   #
#                      p_id_imagen_docto - Clave de imagen para referencia    #
#                      p_tpo_ren - Tipo de mantenimiento:                     #
#                                  'A' renombrar archivo temporal en la alta  #
#                                  'B' borrar archivo al cancelar o confir-   #
#                                  mar su eliminaci�n                         #
#                                  'M' Realizar las 2 operaciones anteriores  #
# Salida:           => v_archivo - Nombre del archivo o notificaci�n de man-  #
#                                  tenimiento:                                #
#                                  'A' Archivo renombrado de temporal por su  #
#                                  nombre definitivo o leyenda NO MODIFICADO  #
#                                  'B' Archivo renombrado de temporal por su  #
#                                  nombre definitivo o leyenda de NO ELIMINADO#
# Modificacion      => Alexandro Hollmann, EFP                                #
# Fecha             => 06 Marzo 2012                                          #
###############################################################################
FUNCTION fn_admon_archivo_mdt_modificaciones(p_archivo_org,p_archivo_nvo, p_id_imagen_docto, p_tpo_ren)
   DEFINE p_archivo_org     STRING
   DEFINE p_archivo_nvo     STRING
   DEFINE p_id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto
   DEFINE p_tpo_ren         CHAR(1)
   DEFINE v_archivo         STRING
   DEFINE v_buf             base.StringBuffer
   DEFINE v_res             SMALLINT
   DEFINE v_ruta_docto      LIKE seg_modulo.ruta_docto

   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'
   
   DISPLAY "fn_admon_archivo_mdt_modificaciones - parametros:"
   DISPLAY "p_archivo_org     - ", p_archivo_org        
   DISPLAY "p_archivo_nvo     - ", p_archivo_nvo
   DISPLAY "p_tpo_ren         - ", p_tpo_ren        

   IF p_tpo_ren = 'A' OR p_tpo_ren = 'M' THEN

      IF LENGTH(p_archivo_nvo CLIPPED) = 0 THEN
         DISPLAY "Archivo nulo, no procede el guardado ...."
         RETURN v_archivo
      END IF 
       LET v_buf = base.StringBuffer.create()
      CALL v_buf.append(p_archivo_nvo)
      CALL v_buf.replace("tmp", p_id_imagen_docto USING "&&&&", 1)
      LET v_archivo = v_buf.toString()
      DISPLAY "os.Path.rename - Ruta origen  - ",v_ruta_docto CLIPPED||p_archivo_nvo CLIPPED
      DISPLAY "os.Path.rename - Ruta destino - ",v_ruta_docto CLIPPED||v_archivo CLIPPED
      CALL os.Path.rename(v_ruta_docto CLIPPED||p_archivo_nvo CLIPPED, v_ruta_docto CLIPPED||v_archivo CLIPPED) RETURNING v_res
      IF NOT v_res THEN
         LET v_archivo = "NO MODIFICADO"
         RETURN v_archivo
      END IF 
   END IF

   IF p_tpo_ren = 'B' OR p_tpo_ren = 'M' THEN
      --IF p_archivo_org <> p_archivo_nvo AND p_archivo_nvo.substring THEN
      
      --END IF
      CALL os.Path.delete(v_ruta_docto CLIPPED||p_archivo_org CLIPPED) RETURNING v_res
      DISPLAY "os.Path.delete - '", v_ruta_docto CLIPPED||p_archivo_org  CLIPPED, "' - resultado: ",v_res
      IF NOT v_res THEN
         LET v_archivo = "NO ELIMINADO"
      END IF 
   END IF

   RETURN v_archivo
END FUNCTION

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG01                                                   #
#Objetivo          => Reporte de solicitudes de mandatos                       #
#Autor             => Hugo C�sar Ram�rez Grac�a                                #
#Modifico          => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 03/04/2012                                               #
#Fecha Modificacion=> 04/04/2012                                               #
################################################################################
REPORT rpt_solicitudes_mandatos(v_datos_generales,v_det_operacion,v_detalle_rechazos)
DEFINE v_datos_generales RECORD 
        v_origen         char(40),
        v_proceso        char(40),
        v_lote_mdt       DECIMAL(9,0),
        v_fecha_lote     CHAR(10),--LIKE mdt_solicitud_mandato.f_lote,
        v_altas          INTEGER,
        v_bajas          INTEGER,
        v_modificaciones INTEGER
       END RECORD,
       v_det_operacion RECORD
        v_altas_aceptadas          INTEGER,
        v_bajas_aceptadas          INTEGER,
        v_modificaciones_aceptadas INTEGER,
        v_altas_rechazadas         INTEGER,
        v_bajas_rechazadas         INTEGER,
        v_mdificaciones_rechazadas INTEGER
       END RECORD,
       v_detalle_rechazos RECORD
        v_nss         char(11),
        v_mandato     char(40),
        v_diagnostico char(03)
       END RECORD,
       v_total                INTEGER,
       v_total_aceptadas      INTEGER,
       v_total_rechazadas     INTEGER,
       v_total_det_aceptadas  INTEGER,
       v_total_det_rechazadas INTEGER,       
       v_auxiliar1       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar2       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar3       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar4       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar6       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar7,
       v_auxiliar7_1,
       v_auxiliar7_2,
       v_auxiliar7_3,
       v_auxiliar7_4,
       v_auxiliar7_5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar8       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar9       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar10       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar11       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar12       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar13       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_auxiliar14,
       v_auxiliar14_1,
       v_auxiliar14_2,
       v_auxiliar14_3,
       v_auxiliar14_4,
       v_auxiliar14_5       RECORD 
        tpo_mandato      STRING,
        operacion        CHAR(20),
        total_mdt        INTEGER
       END RECORD,
       v_pagina          SMALLINT,
       v_diagnostico     VARCHAR(40)
   OUTPUT
      PAGE LENGTH 100 
      
         
   FORMAT

      FIRST PAGE HEADER
DISPLAY "v_datos_generales: ",v_datos_generales.* 
DISPLAY "v_det_operacion: ",v_det_operacion.*
DISPLAY "v_detalle_rechazo: ",v_detalle_rechazos.*

         PRINTX v_datos_generales.v_origen
         PRINTX v_datos_generales.v_proceso
         PRINTX v_datos_generales.v_lote_mdt
         PRINTX v_datos_generales.v_fecha_lote
         PRINTX v_datos_generales.v_altas
         PRINTX v_datos_generales.v_bajas
         
         PRINTX v_datos_generales.v_modificaciones
         LET v_total = v_datos_generales.v_altas + v_datos_generales.v_bajas + v_datos_generales.v_modificaciones    
         PRINTX v_total
         PRINTX v_det_operacion.v_altas_aceptadas
         PRINTX v_det_operacion.v_bajas_aceptadas
         PRINTX v_det_operacion.v_modificaciones_aceptadas
         LET v_total_aceptadas = v_det_operacion.v_altas_aceptadas + v_det_operacion.v_bajas_aceptadas + v_det_operacion.v_modificaciones_aceptadas
         PRINTX v_total_aceptadas
         PRINTX v_det_operacion.v_altas_rechazadas
         PRINTX v_det_operacion.v_bajas_rechazadas
         PRINTX v_det_operacion.v_mdificaciones_rechazadas
         
         LET v_total_rechazadas = v_det_operacion.v_altas_rechazadas + v_det_operacion.v_bajas_rechazadas + v_det_operacion.v_mdificaciones_rechazadas
         PRINTX v_total_rechazadas

         #Desglose de aceptadas
         LET v_total_det_aceptadas = 0
         LET v_auxiliar1.* = v_r_rpt_aceptadas[1].*
            
         PRINTX v_auxiliar1.tpo_mandato
         PRINTX v_auxiliar1.operacion
         PRINTX v_auxiliar1.total_mdt
            
            {PRINTX v_r_rpt_aceptadas[1].tpo_mandato
            PRINTX v_r_rpt_aceptadas[1].operacion
            PRINTX v_r_rpt_aceptadas[1].total_mdt}
         IF v_r_rpt_aceptadas[1].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[1].total_mdt 
         END IF
         --END FOR
         {PRINTX v_r_rpt_aceptadas[2].tpo_mandato
         PRINTX v_r_rpt_aceptadas[2].operacion
         PRINTX v_r_rpt_aceptadas[2].total_mdt}
         LET v_auxiliar2.* = v_r_rpt_aceptadas[2].*
            
         PRINTX v_auxiliar2.tpo_mandato
         PRINTX v_auxiliar2.operacion
         PRINTX v_auxiliar2.total_mdt
         IF v_r_rpt_aceptadas[2].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[2].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[3].tpo_mandato
         PRINTX v_r_rpt_aceptadas[3].operacion
         PRINTX v_r_rpt_aceptadas[3].total_mdt}
         LET v_auxiliar3.* = v_r_rpt_aceptadas[3].*
            
         PRINTX v_auxiliar3.tpo_mandato
         PRINTX v_auxiliar3.operacion
         PRINTX v_auxiliar3.total_mdt
         IF v_r_rpt_aceptadas[3].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[3].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[4].tpo_mandato
         PRINTX v_r_rpt_aceptadas[4].operacion
         PRINTX v_r_rpt_aceptadas[4].total_mdt}
         LET v_auxiliar4.* = v_r_rpt_aceptadas[4].*
            
         PRINTX v_auxiliar4.tpo_mandato
         PRINTX v_auxiliar4.operacion
         PRINTX v_auxiliar4.total_mdt
         IF v_r_rpt_aceptadas[4].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[4].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[5].tpo_mandato
         PRINTX v_r_rpt_aceptadas[5].operacion
         PRINTX v_r_rpt_aceptadas[5].total_mdt}
         LET v_auxiliar5.* = v_r_rpt_aceptadas[5].*
            
         PRINTX v_auxiliar5.tpo_mandato
         PRINTX v_auxiliar5.operacion
         PRINTX v_auxiliar5.total_mdt
         IF v_r_rpt_aceptadas[5].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[5].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[6].tpo_mandato
         PRINTX v_r_rpt_aceptadas[6].operacion
         PRINTX v_r_rpt_aceptadas[6].total_mdt}
         LET v_auxiliar6.* = v_r_rpt_aceptadas[6].*
            
         PRINTX v_auxiliar6.tpo_mandato
         PRINTX v_auxiliar6.operacion
         PRINTX v_auxiliar6.total_mdt
         IF v_r_rpt_aceptadas[6].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[6].total_mdt
         END IF
         {PRINTX v_r_rpt_aceptadas[7].tpo_mandato
         PRINTX v_r_rpt_aceptadas[7].operacion
         PRINTX v_r_rpt_aceptadas[7].total_mdt}
         LET v_auxiliar7.* = v_r_rpt_aceptadas[7].*
            
         PRINTX v_auxiliar7.tpo_mandato
         PRINTX v_auxiliar7.operacion
         PRINTX v_auxiliar7.total_mdt
         IF v_r_rpt_aceptadas[7].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[7].total_mdt            
         END IF

         LET v_auxiliar7_1.* = v_r_rpt_aceptadas[8].*
            
         PRINTX v_auxiliar7_1.tpo_mandato
         PRINTX v_auxiliar7_1.operacion
         PRINTX v_auxiliar7_1.total_mdt
         IF v_r_rpt_aceptadas[8].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[8].total_mdt            
         END IF

         LET v_auxiliar7_2.* = v_r_rpt_aceptadas[9].*
            
         PRINTX v_auxiliar7_2.tpo_mandato
         PRINTX v_auxiliar7_2.operacion
         PRINTX v_auxiliar7_2.total_mdt
         IF v_r_rpt_aceptadas[9].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[9].total_mdt            
         END IF

         LET v_auxiliar7_3.* = v_r_rpt_aceptadas[10].*
            
         PRINTX v_auxiliar7_3.tpo_mandato
         PRINTX v_auxiliar7_3.operacion
         PRINTX v_auxiliar7_3.total_mdt
         IF v_r_rpt_aceptadas[10].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[10].total_mdt            
         END IF

         LET v_auxiliar7_4.* = v_r_rpt_aceptadas[11].*
            
         PRINTX v_auxiliar7_4.tpo_mandato
         PRINTX v_auxiliar7_4.operacion
         PRINTX v_auxiliar7_4.total_mdt
         IF v_r_rpt_aceptadas[11].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[11].total_mdt            
         END IF

         LET v_auxiliar7_5.* = v_r_rpt_aceptadas[12].*
            
         PRINTX v_auxiliar7_5.tpo_mandato
         PRINTX v_auxiliar7_5.operacion
         PRINTX v_auxiliar7_5.total_mdt
         IF v_r_rpt_aceptadas[12].total_mdt > 0 THEN
            LET v_total_det_aceptadas = v_total_det_aceptadas + v_r_rpt_aceptadas[12].total_mdt            
         END IF

         PRINTX v_total_det_aceptadas


         # Desgloce de rechazadas
         LET v_total_det_rechazadas = 0
         LET v_auxiliar8.* = v_r_rpt_canceladas[1].*
            
         PRINTX v_auxiliar8.tpo_mandato
         PRINTX v_auxiliar8.operacion
         PRINTX v_auxiliar8.total_mdt
         IF v_r_rpt_canceladas[1].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[1].total_mdt  
         END IF
         --END FOR
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[2].total_mdt
         LET v_auxiliar9.* = v_r_rpt_canceladas[2].*
            
         PRINTX v_auxiliar9.tpo_mandato
         PRINTX v_auxiliar9.operacion
         PRINTX v_auxiliar9.total_mdt
         IF v_r_rpt_canceladas[2].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[2].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[3].total_mdt
         LET v_auxiliar10.* = v_r_rpt_canceladas[3].*
            
         PRINTX v_auxiliar10.tpo_mandato
         PRINTX v_auxiliar10.operacion
         PRINTX v_auxiliar10.total_mdt
         IF v_r_rpt_canceladas[3].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[3].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[4].total_mdt
         LET v_auxiliar11.* = v_r_rpt_canceladas[4].*
            
         PRINTX v_auxiliar11.tpo_mandato
         PRINTX v_auxiliar11.operacion
         PRINTX v_auxiliar11.total_mdt
         IF v_r_rpt_canceladas[4].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[4].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[5].total_mdt
         LET v_auxiliar12.* = v_r_rpt_canceladas[5].*
            
         PRINTX v_auxiliar12.tpo_mandato
         PRINTX v_auxiliar12.operacion
         PRINTX v_auxiliar12.total_mdt
         IF v_r_rpt_canceladas[5].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[5].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[6].total_mdt
         LET v_auxiliar13.* = v_r_rpt_canceladas[6].*
            
         PRINTX v_auxiliar13.tpo_mandato
         PRINTX v_auxiliar13.operacion
         PRINTX v_auxiliar13.total_mdt
         IF v_r_rpt_canceladas[6].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[6].total_mdt
         END IF
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].tpo_mandato
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].operacion
         -- TMP AHM PRINTX v_r_rpt_canceladas[7].total_mdt
         LET v_auxiliar14.* = v_r_rpt_canceladas[7].*
            
         PRINTX v_auxiliar14.tpo_mandato
         PRINTX v_auxiliar14.operacion
         PRINTX v_auxiliar14.total_mdt
         IF v_r_rpt_canceladas[7].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[7].total_mdt
         END IF

         LET v_auxiliar14_1.* = v_r_rpt_canceladas[8].*
            
         PRINTX v_auxiliar14_1.tpo_mandato
         PRINTX v_auxiliar14_1.operacion
         PRINTX v_auxiliar14_1.total_mdt
         IF v_r_rpt_canceladas[8].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[8].total_mdt
         END IF

         LET v_auxiliar14_2.* = v_r_rpt_canceladas[9].*
            
         PRINTX v_auxiliar14_2.tpo_mandato
         PRINTX v_auxiliar14_2.operacion
         PRINTX v_auxiliar14_2.total_mdt
         IF v_r_rpt_canceladas[9].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[9].total_mdt
         END IF

         LET v_auxiliar14_3.* = v_r_rpt_canceladas[10].*
            
         PRINTX v_auxiliar14_3.tpo_mandato
         PRINTX v_auxiliar14_3.operacion
         PRINTX v_auxiliar14_3.total_mdt
         IF v_r_rpt_canceladas[10].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[10].total_mdt
         END IF

         LET v_auxiliar14_4.* = v_r_rpt_canceladas[11].*
            
         PRINTX v_auxiliar14_4.tpo_mandato
         PRINTX v_auxiliar14_4.operacion
         PRINTX v_auxiliar14_4.total_mdt
         IF v_r_rpt_canceladas[11].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[11].total_mdt
         END IF

         LET v_auxiliar14_5.* = v_r_rpt_canceladas[12].*
            
         PRINTX v_auxiliar14_5.tpo_mandato
         PRINTX v_auxiliar14_5.operacion
         PRINTX v_auxiliar14_5.total_mdt
         IF v_r_rpt_canceladas[12].total_mdt > 0 THEN
            LET v_total_det_rechazadas = v_total_det_rechazadas + v_r_rpt_canceladas[12].total_mdt
         END IF

         PRINTX v_total_det_rechazadas
         
      
      ON EVERY ROW
         CALL fn_recupera_desc_diagnostico(v_detalle_rechazos.v_diagnostico) RETURNING v_diagnostico
         IF LENGTH(v_detalle_rechazos.v_nss CLIPPED) >0 THEN
            IF LENGTH(v_diagnostico) = 0 THEN
               LET v_diagnostico = v_detalle_rechazos.v_diagnostico,"-SIN DESCRIPCION DE CATALOGO"
            ELSE
               LET v_diagnostico = v_detalle_rechazos.v_diagnostico,"-",v_diagnostico
            END IF
         ELSE
            LET v_diagnostico = " "
         END IF
         PRINTX v_detalle_rechazos.v_nss," ",v_detalle_rechazos.v_mandato," ",v_diagnostico
         {PRINTX v_detalle_rechazos.v_mandato
         PRINTX v_detalle_rechazos.v_diagnostico}

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         
         

END REPORT

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG01                                                   #
#Objetivo          => Funci�n para recuperar descripciones de diagnosticos     #
#Autor             => Alexandro Hollmann, EFP                                  #
#Fecha Inicio      => 19/04/2012                                               #
#Fecha Modificacion=>                                                          #
################################################################################
FUNCTION fn_recupera_desc_diagnostico(p_diagnostico)
DEFINE p_diagnostico      LIKE mdt_solicitud_mandato.diagnostico,
       v_desc_diagnostico LIKE mdt_cat_rechazo_inst.descripcion
   
   SELECT descripcion INTO v_desc_diagnostico
     FROM mdt_cat_rechazo_inst
   WHERE diagnostico = p_diagnostico
   
   IF STATUS = NOTFOUND THEN
      RETURN ""
   ELSE
      RETURN v_desc_diagnostico
   END IF 
   
END FUNCTION                                                                                                                                                                                                        HPSG02.4gl                                                                                          0000777 0000212 0001751 00000002721 13113322760 012300  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-04-2012
--===============================================================

GLOBALS
DEFINE v_r_rpt_aceptadas ARRAY[12] OF RECORD -- registro de resumen por estado
          tpo_mandato      STRING,
          operacion        CHAR(20),
          total_mdt        INTEGER
       END RECORD
DEFINE v_r_rpt_canceladas ARRAY[12] OF RECORD -- registro de resumen por estado
         tpo_mandato      STRING,
         operacion        CHAR(20),
         total_mdt        INTEGER
      END RECORD


CONSTANT g_proceso_cod_pago_mandatos            INTEGER = 3103 # Pago de servicio

CONSTANT g_proceso_cod_rev_liq_pago_mandatos    INTEGER = 1311 # Reverso liquidaci�n pago de mandatos
CONSTANT g_proceso_cod_rev_preliq_pago_mandatos INTEGER = 1312 # Reverso preliquidaci�n Pago de mandatos    

CONSTANT g_proceso_cod_abonos_mandatos          INTEGER = 3101 # Traspaso Fondo Servicio
CONSTANT g_proceso_cod_originacion_deudor       INTEGER = 301 # Traspaso Fondo Servicio

CONSTANT g_opera_cod_carga          INTEGER = 1
CONSTANT g_opera_cod_integracion    INTEGER = 2
CONSTANT g_opera_cod_preliquidacion INTEGER = 3
CONSTANT g_opera_cod_liquidacion    INTEGER = 4

CONSTANT g_estado_abonado_pago_mdt      SMALLINT = 100
CONSTANT g_estado_preliquidado_pago_mdt SMALLINT = 102
CONSTANT g_estado_liquidado_pago_mdt    SMALLINT = 110

END GLOBALS

                                               HPSG03.4gl                                                                                          0000777 0000212 0001751 00000135432 13113322761 012310  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-07-2013
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTG03                                                   #
#Objetivo          => funcion para recuperar los mandatos a preliquidar seg�n  #
#                     configuraci�n de ejecucion                               #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 10 Julio 2013                                            #
################################################################################
FUNCTION fn_valida_ejecucion_mandato(p_estado_abonado_pago_mdt)
DEFINE p_estado_abonado_pago_mdt SMALLINT,
       p_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,
         v_complemento_recurrencia SMALLINT,
         v_ejecuta_cod      SMALLINT,
         v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
         v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD,
       v_indice       SMALLINT,
       v_hora_actual  CHAR(5),
       v_actual_habil SMALLINT,
       v_n_ultimo     CHAR(6),
       v_dia_actual   DATE,
       v_dia_natural  SMALLINT,
       v_dia_semana   SMALLINT,
       v_dia          SMALLINT,
       v_mes          SMALLINT,
       v_continua     BOOLEAN

   LET v_continua = FALSE   
   CALL fn_recupera_mandatos(p_estado_abonado_pago_mdt) RETURNING v_continua, p_mandatos
   IF(v_continua)THEN
      LET v_continua = TRUE
   ELSE
      CALL p_mandatos.clear()
      RETURN v_continua, p_mandatos  
   END IF
   
   FOR v_indice = 1 TO p_mandatos.getLength()
      IF(p_mandatos[v_indice].v_padre <> 0)THEN
         
         CASE p_mandatos[v_indice].v_recurrencia
            

            WHEN 1 # Autom�tica
               # Ejecuta

            WHEN 2 # Diaria
               # Ejecuta

            WHEN 3 # Semanal
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la semana
                     CALL v_obtiene_habil_semanal() RETURNING v_actual_habil,v_n_ultimo
                     IF (v_actual_habil  = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo      = p_mandatos[v_indice].v_descripcion )THEN
                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     LET v_dia_actual  = TODAY
                     LET v_dia_natural = WEEKDAY(v_dia_actual)
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF
 
                  WHEN 3 # Dia
                     LET v_dia_semana = WEEKDAY(TODAY) 
                     CALL UPSHIFT(p_mandatos[v_indice].v_descripcion) RETURNING p_mandatos[v_indice].v_descripcion
                     CASE
                        WHEN p_mandatos[v_indice].v_descripcion = "LUNES"     AND v_dia_semana = 1
                           
                        WHEN p_mandatos[v_indice].v_descripcion = "MARTES"    AND v_dia_semana = 2

                        WHEN p_mandatos[v_indice].v_descripcion = "MIERCOLES" AND v_dia_semana = 3

                        WHEN p_mandatos[v_indice].v_descripcion = "JUEVES"    AND v_dia_semana = 4

                        WHEN p_mandatos[v_indice].v_descripcion = "VIERNES"   AND v_dia_semana = 5

                        WHEN p_mandatos[v_indice].v_descripcion = "SABADO"    AND v_dia_semana = 6

                        WHEN p_mandatos[v_indice].v_descripcion = "DOMINGO"   AND v_dia_semana = 0

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1
                     END CASE

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 4 # Quincenal
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la quincena
                     CALL fn_obtiene_habil_quincenal() RETURNING v_actual_habil, v_n_ultimo

                     IF(v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                        v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     CALL v_obtiene_natural_quincenal() RETURNING v_dia_natural, v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 5 # Mensual
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual del mes
                     CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                     IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF
                     

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual del mes
                     CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 6 

            WHEN 7 # Anual
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     # Obtiene el dia habil actual de la quincena
                     CALL fn_obtiene_habil_ano() RETURNING v_actual_habil,v_n_ultimo
                     IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual de la quincena
                     CALL fn_obtiene_natural_ano() RETURNING v_dia_natural,v_n_ultimo
                     IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 4 # Fecha
                     LET v_dia = DAY(TODAY)USING "&&"
                     LET v_mes = MONTH(TODAY)USING "&&"
                     IF( v_dia = p_mandatos[v_indice].v_descripcion[1,2] AND
                         v_mes = p_mandatos[v_indice].v_descripcion[4,5] )THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1                        
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 8 # Serie

            WHEN 9 # Bimestral
               
               CASE 
                  WHEN p_mandatos[v_indice].v_complemento_recurrencia = 1 AND fn_verifica_mes_impar(TODAY) # verifica mes IMPAR
                     
                     CASE p_mandatos[v_indice].v_ejecuta_cod

                        WHEN 1 # Habil
                           # Obtiene el dia habil actual de la quincena
                           CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                           IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                               v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                              CALL p_mandatos.deleteElement(v_indice)
                              LET v_indice = v_indice - 1
                           END IF

                        WHEN 2 # Natural
                           # Obtiene el dia natural actual de la quincena
                           CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                           IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                               v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                               CALL p_mandatos.deleteElement(v_indice)
                               LET v_indice = v_indice - 1
                           END IF

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1

                     END CASE
                  WHEN p_mandatos[v_indice].v_complemento_recurrencia = 2 AND fn_verifica_mes_par(TODAY) # verifica mes PAR
                     CASE p_mandatos[v_indice].v_ejecuta_cod

                        WHEN 1 # Habil
                           # Obtiene el dia habil actual del mes
                           CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                           IF( v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                               v_n_ultimo     = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                              CALL p_mandatos.deleteElement(v_indice)
                              LET v_indice = v_indice - 1
                           END IF

                        WHEN 2 # Natural
                           # Obtiene el dia natural actual del mes
                           CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                           IF( v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                               v_n_ultimo    = p_mandatos[v_indice].v_descripcion)THEN

                           ELSE
                               CALL p_mandatos.deleteElement(v_indice)
                               LET v_indice = v_indice - 1
                           END IF

                        OTHERWISE
                           CALL p_mandatos.deleteElement(v_indice)
                           LET v_indice = v_indice - 1

                     END CASE

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)  
                     LET v_indice = v_indice - 1

               END CASE

            WHEN 10 # Semestral
               LET v_mes = fn_obtiene_mes_semestre(TODAY)
               CASE p_mandatos[v_indice].v_ejecuta_cod

                  WHEN 1 # Habil
                     
                     # Obtiene el dia habil actual del mes
                     CALL fn_obtiene_habil_mensual() RETURNING v_actual_habil,v_n_ultimo
                     IF((v_actual_habil = p_mandatos[v_indice].v_descripcion OR
                         v_n_ultimo     = p_mandatos[v_indice].v_descripcion) AND 
                         v_mes = p_mandatos[v_indice].v_complemento_recurrencia)THEN
                     
                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  WHEN 2 # Natural
                     # Obtiene el dia natural actual del mes
                     CALL fn_obtiene_natural_mensual() RETURNING v_dia_natural,v_n_ultimo
                     IF((v_dia_natural = p_mandatos[v_indice].v_descripcion OR 
                         v_n_ultimo    = p_mandatos[v_indice].v_descripcion) AND
                         v_mes = p_mandatos[v_indice].v_complemento_recurrencia)THEN

                     ELSE
                        CALL p_mandatos.deleteElement(v_indice)
                        LET v_indice = v_indice - 1
                     END IF

                  OTHERWISE
                     CALL p_mandatos.deleteElement(v_indice)
                     LET v_indice = v_indice - 1

               END CASE

            OTHERWISE
               CALL p_mandatos.deleteElement(v_indice)
               LET v_indice = v_indice - 1

         END CASE

      END IF

   END FOR

   # Valida que a�n existan mandatos
   LET v_continua = FALSE
   FOR v_indice = 1 TO p_mandatos.getLength()
      IF(p_mandatos[v_indice].v_padre <> 0)THEN # solo los mandatos tienen padres diferente de cero
         LET v_continua = TRUE # Desp�es de filtrar por la configuracion se indica que si hay mandatos
         EXIT FOR
      END IF
   END FOR
   IF NOT(v_continua)THEN
      CALL p_mandatos.clear()
   END IF

   RETURN v_continua, p_mandatos
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en la semana                   #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION v_obtiene_habil_semanal()
DEFINE resta smallint

DEFINE hoy     ,
       v_fecha date

DEFINE ultimo  ,
       dia     ,
       mes     ,
       ano     ,
       n_habil ,
       i       ,
       inhabil integer

DEFINE enter char(001)
DEFINE n_ultimo CHAR(006)
DEFINE x           ,
       dia_semana  smallint

   LET hoy     = today
   LET dia     = WEEKDAY(hoy)
   LET n_habil = 0
   LET ultimo  = 0
   LET inhabil = 0

   FOR i = 1 TO dia
      LET dia_semana = WEEKDAY(i)
      IF dia_semana <> 6  and
         dia_semana <> 0  THEN

      LET resta = day(hoy - (dia - i))

      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))


         SELECT "ok"
         FROM   cat_feriado
         WHERE  feriado_fecha = v_fecha
         GROUP BY 1

         IF STATUS <> NOTFOUND  THEN
            LET inhabil = inhabil + 1
         ELSE
            LET n_habil = n_habil + 1
         END IF
      ELSE
          LET inhabil = inhabil + 1
      END IF
   END FOR
   LET ultimo = 0
    FOR i = 1 TO 5

       LET resta   = DAY(hoy - (dia - i))
       LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

       SELECT "ok"
       FROM   cat_feriado
       WHERE  feriado_fecha = v_fecha
       GROUP BY 1

       IF STATUS = NOTFOUND  THEN
          LET ultimo = ultimo + 1
       END IF
   END FOR
   IF n_habil = ultimo THEN
      LET n_ultimo = "ULTIMO"
   ELSE
      LET n_ultimo = "x"
   END IF
   RETURN n_habil,n_ultimo

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en la quincena                 #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_quincenal()

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy      ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
	     ultimo  ,
       n_habil ,
       i       , 
       inhabil     INTEGER 

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)
DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = TODAY
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   IF dia < 16 THEN
      FOR i = 1 TO dia
      
         LET dia_semana = WEEKDAY(i)
         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            LET resta = day(hoy - (dia - i))

            LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
            IF STATUS <> NOTFOUND  THEN 
               LET inhabil = inhabil + 1
            ELSE 
               LET n_habil = n_habil + 1
            END IF
         ELSE 
             LET inhabil = inhabil + 1 
         END IF
      END FOR
	 ELSE
      FOR i = 16 TO dia
      
         LET dia_semana = WEEKDAY(i)
         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            LET resta = day(hoy - (dia - i))
            LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
             IF STATUS <> NOTFOUND  THEN 
                LET inhabil = inhabil + 1
             ELSE 
                LET n_habil = n_habil + 1
             END IF
         ELSE 
            LET inhabil = inhabil + 1 
         END IF
      END FOR
	 END IF 

   IF dia < 16 THEN
      FOR i = 1 TO 15

         LET resta = day(hoy - (dia - i))
         LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
         
         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
         
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
         
            IF STATUS = NOTFOUND  THEN 
               LET ultimo = ultimo + 1
            END IF
         END IF
      END FOR 
   ELSE
      -- Obtiene el ultimo dia del mes proporcionado
      CALL fn_verifica_mes(hoy) RETURNING f_ultima
	    LET dia = DAY(f_ultima)
      FOR i = 16 TO dia 

         LET resta = day(hoy - (dia - i))
         LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
         
         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN
      
            SELECT "ok"
              FROM cat_feriado
             WHERE feriado_fecha = v_fecha
             GROUP BY 1
      
            IF STATUS = NOTFOUND  THEN 
               LET ultimo = ultimo + 1
            END IF
         END IF
      END FOR 
   END IF

	 IF n_habil = ultimo THEN
		  LET n_ultimo = "ULTIMO"
   ELSE 
		  LET n_ultimo = "x"
   END IF
   
   RETURN n_habil,n_ultimo 

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en la quincena               #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION v_obtiene_natural_quincenal()

DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy       = TODAY
   LET dia       = DAY(hoy)
   LET n_natural = 0

   IF dia < 15  THEN
      FOR i = 1 TO dia
         LET n_natural = n_natural + 1
      END FOR
	 ELSE 
      FOR i = 15 TO dia
         LET n_natural = n_natural + 1
      END FOR
	 END IF

   IF dia < 15 THEN
      LET ultimo = 15
   ELSE 
      -- Obtiene el ultimo dia del mes proporcionado
      CALL fn_verifica_mes(hoy) RETURNING f_ultima
	    LET ultimo = DAY(f_ultima)
   END IF

   IF ultimo = n_natural THEN
		  LET n_ultimo = "ULTIMO"
   ELSE
		  LET n_ultimo = "x" 
   END IF
   
   RETURN n_natural,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual en el mes                      #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_mensual()

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_habil ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)


DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = TODAY
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   FOR i = 1 TO dia

      LET resta = day(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      LET dia_semana = WEEKDAY(v_fecha)

      IF dia_semana <> 6  AND
         dia_semana <> 0  THEN

         SELECT "ok"
           FROM cat_feriado
          WHERE feriado_fecha = v_fecha
          GROUP BY 1

         IF STATUS <> NOTFOUND  THEN 
            LET inhabil = inhabil + 1
         ELSE 
            LET n_habil = n_habil + 1
         END IF
      ELSE 
          LET inhabil = inhabil + 1 
      END IF
   END FOR


   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
   
   LET dia = DAY(f_ultima)

   FOR i = 1 TO dia

      LET resta   = DAY(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN

            SELECT "ok"
            FROM   cat_feriado
            WHERE  feriado_fecha = v_fecha
            GROUP BY 1


            IF STATUS = NOTFOUND  THEN

               LET ultimo = ultimo + 1
            END IF
         END IF
   END FOR

   IF ultimo = n_habil THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF

   RETURN n_habil,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en el mes                    #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_natural_mensual()

DEFINE f_ultima,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy       = TODAY
   LET dia       = DAY(hoy)
   LET n_natural = 0
   
   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
	 
	 LET ultimo = DAY(f_ultima)
	 IF ultimo = dia THEN
	    LET n_ultimo = "ULTIMO"
   ELSE 
	    LET n_ultimo = "x"
   END IF

   RETURN dia,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil actual del ano                        #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_ano()
DEFINE v_habil  INTEGER
DEFINE v_habil_ano  INTEGER
DEFINE v_ultimo CHAR(6)
DEFINE v_mes    SMALLINT
DEFINE v_pos    SMALLINT
DEFINE v_fecha  CHAR(10)
DEFINE d_fecha  DATE
DEFINE f_ultima DATE
DEFINE n_ultimo CHAR(6)

  LET v_habil_ano = 0
  LET v_mes = MONTH(TODAY)
  
  FOR v_pos = 1 TO v_mes-1
  	CASE v_pos
  		 WHEN 1
  		 	  LET v_fecha = '01/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 2
  		 	  LET v_fecha = '02/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 3
  		 	  LET v_fecha = '03/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 4
  		 	  LET v_fecha = '04/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 5
  		 	  LET v_fecha = '05/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 6
  		 	  LET v_fecha = '06/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 7
  		 	  LET v_fecha = '07/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 8
  		 	  LET v_fecha = '08/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 9
  		 	  LET v_fecha = '09/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 10
  		 	  LET v_fecha = '10/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
  		 WHEN 11
  		 	  LET v_fecha = '11/01/',YEAR(TODAY)
  		 	  LET d_fecha = v_fecha
  		 	  CALL fn_verifica_mes(d_fecha) RETURNING f_ultima
          CALL fn_obtiene_habil_mensual_xfecha(f_ultima) RETURNING v_habil, v_ultimo
          LET v_habil_ano = v_habil_ano + v_habil
    END CASE
  END FOR
  
  CALL fn_obtiene_habil_mensual() RETURNING v_habil, v_ultimo
  LET v_habil_ano = v_habil_ano + v_habil
  
   IF 265 = v_habil_ano THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF
  
  RETURN v_habil_ano, n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene el ultimo dia del mes proporcionado             #
#Parametros Entrada=> hoy4 - fecha a verificar                                #
#Autor             => Jesus David Yanez Moreno                                #
#Modifico          => Alexandro Hollmann, EFP                                 #
#Fecha Modificacion=> 22 Mayo  2012                                           #
###############################################################################
FUNCTION fn_verifica_mes(hoy4)

DEFINE mes_int CHAR(10)
DEFINE hoy4    DATE
DEFINE hoy3    DATE

   CASE MONTH(hoy4)
      WHEN  1 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  2 IF YEAR(hoy4) MOD 4 = 0 THEN     
                  LET hoy3 = MDY(MONTH(hoy4),29,YEAR(hoy4)) 
              ELSE
                  LET hoy3 = MDY(MONTH(hoy4),28,YEAR(hoy4)) 
              END IF
      WHEN  3 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  4 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN  5 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  6 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN  7 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  8 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN  9 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN 10 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
      WHEN 11 LET hoy3 = MDY(MONTH(hoy4),30,YEAR(hoy4))
      WHEN 12 LET hoy3 = MDY(MONTH(hoy4),31,YEAR(hoy4))
   END CASE

   RETURN hoy3

END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia habil para una fecha especifica del mes     #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_habil_mensual_xfecha(p_fecha)
DEFINE p_fecha     DATE

DEFINE resta       SMALLINT
DEFINE f_ultima ,
	     hoy     ,
       v_fecha     DATE

DEFINE dia     , 
       mes     ,
       ano     ,
       n_habil ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)


DEFINE x           ,
       dia_semana  SMALLINT

   LET hoy     = p_fecha
   LET dia     = DAY(hoy)
   LET n_habil = 0
   LET inhabil = 0

   FOR i = 1 TO dia

      LET resta = day(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))
      LET dia_semana = WEEKDAY(v_fecha)

      IF dia_semana <> 6  AND
         dia_semana <> 0  THEN

         SELECT "ok"
           FROM cat_feriado
          WHERE feriado_fecha = v_fecha
          GROUP BY 1

         IF STATUS <> NOTFOUND  THEN 
            LET inhabil = inhabil + 1
         ELSE 
            LET n_habil = n_habil + 1
         END IF
      ELSE 
          LET inhabil = inhabil + 1 
      END IF
   END FOR


   -- Obtiene el ultimo dia del mes proporcionado
   CALL fn_verifica_mes(hoy) RETURNING f_ultima
   
   LET dia = DAY(f_ultima)

   FOR i = 1 TO dia

      LET resta   = DAY(hoy - (dia - i))
      LET v_fecha = MDY(MONTH(hoy),resta,YEAR(hoy))

         LET dia_semana = WEEKDAY(v_fecha)

         IF dia_semana <> 6  AND
            dia_semana <> 0  THEN

            SELECT "ok"
            FROM   cat_feriado
            WHERE  feriado_fecha = v_fecha
            GROUP BY 1


            IF STATUS = NOTFOUND  THEN

               LET ultimo = ultimo + 1
            END IF
         END IF
   END FOR

   IF ultimo = n_habil THEN
      LET n_ultimo = "ULTIMO"
   ELSE 
   	  LET n_ultimo = "x"
   END IF

   RETURN n_habil,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => Obtiene dia natural actual en el ano                    #
#Parametros Entrada=> Ninguno                                                 #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha             => 29 Mayo  2012                                           #
###############################################################################
FUNCTION fn_obtiene_natural_ano()

DEFINE f_ultima,
	     inicio  ,
	     hoy     ,
       v_fecha     DATE
DEFINE v_inicio  CHAR(10)

DEFINE dia     , 
       mes     ,
       ano     ,
       n_natural ,
	     ultimo  ,
       i       , 
       inhabil     INTEGER

DEFINE enter       CHAR(1)
DEFINE n_ultimo    CHAR(6)

DEFINE x           ,
       dia_semana  SMALLINT

   LET v_inicio  = '12/31/',YEAR(TODAY)-1
   LET inicio    = v_inicio
   LET hoy       = TODAY
   LET dia       = hoy - inicio
   
	 LET ultimo = 365
	 IF ultimo = dia THEN
	    LET n_ultimo = "ULTIMO"
   ELSE 
	    LET n_ultimo = "x"
   END IF

   RETURN dia,n_ultimo
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => verifica si el mes par para la fecha de paramatro       #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_verifica_mes_par(p_fecha)
DEFINE p_fecha DATE,
       v_mes   SMALLINT,
       v_par   BOOLEAN

   LET v_par = FALSE
   LET v_mes = MONTH(p_fecha)
   

   CASE v_mes

      WHEN 2    LET v_par = TRUE
      WHEN 4    LET v_par = TRUE
      WHEN 6    LET v_par = TRUE
      WHEN 8    LET v_par = TRUE
      WHEN 10   LET v_par = TRUE
      WHEN 12   LET v_par = TRUE
      OTHERWISE LET v_par = FALSE

   END CASE

   RETURN v_par
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => verifica si el mes impar para la fecha de paramatro     #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_verifica_mes_impar(p_fecha)
DEFINE p_fecha DATE,
       v_mes   SMALLINT,
       v_par   BOOLEAN

   LET v_par = FALSE
   LET v_mes = MONTH(p_fecha)
   

   CASE v_mes

      WHEN 1    LET v_par = TRUE
      WHEN 3    LET v_par = TRUE
      WHEN 5    LET v_par = TRUE
      WHEN 7    LET v_par = TRUE
      WHEN 9    LET v_par = TRUE
      WHEN 11   LET v_par = TRUE
      OTHERWISE LET v_par = FALSE

   END CASE

   RETURN v_par
END FUNCTION

###############################################################################
#Modulo            => BAT                                                     #
#Programa          => BATG01                                                  #
#Objetivo          => obtiene el mes de un semestre de un rango de 1 -6       #
#Parametros Entrada=> p_fecha                                                 #
#Autor             =>                                  #
#Fecha             => 08 Julio 2013                                           #
###############################################################################
FUNCTION fn_obtiene_mes_semestre(p_fecha)
DEFINE p_fecha        DATE,
       v_mes          SMALLINT,
       v_mes_semestre SMALLINT

   LET v_mes = MONTH(p_fecha)

   CASE v_mes
      WHEN 1 LET v_mes_semestre = 1
      WHEN 2 LET v_mes_semestre = 2
      WHEN 3 LET v_mes_semestre = 3
      WHEN 4 LET v_mes_semestre = 4
      WHEN 5 LET v_mes_semestre = 5
      WHEN 6 LET v_mes_semestre = 6
      WHEN 7 LET v_mes_semestre = 1
      WHEN 8 LET v_mes_semestre = 2
      WHEN 8 LET v_mes_semestre = 3
      WHEN 10 LET v_mes_semestre = 4
      WHEN 11 LET v_mes_semestre = 5
      WHEN 12 LET v_mes_semestre = 6
   END CASE

   RETURN v_mes_semestre
END FUNCTION

{===============================================================================
Nombre: fn_recupera_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para 
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_mandatos(p_estado_abonado_pago_mdt)
DEFINE p_estado_abonado_pago_mdt SMALLINT,
       v_consulta STRING,
       v_mandato RECORD
         v_padre           VARCHAR(10),
         v_identificador   SMALLINT,--LIKE mdt_cat_mandato.id_cat_mandato,
         v_descripcion_mdt VARCHAR(40),--LIKE mdt_cat_mandato.desc_mandato,
         v_tpo_mandato     SMALLINT,--LIKE mdt_cat_mandato.tpo_mandato,
         v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
         v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
         v_ejecuta_cod     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
         v_descripcion     CHAR(10)--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
       END RECORD,
       v_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod      SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD,
       v_municipio RECORD
         v_etiqueta       CHAR(40),--LIKE mdt_cat_gpo_etiqueta.etiqueta,
         v_valor_etiqueta CHAR(120)--LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD,
       v_ent_fed RECORD
         v_etiqueta       CHAR(40),--LIKE mdt_cat_gpo_etiqueta.etiqueta,
         v_valor_etiqueta CHAR(120)--LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD,
       v_indice   SMALLINT,
       v_continua BOOLEAN

   LET v_indice   = 2
   LET v_continua = FALSE
   CALL v_mandatos.clear()

   LET v_mandatos[1].v_descripcion   = "TIPOS DE MANDATO"   
   LET v_mandatos[1].v_padre         = "00"
   LET v_mandatos[1].v_identificador = "0"   
   LET v_mandatos[1].v_expandido     = TRUE
   LET v_mandatos[1].v_municipio     = ""

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

   # consulta para recupeerar la entidad federativa del mandato
   LET v_consulta = "\n SELECT etq.etiqueta,ins.valor_etiqueta",
                    "\n   FROM mdt_cat_mandato mdt JOIN mdt_cat_atributo_nivel nvl",
                    "\n     ON nvl.id_cat_mandato = mdt.id_cat_mandato",
                    "\n        JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n        JOIN mdt_cat_gpo_etiqueta etq",
                    "\n     ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA'",
                    "\n    AND mdt.id_cat_mandato = ?"
   PREPARE prp_rec_ent_fed_etiqueta FROM v_consulta  
   
   LET v_consulta = "\n(SELECT 0,",
                    "\n        tpo_mandato,",
                    "\n        desc_tpo_mandato,",
                    "\n        tpo_mandato,",
                    "\n        0,",
                    "\n        0,",
                    "\n        0,",
                    "\n        ''",
                    "\n   FROM mdt_tpo_mandato",
                    "\n  WHERE 1 = 1",
                    "\n  AND tpo_mandato <> 3 ",
                    "\n UNION ALL ",
                    "\n SELECT DISTINCT cat.tpo_mandato,",
                    "\n        cat.id_cat_mandato,",
                    "\n        cat.desc_mandato,",
                    "\n        cat.tpo_mandato,",
                    "\n        eje.recurrencia,",
                    "\n        eje.complemento_recurrencia,",
                    "\n        eje.ejecuta_cod,",
                    "\n        eje.descripcion",
                    "\n   FROM mdt_cat_mandato_ejecuta_pago eje JOIN mdt_cat_mandato cat",
                    "\n     ON cat.id_cat_mandato = eje.id_cat_mandato",
              --      "\n        JOIN mdt_det_aplica_monto mto", # solo se busca que exista el mandato en la tabla
              --      "\n     ON mto.id_cat_mandato = eje.id_cat_mandato",
                    "\n  WHERE cat.estado = ?)",
                    "\n  ORDER BY 4,1"
   
   PREPARE prp_recupera_mandatos_ejecuta FROM v_consulta
   DECLARE cur_recupera_mandatos_ejecuta CURSOR FOR prp_recupera_mandatos_ejecuta   
   FOREACH cur_recupera_mandatos_ejecuta USING p_estado_abonado_pago_mdt
                                          INTO v_mandato.*

      INITIALIZE v_municipio.* TO NULL
      INITIALIZE v_ent_fed.* TO NULL
      IF(v_mandato.v_padre <> 0)THEN         
         # recupera municipio
         EXECUTE prp_rec_municipio_etiqueta USING v_mandato.v_identificador
                                             INTO v_municipio.*
         # recupera entidad
         EXECUTE prp_rec_ent_fed_etiqueta USING v_mandato.v_identificador
                                           INTO v_ent_fed.*
      END IF
                                          
      LET v_mandatos[v_indice].v_padre           = v_mandato.v_padre
      LET v_mandatos[v_indice].v_identificador   = v_mandato.v_identificador
      LET v_mandatos[v_indice].v_descripcion_mdt = v_mandato.v_descripcion_mdt
      LET v_mandatos[v_indice].v_expandido       = FALSE
      LET v_mandatos[v_indice].v_municipio       = v_municipio.v_valor_etiqueta
      LET v_mandatos[v_indice].v_recurrencia     = v_mandato.v_recurrencia
      LET v_mandatos[v_indice].v_complemento_recurrencia = v_mandato.v_complemento_recurrencia
      LET v_mandatos[v_indice].v_ejecuta_cod     = v_mandato.v_ejecuta_cod
      LET v_mandatos[v_indice].v_descripcion     = v_mandato.v_descripcion
      LET v_mandatos[v_indice].v_ent_federeativa = v_ent_fed.v_valor_etiqueta
      LET v_indice = v_indice + 1
      
   END FOREACH
   FREE cur_recupera_mandatos_ejecuta
   
   
   IF(v_mandatos.getLength() > 1)THEN
      LET v_continua = TRUE
   ELSE
      LET v_continua = FALSE
      CALL v_mandatos.clear()
   END IF

   RETURN v_continua, v_mandatos
END FUNCTION
                                                                                                                                                                                                                                      HPSI10.4gl                                                                                          0000777 0000212 0001751 00000037236 13113322764 012316  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-07-2013
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSI10                                                   #
#Objetivo          => Reporte de pago de mandatos                              # 
#Autor             => Hugo Ram�rez                                             #
#Fecha inicio      => 16 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

{===============================================================================
Nombre: fn_consulta_individual
Fecha creacion: 16 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar y generar reporte de consulta individual de movimientos
 de mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_consulta_individual(p_filtro,p_id_derechohabiente,p_id_cat_mandato,p_abonos,p_pagos, p_tpo_consulta)
DEFINE p_filtro             STRING,
       p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       p_id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
       p_abonos             STRING,
       p_pagos              STRING,
       p_tpo_consulta       STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_nss             LIKE afi_derechohabiente.nss,
       v_nombre_completo VARCHAR(50),
       v_nombre          LIKE afi_derechohabiente.nombre_af,
       v_ap_paterno      LIKE afi_derechohabiente.ap_paterno_af,
       v_ap_materno      LIKE afi_derechohabiente.ap_materno_af,
       v_indice          SMALLINT,
       v_nombre_reporte  STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_consulta        STRING,
       v_movimientos     DYNAMIC ARRAY OF RECORD
         v_desc_mandato    LIKE mdt_cat_mandato.desc_mandato,
         v_f_liquida       LIKE cta_movimiento.f_liquida,
         v_movimiento      LIKE cta_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_periodo_pago    LIKE mdt_det_aplica_mandato.periodo_pago,
         v_folio_liquida   LIKE cta_movimiento.folio_liquida,
         v_monto_pesos     LIKE cta_movimiento.monto_pesos,
         v_estado          LIKE mdt_det_aplica_monto.estado
       END RECORD,
       v_cadena_aux        STRING

   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   LET v_indice = 1
   CALL v_movimientos.clear()
   IF( fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/HPSI101.4rp") )THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = v_ruta_listados CLIPPED ,"/consulta_individual"
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_nombre_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(1)
      
      # se asigna la configuraci�n en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      SELECT nss,
             nombre_af,
             ap_paterno_af,
             ap_materno_af
        INTO v_nss,
             v_nombre,
             v_ap_paterno,
             v_ap_materno
        FROM afi_derechohabiente
       WHERE id_derechohabiente = p_id_derechohabiente

      LET v_nombre_completo = v_nombre CLIPPED," ", v_ap_paterno CLIPPED," ", v_ap_materno CLIPPED
      
      CASE p_id_cat_mandato # determina si es consulta por mandato
         WHEN 0 # Consulta Individual
            LET v_cadena_aux = " "
         OTHERWISE # Consulta por mandatos
            LET v_cadena_aux = "\n    AND mto.id_cat_mandato = ?"
      END CASE
      LET v_consulta = "\n SELECT *",
                       "\n   FROM TABLE(MULTISET(",
                       "\n SELECT cat.desc_mandato,",
                       "\n        cta.f_liquida,",
                       "\n        cta.movimiento,",
                       "\n        mov.movimiento_desc,",
--                       "\n        mto.periodo_pago,",
                       "\n        '',",
                       "\n        cta.folio_liquida,",
                       "\n        cta.monto_pesos,",
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
                       "\n                        WHERE movimiento IN (",p_abonos,"))) cta",
                       "\n     ON afi.id_derechohabiente = cta.id_derechohabiente",
                       "\n        JOIN cat_subcuenta sub",
                       "\n     ON sub.subcuenta = cta.subcuenta",
                       "\n        JOIN cat_movimiento mov",
                       "\n     ON mov.movimiento = cta.movimiento",
                       "\n        JOIN hps_det_aplica_servicio mto",
                       "\n     ON cta.id_referencia = mto.id_det_aplica_servicio",
                       "\n        JOIN mdt_cat_mandato cat",
                       "\n     ON cat.id_cat_mandato = mto.id_cat_mandato",
                       --"\n        JOIN mdt_cat_mandato_paquete paq",
                       --"\n     ON paq.cve_mandato = mdt.cve_mandato",
                       
                       --"\n        JOIN hps_det_aplica_servicio apl",
                       --"\n     ON apl.id_det_aplica_servicio = mto.id_det_aplica_servicio",
                       
                       "\n  WHERE ",p_filtro,
                       "\n    AND cta.id_derechohabiente = ?",
                       --"\n    AND mto.id_cat_mandato = ?",
                       v_cadena_aux, # determina si es consulta por mandato
                       
                       "\n UNION ALL",
                    
                       "\n SELECT cat.desc_mandato,",
                       "\n        cta.f_liquida,",
                       "\n        cta.movimiento,",
                       "\n        mov.movimiento_desc,",
                       "\n        '',",
                       "\n        cta.folio_liquida,",
                       "\n        cta.monto_pesos,",
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
                       "\n                        WHERE movimiento IN (",p_pagos,"))) cta",
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
                       "\n        JOIN mdt_cat_mandato cat",
                       "\n     ON cat.id_cat_mandato = mto.id_cat_mandato",
                       "\n  WHERE ",p_filtro,
                       "\n    AND cta.id_derechohabiente = ?",
                       --"\n    AND mto.id_cat_mandato = ?"
                       v_cadena_aux, # determina si es consulta por mandato
                       "\n ))",
                       "\n ORDER BY 1,2"
                       
      PREPARE prp_rec_movimientos FROM v_consulta
      DECLARE cur_rec_movimientos CURSOR FOR prp_rec_movimientos
      CASE p_id_cat_mandato # determina si es consulta por mandato o individual
         WHEN 0 
            FOREACH cur_rec_movimientos USING p_id_derechohabiente,
                                              p_id_derechohabiente
                                         INTO v_movimientos[v_indice].*
               LET v_indice = v_indice + 1

            END FOREACH 
            FREE cur_rec_movimientos
            IF(v_movimientos[v_movimientos.getLength()].v_desc_mandato IS NULL)THEN
               CALL v_movimientos.deleteElement(v_movimientos.getLength())
            END IF
         OTHERWISE 
            FOREACH cur_rec_movimientos USING p_id_derechohabiente,
                                              p_id_cat_mandato,
                                              p_id_derechohabiente,
                                              p_id_cat_mandato
                                         INTO v_movimientos[v_indice].*
               LET v_indice = v_indice + 1

            END FOREACH 
            FREE cur_rec_movimientos
            IF(v_movimientos[v_movimientos.getLength()].v_desc_mandato IS NULL)THEN
               CALL v_movimientos.deleteElement(v_movimientos.getLength())
            END IF
      END CASE 
      

      IF( v_movimientos.getLength() > 0 )THEN
         START REPORT fn_rpt_consulta_inividual TO XML HANDLER v_manejador_rpt
            FOR v_indice = 1 TO v_movimientos.getLength() 
               OUTPUT TO REPORT fn_rpt_consulta_inividual(p_tpo_consulta,
                                                          v_movimientos[v_indice].*,
                                                          v_nss,
                                                          v_nombre_completo,
                                                          p_abonos,
                                                          p_pagos)
            END FOR
         FINISH REPORT fn_rpt_consulta_inividual
      ELSE
         DISPLAY "\n"
         DISPLAY "NO SE ENCONTRARON REGISTROS"
         DISPLAY "\n"
         RETURN
      END IF

   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

{===============================================================================
Nombre: fn_recupera_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para 
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
REPORT fn_rpt_consulta_inividual(p_tpo_consulta,p_movimientos,p_nss,p_nombre_completo,p_abonos,p_pagos)
DEFINE p_tpo_consulta      STRING,
       p_movimientos       RECORD
         v_desc_mandato    LIKE mdt_cat_mandato.desc_mandato,
         v_f_liquida       LIKE cta_movimiento.f_liquida,
         v_movimiento      LIKE cta_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_periodo_pago    LIKE mdt_det_aplica_mandato.periodo_pago,
         v_folio_liquida   LIKE cta_movimiento.folio_liquida,
         v_monto_pesos     LIKE cta_movimiento.monto_pesos,
         v_estado          LIKE mdt_det_aplica_monto.estado
       END RECORD,
       p_nss             LIKE afi_derechohabiente.nss,
       p_nombre_completo VARCHAR(50),
       p_abonos          STRING,
       p_pagos           STRING,
       v_pagina          SMALLINT,
       v_abonado         LIKE cta_movimiento.monto_pesos,
       v_pagado          LIKE cta_movimiento.monto_pesos,
       v_saldo           LIKE cta_movimiento.monto_pesos,
       v_token           base.StringTokenizer,
       v_indice          SMALLINT,
       v_mov_pagos       ARRAY[3] OF SMALLINT,
       v_mov_abonos      ARRAY[3] OF SMALLINT

   --ORDER BY p_movimientos.v_desc_mandato
   FORMAT

      FIRST PAGE HEADER
         # separa los movimientos de abonos
         LET v_token = base.StringTokenizer.create(p_abonos,",")
         LET v_indice = 1
         WHILE v_token.hasMoreTokens()
            LET v_mov_abonos[v_indice] = v_token.nextToken()
            LET v_indice = v_indice + 1
         END WHILE
         # separa los movimientos de pagos
         LET v_token = base.StringTokenizer.create(p_pagos,",")
         LET v_indice = 1
         WHILE v_token.hasMoreTokens()
            LET v_mov_pagos[v_indice] = v_token.nextToken()
            LET v_indice = v_indice + 1
         END WHILE
         
         PRINTX p_tpo_consulta,
                p_nss,
                p_nombre_completo

      BEFORE GROUP OF p_movimientos.v_desc_mandato
         LET v_abonado = 0
         LET v_pagado  = 0
         LET v_saldo   = 0
         PRINTX p_movimientos.v_desc_mandato

      AFTER GROUP OF p_movimientos.v_desc_mandato
         LET v_saldo = v_abonado + v_pagado
         PRINTX v_abonado,
                v_pagado,
                v_saldo

      ON EVERY ROW
         FOR v_indice = 1 TO v_mov_pagos.getLength()
            IF(p_movimientos.v_movimiento = v_mov_pagos[v_indice])THEN
               LET v_pagado = v_pagado + p_movimientos.v_monto_pesos
            END IF

         END FOR
         FOR v_indice = 1 TO v_mov_abonos.getLength()
            IF(p_movimientos.v_movimiento = v_mov_abonos[v_indice])THEN
               LET v_abonado = v_abonado + p_movimientos.v_monto_pesos
            END IF
         END FOR
         
         PRINTX p_movimientos.v_f_liquida,
                p_movimientos.v_movimiento_desc,
                p_movimientos.v_periodo_pago,
                p_movimientos.v_folio_liquida,
                p_movimientos.v_monto_pesos,
                p_movimientos.v_estado

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
END REPORT                                                                                                                                                                                                                                                                                                                                                                  HPSI31.4gl                                                                                          0000777 0000212 0001751 00000053424 13113322756 012317  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 11-07-2013
--==============================================================================

################################################################################
#Modulo            => HPSI                                                     #
#Programa          => HPSI31                                                   #
#Objetivo          => Reporte de pago de mandatos                              # 
#Autor             => Jesus Ya�ez                                              #
#Fecha inicio      => 11 Julio 2013                                            #
################################################################################
DATABASE safre_viv

FUNCTION fn_mdt_rpt_aplicacion_mdt(p_folio,
                                   p_estado,
                                   p_usuario_cod,
                                   p_pid,
                                   p_proceso_cod,
                                   p_opera_preliquidacion,
                                   p_f_proceso,
                                   p_archivo_batch)
                                   
DEFINE p_folio                LIKE glo_folio.folio,
       p_estado               LIKE mdt_det_aplica_monto.estado,
       p_usuario_cod          LIKE mdt_ctr_aplica_pago_mandato.usuario,
       p_pid                  LIKE bat_ctr_operacion.pid,
       p_proceso_cod          LIKE bat_ctr_operacion.proceso_cod,
       p_opera_preliquidacion LIKE bat_ctr_operacion.opera_cod,
       p_f_proceso            DATE,
       p_archivo_batch        STRING,
       
       v_registros  DYNAMIC ARRAY OF RECORD
         v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato,
         v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_ent_federativa      LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_ent_federativa_desc LIKE cat_entidad_federativa.entidad_desc_larga,
         v_municipio           LIKE mdt_det_aplica_pago_mandato.municipio,
         v_municipio_desc      LIKE cat_municipio.municipio_desc,
         v_mandato             LIKE mdt_det_aplica_monto.id_cat_mandato,
         v_mandato_desc        LIKE mdt_cat_mandato.desc_mandato,
         v_monto_pesos         LIKE mdt_det_aplica_pago_mandato.monto_pesos
       END RECORD,
       
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_indice          SMALLINT,
       v_nombre_reporte  STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_consulta        STRING,
       v_sum_prediales     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_mantenimiento LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_servicios     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_texto_rpt         STRING
       
   SELECT ruta_bin,
          ruta_listados
     INTO v_ruta_ejecutable,
          v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   IF(fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED||"/HPSI311.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nombre_reporte = p_usuario_cod CLIPPED, "-",p_archivo_batch,"-",--"-HPSP01-", 
                             p_pid USING "&&&&&", "-", 
                             p_proceso_cod USING "&&&&&", "-", 
                             p_opera_preliquidacion USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nombre_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuraci�n en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      LET v_indice = 1
      LET v_sum_prediales     = 0
      LET v_sum_mantenimiento = 0
      LET v_sum_servicios     = 0

      
      CASE p_estado
      
         WHEN 50 # liquidacion traspaso fondo servicios

            LET v_texto_rpt = "PRELIQUIDACI�N TRASPASO SCTA SERVICIOS"
            LET v_consulta = " SELECT tpo.tpo_mandato,",
                             " tpo.desc_tpo_mandato, ",
                             " ent.entidad_federativa, ",
                             " ent.valor_etiqueta, ",
                             " mun.municipio, ",
                             " mun.valor_etiqueta, ",
                             " cat.id_cat_mandato, ",
                             " cat.desc_mandato, ",
                             " SUM(mto.mto_pesos) ",
                        " FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat ",
                          " ON cat.tpo_mandato = tpo.tpo_mandato ",
                             " JOIN TABLE(MULTISET(SELECT id_cat_mandato, ",
                                                        " id_ctr_aplica_servicio, ",
                                                        " mto_pesos ",
                                                   " FROM hps_det_aplica_servicio  ",
                                                  " WHERE estado = ? ",
                                                  " )) mto ",                                                  
                          " ON mto.id_cat_mandato = cat.id_cat_mandato ",
                             " JOIN hps_ctr_aplica_servicio apctr ",
                          " ON mto.id_ctr_aplica_servicio = apctr.id_ctr_aplica_servicio ",
                             " JOIN ",
                             " TABLE(MULTISET(SELECT nvl.id_cat_mandato, ",
                                                   " ent.entidad_federativa, ",
                                                   " ins.valor_etiqueta ",
                                              " FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                " ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                   " JOIN mdt_cat_gpo_etiqueta etq ",
                                                " ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                   " JOIN cat_entidad_federativa ent ",
                                                " ON ent.entidad_desc_larga = ins.valor_etiqueta ",
                                             " WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA')) ent ",
                          " ON ent.id_cat_mandato = cat.id_cat_mandato ",
                             " JOIN  ",
                             " TABLE(MULTISET(SELECT mun.municipio,  ",
                                                   " nvl.id_cat_mandato, ",
                                                   " mun.entidad_federativa, ",
                                                   " ins.valor_etiqueta ",
                                              " FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins ",
                                                " ON ins.id_atr_nivel = nvl.id_atr_nivel ",
                                                   " JOIN mdt_cat_gpo_etiqueta etq ",
                                                " ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta ",
                                                   " JOIN cat_municipio mun ",
                                                " ON municipio_desc = ins.valor_etiqueta ",
                                             " WHERE etq.etiqueta = 'MUNICIPIO')) mun ",
                          " ON mun.id_cat_mandato = cat.id_cat_mandato ",
                         " AND mun.entidad_federativa = ent.entidad_federativa ",
                       " WHERE apctr.folio_aplica_servicio = ? ",
                       " GROUP BY 1,2,3,4,5,6,7,8 ",
                       " ORDER BY 1,3,5 "

            PREPARE prp_rec_datos_rpt_abonos FROM v_consulta
            DECLARE cur_rec_datos_rpt_abonos CURSOR FOR prp_rec_datos_rpt_abonos
            FOREACH cur_rec_datos_rpt_abonos USING p_estado,
                                                   p_folio
                                              INTO v_registros[v_indice].*
 
               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt_abonos

         WHEN 100 # abonado
            LET v_texto_rpt = "ABONOS MANDATOS"
            LET v_consulta = "\n SELECT tpo.tpo_mandato,",
                             "\n        tpo.desc_tpo_mandato,",
                             "\n        ent.entidad_federativa,",
                             "\n        ent.valor_etiqueta,",
                             "\n        mun.municipio,",
                             "\n        mun.valor_etiqueta,",
                             "\n        cat.id_cat_mandato,",
                             "\n        cat.desc_mandato,",
                             "\n        SUM(mto.monto_pesos)",
                             "\n   FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat",
                             "\n     ON cat.tpo_mandato = tpo.tpo_mandato",
                             "\n        JOIN TABLE(MULTISET(SELECT id_cat_mandato,",
                             "\n                                  id_det_aplica_mandato,",
                             "\n                                  monto_pesos",
                             "\n                             FROM mdt_det_aplica_monto",
                             "\n                            WHERE estado = ?",
                             "\n                            GROUP BY 1,2,3)) mto",
                             "\n     ON mto.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN mdt_det_aplica_mandato apl",
                             "\n     ON apl.id_det_aplica_mandato = mto.id_det_aplica_mandato",
                             "\n        JOIN",
                             "\n        TABLE(MULTISET(SELECT nvl.id_cat_mandato,",
                             "\n                              ent.entidad_federativa,",
                             "\n                              ins.valor_etiqueta",
                             "\n                         FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                             "\n                           ON ins.id_atr_nivel = nvl.id_atr_nivel",
                             "\n                              JOIN mdt_cat_gpo_etiqueta etq",
                             "\n                           ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                             "\n                              JOIN cat_entidad_federativa ent",
                             "\n                           ON ent.entidad_desc_larga = ins.valor_etiqueta",
                             "\n                        WHERE etq.etiqueta = 'ENTIDAD FEDERATIVA')) ent",
                             "\n     ON ent.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN ",
                             "\n        TABLE(MULTISET(SELECT mun.municipio, ",
                             "\n                              nvl.id_cat_mandato,",
                             "\n                              mun.entidad_federativa,",
                             "\n                              ins.valor_etiqueta",
                             "\n                         FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                             "\n                           ON ins.id_atr_nivel = nvl.id_atr_nivel",
                             "\n                              JOIN mdt_cat_gpo_etiqueta etq",
                             "\n                           ON etq.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                             "\n                              JOIN cat_municipio mun",
                             "\n                           ON municipio_desc = ins.valor_etiqueta",
                             "\n                        WHERE etq.etiqueta = 'MUNICIPIO')) mun",
                             "\n     ON mun.id_cat_mandato = cat.id_cat_mandato",
                             "\n    AND mun.entidad_federativa = ent.entidad_federativa",
                             "\n  WHERE apl.folio_dispersion = ?",
                             "\n  GROUP BY 1,2,3,4,5,6,7,8",
                             "\n  ORDER BY 1,3,5"
            PREPARE prp_rec_datos_rpt_abonado FROM v_consulta
            DECLARE cur_rec_datos_rpt_abonado CURSOR FOR prp_rec_datos_rpt_abonado
            FOREACH cur_rec_datos_rpt_abonado USING p_estado,
                                                  p_folio
                                             INTO v_registros[v_indice].*

               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt_abonado
            
         WHEN 102 # preliquidado pagos
            LET v_texto_rpt = "PRELIQUIDACI�N PAGO MANDATOS"
            LET v_consulta = "\n SELECT tpo.tpo_mandato,",
                             "\n        tpo.desc_tpo_mandato,",
                             "\n        pgo.ent_federativa,",
                             "\n        ent.entidad_desc_larga,",
                             "\n        pgo.municipio,",
                             "\n        mun.municipio_desc,",
                             "\n        cat.id_cat_mandato,",
                             "\n        cat.desc_mandato,",
                             "\n        SUM(pgo.monto_pesos)",
                             "\n   FROM mdt_tpo_mandato tpo LEFT OUTER JOIN mdt_cat_mandato cat",
                             "\n     ON cat.tpo_mandato = tpo.tpo_mandato",
                             "\n        JOIN TABLE(MULTISET(SELECT id_cat_mandato,",
                             "\n                                  id_det_aplica_pago_servicio,",
                             "\n                                  estado",
                             "\n                             FROM hps_det_aplica_monto",
                             "\n                            WHERE 1 = 1",
                             "\n                            GROUP BY 1,2,3)) mto",
                             "\n     ON mto.id_cat_mandato = cat.id_cat_mandato",
                             "\n        JOIN hps_det_aplica_pago_servicio pgo",
                             "\n     ON mto.id_det_aplica_pago_servicio = pgo.id_det_aplica_pago_servicio",
                             "\n        JOIN hps_ctr_aplica_pago_servicio ctr",
                             "\n     ON pgo.id_ctr_aplica_pago_servicio = ctr.id_ctr_aplica_pago_servicio",
                             "\n        LEFT OUTER JOIN cat_entidad_federativa ent",
                             "\n     ON ent.entidad_federativa = pgo.ent_federativa",
                             "\n        LEFT OUTER JOIN cat_municipio mun",
                             "\n     ON mun.municipio = pgo.municipio",
                             "\n    AND mun.entidad_federativa = pgo.ent_federativa",
                             "\n  WHERE mto.estado = ?", # preliquidado
                             "\n    AND ctr.folio_pago_servicio = ?",
                             "\n  GROUP BY 1,2,3,4,5,6,7,8",
                             "\n  ORDER BY 1,3,5"
            PREPARE prp_rec_datos_rpt FROM v_consulta
            DECLARE cur_rec_datos_rpt CURSOR FOR prp_rec_datos_rpt
            FOREACH cur_rec_datos_rpt USING p_estado,
                                            p_folio
                                       INTO v_registros[v_indice].*

               CASE v_registros[v_indice].v_tpo_mandato

                  WHEN 1 # Prediales
                     LET v_sum_prediales     = v_sum_prediales + v_registros[v_indice].v_monto_pesos      

                  WHEN 2 # Mantenimiento
                     LET v_sum_mantenimiento = v_sum_mantenimiento + v_registros[v_indice].v_monto_pesos      

                  WHEN 3 # Servicios
                     LET v_sum_servicios     = v_sum_servicios + v_registros[v_indice].v_monto_pesos

               END CASE
               LET v_indice = v_indice + 1      

            END FOREACH
            FREE cur_rec_datos_rpt

      END CASE
      
      
      IF(v_registros[v_registros.getLength()].v_tpo_mandato IS NULL)THEN
         CALL v_registros.deleteElement(v_registros.getLength())
      END IF
      IF( v_registros.getLength() > 0 )THEN
         START REPORT fn_rpt_pago_mandatos TO XML HANDLER v_manejador_rpt
            FOR v_indice = 1 TO v_registros.getLength() 
               OUTPUT TO REPORT fn_rpt_pago_mandatos(v_texto_rpt,
                                                     v_registros[v_indice].*,
                                                     p_folio,
                                                     p_f_proceso,
                                                     v_sum_prediales,
                                                     v_sum_mantenimiento,
                                                     v_sum_servicios)
            END FOR
         FINISH REPORT fn_rpt_pago_mandatos
      ELSE
         DISPLAY "\n"
         DISPLAY "NO SE ENCONTRARON REGISTROS PARA EL FOLIO ",p_folio
         DISPLAY "\n"
         RETURN v_registros.getLength()
      END IF
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF
   RETURN v_registros.getLength()
END FUNCTION

{===============================================================================
Nombre: fn_rpt_pago_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para 
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
REPORT fn_rpt_pago_mandatos(p_texto_rpt,p_registros,p_folio,p_fecha,p_sum_prediales,p_sum_mantenimiento,p_sum_servicios)
DEFINE p_texto_rpt  STRING,
       p_registros RECORD
         v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato,
         v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato,
         v_ent_federativa      LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_ent_federativa_desc LIKE cat_entidad_federativa.entidad_desc_larga,
         v_municipio           LIKE mdt_det_aplica_pago_mandato.municipio,
         v_municipio_desc      LIKE cat_municipio.municipio_desc,
         v_mandato             LIKE mdt_det_aplica_monto.id_cat_mandato,
         v_mandato_desc        LIKE mdt_cat_mandato.desc_mandato,
         v_monto_pesos         LIKE mdt_det_aplica_pago_mandato.monto_pesos
       END RECORD,
       p_folio                LIKE glo_folio.folio,
       p_fecha                DATE,
       p_sum_prediales     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       p_sum_mantenimiento LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       p_sum_servicios     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_tpo_mandato   LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_entidad       LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_sum_municipio     LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_suma_total        LIKE mdt_det_aplica_pago_mandato.monto_pesos,
       v_pagina            SMALLINT
       
   FORMAT

      FIRST PAGE HEADER
         LET v_suma_total = p_sum_prediales + p_sum_mantenimiento + p_sum_servicios
         PRINTX p_texto_rpt,
                p_folio,
                p_fecha USING "dd-mm-yyyy",
                p_sum_prediales,
                p_sum_mantenimiento,
                p_sum_servicios, 
                v_suma_total

      BEFORE GROUP OF p_registros.v_tpo_mandato
         PRINTX p_registros.v_tpo_mandato_desc

      AFTER GROUP OF p_registros.v_tpo_mandato
         LET v_sum_tpo_mandato = GROUP SUM(p_registros.v_monto_pesos)         
         PRINTX v_sum_tpo_mandato

      BEFORE GROUP OF p_registros.v_ent_federativa
         PRINTX p_registros.v_ent_federativa,
                p_registros.v_ent_federativa_desc

      AFTER GROUP OF p_registros.v_ent_federativa
         LET v_sum_entidad = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_entidad

      BEFORE GROUP OF p_registros.v_municipio
         PRINTX p_registros.v_municipio,
                p_registros.v_municipio_desc

      AFTER GROUP OF p_registros.v_municipio
         LET v_sum_municipio = GROUP SUM(p_registros.v_monto_pesos)
         PRINTX v_sum_municipio
                
      ON EVERY ROW
         PRINTX p_registros.v_mandato_desc,
                p_registros.v_monto_pesos

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina         

END REPORT
                                                                                                                                                                                                                                            HPSL01.4gl                                                                                          0000777 0000212 0001751 00000021257 13113322754 012314  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL30                                                        #
#Objetivo     => Reporte abonos mandatos                                       #
#Autor        => Hugo Ram�rez                                                  #
#Fecha inicio => 24 Junio 2013                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"
DEFINE p_usuario         LIKE seg_usuario.usuario, # usuario firmado al sistema
       p_tipo_carga      SMALLINT,                 # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo_vtna     STRING,                   # Titulo ventana
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       --p_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_opera_cod_rpt_abonos LIKE cat_operacion.opera_cod,
       r_pid             LIKE bat_ctr_proceso.pid


MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo_vtna = ARG_VAL(3)
   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps" 

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"

   CALL fn_genera_rpt_abonos_mandatos()

END MAIN

{===============================================================================
Nombre: fn_genera_rpt_abonos_mandatos
Fecha creacion: 24 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para generar reporte de abonos mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_genera_rpt_abonos_mandatos()
DEFINE r_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_opera_desc      LIKE cat_operacion.opera_desc,
       r_folio           LIKE glo_folio.folio,
       r_resultado_opera SMALLINT,
       r_confirma        BOOLEAN,
       v_comando         STRING
       
   LET v_opera_cod_rpt_abonos = g_opera_cod_carga
   OPEN WINDOW vtna_rpt_abonos_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL011"

      MENU ""
         BEFORE MENU
            CALL fn_recupera_datos_folio(g_proceso_cod_originacion_deudor,
                                         v_opera_cod_rpt_abonos) RETURNING r_proceso_desc,r_opera_desc,r_folio
            IF(r_folio IS NULL OR r_folio = 0)THEN
               CALL fn_mensaje("Aviso","No hay folios para procesar","information")
               EXIT MENU
            ELSE
               DISPLAY r_proceso_desc TO proceso_desc
               DISPLAY r_opera_desc   TO opera_desc
               DISPLAY r_folio        TO folio
               CONTINUE MENU
            END IF

         ON ACTION aceptar
            #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(0,
                                     g_proceso_cod_abonos_mandatos,
                                     v_opera_cod_rpt_abonos) RETURNING r_resultado_opera
            IF(r_resultado_opera = 0)THEN
               CALL fn_ventana_confirma(p_titulo_vtna,"�Generar provision Traspaso Fondo Servicios?","question") RETURNING r_confirma
               IF NOT( r_confirma )THEN
                  CONTINUE MENU
               END IF            
               # se genera el pid para el proceso
               CALL fn_genera_pid(g_proceso_cod_abonos_mandatos,v_opera_cod_rpt_abonos,p_usuario)RETURNING r_pid
               CALL fn_inicializa_proceso(r_pid,
                                          g_proceso_cod_abonos_mandatos,
                                          v_opera_cod_rpt_abonos,
                                          r_folio,
                                          "HPSL01",
                                          "NA",
                                          p_usuario) RETURNING r_resultado_opera
               IF ( r_resultado_opera <> 0 ) THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE MENU
               END IF
               CALL fn_actualiza_opera_ini(r_pid,
                                           g_proceso_cod_abonos_mandatos,
                                           v_opera_cod_rpt_abonos,
                                           r_folio,
                                           "HPSL01",
                                           "NA",
                                           p_usuario) RETURNING r_resultado_opera 
            
               # En el caso de que exista una inconsistencia al iniciar el proceso, se
               # Muestra un mensaje con la descripcion
               IF(r_resultado_opera <> 0)THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE MENU
               END IF
               
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP01.42r '",p_usuario CLIPPED, "' ",
                                                                                         r_pid CLIPPED, " ",
                                                                                         g_proceso_cod_abonos_mandatos CLIPPED," ",
                                                                                         v_opera_cod_rpt_abonos CLIPPED," ",
                                                                                         "0 ",
                                                                                         "NA",
                               " 1>", v_ruta_listados CLIPPED,"/nohup:",r_pid USING "&&&&&",":",
                                                                        g_proceso_cod_abonos_mandatos USING "&&&&&",":",
                                                                        v_opera_cod_rpt_abonos USING "&&&&&"," 2>&1 &"
               
               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_titulo_vtna,"Ocurrio un error al ejecutar la operaci�n","about")
               ELSE
                  CALL fn_mensaje(p_titulo_vtna,"Se ha enviado la operaci�n.\nPodr� revisar el detalle en el monitoreo de procesos","about")
                  EXIT MENU
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE MENU
            END IF
         
         ON ACTION cancelar
            EXIT MENU

      END MENU


   CLOSE WINDOW vtna_rpt_abonos_mandatos

END FUNCTION

{===============================================================================
Nombre: fn_genera_rpt_abonos_mandatos
Fecha creacion: 24 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para generar reporte de abonos mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recupera_datos_folio(p_proceso_cod, p_opera_cod)
DEFINE p_proceso_cod  LIKE cat_proceso.proceso_cod,
       p_opera_cod    LIKE cat_operacion.opera_cod,
       v_proceso_desc LIKE cat_proceso.proceso_desc,
       v_opera_desc   LIKE cat_operacion.opera_desc,
       v_folio        LIKE glo_folio.folio,
       v_consulta    STRING


   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   INITIALIZE v_folio TO NULL
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
--                    "\n    AND opera_cod = ?",
                    "\n    AND opera_cod = 2",
                    "\n    AND status =  0" # registrado
   PREPARE prp_recupera_folio FROM v_consulta
   EXECUTE prp_recupera_folio USING p_proceso_cod
                                    --p_opera_cod
                               INTO v_folio
   
   RETURN v_proceso_desc,
          v_opera_desc,
          v_folio
END FUNCTION                                                                                                                                                                                                                                                                                                                                                 HPSL02.4gl                                                                                          0000777 0000212 0001751 00000007604 13113322753 012314  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19 Junio 2015
--===============================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL02                                                        #
#Objetivo     => Carga de archivos para actualizaci�n de cat�logo de mandatos  #                                     #
#Fecha inicio => 19 Junio 2015                                                  #
################################################################################

DATABASE safre_viv

DEFINE g_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- C�digo del proceso
       g_opera_cod_carga LIKE cat_operacion.opera_cod, -- C�digo de operacion
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING

MAIN
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   CALL STARTLOG(v_ruta_lst CLIPPED ||"/"||p_usuario_cod CLIPPED||".HPSL02.log")
   CALL fn_carga_archivo_mandatos()
   
END MAIN

FUNCTION fn_carga_archivo_mandatos()

DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = 3102 # Actualizacion cat�logo mandatos
   LET g_opera_cod_carga = 1    #
   LET v_tpo_ejecucion   = 2    

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'
   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL021"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga) RETURNING r_resultado_opera
      
      IF(r_resultado_opera = 0)THEN
         LET g_pid = 0
         
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/HPSX02.42r ",
                                                             p_usuario_cod," ",
                                                             g_pid," ",
                                                             g_proceso_cod," ",
                                                             g_opera_cod_carga," ",
                                                             0," ", # folio
                                                             "NA" # archivo
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "HPSL02",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga  # true - la carga inicializa el proceso
         # Si se realiz� la carga se continua con el proceso
         {IF NOT(r_bnd_carga)THEN
            CALL fn_mensaje("OP 29","Se ha cancelado la carga de informaci�n","bn_about")
         END IF}
      ELSE
         # Muestra el mensaje de cual es la causa de que no se puede iniciar con la operacion
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Salir"
              EXIT MENU
         END MENU
      END IF
   CLOSE WINDOW vtna_valicacion



END FUNCTION
                                                                                                                                   HPSL03.4gl                                                                                          0000777 0000212 0001751 00000036465 13113322754 012325  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 8 de julio de 2015
--===============================================================
DATABASE safre_viv

DEFINE p_usuario_cod   LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion INTEGER,
       p_cad_ventana   STRING,
       g_proceso_cod   LIKE cat_proceso.proceso_cod,
       v_opera_cod_int LIKE cat_operacion.opera_cod,
       g_opera_cod_val LIKE cat_operacion.opera_cod,
       v_ruta_ejecutable        LIKE seg_modulo.ruta_bin,
       g_pid           LIKE glo_pid.pid,
       v_archivos_pendientes DYNAMIC ARRAY OF STRING,
       v_archivos_elegidos   DYNAMIC ARRAY OF STRING,
       v_dragdrop            UI.DRAGDROP, # manejador de arrastrar y soltar (drag and drop)
       v_total_tpo_mandatos  INTEGER,
       v_tipos_mandato DYNAMIC ARRAY OF RECORD
         v_tipo_mandato        CHAR(25),
         v_total               INTEGER
       END RECORD

MAIN

--Recupera par�metros definidos
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_integra_mandato()   

END MAIN

FUNCTION fn_integra_mandato()

   DEFINE r_valida          SMALLINT,
          v_continua        BOOLEAN,
          v_ventana         ui.Window,
          v_forma           ui.Form,
          v_arrastra_origen STRING,
          v_indice_arrastre INTEGER,
          v_indice_suelta   INTEGER,
          v_valor_arrastre  STRING,
          v_can_int         BOOLEAN,
          v_renglon_actual  INTEGER,
          r_error           BOOLEAN

   CONSTANT v_sr_pendientes = "sr_pendientes" # Se asigna el nombre de la tabla de archivos pendientes
   CONSTANT v_sr_elegido    = "sr_elegido" # Se asigna el nombre de la tabla de archivos elegidos

   LET g_proceso_cod   = 3102 # Proceso act entidades receptoras
   LET v_opera_cod_int = 2    # Integraci�n 
   LET g_opera_cod_val = 1    # Validacion 

   # se recupera la ruta ejecutable del m�dulo
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'

   SELECT MAX(pid)
     INTO g_pid
     FROM bat_ctr_proceso
    WHERE proceso_cod = g_proceso_cod
      AND estado_cod = 2 # procesando

   CALL fn_valida_operacion(g_pid,g_proceso_cod,v_opera_cod_int) RETURNING r_valida
   # Se verifica si la operacion es valida
   IF(r_valida <> 0)THEN
      # En caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_valida)
      EXIT PROGRAM
   END IF
   LET v_continua = TRUE

   OPEN WINDOW vtna_integra_mdt WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL031"
   #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm()
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      WHILE (v_continua)
         DIALOG ATTRIBUTE(UNBUFFERED)

            DISPLAY ARRAY v_archivos_pendientes TO sr_pendientes.*

               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_pendientes
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_pendientes[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                  IF(v_arrastra_origen == v_sr_pendientes)THEN
                     CALL v_dragdrop.dropInternal()
                  ELSE
                     # Se recupera el indice donde se solt� el archivo
                     LET v_indice_suelta = v_dragdrop.getLocationRow()
                     # Se inserta el archivo en el indice que se recuper�
                     CALL DIALOG.insertRow(v_sr_pendientes, v_indice_suelta)
                     # se establese el foco en la tabla destino
                     CALL DIALOG.setCurrentRow(v_sr_pendientes, v_indice_suelta)
                     # se agrega al arreglo el nomre del archivo
                     LET v_archivos_pendientes[v_indice_suelta] = v_valor_arrastre
                     # elimina el registro de la tabla origen
                     CALL DIALOG.deleteRow(v_sr_elegido, v_indice_arrastre)
                  END IF
            
            END DISPLAY
   
            DISPLAY ARRAY v_archivos_elegidos TO sr_elegido.*
               ON DRAG_START(v_dragdrop)
                  # se recupera tabla origen de arrastre
                  LET v_arrastra_origen = v_sr_elegido
                  # se recupera el indice de la tabla origen de arrastre
                  LET v_indice_arrastre = ARR_CURR()
                  # se recupera el valor de la tabla origen de arrastre
                  LET v_valor_arrastre = v_archivos_elegidos[v_indice_arrastre]
   
               ON DRAG_FINISHED(v_dragdrop)
                  # se indica que no hay tabla origen
                  INITIALIZE v_arrastra_origen TO NULL
   
               ON DRAG_ENTER(v_dragdrop)
                  IF v_arrastra_origen IS NULL THEN
                     CALL v_dragdrop.setOperation(NULL)
                  END IF
   
               ON DROP(v_dragdrop)
                   IF v_arrastra_origen == v_sr_elegido THEN                       
                       CALL v_dragdrop.dropInternal()
                   ELSE
                       # Se recupera el indice donde se solt� el archivo
                       LET v_indice_suelta = v_dragdrop.getLocationRow()
                       # Se inserta el archivo en el indice que se recuper�
                       CALL DIALOG.insertRow(v_sr_elegido, v_indice_suelta)
                       # se establese el foco en la tabla destino
                       CALL DIALOG.setCurrentRow(v_sr_elegido, v_indice_suelta)
                       # se agrega al arreglo el nomre del archivo
                       LET v_archivos_elegidos[v_indice_suelta] = v_valor_arrastre
                       # elimina el registro de la tabla origen
                       CALL DIALOG.deleteRow(v_sr_pendientes, v_indice_arrastre)
                   END IF

            END DISPLAY

            # se despliegan los totales por tipo mandato
            DISPLAY ARRAY v_tipos_mandato TO sr_tipos_mandato.*
            END DISPLAY

            BEFORE DIALOG
               LET v_can_int = FALSE
               # Se recuperan los archivos pendientes
               CALL fn_recupera_archivos_pendientes()
               # se ocultan las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_tipos_mandato",1)
               CALL DIALOG.setActionHidden("integrar",1)
                  
            # botones para elegir archivo
            ON ACTION aceptar
               # se valida que se haya seleccionado un archivo
               IF(v_archivos_elegidos.getLength() = 0)THEN
                  CALL fn_mensaje("Aviso","Debe arrastrar al menos un archivo a integrar","info")
                  CONTINUE DIALOG
               END IF
               # se valida que solo se integre un archivo
               IF(v_archivos_elegidos.getLength() > 1)THEN
                  CALL fn_mensaje("Aviso","Solo puede seleccionar un archivo","info")
                  CONTINUE DIALOG
               END IF
               # se recupera la informacion de predial y cuota de conservacion
               CALL fn_recupera_totales()
               LET v_can_int = TRUE
               # Se muestra las tablas del detalle del archivo
               CALL v_forma.setElementHidden("gpo_tipos_mandato",0)
               CALL v_forma.setElementHidden("integrar",0)
               CALL DIALOG.setActionHidden("integrar",0)
               CALL DIALOG.setActionHidden("aceptar",1)
               # se imprimen los totales por tabla
               DISPLAY v_total_tpo_mandatos TO totales.flbl_tot_tipos_mandato
               --ACCEPT DIALOG

            # botones para integrar archivo
            ON ACTION integrar
               LET v_renglon_actual = DIALOG.getcurrentRow("sr_elegido")
               # Ejecuta SP de integraci�n diagn�stico Op 27
               CALL fn_ejecuta_integracion_mandato(v_archivos_elegidos[v_renglon_actual])
                       RETURNING r_error
               IF(r_error)THEN
                  EXIT DIALOG
               ELSE
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF
            
            ON ACTION cancelar
               IF(v_can_int)THEN
                  # regresa a la seleccion del archivo
                  CALL v_forma.setElementHidden("gpo_tipos_mandato",1)
                  CALL DIALOG.setActionHidden("integrar",1)
                  CALL DIALOG.setActionHidden("aceptar",0)
                  LET v_archivos_pendientes[1] = v_archivos_elegidos[1]
                  CALL v_archivos_elegidos.clear()
                  LET v_can_int = FALSE
                  EXIT DIALOG
               ELSE
                  # sale de la seleccion del archivo
                  LET v_continua = FALSE
                  EXIT DIALOG
               END IF

         END DIALOG
      END WHILE
      
   CLOSE WINDOW vtna_integra_mdt
      
END FUNCTION

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL03                                                   #
#Descripcion       => Recupera los archivos pendientes de integrar             #                                    #
#Fecha inicio      => 8 de Julio de 2015                                       #
################################################################################
FUNCTION fn_recupera_archivos_pendientes()
DEFINE v_consulta STRING,
       v_archivo  LIKE glo_ctr_archivo.nombre_archivo,
       v_indice   SMALLINT

   WHENEVER ERROR CONTINUE

   LET v_indice = 1
   # Recupera archivos pendientes de integrar
   LET v_consulta = "\n SELECT nombre_archivo",
                    "\n   FROM glo_ctr_archivo",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND estado = 1" # Cargado

   PREPARE prp_recupera_pendientes FROM v_consulta
   DECLARE cur_recupera_pendientes CURSOR FOR prp_recupera_pendientes
   FOREACH cur_recupera_pendientes USING g_proceso_cod,g_opera_cod_val
                                   INTO v_archivo
      LET v_archivos_pendientes[v_indice] = v_archivo
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_pendientes
END FUNCTION


################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL03                                                   #
#Descripcion       => Ejecuta el SP que realiza la integraci�n de mandatos     #
#Fecha inicio      => 08 de Julio de 2015                                      #
################################################################################
FUNCTION fn_ejecuta_integracion_mandato(v_archivo)
DEFINE v_consulta STRING,
       v_archivo  STRING,
       v_ruta_listados LIKE seg_modulo.ruta_listados,
       r_resultado_opera SMALLINT, # bandera de resultado de cambio de estatus
       v_error           BOOLEAN,
       r_folio           LIKE glo_folio.folio
       

   WHENEVER ERROR CONTINUE
   
   SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
   WHERE modulo_cod = "bat"

   # inicializa suponiendo que ejecuta correctamente
   LET v_error = FALSE
      
   CALL fn_valida_operacion(g_pid,g_proceso_cod,v_opera_cod_int) RETURNING r_resultado_opera
     
   IF r_resultado_opera <> 0 THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      # Indica que ocurri� error
      RETURN v_error
   END IF

   # genera folio 
   {CALL fn_genera_folio(g_pid,g_proceso_cod,v_opera_cod_int)
                        RETURNING r_folio}
   # Folio se genera en lanzado
   LET r_folio = 0

   CALL fn_actualiza_opera_ini(g_pid,g_proceso_cod,v_opera_cod_int,r_folio,"HPSL03",
                               v_archivo,p_usuario_cod) RETURNING r_resultado_opera
   # si ocurri� un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      LET v_error = TRUE
      CALL fn_muestra_inc_operacion(r_resultado_opera)
      RETURN v_error
   END IF
   
   LET v_consulta = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP03.42r '",
                                    p_usuario_cod CLIPPED, "' ",g_pid CLIPPED, " ",
                                    g_proceso_cod CLIPPED," ",v_opera_cod_int CLIPPED," ",
                                    r_folio CLIPPED, " '",v_archivo CLIPPED,
                     "' 1>", v_ruta_listados CLIPPED,
                           "/nohup:",g_pid USING "&&&&&",":",
                                     g_proceso_cod USING "&&&&&",":",
                                     v_opera_cod_int USING "&&&&&"," 2>&1 &"
   DISPLAY v_consulta 
   RUN v_consulta
   IF(STATUS)THEN
      LET v_error = TRUE
      CALL fn_mensaje(p_cad_ventana,"Ocurrio un error al ejecutar la integraci�n","about")
   ELSE
      CALL fn_mensaje(p_cad_ventana,"Se ha enviado la operacion.\nPodr� revisar el detalle en el monitoreo de procesos","about")
      LET v_error = FALSE
   END IF
   RETURN v_error
END FUNCTION


################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL03                                                   #
#Descripcion  => Carga detalles del archivo a integrar de las tablas temporales#
#Fecha inicio      => 09 de Julio de 2015                                      #
################################################################################
FUNCTION fn_recupera_totales()

   DEFINE v_consulta STRING 
   DEFINE i          INTEGER

   LET i = 1
   LET v_total_tpo_mandatos = 0
   LET v_consulta = "SELECT CASE WHEN tipo_mandato == 1 THEN 'PREDIAL' WHEN tipo_mandato == 2 THEN 'CUOTA DE CONSERVACION' END AS tpo_mdt,COUNT(*) FROM safre_tmp:hps_tmp_det_acmdt GROUP BY tipo_mandato ORDER BY tpo_mdt"
   {SELECT
    CASE
    WHEN tipo_mandato == 1 THEN "PREDIAL"
    WHEN tipo_mandato == 2 THEN "CUOTA DE CONSERVACION"
    END AS tpo_mdt,COUNT(*)
    FROM safre_tmp:hps_tmp_det_acmdt
    GROUP BY tipo_mandato
    ORDER BY tpo_mdt}
   PREPARE prp_det FROM v_consulta
   DECLARE cur_det CURSOR FOR prp_det
   FOREACH cur_det INTO v_tipos_mandato[i].* 
      LET v_total_tpo_mandatos = v_total_tpo_mandatos + v_tipos_mandato[i].v_total
      LET i = i+1
   END FOREACH

   CALL v_tipos_mandato.deleteElement(i)
   
END FUNCTION                                                                                                                                                                                                           HPSL04.4gl                                                                                          0000777 0000212 0001751 00000003651 13113322754 012315  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => HPS                                                     #
#Programa          => HPSL04                                                  #
#Objetivo          => LIQUIDACION TRASPASO FONDO SCUBCUENTA SERVICIOS         #
#Autor             => JESUS YA�EZ MORENO, EFP                                 #
#Fecha Inicio      => 16/Marzo/2015                                           #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".HPSL04.log")

   LET g_proceso_cod = 3101 -- liquidacion de operacion 28
   LET g_opera_cod   = 2 -- liquida saldo operacion 28

   -- se invoca la funcion general de liquidacion operacion 28
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod,2) 
   
END MAIN                                                                                       HPSL05.4gl                                                                                          0000777 0000212 0001751 00000021777 13113322754 012327  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => HPSL05                                                        #
#Objetivo     => Preliquidacion de servicios                                   #
#Autor        => Hugo Ram�rez                                                  #
#Fecha inicio => 16 Marzo 2015                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"
DEFINE p_usuario         LIKE seg_usuario.usuario, # usuario firmado al sistema
       p_tipo_carga      SMALLINT,                 # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo_vtna     STRING,                   # Titulo ventana
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_listados   LIKE seg_modulo.ruta_listados,
       v_ventana         ui.Window,
       --p_proceso_cod     LIKE cat_proceso.proceso_cod,
       p_opera_cod_preliquidacion LIKE cat_operacion.opera_cod,
       r_pid             LIKE bat_ctr_proceso.pid
       
MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo_vtna = ARG_VAL(3)
   

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps" 

   SELECT ruta_listados
    INTO v_ruta_listados
    FROM seg_modulo
   WHERE modulo_cod = "bat"

   CALL fn_preliquida_mandatos()

END MAIN

{===============================================================================
Nombre: fn_preliquida_mandatos
Fecha creacion: 05 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para preliquidar madnatos seg�n cat�logo de ejecuci�n
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_preliquida_mandatos()
DEFINE v_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion     VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa VARCHAR(40)--STRING
       END RECORD,
       r_continuar BOOLEAN,
       v_indice    SMALLINT,
       r_resultado_opera SMALLINT,
       v_comando         STRING,
       r_confirma         BOOLEAN

   OPEN WINDOW vtna_preliquida_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL051"
      IF(p_titulo_vtna IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo_vtna)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo_vtna)
      END IF

      DISPLAY ARRAY v_mandatos TO sr_mandatos.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         BEFORE DISPLAY
            LET p_opera_cod_preliquidacion = g_opera_cod_carga
            CALL fn_busca_mandatos_preliq() RETURNING r_continuar, v_mandatos
            IF NOT( r_continuar )THEN
               EXIT DISPLAY
            END IF
            
         ON ACTION preliquidar
            
            #Se verifica si se puede iniciar la operacion      
            CALL fn_valida_operacion(0,g_proceso_cod_pago_mandatos,p_opera_cod_preliquidacion) RETURNING r_resultado_opera
            IF(r_resultado_opera = 0)THEN
               CALL fn_ventana_confirma(p_titulo_vtna,"�Preliquidar mandatos?","question") RETURNING r_confirma
               IF NOT( r_confirma )THEN
                  CONTINUE DISPLAY
               END IF            
               # se genera el pid para el proceso
               CALL fn_genera_pid(g_proceso_cod_pago_mandatos,p_opera_cod_preliquidacion,p_usuario)RETURNING r_pid
               CALL fn_inicializa_proceso(r_pid,
                                          g_proceso_cod_pago_mandatos,
                                          p_opera_cod_preliquidacion,
                                          0,
                                          "HPSL05", "NA", p_usuario) RETURNING r_resultado_opera IF ( r_resultado_opera <> 0 ) THEN CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE DISPLAY
               END IF
               CALL fn_actualiza_opera_ini(r_pid,
                                           g_proceso_cod_pago_mandatos,
                                           p_opera_cod_preliquidacion,
                                           0,
                                           "HPSL05",
                                           "NA",
                                           p_usuario) RETURNING r_resultado_opera 
            
               # En el caso de que exista una inconsistencia al iniciar el proceso, se
               # Muestra un mensaje con la descripcion
               IF(r_resultado_opera <> 0)THEN
                  CALL fn_muestra_inc_operacion(r_resultado_opera)
                  CONTINUE DISPLAY
               END IF
               
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP05.42r '",
                                    p_usuario CLIPPED, "' ",
                                    r_pid CLIPPED, " ",
                                    g_proceso_cod_pago_mandatos CLIPPED," ",
                                    p_opera_cod_preliquidacion CLIPPED," ",
                                    "0 ",
                                    "NA",
                     " 1>", v_ruta_listados CLIPPED,"/nohup:",r_pid USING "&&&&&",":",
                                                              g_proceso_cod_pago_mandatos USING "&&&&&",":",
                                                              p_opera_cod_preliquidacion USING "&&&&&"," 2>&1 &"
               DISPLAY v_comando 
               RUN v_comando
               IF(STATUS)THEN
                  CALL fn_mensaje(p_titulo_vtna,"Ocurrio un error al ejecutar la preliquidaci�n","about")
               ELSE
                  CALL fn_mensaje(p_titulo_vtna,"Se ha enviado la operaci�n.\nPodr� revisar el detalle en el monitoreo de procesos","about")
                  EXIT DISPLAY
               END IF
            ELSE
               CALL fn_muestra_inc_operacion(r_resultado_opera)
               CONTINUE DISPLAY
            END IF

         ON ACTION cancelar
            EXIT DISPLAY

      END DISPLAY
   CLOSE WINDOW vtna_preliquida_mandatos

END FUNCTION

{===============================================================================
Nombre: fn_busca_mandatos_preliq
Fecha creacion: 05 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para 
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_busca_mandatos_preliq()
DEFINE v_continuar BOOLEAN,
       r_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--STRING,
          v_padre           STRING,
          v_identificador   SMALLINT,--STRING,
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--STRING,
          v_recurrencia     SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.recurrencia,
          v_complemento_recurrencia SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.complemento_recurrencia,
          v_ejecuta_cod      SMALLINT,--LIKE mdt_cat_mandato_ejecuta_pago.ejecuta_cod,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--STRING
       END RECORD


   MENU ""
      BEFORE MENU
         CALL r_mandatos.clear()
         LET v_continuar = FALSE 

      ON ACTION buscar
         --CALL fn_recupera_mandatos() RETURNING v_continuar, r_mandatos
         CALL fn_valida_ejecucion_mandato(g_estado_abonado_pago_mdt) RETURNING v_continuar, r_mandatos
         IF (v_continuar)THEN
            LET v_continuar = TRUE
            EXIT MENU
         ELSE
            CALL fn_mensaje("Aviso","No se encontraror registros con criterio dado","information")
            CONTINUE MENU
         END IF

      ON ACTION cancelar
         LET v_continuar = FALSE
         EXIT MENU

   END MENU

   RETURN v_continuar,r_mandatos
END FUNCTION
 HPSL06.4gl                                                                                          0000777 0000212 0001751 00000003112 13113322754 012307  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL06                                                        #
#Objetivo     => Liquidacion de pagos servicios                                #
#Autor        => Hugo Ram�rez                                                  #
#Fecha inicio => 16 Marzo 2015                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # forma como ejecutara el programa
       p_titulo_vtna    STRING,                       # titulo de la ventana
       v_operacion_liquidacion SMALLINT

   # Recupera par�metros
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   LET v_operacion_liquidacion   = g_opera_cod_integracion # liquidacion pago mandatos

   # se invoca la funcion general de liquidacion
   CALL fn_liquida(p_usuario_cod, 
                   g_proceso_cod_pago_mandatos, 
                   v_operacion_liquidacion,
                   2) # Liquidar 
   
END MAIN

                                                                                                                                                                                                                                                                                                                                                                                                                                                      HPSL20.4gl                                                                                          0000777 0000212 0001751 00000011137 13113322761 012307  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
--===============================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HSPL20                                                        #
#Objetivo     => Validaci�n de archivo con origen recurrente acreditados       #
#Autor        => Hugo C�sar Ram�rez Garc�a                                     #
#Fecha inicio => 09 Junio 2012                                                 #
#Modifico     => Alexandro Hollmann, EFP - Nota: Los ftes se ubicaban en bin   #
#Fecha modif  => 21 Junio 2012                                                 #
################################################################################
DATABASE safre_viv

DEFINE g_pid             LIKE bat_ctr_proceso.pid,     -- ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- C�digo del proceso
       g_opera_cod_carga LIKE cat_operacion.opera_cod, -- C�digo de operacion
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   INTEGER,
       v_ventana         ui.Window,
       p_cad_ventana     STRING

MAIN
   
   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   CALL STARTLOG(v_ruta_lst CLIPPED ||"/"||p_usuario_cod CLIPPED||".HPSL20.log")
   CALL fn_valida_recurrente()
END MAIN

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL20                                                   #
#Descripcion       => Validaci�n de archivo instrucciones pago servicios  HPS  #
#Autor             => Jesus David Ya�ez Moreno                                 #
#Fecha inicio      => 16 marzo 2015                                            #
################################################################################
FUNCTION fn_valida_recurrente()
DEFINE v_comando         STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_bnd_carga       BOOLEAN,
       r_resultado_opera SMALLINT,
       v_tpo_ejecucion   INTEGER
   
   #Se dan las variables de proceso y operacion
   LET g_proceso_cod     = 3104 # valida instrucciones pago servicios HPS
   LET g_opera_cod_carga = 1    # carga archivo
   LET v_tpo_ejecucion   = 2    

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'
   
   OPEN WINDOW vtna_valicacion WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL201"
      #Se asigna el titulo de la ventana
      IF ( p_cad_ventana IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_cad_ventana)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      LET g_pid = 0

      #Se verifica si se puede iniciar la operacion      
      CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod_carga) RETURNING r_resultado_opera
      LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/HPSX20.42r ",
                                                            p_usuario_cod," ",
                                                            g_pid," ",
                                                            g_proceso_cod," ",
                                                            g_opera_cod_carga," ",
                                                            0," ",  # folio
                                                            "NA" # archivo
                                                                 
                                                                 
      IF(r_resultado_opera = 0)THEN
         CALL fn_carga_archivo(g_pid, g_proceso_cod, g_opera_cod_carga, v_tpo_ejecucion,
                               "HPSL20",v_comando, p_usuario_cod, TRUE) RETURNING r_bnd_carga  # true - la carga inicializa el proceso
         
      ELSE
         # Muestra el mensaje del cual es la causa de que no se puede iniciar con la operacion
         --CALL fn_desplega_inc_operacion(r_resultado_opera)
         CALL fn_muestra_inc_operacion(r_resultado_opera)
         MENU
           COMMAND "Cerrar"
              EXIT MENU
         END MENU
      END IF
   CLOSE WINDOW vtna_valicacion
END FUNCTION
                                                                                                                                                                                                                                                                                                                                                                                                                                 HPSL21.4gl                                                                                          0000777 0000212 0001751 00000024016 13113322762 012311  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07-jul-2015
--===============================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Modulo            => HPS                                                      #
#Programa          => HPSL21                                                   #
#Objetivo          => Integracion instrucciones de pagos de servicio HPS       #
#Autor             => Jesus David Ya�ez Moreno                                 #
#Fecha Creacion    => Marzo 16 2015                                            #
################################################################################

DATABASE safre_viv
DEFINE p_usuario_cod  LIKE seg_usuario.usuario_cod,
       g_ruta_bin     LIKE seg_modulo.ruta_bin,       
       p_titulo       STRING,
       v_ventana      ui.Window,
       v_pid          LIKE glo_pid.pid,
       v_proceso             LIKE cat_proceso.proceso_cod,
       v_operacion           LIKE cat_operacion.opera_cod,
       r_bandera             SMALLINT

# Objetivo:
MAIN
DEFINE p_tipo_ejecucion    SMALLINT,
       v_total_solicitudes DYNAMIC ARRAY OF RECORD
         v_tot_sol         INTEGER
       END RECORD,
       v_aux_solicitudes   INTEGER 
   
   # Se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)


   LET v_proceso = 3104 # validacion mandatos origen recurrente
   LET v_operacion = 2
   # Recupera la ruta de los archivos ejecutables para mandatos
   SELECT ruta_bin
     INTO g_ruta_bin
     FROM seg_modulo
    WHERE modulo_cod = "hps"
    
   LET v_total_solicitudes[1].v_tot_sol = 0

   SELECT COUNT(*)
     INTO v_aux_solicitudes 
     FROM safre_tmp:tmp_hps_det_instrucciones
    --WHERE estado = 101
    --  AND id_origen = 2

   # Recupera el pid
   CALL fn_max_pid(v_proceso,v_operacion) RETURNING v_pid

   # Valida que se pueda ejecutar la operaci�n 
   CALL fn_valida_operacion(v_pid,v_proceso,v_operacion) RETURNING r_bandera
           
   IF(r_bandera <> 0)THEN
      # Imprime el mensaje de inconsistencia en consola y archivo
      CALL fn_muestra_inc_operacion(r_bandera)
      EXIT PROGRAM
   END IF

   LET v_total_solicitudes[1].v_tot_sol = v_aux_solicitudes
         
   OPEN WINDOW w_solicitud_mandatos WITH FORM g_ruta_bin CLIPPED||"/HPSL211"
      #Se asigna el titulo de la ventana
      IF(p_titulo IS NOT NULL)THEN
         CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DISPLAY ARRAY v_total_solicitudes TO tbl_solicitudes.* 
                     ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)

         ON ACTION Aceptar
            IF(v_total_solicitudes[1].v_tot_sol <= 0 )THEN
               CALL fn_mensaje(p_titulo, 
                               "No Hay Registros para integrar",
                               "about")   
            ELSE
               CALL fn_muestra_detalle_solicitudes()
            END IF            
            EXIT DISPLAY

         ON ACTION CLOSE -- Cancelar
           EXIT DISPLAY

      END DISPLAY
   CLOSE WINDOW w_solicitud_mandatos
END MAIN

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSL06                                                        #
#Objetivo     => Muestra detealle de mandatos                                  #
#Autor        => Hugo C�sar Ram�rez Grac�a                                     #
#Fecha Inicio => 11 Junio 2012                                                 #
################################################################################
FUNCTION fn_muestra_detalle_solicitudes()
DEFINE v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       v_detalle_sol_aux     RECORD
          v_mandato          LIKE mdt_cat_mandato.desc_mandato,
          v_fecha_ingreso    LIKE safre_tmp:tmp_acr_transf_30.fec_ini_mandato,
          v_total            INTEGER
       END RECORD,
       v_detalle_solicitudes DYNAMIC ARRAY OF RECORD
          v_mandato          LIKE safre_tmp:tmp_acr_transf_30.cve_mandato,
          v_fecha_ingreso    STRING,-- DATE,
          v_total            INTEGER
       END RECORD,
       v_consulta            STRING,
       v_indice              INTEGER,
       v_comando             STRING,
       
       
       v_ruta_listados       LIKE seg_modulo.ruta_listados,
       
       v_cadena              STRING
DEFINE v_folio               LIKE glo_ctr_archivo.folio
DEFINE v_archivo             LIKE glo_ctr_archivo.nombre_archivo

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = "bat"
   
   
     
   OPEN WINDOW w_detalle_solicituides WITH FORM v_ruta_ejecutable CLIPPED||"/HPSL212"
      #Se asigna el titulo de la ventana
      IF(p_titulo IS NOT NULL)THEN
          CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DISPLAY ARRAY v_detalle_solicitudes TO tbl_solicitudes.* 
                                          ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
        BEFORE DISPLAY
           LET v_consulta = "\n SELECT rec.cve_mandato, rec.fec_ini_mandato, COUNT(rec.nss)",
                            "\n FROM safre_tmp:tmp_hps_det_instrucciones rec ",
                            "\n GROUP BY 1,2 "
           #SDL -- Muevo el segmento no utiliado de la consulta
                            --"\n        mdt_cat_mandato mcat ",
                            --"\n     ON mcat.cve_mandato = rec.cve_mandato[12,18]", -- TMP Por definir longitud final cve_mandato
                            --"\n  WHERE sol.estado = 101",
                            --"\n    AND sol.id_origen = 2",           
           PREPARE prp_detalle_solicitudes FROM v_consulta
           DECLARE cur_detalle_solicitudes CURSOR FOR prp_detalle_solicitudes
           LET v_indice = 1
           FOREACH  cur_detalle_solicitudes INTO v_detalle_sol_aux.* 
              LET v_detalle_solicitudes[v_indice].v_mandato       = v_detalle_sol_aux.v_mandato 
              LET v_detalle_solicitudes[v_indice].v_fecha_ingreso = v_detalle_sol_aux.v_fecha_ingreso-- USING "MM-DD-YYYY" 
              LET v_detalle_solicitudes[v_indice].v_total         = v_detalle_sol_aux.v_total
              LET v_indice = v_indice + 1
           END FOREACH
           IF(v_detalle_solicitudes[v_detalle_solicitudes.getLength()].v_total IS NULL)THEN
              CALL v_detalle_solicitudes.deleteElement(v_detalle_solicitudes.getLength()) 
           END IF

        ON ACTION Aceptar
          -- LET v_pid = 0
           
           # Valida que se pueda ejecutar la operaci�n 
           CALL fn_valida_operacion(v_pid,v_proceso,v_operacion) RETURNING r_bandera
           
           IF(r_bandera <> 0)THEN
              # Imprime el mensaje de inconsistencia en consola y archivo
              CALL fn_muestra_inc_operacion(r_bandera)
           ELSE
              
              LET v_folio = 0
              CALL fn_actualiza_opera_ini(v_pid,v_proceso,v_operacion,v_folio,'HPSL21','',p_usuario_cod)
                    RETURNING r_bandera
              IF(r_bandera = 0)THEN
                 CALL fn_recupera_arch_cargado(v_proceso,v_operacion) RETURNING v_archivo
                 #Construye comando
                 LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/HPSP21.42r ",
                                                 p_usuario_cod," ",
                                                 v_pid," ",
                                                 v_proceso," ",
                                                 v_operacion," ",
                                                 v_folio," ",
                                                 v_archivo,
                                 " 1>", v_ruta_listados CLIPPED ,
                                 "/nohup:",v_pid USING "&&&&&",":",
                                           v_proceso USING "&&&&&",":",
                                           v_operacion   USING "&&&&&" ,
                                 " 2>&1 &"
                 RUN v_comando
                 DISPLAY v_comando 
                 IF(STATUS)THEN
                    CALL fn_mensaje(p_titulo, 
                                    "Ocurri� un error al iniciar el proceso batch",
                                    "about")
                    CALL fn_error_opera(v_pid,v_proceso,v_operacion)
                                       RETURNING r_bandera
                    IF(r_bandera)THEN
                       # Imprime el mensaje de inconsistencia en consola y archivo
                       CALL fn_muestra_inc_operacion(r_bandera)
                        EXIT DISPLAY           
                    END IF
                 ELSE
                    LET v_cadena = v_pid 
                    LET v_cadena = v_cadena.trim()
                    LET v_cadena = "Se ha enviado la operacion.\nPodr� revisar el detalle en el monitoreo de procesos"
                    CALL fn_mensaje(p_titulo, 
                                    v_cadena ,
                                    "about")
                 END IF
              ELSE
                 # Imprime el mensaje de inconsistencia en consola y archivo
                 CALL fn_muestra_inc_operacion(r_bandera)
                 EXIT DISPLAY
              END IF
           END IF
           EXIT DISPLAY

        ON ACTION Cancelar
           EXIT DISPLAY

      END DISPLAY
      
   CLOSE WINDOW w_detalle_solicituides 
   
END FUNCTION
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  HPSL22.4gl                                                                                          0000777 0000212 0001751 00000077335 13113322762 012326  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-04-2012
--===============================================================

####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSL22                                        #
#Objetivo          =>Registrar instrucciones de mandatos           #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>07 Febrero 2012                               #
#Fecha modificacion=>26 Marzo 2012                                 #
####################################################################
DATABASE safre_viv
DEFINE x char(1)
DEFINE prueba INTEGER 
DEFINE v_s_qryTxt        STRING
DEFINE v_r_mandato       RECORD LIKE mdt_ctr_mandato.*
DEFINE v_r_mandato_det   RECORD LIKE mdt_det_ctr_mandato.*
DEFINE v_r_solicitud_mandato   RECORD LIKE mdt_solicitud_mandato.*
DEFINE v_r_credito       RECORD LIKE cre_acreditado.*   -- Cambio de acr_transferencia por cre_acreditado
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30),              -- nombre del programa
       v_ventana         ui.Window,
       p_proceso_cod   LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
       p_opera_cod     LIKE cat_operacion.opera_cod, -- operacion de la etapa que llama la funcion
       r_pid   LIKE bat_ctr_operacion.pid

MAIN
   DEFINE v_arr_arch_pend   DYNAMIC ARRAY OF STRING, -- arreglo que contiene los lotes pendientes
          v_arr_archPendAux DECIMAL(9,0), -- arreglo auxiliar que contiene los lotes pendientes
          v_arr_arch_eleg   DYNAMIC ARRAY OF STRING, -- arreglo que contiene los archivos a elegir
          v_arr_elegidos    DYNAMIC ARRAY OF RECORD -- arreglo que contiene los archivos elegidos
             f_proceso        DATE,
             folio            INTEGER,
             tot_reg          INTEGER
          END RECORD,
          v_v_f_Lote        DATE, -- nombre del archivo en proceso
          v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
          v_drag_index      INTEGER, -- indice del drag
          v_drop_index      INTEGER, -- indice del drop
          v_drag_source     STRING, -- fuente del drag
          v_drag_value      STRING, -- valor del drag
          v_i_num_arch      SMALLINT, -- numero de archivos a elegir
          v_i_iter          SMALLINT, -- variable usada para iteracion
          v_i_iter_dep      SMALLINT, -- variable usada para iteracion
          v_i_indice        SMALLINT, -- indice del arrego de archivos pendientes
          
          v_i_opera_cod_ant LIKE cat_operacion.opera_cod, -- operacion de la etapa anterior
          --v_i_operacion     LIKE acr_ctr_archivo.operacion, -- operacion del proceso
          v_d_folio         LIKE glo_ctr_archivo.folio, -- folio
          v_d_pid           LIKE bat_ctr_proceso.pid, -- identificador del proceso
          v_c_ruta_bin_acr  LIKE seg_modulo.ruta_bin, -- ruta del bin de acr
          v_c_ruta_list_bat LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_c_programa_cod  LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_msj           STRING, -- se asigna un mensaje que ser� presentado al usuario
          r_b_valida        SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia      STRING
   DEFINE v_i_tot_reg_sol   INTEGER
          
          CONSTANT l_nom_tbl_pend = "tbl_pendiente" -- se asigna el nombre de la tabla de archivos pendientes
          CONSTANT l_nom_tbl_int = "tbl_elegido" -- se asigna el nombre de la tabla de archivos elegidos

   DEFINE w               ui.window
   DEFINE f               ui.form

   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario    = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog   = ARG_VAL(3)

   LET p_proceso_cod = 3104 -- 46 anterior -- lanzar recurrentes AHM 2012 04 25 Cambio de procesos
   LET p_opera_cod = 3 -- genera archivo mandatos

   -- se inicializa el indice del arreglo
   LET v_i_indice = 1
   # Recupera el pid
   CALL fn_max_pid(p_proceso_cod,2) RETURNING r_pid
   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(r_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
   
   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
      EXIT PROGRAM
   END IF

   -- se asignan los valores necesarios para la intergraci
   -- TMP verificar si va esto LET v_i_proceso_cod = 1  -- LQINFO
   -- TMP verificar si va esto LET v_i_opera_cod = 2 -- Integracion de LQINFO
   -- TMP verificar si va esto LET v_i_opera_cod_ant = 1 -- Carga (Validar) de archivo
   -- TMP verificar si va esto LET v_i_operacion = 01 -- operacion del proceso
   -- TMP verificar si va esto LET v_d_folio = 0      --Se indica folio 0 para que el proceso de historico genere un nuevo folio
   -- TMP verificar si va esto LET v_c_programa_cod = "HPSL22"
   -- TMP verificar si va esto -- se crea la sentencia sql que obtiene el pid perteneciente al folio
   -- TMP verificar si va esto LET v_s_qryTxt = " SELECT MAX(pid)\n",
   -- TMP verificar si va esto                  "   FROM bat_ctr_proceso\n",
   -- TMP verificar si va esto                  "  WHERE proceso_cod = ",v_i_proceso_cod
   -- TMP verificar si va esto 
   -- TMP verificar si va esto PREPARE prp_unq_pid_batCtrProc FROM v_s_qryTxt
   -- TMP verificar si va esto EXECUTE prp_unq_pid_batCtrProc INTO v_d_pid
   -- TMP verificar si va esto 
   -- TMP verificar si va esto 
   -- TMP verificar si va esto -- se invoca la funcion que valida la operacion
   -- TMP verificar si va esto CALL fn_valida_operacion(v_d_pid,v_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida
   -- TMP verificar si va esto 
   -- TMP verificar si va esto -- se verifica si la operacion en proceso es valida
   -- TMP verificar si va esto IF r_b_valida <> 0 THEN
   -- TMP verificar si va esto    -- en caso de error se muestra un mensaje a usuario y no continua
   -- TMP verificar si va esto    CALL fn_muestra_inc_operacion(r_b_valida)
   -- TMP verificar si va esto 
   -- TMP verificar si va esto    EXIT PROGRAM
   -- TMP verificar si va esto END IF
   -- TMP NO se utilizan las salidas ? Que procede --sE OBTIENEN las rutas de los ejecutables
   -- TMP NO se utilizan las salidas ? Que procede CALL fn_rutas("mdt") RETURNING v_c_ruta_bin_acr, v_ruta_vacia
   -- TMP NO se utilizan las salidas ? Que procede CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
   -- se crea la sentencia que busca los lotes disponibles por elegir
   LET v_s_qryTxt = " SELECT UNIQUE folio\n",
                    "   FROM hps_lote_mandato\n",
                    "  WHERE 1=1 ",
                    --"    AND proceso_cod = ", v_i_proceso_cod,"\n",
                    --"    AND opera_cod = ",v_i_opera_cod_ant,"\n",
                    "    AND id_origen = 1",  -- AHM TMP Validarlo si es correcto
                    "    AND estado = 101" -- Lote completo -- AHM 2010423 Ajuste de 101 a 102 para acreditados

   PREPARE prp_archivos_val FROM v_s_qryTxt
   DECLARE cur_archivos_val CURSOR FOR prp_archivos_val 
   FOREACH cur_archivos_val INTO v_arr_archPendAux --  INTO v_arr_arch_pend[v_i_indice]
     
      -- se incrementa el indice del arreglo
      CALL v_arr_arch_pend.appendElement()
      LET v_arr_arch_pend[v_arr_arch_pend.getLength()] = v_arr_archPendAux
      
      LET v_i_indice = v_i_indice + 1
   END FOREACH

   -- se abre la ventana para elejir los archivos a elegir
   OPEN WINDOW w_elige_acred WITH FORM "HPSL22"
   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF
   -- Se define la ventana para manipular los objetos a visualizar de acuerdo a la operacion
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()
      DIALOG ATTRIBUTE(UNBUFFERED)
         -- Despligue de folios pendientes
         DISPLAY ARRAY v_arr_arch_pend TO tbl_pendiente.*
            BEFORE DISPLAY
                           
            ON DRAG_START(v_ui_dnd)
               DISPLAY "v_ui_dnd: ",v_ui_dnd
               LET v_drag_source = l_nom_tbl_pend
               DISPLAY "v_drag_source = ",v_drag_source
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_pend[v_drag_index]
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL
            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF
            ON DROP(v_ui_dnd)
               IF v_drag_source == l_nom_tbl_pend THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(l_nom_tbl_pend, v_drop_index)
                  CALL DIALOG.setCurrentRow(l_nom_tbl_pend, v_drop_index)
                  LET v_arr_arch_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(l_nom_tbl_int, v_drag_index)
               END IF
         END DISPLAY
         -- Despligue de lotes elegidos
         DISPLAY ARRAY v_arr_arch_eleg TO tbl_elegido.*
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = l_nom_tbl_int
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_arr_arch_eleg[v_drag_index]
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL
            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF
            ON DROP(v_ui_dnd)
                IF v_drag_source == l_nom_tbl_int THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                   IF v_arr_arch_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      LET v_s_msj = "Solo se permite elegir un lote a la vez"
                      CALL fn_mensaje("Aviso",v_s_msj,"stop")
                   ELSE
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(l_nom_tbl_int, v_drop_index)
                      CALL DIALOG.setCurrentRow(l_nom_tbl_int, v_drop_index)
                      LET v_arr_arch_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(l_nom_tbl_pend, v_drag_index)
                   END IF
                END IF
         END DISPLAY
         -- Despligue de lotes elegidos y listos para procesar despues de aceptar la primera parte
         DISPLAY ARRAY v_arr_elegidos TO tbl_lotes_elegidos.*
         END DISPLAY
         BEFORE DIALOG
            -- Restaura botones para elegir los lotes pendientes (1ra parte)
            DISPLAY "BEFORE DIALOG"
            CALL dialog.setactionhidden("aceptar_eleccion",0)
            CALL dialog.setactionhidden("close",0)
            CALL dialog.setactionhidden("aceptar",TRUE)
            CALL dialog.setactionhidden("regresar",TRUE)
         -- Accion que controla la aceptacion de elegidos, desplegando detalle de lotes (1ra parte)
         ON ACTION aceptar_eleccion
            -- se obtiene el numero de lotes elegidos
            LET v_i_num_arch = v_arr_arch_eleg.getLength()
            -- en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF v_i_num_arch = 0 THEN
               LET v_s_msj = "Debe arrastrar al menos un archivo a lotes elegidos"
               CALL fn_mensaje("Aviso",v_s_msj,"stop")
               CONTINUE DIALOG
            END IF
            -- se limpia el arreglo de los lotes ya elegidos
            CALL v_arr_elegidos.clear()
            -- Controla los lotes realmente procesados
            LET v_i_iter_dep = 0
            LET v_i_tot_reg_sol = 0
            -- se procesan los lotes seleccionados
            FOR v_i_iter = 1 TO v_i_num_arch
               -- Validacion de lotes nulos o vacios, para que no se procesen
               IF LENGTH(v_arr_arch_eleg[v_i_iter] CLIPPED) = 0 THEN 
                  CONTINUE FOR 
               END IF
               LET v_i_iter_dep = v_i_iter_dep + 1
               -- se asigna el lote en la variable 
               {LET v_v_f_Lote = v_arr_arch_eleg[v_i_iter]
               -- se asigna la informacion del lote elegido
               LET v_arr_elegidos[v_i_iter_dep].f_proceso = v_v_f_Lote
               LET v_s_qryTxt = " SELECT folio",
                                "   FROM mdt_lote_mandato",
                                "  WHERE f_proceso = '", v_v_f_Lote,"'",
                                "    AND id_origen = 1",  -- AHM TMP Validarlo si es correcto
                                "  GROUP BY 1"}
               
               -- AHM TMP Validar que sea uno a uno !!!!!!!!!!!!!!
               -- Recuperacion de datos complementarios del lote elegido
               {PREPARE EnuDetLote FROM v_s_qryTxt
               EXECUTE EnuDetLote INTO v_arr_elegidos[v_i_iter_dep].folio}
--PROMPT "indice: ",v_i_iter FOR CHAR x
--PROMPT "folio: ",v_arr_arch_eleg[v_i_iter] FOR CHAR x 
               
--               LET prueba = v_arr_arch_eleg[v_i_iter]
--PROMPT "PRUEBA",prueba FOR CHAR x               
 --              LET v_arr_elegidos[v_i_iter_dep].folio = prueba
               LET v_arr_elegidos[v_i_iter_dep].folio = v_arr_arch_eleg[v_i_iter]
--PROMPT "indice",v_i_iter_dep FOR CHAR x               
--PROMPT "folio: ",v_arr_elegidos[v_i_iter_dep].folio FOR CHAR x

               SELECT f_proceso
                 INTO v_arr_elegidos[v_i_iter_dep].f_proceso
                 FROM hps_lote_mandato
                WHERE folio = v_arr_elegidos[v_i_iter_dep].folio 
                                       
               LET v_s_qryTxt = " SELECT NVL(count(*),0)",
                                "   FROM hps_solicitud_mandato",
                                "  WHERE folio = ?"
                                
                                
               DISPLAY "Detalle sol_mandatos v_s_qryTxt - ",v_s_qryTxt
               -- AHM TMP Validar que sea uno a uno !!!!!!!!!!!!!!
               -- Recuperacion de datos complementarios del lote elegido
               PREPARE EnuDetSol FROM v_s_qryTxt
               EXECUTE EnuDetSol USING v_arr_elegidos[v_i_iter_dep].folio 
                                  INTO v_arr_elegidos[v_i_iter_dep].tot_reg
                                       
               LET v_i_tot_reg_sol = v_i_tot_reg_sol + v_arr_elegidos[v_i_iter_dep].tot_reg
            END FOR
            -- si se muestra la informacion del lote a elegir se habilitan los botones de la 2da parte
            IF v_arr_elegidos.getLength() > 0 THEN
               -- Se muestra la opcion del proceso batch 
               CALL dialog.setactionhidden("aceptar_eleccion",TRUE)
               CALL dialog.setactionhidden("close",TRUE)
               CALL dialog.setactionhidden("aceptar",FALSE)
               CALL dialog.setactionhidden("regresar",FALSE)
            ELSE
               LET v_s_msj = "Debe arrastrar al menos un archivo a lotes elegidos"
               CALL fn_mensaje("Aviso",v_s_msj,"stop")
            END IF
            CONTINUE DIALOG
         -- Accion que controla la aceptacion de elegidos y autorizacion para su registro en 
         -- mandatos e inicio de proceso batch (2da parte)
         ON ACTION aceptar
            IF v_i_tot_reg_sol > 0 THEN
               --IF FGL_WINQUESTION("Confirmaci�n", "Desea continuar con el proceso ? ",
               --                   "yes", "yes|no", "question", 0) = "yes" THEN
               -- CALL fn_mensaje("Aviso","Proceso 46 asignado.","info") -- AHM 2012 04 25 Cambio de procesos
               
               --IF FGL_WINQUESTION("Confirmaci�n", "Proceso 46 asignado.",
               --                   "yes", "yes", "question", 0) = "yes" THEN
                  LET v_i_num_arch = v_arr_elegidos.getLength()
                  
                  FOR v_i_iter = 1 TO v_i_num_arch
                     -- Inserta movimientos en mdt_ctr_mandato y mdt_det_ctr_mandato
                     -- por cada lote elegido
                     -- TMP AHM Y este insert cuando ? CALL Fn_Ins_Mandato_Recurrente(v_arr_elegidos[v_i_iter].lote, v_arr_elegidos[v_i_iter].id_lote_mandato)
                     -- Se lanzara proceso batch por lote
                     CALL lanza_archivo_mdt(v_arr_elegidos[v_i_iter].folio, v_i_iter, v_i_num_arch)
                  END FOR
                  EXIT DIALOG
               --ELSE
               --   CALL v_arr_elegidos.clear()
               --   -- Restaura botones para elegir nuevamente los lotes (1ra parte)
               --   CONTINUE DIALOG
               --END IF
            ELSE
               CALL fn_mensaje("Advertencia","No hay registros a procesar","error")
            END IF
         -- Accion que controla la cancelacion despues de haber aceptado los elegidos (1ra parte)
         ON ACTION regresar
            CALL v_arr_elegidos.clear()
            -- Restaura botones para elegir nuevamente los lotes (1ra parte)
            CALL dialog.setactionhidden("aceptar_eleccion",0)
            CALL dialog.setactionhidden("close",0)
            CALL dialog.setactionhidden("aceptar",TRUE)
            CALL dialog.setactionhidden("regresar",TRUE)
            CONTINUE DIALOG
         -- Accion que controla la cancelacion de elegidos (2da parte)
         ON ACTION CLOSE   -- cancelar_eleccion
            EXIT DIALOG
      END DIALOG
   CLOSE WINDOW w_elige_acred
END MAIN
######################################################################
#  Modulo:      Libreria de funciones generales de mandatos          #
#  Nombre:      Fn_Ins_Mandato_Recurrente                            #
#  Descripcion: Realiza la insercion a las tablas de mandatos:       #
#               mdt_ctr_mandato y mdt_det_ctr_mandato a partir de    #
#               la tabla de paso (hps_solicitud_mandato)                   #
#  Parametros:  Entrada: v_lote_ins - lote que sera buscado en la        #
#                        tabla de paso y los movimientos ecuperados  #
#                        son los que se insertan en las tablas de    #
#                        mandatos                                    #
#               Salida:  Ninguno                                     #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION Fn_Ins_Mandato_Recurrente(v_lote_ins, p_id_lote_mandato)
DEFINE v_lote_ins             LIKE mdt_solicitud_mandato.folio
DEFINE p_id_lote_mandato  LIKE mdt_ctr_mandato.id_lote_mandato
   LET v_s_qryTxt = "SELECT *",
                    "  FROM hps_solicitud_mandato",
                    " WHERE id_origen = 1",
                    "   AND folio = ",v_lote_ins,
                    " ORDER BY id_credito"
   -- seleccion de movimientos en mdt_solicitud_mandato por lote elegido
   PREPARE EnuEncMandato FROM v_s_qryTxt
   DECLARE CurEncMandato CURSOR FOR EnuEncMandato
   FOREACH CurEncMandato INTO v_r_solicitud_mandato.*
      
      -- Verifica la existencia del credito para ser insertado o no
      IF NOT fn_sql_exi_mandato(v_r_solicitud_mandato.id_credito) THEN
         -- TMP AHM va a cambior por funcion BD seriacion
         SELECT NVL(MAX(id_ctr_mandato),0) INTO v_r_mandato.id_ctr_mandato
           FROM mdt_ctr_mandato
         IF v_r_mandato.id_ctr_mandato IS NULL or v_r_mandato.id_ctr_mandato = 0 THEN
           LET v_r_mandato.id_ctr_mandato = 1
         ELSE
           LET v_r_mandato.id_ctr_mandato = v_r_mandato.id_ctr_mandato + 1
         END IF      
         
         -- AHM TMP Validar que sea relacion uno a uno
         SELECT * INTO v_r_credito.*
           FROM cre_acreditado   -- Cambio de acr_transferencia por cre_acreditado 
          WHERE num_credito = v_r_solicitud_mandato.id_credito
          
         -- LET v_r_mandato.id_ctr_mandato          = 
         -- Inicializacion del arreglo de encabezado de mandatos antes de su insercion
         LET v_r_mandato.id_derechohabiente      = v_r_solicitud_mandato.id_derechohabiente
         LET v_r_mandato.nss                     = v_r_solicitud_mandato.nss
         LET v_r_mandato.id_credito              = v_r_solicitud_mandato.id_credito
         LET v_r_mandato.f_lote                  = NULL --v_r_solicitud_mandato.
         LET v_r_mandato.lote                    = NULL--v_r_solicitud_mandato.folio
         LET v_r_mandato.id_lote                 = 0 --v_r_solicitud_mandato.id_lote
         LET v_r_mandato.tpo_credito             = v_r_credito.tpo_credito
         LET v_r_mandato.edo_credito             = v_r_credito.edo_credito
         LET v_r_mandato.tpo_descuento_credito   = v_r_credito.tpo_dscto
         LET v_r_mandato.valor_descuento_credito = v_r_credito.valor_dscto
         LET v_r_mandato.estado                  = 100
         LET v_r_mandato.tpo_prelacion           = '' -- Por definir origen
         LET v_r_mandato.usuario                 = p_v_usuario
         LET v_r_mandato.id_lote_mandato         = NULL-- p_id_lote_mandato
         
         -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (1ra parte)
         LET v_r_mandato_det.id_ctr_mandato      = v_r_mandato.id_ctr_mandato
         LET v_r_mandato_det.id_derechohabiente  = v_r_mandato.id_derechohabiente
         LET v_r_mandato_det.nss                 = v_r_mandato.nss
         
         INSERT INTO mdt_ctr_mandato VALUES(v_r_mandato.*)
      
      END IF
      
      -- TMP AHM va a cambior por funcion BD seriacion
      SELECT NVL(MAX(id_det_ctr_mandato),0) INTO v_r_mandato_det.id_det_ctr_mandato
        FROM mdt_det_ctr_mandato
      IF v_r_mandato_det.id_det_ctr_mandato IS NULL or v_r_mandato_det.id_det_ctr_mandato = 0 THEN
        LET v_r_mandato_det.id_det_ctr_mandato = 1
      ELSE
        LET v_r_mandato_det.id_det_ctr_mandato = v_r_mandato_det.id_det_ctr_mandato + 1
      END IF      
      
      --SELECT UNIQUE id_cat_mandato INTO v_r_mandato_det.id_cat_mandato
      --  FROM mdt_cat_mandato
      -- WHERE id_mandato = v_r_solicitud_mandato.id_mandato
      -- Inicializacion del arreglo de detalle de mandatos antes de su insercion (2da parte)
      -- AHM Eliminado 2012-02-10 LET v_r_mandato_det.id_mandato              = v_r_solicitud_mandato.id_mandato
      SELECT id_cat_mandato
        INTO v_r_mandato_det.id_cat_mandato
        FROM mdt_cat_mandato_paquete
       WHERE cve_mandato = v_r_solicitud_mandato.cve_mandato
      -- Se recupera mandato del cat�logo a partir del id_mandato de tabla de paso
      {SELECT UNIQUE id_cat_mandato INTO v_r_mandato_det.id_cat_mandato
        FROM mdt_cat_mandato
       WHERE id_mandato = v_r_solicitud_mandato.id_mandato}

      LET v_r_mandato_det.tpo_descuento_mandato   = v_r_solicitud_mandato.tpo_descuento_mandato  
      LET v_r_mandato_det.valor_descuento_mandato = v_r_solicitud_mandato.valor_descuento_mandato
      LET v_r_mandato_det.f_inicio_mandato        = v_r_solicitud_mandato.f_inicio_mandato       
      LET v_r_mandato_det.f_culmina_mandato       = v_r_solicitud_mandato.f_culmina_mandato      
      LET v_r_mandato_det.referencia              = v_r_solicitud_mandato.referencia
      LET v_r_mandato_det.scta_origen_descuento   = v_r_solicitud_mandato.scta_origen_descuento
      LET v_r_mandato_det.movimiento              = NULL
      LET v_r_mandato_det.modalidad_aplicacion    = v_r_solicitud_mandato.modalidad_aplicacion
      LET v_r_mandato_det.f_presentacion          = NULL
      INSERT INTO mdt_det_ctr_mandato VALUES(v_r_mandato_det.*)
      
   END FOREACH
END FUNCTION
######################################################################
#  Modulo:      Libreria de funciones generales de mandatos          #
#  Nombre:      fn_sql_exi_mandato                                   #
#  Descripcion: Verifica la existencia del credito para insertarse   #
#               en la tabla maestro de mandatos                      #
#  Parametros:  Entrada: p_id_credito - Credito a validar si existe  #
#               Salida:  Verdadero si existe el credito, false en    #
#                        caso contrario                              #
#       Fecha de creacion:   08-Feb-2012                             #
#       Responsable:         Alexandro Hollmann Montiel, EFP         #
######################################################################
FUNCTION fn_sql_exi_mandato(p_id_credito)
DEFINE p_id_credito    LIKE mdt_ctr_mandato.id_credito
DEFINE v_i_count       INTEGER
   SELECT NVL(COUNT(*),0) INTO v_i_count
     FROM mdt_ctr_mandato
    WHERE id_credito = p_id_credito
    
   IF v_i_count > 0 THEN
      RETURN TRUE
   END IF
   RETURN FALSE
END FUNCTION

####################################################################
#Modulo            =>HPSL                                          #
#Programa          =>HPSL22                                        #
#Objetivo          =>                                              #
#Autor             =>                                              #
#Modificaci�n      => Hugo C�sar Ram�rez Garc�a                    #
#                     cambio de nombres de columnas de tablas,     #
#                     ajuste para considerar la carga de           #
#                     recurrentes en mandatos  28 Junio 2012       #
#Fecha inicio      => 2012                                         #
####################################################################
FUNCTION lanza_archivo_mdt(p_folio, p_cont_lote, p_tot_lote)
   DEFINE p_folio             INTEGER
   DEFINE --v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          --v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operaci�n que llama la funcion
          v_d_pid             DECIMAL(9,0), -- identificador del proceso
          v_c_programa_cod    LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_v_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, -- nombre del archivo en proceso
          v_c_ruta_bin_mdt    LIKE seg_modulo.ruta_bin, -- ruta del bin de mdt
          v_c_ruta_list_bat   LIKE seg_modulo.ruta_listados, -- ruta listados de bat
          v_s_comando         STRING, -- contiene al comando a correr
          v_folio_mandato     INTEGER, -- folio
          v_s_qryTxt          STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida          SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no
          v_ruta_vacia        STRING 
   DEFINE v_cadena_pid        CHAR(5)
   DEFINE v_cadena_proc       CHAR(5)
   DEFINE v_cadena_opera      CHAR(5)
   DEFINE p_cont_lote         INTEGER
   DEFINE p_tot_lote          INTEGER
          
   -- se inicializan las variables
   
   --LET v_d_pid = 0
   LET v_v_nom_archivo = "NA"
   LET v_c_programa_cod = "HPSP22"

   -- se invoca la funcion que valida la operacion
   CALL fn_valida_operacion(r_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida
   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_b_valida)
   ELSE
      -- ahora es la operacion 3 CALL fn_genera_pid(p_proceso_cod
      -- ahora es la operacion 3                   ,p_opera_cod
      -- ahora es la operacion 3                   ,p_v_usuario)
      -- ahora es la operacion 3                   RETURNING v_d_pid
      -- ahora es la operacion 3 
      -- ahora es la operacion 3 --CALL fn_obten_folio(v_i_proceso_cod,v_i_opera_cod) RETURNING v_folio_mandato
      -- ahora es la operacion 3 CALL fn_genera_folio(p_proceso_cod, p_opera_cod,p_v_usuario)  RETURNING v_folio_mandato
      -- ahora es la operacion 3 --sE OBTIENEN las rutas de los ejecutables
      -- ahora es la operacion 3 CALL fn_inicializa_proceso(v_d_pid,
      -- ahora es la operacion 3                            p_proceso_cod, 
      -- ahora es la operacion 3                            p_opera_cod,
      -- ahora es la operacion 3                            v_folio_mandato,
      -- ahora es la operacion 3                            v_c_programa_cod,
      -- ahora es la operacion 3                            v_v_nom_archivo,
      -- ahora es la operacion 3                            p_v_usuario) RETURNING r_b_valida
      -- ahora es la operacion 3 IF r_b_valida <> 0 THEN
      -- ahora es la operacion 3    -- en caso de error se muestra un mensaje a usuario y no continua
      -- ahora es la operacion 3    CALL fn_muestra_inc_operacion(r_b_valida)
      -- ahora es la operacion 3    RETURN
      -- ahora es la operacion 3 END IF
      -- se invoca la funcion que inicializa la operacion
      LET r_b_valida = fn_actualiza_opera_ini(r_pid, p_proceso_cod, p_opera_cod,
                                             v_folio_mandato, v_c_programa_cod,
                                             v_v_nom_archivo, p_v_usuario)
   
      -- se verifica si fue posible inicializar el proceso
      IF r_b_valida = 0 THEN
         CALL fn_rutas("hps") RETURNING v_c_ruta_bin_mdt, v_ruta_vacia
         CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_c_ruta_list_bat
         -- se crean las cadenas para el nombre del archivo log
         --LET v_cadena_pid   = v_d_pid USING "&&&&&"
         --LET v_cadena_proc  = v_i_proceso_cod USING "&&&&&"
         --LET v_cadena_opera = v_i_opera_cod USING "&&&&&" 
         -- se crea el comando que ejecuta el modulo que genera el archivo de salida de liquidaci�n
         LET v_s_comando = " nohup time fglrun ",v_c_ruta_bin_mdt CLIPPED,"/HPSP22 ",
                                                 p_v_usuario, " ",
                                                 r_pid, " ",
                                                 p_proceso_cod, " ",
                                                 p_opera_cod, " ",
                                                 p_folio, " '",
                                                 v_v_nom_archivo, 
                           "' 1> ",v_c_ruta_list_bat CLIPPED,
                           "/nohup:",r_pid USING "&&&&&",":",
                                     p_proceso_cod USING "&&&&&",":",
                                     p_opera_cod USING "&&&&&",
                           " 2>&1 &"

         DISPLAY v_s_comando
         RUN v_s_comando      
         IF(STATUS)THEN
            CALL fn_mensaje(v_c_programa_cod,"Ocurrio un error al ejecutar: Origen recurrente","about")
         ELSE
            --CALL fn_mensaje("Aviso","Se ejecut� el proceso de generaci�n de archivo del lote: "||p_f_lote,"info")
            --LET v_c_programa_cod = "HPSP22"
            --CALL fn_mensaje("Aviso","Proceso "||v_d_pid||" asignado.","info")
            CALL fn_mensaje("Aviso","Se ha enviado la operacion con pid "||r_pid||".\nPodr� revisar el detalle en el monitoreo de procesos","info")

           -- se invoca la funci�n que deja la operaci�n en estado Procesando
          --   LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
          --                                           v_folio_mandato, v_c_programa_cod,
          --                                           p_f_lote, p_v_usuario)

            -- se verifica si fue posible inicializar la operacion
          --IF r_b_valida <> 0 THEN
             -- en caso de error se muestra un mensaje a usuario y no continua
             --   CALL fn_muestra_inc_operacion(r_b_valida)
            --    EXIT PROGRAM
            --END IF
         END IF
      ELSE
         -- en caso de error se muestra un mensaje a usuario y no continua
         CALL fn_muestra_inc_operacion(r_b_valida)
         DISPLAY "ERROR en fn_inicializa_proceso"
      END IF
   END IF

END FUNCTION
                                                                                                                                                                                                                                                                                                   HPSL34.4gl                                                                                          0000777 0000212 0001751 00000037130 13113322765 012321  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 15-07-2015 
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL34                                                   #
#Objetivo          => Reverso preliquidaci�n pago servicios                    #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 16 Marzo 2015                                            #
#Modificado        =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

DEFINE v_ventana ui.Window

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario, # usuario logeado
       p_tipo_carga    SMALLINT, # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo        VARCHAR(30), # nombre de la ventana
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_folios_eleg   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios a reversar
       v_proceso_cod_rev LIKE cat_proceso.proceso_cod, # proceso que realiza el reverso
       v_opera_cod_rev   LIKE cat_operacion.opera_cod, # operaci�n que realiza el reverso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # proceso a reversar
       v_opera_cod       LIKE cat_operacion.opera_cod, # operaci�n a reversar
       
       v_programa_cod    LIKE bat_ctr_operacion.programa_cod, # nombre del programa que ejecuta el reverso
       v_pid             LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_pid_rev         LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_ruta_bin        LIKE seg_modulo.ruta_bin, # ruta ejecutable sep
       v_ruta_lst        LIKE seg_modulo.ruta_listados, # ruta listados de bat
       v_ruta_vacia      STRING,
       v_confirma        BOOLEAN,
       v_folio           LIKE glo_ctr_archivo.folio, # folio a reversar
       v_comando         STRING, # variable para contener comando para ejecutar batch
       v_archivo         LIKE glo_ctr_archivo.nombre_archivo,# nombre del archivo en proceso
       r_res_opera       SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no

       v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
       v_drag_index      INTEGER, -- indice del drag
       v_drop_index      INTEGER, -- indice del drop
       v_drag_source     STRING, -- fuente del drag
       v_drag_value      STRING -- valor del drag
       
       CONSTANT v_tbl_pendientes = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_tbl_elegidos = "tbl_elegidos" -- se asigna el nombre de la tabla de archivos a integrar

   # Par�metros del menu
   LET p_usuario_cod = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo      = ARG_VAL(3)

   # se asignan los valores necesarios para el reverso
   LET v_proceso_cod_rev = g_proceso_cod_rev_preliq_pago_mandatos # reverso de preliquidacion de pago mandatos
   LET v_opera_cod_rev   = g_opera_cod_carga # reverso de preliquidacion de pago mandatos
   LET v_proceso_cod     = g_proceso_cod_pago_mandatos # Pago mandatos
   LET v_opera_cod       = g_opera_cod_carga # preliquidacion de pago mandatos
   
   LET v_folio = 0      # Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_programa_cod = "HPSL34"
  
   # se invoca la funcion que valida ejecucion de reverso
   CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
   # se verifica si la operacion en proceso se puede ejecutar
   IF ( r_res_opera <> 0 ) THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF
   
   # se obtienen las rutas de separacion
   CALL fn_rutas("hps") RETURNING v_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_lst

   
   
   OPEN WINDOW vtna_reversa_folio WITH FORM v_ruta_bin CLIPPED||"/HPSL341"
      # se asigna el titulo de la ventana
      IF ( p_titulo IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_folios_pend TO tbl_pendientes.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_pendientes
               LET v_drag_index  = arr_curr()
               LET v_drag_value  = v_folios_pend[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
               IF v_drag_source == v_tbl_pendientes THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(v_tbl_pendientes, v_drop_index)
                  CALL DIALOG.setCurrentRow(v_tbl_pendientes, v_drop_index)
                  LET v_folios_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(v_tbl_elegidos, v_drag_index)
               END IF
               CALL DIALOG.setActionHidden("reverso",1)
         END DISPLAY

         DISPLAY ARRAY v_folios_eleg TO tbl_elegidos.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_elegidos
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_folios_eleg[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
                IF v_drag_source == v_tbl_elegidos THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                   IF v_folios_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      CALL fn_mensaje("Aviso","Solo se permite elegir un folio a la vez","stop")
                   ELSE 
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(v_tbl_elegidos, v_drop_index)
                      CALL DIALOG.setCurrentRow(v_tbl_elegidos, v_drop_index)
                      LET v_folios_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(v_tbl_pendientes, v_drag_index)
                   END IF
                END IF
                
         END DISPLAY

         BEFORE DIALOG
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         ON ACTION ACCEPT
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF ( v_folios_eleg.getLength() = 0 OR v_folios_eleg.getLength() IS NULL) THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a reversar","stop")
               CONTINUE DIALOG
            END IF

            CALL DIALOG.setActionHidden("reverso",0)
            CALL DIALOG.setActionHidden("cancelar",0)
            CALL DIALOG.setActionHidden("accept",1)
            CALL DIALOG.setActionHidden("close",1)
            
            CONTINUE DIALOG

         ON ACTION reverso
            # se invoca la funcion que valida ejecucion de reverso
            CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                
            # se verifica si fue posible inicializar la operacion
            IF ( r_res_opera = 0 ) THEN         
               CALL fn_ventana_confirma("Confimar","�Reversar folio?","info") RETURNING v_confirma
               IF ( v_confirma ) THEN                
                
                   # se recupera el folio elegido
                   LET v_folio = v_folios_eleg[1]
                   LET v_archivo = "NA"
                   CALL fn_genera_pid(v_proceso_cod_rev,
                                      v_opera_cod_rev,
                                      p_usuario_cod) RETURNING v_pid_rev 
                   #inicializa el proceso con todas sus operaciones en estado LISTO
                   CALL fn_inicializa_proceso(v_pid_rev, 
                                              v_proceso_cod_rev, 
                                              v_opera_cod_rev, 
                                              v_folio,
                                              "HPSL34",
                                              "NA", 
                                              p_usuario_cod) RETURNING r_res_opera                        
                   IF ( r_res_opera  <> 0 ) THEN
                      # Imprime el mensaje de inconsistencia en pantalla
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      EXIT DIALOG
                   END IF

                   CALL fn_actualiza_opera_ini(v_pid_rev,         # pid que realiza el reverso
                                               v_proceso_cod_rev, # proceso que realiza el reverso
                                               v_opera_cod_rev,   # operacion que realiza el reverso
                                               v_folio,           # folio
                                               "HPSL34",          # programa
                                               "NA",              # archivo
                                               p_usuario_cod)     # usuario
                                            RETURNING r_res_opera
                   IF ( r_res_opera <> 0 ) THEN
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                      EXIT DIALOG
                   END IF
                   
                   # se construye el comando para ser ejecutado el reverso
                   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/HPSR34.42r ",p_usuario_cod CLIPPED, " ",
                                                                                     v_pid_rev            , " ",
                                                                                     v_proceso_cod_rev    , " ",
                                                                                     v_opera_cod_rev      , " ",
                                                                                     v_folio              , " '",
                                                                                     v_archivo CLIPPED    ,
                                                                                     "' 1>", v_ruta_lst CLIPPED,
                                   "/nohup:",v_pid_rev         USING "&&&&&",":",
                                             v_proceso_cod_rev USING "&&&&&",":",
                                             v_opera_cod_rev   USING "&&&&&",
                                   " 2>&1 &"
                   
                   --DISPLAY v_comando
                   RUN v_comando
                   IF ( STATUS ) THEN
                      CALL fn_mensaje(p_titulo,"Ocurri� un error al ejecutar el reverso","about")
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                   ELSE
                      CALL fn_mensaje(p_titulo,"Se ha enviado la operaci�n.\nPodr� revisar el detalle en el monitoreo de procesos","about")
                   END IF
                   EXIT DIALOG

               END IF
            ELSE
               # en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_res_opera)             
               CONTINUE DIALOG
            END IF
            
            # se limpia el arreglo que contiene los folios
            CALL v_folios_eleg.clear()
            CALL v_folios_pend.clear()
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)           
            CONTINUE DIALOG

         ON ACTION cancelar
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)

            CALL v_folios_pend.clear()
            CALL v_folios_eleg.clear()
            
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW vtna_reversa_folio

END MAIN

{===============================================================================
Nombre: fn_recurpera_folio
Fecha creacion: 15 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar folio de pago mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recurpera_folio(p_proceso_cod,p_opera_cod)
DEFINE p_proceso_cod   LIKE cat_proceso.proceso_cod, # proceso a reversar
       p_opera_cod     LIKE cat_operacion.opera_cod, # operaci�n a reversar
       v_folio         LIKE glo_folio.folio,
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_consulta      STRING,
       v_indice        SMALLINT # indice del arrego de folios pendientes

   CALL v_folios_pend.clear()
   # se inicializa el indice del arreglo
   LET v_indice = 1
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND status = 1" # preliquidado
                    
   PREPARE prp_rec_folios FROM v_consulta   
   DECLARE cur_rec_folios CURSOR FOR prp_rec_folios
   FOREACH cur_rec_folios USING p_proceso_cod,
                                p_opera_cod
                           INTO v_folio
      LET v_folios_pend[v_indice] = v_folio
      # se incrementa el indice del arreglo
      LET v_indice = v_indice + 1
   END FOREACH
   # Si el ultimo registro es nulo, se elimina
   IF( v_folios_pend[v_folios_pend.getLength()] IS NULL )THEN
      CALL v_folios_pend.deleteElement(v_folios_pend.getLength())
   END IF

   RETURN v_folios_pend
END FUNCTION
                                                                                                                                                                                                                                                                                                                                                                                                                                        HPSL35.4gl                                                                                          0000777 0000212 0001751 00000037245 13113322765 012331  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013 
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSL35                                                   #
#Objetivo          => Reverso liquidacion pago mandatos                        #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 12 Julio 2013                                            #
#Modificado        =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

DEFINE v_ventana ui.Window

MAIN
DEFINE p_usuario_cod   LIKE seg_usuario.usuario, # usuario logeado
       p_tipo_carga    SMALLINT, # tipo de carga (1 - modo en linea y 2 - modo batch)
       p_titulo        VARCHAR(30), # nombre de la ventana
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_folios_eleg   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios a reversar
       v_proceso_cod_rev LIKE cat_proceso.proceso_cod, # proceso que realiza el reverso
       v_opera_cod_rev   LIKE cat_operacion.opera_cod, # operaci�n que realiza el reverso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # proceso a reversar
       v_opera_cod       LIKE cat_operacion.opera_cod, # operaci�n a reversar
       
       v_programa_cod    LIKE bat_ctr_operacion.programa_cod, # nombre del programa que ejecuta el reverso
       v_pid             LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_pid_rev         LIKE bat_ctr_proceso.pid, # identificador del proceso a reversar
       v_ruta_bin        LIKE seg_modulo.ruta_bin, # ruta ejecutable sep
       v_ruta_lst        LIKE seg_modulo.ruta_listados, # ruta listados de bat
       v_ruta_vacia      STRING,
       v_confirma        BOOLEAN,
       v_folio           LIKE glo_ctr_archivo.folio, # folio a reversar
       v_comando         STRING, # variable para contener comando para ejecutar batch
       v_archivo         LIKE glo_ctr_archivo.nombre_archivo,# nombre del archivo en proceso
       r_res_opera       SMALLINT, -- booleana que indica si el proceso se puede ejecutar o no

       v_ui_dnd          UI.DRAGDROP, -- manejador del arrastrar y soltar (drag and drop)
       v_drag_index      INTEGER, -- indice del drag
       v_drop_index      INTEGER, -- indice del drop
       v_drag_source     STRING, -- fuente del drag
       v_drag_value      STRING -- valor del drag
       
       CONSTANT v_tbl_pendientes = "tbl_pendientes" -- se asigna el nombre de la tabla de archivos pendientes
       CONSTANT v_tbl_elegidos = "tbl_elegidos" -- se asigna el nombre de la tabla de archivos a integrar

   # Par�metros del menu
   LET p_usuario_cod = ARG_VAL(1)
   LET p_tipo_carga  = ARG_VAL(2)
   LET p_titulo      = ARG_VAL(3)

   # se asignan los valores necesarios para el reverso
   LET v_proceso_cod_rev = g_proceso_cod_rev_liq_pago_mandatos # reverso de liquidacion de pago mandatos
   LET v_opera_cod_rev   = g_opera_cod_carga # reverso de liquidacion de pago mandatos
   LET v_proceso_cod     = g_proceso_cod_pago_mandatos # Pago mandatos
   LET v_opera_cod       = g_opera_cod_carga # preliquidacion de pago mandatos * se indica g_opera_cod_carga(1) ya que solo se necesita para recuperar el folio, y el folio esta registrado con operacion 1
   
   LET v_folio = 0      # Se indica folio 0 para que el proceso de historico genere un nuevo folio
   LET v_programa_cod = "HPSL35"
  
   # se invoca la funcion que valida ejecucion de reverso
   CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
   # se verifica si la operacion en proceso se puede ejecutar
   IF ( r_res_opera <> 0 ) THEN
      # en caso de error se muestra un mensaje a usuario y no continua
      CALL fn_muestra_inc_operacion(r_res_opera)
      EXIT PROGRAM
   END IF
   
   # se obtienen las rutas de separacion
   CALL fn_rutas("hps") RETURNING v_ruta_bin, v_ruta_vacia
   CALL fn_rutas("bat") RETURNING v_ruta_vacia, v_ruta_lst

   
   
   OPEN WINDOW vtna_reversa_folio WITH FORM v_ruta_bin CLIPPED||"/HPSL351"
      # se asigna el titulo de la ventana
      IF ( p_titulo IS NOT NULL ) THEN
         CALL ui.Interface.setText(p_titulo)
         LET v_ventana = ui.Window.getCurrent()
         CALL v_ventana.setText(p_titulo)
      END IF
      DIALOG ATTRIBUTE(UNBUFFERED)
         DISPLAY ARRAY v_folios_pend TO tbl_pendientes.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_pendientes
               LET v_drag_index  = arr_curr()
               LET v_drag_value  = v_folios_pend[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
               IF v_drag_source == v_tbl_pendientes THEN
                  CALL v_ui_dnd.dropInternal()
               ELSE
                  LET v_drop_index = v_ui_dnd.getLocationRow()
                  CALL DIALOG.insertRow(v_tbl_pendientes, v_drop_index)
                  CALL DIALOG.setCurrentRow(v_tbl_pendientes, v_drop_index)
                  LET v_folios_pend[v_drop_index] = v_drag_value
                  CALL DIALOG.deleteRow(v_tbl_elegidos, v_drag_index)
               END IF
               CALL DIALOG.setActionHidden("reverso",1)
         END DISPLAY

         DISPLAY ARRAY v_folios_eleg TO tbl_elegidos.*
            # recupera datos de origen
            ON DRAG_START(v_ui_dnd)
               LET v_drag_source = v_tbl_elegidos
               LET v_drag_index = arr_curr()
               LET v_drag_value = v_folios_eleg[v_drag_index]

            # inicializa dato origen al terminar el arrastre
            ON DRAG_FINISHED(v_ui_dnd)
               INITIALIZE v_drag_source TO NULL

            ON DRAG_ENTER(v_ui_dnd)
               IF v_drag_source IS NULL THEN
                  CALL v_ui_dnd.setOperation(NULL)
               END IF

            # asigna valores al destino
            ON DROP(v_ui_dnd)
                IF v_drag_source == v_tbl_elegidos THEN
                    CALL v_ui_dnd.dropInternal()
                ELSE
                   IF v_folios_eleg.getLength() = 1 THEN
                      CALL v_ui_dnd.dropInternal()
                      CALL fn_mensaje("Aviso","Solo se permite elegir un folio a la vez","stop")
                   ELSE 
                      LET v_drop_index = v_ui_dnd.getLocationRow()
                      CALL DIALOG.insertRow(v_tbl_elegidos, v_drop_index)
                      CALL DIALOG.setCurrentRow(v_tbl_elegidos, v_drop_index)
                      LET v_folios_eleg[v_drop_index] = v_drag_value
                      CALL DIALOG.deleteRow(v_tbl_pendientes, v_drag_index)
                   END IF
                END IF
                
         END DISPLAY

         BEFORE DIALOG
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)

         ON ACTION ACCEPT
            # en caso de no existir registros a procesar se informa que debe de haber al menos uno
            IF ( v_folios_eleg.getLength() = 0 ) THEN
               CALL fn_mensaje("Aviso","Debe arrastrar al menos un folio a reversar","stop")
               CONTINUE DIALOG
            END IF

            CALL DIALOG.setActionHidden("reverso",0)
            CALL DIALOG.setActionHidden("cancelar",0)
            CALL DIALOG.setActionHidden("accept",1)
            CALL DIALOG.setActionHidden("close",1)
            
            CONTINUE DIALOG

         ON ACTION reverso
            # se invoca la funcion que valida ejecucion de reverso
            CALL fn_valida_operacion(0,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                
            # se verifica si fue posible inicializar la operacion
            IF ( r_res_opera = 0 ) THEN         
               CALL fn_ventana_confirma("Confimar","�Reversar folio?","info") RETURNING v_confirma
               IF ( v_confirma ) THEN                
                
                   # se recupera el folio elegido
                   LET v_folio = v_folios_eleg[1]
                   LET v_archivo = "NA"
                   CALL fn_genera_pid(v_proceso_cod_rev,
                                      v_opera_cod_rev,
                                      p_usuario_cod) RETURNING v_pid_rev 
                   #inicializa el proceso con todas sus operaciones en estado LISTO
                   CALL fn_inicializa_proceso(v_pid_rev, 
                                              v_proceso_cod_rev, 
                                              v_opera_cod_rev, 
                                              v_folio,
                                              "HPSL35",
                                              "NA", 
                                              p_usuario_cod) RETURNING r_res_opera                        
                   IF ( r_res_opera  <> 0 ) THEN
                      # Imprime el mensaje de inconsistencia en pantalla
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      EXIT DIALOG
                   END IF

                   CALL fn_actualiza_opera_ini(v_pid_rev,         # pid que realiza el reverso
                                               v_proceso_cod_rev, # proceso que realiza el reverso
                                               v_opera_cod_rev,   # operacion que realiza el reverso
                                               v_folio,           # folio
                                               "HPSL35",          # programa
                                               "NA",              # archivo
                                               p_usuario_cod)     # usuario
                                            RETURNING r_res_opera
                   IF ( r_res_opera <> 0 ) THEN
                      CALL fn_muestra_inc_operacion(r_res_opera)
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                      EXIT DIALOG
                   END IF
                   
                   # se construye el comando para ser ejecutado el reverso
                   LET v_comando = "nohup fglrun ",v_ruta_bin CLIPPED,"/HPSR35.42r ",p_usuario_cod CLIPPED, " ",
                                                                                     v_pid_rev            , " ",
                                                                                     v_proceso_cod_rev    , " ",
                                                                                     v_opera_cod_rev      , " ",
                                                                                     v_folio              , " '",
                                                                                     v_archivo CLIPPED    ,
                                                                                     "' 1>", v_ruta_lst CLIPPED,
                                   "/nohup:",v_pid_rev         USING "&&&&&",":",
                                             v_proceso_cod_rev USING "&&&&&",":",
                                             v_opera_cod_rev   USING "&&&&&",
                                   " 2>&1 &"
                   
                   --DISPLAY v_comando
                   RUN v_comando
                   IF ( STATUS ) THEN
                      CALL fn_mensaje(p_titulo,"Ocurri� un error al ejecutar el reverso","about")
                      CALL fn_error_opera(v_pid_rev,v_proceso_cod_rev,v_opera_cod_rev) RETURNING r_res_opera
                   ELSE
                      CALL fn_mensaje(p_titulo,"Se ha enviado la operaci�n.\nPodr� revisar el detalle en el monitoreo de procesos","about")
                   END IF
                   EXIT DIALOG

               END IF
            ELSE
               # en caso de error se muestra un mensaje a usuario y no continua
               CALL fn_muestra_inc_operacion(r_res_opera)             
               CONTINUE DIALOG
            END IF
            
            # se limpia el arreglo que contiene los folios
            CALL v_folios_eleg.clear()
            CALL v_folios_pend.clear()
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)           
            CONTINUE DIALOG

         ON ACTION cancelar
            CALL DIALOG.setActionHidden("reverso",1)
            CALL DIALOG.setActionHidden("cancelar",1)
            CALL DIALOG.setActionHidden("accept",0)
            CALL DIALOG.setActionHidden("close",0)

            CALL v_folios_pend.clear()
            CALL v_folios_eleg.clear()
            
            CALL fn_recurpera_folio(v_proceso_cod,v_opera_cod) RETURNING v_folios_pend
            CONTINUE DIALOG
         
         ON ACTION close
            EXIT DIALOG
            
      END DIALOG

   CLOSE WINDOW vtna_reversa_folio

END MAIN

{===============================================================================
Nombre: fn_recurpera_folio
Fecha creacion: 15 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para recuperar folio de pago mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_recurpera_folio(p_proceso_cod,p_opera_cod)
DEFINE p_proceso_cod   LIKE cat_proceso.proceso_cod, # proceso a reversar
       p_opera_cod     LIKE cat_operacion.opera_cod, # operaci�n a reversar
       v_folio         LIKE glo_folio.folio,
       v_folios_pend   DYNAMIC ARRAY OF VARCHAR(12), # arreglo que contiene los folios pendientes 
       v_consulta      STRING,
       v_indice        SMALLINT # indice del arrego de folios pendientes

   CALL v_folios_pend.clear()
   # se inicializa el indice del arreglo
   LET v_indice = 1
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND status = 2" # liquidado
                    
   PREPARE prp_rec_folios FROM v_consulta   
   DECLARE cur_rec_folios CURSOR FOR prp_rec_folios
   FOREACH cur_rec_folios USING p_proceso_cod,
                                p_opera_cod
                           INTO v_folio
      LET v_folios_pend[v_indice] = v_folio
      # se incrementa el indice del arreglo
      LET v_indice = v_indice + 1
   END FOREACH
   # Si el ultimo registro es nulo, se elimina
   IF( v_folios_pend[v_folios_pend.getLength()] IS NULL )THEN
      CALL v_folios_pend.deleteElement(v_folios_pend.getLength())
   END IF

   RETURN v_folios_pend
END FUNCTION
                                                                                                                                                                                                                                                                                                                                                           HPSM01.4gl                                                                                          0000777 0000212 0001751 00000276434 13113322761 012324  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--==============================================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => CATALOGO DE ALTA DE MANDATOS                            #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
IMPORT os
DATABASE safre_viv

--VARIABLES GLOBALES
DEFINE v_arr_grupos DYNAMIC ARRAY OF RECORD
   asignar       BOOLEAN
  ,id_cat_gpo    LIKE mdt_cat_gpo.id_cat_gpo
  ,descripcion   LIKE mdt_cat_gpo.descripcion
END RECORD
---Array de los elementos del grupo
DEFINE v_arr_elementos DYNAMIC ARRAY OF RECORD
   id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
  ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
  ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
END RECORD
--Array para el detalle
DEFINE v_arr_detalle DYNAMIC ARRAY OF RECORD
   id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
  ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
  ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
  ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato
  ,orden               LIKE mdt_cat_atributo_nivel.orden
  ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
  ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
  ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
END RECORD
---Array de los elementos del grupo
DEFINE v_arr_elementos_tmp DYNAMIC ARRAY OF RECORD
   id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
  ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
  ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
END RECORD
--Array para el detalle
DEFINE v_arr_detalle_tmp DYNAMIC ARRAY OF RECORD
   id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
  ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
  ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
  ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato  
  ,orden               LIKE mdt_cat_atributo_nivel.orden
  ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
  ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
  ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
END RECORD
DEFINE v_imagen_docto   DYNAMIC ARRAY OF RECORD 
        id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato  LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen   STRING,--LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int STRING, -- conserva la ruta seleccionada por el usuario
        v_documento     STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen     STRING--LIKE mdt_imagen_docto.desc_imagen
       END RECORD,
       v_imagen_docto_general   DYNAMIC ARRAY OF RECORD 
        id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato  LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen   STRING, # Boton examininar y continene la ruta de archivo local
        v_documento_int STRING, # link del documento
        v_documento     STRING, # nombre del documento, etiqueta oculta
        desc_imagen     STRING  # descripcion de la ,imagen
       END RECORD
       
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod -- codigo del proceso
      ,g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
      ,v_desc_mandato        LIKE mdt_cat_mandato.desc_mandato
      ,v_desc_larga_mandato  LIKE mdt_cat_mandato.desc_larga_mandato
      ,v_tpo_mandato         LIKE mdt_cat_mandato.tpo_mandato  --Tipo de mandato
      ,v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato  --Tipo de mandato
      ,p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
      ,v_prompt              INTEGER,
       v_ruta_docto          LIKE seg_modulo.ruta_docto,
       v_ventana             ui.Window,
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       g_id_municipio LIKE cat_municipio.municipio
      

######################################################################################
# INICIA EL MAIN
######################################################################################
MAIN
DEFINE p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
      ,p_titulo              STRING   -- titulo de la ventana
      ,v_ingreso_mandato     BOOLEAN
      ,v_id_cat_gpo_actual   LIKE mdt_cat_gpo.id_cat_gpo
      ,v_desc_mandato_tmp        LIKE mdt_cat_mandato.desc_mandato
      ,v_desc_larga_mandato_tmp  LIKE mdt_cat_mandato.desc_larga_mandato
      ,v_estatus                 INTEGER
DEFINE cb ui.ComboBox  --Tipo Combobox
DEFINE v_rec_tipo_mandato    RECORD LIKE mdt_tpo_mandato.*
DEFINE p_tipo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
       v_direccion     RECORD # arreglo para desplegar todas las direcciones encontradas
        v_cp           LIKE cat_cp.cp,
        v_id_colonia   LIKE cat_colonia.colonia, 
        v_colonia      LIKE cat_colonia.colonia_desc,
        v_id_municipio LIKE cat_municipio.municipio,
        v_municipio    LIKE cat_municipio.municipio_desc,
        v_id_ciudad    LIKE cat_ciudad.ciudad,
        v_ciudad       LIKE cat_ciudad.ciudad_desc,
        v_id_entidad   LIKE cat_entidad_federativa.entidad_federativa,
        v_entidad      LIKE cat_entidad_federativa.entidad_desc_larga
       END RECORD,
       v_bnd_continua     BOOLEAN,
       v_consulta         STRING,
       v_conteo_direccion INTEGER,
       v_indice           INTEGER,
       v_indice_bus       INTEGER
DEFINE v_path             STRING
DEFINE v_file_ext         STRING
DEFINE v_ext              STRING
DEFINE v_caption          STRING
DEFINE v_filesel          STRING,
       v_resultado        STRING,
       v_indice_aux       INTEGER
DEFINE v_s_genera_digito_vrf_b   SMALLINT
DEFINE v_s_genera_digito_vrf_p   SMALLINT
DEFINE v_s_genera_digito_vrf_n   SMALLINT
DEFINE v_s_genera_digito_vrf_c   SMALLINT
DEFINE v_c_banco          CHAR(3)
DEFINE v_c_plaza          CHAR(3)
DEFINE v_c_cta            CHAR(11) 
DEFINE v_c_cta_clabe      CHAR(18)
DEFINE v_bnd_estatus      SMALLINT
DEFINE v_bnd_municipio    BOOLEAN

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)
   LET p_tipo_mandato   = ARG_VAL(4)

   --CALL ui.Interface.loadStyles("mdtstyle")

   --DISPLAY resourceUri("myImage.png", "mdtdocto")
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   # Recuper la ruta de los documetos 
   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = "mdt"

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps"   

   # busca si hay una instruccion realcionada con direccion
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM mdt_cat_gpo_etiqueta et JOIN mdt_cat_gpo gpo",
                    "\n     ON gpo.id_cat_gpo = et.id_cat_gpo",
                    "\n  WHERE gpo.descripcion = 'DIRECCION'",
                    "\n    AND et.id_gpo_etiqueta = ?"
   PREPARE prp_encuentra_elemento_direccion FROM v_consulta
   LET v_conteo_direccion = 0

   -- Se asignan las variables de control 
   --LET g_proceso_cod = 0
   --LET g_opera_cod   = 0
   LET v_ingreso_mandato = FALSE  --Se inicia la bandera de ingreso de mandato
   LET v_desc_mandato_tmp = ""
   LET v_desc_larga_mandato_tmp = ""
   # se abre la primera captura de documento
   --CALL v_imagen_docto.insertElement(1)
   --Se abre la ventana de captura del nuevo mandato
   OPEN WINDOW w_mant_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/HPSM011"
      INPUT v_desc_mandato, v_desc_larga_mandato FROM desc_mandato, desc_larga_mandato ATTRIBUTES (UNBUFFERED)
         BEFORE INPUT
            IF(p_titulo IS NOT NULL)THEN
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo)
            END IF
            
            --Se llena el combo con los datos
            --LET cb = ui.ComboBox.forName("formonly.cb_tipo_mandato")
            --DECLARE cur_tipo_mandato CURSOR FOR
            SELECT desc_tpo_mandato
              INTO v_tpo_mandato_desc
              FROM mdt_tpo_mandato
             WHERE tpo_mandato = p_tipo_mandato
            LET v_tpo_mandato = p_tipo_mandato

            DISPLAY v_tpo_mandato_desc TO cb_tipo_mandato
            --FOREACH cur_tipo_mandato INTO v_rec_tipo_mandato.*
            --   CALL cb.addItem(v_rec_tipo_mandato.tpo_mandato,v_rec_tipo_mandato.desc_tpo_mandato)
            --END FOREACH
            
         ON ACTION ACCEPT
            --Se valida que los registros no se esten vacios
            IF v_desc_mandato CLIPPED = v_desc_mandato_tmp CLIPPED  OR v_desc_mandato  IS NULL THEN
               CALL fn_mensaje(p_titulo,"Capture descripci�n del mandato","information")
               NEXT FIELD desc_mandato
               --CONTINUE INPUT
            END IF
            IF v_desc_larga_mandato CLIPPED = v_desc_larga_mandato_tmp CLIPPED OR v_desc_larga_mandato IS NULL THEN
               CALL fn_mensaje(p_titulo,"Capture descripci�n larga del mandato","information")
               NEXT FIELD desc_larga_mandato
               --CONTINUE INPUT
            ELSE
               LET v_ingreso_mandato = TRUE
               EXIT INPUT
            END IF
         ON ACTION CANCEL
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_mant_mandatos

   CALL f_inicia_arrays()
   
   IF v_ingreso_mandato = TRUE THEN
   --Se abre la ventana para la captura de los detalles y grupos del mandato
   OPEN WINDOW w_mant_mandatos_detalle WITH FORM v_ruta_ejecutable CLIPPED||"/HPSM012"
      CALL ui.Dialog.setDefaultUnbuffered(TRUE)
      DIALOG  ATTRIBUTES (UNBUFFERED , FIELD ORDER FORM)
      
      
         INPUT ARRAY v_arr_grupos FROM tabla_grupo.* 
         ATTRIBUTE( INSERT ROW=FALSE, APPEND ROW=FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, WITHOUT DEFAULTS)
            BEFORE INPUT
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo)
            
            BEFORE ROW
               IF v_arr_grupos[ARR_CURR()].asignar = 1 THEN 
                  LET v_id_cat_gpo_actual = v_arr_grupos[ARR_CURR()].id_cat_gpo
               ELSE
                  LET v_id_cat_gpo_actual = NULL
               END IF
               CALL f_pasa_tmp_elementos(v_id_cat_gpo_actual)
               CALL f_pasa_tmp_detalle(v_id_cat_gpo_actual) --Se muestra el detalle del primer elemento
               # selecciona solo los registros del grupo
               CALL f_pasa_tmp_archivos(v_id_cat_gpo_actual)
               CALL ui.Interface.refresh()

            ON CHANGE tb_asignar
               IF v_arr_grupos[ARR_CURR()].asignar = 1 THEN 
                  LET v_id_cat_gpo_actual = v_arr_grupos[ARR_CURR()].id_cat_gpo
               ELSE
                  LET v_id_cat_gpo_actual = NULL
               END IF
               CALL f_pasa_tmp_elementos(v_id_cat_gpo_actual)
               CALL f_pasa_tmp_detalle(v_id_cat_gpo_actual) --Se muestra el detalle del primer elemento
               # selecciona solo los registros del grupo
               CALL f_pasa_tmp_archivos(v_id_cat_gpo_actual)
               CALL ui.Interface.refresh()
         END INPUT
         
         DISPLAY ARRAY v_arr_elementos_tmp TO tabla_elemento.*
            BEFORE DISPLAY
               --Se HABILITAN los botones del eliminar y agregar al entrar al display
               IF v_arr_elementos_tmp.getLength() > 0 THEN
                  CALL DIALOG.setActionActive("agregar",TRUE)
                  CALL DIALOG.setActionActive("eliminar",TRUE)
               END IF
            AFTER DISPLAY
               --Se desabilitan los botones del eliminar y agregar para que solo se modifique cuando se esta en este display array
               CALL DIALOG.setActionActive("agregar",FALSE)
               CALL DIALOG.setActionActive("eliminar",FALSE)
         END DISPLAY
         
         INPUT ARRAY v_arr_detalle_tmp FROM v_arr_detalle_tmp.* --tabla_detalle.*
         ATTRIBUTE( INSERT ROW=FALSE, APPEND ROW=FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, WITHOUT DEFAULTS)
         --ATTRIBUTE( WITHOUT DEFAULTS)
            BEFORE INPUT
               
               CALL DIALOG.setActionActive("subir",TRUE)
               CALL DIALOG.setActionActive("bajar",TRUE)
               
               IF v_arr_detalle_tmp.getLength() <> 0 THEN
                  LET v_indice = ARR_CURR()
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "ENTIDAD FINANCIERA" AND 
                     LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                      CALL fn_busca_entidad_financiera() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
                  END IF
                  
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "PLAZA" AND 
                     LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                      CALL fn_busca_plaza() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
                  END IF
                  
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                     LET v_s_genera_digito_vrf_b = 0
                     LET v_s_genera_digito_vrf_p = 0
                     LET v_s_genera_digito_vrf_n = 0
                     LET v_c_banco = '   '
                     LET v_c_plaza = '   '
                     LET v_c_cta   = '           '
                     IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                        FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                              CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                              LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                           END IF
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                              CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                              LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                           END IF
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                                 LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                                 LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                              END IF
                           END IF
                        END FOR
                        IF v_s_genera_digito_vrf_b = 1 AND
                           v_s_genera_digito_vrf_p = 1 AND
                           v_s_genera_digito_vrf_n = 1 THEN
                           -- Genera digito verificador
                           CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                           IF v_bnd_estatus = 1 THEN
                              CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                           END IF
                        ELSE
                           IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                              IF v_s_genera_digito_vrf_b = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                    IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                       LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                       LET v_c_banco = '   '
                                    END IF
                                 END IF
                              END IF
                              IF v_s_genera_digito_vrf_p = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                    IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                       LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                       LET v_c_plaza = '   '
                                    END IF
                                 END IF
                              END IF
                              IF v_s_genera_digito_vrf_n = 0 THEN
                                 LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                              END IF
                              LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                           END IF
                        END IF
                     ELSE
                        CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                                  1)
                             RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                        END IF
                     END IF
                  END IF
               END IF
               
               # busca si hay una instruccion realcionada con direccion
               FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                  LET v_conteo_direccion = 0
                  EXECUTE prp_encuentra_elemento_direccion USING v_arr_detalle_tmp[v_indice].id_gpo_etiqueta
                                                           INTO  v_conteo_direccion
                  IF(v_conteo_direccion > 0)THEN
                     # Habilita boton para busqueda de direccion 
                     CALL DIALOG.setActionActive("direccion",TRUE)
                     CALL fn_busca_direccion_postal()RETURNING v_direccion.*,v_bnd_continua
                     IF(v_bnd_continua)THEN
                        FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                           # busca en el arreglo de atributos, alg�no que sea de direccion postal y le agrega el valor recuperado
                           CASE v_arr_detalle_tmp[v_indice].etiqueta CLIPPED
                              WHEN 'ENTIDAD FEDERATIVA'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_entidad                                 
                              WHEN 'CIUDAD'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_ciudad
                              WHEN 'MUNICIPIO'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_municipio
                                 # variable que se inserta como paquete en mdt_cat_mandato_paquete
                                 LET g_id_municipio = v_direccion.v_id_municipio      
                              WHEN 'CODIGO POSTAL'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_cp                                 
                              WHEN 'COLONIA'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_colonia
                           END CASE
                        END FOR
                     END IF
                     --FOR v_indice = 1 TO v_imagen_docto.getLength()
                     --   DISPLAY "RUTA: "||v_imagen_docto[v_indice].nombre_imagen
                     --END FOR
                     
                     EXIT FOR
                  END IF
               END FOR
               LET v_bnd_continua = FALSE
               
            BEFORE ROW
               LET v_indice = ARR_CURR()
               IF v_arr_detalle_tmp[v_indice].etiqueta = "ENTIDAD FINANCIERA" AND 
                  LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                   CALL fn_busca_entidad_financiera() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
               END IF

               IF v_arr_detalle_tmp[v_indice].etiqueta = "PLAZA" AND 
                  LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                   CALL fn_busca_plaza() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
               END IF

               IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                  LET v_s_genera_digito_vrf_b = 0
                  LET v_s_genera_digito_vrf_p = 0
                  LET v_s_genera_digito_vrf_n = 0
                  LET v_c_banco = '   '
                  LET v_c_plaza = '   '
                  LET v_c_cta   = '           '
                  IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                     FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                           CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                           LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                           CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                           LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                           IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                              LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                           END IF
                        END IF
                     END FOR
                     IF v_s_genera_digito_vrf_b = 1 AND
                        v_s_genera_digito_vrf_p = 1 AND
                        v_s_genera_digito_vrf_n = 1 THEN
                        -- Genera digito verificador
                        CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                    LET v_c_plaza = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_n = 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                           END IF
                           LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                        END IF
                     END IF
                  ELSE
                     CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                               1)
                          RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                     IF v_bnd_estatus = 1 THEN
                        CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     END IF
                  END IF
               END IF
            
            AFTER INPUT
               CALL DIALOG.setActionActive("subir",FALSE)
               CALL DIALOG.setActionActive("bajar",FALSE)
               CALL DIALOG.setActionActive("bajar",FALSE)
               CALL DIALOG.setActionActive("direccion",FALSE)
            AFTER ROW
               LET v_indice = ARR_CURR()
               IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                  LET v_s_genera_digito_vrf_b = 0
                  LET v_s_genera_digito_vrf_p = 0
                  LET v_s_genera_digito_vrf_n = 0
                  LET v_c_banco = '   '
                  LET v_c_plaza = '   '
                  LET v_c_cta   = '           '
                  IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                     FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                           CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                           LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                           CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                           LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                           IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                              LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                           END IF
                        END IF
                     END FOR
                     IF v_s_genera_digito_vrf_b = 1 AND
                        v_s_genera_digito_vrf_p = 1 AND
                        v_s_genera_digito_vrf_n = 1 THEN
                        -- Genera digito verificador
                        CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                    LET v_c_plaza = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_n = 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                           END IF
                           LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                        END IF
                     END IF
                  ELSE
                     CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                               1)
                          RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                     IF v_bnd_estatus = 1 THEN
                        CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     END IF
                  END IF
               END IF

               CALL fn_pasa_tmp_general_detalle()
            {ON CHANGE tb_valor_etiqueta
               CALL fn_pasa_tmp_general_detalle()}
            
         END INPUT

         #######################################################################
         #---------------------- Arreglo de documentos ------------------------#
         #######################################################################
         INPUT ARRAY v_imagen_docto FROM sr_imagenes.* --tabla_detalle.*
                                    ATTRIBUTE(APPEND ROW = FALSE, INSERT ROW=FALSE, 
                                              DELETE ROW = FALSE, AUTO APPEND = FALSE, 
                                              WITHOUT DEFAULTS)
         --ATTRIBUTE( WITHOUT DEFAULTS)
            BEFORE INPUT
               --CALL DIALOG.setActionActive("btn_img_ver",TRUE)
               CALL DIALOG.setActionActive("btn_img_doc",TRUE)
               CALL DIALOG.setActionActive("btn_img_quitar",TRUE)
               
               
            AFTER INPUT
               --CALL DIALOG.setActionActive("btn_img_ver",FALSE)
               CALL DIALOG.setActionActive("btn_img_doc",FALSE)
               CALL DIALOG.setActionActive("btn_img_quitar",FALSE)
               
            ON CHANGE tedi_ruta
               CALL GET_FLDBUF(tedi_ruta) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               CALL GET_FLDBUF(tedi_documento) RETURNING v_imagen_docto[ARR_CURR()].v_documento

               
               # se consideran 10 caracteres para identificadores para un total de 100
               IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) > 90)THEN
                  CALL fn_mensaje("Aviso","Tama�o del nombre de archivo excede el m�ximo permitido","information")
                  INITIALIZE v_imagen_docto[ARR_CURR()].nombre_imagen TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento_int TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].desc_imagen TO NULL
               ELSE
                  LET v_imagen_docto[ARR_CURR()].v_documento_int = v_imagen_docto[ARR_CURR()].v_documento 
                  IF(v_imagen_docto[ARR_CURR()].v_documento IS NOT NULL OR v_imagen_docto[ARR_CURR()].v_documento <> " ")THEN
                     # elimina el registro en caso de que se seleccion� otro archivo
                     CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B') 
                         RETURNING v_resultado
                  END IF
               END IF

            AFTER FIELD tedi_ruta
               
               CALL GET_FLDBUF(tedi_ruta) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               # si no es nulo o por que no seleccion� un archivo
               DISPLAY "AFTER FIELD: ",v_imagen_docto[ARR_CURR()].nombre_imagen 
               # se consideran 10 caracteres para identificadores para un total de 100
               IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) < 90 AND v_imagen_docto[ARR_CURR()].nombre_imagen IS NOT NULL)THEN
                  # se recupera el identificador de grupo actual
                  LET v_indice = DIALOG.getCurrentRow("tabla_grupo")
                  # Se agrega al nuevo registro el id_cat_gpo al que pertenecer�
                  LET v_imagen_docto[ARR_CURR()].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo
                  
                  # Modifica el nombre de archivo y llama funcion fgl_getFile
                  CALL fn_transfiere_archivo(v_imagen_docto[ARR_CURR()].*,'','') RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
                  LET v_imagen_docto[ARR_CURR()].v_documento = v_imagen_docto[ARR_CURR()].nombre_imagen
                  INITIALIZE v_imagen_docto[ARR_CURR()].nombre_imagen  TO NULL
                  LET v_imagen_docto[ARR_CURR()].v_documento_int = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                  --LET v_imagen_docto[ARR_CURR()].v_documento_int = v_imagen_docto[ARR_CURR()].v_documento  
                  # Agrega un documento nuevo
                  # Pasa el elemento de arreglo tmp a arreglo general               
                  CALL v_imagen_docto_general.appendElement()
                  LET v_imagen_docto_general[v_imagen_docto_general.getLength()].* = v_imagen_docto[ARR_CURR()].*
               ELSE
                  INITIALIZE v_imagen_docto[ARR_CURR()].nombre_imagen TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].desc_imagen TO NULL
                  
               END IF

            AFTER FIELD tedi_descripcion
               CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[ARR_CURR()].desc_imagen
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado 
               --FOR v_indice = 1 TO v_imagen_docto_general.getLength()
               --   IF(v_imagen_docto_general[v_indice].v_documento     = v_imagen_docto[ARR_CURR()].v_documento AND
               --      v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo )THEN
                       
               --      LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[ARR_CURR()].desc_imagen
               --      EXIT FOR
                        
               --   END IF
               --END FOR 

            ON CHANGE tedi_descripcion               
               CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[ARR_CURR()].desc_imagen
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado
               FOR v_indice = 1 TO v_imagen_docto_general.getLength()
                  IF(v_imagen_docto_general[v_indice].v_documento     = v_imagen_docto[ARR_CURR()].v_documento AND
                     v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo)THEN
                       
                     LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[ARR_CURR()].desc_imagen
                     EXIT FOR
                        
                  END IF
               END FOR 
            
         END INPUT
         
         BEFORE DIALOG
            IF(p_titulo IS NOT NULL)THEN
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo)
            END IF
            --Se desabilitan los botones del eliminar y agregar
            CALL DIALOG.setActionActive("agregar",FALSE)
            CALL DIALOG.setActionActive("eliminar",FALSE)
            --Se desabilitan los botones de subir y bajar
            CALL DIALOG.setActionActive("direccion",FALSE)
            CALL DIALOG.setActionActive("subir",FALSE)
            CALL DIALOG.setActionActive("bajar",FALSE)
            # Se desabilitan los botones de ver, documento y quitar
            --CALL DIALOG.setActionActive("btn_img_ver",FALSE)
            CALL DIALOG.setActionActive("btn_img_doc",FALSE)
            CALL DIALOG.setActionActive("btn_img_quitar",FALSE)

            
         ON ACTION Agregar
            IF v_arr_elementos_tmp.getLength() > 0 THEN
               {DISPLAY "v_arr_elementos_tmp.getLength():",v_arr_elementos_tmp.getLength()
               DISPLAY "v_arr_elementos_tmp[ARR_CURR()].*:",v_arr_elementos_tmp[ARR_CURR()].*}
               --CALL FGL_DIALOG_GETBUFFERSTART( ) RETURNING v_estatus
               CALL fn_agegar_eliminar_detalle(v_arr_elementos_tmp[ARR_CURR()].*,"agregar")
               --DISPLAY v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* TO tabla_detalle[v_arr_detalle_tmp.getLength()].*
               {CALL ui.Interface.refresh()
               DISPLAY "**************************"
               DISPLAY "v_arr_detalle_tmp: ",v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].*
               CALL ui.Interface.refresh()
               CALL SET_COUNT(2)
               DISPLAY ARRAY v_arr_detalle_tmp TO v_arr_detalle_tmp.*
               --DISPLAY BY NAME v_arr_detalle_tmp
                  BEFORE DISPLAY 
                     EXIT DISPLAY
               END DISPLAY}
               CALL ui.Interface.refresh()
            END IF
         ON ACTION Eliminar
            IF v_arr_elementos_tmp.getLength() > 0 THEN
               {DISPLAY "v_arr_elementos_tmp.getLength():",v_arr_elementos_tmp.getLength()
               DISPLAY "v_arr_elementos_tmp[ARR_CURR()].*:",v_arr_elementos_tmp[ARR_CURR()].*}
               CALL fn_agegar_eliminar_detalle(v_arr_elementos_tmp[ARR_CURR()].*,"eliminar")
               CALL ui.Interface.refresh()
            END IF

         ON ACTION direccion
            CALL fn_busca_direccion_postal()RETURNING v_direccion.*,v_bnd_continua
            IF(v_bnd_continua)THEN
               FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                  CASE v_arr_detalle_tmp[v_indice].etiqueta CLIPPED
                     WHEN 'ENTIDAD FEDERATIVA'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_entidad
                     WHEN 'CIUDAD'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_ciudad
                     WHEN 'MUNICIPIO'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_municipio
                        # variable que se inserta como paquete en mdt_cat_mandato_paquete
                        LET g_id_municipio = v_direccion.v_id_municipio      
                     WHEN 'CODIGO POSTAL'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_cp
                     WHEN 'COLONIA'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_colonia
                  END CASE
               END FOR
            END IF
         
         ON ACTION subir
            CALL fn_pasa_tmp_general_detalle()
            CALL fn_ordena_posicion_detalle(ARR_CURR(),"subir") RETURNING v_prompt
            CALL FGL_SET_ARR_CURR( v_prompt )
            
         ON ACTION bajar
            CALL fn_pasa_tmp_general_detalle()
            CALL fn_ordena_posicion_detalle(ARR_CURR(),"bajar") RETURNING v_prompt
            CALL FGL_SET_ARR_CURR( v_prompt )
            
         ON ACTION ACCEPT
            LET v_indice_aux = 0
            LET v_bnd_municipio = FALSE
            LET v_indice_aux = DIALOG.getCurrentRow("sr_imagenes")
            IF(v_indice_aux > 0)THEN
               LET v_imagen_docto[v_indice_aux].desc_imagen = GET_FLDBUF(tedi_descripcion) CLIPPED
              
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado
               FOR v_indice = 1 TO v_imagen_docto_general.getLength()
                  IF(v_imagen_docto_general[v_indice].v_documento     = v_imagen_docto[ARR_CURR()].v_documento AND
                     v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo)THEN
                       
                     LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[ARR_CURR()].desc_imagen
                     EXIT FOR
                        
                  END IF
               END FOR 
            END IF
               
            --Se valida si es posible guardar los registros
            CALL fn_pasa_tmp_general_detalle()
            IF NOT f_valida_captura_cat() THEN
               --Si regresa falso la validaci�n se indica que debe revisar los datos
               CALL fn_mensaje("Aviso","Campo sin valor asignado, Verificar Captura","error")
            ELSE
               -- Valizaci�n de entidades financieras
               LET v_s_genera_digito_vrf_b = 0
               LET v_s_genera_digito_vrf_p = 0
               LET v_s_genera_digito_vrf_n = 0
               LET v_s_genera_digito_vrf_c = 0
               LET v_c_banco = '   '
               LET v_c_plaza = '   '
               LET v_c_cta   = '           '
               FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                     CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                     LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                     CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                     LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                     IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                        LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                        LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                     END IF
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "CUENTA CLABE" THEN
                     IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                        LET v_c_cta_clabe = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta
                        LET v_s_genera_digito_vrf_c = v_s_genera_digito_vrf_c + 1
                     END IF
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "MUNICIPIO" THEN
                     LET v_bnd_municipio = TRUE
                  END IF
                  
               END FOR
               IF v_s_genera_digito_vrf_b = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[1,3] <> v_c_banco THEN
                     CALL fn_mensaje("Aviso","La clave de banco no corresponde entre\nla cuenta CLABE y la entidad financiera","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_b = 1 OR v_s_genera_digito_vrf_c = 1 THEN
                  IF v_s_genera_digito_vrf_c = 1 THEN
                     LET v_c_banco = v_c_cta_clabe[1,3]
                  END IF
                  IF NOT fn_existe_entidad_financiera(v_c_banco) THEN
                     CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_p = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[4,6] <> v_c_plaza THEN
                     CALL fn_mensaje("Aviso","La clave de plaza no corresponde con la cuenta CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_p = 1 OR v_s_genera_digito_vrf_c = 1 THEN
                  IF v_s_genera_digito_vrf_c = 1 THEN
                     LET v_c_plaza = v_c_cta_clabe[4,6]
                  END IF
                  IF NOT fn_existe_plaza(v_c_plaza) THEN
                     CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_n = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[7,17] <> v_c_cta THEN
                     CALL fn_mensaje("Aviso","El n�mero de cuenta no corresponde con la cuenta CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF    
               IF v_s_genera_digito_vrf_c = 1 THEN
                  CALL fn_genera_digito_vrf(v_c_cta_clabe[1,3]  ,
                                            v_c_cta_clabe[4,6]  ,
                                            v_c_cta_clabe[7,17] ,
                                            v_c_cta_clabe[18,18],
                                            1)
                       RETURNING v_bnd_estatus, v_c_cta_clabe
                  IF v_bnd_estatus = 1 THEN
                     CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               # Valida que se haya captura la etiqueta municipio para el tipo mandato predial
               # p_tipo_mandato = "01"    - prediales
               # v_bnd_municipio = FALSE  - no se ha capturado municipio para construir clave mandato (paquete)
               --IF(p_tipo_mandato = "01" AND v_bnd_municipio = FALSE)THEN
               IF(v_bnd_municipio = FALSE)THEN
                  CALL fn_mensaje("Aviso","Capture municipio","error")
                  CONTINUE DIALOG
               END IF
                           
               IF fn_ventana_confirma("Confimar","�Desea guardar el registro?","info") = 1 THEN
                  --Si confimo se sale del dialog y se marca la badera para guardar los registros
                  IF fn_almacena_registro_det() = FALSE THEN
                     CALL fn_mensaje("Aviso","Mandato "|| v_desc_mandato CLIPPED||" capturado","error")
                     EXIT DIALOG
                  ELSE
                     CALL fn_mensaje("Error","Ocurrio un error al almacenar los registros","error")
                     CONTINUE DIALOG
                  END IF
               ELSE
                  CONTINUE DIALOG
               END IF
            END IF

         --ON ACTION btn_img_ver
         --  CALL fgl_winmessage("Ver",v_imagen_docto[ARR_CURR()].v_documento_int,"information")
         --  FOR v_indice = 1 TO v_imagen_docto.getLength()
         --
         --  END FOR
            
            
         ON ACTION btn_img_doc
            LET v_indice = 1
            # recupera el numero de renglon seleccionado del arreglo de docimentos
            LET v_indice = DIALOG.getCurrentRow("sr_imagenes")
            IF(v_indice IS NULL OR v_indice = 0)THEN
               # no hay alg�n registro y se inserta uno
               CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
            ELSE
               # se recuperan los valores actuales
               CALL GET_FLDBUF(tedi_ruta) RETURNING v_imagen_docto[v_indice].nombre_imagen
               CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[v_indice].desc_imagen
               # Se indica que los campos han cambiado
               CALL DIALOG.setFieldTouched("tedi_ruta", TRUE)
               CALL DIALOG.setFieldTouched("tedi_descripcion", TRUE)
               --CALL v_imagen_docto.appendElement()
               # agrega un nuevo regitro al final del arreglo 
               CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
               # en el caso de que necesote pasar por si solo al nuevo renglon
               --CALL DIALOG.setCurrentRow("sr_imagenes", v_imagen_docto.getLength())
            END IF
            
            --CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
         --   LET v_path     = ""
         --   LET v_file_ext = ""
         --   LET v_ext      = ""
         --   LET v_caption  = "Seleccione archivo"
         --   CALL ui.interface.frontCall("standard", "openfile", [v_path,v_file_ext,v_ext,v_caption], [v_filesel])
 
         ON ACTION btn_img_quitar
            # elimina fisicamente el archivo
            CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B') 
                   RETURNING v_resultado
            IF(v_resultado <> "ELIMINADO")THEN
               # si no se pudo elminiar
               CALL fn_mensaje("Aviso","Registro "||v_resultado CLIPPED,"about")
            ELSE
               # Elimina de los arreglos( temporal y general)
               CALL fn_elimina_documento(v_imagen_docto[ARR_CURR()].*)
               CALL v_imagen_docto.deleteElement(ARR_CURR())
            END IF
            

         --ON ACTION captura
         --   CALL v_imagen_docto.appendElement() --deleteElement(ARR_CURR())
            
         ON ACTION CLOSE -- cancelar
            IF fn_ventana_confirma("Confimar","�Desea salir y cancelar la captura?","info") = 1 THEN
               # Se eliminan los archivos que han sido transferidos
               IF(v_imagen_docto_general.getLength() > 0)THEN
                  FOR v_indice = 1 TO v_imagen_docto_general.getLength()
                     CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,0, 'B') 
                            RETURNING v_resultado
                  END FOR 
               END IF
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
      END DIALOG
   CLOSE WINDOW w_mant_mandatos_detalle
   END IF
END MAIN

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Llenar los arrays globales del modulo                   #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_inicia_arrays()
   DEFINE v_indice   INTEGER

   --Se ingresan los registros de los grupos
   DECLARE cur_grupos CURSOR FOR
   SELECT id_cat_gpo, descripcion
   FROM mdt_cat_gpo 

   LET v_indice = 1
   FOREACH cur_grupos INTO v_arr_grupos[v_indice].id_cat_gpo
                          ,v_arr_grupos[v_indice].descripcion
      --Se asigana por default que no se selecciona el registro
      LET v_arr_grupos[v_indice].asignar = 0
      LET v_indice = v_indice + 1
   END FOREACH
   --Se elimina el ultimo registro si es que esta nulo
   IF v_arr_grupos[v_arr_grupos.getLength()].id_cat_gpo IS NULL THEN 
      CALL v_arr_grupos.deleteElement(v_arr_grupos.getLength())
   END IF

   DECLARE cur_elementos CURSOR FOR
   SELECT id_gpo_etiqueta, id_cat_gpo, etiqueta
   FROM mdt_cat_gpo_etiqueta

   LET v_indice = 1
   FOREACH cur_elementos INTO v_arr_elementos[v_indice].id_gpo_etiqueta
                             ,v_arr_elementos[v_indice].id_cat_gpo
                             ,v_arr_elementos[v_indice].etiqueta
      --Se asigana por default que no se selecciona el registro
      LET v_indice = v_indice + 1
   END FOREACH
   --Se elimina el ultimo registro si es que esta nulo
   IF v_arr_elementos[v_arr_elementos.getLength()].id_gpo_etiqueta IS NULL THEN 
      CALL v_arr_elementos.deleteElement(v_arr_elementos.getLength())
   END IF
   {
   LET v_arr_grupos[1].asignar = 1
   LET v_arr_grupos[1].id_cat_gpo = "1"
   LET v_arr_grupos[1].descripcion = "Grupo 1"
   LET v_arr_grupos[2].asignar = 0
   LET v_arr_grupos[2].id_cat_gpo = "2"
   LET v_arr_grupos[2].descripcion = "Grupo 2"
   }{
   LET v_arr_elementos[1].id_gpo_etiqueta = 1
   LET v_arr_elementos[1].id_cat_gpo = 1
   LET v_arr_elementos[1].etiqueta = "Elemento 1"
   LET v_arr_elementos[2].id_gpo_etiqueta = 2
   LET v_arr_elementos[2].id_cat_gpo = 2
   LET v_arr_elementos[2].etiqueta = "Elemento 2"
   LET v_arr_elementos[3].id_gpo_etiqueta = 3
   LET v_arr_elementos[3].id_cat_gpo = 2
   LET v_arr_elementos[3].etiqueta = "Elemento 3"
   LET v_arr_elementos[4].id_gpo_etiqueta = 4
   LET v_arr_elementos[4].id_cat_gpo = 2
   LET v_arr_elementos[4].etiqueta = "Elemento 4"
}
   {
   LET v_arr_detalle[1].id_atr_nivel = ""
   LET v_arr_detalle[1].id_gpo_etiqueta = 1
   LET v_arr_detalle[1].id_cat_mandato = ""
   LET v_arr_detalle[1].id_gpo_mandato = 1
   LET v_arr_detalle[1].orden = "1"
   LET v_arr_detalle[1].etiqueta = "Elemento 1"
   LET v_arr_detalle[1].valor_etiqueta = "Detalle 1"
   --
   LET v_arr_detalle[2].id_atr_nivel = ""
   LET v_arr_detalle[2].id_gpo_etiqueta = 1
   LET v_arr_detalle[2].id_cat_mandato = ""
   LET v_arr_detalle[2].id_gpo_mandato = 2
   LET v_arr_detalle[2].orden = "1"
   LET v_arr_detalle[2].etiqueta = "Elemento 2"
   LET v_arr_detalle[2].valor_etiqueta = "Detalle 2"
   --
   LET v_arr_detalle[3].id_atr_nivel = ""
   LET v_arr_detalle[3].id_gpo_etiqueta = 1
   LET v_arr_detalle[3].id_cat_mandato = ""
   LET v_arr_detalle[3].id_gpo_mandato = 2
   LET v_arr_detalle[3].orden = "2"
   LET v_arr_detalle[3].etiqueta = "Elemento 2"
   LET v_arr_detalle[3].valor_etiqueta = "Detalle 3"
   INITIALIZE v_arr_elementos_tmp TO NULL
   INITIALIZE v_arr_detalle_tmp TO NULL
   }
END FUNCTION

###############################################################################
#Modulo            => HPS                                                  #
#Programa          => HPSM01                                                  #
#Objetivo          => eliminar el documento seleccionado por el usuario       #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      => 06 Marzo 2012                                           #
###############################################################################
FUNCTION fn_elimina_documento(v_imagen_docto)
DEFINE v_indice_general INTEGER,
       v_imagen_docto   RECORD 
        id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato  LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen   STRING,--LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int STRING, -- conserva la ruta seleccionada por el usuario
        v_documento     STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen     LIKE mdt_imagen_docto.desc_imagen
       END RECORD

   # busca el elemento a eliminar en el arreglo general
   FOR v_indice_general = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice_general].id_cat_gpo = v_imagen_docto.id_cat_gpo AND
         v_imagen_docto_general[v_indice_general].v_documento = v_imagen_docto.v_documento AND 
         v_imagen_docto_general[v_indice_general].desc_imagen = v_imagen_docto.desc_imagen)THEN
         # Elimina el elemento del arreglo general y sale del for
         CALL v_imagen_docto_general.deleteElement(v_indice_general)
         EXIT FOR
      END IF
   END FOR 
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los elementos       #
#                     segun su id_grupo                                       #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_elementos(p_id_cat_gpo)
   DEFINE p_id_cat_gpo  LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador    INTEGER
   --Se inicia el array temporal
   INITIALIZE v_arr_elementos_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL THEN
      FOR v_contador = 1 TO v_arr_elementos.getLength()
         IF v_arr_elementos[v_contador].id_cat_gpo = p_id_cat_gpo THEN
            CALL v_arr_elementos_tmp.appendElement()
            LET  v_arr_elementos_tmp[v_arr_elementos_tmp.getLength()].* = v_arr_elementos[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa el arreglo temporal al general del detalle         #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_pasa_tmp_general_detalle()
   DEFINE v_indice_tmp  INTEGER
   DEFINE v_indice      INTEGER
   --Se recorre todo el arreglo temporal
   FOR v_indice_tmp = 1 TO v_arr_detalle_tmp.getLength()
      --Se recorre todo el arreglo general
      FOR v_indice = 1 TO v_arr_detalle.getLength()
         IF  --v_arr_detalle_tmp[v_indice_tmp].id_atr_nivel    = v_arr_detalle[v_indice].id_atr_nivel   
          v_arr_detalle_tmp[v_indice_tmp].id_gpo_etiqueta = v_arr_detalle[v_indice].id_gpo_etiqueta
         --AND v_arr_detalle_tmp[v_indice_tmp].id_cat_mandato  = v_arr_detalle[v_indice].id_cat_mandato 
         AND v_arr_detalle_tmp[v_indice_tmp].id_gpo_mandato  = v_arr_detalle[v_indice].id_gpo_mandato 
         --AND v_arr_detalle_tmp[v_indice_tmp].orden           = v_arr_detalle[v_indice].orden          
         AND v_arr_detalle_tmp[v_indice_tmp].etiqueta        = v_arr_detalle[v_indice].etiqueta THEN       
         --AND v_arr_detalle_tmp[v_indice_tmp].valor_etiqueta  = v_arr_detalle[v_indice].valor_etiqueta THEN
            LET v_arr_detalle[v_indice].valor_etiqueta = v_arr_detalle_tmp[v_indice_tmp].valor_etiqueta
            EXIT FOR
         END IF
      END FOR
   END FOR
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_detalle(p_id_cat_gpo)
   DEFINE p_id_cat_gpo       LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador         INTEGER
   --Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].id_gpo_mandato  = p_id_cat_gpo 
         THEN
            CALL v_arr_detalle_tmp.appendElement()
            LET  v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* = v_arr_detalle[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los documentos      #
#                     seg�n su id_cat_gpo y el grupo seleccionado             #
#Autor             => Hugo C�sar Ramp�rez Grac�a                              #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_archivos(p_id_cat_gpo)
DEFINE p_id_cat_gpo LIKE mdt_cat_gpo.id_cat_gpo,
       v_contador   INTEGER
   # Se incia el array temporal
   INITIALIZE v_imagen_docto TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      FOR v_contador = 1 TO v_imagen_docto_general.getLength()
         IF(v_imagen_docto_general[v_contador].id_cat_gpo  = p_id_cat_gpo)THEN
            CALL v_imagen_docto.appendElement()
            LET  v_imagen_docto[v_imagen_docto.getLength()].* = v_imagen_docto_general[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
{FUNCTION f_pasa_tmp_detalle(p_id_cat_gpo)
   DEFINE p_id_cat_gpo       LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador         INTEGER
   --Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].id_gpo_mandato  = p_id_cat_gpo 
         THEN
            CALL v_arr_detalle_tmp.appendElement()
            LET  v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* = v_arr_detalle[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION}
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Sube o baja una posici�n del array temporal y general   #
#                     del detalle                                             #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_ordena_posicion_detalle(v_posicion_actual,v_tipo_operacion)
   DEFINE v_posicion_actual      INTEGER
         ,v_posicion_intercambio INTEGER
         ,v_posicion_actual_aux  INTEGER
         ,v_posicion_intercambio_aux INTEGER
         ,v_tipo_operacion       VARCHAR(10)
         ,v_contador             INTEGER
   DEFINE v_arr_detalle_actual, v_arr_detalle_intercambio RECORD
      id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
     ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
     ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
     ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato
     ,orden               LIKE mdt_cat_atributo_nivel.orden
     ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
     ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
     ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
   END RECORD
   LET v_posicion_intercambio = v_posicion_actual
   IF v_tipo_operacion = "subir" AND v_posicion_actual > 1 THEN
      LET v_posicion_intercambio = v_posicion_actual - 1
      --Se almacenan en records temporales los registros que se van a intercambiar
      LET v_arr_detalle_actual.*      = v_arr_detalle_tmp[v_posicion_actual].*
      LET v_arr_detalle_intercambio.* = v_arr_detalle_tmp[v_posicion_intercambio].*
      --Se hace el intercambio de renglones en la tabla temporal
      LET v_arr_detalle_tmp[v_posicion_actual].* = v_arr_detalle_intercambio.*
      LET v_arr_detalle_tmp[v_posicion_intercambio].* = v_arr_detalle_actual.*
      --Se obtienen los indices para el intercambio en la tabla general del detalle
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].* = v_arr_detalle_intercambio.* THEN
            LET v_posicion_intercambio_aux = v_contador
         END IF
      END FOR
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].* = v_arr_detalle_actual.* THEN
            LET v_posicion_actual_aux = v_contador
         END IF
      END FOR
      --Se realiza el intercambio en la tabla general del detalle
      LET v_arr_detalle[v_posicion_actual_aux].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle[v_posicion_intercambio_aux].* = v_arr_detalle_actual.* 
   END IF
   
   IF v_tipo_operacion = "bajar" AND v_posicion_actual < v_arr_detalle_tmp.getLength() THEN
      LET v_posicion_intercambio = v_posicion_actual + 1
      --Se almacenan en records temporales los registros que se van a intercambiar
      LET v_arr_detalle_actual.*      = v_arr_detalle_tmp[v_posicion_actual].*
      LET v_arr_detalle_intercambio.* = v_arr_detalle_tmp[v_posicion_intercambio].*
      --Se hace el intercambio de renglones en la tabla temporal
      LET v_arr_detalle_tmp[v_posicion_actual].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle_tmp[v_posicion_intercambio].* = v_arr_detalle_actual.*
      --Se obtienen los indices para el intercambio en la tabla general del detalle
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].* = v_arr_detalle_intercambio.* THEN
            LET v_posicion_intercambio_aux = v_contador
         END IF
      END FOR
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].* = v_arr_detalle_actual.* THEN
            LET v_posicion_actual_aux = v_contador
         END IF
      END FOR
      --Se realiza el intercambio en la tabla general del detalle
      LET v_arr_detalle[v_posicion_actual_aux].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle[v_posicion_intercambio_aux].* = v_arr_detalle_actual.* 
   END IF
   RETURN v_posicion_intercambio
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Agrega o elimina un registro a las tablas temporales    #
#                     y generales del detalle                                 #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_agegar_eliminar_detalle(v_arr_elementos_aux,v_tipo_operacion)
   DEFINE v_tipo_operacion   VARCHAR(10)
         ,v_contador_1       INTEGER
         ,v_badera_existente BOOLEAN
         ,v_indice           INTEGER
         ,v_indice_tmp       INTEGER
         ,v_nuevo_indice     INTEGER
---Array de los elementos del grupo
   DEFINE v_arr_elementos_aux RECORD
      id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
     ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
     ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
   END RECORD
   --DISPLAY "****** AGREGAR O ELIMINAR   **********  - ", v_tipo_operacion
   --DISPLAY "Array recibido v_arr_elementos_aux.*:",v_arr_elementos_aux.*
   --Solo se a�aden si el elemento existe
   LET v_badera_existente = FALSE
   IF v_tipo_operacion = "agregar" THEN 
      --Antes de agregar se verifica que no exista el registro en la tabla temporal
      FOR v_contador_1 = 1 TO v_arr_detalle_tmp.getLength()
         IF v_arr_detalle_tmp[v_contador_1].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle_tmp[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle_tmp[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se sale del segundo FOR y se marca la bandera como encontrado
            LET v_badera_existente = TRUE
            EXIT FOR
         END IF
      END FOR
      --Se verifica si el registro existio
      IF v_badera_existente = FALSE THEN
         --DISPLAY "ANTES v_arr_detalle.getLength()    :",v_arr_detalle.getLength()
         --DISPLAY "ANTES v_arr_detalle_tmp.getLength():",v_arr_detalle_tmp.getLength()
         --Se a�ade el registro hasta el final del arreglo temporal
         --CALL v_arr_detalle_tmp.appendElement()
         LET v_nuevo_indice = v_arr_detalle_tmp.getLength() + 1
         {LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].etiqueta = v_arr_elementos_aux.etiqueta}
         LET v_arr_detalle_tmp[v_nuevo_indice].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_nuevo_indice].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_nuevo_indice].etiqueta = v_arr_elementos_aux.etiqueta
         --Se a�ade el registro hasta el final del arreglo global
         CALL v_arr_detalle.appendElement()
         LET v_arr_detalle[v_arr_detalle.getLength()].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle[v_arr_detalle.getLength()].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle[v_arr_detalle.getLength()].etiqueta = v_arr_elementos_aux.etiqueta
         --DISPLAY "SE INSERTO: v_arr_detalle_tmp.*:",v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].*
         --DISPLAY "SE INSERTO: v_arr_detalle.*    :",v_arr_detalle[v_arr_detalle.getLength()].*
         --DISPLAY "DESPUES v_arr_detalle.getLength()    :",v_arr_detalle.getLength()
         --DISPLAY "DESPUES v_arr_detalle_tmp.getLength():",v_arr_detalle_tmp.getLength()
      END IF
   END IF
   IF v_tipo_operacion = "eliminar" THEN 
      FOR v_contador_1 = 1 TO v_arr_detalle_tmp.getLength()
         IF v_arr_detalle_tmp[v_contador_1].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle_tmp[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle_tmp[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se obtiene el indice de la tabla temporal
            LET v_indice_tmp = v_contador_1
            EXIT FOR
         END IF
      END FOR
      --Obtenemos el indice de la tabla general de detalle
      FOR v_contador_1 = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador_1].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se obtiene el indice de la tabla temporal
            LET v_indice = v_contador_1
            EXIT FOR
         END IF
      END FOR
      --Borramos los registros obtenidos
      IF v_indice > 0 THEN
         --DISPLAY "ANTES v_arr_detalle.getLength(): ",v_arr_detalle.getLength()
         CALL v_arr_detalle.deleteElement(v_indice)
         --DISPLAY "DESPUES v_arr_detalle.getLength(): ",v_arr_detalle.getLength()
      END IF
      IF v_indice_tmp > 0 THEN
         --DISPLAY "ANTES v_arr_detalle_tmp.getLength(): ",v_arr_detalle_tmp.getLength()
         CALL v_arr_detalle_tmp.deleteElement(v_indice_tmp)
         --DISPLAY "DESPUES v_arr_detalle_tmp.getLength(): ",v_arr_detalle_tmp.getLength()
      END IF
   END IF
   
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Verifica si es posible guardar el registro              #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_valida_captura_cat()
   DEFINE v_indice_grupo       INTEGER
         ,v_indice_validacion  INTEGER
         ,v_indice_detalle     INTEGER
         ,v_bandera_valido     BOOLEAN
         ,v_bandera_grupo      BOOLEAN  --indica si se selecciono al menos un grupo
         ,v_bandera_detalle    BOOLEAN  --Indica que por lo menos se encontro un registro del detalle
         ,v_string_comparacion  STRING

   DEFINE v_arr_bandera_valido DYNAMIC ARRAY OF RECORD
      grupo   INTEGER  --Para indicar indice del grupo que se debe de validar
     ,detalle BOOLEAN  --Indica que por lo menos debe de tener un detalle
   END RECORD
   INITIALIZE v_arr_bandera_valido TO NULL
   LET v_indice_validacion = 0
   LET v_indice_validacion = FALSE
   LET v_bandera_grupo = FALSE
   
   --Se recorre el arreglo de los grupos que esten indicados para almacenar
   FOR v_indice_grupo = 1 TO v_arr_grupos.getLength()
      IF v_arr_grupos[v_indice_grupo].asignar = 1 THEN
         --Si inicializa el array que indica los que se deben guardar
         LET v_bandera_grupo = TRUE
         LET v_indice_validacion = v_indice_validacion + 1
         LET v_arr_bandera_valido[v_indice_validacion].grupo   = v_indice_grupo
         LET v_arr_bandera_valido[v_indice_validacion].detalle = TRUE  --Se asume que el registro es correcto
         LET v_bandera_detalle = FALSE
         --Se recorre el arreglo de los detalles para validar que tenga por lo menos un registro
         
         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
            IF v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_arr_grupos[v_indice_grupo].id_cat_gpo THEN
               LET v_bandera_detalle = TRUE
               LET v_string_comparacion = v_arr_detalle[v_indice_detalle].valor_etiqueta CLIPPED
               IF v_string_comparacion.trim() = "" OR v_string_comparacion.trim() IS NULL THEN
                  --Si es nulo o vacio se  indica que ocurrio un error
                  LET v_arr_bandera_valido[v_indice_validacion].detalle = FALSE
                  EXIT FOR --Ya no se siguen validando el resto de los registros
               END IF
            END IF
         END FOR
         --Se verifica si por lo menos se encontro un registro
         IF v_bandera_detalle = FALSE THEN
            LET v_arr_bandera_valido[v_indice_validacion].detalle = FALSE
         END IF
      END IF
   END FOR
   
   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validaci�n
   IF v_arr_bandera_valido.getLength() > 0 THEN
      FOR v_indice_grupo = 1 TO v_arr_bandera_valido.getLength()
         IF v_arr_bandera_valido[v_indice_grupo].detalle = FALSE THEN
            LET v_bandera_valido = FALSE --Si encuentra un false todo se rechasa
            EXIT FOR
         END IF
      END FOR
   ELSE
      LET v_bandera_valido = FALSE
   END IF
   # se recorre el arreglo para validar las rutas de los archivos a sibur, revisa que no sean nulas
   FOR v_indice_grupo = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice_grupo].v_documento IS NULL OR v_imagen_docto_general[v_indice_grupo].v_documento = " ")THEN
         LET v_bandera_valido = FALSE
         EXIT FOR
      END IF
   END FOR
   
   IF v_bandera_valido = TRUE AND v_bandera_grupo = TRUE THEN 
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Se almacena el registro en base de datos                #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_almacena_registro_det()
   DEFINE v_max_mdt_cat_mandato INTEGER
         ,v_max_id_gpo_mandato  INTEGER
         ,v_max_id_gpo_etiqueta INTEGER
         ,v_max_id_atr_nivel    INTEGER
         ,v_max_id_mdt_notifica INTEGER
         ,v_id_instancia_mandato INTEGER
         ,v_indice              INTEGER
         ,v_indice_detalle      INTEGER
         ,v_indice_gpo          INTEGER
         ,v_orden               INTEGER
         ,v_error               BOOLEAN
         ,v_n_cuenta_bancaria     LIKE mdt_notifica_mandato.n_cuenta_bancaria
         ,v_n_convenio            LIKE mdt_notifica_mandato.n_convenio
         ,v_n_referencia          LIKE mdt_notifica_mandato.n_referencia
         ,v_cta_clabe             LIKE mdt_notifica_mandato.cta_clabe,
         v_resultado             STRING,
         v_indice_doc            INTEGER,
         v_cve_mandato           LIKE mdt_notifica_mandato.cve_mandato -- Ajuste 20120410 Campo modificado
  DEFINE v_l_tpo_mandato         CHAR(2)
  DEFINE v_l_cat_mandato         CHAR(5)
  DEFINE v_cve_mandato_muni      INTEGER
  DEFINE v_cve_mandato_paquete   CHAR(16)
  DEFINE v_cve_mandato_paquete_aux LIKE mdt_cat_mandato_paquete.cve_mandato --  CHAR(18)
  DEFINE v_id_cat_mandato_paquete  INTEGER

   LET v_error = FALSE
   LET v_n_cuenta_bancaria = NULL
   LET v_n_convenio = NULL
   LET v_n_referencia = NULL
   LET v_cta_clabe = NULL

   WHENEVER ERROR CONTINUE
   
   --PASO 1: Se inserta en mdt_cat_mandato
       SELECT (NVL(MAX(id_cat_mandato),0) +1) INTO V_max_mdt_cat_mandato
       FROM mdt_cat_mandato

   LET v_id_cat_mandato_paquete = V_max_mdt_cat_mandato
   
   LET v_l_tpo_mandato = v_tpo_mandato USING "&&" 
   LET v_l_cat_mandato = V_max_mdt_cat_mandato USING "&&&&&" 
   LET v_cve_mandato = v_l_tpo_mandato||v_l_cat_mandato
   
   INSERT INTO mdt_cat_mandato
   (id_cat_mandato, 
    cve_mandato, 
    desc_mandato, 
    desc_larga_mandato, 
    usuario, 
    tpo_mandato, 
    f_creacion, 
    estado)
   VALUES
   (
    V_max_mdt_cat_mandato
   ,v_cve_mandato
   ,v_desc_mandato
   ,v_desc_larga_mandato
   ,p_usuario_cod
   ,v_tpo_mandato
   ,TODAY
   ,100
   )
   IF SQLCA.SQLCODE <> 0 THEN
      LET v_error = TRUE
   END IF
   
   --PASO 2: Se inserta la asciacion de los grupos en mdt_gpo_mandato
   LET v_orden = 1
   FOR v_indice = 1 TO v_arr_grupos.getLength()
      IF v_arr_grupos[v_indice].asignar = 1 THEN
         SELECT (NVL(MAX(id_gpo_mandato),0) +1) INTO v_max_id_gpo_mandato
         FROM mdt_gpo_mandato
         INSERT INTO mdt_gpo_mandato
         (id_gpo_mandato, id_cat_mandato, id_cat_gpo, orden, usuario)
         VALUES
         (
          v_max_id_gpo_mandato
         ,V_max_mdt_cat_mandato
         ,v_arr_grupos[v_indice].id_cat_gpo
         ,v_orden
         ,p_usuario_cod
         )
         --El que se inserto se busca en el arreglo de los detalles para asignarle el id_gpo_mandato
         IF SQLCA.SQLCODE <> 0 THEN
            LET v_error = TRUE
         END IF
         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
            --
            IF v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_arr_grupos[v_indice].id_cat_gpo THEN
               --Se asigna el grupo obtenido
               LET v_arr_detalle[v_indice_detalle].id_cat_mandato = V_max_mdt_cat_mandato
               LET v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_detalle[v_indice_detalle].id_gpo_mandato
               LET v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_max_id_gpo_mandato
               LET v_arr_detalle[v_indice_detalle].orden = v_orden
               LET v_orden = v_orden + 1
            END IF
         END FOR
         # Se insertan todos los documentos capturados y que correspondan al grupo
         FOR v_indice_doc = 1 TO v_imagen_docto_general.getLength()
            IF(v_imagen_docto_general[v_indice_doc].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo)THEN
               IF(LENGTH(v_imagen_docto_general[v_indice_doc].v_documento.trim()) > 0)THEN
                  LET v_imagen_docto_general[v_indice_doc].id_gpo_mandato = v_max_id_gpo_mandato
                  # recupera el identificador consecutivo
                  SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice_doc].id_imagen_docto
                     FROM mdt_imagen_docto
                  # se modifica el nombre del archivo temporal al nombre con el que quedara
                  CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice_doc].v_documento,v_imagen_docto_general[v_indice_doc].id_imagen_docto, 'A') 
                         RETURNING v_imagen_docto_general[v_indice_doc].nombre_imagen
                  IF(v_imagen_docto_general[v_indice_doc].nombre_imagen = "NO MODIFICADO")THEN
                     # en caso de error
                     CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice_doc].nombre_imagen CLIPPED,"about")
                     LET v_error = TRUE
                  ELSE
                  
                     # almacena registro en BD
                     CALL fn_inserta_docto(v_imagen_docto_general[v_indice_doc].*)
                  END IF
               END IF
            END IF
         END FOR
      ELSE
         FOR v_indice_doc = 1 TO v_imagen_docto_general.getLength()
            IF(v_imagen_docto_general[v_indice_doc].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo)THEN
               IF(LENGTH(v_imagen_docto_general[v_indice_doc].v_documento.trim()) > 0)THEN
                  # se modifica el nombre del archivo temporal al nombre con el que quedara
                  CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice_doc].v_documento,0, 'B') 
                         RETURNING v_imagen_docto_general[v_indice_doc].nombre_imagen
                  IF(v_imagen_docto_general[v_indice_doc].nombre_imagen = "NO ELIMINADO")THEN
                     # en caso de error
                     CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice_doc].nombre_imagen CLIPPED,"about")
                     LET v_error = TRUE
                  END IF
               END IF
            END IF
         END FOR
      END IF
   END FOR
   {
   --PASO 3: Se inserta la asciacion de las etiquetas en mdt_cat_gpo_etiqueta
   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
      --Se insertan todos los registros de este array
      SELECT (NVL(MAX(id_gpo_etiqueta),0) +1) INTO v_max_id_gpo_etiqueta
      FROM mdt_cat_gpo_etiqueta
      --Se efectua el insert
      --DISPLAY "mdt_cat_gpo_etiqueta:",v_max_id_gpo_etiqueta
      INSERT INTO mdt_cat_gpo_etiqueta
      (id_gpo_etiqueta, id_cat_gpo, etiqueta, usuario)
      VALUES
      (
       v_max_id_gpo_etiqueta
      ,v_arr_detalle[v_indice_detalle].id_cat_gpo
      ,v_arr_detalle[v_indice_detalle].etiqueta
      ,p_usuario_cod
      )
      IF SQLCA.SQLCODE <> 0 THEN
         LET v_error = TRUE
      END IF
      --Se inserta su id_gpo_etiqueta
      LET v_arr_detalle[v_indice_detalle].id_gpo_etiqueta = v_max_id_gpo_etiqueta
   END FOR
}
   --PASO 4: Se almacena el resto del detalle en mdt_cat_atributo_nivel
   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
      --Se inserta en BD
      SELECT (NVL(MAX(id_atr_nivel),0) +1) INTO v_max_id_atr_nivel
      FROM mdt_cat_atributo_nivel
      INSERT INTO mdt_cat_atributo_nivel
      (id_atr_nivel, id_gpo_etiqueta, id_cat_mandato, id_gpo_mandato, orden, id_habilita, usuario)
      VALUES
      (
       v_max_id_atr_nivel
      ,v_arr_detalle[v_indice_detalle].id_gpo_etiqueta
      ,v_arr_detalle[v_indice_detalle].id_cat_mandato
      ,v_arr_detalle[v_indice_detalle].id_gpo_mandato
      ,v_arr_detalle[v_indice_detalle].orden
      ,0
      ,p_usuario_cod
      )
      IF SQLCA.SQLCODE <> 0 THEN
         LET v_error = TRUE
      END IF
      LET v_arr_detalle[v_indice_detalle].id_atr_nivel = v_max_id_atr_nivel
   END FOR
   LET v_cve_mandato_paquete_aux = "                  "
   --PASO 5: Se inserta la instancia del mandato
   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
      SELECT (NVL(MAX(id_instancia_mandato),0) +1) INTO v_id_instancia_mandato
      FROM mdt_cat_instancia_mandato
      INSERT INTO mdt_cat_instancia_mandato
      (id_instancia_mandato, id_atr_nivel, valor_etiqueta, usuario)
      VALUES
      (
       v_id_instancia_mandato
      ,v_arr_detalle[v_indice_detalle].id_atr_nivel
      ,v_arr_detalle[v_indice_detalle].valor_etiqueta
      ,p_usuario_cod
      )
      
      --Se obtienen los identificadores segun el campo que nos interesa
      CASE v_arr_detalle[v_indice_detalle].etiqueta CLIPPED
         WHEN "NUMERO DE CUENTA"
            LET v_n_cuenta_bancaria = v_arr_detalle[v_indice_detalle].valor_etiqueta
         WHEN "CONVENIO"
            LET v_n_convenio = v_arr_detalle[v_indice_detalle].valor_etiqueta
         WHEN "REFERENCIA"
            LET v_n_referencia = v_arr_detalle[v_indice_detalle].valor_etiqueta
         WHEN "CLABE"
            LET v_cta_clabe = v_arr_detalle[v_indice_detalle].valor_etiqueta
         WHEN "MUNICIPIO"
            DISPLAY "g_id_municipio:",g_id_municipio
            LET v_cve_mandato_muni = g_id_municipio
            DISPLAY "v_cve_mandato_muni:",v_cve_mandato_muni 
            LET v_cve_mandato_paquete = v_cve_mandato_muni USING "&&&&&&&&&&&&&&&&"
            DISPLAY "v_cve_mandato_paquete:",v_cve_mandato_paquete 
            LET v_cve_mandato_paquete_aux = v_tpo_mandato||v_cve_mandato_paquete  USING "&&&&&&&&&&&&&&&&&&"
            DISPLAY "v_id_cat_mandato_paquete:",v_id_cat_mandato_paquete
            IF(v_tpo_mandato = 1)THEN # solo se inserta si es de tipo predial
               INSERT INTO mdt_cat_mandato_paquete(id_cat_mandato,cve_mandato)
               VALUES(v_id_cat_mandato_paquete,v_cve_mandato_paquete_aux)
               IF SQLCA.SQLCODE <> 0 THEN
                  LET v_error = TRUE
                  DISPLAY SQLCA.SQLCODE
               END IF
            END IF
      END CASE
      IF SQLCA.SQLCODE <> 0 THEN
         LET v_error = TRUE
      END IF
      
   END FOR

   # PASO 6: se insertan todos los documentos capturados
   --FOR v_indice_gpo = 1 TO v_arr_grupos.getLength()
   --   IF v_arr_grupos[v_indice].asignar = 1 THEN
   --   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
   --      --SELECT COUNT(id_imagen_docto)
   --      --AHM TMP CALL fn_inserta_docto(v_imagen_docto[v_indice].*)
   --      --AHM TMP CALL fn_transfiere_docto(v_imagen_docto[v_indice].*)
   --      IF(v_imagen_docto_general[v_indice].v_documento IS NOT NULL AND v_imagen_docto_general[v_indice].v_documento <> " ")THEN
   --         LET v_imagen_docto_general[v_indice].id_gpo_mandato = v_max_id_gpo_mandato
   --         --CALL fn_transfiere_archivo(v_imagen_docto[v_indice].*,'','') RETURNING v_imagen_docto[v_indice].nombre_imagen
   --         # recupera el identificador consecutivo
   --         SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice].id_imagen_docto
   --            FROM mdt_imagen_docto
   --         # se
   --         CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,v_imagen_docto_general[v_indice].id_imagen_docto, 'A') 
   --                RETURNING v_imagen_docto_general[v_indice].nombre_imagen
   --         IF(v_imagen_docto_general[v_indice].nombre_imagen = "NO MODIFICADO")THEN
   --            # en caso de error
   --            CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice].nombre_imagen CLIPPED,"about")
   --         ELSE
   --            # almacena registro en BD
   --            CALL fn_inserta_docto(v_imagen_docto_general[v_indice].*)
   --         END IF
   --      END IF
   --   END FOR

   --Se inserta en la tabla para notificar la alta hacia hipotecaria social
   --
   
   --Se obtiene el maximo registro para obtener el ID a insertar
   SELECT (NVL(MAX(id_mdt_notifica),0) + 1) INTO v_max_id_mdt_notifica
   FROM mdt_notifica_mandato
   --Se inserta el registro
   INSERT INTO mdt_notifica_mandato
   (id_mdt_notifica
   ,id_tpo_mandato
   ,id_cat_mandato
   ,cve_mandato
   ,tipo_operacion
   ,descripcion1
   ,descripcion2
   ,descripcion3
   ,n_cuenta_bancaria
   ,n_convenio
   ,n_referencia
   ,cta_clabe 
   ,estado
   ,resultado_operacion
   ,diagnostico
   ,f_creacion)
   VALUES 
   (
    v_max_id_mdt_notifica  --id_mdt_notifica      decimal(9,0)
   ,v_tpo_mandato          --id_tpo_mandato       smallint
   ,V_max_mdt_cat_mandato  --id_cat_mandato       smallint
   ,v_cve_mandato_paquete_aux --mdt_cat_mandato_paquete.cve_mandato, si no se genera la clave, se insertan 18 espacios 
   ,"A"                    --tipo_operacion       char(1)
   ,v_desc_larga_mandato   --descripcion1         char(40)
   ,NULL                   --descripcion2         char(40)
   ,NULL                   --descripcion3         char(40)
   ,v_n_cuenta_bancaria    --n_cuenta_bancaria    char(40)
   ,v_n_convenio           --n_convenio           char(40)
   ,v_n_referencia         --n_referencia         char(40)
   ,v_cta_clabe            --cta_clabe            char(18)
   ,100                    --estado               smallint
   ,NULL                   --resultado_operacion  char(2)
   ,NULL                   --diagnostico          char(3)
   ,TODAY                  --f_creacion           date
   )

   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "ERROR SQL(INSERT INTO mdt_notifica_mandato): ",SQLCA.SQLCODE
      --LET v_error = TRUE
   END IF

   
   
   
   RETURN v_error
   {
   IF v_error = TRUE THEN
      --Ocurrio error
      ROLLBACK WORK
      RETURN FALSE
   ELSE
      --Se insertan todos los detalles
      COMMIT WORK
      RETURN TRUE
   END IF
   }
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Inserta el registro recibido como parametro en la tabla #
#                     mdt_imagen_docto                                        #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      => 03 Marzo 2012                                           #
###############################################################################
FUNCTION fn_inserta_docto(v_documento)
DEFINE --v_documento RECORD LIKE mdt_imagen_docto.*
       v_documento      RECORD 
        id_imagen_docto LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato  LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen   LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int STRING, -- conserva la ruta seleccionada por el usuario
        v_documento     STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen     LIKE mdt_imagen_docto.desc_imagen
       END RECORD

   WHENEVER ERROR CONTINUE
   LET v_documento.nombre_imagen = os.path.basename( v_documento.nombre_imagen)
   -- AHM TMP Solo archivo LET v_documento.nombre_imagen = v_ruta_docto CLIPPED||"/"||v_documento.nombre_imagen CLIPPED

   INSERT INTO mdt_imagen_docto(id_imagen_docto,id_gpo_mandato,nombre_imagen,desc_imagen)
              VALUES(v_documento.id_imagen_docto,v_documento.id_gpo_mandato,
                     v_documento.nombre_imagen,v_documento.desc_imagen)
   
END FUNCTION                                                                                                                                                                                                                                     HPSM03.4gl                                                                                          0000777 0000212 0001751 00000064326 13113322762 012322  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-04-2012
--===============================================================

####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTM03                                        #
#Objetivo          =>Eliminacion de mandatos registrados           #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 Febrero 2012                               #
####################################################################

DATABASE safre_viv

DEFINE v_s_qryTxt        STRING
DEFINE p_v_usuario       LIKE seg_usuario.usuario,-- usuario firmado al sistema
       p_b_tipo_carga    SMALLINT,                -- tipo de carga (1 - modo en linea y 2 - modo batch)
       p_v_nom_prog      VARCHAR(30)              -- nombre del programa
DEFINE cb_mdt            ui.combobox  -- Control de combo dinamico de mandatos
DEFINE cb_tpomdt         ui.combobox  -- Control de combo dinamico de tipos de mandato
DEFINE cmbmdt            LIKE mdt_cat_mandato.id_cat_mandato
DEFINE cmbtpomdt         LIKE mdt_tpo_mandato.tpo_mandato
DEFINE v_arr_mandato     DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          etiqueta          LIKE mdt_cat_gpo_etiqueta.etiqueta,
          valor_etiqueta    LIKE mdt_cat_instancia_mandato.valor_etiqueta
       END RECORD
DEFINE v_arr_imagen      DYNAMIC ARRAY OF RECORD -- arreglo que contiene los mandatos
          nombre_imagen     CHAR(200),
          desc_imagen       CHAR(40)
       END RECORD
DEFINE r_not_mdt RECORD --LIKE mdt_notifica_mandato.*
         id_mdt_notifica   LIKE mdt_notifica_mandato.id_cat_mandato,
         id_tpo_mandato    LIKE mdt_notifica_mandato.id_tpo_mandato,
         id_cat_mandato    LIKE mdt_notifica_mandato.id_cat_mandato,
         cve_mandato       LIKE mdt_notifica_mandato.cve_mandato,
         tipo_operacion    LIKE mdt_notifica_mandato.tipo_operacion,
         descripcion1      LIKE mdt_notifica_mandato.descripcion1,
         descripcion2      LIKE mdt_notifica_mandato.descripcion2,
         descripcion3      LIKE mdt_notifica_mandato.descripcion3,
         n_cuenta_bancaria LIKE mdt_notifica_mandato.n_cuenta_bancaria,
         n_convenio        LIKE mdt_notifica_mandato.n_convenio,
         n_referencia      LIKE mdt_notifica_mandato.n_referencia,
         cta_clabe         LIKE mdt_notifica_mandato.cta_clabe,
         estado            LIKE mdt_notifica_mandato.estado,
         resultado_operacion LIKE mdt_notifica_mandato.resultado_operacion,
         diagnostico         LIKE mdt_notifica_mandato.diagnostico,
         f_creacion          LIKE mdt_notifica_mandato.f_creacion
       END RECORD
DEFINE r_mandato RECORD --LIKE mdt_cat_mandato.*
         id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato,
         cve_mandato    LIKE mdt_notifica_mandato.cve_mandato,
         desc_mandato   LIKE mdt_cat_mandato.desc_mandato,
         desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         usuario            LIKE mdt_cat_mandato.usuario,
         tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         f_creacion         LIKE mdt_cat_mandato.f_creacion,
         estado             LIKE mdt_cat_mandato.estado
       END RECORD
          
DEFINE v_gpo_mandato     LIKE mdt_gpo_mandato.id_gpo_mandato
DEFINE v_s_nota          STRING
DEFINE v_sp_estado       INTEGER,
       v_ventana         ui.Window

MAIN
DEFINE v_consulta       STRING,
       v_bnd_notifica   BOOLEAN,
       v_etiqueta       LIKE mdt_cat_gpo_etiqueta.etiqueta,
       v_valor_etiqueta LIKE mdt_cat_instancia_mandato.valor_etiqueta,
       v_error_sql      BOOLEAN
       
   -- se asignan los parametros que vienen del fglrun
   LET p_v_usuario = ARG_VAL(1)
   LET p_b_tipo_carga = ARG_VAL(2)
   LET p_v_nom_prog = ARG_VAL(3)
   LET cmbtpomdt = ARG_VAL(4)
   LET cmbmdt = ARG_VAL(5)

   --CALL ui.Interface.loadStyles("mdtstyle")

   -- se asigna el titulo del programa
   IF ( p_v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_v_nom_prog)
   END IF

   IF NOT fn_verifica_privilegios("MDTM03",p_v_usuario) THEN
      CALL fn_mensaje("Advertencia","No cuenta con privilegios para esta opci�n","error")
      EXIT PROGRAM
   END IF
   
   -- se abre la ventana de consulta
   OPEN WINDOW w_eli_mandato WITH FORM "HPSM031"

   IF(p_v_nom_prog IS NOT NULL)THEN
      CALL ui.Interface.setText(p_v_nom_prog)
      LET v_ventana = ui.Window.getCurrent()
      CALL v_ventana.setText(p_v_nom_prog)
   END IF

   -- Ajuste 20120229 AHM INPUT BY NAME cmbtpomdt, cmbmdt WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)

   DIALOG ATTRIBUTES (UNBUFFERED)

      DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.*
      END DISPLAY

      DISPLAY ARRAY v_arr_imagen TO tbl_imagen.*
         ON ACTION imagen
            -- Despliegue de imagen
            
      END DISPLAY

      BEFORE DIALOG
         DISPLAY BY NAME cmbtpomdt, cmbmdt
         -- Inicializa el combo de mandatos
         CALL inicializa_mandatos()
         
         -- Carga el arrerglo de mandatos-detalle por clave seleccionada
         CALL fn_carga_arr_mdt(cmbtpomdt, cmbmdt)
         CALL fn_carga_arr_img()
         -- Despliega arreglo de mandatos-detalle
      
      -- Ajuste 20120229 AHM ON CHANGE cmbtpomdt
      -- Ajuste 20120229 AHM    CALL init_combo_mandato(cmbtpomdt)
      -- Ajuste 20120229 AHM    CALL v_arr_mandato.clear()
      -- Ajuste 20120229 AHM    DISPLAY ARRAY v_arr_mandato TO r_atributos_mdt.* 
      -- Ajuste 20120229 AHM       BEFORE DISPLAY
      -- Ajuste 20120229 AHM          EXIT DISPLAY
      -- Ajuste 20120229 AHM    END DISPLAY
         
      ON ACTION CLOSE -- cancelar
         EXIT DIALOG
      
      ON ACTION eliminar
         IF fn_existe_instruccion_mdt(cmbmdt) THEN
            LET v_s_nota = "\nSe eliminar�n las instrucciones de mandato relacionadas a este,\ny para poderlas reactivar ser� necesario informarlo a trav�s del recurrente."
         ELSE
            LET v_s_nota = " "
         END IF
         
         -- Eliminar el mandato
         
         SELECT desc_mandato,desc_larga_mandato
           INTO r_mandato.desc_mandato,r_mandato.desc_larga_mandato
           FROM mdt_cat_mandato
          WHERE id_cat_mandato = cmbmdt
          
         IF fn_ventana_confirma("Confimar","Confirmar eliminaci�n del mandato: "||r_mandato.desc_mandato||v_s_nota,"info") = 1 THEN
         --IF fgl_winquestion("Confirmaci�n","Confirmar eliminaci�n del mandato: "||r_mandato.desc_mandato||v_s_nota,"no","yes|no", "question", 0) = 'yes' THEN
            LET v_error_sql = FALSE
            INITIALIZE r_not_mdt TO NULL
            -- insert into mdt_notifica_mandato
            SELECT NVL(MAX(id_mdt_notifica),0) + 1 INTO r_not_mdt.id_mdt_notifica
              FROM mdt_notifica_mandato
            LET r_not_mdt.descripcion1   = r_mandato.desc_larga_mandato
            LET r_not_mdt.tipo_operacion = 'B'
            LET r_not_mdt.estado         = 100

            # Recuepra todas las etiquetas asignadas al mandato
            LET v_consulta = "\n SELECT etq.etiqueta, ins.valor_etiqueta",
                             "\n   FROM mdt_cat_atributo_nivel atr JOIN mdt_cat_instancia_mandato ins",
                             "\n     ON ins.id_atr_nivel = atr.id_atr_nivel",
                             "\n        JOIN mdt_cat_gpo_etiqueta etq",
                             "\n     ON etq.id_gpo_etiqueta = atr.id_gpo_etiqueta",
                             "\n  WHERE id_cat_mandato = ?"
            PREPARE prp_rec_atributos FROM v_consulta
            DECLARE cur_rec_atributos CURSOR FOR prp_rec_atributos
            FOREACH cur_rec_atributos USING cmbmdt INTO v_etiqueta,
                                                        v_valor_etiqueta
               # Se obtienen solo los campos que nos interesa, para insertar en mdt_notifica_mandato
               CASE v_etiqueta CLIPPED
                  WHEN "NUMERO DE CUENTA"
                    LET r_not_mdt.n_cuenta_bancaria = v_valor_etiqueta
                  WHEN "CONVENIO"
                    LET r_not_mdt.n_convenio = v_valor_etiqueta
                  WHEN "REFERENCIA"
                    LET r_not_mdt.n_referencia = v_valor_etiqueta
                  WHEN "CLABE"
                    LET r_not_mdt.cta_clabe = v_valor_etiqueta
               END CASE
            END FOREACH 
            

            LET v_bnd_notifica = FALSE
            # Por cada paquete se agrega un registro en notificacion, para eliminar todos los registros de mandato 
            LET v_consulta = "\n SELECT cat.id_cat_mandato,",
                             "\n        paq.cve_mandato,",
                             "\n        cat.desc_mandato,",
                             "\n        cat.desc_larga_mandato,",
                             "\n        cat.usuario,",
                             "\n        cat.tpo_mandato,",
                             "\n        cat.f_creacion,",
                             "\n        cat.estado", 
                             "\n   FROM mdt_cat_mandato cat JOIN mdt_cat_mandato_paquete paq",
                             "\n     ON paq.id_cat_mandato = cat.id_cat_mandato",
                             "\n  WHERE cat.id_cat_mandato = ?"
            PREPARE prp_rec_mandatos FROM v_consulta 
            DECLARE cur_rec_mandatos CURSOR FOR prp_rec_mandatos
            --CALL Fn_RecMandato(cmbmdt) RETURNING r_mandato.*
            FOREACH cur_rec_mandatos USING cmbmdt INTO r_mandato.*
               # indica que si se encontr� relacion de paquete para el mandato
               LET v_bnd_notifica = TRUE
               
               # se recuperan para insertar en mdt_notifica_mandato
               LET r_not_mdt.cve_mandato    = r_mandato.cve_mandato
               LET r_not_mdt.id_tpo_mandato = r_mandato.tpo_mandato
               LET r_not_mdt.id_cat_mandato = r_mandato.id_cat_mandato
               LET r_not_mdt.f_creacion     = r_mandato.f_creacion
               
               INSERT INTO mdt_notifica_mandato
               (id_mdt_notifica,
                id_tpo_mandato,
                id_cat_mandato,
                cve_mandato,
                tipo_operacion,
                descripcion1,
                descripcion2,
                descripcion3,
                n_cuenta_bancaria,
                n_convenio,
                n_referencia,
                cta_clabe,
                estado,
                resultado_operacion,
                diagnostico,
                f_creacion)
               VALUES(
                r_not_mdt.id_mdt_notifica,
                r_not_mdt.id_tpo_mandato,
                r_not_mdt.id_cat_mandato,
                r_not_mdt.cve_mandato,
                r_not_mdt.tipo_operacion,
                r_not_mdt.descripcion1,
                r_not_mdt.descripcion2,
                r_not_mdt.descripcion3,
                r_not_mdt.n_cuenta_bancaria,
                r_not_mdt.n_convenio,
                r_not_mdt.n_referencia,
                r_not_mdt.cta_clabe,
                r_not_mdt.estado,
                r_not_mdt.resultado_operacion,
                r_not_mdt.diagnostico,
                r_not_mdt.f_creacion
               )
            END FOREACH
            # en caso de que no se encontr� relaci�n con alguna clave de paquete
            # se notfica la baja del mandato sin clave
            IF NOT(v_bnd_notifica)THEN
               LET r_not_mdt.cve_mandato = "                  "
               INSERT INTO mdt_notifica_mandato
               (id_mdt_notifica,
                id_tpo_mandato,
                id_cat_mandato,
                cve_mandato,
                tipo_operacion,
                descripcion1,
                descripcion2,
                descripcion3,
                n_cuenta_bancaria,
                n_convenio,
                n_referencia,
                cta_clabe,
                estado,
                resultado_operacion,
                diagnostico,
                f_creacion)
               VALUES(
                r_not_mdt.id_mdt_notifica,
                r_not_mdt.id_tpo_mandato,
                r_not_mdt.id_cat_mandato,
                r_not_mdt.cve_mandato,
                r_not_mdt.tipo_operacion,
                r_not_mdt.descripcion1,
                r_not_mdt.descripcion2,
                r_not_mdt.descripcion3,
                r_not_mdt.n_cuenta_bancaria,
                r_not_mdt.n_convenio,
                r_not_mdt.n_referencia,
                r_not_mdt.cta_clabe,
                r_not_mdt.estado,
                r_not_mdt.resultado_operacion,
                r_not_mdt.diagnostico,
                r_not_mdt.f_creacion
               )
            END IF
            -- update estado en mdt_cat_mandato a 106
            UPDATE mdt_cat_mandato
               SET estado = 106
             WHERE id_cat_mandato = cmbmdt
            IF(SQLCA.SQLCODE <> 0)THEN
               LET v_error_sql = TRUE
            END IF

            DISPLAY "cmbmdt:",cmbmdt

            DELETE 
              FROM mdt_cat_mandato_paquete
             WHERE id_cat_mandato = cmbmdt
            
            IF LENGTH(v_s_nota CLIPPED) > 0 THEN
              
               LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_baja_instrucciones_por_mandato(?)"
            
               # Se prepara la ejecucion del stored procedure para la baja de instrucciones de mandatos por id_cat_mandato
               PREPARE prp_baja_inst_mdt FROM v_s_qryTxt
               EXECUTE prp_baja_inst_mdt USING cmbmdt INTO v_sp_estado
               IF(SQLCA.SQLCODE <> 0)THEN
                  LET v_error_sql = TRUE
               END IF
               
               IF v_sp_estado = 1 THEN
                  DISPLAY "Se registraron bajas de instrucciones de mandatos"
               ELSE
                  DISPLAY "No se registraron bajas de instrucciones de mandatos"
               END IF
               
            END IF
            IF(v_error_sql)THEN
               CALL fn_mensaje("Aviso","Ocurri� un error al eliminar el mandato","information")
            ELSE
               CALL fn_mensaje("Aviso","Operaci�n realizada correctamente","information")
               EXIT DIALOG
            END IF
            
            CALL init_combo_mandato(cmbtpomdt)
            CALL v_arr_mandato.clear()
            CALL v_arr_imagen.clear()
            
         END IF
                  
         CONTINUE DIALOG
      
   END DIALOG
   
   CLOSE WINDOW w_eli_mandato 

END MAIN

#############################################################################
# Funcion           => fn_carga_arr_mdt - Carga detalle del mandato         #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
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
   FOREACH CurDetMandato INTO v_arr_mandato[v_i_count].*
      LET v_i_count = v_i_count + 1
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => fn_carga_arr_img - Carga imagenes del mandato        #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION fn_carga_arr_img()
DEFINE v_i_count     INTEGER
DEFINE p_gpo_mdt     LIKE mdt_imagen_docto.id_gpo_mandato
DEFINE v_ruta_docto  LIKE seg_modulo.ruta_docto

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
      
      FOREACH CurImgMandato USING p_gpo_mdt INTO v_arr_imagen[v_i_count].*
         --LET v_arr_imagen[v_i_count].nombre_imagen = "<a href='",v_ruta_docto CLIPPED,v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"' target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         LET v_arr_imagen[v_i_count].nombre_imagen = "<a gwc:attributes=\"href resourceuri('",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"','mdtdocto')\" target='nueva'>",v_arr_imagen[v_i_count].nombre_imagen CLIPPED,"</a>"
         DISPLAY v_arr_imagen[v_i_count].nombre_imagen
         LET v_i_count = v_i_count + 1
      END FOREACH
   END FOREACH
   
END FUNCTION 

#############################################################################
# Funcion           => inicializa_mandatos - Definicion e inicializacion    #
#                      de los combos dinamicos del modulo de mandatos       #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION inicializa_mandatos()
 
   -- Inicializa combo de bancos en funcion a su tabla de base de datos
   CALL init_combo_mandato(NULL)
   CALL init_combo_tipomandato()

END FUNCTION

#############################################################################
# Funcion           => init_combo_mandato - Inicializa el combo de mandatos #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_tpo_mdt - tipo de mandato para el filtro           #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
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
# Funcion           => init_combo_tipomandato - Inicializa el combo tpo mdt.#
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
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
# Funcion           => Fn_RecMandato - Recupera registro del catalogo de mdt#
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_mandato - clave del mandato a recuperar            #
# Salida:           => r_mandato - regsitro del mandato recuperado          #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 14 Febrero 2012                                      #
#############################################################################
FUNCTION Fn_RecMandato(p_mandato)
DEFINE p_mandato    LIKE mdt_cat_mandato.id_cat_mandato
DEFINE v_r_mandato  RECORD --LIKE mdt_cat_mandato.*
         id_cat_mandato     LIKE mdt_cat_mandato.id_cat_mandato,
         cve_mandato        LIKE mdt_cat_mandato_paquete.cve_mandato,
         desc_mandato       LIKE mdt_cat_mandato.desc_mandato,
         desc_larga_mandato LIKE mdt_cat_mandato.desc_larga_mandato,
         usuario            LIKE mdt_cat_mandato.usuario,
         tpo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
         f_creacion         LIKE mdt_cat_mandato.f_creacion,
         estado             LIKE mdt_cat_mandato.estado
        END RECORD 
    
   SELECT cat.id_cat_mandato,
          --paq.cve_mandato,
          cat.cve_mandato,
          cat.desc_mandato,
          cat.desc_larga_mandato,
          cat.usuario,
          cat.tpo_mandato,
          cat.f_creacion,
          cat.estado 
     INTO v_r_mandato.*
     FROM mdt_cat_mandato cat --JOIN mdt_cat_mandato_paquete paq
       --ON paq.id_cat_mandato = cat.id_cat_mandato
    WHERE cat.id_cat_mandato = p_mandato
    
   RETURN v_r_mandato.*
   
END FUNCTION

#############################################################################
# Funcion           => fn_existe_instruccion_mdt - Verifica la existencia   #
#                      de instrucciones de mandato                          #
# Propietario       => E.F.P                                                #
# Sistema           => MDT                                                  #
# Entrada:          => p_id_cat_mandato - Mandato a validar si cuenta con   #
#                      instrucciones de mandato                             #
# Salida:           => Verdadero si tiene asociadas instrucciones y falso en#
#                      caso contrario                                       #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 01 Marzo 2012                                        #
#############################################################################
FUNCTION fn_existe_instruccion_mdt(p_id_cat_mandato)
   DEFINE p_id_cat_mandato LIKE mdt_cat_mandato.id_cat_mandato
   DEFINE v_existe         INTEGER
   
   SELECT NVL(COUNT(*),0) INTO v_existe
     FROM mdt_det_ctr_mandato
    WHERE id_cat_mandato = p_id_cat_mandato       
      AND estado in (103,104)
     
   IF v_existe > 0 THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
   
END FUNCTION                                                                                                                                                                                                                                                                                                          HPSM04.4gl                                                                                          0000777 0000212 0001751 00000534617 13113322763 012331  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 13-04-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => CATALOGO DE ALTA DE MANDATOS                            #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
IMPORT os
DATABASE safre_viv

--VARIABLES GLOBALES
DEFINE v_arr_grupos DYNAMIC ARRAY OF RECORD
        asignar       BOOLEAN,
        id_cat_gpo    LIKE mdt_cat_gpo.id_cat_gpo,
        descripcion   LIKE mdt_cat_gpo.descripcion,
        v_existe      BOOLEAN
       END RECORD
---Array de los elementos del grupo
DEFINE v_arr_elementos DYNAMIC ARRAY OF RECORD
   id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
  ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
  ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
END RECORD
--Array para el detalle
DEFINE v_arr_detalle DYNAMIC ARRAY OF RECORD
   id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
  ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
  ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
  ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato
  ,orden               LIKE mdt_cat_atributo_nivel.orden
  ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
  ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
  ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
   v_operacion         CHAR(1),
   v_nombre_original   STRING
END RECORD
---Array de los elementos del grupo
DEFINE v_arr_elementos_tmp DYNAMIC ARRAY OF RECORD
   id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
  ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
  ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
END RECORD
--Array para el detalle
DEFINE v_arr_detalle_tmp DYNAMIC ARRAY OF RECORD
   id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
  ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
  ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
  ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato  
  ,orden               LIKE mdt_cat_atributo_nivel.orden
  ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
  ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
  ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
   v_operacion         CHAR(1),
   v_nombre_original   STRING
END RECORD
DEFINE v_imagen_docto     DYNAMIC ARRAY OF RECORD 
        id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen     STRING, -- LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int   STRING, -- conserva la ruta seleccionada por el usuario
        v_documento       STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen       STRING, --LIKE mdt_imagen_docto.desc_imagen,
        v_operacion       CHAR(1),  -- Operacion que se le aplicara al registro(A-alta, B-baja y C-cambio)
        v_nombre_original STRING   -- nombre con el que comenz� antes de modificar 
       END RECORD,
       v_imagen_docto_general   DYNAMIC ARRAY OF RECORD 
        id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen     STRING, -- LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int   STRING, -- conserva la ruta seleccionada por el usuario
        v_documento       STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen       STRING, --LIKE mdt_imagen_docto.desc_imagen,
        v_operacion       CHAR(1),  -- Operacion que se le aplicara al registro(A-alta, B-baja y C-cambio)
        v_nombre_original STRING   -- nombre con el que comenz� antes de modificar 
       END RECORD

DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod -- codigo del proceso
      ,g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
      ,v_desc_mandato        LIKE mdt_cat_mandato.desc_mandato
      ,v_desc_larga_mandato  LIKE mdt_cat_mandato.desc_larga_mandato
      ,v_tpo_mandato         LIKE mdt_cat_mandato.tpo_mandato  --Tipo de mandato
      ,v_tpo_mandato_desc    LIKE mdt_tpo_mandato.desc_tpo_mandato  --Tipo de mandato
      ,p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
      ,v_prompt              INTEGER,
       v_ruta_docto          LIKE seg_modulo.ruta_docto
DEFINE p_tipo_mandato        LIKE mdt_cat_mandato.tpo_mandato,
       p_id_cat_mandato      LIKE mdt_cat_mandato.id_cat_mandato,
       v_ruta_ejecutable     LIKE seg_modulo.ruta_bin,
       g_id_municipio LIKE cat_municipio.municipio

######################################################################################
# INICIA EL MAIN
######################################################################################
MAIN
DEFINE p_tipo_ejecucion      SMALLINT -- forma como ejecutara el programa
      ,p_titulo              STRING   -- titulo de la ventana
      ,v_ingreso_mandato     BOOLEAN
      ,v_id_cat_gpo_actual   LIKE mdt_cat_gpo.id_cat_gpo
      ,v_desc_mandato_tmp        LIKE mdt_cat_mandato.desc_mandato
      ,v_desc_larga_mandato_tmp  LIKE mdt_cat_mandato.desc_larga_mandato
      ,v_estatus                 INTEGER
DEFINE cb ui.ComboBox  --Tipo Combobox
DEFINE v_rec_tipo_mandato    RECORD LIKE mdt_tpo_mandato.*,
       v_direccion     RECORD # arreglo para desplegar todas las direcciones encontradas
        v_cp           LIKE cat_cp.cp,
        v_id_colonia   LIKE cat_colonia.colonia, 
        v_colonia     LIKE cat_colonia.colonia_desc,-- Falta la descripcion de colonia
        v_id_municipio LIKE cat_municipio.municipio,
        v_municipio    LIKE cat_municipio.municipio_desc,
        v_id_ciudad    LIKE cat_ciudad.ciudad,
        v_ciudad       LIKE cat_ciudad.ciudad_desc,
        v_id_entidad   LIKE cat_entidad_federativa.entidad_federativa,
        v_entidad      LIKE cat_entidad_federativa.entidad_desc_larga
       END RECORD,
       v_bnd_continua      BOOLEAN,
       v_consulta          STRING,
       v_conteo_direccion  INTEGER,
       v_indice            INTEGER,
       v_indice_bus        INTEGER,
       v_indice_aux        INTEGER,
       v_resultado         STRING,
       v_ventana           ui.Window,
       v_nombre_imagen_aux  LIKE mdt_imagen_docto.nombre_imagen -- en caso de modificacion de un documento se almacena el anterior al que se seleccione
DEFINE v_s_genera_digito_vrf_b   SMALLINT
DEFINE v_s_genera_digito_vrf_p   SMALLINT
DEFINE v_s_genera_digito_vrf_n   SMALLINT
DEFINE v_s_genera_digito_vrf_c   SMALLINT
DEFINE v_c_banco          CHAR(3)
DEFINE v_c_plaza          CHAR(3)
DEFINE v_c_cta            CHAR(11) 
DEFINE v_c_cta_clabe      CHAR(18)
DEFINE v_bnd_estatus      SMALLINT
DEFINE v_bnd_municipio    BOOLEAN

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)
   LET p_tipo_mandato   = ARG_VAL(4)
   LET p_id_cat_mandato = ARG_VAL(5)

   --CALL ui.Interface.loadStyles("mdtstyle")

   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   # Recuper la ruta de los documetos 
   
   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = "mdt"
    
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   # busca si hay una instruccion realcionada con direccion
   LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM mdt_cat_gpo_etiqueta et JOIN mdt_cat_gpo gpo",
                    "\n     ON gpo.id_cat_gpo = et.id_cat_gpo",
                    "\n  WHERE gpo.descripcion = 'DIRECCION'",
                    "\n    AND et.id_gpo_etiqueta = ?"
   PREPARE prp_encuentra_elemento_direccion FROM v_consulta
   LET v_conteo_direccion = 0
   -- Se asignan las variables de control 
   --LET g_proceso_cod = 0
   --LET g_opera_cod   = 0
   LET v_ingreso_mandato = FALSE  --Se inicia la bandera de ingreso de mandato
   LET v_desc_mandato_tmp = ""
   LET v_desc_larga_mandato_tmp = ""
   
   # Se abre la ventana de captura del nuevo mandato
   OPEN WINDOW w_mant_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/HPSM041"
      INPUT v_desc_mandato, v_desc_larga_mandato FROM desc_mandato, desc_larga_mandato ATTRIBUTES (UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE)
         BEFORE INPUT
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText(p_titulo)
            # se recuperan las descripciones actuales
            LET v_consulta = "\n SELECT FIRST 1 tpo.desc_tpo_mandato, cat.desc_mandato, cat.desc_larga_mandato",
                             "\n   FROM mdt_cat_mandato cat JOIN mdt_tpo_mandato tpo",
                             "\n     ON tpo.tpo_mandato = cat.tpo_mandato",
                             "\n  WHERE cat.tpo_mandato = ?",
                             "\n    AND cat.id_cat_mandato = ?"
            PREPARE prp_rec_descripcion_tpo_mandato FROM v_consulta
            EXECUTE prp_rec_descripcion_tpo_mandato USING p_tipo_mandato, p_id_cat_mandato
                                                    INTO  v_tpo_mandato_desc,v_desc_mandato,v_desc_larga_mandato
            # asigna el identificador de tipo de mandato
            LET v_tpo_mandato = p_tipo_mandato
            # imprime en pantalla las descripciones recuperadas
            DISPLAY v_tpo_mandato_desc   TO cb_tipo_mandato
            DISPLAY v_desc_mandato       TO desc_mandato
            DISPLAY v_desc_larga_mandato TO desc_larga_mandato
            
         ON ACTION aceptar
            # Se valida que los registros no esten vacios
            IF v_desc_mandato CLIPPED = v_desc_mandato_tmp CLIPPED  OR v_desc_mandato  IS NULL THEN
               CALL fn_mensaje(p_titulo,"Capture descripci�n del mandato","information")
               NEXT FIELD desc_mandato
               --CONTINUE INPUT
            END IF
            IF v_desc_larga_mandato CLIPPED = v_desc_larga_mandato_tmp CLIPPED OR v_desc_larga_mandato IS NULL THEN
               CALL fn_mensaje(p_titulo,"Capture descripci�n larga del mandato","information")
               NEXT FIELD desc_larga_mandato
               --CONTINUE INPUT
            ELSE
               LET v_ingreso_mandato = TRUE
               EXIT INPUT
            END IF
         ON ACTION close
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_mant_mandatos

   CALL f_inicia_arrays()
   
   IF v_ingreso_mandato = TRUE THEN
   # Se abre la ventana para la captura de los detalles y grupos del mandato
   OPEN WINDOW w_mant_mandatos_detalle WITH FORM v_ruta_ejecutable CLIPPED||"/HPSM042"
      CALL ui.Dialog.setDefaultUnbuffered(TRUE)
      DIALOG  ATTRIBUTES (UNBUFFERED , FIELD ORDER FORM)
      
         INPUT ARRAY v_arr_grupos FROM tabla_grupo.* 
         ATTRIBUTE( INSERT ROW=FALSE, APPEND ROW=FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, WITHOUT DEFAULTS)
            BEFORE INPUT
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo)
            
            BEFORE ROW
               LET v_ventana = ui.Window.getCurrent()
               CALL v_ventana.setText(p_titulo)
              
               IF v_arr_grupos[ARR_CURR()].asignar = 1 THEN 
                  LET v_id_cat_gpo_actual = v_arr_grupos[ARR_CURR()].id_cat_gpo
               ELSE
                  LET v_id_cat_gpo_actual = NULL
               END IF
               CALL f_pasa_tmp_elementos(v_id_cat_gpo_actual)
               CALL f_pasa_tmp_detalle(v_id_cat_gpo_actual) --Se muestra el detalle del primer elemento
               # selecciona solo los registros del grupo
               CALL f_pasa_tmp_archivos(v_id_cat_gpo_actual)
               
               CALL ui.Interface.refresh()

            ON CHANGE tb_asignar
               IF v_arr_grupos[ARR_CURR()].asignar = 1 THEN 
                  LET v_id_cat_gpo_actual = v_arr_grupos[ARR_CURR()].id_cat_gpo
                  # recupera las etiquetas y documentos relacionados al grupo desde BD
                  CALL f_recupera_etiqueta_docto(v_id_cat_gpo_actual)
               ELSE
                  LET v_id_cat_gpo_actual = v_arr_grupos[ARR_CURR()].id_cat_gpo
                  # Elimina los documetnos y atributos del grupo
                  CALL f_elimina_etiqueta_docto(v_id_cat_gpo_actual)
                  LET v_id_cat_gpo_actual = NULL
               END IF
               CALL f_pasa_tmp_elementos(v_id_cat_gpo_actual)
               --CALL f_pasa_tmp_detalle(v_id_cat_gpo_actual) --Se muestra el detalle del primer elemento
               # selecciona solo los registros del grupo
               CALL f_pasa_tmp_archivos(v_id_cat_gpo_actual)
               CALL ui.Interface.refresh()
         END INPUT
         
         DISPLAY ARRAY v_arr_elementos_tmp TO tabla_elemento.*
            BEFORE DISPLAY
               --Se HABILITAN los botones del eliminar y agregar al entrar al display
               IF v_arr_elementos_tmp.getLength() > 0 THEN
                  CALL DIALOG.setActionActive("agregar",TRUE)
                  CALL DIALOG.setActionActive("eliminar",TRUE)
               END IF
            AFTER DISPLAY
               --Se desabilitan los botones del eliminar y agregar para que solo se modifique cuando se esta en este display array
               CALL DIALOG.setActionActive("agregar",FALSE)
               CALL DIALOG.setActionActive("eliminar",FALSE)
         END DISPLAY
         
         INPUT ARRAY v_arr_detalle_tmp FROM v_arr_detalle_tmp.* --tabla_detalle.*
         ATTRIBUTE( INSERT ROW=FALSE, APPEND ROW=FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, WITHOUT DEFAULTS)
         --ATTRIBUTE( WITHOUT DEFAULTS)
            BEFORE INPUT
               CALL DIALOG.setActionActive("subir",TRUE)
               CALL DIALOG.setActionActive("bajar",TRUE)

               IF v_arr_detalle_tmp.getLength() <> 0 THEN
                  LET v_indice = ARR_CURR()
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "ENTIDAD FINANCIERA" AND 
                     LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                      CALL fn_busca_entidad_financiera() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
                  END IF
                  
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "PLAZA" AND 
                     LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                      CALL fn_busca_plaza() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
                  END IF
                  
                  IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                     LET v_s_genera_digito_vrf_b = 0
                     LET v_s_genera_digito_vrf_p = 0
                     LET v_s_genera_digito_vrf_n = 0
                     LET v_c_banco = '   '
                     LET v_c_plaza = '   '
                     LET v_c_cta   = '           '
                     IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                        FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                              CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                              LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                           END IF
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                              CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                              LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                           END IF
                           IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                                 LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                                 LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                              END IF
                           END IF
                        END FOR
                        IF v_s_genera_digito_vrf_b = 1 AND
                           v_s_genera_digito_vrf_p = 1 AND
                           v_s_genera_digito_vrf_n = 1 THEN
                           -- Genera digito verificador
                           CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                           IF v_bnd_estatus = 1 THEN
                              CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                           END IF
                        ELSE
                           IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                              IF v_s_genera_digito_vrf_b = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                    IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                       LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                       LET v_c_banco = '   '
                                    END IF
                                 END IF
                              END IF
                              IF v_s_genera_digito_vrf_p = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                    IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                       LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                       LET v_c_plaza = '   '
                                    END IF
                                 END IF
                              END IF
                              IF v_s_genera_digito_vrf_n = 0 THEN
                                 LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                              END IF
                              LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                           END IF
                        END IF
                     ELSE
                        CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                                  v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                                  1)
                             RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                        END IF
                     END IF
                  END IF
               END IF
                              # busca si hay una instruccion realcionada con direccion
               FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                  LET v_conteo_direccion = 0
                  EXECUTE prp_encuentra_elemento_direccion USING v_arr_detalle_tmp[v_indice].id_gpo_etiqueta
                                                           INTO  v_conteo_direccion
                  IF(v_conteo_direccion > 0)THEN
                     # Habilita boton para busqueda de direccion 
                     CALL DIALOG.setActionActive("direccion",TRUE)
                     # si ya existe el registro en BD, no se desplega de imediato la ventana de consulta de direccion
                     IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) > 0 THEN
                        CONTINUE FOR
                     END IF
                     # funcuio
                     CALL fn_busca_direccion_postal()RETURNING v_direccion.*,v_bnd_continua
                     IF(v_bnd_continua)THEN
                        FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                           CASE v_arr_detalle_tmp[v_indice].etiqueta CLIPPED
                              WHEN 'ENTIDAD FEDERATIVA'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_entidad                                 
                              WHEN 'CIUDAD'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_ciudad
                              WHEN 'MUNICIPIO'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_municipio
                                 # variable que se inserta como paquete en mdt_cat_mandato_paquete
                                 LET g_id_municipio = v_direccion.v_id_municipio                                 
                              WHEN 'CODIGO POSTAL'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_cp                                 
                              WHEN 'COLONIA'
                                 LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_colonia
                           END CASE
                        END FOR
                     END IF
                     --FOR v_indice = 1 TO v_imagen_docto.getLength()
                     --   DISPLAY "RUTA: "||v_imagen_docto[v_indice].nombre_imagen
                     --END FOR
                     
                     EXIT FOR
                  END IF
               END FOR
               LET v_bnd_continua = FALSE

            ON CHANGE valor_etiqueta
               CALL GET_FLDBUF(valor_etiqueta) RETURNING v_arr_detalle_tmp[ARR_CURR()].valor_etiqueta
               IF(v_arr_detalle_tmp[ARR_CURR()].v_operacion IS NULL)THEN
                  LET v_arr_detalle_tmp[ARR_CURR()].v_operacion = 'M'
                  FOR v_indice = 1 TO v_arr_detalle.getLength()
                     IF(v_arr_detalle[v_indice].id_gpo_etiqueta = v_arr_detalle_tmp[ARR_CURR()].id_gpo_etiqueta AND
                        v_arr_detalle[v_indice].id_cat_gpo      = v_arr_detalle_tmp[ARR_CURR()].id_cat_gpo AND 
                        v_arr_detalle[v_indice].etiqueta        = v_arr_detalle_tmp[ARR_CURR()].etiqueta)THEN

                        LET v_arr_detalle[v_indice].v_operacion = 'M'
                        LET v_arr_detalle[v_indice].valor_etiqueta = v_arr_detalle_tmp[ARR_CURR()].valor_etiqueta
                        EXIT FOR
                     END IF
                  END FOR
               END IF 
               CALL fn_pasa_tmp_general_detalle()
               
               
            AFTER INPUT
               CALL DIALOG.setActionActive("subir",FALSE)
               CALL DIALOG.setActionActive("bajar",FALSE)
               CALL DIALOG.setActionActive("bajar",FALSE)
               CALL DIALOG.setActionActive("direccion",FALSE)
            BEFORE ROW   
               LET v_indice = ARR_CURR()
               IF v_arr_detalle_tmp[v_indice].etiqueta = "ENTIDAD FINANCIERA" AND 
                  LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                   CALL fn_busca_entidad_financiera() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
               END IF

               IF v_arr_detalle_tmp[v_indice].etiqueta = "PLAZA" AND 
                  LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) = 0 THEN
                   CALL fn_busca_plaza() RETURNING v_arr_detalle_tmp[v_indice].valor_etiqueta
               END IF

               IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                  LET v_s_genera_digito_vrf_b = 0
                  LET v_s_genera_digito_vrf_p = 0
                  LET v_s_genera_digito_vrf_n = 0
                  LET v_c_banco = '   '
                  LET v_c_plaza = '   '
                  LET v_c_cta   = '           '
                  IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                     FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                           CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                           LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                           CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                           LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                           IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                              LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                           END IF
                        END IF
                     END FOR
                     IF v_s_genera_digito_vrf_b = 1 AND
                        v_s_genera_digito_vrf_p = 1 AND
                        v_s_genera_digito_vrf_n = 1 THEN
                        -- Genera digito verificador
                        CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                    LET v_c_plaza = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_n = 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                           END IF
                           LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                        END IF
                     END IF
                  ELSE
                     CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                               1)
                          RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                     IF v_bnd_estatus = 1 THEN
                        CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     END IF
                  END IF
               END IF
               
            AFTER ROW
               LET v_indice = ARR_CURR()
               IF v_arr_detalle_tmp[v_indice].etiqueta = "CUENTA CLABE" THEN
                  LET v_s_genera_digito_vrf_b = 0
                  LET v_s_genera_digito_vrf_p = 0
                  LET v_s_genera_digito_vrf_n = 0
                  LET v_c_banco = '   '
                  LET v_c_plaza = '   '
                  LET v_c_cta   = '           '
                  IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                     FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                           CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                           LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                           CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                           LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                        END IF
                        IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                           IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                              LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                           END IF
                        END IF
                     END FOR
                     IF v_s_genera_digito_vrf_b = 1 AND
                        v_s_genera_digito_vrf_p = 1 AND
                        v_s_genera_digito_vrf_n = 1 THEN
                        -- Genera digito verificador
                        CALL fn_genera_digito_vrf(v_c_banco,v_c_plaza,v_c_cta,'',0) RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                        IF v_bnd_estatus = 1 THEN
                           CALL fn_mensaje("Aviso","Clabe inv�lida, recaptura para su validaci�n","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                                    LET v_c_plaza = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_n = 0 THEN
                              LET v_c_cta = v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] USING "&&&&&&&&&&&"
                           END IF
                           LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_c_banco,v_c_plaza,v_c_cta
                        END IF
                     END IF
                  ELSE
                     CALL fn_genera_digito_vrf(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]  ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[7,17] ,
                                               v_arr_detalle_tmp[v_indice].valor_etiqueta[18,18],
                                               1)
                          RETURNING v_bnd_estatus, v_arr_detalle_tmp[v_indice].valor_etiqueta
                     IF v_bnd_estatus = 1 THEN
                        CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     END IF
                  END IF
               END IF
               
               --CALL fn_pasa_tmp_general_detalle()
            {ON CHANGE tb_valor_etiqueta
               CALL fn_pasa_tmp_general_detalle()}
            
         END INPUT

         #######################################################################
         #---------------------- Arreglo de documentos ------------------------#
         #######################################################################
         INPUT ARRAY v_imagen_docto FROM sr_imagenes.* --tabla_detalle.*
                                    ATTRIBUTE( APPEND ROW=FALSE, INSERT ROW=FALSE, 
                                              DELETE ROW = FALSE, AUTO APPEND = FALSE, 
                                              WITHOUT DEFAULTS)
         --ATTRIBUTE( WITHOUT DEFAULTS)
            BEFORE INPUT
               CALL DIALOG.setActionActive("btn_img_doc",TRUE)
               CALL DIALOG.setActionActive("btn_img_quitar",TRUE)
               
               
            AFTER INPUT
               CALL DIALOG.setActionActive("btn_img_doc",FALSE)
               CALL DIALOG.setActionActive("btn_img_quitar",FALSE)
               # Se checa si la descripcion actual ha cambiado
               --LET v_imagen_docto[ARR_CURR()].desc_imagen = GET_FLDBUF(tedi_descripcion) CLIPPED 
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado 
               --FOR v_indice = 1 TO v_imagen_docto_general.getLength()
               --   IF(v_imagen_docto_general[v_indice].id_imagen_docto = v_imagen_docto[ARR_CURR()].id_imagen_docto AND
               --      v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo AND
               --      v_imagen_docto_general[v_indice].id_gpo_mandato  = v_imagen_docto[ARR_CURR()].id_gpo_mandato AND 
               --      v_imagen_docto_general[v_indice].desc_imagen  <> v_imagen_docto[ARR_CURR()].desc_imagen)THEN
                       
               --      LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[ARR_CURR()].desc_imagen
                     #Si es un registro nuevo no se le modifica la operacion
               --      IF(v_imagen_docto[ARR_CURR()].v_operacion <> 'A')THEN
               --         LET v_imagen_docto_general[v_indice].v_operacion = 'M'
               --         LET v_imagen_docto[ARR_CURR()].v_operacion       = 'M'
               --      END IF
               --      EXIT FOR
                        
               --   END IF
               --END FOR 

            BEFORE FIELD tedi_rutam
               INITIALIZE v_nombre_imagen_aux TO NULL
               # Recupera el nombre del documento antes de modificar el existente
               CALL GET_FLDBUF(tedi_documento_oculto) RETURNING v_nombre_imagen_aux
               
            ON CHANGE tedi_rutam
               CALL GET_FLDBUF(tedi_rutam) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               --CALL GET_FLDBUF(tedi_documento) RETURNING v_imagen_docto[ARR_CURR()].v_documento
               # se consideran 10 caracteres para identificadores para un total de 100
               IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) < 90)THEN
                  IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) > 0 )THEN
                     --CALL fn_mensaje("0",v_imagen_docto[ARR_CURR()].v_operacion,"")
                     DISPLAY "CHANGE iamgen - ",v_imagen_docto[ARR_CURR()].nombre_imagen
                     DISPLAY "Operacion - ",v_imagen_docto[ARR_CURR()].v_operacion
                     CASE 
                        WHEN v_imagen_docto[ARR_CURR()].v_operacion = 'A' AND  # se esta agregando un documento nuevo por primera vez (en modificacion)
                             v_imagen_docto[ARR_CURR()].v_documento IS NULL
                           CALL fn_transfiere_archivo(v_imagen_docto[ARR_CURR()].*) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
                           LET v_imagen_docto[ARR_CURR()].v_documento = v_imagen_docto[ARR_CURR()].nombre_imagen
                           LET v_imagen_docto[ARR_CURR()].v_documento_int   = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                           CALL v_imagen_docto_general.appendElement()
                           LET v_imagen_docto_general[v_imagen_docto_general.getLength()].* = v_imagen_docto[ARR_CURR()].* 

                        WHEN v_imagen_docto[ARR_CURR()].v_operacion = 'A' AND  # se esta modificando un registro nuevo
                             v_imagen_docto[ARR_CURR()].v_documento IS NOT NULL
                           # elimina el registro en caso de que se seleccion� otro archivo
                           DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 1"
                           CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B') 
                                    RETURNING v_resultado
                           IF(v_resultado = "NO ELIMINADO")THEN
                              CALL fn_mensaje("Aviso","Archivo NO REEMPLAZADO","about")
                           ELSE
                              CALL fn_transfiere_archivo(v_imagen_docto[ARR_CURR()].*) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto[ARR_CURR()].v_documento = v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto[ARR_CURR()].v_documento_int   = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                              LET v_imagen_docto_general[v_imagen_docto_general.getLength()].* = v_imagen_docto[ARR_CURR()].* 
                           END IF
                        
                        WHEN v_imagen_docto[ARR_CURR()].v_operacion IS NULL # se esta modificando un registro que esta en BD
                           LET v_imagen_docto[ARR_CURR()].v_documento     = v_imagen_docto[ARR_CURR()].nombre_imagen
                           LET v_imagen_docto[ARR_CURR()].v_documento_int = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                           LET v_imagen_docto[ARR_CURR()].v_operacion     = 'M'
 
                        WHEN v_imagen_docto[ARR_CURR()].v_operacion = 'M' AND # se esta modificacndo un registro que esta en BD por seguna vez o m�s
                             v_imagen_docto[ARR_CURR()].v_nombre_original <> v_imagen_docto[ARR_CURR()].v_documento
                           # elimina el registro temporal
                           DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 2"
                           CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B') 
                                    RETURNING v_resultado
                           IF(v_resultado = "NO ELIMINADO")THEN
                              CALL fn_mensaje("Aviso","Archivo NO REEMPLAZADO","about")
                           ELSE
                              LET v_imagen_docto[ARR_CURR()].v_documento = v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto[ARR_CURR()].v_documento_int   = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                           END IF
                     END CASE
                     # Se excluye solo los registros que son nuevos y se esta modificacndo por primera vez
                     IF(v_imagen_docto[ARR_CURR()].v_nombre_original <> v_imagen_docto[ARR_CURR()].v_documento AND (v_imagen_docto[ARR_CURR()].v_operacion <> 'A' OR v_imagen_docto[ARR_CURR()].v_documento IS NOT NULL))THEN
                        FOR v_indice = 1 TO v_imagen_docto_general.getLength()
                           IF(v_imagen_docto_general[v_indice].id_imagen_docto = v_imagen_docto[ARR_CURR()].id_imagen_docto AND
                              v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo AND
                              v_imagen_docto_general[v_indice].id_gpo_mandato  = v_imagen_docto[ARR_CURR()].id_gpo_mandato)THEN
   
                              CALL fn_transfiere_archivo(v_imagen_docto[ARR_CURR()].*) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto[ARR_CURR()].v_documento           = v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto[ARR_CURR()].v_documento_int = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[ARR_CURR()].v_documento CLIPPED,"</a>"
                              LET v_imagen_docto_general[v_indice].v_documento     = v_imagen_docto[ARR_CURR()].v_documento
                              LET v_imagen_docto_general[v_indice].v_documento_int = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto_general[ARR_CURR()].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto_general[ARR_CURR()].v_documento CLIPPED,"</a>"
                              LET v_imagen_docto_general[v_indice].nombre_imagen   = v_imagen_docto[ARR_CURR()].nombre_imagen
                              LET v_imagen_docto_general[v_indice].v_operacion     = v_imagen_docto[ARR_CURR()].v_operacion
                              EXIT FOR
                           END IF
                        END FOR
                     END IF 
                  
                  END IF
               ELSE
                  # validaci�n para el tama�o del archivo
                  CALL fn_mensaje("Aviso","Tama�o del nombre de archivo excede el m�ximo permitido","information")
               END IF
               --NEXT FIELD tedi_descripcion

            AFTER FIELD tedi_rutam
               CALL GET_FLDBUF(tedi_rutam) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               # se inicializan a null los campos en caso de que el archvo exceda el tama�o permitido
               # se consideran 10 caracteres para identificadores para un total de 100
               IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) > 90)THEN
                  INITIALIZE v_imagen_docto[ARR_CURR()].nombre_imagen   TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento     TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].desc_imagen     TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento_int TO NULL
               END IF
               --CALL fn_transfiere_archivo(v_imagen_docto[ARR_CURR()].*) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               --CALL fn_mensaje("original",v_imagen_docto[ARR_CURR()].v_nombre_original,"")
               --CALL fn_mensaje("actual",v_imagen_docto[ARR_CURR()].v_documento,"")
               --CALL fn_mensaje("operacion",v_imagen_docto[ARR_CURR()].v_operacion,"")
            

            ON CHANGE tedi_descripcion
               --CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[ARR_CURR()].desc_imagen
               LET v_imagen_docto[ARR_CURR()].desc_imagen = GET_FLDBUF(tedi_descripcion) CLIPPED 
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado 
               FOR v_indice = 1 TO v_imagen_docto_general.getLength()
                  --IF(v_imagen_docto_general[v_indice].id_imagen_docto = v_imagen_docto[ARR_CURR()].id_imagen_docto AND
                  IF(v_imagen_docto_general[v_indice].v_documento = v_imagen_docto[ARR_CURR()].v_documento AND
                     v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[ARR_CURR()].id_cat_gpo)THEN-- AND
                     --v_imagen_docto_general[v_indice].id_gpo_mandato  = v_imagen_docto[ARR_CURR()].id_gpo_mandato)THEN
                       
                     LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[ARR_CURR()].desc_imagen
                     #Si es un registro nuevo no se le modifica la operacion
                     IF(v_imagen_docto[ARR_CURR()].v_operacion <> 'A')THEN
                        LET v_imagen_docto_general[v_indice].v_operacion = 'M'
                        LET v_imagen_docto[ARR_CURR()].v_operacion       = 'M'
                     END IF
                     EXIT FOR
                        
                  END IF
               END FOR 
               
            AFTER FIELD tedi_descripcion
               --CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[ARR_CURR()].desc_imagen
               LET v_imagen_docto[ARR_CURR()].desc_imagen = GET_FLDBUF(tedi_descripcion) CLIPPED

         END INPUT
         
         BEFORE DIALOG
            --Se desabilitan los botones del eliminar y agregar
            CALL DIALOG.setActionActive("agregar",FALSE)
            CALL DIALOG.setActionActive("eliminar",FALSE)
            --Se desabilitan los botones de subir y bajar
            CALL DIALOG.setActionActive("direccion",FALSE)
            CALL DIALOG.setActionActive("subir",FALSE)
            CALL DIALOG.setActionActive("bajar",FALSE)
            # Se desabilitan los botones de ver, documento y quitar
            CALL DIALOG.setActionActive("btn_img_doc",FALSE)
            CALL DIALOG.setActionActive("btn_img_quitar",FALSE)

            
         ON ACTION Agregar
            IF v_arr_elementos_tmp.getLength() > 0 THEN
               {DISPLAY "v_arr_elementos_tmp.getLength():",v_arr_elementos_tmp.getLength()
               DISPLAY "v_arr_elementos_tmp[ARR_CURR()].*:",v_arr_elementos_tmp[ARR_CURR()].*}
               --CALL FGL_DIALOG_GETBUFFERSTART( ) RETURNING v_estatus
               CALL fn_agegar_eliminar_detalle(v_arr_elementos_tmp[ARR_CURR()].*,"agregar")
               --DISPLAY v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* TO tabla_detalle[v_arr_detalle_tmp.getLength()].*
               {CALL ui.Interface.refresh()
               DISPLAY "**************************"
               DISPLAY "v_arr_detalle_tmp: ",v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].*
               CALL ui.Interface.refresh()
               CALL SET_COUNT(2)
               DISPLAY ARRAY v_arr_detalle_tmp TO v_arr_detalle_tmp.*
               --DISPLAY BY NAME v_arr_detalle_tmp
                  BEFORE DISPLAY 
                     EXIT DISPLAY
               END DISPLAY}
               CALL ui.Interface.refresh()
            END IF
         ON ACTION Eliminar
            IF v_arr_elementos_tmp.getLength() > 0 THEN
               {DISPLAY "v_arr_elementos_tmp.getLength():",v_arr_elementos_tmp.getLength()
               DISPLAY "v_arr_elementos_tmp[ARR_CURR()].*:",v_arr_elementos_tmp[ARR_CURR()].*}
               CALL fn_agegar_eliminar_detalle(v_arr_elementos_tmp[ARR_CURR()].*,"eliminar")
               CALL ui.Interface.refresh()
            END IF

         ON ACTION direccion
            CALL fn_busca_direccion_postal()RETURNING v_direccion.*,v_bnd_continua
            IF(v_bnd_continua)THEN
               FOR v_indice = 1 TO v_arr_detalle_tmp.getLength()
                  CASE v_arr_detalle_tmp[v_indice].etiqueta CLIPPED
                     WHEN 'ENTIDAD FEDERATIVA'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_entidad
                     WHEN 'CIUDAD'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_ciudad
                     WHEN 'MUNICIPIO'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_municipio
                        # variable que se inserta como paquete en mdt_cat_mandato_paquete
                        LET g_id_municipio = v_direccion.v_id_municipio
                     WHEN 'CODIGO POSTAL'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_cp
                     WHEN 'COLONIA'
                        LET v_arr_detalle_tmp[v_indice].valor_etiqueta = v_direccion.v_colonia
                  END CASE
               END FOR
            END IF
         
         ON ACTION subir
            CALL fn_pasa_tmp_general_detalle()
            CALL fn_ordena_posicion_detalle(ARR_CURR(),"subir") RETURNING v_prompt
            CALL FGL_SET_ARR_CURR( v_prompt )
            
         ON ACTION bajar
            CALL fn_pasa_tmp_general_detalle()
            CALL fn_ordena_posicion_detalle(ARR_CURR(),"bajar") RETURNING v_prompt
            CALL FGL_SET_ARR_CURR( v_prompt )
            
         ON ACTION ACCEPT
         
            # Se checa si la descripcion de la imagen actual ha cambiado
            LET v_indice_aux = 0
            LET v_bnd_municipio = FALSE 
            LET v_indice_aux = DIALOG.getCurrentRow("sr_imagenes")
            IF(v_indice_aux > 0)THEN
               LET v_imagen_docto[v_indice_aux].desc_imagen = GET_FLDBUF(tedi_descripcion) CLIPPED
              
               # Busca la descripcion del arreglo general y lo actualiza con la descripcion que se ha modificado 
               FOR v_indice = 1 TO v_imagen_docto_general.getLength()
               
                  --IF(v_imagen_docto_general[v_indice].id_imagen_docto = v_imagen_docto[v_indice_aux].id_imagen_docto AND
                  IF(v_imagen_docto_general[v_indice].v_documento = v_imagen_docto[v_indice_aux].v_documento AND
                     v_imagen_docto_general[v_indice].id_cat_gpo      = v_imagen_docto[v_indice_aux].id_cat_gpo)THEN-- AND
                     --v_imagen_docto_general[v_indice].id_gpo_mandato  = v_imagen_docto[v_indice_aux].id_gpo_mandato)THEN
                       
                     LET v_imagen_docto_general[v_indice].desc_imagen = v_imagen_docto[v_indice_aux].desc_imagen
                        #Si es un registro nuevo no se le modifica la operacion
                     IF(v_imagen_docto[v_indice_aux].v_operacion <> 'A')THEN
                        LET v_imagen_docto_general[v_indice].v_operacion = 'M'
                        LET v_imagen_docto[v_indice_aux].v_operacion       = 'M'
                     END IF
                     EXIT FOR
                  END IF
               END FOR
            END IF 
               
            --Se valida si es posible guardar los registros
            CALL fn_pasa_tmp_general_detalle()
            IF NOT f_valida_captura_cat() THEN
               --Si regresa falso la validaci�n se indica que debe revisar los datos
               CALL fn_mensaje("Aviso","Campo sin valor asignado, Verificar Captura","error")
            ELSE
               -- Valizaci�n de entidades financieras
               LET v_s_genera_digito_vrf_b = 0
               LET v_s_genera_digito_vrf_p = 0
               LET v_s_genera_digito_vrf_n = 0
               LET v_s_genera_digito_vrf_c = 0
               LET v_c_banco = '   '
               LET v_c_plaza = '   '
               LET v_c_cta   = '           '
               FOR v_indice_bus = 1 TO v_arr_detalle_tmp.getLength()
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "ENTIDAD FINANCIERA" THEN
                     CALL fn_recupera_entidad_financiera(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_banco
                     LET v_s_genera_digito_vrf_b = v_s_genera_digito_vrf_b + 1
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "PLAZA" THEN
                     CALL fn_recupera_plaza(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta) RETURNING v_c_plaza
                     LET v_s_genera_digito_vrf_p = v_s_genera_digito_vrf_p + 1
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "NUMERO DE CUENTA" THEN
                     IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                        LET v_c_cta = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta USING "&&&&&&&&&&&"
                        LET v_s_genera_digito_vrf_n = v_s_genera_digito_vrf_n + 1
                     END IF
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "CUENTA CLABE" THEN
                     IF LENGTH(v_arr_detalle_tmp[v_indice_bus].valor_etiqueta CLIPPED) > 0 THEN
                        LET v_c_cta_clabe = v_arr_detalle_tmp[v_indice_bus].valor_etiqueta
                        LET v_s_genera_digito_vrf_c = v_s_genera_digito_vrf_c + 1
                     END IF
                  END IF
                  IF v_arr_detalle_tmp[v_indice_bus].etiqueta = "MUNICIPIO" THEN
                     LET v_bnd_municipio = TRUE
                  END IF
                  
               END FOR
               IF v_s_genera_digito_vrf_b = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[1,3] <> v_c_banco THEN
                     CALL fn_mensaje("Aviso","La clave de banco no corresponde entre\nla cuenta CLABE y la entidad financiera","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_b = 1 OR v_s_genera_digito_vrf_c = 1 THEN
                  IF v_s_genera_digito_vrf_c = 1 THEN
                     LET v_c_banco = v_c_cta_clabe[1,3]
                  END IF
                  IF NOT fn_existe_entidad_financiera(v_c_banco) THEN
                     CALL fn_mensaje("Aviso","La clave de banco es inv�lida dentro de la CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_p = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[4,6] <> v_c_plaza THEN
                     CALL fn_mensaje("Aviso","La clave de plaza no corresponde con la cuenta CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_p = 1 OR v_s_genera_digito_vrf_c = 1 THEN
                  IF v_s_genera_digito_vrf_c = 1 THEN
                     LET v_c_plaza = v_c_cta_clabe[4,6]
                  END IF
                  IF NOT fn_existe_plaza(v_c_plaza) THEN
                     CALL fn_mensaje("Aviso","La clave de plaza es inv�lida dentro de la CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_n = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[7,17] <> v_c_cta THEN
                     CALL fn_mensaje("Aviso","La n�mero de cuenta no corresponde con la cuenta CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF    
               IF v_s_genera_digito_vrf_c = 1 THEN
                  CALL fn_genera_digito_vrf(v_c_cta_clabe[1,3]  ,
                                            v_c_cta_clabe[4,6]  ,
                                            v_c_cta_clabe[7,17] ,
                                            v_c_cta_clabe[18,18],
                                            1)
                       RETURNING v_bnd_estatus, v_c_cta_clabe
                  IF v_bnd_estatus = 1 THEN
                     CALL fn_mensaje("Aviso","La CLABE es inv�lida, favor de verificar","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               
               # Valida que se haya captura la etiqueta municipio para el tipo mandato predial
               # p_tipo_mandato = "01"    - prediales
               # v_bnd_municipio = FALSE  - no se ha capturado municipio para construir clave mandato (paquete)
               --IF(p_tipo_mandato = "01" AND v_bnd_municipio = FALSE)THEN
               IF(v_bnd_municipio = FALSE)THEN
                  CALL fn_mensaje("Aviso","Capture municipio","error")
                  CONTINUE DIALOG
               END IF
               
               IF fn_ventana_confirma("Confimar","�Desea guardar el registro?","info") = 1 THEN
                  --Si confimo se sale del dialog y se marca la badera para guardar los registros
                  IF fn_registra_detalle_modificaciones() = FALSE THEN
                     CALL fn_mensaje("Aviso","Mandato "|| v_desc_mandato CLIPPED||" modificado","error")
                     EXIT DIALOG
                  ELSE
                     CALL fn_mensaje("Error","Ocurrio un error al almacenar los registros","error")
                     CONTINUE DIALOG
                  END IF
               ELSE
                  CONTINUE DIALOG
               END IF
            END IF

            
         ON ACTION btn_img_doc
            LET v_indice = 1
            # recupera el numero de renglon seleccionado del arreglo de documentos
            LET v_indice = DIALOG.getCurrentRow("sr_imagenes")
            IF(v_indice IS NULL OR v_indice = 0)THEN
               # no hay alg�n registro y se inserta uno
               CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
               LET v_indice = DIALOG.getCurrentRow("tabla_grupo")
               LET v_imagen_docto[v_imagen_docto.getLength()].id_cat_gpo =  v_arr_grupos[v_indice].id_cat_gpo
               LET v_imagen_docto[v_imagen_docto.getLength()].v_operacion = 'A'
               # Recuepra id_gpo_mandato desde base de datos con el grupo seleccionado y el mandato que se recibi� como par�metro 
               SELECT id_gpo_mandato
                 INTO v_imagen_docto[v_imagen_docto.getLength()].id_gpo_mandato
                 FROM mdt_gpo_mandato
                WHERE id_cat_gpo =  v_arr_grupos[v_indice].id_cat_gpo
                  AND id_cat_mandato = p_id_cat_mandato
               
            ELSE
               # se recuperan los valores actuales
               CALL GET_FLDBUF(tedi_rutam) RETURNING v_imagen_docto[v_indice].nombre_imagen
               CALL GET_FLDBUF(tedi_descripcion) RETURNING v_imagen_docto[v_indice].desc_imagen
               # Se indica que los campos han cambiado
               CALL DIALOG.setFieldTouched("tedi_rutam", TRUE)
               CALL DIALOG.setFieldTouched("tedi_descripcion", TRUE)
               --CALL v_imagen_docto.appendElement()
               # agrega un nuevo regitro al final del arreglo 
               CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
               # en el caso de que necesote pasar por si solo al nuevo renglon
               LET v_indice = DIALOG.getCurrentRow("tabla_grupo")
               LET v_imagen_docto[v_imagen_docto.getLength()].id_cat_gpo =  v_arr_grupos[v_indice].id_cat_gpo
               LET v_imagen_docto[v_imagen_docto.getLength()].v_operacion = 'A'
               # Recuepra id_gpo_mandato desde base de datos con el grupo seleccionado y el mandato que se recibi� como par�metro 
               SELECT id_gpo_mandato
                 INTO v_imagen_docto[v_imagen_docto.getLength()].id_gpo_mandato
                 FROM mdt_gpo_mandato
                WHERE id_cat_gpo =  v_arr_grupos[v_indice].id_cat_gpo
                  AND id_cat_mandato = p_id_cat_mandato
            END IF
            
         ON ACTION btn_img_quitar
            IF(v_imagen_docto[ARR_CURR()].v_nombre_original IS NULL)THEN
               # elimina fisicamente el archivo
               DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 3"
               CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B')
                      RETURNING v_resultado
               IF(v_resultado <> "ELIMINADO")THEN
                  # si no se pudo elminiar
                  CALL fn_mensaje("Aviso","Registro "||v_resultado CLIPPED,"about")
               ELSE
                  # Elimina de los arreglos( temporal y general)
                  CALL fn_elimina_documento(v_imagen_docto[ARR_CURR()].*, 0) # 0 = no existe en BD
                  CALL v_imagen_docto.deleteElement(ARR_CURR())
               END IF
            ELSE
               IF(v_imagen_docto[ARR_CURR()].v_operacion = 'M')THEN
                  DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 4"
                  CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B')
                         RETURNING v_resultado
                  IF(v_resultado <> "ELIMINADO")THEN
                     # si no se pudo elminiar
                     CALL fn_mensaje("Aviso","Registro "||v_resultado CLIPPED,"about")
                  ELSE
                     
                     CALL fn_elimina_documento(v_imagen_docto[ARR_CURR()].*, 1) # 1 = existe en BD
                     CALL v_imagen_docto.deleteElement(ARR_CURR())
                  END IF
               ELSE
                  CALL fn_elimina_documento(v_imagen_docto[ARR_CURR()].*, 1) # 1 = existe en BD
                  CALL v_imagen_docto.deleteElement(ARR_CURR())
               END IF
            END IF
            
         ON ACTION CLOSE -- cancelar
            IF fn_ventana_confirma("Confimar","�Desea salir y cancelar la captura?","info") = 1 THEN
               # Se eliminan los archivos que han sido transferidos
               IF(v_imagen_docto_general.getLength() > 0)THEN
                  CALL fn_cancela_imagen()RETURNING v_resultado
                  IF(v_resultado)THEN
                     # si no se pudo elminiar
                     CALL fn_mensaje("Aviso","Ocurrio un error al limpiar los temporales","about")
                     DISPLAY "Error al limpiar los temporales"   
                  END IF
               END IF
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
            
         AFTER DIALOG
            
      END DIALOG
   CLOSE WINDOW w_mant_mandatos_detalle
   END IF
END MAIN

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Carga los arreglos con los registros recuperados para   #
#                     el id_cat_mandato recibido como par�metro               #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      => 07/03/2012                                              #
###############################################################################
FUNCTION f_inicia_arrays()
DEFINE v_indice       INTEGER,
       v_indice_docto INTEGER,
       v_consulta     STRING,
       v_c_nombre_imagen   LIKE mdt_imagen_docto.nombre_imagen,
       v_desc_imagen       LIKE mdt_imagen_docto.desc_imagen

   WHENEVER ERROR CONTINUE
   # Se ingresan los registros de los grupos
   LET v_consulta = "\n SELECT cat.id_cat_gpo, cat.descripcion,", 
                    "\n        NVL(CASE ",
                    "\n              WHEN mto.id_cat_gpo IS NULL THEN 0",
                    "\n              WHEN mto.id_cat_gpo IS NOT NULL THEN 1",
                    "\n            END,0)",
                    "\n   FROM mdt_cat_gpo cat LEFT OUTER JOIN mdt_gpo_mandato mto",
                    "\n     ON mto.id_cat_mandato = ?",
                    "\n    AND cat.id_cat_gpo = mto.id_cat_gpo"
                    --"\n  WHERE mto.id_cat_mandato = ?"
                    
   PREPARE prp_rec_grupos_mandatos FROM v_consulta
   DECLARE cur_rec_grupos_mandatos CURSOR FOR prp_rec_grupos_mandatos

   # Recupera los documentos del grupo
   LET v_consulta = "\n SELECT doc.id_imagen_docto, doc.id_gpo_mandato, doc.nombre_imagen, doc.desc_imagen",
                    "\n   FROM mdt_imagen_docto doc JOIN mdt_gpo_mandato gpo",
                    "\n     ON doc.id_gpo_mandato = gpo.id_gpo_mandato",
                    "\n  WHERE gpo.id_cat_mandato = ?",
                    "\n    AND gpo.id_cat_gpo = ?"
   PREPARE prp_rec_docto_grupo FROM v_consulta
   DECLARE cur_rec_docto_grupo CURSOR FOR prp_rec_docto_grupo

    
   
   LET v_indice = 1
   LET v_indice_docto = 1
   # se recuperan todos los grupos y los que estan realcionado al mandato
   FOREACH cur_rec_grupos_mandatos USING p_id_cat_mandato 
                                   INTO  v_arr_grupos[v_indice].id_cat_gpo,
                                         v_arr_grupos[v_indice].descripcion,
                                         v_arr_grupos[v_indice].v_existe
      # Se dejan como selecionados los grupos que estan relacionados al mandato
      LET v_arr_grupos[v_indice].asignar = v_arr_grupos[v_indice].v_existe
      # recupera los documentos del grupo que esta registrado
      IF(v_arr_grupos[v_indice].v_existe = 1)THEN
         FOREACH cur_rec_docto_grupo USING p_id_cat_mandato, v_arr_grupos[v_indice].id_cat_gpo
                                     INTO  v_imagen_docto_general[v_indice_docto].id_imagen_docto,
                                           v_imagen_docto_general[v_indice_docto].id_gpo_mandato,
                                           v_c_nombre_imagen, -- v_imagen_docto_general[v_indice_docto].nombre_imagen,
                                           v_desc_imagen
                                           
            LET v_imagen_docto_general[v_indice_docto].desc_imagen = v_desc_imagen 
                                           
            LET v_imagen_docto_general[v_indice_docto].nombre_imagen = v_c_nombre_imagen
            LET v_imagen_docto_general[v_indice_docto].v_documento       = v_imagen_docto_general[v_indice_docto].nombre_imagen
            LET v_imagen_docto_general[v_indice_docto].v_documento_int   = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto_general[v_indice_docto].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto_general[v_indice_docto].v_documento CLIPPED,"</a>" 
            LET v_imagen_docto_general[v_indice_docto].v_nombre_original = v_imagen_docto_general[v_indice_docto].nombre_imagen
            LET v_imagen_docto_general[v_indice_docto].id_cat_gpo        = v_arr_grupos[v_indice].id_cat_gpo 

            # No se llena el campo operacion ya que si esta en BD, es un registro que ya exite y solo se llena si se modifica
            LET v_indice_docto = v_indice_docto + 1
         END FOREACH
         
         # Se elimina el ultimo registro si es que esta nulo
         IF(v_imagen_docto_general[v_imagen_docto_general.getLength()].id_imagen_docto IS NULL)THEN 
            CALL v_imagen_docto_general.deleteElement(v_imagen_docto_general.getLength())
         END IF
         
         LET v_indice_docto = v_imagen_docto_general.getLength() + 1
      END IF
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_grupos_mandatos
   FREE cur_rec_docto_grupo
   # Se elimina el ultimo registro si es que esta nulo
   IF v_arr_grupos[v_arr_grupos.getLength()].id_cat_gpo IS NULL THEN 
      CALL v_arr_grupos.deleteElement(v_arr_grupos.getLength())
   END IF
   
   DECLARE cur_elementos CURSOR FOR
   SELECT id_gpo_etiqueta, id_cat_gpo, etiqueta
   FROM mdt_cat_gpo_etiqueta

   LET v_indice = 1
   FOREACH cur_elementos INTO v_arr_elementos[v_indice].id_gpo_etiqueta
                             ,v_arr_elementos[v_indice].id_cat_gpo
                             ,v_arr_elementos[v_indice].etiqueta
      --Se asigana por default que no se selecciona el registro
      LET v_indice = v_indice + 1
   END FOREACH
   --Se elimina el ultimo registro si es que esta nulo
   IF v_arr_elementos[v_arr_elementos.getLength()].id_gpo_etiqueta IS NULL THEN 
      CALL v_arr_elementos.deleteElement(v_arr_elementos.getLength())
   END IF
   
   # recupera los registros de atributos de los mandatos
   LET v_consulta = "\n SELECT nvl.id_atr_nivel, nvl.id_gpo_etiqueta, nvl.id_cat_mandato, nvl.id_gpo_mandato,",
                    "\n        nvl.orden, eti.etiqueta, ins.valor_etiqueta, eti.id_cat_gpo",
                    "\n   FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n   JOIN mdt_cat_gpo_etiqueta eti",
                    "\n     ON eti.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE nvl.id_cat_mandato = ? ",
                    "\n  ORDER BY nvl.id_gpo_mandato, nvl.orden"
   PREPARE prp_rec_atributos_mandato FROM v_consulta
   DECLARE cur_rec_atributos_mandato CURSOR FOR prp_rec_atributos_mandato
   LET v_indice = 1
   
   FOREACH cur_rec_atributos_mandato USING p_id_cat_mandato
                                     INTO  v_arr_detalle[v_indice].id_atr_nivel,
                                           v_arr_detalle[v_indice].id_gpo_etiqueta,
                                           v_arr_detalle[v_indice].id_cat_mandato,
                                           v_arr_detalle[v_indice].id_gpo_mandato,
                                           v_arr_detalle[v_indice].orden,
                                           v_arr_detalle[v_indice].etiqueta,
                                           v_arr_detalle[v_indice].valor_etiqueta,
                                           v_arr_detalle[v_indice].id_cat_gpo
      LET v_arr_detalle[v_indice].v_nombre_original = v_arr_detalle[v_indice].valor_etiqueta
      LET v_arr_detalle[v_indice].v_operacion       = 'M'

      IF(v_arr_detalle[v_indice].etiqueta = 'MUNICIPIO')THEN
         SELECT municipio
           INTO g_id_municipio
           FROM cat_municipio
          WHERE municipio_desc = v_arr_detalle[v_indice].valor_etiqueta
      END IF
      --CALL fn_mensaje("",v_arr_detalle[v_indice].id_cat_gpo,"")
      LET v_indice = v_indice + 1
      
   END FOREACH
   --CALL fn_mensaje("",v_arr_detalle.getLength(),"")
   --CALL fn_mensaje("",v_arr_detalle[1].id_atr_nivel,"")
   --CALL fn_mensaje("",v_arr_detalle[2].id_atr_nivel,"")
   IF(v_arr_detalle[v_arr_detalle.getLength()].id_atr_nivel IS NULL)THEN
      CALL v_arr_detalle.deleteElement(v_arr_detalle.getLength())
   END IF
   FREE cur_rec_atributos_mandato
   # arreglo de documentos
   -- TMP AHM SELECT id_imagen_docto, id_gpo_mandato, nombre_imagen, desc_imagen 
   -- TMP AHM   FROM mdt_imagen_docto
   -- TMP AHM  WHERE 
   -- TMP AHM v_imagen_docto
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Carga el arreglo con los documentos recuperados para    #
#                     el id_cat_mandato recibido como par�metro               #
#Autor             => Hugo C�sar Ram�rez Garc�a                               #
#Fecha Inicio      => 07/03/2012                                              #
###############################################################################
FUNCTION fn_recupera_documentos()
DEFINE v_indice   INTEGER, 
       v_consulta STRING

   LET v_consulta = ""
   FOR v_indice = 1 TO v_arr_grupos.getLength()
      
   END FOR
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => eliminar el documento seleccionado por el usuario       #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      => 06 Marzo 2012                                           #
###############################################################################
FUNCTION fn_elimina_documento(v_imagen_docto, p_existe)
DEFINE v_indice_general INTEGER,
       p_existe         BOOLEAN,
       v_imagen_docto   RECORD 
        id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen     STRING,--LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int   STRING, -- conserva la ruta seleccionada por el usuario
        v_documento       STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen       LIKE mdt_imagen_docto.desc_imagen,
        v_operacion       CHAR(1),  -- Operacion que se le aplicara al registro(A-alta, B-baja y C-cambio)
        v_nombre_original STRING   -- nombre con el que comenz� antes de modificar 
       END RECORD

   # busca el elemento a eliminar en el arreglo general
   FOR v_indice_general = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice_general].id_cat_gpo = v_imagen_docto.id_cat_gpo AND
         --v_imagen_docto_general[v_indice_general].id_gpo_mandato = v_imagen_docto.id_gpo_mandato AND
         v_imagen_docto_general[v_indice_general].v_documento = v_imagen_docto.v_documento)THEN
         IF(p_existe)THEN
            # se marca para eliminar en BD
            LET v_imagen_docto_general[v_indice_general].v_operacion = 'B'
         ELSE
            # Elimina el elemento del arreglo general y sale del for
            CALL v_imagen_docto_general.deleteElement(v_indice_general)
         END IF
         
         EXIT FOR
      END IF
   END FOR 
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los elementos       #
#                     segun su id_grupo                                       #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_elementos(p_id_cat_gpo)
   DEFINE p_id_cat_gpo  LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador    INTEGER
   --Se inicia el array temporal
   INITIALIZE v_arr_elementos_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL THEN
      FOR v_contador = 1 TO v_arr_elementos.getLength()
         IF v_arr_elementos[v_contador].id_cat_gpo = p_id_cat_gpo THEN
               CALL v_arr_elementos_tmp.appendElement()
               LET  v_arr_elementos_tmp[v_arr_elementos_tmp.getLength()].* = v_arr_elementos[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa el arreglo temporal al general del detalle         #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_pasa_tmp_general_detalle()
   DEFINE v_indice_tmp  INTEGER
   DEFINE v_indice      INTEGER
   --Se recorre todo el arreglo temporal
   FOR v_indice_tmp = 1 TO v_arr_detalle_tmp.getLength()
      --Se recorre todo el arreglo general
      FOR v_indice = 1 TO v_arr_detalle.getLength()
         IF  --v_arr_detalle_tmp[v_indice_tmp].id_atr_nivel    = v_arr_detalle[v_indice].id_atr_nivel   
          v_arr_detalle_tmp[v_indice_tmp].id_gpo_etiqueta = v_arr_detalle[v_indice].id_gpo_etiqueta
         --AND v_arr_detalle_tmp[v_indice_tmp].id_cat_mandato  = v_arr_detalle[v_indice].id_cat_mandato 
         AND v_arr_detalle_tmp[v_indice_tmp].id_cat_gpo   = v_arr_detalle[v_indice].id_cat_gpo 
         --AND v_arr_detalle_tmp[v_indice_tmp].orden           = v_arr_detalle[v_indice].orden          
         AND v_arr_detalle_tmp[v_indice_tmp].etiqueta     = v_arr_detalle[v_indice].etiqueta THEN       
         --AND v_arr_detalle_tmp[v_indice_tmp].valor_etiqueta  = v_arr_detalle[v_indice].valor_etiqueta THEN
            LET v_arr_detalle[v_indice].valor_etiqueta = v_arr_detalle_tmp[v_indice_tmp].valor_etiqueta
            EXIT FOR
         END IF
      END FOR
   END FOR
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_detalle(p_id_cat_gpo)
   DEFINE p_id_cat_gpo       LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador         INTEGER
   --Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF(v_arr_detalle[v_contador].id_cat_gpo  = p_id_cat_gpo)THEN
            IF v_arr_detalle[v_contador].v_operacion = 'B' THEN
            ELSE
               CALL v_arr_detalle_tmp.appendElement()
               LET  v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* = v_arr_detalle[v_contador].*
            END IF
         END IF
      END FOR
   END IF
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Recupera los documentos y atributos realcionados al     #
#                     grupo                                                   #
#Autor             => Hugo C�sar Ram�reaz Grac�a                              #
#Fecha Inicio      => 07/03/2012                                              #
###############################################################################
FUNCTION f_recupera_etiqueta_docto(p_id_cat_gpo)
DEFINE p_id_cat_gpo LIKE mdt_cat_gpo.id_cat_gpo,
       v_contador   INTEGER,
       v_consulta   STRING,
       v_indice     INTEGER,
       v_c_nombre_imagen LIKE mdt_imagen_docto.nombre_imagen,
       v_desc_imagen     LIKE mdt_imagen_docto.desc_imagen

   # Elimina todos los registros para volverlos a cargar desde base de datos
   FOR v_indice = 1 TO v_arr_detalle.getLength()
      IF(v_arr_detalle[v_indice].id_cat_gpo = p_id_cat_gpo)THEN
         CALL v_arr_detalle.deleteElement(v_indice)
      END IF
   END FOR
   # recupera los registros de atributos de los mandatos
   LET v_consulta = "\n SELECT nvl.id_atr_nivel, nvl.id_gpo_etiqueta, nvl.id_cat_mandato, nvl.id_gpo_mandato,",
                    "\n        nvl.orden, eti.etiqueta, ins.valor_etiqueta, eti.id_cat_gpo",
                    "\n   FROM mdt_cat_atributo_nivel nvl JOIN mdt_cat_instancia_mandato ins",
                    "\n     ON ins.id_atr_nivel = nvl.id_atr_nivel",
                    "\n   JOIN mdt_cat_gpo_etiqueta eti",
                    "\n     ON eti.id_gpo_etiqueta = nvl.id_gpo_etiqueta",
                    "\n  WHERE nvl.id_cat_mandato = ?",
                    "\n    AND eti.id_cat_gpo = ?"
   PREPARE prp_rec_detalle FROM v_consulta
   DECLARE cur_rec_detalle CURSOR FOR prp_rec_detalle 
   --Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   LET v_indice  = 1
   FOREACH cur_rec_detalle USING p_id_cat_mandato, p_id_cat_gpo
                           INTO  v_arr_detalle_tmp[v_indice].id_atr_nivel,
                                 v_arr_detalle_tmp[v_indice].id_gpo_etiqueta,
                                 v_arr_detalle_tmp[v_indice].id_cat_mandato,
                                 v_arr_detalle_tmp[v_indice].id_gpo_mandato,
                                 v_arr_detalle_tmp[v_indice].orden,
                                 v_arr_detalle_tmp[v_indice].etiqueta,
                                 v_arr_detalle_tmp[v_indice].valor_etiqueta,
                                 v_arr_detalle_tmp[v_indice].id_cat_gpo
      LET v_arr_detalle_tmp[v_indice].v_nombre_original = v_arr_detalle_tmp[v_indice].valor_etiqueta
      LET v_arr_detalle_tmp[v_indice].v_operacion       = 'M'
      CALL v_arr_detalle.appendElement()
      LET v_arr_detalle[v_arr_detalle.getLength()].* = v_arr_detalle_tmp[v_indice].*
      LET v_indice  = v_indice + 1 

   END FOREACH
   # Elimina el ultimo registro si es nulo
   IF(v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_atr_nivel IS NULL)THEN
      CALL v_arr_detalle_tmp.deleteElement(v_arr_detalle_tmp.getLength())
   END IF
   FREE cur_rec_detalle
   

   # Elimina todos los registros para volverlos a cargar desde base de datos
   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice].id_cat_gpo = p_id_cat_gpo)THEN
         CALL v_imagen_docto_general.deleteElement(v_indice)
      END IF
   END FOR
   INITIALIZE v_imagen_docto TO NULL
   # Recupera los documentos del grupo
   LET v_consulta = "\n SELECT doc.id_imagen_docto, doc.id_gpo_mandato, doc.nombre_imagen, doc.desc_imagen",
                    "\n   FROM mdt_imagen_docto doc JOIN mdt_gpo_mandato gpo",
                    "\n     ON doc.id_gpo_mandato = gpo.id_gpo_mandato",
                    "\n  WHERE gpo.id_cat_mandato = ?",
                    "\n    AND gpo.id_cat_gpo = ?"
   PREPARE prp_rec_docto_grupo_seleccionado FROM v_consulta
   DECLARE cur_rec_docto_grupo_seleccionado CURSOR FOR prp_rec_docto_grupo_seleccionado
   LET v_indice = 1
   FOREACH cur_rec_docto_grupo_seleccionado USING p_id_cat_mandato, p_id_cat_gpo
                                            INTO  v_imagen_docto[v_indice].id_imagen_docto,
                                                  v_imagen_docto[v_indice].id_gpo_mandato,
                                                  v_c_nombre_imagen, --v_imagen_docto[v_indice].nombre_imagen,
                                                  v_desc_imagen
      LET v_imagen_docto[v_indice].desc_imagen       = v_desc_imagen
      LET v_imagen_docto[v_indice].nombre_imagen     = v_c_nombre_imagen
      LET v_imagen_docto[v_indice].id_cat_gpo        = p_id_cat_gpo
      LET v_imagen_docto[v_indice].v_nombre_original = v_imagen_docto[v_indice].nombre_imagen
      LET v_imagen_docto[v_indice].v_documento       = v_imagen_docto[v_indice].nombre_imagen 
      LET v_imagen_docto[v_indice].v_documento_int   = "<a gwc:attributes=\"href resourceuri('",v_imagen_docto[v_indice].v_documento CLIPPED,"','mdtdocto')\" target='nueva'>",v_imagen_docto[v_indice].v_documento CLIPPED,"</a>"
      CALL v_imagen_docto_general.appendElement()
      LET v_imagen_docto_general[v_imagen_docto_general.getLength()].* = v_imagen_docto[v_indice].*
      LET v_indice = v_indice + 1 
      
      # la operacion se deja en nulo ya que se esta recuperando de nuevo los registros
   END FOREACH
   IF(v_imagen_docto[v_imagen_docto.getLength()].id_imagen_docto IS NULL)THEN
      CALL v_imagen_docto.deleteElement(v_imagen_docto.getLength())
   END IF
   FREE cur_rec_docto_grupo_seleccionado

END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Elimina temporalmente los documentos y atributos        #
#                     realcionados al grupo                                   #
#Autor             => Hugo C�sar Ram�reaz Grac�a                              #
#Fecha Inicio      => 07/03/2012                                              #
###############################################################################
FUNCTION f_elimina_etiqueta_docto(p_id_cat_gpo)
DEFINE p_id_cat_gpo LIKE mdt_cat_gpo.id_cat_gpo,
       v_indice     INTEGER
    
   # Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   LET v_indice = 1
   FOR v_indice = 1 TO v_arr_detalle.getLength()
      IF(v_arr_detalle[v_indice].id_cat_gpo = p_id_cat_gpo)THEN
         IF LENGTH(v_arr_detalle[v_indice].v_nombre_original CLIPPED) > 0 THEN
            LET v_arr_detalle[v_indice].v_operacion = 'B' 
         ELSE
            CALL v_arr_detalle.deleteElement(v_indice)
         END IF
      END IF
   END FOR

   # Se incia el array temporal
   INITIALIZE v_imagen_docto TO NULL
   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice].id_cat_gpo = p_id_cat_gpo)THEN
         IF LENGTH(v_imagen_docto_general[v_indice].v_nombre_original CLIPPED) > 0 THEN
            LET v_imagen_docto_general[v_indice].v_operacion = 'B'
         ELSE
            CALL v_imagen_docto_general.deleteElement(v_indice)
         END IF
      END IF
   END FOR
   
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los documentos      #
#                     seg�n su id_cat_gpo y el grupo seleccionado             #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_pasa_tmp_archivos(p_id_cat_gpo)
DEFINE p_id_cat_gpo LIKE mdt_cat_gpo.id_cat_gpo,
       v_contador   INTEGER
   # Se incia el array temporal
   INITIALIZE v_imagen_docto TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      -- p_id_cat_gpo = 2
      -- v_imagen_docto_general.getLength() = 1
      FOR v_contador = 1 TO v_imagen_docto_general.getLength()
         IF(v_imagen_docto_general[v_contador].id_cat_gpo  = p_id_cat_gpo)THEN
            IF(v_imagen_docto_general[v_contador].v_operacion = 'B')THEN
               # Solo se filtran aquellos registros que estan marcados como eliminacion(B)
            ELSE
               CALL v_imagen_docto.appendElement()
               LET  v_imagen_docto[v_imagen_docto.getLength()].* = v_imagen_docto_general[v_contador].*
            END IF            
         END IF
      END FOR
   END IF
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
{FUNCTION f_pasa_tmp_detalle(p_id_cat_gpo)
   DEFINE p_id_cat_gpo       LIKE mdt_cat_gpo.id_cat_gpo
         ,v_contador         INTEGER
   --Se incia el array temporal
   INITIALIZE v_arr_detalle_tmp TO NULL
   IF p_id_cat_gpo IS NOT NULL  THEN
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].id_gpo_mandato  = p_id_cat_gpo 
         THEN
            CALL v_arr_detalle_tmp.appendElement()
            LET  v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* = v_arr_detalle[v_contador].*
         END IF
      END FOR
   END IF
END FUNCTION}
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Sube o baja una posici�n del array temporal y general   #
#                     del detalle                                             #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_ordena_posicion_detalle(v_posicion_actual,v_tipo_operacion)
   DEFINE v_posicion_actual      INTEGER
         ,v_posicion_intercambio INTEGER
         ,v_posicion_actual_aux  INTEGER
         ,v_posicion_intercambio_aux INTEGER
         ,v_tipo_operacion       VARCHAR(10)
         ,v_contador             INTEGER
   DEFINE v_arr_detalle_actual, v_arr_detalle_intercambio RECORD
      id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
     ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
     ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
     ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato
     ,orden               LIKE mdt_cat_atributo_nivel.orden
     ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
     ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
     ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo,
      v_operacion         CHAR(1),
      v_nombre_original   STRING
   END RECORD
   DISPLAY "v_posicion_actual - ",v_posicion_actual 
   DISPLAY "v_tipo_operacion  - ",v_tipo_operacion  
   LET v_posicion_intercambio = v_posicion_actual
   IF v_tipo_operacion = "subir" AND v_posicion_actual > 1 THEN
      LET v_posicion_intercambio = v_posicion_actual - 1
      --Se almacenan en records temporales los registros que se van a intercambiar
      LET v_arr_detalle_actual.*      = v_arr_detalle_tmp[v_posicion_actual].*
      LET v_arr_detalle_intercambio.* = v_arr_detalle_tmp[v_posicion_intercambio].*
      --Se hace el intercambio de renglones en la tabla temporal
      LET v_arr_detalle_tmp[v_posicion_actual].* = v_arr_detalle_intercambio.*
      LET v_arr_detalle_tmp[v_posicion_intercambio].* = v_arr_detalle_actual.*
      --Se obtienen los indices para el intercambio en la tabla general del detalle
      FOR v_contador = 1 TO v_arr_detalle.getLength()               
         DISPLAY "v_arr_detalle[v_contador].etiqueta - ", v_arr_detalle[v_contador].etiqueta
         DISPLAY "v_arr_detalle_intercambio.etiqueta - ", v_arr_detalle_intercambio.etiqueta
         IF v_arr_detalle[v_contador].etiqueta CLIPPED = v_arr_detalle_intercambio.etiqueta CLIPPED THEN
            LET v_posicion_intercambio_aux = v_contador
            DISPLAY "Encontrado en ", v_contador
            EXIT FOR
         END IF
      END FOR
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         DISPLAY "v_arr_detalle[v_contador].etiqueta - ", v_arr_detalle[v_contador].etiqueta
         DISPLAY "v_arr_detalle_actual.etiqueta - ", v_arr_detalle_actual.etiqueta
         IF v_arr_detalle[v_contador].etiqueta CLIPPED = v_arr_detalle_actual.etiqueta CLIPPED THEN
            LET v_posicion_actual_aux = v_contador
            DISPLAY "Encontrado en ", v_contador
            EXIT FOR
         END IF
      END FOR
      --Se realiza el intercambio en la tabla general del detalle
      DISPLAY "Asignada v_posicion_actual_aux - ", v_posicion_actual_aux
      LET v_arr_detalle[v_posicion_actual_aux].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle[v_posicion_actual_aux].v_operacion = "M"
      DISPLAY "Asignada v_posicion_intercambio_aux - ", v_posicion_intercambio_aux
      LET v_arr_detalle[v_posicion_intercambio_aux].* = v_arr_detalle_actual.* 
      LET v_arr_detalle[v_posicion_intercambio_aux].v_operacion = "M"
   END IF
   
   IF v_tipo_operacion = "bajar" AND v_posicion_actual < v_arr_detalle_tmp.getLength() THEN
      LET v_posicion_intercambio = v_posicion_actual + 1
      --Se almacenan en records temporales los registros que se van a intercambiar
      LET v_arr_detalle_actual.*      = v_arr_detalle_tmp[v_posicion_actual].*
      LET v_arr_detalle_intercambio.* = v_arr_detalle_tmp[v_posicion_intercambio].*
      --Se hace el intercambio de renglones en la tabla temporal
      LET v_arr_detalle_tmp[v_posicion_actual].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle_tmp[v_posicion_intercambio].* = v_arr_detalle_actual.*
      --Se obtienen los indices para el intercambio en la tabla general del detalle
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].etiqueta CLIPPED = v_arr_detalle_intercambio.etiqueta CLIPPED THEN
            LET v_posicion_intercambio_aux = v_contador
         END IF
      END FOR
      FOR v_contador = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador].etiqueta CLIPPED = v_arr_detalle_actual.etiqueta CLIPPED THEN
            LET v_posicion_actual_aux = v_contador
         END IF
      END FOR
      --Se realiza el intercambio en la tabla general del detalle
      LET v_arr_detalle[v_posicion_actual_aux].*      = v_arr_detalle_intercambio.*
      LET v_arr_detalle[v_posicion_actual_aux].v_operacion = "M"
      LET v_arr_detalle[v_posicion_intercambio_aux].* = v_arr_detalle_actual.* 
      LET v_arr_detalle[v_posicion_intercambio_aux].v_operacion = "M"
   END IF
   RETURN v_posicion_intercambio
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Agrega o elimina un registro a las tablas temporales    #
#                     y generales del detalle                                 #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_agegar_eliminar_detalle(v_arr_elementos_aux,v_tipo_operacion)
DEFINE v_tipo_operacion   VARCHAR(10),
       v_contador_1       INTEGER,
       v_badera_existente BOOLEAN,
       v_indice           INTEGER,
       v_indice_tmp       INTEGER,
       v_nuevo_indice     INTEGER,
       v_consulta         STRING
---Array de los elementos del grupo
   DEFINE v_arr_elementos_aux RECORD
      id_gpo_etiqueta  LIKE mdt_cat_gpo_etiqueta.id_gpo_etiqueta
     ,id_cat_gpo       LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
     ,etiqueta         LIKE mdt_cat_gpo_etiqueta.etiqueta
   END RECORD
   --DISPLAY "****** AGREGAR O ELIMINAR   **********  - ", v_tipo_operacion
   --DISPLAY "Array recibido v_arr_elementos_aux.*:",v_arr_elementos_aux.*
   --Solo se a�aden si el elemento existe
   LET v_badera_existente = FALSE
   IF v_tipo_operacion = "agregar" THEN 
      --Antes de agregar se verifica que no exista el registro en la tabla temporal
      FOR v_contador_1 = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador_1].id_cat_gpo  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se sale del segundo FOR y se marca la bandera como encontrado
            LET v_badera_existente = TRUE
            # si el registro ya existia y fue dado de baja, se reactiva
            IF(v_arr_detalle[v_contador_1].v_operacion = 'B')THEN
               LET v_arr_detalle[v_contador_1].v_operacion = NULL
               CALL v_arr_detalle_tmp.appendElement()
               LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].* = v_arr_detalle[v_contador_1].*
            END IF
            EXIT FOR
         END IF
      END FOR
      --Se verifica si el registro existio
      IF v_badera_existente = FALSE THEN
         --Se a�ade el registro hasta el final del arreglo temporal
         --CALL v_arr_detalle_tmp.appendElement()
         LET v_nuevo_indice = v_arr_detalle_tmp.getLength() + 1
         {LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].etiqueta = v_arr_elementos_aux.etiqueta}
         LET v_arr_detalle_tmp[v_nuevo_indice].id_cat_gpo      = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_nuevo_indice].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_nuevo_indice].etiqueta        = v_arr_elementos_aux.etiqueta
         LET v_arr_detalle_tmp[v_nuevo_indice].v_operacion     = 'A'
         LET v_arr_detalle_tmp[v_nuevo_indice].id_cat_mandato  = p_id_cat_mandato
         LET v_consulta = "\n SELECT FIRST 1 id_atr_nivel",
                          "\n   FROM mdt_cat_atributo_nivel", 
                          "\n  WHERE id_cat_mandato = ?",
                          "\n    AND id_gpo_etiqueta = ?"
         PREPARE prp_rec_id_atr_nivel FROM v_consulta
         EXECUTE prp_rec_id_atr_nivel USING p_id_cat_mandato, v_arr_elementos_aux.id_gpo_etiqueta
                                      INTO  v_arr_detalle_tmp[v_nuevo_indice].id_atr_nivel
         SELECT id_gpo_mandato 
           INTO v_arr_detalle_tmp[v_nuevo_indice].id_gpo_mandato
           FROM mdt_gpo_mandato
          WHERE id_cat_mandato = p_id_cat_mandato
            AND id_cat_gpo = v_arr_elementos_aux.id_cat_gpo
            
         
         --Se a�ade el registro hasta el final del arreglo global
         CALL v_arr_detalle.appendElement()
         LET v_arr_detalle[v_arr_detalle.getLength()].* = v_arr_detalle_tmp[v_nuevo_indice].*
         --LET v_arr_detalle[v_arr_detalle.getLength()].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         --LET v_arr_detalle[v_arr_detalle.getLength()].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         --LET v_arr_detalle[v_arr_detalle.getLength()].etiqueta = v_arr_elementos_aux.etiqueta

      END IF
   END IF
   IF v_tipo_operacion = "eliminar" THEN 
      FOR v_contador_1 = 1 TO v_arr_detalle_tmp.getLength()
         IF v_arr_detalle_tmp[v_contador_1].id_cat_gpo  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle_tmp[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle_tmp[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se obtiene el indice de la tabla temporal
            LET v_indice_tmp = v_contador_1
            EXIT FOR
         END IF
      END FOR
      --Obtenemos el indice de la tabla general de detalle
      FOR v_contador_1 = 1 TO v_arr_detalle.getLength()
         IF v_arr_detalle[v_contador_1].id_cat_gpo  = v_arr_elementos_aux.id_cat_gpo
         AND v_arr_detalle[v_contador_1].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         AND v_arr_detalle[v_contador_1].etiqueta = v_arr_elementos_aux.etiqueta THEN
            --Si se encuentre se obtiene el indice de la tabla temporal
            LET v_indice = v_contador_1
            EXIT FOR
         END IF
      END FOR
      --Borramos los registros obtenidos
      IF v_indice > 0 THEN
         IF(v_arr_detalle[v_indice].v_nombre_original IS NULL)THEN
            # si es un registro que no esta en base de datos, lo elimina
            CALL v_arr_detalle.deleteElement(v_indice)
         ELSE
            # si es un registro de base de datos, lo marca para eliminar
            LET v_arr_detalle[v_indice].v_operacion = 'B'
         END IF
         
      END IF
      # se elimina del arreglo temporal, el arreglo que se desplega en pantalla
      IF v_indice_tmp > 0 THEN
         CALL v_arr_detalle_tmp.deleteElement(v_indice_tmp)
      END IF
   END IF
   
END FUNCTION
###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Verifica si es posible guardar el registro              #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION f_valida_captura_cat()
   DEFINE v_indice_grupo       INTEGER
         ,v_indice_validacion  INTEGER
         ,v_indice_detalle     INTEGER
         ,v_bandera_valido     BOOLEAN
         ,v_bandera_grupo      BOOLEAN  --indica si se selecciono al menos un grupo
         ,v_bandera_detalle    BOOLEAN  --Indica que por lo menos se encontro un registro del detalle
         ,v_string_comparacion  STRING

   
   --Se recorre el arreglo de los grupos que esten indicados para almacenar
   FOR v_indice_grupo = 1 TO v_arr_grupos.getLength()
      LET v_bandera_grupo = FALSE
      IF v_arr_grupos[v_indice_grupo].asignar = 1 THEN
         --Si inicializa el array que indica los que se deben guardar
         LET v_bandera_detalle = TRUE
         --Se recorre el arreglo de los detalles para validar que tenga por lo menos un registro
         
         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
            IF v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_grupos[v_indice_grupo].id_cat_gpo THEN
               LET v_bandera_grupo = TRUE
               LET v_string_comparacion = v_arr_detalle[v_indice_detalle].valor_etiqueta CLIPPED
               DISPLAY "v_arr_detalle[v_indice_detalle].etiqueta - ", v_arr_detalle[v_indice_detalle].etiqueta
               DISPLAY "v_string_comparacion - ", v_string_comparacion
               IF v_string_comparacion.trim() = "" OR v_string_comparacion.trim() IS NULL THEN
                  LET v_bandera_detalle = FALSE
                  --Si es nulo o vacio se  indica que ocurrio un error
                  EXIT FOR --Ya no se siguen validando el resto de los registros
               END IF
            END IF
         END FOR
         
         IF v_bandera_grupo = FALSE THEN
            CALL fn_mensaje("Advertencia","Grupo sin atributos seleccionados, marcar alguno","information")
            RETURN FALSE
         END IF
         --Se verifica si por lo menos se encontro un registro
         IF v_bandera_detalle = FALSE THEN
            CALL fn_mensaje("Advertencia","Atributo sin valor","information")
            RETURN FALSE
         END IF
      END IF
   END FOR
   
   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validaci�n
   # se recorre el arreglo para validar las rutas de los archivos a sibur, revisa que no sean nulas
   FOR v_indice_grupo = 1 TO v_imagen_docto_general.getLength()
      IF(v_imagen_docto_general[v_indice_grupo].v_documento IS NULL OR v_imagen_docto_general[v_indice_grupo].v_documento = " ")THEN
         LET v_bandera_valido = FALSE
         EXIT FOR
      END IF
   END FOR
   
   IF v_bandera_valido = TRUE THEN 
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF
END FUNCTION 
--FUNCTION f_valida_captura_cat()
--   DEFINE v_indice_grupo       INTEGER
--         ,v_indice_validacion  INTEGER
--         ,v_indice_detalle     INTEGER
--         ,v_bandera_valido     BOOLEAN
--         ,v_bandera_grupo      BOOLEAN  --indica si se selecciono al menos un grupo
--         ,v_bandera_detalle    BOOLEAN  --Indica que por lo menos se encontro un registro del detalle
--         ,v_string_comparacion  STRING
--
--   DEFINE v_arr_bandera_valido DYNAMIC ARRAY OF RECORD
--      grupo   INTEGER  --Para indicar indice del grupo que se debe de validar
--     ,detalle BOOLEAN  --Indica que por lo menos debe de tener un detalle
--   END RECORD
--   INITIALIZE v_arr_bandera_valido TO NULL
--   LET v_indice_validacion = 0
--   LET v_indice_validacion = FALSE
--   LET v_bandera_grupo = FALSE
--   
--   --Se recorre el arreglo de los grupos que esten indicados para almacenar
--   FOR v_indice_grupo = 1 TO v_arr_grupos.getLength()
--      IF v_arr_grupos[v_indice_grupo].asignar = 1 THEN
--         --Si inicializa el array que indica los que se deben guardar
--         LET v_bandera_grupo = TRUE
--         LET v_indice_validacion = v_indice_validacion + 1
--         LET v_arr_bandera_valido[v_indice_validacion].grupo   = v_indice_grupo
--         LET v_arr_bandera_valido[v_indice_validacion].detalle = TRUE  --Se asume que el registro es correcto
--         LET v_bandera_detalle = FALSE
--         --Se recorre el arreglo de los detalles para validar que tenga por lo menos un registro
--         
--         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
--            IF v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_grupos[v_indice_grupo].id_cat_gpo THEN
--               LET v_bandera_detalle = TRUE
--               LET v_string_comparacion = v_arr_detalle[v_indice_detalle].valor_etiqueta CLIPPED
--               IF v_string_comparacion.trim() = "" OR v_string_comparacion.trim() IS NULL THEN
--                  --Si es nulo o vacio se  indica que ocurrio un error
--                  LET v_arr_bandera_valido[v_indice_validacion].detalle = FALSE
--                  EXIT FOR --Ya no se siguen validando el resto de los registros
--               END IF
--            END IF
--         END FOR
--         --Se verifica si por lo menos se encontro un registro
--         IF v_bandera_detalle = FALSE THEN
--            LET v_arr_bandera_valido[v_indice_validacion].detalle = FALSE
--         END IF
--      END IF
--   END FOR
--   
--   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validaci�n
--   IF v_arr_bandera_valido.getLength() > 0 THEN
--      FOR v_indice_grupo = 1 TO v_arr_bandera_valido.getLength()
--         IF v_arr_bandera_valido[v_indice_grupo].detalle = FALSE THEN
--            CALL fn_mensaje("2","VALIDACION","")
--            LET v_bandera_valido = FALSE --Si encuentra un false todo se rechasa
--            EXIT FOR
--         END IF
--      END FOR
--   ELSE
--      LET v_bandera_valido = FALSE
--   END IF
--   # se recorre el arreglo para validar las rutas de los archivos a sibur, revisa que no sean nulas
--   FOR v_indice_grupo = 1 TO v_imagen_docto_general.getLength()
--      IF(v_imagen_docto_general[v_indice_grupo].v_documento IS NULL OR v_imagen_docto_general[v_indice_grupo].v_documento = " ")THEN
--         LET v_bandera_valido = FALSE
--         EXIT FOR
--      END IF
--   END FOR
--   
--   IF v_bandera_valido = TRUE AND v_bandera_grupo = TRUE THEN 
--      RETURN TRUE
--   ELSE
--      RETURN FALSE
--   END IF
--END FUNCTION 

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Se almacena el registro en base de datos                #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
--FUNCTION fn_almacena_registro_det()
--   DEFINE v_max_mdt_cat_mandato INTEGER
--         ,v_max_id_gpo_mandato  INTEGER
--         ,v_max_id_gpo_etiqueta INTEGER
--         ,v_max_id_atr_nivel    INTEGER
--         ,v_max_id_mdt_notifica INTEGER
--         ,v_id_instancia_mandato INTEGER
--         ,v_indice              INTEGER
--         ,v_indice_detalle      INTEGER
--         ,v_indice_gpo          INTEGER
--         ,v_orden               INTEGER
--         ,v_error               BOOLEAN
--         ,v_n_cuenta_bancaria     LIKE mdt_notifica_mandato.n_cuenta_bancaria
--         ,v_n_convenio            LIKE mdt_notifica_mandato.n_convenio
--         ,v_n_referencia          LIKE mdt_notifica_mandato.n_referencia
--         ,v_cta_clabe             LIKE mdt_notifica_mandato.cta_clabe,
--         v_resultado             STRING,
--         v_indice_doc            INTEGER
--
--   LET v_error = FALSE
--   LET v_n_cuenta_bancaria = NULL
--   LET v_n_convenio = NULL
--   LET v_n_referencia = NULL
--   LET v_cta_clabe = NULL
--   
--   --PASO 1: Se actualiza en mdt_cat_mandato
--   --SELECT (NVL(MAX(id_cat_mandato),0) +1) INTO V_max_mdt_cat_mandato
--   --FROM mdt_cat_mandato
--   --INSERT INTO mdt_cat_mandato
--   --(id_cat_mandato, id_mandato, desc_mandato, desc_larga_mandato
--   --, usuario, tpo_mandato, f_creacion, estado)
--   --VALUES
--   --(
--   --V_max_mdt_cat_mandato
--   --,NULL
--   --,v_desc_mandato
--   --,v_desc_larga_mandato
--   --,p_usuario_cod
--   --,v_tpo_mandato
--   --,TODAY
--   --,100
--   --)
--   UPDATE mdt_cat_mandato
--      SET desc_mandato       = v_desc_mandato,
--          desc_larga_mandato = v_desc_larga_mandato
--    WHERE id_cat_mandato = p_id_cat_mandato
--   
--   IF(SQLCA.SQLCODE <> 0)THEN
--      LET v_error = TRUE
--   END IF
--   
--   --PASO 2: Se inserta la asciacion de los grupos en mdt_gpo_mandato
--   LET v_orden = 1
--   FOR v_indice = 1 TO v_arr_grupos.getLength()
--      IF v_arr_grupos[v_indice].asignar = 1 THEN
--         SELECT (NVL(MAX(id_gpo_mandato),0) +1) INTO v_max_id_gpo_mandato
--         FROM mdt_gpo_mandato
--         INSERT INTO mdt_gpo_mandato
--         (id_gpo_mandato, id_cat_mandato, id_cat_gpo, orden, usuario)
--         VALUES
--         (
--          v_max_id_gpo_mandato
--         ,V_max_mdt_cat_mandato
--         ,v_arr_grupos[v_indice].id_cat_gpo
--         ,v_orden
--         ,p_usuario_cod
--         )
--         --El que se inserto se busca en el arreglo de los detalles para asignarle el id_gpo_mandato
--         IF SQLCA.SQLCODE <> 0 THEN
--            LET v_error = TRUE
--         END IF
--         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
--            --
--            IF v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo THEN
--               --Se asigna el grupo obtenido
--               LET v_arr_detalle[v_indice_detalle].id_cat_mandato = V_max_mdt_cat_mandato
--               --LET v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_detalle[v_indice_detalle].id_gpo_mandato
--               LET v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_max_id_gpo_mandato
--               LET v_arr_detalle[v_indice_detalle].orden = v_orden
--               LET v_orden = v_orden + 1
--            END IF
--         END FOR
--         # Se insertan todos los documentos capturados y que correspondan al grupo
--         FOR v_indice_doc = 1 TO v_imagen_docto_general.getLength()
--            IF(v_imagen_docto_general[v_indice_doc].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo)THEN
--               IF(LENGTH(v_imagen_docto_general[v_indice_doc].v_documento.trim()) > 0)THEN
--                  LET v_imagen_docto_general[v_indice_doc].id_gpo_mandato = v_max_id_gpo_mandato
--                  # recupera el identificador consecutivo
--                  SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice_doc].id_imagen_docto
--                     FROM mdt_imagen_docto
--                  # se modifica el nombre del archivo temporal al nombre con el que quedara
--                  CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice_doc].v_documento,v_imagen_docto_general[v_indice_doc].id_imagen_docto, 'A') 
--                         RETURNING v_imagen_docto_general[v_indice_doc].nombre_imagen
--                  IF(v_imagen_docto_general[v_indice_doc].nombre_imagen = "NO MODIFICADO")THEN
--                     # en caso de error
--                     CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice_doc].nombre_imagen CLIPPED,"about")
--                     LET v_error = TRUE
--                  ELSE
--                     # se valida que el archivo no sea nulo
--                     IF(v_imagen_docto_general[v_indice_doc].nombre_imagen IS NOT NULL)THEN
--                        # almacena registro en BD
--                        CALL fn_inserta_docto(v_imagen_docto_general[v_indice_doc].*)
--                     END IF
--                  END IF
--               END IF
--            END IF
--         END FOR
--         
--      END IF
--   END FOR
--   {
--   --PASO 3: Se inserta la asciacion de las etiquetas en mdt_cat_gpo_etiqueta
--   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
--      --Se insertan todos los registros de este array
--      SELECT (NVL(MAX(id_gpo_etiqueta),0) +1) INTO v_max_id_gpo_etiqueta
--      FROM mdt_cat_gpo_etiqueta
--      --Se efectua el insert
--      --DISPLAY "mdt_cat_gpo_etiqueta:",v_max_id_gpo_etiqueta
--      INSERT INTO mdt_cat_gpo_etiqueta
--      (id_gpo_etiqueta, id_cat_gpo, etiqueta, usuario)
--      VALUES
--      (
--       v_max_id_gpo_etiqueta
--      ,v_arr_detalle[v_indice_detalle].id_cat_gpo
--      ,v_arr_detalle[v_indice_detalle].etiqueta
--      ,p_usuario_cod
--      )
--      IF SQLCA.SQLCODE <> 0 THEN
--         LET v_error = TRUE
--      END IF
--      --Se inserta su id_gpo_etiqueta
--      LET v_arr_detalle[v_indice_detalle].id_gpo_etiqueta = v_max_id_gpo_etiqueta
--   END FOR
--}
--   --PASO 4: Se almacena el resto del detalle en mdt_cat_atributo_nivel
--   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
--      --Se inserta en BD
--      SELECT (NVL(MAX(id_atr_nivel),0) +1) INTO v_max_id_atr_nivel
--      FROM mdt_cat_atributo_nivel
--      INSERT INTO mdt_cat_atributo_nivel
--      (id_atr_nivel, id_gpo_etiqueta, id_cat_mandato, id_gpo_mandato, orden, id_habilita, usuario)
--      VALUES
--      (
--       v_max_id_atr_nivel
--      ,v_arr_detalle[v_indice_detalle].id_gpo_etiqueta
--      ,v_arr_detalle[v_indice_detalle].id_cat_mandato
--      ,v_arr_detalle[v_indice_detalle].id_gpo_mandato
--      ,v_arr_detalle[v_indice_detalle].orden
--      ,0
--      ,p_usuario_cod
--      )
--      IF SQLCA.SQLCODE <> 0 THEN
--         LET v_error = TRUE
--      END IF
--      LET v_arr_detalle[v_indice_detalle].id_atr_nivel = v_max_id_atr_nivel
--   END FOR
--
--   --PASO 5: Se inserta la instancia del mandato
--   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
--      SELECT (NVL(MAX(id_instancia_mandato),0) +1) INTO v_id_instancia_mandato
--      FROM mdt_cat_instancia_mandato
--      INSERT INTO mdt_cat_instancia_mandato
--      (id_instancia_mandato, id_atr_nivel, valor_etiqueta, usuario)
--      VALUES
--      (
--       v_id_instancia_mandato
--      ,v_arr_detalle[v_indice_detalle].id_atr_nivel
--      ,v_arr_detalle[v_indice_detalle].valor_etiqueta
--      ,p_usuario_cod
--      )
--      --Se obtienen los identificadores segun el campo que nos interesa
--      CASE v_arr_detalle[v_indice_detalle].etiqueta CLIPPED
--         WHEN "NUMERO DE CUENTA"
--            LET v_n_cuenta_bancaria = v_arr_detalle[v_indice_detalle].valor_etiqueta
--         WHEN "CONVENIO"
--            LET v_n_convenio = v_arr_detalle[v_indice_detalle].valor_etiqueta
--         WHEN "REFERENCIA"
--            LET v_n_referencia = v_arr_detalle[v_indice_detalle].valor_etiqueta
--         WHEN "CLABE"
--            LET v_cta_clabe = v_arr_detalle[v_indice_detalle].valor_etiqueta
--      END CASE
--      IF SQLCA.SQLCODE <> 0 THEN
--         LET v_error = TRUE
--      END IF
--   END FOR
--
--   # PASO 6: se insertan todos los documentos capturados
--   --FOR v_indice_gpo = 1 TO v_arr_grupos.getLength()
--   --   IF v_arr_grupos[v_indice].asignar = 1 THEN
--   --   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
--   --      --SELECT COUNT(id_imagen_docto)
--   --      --AHM TMP CALL fn_inserta_docto(v_imagen_docto[v_indice].*)
--   --      --AHM TMP CALL fn_transfiere_docto(v_imagen_docto[v_indice].*)
--   --      IF(v_imagen_docto_general[v_indice].v_documento IS NOT NULL AND v_imagen_docto_general[v_indice].v_documento <> " ")THEN
--   --         LET v_imagen_docto_general[v_indice].id_gpo_mandato = v_max_id_gpo_mandato
--   --         --CALL fn_transfiere_archivo(v_imagen_docto[v_indice].*) RETURNING v_imagen_docto[v_indice].nombre_imagen
--   --         # recupera el identificador consecutivo
--   --         SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice].id_imagen_docto
--   --            FROM mdt_imagen_docto
--   --         # se
--   --         CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,v_imagen_docto_general[v_indice].id_imagen_docto, 'A') 
--   --                RETURNING v_imagen_docto_general[v_indice].nombre_imagen
--   --         IF(v_imagen_docto_general[v_indice].nombre_imagen = "NO MODIFICADO")THEN
--   --            # en caso de error
--   --            CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice].nombre_imagen CLIPPED,"about")
--   --         ELSE
--   --            # almacena registro en BD
--   --            CALL fn_inserta_docto(v_imagen_docto_general[v_indice].*)
--   --         END IF
--   --      END IF
--   --   END FOR
--
--   --Se inserta en la tabla para notificar la alta hacia hipotecaria social
--   --
--   
--   --Se obtiene el maximo registro para obtener el ID a insertar
--   SELECT (NVL(MAX(id_mdt_notifica),0) + 1) INTO v_max_id_mdt_notifica
--   FROM mdt_notifica_mandato
--   --Se inserta el registro
--   INSERT INTO mdt_notifica_mandato
--   (id_mdt_notifica
--   ,id_tpo_mandato
--   ,id_cat_mandato
--   ,tipo_operacion
--   ,descripcion1
--   ,descripcion2
--   ,descripcion3
--   ,n_cuenta_bancaria
--   ,n_convenio
--   ,n_referencia
--   ,cta_clabe 
--   ,estado
--   ,resultado_operacion
--   ,diagnostico
--   ,f_creacion)
--   VALUES 
--   (
--    v_max_id_mdt_notifica  --id_mdt_notifica      decimal(9,0)
--   ,v_tpo_mandato          --id_tpo_mandato       smallint
--   ,V_max_mdt_cat_mandato  --id_cat_mandato       smallint
--   ,"A"                    --tipo_operacion       char(1)
--   ,v_desc_larga_mandato   --descripcion1         char(40)
--   ,NULL                   --descripcion2         char(40)
--   ,NULL                   --descripcion3         char(40)
--   ,v_n_cuenta_bancaria    --n_cuenta_bancaria    char(40)
--   ,v_n_convenio           --n_convenio           char(40)
--   ,v_n_referencia         --n_referencia         char(40)
--   ,v_cta_clabe            --cta_clabe            char(18)
--   ,100                    --estado               smallint
--   ,NULL                   --resultado_operacion  char(2)
--   ,NULL                   --diagnostico          char(3)
--   ,TODAY                  --f_creacion           date
--   )
--   
--   
--   RETURN v_error
--   {
--   IF v_error = TRUE THEN
--      --Ocurrio error
--      ROLLBACK WORK
--      RETURN FALSE
--   ELSE
--      --Se insertan todos los detalles
--      COMMIT WORK
--      RETURN TRUE
--   END IF
--   }
--END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Inserta el registro recibido como parametro en la tabla #
#                     mdt_imagen_docto                                        #
#Autor             => Hugo C�sar Ram�rez Grac�a                               #
#Fecha Inicio      => 03 Marzo 2012                                           #
###############################################################################
FUNCTION fn_inserta_docto(v_documento)
DEFINE --v_documento RECORD LIKE mdt_imagen_docto.*
       v_documento      RECORD 
        id_imagen_docto   LIKE mdt_imagen_docto.id_imagen_docto,
        id_gpo_mandato    LIKE mdt_imagen_docto.id_gpo_mandato,
        id_cat_gpo        LIKE mdt_cat_gpo.id_cat_gpo,
        nombre_imagen     LIKE mdt_imagen_docto.nombre_imagen,
        v_documento_int   STRING, -- conserva la ruta seleccionada por el usuario
        v_documento       STRING, -- conserva la ruta seleccionada por el usuario
        desc_imagen       LIKE mdt_imagen_docto.desc_imagen,
        v_operacion       CHAR(1),  -- Operacion que se le aplicara al registro(A-alta, B-baja y C-cambio)
        v_nombre_original STRING   -- nombre con el que comenz� antes de modificar 
       END RECORD

   WHENEVER ERROR CONTINUE
   LET v_documento.nombre_imagen = os.path.basename( v_documento.nombre_imagen)
   -- AHM TMP Solo archivo LET v_documento.nombre_imagen = v_ruta_docto CLIPPED||"/"||v_documento.nombre_imagen CLIPPED

   INSERT INTO mdt_imagen_docto(id_imagen_docto,id_gpo_mandato,nombre_imagen,desc_imagen)
              VALUES(v_documento.id_imagen_docto,v_documento.id_gpo_mandato,
                     v_documento.nombre_imagen,v_documento.desc_imagen)
   
END FUNCTION 

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Libera un grupo del mandato seleccionado                #
#Autor             => Alexandro Hollmann Montiel                              #
#Fecha Inicio      => 07 Marzo 2012                                           #
###############################################################################
FUNCTION fn_libera_gpo_mandato(p_id_cat_gpo,p_existe_bd)
   DEFINE p_id_cat_gpo      LIKE mdt_cat_gpo.id_cat_gpo
   DEFINE p_existe_bd           SMALLINT
   DEFINE v_error               SMALLINT
   DEFINE v_r_notifica_mdt      RECORD LIKE mdt_notifica_mandato.* 
   DEFINE v_indice              INTEGER
   DEFINE v_n_cuenta_bancaria   LIKE mdt_notifica_mandato.n_cuenta_bancaria
   DEFINE v_n_convenio          LIKE mdt_notifica_mandato.n_convenio
   DEFINE v_n_referencia        LIKE mdt_notifica_mandato.n_referencia
   DEFINE v_cta_clabe           LIKE mdt_notifica_mandato.cta_clabe
   
   LET v_error = FALSE
   
   --WHENEVER ERROR CONTINUE
   WHENEVER ERROR STOP
   
   IF p_existe_bd = 1 THEN
      DISPLAY "Para baja: v_arr_detalle.getLength() - ",v_arr_detalle.getLength()
      FOR v_indice = 1 TO v_arr_detalle.getLength()
         DISPLAY "v_arr_detalle[v_indice].id_cat_gpo = p_id_cat_gpo ", v_arr_detalle[v_indice].id_cat_gpo," = ",p_id_cat_gpo
         IF v_arr_detalle[v_indice].id_cat_gpo = p_id_cat_gpo THEN
            
            DISPLAY "BAJA de grupos: v_arr_detalle[v_indice].id_atr_nivel - ",v_arr_detalle[v_indice].id_atr_nivel
            IF v_arr_detalle[v_indice].id_atr_nivel > 0 THEN
      
               -- Borra instancias asociado al grupo del mandato seleccionado
               DELETE FROM mdt_cat_instancia_mandato
                WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
                
               -- Borra atributos asociado al grupo del mandato seleccionado
               DELETE FROM mdt_cat_atributo_nivel
                WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
               
               CASE v_arr_detalle[v_indice].etiqueta CLIPPED
                  WHEN "NUMERO DE CUENTA"
                     LET v_n_cuenta_bancaria = v_arr_detalle[v_indice].valor_etiqueta
                  WHEN "CONVENIO"
                     LET v_n_convenio = v_arr_detalle[v_indice].valor_etiqueta
                  WHEN "REFERENCIA"
                     LET v_n_referencia = v_arr_detalle[v_indice].valor_etiqueta
                  WHEN "CLABE"
                     LET v_cta_clabe = v_arr_detalle[v_indice].valor_etiqueta
               END CASE
      
            END IF
             
         END IF
      END FOR
   END IF
   
   -- Borra imagenes/documentos asociados al grupo del madto seleccionado
   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
      DISPLAY "ID_cat-gpo: ", v_imagen_docto_general[v_indice].id_cat_gpo," = ",p_id_cat_gpo
      IF(v_imagen_docto_general[v_indice].id_cat_gpo = p_id_cat_gpo)THEN
         DISPLAY "v_imagen_docto_general[v_indice].v_nombre_original - ",v_imagen_docto_general[v_indice].v_nombre_original
         DISPLAY "v_imagen_docto_general[v_indice].v_nombre_original - ",v_imagen_docto_general[v_indice].v_documento
         IF(LENGTH(v_imagen_docto_general[v_indice].v_documento.trim()) > 0)THEN
            IF(LENGTH(v_imagen_docto_general[v_indice].v_nombre_original.trim()) > 0 AND
               v_imagen_docto_general[v_indice].v_documento.trim() <> v_imagen_docto_general[v_indice].v_nombre_original.trim())THEN
               DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 5"
               CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_nombre_original,0, 'B') 
                      RETURNING v_imagen_docto_general[v_indice].nombre_imagen
               IF(v_imagen_docto_general[v_indice].nombre_imagen = "NO ELIMINADO")THEN
                  # en caso de error
                  CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice].nombre_imagen CLIPPED,"about")
                  LET v_error = TRUE
               END IF
            END IF
            # se borra el nombre del archivo asociado en el grupo
            DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 6"
            CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,0, 'B') 
                   RETURNING v_imagen_docto_general[v_indice].nombre_imagen
            IF(v_imagen_docto_general[v_indice].nombre_imagen = "NO ELIMINADO")THEN
               # en caso de error
               CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice].nombre_imagen CLIPPED,"about")
               LET v_error = TRUE
            ELSE
               # borra registro en BD de imagenes   
               IF v_imagen_docto_general[v_indice].id_imagen_docto > 0 THEN
                  DELETE FROM mdt_imagen_docto
                   WHERE id_imagen_docto = v_imagen_docto_general[v_indice].id_imagen_docto
               END IF
            END IF
         END IF
      END IF
   END FOR

   -- Borra grupo asociado en el mandato seleccionado
   IF p_existe_bd = 1 THEN
      DELETE FROM mdt_gpo_mandato
       WHERE id_cat_gpo     = p_id_cat_gpo
         AND id_cat_mandato = p_id_cat_mandato
   END IF

   IF SQLCA.SQLCODE < 0 THEN
      LET v_error = TRUE
   END IF

   -- AHM TMP Debe ir al finalizar la modifcicaci�n --Se obtiene el maximo registro para obtener el ID a insertar
   -- AHM TMP Debe ir al finalizar la modifcicaci�n SELECT (NVL(MAX(id_mdt_notifica),0) + 1) 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n   INTO v_r_notifica_mdt.id_mdt_notifica
   -- AHM TMP Debe ir al finalizar la modifcicaci�n FROM mdt_notifica_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.id_tpo_mandato      = p_tipo_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.id_cat_mandato      = p_id_cat_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.tipo_operacion      = 'M'
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion1        = v_desc_larga_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion2        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion3        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_cuenta_bancaria   = v_n_cuenta_bancaria
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_convenio          = v_n_convenio       
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_referencia        = v_n_referencia     
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.cta_clabe           = v_cta_clabe        
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.estado              = 100
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.resultado_operacion = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.diagnostico         = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.f_creacion          = TODAY
   -- AHM TMP Debe ir al finalizar la modifcicaci�n 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n --Se inserta el registro
   -- AHM TMP Debe ir al finalizar la modifcicaci�n INSERT INTO mdt_notifica_mandato VALUES (v_r_notifica_mdt.*)
      
   --WHENEVER ERROR STOP
  
   RETURN v_error
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM04                                                  #
#Objetivo          => Cancela la seleccion de iamgenes seleccionadas          #
#Autor             => Alexandro Hollmann Montiel                              #
#Fecha Inicio      => 07 Marzo 2012                                           #
###############################################################################
FUNCTION fn_cancela_imagen()
   DEFINE v_error               SMALLINT
   DEFINE v_indice              INTEGER
   
   LET v_error = FALSE
   
   WHENEVER ERROR CONTINUE
   
   -- Borra imagenes/documentos asociados al grupo del madto seleccionado
   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
      IF(LENGTH(v_imagen_docto_general[v_indice].v_documento.trim()) > 0)THEN
         IF v_imagen_docto_general[v_indice].v_documento.subString(1,4) = 'tmp_' THEN
            # se borra el nombre del archivo temporal asociado en el grupo
            DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 7"
            CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,0, 'B') 
                   RETURNING v_imagen_docto_general[v_indice].nombre_imagen
            IF(v_imagen_docto_general[v_indice].nombre_imagen = "NO ELIMINADO")THEN
               # en caso de error
               CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice].nombre_imagen CLIPPED,"about")
               LET v_error = TRUE
            END IF
         END IF
      END IF
   END FOR
      
   WHENEVER ERROR STOP
  
   RETURN v_error
END FUNCTION

#############################################################################
# Funcion           => fn_registra_detalle_modificaciones - Registro de los #
#                      cambios efectuados al mandato                        #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Marzo 2012                                        #
#############################################################################
FUNCTION fn_registra_detalle_modificaciones()
   DEFINE v_indice           INTEGER
   DEFINE v_estatus          SMALLINT
   DEFINE v_r_notifica_mdt   RECORD LIKE mdt_notifica_mandato.* 
   
   --PASO 1: Se inserta en mdt_cat_mandato
   --SELECT (NVL(MAX(id_cat_mandato),0) +1) INTO V_max_mdt_cat_mandato
   --FROM mdt_cat_mandato

   -- lanza actualizaci�n de movimientos por grupo
   UPDATE mdt_cat_mandato
      SET desc_mandato       = v_desc_mandato, 
          desc_larga_mandato = v_desc_larga_mandato
    WHERE id_cat_mandato     = p_id_cat_mandato

   LET v_estatus = FALSE
   FOR v_indice = 1 TO v_arr_grupos.getLength()
      -- Condiciona si el registro existe en base de datos antes de las modificaciones
      IF v_arr_grupos[v_indice].v_existe = 1 THEN
         -- Condiciona si a�n sigue activo el grupo o proceder a eliminar el grupo completo
         IF v_arr_grupos[v_indice].asignar = 1 THEN
            
            -- actualizar elementos del grupo
            CALL fn_actualiza_mandato(v_arr_grupos[v_indice].id_cat_gpo)
            
         ELSE
            -- Rutina para eliminar el grupo completo
            CALL fn_libera_gpo_mandato(v_arr_grupos[v_indice].id_cat_gpo,1) RETURNING v_estatus
         END IF
      ELSE
         IF v_arr_grupos[v_indice].asignar = 1 THEN
            -- Almacenar grupo aplicando reglas de alta 
            CALL fn_almacena_registro_det_gpo(v_indice) RETURNING v_estatus
            IF v_estatus THEN
               -- CALL fgl_winmessage("Advertencia","Hubo problemas al guardar grupo: "||v_arr_grupos[v_indice].descripcion,"exclamation")
               CALL fn_mensaje("Advertencia","Hubo problemas al guardar grupo: "||v_arr_grupos[v_indice].descripcion,"exclamation")
            END IF
         ELSE
            IF fn_verifica_captura_gpo(v_arr_grupos[v_indice].id_cat_gpo) THEN
               -- Rutina para eliminar las imagenes temporales del grupo
               CALL fn_libera_gpo_mandato(v_arr_grupos[v_indice].id_cat_gpo,0) RETURNING v_estatus
            END IF
         END IF
      END IF
   END FOR

   -- AHM TMP Debe ir al finalizar la modifcicaci�n --Se obtiene el maximo registro para obtener el ID a insertar
   -- AHM TMP Debe ir al finalizar la modifcicaci�n SELECT (NVL(MAX(id_mdt_notifica),0) + 1) 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n   INTO v_r_notifica_mdt.id_mdt_notifica
   -- AHM TMP Debe ir al finalizar la modifcicaci�n FROM mdt_notifica_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.id_tpo_mandato      = p_tipo_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.id_cat_mandato      = p_id_cat_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.tipo_operacion      = 'M'
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion1        = v_desc_larga_mandato
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion2        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.descripcion3        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_cuenta_bancaria   = v_n_cuenta_bancaria
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_convenio          = v_n_convenio       
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.n_referencia        = v_n_referencia     
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.cta_clabe           = v_cta_clabe        
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.estado              = 100
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.resultado_operacion = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.diagnostico         = NULL
   -- AHM TMP Debe ir al finalizar la modifcicaci�n LET v_r_notifica_mdt.f_creacion          = TODAY
   -- AHM TMP Debe ir al finalizar la modifcicaci�n 
   -- AHM TMP Debe ir al finalizar la modifcicaci�n --Se inserta el registro
   -- AHM TMP Debe ir al finalizar la modifcicaci�n INSERT INTO mdt_notifica_mandato VALUES (v_r_notifica_mdt.*)
   RETURN v_estatus
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Se almacena el registro en base de datos                #
#Autor             => Francisco L�pez                                         #
#Fecha Inicio      =>                                                         #
###############################################################################
FUNCTION fn_almacena_registro_det_gpo(p_indice)
   DEFINE p_indice              INTEGER
   DEFINE v_max_mdt_cat_mandato INTEGER
         ,v_max_id_gpo_mandato  INTEGER
         ,v_max_id_gpo_etiqueta INTEGER
         ,v_max_id_atr_nivel    INTEGER
         ,v_max_id_mdt_notifica INTEGER
         ,v_id_instancia_mandato INTEGER
         ,v_indice              INTEGER
         ,v_indice_detalle      INTEGER
         ,v_indice_gpo          INTEGER
         ,v_orden               INTEGER
         ,v_error               BOOLEAN
         ,v_resultado           STRING
         ,v_indice_doc          INTEGER

   LET v_error = FALSE
   
   --PASO 1: Se inserta la asciacion de los grupos en mdt_gpo_mandato
   LET v_orden = 1
   LET v_indice = p_indice

         SELECT (NVL(MAX(id_gpo_mandato),0) +1) INTO v_max_id_gpo_mandato
         FROM mdt_gpo_mandato
         
         INSERT INTO mdt_gpo_mandato
         (id_gpo_mandato, id_cat_mandato, id_cat_gpo, orden, usuario)
         VALUES
         (
          v_max_id_gpo_mandato
         ,p_id_cat_mandato
         ,v_arr_grupos[v_indice].id_cat_gpo
         ,v_orden
         ,p_usuario_cod
         )
         --El que se inserto se busca en el arreglo de los detalles para asignarle el id_gpo_mandato
         IF SQLCA.SQLCODE < 0 THEN
            LET v_error = TRUE
         END IF
         FOR v_indice_detalle = 1 TO v_arr_detalle.getLength()
            --
            IF v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo THEN
               --Se asigna el grupo obtenido
               LET v_arr_detalle[v_indice_detalle].id_cat_mandato = p_id_cat_mandato
               --LET v_arr_detalle[v_indice_detalle].id_cat_gpo = v_arr_detalle[v_indice_detalle].id_gpo_mandato
               LET v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_max_id_gpo_mandato
               LET v_arr_detalle[v_indice_detalle].orden = v_orden
               LET v_orden = v_orden + 1
            END IF
         END FOR
         # Se insertan todos los documentos capturados y que correspondan al grupo
         FOR v_indice_doc = 1 TO v_imagen_docto_general.getLength()
            IF(v_imagen_docto_general[v_indice_doc].id_cat_gpo = v_arr_grupos[v_indice].id_cat_gpo)THEN
               IF(LENGTH(v_imagen_docto_general[v_indice_doc].v_documento.trim()) > 0)THEN
                  LET v_imagen_docto_general[v_indice_doc].id_gpo_mandato = v_max_id_gpo_mandato
                  # recupera el identificador consecutivo
                  SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice_doc].id_imagen_docto
                     FROM mdt_imagen_docto
                  # se modifica el nombre del archivo temporal al nombre con el que quedara
                  CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice_doc].v_documento,v_imagen_docto_general[v_indice_doc].id_imagen_docto, 'A') 
                         RETURNING v_imagen_docto_general[v_indice_doc].nombre_imagen
                  IF(v_imagen_docto_general[v_indice_doc].nombre_imagen = "NO MODIFICADO")THEN
                     # en caso de error
                     CALL fn_mensaje("Aviso","Registro "||v_imagen_docto_general[v_indice_doc].nombre_imagen CLIPPED,"about")
                     LET v_error = TRUE
                  ELSE
                     # se valida que el archivo no sea nulo
                     IF(v_imagen_docto_general[v_indice_doc].nombre_imagen IS NOT NULL)THEN
                        # almacena registro en BD
                        CALL fn_inserta_docto(v_imagen_docto_general[v_indice_doc].*)
                     END IF
                  END IF
               END IF
            END IF
         END FOR
   --END FOR

   --PASO 2: Se almacena el resto del detalle en mdt_cat_atributo_nivel
   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
      IF v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_max_id_gpo_mandato THEN
         --Se inserta en BD
         SELECT (NVL(MAX(id_atr_nivel),0) +1) INTO v_max_id_atr_nivel
         FROM mdt_cat_atributo_nivel
         INSERT INTO mdt_cat_atributo_nivel
         (id_atr_nivel, id_gpo_etiqueta, id_cat_mandato, id_gpo_mandato, orden, id_habilita, usuario)
         VALUES
         (
          v_max_id_atr_nivel
         ,v_arr_detalle[v_indice_detalle].id_gpo_etiqueta
         ,v_arr_detalle[v_indice_detalle].id_cat_mandato
         ,v_arr_detalle[v_indice_detalle].id_gpo_mandato
         ,v_arr_detalle[v_indice_detalle].orden
         ,0
         ,p_usuario_cod
         )
         IF SQLCA.SQLCODE < 0 THEN
            LET v_error = TRUE
         END IF
         LET v_arr_detalle[v_indice_detalle].id_atr_nivel = v_max_id_atr_nivel
      END IF
   END FOR

   --PASO 3: Se inserta la instancia del mandato
   FOR v_indice_detalle = 1 TO  v_arr_detalle.getLength()
      IF v_arr_detalle[v_indice_detalle].id_gpo_mandato = v_max_id_gpo_mandato THEN
         SELECT (NVL(MAX(id_instancia_mandato),0) +1) INTO v_id_instancia_mandato
         FROM mdt_cat_instancia_mandato
         INSERT INTO mdt_cat_instancia_mandato
         (id_instancia_mandato, id_atr_nivel, valor_etiqueta, usuario)
         VALUES
         (
          v_id_instancia_mandato
         ,v_arr_detalle[v_indice_detalle].id_atr_nivel
         ,v_arr_detalle[v_indice_detalle].valor_etiqueta
         ,p_usuario_cod
         )

         IF SQLCA.SQLCODE < 0 THEN
            LET v_error = TRUE
         END IF
      END IF
   END FOR
  
   RETURN v_error

END FUNCTION

#############################################################################
# Funcion           => fn_verifica_captura_gpo - Valida la captura de image-#
#                      nes e inhabilitado el grupo, para proceder a elimi-  #
#                      nar los temporales                                   #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => p_gpo - Grupo que ser� validado si se capturaron img #
# Salida:           => v_estatus - Indica si hay imagenes capturadas a un   #
#                      grupo espec�fico. Verdadero si hay captura, falso    #
#                      cuando no hay captura de imagenes                    #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Marzo 2012                                        #
#############################################################################
FUNCTION fn_verifica_captura_gpo(p_gpo)
DEFINE v_indice  INTEGER
DEFINE v_estatus SMALLINT,
       p_gpo     LIKE mdt_cat_gpo.id_cat_gpo
   
   LET v_estatus = FALSE
   FOR v_indice = 1 TO v_imagen_docto_general.getLength()
      IF v_imagen_docto_general[v_indice].id_cat_gpo = p_gpo THEN
         LET v_estatus = TRUE
         EXIT FOR
      END IF
   END FOR
   
   RETURN v_estatus
END FUNCTION

#############################################################################
# Funcion           => fn_actualiza_mandato - Actualiza el grupo del mandato#
#                      seleccionado                                         #
# Propietario       => E.F.P                                                #
# Sistema           => HPS                                                  #
# Entrada:          => p_grupo - identificador de grupo a actualizar        #
# Salida:           => Ninguno                                              #
# Autor             => Alexandro Hollmann, EFP                              #
# Fecha             => 08 Marzo 2012                                        #
#############################################################################
FUNCTION fn_actualiza_mandato(p_grupo)
   DEFINE p_grupo               LIKE mdt_imagen_docto.id_gpo_mandato
   DEFINE v_indice              INTEGER
   DEFINE v_max_id_gpo_mandato  LIKE mdt_gpo_mandato.id_gpo_mandato
   DEFINE v_error               SMALLINT
   DEFINE v_id_instancia_mandato LIKE mdt_cat_instancia_mandato.id_instancia_mandato
   DEFINE v_max_id_atr_nivel     LIKE mdt_cat_atributo_nivel.id_atr_nivel
   DEFINE v_c_nombre_imagen      LIKE mdt_imagen_docto.nombre_imagen
   DEFINE v_orden               INTEGER,
          v_desc_imagen         LIKE mdt_imagen_docto.desc_imagen
   DEFINE v_cve_mandato_muni      INTEGER
   DEFINE v_cve_mandato_paquete   CHAR(16)
   DEFINE v_cve_mandato_paquete_aux   CHAR(18)
   
   FOR v_indice = 1 TO v_imagen_docto_general.getlength()
      DISPLAY "Documento - ",v_imagen_docto_general[v_indice].v_documento
      DISPLAY "Operacion - ",v_imagen_docto_general[v_indice].v_operacion
      IF v_imagen_docto_general[v_indice].id_cat_gpo = p_grupo THEN
         -- Alta de una nueva imagen al grupo activo
         IF LENGTH(v_imagen_docto_general[v_indice].v_nombre_original CLIPPED) = 0 THEN
            DISPLAY "Imagen nueva "
            SELECT NVL(MAX(id_imagen_docto),0)+1 INTO v_imagen_docto_general[v_indice].id_imagen_docto
             FROM mdt_imagen_docto
            CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento, 
                                      v_imagen_docto_general[v_indice].id_imagen_docto, 
                                      'A') RETURNING v_imagen_docto_general[v_indice].nombre_imagen
            DISPLAY "Inserta imagen:  ",v_imagen_docto_general[v_indice].id_imagen_docto
            DISPLAY "Con nombre_imagen: ",v_imagen_docto_general[v_indice].nombre_imagen
            DISPLAY "Con desc_imagen:   ",v_imagen_docto_general[v_indice].desc_imagen
            IF(v_imagen_docto_general[v_indice].nombre_imagen IS NOT NULL)THEN
               CALL fn_inserta_docto(v_imagen_docto_general[v_indice].*)
            END IF
         ELSE
            IF v_imagen_docto_general[v_indice].v_operacion = 'B' THEN
               CALL fn_admon_archivo_mdt(v_imagen_docto_general[v_indice].v_documento,0, 'B') 
                                                                RETURNING v_imagen_docto_general[v_indice].nombre_imagen
               DISPLAY "Estatus baja: ",v_imagen_docto_general[v_indice].nombre_imagen
               # borra registro en BD de imagenes   
               IF v_imagen_docto_general[v_indice].id_imagen_docto > 0 THEN
                  DELETE FROM mdt_imagen_docto
                   WHERE id_imagen_docto = v_imagen_docto_general[v_indice].id_imagen_docto
               END IF
            ELSE
               
               -- Verifica si cambio el archivo original para hacer el reemplazo
               IF v_imagen_docto_general[v_indice].v_documento.substring(1,4) = 'tmp_' THEN
                  DISPLAY "ELIMINO EN ON CHANGE tedi_rutam 10"
                  CALL fn_admon_archivo_mdt_modificaciones(v_imagen_docto_general[v_indice].v_nombre_original,
                                                           v_imagen_docto_general[v_indice].v_documento, 
                                                           v_imagen_docto_general[v_indice].id_imagen_docto, 
                                                           'M') RETURNING v_imagen_docto_general[v_indice].nombre_imagen
               END IF
               -- Actualiza archivo anterior con el nuevo
               DISPLAY "Actualiza imagen:  ",v_imagen_docto_general[v_indice].id_imagen_docto
               DISPLAY "Con nombre_imagen: ",v_imagen_docto_general[v_indice].nombre_imagen
               DISPLAY "Con desc_imagen:   ",v_imagen_docto_general[v_indice].desc_imagen
               LET v_c_nombre_imagen = v_imagen_docto_general[v_indice].nombre_imagen
               LET v_desc_imagen     = v_imagen_docto_general[v_indice].desc_imagen
               UPDATE mdt_imagen_docto
                  SET nombre_imagen = v_c_nombre_imagen, 
                      desc_imagen   = v_desc_imagen 
                WHERE id_imagen_docto = v_imagen_docto_general[v_indice].id_imagen_docto
            END IF
         END IF
      END IF
   END FOR
   
   LET v_orden = 1
   FOR v_indice = 1 TO  v_arr_detalle.getLength()
      DISPLAY "Por modificar v_arr_detalle.etiqueta - ", v_arr_detalle[v_indice].etiqueta
      DISPLAY "Por modificar v_arr_detalle.v_operacion", v_arr_detalle[v_indice].v_operacion
      -- Asigna el orden solo para elementos en alta y modificados
      IF v_arr_detalle[v_indice].v_operacion <> 'B' THEN
         LET v_arr_detalle[v_indice].orden = v_orden
         LET v_orden = v_orden + 1
      END IF
      DISPLAY "Grupo - ",v_arr_detalle[v_indice].id_cat_gpo," = ",p_grupo
      IF v_arr_detalle[v_indice].id_cat_gpo = p_grupo THEN
         IF v_arr_detalle[v_indice].v_operacion = 'A' THEN
            --Se inserta en BD
            SELECT (NVL(MAX(id_atr_nivel),0) +1) INTO v_max_id_atr_nivel
            FROM mdt_cat_atributo_nivel
            INSERT INTO mdt_cat_atributo_nivel
            (id_atr_nivel, id_gpo_etiqueta, id_cat_mandato, id_gpo_mandato, orden, id_habilita, usuario)
            VALUES
            (
             v_max_id_atr_nivel
            ,v_arr_detalle[v_indice].id_gpo_etiqueta
            ,v_arr_detalle[v_indice].id_cat_mandato
            ,v_arr_detalle[v_indice].id_gpo_mandato
            ,v_arr_detalle[v_indice].orden
            ,0
            ,p_usuario_cod
            )
            IF SQLCA.SQLCODE <> 0 THEN
               LET v_error = TRUE
            END IF
            --Se inserta en BD
            SELECT (NVL(MAX(id_instancia_mandato),0) +1) INTO v_id_instancia_mandato
            FROM mdt_cat_instancia_mandato
            INSERT INTO mdt_cat_instancia_mandato
            (id_instancia_mandato, id_atr_nivel, valor_etiqueta, usuario)
            VALUES
            (
             v_id_instancia_mandato
            ,v_max_id_atr_nivel
            ,v_arr_detalle[v_indice].valor_etiqueta
            ,p_usuario_cod
            )
            IF SQLCA.SQLCODE <> 0 THEN
               LET v_error = TRUE
            END IF
         ELSE
            IF v_arr_detalle[v_indice].v_operacion = 'B' THEN
               DELETE FROM mdt_cat_instancia_mandato
                WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
               DELETE FROM mdt_cat_atributo_nivel
                WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
            ELSE
               IF v_arr_detalle[v_indice].v_operacion = 'M' THEN
                  UPDATE mdt_cat_instancia_mandato
                     SET valor_etiqueta = v_arr_detalle[v_indice].valor_etiqueta,
                         usuario = p_usuario_cod
                   WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
                  UPDATE mdt_cat_atributo_nivel
                     SET orden = v_arr_detalle[v_indice].orden,
                         usuario = p_usuario_cod
                   WHERE id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel

                   IF(v_arr_detalle[v_indice].etiqueta = "MUNICIPIO")THEN

                      # Actualiza el valor del paquete para 
                      DISPLAY "g_id_municipio:",g_id_municipio
                      LET v_cve_mandato_muni = g_id_municipio
                      DISPLAY "v_cve_mandato_muni:",v_cve_mandato_muni 
                      LET v_cve_mandato_paquete = v_cve_mandato_muni USING "&&&&&&&&&&&&&&&&"
                      DISPLAY "v_cve_mandato_paquete:",v_cve_mandato_paquete  
                      LET v_cve_mandato_paquete_aux = v_tpo_mandato||v_cve_mandato_paquete  USING "&&&&&&&&&&&&&&&&&&"
                      DISPLAY "v_cve_mandato_paquete_aux:",v_cve_mandato_paquete_aux                 
                      IF(v_tpo_mandato = 1)THEN # solo se inserta si es de tipo predial
                         UPDATE mdt_cat_mandato_paquete
                            SET cve_mandato = v_cve_mandato_paquete_aux
                          WHERE id_cat_mandato = p_id_cat_mandato
                         IF SQLCA.SQLCODE <> 0 THEN
                            LET v_error = TRUE
                            DISPLAY SQLCA.SQLCODE 
                         END IF
                      END IF
                    END IF
                   
               END IF
            END IF
         END IF
      END IF
   END FOR

END FUNCTION








-- 
-- 
-- 
-- 
-- 
-- ###############################################################################
-- #Proyecto          => SAFRE VIVIENDA                                          #
-- #Propietario       => E.F.P.                                                  #
-- -------------------------------------------------------------------------------
-- #Modulo            => HPS                                                     #
-- #Programa          => HPSM04                                                  #
-- #Objetivo          => CATALOGO PARA MODIFICAR LOS MANDATOS                    #
-- #Autor             => Francisco L�pez                                         #
-- #Fecha Inicio      =>                                                         #
-- ###############################################################################
-- DATABASE safre_viv
-- 
-- DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
--       ,v_tpo_mandato         LIKE mdt_tpo_mandato.tpo_mandato  --Tipo de mandato
--       ,v_id_cat_mandato      LIKE mdt_cat_mandato.id_cat_mandato
--       ,v_desc_mandato           LIKE mdt_cat_mandato.desc_mandato
--       ,v_desc_larga_mandato     LIKE mdt_cat_mandato.desc_larga_mandato
--       ,v_desc_mandato_original  LIKE mdt_cat_mandato.desc_mandato
--       ,v_desc_larga_mandato_original LIKE mdt_cat_mandato.desc_larga_mandato
--       
-- 
-- DEFINE v_arr_detalle, v_arr_detalle_original DYNAMIC ARRAY OF RECORD
--    id_atr_nivel        LIKE mdt_cat_atributo_nivel.id_atr_nivel    --No se ocupa
--    ,id_gpo_etiqueta     LIKE mdt_cat_atributo_nivel.id_gpo_etiqueta
--    ,id_cat_mandato      LIKE mdt_cat_atributo_nivel.id_cat_mandato  --No se ocupa
--    ,id_gpo_mandato      LIKE mdt_cat_atributo_nivel.id_gpo_mandato  
--    ,orden               LIKE mdt_cat_atributo_nivel.orden
--    ,etiqueta            LIKE mdt_cat_gpo_etiqueta.etiqueta
--    ,valor_etiqueta      LIKE mdt_cat_instancia_mandato.valor_etiqueta
--    ,id_cat_gpo          LIKE mdt_cat_gpo_etiqueta.id_cat_gpo
--    ,id_modifica         LIKE mdt_cat_gpo_etiqueta.id_modifica
-- END RECORD
-- 
-- ######################################################################################
-- # INICIA EL MAIN
-- ######################################################################################
-- MAIN
--    DEFINE p_tipo_ejecucion         SMALLINT -- forma como ejecutara el programa
--          ,p_titulo                 STRING   -- titulo de la ventana
--          ,v_id_cat_gpo_actual      LIKE mdt_cat_gpo.id_cat_gpo
--          ,v_desc_mandato_tmp       LIKE mdt_cat_mandato.desc_mandato
--          ,v_desc_larga_mandato_tmp LIKE mdt_cat_mandato.desc_larga_mandato
--          ,v_estatus                INTEGER
--          
--          ,v_bandera_continua       BOOLEAN
--    DEFINE f ui.Form
--    DEFINE d ui.Dialog
--    
-- 
--    DEFINE cb_1 ui.ComboBox  --Tipo Combobox
--    DEFINE cb_2 ui.ComboBox  --Tipo Combobox
--    
--    
--    -- se recupera la clave de usuario desde parametro 
--    LET p_usuario_cod    = ARG_VAL(1)
--    LET p_tipo_ejecucion = ARG_VAL(2)
--    LET p_titulo         = ARG_VAL(3)
-- 
--    # Valida si el usuario puede ejecutar el programa
--    IF NOT fn_verifica_privilegios("HPSM04",p_usuario_cod) THEN
--       CALL fgl_winmessage("Advertencia","No cuenta con privilegios para esta opci�n","exclamation")
--       EXIT PROGRAM
--    END IF
-- 
--    -- si se obtuvo el titulo, se pone como titulo de programa
--    IF ( p_titulo IS NOT NULL ) THEN
--       CALL ui.Interface.setText(p_titulo)
--    END IF
--    LET v_bandera_continua = FALSE
--    
--    --Se abre la ventana de captura del nuevo mandato
--    OPEN WINDOW w_modifi_mant_mandatos WITH FORM "HPSM041"
--       INPUT v_tpo_mandato, v_id_cat_mandato FROM cb_tipo_mandato, cb_mandato  ATTRIBUTES (UNBUFFERED)
--          BEFORE INPUT
--             --Se llena el combo con los datos
--             LET cb_1 = ui.ComboBox.forName("formonly.cb_tipo_mandato")
--             LET cb_2 = ui.ComboBox.forName("formonly.cb_mandato")
--             CALL fn_llena_combo_1(cb_1)
--          ON CHANGE cb_tipo_mandato
--             CALL fn_llena_combo_2(cb_2, v_tpo_mandato)
--          ON ACTION ACCEPT
--             IF v_tpo_mandato IS NOT NULL AND v_id_cat_mandato IS NOT NULL THEN
--                LET v_bandera_continua = TRUE
--                EXIT INPUT
--             ELSE
--                CONTINUE INPUT
--             END IF
--          ON ACTION CANCEL
--             EXIT INPUT
--       END INPUT
-- 
--       IF v_bandera_continua = TRUE THEN
--          --Si capturo los registros para ser modificados 
--          DIALOG   ATTRIBUTES (UNBUFFERED , FIELD ORDER FORM)
--             
--             INPUT v_desc_mandato, v_desc_larga_mandato FROM desc_mandato, desc_larga_mandato
--             ATTRIBUTES (WITHOUT DEFAULTS)
--                BEFORE INPUT 
--                   --Se obtienen las descripciones
--                   SELECT desc_mandato, desc_larga_mandato
--                   INTO   v_desc_mandato, v_desc_larga_mandato
--                   FROM   mdt_cat_mandato
--                   WHERE  id_cat_mandato = v_id_cat_mandato
--                   --Se asigna el valor las variables modulares que almacenaran el valor original
--                   LET v_desc_mandato_original  = v_desc_mandato
--                   LET v_desc_larga_mandato_original = v_desc_larga_mandato
--             END INPUT
--             INPUT  ARRAY v_arr_detalle FROM v_arr_detalle.*
--             ATTRIBUTE( INSERT ROW=FALSE, APPEND ROW=FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, WITHOUT DEFAULTS)
--                BEFORE ROW
--                   IF v_arr_detalle[ARR_CURR()].id_modifica <> 1 THEN
--                      --Si es difetende de 1  no se permite la modificaci�n
--                      -- y se pasas el control al siguiente renlogn hasta que se pueda modificar
--                      IF ARR_CURR() >= ARR_COUNT() THEN
--                         CALL DIALOG.nextField("formonly.desc_mandato")
--                      ELSE
--                         CALL FGL_SET_ARR_CURR( ARR_CURR() + 1)
--                      END IF
--                   END IF
--             END INPUT
--             BEFORE DIALOG
--                --Se obtiene el detalle de la tabla
--                CALL fn_llena_atributos_detalle()
--             ON ACTION Modificar
--                IF fn_ventana_confirma("Confimar","�Desea guardar los cambios efectuados?","info") = 1 THEN
--                   --Si confirmo el guardar los cambios se almacenan los cambios efectuados
--                   CALL fn_modifica_registros_mandato() RETURNING v_estatus
--                   IF v_estatus = TRUE THEN
--                      CALL fn_mensaje("Aviso","Modificaci�n al mandato "|| v_desc_mandato ||" efectuada","info")
--                   ELSE
--                      CALL fn_mensaje("Aviso","Mandato con un registro previo en espera de notificaci�n","error")
--                   END IF
--                END IF
--                EXIT DIALOG
--             ON ACTION CANCELAR
--                CALL fn_mensaje("Aviso","Modificaci�n Cancelada","info")
--                EXIT DIALOG
--          END DIALOG
--       END IF
--    CLOSE WINDOW w_modifi_mant_mandatos
-- 
--    
-- END MAIN
-- ###############################################################################
-- #Modulo            => HPS                                                     #
-- #Programa          => HPSM04                                                  #
-- #Objetivo          => LLena el combobox del tipo de mandato                   #
-- #Autor             => Francisco L�pez                                         #
-- #Fecha Inicio      =>                                                         #
-- ###############################################################################
-- FUNCTION fn_llena_combo_1(cb)
--    DEFINE cb ui.ComboBox  --Tipo Combobox
--    DEFINE v_rec_mdt_tpo_mandato RECORD LIKE mdt_tpo_mandato.*
--    
--    DECLARE cur_tipo_mandato CURSOR FOR
--    SELECT * FROM mdt_tpo_mandato
--    CALL cb.clear()
--    FOREACH cur_tipo_mandato INTO v_rec_mdt_tpo_mandato.*
--       CALL cb.addItem(v_rec_mdt_tpo_mandato.tpo_mandato,v_rec_mdt_tpo_mandato.desc_tpo_mandato)
--    END FOREACH
--    FREE cur_tipo_mandato
-- END FUNCTION
-- ###############################################################################
-- #Modulo            => HPS                                                     #
-- #Programa          => HPSM01                                                  #
-- #Objetivo          => LLena el combobox de los mandatos existentes            #
-- #Autor             => Francisco L�pez                                         #
-- #Fecha Inicio      =>                                                         #
-- ###############################################################################
-- FUNCTION fn_llena_combo_2(cb, v_tpo_mandato)
--    DEFINE cb ui.ComboBox  --Tipo Combobox
--    DEFINE v_rec_mdt_cat_mandato RECORD LIKE mdt_cat_mandato.*
--    DEFINE v_tpo_mandato         LIKE mdt_cat_mandato.tpo_mandato
--    
--    DECLARE cur_mdt_cat_mandato CURSOR FOR
--    SELECT * FROM mdt_cat_mandato
--    WHERE  tpo_mandato = v_tpo_mandato
--    AND    estado = 100
-- 
--    CALL cb.clear()
--    FOREACH cur_mdt_cat_mandato INTO v_rec_mdt_cat_mandato.*
--       CALL cb.addItem(v_rec_mdt_cat_mandato.id_cat_mandato,v_rec_mdt_cat_mandato.desc_mandato)
--    END FOREACH
--    FREE cur_mdt_cat_mandato
-- END FUNCTION
-- ###############################################################################
-- #Modulo            => HPS                                                     #
-- #Programa          => HPSM04                                                  #
-- #Objetivo          => LLena el combobox de los mandatos existentes            #
-- #Autor             => Francisco L�pez                                         #
-- #Fecha Inicio      =>                                                         #
-- ###############################################################################
-- FUNCTION fn_llena_atributos_detalle()
--    DEFINE v_indice      INTEGER
--          ,v_orden_aux  INTEGER
-- 
--    INITIALIZE v_arr_detalle TO NULL
--    INITIALIZE v_arr_detalle_original TO NULL
--    
--    DECLARE cur_detalle CURSOR FOR 
--    SELECT a.id_atr_nivel
--          ,a.id_gpo_etiqueta
--          ,a.id_cat_mandato
--          ,a.id_gpo_mandato
--          ,a.orden
--          ,(SELECT etiqueta FROM mdt_cat_gpo_etiqueta AS b
--            WHERE  b.id_gpo_etiqueta  = a.id_gpo_etiqueta )
--          ,(SELECT valor_etiqueta FROM mdt_cat_instancia_mandato AS c
--            WHERE  c.id_atr_nivel = a.id_atr_nivel )
--          ,(SELECT id_cat_gpo FROM mdt_gpo_mandato AS d
--            WHERE  d.id_gpo_mandato = a.id_gpo_mandato
--            AND    d.id_cat_mandato = a.id_cat_mandato )
--          ,(SELECT id_modifica FROM mdt_cat_gpo_etiqueta e
--            WHERE  e.id_gpo_etiqueta = a.id_gpo_etiqueta)
--          ,(SELECT orden FROM mdt_gpo_mandato AS f
--            WHERE  f.id_gpo_mandato = a.id_gpo_mandato
--            AND    f.id_cat_mandato = a.id_cat_mandato) AS orden_gpo
--    FROM mdt_cat_atributo_nivel AS a
--    WHERE a.id_cat_mandato = v_id_cat_mandato
--    ORDER BY orden_gpo ASC, a.orden ASC
-- 
--    LET v_indice = 1
--    FOREACH cur_detalle INTO v_arr_detalle[v_indice].id_atr_nivel
--                            ,v_arr_detalle[v_indice].id_gpo_etiqueta
--                            ,v_arr_detalle[v_indice].id_cat_mandato
--                            ,v_arr_detalle[v_indice].id_gpo_mandato
--                            ,v_arr_detalle[v_indice].orden
--                            ,v_arr_detalle[v_indice].etiqueta
--                            ,v_arr_detalle[v_indice].valor_etiqueta
--                            ,v_arr_detalle[v_indice].id_cat_gpo
--                            ,v_arr_detalle[v_indice].id_modifica
--                            ,v_orden_aux
--       LET v_arr_detalle_original[v_indice].* = v_arr_detalle[v_indice].*
--       LET v_indice = v_indice + 1
--    END FOREACH
--    --SE ELIMINA EL ULTIMO REGISTRO SI ES QUE VIENE NULO
--    IF v_arr_detalle[v_arr_detalle.getLength()].id_atr_nivel IS NULL THEN
--       CALL v_arr_detalle.deleteElement(v_arr_detalle.getLength())
--    END IF
--    --Se asignan los valores al areglo original
--    --LET v_arr_detalle_original.* = v_arr_detalle.*
-- END FUNCTION
-- 
-- ###############################################################################
-- #Modulo            => HPS                                                     #
-- #Programa          => HPSM04                                                  #
-- #Objetivo          => Funcion que realiza las modificaciones en base de datos #
-- #Autor             => Francisco L�pez                                         #
-- #Fecha Inicio      =>                                                         #
-- ###############################################################################
-- FUNCTION fn_modifica_registros_mandato()
--    DEFINE v_bandera_notifica  BOOLEAN
--          ,v_bandera_existente BOOLEAN
--          ,v_bandera_query     BOOLEAN
--          ,v_indice            INTEGER
--          ,v_max_id_mdt_notifica INTEGER
--          ,v_n_cuenta_bancaria     LIKE mdt_notifica_mandato.n_cuenta_bancaria
--          ,v_n_convenio            LIKE mdt_notifica_mandato.n_convenio
--          ,v_n_referencia          LIKE mdt_notifica_mandato.n_referencia
--          ,v_cta_clabe             LIKE mdt_notifica_mandato.cta_clabe
-- 
--    LET v_bandera_notifica = FALSE
--    --Se valida que es necesario notificar el cambio en la tabla mdt_notifica_mandato
--    --Si la descripcion larga fue modificada se notifica el cambia
--    IF v_desc_larga_mandato_original <> v_desc_larga_mandato THEN
--       LET v_bandera_notifica = TRUE
--    END IF
--    --Se recorre el arreglo para verificar si ocurrio algun cambio en los atributos
--    FOR v_indice = 1 TO v_arr_detalle.getLength()
--       --Solo se validan las que tengan valor = 1 en id_modifica
--       IF v_arr_detalle[v_indice].id_modifica = 1 THEN
--          --Validamos si ocurrio un cambio en el valor
--          IF v_arr_detalle[v_indice].valor_etiqueta <> v_arr_detalle_original[v_indice].valor_etiqueta THEN
--             LET v_bandera_notifica = TRUE
--          END IF
--       END IF
--    END FOR
-- 
--    LET v_bandera_existente = FALSE
--    IF v_bandera_notifica = TRUE THEN
--       --Si se debe de notificar se valida que no exista un registro previo en la tabla mdt_notifica_mandato
--       FOR v_indice = 1 TO v_arr_detalle.getLength() 
--          --Solo se validan las que tengan valor = 1 en id_modifica
--          IF v_arr_detalle[v_indice].id_modifica = 1 THEN
--             LET v_bandera_query = FALSE
--             SELECT TRUE
--             INTO  v_bandera_query
--             FROM  mdt_notifica_mandato
--             WHERE id_tpo_mandato = v_tpo_mandato
--             AND   id_cat_mandato = id_cat_mandato
--             AND   estado = 100
--             --Si encontro el registro el el query se marca la bandera de existente para indicar que no es 
--             --�posible efectuar los cambios ya que existe una notificaci�n pendiente
--             IF v_bandera_query = TRUE THEN
--                LET v_bandera_existente = TRUE
--             END IF
--          END IF
--       END FOR
--    END IF
-- 
--    --Solo se almacenan los registros
--    IF v_bandera_existente = TRUE THEN
--       --Si encontro con que existe una notificacion pendiente no continua con la modificaci�n
--       RETURN FALSE
--    END IF
-- 
--    --Se almacenan los registros que sufrieron cambio
--    
--    UPDATE mdt_cat_mandato
--    SET    desc_mandato = v_desc_mandato
--          ,desc_larga_mandato = v_desc_larga_mandato
--    WHERE  id_cat_mandato = v_id_cat_mandato
-- 
--    --Se actualizan los registros que sufrieron cambio
--    FOR v_indice = 1 TO v_arr_detalle.getLength()
--       --Solo se validan las que tengan valor = 1 en id_modifica
--       IF v_arr_detalle[v_indice].id_modifica = 1 THEN
--          --Validamos si ocurrio un cambio en el valor
--          DISPLAY "validando Cambio  :",v_arr_detalle[v_indice].valor_etiqueta
--          DISPLAY "validando Original:",v_arr_detalle_original[v_indice].valor_etiqueta
--          IF v_arr_detalle[v_indice].valor_etiqueta <> v_arr_detalle_original[v_indice].valor_etiqueta THEN
--             DISPLAY "ENCONTRO CAMBIO EN:", v_arr_detalle[v_indice].valor_etiqueta
--             --Si sufrio cambio se efectua el update
--             UPDATE mdt_cat_instancia_mandato
--             SET    valor_etiqueta = v_arr_detalle[v_indice].valor_etiqueta
--             WHERE  id_atr_nivel = v_arr_detalle[v_indice].id_atr_nivel
--             --AND    valor_etiqueta = v_arr_detalle_original[v_indice].valor_etiqueta
--          END IF
--       END IF
--    END FOR
-- 
--    LET v_n_cuenta_bancaria = NULL
--    LET v_n_convenio = NULL
--    LET v_n_referencia = NULL
--    LET v_cta_clabe = NULL
--    
--    --Se valida que sea necesario noticar el cambio
--    IF v_bandera_notifica = TRUE THEN
--       --Se obtienen los datos necesarios para la notificaci�n
--       FOR v_indice = 1 TO v_arr_detalle.getLength()
--                --Se obtienen los identificadores segun el campo que nos interesa
--          CASE v_arr_detalle[v_indice].etiqueta CLIPPED
--             WHEN "NUMERO DE CUENTA"
--                LET v_n_cuenta_bancaria = v_arr_detalle[v_indice].valor_etiqueta
--             WHEN "CONVENIO"
--                LET v_n_convenio = v_arr_detalle[v_indice].valor_etiqueta
--             WHEN "REFERENCIA"
--                LET v_n_referencia = v_arr_detalle[v_indice].valor_etiqueta
--             WHEN "CLABE"
--                LET v_cta_clabe = v_arr_detalle[v_indice].valor_etiqueta
--          END CASE
--       END FOR
--       --Se inserta la notificacion en mdt_notifica_mandato
--       --Se obtiene el maximo registro para obtener el ID a insertar
--       SELECT (NVL(MAX(id_mdt_notifica),0) + 1) INTO v_max_id_mdt_notifica
--       FROM mdt_notifica_mandato
--       --Se inserta el registro
--       INSERT INTO mdt_notifica_mandato
--       (id_mdt_notifica
--       ,id_tpo_mandato
--       ,id_cat_mandato
--       ,tipo_operacion
--       ,descripcion1
--       ,descripcion2
--       ,descripcion3
--       ,n_cuenta_bancaria
--       ,n_convenio
--       ,n_referencia
--       ,cta_clabe 
--       ,estado
--       ,resultado_operacion
--       ,diagnostico
--       ,f_creacion)
--       VALUES 
--       (
--        v_max_id_mdt_notifica  --id_mdt_notifica      decimal(9,0)
--       ,v_tpo_mandato          --id_tpo_mandato       smallint
--       ,v_id_cat_mandato       --id_cat_mandato       smallint
--       ,"C"                    --tipo_operacion       char(1)
--       ,v_desc_larga_mandato   --descripcion1         char(40)
--       ,NULL                   --descripcion2         char(40)
--       ,NULL                   --descripcion3         char(40)
--       ,v_n_cuenta_bancaria    --n_cuenta_bancaria    char(40)
--       ,v_n_convenio           --n_convenio           char(40)
--       ,v_n_referencia         --n_referencia         char(40)
--       ,v_cta_clabe            --cta_clabe            char(18)
--       ,100                    --estado               smallint
--       ,NULL                   --resultado_operacion  char(2)
--       ,NULL                   --diagnostico          char(3)
--       ,TODAY                  --f_creacion           date
--       )
--    END IF
--    
--    RETURN TRUE
-- END FUNCTION
-- 
-- fn_admon_archivo_mdt_modificaciones
-- (p_archivo_org,p_archivo_nvo, p_id_imagen_docto, p_tpo_ren)

                                                                                                                 HPSP01.4gl                                                                                          0000777 0000212 0001751 00000020107 13113322754 012311  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24-07-2013
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => HPSP01                                                   #
#Objetivo          => Programa batch de reporte traspaso fondo servicios       #
#Autor             => Jesus Ya�ez Moreno                                       #
#Fecha inicio      => 18 Enero 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "HPSG02.4gl"

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_resultado_opera SMALLINT,
       v_mensaje         STRING,
       v_consulta        STRING,
       v_folio_rpt       LIKE glo_folio.folio,
       v_folio_dummy     DECIMAL(9,0),
       v_usuario_dummy   VARCHAR(20),
       v_registros       INTEGER
DEFINE v_si_resultado    SMALLINT
DEFINE v_isam_err        INTEGER
DEFINE v_err_txt         CHAR(200) 
DEFINE v_c_msj           VARCHAR(200)
DEFINE l_txt             VARCHAR(1000) 

   # Se recuperan los par�metros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   LET v_folio_dummy = 11740
   LET v_usuario_dummy = "prueba_hps"   
   
   LET l_txt = "EXECUTE PROCEDURE sp_hps_aplica_fondo_servicio(?,?) "
   PREPARE qry_aplica_fondo_servicio FROM l_txt

   LET l_txt = "EXECUTE FUNCTION fn_hps_preliquidar_traspaso_servicios(?,?) "
   PREPARE qry_preliquidar_traspaso_servicios FROM l_txt

   
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'

   # recupera la descripci�n del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   DISPLAY "INTEGRANDO SALDOS A TRASPASAR SUBCUENTAS SERVICIOS" 

   EXECUTE qry_aplica_fondo_servicio USING v_folio_dummy, v_usuario_dummy 
                                     INTO  v_si_resultado ,
                                           v_isam_err     ,
                                           v_c_msj
   IF v_si_resultado <> 0 THEN
   
      # Actualiza a estado err�neo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = v_c_msj
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reporte Traspaso Fondos Subcuentas Servicios',
                             v_mensaje)

   END IF
                                                               
   INITIALIZE v_folio_rpt TO NULL
   
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND status = 0" # registrado
   PREPARE prp_recupera_folio FROM v_consulta
   EXECUTE prp_recupera_folio USING p_proceso_cod,
                                    p_opera_cod
                               INTO v_folio_rpt

   DISPLAY "\n"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "folio  : ",v_folio_rpt
   DISPLAY "Proceso: ",v_proceso_desc
   DISPLAY "\n"

  
   DISPLAY "INICIANDO PROVISION"

   EXECUTE qry_preliquidar_traspaso_servicios USING v_folio_rpt ,p_usuario_cod 
                                              INTO  v_si_resultado ,
                                                    v_isam_err     ,
                                                    v_c_msj     
   IF v_si_resultado <> 0 THEN
   
      # Actualiza a estado err�neo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = v_c_msj
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reporte Traspaso Fondos Subcuentas Servicios',
                             v_mensaje)
   END IF

   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "GENERANDO REPORTE"
   DISPLAY "\n"

   LET v_registros = 0
   # Libreria(HPSI31) que genera el reporte de mandatos
   CALL fn_mdt_rpt_aplicacion_mdt(v_folio_rpt,
                                  50, # abono mandato
                                  p_usuario_cod,
                                  p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  TODAY,
                                  "HPSP01") RETURNING v_registros

IF( v_registros > 0)THEN
 
   DISPLAY "\n"
   DISPLAY "REPORTE CONCLUIDO"
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

   CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN
      LET v_mensaje = "La operaci�n ha finalizado correctamente"
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reporte Abonos Mandatos',
                             v_mensaje)
   ELSE              
      # Actualiza a estado err�neo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = "Ocurri� un error al actualizar el estado de la operaci�n"
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reporte Abonos Mandatos',
                             v_mensaje)
   END IF
ELSE
   CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

END IF
END MAIN
                                                                                                                                                                                                                                                                                                                                                                                                                                                         HPSP03.4gl                                                                                          0000777 0000212 0001751 00000011040 13113322755 012310  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               ################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSP03                                                   #
#Objetivo          => Integraci�n de Actualiza entidad receptora mandato       # 
#Fecha inicio      => Julio 10, 2015                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio       LIKE glo_ctr_archivo.folio, # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo # nombre del archivo a integrar

MAIN

   DEFINE r_folio           LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
   v_proceso_desc    LIKE cat_proceso.proceso_desc,
   v_total_predial   INTEGER,
   v_total_cuota     INTEGER,
   v_consulta        VARCHAR(254),
   v_detalle_temp    VARCHAR(254),
   v_error_sql       INTEGER,
   v_isam_error      INTEGER,
   v_msg_error       CHAR(254),
   r_resultado_opera SMALLINT,
   v_mensaje         CHAR(254),
   v_muestra_detalle INTEGER

   # Se recuperan los par�metros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario)
                        RETURNING r_folio
   
   # recupera la descripci�n del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   #Se genera una tabla temporal para vaciar los detalles en la integraci�n.
   CREATE TEMP TABLE hps_actualiza_ent_mdt(mensaje CHAR(255))

   #Se recupera la cantidad de registros a integrar
   SELECT COUNT(*) 
      INTO v_total_predial 
      FROM safre_tmp:hps_tmp_det_acmdt
      WHERE tipo_mandato = 1
   SELECT COUNT(*)
      INTO v_total_cuota
      FROM safre_tmp:hps_tmp_det_acmdt
      WHERE tipo_mandato = 2

   #Imprime en el log los datos recabados
   DISPLAY "\n"
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "ARCHIVO: ",v_nom_archivo
   DISPLAY "FOLIO: ",r_folio
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA TIPO PREDIAL :",v_total_predial USING "###,##&"
   DISPLAY "TOTAL DE REGISTROS A SER INTEGRADOS PARA CUOTA CONSERVACION :",v_total_cuota USING "###,##&"
   DISPLAY "\n"

   #Ejecuta SP de integraci�n
   LET v_consulta = "EXECUTE PROCEDURE sp_hps_integra_cat_ent_rec_mdt(?,?,?,?)"
   PREPARE prp_consulta FROM v_consulta
   EXECUTE prp_consulta USING p_usuario,p_folio,p_proceso_cod,v_nom_archivo INTO v_error_sql,v_isam_error,v_msg_error

   IF v_error_sql <> 0 THEN
      DISPLAY "OCURRI� UN ERROR AL EJECUTAR EL SP"
      DISPLAY "C�DIGO: ",v_error_sql
      DISPLAY "MENSAJE: ",v_msg_error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                                RETURNING r_resultado_opera
      # si ocurri� un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      EXIT PROGRAM
   END IF 

   ## Mostrando detalles de la tabla temporal
   SELECT 
      COUNT(*) 
      INTO v_muestra_detalle
      FROM hps_actualiza_ent_mdt
   IF v_muestra_detalle > 0 THEN
      DISPLAY "SE ENCONTRARON LOS SIGUIENTES DETALLES EN LA INTEGRACION"
      LET v_detalle_temp = "SELECT * FROM hps_actualiza_ent_mdt"
      PREPARE prp_detalles FROM v_detalle_temp
      DECLARE cur_detalle CURSOR FOR prp_detalles
      FOREACH cur_detalle INTO v_mensaje 
         DISPLAY v_mensaje
      END FOREACH
      CLOSE cur_detalle
      FREE cur_detalle
      FREE prp_detalles
   ELSE
      DISPLAY "LOS REGISTROS SE INTEGRARON CORRECTAMENTE"
   END IF

   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurri� un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   END IF
    
END MAIN                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                HPSP05.4gl                                                                                          0000777 0000212 0001751 00000045704 13113322755 012330  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-07-2013
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSP05                                                   #
#Objetivo          => Programa batch de preliquidacion de servicios            #
#Autor             => Jesus David Ya�ez Moreno                                 #
#Fecha inicio      => 16 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "HPSG02.4gl" -- libreria de varibales globales

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE r_folio           LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_mandatos DYNAMIC ARRAY OF RECORD
          v_descripcion_mdt VARCHAR(40),--LIKE mdt_cat_mandato.desc_mandato,
          v_padre           STRING,
          v_identificador   SMALLINT,--LIKE mdt_cat_mandato.id_cat_mandato,          
          v_expandido       BOOLEAN,
          v_municipio       VARCHAR(40),--LIKE cat_municipio.municipio_desc,
          v_recurrencia     SMALLINT,
          v_complemento_recurrencia SMALLINT,
          v_ejecuta_cod      SMALLINT,
          v_descripcion      VARCHAR(10),--LIKE mdt_cat_mandato_ejecuta_pago.descripcion
          v_ent_federeativa  VARCHAR(40)--LIKE cat_entidad_federativa.entidad_desc_larga
       END RECORD,
       r_continuar          BOOLEAN,
       v_consulta           STRING,
       v_fecha_actual       DATE,
       v_ind_proceso        SMALLINT,
       v_id_ctr_aplica_pago_mandato DECIMAL(9,0),--LIKE mdt_ctr_aplica_pago_mandato.id_ctr_aplica_pago_mandato,
       v_indice             SMALLINT,
       v_entidad_federativa LIKE cat_entidad_federativa.entidad_federativa,
       v_municipio          LIKE cat_municipio.municipio,
       r_sql_error          INTEGER,
       r_isam_error         SMALLINT,
       r_msg_error          CHAR(80),
       r_desc_mandato       LIKE mdt_cat_mandato.desc_mandato,
       r_sum_monto_pesos    DECIMAL(22,2),
       v_total_monto_pesos  DECIMAL(22,2),
       r_actualiza          SMALLINT,
       r_resultado_opera    SMALLINT,
       v_mensaje            STRING,
       r_archivo_salida     STRING,
       v_estado_pago_mdt    SMALLINT,
       v_tot_reg            INTEGER       

   # Se recuperan los par�metros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   LET v_total_monto_pesos = 0

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'hps'

   
   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod) RETURNING r_folio

   # recupera la descripci�n del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   DISPLAY "\n"
   DISPLAY "Proceso: ",v_proceso_desc
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "folio  : ",r_folio
   DISPLAY "INICIANDO PROCESO DE PRELIQUIDACI�N"
   DISPLAY "\n"

   # Actualiza folio de proceso batch
   UPDATE bat_ctr_proceso
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod

   UPDATE bat_ctr_operacion
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      --AND opera_cod = p_opera_cod

   # la libreria recuperara informacion ya que el lanzador valid� que existan mandatos a preliquidar
   CALL fn_valida_ejecucion_mandato(g_estado_abonado_pago_mdt) RETURNING r_continuar, r_mandatos

   LET v_fecha_actual = TODAY
   LET v_ind_proceso  = 1 # preliquidado 
   LET v_consulta = "\n INSERT INTO hps_ctr_aplica_pago_servicio",
                    "\n (id_ctr_aplica_pago_servicio,",
                    "\n  folio_pago_servicio,",
                    "\n  f_pago_servicio,",
                    "\n  ind_proceso,",
                    "\n  usuario)",
                    "\n VALUES(seq_hps_ctr_aplica_pago_servicio.NEXTVAL,?,?,?,?)"
   PREPARE prp_almacena_ctr_pago FROM v_consulta
   EXECUTE prp_almacena_ctr_pago USING r_folio,
                                       v_fecha_actual,
                                       v_ind_proceso,
                                       p_usuario_cod

   LET v_consulta = "\n SELECT FIRST 1 seq_hps_ctr_aplica_pago_servicio.CURRVAL",
                    "\n   FROM hps_ctr_aplica_pago_servicio"
   PREPARE prp_rec_identificador_ctr FROM v_consulta
   EXECUTE prp_rec_identificador_ctr INTO v_id_ctr_aplica_pago_mandato

   LET v_consulta = "\n SELECT entidad_federativa",
                    "\n   FROM cat_entidad_federativa",
                    "\n  WHERE entidad_desc_larga MATCHES ?"
   PREPARE prp_rec_cve_entidad FROM v_consulta

   LET v_consulta = "\n SELECT municipio",
                    "\n   FROM cat_municipio",
                    "\n  WHERE entidad_federativa = ?",
                    "\n    AND municipio_desc MATCHES ?"
   PREPARE prp_rec_cve_municipio FROM v_consulta  
   LET v_consulta = " EXECUTE FUNCTION sp_hps_preliquida_pago_instruccion(?,?,?,?,?,?,?,?)"
   PREPARE prp_ejecuta_preliquidacion_mandato FROM v_consulta
   DISPLAY "\n "
   FOR v_indice = 1 TO r_mandatos.getLength()
      IF( r_mandatos[v_indice].v_padre <> 0 )THEN # solo los mandatos tienen padre <> 0
         
         DISPLAY TODAY," ",
                 EXTEND(CURRENT,HOUR TO SECOND)," ",
                 r_mandatos[v_indice].v_descripcion_mdt," "

         EXECUTE prp_rec_cve_entidad USING r_mandatos[v_indice].v_ent_federeativa
                                      INTO v_entidad_federativa

         EXECUTE prp_rec_cve_municipio USING v_entidad_federativa,
                                             r_mandatos[v_indice].v_municipio
                                        INTO v_municipio
         LET r_sql_error = 0
                 
         EXECUTE prp_ejecuta_preliquidacion_mandato USING  r_mandatos[v_indice].v_identificador,
                                                           g_estado_abonado_pago_mdt,
                                                           r_mandatos[v_indice].v_descripcion_mdt,
                                                           v_entidad_federativa,
                                                           v_municipio,
                                                           r_folio,
                                                           v_id_ctr_aplica_pago_mandato,
                                                           p_usuario_cod
                                                      INTO r_sql_error,
                                                           r_isam_error,
                                                           r_msg_error,
                                                           r_desc_mandato,
                                                           r_sum_monto_pesos
         DISPLAY "Monto: ",r_sum_monto_pesos
         DISPLAY "\n"
         LET v_total_monto_pesos = v_total_monto_pesos + r_sum_monto_pesos
         IF(r_sql_error )THEN
            DISPLAY "\n"
            DISPLAY "C�digo:",r_sql_error
            DISPLAY "ISAM:",r_isam_error
            DISPLAY "Mensaje:",r_msg_error
            DISPLAY "Mandato:",r_desc_mandato
            DISPLAY "Suma Monto:",r_sum_monto_pesos
            DISPLAY "\n"
            DISPLAY "Program Stopped"

            # Actualiza a estado err�neo
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
            IF( r_resultado_opera )THEN
               # Muestra el mensaje de inconsistencia en archivo y consola
               CALL fn_desplega_inc_operacion(r_resultado_opera)
            END IF
            EXIT PROGRAM 
         END IF         

      END IF
   END FOR
   IF( v_total_monto_pesos = 0 )THEN
      DISPLAY "PROCESO TERMINADO SIN PROCESAR NING�N REGISTRO"
   END IF
   DISPLAY "\n "
   DISPLAY "\n"
   DISPLAY "FINALIZANDO PRELIQUIDACI�N"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   # Si no proces� ning�n registro, termina la operaci�n en erronea
   IF( v_total_monto_pesos = 0 )THEN
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      RETURN
   END IF
   DISPLAY "GENERANDO ARCHIVO DE SALIDA"
   DISPLAY "\n"

   # Genera archivo de salida con pago de mandatos
   CALL fn_genera_archivo_salida(r_folio) RETURNING r_archivo_salida

   DISPLAY "\n"
   DISPLAY "ARCHIVO GENERADO:",r_archivo_salida
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "GENERANDO REPORTE"
   DISPLAY "\n"
   LET v_tot_reg = 0
   CALL fn_mdt_rpt_aplicacion_mdt(r_folio,
                                  g_estado_preliquidado_pago_mdt, # estado 105 preliquidado
                                  p_usuario_cod,
                                  p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  TODAY,
                                  "HPSP05") # nombre de archivo batch que ejecuta reporte
   RETURNING v_tot_reg
   
   DISPLAY "\n"
   DISPLAY "REPORTE CONCLUIDO"
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

   CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN
      LET v_mensaje = "El proceso ha finalizado correctamente"
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Preliquidaci�n',
                             v_mensaje)
   ELSE              
      # Actualiza a estado err�neo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = "Ocurri� un error al actualizar el estado de la operaci�n"
      # Envia correo de estado de operaci�n               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Preliquidaci�n',
                             v_mensaje)
   END IF               
END MAIN

{===============================================================================
Nombre: fn_genera_archivo_salida
Fecha creacion: 11 Julio 2013
Autor: Hugo C�sar Ram�rez Grac�a
Narrativa del proceso que realiza:
 Funci�n para generar archivo de salida pagos de mandatos
 Parametros de Entrada:
  -
 Par�metros de salida:
  -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
================================================================================}
FUNCTION fn_genera_archivo_salida(p_folio)
DEFINE p_folio LIKE glo_folio.folio,
       v_registros RECORD
         v_caso           LIKE mdt_det_aplica_pago_mandato.caso_proceso,
         v_id_credito     LIKE mdt_det_aplica_pago_mandato.id_credito,
         v_situacion      CHAR(10),
         v_ent_federativa LIKE mdt_det_aplica_pago_mandato.ent_federativa,
         v_oficina_cesi   CHAR(10),
         v_cve_convenio   LIKE mdt_det_aplica_pago_mandato.clave_convenio,
         v_monto          LIKE mdt_det_aplica_pago_mandato.valor_descuento,
         v_f_inicio       LIKE mdt_det_aplica_pago_mandato.f_inicio_mandato,
         v_trabajador     LIKE mdt_det_aplica_pago_mandato.nombre_trabajador,
         v_regimen_pago   CHAR(10),
         v_calle          CHAR(20),
         v_colonia        CHAR(20),
         v_cp             CHAR(5),
         v_municipio      LIKE mdt_det_aplica_pago_mandato.municipio,
         v_espacio        CHAR(11),
         v_plazo          CHAR(2),
         v_pago           LIKE mdt_det_aplica_pago_mandato.monto_pesos,
         v_f_liquida      LIKE mdt_det_aplica_pago_mandato.f_liquida
       END RECORD,
       v_registros_tmp RECORD
         v_caso           CHAR(1),
         v_id_credito     CHAR(10),
         v_situacion      CHAR(10),
         v_ent_federativa CHAR(5),
         v_oficina_cesi   CHAR(10),
         v_cve_convenio   CHAR(16),
         v_monto          CHAR(8),
         v_f_inicio       DATE,
         v_trabajador     CHAR(50),
         v_regimen_pago   CHAR(10),
         v_calle          CHAR(20),
         v_colonia        CHAR(20),
         v_cp             CHAR(5),
         v_municipio      CHAR(5),
         v_espacio        CHAR(11),
         v_plazo          CHAR(2),
         v_pago           CHAR(8),
         v_f_liquida      DATE
       END RECORD,
       v_f_inicio   CHAR(8),
       v_f_liquida  CHAR(8),
       v_consulta   STRING,
       v_indice     SMALLINT,
       v_ruta_envio LIKE seg_modulo.ruta_envio,
       v_canal      base.Channel,
       v_archivo_salida STRING,
       v_fecha_actual   CHAR(10),
       v_hora           CHAR(8)

   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "hps"

   LET v_fecha_actual = TODAY
   LET v_hora = TIME
   LET v_archivo_salida = v_fecha_actual[7,10], # A�o
                          v_fecha_actual[1,2],  # Mes
                          v_fecha_actual[4,5],  # D�a                    
                          v_hora[1,2], # Hora
                          v_hora[4,5], # Minutos
                          v_hora[7,8], # Segundos
                          "_DETPAGOS.HPS"
   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )

   LET v_indice = 1
   LET v_consulta = "\n SELECT det.caso_proceso,",
                    "\n        det.id_credito,",
                    "\n        '          ',",
                    "\n        det.ent_federativa,",
                    "\n        '          ',",
                    "\n        det.clave_convenio,",
                    "\n        det.valor_descuento * 100,", # Multiplica * 100 para no considerar el punto decimal en archivo
                    "\n        det.f_inicio_mandato,",
                    "\n        det.nombre_trabajador,",
                    "\n        '          ',",
                    "\n        '                    ',",
                    "\n        '                    ',",
                    "\n        '     ',",
                    "\n        det.municipio,",
                    "\n        '           ',",
                    "\n        '  ',",
                    "\n        det.monto_pesos * 100,", # Multiplica * 100 para no considerar el punto decimal en archivo
                    "\n        det.f_liquida",
                    "\n   FROM hps_ctr_aplica_pago_servicio ctr JOIN hps_det_aplica_pago_servicio det",
                    "\n     ON ctr.id_ctr_aplica_pago_servicio = det.id_ctr_aplica_pago_servicio ",
                    "\n  WHERE ctr.folio_pago_servicio = ?"
   PREPARE prp_rec_registros_pago_mdt FROM v_consulta
   DECLARE cur_rec_registros_pago_mdt CURSOR FOR prp_rec_registros_pago_mdt
   FOREACH cur_rec_registros_pago_mdt USING p_folio
                                       INTO v_registros.*
      LET v_registros_tmp.v_caso           = v_registros.v_caso
      LET v_registros_tmp.v_id_credito     = v_registros.v_id_credito USING "&&&&&&&&&&"
      LET v_registros_tmp.v_situacion      = v_registros.v_situacion
      LET v_registros_tmp.v_ent_federativa = v_registros.v_ent_federativa USING "&&&&&"
      LET v_registros_tmp.v_oficina_cesi   = v_registros.v_oficina_cesi
      LET v_registros_tmp.v_cve_convenio   = v_registros.v_cve_convenio[3,18]
      LET v_registros_tmp.v_monto          = v_registros.v_monto USING "&&&&&&&&"
      LET v_f_inicio                       = v_registros.v_f_inicio USING "yyyymmdd"
      LET v_registros_tmp.v_trabajador     = v_registros.v_trabajador
      LET v_registros_tmp.v_regimen_pago   = v_registros.v_regimen_pago
      LET v_registros_tmp.v_calle          = v_registros.v_calle
      LET v_registros_tmp.v_colonia        = v_registros.v_colonia
      LET v_registros_tmp.v_cp             = v_registros.v_cp
      LET v_registros_tmp.v_municipio      = v_registros.v_municipio USING "&&&&&"
      LET v_registros_tmp.v_espacio        = v_registros.v_espacio
      LET v_registros_tmp.v_plazo          = v_registros.v_plazo
      LET v_registros_tmp.v_pago           = v_registros.v_pago USING "&&&&&&&&"
      LET v_f_liquida                      = v_registros.v_f_liquida USING "yyyymmdd"
      
      CALL v_canal.writeLine(v_registros_tmp.v_caso||
                             v_registros_tmp.v_id_credito ||
                             v_registros_tmp.v_situacion||
                             v_registros_tmp.v_ent_federativa ||
                             v_registros_tmp.v_oficina_cesi||
                             v_registros_tmp.v_cve_convenio||
                             v_registros_tmp.v_monto ||
                             v_f_inicio ||
                             v_registros_tmp.v_trabajador||
                             v_registros_tmp.v_regimen_pago||
                             v_registros_tmp.v_calle||
                             v_registros_tmp.v_colonia||
                             v_registros_tmp.v_cp||
                             v_registros_tmp.v_municipio ||
                             v_registros_tmp.v_espacio||
                             v_registros_tmp.v_plazo||
                             v_registros_tmp.v_pago ||
                             v_f_liquida )

   END FOREACH
   FREE cur_rec_registros_pago_mdt
   CALL v_canal.close()
   RETURN v_archivo_salida
END FUNCTION
                                                            HPSP21.4gl                                                                                          0000777 0000212 0001751 00000032177 13113322763 012325  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-06-2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => HPS                                                     #
#Programa          => HPSP21                                                  #
#Objetivo          => Generar Archivo de integracion                          #
#Autor             => Jesus David Yanez M, EFP                                #
#Fecha Inicio      => 09 Junio 2012                                           #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE
   g_prog_reporte              CHAR(200),
   g_pid                       LIKE bat_ctr_proceso.pid,     --  ID del proceso
   g_proceso_cod               LIKE cat_proceso.proceso_cod, -- codigo del proceso
   g_opera_cod                 LIKE cat_operacion.opera_cod, -- codigo de operacion
   p_usuario_cod               LIKE seg_usuario.usuario_cod, -- Clave de usuario
   g_opera_cod_preliquidacion  LIKE cat_operacion.opera_cod,  -- codigo de operacion
   g_ruta_envio      LIKE seg_modulo.ruta_envio,
   v_nom_archivo     CHAR(200)
   --v_nom_archivo     STRING
DEFINE v_total_casos, v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado  CHAR(1)
     ,casos   INTEGER
   END RECORD
DEFINE v_total_procesados INTEGER
DEFINE v_total_x_procesar INTEGER
DEFINE v_r_rpt_res   RECORD -- registro de resumen
          des_origen   LIKE mdt_cat_origen.des_origen,
          proceso_desc LIKE cat_proceso.proceso_desc,
          lote         LIKE mdt_solicitud_mandato.folio,
          f_lote       LIKE mdt_lote_mandato.f_proceso,
          altas        INTEGER,
          bajas        INTEGER,
          modif        INTEGER
       END RECORD,
       v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
       v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
       r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
       r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
DEFINE tot_altas_proc          INTEGER,
       tot_bajas_proc          INTEGER,
       tot_modificaciones_proc INTEGER,
       r_b_valida              SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
DEFINE p_num_folio             LIKE glo_folio.folio
DEFINE r_resultado_opera       SMALLINT
END GLOBALS

#Objetivo:
MAIN
DEFINE 
   v_estatus         SMALLINT,
   v_hora            CHAR(8),
   v_query           STRING,
   v_fec_ejecucion   DATETIME YEAR TO SECOND 

   # se recupera la hora a la que inici� la operacion
   LET v_fec_ejecucion = CURRENT YEAR TO SECOND 
   #Si se ha recibido par�metros se continua    

   LET p_usuario_cod = ARG_VAL(1)
   LET g_pid         = ARG_VAL(2)
   LET g_proceso_cod = ARG_VAL(3)
   LET g_opera_cod   = ARG_VAL(4)  
   LET p_num_folio   = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   LET v_hora = TIME (CURRENT HOUR TO SECOND) 
   
   LET v_query = "\n SELECT ruta_envio         "
                ,"\n FROM   seg_modulo         "
                ,"\n WHERE  modulo_cod = 'hps' "
   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO g_ruta_envio

   IF p_num_folio = 0 OR p_num_folio IS NULL THEN
      CALL fn_genera_folio(g_proceso_cod, g_opera_cod, p_usuario_cod) RETURNING p_num_folio 
   END IF
   
   -- TMP Por integrar DISPLAY " PID ASIGNADO :",g_pid
   CALL fn_display_proceso(0,"GENERAR ARCHIVO DE MANDATOS - INTEGRACION")
   
   --Se escribe la descripci�n del proceso
   DISPLAY "\n DESCRIPCI�N DEL PROCESO:"
   DISPLAY " Generar archivo resumen de integracion."
   DISPLAY "Folio: ",p_num_folio

   CALL fn_total_registros()
   
   DISPLAY "\n TOTAL DE REGISTROS X INTEGRAR:",v_total_x_procesar
   DISPLAY '\n GENERANDO ARCHIVO DE INTEGRACION'
   
      CALL f_integra_mandatos() RETURNING v_total_procesados
      
      UPDATE bat_ctr_operacion 
      SET nom_archivo = v_nom_archivo ,
          folio = p_num_folio
      WHERE pid  =  g_pid
      AND   proceso_cod = g_proceso_cod   
      AND   opera_cod = g_opera_cod   

   CALL fn_rutas("hps") RETURNING r_ruta_bin, r_ruta_listados
   DISPLAY "Ruta bin - ", r_ruta_bin
   DISPLAY "Ruta lst - ", r_ruta_listados    
   
   LET g_prog_reporte  = r_ruta_bin CLIPPED ,"/HPSI21.4rp" 
   
   DISPLAY g_prog_reporte CLIPPED 
   
   -- se indica que el reporte usara la plantilla creada
   IF fgl_report_loadCurrentSettings(g_prog_reporte CLIPPED ) THEN
      -- se indica la salida del reporte
      CALL fgl_report_selectDevice("PDF") 
      LET v_v_nom_reporte = p_usuario_cod CLIPPED, "-HPSP21-", g_pid USING "&&&&&", "-", g_proceso_cod USING "&&&&&", "-", g_opera_cod USING "&&&&&"
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
   
      -- sin indica que no es necesario el preview
      CALL fgl_report_selectPreview(0)
   
      -- se asigna la configuraci�n en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "no fue posible generar el reporte"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                         RETURNING r_b_valida
   
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
   END IF
   
   -- inicia el reporte de registros procesados
   START REPORT reporte_reg_proc TO XML HANDLER v_manejador_rpt

   -- Recupera descripcion del tipo de origen
   SELECT des_origen 
     INTO v_r_rpt_res.des_origen 
     FROM mdt_cat_origen
   WHERE id_origen = 1
   
   -- Recupera descripcion del proceso
   SELECT proceso_desc 
     INTO v_r_rpt_res.proceso_desc 
     FROM cat_proceso
   WHERE proceso_cod = g_proceso_cod

   LET v_query = "SELECT NVL(COUNT(*),0) ",
                 "  FROM hps_solicitud_mandato",
                 " WHERE folio = ",p_num_folio,
                 "   AND tipo_operacion = ?"
   PREPARE EnuTotIntegrados FROM v_query
               
   EXECUTE EnuTotIntegrados USING 'A' INTO tot_altas_proc
   EXECUTE EnuTotIntegrados USING 'B' INTO tot_bajas_proc
   EXECUTE EnuTotIntegrados USING 'M' INTO tot_modificaciones_proc

   LET v_r_rpt_res.lote   = p_num_folio
   LET v_r_rpt_res.f_lote = TODAY
   LET v_r_rpt_res.altas = tot_altas_proc
   LET v_r_rpt_res.bajas = tot_bajas_proc
   LET v_r_rpt_res.modif = tot_modificaciones_proc
   
   OUTPUT TO REPORT reporte_reg_proc(v_r_rpt_res.*)
   
   -- finaliza el reporte
   FINISH REPORT reporte_reg_proc

   CALL fn_actualiza_opera_fin (g_pid
                               ,g_proceso_cod
                               ,g_opera_cod)
                               RETURNING v_estatus

   # Env�o de correo de notificaci�n de proceso finalizado
   CALL fn_correo_proceso(g_pid, 
                          g_proceso_cod, 
                          g_opera_cod, 
                          NULL, 
                          'Integra mandatos - Origen recurrente',
                          'ID Proceso   : '||g_pid||
                          'Proceso      : '||g_proceso_cod||
                          'Operacion    : '||g_opera_cod||
                          'Fecha Inicio : '||v_fec_ejecucion||
                          'Fecha Fin    : '||CURRENT YEAR TO SECOND 
                         )
   CALL fn_act_edo_archivo(v_nom_archivo,p_num_folio,2,p_usuario_cod)
                           RETURNING r_resultado_opera
                           
   UPDATE glo_folio
      SET STATUS = 1
     WHERE proceso_cod = g_proceso_cod 
       AND opera_cod = g_opera_cod
     
   IF(r_resultado_opera <> 0)THEN
      DISPLAY "\n ERROR AL ACTUALIZAR ESTADO DE ARCHIVO (C�DIGO):",r_resultado_opera
   END IF

   DISPLAY "\n TOTAL DE REGISTROS PROCESADOS:",v_total_procesados
   
   DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",v_nom_archivo
   
   CALL fn_display_proceso(1,"GENERAR ARCHIVO DE MANDATOS - INTEGRACION")
END MAIN

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSP21                                                   #
#Objetivo          => Integracion de registros de mandatos origen recurrente   #
#Autor             => Jesus David Yanez M, EFP                                  #
#Fecha Inicio      => 11 Junio 2012                                            #
#Modificaci�n      => se agrega ciclo de busqueda al SP                        #
################################################################################
FUNCTION f_integra_mandatos()
DEFINE v_qry                 STRING
DEFINE v_r_tmp_mdt_recurrente_acr RECORD LIKE safre_tmp:tmp_acr_transf_30.*
DEFINE v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente
DEFINE v_f_inicio            DATE
DEFINE v_f_culmina           DATE
DEFINE v_f_temporal          DATE
DEFINE v_integrados          INTEGER
DEFINE v_cve_mandato         LIKE mdt_cat_mandato.cve_mandato
DEFINE r_folio               LIKE glo_folio.folio
DEFINE v_sql_error      INTEGER
DEFINE v_msg_error      CHAR(200)

   LET v_f_temporal = TODAY
   LET v_integrados = 0
   
   LET v_qry = "EXECUTE PROCEDURE sp_hps_inserta_inst_recurrente(?,?)"
   PREPARE EnuRegRecurente FROM v_qry


   EXECUTE EnuRegRecurente USING p_num_folio,
                                 p_usuario_cod
                            INTO v_integrados,
                                 v_sql_error,
                                 v_msg_error
   IF(v_sql_error <> 0)THEN
      DISPLAY "OCURRI� UN ERROR AL ALMACENAR INFORMACI�N"
      CALL fn_error_opera(g_pid, g_proceso_cod, g_opera_cod) 
                        RETURNING r_b_valida
      
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF
   
   RETURN v_integrados
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSP21                                                  #
#Objetivo          => Obtiene el total de registros que se integraron         #
#Autor             => Jesus David Yanez M, EFP                                 #
#Fecha Inicio      => 09 Junio 2012                                                        #
###############################################################################
FUNCTION fn_total_registros()

   LET v_total_x_procesar = 0
   
   SELECT NVL(COUNT(*),0) INTO v_total_x_procesar
     FROM safre_tmp:tmp_hps_det_instrucciones

END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSP21                                                  #
#Objetivo          => Validacion de registros temporales de mandatos origen rec.
#Autor             => Jesus David Yanez M, EFP                                 #
#Fecha Inicio      => 11 Junio 2012                                                        #
###############################################################################
FUNCTION fn_valida_mandato()
   
   
   RETURN TRUE
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSP21                                                  #
#Objetivo          => Genera el reporte de integrados                         #
#Autor             => Jesus David Yanez M, EFP                                 #
#Fecha Inicio      => 09 Junio 2012                                           #
###############################################################################
REPORT reporte_reg_proc(p_r_res)
   DEFINE p_r_res   RECORD -- registro de resumen
             des_origen   LIKE mdt_cat_origen.des_origen,
             proceso_desc LIKE cat_proceso.proceso_desc,
             lote         LIKE mdt_solicitud_mandato.folio,
             f_lote       LIKE mdt_lote_mandato.f_proceso,
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD

   FORMAT

   FIRST PAGE HEADER
      PRINTX p_r_res.des_origen  
      PRINTX p_r_res.proceso_desc
      PRINTX p_r_res.lote        
      PRINTX p_r_res.f_lote USING "DD-MMM-YYYY"     
      PRINTX p_r_res.altas USING "#########&"
      PRINTX p_r_res.bajas USING "#########&"
      PRINTX p_r_res.modif USING "#########&"

END REPORT
                                                                                                                                                                                                                                                                                                                                                                                                 HPSP22.4gl                                                                                          0000777 0000212 0001751 00000073535 13113322763 012331  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               ---===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
-- Fecha ultima modificacion: 13-07-2012 se modifica ruta en duro
-- del reporte, se agrega variable v_prog_report
--===============================================================

####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSP22                                        #
#Objetivo          =>Programa que genera el archivo de salida de   # 
#                    mandatos                                      #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 FEBRERO 2012                               #
####################################################################
GLOBALS "HPSG02.4gl"
DATABASE safre_viv
DEFINE v_prog_reporte      CHAR(200)
DEFINE p_v_usuario         CHAR(20), -- nombre del usuario
          p_d_pid          LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_folio          DECIMAL(9,0)

MAIN
   --DEFINE ppp_estado  smallint
   DEFINE v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_r_mandato          RECORD LIKE hps_solicitud_mandato.* 

   DEFINE tot_altas               INTEGER,
          tot_bajas               INTEGER,
          tot_modificaciones      INTEGER,
          tot_reactivacion        INTEGER
   DEFINE tot_altas_proc          INTEGER,
          tot_bajas_proc          INTEGER,
          tot_modificaciones_proc INTEGER,
          tot_reactivacion_proc   INTEGER
   DEFINE v_sp_estado             INTEGER
   DEFINE v_reg_102_a,
          v_reg_102_b,
          v_reg_102_m,
          v_reg_102_r             INTEGER
   DEFINE v_reg_106_a,
          v_reg_106_b,
          v_reg_106_m,
          v_reg_106_r             INTEGER
   DEFINE p_fec_ejecucion         DATE
   DEFINE v_r_rpt_res   RECORD -- registro de resumen
             des_origen   LIKE mdt_cat_origen.des_origen,
             proceso_desc LIKE cat_proceso.proceso_desc,
             folio        DECIMAL(9,0),
             f_proceso    CHAR(10),
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD
   DEFINE v_r_rpt_res_edo   RECORD -- registro de resumen por estado
             altas_aceptadas  INTEGER,
             bajas_aceptadas  INTEGER,
             modif_aceptadas  INTEGER,
             altas_rechazadas INTEGER,
             bajas_rechazadas INTEGER,
             modif_rechazadas INTEGER
          END RECORD,
          v_r_reporte_det   DYNAMIC ARRAY OF RECORD -- registro de detalle
             nss            char(011),
             mandato_desc   LIKE mdt_cat_mandato.desc_mandato,
             diagnostico    string
          END RECORD,
          v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        char(40),
          r_ruta_listados   char(40)
   DEFINE v_pos_rep_det        INTEGER
   DEFINE v_pos_rep_det_fin    INTEGER
   DEFINE v_mandato_desc    LIKE mdt_cat_mandato.desc_mandato
   DEFINE v_estado          CHAR(1)
   DEFINE v_tpo_mandato     LIKE mdt_tpo_mandato.desc_tpo_mandato
   DEFINE v_tipo_operacion  LIKE mdt_solicitud_mandato.tipo_operacion
   DEFINE v_tipo_operacion_desc   CHAR(20)
   DEFINE v_tot_mdt         INTEGER
   DEFINE v_pos_acep        SMALLINT
   DEFINE v_pos_rech        SMALLINT
   DEFINE v_day             CHAR(2)
   DEFINE v_mes             CHAR(2)
   DEFINE v_ano             CHAR(4)
   DEFINE v_f_proceso  DATE
   DEFINE p_nom_archivo CHAR(20)
   
   SLEEP 5
   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario     = ARG_VAL(1)
   LET p_d_pid         = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod   = ARG_VAL(4)
   LET p_folio         = ARG_VAL(5)
   LET p_nom_archivo   = ARG_VAL(6) -- Fecha del lote 

   {SELECT a.folio 
   INTO   p_v_fecha_lote 
   FROM   mdt_lote_mandato a
   WHERE  a.estado = 102}
   
   --WHENEVER ERROR STOP
   WHENEVER ERROR CONTINUE
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".HPSP22.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-HPSP22-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
   
   DISPLAY "PID asignado: ",p_d_pid
   DISPLAY "Proceso asignado: ",p_i_proceso_cod
   DISPLAY "Operacion asignado: ",p_i_opera_cod
   DISPLAY "Fecha de Proceso: ",today
   DISPLAY "Folio: ",p_folio

   LET v_s_qryTxt = "create TEMP table tmp_procesa_mdt ",
                    "  (",
                    "    estado CHAR(1),",
                    "    cve_mandato CHAR(18),",
                    "    tipo_operacion CHAR(1)",
                    "  );"
   PREPARE EnuTmpProc FROM v_s_qryTxt
   EXECUTE EnuTmpProc 

   LET v_s_qryTxt = "DROP table tmp_procesa_mdt; "
   PREPARE EnuTmpProcDrop FROM v_s_qryTxt

   LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                    "   FROM hps_solicitud_mandato ",
                    "  WHERE estado = 101 ",
                    "    AND folio = ? ",
                    "    AND tipo_operacion = ? "
                    
   PREPARE EnuTotReg FROM v_s_qryTxt
   
   EXECUTE EnuTotReg USING p_folio,"A" INTO tot_altas
   EXECUTE EnuTotReg USING p_folio,"B" INTO tot_bajas
   EXECUTE EnuTotReg USING p_folio,"M" INTO tot_modificaciones
   EXECUTE EnuTotReg USING p_folio,"R" INTO tot_reactivacion

   DISPLAY "TOTAL ALTAS: ",tot_altas
   DISPLAY "TOTAL BAJAS: ",tot_bajas
   DISPLAY "TOTAL MODIFICACIONES: ",tot_modificaciones
   DISPLAY "TOTAL REACTIVACIONES: ",tot_reactivacion
   
   DISPLAY "PROCESANDO VALIDACION DE INSTRUCCIONES DE MANDATO"
   
   -- asigna el folio en la variable de folio liquidaci�n
   LET v_c_programa_cod = "HPSP22"

   UPDATE bat_ctr_operacion 
   SET nom_archivo = p_nom_archivo ,
       folio       = p_folio
   WHERE pid  =  p_d_pid
   AND   proceso_cod = p_i_proceso_cod   
   AND   opera_cod = p_i_opera_cod   

   LET v_s_qryTxt = " SELECT cat.desc_mandato ",
                    "   FROM mdt_cat_mandato cat JOIN mdt_cat_mandato_paquete pte",
                    "     ON cat.id_cat_mandato = pte.id_cat_mandato",
                    "  WHERE pte.cve_mandato = ? "
                    
   PREPARE EnuRegMDT FROM v_s_qryTxt

   LET v_s_qryTxt = " SELECT * ",
                    "   FROM hps_solicitud_mandato",
                    "  WHERE estado = 101 ",
                    "    AND folio = ? "
                    
   PREPARE EnuRegProc FROM v_s_qryTxt
   DECLARE CurRegProc CURSOR FOR EnuRegProc
   
   LET v_reg_102_a = 0
   LET v_reg_106_a = 0
   LET v_reg_102_b = 0
   LET v_reg_106_b = 0
   LET v_reg_102_m = 0
   LET v_reg_106_m = 0
   LET v_reg_102_r = 0
   LET v_reg_106_r = 0
   LET tot_altas_proc          = 0
   LET tot_bajas_proc          = 0
   LET tot_modificaciones_proc = 0
   LET tot_reactivacion_proc   = 0
   LET v_pos_rep_det = 1

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_hps_alta_mandato(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la alta de mandatos
   PREPARE prp_alta_mdt FROM v_s_qryTxt

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_hps_baja_mandato(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la baja de mandatos
   PREPARE prp_baja_mdt FROM v_s_qryTxt

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_hps_modifica_mandato(?,?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la modificacion y reactivacion de mandatos
   PREPARE prp_modi_mdt FROM v_s_qryTxt
DISPLAY "folio: ",p_folio
   FOREACH CurRegProc USING p_folio INTO v_r_mandato.*
DISPLAY "reg: ",v_r_mandato.*
      CASE v_r_mandato.tipo_operacion
         WHEN "A"
            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
DISPLAY "v_mandato_desc: ",v_mandato_desc            
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato 
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1
            
DISPLAY "id_solicitud_mandato: ",v_r_mandato.id_solicitud_mandato            
DISPLAY "id_credito: ",v_r_mandato.id_credito
DISPLAY "id_solicitud_mandato: ",v_r_mandato.id_derechohabiente

            EXECUTE prp_alta_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       p_v_usuario
                                 INTO v_sp_estado
DISPLAY "v_sp_estado: ",v_sp_estado                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_a = v_reg_102_a + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  INITIALIZE v_r_mandato.diagnostico TO NULL
                  SELECT diagnostico INTO v_r_mandato.diagnostico                  
                    FROM hps_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato

                  IF(v_r_mandato.diagnostico IS NULL)THEN
                     LET v_r_mandato.diagnostico = '000'
                  END IF
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico
                  
                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_a = v_reg_106_a + 1
               END IF
               LET tot_altas_proc = tot_altas_proc + 1
            ELSE
            
               UPDATE hps_lote_mandato 
               SET    estado = 106 
               WHERE  folio = p_folio 
               
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion alta: ",v_sp_estado
            
               DISPLAY "Program stopped at 'HPSP22.4gl' sp alta"
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         WHEN "B"

            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato
            
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1

            EXECUTE prp_baja_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       p_v_usuario
                                 INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_b = v_reg_102_b + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  INITIALIZE v_r_mandato.diagnostico TO NULL
                  SELECT diagnostico INTO v_r_mandato.diagnostico
                    FROM hps_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato
                  IF(v_r_mandato.diagnostico IS NULL)THEN
                     LET v_r_mandato.diagnostico = '000'
                  END IF
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico

                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_b = v_reg_106_b + 1
               END IF
               LET tot_bajas_proc = tot_bajas_proc + 1
            ELSE
               UPDATE hps_lote_mandato 
               SET    estado = 106 
               WHERE  folio = p_folio 
            
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion baja: ",v_sp_estado
               
               DISPLAY "Program stopped at 'HPSP22.4gl' sp baja"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         WHEN "M"

            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1

            EXECUTE prp_modi_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       v_r_mandato.tipo_operacion      ,
                                       p_v_usuario  
                                    INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_m = v_reg_102_m + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  SELECT diagnostico INTO v_r_mandato.diagnostico
                    FROM hps_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato
                  
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico

                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_m = v_reg_106_m + 1
               END IF
               LET tot_modificaciones_proc = tot_modificaciones_proc + 1
            ELSE

               UPDATE hps_lote_mandato 
               SET    estado = 106 
               WHERE  folio = p_folio 
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion mod : ",v_sp_estado
               
               DISPLAY "Program stopped at 'HPSP22.4gl' sp modificacion"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         WHEN "R"
            EXECUTE prp_modi_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       v_r_mandato.tipo_operacion
                                 INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  LET v_reg_102_r = v_reg_102_r + 1
               END IF
               IF v_sp_estado = 106 THEN
                  LET v_reg_106_r = v_reg_106_r + 1
               END IF
               LET tot_reactivacion_proc = tot_reactivacion_proc + 1
            ELSE

               UPDATE hps_lote_mandato 
               SET    estado = 106 
               WHERE  folio = p_folio 
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion reac: ",v_sp_estado
               
               DISPLAY "Program stopped at 'HPSP22.4gl' sp reactivacion"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         
      END CASE
      
   END FOREACH
   LET v_pos_rep_det_fin = v_pos_rep_det
   
   DISPLAY "TOTAL DE REGISTROS PROCESADOS: ",tot_altas_proc+tot_bajas_proc+tot_modificaciones_proc+tot_reactivacion_proc
   -- ahm temp poR GRUPO ESTADO (102 y 106)?????????????
   DISPLAY "TOTAL DE ALTAS PROCESADAS:          ",tot_altas_proc
   DISPLAY "TOTAL DE BAJAS PROCESADAS:          ",tot_bajas_proc
   DISPLAY "TOTAL DE MODIFICACIONES PROCESADAS: ",tot_modificaciones_proc
   DISPLAY "TOTAL DE REACTIVACIONES PROCESADAS: ",tot_reactivacion_proc
   DISPLAY "REGISTROS VALIDADOS: ",v_reg_102_a+v_reg_102_b+v_reg_102_m+v_reg_102_r
   DISPLAY "REGISTROS RECHAZADOS: ",v_reg_106_a+v_reg_106_b+v_reg_106_m+v_reg_106_r
   DISPLAY "FIN DEL PROCESO: ",DATE

   IF v_reg_106_a+v_reg_106_b+v_reg_106_m+v_reg_106_r > 0 THEN
      UPDATE hps_lote_mandato
        SET estado = 102
        WHERE folio = p_folio -- AHM TMP Validarlo si es por fecha
   ELSE
      UPDATE hps_lote_mandato
        SET estado = 103
        WHERE folio = p_folio -- AHM TMP Validarlo si es por fecha
   END IF
   
--DISPLAY p_d_pid,p_i_proceso_cod, p_i_opera_cod
   # Finaliza la operacion 3 (validaci�n de acreditados recurrentes)
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
--DISPLAY r_b_valida
   
   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "Program stopped ERROR en fn_actualiza_opera_fin Op: ",p_i_opera_cod
   ELSE

      -- recupera la ruta de listados en el que se enviara el archivo
      CALL fn_rutas("hps") RETURNING r_ruta_bin, r_ruta_listados
      --DISPLAY "Ruta bin - ", r_ruta_bin
      --DISPLAY "Ruta lst - ", r_ruta_listados

      LET v_r_rpt_res_edo.altas_aceptadas  = v_reg_102_a
      LET v_r_rpt_res_edo.bajas_aceptadas  = v_reg_102_b
      LET v_r_rpt_res_edo.modif_aceptadas  = v_reg_102_m
      LET v_r_rpt_res_edo.altas_rechazadas = v_reg_106_a
      LET v_r_rpt_res_edo.bajas_rechazadas = v_reg_106_b
      LET v_r_rpt_res_edo.modif_rechazadas = v_reg_106_m


      -- Recupera descripcion del tipo de origen
      SELECT des_origen 
        INTO v_r_rpt_res.des_origen 
        FROM mdt_cat_origen
      WHERE id_origen = 1
      
      -- Recupera descripcion del proceso
      SELECT proceso_desc 
        INTO v_r_rpt_res.proceso_desc 
        FROM cat_proceso
      WHERE proceso_cod = p_i_proceso_cod

        LET v_r_rpt_res.folio = p_folio
      SELECT f_proceso
        INTO v_f_proceso
        FROM hps_lote_mandato
       WHERE folio = p_folio
      
      LET v_day = DAY(v_f_proceso) USING "&&"
      LET v_mes = MONTH(v_f_proceso) USING "&&"
      LET v_ano = YEAR(v_f_proceso) USING "&&&&"
      LET v_r_rpt_res.f_proceso = v_day,"-",v_mes,"-",v_ano
      LET v_r_rpt_res.altas  = tot_altas_proc
      LET v_r_rpt_res.bajas  = tot_bajas_proc
      LET v_r_rpt_res.modif  = tot_modificaciones_proc

      -- Carga de arreglos de aceptadas y rechazadas
      LET v_s_qryTxt = "\n SELECT a.estado, m.desc_tpo_mandato, a.tipo_operacion, count(*)",
                       "\n   FROM tmp_procesa_mdt a LEFT OUTER JOIN mdt_cat_mandato_paquete b",
                       "\n     ON a.cve_mandato = b.cve_mandato",
                       "\n        LEFT OUTER JOIN mdt_cat_mandato c",
                       "\n     ON c.id_cat_mandato = b.id_cat_mandato",
                       "\n        LEFT OUTER JOIN mdt_tpo_mandato m",
                       "\n     ON a.cve_mandato[2] = m.tpo_mandato",
                       "\n  --WHERE ",
                       "\n  GROUP BY 1,2,3 ",
                       "\n  ORDER BY estado, desc_tpo_mandato, tipo_operacion"
	    
	    PREPARE EnuAgrupaMDT FROM v_s_qryTxt
      DECLARE CurAgrupaMDT CURSOR FOR EnuAgrupaMDT
      
      LET v_pos_acep = 1
      LET v_pos_rech = 1
      CALL v_r_rpt_aceptadas.clear()
      CALL v_r_rpt_canceladas.clear()
      FOREACH CurAgrupaMDT INTO v_estado, v_tpo_mandato, v_tipo_operacion, v_tot_mdt
         IF v_estado = 'A' THEN
            IF v_pos_acep < 13 THEN
               IF(v_tpo_mandato IS NULL OR v_tpo_mandato = ' ')THEN
               
               END IF
               LET v_r_rpt_aceptadas[v_pos_acep].tpo_mandato = v_tpo_mandato
               CASE v_tipo_operacion 
                  WHEN 'A' 
                     LET v_tipo_operacion_desc = "ALTA"
                  WHEN 'B' 
                     LET v_tipo_operacion_desc = "BAJA"
                  WHEN 'M' 
                     LET v_tipo_operacion_desc = "MODIFICACION"
                  OTHERWISE 
                     LET v_tipo_operacion_desc = ""
               END CASE
               LET v_r_rpt_aceptadas[v_pos_acep].operacion   = v_tipo_operacion_desc
               LET v_r_rpt_aceptadas[v_pos_acep].total_mdt   = v_tot_mdt
               --DISPLAY "Aceptadas[",v_pos_acep,": ",v_r_rpt_aceptadas[v_pos_acep].*
               LET v_pos_acep = v_pos_acep + 1
            END IF
         END IF
         IF v_estado = 'R' THEN
            IF v_pos_rech < 13 THEN
               LET v_r_rpt_canceladas[v_pos_rech].tpo_mandato = v_tpo_mandato
               CASE v_tipo_operacion 
                  WHEN 'A' 
                     LET v_tipo_operacion_desc = "ALTA"
                  WHEN 'B' 
                     LET v_tipo_operacion_desc = "BAJA"
                  WHEN 'M' 
                     LET v_tipo_operacion_desc = "MODIFICACION"
                  OTHERWISE 
                     LET v_tipo_operacion_desc = ""
               END CASE
               LET v_r_rpt_canceladas[v_pos_rech].operacion   = v_tipo_operacion_desc
               LET v_r_rpt_canceladas[v_pos_rech].total_mdt   = v_tot_mdt
               --DISPLAY "Rechazadas[",v_pos_rech,": ",v_r_rpt_canceladas[v_pos_rech].*
               LET v_pos_rech = v_pos_rech + 1
            END IF
         END IF
      END FOREACH
      
      --Borra tabla temporal 
      EXECUTE EnuTmpProcDROP 

      --DISPLAY "v_pos_acep: ", v_pos_acep
      --DISPLAY "v_pos_rech: ", v_pos_rech
      
      IF v_pos_rep_det_fin = 1 THEN
         CALL v_r_reporte_det.clear() 
         LET v_r_reporte_det[1].nss = '  '
         CALL fn_cierra_proceso_mdt_batch(p_v_usuario)
      END IF
      # se quita esta condicion para poder generar el reporte
      --IF(v_pos_rep_det_fin > 1)THEN
         -- se indica que el reporte usara la plantilla creada
         
         LET v_prog_reporte = r_ruta_bin CLIPPED, "/HPSG011.4rp"
         
         IF fgl_report_loadCurrentSettings(v_prog_reporte) THEN
         
            -- se indica la salida del reporte
            CALL fgl_report_selectDevice("PDF")
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
            -- sin indica que no es necesario el preview
            CALL fgl_report_selectPreview(0)
             -- se asigna la configuraci�n en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
         ELSE
            DISPLAY "no fue posible generar el reporte"
         END IF
      
         -- inicia el reporte de registros procesados
         START REPORT rpt_solicitudes_mandatos TO XML HANDLER v_manejador_rpt
DISPLAY "v_pos_rep_det_fin: ",v_pos_rep_det_fin
         FOR v_pos_rep_det = 1 TO (v_pos_rep_det_fin - 1) -- AHM 20120416 v_r_reporte_det.getLength()
            OUTPUT TO REPORT rpt_solicitudes_mandatos(v_r_rpt_res.*, v_r_rpt_res_edo.*,
                                                  v_r_reporte_det[v_pos_rep_det].*)
DISPLAY "v_r_rpt_res: ",v_r_rpt_res.*
DISPLAY ""
DISPLAY "det: ",v_r_reporte_det[v_pos_rep_det].*
DISPLAY ""
DISPLAY "v_r_rpt_res_edo: ",v_r_rpt_res_edo.*

         END FOR
         OUTPUT TO REPORT rpt_solicitudes_mandatos(v_r_rpt_res.*, v_r_rpt_res_edo.*,
                                               " "," "," ")
                                               
DISPLAY "v_r_rpt_res: ",v_r_rpt_res.*
DISPLAY ""
DISPLAY "det: ",v_r_reporte_det[v_pos_rep_det].*
DISPLAY ""
DISPLAY "v_r_rpt_res_edo: ",v_r_rpt_res_edo.*
      
         -- finaliza el reporte
         FINISH REPORT rpt_solicitudes_mandatos

         -- Env�o de correo de notificaci�n de proceso finalizado
         CALL fn_correo_proceso(p_d_pid, 
                                p_i_proceso_cod, 
                                p_i_opera_cod, 
                                '', -- TMP AHM adjunto ?
                                'Registrar instrucciones de mandatos - Origen recurrente',
                             'ID Proceso   : '||p_d_pid||
                             'Proceso      : '||p_i_proceso_cod||
                             'Operacion    : '||p_i_opera_cod||
                             'Fecha Inicio : '||p_fec_ejecucion||
                             'Fecha Fin    : '||DATE
                             )

   END IF
END MAIN

####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSP22                                        #
#Descripcion       =>Genera el cierre autom�tico del proceso cuando#
#                    no hay rechazos en la operaci�n anterior      #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>25 Abril 2012                                 #
####################################################################
FUNCTION fn_cierra_proceso_mdt_batch(p_v_usuario)
   DEFINE p_v_usuario          char(20)
   DEFINE v_i_opera_cod        LIKE cat_operacion.opera_cod -- operaci�n que llama la funcion

   DEFINE v_hora_actual DATETIME HOUR TO MINUTE
          
   -- se inicializan las variables
   # En caso de no haber rechazos se finaiza en autom�tico la 
   # operaci�n 4 de rechazos
   
   LET v_i_opera_cod = 4 -- genera archivo mandatos
   
   LET v_hora_actual = CURRENT YEAR TO MINUTE

   UPDATE bat_ctr_operacion
      SET fecha_fin   = v_hora_actual ,
          estado_cod  = 4
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = v_i_opera_cod

   UPDATE bat_ctr_proceso
      SET fecha_fin   = TODAY,
          estado_cod  = 4
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod

END FUNCTION
                                                                                                                                                                   HPSR34.4gl                                                                                          0000777 0000212 0001751 00000015516 13113322765 012333  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013 
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSR34                                                   #
#Objetivo          => Batch reverso de preliquidacion pago mandatos            #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 12 Julio 2013                                            #
#Modificado        =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Usuario
       p_pid_rev         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod_rev LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod_rev   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio           LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       v_pid             LIKE bat_ctr_proceso.pid,     # identificador de proceso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # C�digo del proceso
       v_opera_cod       LIKE cat_operacion.opera_cod # C�digo de la operacion

MAIN
DEFINE v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_opera_desc     LIKE cat_operacion.opera_desc,
       v_consulta       STRING,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_sql_error      INTEGER,
       v_isam_error     SMALLINT,
       v_msg_error      CHAR(100),
       v_tot_reversados INTEGER,
       v_mensaje        STRING,
       r_res_opera      SMALLINT,
       v_bnd_rev_cnt    SMALLINT

   # Se recuperan los par�metros
   LET p_usuario_cod     = ARG_VAL(1)
   LET p_pid_rev         = ARG_VAL(2)
   LET p_proceso_cod_rev = ARG_VAL(3)
   LET p_opera_cod_rev   = ARG_VAL(4)
   LET p_folio           = ARG_VAL(5)
   LET p_nom_archivo     = ARG_VAL(6)

   # proceso y operaciones iniciales
   LET v_proceso_cod = g_proceso_cod_pago_mandatos # Pago mandatos
   LET v_opera_cod   = g_opera_cod_carga     # Preliquidaci�n pago mandatos
   
   # Valida si se puede ejecutar reverso para registro contable
   --CALL fn_reverso_reg_cnt(p_folio) RETURNING v_bnd_rev_cnt
   LET v_bnd_rev_cnt = 0
   IF( v_bnd_rev_cnt )THEN
      DISPLAY "Reverso no procedente: registro contable realizado"
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF( r_res_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   ELSE

      # Recuepra descripcion de proceso y operacion de reverso
      CALL fn_proceso_cod_desc(p_proceso_cod_rev) RETURNING v_proceso_desc
      CALL fn_opera_cod_desc(p_proceso_cod_rev, p_opera_cod_rev) RETURNING v_opera_desc
      DISPLAY "\n"
      DISPLAY "PROCESO      :",v_proceso_desc
      DISPLAY "OPERACI�N    :",v_opera_desc
      DISPLAY "FOLIO        :",p_folio
      DISPLAY "\n"

      # recupera el pid que le corresponde al proceso y folio
      SELECT pid
        INTO v_pid
        FROM bat_ctr_operacion
       WHERE proceso_cod = v_proceso_cod
         AND opera_cod   = v_opera_cod
         AND folio       = p_folio

      LET v_consulta = "EXECUTE FUNCTION fn_hps_r_preliquidar_pagos(?,?,?,?,?)"
      PREPARE prp_reverso_integracion FROM v_consulta
      EXECUTE prp_reverso_integracion USING p_folio,
                                            v_pid,
                                            v_proceso_cod,
                                            v_opera_cod,
                                            p_usuario_cod
                                       INTO v_ind,
                                            v_diag,
                                            v_sql_error,
                                            v_isam_error,
                                            v_msg_error,
                                            v_tot_reversados
      
      IF( v_sql_error <> 0 )THEN
         DISPLAY "\nError en ejecuci�n de SP (Codigo): ",v_sql_error
         DISPLAY "Error en SP (Mensaje):",v_msg_error,"\n"
         LET v_mensaje = "Error al realizar el reverso. C�digo: ",v_sql_error,", mensaje: ",v_msg_error
         CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
         IF( r_res_opera <> 0 )THEN
            CALL fn_desplega_inc_operacion(r_res_opera)
         END IF
      ELSE
         IF( v_ind <> 0 )THEN
            LET v_mensaje = "Error al realizar el reverso. "
            # Muestra las incosistencias      
            DISPLAY "\nError en ejecuci�n de SP: "
            CASE v_diag
               WHEN "001" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
               WHEN "002" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
            END CASE
            CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
            IF ( r_res_opera <> 0 ) THEN
               CALL fn_desplega_inc_operacion(r_res_opera)
            END IF
         ELSE
            LET v_mensaje = "Operaci�n finalzada correctamente"
            # imprime totales reversados
            DISPLAY "\nTOTAL REGISTROS REVERSADOS: ",v_tot_reversados
            DISPLAY "\n"
            # finaliza operacion de reverso
            CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
               IF ( r_res_opera  <> 0 ) THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(r_res_opera)
                  # trata de establecer erronea la operacion
                  CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
                  IF ( r_res_opera  <> 0 ) THEN
                     # Imprime el mensaje de inconsistencia en consola
                     CALL fn_desplega_inc_operacion(r_res_opera)
                  END IF
               END IF
         END IF
      END IF
   END IF
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reverso de liquidaci�n pagos servicios hps',
                          v_mensaje
                          )

END MAIN
                                                                                                                                                                                  HPSR35.4gl                                                                                          0000777 0000212 0001751 00000015516 13113322765 012334  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013 
--==============================================================================

################################################################################
#Modulo            => HPS                                                      #
#Programa          => HPSR35                                                   #
#Objetivo          => Batch reverso de liquidacion pago mandatos               #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 12 Julio 2013                                            #
#Modificado        =>                                                          #
################################################################################
DATABASE safre_viv
GLOBALS "HPSG02.4gl"

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Usuario
       p_pid_rev         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod_rev LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod_rev   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio           LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo,
       v_pid             LIKE bat_ctr_proceso.pid,     # identificador de proceso
       v_proceso_cod     LIKE cat_proceso.proceso_cod, # C�digo del proceso
       v_opera_cod       LIKE cat_operacion.opera_cod # C�digo de la operacion

MAIN
DEFINE v_proceso_desc   LIKE cat_proceso.proceso_desc,
       v_opera_desc     LIKE cat_operacion.opera_desc,
       v_consulta       STRING,
       v_ind            SMALLINT,
       v_diag           CHAR(3),
       v_sql_error      INTEGER,
       v_isam_error     SMALLINT,
       v_msg_error      CHAR(100),
       v_tot_reversados INTEGER,
       v_mensaje        STRING,
       r_res_opera      SMALLINT,
       v_bnd_rev_cnt    SMALLINT

   # Se recuperan los par�metros
   LET p_usuario_cod     = ARG_VAL(1)
   LET p_pid_rev         = ARG_VAL(2)
   LET p_proceso_cod_rev = ARG_VAL(3)
   LET p_opera_cod_rev   = ARG_VAL(4)
   LET p_folio           = ARG_VAL(5)
   LET p_nom_archivo     = ARG_VAL(6)

   # proceso y operaciones iniciales
   LET v_proceso_cod = g_proceso_cod_pago_mandatos # Pago mandatos
   LET v_opera_cod   = g_opera_cod_integracion     # liquidacion pago mandatos
   
   # Valida si se puede ejecutar reverso para registro contable
   --CALL fn_reverso_reg_cnt(p_folio) RETURNING v_bnd_rev_cnt
   LET v_bnd_rev_cnt = 0
   IF( v_bnd_rev_cnt )THEN
      DISPLAY "Reverso no procedente: registro contable realizado"
      CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
      IF( r_res_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_res_opera)
      END IF
   ELSE

      # Recuepra descripcion de proceso y operacion de reverso
      CALL fn_proceso_cod_desc(p_proceso_cod_rev) RETURNING v_proceso_desc
      CALL fn_opera_cod_desc(p_proceso_cod_rev, p_opera_cod_rev) RETURNING v_opera_desc
      DISPLAY "\n"
      DISPLAY "PROCESO      :",v_proceso_desc
      DISPLAY "OPERACI�N    :",v_opera_desc
      DISPLAY "FOLIO        :",p_folio
      DISPLAY "\n"

      # recupera el pid que le corresponde al proceso y folio
      SELECT pid
        INTO v_pid
        FROM bat_ctr_operacion
       WHERE proceso_cod = v_proceso_cod
         AND opera_cod   = v_opera_cod
         AND folio       = p_folio

      LET v_consulta = "EXECUTE FUNCTION fn_hps_r_liquidar_pagos(?,?,?,?,?)"
      PREPARE prp_reverso_integracion FROM v_consulta
      EXECUTE prp_reverso_integracion USING p_folio,
                                            v_pid,
                                            v_proceso_cod,
                                            v_opera_cod,
                                            p_usuario_cod
                                       INTO v_ind,
                                            v_diag,
                                            v_sql_error,
                                            v_isam_error,
                                            v_msg_error,
                                            v_tot_reversados
      
      IF( v_sql_error <> 0 )THEN
         DISPLAY "\nError en ejecuci�n de SP (Codigo): ",v_sql_error
         DISPLAY "Error en SP (Mensaje):",v_msg_error,"\n"
         LET v_mensaje = "Error al realizar el reverso. C�digo: ",v_sql_error,", mensaje: ",v_msg_error
         CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
         IF( r_res_opera <> 0 )THEN
            CALL fn_desplega_inc_operacion(r_res_opera)
         END IF
      ELSE
         IF( v_ind <> 0 )THEN
            LET v_mensaje = "Error al realizar el reverso. "
            # Muestra las incosistencias      
            DISPLAY "\nError en ejecuci�n de SP: "
            CASE v_diag
               WHEN "001" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
               WHEN "002" 
                 DISPLAY v_msg_error," \n"
                 LET v_mensaje = v_msg_error
            END CASE
            CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
            IF ( r_res_opera <> 0 ) THEN
               CALL fn_desplega_inc_operacion(r_res_opera)
            END IF
         ELSE
            LET v_mensaje = "Operaci�n finalzada correctamente"
            # imprime totales reversados
            DISPLAY "\nTOTAL REGISTROS REVERSADOS: ",v_tot_reversados
            DISPLAY "\n"
            # finaliza operacion de reverso
            CALL fn_actualiza_opera_fin(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
               IF ( r_res_opera  <> 0 ) THEN
                  # Imprime el mensaje de inconsistencia en consola
                  CALL fn_desplega_inc_operacion(r_res_opera)
                  # trata de establecer erronea la operacion
                  CALL fn_error_opera(p_pid_rev,p_proceso_cod_rev,p_opera_cod_rev) RETURNING r_res_opera
                  IF ( r_res_opera  <> 0 ) THEN
                     # Imprime el mensaje de inconsistencia en consola
                     CALL fn_desplega_inc_operacion(r_res_opera)
                  END IF
               END IF
         END IF
      END IF
   END IF
   CALL fn_correo_proceso(p_pid_rev, 
                          p_proceso_cod_rev, 
                          p_opera_cod_rev, 
                          '', # Archivo adjunto
                          'Finalizaci�n de operaci�n - '||v_proceso_desc CLIPPED||' - Reverso de liquidaci�n pagos servicios hps',
                          v_mensaje
                          )

END MAIN
                                                                                                                                                                                  HPSWS01.4gl                                                                                         0000777 0000212 0001751 00000004715 13113322755 012453  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               ####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS05.4gl                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    asi como la publicacion del webServices de    #
#                    credito ejercido                              #
#Fecha inicio      =>18 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "HPSWS01.inc"

MAIN
    DEFINE servicio     INTEGER
    DEFINE respuesta    INTEGER

   CALL CreateInformaCreditoEjercidoServiciosService() RETURNING servicio

   CALL com.WebServiceEngine.Start()

   DISPLAY("The server is listening.")

   WHILE TRUE
      # Procesa cada request, regresa un entero que representa el estatus del request
      # el parametro -1 representa un valor infinito de espera
      LET respuesta = com.WebServiceEngine.ProcessServices(-1)
      CASE respuesta
         WHEN 0
            --DISPLAY "Request processed."
         WHEN -1
            DISPLAY "Timeout reached."
         WHEN -2
            DISPLAY "Disconnected from application server."
            EXIT PROGRAM
         WHEN -3
            DISPLAY "Client Connection lost."
         WHEN -4
            DISPLAY "Server interrupted with Ctrl-C."
         WHEN -10
            DISPLAY "Internal server error."
     END CASE

     IF int_flag<>0 THEN
        LET int_flag=0
        EXIT WHILE
     END IF
   END WHILE
END MAIN

FUNCTION CreateInformaCreditoEjercidoServiciosService()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("InformaCreditoEjercidoServiciosServices","http://services.safre.efp.com")

    # Publish Operation : informarCreditoEjercido
    LET operation = com.WebOperation.CreateDOCStyle("informarCreditoEjercidoServicios","informarCreditoEjercidoServicios",ns1informarCreditoEjercido,ns1informarCreditoEjercidoResponse)
    CALL service.publishOperation(operation,"")

    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

FUNCTION informarCreditoEjercidoServicios()
   CALL fn_informar_credito_ejercido_servicios()
END FUNCTION
                                                   HPSWS02.4gl                                                                                         0000777 0000212 0001751 00000016545 13113322756 012461  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               ####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS06.4gl                                   #
#Objetivo          =>Programa que contiene la implementacion del   #
#                    webServices que expone el servicio de         #
#                    credito ejercido                              #
#Fecha inicio      =>18 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT XML

DATABASE safre_viv

GLOBALS "HPSWS01.inc"

PRIVATE DEFINE v_error              SMALLINT


# variables de origienacion
PRIVATE DEFINE v_nss                CHAR(11) 
PRIVATE DEFINE v_tipo_credito       CHAR(3) 
PRIVATE DEFINE v_numero_credito     DECIMAL(10,0) 
PRIVATE DEFINE v_fecha_otorgamiento DATE 
PRIVATE DEFINE v_tipo_descuento     SMALLINT 
PRIVATE DEFINE v_valor_descuento    DECIMAL(8,0) 
PRIVATE DEFINE v_monto_liquida      DECIMAL(15,2)
PRIVATE DEFINE v_fecha_proceso      DATE 
PRIVATE DEFINE v_nrp                CHAR(11) 
PRIVATE DEFINE v_tipo_operacion     CHAR(3)

# variables de mandatos (hps)

PRIVATE DEFINE v_ind_alta_servicios             SMALLINT
PRIVATE DEFINE v_cve_mandato_predial            CHAR(18)
PRIVATE DEFINE v_f_ini_predial                  DATE
PRIVATE DEFINE v_f_fin_predial                  DATE
PRIVATE DEFINE v_mto_fondo_predial              DECIMAL(15,2)
PRIVATE DEFINE v_f_primer_pago_predial          DATE
PRIVATE DEFINE v_mto_primer_pago_predial        DECIMAL(15,2)
PRIVATE DEFINE v_referencia_predial             CHAR(40)

PRIVATE DEFINE v_cve_mandato_conservacion       CHAR(18)
PRIVATE DEFINE v_f_ini_conservacion             DATE
PRIVATE DEFINE v_f_fin_conservacion             DATE
PRIVATE DEFINE v_mto_fondo_conservacion         DECIMAL(15,2)
PRIVATE DEFINE v_f_primer_pago_conservacion     DATE
PRIVATE DEFINE v_mto_primer_pago_conservacion   DECIMAL(15,2)
PRIVATE DEFINE v_referencia_conservacion        CHAR(40)

PRIVATE DEFINE v_qryTxt             STRING
 
#variables de salida
PRIVATE DEFINE v_codResp            CHAR(4)
PRIVATE DEFINE v_descResp           VARCHAR(140)



FUNCTION fn_informar_credito_ejercido_servicios()
   #En esta funcion implementar el negocio
   #Los parametros de entrada esta en la variable ns2request.nss
   LET v_nss                        =   ns1informarCreditoEjercido.request.nss
   LET v_tipo_credito               =   ns1informarCreditoEjercido.request.tipoCredito
   LET v_numero_credito             =   ns1informarCreditoEjercido.request.numeroCredito
   LET v_fecha_otorgamiento         =   ns1informarCreditoEjercido.request.fechaOtorgamiento
   LET v_tipo_descuento             =   ns1informarCreditoEjercido.request.tipoDescuento
   LET v_valor_descuento            =   ns1informarCreditoEjercido.request.valorDescuento
   LET v_monto_liquida              =   ns1informarCreditoEjercido.request.montoLiquida
   LET v_fecha_proceso              =   ns1informarCreditoEjercido.request.fechaProceso
   LET v_nrp                        =   ns1informarCreditoEjercido.request.nrp
   LET v_tipo_operacion             =   ns1informarCreditoEjercido.request.tipoOperacion
   LET v_ind_alta_servicios         =   ns1informarCreditoEjercido.request.identificadorAltaMandato
   LET v_cve_mandato_predial        =   ns1informarCreditoEjercido.request.identificadorAltaPredial
   LET v_f_ini_predial              =   ns1informarCreditoEjercido.request.fechaInicioPredial
   LET v_f_fin_predial              =   ns1informarCreditoEjercido.request.fechaFinPredial
   LET v_mto_fondo_predial          =   ns1informarCreditoEjercido.request.montoFondoPredial
   LET v_f_primer_pago_predial      =   ns1informarCreditoEjercido.request.fechaPrimerPagoPredial
   LET v_mto_primer_pago_predial    =   ns1informarCreditoEjercido.request.montoPrimerPagoPredial
   LET v_referencia_predial         =   ns1informarCreditoEjercido.request.referenciaPagoPredial
   LET v_cve_mandato_conservacion        =   ns1informarCreditoEjercido.request.identificadorAltaConservacion
   LET v_f_ini_conservacion              =   ns1informarCreditoEjercido.request.fechaInicioConservacion
   LET v_f_fin_conservacion              =   ns1informarCreditoEjercido.request.fechaFinConservacion
   LET v_mto_fondo_conservacion          =   ns1informarCreditoEjercido.request.montoFondoConservacion
   LET v_f_primer_pago_conservacion      =   ns1informarCreditoEjercido.request.fechaPrimerPagoConservacion
   LET v_mto_primer_pago_conservacion    =   ns1informarCreditoEjercido.request.montoPrimerPagoConservacion
   LET v_referencia_conservacion         =   ns1informarCreditoEjercido.request.referenciaPagoConservacion


   --LET v_qryTxt = "EXECUTE FUNCTION fn_credito_ejercido(?,?,?,?,?,?,?,?,?,?)"
   LET v_qryTxt = "EXECUTE FUNCTION sp_hps_registra_servicio(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

   PREPARE prp_consulta FROM v_qryTxt
   EXECUTE prp_consulta USING v_nss               ,
                              v_tipo_credito      ,
                              v_numero_credito    ,
                              v_fecha_otorgamiento,
                              v_tipo_descuento    ,
                              v_valor_descuento   ,
                              v_monto_liquida     ,
                              v_fecha_proceso     ,
                              v_nrp               ,
                              v_tipo_operacion                  ,
                              v_ind_alta_servicios              ,
                              v_cve_mandato_predial             ,
                              v_f_ini_predial                   ,
                              v_f_fin_predial                   ,
                              v_mto_fondo_predial               ,
                              v_f_primer_pago_predial           ,
                              v_mto_primer_pago_predial         ,
                              v_referencia_predial              ,
                              v_cve_mandato_conservacion        ,
                              v_f_ini_conservacion              ,
                              v_f_fin_conservacion              ,
                              v_mto_fondo_conservacion          ,
                              v_f_primer_pago_conservacion      ,
                              v_mto_primer_pago_conservacion    ,
                              v_referencia_conservacion     

             INTO v_error, v_nss, v_codResp, v_descResp, v_numero_credito, v_tipo_credito 


   #Antes de terminar la funcion llenar las siguientes variables de salida
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.nss               = v_nss
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.codigoRespuesta   = v_codResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.descripcion       = v_descResp
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.numeroCredito     = v_numero_credito
   LET ns1informarCreditoEjercidoResponse.informarCreditoEjercidoReturn.tipoCredito       = v_tipo_credito
   #Esta funcion no utiliza RETURN, solo se tiene que llenar las variables de salida y terminar la funcion
END FUNCTION
                                                                                                                                                           HPSWS03.4gl                                                                                         0000777 0000212 0001751 00000006615 13113322756 012457  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/08/2015
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSWS03                                                       #
#Objetivo     => Registra solicitud de cancelaci�n de pagos                    #
#Fecha inicio => 10 Agosto 2015                                                #
################################################################################

IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "HPSWS03.inc"

MAIN
DEFINE v_respuesta_ws INTEGER
   
   LET g_usuario_cod = "SAFREVIV" # Por no intervenir un usuario especifico, se usa safreviv
   
   CALL fn_inicializa_consultas()
   
   CALL fn_crea_servicio() RETURNING v_respuesta_ws

   # Puerto 
   CALL com.WebServiceEngine.Start()
   DISPLAY "INICIA WS SOLICITUD CANCELACI�N PAGOS",CURRENT YEAR TO SECOND

   WHILE TRUE
      # Process each incoming requests (infinite loop)
      LET v_respuesta_ws = com.WebServiceEngine.ProcessServices(-1)
      CASE v_respuesta_ws
         WHEN 0
            DISPLAY "Respuesta WS procesada. ",CURRENT YEAR TO SECOND
        
         WHEN -1
            DISPLAY "Tiempo de espera terminado. ",CURRENT YEAR TO SECOND
        
         WHEN -2
            DISPLAY "Desconectado desde el servidor de aplicaci�n. ",CURRENT YEAR TO SECOND
            EXIT PROGRAM 
        
         WHEN -3
            DISPLAY "Conexi�n del cliente perdida. ",CURRENT YEAR TO SECOND
        
         WHEN -4
            DISPLAY "Servidor interrumpido con Ctrl-C. ",CURRENT YEAR TO SECOND
        
         WHEN -10
            DISPLAY "Error interno de servidor. ",CURRENT YEAR TO SECOND
        
      END CASE
      
      IF( INT_FLAG <> 0 )THEN
         LET INT_FLAG = 0
         EXIT WHILE
      END IF
  
   END WHILE

DISPLAY "Server stopped"
   
END MAIN

#-------------------------------------------------------------------------------
# Service: SolicitudCancelacionPagosService
# Port:    SolicitudCancelacionPagosS
#-------------------------------------------------------------------------------
#
# FUNCTION Createuntitled-1Service
#   RETURNING soapstatus
#
FUNCTION fn_crea_servicio()
  DEFINE service      com.WebService
  DEFINE operation    com.WebOperation

  TRY
    # Create Web Service
    LET service = com.WebService.CreateWebService("SolicitudCancelacionPagosService","http://infonavit.hipoteca.solicitud.cancelacion.com.portabilidad")


    #
    # Operation: solicitaCancelacionPago
    #

    # Publish Operation : solicitaCreditoFovissste
    LET operation = com.WebOperation.CreateDOCStyle("solicitaCancelacionPagos","solicitaCancelacionPagos",mensajeEntradaSolicitud,mensajeSalidaSolicitud)
    CALL service.publishOperation(operation,"")


    #
    # Register Service
    #
    CALL com.WebServiceEngine.RegisterService(service)
    RETURN 0

  CATCH
    RETURN STATUS
  END TRY

END FUNCTION

#Objetivo: Funci�n invocada por WS para generar la solicitud de cancelaci�n de pagos
FUNCTION solicitaCancelacionPagos()

   CALL fn_determina_tipo_consulta()

END FUNCTION                                                                                                                   HPSWS04.4gl                                                                                         0000777 0000212 0001751 00000063037 13113322757 012462  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/08/2015
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSWS04                                                       #
#Objetivo     => Funciones de WS de hipoteca, solicitud de cancelaci�n de pagos#
#                Constandes provenientes de HPSWS04.inc                        #
#Fecha inicio => 10 Agosto 2015                                                #
################################################################################
IMPORT FGL WSHelper
IMPORT com
IMPORT xml
DATABASE safre_viv

GLOBALS "HPSWS04.inc"
GLOBALS "HPSWS03.inc"
--GLOBALS "PRTW01.inc"
--GLOBALS "PRTG01.4gl"

PRIVATE
DEFINE v_f_actual  DATE,
       v_error     BOOLEAN,
       v_id_hps_solicitud     LIKE hps_solicitud_cancelacion.id_hps_solicitud_cancelacion,
       v_id_hps_solicitud_tmp LIKE hps_solicitud_cancelacion.id_hps_solicitud_cancelacion,
       v_id_hps_solicitud_pago_servicio LIKE hps_solicitud_pago_servicio.id_solicitud_pago_servicio,
       v_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente

PRIVATE
DEFINE v_saldo_derechohabiente RECORD
          v_acciones LIKE cta_movimiento.monto_acciones,
          v_pesos    LIKE cta_movimiento.monto_pesos
       END RECORD

PRIVATE
DEFINE v_derechohabiente RECORD
          apPaterno LIKE afi_derechohabiente.ap_paterno_af,
          apMaterno LIKE afi_derechohabiente.ap_materno_af,
          nombre    LIKE afi_derechohabiente.nombre_af
       END RECORD

DEFINE r_error            BOOLEAN

# Descripci�n: Inicializa consultas SQL
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 seq_hps_solicitud_cancelacion.NEXTVAL",
                    "   FROM systables"
   PREPARE prp_rec_seq_sol FROM v_consulta           
   
   LET v_consulta = " INSERT INTO hps_solicitud_cancelacion",
                    " (id_hps_solicitud_cancelacion,",
                    "  id_solicitud_pago_servicio,",
                    "  nss,",
                    "  n_caso,",
                    --"  resultado_operacion,",
                    --"  diagnostico_interno,",
                    "  estado,",
                    "  f_actualiza)",
                    " VALUES(?,0,?,?,?,?)"
   PREPARE prp_genera_solicitud_cedente FROM v_consulta

   LET v_consulta = "INSERT INTO hps_restitucion",
                    "(id_hps_restitucion,",
                    " id_solicitud_pago_servicio,",
                    " id_hps_solicitud_cancelacion,",
                    " id_derechohabiente,",
                    " nss,",
                    " subcuenta,",
                    " mto_acciones,",
                    " mto_pesos,",
                    " estado)",
                    "VALUES(seq_hps_restitucion.NEXTVAL,?,?,?,?,?,?,?,?)"
   PREPARE prp_genera_restitucion FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET diagnostico_interno = ?,",
                    "        resultado_operacion = ?,",
                    "        id_solicitud_pago_servicio = ?,",
                    "        nombre = ?,",
                    "        paterno = ?,",
                    "        materno = ?,",
                    "        n_caso = ?,",
                    "        estado = ?,",
                    "        f_actualiza = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_solicitud FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET diagnostico_interno = ?,",
                    "        resultado_operacion = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_solicitud_error FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_cancelacion",
                    "    SET estado = ?",
                    "  WHERE id_hps_solicitud_cancelacion = ?"
   PREPARE prp_actualiza_estado_solicitud_cancelacion FROM v_consulta

   LET v_consulta = " UPDATE hps_solicitud_pago_servicio",
                    "    SET ind_actividad = ?",
                    "  WHERE id_solicitud_pago_servicio = ?"
   PREPARE prp_actualiza_estado_pago_srv FROM v_consulta

   LET v_consulta = " UPDATE hps_cat_pago_servicio",
                    "    SET estado = ?",
                    "  WHERE id_solicitud_pago_servicio = ?"
   PREPARE prp_actualiza_estado_cat_pago_srv FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_solicitud_pago_servicio",
                    "   FROM hps_solicitud_pago_servicio",
                    "  WHERE nss = ?", 
                    "    AND ind_actividad = ? "
   PREPARE prp_rec_existe_pago_nss FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE nss = ?",
                    "    AND estado = ?",
                    "  ORDER BY id_hps_solicitud_cancelacion DESC"
   PREPARE prp_busca_solicitud_cancelacion FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE nss = ?",
                    "    AND estado IN (?,?)",
                    "  ORDER BY id_hps_solicitud_cancelacion DESC"
   PREPARE prp_valida_existencia_solicitud FROM v_consulta
   
   LET v_consulta = " SELECT FIRST 1 id_hps_solicitud_cancelacion,",
                    "        id_solicitud_pago_servicio",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE n_caso = ?",
                    "    AND estado = ?"
   PREPARE prp_val_num_caso FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 diagnostico_interno",
                    "   FROM hps_solicitud_cancelacion",
                    "  WHERE id_hps_solicitud_cancelacion = ?",
                    "    AND estado = ?"
   PREPARE prp_consulta_diag_restitucion FROM v_consulta
   
   LET v_consulta = " SELECT FIRST 1 id_derechohabiente,",
                    "        nombre_af,",
                    "        ap_paterno_af,",
                    "        ap_materno_af",
                    "   FROM afi_derechohabiente",
                    "  WHERE nss = ?"
   PREPARE prp_rec_id_derechohabiente FROM v_consulta
   
   LET v_consulta = " SELECT SUM(monto_acciones),",
                    "        SUM(monto_pesos)",
                    "   FROM cta_movimiento",
                    "  WHERE id_derechohabiente = ?",
                    "    AND subcuenta IN (?,?)"
   PREPARE prp_rec_saldo_derechohabiente FROM v_consulta

   LET v_consulta = " SELECT diagnostico_interno",
                    "   FROM prt_rch_marca_diagnostico",
                    "  WHERE marca_activa = ?",
                    "    AND destino_diagnostico = ?"
   PREPARE prp_rec_diag_marca FROM v_consulta

   # Especifica 5 segundos como m�ximo para la espera de respuesta del servidor externo
   CALL com.WebServiceEngine.SetOption( "readwritetimeout", 5 )

END FUNCTION

FUNCTION fn_determina_tipo_consulta()

   INITIALIZE v_derechohabiente.*,
              v_id_hps_solicitud,
              v_id_hps_solicitud_pago_servicio,
              v_id_derechohabiente,
              v_saldo_derechohabiente.* TO NULL
   
   CASE mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus

      WHEN C_ID_ESTATUS_VALIDACION_PORTAL # consulta del portal
         CALL fn_registra_solicitud_cancelacion() RETURNING r_error                                               
         IF NOT(r_error)THEN
            CALL fn_valida_solicitud_portabilidad(1) RETURNING r_error
         END IF

      WHEN C_ID_ESTATUS_VALIDACION_CRM # Consulta de validaci�n de portabilidad ADAI
         CALL fn_recupera_id_solicitud() RETURNING r_error         
         IF NOT( r_error )THEN
            CALL fn_valida_solicitud_portabilidad(1) RETURNING r_error
         END IF

      WHEN C_ID_ESTATUS_CANCELA_HPS # Confirmaci�n de ADAI para cancelaci�nde pagos
         CALL fn_valida_numero_caso() RETURNING r_error
         IF NOT ( r_error )THEN
            CALL fn_actualiza_sol_formalizada() RETURNING r_error
            IF NOT( r_error )THEN
               # Llamada a bus para enviar solicitud de portabilidad a procesar
               CALL fn_genera_restitucion()
            END IF
         END IF

      OTHERWISE
         CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1001)
         
   END CASE

END FUNCTION

#Objetivo: Registra la solicitud de cancelaci�n de pagos
FUNCTION fn_registra_solicitud_cancelacion()

   LET v_f_actual = TODAY
   TRY 
      INITIALIZE v_id_hps_solicitud TO NULL
      # Recupera sequencia
      EXECUTE prp_rec_seq_sol INTO v_id_hps_solicitud
      EXECUTE prp_genera_solicitud_cedente USING v_id_hps_solicitud,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,                                                 
                                                 C_ESTADO_INI_SOLICITUD_CANCELACION,
                                                 v_f_actual

   CATCH # Captura error sql
      DISPLAY "Error al insertar datos para:"
      DISPLAY "NSS:      ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
      DISPLAY "No. caso: ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso
      DISPLAY "Flujo:    ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
      DISPLAY "C�digo:   ",SQLCA.SQLCODE
      DISPLAY "Mensaje:  ",SQLCA.sqlerrm
      
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)

      RETURN TRUE # Error en sql             
   END TRY
                                              
   RETURN FALSE # Ejecuci�n realizada correctamente
END FUNCTION

#Objetivo: Funcion para validar que los datos de la solicitud de cancelaci�n
FUNCTION fn_valida_solicitud_portabilidad(p_verifica_existente)
DEFINE v_estado             SMALLINT,
       p_verifica_existente SMALLINT

   INITIALIZE v_estado TO NULL
   LET v_error = FALSE # sin error
   # VALIDA NSS
   IF(mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss IS NULL OR mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss = ' ')THEN
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
      LET v_error = TRUE
   ELSE
      TRY
         # BUSCA DERECHOHABIENTE EN afi_derechohabiente
         INITIALIZE v_id_derechohabiente,v_derechohabiente.* TO NULL
         EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                             INTO v_id_derechohabiente,
                                                  v_derechohabiente.nombre,
                                                  v_derechohabiente.apPaterno,
                                                  v_derechohabiente.apMaterno
         IF( v_id_derechohabiente IS NOT NULL )THEN
            IF( p_verifica_existente )THEN
               CALL fn_valida_existencia_portabilidad() RETURNING v_error
            ELSE
               LET v_error = FALSE
            END IF
            IF NOT( v_error )THEN   
               # VALIDA EXISTENCIA DE PAGO DE SERVICIOS
               INITIALIZE v_id_hps_solicitud_pago_servicio TO NULL
               EXECUTE prp_rec_existe_pago_nss USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                     C_ESTADO_PAGO_SERVICIO_ACTIVO
                                                INTO v_id_hps_solicitud_pago_servicio
               IF( v_id_hps_solicitud_pago_servicio IS NOT NULL)THEN
                  IF NOT( v_error )THEN
                     # VALIDA EXISTENCIA DE SALDOS DE PAGOS
                     INITIALIZE v_saldo_derechohabiente.* TO NULL
                     EXECUTE prp_rec_saldo_derechohabiente USING v_id_derechohabiente,
                                                                 C_SUBCUENTA_PREDIAL,
                                                                 C_SUBCUENTA_CONSERVACION
                                                            INTO v_saldo_derechohabiente.v_acciones,
                                                                 v_saldo_derechohabiente.v_pesos
                     # DETERMINA ESTADO DE LA SOLICITUD SEG�N FLUJO
                     CASE mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
                        WHEN C_ID_ESTATUS_VALIDACION_PORTAL
                           LET v_estado = C_ESTADO_SOLICITUD_REGISTRADA

                        WHEN C_ID_ESTATUS_VALIDACION_CRM
                        LET v_estado = C_ESTADO_SOLICITUD_ACEPTADA

                     END CASE
                     # SALDO > 0 con restituci�n, SALDO <= 0 sin restituci�n
                     IF( v_saldo_derechohabiente.v_acciones > 0 )THEN
                        CALL fn_actualiza_solicitud(C_RESULTADO_OP_ACEPTADA_FOV,
                                                    C_DIAGNOSTICO_1000,
                                                    v_estado)
                     ELSE
                        CALL fn_actualiza_solicitud(C_RESULTADO_OP_ACEPTADA_FOV,
                                                    C_DIAGNOSTICO_1002,
                                                    v_estado)
                     END IF
                  END IF
               ELSE
                  CALL fn_actualiza_solicitud(C_RESULTADO_OP_RECHAZADA_FOV,
                                              C_DIAGNOSTICO_1001,
                                              C_ESTADO_SOLICITUD_RECHAZADA)
                           
                  LET v_error = TRUE            
               END IF
            END IF
         ELSE
            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
            LET v_error = TRUE
         END IF
      CATCH
         DISPLAY "Error en estructura"
         DISPLAY "NSS:    ",mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
         DISPLAY "C�digo: ",SQLCA.SQLCODE
         DISPLAY "Mensaje:",SQLCA.sqlerrm
      
         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)
         LET v_error = TRUE
      END TRY      
   END IF

   RETURN v_error
END FUNCTION

# Descripci�n: Funci�n para validar la existencia del nss solicitud de cancelaci�n
FUNCTION fn_valida_existencia_portabilidad()

   # VALIDA SI YA EXISTE EL NSS EN SOLICITUD DE CANCELACI�N
   INITIALIZE v_id_hps_solicitud_tmp TO NULL

   EXECUTE prp_valida_existencia_solicitud USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 C_ESTADO_SOLICITUD_ACEPTADA,
                                                 C_ESTADO_SOL_REST_SOLICITADA
                                            INTO v_id_hps_solicitud_tmp
   IF(v_id_hps_solicitud_tmp IS NULL)THEN
      RETURN FALSE
   ELSE
      IF( mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus = C_ID_ESTATUS_VALIDACION_CRM )THEN
         INITIALIZE v_derechohabiente.*,mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      END IF
      
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   END IF

END FUNCTION

# Descripci�n: Recupera y valida existencia de solicitud previa
FUNCTION fn_recupera_id_solicitud()

   INITIALIZE v_id_hps_solicitud TO NULL   
   EXECUTE prp_busca_solicitud_cancelacion USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                                 C_ESTADO_SOLICITUD_REGISTRADA
                                            INTO v_id_hps_solicitud
   IF(v_id_hps_solicitud IS NULL)THEN
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

# Descripci�n: Funci�n para validar que el n�mero de caso exista en las solicitudes de cancelaci�n
FUNCTION fn_valida_numero_caso()

   INITIALIZE v_id_hps_solicitud TO NULL
   EXECUTE prp_rec_id_derechohabiente USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
                                       INTO v_id_derechohabiente,
                                            v_derechohabiente.nombre,
                                            v_derechohabiente.apPaterno,
                                            v_derechohabiente.apMaterno
   INITIALIZE v_derechohabiente.* TO NULL
   EXECUTE prp_val_num_caso USING mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                  C_ESTADO_SOLICITUD_ACEPTADA
                             INTO v_id_hps_solicitud,
                                  v_id_hps_solicitud_pago_servicio
                                  
   IF(v_id_hps_solicitud IS NULL)THEN # si el n�mero de caso recibido no existe devuelve mensaje rechazado
      # Asigna mensaje de respuesta a portal
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1003)
      RETURN TRUE
   ELSE
      RETURN FALSE
   END IF

END FUNCTION

#Objetivo: Funci�n para asignar los valores al mensaje de salida
FUNCTION fn_asigna_msj_salida(p_diagnostico)
DEFINE p_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.numeroCaso  = mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.nss         = mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.apPaterno   = v_derechohabiente.apPaterno
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.apMaterno   = v_derechohabiente.apMaterno
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.nombre      = v_derechohabiente.nombre   
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.idEstatus   = mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus
   LET mensajeSalidaSolicitud.mensajeSalidaSolicitudReturn.diagnostico = p_diagnostico

END FUNCTION

#Objetivo: Funci�n para asignar mensaje erroneo de salida
FUNCTION fn_asigna_msj_erroneo_salida(p_diagnostico)
DEFINE p_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   TRY   
      # Actuzliza registros de solicitud rechazada
      EXECUTE prp_actualiza_solicitud_error USING p_diagnostico,
                                                  C_RESULTADO_OP_RECHAZADA_FOV,
                                                  v_id_hps_solicitud
   CATCH
      DISPLAY "Error al actualizar datos:"
      DISPLAY "Id solicitud: ",v_id_hps_solicitud
      DISPLAY "C�digo:       ",SQLCA.SQLCODE
      DISPLAY "Mensaje:      ",SQLCA.sqlerrm
   END TRY

   CALL fn_asigna_msj_salida(p_diagnostico)

END FUNCTION

#Objetivo: Funci�n para actualizar el estado de la solicitud cedente
FUNCTION fn_actualiza_solicitud(p_diagnostico_operacion,
                                p_diagnostico,
                                p_estado)
DEFINE p_diagnostico_operacion CHAR(2),
       p_diagnostico           SMALLINT,
       p_estado                SMALLINT,
       v_fecha_actual          DATE

   # El registro de salida ya debe haber recuperado el reultado de la operaci�n
   CASE p_diagnostico_operacion
      WHEN C_RESULTADO_OP_ACEPTADA_FOV
         TRY
            LET v_fecha_actual = TODAY
            # Actualiza registros de solicitud aceptada
            EXECUTE prp_actualiza_solicitud USING p_diagnostico,
                                                  p_diagnostico_operacion,
                                                  v_id_hps_solicitud_pago_servicio,
                                                  v_derechohabiente.nombre,
                                                  v_derechohabiente.apPaterno,
                                                  v_derechohabiente.apMaterno,
                                                  mensajeEntradaSolicitud.mensajeEntradaSolicitud.numeroCaso,
                                                  p_estado,
                                                  v_fecha_actual,
                                                  v_id_hps_solicitud

            IF( mensajeEntradaSolicitud.mensajeEntradaSolicitud.idEstatus = C_ID_ESTATUS_VALIDACION_CRM )THEN
               INITIALIZE v_derechohabiente.*,mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
            END IF
            # Asigna mensaje de respuesta a portal
            CALL fn_asigna_msj_salida(p_diagnostico)
            
         CATCH
            DISPLAY "Error al actualizar datos aceptados:"
            DISPLAY "Id solicitud cedente:",v_id_hps_solicitud
            DISPLAY "C�digo: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm
            
            CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1001)

         END TRY

      WHEN C_RESULTADO_OP_RECHAZADA_FOV
         TRY
            # Actualiza estado a rechazado
            EXECUTE prp_actualiza_estado_solicitud_cancelacion USING p_estado,
                                                                     v_id_hps_solicitud

            CALL fn_asigna_msj_erroneo_salida(p_diagnostico)
         CATCH
            DISPLAY "Error al actualizar datos rechazados:"
            DISPLAY "Id solicitud cedente:",v_id_hps_solicitud
            DISPLAY "C�digo: ",SQLCA.SQLCODE
            DISPLAY "Mensaje: ",SQLCA.sqlerrm

            CALL fn_asigna_msj_erroneo_salida(p_diagnostico)
         END TRY
         
   END CASE

END FUNCTION

# Descripci�n: Actualiza datos personales del nss
FUNCTION fn_actualiza_sol_formalizada()
DEFINE v_error BOOLEAN

   LET v_error = FALSE
   TRY

      EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_PAGO_CANCELADA,
                                                               v_id_hps_solicitud

      EXECUTE prp_actualiza_estado_pago_srv USING C_ESTADO_PAGO_SERVICIO_CANCELADO,
                                                  v_id_hps_solicitud_pago_servicio

      EXECUTE prp_actualiza_estado_cat_pago_srv USING C_ESTADO_PAGO_SERVICIO_CANCELADO,
                                                      v_id_hps_solicitud_pago_servicio
      LET v_error = FALSE
   CATCH
      DISPLAY "Error al actualizar solicitud formalizada:"
      DISPLAY "Id solicitud cancelaci�n:",v_id_hps_solicitud
      DISPLAY "Id pago de servicios:    ",v_id_hps_solicitud_pago_servicio
      DISPLAY "C�digo:                  ",SQLCA.SQLCODE
      DISPLAY "Mensaje:                 ",SQLCA.sqlerrm
      LET v_error = TRUE
      CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)      
   END TRY
   RETURN v_error
END FUNCTION

# Descripci�n: Genera restituci�n en caso de que el diagn�stico sea
FUNCTION fn_genera_restitucion()
DEFINE v_diagnostico LIKE hps_solicitud_cancelacion.diagnostico_interno

   EXECUTE prp_consulta_diag_restitucion USING v_id_hps_solicitud,
                                               C_ESTADO_SOL_PAGO_CANCELADA
                                          INTO v_diagnostico

   IF( v_diagnostico = C_DIAGNOSTICO_1000 )THEN
      TRY
         INITIALIZE v_saldo_derechohabiente.* TO NULL
         EXECUTE prp_rec_saldo_derechohabiente USING v_id_derechohabiente,
                                                     C_SUBCUENTA_PREDIAL,
                                                     C_SUBCUENTA_CONSERVACION
                                                INTO v_saldo_derechohabiente.v_acciones,
                                                     v_saldo_derechohabiente.v_pesos
                                                     
         EXECUTE prp_genera_restitucion USING v_id_hps_solicitud_pago_servicio,
                                              v_id_hps_solicitud,                                              
                                              v_id_derechohabiente,
                                              mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss,
                                              C_SUBCUENTA_VIVIENDA,
                                              v_saldo_derechohabiente.v_acciones,
                                              v_saldo_derechohabiente.v_pesos,
                                              C_ESTADO_REST_REGISTRADA

         EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_REST_REGISTRADA,
                                                                  v_id_hps_solicitud
         INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
         CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1100)
      CATCH
         DISPLAY "Error al registrar restituci�n:"
         DISPLAY "Id solicitud cancelaci�n:",v_id_hps_solicitud
         DISPLAY "Id pago de servicios:    ",v_id_hps_solicitud_pago_servicio
         DISPLAY "C�digo:                  ",SQLCA.SQLCODE
         DISPLAY "Mensaje:                 ",SQLCA.sqlerrm

         CALL fn_asigna_msj_erroneo_salida(C_DIAGNOSTICO_1003)
      END TRY
   ELSE
      EXECUTE prp_actualiza_estado_solicitud_cancelacion USING C_ESTADO_SOL_REST_REGISTRADA,
                                                               v_id_hps_solicitud
      INITIALIZE mensajeEntradaSolicitud.mensajeEntradaSolicitud.nss TO NULL
      CALL fn_asigna_msj_salida(C_DIAGNOSTICO_1100)
   END IF
END FUNCTION                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 HPSX02.4gl                                                                                          0000777 0000212 0001751 00000016612 13113322755 012331  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01-07-2015
--===============================================================
################################################################################
#Modulo            => SEP                                                      #
#Programa          => HPSX02                                                   #
#Objetivo          => Programa batch de validacion para sumario y detalle de   # 
#actualizacion catalogo entidades receptoras mandato                                                                              #
#Fecha inicio      => Julio, 2015                                              #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN

   --Definiendo variables locales
   DEFINE bnd_valida BOOLEAN 
   DEFINE bnd_err    BOOLEAN
   
   DEFINE v_tiempo   CHAR(23),
       v_edo_error          SMALLINT,
       v_archivo_nohup      STRING,
       v_archivo_errnohup   STRING,
       v_ruta_listado_nohup LIKE seg_modulo.ruta_listados,
       v_comando            STRING,
       v_ind_tipo_ejecucion LIKE bat_ctr_operacion.ind_tipo_ejecucion

   # Se recuperan los par�metros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   --Inicializaci�n de variables
   LET bnd_valida = TRUE
   LET bnd_err    = TRUE

   --Valida archivo de integracion
   CALL fn_valida_catalogo() RETURNING bnd_valida

   --Verificando posibles inconsistencias entre sumario y registros totales
   CALL fn_valida_inconsis() RETURNING bnd_err

   --Establece erronea la operacion
   IF bnd_valida OR bnd_err THEN
      CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
      LET v_tiempo = CURRENT YEAR TO SECOND;
      LET v_edo_error = 3;
      # actualiza los estado del proceso y operacion de manera manual, ya que
      # no se puede utilizar fn_error_opera cuando se ha finalizado la operacion
      UPDATE bat_ctr_operacion
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod;

      UPDATE bat_ctr_proceso
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod;

      SELECT ruta_listados
        INTO v_ruta_listado_nohup
        FROM seg_modulo
        WHERE modulo_cod = 'bat'

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
        INTO v_ind_tipo_ejecucion
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod
         
      IF(v_ind_tipo_ejecucion = 0)THEN # para el caso de ejecucion batch, solo se imprime la leyenda para que realice la tarea por si sola         
         # se cambia el log de la operacion, para que la funcion de correo lo tome correctamente
         # y no provoque error
         LET v_archivo_nohup = v_ruta_listado_nohup CLIPPED, "/finnohup:",
                                p_pid         USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod   USING "&&&&&"

         LET v_archivo_errnohup = v_ruta_listado_nohup CLIPPED, "/errnohup:",
                                  p_pid         USING "&&&&&",":",
                                  p_proceso_cod USING "&&&&&",":",
                                  p_opera_cod   USING "&&&&&" 

         LET v_comando = "mv "||v_archivo_nohup||" "||v_archivo_errnohup
         RUN v_comando
      ELSE
         DISPLAY "Program stopped"
      END IF
   END IF
   
END MAIN

FUNCTION fn_valida_catalogo()

--Variables internas
   DEFINE v_valida BOOLEAN
   DEFINE v_error_sql  INTEGER
   DEFINE v_isam_error INTEGER
   DEFINE v_msg_error  CHAR(254)

   DEFINE tot_reg      INTEGER
   DEFINE v_mensaje    CHAR(255)

   --inicia la funcion
   PREPARE prp_tmp_table FROM "CREATE TEMP TABLE hps_diag_tmp_inconsistencias_mdt(mensaje CHAR(255));"
   EXECUTE prp_tmp_table

   PREPARE prp_exec_func FROM "EXECUTE PROCEDURE safre_viv:sp_hps_valida_catalogo_mdt()"
   DECLARE cur_func CURSOR FOR prp_exec_func
   OPEN cur_func
   FETCH cur_func INTO v_error_sql,v_isam_error,v_msg_error
   CLOSE cur_func
   FREE cur_func
   FREE prp_exec_func
   FREE prp_tmp_table

   IF v_error_sql <> 0 THEN
      DISPLAY "Ocurri� un error en validaci�n de carga de actualizaci�n de cat�logo de entidades receptoras de mandatos : ",v_error_sql," ",v_msg_error," ",v_isam_error
      LET v_valida = TRUE --TRUE SI EXISTE ERROR
      RETURN v_valida
   END IF

   SELECT COUNT(*) 
      INTO tot_reg 
      FROM safre_tmp:hps_diag_tmp_inconsistencias_mdt

   IF tot_reg > 0 THEN
      DISPLAY "INCONSISTENCIAS DETECTADAS EN REGISTROS DE DETALLE"
       --actualizando encabezado
      UPDATE safre_tmp:hps_tmp_cza_acmdt 
	      SET resultado_opera = "02",
		      diagnostico 	  = "001"

      --Desplegar inconsistencias de la tabla
      DISPLAY "\n================================================================="
      DECLARE cur_rec_inconsistencias CURSOR FOR SELECT mensaje FROM hps_diag_tmp_inconsistencias_mdt ORDER BY 1
      FOREACH cur_rec_inconsistencias INTO v_mensaje
         DISPLAY v_mensaje
      END FOREACH
      FREE cur_rec_inconsistencias
      DISPLAY "=================================================================\n"
      LET v_valida = TRUE
   ELSE 
      LET v_valida = FALSE
   END IF
    
   RETURN v_valida

END FUNCTION

FUNCTION fn_valida_inconsis()

   DEFINE v_valida      BOOLEAN
   DEFINE v_detalle     INTEGER
   DEFINE v_detalle02   INTEGER
   DEFINE v_totaldet    INTEGER
   DEFINE s_detalle     INTEGER
   DEFINE s_detalle02   INTEGER
   DEFINE s_totaldet    INTEGER

   SELECT COUNT(*)
      INTO v_detalle
      FROM safre_tmp:hps_tmp_det_acmdt

   SELECT COUNT(*)
      INTO v_detalle02
      FROM safre_tmp:hps_tmp_det02_acmdt

   LET v_totaldet = v_detalle + v_detalle02

   SELECT tot_reg_detalle01,
          tot_reg_detalle02,
          total_registros_detalle
      INTO s_detalle,
           s_detalle02,
           s_totaldet
      FROM safre_tmp:hps_tmp_sum_acmdt

   IF(v_detalle <> s_detalle OR v_detalle02 <> s_detalle02 OR v_totaldet <> s_totaldet)THEN
      DISPLAY "\n================================================================="
      DISPLAY "ERROR. SE ENCONTRARON INCONSITENCIAS ENTRE EL DETALLE Y EL SUMARIO"
      DISPLAY "=================================================================\n"
      LET v_valida = TRUE -- TRUE SI EXISTE ERROR
   ELSE
      LET v_valida = FALSE
   END IF 
   
   RETURN v_valida 

END FUNCTION                                                                                                                      HPSX20.4gl                                                                                          0000777 0000212 0001751 00000016220 13113322764 012324  0                                                                                                    ustar   jyanez                          safreviv                                                                                                                                                                                                               --==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09-08-2012
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTP20                                                   #
#Objetivo          => Programa batch de validacion para sumario y detalle      # 
#                     recurrente instrucciones pago servicios HPS              # 
#Autor             => Jesus David Ya�ez Moreno                                 #
#Fecha inicio      => 16 marzo 2015                                            #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # C�digo del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # C�digo de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE v_conteo_detalle     INTEGER,
       v_total_sum          INTEGER,
       v_tiempo             CHAR(23),
       v_edo_error          SMALLINT,
       v_archivo_nohup      STRING,
       v_archivo_errnohup   STRING,
       v_ruta_listado_nohup LIKE seg_modulo.ruta_listados,
       v_comando            STRING,
       v_ind_tipo_ejecucion LIKE bat_ctr_operacion.ind_tipo_ejecucion,
       v_error_val_det      BOOLEAN,
       v_bnd_error_val      BOOLEAN

   # Se recuperan los par�metros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   LET v_conteo_detalle = 0
   LET v_total_sum = 0
   LET v_edo_error = 3
   LET v_error_val_det = FALSE
   LET v_bnd_error_val = FALSE

   # llamada a la funcion de validacion del detalle
   CALL fn_valida_detalle() RETURNING v_error_val_det
   IF(v_error_val_det)THEN
      LET v_bnd_error_val = TRUE
   END IF

   # consultas para validaci�n de sumario
   SELECT COUNT(*)
     INTO v_conteo_detalle
     FROM safre_tmp:tmp_hps_det_instrucciones
    WHERE 1 = 1

   SELECT cant_reg_detalle
     INTO v_total_sum
     FROM safre_tmp:tmp_hps_sum_instrucciones
    WHERE 1 = 1

   # imprime mensaje de error del sumario
   IF(v_conteo_detalle <> v_total_sum)THEN
      DISPLAY "\n================================================================="
      DISPLAY "ERROR. SE ENCONTRARON INCONSITENCIAS ENTRE EL DETALLE Y EL SUMARIO"
      DISPLAY "=================================================================\n"
      LET v_bnd_error_val = TRUE
   END IF

   # Establece erronea la operacion
   IF(v_bnd_error_val)THEN
      CALL fn_max_pid(p_proceso_cod,p_opera_cod) RETURNING p_pid
      LET v_tiempo = CURRENT YEAR TO SECOND;
      # actualiza los estado del proceso y operacion de manera manual, ya que
      # no se puede utilizar fn_error_opera cuando se ha finalizado la operacion
      UPDATE bat_ctr_operacion
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod;

      UPDATE bat_ctr_proceso
         SET fecha_fin   = v_tiempo,
             estado_cod  = v_edo_error
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod;

      SELECT ruta_listados
        INTO v_ruta_listado_nohup
        FROM seg_modulo
        WHERE modulo_cod = 'bat'

      # consulta para determinar el tipo de ejecucion, 0 = manual  1 = batch
      SELECT ind_tipo_ejecucion
        INTO v_ind_tipo_ejecucion
        FROM bat_ctr_operacion
       WHERE pid = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod = p_opera_cod

      IF(v_ind_tipo_ejecucion = 0)THEN # para el caso de ejecucion batch, solo se imprime la leyenda para que realice la tarea por si sola
         
         # se cambia el log de la operacion, para que la funcion de correo lo tome correctamente
         # y no provoque error
         LET v_archivo_nohup = v_ruta_listado_nohup CLIPPED, "/finnohup:",
                                p_pid         USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod   USING "&&&&&"

         LET v_archivo_errnohup = v_ruta_listado_nohup CLIPPED, "/errnohup:",
                                  p_pid         USING "&&&&&",":",
                                  p_proceso_cod USING "&&&&&",":",
                                  p_opera_cod   USING "&&&&&" 

         LET v_comando = "mv "||v_archivo_nohup||" "||v_archivo_errnohup
         RUN v_comando
      ELSE
         DISPLAY "Program stopped"
      END IF
   END IF
   
END MAIN

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTL20                                                   #
#Descripcion       => Validaci�n de los campos del detalle de archivo para acr #
#                     con origen recurrente                                    #
#Autor             => Hugo C�sar Ram�rez Garc�a                                #
#Fecha inicio      => 10 Junio 2012                                            #
################################################################################
FUNCTION fn_valida_detalle()
DEFINE v_consulta   STRING,
       v_conteo_inc INTEGER,
       v_error_validacion BOOLEAN,
       v_mensaje          VARCHAR(254)

   LET v_error_validacion = FALSE 

   PREPARE prp_crea_tbl_tmp FROM "CREATE TEMP TABLE hps_tmp_inconsistencias_val(mensaje CHAR(254))"
   EXECUTE prp_crea_tbl_tmp

   LET v_consulta = "EXECUTE PROCEDURE sp_hps_valida_carga_recurrente()"
   PREPARE prp_valida_carga FROM v_consulta
   EXECUTE prp_valida_carga
   IF(SQLCA.SQLCODE <> 0)THEN
      DISPLAY "Ocurri� un error en validaci�n de carga de instrucciones pago servicios hps"
   END IF

   
   LET v_conteo_inc = 0
   SELECT COUNT(*) 
     INTO v_conteo_inc
     FROM hps_tmp_inconsistencias_val
    WHERE 1 = 1

   IF(v_conteo_inc > 0)THEN
      DISPLAY "\n================================================================="
      DECLARE cur_rec_inconsistencias CURSOR FOR SELECT mensaje
                                                   FROM hps_tmp_inconsistencias_val
                                                  WHERE 1 = 1
      FOREACH cur_rec_inconsistencias INTO v_mensaje
         DISPLAY v_mensaje
      END FOREACH
      FREE cur_rec_inconsistencias
      DISPLAY "=================================================================\n"
      LET v_error_validacion = TRUE
   END IF

   RETURN v_error_validacion
                             
END FUNCTION
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                