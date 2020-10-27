--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
--==============================================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => CATALOGO DE ALTA DE MANDATOS                            #
#Autor             => Francisco López                                         #
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
    WHERE modulo_cod = "mdt"   

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
   OPEN WINDOW w_mant_mandatos WITH FORM v_ruta_ejecutable CLIPPED||"/MDTM011"
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
               CALL fn_mensaje(p_titulo,"Capture descripción del mandato","information")
               NEXT FIELD desc_mandato
               --CONTINUE INPUT
            END IF
            IF v_desc_larga_mandato CLIPPED = v_desc_larga_mandato_tmp CLIPPED OR v_desc_larga_mandato IS NULL THEN
               CALL fn_mensaje(p_titulo,"Capture descripción larga del mandato","information")
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
   OPEN WINDOW w_mant_mandatos_detalle WITH FORM v_ruta_ejecutable CLIPPED||"/MDTM012"
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
                              CALL fn_mensaje("Aviso","Clabe inválida, recaptura para su validación","error")
                           END IF
                        ELSE
                           IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                              IF v_s_genera_digito_vrf_b = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                    IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                       LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de banco es inválida dentro de la CLABE","error")
                                       LET v_c_banco = '   '
                                    END IF
                                 END IF
                              END IF
                              IF v_s_genera_digito_vrf_p = 0 THEN
                                 IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                    IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                       LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                    ELSE
                                       CALL fn_mensaje("Aviso","La clave de plaza es inválida dentro de la CLABE","error")
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
                           CALL fn_mensaje("Aviso","La CLABE es inválida, favor de verificar","error")
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
                           # busca en el arreglo de atributos, algúno que sea de direccion postal y le agrega el valor recuperado
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
                           CALL fn_mensaje("Aviso","Clabe inválida, recaptura para su validación","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inválida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inválida dentro de la CLABE","error")
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
                        CALL fn_mensaje("Aviso","La CLABE es inválida, favor de verificar","error")
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
                           CALL fn_mensaje("Aviso","Clabe inválida, recaptura para su validación","error")
                        END IF
                     ELSE
                        IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta CLIPPED) <> 18 THEN
                           IF v_s_genera_digito_vrf_b = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] CLIPPED) <> 0 THEN
                                 IF fn_existe_entidad_financiera(v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3]) THEN
                                    LET v_c_banco = v_arr_detalle_tmp[v_indice].valor_etiqueta[1,3] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de banco es inválida dentro de la CLABE","error")
                                    LET v_c_banco = '   '
                                 END IF
                              END IF
                           END IF
                           IF v_s_genera_digito_vrf_p = 0 THEN
                              IF LENGTH(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] CLIPPED) <> 0 THEN
                                 IF fn_existe_plaza(v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6]) THEN
                                    LET v_c_plaza = v_arr_detalle_tmp[v_indice].valor_etiqueta[4,6] USING "&&&"
                                 ELSE
                                    CALL fn_mensaje("Aviso","La clave de plaza es inválida dentro de la CLABE","error")
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
                        CALL fn_mensaje("Aviso","La CLABE es inválida, favor de verificar","error")
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
                  CALL fn_mensaje("Aviso","Tamaño del nombre de archivo excede el máximo permitido","information")
                  INITIALIZE v_imagen_docto[ARR_CURR()].nombre_imagen TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].v_documento_int TO NULL
                  INITIALIZE v_imagen_docto[ARR_CURR()].desc_imagen TO NULL
               ELSE
                  LET v_imagen_docto[ARR_CURR()].v_documento_int = v_imagen_docto[ARR_CURR()].v_documento 
                  IF(v_imagen_docto[ARR_CURR()].v_documento IS NOT NULL OR v_imagen_docto[ARR_CURR()].v_documento <> " ")THEN
                     # elimina el registro en caso de que se seleccionó otro archivo
                     CALL fn_admon_archivo_mdt(v_imagen_docto[ARR_CURR()].v_documento,0, 'B') 
                         RETURNING v_resultado
                  END IF
               END IF

            AFTER FIELD tedi_ruta
               
               CALL GET_FLDBUF(tedi_ruta) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               # si no es nulo o por que no seleccionó un archivo
               DISPLAY "AFTER FIELD: ",v_imagen_docto[ARR_CURR()].nombre_imagen 
               # se consideran 10 caracteres para identificadores para un total de 100
               IF(LENGTH(v_imagen_docto[ARR_CURR()].nombre_imagen CLIPPED) < 90 AND v_imagen_docto[ARR_CURR()].nombre_imagen IS NOT NULL)THEN
                  # se recupera el identificador de grupo actual
                  LET v_indice = DIALOG.getCurrentRow("tabla_grupo")
                  # Se agrega al nuevo registro el id_cat_gpo al que pertenecerá
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
               --Si regresa falso la validación se indica que debe revisar los datos
               CALL fn_mensaje("Aviso","Campo sin valor asignado, Verificar Captura","error")
            ELSE
               -- Valización de entidades financieras
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
                     CALL fn_mensaje("Aviso","La clave de banco es inválida dentro de la CLABE","error")
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
                     CALL fn_mensaje("Aviso","La clave de plaza es inválida dentro de la CLABE","error")
                     CONTINUE DIALOG
                  END IF
               END IF
               IF v_s_genera_digito_vrf_n = 1 AND v_s_genera_digito_vrf_c = 1 THEN
                  IF v_c_cta_clabe[7,17] <> v_c_cta THEN
                     CALL fn_mensaje("Aviso","El número de cuenta no corresponde con la cuenta CLABE","error")
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
                     CALL fn_mensaje("Aviso","La CLABE es inválida, favor de verificar","error")
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
                           
               IF fn_ventana_confirma("Confimar","¿Desea guardar el registro?","info") = 1 THEN
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
               # no hay algún registro y se inserta uno
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
            IF fn_ventana_confirma("Confimar","¿Desea salir y cancelar la captura?","info") = 1 THEN
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Llenar los arrays globales del modulo                   #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => eliminar el documento seleccionado por el usuario       #
#Autor             => Hugo César Ramírez Gracía                               #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los elementos       #
#                     segun su id_grupo                                       #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Pasa el arreglo temporal al general del detalle         #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los documentos      #
#                     según su id_cat_gpo y el grupo seleccionado             #
#Autor             => Hugo César Rampírez Gracía                              #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Pasa los datos al array temporal de los detalles        #
#                     segun su id_grupo e id_gpo_etiqueta                     #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Sube o baja una posición del array temporal y general   #
#                     del detalle                                             #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Agrega o elimina un registro a las tablas temporales    #
#                     y generales del detalle                                 #
#Autor             => Francisco López                                         #
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
   --Solo se añaden si el elemento existe
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
         --Se añade el registro hasta el final del arreglo temporal
         --CALL v_arr_detalle_tmp.appendElement()
         LET v_nuevo_indice = v_arr_detalle_tmp.getLength() + 1
         {LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_arr_detalle_tmp.getLength()].etiqueta = v_arr_elementos_aux.etiqueta}
         LET v_arr_detalle_tmp[v_nuevo_indice].id_gpo_mandato  = v_arr_elementos_aux.id_cat_gpo
         LET v_arr_detalle_tmp[v_nuevo_indice].id_gpo_etiqueta = v_arr_elementos_aux.id_gpo_etiqueta
         LET v_arr_detalle_tmp[v_nuevo_indice].etiqueta = v_arr_elementos_aux.etiqueta
         --Se añade el registro hasta el final del arreglo global
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Verifica si es posible guardar el registro              #
#Autor             => Francisco López                                         #
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
   
   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validación
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Se almacena el registro en base de datos                #
#Autor             => Francisco López                                         #
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
#Modulo            => MDT                                                     #
#Programa          => MDTM01                                                  #
#Objetivo          => Inserta el registro recibido como parametro en la tabla #
#                     mdt_imagen_docto                                        #
#Autor             => Hugo César Ramírez Gracía                               #
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
   
END FUNCTION 