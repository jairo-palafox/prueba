--===============================================================
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
#Autor             => Francisco López                                         #
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
        v_nombre_original STRING   -- nombre con el que comenzó antes de modificar 
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
        v_nombre_original STRING   -- nombre con el que comenzó antes de modificar 
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
                           # elimina el registro en caso de que se seleccionó otro archivo
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
 
                        WHEN v_imagen_docto[ARR_CURR()].v_operacion = 'M' AND # se esta modificacndo un registro que esta en BD por seguna vez o más
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
                  # validación para el tamaño del archivo
                  CALL fn_mensaje("Aviso","Tamaño del nombre de archivo excede el máximo permitido","information")
               END IF
               --NEXT FIELD tedi_descripcion

            AFTER FIELD tedi_rutam
               CALL GET_FLDBUF(tedi_rutam) RETURNING v_imagen_docto[ARR_CURR()].nombre_imagen
               # se inicializan a null los campos en caso de que el archvo exceda el tamaño permitido
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
                     CALL fn_mensaje("Aviso","La número de cuenta no corresponde con la cuenta CLABE","error")
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
               # no hay algún registro y se inserta uno
               CALL v_imagen_docto.insertElement(v_imagen_docto.getLength()+1)
               LET v_indice = DIALOG.getCurrentRow("tabla_grupo")
               LET v_imagen_docto[v_imagen_docto.getLength()].id_cat_gpo =  v_arr_grupos[v_indice].id_cat_gpo
               LET v_imagen_docto[v_imagen_docto.getLength()].v_operacion = 'A'
               # Recuepra id_gpo_mandato desde base de datos con el grupo seleccionado y el mandato que se recibió como parámetro 
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
               # Recuepra id_gpo_mandato desde base de datos con el grupo seleccionado y el mandato que se recibió como parámetro 
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
            IF fn_ventana_confirma("Confimar","¿Desea salir y cancelar la captura?","info") = 1 THEN
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
#                     el id_cat_mandato recibido como parámetro               #
#Autor             => Hugo César Ramírez Gracía                               #
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
#                     el id_cat_mandato recibido como parámetro               #
#Autor             => Hugo César Ramírez García                               #
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
#Autor             => Hugo César Ramírez Gracía                               #
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
        v_nombre_original STRING   -- nombre con el que comenzó antes de modificar 
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
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
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
#Autor             => Hugo César Ramíreaz Gracía                              #
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
#Autor             => Hugo César Ramíreaz Gracía                              #
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
#                     según su id_cat_gpo y el grupo seleccionado             #
#Autor             => Hugo César Ramírez Gracía                               #
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
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
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
#Autor             => Francisco López                                         #
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
   --Solo se añaden si el elemento existe
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
         --Se añade el registro hasta el final del arreglo temporal
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
            
         
         --Se añade el registro hasta el final del arreglo global
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
   
   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validación
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
--   LET v_bandera_valido = TRUE   --Se recorre el arrlego de la validación
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
#Autor             => Francisco López                                         #
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
#Autor             => Hugo César Ramírez Gracía                               #
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
        v_nombre_original STRING   -- nombre con el que comenzó antes de modificar 
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

   -- AHM TMP Debe ir al finalizar la modifcicación --Se obtiene el maximo registro para obtener el ID a insertar
   -- AHM TMP Debe ir al finalizar la modifcicación SELECT (NVL(MAX(id_mdt_notifica),0) + 1) 
   -- AHM TMP Debe ir al finalizar la modifcicación   INTO v_r_notifica_mdt.id_mdt_notifica
   -- AHM TMP Debe ir al finalizar la modifcicación FROM mdt_notifica_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación 
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.id_tpo_mandato      = p_tipo_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.id_cat_mandato      = p_id_cat_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.tipo_operacion      = 'M'
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion1        = v_desc_larga_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion2        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion3        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_cuenta_bancaria   = v_n_cuenta_bancaria
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_convenio          = v_n_convenio       
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_referencia        = v_n_referencia     
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.cta_clabe           = v_cta_clabe        
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.estado              = 100
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.resultado_operacion = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.diagnostico         = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.f_creacion          = TODAY
   -- AHM TMP Debe ir al finalizar la modifcicación 
   -- AHM TMP Debe ir al finalizar la modifcicación --Se inserta el registro
   -- AHM TMP Debe ir al finalizar la modifcicación INSERT INTO mdt_notifica_mandato VALUES (v_r_notifica_mdt.*)
      
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

   -- lanza actualización de movimientos por grupo
   UPDATE mdt_cat_mandato
      SET desc_mandato       = v_desc_mandato, 
          desc_larga_mandato = v_desc_larga_mandato
    WHERE id_cat_mandato     = p_id_cat_mandato

   LET v_estatus = FALSE
   FOR v_indice = 1 TO v_arr_grupos.getLength()
      -- Condiciona si el registro existe en base de datos antes de las modificaciones
      IF v_arr_grupos[v_indice].v_existe = 1 THEN
         -- Condiciona si aún sigue activo el grupo o proceder a eliminar el grupo completo
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

   -- AHM TMP Debe ir al finalizar la modifcicación --Se obtiene el maximo registro para obtener el ID a insertar
   -- AHM TMP Debe ir al finalizar la modifcicación SELECT (NVL(MAX(id_mdt_notifica),0) + 1) 
   -- AHM TMP Debe ir al finalizar la modifcicación   INTO v_r_notifica_mdt.id_mdt_notifica
   -- AHM TMP Debe ir al finalizar la modifcicación FROM mdt_notifica_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación 
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.id_tpo_mandato      = p_tipo_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.id_cat_mandato      = p_id_cat_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.tipo_operacion      = 'M'
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion1        = v_desc_larga_mandato
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion2        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.descripcion3        = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_cuenta_bancaria   = v_n_cuenta_bancaria
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_convenio          = v_n_convenio       
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.n_referencia        = v_n_referencia     
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.cta_clabe           = v_cta_clabe        
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.estado              = 100
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.resultado_operacion = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.diagnostico         = NULL
   -- AHM TMP Debe ir al finalizar la modifcicación LET v_r_notifica_mdt.f_creacion          = TODAY
   -- AHM TMP Debe ir al finalizar la modifcicación 
   -- AHM TMP Debe ir al finalizar la modifcicación --Se inserta el registro
   -- AHM TMP Debe ir al finalizar la modifcicación INSERT INTO mdt_notifica_mandato VALUES (v_r_notifica_mdt.*)
   RETURN v_estatus
END FUNCTION

###############################################################################
#Modulo            => HPS                                                     #
#Programa          => HPSM01                                                  #
#Objetivo          => Se almacena el registro en base de datos                #
#Autor             => Francisco López                                         #
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
# Entrada:          => p_gpo - Grupo que será validado si se capturaron img #
# Salida:           => v_estatus - Indica si hay imagenes capturadas a un   #
#                      grupo específico. Verdadero si hay captura, falso    #
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
-- #Autor             => Francisco López                                         #
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
--       CALL fgl_winmessage("Advertencia","No cuenta con privilegios para esta opción","exclamation")
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
--                      --Si es difetende de 1  no se permite la modificación
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
--                IF fn_ventana_confirma("Confimar","¿Desea guardar los cambios efectuados?","info") = 1 THEN
--                   --Si confirmo el guardar los cambios se almacenan los cambios efectuados
--                   CALL fn_modifica_registros_mandato() RETURNING v_estatus
--                   IF v_estatus = TRUE THEN
--                      CALL fn_mensaje("Aviso","Modificación al mandato "|| v_desc_mandato ||" efectuada","info")
--                   ELSE
--                      CALL fn_mensaje("Aviso","Mandato con un registro previo en espera de notificación","error")
--                   END IF
--                END IF
--                EXIT DIALOG
--             ON ACTION CANCELAR
--                CALL fn_mensaje("Aviso","Modificación Cancelada","info")
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
-- #Autor             => Francisco López                                         #
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
-- #Autor             => Francisco López                                         #
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
-- #Autor             => Francisco López                                         #
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
-- #Autor             => Francisco López                                         #
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
--             --´posible efectuar los cambios ya que existe una notificación pendiente
--             IF v_bandera_query = TRUE THEN
--                LET v_bandera_existente = TRUE
--             END IF
--          END IF
--       END FOR
--    END IF
-- 
--    --Solo se almacenan los registros
--    IF v_bandera_existente = TRUE THEN
--       --Si encontro con que existe una notificacion pendiente no continua con la modificación
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
--       --Se obtienen los datos necesarios para la notificación
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

