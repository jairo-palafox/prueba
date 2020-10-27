--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/12/2013
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTC02                                                     #
#Objetivo        => Consulta de tramites                                       #
#Fecha Inicio    => 27 Febrero 2015                                            #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion      SMALLINT,
       p_titulo_ventana     STRING,
       p_nss_consulta       CHAR(11),
       v_ventana            ui.Window,
       v_forma              ui.Form,
       v_ruta_ejecutables   LIKE seg_modulo.ruta_bin,
       v_ruta_listados      LIKE seg_modulo.ruta_listados,
       g_detalle_bloque_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta_det_sol   LIKE bus_detalle_solicitud.nombre_campo,
         v_etiqueta           LIKE bus_detalle_solicitud.nombre_campo,
         v_valor              LIKE bus_detalle_solicitud.valor
       END RECORD

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
MAIN
DEFINE v_ruta LIKE seg_modulo.ruta_bin

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tpo_ejecucion  = ARG_VAL(2)
   LET p_titulo_ventana = ARG_VAL(3)
   LET p_nss_consulta   = ARG_VAL(4)
   
   # prueba con bd
   -- DATABASE safre_busd

   # Recupera las rutas del módulo BUS
   CALL fn_rutas("prt") RETURNING v_ruta_ejecutables, v_ruta_listados
   
      
   # inicializa las consultas utilizadas en el programa
   CALL fn_inicializa_consultas()
   
   # llama ventana para elegir modulo
   CALL fn_consulta_tramite()
   
END MAIN

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función principal e inicialización de variables          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " SELECT FIRST 1 modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE modulo_cod = ?",
                    "  ORDER BY 1"
   PREPARE prp_recupera_desc_modulo FROM v_consulta

   LET v_consulta = " SELECT desc_proceso_bus",    
                    "   FROM cat_bus_proceso",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_recupera_desc_proceso FROM v_consulta

   LET v_consulta = " SELECT desc_opera_bus",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_operacion = ?"
   PREPARE prp_recupera_desc_operacion FROM v_consulta

   LET v_consulta = " SELECT desc_contrato",
                    "   FROM cat_bus_contrato",
                    "  WHERE id_cat_bus_contrato = ?"
   PREPARE prp_recupera_desc_contrato FROM v_consulta 

   # verifica si existe el folio a consultar
   LET v_consulta = " SELECT FIRST 1 NVL(1,0)",
                    "   FROM bus_tramite",
                    "  WHERE folio_procesar = ?"
   PREPARE prp_existe_folio_porcesar FROM v_consulta

   
   

   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor
   LET v_consulta = "EXECUTE FUNCTION fn_glo_recupera_etiquetas_id(?,?,?,?,?,?,?)"
   PREPARE prp_recupera_etiquetas FROM v_consulta

   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor
   LET v_consulta = "EXECUTE FUNCTION fn_prt_recupera_etiquetas_bloque(?,?,?,?,?,?,?)"
   PREPARE prp_recupera_etiquetas_bloque FROM v_consulta
   
   # Recupera los errores de solicitud
   LET v_consulta = " SELECT cod_error,",
                    "        desc_error",
                    "   FROM bus_error_solicitud",
                    "  WHERE id_bus_solicitud_tramite = ?"
   PREPARE prp_rec_errores_solicitud FROM v_consulta
   
   # Recupera los errores de respuesta
   LET v_consulta = " SELECT cod_error,",
                    "        desc_error",
                    "   FROM bus_error_respuesta",
                    "  WHERE id_bus_respuesta_tramite = ?"
   PREPARE prp_rec_errores_respuesta FROM v_consulta
   
   # Recupera los módulos
   LET v_consulta = " SELECT modulo_cod,",
                    "        modulo_desc",
                    "   FROM seg_modulo",
                    "  WHERE 1 = 1",
                    "  ORDER BY modulo_desc"
   PREPARE prp_recupera_modulos FROM v_consulta
   
   # Recupera datos del proceso
   LET v_consulta = " SELECT id_cat_bus_proceso,",
                    "        desc_proceso_bus",
                    "   FROM cat_bus_proceso",
                    "  WHERE modulo_cod = ?"
   PREPARE prp_recupera_proceso FROM v_consulta 
   
   # Recupera datos de la operación
   LET v_consulta = " SELECT id_cat_bus_operacion,",
                    "        desc_opera_bus",
                    "   FROM cat_bus_operacion",
                    "  WHERE id_cat_bus_proceso = ?"
   PREPARE prp_recupera_operacion FROM v_consulta
   
   # Recupera datos del contrato
   LET v_consulta = " SELECT id_cat_bus_contrato,",
                    "        desc_contrato",
                    "   FROM cat_bus_contrato",
                    "  WHERE id_cat_bus_operacion = ?"
   PREPARE prp_recupera_contrato FROM v_consulta
   
   # Recupera id de bloque relacionado al contrato. La relacion es uno a uno
   LET v_consulta = " SELECT id_cat_bus_bloque",
                    "   FROM cat_bus_bloque",
                    "  WHERE id_cat_bus_detalle_contrato = ?"
   PREPARE prp_recupera_id_cat_bloque FROM v_consulta

END FUNCTION


################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC01                                                   #
#Descripcion       => Función elegir tipo de consulta del tramite              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                         #
################################################################################
FUNCTION fn_consulta_tramite()
DEFINE v_tipo_consulta  SMALLINT,
       v_folio_procesar LIKE bus_tramite.folio_procesar,
       r_existe         BOOLEAN

   IF( p_nss_consulta CLIPPED IS NOT NULL OR LENGTH(p_nss_consulta CLIPPED) > 0 )THEN
      # Consulta externa de tramite bus
      CALL fn_consulta_tramite_bus("",p_nss_consulta)
   ELSE
      OPEN WINDOW vtna_captura_filtro_consulta WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC021"

         LET v_ventana = ui.Window.getCurrent()
         IF( p_titulo_ventana IS NOT NULL )THEN
            CALL v_ventana.setText(p_titulo_ventana)
            CALL ui.Interface.setText(p_titulo_ventana)
         END IF

         INPUT v_tipo_consulta, # 1 = individual, 2 = detalle
               v_folio_procesar
          FROM tipo_consulta,
               folio_procesar ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

            ON ACTION aceptar
               # valida captura de folio
               IF(v_tipo_consulta = 1)THEN # tipo consulta individual
                  IF( v_folio_procesar IS NULL )THEN
                     CALL fn_mensaje("AVISO","Capture folio procesar","information")
                     NEXT FIELD folio_procesar
                  END IF
                  # verifica si existe el folio porcesar en bus_tramite               
                  EXECUTE prp_existe_folio_porcesar USING v_folio_procesar
                                                     INTO r_existe
                  IF NOT( r_existe )THEN
                     CALL fn_mensaje("AVISO","Folio no existe","information")
                     NEXT FIELD folio_procesar
                  END IF
                  # individual
                  CALL fn_consulta_tramite_bus(v_folio_procesar,"")
               ELSE # Detalle
                  CALL fn_consulta_detalle_tramite()
               END IF
               CONTINUE INPUT
            
            ON ACTION cancelar
               EXIT INPUT

         END INPUT

      CLOSE WINDOW vtna_captura_filtro_consulta
   END IF

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para filtrar la consulta detalle del tramite     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_consulta_detalle_tramite()
DEFINE v_tipo_consulta        CHAR(1),
       v_modulo_cod           LIKE cat_bus_proceso.modulo_cod, 
       v_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       v_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       v_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       v_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       v_nss                  LIKE bus_solicitud_tramite.nss,
       v_curp                 LIKE bus_solicitud_tramite.curp,
       v_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       v_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       v_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       v_cb_modulo            ui.ComboBox,
       v_cb_proceso           ui.ComboBox,
       v_cb_operacion         ui.ComboBox,
       v_cb_contrato          ui.ComboBox,
       r_tramites DYNAMIC ARRAY OF RECORD         
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD

   OPEN WINDOW vtna_filtra_consulta_detalle WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC023"

      LET v_ventana = ui.Window.getCurrent()
      LET v_forma   = v_ventana.getForm() 
      IF( p_titulo_ventana IS NOT NULL )THEN
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF
      # captura filtros de consulta detalle
      INPUT v_tipo_consulta,
            v_modulo_cod, 
            v_id_cat_bus_proceso,
            v_id_cat_bus_operacion,
            v_id_cat_bus_contrato,
            v_f_operacion_ini,
            v_f_operacion_fin,
            v_f_operacion_ack_ini,
            v_f_operacion_ack_fin,
            v_nss,
            v_curp,
            v_cod_transaccion_opr,
            v_cod_respuesta,
            v_cod_respuesta_opr
            
       FROM tipo_consulta,
            modulo_cod, 
            id_cat_bus_proceso,
            id_cat_bus_operacion,
            id_cat_bus_contrato,
            f_operacion_inicio,
            f_operacion_fin,
            f_operacion_ack_inicio,
            f_proceso_ack_fin,
            nss,
            curp,
            cod_transaccion_opr,
            cod_respuesta,
            cod_respuesta_opr ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE, UNBUFFERED)

         BEFORE INPUT
            # recupera atributos de combos
            LET v_cb_modulo    = ui.ComboBox.forName("formonly.modulo_cod")
            LET v_cb_proceso   = ui.ComboBox.forName("formonly.id_cat_bus_proceso")
            LET v_cb_operacion = ui.ComboBox.forName("formonly.id_cat_bus_operacion")
            LET v_cb_contrato  = ui.ComboBox.forName("formonly.id_cat_bus_contrato")
            # llena combo de modulos
            CALL fn_llena_combo_modulo(v_cb_modulo)

         ON CHANGE modulo_cod
            INITIALIZE v_id_cat_bus_proceso,
                       v_id_cat_bus_operacion,
                       v_id_cat_bus_contrato TO NULL
            CALL v_cb_proceso.clear()            
            CALL v_cb_operacion.clear()
            CALL v_cb_contrato.clear()
            CALL fn_llena_combo_proceso(v_cb_proceso,v_modulo_cod)

         ON CHANGE id_cat_bus_proceso
            INITIALIZE v_id_cat_bus_operacion,
                       v_id_cat_bus_contrato TO NULL
            CALL v_cb_operacion.clear()
            CALL v_cb_contrato.clear()
            CALL fn_llena_combo_operacion(v_cb_operacion,v_id_cat_bus_proceso)

         ON CHANGE id_cat_bus_operacion
            INITIALIZE v_id_cat_bus_contrato TO NULL
            CALL v_cb_contrato.clear()
            CALL fn_llena_combo_contrato(v_cb_contrato,v_id_cat_bus_operacion)

         ON ACTION aceptar
            IF( v_tipo_consulta = 1 )THEN # todas
               # son obligatorios los campos modulo, proceso, operacion y contrato para el caso de todas
               IF( v_modulo_cod IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture módulo","information")
                  NEXT FIELD modulo_cod
               END IF
               IF( v_id_cat_bus_proceso IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture proceso","information")
                  NEXT FIELD id_cat_bus_proceso
               END IF
               IF( v_id_cat_bus_operacion IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture operación","information")
                  NEXT FIELD id_cat_bus_operacion
               END IF
               IF( v_id_cat_bus_contrato IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture contrato","information")
                  NEXT FIELD id_cat_bus_contrato
               END IF
            ELSE
               # es obligatoria la fecha operacion para el caso excepxiones
               IF( v_f_operacion_ini IS NULL AND v_f_operacion_fin IS NULL)THEN
                  CALL fn_mensaje("AVISO","Capture fecha operación","information")
                  NEXT FIELD f_operacion_inicio
               END IF
            END IF
            IF(v_f_operacion_ini IS NOT NULL AND v_f_operacion_fin IS NOT NULL AND v_f_operacion_ini > v_f_operacion_fin)THEN
               CALL fn_mensaje("AVISO","Fecha operación inicial no puede ser mayor a fecha operación final","information")
               NEXT FIELD f_operacion_fin
            END IF
            IF(v_f_operacion_ack_ini IS NOT NULL AND v_f_operacion_ack_fin IS NOT NULL AND v_f_operacion_ack_ini > v_f_operacion_ack_fin)THEN
               CALL fn_mensaje("AVISO","Fecha operación ACK inicial no puede ser mayor a fecha operación ACK final","information")
               NEXT FIELD f_operacion_ack_fin
            END IF

            # recupera el detalle del tramite
            CALL r_tramites.clear()
            --CALL r_tramites_excepciones.clear()
            CALL fn_recupera_detalle_tramite(v_tipo_consulta,
                                             v_modulo_cod, 
                                             v_id_cat_bus_proceso,
                                             v_id_cat_bus_operacion,
                                             v_id_cat_bus_contrato,
                                             v_f_operacion_ini,
                                             v_f_operacion_fin,
                                             v_f_operacion_ack_ini,
                                             v_f_operacion_ack_fin,
                                             v_nss,
                                             v_curp,
                                             v_cod_transaccion_opr,
                                             v_cod_respuesta,
                                             v_cod_respuesta_opr) RETURNING r_tramites--, r_tramites_excepciones
                                             
            IF( v_tipo_consulta = 1 )THEN # todas
               
               IF( r_tramites.getLength() = 0 )THEN
                  CALL fn_mensaje("AVISO","No se encontraron registros con criterio dado","information")
                  CONTINUE INPUT
               END IF
               CALL fn_muestra_tramite_detalle_todos(v_modulo_cod,
                                                     v_id_cat_bus_proceso,
                                                     v_id_cat_bus_operacion,
                                                     v_id_cat_bus_contrato,
                                                     v_f_operacion_ini,
                                                     v_f_operacion_fin,
                                                     v_f_operacion_ack_ini,
                                                     v_f_operacion_ack_fin,
                                                     v_nss,
                                                     v_curp,
                                                     v_cod_transaccion_opr,
                                                     v_cod_respuesta,
                                                     v_cod_respuesta_opr,
                                                     r_tramites)
            ELSE
               IF( r_tramites.getLength() = 0 )THEN
                  CALL fn_mensaje("AVISO","No se encontraron registros con criterio dado","information")
                  CONTINUE INPUT
               END IF
               CALL fn_muestra_tramite_detalle_excepciones(v_f_operacion_ini,
                                                           v_f_operacion_fin,
                                                           r_tramites)
               

            END IF
            
         ON ACTION cancelar
            EXIT INPUT

      END INPUT

   CLOSE WINDOW vtna_filtra_consulta_detalle

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para consultar el detalle del tramite por tipo   #
#                     consulta excepciones                                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_muestra_tramite_detalle_excepciones(p_f_operacion_ini,p_f_operacion_fin,p_tramites_excepciones)
DEFINE p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_tramites_excepciones DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       r_detalle_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, --VARCHAR(40),
         v_valor    LIKE bus_detalle_solicitud.valor --VARCHAR(200)
       END RECORD,
       r_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, 
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_errores_solicitud DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       r_errores_respuesta DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_id_col_cat_bloque DECIMAL(9,0),
       v_cad_periodo STRING,
       v_indice   SMALLINT,
       v_mensaje  STRING,
       v_error    BOOLEAN
       
   OPEN WINDOW vtna_consulta_tramite_detalle_excepciones WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC025"

      LET v_ventana = ui.Window.getCurrent()
      IF( p_titulo_ventana IS NOT NULL )THEN
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF

      DIALOG ATTRIBUTES(UNBUFFERED)

         DISPLAY ARRAY p_tramites_excepciones TO sr_tramite.*

            BEFORE ROW
               LET v_indice = ARR_CURR()
               CALL r_detalle_solicitud.clear()
               CALL r_detalle_bloque.clear()
               # Recupera las etiquetas para la tabla bus_detalle_solicitud correspondiente a la tabla de la forma detalle contrato
               CALL fn_recupera_etiquetas("bus_detalle_solicitud", # Tabla que contiene los valores
                                           p_tramites_excepciones[v_indice].v_id_cat_bus_contrato, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                           p_tramites_excepciones[v_indice].v_id_solicitid, # identificador de la solicitud para recuperar los registros del detalle
                                           "nombre_campo",
                                           "valor",
                                           "cat_bus_detalle_contrato",
                                           "cve_natural") RETURNING v_error, 
                                                                    v_mensaje,
                                                                    r_detalle_solicitud
                
               CALL r_errores_solicitud.clear()
               # Recupera las etiquetas para la tabla bus_error_solicitud correspondiente a la tabla de la forma errores solicitud
               CALL fn_recupera_errores_solicitud(p_tramites_excepciones[v_indice].v_id_solicitid) RETURNING r_errores_solicitud
               
               CALL r_errores_respuesta.clear()
               # Recupera las etiquetas para la tabla bus_error_respuesta correspondiente a la tabla de la forma errores respuesta
               CALL fn_recupera_errores_respuesta(p_tramites_excepciones[v_indice].v_id_respuesta) RETURNING r_errores_respuesta
               
         END DISPLAY

         DISPLAY ARRAY r_detalle_solicitud TO sr_detalle_solicitud.*

            BEFORE ROW
               CALL r_detalle_bloque.clear()
               EXECUTE prp_recupera_id_cat_bloque USING r_detalle_solicitud[ARR_CURR()].v_id_col_cat_entidad 
                                                   INTO v_id_col_cat_bloque
               CALL fn_recupera_etiquetas_bloque("bus_detalle_bloque", # Tabla que contiene los valores
                                          v_id_col_cat_bloque, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                          r_detalle_solicitud[ARR_CURR()].v_id_col_entidad, # identificador de la solicitud para recuperar los registros del detalle
                                          "nombre_campo_bloque",
                                          "valor",
                                          "cat_bus_detalle_bloque", # Entidad catálogo del bloque
                                          "cve_natural_bloque") 
                                            RETURNING v_error, 
                                                      v_mensaje,
                                                      r_detalle_bloque

         END DISPLAY

         DISPLAY ARRAY r_detalle_bloque TO sr_detalle_bloque.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_solicitud TO sr_errores_solicitud.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_respuesta TO sr_errores_respuesta.* 

         END DISPLAY


         BEFORE DIALOG
            IF( p_tramites_excepciones.getLength() = 1000 )THEN # en caso de que se hayan recuperado 1000, se indica que solo se muestran lso primeros 1000, ya que no se consideraron los demas registros
               CALL fn_mensaje("AVISO","Sólo se mostraran los primeros 1000 registros, puede refinar su consulta en la pantalla anterior","information")
            END IF
            IF(p_f_operacion_ini IS NOT NULL)THEN
               IF(p_f_operacion_fin IS NOT NULL)THEN
                  LET p_f_operacion_ini = p_f_operacion_ini USING "dd-mm-yyyy"
                  LET p_f_operacion_fin = p_f_operacion_fin USING "dd-mm-yyyy"
                  LET v_cad_periodo = "Excepción del periodo "||p_f_operacion_ini||" al "||p_f_operacion_fin
               ELSE
                  LET p_f_operacion_ini = p_f_operacion_ini USING "dd-mm-yyyy"
                  LET v_cad_periodo = "Excepción a la fecha "||p_f_operacion_ini
               END IF
            ELSE
               LET p_f_operacion_fin = p_f_operacion_fin USING "dd-mm-yyyy"
               LET v_cad_periodo = "Excepción a la fecha "||p_f_operacion_fin
            END IF
            DISPLAY v_cad_periodo TO periodo

         ON ACTION traza
            CALL fn_genera_reporte_traza_bitacora("",
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  p_tramites_excepciones)
         

         ON ACTION notif_xml
            
            CALL fn_genera_reporte_notificacion_xml(p_tramites_excepciones[v_indice].v_folio_procesar,
                                           p_tramites_excepciones[v_indice].v_consecutivo, 
                                           p_tramites_excepciones[v_indice].v_origen ,
                                           p_tramites_excepciones[v_indice].v_desc_proceso,
                                           p_tramites_excepciones[v_indice].v_desc_operacion,
                                           p_tramites_excepciones[v_indice].v_id_cat_bus_contrato,
                                           p_tramites_excepciones[v_indice].v_desc_contrato,
                                           p_tramites_excepciones[v_indice].v_id_solicitid,
                                           p_tramites_excepciones[v_indice].v_folio_transaccion,
                                           p_tramites_excepciones[v_indice].v_nss,
                                           p_tramites_excepciones[v_indice].v_curp,
                                           p_tramites_excepciones[v_indice].v_cod_resp_tran,
                                           p_tramites_excepciones[v_indice].v_fecha_tran,
                                           p_tramites_excepciones[v_indice].v_id_respuesta,
                                           p_tramites_excepciones[v_indice].v_folio_ack,
                                           p_tramites_excepciones[v_indice].v_fecha_ack,
                                           p_tramites_excepciones[v_indice].v_cod_resp_ack,
                                           p_tramites_excepciones[v_indice].v_cod_resp_ope_ack,
                                           p_tramites_excepciones[v_indice].v_desc_respuesta,
                                           p_tramites_excepciones[v_indice].v_excepcion,                                           
                                           r_detalle_solicitud,
                                           r_errores_solicitud,
                                           r_errores_respuesta)
         
         ON ACTION cancelar
            EXIT DIALOG

         
      END DIALOG

   CLOSE WINDOW vtna_consulta_tramite_detalle_excepciones

       
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para consultar el detalle del tramite por tipo   #
#                     consulta todos                                           #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_muestra_tramite_detalle_todos(p_modulo_cod,
                                          p_id_cat_bus_proceso,
                                          p_id_cat_bus_operacion,
                                          p_id_cat_bus_contrato,
                                          p_f_operacion_ini,
                                          p_f_operacion_fin,
                                          p_f_operacion_ack_ini,
                                          p_f_operacion_ack_fin,
                                          p_nss,
                                          p_curp,
                                          p_cod_transaccion_opr,
                                          p_cod_respuesta,
                                          p_cod_respuesta_opr,
                                          p_tramites)
DEFINE p_modulo_cod           LIKE cat_bus_proceso.modulo_cod, 
       p_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       p_tramites DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       r_detalle_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, 
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,       
       r_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, 
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_errores_solicitud DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       r_errores_respuesta DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_mensaje  STRING,
       v_error    BOOLEAN,
       v_indice   SMALLINT,
       v_id_col_cat_bloque DECIMAL(9,0),
       r_modulo_desc   LIKE seg_modulo.modulo_desc,
       r_proceso_desc  LIKE cat_bus_proceso.desc_proceso_bus,
       r_operacio_desc LIKE cat_bus_operacion.desc_opera_bus,
       r_contrato_desc LIKE cat_bus_contrato.desc_contrato

   OPEN WINDOW vtna_consulta_tramite_detalle_todos WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC024"

      LET v_ventana = ui.Window.getCurrent()
      IF( p_titulo_ventana IS NOT NULL )THEN
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF

      DIALOG ATTRIBUTES(UNBUFFERED)

         DISPLAY ARRAY p_tramites TO sr_tramite.*

            BEFORE ROW
               LET v_indice = ARR_CURR()
               CALL r_detalle_solicitud.clear()
               CALL r_detalle_bloque.clear()
               # Recupera las etiquetas para la tabla bus_detalle_solicitud correspondiente a la tabla de la forma detalle contrato
               CALL fn_recupera_etiquetas("bus_detalle_solicitud", # Tabla que contiene los valores
                                           p_tramites[v_indice].v_id_cat_bus_contrato, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                           p_tramites[v_indice].v_id_solicitid, # identificador de la solicitud para recuperar los registros del detalle
                                           "nombre_campo",
                                           "valor",
                                           "cat_bus_detalle_contrato",
                                           "cve_natural") RETURNING v_error, 
                                                                    v_mensaje,
                                                                    r_detalle_solicitud
               
               CALL r_errores_solicitud.clear()
               # Recupera las etiquetas para la tabla bus_error_solicitud correspondiente a la tabla de la forma errores solicitud
               CALL fn_recupera_errores_solicitud(p_tramites[v_indice].v_id_solicitid) RETURNING r_errores_solicitud
               
               CALL r_errores_respuesta.clear()
               # Recupera las etiquetas para la tabla bus_error_respuesta correspondiente a la tabla de la forma errores respuesta
               CALL fn_recupera_errores_respuesta(p_tramites[v_indice].v_id_respuesta) RETURNING r_errores_respuesta
               
         END DISPLAY

         DISPLAY ARRAY r_detalle_solicitud TO sr_detalle_solicitud.*
         
            BEFORE ROW
               CALL r_detalle_bloque.clear()
               EXECUTE prp_recupera_id_cat_bloque USING r_detalle_solicitud[ARR_CURR()].v_id_col_cat_entidad 
                                                   INTO v_id_col_cat_bloque
               CALL fn_recupera_etiquetas_bloque("bus_detalle_bloque", # Tabla que contiene los valores
                                          v_id_col_cat_bloque, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                          r_detalle_solicitud[ARR_CURR()].v_id_col_entidad, # identificador de la solicitud para recuperar los registros del detalle
                                          "nombre_campo_bloque",
                                          "valor",
                                          "cat_bus_detalle_bloque", # Entidad catálogo del bloque
                                          "cve_natural_bloque") 
                                            RETURNING v_error, 
                                                      v_mensaje,
                                                      r_detalle_bloque
                                                      

         END DISPLAY

         DISPLAY ARRAY r_detalle_bloque TO sr_detalle_bloque.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_solicitud TO sr_errores_solicitud.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_respuesta TO sr_errores_respuesta.* 

         END DISPLAY


         BEFORE DIALOG
            IF( p_tramites.getLength() >= 1000 )THEN # en caso de que se hayan recuperado 1000, se indica que solo se muestran lso primeros 1000, ya que no se consideraron los demas registros
               CALL fn_mensaje("AVISO","Sólo se mostraran los primeros 1000 registros, puede refinar su consulta en la pantalla anterior","information")
            END IF
            CALL fn_recupera_descripciones_modulo(p_modulo_cod,
                                                  p_id_cat_bus_proceso,
                                                  p_id_cat_bus_operacion,
                                                  p_id_cat_bus_contrato) RETURNING r_modulo_desc,
                                                                                   r_proceso_desc,
                                                                                   r_operacio_desc,
                                                                                   r_contrato_desc
            DISPLAY r_modulo_desc   TO modulo_desc
            DISPLAY r_proceso_desc  TO desc_proceso_bus 
            DISPLAY r_operacio_desc TO desc_opera_bus
            DISPLAY r_contrato_desc TO desc_contrato

         ON ACTION traza
            CALL fn_genera_reporte_traza_bitacora(r_modulo_desc,
                                                  r_proceso_desc,
                                                  r_operacio_desc,
                                                  r_contrato_desc,
                                                  p_f_operacion_ini,
                                                  p_f_operacion_fin,
                                                  p_f_operacion_ack_ini,
                                                  p_f_operacion_ack_fin,
                                                  p_nss,
                                                  p_curp,
                                                  p_cod_transaccion_opr,
                                                  p_cod_respuesta,
                                                  p_cod_respuesta_opr,
                                                  p_tramites)
         

         ON ACTION notif_xml
            # reporte (PRTC021) notificaciones y detalle de tramite
            CALL fn_genera_reporte_notificacion_xml(p_tramites[v_indice].v_folio_procesar,
                                           p_tramites[v_indice].v_consecutivo,
                                           p_tramites[v_indice].v_origen,
                                           r_proceso_desc,
                                           r_operacio_desc,
                                           p_tramites[v_indice].v_id_cat_bus_contrato,
                                           p_tramites[v_indice].v_desc_contrato,
                                           p_tramites[v_indice].v_id_solicitid,
                                           p_tramites[v_indice].v_folio_transaccion,
                                           p_tramites[v_indice].v_nss,
                                           p_tramites[v_indice].v_curp,
                                           p_tramites[v_indice].v_cod_resp_tran,
                                           p_tramites[v_indice].v_fecha_tran,
                                           p_tramites[v_indice].v_id_respuesta,
                                           p_tramites[v_indice].v_folio_ack,
                                           p_tramites[v_indice].v_fecha_ack,
                                           p_tramites[v_indice].v_cod_resp_ack,
                                           p_tramites[v_indice].v_cod_resp_ope_ack,
                                           p_tramites[v_indice].v_desc_respuesta,
                                           p_tramites[v_indice].v_excepcion,
                                           r_detalle_solicitud,
                                           r_errores_solicitud,
                                           r_errores_respuesta)
         
         ON ACTION cancelar
            EXIT DIALOG

         
      END DIALOG

   CLOSE WINDOW vtna_consulta_tramite_detalle_todos

         
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar las descripciones de modulo,      #
#                     proceso, operación y contrato                            #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_descripciones_modulo(p_modulo_cod,
                                          p_id_cat_bus_proceso,
                                          p_id_cat_bus_operacion,
                                          p_id_cat_bus_contrato)
DEFINE p_modulo_cod           LIKE cat_bus_proceso.modulo_cod, 
       p_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_modulo_desc          LIKE seg_modulo.modulo_desc,
       v_proceso_desc         LIKE cat_bus_proceso.desc_proceso_bus,
       v_operacio_desc        LIKE cat_bus_operacion.desc_opera_bus,
       v_contrato_desc        LIKE cat_bus_contrato.desc_contrato,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254)


   # Recupera descipción del módulo
   EXECUTE prp_recupera_desc_modulo USING p_modulo_cod
                                     INTO v_modulo_desc

   # Recupera descipción del proceso
   EXECUTE prp_recupera_desc_proceso USING p_id_cat_bus_proceso
                                      INTO v_proceso_desc  

   # Recupera descipción de la operación
   EXECUTE prp_recupera_desc_operacion USING p_id_cat_bus_operacion
                                        INTO v_operacio_desc

   # Recupera descipción del contrato
   EXECUTE prp_recupera_desc_contrato USING p_id_cat_bus_contrato
                                       INTO v_contrato_desc   

    RETURN v_modulo_desc,
           v_proceso_desc,
           v_operacio_desc,
           v_contrato_desc
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                        #
################################################################################
FUNCTION fn_consulta_tramite_bus(r_folio_procesar, p_nss_consulta)
DEFINE r_folio_procesar LIKE bus_tramite.folio_procesar,
       p_nss_consulta   CHAR(11),
       r_tramite DYNAMIC ARRAY OF RECORD
         v_consecutivo       INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar    LIKE bus_tramite.folio_procesar,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,         
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente   LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       r_detalle_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       r_errores_solicitud DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       r_errores_respuesta DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_id_col_cat_bloque DECIMAL(9,0),
       v_indice   SMALLINT,
       v_mensaje  STRING,
       v_error    BOOLEAN

   OPEN WINDOW vtna_consulta_tramite_bus WITH FORM v_ruta_ejecutables CLIPPED||"/PRTC022"

      LET v_ventana = ui.Window.getCurrent()
      IF( p_titulo_ventana IS NOT NULL )THEN
         CALL v_ventana.setText(p_titulo_ventana)
         CALL ui.Interface.setText(p_titulo_ventana)
      END IF

      DIALOG ATTRIBUTES(UNBUFFERED)

         DISPLAY ARRAY r_tramite TO sr_tramite.*

            BEFORE ROW
               LET v_indice = ARR_CURR()
               LET r_folio_procesar = r_tramite[v_indice].v_folio_procesar
               DISPLAY r_folio_procesar TO folio_procesar
               
               CALL r_detalle_solicitud.clear()
               CALL r_detalle_bloque.clear()
               # Recupera las etiquetas para la tabla bus_detalle_solicitud correspondiente a la tabla de la forma detalle contrato
               CALL fn_recupera_etiquetas("bus_detalle_solicitud", # Tabla que contiene los valores
                                          r_tramite[v_indice].v_id_cat_bus_contrato, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                          r_tramite[v_indice].v_id_solicitid, # identificador de la solicitud para recuperar los registros del detalle
                                          "nombre_campo",
                                          "valor",
                                          "cat_bus_detalle_contrato",
                                          "cve_natural") RETURNING v_error, 
                                                                   v_mensaje,
                                                                   r_detalle_solicitud
               
               CALL r_errores_solicitud.clear()
               # Recupera las etiquetas para la tabla bus_error_solicitud correspondiente a la tabla de la forma errores solicitud
               CALL fn_recupera_errores_solicitud(r_tramite[v_indice].v_id_solicitid) RETURNING r_errores_solicitud
               
               CALL r_errores_respuesta.clear()
               # Recupera las etiquetas para la tabla bus_error_respuesta correspondiente a la tabla de la forma errores respuesta
               CALL fn_recupera_errores_respuesta(r_tramite[v_indice].v_id_respuesta) RETURNING r_errores_respuesta
               
         END DISPLAY

         DISPLAY ARRAY r_detalle_solicitud TO sr_detalle_solicitud.*

            BEFORE ROW
               CALL r_detalle_bloque.clear()
               EXECUTE prp_recupera_id_cat_bloque USING r_detalle_solicitud[ARR_CURR()].v_id_col_cat_entidad 
                                                   INTO v_id_col_cat_bloque
               CALL fn_recupera_etiquetas_bloque("bus_detalle_bloque", # Tabla que contiene los valores
                                          v_id_col_cat_bloque, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                          r_detalle_solicitud[ARR_CURR()].v_id_col_entidad, # identificador de la solicitud para recuperar los registros del detalle
                                          "nombre_campo_bloque",
                                          "valor",
                                          "cat_bus_detalle_bloque", # Entidad catálogo del bloque
                                          "cve_natural_bloque") 
                                            RETURNING v_error, 
                                                      v_mensaje,
                                                      r_detalle_bloque
           
         END DISPLAY

         DISPLAY ARRAY r_detalle_bloque TO sr_detalle_bloque.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_solicitud TO sr_errores_solicitud.* 

         END DISPLAY

         DISPLAY ARRAY r_errores_respuesta TO sr_errores_respuesta.* 

         END DISPLAY


         BEFORE DIALOG
            DISPLAY r_folio_procesar TO folio_procesar
            CALL fn_recupera_tramite(r_folio_procesar,p_nss_consulta) RETURNING r_tramite
            IF( r_tramite.getLength() = 0 )THEN
               CALL fn_mensaje(p_titulo_ventana,"No se encontraron registros con criterio dado","information")
               EXIT DIALOG
            END IF

         ON ACTION traza
            CALL fn_genera_reporte_traza_bitacora("",
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  r_tramite)
         

         ON ACTION notif_xml
            
            CALL fn_genera_reporte_notificacion_xml(r_folio_procesar,
                                           r_tramite[v_indice].v_consecutivo,
                                           r_tramite[v_indice].v_origen,
                                           r_tramite[v_indice].v_desc_proceso,
                                           r_tramite[v_indice].v_desc_operacion,
                                           r_tramite[v_indice].v_id_cat_bus_contrato,
                                           r_tramite[v_indice].v_desc_contrato,
                                           r_tramite[v_indice].v_id_solicitid,
                                           r_tramite[v_indice].v_folio_transaccion,
                                           r_tramite[v_indice].v_nss,
                                           r_tramite[v_indice].v_curp,
                                           r_tramite[v_indice].v_cod_resp_tran,
                                           r_tramite[v_indice].v_fecha_tran,
                                           r_tramite[v_indice].v_id_respuesta,
                                           r_tramite[v_indice].v_folio_ack,
                                           r_tramite[v_indice].v_fecha_ack,
                                           r_tramite[v_indice].v_cod_resp_ack,
                                           r_tramite[v_indice].v_cod_resp_ope_ack,
                                           r_tramite[v_indice].v_desc_respuesta,
                                           r_tramite[v_indice].v_excepcion,
                                           r_detalle_solicitud,
                                           r_errores_solicitud,
                                           r_errores_respuesta)

         
         ON ACTION cancelar
            EXIT DIALOG

         
      END DIALOG

   CLOSE WINDOW vtna_consulta_tramite_bus

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_tramite(p_folio_procesar, p_nss_consulta)
DEFINE p_folio_procesar LIKE bus_tramite.folio_procesar,
       p_nss_consulta   CHAR(11),
       v_tramite RECORD
         v_id_bus_tramite    LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar    LIKE bus_tramite.folio_procesar,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        LIKE bus_solicitud_tramite.f_operacion,
         v_h_operacion_tran  LIKE bus_solicitud_tramite.h_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         LIKE bus_respuesta_tramite.f_operacion,
         v_h_operacion_ack   LIKE bus_respuesta_tramite.h_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente   LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       v_tramites DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_indice   SMALLINT,
       v_consulta STRING

   LET v_indice = 1
   CALL v_tramites.clear()

   # Recupera el tramite de la solicitud
   LET v_consulta = "\n SELECT DISTINCT tra.id_bus_tramite,",
                    "\n        tra.folio_procesar,",
                    "\n        CASE tra.origen",
                    "\n          WHEN 0 THEN 'PROCESAR'",
                    "\n          WHEN 1 THEN 'AFORE'",
                    "\n        END CASE,",
                    "\n        pro.desc_proceso_bus,",
                    "\n        ope.desc_opera_bus,",                    
                    "\n        con.id_cat_bus_contrato,",
                    "\n        con.desc_contrato,",
                    "\n        sol.id_bus_solicitud_tramite,",
                    "\n        sol.folio_transaccion,",
                    "\n        sol.nss,",
                    "\n        sol.curp,",
                    "\n        sol.cod_respuesta_opr,",
                    "\n        sol.f_operacion,",
                    "\n        sol.h_operacion,",
                    "\n        res.id_bus_respuesta_tramite,",
                    "\n        res.folio_ack,",
                    "\n        res.f_operacion,",
                    "\n        res.h_operacion,",
                    "\n        res.cod_respuesta,",
                    "\n        res.cod_respuesta_opr,",
                    "\n        res.desc_respuesta,",
                    "\n        res.cod_oper_cliente,",
                    "\n        exc.descripcion",
                    "\n   FROM bus_tramite tra LEFT OUTER JOIN cat_bus_contrato con",
                    "\n     ON con.id_cat_bus_contrato = tra.id_cat_bus_contrato",
                    "\n        LEFT OUTER JOIN cat_bus_operacion ope",
                    "\n     ON ope.id_cat_bus_operacion = con.id_cat_bus_operacion",
                    "\n        LEFT OUTER JOIN cat_bus_proceso pro",
                    "\n     ON pro.id_cat_bus_proceso = ope.id_cat_bus_proceso",
                    "\n        LEFT OUTER JOIN bus_solicitud_tramite sol",
                    "\n     ON sol.id_bus_tramite = tra.id_bus_tramite",
                    "\n        LEFT OUTER JOIN bus_excepcion exc", # si existe la excepcion la recupera
                    "\n     ON sol.id_bus_solicitud_tramite = exc.id_bus_solicitud_tramite",
                    "\n        LEFT OUTER JOIN bus_respuesta_tramite res",
                    "\n     ON res.id_bus_tramite = tra.id_bus_tramite"
                    --"\n  WHERE tra.folio_procesar = ?"

   IF( p_nss_consulta CLIPPED IS NOT NULL OR LENGTH(p_nss_consulta CLIPPED) > 0)THEN
      LET v_consulta = v_consulta,"\n  WHERE sol.nss = '",p_nss_consulta,"'"
   ELSE
      LET v_consulta = v_consulta,"\n  WHERE tra.folio_procesar = '",p_folio_procesar,"'"
   END IF

   PREPARE prp_recupera_tramite FROM v_consulta
   DECLARE cur_recupera_tramite CURSOR FOR prp_recupera_tramite
   FOREACH cur_recupera_tramite --USING p_folio_procesar
                                 INTO v_tramite.*

      LET v_tramites[v_indice].v_consecutivo         = v_indice --v_tramite.v_id_bus_tramite
      LET v_tramites[v_indice].v_folio_procesar      = v_tramite.v_folio_procesar 
      LET v_tramites[v_indice].v_origen              = v_tramite.v_origen 
      LET v_tramites[v_indice].v_desc_proceso        = v_tramite.v_desc_proceso
      LET v_tramites[v_indice].v_desc_operacion      = v_tramite.v_desc_operacion
      LET v_tramites[v_indice].v_id_cat_bus_contrato = v_tramite.v_id_cat_bus_contrato
      LET v_tramites[v_indice].v_desc_contrato = v_tramite.v_desc_contrato
      LET v_tramites[v_indice].v_id_solicitid        = v_tramite.v_id_solicitid
      LET v_tramites[v_indice].v_folio_transaccion   = v_tramite.v_folio_transaccion
      LET v_tramites[v_indice].v_nss                 = v_tramite.v_nss
      LET v_tramites[v_indice].v_curp                = v_tramite.v_curp
      LET v_tramites[v_indice].v_cod_resp_tran       = v_tramite.v_cod_resp_tran
      LET v_tramites[v_indice].v_fecha_tran          = v_tramite.v_fecha_tran USING "dd-mm-yyyy"," ",v_tramite.v_h_operacion_tran
      LET v_tramites[v_indice].v_id_respuesta        = v_tramite.v_id_respuesta
      LET v_tramites[v_indice].v_folio_ack           = v_tramite.v_folio_ack
      LET v_tramites[v_indice].v_fecha_ack           = v_tramite.v_fecha_ack USING "dd-mm-yyyy"," ",v_tramite.v_h_operacion_ack
      LET v_tramites[v_indice].v_cod_resp_ack        = v_tramite.v_cod_resp_ack
      LET v_tramites[v_indice].v_cod_resp_ope_ack    = v_tramite.v_cod_resp_ope_ack
      LET v_tramites[v_indice].v_desc_respuesta      = v_tramite.v_desc_respuesta
      LET v_tramites[v_indice].v_cod_ope_cliente     = v_tramite.v_cod_ope_cliente
      LET v_tramites[v_indice].v_excepcion           = v_tramite.v_excepcion

      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_recupera_tramite

   RETURN v_tramites
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_etiquetas(p_entidad,
                               p_id_contrato,
                               p_id_registro_entidad,
                               p_col_campo,
                               p_col_valor,
                               p_entidad_catalogo,
                               p_col_cve_natural)
                               
DEFINE p_entidad             VARCHAR(40),
       p_id_contrato         LIKE cat_bus_contrato.id_cat_bus_contrato,
       p_id_registro_entidad DECIMAL(9,0),
       p_col_campo           VARCHAR(50),
       p_col_valor           VARCHAR(50),
       p_entidad_catalogo    VARCHAR(50),
       p_col_cve_natural     VARCHAR(50),
       v_registro RECORD
         v_ind        SMALLINT,
         v_diag       CHAR(3),
         v_error_sql  INTEGER,
         v_isam_error INTEGER,
         v_msg_error  CHAR(1024),
         v_etiqueta   LIKE bus_detalle_solicitud.nombre_campo,
         v_valor      LIKE bus_detalle_solicitud.valor,
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0)
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor         
       END RECORD,
       v_indice   SMALLINT,
       v_mensaje  STRING,
       v_error    BOOLEAN

   LET v_error   = FALSE
   LET v_indice  = 1
   LET v_mensaje = ""
   
   DECLARE cur_recupera_etiquetas CURSOR FOR prp_recupera_etiquetas
   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor   
   FOREACH cur_recupera_etiquetas USING p_entidad,
                                        p_id_contrato,
                                        p_id_registro_entidad,
                                        p_col_campo,
                                        p_col_valor,
                                        p_entidad_catalogo,
                                       p_col_cve_natural
                                   INTO v_registro.*
      LET r_registros[v_indice].v_id_col_entidad     = v_registro.v_id_col_entidad
      LET r_registros[v_indice].v_id_col_cat_entidad = v_registro.v_id_col_cat_entidad
      LET r_registros[v_indice].v_etiqueta           = v_registro.v_etiqueta CLIPPED
      LET r_registros[v_indice].v_valor              = v_registro.v_valor    CLIPPED
      LET v_indice = v_indice + 1
   END FOREACH
   
   IF(v_registro.v_ind <> 0)THEN  
      # en caso de error de validacion se recupera el mensaje   
      LET v_mensaje = v_registro.v_msg_error
      LET v_error   = TRUE
   ELSE
      IF(v_registro.v_error_sql <> 0)THEN
         # en caso de ejecucion, se indica mensaje de error
         LET v_mensaje = "No se pudieron recuperar las etiquetas"
         LET v_error   = TRUE
      END IF      
   END IF
   
   
   FREE cur_recupera_etiquetas

   RETURN v_error,
          v_mensaje,
          r_registros
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para verificar si existe el folio capturado      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_etiquetas_bloque(p_entidad,
                                      p_id_contrato,
                                      p_id_registro_entidad,
                                      p_col_campo,
                                      p_col_valor,
                                      p_entidad_catalogo,
                                      p_col_cve_natural)
                               
DEFINE p_entidad             VARCHAR(40),
       p_id_contrato         LIKE cat_bus_contrato.id_cat_bus_contrato,
       p_id_registro_entidad DECIMAL(9,0),
       p_col_campo           VARCHAR(50),
       p_col_valor           VARCHAR(50),
       p_entidad_catalogo    VARCHAR(50),
       p_col_cve_natural     VARCHAR(50),
       v_registro RECORD
         v_ind        SMALLINT,
         v_diag       CHAR(3),
         v_error_sql  INTEGER,
         v_isam_error INTEGER,
         v_msg_error  CHAR(1024),
         v_etiqueta   LIKE bus_detalle_solicitud.nombre_campo,
         v_valor      LIKE bus_detalle_solicitud.valor,
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0)
       END RECORD,
       r_registros DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor         
       END RECORD,
       v_indice   SMALLINT,
       v_mensaje  STRING,
       v_error    BOOLEAN

   LET v_error   = FALSE
   LET v_indice  = 1
   LET v_mensaje = ""
   
   DECLARE cur_recupera_etiquetas_bloque CURSOR FOR prp_recupera_etiquetas_bloque
   # funcion para recuperar las etiquetas de una entidad(tabla) y su valor   
   FOREACH cur_recupera_etiquetas_bloque USING p_entidad,
                                        p_id_contrato,
                                        p_id_registro_entidad,
                                        p_col_campo,
                                        p_col_valor,
                                        p_entidad_catalogo,
                                        p_col_cve_natural
                                   INTO v_registro.*
      LET r_registros[v_indice].v_id_col_entidad     = v_registro.v_id_col_entidad
      LET r_registros[v_indice].v_id_col_cat_entidad = v_registro.v_id_col_cat_entidad
      LET r_registros[v_indice].v_etiqueta           = v_registro.v_etiqueta CLIPPED
      LET r_registros[v_indice].v_valor              = v_registro.v_valor    CLIPPED
      LET v_indice = v_indice + 1
   END FOREACH
   
   IF(v_registro.v_ind <> 0)THEN  
      # en caso de error de validacion se recupera el mensaje   
      LET v_mensaje = v_registro.v_msg_error
      LET v_error   = TRUE
   ELSE
      IF(v_registro.v_error_sql <> 0)THEN
         # en caso de ejecucion, se indica mensaje de error
         LET v_mensaje = "No se pudieron recuperar las etiquetas"
         LET v_error   = TRUE
      END IF      
   END IF
   
   
   FREE cur_recupera_etiquetas_bloque

   RETURN v_error,
          v_mensaje,
          r_registros
END FUNCTION
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar los errores de solicitud          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_errores_solicitud(p_id_bus_solicitud_tramite)
DEFINE p_id_bus_solicitud_tramite LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
       v_errores DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       v_error RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_rec_errores_solicitud CURSOR FOR prp_rec_errores_solicitud
   FOREACH cur_rec_errores_solicitud USING p_id_bus_solicitud_tramite
                                      INTO v_error.*
      LET v_errores[v_indice].v_codigo      = v_error.v_codigo
      LET v_errores[v_indice].v_descripcion = v_error.v_descripcion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_errores_solicitud
   
   RETURN v_errores
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para recuperar los errores de respuesta          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_errores_respuesta(p_id_bus_respuesta_tramite)
DEFINE p_id_bus_respuesta_tramite LIKE bus_error_respuesta.id_bus_respuesta_tramite,
       v_errores DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_error RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   DECLARE cur_rec_errores_respuesta CURSOR FOR prp_rec_errores_respuesta
   FOREACH cur_rec_errores_respuesta USING p_id_bus_respuesta_tramite
                                      INTO v_error.*
      LET v_errores[v_indice].v_codigo      = v_error.v_codigo
      LET v_errores[v_indice].v_descripcion = v_error.v_descripcion
      LET v_indice = v_indice + 1
   END FOREACH
   FREE cur_rec_errores_respuesta
   
   RETURN v_errores
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para llenar combo de modulos                     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                        #
################################################################################
FUNCTION fn_llena_combo_modulo(p_cb_modulo)
DEFINE p_cb_modulo  ui.ComboBox,
       v_error_sql  INTEGER,
       v_error_isam INTEGER,
       v_msg_sql    CHAR(254),
       v_modulo_cod  LIKE seg_modulo.modulo_cod,
       v_modulo_desc LIKE seg_modulo.modulo_desc

   CALL p_cb_modulo.clear()
   DECLARE cur_recupera_modulos CURSOR FOR prp_recupera_modulos
   FOREACH cur_recupera_modulos INTO v_modulo_cod,
                                     v_modulo_desc
      CALL p_cb_modulo.addItem(v_modulo_cod,v_modulo_desc CLIPPED)

   END FOREACH
   FREE cur_recupera_modulos

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para llenar combo de procesos                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_llena_combo_proceso(p_cb_proceso,p_modulo_cod)
DEFINE p_cb_proceso         ui.ComboBox,
       p_modulo_cod         LIKE cat_bus_proceso.modulo_cod, 
       v_id_cat_bus_proceso LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_desc_proceso_bus   LIKE cat_bus_proceso.desc_proceso_bus

   CALL p_cb_proceso.clear()
   DECLARE cur_recupera_proceso CURSOR FOR prp_recupera_proceso
   FOREACH cur_recupera_proceso USING p_modulo_cod
                                 INTO v_id_cat_bus_proceso,
                                      v_desc_proceso_bus
      CALL p_cb_proceso.addItem(v_id_cat_bus_proceso,v_desc_proceso_bus CLIPPED)

   END FOREACH
   FREE cur_recupera_proceso

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para llenar combo de operaciones                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_llena_combo_operacion(p_cb_operacion,p_id_cat_bus_proceso)
DEFINE p_cb_operacion         ui.ComboBox,
       p_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       v_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_desc_opera_bus       LIKE cat_bus_operacion.desc_opera_bus

   CALL p_cb_operacion.clear()
   DECLARE cur_recupera_operacion CURSOR FOR prp_recupera_operacion
   FOREACH cur_recupera_operacion USING p_id_cat_bus_proceso 
                                   INTO v_id_cat_bus_operacion,
                                        v_desc_opera_bus
      CALL p_cb_operacion.addItem(v_id_cat_bus_operacion,v_desc_opera_bus CLIPPED)

   END FOREACH
   FREE cur_recupera_operacion

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para llenar combo de contratos                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                        #
################################################################################
FUNCTION fn_llena_combo_contrato(p_cb_contrato,p_id_cat_bus_operacion)
DEFINE p_cb_contrato          ui.ComboBox,
       p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       v_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       v_desc_contrato        LIKE cat_bus_contrato.desc_contrato

   CALL p_cb_contrato.clear()
   DECLARE cur_recupera_contrato CURSOR FOR prp_recupera_contrato 
   FOREACH cur_recupera_contrato USING p_id_cat_bus_operacion
                                  INTO v_id_cat_bus_contrato,
                                       v_desc_contrato
      CALL p_cb_contrato.addItem(v_id_cat_bus_contrato,v_desc_contrato CLIPPED)

   END FOREACH
   FREE cur_recupera_contrato

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Función para llenar combo de contratos                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_recupera_detalle_tramite(p_tipo_consulta,
                                     p_modulo_cod,
                                     p_id_cat_bus_proceso,
                                     p_id_cat_bus_operacion,
                                     p_id_cat_bus_contrato,
                                     p_f_operacion_ini,
                                     p_f_operacion_fin,
                                     p_f_operacion_ack_ini,
                                     p_f_operacion_ack_fin,
                                     p_nss,
                                     p_curp,
                                     p_cod_transaccion_opr,
                                     p_cod_respuesta,
                                     p_cod_respuesta_opr)
DEFINE p_tipo_consulta        SMALLINT,
       p_modulo_cod           LIKE cat_bus_proceso.modulo_cod, 
       p_id_cat_bus_proceso   LIKE cat_bus_proceso.id_cat_bus_proceso,
       p_id_cat_bus_operacion LIKE cat_bus_operacion.id_cat_bus_operacion,
       p_id_cat_bus_contrato  LIKE cat_bus_contrato.id_cat_bus_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       v_consulta             STRING,
       v_filtro               STRING,
       v_tramites_todas DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_tramite_todas RECORD
         --v_id_bus_tramite      LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          LIKE bus_solicitud_tramite.f_operacion,
         v_h_operacion_tran    LIKE bus_solicitud_tramite.h_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           LIKE bus_respuesta_tramite.f_operacion,
         v_h_operacion_ack     LIKE bus_respuesta_tramite.h_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_tramite_excepciones RECORD
         --v_id_bus_tramite      LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,         
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          LIKE bus_solicitud_tramite.f_operacion,
         v_h_operacion_tran    LIKE bus_solicitud_tramite.h_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           LIKE bus_respuesta_tramite.f_operacion,
         v_h_operacion_ack     LIKE bus_respuesta_tramite.h_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_indice SMALLINT

   LET v_indice = 1
   LET v_filtro = " 1 = 1 "

   IF( p_modulo_cod CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND pro.modulo_cod = '"||p_modulo_cod||"'"
   END IF
   IF( p_id_cat_bus_proceso CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND pro.id_cat_bus_proceso = "||p_id_cat_bus_proceso
   END IF
   IF( p_id_cat_bus_operacion CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND ope.id_cat_bus_operacion = "||p_id_cat_bus_operacion
    END IF
   IF (p_id_cat_bus_contrato CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND con.id_cat_bus_contrato = "||p_id_cat_bus_contrato
    END IF
   IF( p_f_operacion_ini CLIPPED IS NOT NULL )THEN
      IF( p_f_operacion_fin CLIPPED IS NOT NULL )THEN
         LET v_filtro = v_filtro||" AND sol.f_operacion BETWEEN '"||p_f_operacion_ini||"' AND '"||p_f_operacion_fin||"'"
       ELSE
         LET v_filtro = v_filtro||" AND sol.f_operacion = '"||p_f_operacion_ini||"'"
       END IF
   ELSE
      IF( p_f_operacion_fin CLIPPED IS NOT NULL )THEN
         LET v_filtro = v_filtro||" AND sol.f_operacion = '"||p_f_operacion_fin||"'"
       END IF
   END IF
   IF( p_f_operacion_ack_ini CLIPPED IS NOT NULL )THEN
      IF( p_f_operacion_ack_fin CLIPPED IS NOT NULL )THEN
         LET v_filtro = v_filtro||" AND res.f_operacion BETWEEN '"||p_f_operacion_ack_ini||"' AND '"||p_f_operacion_ack_fin||"'"
       ELSE
         LET v_filtro = v_filtro||" AND res.f_operacion = '"||p_f_operacion_ack_ini||"'"            
       END IF
   ELSE
      IF(p_f_operacion_ack_fin CLIPPED IS NOT NULL)THEN
         LET v_filtro = v_filtro||" AND res.f_operacion = '"||p_f_operacion_ack_fin||"'"         
       END IF
   END IF
   IF( p_nss CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND sol.nss = '"||p_nss||"'"
    END IF
   IF( p_curp CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND sol.curp = '"||p_curp||"'"
    END IF
   IF( p_cod_transaccion_opr CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND sol.cod_respuesta_opr = '"||p_cod_transaccion_opr||"'"
    END IF
   IF( p_cod_respuesta CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND res.cod_respuesta = '"||p_cod_respuesta||"'"
    END IF
   IF( p_cod_respuesta_opr CLIPPED IS NOT NULL )THEN
      LET v_filtro = v_filtro||" AND res.cod_respuesta_opr = '"||p_cod_respuesta_opr||"'"
   END IF

   IF( p_tipo_consulta = 1 )THEN # todas
      LET v_consulta = "\n SELECT FIRST 1000 ",--tra.id_bus_tramite,",
                       "\n        tra.folio_procesar,",
                       "\n        CASE tra.origen",
                       "\n          WHEN 0 THEN 'PROCESAR'",
                       "\n          WHEN 1 THEN 'AFORE'",
                       "\n        END CASE,",
                       "\n        pro.desc_proceso_bus,",
                       "\n        ope.desc_opera_bus,",
                       "\n        con.id_cat_bus_contrato,",
                       "\n        con.desc_contrato,",
                       "\n        sol.id_bus_solicitud_tramite,",
                       "\n        sol.folio_transaccion,",
                       "\n        sol.nss,",
                       "\n        sol.curp,",
                       "\n        sol.cod_respuesta_opr,",
                       "\n        sol.f_operacion,",
                       "\n        sol.h_operacion,",
                       "\n        res.id_bus_respuesta_tramite,",
                       "\n        res.folio_ack,",
                       "\n        res.f_operacion,",
                       "\n        res.h_operacion,",
                       "\n        res.cod_respuesta,",
                       "\n        res.cod_respuesta_opr,",
                       "\n        res.desc_respuesta,",
                       "\n        res.cod_oper_cliente,",
                       "\n        exc.descripcion",
                       "\n   FROM bus_tramite tra LEFT OUTER JOIN cat_bus_contrato con",
                       "\n     ON con.id_cat_bus_contrato = tra.id_cat_bus_contrato",
                       "\n        LEFT OUTER JOIN cat_bus_operacion ope",
                       "\n     ON ope.id_cat_bus_operacion = con.id_cat_bus_operacion",
                       "\n        LEFT OUTER JOIN cat_bus_proceso pro",
                       "\n     ON pro.id_cat_bus_proceso = ope.id_cat_bus_proceso",
                       "\n        LEFT OUTER JOIN bus_solicitud_tramite sol",
                       "\n     ON sol.id_bus_tramite = tra.id_bus_tramite",
                       "\n        LEFT OUTER JOIN bus_excepcion exc", # si existe la excepcion la recupera
                       "\n     ON sol.id_bus_solicitud_tramite = exc.id_bus_solicitud_tramite",
                       "\n        LEFT OUTER JOIN bus_respuesta_tramite res",
                       "\n     ON res.id_bus_tramite = tra.id_bus_tramite",
                       "\n  WHERE ",v_filtro

      PREPARE prp_rec_detalle_todas_tramites FROM v_consulta
      DECLARE cur_rec_detalle_todas_tramites CURSOR FOR prp_rec_detalle_todas_tramites
      FOREACH cur_rec_detalle_todas_tramites INTO v_tramite_todas.*
         LET v_tramites_todas[v_indice].v_consecutivo         = v_indice--v_tramite_todas.v_id_bus_tramite
         LET v_tramites_todas[v_indice].v_folio_procesar      = v_tramite_todas.v_folio_procesar
         LET v_tramites_todas[v_indice].v_origen              = v_tramite_todas.v_origen
         LET v_tramites_todas[v_indice].v_desc_proceso        = v_tramite_todas.v_desc_proceso
         LET v_tramites_todas[v_indice].v_desc_operacion      = v_tramite_todas.v_desc_operacion
         LET v_tramites_todas[v_indice].v_id_cat_bus_contrato = v_tramite_todas.v_id_cat_bus_contrato
         LET v_tramites_todas[v_indice].v_desc_contrato       = v_tramite_todas.v_desc_contrato
         LET v_tramites_todas[v_indice].v_id_solicitid        = v_tramite_todas.v_id_solicitid
         LET v_tramites_todas[v_indice].v_folio_transaccion   = v_tramite_todas.v_folio_transaccion
         LET v_tramites_todas[v_indice].v_nss                 = v_tramite_todas.v_nss
         LET v_tramites_todas[v_indice].v_curp                = v_tramite_todas.v_curp
         LET v_tramites_todas[v_indice].v_cod_resp_tran       = v_tramite_todas.v_cod_resp_tran
         LET v_tramites_todas[v_indice].v_fecha_tran          = v_tramite_todas.v_fecha_tran USING "dd-mm-yyyy"," ",v_tramite_todas.v_h_operacion_tran
         LET v_tramites_todas[v_indice].v_id_respuesta        = v_tramite_todas.v_id_respuesta
         LET v_tramites_todas[v_indice].v_folio_ack           = v_tramite_todas.v_folio_ack
         LET v_tramites_todas[v_indice].v_fecha_ack           = v_tramite_todas.v_fecha_ack USING "dd-mm-yyyy"," ",v_tramite_todas.v_h_operacion_ack
         LET v_tramites_todas[v_indice].v_cod_resp_ack        = v_tramite_todas.v_cod_resp_ack
         LET v_tramites_todas[v_indice].v_cod_resp_ope_ack    = v_tramite_todas.v_cod_resp_ope_ack
         LET v_tramites_todas[v_indice].v_desc_respuesta      = v_tramite_todas.v_desc_respuesta
         LET v_tramites_todas[v_indice].v_cod_ope_cliente     = v_tramite_todas.v_cod_ope_cliente
         LET v_tramites_todas[v_indice].v_excepcion           = v_tramite_todas.v_excepcion
         LET v_indice = v_indice + 1
      END FOREACH
      FREE cur_rec_detalle_todas_tramites
   ELSE # excepciones
      LET v_consulta = "\n SELECT FIRST 1000 ",--tra.id_bus_tramite,",
                       "\n        tra.folio_procesar,",                       
                       "\n        CASE tra.origen",
                       "\n          WHEN 0 THEN 'PROCESAR'",
                       "\n          WHEN 1 THEN 'AFORE'",
                       "\n        END CASE,",
                       "\n        pro.desc_proceso_bus,",
                       "\n        ope.desc_opera_bus,",
                       "\n        con.id_cat_bus_contrato,",
                       "\n        con.desc_contrato,",
                       "\n        sol.id_bus_solicitud_tramite,",
                       "\n        sol.folio_transaccion,",
                       "\n        sol.nss,",
                       "\n        sol.curp,",
                       "\n        sol.cod_respuesta_opr,",
                       "\n        sol.f_operacion,",
                       "\n        sol.h_operacion,",
                       "\n        res.id_bus_respuesta_tramite,",
                       "\n        res.folio_ack,",
                       "\n        res.f_operacion,",
                       "\n        res.h_operacion,",
                       "\n        res.cod_respuesta,",
                       "\n        res.cod_respuesta_opr,",
                       "\n        res.desc_respuesta,",
                       "\n        exc.descripcion",
                       "\n   FROM  bus_tramite tra LEFT OUTER JOIN cat_bus_contrato con",
                       "\n     ON con.id_cat_bus_contrato = tra.id_cat_bus_contrato",
                       "\n        LEFT OUTER JOIN cat_bus_operacion ope",
                       "\n     ON ope.id_cat_bus_operacion = con.id_cat_bus_operacion",
                       "\n        LEFT OUTER JOIN cat_bus_proceso pro",
                       "\n     ON pro.id_cat_bus_proceso = ope.id_cat_bus_proceso",
                       "\n        JOIN bus_solicitud_tramite sol", # deben existir los registros tanto en bus_tramite como en bus_solicitud_tramite 
                       "\n     ON sol.id_bus_tramite = tra.id_bus_tramite",
                       "\n        RIGHT OUTER JOIN bus_excepcion exc", # sólo se deben recuperar los registros relacionados a la tabla de excepciones
                       "\n     ON sol.id_bus_solicitud_tramite = exc.id_bus_solicitud_tramite",
                       "\n        LEFT OUTER JOIN bus_respuesta_tramite res",
                       "\n     ON res.id_bus_tramite = tra.id_bus_tramite",
                       "\n  WHERE ",v_filtro
      --DISPLAY "Excepciones:",v_consulta
      PREPARE prp_rec_detalle_excepciones_tramites FROM v_consulta
      DECLARE cur_rec_detalle_excepciones_tramites CURSOR FOR prp_rec_detalle_excepciones_tramites
      FOREACH cur_rec_detalle_excepciones_tramites INTO v_tramite_excepciones.*         
         LET v_tramites_todas[v_indice].v_consecutivo         = v_indice--v_tramite_excepciones.v_id_bus_tramite
         LET v_tramites_todas[v_indice].v_folio_procesar      = v_tramite_excepciones.v_folio_procesar
         LET v_tramites_todas[v_indice].v_desc_proceso        = v_tramite_excepciones.v_desc_proceso
         LET v_tramites_todas[v_indice].v_desc_operacion      = v_tramite_excepciones.v_desc_operacion
         LET v_tramites_todas[v_indice].v_origen              = v_tramite_excepciones.v_origen
         LET v_tramites_todas[v_indice].v_id_cat_bus_contrato = v_tramite_excepciones.v_id_cat_bus_contrato
         LET v_tramites_todas[v_indice].v_desc_contrato       = v_tramite_excepciones.v_desc_contrato
         LET v_tramites_todas[v_indice].v_id_solicitid        = v_tramite_excepciones.v_id_solicitid
         LET v_tramites_todas[v_indice].v_folio_transaccion   = v_tramite_excepciones.v_folio_transaccion
         LET v_tramites_todas[v_indice].v_nss                 = v_tramite_excepciones.v_nss
         LET v_tramites_todas[v_indice].v_curp                = v_tramite_excepciones.v_curp
         LET v_tramites_todas[v_indice].v_cod_resp_tran       = v_tramite_excepciones.v_cod_resp_tran
         LET v_tramites_todas[v_indice].v_fecha_tran          = v_tramite_excepciones.v_fecha_tran USING "dd-mm-yyyy"," ",v_tramite_excepciones.v_h_operacion_tran
         LET v_tramites_todas[v_indice].v_id_respuesta        = v_tramite_excepciones.v_id_respuesta
         LET v_tramites_todas[v_indice].v_folio_ack           = v_tramite_excepciones.v_folio_ack
         LET v_tramites_todas[v_indice].v_fecha_ack           = v_tramite_excepciones.v_fecha_ack USING "dd-mm-yyyy"," ",v_tramite_excepciones.v_h_operacion_ack
         LET v_tramites_todas[v_indice].v_cod_resp_ack        = v_tramite_excepciones.v_cod_resp_ack
         LET v_tramites_todas[v_indice].v_cod_resp_ope_ack    = v_tramite_excepciones.v_cod_resp_ope_ack
         LET v_tramites_todas[v_indice].v_desc_respuesta      = v_tramite_excepciones.v_desc_respuesta
         LET v_tramites_todas[v_indice].v_excepcion           = v_tramite_excepciones.v_excepcion
         LET v_indice = v_indice + 1
      END FOREACH
      FREE cur_rec_detalle_excepciones_tramites
   END IF

   RETURN v_tramites_todas--,v_tramites_excepciones
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_genera_reporte_traza_bitacora(p_modulo_desc,
                                          p_proceso_desc,
                                          p_operacion_desc,
                                          p_contrato_desc,
                                          p_f_operacion_ini,
                                          p_f_operacion_fin,
                                          p_f_operacion_ack_ini,
                                          p_f_operacion_ack_fin,
                                          p_nss,
                                          p_curp,
                                          p_cod_transaccion_opr,
                                          p_cod_respuesta,
                                          p_cod_respuesta_opr,
                                          p_tramites)
DEFINE p_modulo_desc   LIKE seg_modulo.modulo_desc,
       p_proceso_desc  LIKE cat_bus_proceso.desc_proceso_bus,
       p_operacion_desc LIKE cat_bus_operacion.desc_opera_bus,
       p_contrato_desc LIKE cat_bus_contrato.desc_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       v_nom_reporte   STRING,
       v_manejador_rpt OM.SaxDocumentHandler,
       p_tramites DYNAMIC ARRAY OF RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_indice  SMALLINT

   IF( fgl_report_loadCurrentSettings(v_ruta_ejecutables CLIPPED ||"/PRTC022.4rp") )THEN

      
      
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, 
                          "-PRTC02-",
                          "TRAZA"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(1)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      
      START REPORT rpt_consultas_traza TO XML HANDLER v_manejador_rpt
         FOR v_indice = 1 TO p_tramites.getLength()

            OUTPUT TO REPORT rpt_consultas_traza(p_modulo_desc,
                                                 p_proceso_desc,
                                                 p_operacion_desc,
                                                 p_contrato_desc,
                                                 p_f_operacion_ini,
                                                 p_f_operacion_fin,
                                                 p_f_operacion_ack_ini,
                                                 p_f_operacion_ack_fin,
                                                 p_nss,
                                                 p_curp,
                                                 p_cod_transaccion_opr,
                                                 p_cod_respuesta,
                                                 p_cod_respuesta_opr,
                                                 v_indice,
                                                 p_tramites[v_indice].*)
         END FOR

      FINISH REPORT rpt_consultas_traza
   ELSE
      CALL fn_mensaje("AVISO","No fue posible generar el reporte","information")

   END IF

END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus y detalle de tramite              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
FUNCTION fn_genera_reporte_notificacion_xml(p_folio_procesar,
                                   p_tramite,
                                   p_detalle_solicitud,
                                   p_errores_solicitud,
                                   p_errores_respuesta)
DEFINE p_folio_procesar LIKE bus_tramite.folio_procesar,
       p_tramite RECORD
         v_consecutivo       INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,         
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         --v_cod_ope_cliente   LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       p_detalle_solicitud DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, --VARCHAR(40),
         v_valor    LIKE bus_detalle_solicitud.valor --VARCHAR(200)
       END RECORD,
       p_errores_solicitud DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       p_errores_respuesta DYNAMIC ARRAY OF RECORD
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       p_detalle_solicitud_tmp RECORD
         v_indice   SMALLINT,
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, --VARCHAR(40),
         v_valor    LIKE bus_detalle_solicitud.valor --VARCHAR(200)
       END RECORD,
       p_errores_solicitud_tmp RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       p_errores_respuesta_tmp RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       
       v_detalle_bloque DYNAMIC ARRAY OF RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo,
         v_valor    LIKE bus_detalle_solicitud.valor
       END RECORD,
       v_id_col_cat_bloque DECIMAL(9,0),
       v_nom_reporte   STRING,
       v_manejador_rpt OM.SaxDocumentHandler,
       v_mensaje     STRING,
       v_error       BOOLEAN,
       v_indice      SMALLINT,
       v_indice_bloq SMALLINT,
       v_indice_max  SMALLINT
       

   
   IF( fgl_report_loadCurrentSettings(v_ruta_ejecutables CLIPPED ||"/PRTC021.4rp") )THEN

      
      
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, 
                          "-PRTC02-",
                          "NOTIFICACION"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(v_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(1)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()


      LET v_indice_max = p_detalle_solicitud.getLength()
      IF(p_errores_solicitud.getLength() > v_indice_max)THEN
         LET v_indice_max = p_errores_solicitud.getLength()
      END IF
      IF(p_errores_respuesta.getLength() > v_indice_max)THEN
         LET v_indice_max = p_errores_respuesta.getLength()
      END IF   

      #####################
      CALL g_detalle_bloque_solicitud.clear()
      # Recupera los bloques del detalle de solicitud
      FOR v_indice = 1 TO p_detalle_solicitud.getLength()
         CALL v_detalle_bloque.clear()
         LET v_id_col_cat_bloque = 0
         EXECUTE prp_recupera_id_cat_bloque USING p_detalle_solicitud[v_indice].v_id_col_cat_entidad 
                                             INTO v_id_col_cat_bloque
         # si recupera el id del catalogo continua con la busqueda
         IF(v_id_col_cat_bloque IS NOT NULL OR v_id_col_cat_bloque > 0)THEN
            CALL fn_recupera_etiquetas_bloque("bus_detalle_bloque", # Tabla que contiene los valores
                                       v_id_col_cat_bloque, # contrato vigente del cual se obtiene la relacion de la entidad y columnas
                                       p_detalle_solicitud[v_indice].v_id_col_entidad, # identificador de la solicitud para recuperar los registros del detalle
                                       "nombre_campo_bloque",
                                       "valor",
                                       "cat_bus_detalle_bloque", # Entidad catálogo del bloque
                                       "cve_natural_bloque") 
                                       RETURNING v_error, 
                                                 v_mensaje,
                                                 v_detalle_bloque
            IF(v_detalle_bloque.getLength() > 0)THEN
               FOR v_indice_bloq = 1 TO v_detalle_bloque.getLength()
                  LET g_detalle_bloque_solicitud[g_detalle_bloque_solicitud.getLength() + 1].v_id_col_cat_entidad = v_detalle_bloque[v_indice_bloq].v_id_col_cat_entidad
                  LET g_detalle_bloque_solicitud[g_detalle_bloque_solicitud.getLength()].v_id_col_entidad   = v_detalle_bloque[v_indice_bloq].v_id_col_entidad
                  LET g_detalle_bloque_solicitud[g_detalle_bloque_solicitud.getLength()].v_etiqueta_det_sol = p_detalle_solicitud[v_indice].v_etiqueta
                  LET g_detalle_bloque_solicitud[g_detalle_bloque_solicitud.getLength()].v_etiqueta         = v_detalle_bloque[v_indice_bloq].v_etiqueta  
                  LET g_detalle_bloque_solicitud[g_detalle_bloque_solicitud.getLength()].v_valor            = v_detalle_bloque[v_indice_bloq].v_valor      
               END FOR
            END IF
         END IF
      END FOR

      #####################   
      IF(v_indice_max > 0)THEN
         START REPORT rpt_consultas_notificacion TO XML HANDLER v_manejador_rpt

            FOR v_indice = 1 TO v_indice_max
               IF(p_detalle_solicitud.getLength() >= v_indice)THEN
                  LET p_detalle_solicitud_tmp.v_indice   = v_indice
                  LET p_detalle_solicitud_tmp.v_etiqueta = p_detalle_solicitud[v_indice].v_etiqueta
                  LET p_detalle_solicitud_tmp.v_valor    = p_detalle_solicitud[v_indice].v_valor
               ELSE
                 INITIALIZE p_detalle_solicitud_tmp.* TO NULL
               END IF
               IF(p_errores_solicitud.getLength() >= v_indice)THEN
                  LET p_errores_solicitud_tmp.v_indice      = v_indice
                  LET p_errores_solicitud_tmp.v_codigo      = p_errores_solicitud[v_indice].v_codigo
                  LET p_errores_solicitud_tmp.v_descripcion = p_errores_solicitud[v_indice].v_descripcion
               ELSE
                  INITIALIZE p_errores_solicitud_tmp.* TO NULL            
               END IF
               IF(p_errores_respuesta.getLength() >= v_indice)THEN
                  LET p_errores_respuesta_tmp.v_indice = v_indice
                  LET p_errores_respuesta_tmp.v_codigo = p_errores_respuesta[v_indice].v_codigo
                  LET p_errores_respuesta_tmp.v_descripcion = p_errores_respuesta[v_indice].v_descripcion
               ELSE
                  INITIALIZE p_errores_respuesta_tmp.* TO NULL            
               END IF
            
               OUTPUT TO REPORT rpt_consultas_notificacion(p_folio_procesar,
                                                           p_tramite.*,
                                                           p_detalle_solicitud_tmp.*,
                                                           p_errores_solicitud_tmp.*,
                                                           p_errores_respuesta_tmp.*)
            END FOR

         FINISH REPORT rpt_consultas_notificacion
      ELSE
         CALL fn_mensaje("AVISO","No se encontraron registros para reporte","information")         
      END IF
   ELSE
      CALL fn_mensaje("AVISO","No fue posible generar el reporte","information")

   END IF
END FUNCTION

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus                                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_consultas_traza(p_modulo_desc,
                           p_proceso_desc,
                           p_operacion_desc,
                           p_contrato_desc,
                           p_f_operacion_ini,
                           p_f_operacion_fin,
                           p_f_operacion_ack_ini,
                           p_f_operacion_ack_fin,
                           p_nss,
                           p_curp,
                           p_cod_transaccion_opr,
                           p_cod_respuesta,
                           p_cod_respuesta_opr,
                           p_indice,
                           p_tramite)
DEFINE p_modulo_desc          LIKE seg_modulo.modulo_desc,
       p_proceso_desc         LIKE cat_bus_proceso.desc_proceso_bus,
       p_operacion_desc       LIKE cat_bus_operacion.desc_opera_bus,
       p_contrato_desc        LIKE cat_bus_contrato.desc_contrato,
       p_f_operacion_ini      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_fin      LIKE bus_solicitud_tramite.f_operacion,
       p_f_operacion_ack_ini  LIKE bus_respuesta_tramite.f_operacion,
       p_f_operacion_ack_fin  LIKE bus_respuesta_tramite.f_operacion,
       p_nss                  LIKE bus_solicitud_tramite.nss,
       p_curp                 LIKE bus_solicitud_tramite.curp,
       p_cod_transaccion_opr  LIKE bus_solicitud_tramite.cod_respuesta_opr,
       p_cod_respuesta        LIKE bus_respuesta_tramite.cod_respuesta,
       p_cod_respuesta_opr    LIKE bus_respuesta_tramite.cod_respuesta_opr,
       p_indice               SMALLINT,
       p_tramite RECORD
         v_consecutivo         INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_folio_procesar      LIKE bus_tramite.folio_procesar,
         v_origen              VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso        LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion      LIKE cat_bus_operacion.desc_opera_bus,
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid        LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion   LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss                 LIKE bus_solicitud_tramite.nss,
         v_curp                LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran       LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran          CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta        LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack           LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack           CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack        LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack    LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta      LIKE bus_respuesta_tramite.desc_respuesta,
         v_cod_ope_cliente     LIKE bus_respuesta_tramite.cod_oper_cliente,
         v_excepcion           LIKE bus_excepcion.descripcion
       END RECORD,
       v_origen  CHAR(1),       
       v_pagina  SMALLINT,
       v_fecha_actual CHAR(10)
       

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY USING "dd-mm-yyyy"
         
         PRINTX v_fecha_actual,
                p_modulo_desc,
                p_proceso_desc,
                p_operacion_desc,
                p_contrato_desc,
                p_f_operacion_ini,
                p_f_operacion_fin,
                p_f_operacion_ack_ini,
                p_f_operacion_ack_fin,
                p_nss,
                p_curp,
                p_cod_transaccion_opr,
                p_cod_respuesta,
                p_cod_respuesta_opr 

      BEFORE GROUP OF p_tramite.v_folio_procesar
         PRINTX p_tramite.v_folio_procesar

      ON EVERY ROW
         LET v_origen = p_tramite.v_origen[1] 
         PRINTX p_tramite.v_consecutivo,
                p_indice,
                p_tramite.v_folio_procesar,
                v_origen, # solo se imprime el primer caracter del origen
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, --USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, --USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_cod_ope_cliente,
                p_tramite.v_excepcion

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

         
END REPORT

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Reporte de tramite bus y detalle tramite                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_consultas_notificacion(p_folio_procesar,p_tramite,p_detalle_solicitud,p_errores_solicitud,p_errores_respuesta)
DEFINE p_folio_procesar LIKE bus_tramite.folio_procesar,
       p_tramite RECORD
         v_consecutivo       INTEGER,--LIKE bus_tramite.id_bus_tramite,
         v_origen            VARCHAR(8),--LIKE bus_tramite.origen,
         v_desc_proceso      LIKE cat_bus_proceso.desc_proceso_bus,
         v_desc_operacion    LIKE cat_bus_operacion.desc_opera_bus,         
         v_id_cat_bus_contrato LIKE cat_bus_contrato.id_cat_bus_contrato,
         v_desc_contrato       LIKE cat_bus_contrato.desc_contrato,
         v_id_solicitid      LIKE bus_solicitud_tramite.id_bus_solicitud_tramite,
         v_folio_transaccion LIKE bus_solicitud_tramite.folio_transaccion,
         v_nss               LIKE bus_solicitud_tramite.nss,
         v_curp              LIKE bus_solicitud_tramite.curp,
         v_cod_resp_tran     LIKE bus_solicitud_tramite.cod_respuesta_opr,
         v_fecha_tran        CHAR(19),--LIKE bus_solicitud_tramite.f_operacion,
         v_id_respuesta      LIKE bus_respuesta_tramite.id_bus_respuesta_tramite,
         v_folio_ack         LIKE bus_respuesta_tramite.folio_ack,
         v_fecha_ack         CHAR(19),--LIKE bus_respuesta_tramite.f_operacion,
         v_cod_resp_ack      LIKE bus_respuesta_tramite.cod_respuesta,
         v_cod_resp_ope_ack  LIKE bus_respuesta_tramite.cod_respuesta_opr,
         v_desc_respuesta    LIKE bus_respuesta_tramite.desc_respuesta,
         v_excepcion         LIKE bus_excepcion.descripcion
       END RECORD,
       p_detalle_solicitud RECORD
         v_indice   SMALLINT,
         v_etiqueta LIKE bus_detalle_solicitud.nombre_campo, --VARCHAR(40),
         v_valor    LIKE bus_detalle_solicitud.valor --VARCHAR(200)
       END RECORD,
       p_errores_solicitud RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_solicitud.cod_error,
         v_descripcion LIKE bus_error_solicitud.desc_error
       END RECORD,
       p_errores_respuesta RECORD
         v_indice   SMALLINT,
         v_codigo      LIKE bus_error_respuesta.cod_error,
         v_descripcion LIKE bus_error_respuesta.desc_error
       END RECORD,
       v_pagina  SMALLINT,
       v_indice  SMALLINT,
       v_fecha_actual CHAR(10)
       

   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY USING "dd-mm-yyyy"
         PRINTX v_fecha_actual
         
         PRINTX p_folio_procesar,
                p_tramite.v_consecutivo,
                p_tramite.v_origen,
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, -- USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, -- USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_excepcion

      PAGE HEADER 
         PRINTX p_folio_procesar,
                p_tramite.v_consecutivo,
                p_tramite.v_origen,
                p_tramite.v_desc_proceso,
                p_tramite.v_desc_operacion,
                p_tramite.v_id_cat_bus_contrato,
                p_tramite.v_desc_contrato,
                p_tramite.v_id_solicitid,
                p_tramite.v_folio_transaccion,
                p_tramite.v_nss,
                p_tramite.v_curp,
                p_tramite.v_cod_resp_tran,
                p_tramite.v_fecha_tran, -- USING "dd-mm-yyyy",
                p_tramite.v_id_respuesta,
                p_tramite.v_folio_ack,
                p_tramite.v_fecha_ack, -- USING "dd-mm-yyyy",
                p_tramite.v_cod_resp_ack,
                p_tramite.v_cod_resp_ope_ack,
                p_tramite.v_desc_respuesta,
                p_tramite.v_excepcion

      ON EVERY ROW
         
         PRINTX p_detalle_solicitud.v_indice,
                p_detalle_solicitud.v_etiqueta,
                p_detalle_solicitud.v_valor,
                p_errores_solicitud.v_indice,
                p_errores_solicitud.v_codigo,
                p_errores_solicitud.v_descripcion,
                p_errores_respuesta.v_indice,
                p_errores_respuesta.v_codigo,
                p_errores_respuesta.v_descripcion
        

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         
         IF(g_detalle_bloque_solicitud.getLength() = 0)THEN
            LET g_detalle_bloque_solicitud[1].v_etiqueta = ""
            LET g_detalle_bloque_solicitud[1].v_valor    = "" 
         END IF
            # genera sub reporte de detalle de bloque para todo el detalle de solicitud
            START REPORT rpt_det_bloque_solicitud
               FOR v_indice = 1 TO g_detalle_bloque_solicitud.getLength()
                  OUTPUT TO REPORT rpt_det_bloque_solicitud(v_indice,
                                                            g_detalle_bloque_solicitud[v_indice].*)
               END FOR
            FINISH REPORT rpt_det_bloque_solicitud
            
         --END IF
         


END REPORT

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTC02                                                   #
#Descripcion       => Sub reporte detalle de bloque del detalle de solicitud   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Febrero 2015                                          #
################################################################################
REPORT rpt_det_bloque_solicitud(p_indice,p_detalle_bloque_solicitud)
DEFINE p_indice   SMALLINT,
       p_detalle_bloque_solicitud RECORD
         v_id_col_entidad     DECIMAL(9,0),
         v_id_col_cat_entidad DECIMAL(9,0),
         v_etiqueta_det_sol   LIKE bus_detalle_solicitud.nombre_campo,
         v_etiqueta           LIKE bus_detalle_solicitud.nombre_campo,
         v_valor              LIKE bus_detalle_solicitud.valor
       END RECORD,
       v_titulo STRING
   
   FORMAT

      FIRST PAGE HEADER
         LET v_titulo = "DETALLE BLOQUE NOTIFICACIÓN XML"
         PRINTX v_titulo

      PAGE HEADER
         PRINTX v_titulo 

      BEFORE GROUP OF p_detalle_bloque_solicitud.v_etiqueta_det_sol
         PRINTX p_detalle_bloque_solicitud.v_etiqueta_det_sol

      ON EVERY ROW
         PRINTX p_indice,
                p_detalle_bloque_solicitud.v_id_col_entidad,
                p_detalle_bloque_solicitud.v_id_col_cat_entidad,
                p_detalle_bloque_solicitud.v_etiqueta,
                p_detalle_bloque_solicitud.v_valor

END REPORT
