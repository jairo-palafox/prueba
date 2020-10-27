--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/06/2012
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPF26                                                    #
#Objetivo        => Programa de Carga de Restitución                           #
#Fecha Inicio    => Junio 21, 2012                                             #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_expediente RECORD
         v_caso_adai  LIKE sep_expediente.caso_adai,
         v_tipo_flujo LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura  LIKE sep_expediente.f_captura,
         v_canal      LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_acreditado LIKE sep_nss_expediente.nss,
         v_trabajador LIKE sep_nss_expediente.nss
       END RECORD,
       v_documento DYNAMIC ARRAY OF RECORD
         v_id_expediente  LIKE sep_expediente.id_expediente,
         v_nom_documento  STRING,
         v_ruta_local_exp STRING,
         v_ruta_local     STRING,
         v_ruta_servidor  STRING,
         --v_archivo        LIKE sep_expediente.docto_dictamen
         v_archivo        VARCHAR(80)
       END RECORD,
       v_ruta_docto      LIKE seg_modulo.ruta_docto

MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   CALL fn_genera_restitucion()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF26                                                   #
#Descripcion       => Carga de archivo Restitución                                #
#Autor             =>                               #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_genera_restitucion()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       r_error     BOOLEAN,
       v_comando   STRING,
       v_id_expediente     INTEGER,
       sin_archivo         INTEGER ,
       v_aport_no_aplicada INTEGER,
       v_indicador         SMALLINT,
       v_cad_busqueda      base.StringTokenizer,
       v_cad_reemplazo     base.StringBuffer,
       r_mensaje           STRING

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF261"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      # oculta grupos de datos exoediente y documento
      CALL v_forma.setElementHidden("gpo_expediente",1)
      CALL v_forma.setElementHidden("gpo_dicatamen",1)
      CALL v_forma.setElementHidden("btn_quitar",1)
      CALL fn_busca_expediente() RETURNING v_continuar, v_aport_no_aplicada
      IF(v_continuar)THEN
         # si recupewró infromacion, muestra detalle
         CALL v_forma.setElementHidden("gpo_expediente",0)
         CALL v_forma.setElementHidden("gpo_dicatamen",0)

         DIALOG ATTRIBUTES (UNBUFFERED)
          
            INPUT BY  NAME sin_archivo
          
               BEFORE  INPUT 
                  DISPLAY v_expediente.v_caso_adai  TO flbl_caso_expediente
                  DISPLAY v_expediente.v_tipo_flujo TO flbl_tipo_flujo_expediente
                  DISPLAY v_expediente.v_f_captura  TO flbl_fecha_captura_expediente
                  DISPLAY v_expediente.v_canal      TO flbl_origen_expediente
                  DISPLAY v_expediente.v_acreditado TO flbl_acreditado_expediente
                  DISPLAY v_expediente.v_trabajador TO flbl_trabajador_expediente
                  IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setFieldHidden("sin_archivo",0)
                     LET sin_archivo = 0
                  ELSE
                     CALL v_forma.setFieldHidden("sin_archivo",1)
                     LET sin_archivo = 0
                  END IF
                  IF(v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setElementHidden("btn_quitar",1)
                  ELSE
                     CALL v_forma.setElementHidden("btn_quitar",0)
                  END IF
            END INPUT
            
            INPUT ARRAY v_documento FROM sr_documento.*  ATTRIBUTES(WITHOUT DEFAULTS,  APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

               BEFORE INPUT 
                  CALL DIALOG.setActionHidden("dialogtouched",TRUE)

               ON ACTION dialogtouched
               --ON CHANGE tedi_seleccion
                  # reemplaza \ por |
                  LET v_cad_reemplazo = base.StringBuffer.create()
                  CALL v_cad_reemplazo.append(v_documento[1].v_ruta_local_exp CLIPPED) 
                  CALL v_cad_reemplazo.replace("\\","|",0)
               
                  LET v_documento[1].v_archivo = v_cad_reemplazo.toString()

                  # Recupera el ultimo elemento de la cadena 
                  LET v_cad_busqueda = base.StringTokenizer.create(v_documento[1].v_archivo,"|")
                  WHILE v_cad_busqueda.hasMoreTokens()
                     --LET v_nom_archivo = v_cad_busqueda.nextToken()
                     LET v_documento[1].v_archivo = v_cad_busqueda.nextToken()
                  END WHILE
                  LET v_documento[1].v_ruta_local = v_documento[1].v_archivo
               
                  IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setFieldHidden("sin_archivo",0)
                     LET sin_archivo = 0
                  ELSE
                     CALL v_forma.setFieldHidden("sin_archivo",1)
                     LET sin_archivo = 0
                  END IF
                  IF(v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setElementHidden("btn_quitar",1)
                  ELSE
                     CALL v_forma.setElementHidden("btn_quitar",0)
                  END IF
                  NEXT FIELD tedi_ruta
                  

               BEFORE FIELD tedi_seleccion
                  LET v_documento[1].v_archivo        = NULL
                  LET v_documento[1].v_ruta_local     = NULL
                  LET v_documento[1].v_ruta_servidor  = NULL
                  LET v_documento[1].v_ruta_local_exp = NULL  
                  LET sin_archivo = 0

               AFTER FIELD tedi_seleccion
                  IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setFieldHidden("sin_archivo",0)
                     LET sin_archivo = 0
                  ELSE
                     CALL v_forma.setFieldHidden("sin_archivo",1)
                     LET sin_archivo = 0
                  END IF
                  IF(v_documento[1].v_nom_documento IS NULL)THEN
                     CALL v_forma.setElementHidden("btn_quitar",1)
                  ELSE
                     CALL v_forma.setElementHidden("btn_quitar",0)
                  END IF
                   
            END INPUT 
                
             
            ON ACTION aceptar
               IF( sin_archivo = 0 )THEN
                  IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                     CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Restitución","information")
                     CONTINUE DIALOG
                  END IF
                  LET v_continuar = FALSE
                  IF(v_documento[1].v_archivo IS NULL)THEN
                     CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Restitución","information")
                  ELSE
                     CALL fn_actualiza_docto(v_documento[1].*,v_aport_no_aplicada) RETURNING v_continuar
                     IF(v_continuar)THEN
                        IF(v_aport_no_aplicada = 1)THEN
                           UPDATE sep_expediente
                              SET ind_restitucion_no_aplicados = 1 # archivo sin confirmar
                            WHERE id_expediente = v_documento[1].v_id_expediente
                        ELSE
                           UPDATE sep_expediente
                              SET ind_restitucion = 1 # archivo sin confirmar
                            WHERE id_expediente = v_documento[1].v_id_expediente
                        END IF
                        CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                        EXIT DIALOG
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                        CONTINUE DIALOG  
                     END IF
                  END IF
               ELSE
                  CALL fn_mensaje(p_cad_ventana,"Casilla 'sin Archivo' activa.","information")
                  CONTINUE DIALOG
               END IF 
             
            ON ACTION confirmar
               IF(sin_archivo = 0)THEN           
                  IF(v_documento[1].v_nom_documento IS NULL AND v_documento[1].v_ruta_local_exp IS NULL)THEN
                     CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Restitución","information")
                     CONTINUE DIALOG
                  END IF
                  CALL fn_ventana_confirma(p_cad_ventana,"Al confirmar no se podrá sustituir archivo","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     # si seleccionó un archivo local, se transfiere
                     IF(v_documento[1].v_archivo IS NOT NULL)THEN
                        CALL fn_actualiza_docto(v_documento[1].*,v_aport_no_aplicada) RETURNING v_continuar
                        IF(v_continuar)THEN
                           # Funcionque realiza la carga del archivo
                           CALL fn_carga_restitucion(v_documento[1].v_id_expediente,p_usuario_cod,v_aport_no_aplicada,v_documento[1].v_archivo)
                                 RETURNING v_indicador,r_mensaje
                           IF(v_indicador = 0)THEN
                              IF(v_aport_no_aplicada = 1)THEN
                                 UPDATE sep_expediente
                                    SET ind_restitucion_no_aplicados = 2 # archivo confirmado
                                  WHERE id_expediente = v_documento[1].v_id_expediente
                              ELSE
                                 UPDATE sep_expediente
                                    SET ind_restitucion = 2 # archivo confirmado
                                  WHERE id_expediente = v_documento[1].v_id_expediente
                              END IF
                              CALL fn_mensaje(p_cad_ventana,"Datos Cargados...","information")
                              EXIT DIALOG
                           ELSE
                              # Al ocurrir error, la carga envia el mensaje de error
                              CALL fn_mensaje(p_cad_ventana,r_mensaje,"information")                              
                           END IF
                        ELSE
                           CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","stop")
                           CONTINUE DIALOG  
                        END IF
                     ELSE
                        # Función que realiza la carga del archivo
                        CALL fn_carga_restitucion(v_documento[1].v_id_expediente,p_usuario_cod,v_aport_no_aplicada,v_documento[1].v_nom_documento)
                                 RETURNING v_indicador,r_mensaje
                        IF(v_indicador = 0)THEN
                           IF(v_aport_no_aplicada = 1)THEN
                              UPDATE sep_expediente
                                 SET ind_restitucion_no_aplicados = 2 # archivo confirmado
                               WHERE id_expediente = v_documento[1].v_id_expediente
                           ELSE
                              UPDATE sep_expediente
                                 SET ind_restitucion = 2 # archivo sin confirmar
                               WHERE id_expediente = v_documento[1].v_id_expediente
                           END IF
                           CALL fn_mensaje(p_cad_ventana,"Datos Cargados...","information")
                           ACCEPT DIALOG
                        ELSE
                           # Al ocurrir error, la carga envia el mensaje de error
                           CALL fn_mensaje(p_cad_ventana,r_mensaje,"stop")
                        END IF
                     END IF
                  ELSE
                     CONTINUE DIALOG
                  END IF
               ELSE
                  CALL fn_ventana_confirma(p_cad_ventana,"Al confirmar 'sin archivo' no podrá actualiza el registro posteriormente.\n¿Desea continuar?","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     # inicializa a null detallde documento
                     INITIALIZE v_documento[1].v_nom_documento,
                                v_documento[1].v_ruta_local_exp,
                                v_documento[1].v_ruta_local TO NULL
                     # sólo realiza el avance de la maquinaria y no carga ningún registro por confirmar sin archivo
                     CALL fn_carga_restitucion(v_documento[1].v_id_expediente,p_usuario_cod,v_aport_no_aplicada,v_documento[1].v_archivo)
                                  RETURNING v_indicador,
                                            r_mensaje
                     IF(v_indicador = 0)THEN
                        IF(v_aport_no_aplicada = 1)THEN
                           UPDATE sep_expediente
                              SET ind_restitucion_no_aplicados = 4 # Confirmada sin archivo
                            WHERE id_expediente = v_documento[1].v_id_expediente
                        ELSE
                           UPDATE sep_expediente
                              SET ind_restitucion = 4 # Confirmada sin archivo
                            WHERE id_expediente = v_documento[1].v_id_expediente
                        END IF
                          
                        CALL fn_mensaje(p_cad_ventana,"Datos Actualizados...","information")
                        ACCEPT DIALOG
                     ELSE
                        # Al ocurrir error, la carga envia el mensaje de error
                        CALL fn_mensaje(p_cad_ventana,r_mensaje,"stop")
                     END IF
                  END IF
               END IF 
               CONTINUE DIALOG
            
            ON ACTION cancelar
               IF(LENGTH(v_documento[1].v_archivo) > 0)THEN
                  CALL fn_ventana_confirma(p_cad_ventana,"¿Desea salir sin registrar archivo?","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     EXIT DIALOG
                  ELSE               
                     CONTINUE DIALOG 
                  END IF
               ELSE
                  EXIT DIALOG
               END IF
           
            ON ACTION btn_quitar
               CALL fn_ventana_confirma(p_cad_ventana,"¿Eliminar archivo?","question") 
                   RETURNING r_confirma
               IF(r_confirma)THEN
                  # Elimina fisicamente el archivo
                  LET v_id_expediente = v_documento[1].v_id_expediente 
                  LET v_comando = v_id_expediente 
                  LET v_comando = "rm ",v_ruta_docto CLIPPED,
                                  v_documento[1].v_nom_documento CLIPPED
                               
                  DISPLAY v_comando
                  RUN v_comando
                  IF(STATUS)THEN
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al eliminar archivo","information")
                  ELSE
                     IF(v_aport_no_aplicada = 1)THEN
                        UPDATE sep_expediente
                           SET docto_restitucion_no_aplicados = NULL
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     ELSE
                        UPDATE sep_expediente
                           SET docto_restitucion = NULL
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     END IF
                     IF(SQLCA.SQLCODE = 0)THEN
                        # inicializa a null detallde documento
                        INITIALIZE v_documento[1].v_nom_documento,
                                   v_documento[1].v_ruta_local_exp,
                                   v_documento[1].v_ruta_local TO NULL
                        # oculta boton de quitar
                        CALL v_forma.setElementHidden("btn_quitar",1)
                        CALL fn_mensaje(p_cad_ventana,"Archivo eliminado con éxito","information")
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al eliminar archivo","information")
                     END IF
                  END IF
               END IF
               IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                  CALL v_forma.setFieldHidden("sin_archivo",0)
                  LET sin_archivo = 0
               ELSE
                  CALL v_forma.setFieldHidden("sin_archivo",1)
                  LET sin_archivo = 0
               END IF
               IF(v_documento[1].v_nom_documento IS NULL)THEN
                  CALL v_forma.setElementHidden("btn_quitar",1)
               ELSE
                  CALL v_forma.setElementHidden("btn_quitar",0)
               END IF

            BEFORE DIALOG
               IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                  CALL v_forma.setFieldHidden("sin_archivo",0)
                  LET sin_archivo = 0
               ELSE
                  CALL v_forma.setFieldHidden("sin_archivo",1)
                  LET sin_archivo = 0
               END IF
               IF(v_documento[1].v_nom_documento IS NULL)THEN
                  CALL v_forma.setElementHidden("btn_quitar",1)
               ELSE
                  CALL v_forma.setElementHidden("btn_quitar",0)
               END IF
        
         END DIALOG
      END IF
   CLOSE WINDOW vtna_carag_dictamen

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Restitución                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente,
       aport_no_aplicada INTEGER ,
       v_aportacion_no_aplicada  SMALLINT 
   
   LET v_continuar = FALSE
   LET v_bnd_con = TRUE
   LET v_filtro = " 1 = 2 "
   WHILE (v_bnd_con)
      DIALOG  ATTRIBUTES(UNBUFFERED)
         CONSTRUCT v_filtro ON exp.caso_adai, exp.id_expediente FROM edi_caso_adai,edi_id_expediente 
         
         END CONSTRUCT
    
         INPUT BY NAME aport_no_aplicada
            BEFORE INPUT 
               IF aport_no_aplicada IS NULL THEN 
                  LET aport_no_aplicada=0
               END IF 
               
            ON CHANGE aport_no_aplicada
               IF(aport_no_aplicada = 0)THEN 
                  LET v_aportacion_no_aplicada = 0
               ELSE 
                  LET v_aportacion_no_aplicada = 1
               END IF 

         END INPUT
      
         ON ACTION aceptar
             INITIALIZE v_caso_adai, v_id_expediente TO NULL 
             CALL GET_FLDBUF(edi_caso_adai) RETURNING v_caso_adai
             CALL GET_FLDBUF(edi_id_expediente) RETURNING v_id_expediente
             IF(v_caso_adai IS NULL AND v_id_expediente IS NULL)THEN
                CALL fn_mensaje(p_cad_ventana,"Al menos debe capturar un campo","information")
                CONTINUE DIALOG
             END IF
             LET v_bnd_con = TRUE
             EXIT DIALOG
          
         ON ACTION cancelar
            LET v_bnd_con = FALSE
            EXIT DIALOG
      END DIALOG 

      IF(v_bnd_con)THEN
         CALL fn_recupera_expediente(v_filtro,v_aportacion_no_aplicada) RETURNING v_continuar
         # si se recuperó información termina ciclo
         IF(v_continuar)THEN
            LET v_bnd_con = FALSE
         ELSE
            CALL fn_mensaje(p_cad_ventana,"Expediente no encontrado","information")
            LET v_bnd_con = TRUE
         END IF
      END IF
   END WHILE

   RETURN v_continuar,v_aportacion_no_aplicada
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Restitución                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_recupera_expediente(v_filtro,v_aport_no_aplicada)
DEFINE v_filtro   STRING,
       v_consulta STRING,
       v_expediente_aux RECORD
         v_caso_adai     LIKE sep_expediente.caso_adai,
         v_tipo_flujo    LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura     DATE,--LIKE sep_expediente.f_captura,
         v_canal         LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_id_expediente LIKE sep_expediente.id_expediente
       END RECORD,
       v_docto_dictamen  LIKE sep_expediente.docto_dictamen,       
       v_continua BOOLEAN,
       v_aport_no_aplicada SMALLINT

   INITIALIZE v_expediente_aux.* TO NULL
   LET v_continua = FALSE 
   # Recupera el expediente
   IF(v_aport_no_aplicada)THEN
      LET v_consulta = "\n SELECT exp.caso_adai, flo.flujo_desc, ",
                       "\n        exp.f_captura, can.canal_desc, exp.id_expediente",
                       "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_cat_tipo_flujo flo",
                       "\n     ON exp.flujo_cod = flo.flujo_cod",
                       "\n        LEFT OUTER JOIN sep_cat_canal_recepcion_exp can",
                       "\n     ON exp.canal_cod = can.canal_cod",
                       "\n  WHERE (exp.estado = 45", # 45 "Restitucion Solicitada" 
                       "\n    OR  exp.estado = 50)", # 50 "Restitucion Liquidada"
                       "\n    AND exp.ind_restitucion_no_aplicados IN (0,1)",
                       "\n    AND ",v_filtro
   ELSE
      LET v_consulta = "\n SELECT exp.caso_adai, flo.flujo_desc, ",
                       "\n        exp.f_captura, can.canal_desc, exp.id_expediente",
                       "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_cat_tipo_flujo flo",
                       "\n     ON exp.flujo_cod = flo.flujo_cod",
                       "\n        LEFT OUTER JOIN sep_cat_canal_recepcion_exp can",
                       "\n     ON exp.canal_cod = can.canal_cod",
                       "\n  WHERE exp.estado = 40", # 40 "Dictamen Registrado"
                       "\n    AND exp.ind_restitucion IN (0,1)",
                       "\n    AND ",v_filtro

   END IF
   --DISPLAY v_consulta
   
   PREPARE prp_recupera_expediente FROM v_consulta
   EXECUTE prp_recupera_expediente INTO v_expediente_aux.*
   IF(v_expediente_aux.v_id_expediente IS NOT NULL)THEN
      LET v_continua = TRUE
      LET v_expediente.v_caso_adai  = v_expediente_aux.v_caso_adai
      LET v_expediente.v_canal      = v_expediente_aux.v_canal
      LET v_expediente.v_tipo_flujo = v_expediente_aux.v_tipo_flujo
      LET v_expediente.v_f_captura  = v_expediente_aux.v_f_captura USING "mm-dd-yyyy"
      LET v_expediente.v_canal      = v_expediente_aux.v_canal

      # Recupera nss invadido del expediente 
      SELECT nss
        INTO v_expediente.v_acreditado
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 1 # Invadido

      # Recupera nss asociado del expediente 
      SELECT nss
        INTO v_expediente.v_trabajador
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 2 # Asociado

      IF(v_aport_no_aplicada = 1)THEN
         # si ya existe el documento, lo recupera
         SELECT docto_restitucion_no_aplicados
           INTO v_docto_dictamen
           FROM sep_expediente
          WHERE id_expediente = v_expediente_aux.v_id_expediente
      ELSE
         # si ya existe el documento, lo recupera
         SELECT docto_restitucion
           INTO v_docto_dictamen
           FROM sep_expediente
          WHERE id_expediente = v_expediente_aux.v_id_expediente

      END IF
      # Se establecen los datos del excel de dictamen, si es que ya existe
      LET v_documento[1].v_id_expediente  = v_expediente_aux.v_id_expediente
      LET v_documento[1].v_nom_documento  = v_docto_dictamen
      LET v_documento[1].v_ruta_servidor  = v_ruta_docto CLIPPED,"/",v_docto_dictamen CLIPPED            
   END IF

   RETURN v_continua
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Actualiza el documeto                                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_actualiza_docto(v_documento_imp,v_aport_no_aplicada)
DEFINE v_continuar BOOLEAN,
       v_documento_imp RECORD
         v_id_expediente  INTEGER,--LIKE sep_expediente.id_expediente,
         v_nom_documento  STRING,
         v_ruta_local_exp STRING,
         v_ruta_local     STRING,
         v_ruta_servidor  STRING,
         v_archivo        LIKE sep_expediente.docto_dictamen
       END RECORD,
       v_ruta_destino     STRING,
       v_archivo_destino  STRING,
       v_comando          STRING,
       v_cad_reemplazo    base.StringBuffer,
       v_nom_archivo      STRING,
       v_aport_no_aplicada SMALLINT

   WHENEVER ERROR CONTINUE
   LET v_continuar = FALSE
   # Elimina el documento anterior(documento que ya existe)
   LET v_comando = "rm ",v_ruta_docto CLIPPED,
                         v_documento_imp.v_nom_documento CLIPPED
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      DISPLAY "Ocurrió un error al eliminar archivo"
      RETURN v_continuar
   END IF
   # Reemplaza los espacios del archiv por guion bajo
   LET v_cad_reemplazo = base.StringBuffer.create()
   CALL v_cad_reemplazo.append(v_documento_imp.v_archivo CLIPPED) 
   CALL v_cad_reemplazo.replace(" ","_",0)
   LET v_nom_archivo = v_cad_reemplazo.toString()

   LET v_archivo_destino = v_documento_imp.v_id_expediente
   LET v_archivo_destino = v_archivo_destino.trim() ,
                           "-",
                            v_nom_archivo
   LET v_ruta_destino = v_ruta_docto CLIPPED,
                        v_archivo_destino

   # se transfiere el archivo a servidor
   DISPLAY "Ruta seleccionada: ",v_documento_imp.v_ruta_local_exp
   DISPLAY "Ruta destino: ",v_ruta_destino
   CALL FGL_GETFILE(v_documento_imp.v_ruta_local_exp, v_ruta_destino)
   IF(STATUS)THEN
      CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information")
      DISPLAY "Error de transferencia de archivo Restitución"
   ELSE
      LET v_documento[1].v_archivo = v_archivo_destino
      IF(v_aport_no_aplicada = 1)THEN      
         # se actualiza el nombre del documento
         UPDATE sep_expediente
            SET docto_restitucion_no_aplicados = v_documento[1].v_archivo
          WHERE id_expediente = v_documento[1].v_id_expediente
      ELSE
         # se actualiza el nombre del documento
         UPDATE sep_expediente
            SET docto_restitucion = v_documento[1].v_archivo
          WHERE id_expediente = v_documento[1].v_id_expediente
      END IF

      IF(SQLCA.SQLCODE = 0)THEN
         LET v_continuar = TRUE
      ELSE
        DISPLAY "Ocurrió un error de al actualizar archivo"
        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error de al actualizar archivo","information")
      END IF
   END IF
   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Actualiza el maquinaria                                  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
{FUNCTION fn_actualiza_maquinaria(p_id_expediente)
DEFINE v_consulta       STRING,
       v_error          BOOLEAN,
       v_ind            SMALLINT,       
       v_diag           CHAR(3),
       v_estado_destino SMALLINT,
       v_maquinaria     CHAR(40),
       v_senial         SMALLINT,
       --v_estado_origen  SMALLINT,
       p_id_expediente  LIKE sep_expediente.id_expediente,
       v_campo_id_ent   CHAR(40)

   WHENEVER ERROR CONTINUE
   LET v_error = FALSE
   LET v_maquinaria = "maq_sep_expediente"
   LET v_senial = 50  # señal definida en documento maquinarias_sep
   --LET v_estado_origen = 35 # estado definido en documento maquinarias_sep
   LET v_campo_id_ent = "id_expediente"
       
   LET v_consulta = "EXECUTE FUNCTION fn_maquinaria_individual(?,?,?,?,?)"
   PREPARE prp_recupera_edo_maq FROM v_consulta
   EXECUTE prp_recupera_edo_maq USING v_maquinaria,
                                      p_id_expediente,
                                      v_campo_id_ent,
                                      v_senial,
                                      p_usuario_cod
                                  INTO v_ind,v_diag,v_estado_destino 
   -- maq_sep_expediente = registro de cargas de dictamen
   IF(v_ind <> 0)THEN
      LET v_error = TRUE
   END IF

   RETURN v_error
END FUNCTION}


FUNCTION fn_oculta_campos (campo,valor)
  DEFINE valor SMALLINT
  DEFINE campo STRING
  DEFINE win ui.Window, fm ui.Form
  LET win = ui.Window.getCurrent()
  LET fm = win.getForm()
  --CALL fm.setELEMENTHidden(campo, valor)
  CALL fm.setElementHidden("campo", valor)
END FUNCTION              

