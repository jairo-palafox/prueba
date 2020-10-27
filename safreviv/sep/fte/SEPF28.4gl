--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/01/2015
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPF28                                                     #
#Objetivo        => Programa de Carga de Restitución Complementarias           #
#Fecha Inicio    => 20 Enero 2015                                              #
################################################################################
DATABASE safre_viv
DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod,
       p_tpo_ejecucion   SMALLINT,
       p_cad_ventana     STRING,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_docto      LIKE seg_modulo.ruta_docto,
       v_ventana         ui.Window,
       v_forma           ui.Form,
       v_documento DYNAMIC ARRAY OF RECORD
         v_id_expediente  LIKE sep_expediente.id_expediente,
         v_nom_documento  STRING,
         v_ruta_local_exp STRING,
         v_ruta_local     STRING,
         v_ruta_servidor  STRING,
         v_archivo        VARCHAR(80)
       END RECORD

DEFINE g_expediente RECORD
         v_caso_adai  LIKE sep_expediente.caso_adai,
         v_tipo_flujo LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura  LIKE sep_expediente.f_captura,
         v_canal      LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_acreditado LIKE sep_nss_expediente.nss,
         v_trabajador LIKE sep_nss_expediente.nss,
         v_ind_complementario_1 LIKE sep_expediente.ind_restitucion_complementario_1 
       END RECORD
       
MAIN

   LET p_usuario_cod   = ARG_VAL(1)
   LET p_tpo_ejecucion = ARG_VAL(2)
   LET p_cad_ventana   = ARG_VAL(3)

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   CALL fn_inicializa_consultas()
   CALL fn_captura_restitucion_complementaria()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF28                                                   #
#Descripcion       => Inicializa las consultas SQL del programa                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING


   LET v_consulta = "\n DELETE ",
                    "\n   FROM sep_115_restitucion ",
                    "\n  WHERE id_expediente = ? ",
                    "\n    AND ind_restitucion = 0" # Registro temporal de restitución complementaria antes de confirmar
   PREPARE prp_elimina_155 FROM v_consulta

   LET v_consulta = "\n DELETE",
                    "\n   FROM sep_mto_restitucion_analisis",
                    "\n  WHERE id_expediente = ? ",
                    "\n    AND ind_restitucion = 0" # Registro temporal de restitución complementaria antes de confirmar
   PREPARE prp_elimina_mto_analisis FROM v_consulta

   # El último registro en sep_restitucion corresponde a la restitucion complementaria 1 ó 2 al ejecutar ésta consulta
   LET v_consulta = "\n DELETE",
                    "\n   FROM sep_restitucion",
                    "\n  WHERE id_expediente = ? ",
                    "\n    AND id_restitucion = (SELECT MAX(id_restitucion) FROM sep_restitucion WHERE id_expediente = ?)" # Registro temporal de restitución complementaria antes de confirmar
   PREPARE prp_elimina_restitucion FROM v_consulta

   LET v_consulta = "\n DELETE",
                    "\n   FROM sep_inf_restitucion",
                    "\n  WHERE id_expediente = ? ",
                    "\n    AND id_restitucion = (SELECT MAX(id_restitucion) FROM sep_restitucion WHERE id_expediente = ?)" # Registro temporal de restitución complementaria antes de confirmar
   PREPARE prp_elimina_inf_restitucion FROM v_consulta

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF28                                                   #
#Descripcion       => Carga de archivo Restitución complementaria              #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_captura_restitucion_complementaria()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       v_comando   STRING,
       v_id_expediente     INTEGER,
       sin_archivo         INTEGER ,
       --v_aport_no_aplicada INTEGER,
       r_mensaje           STRING,
       v_indicador         SMALLINT,
       v_cad_busqueda      base.StringTokenizer,
       v_cad_reemplazo     base.StringBuffer

   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF281"
      # Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      # Oculta grupos de datos expediente y documento
      CALL v_forma.setElementHidden("gpo_expediente",1)
      CALL v_forma.setElementHidden("gpo_dicatamen",1)
      CALL v_forma.setElementHidden("btn_quitar",1)

      # Consulta expediente
      CALL fn_busca_expediente() RETURNING v_continuar
      
      IF(v_continuar)THEN
         # si recuperó infromación, muestra detalle
         CALL v_forma.setElementHidden("gpo_expediente",0)
         CALL v_forma.setElementHidden("gpo_dicatamen",0)

         DIALOG ATTRIBUTES (UNBUFFERED)
          
            INPUT BY NAME sin_archivo
          
               BEFORE INPUT
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
            
            INPUT ARRAY v_documento FROM sr_documento.*  ATTRIBUTES(WITHOUT DEFAULTS,  APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE)

               BEFORE INPUT 
                  CALL DIALOG.setActionHidden("dialogtouched",TRUE)        

               ON ACTION dialogtouched
               --ON CHANGE tedi_seleccion
                  # reemplaza \ por |
                  LET v_cad_reemplazo = base.StringBuffer.create()
                  CALL v_cad_reemplazo.append(v_documento[1].v_ruta_local_exp CLIPPED) 
                  CALL v_cad_reemplazo.replace("\\","|",0)
               
                  LET v_documento[1].v_archivo = v_cad_reemplazo.toString()
                  CALL v_cad_reemplazo.clear()

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
                     CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Restitución Complementaria","information")
                     CONTINUE DIALOG
                  END IF
                  LET v_continuar = FALSE
                  IF(v_documento[1].v_archivo IS NULL)THEN
                     CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Restitución Complementaria","information")
                  ELSE
                     CALL fn_actualiza_docto(v_documento[1].*) RETURNING v_continuar
                     IF(v_continuar)THEN
                        # Elimina los registros anteriores de restitucion
                        IF(LENGTH(v_documento[1].v_nom_documento CLIPPED) > 0)THEN
                           CALL fn_elimina_registros_temporales(v_documento[1].v_id_expediente)
                        END IF
                        
                        # Funcion que realiza la carga del archivo
                        CALL fn_carga_restitucion(v_documento[1].v_id_expediente,
                                                  p_usuario_cod,
                                                  2, # Parámetro para indicar tipo de restitución. 0 Rest Aplic, 1 Rest No Aplic, 2 Rest Compl
                                                  v_documento[1].v_archivo)  RETURNING v_indicador,r_mensaje
                        IF(v_indicador = 0)THEN                           
                           IF(g_expediente.v_ind_complementario_1 = 0 OR    # Sin Archivo Guardado
                              g_expediente.v_ind_complementario_1 = 1 )THEN # Archivo Sin Confirmar
                              UPDATE sep_expediente
                                 SET ind_restitucion_complementario_1 = 1 # archivo sin confirmar
                               WHERE id_expediente = v_documento[1].v_id_expediente
                           ELSE
                              UPDATE sep_expediente
                                 SET ind_restitucion_complementario_2 = 1 # archivo sin confirmar
                               WHERE id_expediente = v_documento[1].v_id_expediente
                           END IF
                           CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                           ACCEPT DIALOG
                        ELSE
                           # Muetra mensaje de error proveniente de la carga
                           CALL fn_mensaje(p_cad_ventana,r_mensaje,"stop")
                        END IF
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                     END IF
                  END IF
                  CONTINUE DIALOG
               ELSE
                  CALL fn_ventana_confirma(p_cad_ventana,"Al confirmar 'sin archivo' no podrá actualiza el registro posteriormente.\n¿Desea continuar?","question") RETURNING r_confirma
                  IF(r_confirma)THEN
                     IF(g_expediente.v_ind_complementario_1 = 0 OR    # Sin Archivo Guardado
                        g_expediente.v_ind_complementario_1 = 1 )THEN # Archivo Sin Confirmar
                        UPDATE sep_expediente
                           SET ind_restitucion_complementario_1 = 4 # archivo sin confirmar
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     ELSE
                        UPDATE sep_expediente
                           SET ind_restitucion_complementario_2 = 4 # archivo sin confirmar
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     END IF
                          
                     CALL fn_mensaje(p_cad_ventana,"Datos Actualizados...","information")
                     ACCEPT DIALOG
                  END IF
                  CONTINUE DIALOG
               END IF 
            
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
               CALL fn_ventana_confirma(p_cad_ventana,"¿Eliminar archivo guardado?","question") 
                   RETURNING r_confirma
               IF(r_confirma)THEN
                  # Elimina fisicamente el archivo
                  LET v_id_expediente = v_documento[1].v_id_expediente 
                  LET v_comando = v_id_expediente 
                  LET v_comando = "rm ",v_ruta_docto CLIPPED,
                                  v_documento[1].v_nom_documento CLIPPED

                  RUN v_comando
                  IF(STATUS)THEN
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al eliminar archivo","information")
                  ELSE
                     IF(g_expediente.v_ind_complementario_1 = 0 OR    # Sin Archivo Guardado
                        g_expediente.v_ind_complementario_1 = 1 )THEN # Archivo Sin Confirmar
                        UPDATE sep_expediente
                           SET docto_restitucion_complementario_1 = NULL,
                               ind_restitucion_complementario_1 = 0
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     ELSE
                        UPDATE sep_expediente
                           SET docto_restitucion_complementario_2 = NULL,
                               ind_restitucion_complementario_2 = 0
                         WHERE id_expediente = v_documento[1].v_id_expediente
                     END IF
                     # Si ya habia guardado un documento, se eliminas los registros de restitucion complementaria
                     IF(LENGTH(v_documento[1].v_nom_documento CLIPPED) > 0)THEN
                        # Elimina los registros de restitucion complementarios registrados para el expediente
                        CALL fn_elimina_registros_temporales(v_documento[1].v_id_expediente)
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
               DISPLAY g_expediente.v_caso_adai  TO flbl_caso_expediente
               DISPLAY g_expediente.v_tipo_flujo TO flbl_tipo_flujo_expediente
               DISPLAY g_expediente.v_f_captura  TO flbl_fecha_captura_expediente
               DISPLAY g_expediente.v_canal      TO flbl_origen_expediente
               DISPLAY g_expediente.v_acreditado TO flbl_acreditado_expediente
               DISPLAY g_expediente.v_trabajador TO flbl_trabajador_expediente
               
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
#Programa          => SEPF28                                                   #
#Descripcion       => Consulta expediente                                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente
   
   LET v_continuar = FALSE
   LET v_bnd_con = TRUE

   WHILE v_bnd_con # Ciclo para construir la consulta y recuperar los datos del expediente
      INITIALIZE v_filtro TO NULL
      CONSTRUCT v_filtro ON exp.caso_adai, 
                            exp.id_expediente 
                       FROM edi_caso_adai,
                            edi_id_expediente ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
         ON ACTION aceptar
            CALL GET_FLDBUF(edi_caso_adai) RETURNING v_caso_adai
            CALL GET_FLDBUF(edi_id_expediente) RETURNING v_id_expediente
            IF( v_caso_adai IS NULL AND v_id_expediente IS NULL )THEN
               CALL fn_mensaje(p_cad_ventana,"Al menos debe capturar un campo","information")
               NEXT FIELD edi_caso_adai
            END IF
            LET v_continuar = TRUE         
            ACCEPT CONSTRUCT # Construye consulta
          
         ON ACTION cancelar
            LET v_continuar = FALSE
            LET v_bnd_con = FALSE
            EXIT CONSTRUCT
         
      END CONSTRUCT

      IF(v_continuar)THEN # consulta datos si construye consulta(ACCEPT CONSTRUCT)
         CALL fn_recupera_expediente(v_filtro) RETURNING v_continuar
         # si se recuperó información termina ciclo
         IF(v_continuar)THEN
            EXIT WHILE
         ELSE
            CALL fn_mensaje(p_cad_ventana,"Expediente no encontrado","information")
            # Continua ciclo para realizar consulta de expediente
         END IF
      END IF
      
   END WHILE

   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF28                                                   #
#Descripcion       => Recupera datos expediente                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_recupera_expediente(v_filtro)
DEFINE v_filtro   STRING,
       v_consulta STRING,
       v_expediente_aux RECORD
         v_caso_adai           LIKE sep_expediente.caso_adai,
         v_tipo_flujo          LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura           DATE,
         v_canal               LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_id_expediente       LIKE sep_expediente.id_expediente,
         v_doc_complemetaria_1 LIKE sep_expediente.docto_restitucion_complementario_1,
         v_doc_complemetaria_2 LIKE sep_expediente.docto_restitucion_complementario_2,
         v_ind_complemetaria_1 LIKE sep_expediente.ind_restitucion_complementario_1
       END RECORD,
       v_docto_dictamen    LIKE sep_expediente.docto_dictamen,       
       v_continua          BOOLEAN

   INITIALIZE v_expediente_aux.* TO NULL
   LET v_continua = FALSE 
   # Recupera el expediente

   LET v_consulta = "\n SELECT exp.caso_adai,",
                    "\n        flo.flujo_desc,",
                    "\n        exp.f_captura,",
                    "\n        can.canal_desc,",
                    "\n        exp.id_expediente,",
                    "\n        exp.docto_restitucion_complementario_1,",
                    "\n        exp.docto_restitucion_complementario_2,",
                    "\n        exp.ind_restitucion_complementario_1",
                    "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_cat_tipo_flujo flo",
                    "\n     ON exp.flujo_cod = flo.flujo_cod",
                    "\n        LEFT OUTER JOIN sep_cat_canal_recepcion_exp can",
                    "\n     ON exp.canal_cod = can.canal_cod",
                    "\n  WHERE (exp.estado = 45", # 45 "Restitucion Solicitada" 
                    "\n     OR exp.estado = 50)", # 50 "Restitucion Liquidada"
                    "\n    AND (exp.ind_restitucion_complementario_1 IN (0,1)", # 0 "Sin Archivo Guardado", 1 "Archivo Sin Confirmar"
                    "\n     OR exp.ind_restitucion_complementario_2 IN (0,1))", # 0 "Sin Archivo Guardado", 1 "Archivo Sin Confirmar"
                    "\n    AND ",v_filtro

   PREPARE prp_recupera_expediente FROM v_consulta
   EXECUTE prp_recupera_expediente INTO v_expediente_aux.*
   IF(v_expediente_aux.v_id_expediente IS NOT NULL)THEN
      LET v_continua = TRUE
      LET g_expediente.v_caso_adai            = v_expediente_aux.v_caso_adai
      LET g_expediente.v_canal                = v_expediente_aux.v_canal
      LET g_expediente.v_tipo_flujo           = v_expediente_aux.v_tipo_flujo
      LET g_expediente.v_f_captura            = v_expediente_aux.v_f_captura USING "mm-dd-yyyy"
      LET g_expediente.v_canal                = v_expediente_aux.v_canal
      LET g_expediente.v_ind_complementario_1 = v_expediente_aux.v_ind_complemetaria_1

      # Recupera nss invadido del expediente 
      SELECT nss
        INTO g_expediente.v_acreditado
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 1 # Invadido

      # Recupera nss asociado del expediente 
      SELECT nss
        INTO g_expediente.v_trabajador
        FROM sep_nss_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente
         AND tipo_nss = 2 # Asociado

      IF(v_expediente_aux.v_ind_complemetaria_1 = 0 OR   # Sin Archivo Guardado
         v_expediente_aux.v_ind_complemetaria_1 = 1)THEN # Archivo Sin Confirmar
         # si no ha sido procesada complementaria 1
         LET v_docto_dictamen = v_expediente_aux.v_doc_complemetaria_1
      ELSE
         # si complementaria 1 ya fué procesada, procesa complementaria 2
         LET v_docto_dictamen = v_expediente_aux.v_doc_complemetaria_2
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
#Programa          => SEPF28                                                   #
#Descripcion       => Actualiza el documeto de transferencia                   #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_actualiza_docto(v_documento_imp)
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
       v_nom_archivo      STRING

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
      DISPLAY "Error de transferencia de archivo Restitución"
   ELSE
      LET v_documento[1].v_archivo = v_archivo_destino
      IF( g_expediente.v_ind_complementario_1 = 0 OR    # Sin Archivo Guardado
          g_expediente.v_ind_complementario_1 = 1 )THEN # Archivo Sin Confirmar
          
         # se actualiza el nombre del documento
         UPDATE sep_expediente
            SET docto_restitucion_complementario_1 = v_documento[1].v_archivo
          WHERE id_expediente = v_documento[1].v_id_expediente
      ELSE
         # se actualiza el nombre del documento
         UPDATE sep_expediente
            SET docto_restitucion_complementario_2 = v_documento[1].v_archivo
          WHERE id_expediente = v_documento[1].v_id_expediente
      END IF
      IF(SQLCA.SQLCODE = 0)THEN
         LET v_continuar = TRUE
      ELSE
        DISPLAY "Ocurrió un error de al actualizar archivo"
      END IF
   END IF
   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF28                                                   #
#Descripcion       => Elimina los registros temporales de las tablas de        #
#                     restitución complementrias                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 20 Enero 2015                                            #
################################################################################
FUNCTION fn_elimina_registros_temporales(p_id_expediente)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente 

   EXECUTE prp_elimina_155 USING p_id_expediente

   EXECUTE prp_elimina_mto_analisis USING p_id_expediente

   # Primero elimina sep_inf_restitucion con el id_restitucion de sep_restitucion
   EXECUTE prp_elimina_inf_restitucion USING p_id_expediente,
                                             p_id_expediente
   # Después se elimina sep_restitucion                                          
   EXECUTE prp_elimina_restitucion USING p_id_expediente,
                                         p_id_expediente

END FUNCTION
