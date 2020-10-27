--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 21/06/2012
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPF27                                                    #
#Objetivo        => Programa de Carga de Aviso de Suspensión                           #
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

   CALL fn_carga_restitucion()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF27                                                   #
#Descripcion       => Carga de archivo Aviso de Suspensión                                #
#Autor             =>                               #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_carga_restitucion()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       r_error     BOOLEAN,
       v_comando   STRING,
       v_id_expediente   INTEGER,
       sin_archivo       INTEGER ,
       aport_no_aplicada INTEGER,
       v_indicador       SMALLINT,
       v_cad_busqueda    base.StringTokenizer,
       v_cad_reemplazo   base.StringBuffer

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_docto
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF271"
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
      CALL fn_busca_expediente() RETURNING v_continuar
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
               {IF(v_documento[1].v_nom_documento IS NOT NULL)THEN
                  CALL v_forma.setElementHidden("btn_quitar",0)
                  CALL v_forma.setElementHidden ("sin_archivo",1) 
                  LET sin_archivo = 0
               ELSE
                  CALL v_forma.setElementHidden("btn_quitar",1)
                  CALL v_forma.setElementHidden ("sin_archivo",0) 
                  LET sin_archivo = 0
               END IF}
                         
            --ON CHANGE tedi_seleccion
            ON ACTION dialogtouched 

               LET v_cad_reemplazo = base.StringBuffer.create()
               CALL v_cad_reemplazo.append(v_documento[1].v_ruta_local_exp CLIPPED) 
               CALL v_cad_reemplazo.replace("\\","|",0)
               LET v_documento[1].v_archivo = v_cad_reemplazo.toString()
               LET v_cad_busqueda = base.StringTokenizer.create(v_documento[1].v_archivo,"|")
               WHILE v_cad_busqueda.hasMoreTokens()
                  --LET v_nom_archivo = v_cad_busqueda.nextToken()
                  LET v_documento[1].v_archivo = v_cad_busqueda.nextToken()
               END WHILE

               --LET v_documento[1].v_ruta_local = v_documento[1].v_ruta_local_exp
               LET v_documento[1].v_ruta_local = v_documento[1].v_archivo
               --LET v_documento[1].v_archivo    = v_documento[1].v_ruta_local_exp
               
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
            IF(sin_archivo = 0)THEN
               IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL )THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Aviso de Suspensión","information")
                  CONTINUE DIALOG
               END IF
               LET v_continuar = FALSE
               IF(v_documento[1].v_archivo IS NULL)THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Aviso de Suspensión","information")
               ELSE
                  UPDATE sep_expediente
                     SET ind_aviso_suspension = 1 # archivo sin confirmar
                   WHERE id_expediente = v_documento[1].v_id_expediente
                  IF(SQLCA.SQLCODE <> 0)THEN
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al actualizar información","information")
                     CONTINUE DIALOG
                  END IF
                  CALL fn_actualiza_docto(v_documento[1].*) RETURNING v_continuar
                  IF(v_continuar)THEN
                     CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                     EXIT DIALOG
                  ELSE
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                     CONTINUE DIALOG  
                  END IF
               END IF
            ELSE
                  CALL fn_mensaje(p_cad_ventana,"Casilla 'sin Archivo' activa.","information")
                  EXIT DIALOG
            END  IF 


            
            ON ACTION confirmar
            IF(sin_archivo = 0)THEN
               IF(v_documento[1].v_nom_documento IS NULL AND v_documento[1].v_ruta_local_exp IS NULL)THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Aviso de Suspensión","information")
                  CONTINUE DIALOG
               END IF
               CALL fn_ventana_confirma(p_cad_ventana,"Al confirmar no se podrá sustituir archivo","question") 
                   RETURNING r_confirma
               IF(r_confirma)THEN
                  # si seleccionó un archivo local, se transfiere
                  IF(v_documento[1].v_nom_documento IS NULL)THEN
                     CALL fn_actualiza_docto(v_documento[1].*) RETURNING v_continuar
                     IF(v_continuar)THEN
                         {CALL fn_actualiza_maquinaria(v_documento[1].v_id_expediente) 
                                RETURNING r_error
                        IF(r_error)THEN                        
                           CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                           CONTINUE DIALOG
                        ELSE}
                           CALL fn_carga_aviso_suspension(v_documento[1].v_id_expediente,p_usuario_cod,v_documento[1].v_archivo)
                           RETURNING v_indicador
                           IF(v_indicador = 0)THEN
                              CALL fn_mensaje(p_cad_ventana,"Datos Cargados...","information")
                              EXIT DIALOG
                           ELSE
                              CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al cargar información del archivo","stop")
                           END IF
                        --END IF
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                        CONTINUE DIALOG  
                     END IF
                  ELSE
                     {CALL fn_actualiza_maquinaria(v_documento[1].v_id_expediente) 
                             RETURNING r_error
                     IF(r_error)THEN                        
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                        CONTINUE DIALOG
                     ELSE}
                        CALL fn_carga_aviso_suspension(v_documento[1].v_id_expediente,p_usuario_cod,v_documento[1].v_nom_documento)
                               RETURNING v_indicador
                        IF(v_indicador = 0)THEN
                           CALL fn_mensaje(p_cad_ventana,"Datos Cargados...","information")
                           EXIT DIALOG
                        ELSE
                           CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al cargar información del archivo","stop")
                        END IF
                     --END IF

                  END IF
               ELSE
                  CONTINUE DIALOG
               END IF
            ELSE 
                {CALL fn_actualiza_maquinaria(v_documento[1].v_id_expediente) 
                             RETURNING r_error
                 IF(r_error)THEN                        
                    CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                    CONTINUE DIALOG
                 ELSE}
                    CALL fn_carga_aviso_suspension(v_documento[1].v_id_expediente,p_usuario_cod,v_documento[1].v_archivo)
                          RETURNING v_indicador
                    IF(v_indicador = 0)THEN
                       UPDATE sep_expediente
                          SET ind_aviso_suspension = 4 # confirmado sin archivo
                        WHERE id_expediente = v_documento[1].v_id_expediente
                       CALL fn_mensaje(p_cad_ventana,"Datos Cargados...","information")
                       EXIT DIALOG
                    ELSE
                       CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al cargar información del archivo","stop")
                    END IF
                 --END IF   
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
               CALL fn_ventana_confirma(p_cad_ventana,"Eliminar archivo","question") 
                   RETURNING r_confirma
               IF(r_confirma)THEN
                  # Elimina fisicamente el archivo
                  LET v_id_expediente = v_documento[1].v_id_expediente 
                  LET v_comando = v_id_expediente 
                  LET v_comando = "rm ",v_ruta_docto CLIPPED,
                                  --v_comando.trim(), "-",
                                  v_documento[1].v_nom_documento CLIPPED
                               
                  DISPLAY v_comando
                  RUN v_comando
                  IF(STATUS)THEN
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al eliminar archivo","information")
                  ELSE
                     UPDATE sep_expediente
                        SET docto_aviso_suspension = NULL
                      WHERE id_expediente = v_documento[1].v_id_expediente
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
#Programa          => SEPF27                                                   #
#Descripcion       => Carga de archivo Aviso de Suspensión                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente
   -- LET aport_no_aplicada=0
   LET v_continuar = FALSE
   LET v_bnd_con = TRUE
   LET v_filtro = " 1 = 2 "
    WHILE (v_bnd_con)
      CONSTRUCT v_filtro ON exp.caso_adai, exp.id_expediente FROM edi_caso_adai,edi_id_expediente 
          ATTRIBUTES(ACCEPT = FALSE, CANCEL = FALSE)
      
       ON ACTION aceptar
          
          INITIALIZE v_caso_adai, v_id_expediente TO NULL 
          CALL GET_FLDBUF(edi_caso_adai) RETURNING v_caso_adai
          CALL GET_FLDBUF(edi_id_expediente) RETURNING v_id_expediente
          IF(v_caso_adai IS NULL AND v_id_expediente IS NULL)THEN
             CALL fn_mensaje(p_cad_ventana,"Al menos debe capturar un campo","information")
             CONTINUE CONSTRUCT
          END IF
          LET v_bnd_con = TRUE
          EXIT CONSTRUCT
          
        ON ACTION cancelar
           LET v_bnd_con = FALSE
           EXIT CONSTRUCT
    END CONSTRUCT
 
      IF(v_bnd_con)THEN
         
         CALL fn_recupera_expediente(v_filtro) RETURNING v_continuar
         # si se recuperó información termina ciclo
         IF(v_continuar)THEN
            LET v_bnd_con = FALSE
         ELSE
            CALL fn_mensaje(p_cad_ventana,"Expediente no encontrado","information")
            LET v_bnd_con = TRUE
         END IF
      END IF
   END WHILE

   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF27                                                   #
#Descripcion       => Carga de archivo Aviso de Suspensión                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_recupera_expediente(v_filtro)
DEFINE v_filtro   STRING,
       v_consulta STRING,
       v_expediente_aux RECORD
         v_caso_adai     LIKE sep_expediente.caso_adai,
         v_tipo_flujo    LIKE sep_cat_tipo_flujo.flujo_desc,
         v_f_captura     DATE,--LIKE sep_expediente.f_captura,
         v_canal         LIKE sep_cat_canal_recepcion_exp.canal_desc,
         v_id_expediente LIKE sep_expediente.id_expediente
       END RECORD,
       v_docto_suspension  LIKE sep_expediente.docto_dictamen,
       
       v_continua BOOLEAN,
       v_aport_no_aplicada INTEGER

   WHENEVER ERROR CONTINUE
   INITIALIZE v_expediente_aux.* TO NULL
   LET v_continua = FALSE 
   # Recupera el expediente
   LET v_consulta = "\n SELECT exp.caso_adai, flo.flujo_desc, ",
                    "\n        exp.f_captura, can.canal_desc, exp.id_expediente",
                    "\n   FROM sep_expediente exp LEFT OUTER JOIN sep_cat_tipo_flujo flo",
                    "\n     ON exp.flujo_cod = flo.flujo_cod",
                    "\n        LEFT OUTER JOIN sep_cat_canal_recepcion_exp can",
                    "\n     ON exp.canal_cod = can.canal_cod",
                    "\n  WHERE exp.estado = 40", # Caso asignado
                    "\n    AND exp.ind_aviso_suspension IN (0,1)",
                    "\n    AND ",v_filtro
   
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

      # si ya existe el documento, lo recupera
      SELECT docto_aviso_suspension
        INTO v_docto_suspension
        FROM sep_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente

      # se recupera la ruta de documentos de separacion
      

      # Se establecen los datos del excel de dictamen, si es que ya existe
      LET v_documento[1].v_id_expediente  = v_expediente_aux.v_id_expediente
      LET v_documento[1].v_nom_documento  = v_docto_suspension
      LET v_documento[1].v_ruta_servidor  = v_ruta_docto CLIPPED,"/",v_docto_suspension CLIPPED
      --LET v_documento[1].v_ruta_local_exp = v_ruta_docto CLIPPED
            
   END IF

   RETURN v_continua
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF27                                                   #
#Descripcion       => Actualiza el documeto                                    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
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
                         --v_comando.trim(), "-",
                         v_documento_imp.v_nom_documento CLIPPED
   DISPLAY v_comando
   RUN v_comando
   IF(STATUS)THEN
      DISPLAY "Error al eliminar archivo Aviso de Suspensión"
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
      --CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information")
      DISPLAY "Error de transferencia de archivo Aviso de Suspensión"
   ELSE
      
      --LET v_documento[1].v_archivo = v_nom_archivo # antes
      LET v_documento[1].v_archivo = v_archivo_destino
      # se actualiza el nombre del documento
      UPDATE sep_expediente
         SET docto_aviso_suspension = v_documento[1].v_archivo
       WHERE id_expediente = v_documento[1].v_id_expediente

      IF(SQLCA.SQLCODE = 0)THEN
         LET v_continuar = TRUE
         --LET v_comando = v_documento_imp.v_id_expediente
         
      ELSE
        DISPLAY "Error de actualizacion de registro de archivo Aviso de Suspensión"
      END IF
   END IF
   RETURN v_continuar
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF27                                                   #
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

