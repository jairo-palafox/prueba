--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 14/06/2012
--===============================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPF04                                                     #
#Objetivo        => Programa de Carga de Op 40 correccion op28                 #
#                   de ruta local a ruta de rescate                            #
#Fecha Inicio    => Abrl 14, 2018                                              #
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

   CALL fn_carga_dicatmen()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_carga_dicatmen()
DEFINE v_continuar BOOLEAN,
       r_confirma  BOOLEAN,
       r_error     BOOLEAN,
       r_filtro    STRING,
       r_flujo_normal BOOLEAN, # variable para determinar activiad a realizar
       v_comando   STRING,
       v_id_expediente INTEGER,
       v_cad_busqueda  base.StringTokenizer,
       v_cad_reemplazo base.StringBuffer

   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   SELECT ruta_rescate
     INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   LET v_ruta_docto = v_ruta_docto CLIPPED,"/"

    
   OPEN WINDOW vtna_carag_dictamen WITH FORM v_ruta_ejecutable CLIPPED||"/SEPF041"
      #Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF(p_cad_ventana IS NOT NULL)THEN
         CALL ui.Interface.setText(p_cad_ventana)         
         CALL v_ventana.setText(p_cad_ventana)
      END IF
      # oculta grupos de datos exoediente y documento
      CALL v_forma.setElementHidden("gpo_captura",1)
      CALL v_forma.setElementHidden("gpo_expediente",1)
      CALL v_forma.setElementHidden("gpo_dicatamen",1)
      CALL v_forma.setElementHidden("btn_quitar",1)

      
#--      CALL fn_busca_expediente() RETURNING v_continuar,r_filtro

      LET v_continuar = 1
      
      IF(v_continuar)THEN
         # si recupewró infromacion, muestra detalle
#--         CALL v_forma.setElementHidden("gpo_expediente",0)
         CALL v_forma.setElementHidden("gpo_dicatamen",0)
         
         INPUT ARRAY v_documento FROM sr_documento.* 
            ATTRIBUTES(WITHOUT DEFAULTS, UNBUFFERED, ACCEPT = FALSE, CANCEL = FALSE, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)

            BEFORE INPUT
               CALL DIALOG.setActionHidden("dialogtouched", TRUE)
               # funicion que determina tipo de flujo
               #--CALL fn_determina_tarea(r_filtro)
               #--      RETURNING r_flujo_normal
               # en el caso de que NO sea flujo normal, se inhabilita el boton de confirmar
               
               LET r_flujo_normal = FALSE
               
               IF NOT(r_flujo_normal)THEN
                  CALL DIALOG.setActionHidden("confirmar",TRUE)
               ELSE
                  CALL DIALOG.setActionHidden("confirmar",FALSE)
               END IF
               
               DISPLAY v_expediente.v_caso_adai  TO flbl_caso_expediente
               DISPLAY v_expediente.v_tipo_flujo TO flbl_tipo_flujo_expediente
               DISPLAY v_expediente.v_f_captura  TO flbl_fecha_captura_expediente
               DISPLAY v_expediente.v_canal      TO flbl_origen_expediente
               DISPLAY v_expediente.v_acreditado TO flbl_acreditado_expediente
               DISPLAY v_expediente.v_trabajador TO flbl_trabajador_expediente
               
               IF(v_documento[1].v_nom_documento IS NOT NULL)THEN
                  CALL v_forma.setElementHidden("btn_quitar",0)
               END IF

            BEFORE FIELD tedi_seleccion
               INITIALIZE v_documento[1].v_ruta_local,v_documento[1].v_archivo TO NULL
               
            --ON CHANGE tedi_seleccion
            ON ACTION dialogtouched
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

               --LET v_documento[1].v_ruta_local = v_documento[1].v_ruta_local_exp
               LET v_documento[1].v_ruta_local = v_documento[1].v_archivo
               --LET v_documento[1].v_archivo    = v_documento[1].v_ruta_local_exp
               NEXT FIELD tedi_ruta
               
            ON ACTION aceptar
               IF(v_documento[1].v_ruta_local_exp IS NULL AND v_documento[1].v_nom_documento IS NULL)THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Dictámen","information")
                  CONTINUE INPUT
               END IF
               LET v_continuar = FALSE
               IF(v_documento[1].v_archivo IS NULL)THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Dictámen","information")
               ELSE
                  
                  CALL fn_actualiza_docto(v_documento[1].*) RETURNING v_continuar
                  IF(v_continuar)THEN
                     CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                     EXIT INPUT
                  ELSE
                     CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                     CONTINUE INPUT  
                  END IF
               END IF

            ON ACTION confirmar
               IF(v_documento[1].v_nom_documento IS NULL AND v_documento[1].v_ruta_local_exp IS NULL)THEN
                  CALL fn_mensaje(p_cad_ventana,"Seleccione archivo de Dictámen","information")
                  CONTINUE INPUT
               END IF
               CALL fn_ventana_confirma(p_cad_ventana,"Al confirmar no se podrá sustituir archivo","question") 
                   RETURNING r_confirma
               IF(r_confirma)THEN
                  # si seleccionó un archivo local, se transfiere
                  IF(v_documento[1].v_nom_documento IS NULL)THEN
                     CALL fn_actualiza_docto(v_documento[1].*) RETURNING v_continuar
                     IF(v_continuar)THEN
                         CALL fn_actualiza_maquinaria(v_documento[1].v_id_expediente) 
                                RETURNING r_error
                        IF(r_error)THEN                        
                           CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                           CONTINUE INPUT
                        ELSE
                           CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                           EXIT INPUT
                        END IF
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information") 
                        CONTINUE INPUT  
                     END IF
                  ELSE
                     CALL fn_actualiza_maquinaria(v_documento[1].v_id_expediente) 
                             RETURNING r_error
                     IF(r_error)THEN                        
                        CALL fn_mensaje(p_cad_ventana,"Ocurrió un error","information")
                        CONTINUE INPUT
                     ELSE
                        CALL fn_mensaje(p_cad_ventana,"Archivo actualizado con éxito","information")
                        EXIT INPUT
                     END IF

                  END IF
               ELSE
                  CONTINUE INPUT
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
                        SET docto_dictamen = NULL
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

            ON ACTION cancelar
               IF(LENGTH(v_documento[1].v_archivo) > 0)THEN
                  CALL fn_ventana_confirma(p_cad_ventana,"Desea salir sin registrar archivo?","question") 
                      RETURNING r_confirma
                  IF(r_confirma)THEN
                     EXIT INPUT
                  ELSE               
                     CONTINUE INPUT
                  END IF
               ELSE
                  EXIT INPUT
               END IF

         END INPUT

      END IF

   CLOSE WINDOW vtna_carag_dictamen

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 14 Junio 2012                                            #
################################################################################
FUNCTION fn_busca_expediente()
DEFINE v_filtro    STRING,
       v_continuar BOOLEAN,
       v_bnd_con   BOOLEAN,
       v_caso_adai LIKE sep_expediente.caso_adai,
       v_id_expediente LIKE sep_expediente.id_expediente

   LET v_continuar = FALSE
   LET v_bnd_con = TRUE
   LET v_filtro = " 1 = 2 "
   WHILE(v_bnd_con)
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

   RETURN v_continuar,v_filtro
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => Carga de archivo Dictámen                                #
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
       v_docto_dictamen  LIKE sep_expediente.docto_dictamen,
       
       v_continua BOOLEAN

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
                    "\n  WHERE exp.estado IN (35,40,45,50) ", # Modificacion para indicendia INFQA-117                    
                    "\n    AND ",v_filtro
                    --"\n  WHERE exp.estado = 35 ", # Caso asignado
                    --"\n    AND ",v_filtro
   
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
      SELECT docto_dictamen
        INTO v_docto_dictamen
        FROM sep_expediente
       WHERE id_expediente = v_expediente_aux.v_id_expediente

      # se recupera la ruta de documentos de separacion
      

      # Se establecen los datos del excel de dictamen, si es que ya existe
      LET v_documento[1].v_id_expediente  = v_expediente_aux.v_id_expediente
      LET v_documento[1].v_nom_documento  = v_docto_dictamen
      LET v_documento[1].v_ruta_servidor  = v_ruta_docto CLIPPED,"/",v_docto_dictamen CLIPPED
      --LET v_documento[1].v_ruta_local_exp = v_ruta_docto CLIPPED
            
   END IF

   RETURN v_continua
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
#Descripcion       => valida estado de expediente y determina si es flujo      #
#                     normal o solo es actualizacion de documento              #
#                     valores de retorno  TRUE  -> flujo normal                #
#                                         FALSE -> actualiza solo documento    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 17 Agosto 2012                                           #
################################################################################
FUNCTION fn_determina_tarea(p_filtro)
DEFINE p_filtro       STRING,
       v_consulta     STRING,
       v_estado       LIKE sep_expediente.estado,
       v_flujo_normal BOOLEAN

   LET v_flujo_normal = TRUE

   # Recupera estado del expediente para determinar actividad
   LET v_consulta = "\n SELECT FIRST 1 exp.estado",
                    "\n   FROM sep_expediente exp",
                    "\n  WHERE ",p_filtro
   PREPARE prp_rec_estado_exp FROM v_consulta
   EXECUTE prp_rec_estado_exp INTO v_estado

   # determina flujo
   CASE v_estado
      # para estado 'caso asignado' se toma flujo normal
      WHEN 35
         LET v_flujo_normal = TRUE 

      # para cualquier otro estado, ya se cofirmo archivo y solo se permite
      # actualizar el archivo
      OTHERWISE
         LET v_flujo_normal = FALSE

   END CASE
                    
   RETURN v_flujo_normal
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPF23                                                   #
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
      DISPLAY "Error al eliminar archivo dictámen"
   END IF
   # Reemplaza los espacios del archivo por guión bajo
   LET v_cad_reemplazo = base.StringBuffer.create()
   CALL v_cad_reemplazo.append(v_documento_imp.v_archivo CLIPPED) 
   CALL v_cad_reemplazo.replace(" ","_",0)
   LET v_nom_archivo = v_cad_reemplazo.toString()

   LET v_archivo_destino = v_documento_imp.v_id_expediente
   LET v_archivo_destino = v_archivo_destino.trim() ,
#--                           "-",
                            v_nom_archivo
   LET v_ruta_destino = v_ruta_docto CLIPPED,
                        v_archivo_destino

   # se transfiere el archivo a servidor
   DISPLAY "Ruta seleccionada: ",v_documento_imp.v_ruta_local_exp
   DISPLAY "Ruta destino: ",v_ruta_destino
   CALL FGL_GETFILE(v_documento_imp.v_ruta_local_exp, v_ruta_destino)
   IF(STATUS)THEN
      --CALL fn_mensaje(p_cad_ventana,"Ocurrió un error al transferir el documento","information")
      DISPLAY "Error de transferencia de archivo dictámen"
   ELSE
      
      --LET v_documento[1].v_archivo = v_nom_archivo # antes
      LET v_documento[1].v_archivo = v_archivo_destino
      # se actualiza el nombre del documento
      UPDATE sep_expediente
         SET docto_dictamen = v_documento[1].v_archivo
       WHERE id_expediente = v_documento[1].v_id_expediente

      IF(SQLCA.SQLCODE = 0)THEN
         LET v_continuar = TRUE
         --LET v_comando = v_documento_imp.v_id_expediente
         
      ELSE
        DISPLAY "Error de actualizacion de registro de archivo dictámen"
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
FUNCTION fn_actualiza_maquinaria(p_id_expediente)
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
END FUNCTION
