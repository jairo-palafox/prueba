####################################################################
#Modulo            =>PRT                                           #
#Programa          =>PRTF20                                        #
#Objetivo          =>Captura de expediente portabilidad convenios  #
#Autor             =>Jesús Yanez Moreno, EFP                       #
#Fecha inicio      =>27 DIC 2017                                   #
####################################################################
IMPORT os  
DATABASE safre_viv
DEFINE v_txt   char(500)
DEFINE g_enter char(01)

DEFINE v_r_nss_inv_id_nss_trab1 decimal(9,0)
DEFINE v_r_nss_inv_id_nss_trab2 decimal(9,0)
DEFINE v_r_id_cre_acreditado    decimal(9,0)

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod
DEFINE p_tipo_ejecucion SMALLINT
DEFINE p_titulo         STRING
DEFINE v_r_nss_inv      RECORD
          id_expediente              LIKE prt_expediente_convenios_iss.id_prt_expediente_convenios_iss,
          id_nss_expediente1         LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab1                  LIKE sep_nss_expediente.nss,
          nombre_trab                LIKE prt_expediente_convenios_iss.nombre,
          saldo_insoluto_credito_iss LIKE prt_expediente_convenios_iss.saldo_insoluto_credito_iss, 
          aivs_sar92                 LIKE cta_movimiento.monto_acciones,          
          pesos_sar92                LIKE cta_movimiento.monto_pesos,          
          aivs_viv97                 LIKE cta_movimiento.monto_acciones,          
          pesos_viv97                LIKE cta_movimiento.monto_pesos,          
          aivs_sar92_solicitado      LIKE cta_movimiento.monto_acciones,          
          pesos_sar92_solicitado     LIKE cta_movimiento.monto_pesos,          
          aivs_viv97_solicitado      LIKE cta_movimiento.monto_acciones,          
          pesos_viv97_solicitado     LIKE cta_movimiento.monto_pesos,          
          aivs_sar92_transferido     LIKE cta_movimiento.monto_acciones,          
          pesos_sar92_transferido    LIKE cta_movimiento.monto_pesos,          
          aivs_viv97_transferido     LIKE cta_movimiento.monto_acciones,          
          pesos_viv97_transferido    LIKE cta_movimiento.monto_pesos,          
          estado                     LIKE prt_expediente_convenios_iss.estado,          
          f_captura                  LIKE prt_expediente_convenios_iss.f_captura,
          f_confirma                 LIKE prt_expediente_convenios_iss.f_confirma,          
          folio_liquida              LIKE prt_expediente_convenios_iss.folio_liquida,          
          id_cat_entidad             LIKE prt_expediente_convenios_iss.id_cat_entidad 
       END RECORD
       
DEFINE v_ar_docto_exp   DYNAMIC ARRAY OF RECORD 
          id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
          docto_cod     LIKE sep_cat_docto.docto_cod,
          ind_requerido LIKE sep_cat_docto.ind_requerido,
          docto_desc    LIKE sep_cat_docto.docto_desc,
          file_upload   STRING,
          docto_nota    CHAR(200),
          file_upload2  CHAR(200),
          ruta_docto    CHAR(200)
       END RECORD
DEFINE v_ventana        ui.Window
DEFINE v_forma          ui.Form
DEFINE v_SqlQry         STRING
DEFINE cb               ui.ComboBox
DEFINE v_g_estado       LIKE sep_det_02_op27.estado -- Estado localizado en op27 del expediente capturado
DEFINE v_id_maquinaria  LIKE glo_cat_maquinaria.id_maquinaria

DEFINE v_ind                               SMALLINT       
DEFINE v_diag                              CHAR(255)      
DEFINE v_error_sql                         SMALLINT       
DEFINE v_error_isam                        SMALLINT       
DEFINE v_msg_sql                           CHAR(255)      
DEFINE v_estado_destino                    SMALLINT       
DEFINE v_precio_fondo                      LIKE glo_valor_fondo.precio_fondo
MAIN

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   CALL STARTLOG(p_usuario_cod CLIPPED||".SEPL10.log")

   LET v_id_maquinaria = 6      
   -- Rutina que define el estado final despues de cualquier accion
   -- AHM 20120601 Cambio la maquinaria de estados LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_maq_sep_expediente(?,?)"
   --LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_maquinaria(?,?,?)"
   LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_glo_maq_individual(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la maquinaria de estados
   PREPARE prp_glo_maq_individual FROM v_SqlQry

   --CALL ui.Interface.loadStyles("sepstyle")
   
   -- Menu principal de la captura de expedientes
   OPEN WINDOW w_captura_expediente WITH FORM "PRTF201"
   MENU
      BEFORE MENU
         IF p_titulo IS NOT NULL THEN
            CALL ui.Interface.setText(p_titulo)
            LET v_ventana = ui.Window.getCurrent()
            CALL v_ventana.setText(p_titulo)
         END IF
      
      ON ACTION nuevo   
         -- Rutina para la captura de expedientes nuevos
         CALL fn_captura_expediente('A')
         
      ON ACTION Recuperar
         -- Rutina para la consutla de expedientes pervinente capturados
         CALL fn_consulta_expediente()
         IF LENGTH(v_r_nss_inv.f_captura CLIPPED) > 0 THEN
            -- Rutina para complementar la captura de expedientes existentes
            CALL fn_captura_expediente('M')
             
         END IF
         
      ON ACTION cancel
         EXIT MENU
         
   END MENU

   CLOSE WINDOW w_captura_expediente

END MAIN

#############################################################################
# Funcion           => fn_captura_expediente - Rutina para la captura y mo- #
#                      dificacion de expedientes en estado capturado        #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => v_modo - modo de captura 'A' Nuevo, 'M' Modificacion #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_captura_expediente(v_modo)
DEFINE v_modo          CHAR(1)
DEFINE v_resultado     STRING
DEFINE v_pos           SMALLINT
DEFINE v_c_archivo     VARCHAR(200)
DEFINE v_estado_desc   LIKE sep_estado_expediente.descripcion
DEFINE v_cad_busqueda  base.StringTokenizer,
       v_cad_reemplazo base.StringBuffer,
       v_archivo       STRING

   DIALOG ATTRIBUTES (UNBUFFERED)
   
   -- Captura de los NSS del expediente a separar
   INPUT BY NAME v_r_nss_inv.*
      ATTRIBUTES (WITHOUT DEFAULTS = TRUE)
   
      BEFORE INPUT
      
         # recupera la descripcion del estado del expediente
         LET v_estado_desc = ''
         SELECT descripcion
           INTO v_estado_desc
           FROM sep_estado_expediente
          WHERE estado = v_r_nss_inv.estado

         DISPLAY v_estado_desc TO edi_exp_estado 
         
         -- asiganación de expediente

                 
         LET v_forma = v_ventana.getForm()
         
         LET v_r_nss_inv.f_captura = TODAY
         DISPLAY BY NAME v_r_nss_inv.f_captura,
                         v_r_nss_inv.id_expediente



                         
      ON CHANGE nss_trab1
              LET v_r_nss_inv_id_nss_trab1 = null      
              
              SELECT a.id_derechohabiente 
              INTO   v_r_nss_inv_id_nss_trab1
              FROM   afi_derechohabiente a
              WHERE  a.nss = v_r_nss_inv.nss_trab1              

              IF STATUS = NOTFOUND THEN 
                 IF v_r_nss_inv.nss_trab1 IS NOT NULL THEN
                    CALL fn_mensaje("Aviso","Nss no valido","info")
                    NEXT FIELD nss_trab1
                 END IF 
              ELSE 
              
                 LET v_r_nss_inv.id_nss_expediente1 = v_r_nss_inv_id_nss_trab1              
                 
                 SELECT TRIM(a.ap_paterno_af)||" "||
                        TRIM(a.ap_materno_af)||" "||
                        TRIM(a.nombre_af)
                 INTO v_r_nss_inv.nombre_trab
                 FROM  afi_derechohabiente a
                 WHERE a.id_derechohabiente = v_r_nss_inv_id_nss_trab1 

                 SELECT "OK" 
                 FROM   sfr_marca_activa 
                 WHERE  id_derechohabiente  = v_r_nss_inv_id_nss_trab1
                 AND    marca IN (SELECT b.marca 
                                  FROM sfr_marca b 
                                  WHERE b.descripcion_marca MATCHES '*CR*DITO*')

                 IF STATUS <> NOTFOUND THEN 
                    CALL fn_mensaje("Aviso","Cuenta con un crédito vigente, solicitud no procedente","info")
                    NEXT FIELD nss_trab1
                 END IF

                CALL sp_obtiene_saldo(0,0,0) 
                
                DISPLAY BY NAME v_r_nss_inv.aivs_sar92
                DISPLAY BY NAME v_r_nss_inv.pesos_sar92
                DISPLAY BY NAME v_r_nss_inv.aivs_viv97
                DISPLAY BY NAME v_r_nss_inv.pesos_viv97

              END IF

       AFTER FIELD aivs_sar92_solicitado
                CALL sp_obtiene_saldo(1,8,v_r_nss_inv.aivs_sar92_solicitado)       
       AFTER FIELD aivs_viv97_solicitado
                CALL sp_obtiene_saldo(1,4,v_r_nss_inv.aivs_viv97_solicitado)       

           
   END INPUT

   -- Captura de los documentos que asociaran al expediente
   --DISPLAY ARRAY v_ar_docto_exp TO tbl_sep_docto_exp.*
     
   INPUT ARRAY v_ar_docto_exp FROM tbl_sep_docto_exp.*
      ATTRIBUTE (INSERT ROW = FALSE, APPEND ROW = FALSE ,
                 DELETE ROW = FALSE, AUTO APPEND = FALSE ,
                 WITHOUT DEFAULTS = TRUE)

      --BEFORE DISPLAY
      BEFORE INPUT
         CALL DIALOG.setActionHidden("dialogtouched", TRUE)
         LET v_forma = v_ventana.getForm() 
         IF v_r_nss_inv.estado = 22 THEN
            --CALL v_forma.setFieldHidden( "tedi_ruta", 0 )
            CALL DIALOG.setFieldActive( "tedi_ruta", 0 )
         ELSE --CALL v_forma.setFieldHidden( "tedi_ruta", 1 )
            CALL DIALOG.setFieldActive( "tedi_ruta", 1 )
         END IF
 
      -- Rutina para seleccionar archivos a vincular desde el equipo local
      --ON CHANGE tedi_ruta
      ON ACTION dialogtouched
         IF(FIELD_TOUCHED(tbl_sep_docto_exp.tedi_ruta))THEN
            # reemplaza \ por |
            LET v_cad_reemplazo = base.StringBuffer.create()
            CALL v_cad_reemplazo.append(v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED) 
            CALL v_cad_reemplazo.replace("\\","|",0)
               
            --LET v_documento[1].v_archivo = v_cad_reemplazo.toString()
            LET v_archivo = v_cad_reemplazo.toString()

            # Recupera el ultimo elemento de la cadena 
            LET v_cad_busqueda = base.StringTokenizer.create(v_archivo,"|")
            WHILE v_cad_busqueda.hasMoreTokens()
               --LET v_nom_archivo = v_cad_busqueda.nextToken()
               LET v_archivo = v_cad_busqueda.nextToken()
            END WHILE
         
            CALL GET_FLDBUF(tedi_ruta) RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
            LET v_ar_docto_exp[ARR_CURR()].file_upload  = v_archivo 
            CALL GET_FLDBUF(ruta_docto) RETURNING v_ar_docto_exp[ARR_CURR()].ruta_docto

            IF(v_ar_docto_exp[ARR_CURR()].file_upload IS NOT NULL)THEN            
               -- Rutina que renombra y transfiere el archivo al servidor
               CALL fn_transfiere_archivo(v_ar_docto_exp[ARR_CURR()].*,'','') RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
               -- Rutina para visualizar la liga y ruta de los documentos asociados al expediente
               LET v_ar_docto_exp[ARR_CURR()].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"','prtdocto')\" target='nueva'>",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"</a>"
            END IF
            NEXT FIELD docto_nota
         END IF
         
 
      -- Rutina para transferencia de archivos selecionados al servidor para su posterior vinculación al expediente
      {AFTER FIELD tedi_ruta
         CALL fn_mensaje("","llamada","")
         CALL GET_FLDBUF(tedi_ruta) RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
         IF(v_ar_docto_exp[ARR_CURR()].file_upload IS NOT NULL)THEN
            --LET v_ar_docto_exp[ARR_CURR()].file_upload2 = v_ar_docto_exp[ARR_CURR()].file_upload
            -- Rutina que renombra y transfiere el archivo al servidor
            CALL fn_transfiere_archivo(v_ar_docto_exp[ARR_CURR()].*,'','') RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
            -- Rutina para visualizar la liga y ruta de los documentos asociados al expediente
            LET v_ar_docto_exp[ARR_CURR()].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"','prtdocto')\" target='nueva'>",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"</a>"
         END IF}

      -- Rutina para desvincular documentos previamente asociados
      ON ACTION btn_img_quitar
         # elimina fisicamente el archivo
         CALL fn_admon_archivo_sep(v_ar_docto_exp[ARR_CURR()].file_upload, 0, 0, 'B') 
                RETURNING v_resultado
         IF(v_resultado = "ELIMINADO")THEN
            -- Eliminación de documentos de la base de datos,
            # si el archivo es temporal, no esta registrado en sep_docto_exp y no 
            # encuentra el archivo a eliminar. Solo se actualiza el campo en sep_expediente  
            DELETE 
              FROM prt_docto_exp
             WHERE id_expediente = v_r_nss_inv.id_expediente
               AND docto_cod = v_ar_docto_exp[ARR_CURR()].docto_cod
               
            {UPDATE sep_docto_exp
               SET docto_nombre = "",
                   docto_nota   = v_ar_docto_exp[v_pos].docto_nota
             WHERE id_sep_docto_exp = v_ar_docto_exp[v_pos].id_sep_docto_exp}

            # Elimina de los arreglos( temporal y general)
            LET v_ar_docto_exp[ARR_CURR()].file_upload = NULL
            LET v_ar_docto_exp[ARR_CURR()].docto_nota  = ''
            LET v_ar_docto_exp[ARR_CURR()].file_upload2= ''
            LET v_ar_docto_exp[ARR_CURR()].ruta_docto  = ''
            DISPLAY v_ar_docto_exp[ARR_CURR()].file_upload ,
                    v_ar_docto_exp[ARR_CURR()].file_upload2,
                    v_ar_docto_exp[ARR_CURR()].ruta_docto
                 TO tedi_ruta,
                    file_upload2,
                    ruta_docto
         ELSE
            # si no se pudo elminiar
            CALL fn_mensaje("Aviso","Registro "||v_resultado CLIPPED,"about")
            # Elimina de los arreglos( temporal y general)
            {LET v_ar_docto_exp[ARR_CURR()].file_upload = ''
            LET v_ar_docto_exp[ARR_CURR()].file_upload2= ''
            LET v_ar_docto_exp[ARR_CURR()].ruta_docto  = ''
            DISPLAY v_ar_docto_exp[ARR_CURR()].file_upload ,
                    v_ar_docto_exp[ARR_CURR()].file_upload2,
                    v_ar_docto_exp[ARR_CURR()].ruta_docto
                 TO tedi_ruta,
                    file_upload2,
                    ruta_docto}         

         END IF

   --END DISPLAY
   END INPUT
 
   BEFORE DIALOG
      --Solo lectura para estado 22 Cancelado
      IF v_r_nss_inv.estado = 22 THEN
         CALL DIALOG.setActionHidden( "guardar", 1 )
         CALL DIALOG.setActionHidden( "aceptar", 1 )
      ELSE
         CALL DIALOG.setActionHidden( "guardar", 0 )
         CALL DIALOG.setActionHidden( "aceptar", 0 )
      END IF
      
      -- Inicialización general de las pantallas de captura
DISPLAY "expediente carga docto: ",v_r_nss_inv.id_expediente      
      CALL fn_carga_docto(v_modo,v_r_nss_inv.id_expediente)
      
      CALL init_combo_entidad_convenio()
      

      IF v_modo = 'M' THEN
         CALL fn_carga_nss(v_r_nss_inv.id_expediente)
         DISPLAY BY NAME v_r_nss_inv.*
      END IF 

   -- Rutina para almacenar expediente sin finalizarlo
   ON ACTION guardar
      -- Validacion parcial de la información del expediente
      IF fn_valida_guardado(0) THEN
         -- Solicitud de confirnación de operacion
         IF fn_ventana_confirma("Aviso","¿Guardar Captura?","question") = 1 THEN
            -- Rutina para gurdar expediente sin finalizar
            IF fn_guarda_expediente('G') THEN
               INITIALIZE v_r_nss_inv TO NULL
               CALL fn_mensaje("Aviso","Registro almacenado exitosamente","about")
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
            
         END IF
      END IF
      
   ON ACTION aceptar
      -- Validacion total de la información del expediente
      IF fn_valida_aceptar() THEN
         -- Solicitud de confirnación de operacion
         IF fn_ventana_confirma("Aviso","¿Registrar Captura?","question") = 1 THEN
            -- Rutina para gurdar expediente y finalizarlo
            IF fn_guarda_expediente('A') THEN
               INITIALIZE v_r_nss_inv TO NULL
               CALL fn_mensaje("Aviso","Registro almacenado exitosamente","about")
               EXIT DIALOG
            ELSE
               CONTINUE DIALOG
            END IF
         END IF
      END IF
      
   -- Rutina para eliminar el expediente completo
   ON ACTION eliminar
      -- Solicitud de confirnación de operacion
      IF fn_ventana_confirma("Confimar","Confirmación de eliminación de Expediente Preliminar","info") = 1 THEN
         # Se eliminan los archivos que han sido transferidos
         IF(v_ar_docto_exp.getLength() > 0)THEN
            FOR v_pos = 1 TO v_ar_docto_exp.getLength()
               CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload, 0, 0, 'B') 
                  RETURNING v_resultado
            END FOR 
         END IF
         
         -- Eliminación de documentos de la base de datos
         DELETE FROM prt_docto_exp
         WHERE id_expediente = v_r_nss_inv.id_expediente
         
         -- Eliminación del expediente de la base de datos
         DELETE FROM prt_expediente_convenios_iss
         WHERE id_prt_expediente_convenios_iss = v_r_nss_inv.id_expediente

         CALL v_ar_docto_exp.clear()
         INITIALIZE v_r_nss_inv TO NULL

         EXIT DIALOG
      ELSE
         CONTINUE DIALOG
      END IF
   
   ON ACTION salir
      -- Solicitud de confirnación de operacion
      IF fn_ventana_confirma("Confimar","¿Desea salir y cancelar la captura?","info") = 1 THEN
         # Se eliminan los archivos nuevos que han sido transferidos
         IF(v_ar_docto_exp.getLength() > 0)THEN
            FOR v_pos = 1 TO v_ar_docto_exp.getLength()
               LET v_c_archivo = v_ar_docto_exp[v_pos].file_upload
               IF v_c_archivo[1,3] = 'tmp' THEN
                  CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload, 0, 0, 'B') 
                         RETURNING v_resultado
               END IF
            END FOR 
         END IF

         CALL v_ar_docto_exp.clear()
         INITIALIZE v_r_nss_inv TO NULL

         EXIT DIALOG
      ELSE
         CONTINUE DIALOG
      END IF

   END DIALOG
END FUNCTION

#############################################################################
# Funcion           => fn_consulta_expediente - Rutina para consulta de     #
#                      expedientes en estado capturado                      #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_consulta_expediente()
DEFINE v_ar_expediente DYNAMIC ARRAY OF RECORD 
          id_expediente                   LIKE sep_expediente.id_expediente    ,
          f_captura                       LIKE sep_expediente.f_captura        ,
	      nss                             LIKE sep_nss_expediente.nss          ,
          nombre_trab                     LIKE sep_expediente.nombre_reclamante ,
          estado                          LIKE sep_expediente.estado                        
		END RECORD
DEFINE v_qrytxt        STRING
DEFINE v_pos           SMALLINT

   OPEN WINDOW w_consulta_expediente WITH FORM "PRTF202"
   
   -- Seleccion de expedientes en estado de captura
   LET v_qrytxt = "SELECT a.id_prt_expediente_convenios_iss,",
                  "       a.f_captura, ",
                  "       a.nss , ",
                  "       TRIM(b.ap_paterno_af)||' '||TRIM(b.ap_materno_af)||' '||TRIM(nombre_af), ",                  
                  "       a.estado ",
                  "  FROM prt_expediente_convenios_iss a, ",
                  "       afi_derechohabiente b ",
                  " WHERE a.estado in (0)",
                  "   AND a.id_derechohabiente = b.id_derechohabiente "
   
   PREPARE EnuRecExp FROM v_qrytxt
   DECLARE CurRecExp CURSOR FOR EnuRecExp
   
   LET v_pos = 1
   FOREACH CurRecExp INTO v_ar_expediente[v_pos].*
      LET v_pos = v_pos + 1
   END FOREACH
   IF LENGTH(v_ar_expediente[v_ar_expediente.getLength()].id_expediente CLIPPED) = 0 THEN
      CALL v_ar_expediente.deleteElement(v_ar_expediente.getLength())
   END IF
   
   -- Despliegue de los expedientes que pueden ser modificados
   LET INT_FLAG = FALSE
   DISPLAY ARRAY v_ar_expediente TO tbl_expediente.*
      ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE)
      BEFORE DISPLAY 
         IF v_ar_expediente.getLength() = 0 THEN
            CALL fn_mensaje("Aviso","No hay expedientes por mostrar","info")
            INITIALIZE v_r_nss_inv TO NULL
            EXIT DISPLAY
         END IF
            
      ON ACTION seleccionar
         LET v_r_nss_inv.id_expediente                   = v_ar_expediente[ARR_CURR()].id_expediente                   
         LET v_r_nss_inv.f_captura                       = v_ar_expediente[ARR_CURR()].f_captura                       
         LET v_r_nss_inv.nss_trab1                       = v_ar_expediente[ARR_CURR()].nss
         LET v_r_nss_inv.nombre_trab                     = v_ar_expediente[ARR_CURR()].nombre_trab
         LET v_r_nss_inv.estado                          = v_ar_expediente[ARR_CURR()].estado                          
               
         EXIT DISPLAY
         
      ON ACTION CANCEL
            INITIALIZE v_r_nss_inv TO NULL
            EXIT DISPLAY

   END DISPLAY
   
   CLOSE WINDOW w_consulta_expediente
END FUNCTION


#############################################################################
# Funcion           => fn_carga_docto - Carga arreglo de documentos del ca- #
#                      talogo y si ya tiene información capturada es agrega-#
#                      de acuerdo al modo de carga                          #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_modo - Modo de carga del arreglo de documentos 'A' #
#                      para un nuevo regsitro y 'M' para agregar informacion#
#                      previamente capturada                                #
#                      p_id_expediente - expediente del que se extraera in- #
#                      formacion previamente capturada                      #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
######################################################/#######################
FUNCTION fn_carga_docto(p_modo,p_id_expediente)
DEFINE p_modo          CHAR(1)
DEFINE p_id_expediente LIKE sep_docto_exp.id_expediente
DEFINE v_pos    INTEGER
DEFINE v_c_archivo   CHAR(40)

   CALL v_ar_docto_exp.clear()
   IF p_modo = 'A' THEN
      -- Recupera documentos del catálogo
      LET v_SqlQry = "SELECT 0, docto_cod, ind_requerido, docto_desc,",
                     " '', '', ''",
                     "  FROM prt_cat_docto",
                     " ORDER BY 3 DESC, 2 ASC"
   ELSE
      IF p_modo = 'M' THEN
         -- Recupera documentos del catalogo y agrega la informacion previamente capturada
         LET v_SqlQry = "SELECT id_prt_docto_exp, prt_cat_docto.docto_cod, ind_requerido, docto_desc, ",
                        "docto_nombre, docto_nota, docto_nombre",
                        "  FROM prt_cat_docto, OUTER prt_docto_exp",
                        " WHERE prt_cat_docto.docto_cod = prt_docto_exp.docto_cod",
                        "   AND prt_docto_exp.id_expediente = ",p_id_expediente,
                        " ORDER BY 3 DESC, 2 ASC"
      END IF
   END IF
   
   PREPARE EnuDoctoExp FROM v_SqlQry
   DECLARE CurDoctoExp CURSOR FOR EnuDoctoExp

   -- Carga del arreglo de documentos asociados
   LET v_pos = 1
   FOREACH CurDoctoExp INTO v_ar_docto_exp[v_pos].id_sep_docto_exp,
                            v_ar_docto_exp[v_pos].docto_cod,
                            v_ar_docto_exp[v_pos].ind_requerido,
                            v_ar_docto_exp[v_pos].docto_desc,
                            v_c_archivo,
                            v_ar_docto_exp[v_pos].docto_nota,
                            v_c_archivo
      LET v_ar_docto_exp[v_pos].file_upload = v_c_archivo
      LET v_ar_docto_exp[v_pos].file_upload2= v_c_archivo

      IF p_modo = 'M' THEN
         -- Generación de la liga para visualizacion de documentos asociados
         LET v_ar_docto_exp[v_pos].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[v_pos].file_upload CLIPPED,"','prtdocto')\" target='nueva'>",v_ar_docto_exp[v_pos].file_upload CLIPPED,"</a>"
      END IF
      LET v_pos = v_pos + 1   
   END FOREACH
   
   IF LENGTH(v_ar_docto_exp[v_ar_docto_exp.getLength()].docto_cod CLIPPED) = 0 THEN
      CALL v_ar_docto_exp.deleteElement(v_ar_docto_exp.getLength())
   END IF
END FUNCTION

#############################################################################
# Funcion           => init_combo_entidad_convenio                          #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
#############################################################################
FUNCTION init_combo_entidad_convenio()

   DEFINE v_id_cat_entidad     LIKE prt_cat_entidad.id_cat_entidad
   DEFINE v_desc_entidad       LIKE prt_cat_entidad.desc_entidad
   DEFINE desc_combo          CHAR(50)

   LET cb   = ui.combobox.forname("id_cat_entidad")
   CALL cb.clear()
   
   DECLARE cur_id_entidad CURSOR FOR
    SELECT tb.id_cat_entidad, tb.desc_entidad
      FROM prt_cat_entidad tb
     ORDER  BY 1
   
   FOREACH cur_id_entidad INTO v_id_cat_entidad, v_desc_entidad
       LET desc_combo= v_id_cat_entidad       USING "&&&", " ",
                       v_desc_entidad CLIPPED
       CALL cb.additem( v_id_cat_entidad, desc_combo )
   END FOREACH

END FUNCTION


#############################################################################
# Funcion           => fn_valida_guardado - Validaciones parciales al guardar
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_tpo_valida - tipo de validacion '0' para guardar y #
#                      '1' al aceptar el expediente                         #
# Salida:           => Verdadero si no hay errores, falso en caso contrario #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_valida_guardado(p_tpo_valida)

DEFINE p_tpo_valida   SMALLINT
   
   IF v_r_nss_inv.id_nss_expediente1 IS NULL  OR
      v_r_nss_inv.id_nss_expediente1 = 0      THEN
         CALL fn_mensaje("Aviso","Debe capturar un nss valido","info")
      RETURN FALSE
   END IF
DISPLAY v_r_nss_inv.id_nss_expediente1
   SELECT a.nss
     INTO   v_r_nss_inv.nss_trab1
     FROM   afi_derechohabiente a
    WHERE  a.id_derechohabiente = v_r_nss_inv.id_nss_expediente1

    IF STATUS = NOTFOUND THEN 
           CALL fn_mensaje("Aviso","Nss inexistente","info")
           RETURN FALSE          
    END IF

    IF LENGTH(v_r_nss_inv.id_cat_entidad) = 0 THEN 
           CALL fn_mensaje("Aviso","Entidad en Convenio es requerida","info")
           RETURN FALSE          
    END IF 

    
   RETURN TRUE
END FUNCTION

#############################################################################
# Funcion           => fn_valida_aceptar - incorpora validaciones parciales #
#                      adicionales a las validaciones finales               #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Verdadero si no hay errores, falso en caso contrario #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_valida_aceptar()
DEFINE v_cont    SMALLINT
DEFINE v_estado  SMALLINT
DEFINE v_status  SMALLINT,
       v_flujo_descr LIKE sep_cat_tipo_flujo.flujo_desc

   IF (v_r_nss_inv.aivs_sar92_solicitado + v_r_nss_inv.aivs_viv97_solicitado <= 0) THEN
      CALL fn_mensaje("Aviso","Capturar Aiv's a solicitar","info")
      RETURN FALSE
   END IF

   IF v_r_nss_inv.id_cat_entidad IS NULL THEN 
      CALL fn_mensaje("Aviso","Capturar Entidad con Convenio","info")
      RETURN FALSE
   END IF 

   IF (v_r_nss_inv.saldo_insoluto_credito_iss IS NULL OR 
       v_r_nss_inv.saldo_insoluto_credito_iss = 0) THEN
      CALL fn_mensaje("Aviso","Capturar Saldo insoluto del crédito","info")
      RETURN FALSE
   END IF


   LET v_r_nss_inv_id_nss_trab1 = null

    SELECT a.id_derechohabiente 
    INTO   v_r_nss_inv_id_nss_trab1
    FROM   afi_derechohabiente a
    WHERE  a.nss = v_r_nss_inv.nss_trab1              

    IF STATUS = NOTFOUND THEN 
       IF v_r_nss_inv.nss_trab1 IS NOT NULL THEN
          CALL fn_mensaje("Aviso","Nss Inexistente ","info")
          RETURN FALSE          
        END IF 
    END if
              
   -- Rutina para validar parcialmente el expediente y complementa esta validacion
   IF NOT fn_valida_guardado(1) THEN
      RETURN FALSE
   END IF
   
   -- Rutina para buscar y solicitar documentos con estado de requerido
   LET v_estado = 0
   
   FOR v_cont = 1 TO v_ar_docto_exp.getLength()
      IF v_ar_docto_exp[v_cont].ind_requerido = 1 AND 
         LENGTH(v_ar_docto_exp[v_cont].ruta_docto CLIPPED) = 0 THEN
         LET v_estado = v_cont
         EXIT FOR
      END IF
   END FOR
   
   IF v_estado <> 0 THEN
         CALL fn_mensaje("Aviso","El documento No. "||v_estado||" es requerido","info")
         RETURN FALSE
   END IF
  
   RETURN TRUE
END FUNCTION

#############################################################################
# Funcion           => fn_admon_archivo_sep - Mantenimiento a los archivos  #
#                      subidos al servidor de forma temporal                #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_archivo - Nombre del archivo a dar mantenimiento   #
#                      p_id_expediente - Clave de imagen para referencia    #
#                      p_tpo_ren - Tipo de mantenimiento:                   #
#                                  'A' renombrar archivo temporal en la alta#
#                                  'B' borrar archivo al cancelar o confir- #
#                                  mar su eliminación                       #
# Salida:           => v_archivo - Nombre del archivo o notificación de man-#
#                                  tenimiento:                              #
#                                  'A' Archivo renombrado de temporal por su#
#                                  nombre definitivo o leyenda NO_MODIFICADO#
#                                  'B' leyenda de ELIMINADO o NO_ELIMINADO  #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 04 Mayo  2012                                        #
#############################################################################
FUNCTION fn_admon_archivo_sep(p_archivo,p_id_expediente, p_id_sep_docto, p_tpo_ren)
   DEFINE p_archivo         STRING,
          p_id_expediente   LIKE sep_expediente.id_expediente,
          p_id_sep_docto    LIKE sep_docto_exp.id_sep_docto_exp,
          p_tpo_ren         CHAR(1),  -- Renombrar para Alta 'A' o modificación 'M'
          v_archivo         STRING,
          v_buf             base.StringBuffer
   DEFINE v_res             SMALLINT
   DEFINE v_ruta_docto      LIKE seg_modulo.ruta_docto
   DEFINE v_c_exp           CHAR(4)
   DEFINE v_c_docto         CHAR(4)
   DEFINE v_c_remplaza      CHAR(9)
   
   IF LENGTH(p_archivo CLIPPED) = 0 THEN
      DISPLAY "Archivo nulo, no procede el guardado ...."
      LET v_archivo = "No procesado"
      RETURN v_archivo
   END IF 
          
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'prt'
   
   
   LET v_buf = base.StringBuffer.create()
            
   IF p_tpo_ren = 'A' THEN
      -- Renombrar documentos del servidor
      CALL v_buf.append(p_archivo)
      LET v_c_exp   = p_id_expediente USING "&&&&"
      LET v_c_docto = p_id_sep_docto USING "&&&&"
      LET v_c_remplaza = v_c_exp,"-",v_c_docto
      CALL v_buf.replace("tmp", v_c_remplaza, 1)
      LET v_archivo = v_buf.toString()
      DISPLAY "Ruta destino - ",v_ruta_docto CLIPPED||v_archivo CLIPPED
      CALL os.Path.rename(v_ruta_docto CLIPPED||p_archivo CLIPPED, v_ruta_docto CLIPPED||v_archivo CLIPPED) RETURNING v_res
      IF NOT v_res THEN
         LET v_archivo = "NO MODIFICADO"
      END IF 
   ELSE     
      IF p_tpo_ren = 'B' THEN
         -- Eliminacion de documentos del servidor
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

#############################################################################
# Funcion           => fn_transfiere_archivo - Copiar archivos de un equipo #
#                      remoto al servidor                                   #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_r_imagen - Registro que identifica la iamgen       #
#                      v_operacion - Tipo operacion, inoperable actualmente #
#                      v_nombre_original - NOmbre original del archivo,     #
#                      actualmente inoperable                               #
# Salida:           => v_NomArcDepT - nombre del archivo almacenado         #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 04 Mayo  2012                                        #
#############################################################################
FUNCTION fn_transfiere_archivo(p_r_imagen, v_operacion, v_nombre_original)
   DEFINE p_r_imagen     RECORD
             id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
             docto_cod     LIKE sep_cat_docto.docto_cod,
             ind_requerido LIKE sep_cat_docto.ind_requerido,
             docto_desc    LIKE sep_cat_docto.docto_desc,
             file_upload   STRING,
             docto_nota    CHAR(200),
             file_upload2  CHAR(200),
             ruta_docto    CHAR(200)
          END RECORD,
          v_operacion       CHAR(1),
          v_nombre_original STRING
            
   DEFINE v_NomArcDep    VARCHAR(500)
   DEFINE v_NomArcDepT   STRING
   DEFINE v_ruta_docto   LIKE seg_modulo.ruta_docto
   DEFINE v_verifica_file STRING
            
   SELECT ruta_docto INTO v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = 'prt'
            
   LET v_verifica_file = v_ruta_docto CLIPPED||p_r_imagen.file_upload
   DISPLAY "Verifica existencia: ",v_verifica_file
   
   IF NOT os.Path.exists(v_verifica_file) THEN
      -- Transferir documentos al servidor
      DISPLAY "NomArc - ",p_r_imagen.file_upload CLIPPED
      CALL os.Path.basename(p_r_imagen.file_upload) RETURNING v_NomArcDep
      DISPLAY "os.Path.basename - ", v_NomArcDep
   
      SLEEP 5
      
      CALL fn_depura_archivo(v_NomArcDep) RETURNING v_NomArcDep
               
      LET v_NomArcDepT = "tmp_", v_NomArcDep

      DISPLAY "Origen : ",p_r_imagen.file_upload
      DISPLAY "Destino: ",v_ruta_docto CLIPPED, v_NomArcDepT
      CALL fgl_getfile(p_r_imagen.file_upload, v_ruta_docto CLIPPED||v_NomArcDepT)
   ELSE
      CALL os.Path.basename(p_r_imagen.file_upload) RETURNING v_NomArcDepT
   END IF
   
   RETURN v_NomArcDepT
            
END FUNCTION

#############################################################################
# Funcion           => fn_depura_archivo - Depurar nombre de archivo, en    #
#                      especifico los espacios del archivo                  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_NomArc - Archivo a subir del local al servidor     #
# Salida:           => p_NomArcDep - Archivo depurado con guiones en lugar  #
#                      de espacios                                          #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 04 Mayo  2012                                        #
#############################################################################
FUNCTION fn_depura_archivo(p_NomArc)
  DEFINE p_NomArc      VARCHAR(500)
  DEFINE p_NomArcDep   VARCHAR(500)
  DEFINE n INTEGER
            
  CALL fn_sustituye_cadena (p_NomArc," ","_") RETURNING p_NomArcDep
  CALL fn_sustituye_cadena (p_NomArcDep,"tmp_","temp_") RETURNING p_NomArcDep
            
  RETURN p_NomArcDep
END FUNCTION

#############################################################################
# Funcion           => fn_guarda_expediente - Rutina para guardar cualquier #
#                      cambio al expediente                                 #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_modo - Modo de guardado 'A' Nuevo, 'M' Modificacion#
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_guarda_expediente(p_modo)
DEFINE p_modo                 CHAR(1)
DEFINE v_r_sep_nss_expediente RECORD LIKE sep_nss_expediente.*
DEFINE v_r_sep_docto_exp      RECORD LIKE sep_docto_exp.*
DEFINE v_pos                  SMALLINT
DEFINE v_resultado            STRING
DEFINE v_c_archivo            VARCHAR(200)
DEFINE v_sp_estado            SMALLINT
DEFINE v_sp_diag              CHAR(3)
DEFINE v_sp_edo_des           SMALLINT
DEFINE v_senal                SMALLINT
DEFINE v_estado_op27          SMALLINT
DEFINE v_consulta             STRING
DEFINE v_edo_destino_op27     SMALLINT
DEFINE v_ind                  SMALLINT
DEFINE v_diag                 CHAR(3)
DEFINE v_senal_op27           SMALLINT
DEFINE v_codigo_rechazo       SMALLINT
DEFINE v_nulo                 SMALLINT
DEFINE v_proceso_cod          LIKE cat_proceso.proceso_cod

CONSTANT v_marca SMALLINT = 706 --portabilidad TSSVP Convenios
CONSTANT v_cero  SMALLINT = 0 

   #1.- === Actualización del estado del expediente ===
   -- Rutina para determinar el estado final del expediente de acuerdo a la señal y estado actual
   IF p_modo = 'G' THEN
      IF NOT (v_r_nss_inv.id_expediente > 0) THEN

         LET v_senal = 0
         EXECUTE prp_glo_maq_individual USING v_id_maquinaria ,
                                              v_r_nss_inv.id_expediente,
                                              v_senal ,
                                              p_usuario_cod
                                        INTO v_ind            ,
                                             v_diag           ,
                                             v_error_sql      ,
                                             v_error_isam     ,
                                             v_msg_sql        ,
                                             v_estado_destino  
         IF v_ind = 0 THEN
            LET v_r_nss_inv.estado = v_estado_destino
         ELSE
            CALL fn_mensaje("Aviso","Problema con la maquinaria de estados\ncon señal: "||v_senal||" y estado:"||v_r_nss_inv.estado,"about")
            RETURN FALSE
         END IF
      END IF
      LET v_r_nss_inv.estado = v_estado_destino
   ELSE
      LET v_senal = 10
         EXECUTE prp_glo_maq_individual USING v_id_maquinaria ,
                                              v_r_nss_inv.id_expediente,
                                              v_senal ,
                                              p_usuario_cod
                                        INTO v_ind            ,
                                             v_diag           ,
                                             v_error_sql      ,
                                             v_error_isam     ,
                                             v_msg_sql        ,
                                             v_estado_destino  
      IF v_ind = 0 THEN
         LET v_r_nss_inv.estado = v_estado_destino
      ELSE
         CALL fn_mensaje("Aviso","Problema con la maquinaria de estados\ncon señal: "||v_senal||" y estado:"||v_r_nss_inv.estado,"about")
         RETURN FALSE
      END IF
   END IF
   
   -- Verificación del expediente para actualizar datos o insertar datos del expediente
   IF v_r_nss_inv.id_expediente > 0 THEN
      -- actualiza expediente
      UPDATE prt_expediente_convenios_iss
         SET f_confirma                  = TODAY,
             saldo_insoluto_credito_iss = v_r_nss_inv.saldo_insoluto_credito_iss,
             aivs_sar92_solicitado = v_r_nss_inv.aivs_sar92_solicitado,
             pesos_sar92_solicitado = v_r_nss_inv.pesos_sar92_solicitado,
             aivs_viv97_solicitado = v_r_nss_inv.aivs_viv97_solicitado,
             pesos_viv97_solicitado = v_r_nss_inv.pesos_viv97_solicitado,
             id_cat_entidad         = v_r_nss_inv.id_cat_entidad,
             nss                    = v_r_nss_inv.nss_trab1
       WHERE id_prt_expediente_convenios_iss = v_r_nss_inv.id_expediente
   ELSE
   
       LET v_r_nss_inv.id_expediente = fn_genera_seq("seq_prt_expediente_convenios_iss")
       DISPLAY v_r_nss_inv.id_expediente  
       
      -- inserta expediente
      INSERT INTO prt_expediente_convenios_iss
                 (id_prt_expediente_convenios_iss ,
                  id_derechohabiente ,
                  nss , 
                  nombre,
                  saldo_insoluto_credito_iss ,
                  aivs_sar92_solicitado ,
                  pesos_sar92_solicitado ,
                  aivs_viv97_solicitado ,
                  pesos_viv97_solicitado,
                  estado ,
                  f_captura,
                  id_cat_entidad)
         VALUES  (v_r_nss_inv.id_expediente,
                  v_r_nss_inv.id_nss_expediente1,
                  v_r_nss_inv.nss_trab1,
                  v_r_nss_inv.nombre_trab,                  
                  v_r_nss_inv.saldo_insoluto_credito_iss,
                  v_r_nss_inv.aivs_sar92_solicitado,
                  v_r_nss_inv.pesos_sar92_solicitado,
                  v_r_nss_inv.aivs_viv97_solicitado,
                  v_r_nss_inv.pesos_viv97_solicitado,
                  v_sp_estado ,
                  TODAY,
                  v_r_nss_inv.id_cat_entidad) 
                  
         LET v_senal = 0
         EXECUTE prp_glo_maq_individual USING v_id_maquinaria ,
                                              v_r_nss_inv.id_expediente,
                                              v_senal ,
                                              p_usuario_cod
                                        INTO v_ind            ,
                                             v_diag           ,
                                             v_error_sql      ,
                                             v_error_isam     ,
                                             v_msg_sql        ,
                                             v_estado_destino  
DISPLAY "maq: ",v_id_maquinaria
DISPLAY "EXP: ",v_r_nss_inv.id_expediente
display "senal :",v_senal
DISPLAY "usuario: ",p_usuario_Cod
DISPLAY ""
DISPLAY "v_ind: ",v_ind
DISPLAY "v_diag: ",v_diag

                                             
   END IF

   # 2.1 === Marca cuenta al confirmar expediente ===
   # solo para el caso de "solo infonavit"
   IF(p_modo = 'A' ) THEN
      LET v_proceso_cod = 2201
      LET v_codigo_rechazo = 0
      INITIALIZE v_nulo TO NULL
      LET v_consulta = " EXECUTE FUNCTION safre_viv:fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
      PREPARE prp_marca_cuenta_separacion FROM v_consulta 
      EXECUTE prp_marca_cuenta_separacion USING v_r_nss_inv_id_nss_trab1,      -- id_derechohabiente
                                                v_marca,                       -- Marca de separación
                                                --v_r_sep_nss_expediente.id_nss_expediente, -- identificador de registro de sep_nss_expediente
                                                v_r_nss_inv.id_expediente, -- identificador de referencia al expediente
                                                v_r_nss_inv.id_expediente, -- folio --> id_expediete mismo que referencia
                                                v_cero,                        -- estado de marca
                                                v_cero,                        -- código de rechazo
                                                v_nulo,                        -- marca causa
                                                v_nulo,                        -- fecha causa
                                                p_usuario_cod,                 -- usuario
                                                v_proceso_cod                  -- proceso --> se utiliza el proceso de marca op 27
                                           INTO v_codigo_rechazo
      IF(v_codigo_rechazo <> 0)THEN
         CALL fn_mensaje("Aviso","Ocurrió un error al generar marca de cuenta, código:"||v_codigo_rechazo,"stop")
         RETURN FALSE
      END IF
   END IF 

   #3.- === Actualiza los documentos ===
   -- Rutina para renombrar documentos finales y ser vinculados, asi como depurar directorio de archivos desvinculados
   FOR v_pos = 1 TO v_ar_docto_exp.getLength()
      INITIALIZE v_r_sep_docto_exp TO NULL
      IF v_ar_docto_exp[v_pos].id_sep_docto_exp > 0 THEN
         IF LENGTH(v_ar_docto_exp[v_pos].file_upload CLIPPED) > 0 THEN
            --DISPLAY "file_upload <> file_upload2 - ",v_ar_docto_exp[v_pos].file_upload, "<>", v_ar_docto_exp[v_pos].file_upload2
            -- Limpia archivos antiguos
            IF LENGTH(v_ar_docto_exp[v_pos].file_upload2 CLIPPED) > 0 AND
               v_ar_docto_exp[v_pos].file_upload <> v_ar_docto_exp[v_pos].file_upload2 THEN
               CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload2, 0, 0, 'B') 
                  RETURNING v_resultado
            END IF
            LET v_c_archivo = v_ar_docto_exp[v_pos].file_upload
            IF v_c_archivo[1,3] = 'tmp' THEN
               CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload,v_r_nss_inv.id_expediente, v_ar_docto_exp[v_pos].id_sep_docto_exp, 'A') 
                  RETURNING v_ar_docto_exp[v_pos].file_upload
            END IF
            -- udpate 
            LET v_c_archivo = v_ar_docto_exp[v_pos].file_upload
            UPDATE prt_docto_exp
               SET docto_nombre = v_c_archivo,
                   docto_nota   = v_ar_docto_exp[v_pos].docto_nota
             WHERE id_prt_docto_exp = v_ar_docto_exp[v_pos].id_sep_docto_exp
         ELSE
            DELETE FROM prt_docto_exp
             WHERE id_prt_docto_exp = v_ar_docto_exp[v_pos].id_sep_docto_exp
         END IF
      ELSE
         IF LENGTH(v_ar_docto_exp[v_pos].file_upload CLIPPED) > 0 THEN
            -- insert
            LET v_r_sep_docto_exp.id_sep_docto_exp = fn_genera_seq("seq_prt_docto_exp")
            IF v_r_sep_docto_exp.id_sep_docto_exp = -1 THEN
      	       CALL fn_mensaje("Advertencia","Problemas con la secuencia del expediente","info")
      	       RETURN FALSE
            END IF
            LET v_r_sep_docto_exp.docto_cod     = v_ar_docto_exp[v_pos].docto_cod
            LET v_r_sep_docto_exp.id_expediente = v_r_nss_inv.id_expediente
            LET v_r_sep_docto_exp.docto_nombre  = v_ar_docto_exp[v_pos].file_upload
            LET v_r_sep_docto_exp.docto_nota    = v_ar_docto_exp[v_pos].docto_nota 
            CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload,v_r_nss_inv.id_expediente, v_r_sep_docto_exp.id_sep_docto_exp, 'A') 
               RETURNING v_r_sep_docto_exp.docto_nombre
DISPLAY "expediente docto id: ",v_r_sep_docto_exp.id_sep_docto_exp
DISPLAY "expediente docto id expediente: ",v_r_sep_docto_exp.id_expediente
            INSERT INTO prt_docto_exp
            VALUES (v_r_sep_docto_exp.*)
         END IF
      END IF
   END FOR
   
   RETURN TRUE   
   
END FUNCTION

FUNCTION fn_carga_nss(p_id_expediente)
DEFINE p_id_expediente   LIKE sep_nss_expediente.id_expediente
DEFINE v_r_nss_exp       RECORD LIKE prt_expediente_convenios_iss.*
DEFINE v_qrytxt          STRING
DEFINE v_pos             SMALLINT

   LET v_qrytxt = "SELECT * ",
                  "  FROM prt_expediente_convenios_iss ",
                  " WHERE id_prt_expediente_convenios_iss = ",p_id_expediente
   
   PREPARE EnuRecNSSExp FROM v_qrytxt
   DECLARE CurRecNSSExp CURSOR FOR EnuRecNSSExp
   
   -- Rutina para recuperar NSS's capturados por expediente
   LET v_pos = 1
   FOREACH CurRecNSSExp INTO v_r_nss_exp.*
            LET v_r_nss_inv.id_nss_expediente1         = v_r_nss_exp.id_derechohabiente
            LET v_r_nss_inv.nss_trab1                  = v_r_nss_exp.nss
            LET v_r_nss_inv.nombre_trab                = v_r_nss_exp.nombre
            LET v_r_nss_inv.saldo_insoluto_credito_iss = v_r_nss_exp.saldo_insoluto_credito_iss
            LET v_r_nss_inv.aivs_sar92_solicitado      = v_r_nss_exp.aivs_sar92_solicitado
            LET v_r_nss_inv.pesos_sar92_solicitado     = v_r_nss_exp.pesos_sar92_solicitado
            LET v_r_nss_inv.aivs_viv97_solicitado      = v_r_nss_exp.aivs_viv97_solicitado            
            LET v_r_nss_inv.pesos_viv97_solicitado     = v_r_nss_exp.pesos_viv97_solicitado
            LET v_r_nss_inv.estado                     = v_r_nss_exp.estado 
            LET v_r_nss_inv.f_captura                  = TODAY
            LET v_r_nss_inv.id_cat_entidad             = v_r_nss_exp.id_cat_entidad
   END FOREACH
DISPLAY v_r_nss_inv.id_nss_expediente1   

            CALL sp_obtiene_saldo(0,0,0)
   
END FUNCTION

FUNCTION sp_obtiene_saldo(p_tipo,p_subcuenta,p_aivs)

   DEFINE p_tipo SMALLINT
   DEFINE p_subcuenta SMALLINT   
   DEFINE p_aivs LIKE cta_movimiento.monto_acciones   

                SELECT a.precio_fondo
                INTO   v_precio_fondo
                FROM   glo_valor_fondo a
                WHERE  a.f_valuacion = TODAY
                AND    a.fondo = 11
                
                IF v_precio_fondo IS NULL THEN LET v_precio_fondo = 0 END IF
   IF p_tipo = 0 THEN
   
                SELECT SUM(a.monto_acciones)
                INTO   v_r_nss_inv.aivs_sar92
                FROM   cta_movimiento a
                WHERE  a.id_derechohabiente =  v_r_nss_inv.id_nss_expediente1
                AND    a.subcuenta = 8
                SELECT SUM(a.monto_acciones)
                INTO   v_r_nss_inv.aivs_viv97
                FROM   cta_movimiento a
                WHERE  a.id_derechohabiente =  v_r_nss_inv.id_nss_expediente1
                AND    a.subcuenta = 4
                

                LET v_r_nss_inv.pesos_sar92 = v_r_nss_inv.aivs_sar92 * v_precio_fondo
                LET v_r_nss_inv.pesos_viv97 = v_r_nss_inv.aivs_viv97 * v_precio_fondo
    ELSE 
         IF p_subcuenta = 8 THEN
                LET v_r_nss_inv.pesos_sar92_solicitado = p_aivs * v_precio_fondo
         ELSE                 
                LET v_r_nss_inv.pesos_viv97_solicitado = p_aivs * v_precio_fondo
         END IF                
    END IF    

END FUNCTION 

