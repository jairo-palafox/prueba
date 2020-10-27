--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/05/2012
--===============================================================

####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPF20                                        #
#Objetivo          =>Captura de expediente para separacion de cta  #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>04 Mayo 2012                                  #
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
DEFINE v_r_captura_exp  RECORD 
         id_expediente                   LIKE sep_expediente.id_expediente,
         flujo_cod                       LIKE sep_expediente.flujo_cod,
         canal_cod                       LIKE sep_expediente.canal_cod,
         nombre_reclamante               LIKE sep_expediente.nombre_reclamante,
         folio_procesar                  LIKE sep_expediente.folio_procesar,
         f_recepcion_infonavit           LIKE sep_expediente.f_recepcion_infonavit,
         f_restitucion_infonavit         LIKE sep_expediente.f_restitucion_infonavit,
         caso_adai                       LIKE sep_expediente.caso_adai,
         id_envio                        LIKE sep_expediente.id_envio,
         f_captura                       LIKE sep_expediente.f_captura,
         {docto_dictamen                  LIKE sep_expediente.docto_dictamen,
         docto_ajuste                    LIKE sep_expediente.docto_ajuste,
         docto_aviso_suspension          LIKE sep_expediente.docto_aviso_suspension,
         docto_baja_notificacion         LIKE sep_expediente.docto_baja_notificacion,
         docto_restitucion               LIKE sep_expediente.docto_restitucion,
         docto_restitucion_no_aplicados  LIKE sep_expediente.docto_restitucion_no_aplicados,
         ind_contacto                    LIKE sep_expediente.ind_contacto,
         ind_ajuste                      LIKE sep_expediente.ind_ajuste,
         ind_aviso_suspension            LIKE sep_expediente.ind_aviso_suspension,
         ind_baja_notificacion           LIKE sep_expediente.ind_baja_notificacion,
         ind_restitucion                 LIKE sep_expediente.ind_restitucion,
         ind_restitucion_no_aplicados    LIKE sep_expediente.ind_restitucion_no_aplicados,}
         estado                          LIKE sep_expediente.estado
       END RECORD
DEFINE v_r_nss_inv      RECORD
          id_nss_expediente1 LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab1          LIKE sep_nss_expediente.nss,
          tipo_trab1         SMALLINT,
          id_reclamante1     SMALLINT,
          nombre_invadido    LIKE sep_nss_expediente.nombre,
          telefono_invadido  LIKE sep_nss_expediente.tel_contacto1,
          telefono2_invadido LIKE sep_nss_expediente.tel_contacto2,
          correo_e_invadido  LIKE sep_nss_expediente.correo_e,
          celular_invadido   LIKE sep_nss_expediente.tel_celular,
          
          credito_invadido   LIKE sep_nss_expediente.num_credito,
          tipo_credito_invadido LIKE sep_nss_expediente.tpo_credito,
          sin_credito        BOOLEAN,
          
          id_nss_expediente2 LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab2	         LIKE sep_nss_expediente.nss,
          tipo_trab2         SMALLINT,
          id_reclamante2     SMALLINT,
          nombre_asociado    LIKE sep_nss_expediente.nombre,
          telefono_asociado  LIKE sep_nss_expediente.tel_contacto1,
          telefono2_asociado LIKE sep_nss_expediente.tel_contacto2,
          correo_e_asociado  LIKE sep_nss_expediente.correo_e,
          celular_asociado   LIKE sep_nss_expediente.tel_celular
          {id_nss_expediente3  LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab3     LIKE sep_nss_expediente.nss,
          tipo_trab3    SMALLINT,
          id_nss_expediente4  LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab4     LIKE sep_nss_expediente.nss,
          tipo_trab4    SMALLINT,
          id_nss_expediente5  LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab5     LIKE sep_nss_expediente.nss,
          tipo_trab5    SMALLINT}
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

MAIN

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   CALL STARTLOG(p_usuario_cod CLIPPED||".SEPL10.log")
   -- Rutinas para obtener el consecutivo de cada tabla en cuestion
   -- AHM 20120601 Cambia por secuencia PREPARE EnuMaxExpediente FROM "SELECT NVL(MAX(id_expediente)+1,1) FROM sep_expediente"
   PREPARE EnuMaxNss FROM "SELECT NVL(MAX(id_nss_expediente)+1,1) FROM sep_nss_expediente"
   PREPARE EnuMaxDocto FROM "SELECT NVL(MAX(id_sep_docto_exp)+1,1) FROM sep_docto_exp"

   -- Rutina que define el estado final despues de cualquier accion
   -- AHM 20120601 Cambio la maquinaria de estados LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_maq_sep_expediente(?,?)"
   --LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_maquinaria(?,?,?)"
   LET v_SqlQry = "EXECUTE FUNCTION safre_viv:fn_maquinaria_individual(?,?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la maquinaria de estados
   PREPARE prp_maq_edo_sep FROM v_SqlQry

   --CALL ui.Interface.loadStyles("sepstyle")
   
   -- Menu principal de la captura de expedientes
   OPEN WINDOW w_captura_expediente WITH FORM "SEPF201"
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
         IF LENGTH(v_r_captura_exp.f_captura CLIPPED) > 0 THEN
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
       v_archivo       STRING,
       v_flujo_descr   LIKE sep_cat_tipo_flujo.flujo_desc

   LET v_r_nss_inv.id_reclamante1 = 0
   LET v_r_nss_inv.id_reclamante2 = 0
   
   DIALOG ATTRIBUTES (UNBUFFERED)
   
   -- Captura del encabezado del expediente
   INPUT BY NAME v_r_captura_exp.flujo_cod,
                 --v_r_captura_exp.id_expediente,
                 v_r_captura_exp.id_envio,
                 v_r_captura_exp.folio_procesar,
                 v_r_captura_exp.f_recepcion_infonavit,
                 --v_r_captura_exp.f_captura,
                 v_r_captura_exp.canal_cod,
                 v_r_captura_exp.nombre_reclamante 
      ATTRIBUTES (WITHOUT DEFAULTS = TRUE)
      BEFORE INPUT
         # recupera la descripcion del estado del expediente
         LET v_estado_desc = ''
         SELECT descripcion
           INTO v_estado_desc
           FROM sep_estado_expediente
          WHERE estado = v_r_captura_exp.estado

         DISPLAY v_estado_desc TO edi_exp_estado 
         
         LET v_forma = v_ventana.getForm()
         -- Inicializa campos según tipo de captura y variables generales
         CALL v_forma.setFieldHidden( "folio_procesar", (v_r_captura_exp.flujo_cod <> 1) )
         # para el caso de flujo_cod = 3(solo infonavit) se oculta -id envio- y -folio procesar-
         CALL v_forma.setFieldHidden( "id_envio", (v_r_captura_exp.flujo_cod = 3) )
         # inicializa el check box a no seleccionado
         LET v_r_nss_inv.sin_credito = FALSE
         
         
         IF v_modo = 'A' THEN
            LET v_r_captura_exp.f_recepcion_infonavit = NULL
         END IF
         LET v_r_captura_exp.f_restitucion_infonavit = NULL
         LET v_r_captura_exp.f_captura = TODAY
         DISPLAY BY NAME v_r_captura_exp.f_captura,
                         v_r_captura_exp.f_recepcion_infonavit,
                         v_r_captura_exp.id_expediente
         
      ON CHANGE flujo_cod
         --CALL GET_FLDBUF( customer.* ) RETURNING rec_customer.*
         --CALL DIALOG.setFieldActive( "folio_procesar", (v_r_captura_exp.flujo_cod = 1) )
         CALL v_forma.setFieldHidden( "folio_procesar", (v_r_captura_exp.flujo_cod <> 1) )
         # para el caso de flujo_cod = 3(solo infonavit) se oculta -id envio- y -folio procesar-
         CALL v_forma.setFieldHidden( "id_envio", (v_r_captura_exp.flujo_cod = 3) )
         CALL v_forma.setFieldHidden( "sin_credito", (v_r_captura_exp.flujo_cod <> 3) )
         IF(v_r_captura_exp.flujo_cod <> 3)THEN
            LET v_r_nss_inv.sin_credito = FALSE
            CALL DIALOG.setFieldActive( "credito_invadido", 1)
            CALL DIALOG.setFieldActive( "tipo_credito_invadido",1)
            INITIALIZE v_r_nss_inv.credito_invadido,v_r_nss_inv.tipo_credito_invadido TO NULL
         END IF
         
   END INPUT
   
   -- Captura de los NSS del expediente a separar
   INPUT BY NAME v_r_nss_inv.*
   
      BEFORE INPUT
         
         CALL v_forma.setFieldHidden( "sin_credito", (v_r_captura_exp.flujo_cod <> 3) )
         
         

      ON CHANGE sin_credito
         
         IF(v_r_nss_inv.sin_credito)THEN
            CALL DIALOG.setFieldActive( "credito_invadido", 0)
            CALL DIALOG.setFieldActive( "tipo_credito_invadido",0)
            
            --CALL v_forma.setFieldHidden( "credito_invadido", 1)
            --CALL v_forma.setFieldHidden( "tipo_credito_invadido", 1)
            INITIALIZE v_r_nss_inv.credito_invadido,v_r_nss_inv.tipo_credito_invadido TO NULL
         ELSE
            CALL DIALOG.setFieldActive( "credito_invadido", 1)
            CALL DIALOG.setFieldActive( "tipo_credito_invadido",1)
            
            --CALL v_forma.setFieldHidden( "credito_invadido", 0)
            --CALL v_forma.setFieldHidden( "tipo_credito_invadido", 0)
         END IF
   	  	
   	  ON CHANGE id_reclamante1
   	     IF v_r_nss_inv.id_reclamante1 = 1 THEN
   	        IF v_r_nss_inv.id_reclamante2 = 1 THEN
   	        	  LET v_r_nss_inv.id_reclamante2 = 0
   	        	  DISPLAY BY NAME v_r_nss_inv.id_reclamante2
   	        END IF
   	     END IF
      
   	  ON CHANGE id_reclamante2
   	     IF v_r_nss_inv.id_reclamante2 = 1 THEN
   	        IF v_r_nss_inv.id_reclamante1 = 1 THEN
   	        	  LET v_r_nss_inv.id_reclamante1 = 0
   	        	  DISPLAY BY NAME v_r_nss_inv.id_reclamante1
   	        END IF
   	     END IF

      AFTER FIELD nss_trab1
         # para el caso de flujo_cod = 3 (solo infonavit) se valida que las primeras dos posiciones del nss sean 77
         IF(v_r_captura_exp.flujo_cod = 3 AND v_r_nss_inv.nss_trab1[1,2] <> "77")THEN
            INITIALIZE v_flujo_descr TO NULL
            SELECT flujo_desc
              INTO v_flujo_descr
              FROM sep_cat_tipo_flujo
             WHERE flujo_cod = v_r_captura_exp.flujo_cod

             
            INITIALIZE v_r_nss_inv.nss_trab1, v_r_nss_inv.nombre_invadido TO NULL
            CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
            NEXT FIELD nss_trab1
         END IF

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
                 SELECT TRIM(a.ap_paterno_af)||" "||
                        TRIM(a.ap_materno_af)||" "||
                        TRIM(a.nombre_af)
                 INTO v_r_nss_inv.nombre_invadido 
                 FROM  afi_derechohabiente a
                 WHERE a.id_derechohabiente = v_r_nss_inv_id_nss_trab1 
                 
                 LET v_txt =
                 " SELECT UNIQUE a.id_cre_acreditado, a.num_credito  , ",
                        " CASE WHEN c.tpo_originacion = 1 THEN 15 ",
                          "    WHEN c.tpo_originacion = 2 THEN 19 ",
                          "    WHEN c.tpo_originacion = 3 THEN 43 ",
                          "    ELSE 15 ",
                        " END CASE ",
                   --FROM cta_credito          a , 
                  "   FROM cre_acreditado       a , ",
                  "        cat_tipo_credito     b , ",
                  "        cat_cre_originacion  c   ",
                  "  WHERE a.tpo_credito     = b.tpo_credito     ",
                  "    AND b.tpo_originacion = c.tpo_originacion ",
                  "    AND a.id_derechohabiente = ? " ,
                  "    AND a.num_credito <> 0 ",
                  "    ORDER BY 1 "

                  PREPARE prp_busca_cred FROM v_txt
                  DECLARE cur_busca_cred CURSOR FOR prp_busca_cred
                  
                  FOREACH cur_busca_cred USING v_r_nss_inv_id_nss_trab1 
                                          INTO v_r_id_cre_acreditado, 
                                               v_r_nss_inv.credito_invadido,
                                               v_r_nss_inv.tipo_credito_invadido                   
                   EXIT FOREACH                                              
                  END FOREACH                                              
                 
              END IF
              
      AFTER FIELD nss_trab2
         # para el caso de flujo_cod = 3 (solo infonavit) se valida que las primeras dos posiciones del nss sean 77
         IF(v_r_captura_exp.flujo_cod = 3 AND v_r_nss_inv.nss_trab2[1,2] <> "77")THEN
            INITIALIZE v_flujo_descr TO NULL
            SELECT flujo_desc
              INTO v_flujo_descr
              FROM sep_cat_tipo_flujo
             WHERE flujo_cod = v_r_captura_exp.flujo_cod

             
            INITIALIZE v_r_nss_inv.nss_trab2, v_r_nss_inv.nombre_asociado TO NULL
            CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
            NEXT FIELD nss_trab2
         END IF
              
      ON CHANGE nss_trab2
              LET v_r_nss_inv_id_nss_trab2 = null      
              
              SELECT a.id_derechohabiente 
              INTO   v_r_nss_inv_id_nss_trab2
              FROM   afi_derechohabiente a
              WHERE  a.nss = v_r_nss_inv.nss_trab2             
              
              IF STATUS = NOTFOUND THEN 
                 IF v_r_nss_inv.nss_trab2 IS NOT NULL THEN
                    CALL fn_mensaje("Aviso","Nss no valido","info")
                    NEXT FIELD nss_trab2
                 END IF 
              ELSE 
                 SELECT TRIM(a.ap_paterno_af)||" "||
                        TRIM(a.ap_materno_af)||" "||
                        TRIM(a.nombre_af)
                 INTO v_r_nss_inv.nombre_asociado
                 FROM  afi_derechohabiente a
                 WHERE a.id_derechohabiente = v_r_nss_inv_id_nss_trab2
              END IF
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
         IF v_r_captura_exp.estado = 22 THEN
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
               LET v_ar_docto_exp[ARR_CURR()].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"','sepdocto')\" target='nueva'>",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"</a>"
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
            LET v_ar_docto_exp[ARR_CURR()].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"','sepdocto')\" target='nueva'>",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"</a>"
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
              FROM sep_docto_exp
             WHERE id_expediente = v_r_captura_exp.id_expediente
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
      IF v_r_captura_exp.estado = 22 THEN
         CALL DIALOG.setActionHidden( "guardar", 1 )
         CALL DIALOG.setActionHidden( "aceptar", 1 )
      ELSE
         CALL DIALOG.setActionHidden( "guardar", 0 )
         CALL DIALOG.setActionHidden( "aceptar", 0 )
      END IF
      
      -- Inicialización general de las pantallas de captura
      CALL fn_carga_docto(v_modo,v_r_captura_exp.id_expediente)
      CALL init_combo_tipo_flujo()
      CALL init_combo_origen_exp()

      IF v_modo = 'M' THEN
         CALL fn_carga_nss(v_r_captura_exp.id_expediente)
         DISPLAY BY NAME v_r_nss_inv.*
      END IF 

      IF v_r_nss_inv.id_reclamante1 IS NULL THEN
         LET v_r_nss_inv.id_reclamante1 = 0
      END IF
      
      IF v_r_nss_inv.id_reclamante2 IS NULL THEN
         LET v_r_nss_inv.id_reclamante2 = 0
      END IF
   
   -- Rutina para almacenar expediente sin finalizarlo
   ON ACTION guardar
      -- Validacion parcial de la información del expediente
      IF fn_valida_guardado(0) THEN
         -- Solicitud de confirnación de operacion
         IF fn_ventana_confirma("Aviso","¿Guardar Captura?","question") = 1 THEN
            -- Rutina para gurdar expediente sin finalizar
            IF fn_guarda_expediente('G') THEN
               INITIALIZE v_r_captura_exp TO NULL
               CALL v_ar_docto_exp.clear()
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
               INITIALIZE v_r_captura_exp TO NULL
               CALL v_ar_docto_exp.clear()
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
         DELETE FROM sep_docto_exp
         WHERE id_expediente = v_r_captura_exp.id_expediente
         
         -- Eliminación de nss's de la base de datos
         DELETE FROM sep_nss_expediente 
         WHERE id_expediente = v_r_captura_exp.id_expediente

         -- Eliminación del expediente de la base de datos
         DELETE FROM sep_expediente 
         WHERE id_expediente = v_r_captura_exp.id_expediente

         INITIALIZE v_r_captura_exp TO NULL
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

         INITIALIZE v_r_captura_exp TO NULL
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
          id_expediente                   LIKE sep_expediente.id_expediente                 ,
          flujo_cod                       LIKE sep_expediente.flujo_cod                     ,
          f_captura                       LIKE sep_expediente.f_captura                     ,
          canal_cod                       LIKE sep_expediente.canal_cod                     ,
	      nss                             LIKE sep_nss_expediente.nss                       ,
          nombre_reclamante               LIKE sep_expediente.nombre_reclamante             ,
          folio_procesar                  LIKE sep_expediente.folio_procesar                ,
          f_recepcion_infonavit           LIKE sep_expediente.f_recepcion_infonavit         ,
          f_restitucion_infonavit         LIKE sep_expediente.f_restitucion_infonavit       ,
          caso_adai                       LIKE sep_expediente.caso_adai                     ,
          id_envio                        LIKE sep_expediente.id_envio                      ,
          {docto_dictamen                  LIKE sep_expediente.docto_dictamen                ,
          docto_ajuste                    LIKE sep_expediente.docto_ajuste                  ,
          docto_aviso_suspension          LIKE sep_expediente.docto_aviso_suspension        ,
          docto_baja_notificacion         LIKE sep_expediente.docto_baja_notificacion       ,
          docto_restitucion               LIKE sep_expediente.docto_restitucion             ,
          docto_restitucion_no_aplicados  LIKE sep_expediente.docto_restitucion_no_aplicados,
          ind_contacto                    LIKE sep_expediente.ind_contacto                  ,
          ind_ajuste                      LIKE sep_expediente.ind_ajuste                    ,
          ind_aviso_suspension            LIKE sep_expediente.ind_aviso_suspension          ,
          ind_baja_notificacion           LIKE sep_expediente.ind_baja_notificacion         ,
          ind_restitucion                 LIKE sep_expediente.ind_restitucion               ,
          ind_restitucion_no_aplicados    LIKE sep_expediente.ind_restitucion_no_aplicados  ,}
          estado                          LIKE sep_expediente.estado                        
		END RECORD
DEFINE v_qrytxt        STRING
DEFINE v_pos           SMALLINT

   OPEN WINDOW w_consulta_expediente WITH FORM "SEPF202"
   
   -- Seleccion de expedientes en estado de captura
   LET v_qrytxt = "SELECT a.id_expediente,",
                  "       a.flujo_cod, ",
                  "       a.f_captura, ",
                  "       a.canal_cod, ",
                  "       b.nss , ",
                  "       a.nombre_reclamante, ",
                  "       a.folio_procesar, ",
                  "       a.f_recepcion_infonavit, ",
                  "       a.f_restitucion_infonavit, ",
                  "       a.caso_adai, ",
                  "       a.id_envio, ",
                  "       a.estado ",
                  "  FROM sep_expediente a, outer sep_nss_expediente b",
                  -- " WHERE estado = 5" -- AHM Cambio funcionalidad 201200625 
                  " WHERE a.estado in (5,15,20,25)",
                  "   AND a.id_expediente = b.id_expediente",
                  "   AND b.tipo_nss = 1"--Acreditado
   
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
            INITIALIZE v_r_captura_exp TO NULL
            EXIT DISPLAY
         END IF
            
      ON ACTION seleccionar
         --LET v_r_captura_exp.* = v_ar_expediente[ARR_CURR()].*
         LET v_r_captura_exp.id_expediente                   = v_ar_expediente[ARR_CURR()].id_expediente                   
         LET v_r_captura_exp.flujo_cod                       = v_ar_expediente[ARR_CURR()].flujo_cod                       
         LET v_r_captura_exp.canal_cod                       = v_ar_expediente[ARR_CURR()].canal_cod                       
         LET v_r_captura_exp.nombre_reclamante               = v_ar_expediente[ARR_CURR()].nombre_reclamante               
         LET v_r_captura_exp.folio_procesar                  = v_ar_expediente[ARR_CURR()].folio_procesar                  
         LET v_r_captura_exp.f_recepcion_infonavit           = v_ar_expediente[ARR_CURR()].f_recepcion_infonavit           
         LET v_r_captura_exp.f_restitucion_infonavit         = v_ar_expediente[ARR_CURR()].f_restitucion_infonavit         
         LET v_r_captura_exp.caso_adai                       = v_ar_expediente[ARR_CURR()].caso_adai                       
         LET v_r_captura_exp.id_envio                        = v_ar_expediente[ARR_CURR()].id_envio                        
         LET v_r_captura_exp.f_captura                       = v_ar_expediente[ARR_CURR()].f_captura                       
         {LET v_r_captura_exp.docto_dictamen                  = v_ar_expediente[ARR_CURR()].docto_dictamen                  
         LET v_r_captura_exp.docto_ajuste                    = v_ar_expediente[ARR_CURR()].docto_ajuste                    
         LET v_r_captura_exp.docto_aviso_suspension          = v_ar_expediente[ARR_CURR()].docto_aviso_suspension          
         LET v_r_captura_exp.docto_baja_notificacion         = v_ar_expediente[ARR_CURR()].docto_baja_notificacion         
         LET v_r_captura_exp.docto_restitucion               = v_ar_expediente[ARR_CURR()].docto_restitucion               
         LET v_r_captura_exp.docto_restitucion_no_aplicados  = v_ar_expediente[ARR_CURR()].docto_restitucion_no_aplicados  
         LET v_r_captura_exp.ind_contacto                    = v_ar_expediente[ARR_CURR()].ind_contacto                    
         LET v_r_captura_exp.ind_ajuste                      = v_ar_expediente[ARR_CURR()].ind_ajuste                      
         LET v_r_captura_exp.ind_aviso_suspension            = v_ar_expediente[ARR_CURR()].ind_aviso_suspension            
         LET v_r_captura_exp.ind_baja_notificacion           = v_ar_expediente[ARR_CURR()].ind_baja_notificacion           
         LET v_r_captura_exp.ind_restitucion                 = v_ar_expediente[ARR_CURR()].ind_restitucion                 
         LET v_r_captura_exp.ind_restitucion_no_aplicados    = v_ar_expediente[ARR_CURR()].ind_restitucion_no_aplicados}    
         LET v_r_captura_exp.estado                          = v_ar_expediente[ARR_CURR()].estado                          
         EXIT DISPLAY
         
      ON ACTION CANCEL
            INITIALIZE v_r_captura_exp TO NULL
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
                     "  FROM sep_cat_docto",
                     " ORDER BY 3 DESC, 2 ASC"
   ELSE
      IF p_modo = 'M' THEN
         -- Recupera documentos del catalogo y agrega la informacion previamente capturada
         LET v_SqlQry = "SELECT id_sep_docto_exp, sep_cat_docto.docto_cod, ind_requerido, docto_desc, ",
                        "docto_nombre, docto_nota, docto_nombre",
                        "  FROM sep_cat_docto, OUTER sep_docto_exp",
                        " WHERE sep_cat_docto.docto_cod = sep_docto_exp.docto_cod",
                        "   AND sep_docto_exp.id_expediente = ",p_id_expediente,
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
         LET v_ar_docto_exp[v_pos].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[v_pos].file_upload CLIPPED,"','sepdocto')\" target='nueva'>",v_ar_docto_exp[v_pos].file_upload CLIPPED,"</a>"
      END IF
      LET v_pos = v_pos + 1   
   END FOREACH
   
   IF LENGTH(v_ar_docto_exp[v_ar_docto_exp.getLength()].docto_cod CLIPPED) = 0 THEN
      CALL v_ar_docto_exp.deleteElement(v_ar_docto_exp.getLength())
   END IF
END FUNCTION

#############################################################################
# Funcion           => init_combo_tipo_flujo - Inicializa el combo tpo flujo#
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 09 Mayo    2012                                      #
#############################################################################
FUNCTION init_combo_tipo_flujo()

   DEFINE v_flujo_cod        LIKE sep_cat_tipo_flujo.flujo_cod
   DEFINE v_flujo_desc       LIKE sep_cat_tipo_flujo.flujo_desc
   DEFINE desc_combo        CHAR(50)

   LET cb   = ui.combobox.forname("flujo_cod")
   CALL cb.clear()
   
   DECLARE cur_tipo_flujo CURSOR FOR
    SELECT tb.flujo_cod, tb.flujo_desc
      FROM sep_cat_tipo_flujo tb
     ORDER  BY 1
   
   FOREACH cur_tipo_flujo INTO v_flujo_cod, v_flujo_desc
       LET desc_combo= v_flujo_cod       USING "&&&", " ",
                       v_flujo_desc CLIPPED
       CALL cb.additem( v_flujo_cod, desc_combo )
   END FOREACH

END FUNCTION

#############################################################################
# Funcion           => init_combo_origen_exp - Inicializa el combo orig exp #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 09 Mayo    2012                                      #
#############################################################################
FUNCTION init_combo_origen_exp()

   DEFINE v_canal_cod        LIKE sep_cat_canal_recepcion_exp.canal_cod 
   DEFINE v_canal_desc       LIKE sep_cat_canal_recepcion_exp.canal_desc
   DEFINE desc_combo         CHAR(50)

   LET cb   = ui.combobox.forname("canal_cod")
   CALL cb.clear()
   
   DECLARE cur_origen_exp CURSOR FOR
    SELECT tb.canal_cod, tb.canal_desc
      FROM sep_cat_canal_recepcion_exp tb
     ORDER  BY 1
   
   FOREACH cur_origen_exp INTO v_canal_cod, v_canal_desc
       LET desc_combo= v_canal_cod       USING "&&&", " ",
                       v_canal_desc CLIPPED
       CALL cb.additem( v_canal_cod, desc_combo )
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
DEFINE v_estado_act0  SMALLINT
DEFINE v_estado_act1  SMALLINT
DEFINE v_estado_act2  SMALLINT
DEFINE v_estado_act3  SMALLINT
DEFINE v_estado_act4  SMALLINT
DEFINE v_cont         SMALLINT,
       v_flujo_descr  LIKE sep_cat_tipo_flujo.flujo_desc

   LET v_cont = 1
   LET v_estado_act0 = 1
   LET v_r_nss_inv_id_nss_trab1 = null
   LET v_r_nss_inv_id_nss_trab2 = NULL

   # para el caso de flujo_cod = 3 (solo infonavit) se valida que las primeras dos posiciones del nss sean 77
   IF(v_r_captura_exp.flujo_cod = 3)THEN
      IF(v_r_nss_inv.nss_trab1[1,2] <> "77")THEN
         INITIALIZE v_flujo_descr TO NULL
         SELECT flujo_desc
           INTO v_flujo_descr
           FROM sep_cat_tipo_flujo
          WHERE flujo_cod = v_r_captura_exp.flujo_cod
             
         INITIALIZE v_r_nss_inv.nss_trab1,v_r_nss_inv.nombre_invadido TO NULL
         CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
         RETURN FALSE
      END IF
      IF(v_r_nss_inv.nss_trab2[1,2] <> "77")THEN
         INITIALIZE v_flujo_descr TO NULL
         SELECT flujo_desc
           INTO v_flujo_descr
           FROM sep_cat_tipo_flujo
          WHERE flujo_cod = v_r_captura_exp.flujo_cod
             
         INITIALIZE v_r_nss_inv.nss_trab2,v_r_nss_inv.nombre_asociado TO NULL
         CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
         RETURN FALSE
      END IF
   END IF

   SELECT a.id_derechohabiente 
     INTO   v_r_nss_inv_id_nss_trab1
     FROM   afi_derechohabiente a
    WHERE  a.nss = v_r_nss_inv.nss_trab1              

    IF STATUS = NOTFOUND THEN 
       IF v_r_nss_inv.nss_trab1 IS NOT NULL THEN
           CALL fn_mensaje("Aviso","Nss Invadido no valido","info")
           RETURN FALSE          
        END IF 
    END if
              
    SELECT a.id_derechohabiente 
    INTO   v_r_nss_inv_id_nss_trab2
    FROM   afi_derechohabiente a
    WHERE  a.nss = v_r_nss_inv.nss_trab2             
              
    IF STATUS = NOTFOUND THEN 
       IF v_r_nss_inv.nss_trab2 IS NOT NULL THEN
          CALL fn_mensaje("Aviso","Nss Asociado no valido","info")
          RETURN FALSE          
       END IF 
    END IF
    
   IF v_r_nss_inv_id_nss_trab1 IS NULL  OR
      v_r_nss_inv_id_nss_trab1 = 0      THEN
      IF p_tpo_valida = 0 THEN
         -- Señal de guardado sin finalizar
         CALL fn_mensaje("Aviso","Debe captura al menos un nss en el expediente","info")
      ELSE
         -- Señal de aceptar y finalizar
         CALL fn_mensaje("Aviso","Debe captura al menos dos nss en el expediente","info")
      END IF
      RETURN FALSE
   END IF

   IF p_tpo_valida = 1 THEN
      -- Señal de aceptar y finalizar
--      IF LENGTH(v_r_nss_inv.nss_trab2 CLIPPED) = 0 THEN
   IF v_r_nss_inv_id_nss_trab2 IS NULL OR
      v_r_nss_inv_id_nss_trab2 = 0      THEN
         CALL fn_mensaje("Aviso","Debe captura al menos dos nss en el expediente","info")
         RETURN FALSE
      END IF
      LET v_estado_act1 = 1
   ELSE
      IF LENGTH(v_r_nss_inv.nss_trab2 CLIPPED) = 0 THEN
         LET v_estado_act1 = 0
      ELSE
         LET v_estado_act1 = 1
      END IF
   END IF


   -- AHM 2012 mayo 30
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab3 CLIPPED) = 0 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act2 = 0
   -- AHM Solicitud de inhabilitarlo ELSE
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act2 = 1
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab4 CLIPPED) = 0 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act3 = 0
   -- AHM Solicitud de inhabilitarlo ELSE
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act3 = 1
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab5 CLIPPED) = 0 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act4 = 0
   -- AHM Solicitud de inhabilitarlo ELSE
   -- AHM Solicitud de inhabilitarlo    LET v_estado_act4 = 1
   -- AHM Solicitud de inhabilitarlo END IF
   IF v_estado_act0 <> v_estado_act1 THEN
      LET v_cont = v_cont + 1
   END IF
   IF v_estado_act1 <> v_estado_act2 THEN
      LET v_cont = v_cont + 1
   END IF
   IF v_estado_act2 <> v_estado_act3 THEN
      LET v_cont = v_cont + 1
   END IF
   IF v_estado_act3 <> v_estado_act4 THEN
      LET v_cont = v_cont + 1
   END IF
   IF v_cont > 2 THEN
      CALL fn_mensaje("Aviso","Debe captura las claves nss del expediente\nde forma consecutiva","info")
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

   IF LENGTH(v_r_captura_exp.flujo_cod             CLIPPED) = 0 THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: tipo de flujo","info")
      RETURN FALSE
   END IF
   IF LENGTH(v_r_captura_exp.folio_procesar        CLIPPED) = 0 AND 
      v_r_captura_exp.flujo_cod = 1 THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: folio","info")
      RETURN FALSE
   END IF
   IF(LENGTH(v_r_captura_exp.id_envio              CLIPPED) = 0 AND 
      v_r_captura_exp.flujo_cod <> 3)THEN  # flujo_cod <> 3 --> diferente de solo infonavit
      CALL fn_mensaje("Aviso","Es requerido el campo: Id. envío","info")
      RETURN FALSE
   END IF
   IF LENGTH(v_r_captura_exp.f_recepcion_infonavit CLIPPED) = 0 THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: Fecha de recepción ","info")
      RETURN FALSE
   END IF
   IF LENGTH(v_r_captura_exp.canal_cod             CLIPPED) = 0 THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: Origen expediente","info")
      RETURN FALSE
   END IF
   IF LENGTH(v_r_captura_exp.nombre_reclamante     CLIPPED) = 0 THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: Nombre del reclamante ","info")
      RETURN FALSE
   END IF
   IF LENGTH(v_r_nss_inv.nss_trab1 CLIPPED) > 0 THEN
      IF NOT (v_r_nss_inv.tipo_trab1 = 1 OR v_r_nss_inv.tipo_trab1 = 2) OR 
         v_r_nss_inv.tipo_trab1 IS NULL THEN 
         CALL fn_mensaje("Aviso","Es requerido el campo: Tipo de trabajador 1","info")
         RETURN FALSE
      END IF
   END IF

   # para el caso de flujo_cod = 3 (solo infonavit) se valida que las primeras dos posiciones del nss sean 77
   IF(v_r_captura_exp.flujo_cod = 3)THEN
      IF(v_r_nss_inv.nss_trab1[1,2] <> "77")THEN
         INITIALIZE v_flujo_descr TO NULL
         SELECT flujo_desc
           INTO v_flujo_descr
           FROM sep_cat_tipo_flujo
          WHERE flujo_cod = v_r_captura_exp.flujo_cod
             
         INITIALIZE v_r_nss_inv.nss_trab1 TO NULL
         CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
         RETURN FALSE
      END IF
      IF(v_r_nss_inv.nss_trab2[1,2] <> "77")THEN
         INITIALIZE v_flujo_descr TO NULL
         SELECT flujo_desc
           INTO v_flujo_descr
           FROM sep_cat_tipo_flujo
          WHERE flujo_cod = v_r_captura_exp.flujo_cod
             
         INITIALIZE v_r_nss_inv.nss_trab2 TO NULL
         CALL fn_mensaje("Aviso","Nss no valido para "||v_flujo_descr,"info")
         RETURN FALSE
      END IF
   END IF

   IF(v_r_nss_inv.sin_credito = FALSE AND 
      (v_r_nss_inv.credito_invadido IS NULL OR 
      v_r_nss_inv.credito_invadido = 0))THEN
      CALL fn_mensaje("Aviso","Falta Captura de número de crédito ","info")
      RETURN FALSE
   END IF    
   
   LET v_r_nss_inv_id_nss_trab1 = null
   LET v_r_nss_inv_id_nss_trab2 = null

    SELECT a.id_derechohabiente 
    INTO   v_r_nss_inv_id_nss_trab1
    FROM   afi_derechohabiente a
    WHERE  a.nss = v_r_nss_inv.nss_trab1              

    IF STATUS = NOTFOUND THEN 
       IF v_r_nss_inv.nss_trab1 IS NOT NULL THEN
          CALL fn_mensaje("Aviso","Nss Invadido no valido","info")
          RETURN FALSE          
        END IF 
    END if
              
    SELECT a.id_derechohabiente 
    INTO   v_r_nss_inv_id_nss_trab2
    FROM   afi_derechohabiente a
    WHERE  a.nss = v_r_nss_inv.nss_trab2             
              
    IF STATUS = NOTFOUND THEN 
       IF v_r_nss_inv.nss_trab2 IS NOT NULL THEN
         CALL fn_mensaje("Aviso","Nss Asociado no valido","info")
         RETURN FALSE          
       END IF 
    END IF

   IF LENGTH(v_r_nss_inv.nss_trab2 CLIPPED) > 0 THEN
      IF NOT (v_r_nss_inv.tipo_trab2 = 1 OR v_r_nss_inv.tipo_trab2 = 2) OR 
         v_r_nss_inv.tipo_trab2 IS NULL THEN 
         CALL fn_mensaje("Aviso","Es requerido el campo: Tipo de trabajador 2","info")
         RETURN FALSE
      END IF
   END IF
   IF(v_r_nss_inv.sin_credito = FALSE AND
      LENGTH(v_r_nss_inv.credito_invadido CLIPPED) = 0)THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: Crédito","info")
      RETURN FALSE
   END IF
   IF(v_r_nss_inv.sin_credito = FALSE AND
      LENGTH(v_r_nss_inv.tipo_credito_invadido CLIPPED) = 0)THEN
      CALL fn_mensaje("Aviso","Es requerido el campo: Tipo Crédito","info")
      RETURN FALSE
   END IF
   -- AHM 2012 mayo 30
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab3 CLIPPED) > 0 THEN
   -- AHM Solicitud de inhabilitarlo    IF NOT (v_r_nss_inv.tipo_trab3 = 1 OR v_r_nss_inv.tipo_trab3 = 2) OR 
   -- AHM Solicitud de inhabilitarlo       v_r_nss_inv.tipo_trab3 IS NULL THEN 
   -- AHM Solicitud de inhabilitarlo       CALL fn_mensaje("Aviso","Es requerido el campo: Tipo de trabajador 3","info")
   -- AHM Solicitud de inhabilitarlo       RETURN FALSE
   -- AHM Solicitud de inhabilitarlo    END IF
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab4 CLIPPED) > 0 THEN
   -- AHM Solicitud de inhabilitarlo    IF NOT (v_r_nss_inv.tipo_trab4 = 1 OR v_r_nss_inv.tipo_trab4 = 2) OR 
   -- AHM Solicitud de inhabilitarlo       v_r_nss_inv.tipo_trab4 IS NULL THEN 
   -- AHM Solicitud de inhabilitarlo       CALL fn_mensaje("Aviso","Es requerido el campo: Tipo de trabajador 4","info")
   -- AHM Solicitud de inhabilitarlo       RETURN FALSE
   -- AHM Solicitud de inhabilitarlo    END IF
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF LENGTH(v_r_nss_inv.nss_trab5 CLIPPED) > 0 THEN
   -- AHM Solicitud de inhabilitarlo    IF NOT (v_r_nss_inv.tipo_trab5 = 1 OR v_r_nss_inv.tipo_trab5 = 2) OR 
   -- AHM Solicitud de inhabilitarlo       v_r_nss_inv.tipo_trab5 IS NULL THEN 
   -- AHM Solicitud de inhabilitarlo       CALL fn_mensaje("Aviso","Es requerido el campo: Tipo de trabajador 5","info")
   -- AHM Solicitud de inhabilitarlo       RETURN FALSE
   -- AHM Solicitud de inhabilitarlo    END IF
   -- AHM Solicitud de inhabilitarlo END IF
   LET v_cont = 0
   IF v_r_nss_inv.tipo_trab1 = 1 THEN
      LET v_cont = v_cont + 1
   END IF
   IF v_r_nss_inv.tipo_trab2 = 1 THEN
      LET v_cont = v_cont + 1
   END IF
   -- AHM 2012 mayo 30
   -- AHM Solicitud de inhabilitarlo IF v_r_nss_inv.tipo_trab3 = 1 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_cont = v_cont + 1
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF v_r_nss_inv.tipo_trab4 = 1 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_cont = v_cont + 1
   -- AHM Solicitud de inhabilitarlo END IF
   -- AHM Solicitud de inhabilitarlo IF v_r_nss_inv.tipo_trab5 = 1 THEN
   -- AHM Solicitud de inhabilitarlo    LET v_cont = v_cont + 1
   -- AHM Solicitud de inhabilitarlo END IF
   
   IF v_cont = 0 THEN
      CALL fn_mensaje("Aviso","Debe seleccionar el tipo de trabajador acreditado","info")
      RETURN FALSE
   END IF
   
   IF v_cont > 1 THEN
      CALL fn_mensaje("Aviso","Solo debe existir un tipo de trabajador acreditado","info")
      RETURN FALSE
   END IF

   IF v_r_nss_inv.id_reclamante1 = 0 AND v_r_nss_inv.id_reclamante2 = 0 THEN
      CALL fn_mensaje("Aviso","Debe seleccionar al trabajador reclamante","info")
      RETURN FALSE
   END IF
   
   -- Rutina para validar parcialmente el expediente y complementa esta validacion
   IF NOT fn_valida_guardado(1) THEN
      RETURN FALSE
   END IF
   # para el caso de flujo_cod = 3 (solo infonavit) se omite la validación de operación 27
   # cualquier otro flujo se evalua op 27
   LET v_g_estado = 0
   IF(v_r_captura_exp.flujo_cod <> 3)THEN
      -- Validaciones generales de operacion 27 (marca y diagnostico)
      CALL fn_valida_op27(v_r_nss_inv.nss_trab1, v_r_nss_inv.nss_trab2, v_r_captura_exp.id_expediente) RETURNING v_status, v_g_estado
      IF NOT v_status THEN
         RETURN FALSE
      END IF
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
    WHERE modulo_cod = 'sep'
   
   DISPLAY "fn_admon_archivo_sep - parametros:"
   DISPLAY "p_archivo         - ", p_archivo        
   DISPLAY "p_id_expediente - ", p_id_expediente
   DISPLAY "p_id_sep_doct - ", p_id_sep_docto
   DISPLAY "p_tpo_ren         - ", p_tpo_ren        
   DISPLAY "Ruta origen  - ",v_ruta_docto CLIPPED||p_archivo
   
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
    WHERE modulo_cod = 'sep'
            
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

CONSTANT v_marca SMALLINT = 280
CONSTANT v_cero  SMALLINT = 0 


   #1.- === Actualización del estado del expediente ===
   -- Rutina para determinar el estado final del expediente de acuerdo a la señal y estado actual
   IF p_modo = 'G' THEN
      LET v_senal = 5
      LET v_sp_edo_des = 5 # aún no existe el registro y no se ejecuta maquinaria,
                           # se inicializa con 5 el estado del expediente
      
      IF NOT (v_r_captura_exp.id_expediente > 0) THEN
         EXECUTE prp_maq_edo_sep USING 'maq_sep_expediente', -- Maquinaria de sep_expediente
                                       v_r_captura_exp.id_expediente,
                                       "id_expediente",
                                       v_senal, --Guardar
                                       p_usuario_cod
                              INTO v_sp_estado, v_sp_diag, v_sp_edo_des
         IF v_sp_estado = 0 THEN
            LET v_r_captura_exp.estado = v_sp_edo_des
         ELSE
            CALL fn_mensaje("Aviso","Problema con la maquinaria de estados\ncon señal: "||v_senal||" y estado:"||v_r_captura_exp.estado,"about")
            RETURN FALSE
         END IF
      END IF
      LET v_r_captura_exp.estado = v_sp_edo_des
   ELSE
      # =============================?
      # flujo_cod = 1 --> Procesar
      # flujo_cod = 3 --> Solo infonavit
      IF(v_r_captura_exp.flujo_cod = 1 OR v_r_captura_exp.flujo_cod = 3)THEN
         LET v_senal = 10
      ELSE
         # flujo_cod = 2 --> Juridico
         IF v_r_captura_exp.flujo_cod = 2 THEN 
    	      CASE v_r_captura_exp.estado
    	      	 WHEN 5 
                      LET v_senal = 15 -- en espera de marca
                 WHEN 15
               	    -- TMP AHM CALL fn_busca_edo_op27(v_r_captura_exp.id_expediente) RETURNING v_estado_op27
               	    CASE v_g_estado -- estado localizado de op27 del expedinte 
               	  	   WHEN 20
               	  	      LET v_senal = 20
               	  	   WHEN 25
               	  	      LET v_senal = 20
               	  	   WHEN 30
               	  	      LET v_senal = 30
               	  	   OTHERWISE
                          CALL fn_mensaje("Aviso","Registro no encontrado en op. 27","about")
                          RETURN FALSE
               	    END CASE
                 WHEN 20
               	    LET v_senal = 25
                 WHEN 25
               	    LET v_senal = 10
    	      END CASE
    	 END IF
      END IF
      
      EXECUTE prp_maq_edo_sep USING 'maq_sep_expediente', -- Maquinaria de sep_expediente
                                    v_r_captura_exp.id_expediente,
                                    "id_expediente",
                                    v_senal, --Guardar
                                    p_usuario_cod
                           INTO v_sp_estado, v_sp_diag, v_sp_edo_des

      IF v_sp_estado = 0 THEN
         LET v_r_captura_exp.estado = v_sp_edo_des
      ELSE
         CALL fn_mensaje("Aviso","Problema con la maquinaria de estados\ncon señal: "||v_senal||" y estado:"||v_r_captura_exp.estado,"about")
         RETURN FALSE
      END IF
      # Solo en caso de si la señal es 10(para dejar el estado del expediente en registrado) se
      # actualiza el estado de operación 27
      # se omite cuando es "solo infonavit" ya que, no se cuentan con registros de op 27 para este flujo
      IF(v_senal = 10 AND v_r_captura_exp.flujo_cod <> 3)THEN
         # señal op 27
         LET v_senal_op27 = 30
         LET v_consulta = "\n SELECT FIRST 1 estado",
                          "\n   FROM sep_det_02_op27",
                          "\n  WHERE invadido = ?",
                          "\n    AND estado not in (10,20) ", -- rechazados
                          "\n    AND id_expediente IS NULL "
         PREPARE prp_rec_edo_op27 FROM v_consulta
         EXECUTE prp_rec_edo_op27 USING v_r_nss_inv.nss_trab1
                                   INTO v_estado_op27

         {DISPLAY v_consulta
         DISPLAY "v_r_nss_inv.nss_trab1:",v_r_nss_inv.nss_trab1
         DISPLAY "v_estado_op27:",v_estado_op27}

         # se avanza maquinara de espediente para indicar que los nss ya estan ligados a un expediente
         LET v_consulta = " EXECUTE FUNCTION fn_maquinaria(?,?,?)"
         PREPARE prp_rec_edo_destino_op27 FROM v_consulta  
         EXECUTE prp_rec_edo_destino_op27 USING 'maq_sep_op27',
                                                v_senal_op27, 
                                                v_estado_op27 
                                           INTO v_ind,
                                                v_diag,
                                                v_edo_destino_op27

         IF v_ind = 0 THEN
            UPDATE sep_det_02_op27
               SET estado = v_edo_destino_op27
             WHERE invadido = v_r_nss_inv.nss_trab1
               AND diag_confronta = '01'
               --AND clasifica_separacion = 'D'
               AND clasifica_separacion IN ('D','E')
               AND estado NOT IN (10,20) -- RECHAZADOS               
         ELSE
            CALL fn_mensaje("Aviso","Problema con la maquinaria de estados\ncon diagnóstico: "||v_diag,"about")
            RETURN FALSE
         END IF
      END IF
      
   END IF
   
   -- Verificación del expediente para actualizar datos o insertar datos del expediente
   IF v_r_captura_exp.id_expediente > 0 THEN
      -- actualiza expediente
      UPDATE sep_expediente
         SET --id_expediente          = v_r_captura_exp
             flujo_cod              = v_r_captura_exp.flujo_cod,
             canal_cod              = v_r_captura_exp.canal_cod,
             nombre_reclamante      = v_r_captura_exp.nombre_reclamante,
             folio_procesar         = v_r_captura_exp.folio_procesar,
             f_recepcion_infonavit  = v_r_captura_exp.f_recepcion_infonavit,
             f_restitucion_infonavit= v_r_captura_exp.f_restitucion_infonavit,
             caso_adai              = v_r_captura_exp.caso_adai,
             id_envio               = v_r_captura_exp.id_envio,
             f_captura              = v_r_captura_exp.f_captura
       WHERE id_expediente = v_r_captura_exp.id_expediente
   ELSE
      -- inserta expediente
      LET v_r_captura_exp.id_expediente = fn_genera_seq("seq_sep_expediente")
      IF v_r_captura_exp.id_expediente = -1 THEN
      	 CALL fn_mensaje("Advertencia","Problemas con la secuencia del expediente","info")
      	 RETURN FALSE
      END IF
      INSERT INTO sep_expediente 
                 (id_expediente          ,
                  flujo_cod              ,
                  canal_cod              ,
                  nombre_reclamante      ,
                  folio_procesar         ,
                  f_recepcion_infonavit  ,
                  f_restitucion_infonavit,
                  caso_adai              ,
                  id_envio               ,
                  f_captura              ,
                  ind_contacto           ,
                  ind_ajuste             ,
                  ind_aviso_suspension   ,
                  ind_baja_notificacion  ,
                  ind_restitucion        ,
                  ind_restitucion_no_aplicados    ,
--                  ind_restitucion_complementario_1,
                  --ind_restitucion_complementario_2,
                  estado)
         VALUES  (v_r_captura_exp.id_expediente          , -- secuencia de sep_expediente
                  v_r_captura_exp.flujo_cod              ,
                  v_r_captura_exp.canal_cod              ,
                  v_r_captura_exp.nombre_reclamante      ,
                  v_r_captura_exp.folio_procesar         ,
                  v_r_captura_exp.f_recepcion_infonavit  ,
                  v_r_captura_exp.f_restitucion_infonavit,
                  v_r_captura_exp.caso_adai              ,
                  v_r_captura_exp.id_envio               ,
                  v_r_captura_exp.f_captura              ,
                  0                                      , # Indicadores se inicializan con cero
                  0                                      , # Diactamen
                  0                                      , # Suspensión
                  0                                      , # Baja
                  0                                      , # Restitución
                  0                                      , # Restitución no aplicada
                  --0                                      , # Restitución Complementaria 1
                  --0                                      , # Restitución Complementaria 2
                  v_r_captura_exp.estado)
      -- Recuperar id_expediente

   END IF
   # se actualiza el campo expediente para todos los registros de op 27 con 
   # diagnostico aceptado y clasificacion D
   IF(p_modo = 'A')THEN   	
      UPDATE sep_det_02_op27
         SET id_expediente = v_r_captura_exp.id_expediente
       WHERE invadido = v_r_nss_inv.nss_trab1
         AND diag_confronta = '01'
         --AND clasifica_separacion = 'D'
         AND clasifica_separacion in ('D','E')
         AND estado NOT IN (10,20) -- rechazados         
         AND (id_expediente IS NULL OR id_expediente IN (""," "))
   END IF

   #2.- ===Actualización de los nss en tabla sep_nss_expediente ===
   -- Verificación de los NSS's del expediente para actualizar o insertar
   INITIALIZE v_r_sep_nss_expediente TO NULL
   LET v_r_sep_nss_expediente.id_expediente   = v_r_captura_exp.id_expediente
   LET v_r_sep_nss_expediente.nss             = v_r_nss_inv.nss_trab1
   LET v_r_sep_nss_expediente.id_derechohabiente  = v_r_nss_inv_id_nss_trab1
   LET v_r_sep_nss_expediente.tipo_nss        = 1 -- Invadido --v_r_nss_inv.tipo_trab1
   LET v_r_sep_nss_expediente.tipo_trabajador = v_r_nss_inv.tipo_trab1
   LET v_r_sep_nss_expediente.tipo_reclamante = v_r_nss_inv.id_reclamante1
   LET v_r_sep_nss_expediente.nombre          = v_r_nss_inv.nombre_invadido
   LET v_r_sep_nss_expediente.tel_contacto1   = v_r_nss_inv.telefono_invadido
   LET v_r_sep_nss_expediente.tel_contacto2   = v_r_nss_inv.telefono2_invadido
   LET v_r_sep_nss_expediente.correo_e        = v_r_nss_inv.correo_e_invadido
   LET v_r_sep_nss_expediente.tel_celular     = v_r_nss_inv.celular_invadido
   LET v_r_sep_nss_expediente.num_credito     = v_r_nss_inv.credito_invadido
   LET v_r_sep_nss_expediente.tpo_credito     = v_r_nss_inv.tipo_credito_invadido
   
   IF v_r_nss_inv.id_nss_expediente1 > 0 THEN
      IF LENGTH(v_r_nss_inv.nss_trab1 CLIPPED) > 0 THEN
         -- actualiza nss expediente
         LET v_r_sep_nss_expediente.id_nss_expediente = v_r_nss_inv.id_nss_expediente1
         UPDATE sep_nss_expediente
            SET id_nss_expediente = v_r_sep_nss_expediente.id_nss_expediente,
                id_expediente     = v_r_sep_nss_expediente.id_expediente    ,
                nss               = v_r_sep_nss_expediente.nss              ,
                id_derechohabiente = v_r_sep_nss_expediente.id_derechohabiente,                
                tipo_nss          = v_r_sep_nss_expediente.tipo_nss         ,
                tipo_trabajador   = v_r_sep_nss_expediente.tipo_trabajador  ,
                tipo_reclamante   = v_r_sep_nss_expediente.tipo_reclamante  ,
                nombre            = v_r_sep_nss_expediente.nombre           ,
                tel_contacto1     = v_r_sep_nss_expediente.tel_contacto1    ,
                tel_contacto2     = v_r_sep_nss_expediente.tel_contacto2    ,
                correo_e          = v_r_sep_nss_expediente.correo_e         ,
                tel_celular       = v_r_sep_nss_expediente.tel_celular      ,
                num_credito       = v_r_sep_nss_expediente.num_credito      ,
                tpo_credito       = v_r_sep_nss_expediente.tpo_credito
          WHERE id_nss_expediente = v_r_nss_inv.id_nss_expediente1
      ELSE
         DELETE FROM sep_nss_expediente
          WHERE id_nss_expediente = v_r_nss_inv.id_nss_expediente1
      END IF
   ELSE
      IF LENGTH(v_r_nss_inv.nss_trab1 CLIPPED) > 0 THEN
         -- inserta nss expediente
         EXECUTE EnuMaxNss INTO v_r_sep_nss_expediente.id_nss_expediente
         
         INSERT INTO sep_nss_expediente
            VALUES(v_r_sep_nss_expediente.*)
      END IF
   END IF

   # 2.1 === Marca cuenta al confirmar expediente ===
   # solo para el caso de "solo infonavit"
   IF(p_modo = 'A' AND v_r_captura_exp.flujo_cod = 3)THEN
      LET v_proceso_cod = 2201
      LET v_codigo_rechazo = 0
      INITIALIZE v_nulo TO NULL
      LET v_consulta = " EXECUTE FUNCTION safre_viv:fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
      PREPARE prp_marca_cuenta_separacion FROM v_consulta 
      EXECUTE prp_marca_cuenta_separacion USING v_r_nss_inv_id_nss_trab1,      -- id_derechohabiente
                                                v_marca,                       -- Marca de separación
                                                --v_r_sep_nss_expediente.id_nss_expediente, -- identificador de registro de sep_nss_expediente
                                                v_r_captura_exp.id_expediente, -- identificador de referencia al expediente
                                                v_r_captura_exp.id_expediente, -- folio --> id_expediete mismo que referencia
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

   INITIALIZE v_r_sep_nss_expediente TO NULL
   LET v_r_sep_nss_expediente.id_expediente   = v_r_captura_exp.id_expediente
   LET v_r_sep_nss_expediente.nss             = v_r_nss_inv.nss_trab2
   LET v_r_sep_nss_expediente.id_derechohabiente  = v_r_nss_inv_id_nss_trab2
   LET v_r_sep_nss_expediente.tipo_nss        = 2 -- Asociado v_r_nss_inv.tipo_trab2
   LET v_r_sep_nss_expediente.tipo_trabajador = v_r_nss_inv.tipo_trab2
   LET v_r_sep_nss_expediente.tipo_reclamante = v_r_nss_inv.id_reclamante2
   LET v_r_sep_nss_expediente.nombre          = v_r_nss_inv.nombre_asociado
   LET v_r_sep_nss_expediente.tel_contacto1   = v_r_nss_inv.telefono_asociado
   LET v_r_sep_nss_expediente.tel_contacto2   = v_r_nss_inv.telefono2_asociado
   LET v_r_sep_nss_expediente.correo_e        = v_r_nss_inv.correo_e_asociado
   LET v_r_sep_nss_expediente.tel_celular     = v_r_nss_inv.celular_asociado
   IF v_r_nss_inv.id_nss_expediente2 > 0 THEN
      IF LENGTH(v_r_nss_inv.nss_trab2 CLIPPED) > 0 THEN
         -- actualiza nss expediente
         LET v_r_sep_nss_expediente.id_nss_expediente = v_r_nss_inv.id_nss_expediente2
         UPDATE sep_nss_expediente
            SET id_nss_expediente = v_r_sep_nss_expediente.id_nss_expediente,
                id_expediente     = v_r_sep_nss_expediente.id_expediente    ,
                nss               = v_r_sep_nss_expediente.nss              ,
                id_derechohabiente = v_r_sep_nss_expediente.id_derechohabiente,                
                tipo_nss          = v_r_sep_nss_expediente.tipo_nss         ,
                tipo_trabajador   = v_r_sep_nss_expediente.tipo_trabajador  ,
                tipo_reclamante   = v_r_sep_nss_expediente.tipo_reclamante  ,
                nombre            = v_r_sep_nss_expediente.nombre           ,
                tel_contacto1     = v_r_sep_nss_expediente.tel_contacto1    ,
                tel_contacto2     = v_r_sep_nss_expediente.tel_contacto2    ,
                correo_e          = v_r_sep_nss_expediente.correo_e         ,
                tel_celular       = v_r_sep_nss_expediente.tel_celular
          WHERE id_nss_expediente = v_r_nss_inv.id_nss_expediente2
      ELSE
         DELETE FROM sep_nss_expediente
          WHERE id_nss_expediente = v_r_nss_inv.id_nss_expediente2
      END IF
   ELSE
      IF LENGTH(v_r_nss_inv.nss_trab2 CLIPPED) > 0 THEN
         -- inserta nss expediente
         EXECUTE EnuMaxNss INTO v_r_sep_nss_expediente.id_nss_expediente
         INSERT INTO sep_nss_expediente
            VALUES(v_r_sep_nss_expediente.*)
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
               CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload,v_r_captura_exp.id_expediente, v_ar_docto_exp[v_pos].id_sep_docto_exp, 'A') 
                  RETURNING v_ar_docto_exp[v_pos].file_upload
            END IF
            -- udpate 
            LET v_c_archivo = v_ar_docto_exp[v_pos].file_upload
            UPDATE sep_docto_exp
               SET docto_nombre = v_c_archivo,
                   docto_nota   = v_ar_docto_exp[v_pos].docto_nota
             WHERE id_sep_docto_exp = v_ar_docto_exp[v_pos].id_sep_docto_exp
         ELSE
            DELETE FROM sep_docto_exp
             WHERE id_sep_docto_exp = v_ar_docto_exp[v_pos].id_sep_docto_exp
         END IF
      ELSE
         IF LENGTH(v_ar_docto_exp[v_pos].file_upload CLIPPED) > 0 THEN
            -- insert
            EXECUTE EnuMaxDocto INTO v_r_sep_docto_exp.id_sep_docto_exp
            LET v_r_sep_docto_exp.docto_cod     = v_ar_docto_exp[v_pos].docto_cod
            LET v_r_sep_docto_exp.id_expediente = v_r_captura_exp.id_expediente
            LET v_r_sep_docto_exp.docto_nombre  = v_ar_docto_exp[v_pos].file_upload
            LET v_r_sep_docto_exp.docto_nota    = v_ar_docto_exp[v_pos].docto_nota 
            CALL fn_admon_archivo_sep(v_ar_docto_exp[v_pos].file_upload,v_r_captura_exp.id_expediente, v_r_sep_docto_exp.id_sep_docto_exp, 'A') 
               RETURNING v_r_sep_docto_exp.docto_nombre
            INSERT INTO sep_docto_exp
            VALUES (v_r_sep_docto_exp.*)
         END IF
      END IF
   END FOR
   
   RETURN TRUE   
   
END FUNCTION

#############################################################################
# Funcion           => fn_carga_nss - Carga nss's a separar del expediente  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_id_expediente - identificador de expediente del que#
#                      se obtendran los nss's a separar                     #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 07 Mayo  2012                                        #
#############################################################################
FUNCTION fn_carga_nss(p_id_expediente)
DEFINE p_id_expediente   LIKE sep_nss_expediente.id_expediente
DEFINE v_r_nss_exp       RECORD LIKE sep_nss_expediente.*
DEFINE v_qrytxt          STRING
DEFINE v_pos             SMALLINT
   
   LET v_qrytxt = "SELECT * ",
                  "  FROM sep_nss_expediente",
                  " WHERE id_expediente = ",p_id_expediente
   
   PREPARE EnuRecNSSExp FROM v_qrytxt
   DECLARE CurRecNSSExp CURSOR FOR EnuRecNSSExp
   
   -- Rutina para recuperar NSS's capturados por expediente
   LET v_pos = 1
   FOREACH CurRecNSSExp INTO v_r_nss_exp.*
      CASE v_pos
         WHEN 1
            LET v_r_nss_inv.id_nss_expediente1 = v_r_nss_exp.id_nss_expediente
            LET v_r_nss_inv.nss_trab1          = v_r_nss_exp.nss
            LET v_r_nss_inv.tipo_trab1         = v_r_nss_exp.tipo_trabajador
            LET v_r_nss_inv.id_reclamante1     = v_r_nss_exp.tipo_reclamante
            LET v_r_nss_inv.nombre_invadido    = v_r_nss_exp.nombre
            LET v_r_nss_inv.telefono_invadido  = v_r_nss_exp.tel_contacto1
            LET v_r_nss_inv.telefono2_invadido = v_r_nss_exp.tel_contacto2
            LET v_r_nss_inv.correo_e_invadido  = v_r_nss_exp.correo_e
            LET v_r_nss_inv.celular_invadido   = v_r_nss_exp.tel_celular
            LET v_r_nss_inv.credito_invadido   = v_r_nss_exp.num_credito
            LET v_r_nss_inv.tipo_credito_invadido   = v_r_nss_exp.tpo_credito
         WHEN 2
            LET v_r_nss_inv.id_nss_expediente2 = v_r_nss_exp.id_nss_expediente
            LET v_r_nss_inv.nss_trab2          = v_r_nss_exp.nss
            LET v_r_nss_inv.tipo_trab2         = v_r_nss_exp.tipo_trabajador
            LET v_r_nss_inv.id_reclamante2     = v_r_nss_exp.tipo_reclamante
            LET v_r_nss_inv.nombre_asociado    = v_r_nss_exp.nombre
            LET v_r_nss_inv.telefono_asociado  = v_r_nss_exp.tel_contacto1
            LET v_r_nss_inv.telefono2_asociado = v_r_nss_exp.tel_contacto2
            LET v_r_nss_inv.correo_e_asociado  = v_r_nss_exp.correo_e
            LET v_r_nss_inv.celular_asociado   = v_r_nss_exp.tel_celular
         -- AHM 2012 mayo 30
         -- AHM Solicitud de inhabilitarlo WHEN 3
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.id_nss_expediente3 = v_r_nss_exp.id_nss_expediente
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.nss_trab3          = v_r_nss_exp.nss
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.tipo_trab3         = v_r_nss_exp.tipo_nss
         -- AHM Solicitud de inhabilitarlo WHEN 4
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.id_nss_expediente4 = v_r_nss_exp.id_nss_expediente
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.nss_trab4          = v_r_nss_exp.nss
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.tipo_trab4         = v_r_nss_exp.tipo_nss
         -- AHM Solicitud de inhabilitarlo WHEN 5
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.id_nss_expediente5 = v_r_nss_exp.id_nss_expediente
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.nss_trab5          = v_r_nss_exp.nss
         -- AHM Solicitud de inhabilitarlo    LET v_r_nss_inv.tipo_trab5         = v_r_nss_exp.tipo_nss
      END CASE
      LET v_pos = v_pos + 1
   END FOREACH
   
END FUNCTION

#############################################################################
# Funcion           => fn_valida_op27 - Rutina general de validaciones op27 #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_nss_invadido - NSS invadido para validar op27      #
#                   => p_nss_asociado - NSS asociado para validar op27      #
#                      p_id_expediente - Expediente a validar               #
# Salida:           => Verdadero si no hay error, falso en caso contrario   #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 01 Junio 2012                                        #
#############################################################################
FUNCTION fn_valida_op27(p_nss_invadido, p_nss_asociado, p_id_expediente)
DEFINE p_nss_invadido  LIKE sep_nss_expediente.nss
DEFINE p_nss_asociado  LIKE sep_nss_expediente.nss
DEFINE p_id_expediente LIKE sep_expediente.id_expediente
DEFINE v_id_expediente  LIKE sep_det_02_op27.id_expediente
DEFINE v_id_det_02_op27 LIKE sep_det_02_op27.id_det_02_op27
DEFINE v_count          SMALLINT
DEFINE v_estado_op27    SMALLINT

LET v_estado_op27 = 0

IF (v_r_captura_exp.flujo_cod = 2 AND v_r_captura_exp.estado = 5) THEN
-- para flujo juridico
   RETURN TRUE, v_estado_op27
ELSE
 
   SELECT id_expediente, 	id_det_02_op27, estado
     INTO v_id_expediente, v_id_det_02_op27, v_estado_op27
     FROM sep_det_02_op27
    WHERE invadido = p_nss_invadido
      AND diag_confronta = '01'
      --AND clasifica_separacion = 'D'
      AND clasifica_separacion IN ('D','E')
      AND estado NOT IN (10,20) -- RECHAZADAS 
      AND (id_expediente IS NULL OR id_expediente IN (""," "))      
      
   IF STATUS <> NOTFOUND THEN
   
      IF LENGTH(v_id_expediente CLIPPED) > 0 THEN
      	  IF p_id_expediente <> v_id_expediente THEN
      	     CALL fn_mensaje("Advertencia","Pareja de Separación no Encontrada Diagnosticada libre en Op. 27","info")
      	     RETURN FALSE, v_estado_op27
      	  END IF
      END IF

      IF LENGTH(v_id_det_02_op27 CLIPPED) = 0 THEN
      	  IF p_id_expediente <> v_id_expediente THEN
            CALL fn_mensaje("Advertencia","NSS Invadido no Encontrado en Op. 27","info")
            RETURN FALSE, v_estado_op27
         END IF
      ELSE
      	
         SELECT NVL(COUNT(*),0)
           INTO v_count
           FROM sep_det_03_op27
          WHERE asociado = p_nss_asociado
            AND id_det_02_op27 = v_id_det_02_op27
         
         IF v_count = 0 THEN
            CALL fn_mensaje("Advertencia","NSS Asociado no Encontrado en Op. 27","info")
            RETURN FALSE, v_estado_op27
         END IF
         
      END IF

   ELSE
 		
      CALL fn_mensaje("Advertencia","Pareja de Separación no Encontrada en Op. 27","info")
      RETURN FALSE, v_estado_op27

   END IF
   
END IF   
   RETURN TRUE, v_estado_op27
END FUNCTION

#############################################################################
# Funcion           => fn_busca_edo_op27 - Rutina general de busqueda op27  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_id_expediente - Expediente a buscar en op27 det02  #
# Salida:           => v_estado_op27 - Estado del expediente en op27        #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 25 Junio 2012                                        #
#############################################################################
FUNCTION fn_busca_edo_op27(p_id_expediente)
DEFINE p_id_expediente    LIKE sep_expediente.id_expediente
DEFINE v_estado_op27      SMALLINT

   SELECT NVL(a.estado,0)
     INTO v_estado_op27
     FROM sep_det_02_op27 a, sep_det_03_op27 b
    WHERE a.id_expediente = p_id_expediente
      AND a.id_det_02_op27 = b.id_det_02_op27
    
   RETURN v_estado_op27
END FUNCTION