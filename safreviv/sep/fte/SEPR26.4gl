--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => SEP                                                     #
#Programa          => SEPR26                                                  #
#Objetivo          => REVERSO DE CARGA DE ARCHIVO DE RESTITUCION POR SEP      #
#Fecha Inicio      => 13 Nov 2018                                             #
###############################################################################
IMPORT os
DATABASE safre_viv
GLOBALS
DEFINE g_enter char(1)
DEFINE g_pid              LIKE bat_ctr_proceso.pid    , -- pid  para el monitor del batch de lanzado
       g_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario          LIKE seg_usuario.usuario_cod,  -- clave del usuario firmado
       r_resultado_opera  SMALLINT   ,
       v_folio            DEC(10,0)  ,
       v_nom_archivo      STRING    
END GLOBALS

DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
DEFINE v_arr_expediente DYNAMIC ARRAY OF RECORD
          v_id_expediente LIKE sep_expediente.id_expediente,
          v_caso_adai     LIKE sep_expediente.caso_adai,
          v_nss_acred     LIKE sep_nss_expediente.nss,
          v_nss_trab1     LIKE sep_nss_expediente.nss,
          v_folio_procesar LIKE sep_expediente.folio_procesar,          
          v_flujo_cod     LIKE sep_cat_tipo_flujo.flujo_desc,
          v_f_captura     LIKE sep_expediente.f_captura,
          v_canal_cod     LIKE sep_cat_canal_recepcion_exp.canal_desc,
          v_estado        CHAR(30),          
          v_nss_trab2     LIKE sep_nss_expediente.nss,
          v_nss_trab3     LIKE sep_nss_expediente.nss,
          v_nss_trab4     LIKE sep_nss_expediente.nss
       END RECORD
DEFINE vg_pos             INTEGER
DEFINE v_r_captura_exp  RECORD-- LIKE sep_expediente.*
         id_expediente         DECIMAL(9,0),--LIKE sep_expediente.id_expediente,
         flujo_cod             LIKE sep_expediente.flujo_cod,
         canal_cod             LIKE sep_expediente.canal_cod,
         nombre_reclamante     LIKE sep_expediente.nombre_reclamante,
         folio_procesar        DECIMAL(9,0),--LIKE sep_expediente.folio_procesar,
         f_recepcion_infonavit LIKE sep_expediente.f_recepcion_infonavit,
         f_restitucion_infonavit LIKE sep_expediente.f_restitucion_infonavit,
         caso_adai             LIKE sep_expediente.caso_adai,
         id_envio              LIKE sep_expediente.id_envio,
         f_captura             LIKE sep_expediente.f_captura,
         --estado                LIKE sep_expediente.estado,
         estado                LIKE sep_estado_expediente.descripcion,         
         docto_dictamen        LIKE sep_expediente.docto_dictamen
       END RECORD,
       v_indicador_contacto    LIKE sep_expediente.ind_contacto,
       v_indicador_desc_analisis LIKE sep_cat_indicador_exp.indicador_desc,
       v_docto_ajuste     RECORD
         v_docto_ajuste   LIKE sep_expediente.docto_ajuste,
         v_ind_ajuste     LIKE sep_expediente.ind_ajuste,
         v_indicador_desc LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_docto_restitucion     RECORD
         v_docto_restitucion   LIKE sep_expediente.docto_ajuste,
         v_ind_restitucion     LIKE sep_expediente.ind_ajuste,
         v_indicador_desc LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_docto_baja_notificacion     RECORD
         v_docto_baja_notificacion   LIKE sep_expediente.docto_ajuste,
         v_ind_baja_notificacion     LIKE sep_expediente.ind_ajuste,
         v_indicador_desc LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_docto_aviso_suspension RECORD
         v_docto_suspension LIKE sep_expediente.docto_ajuste,
         v_ind_suspension   LIKE sep_expediente.ind_ajuste,
         v_indicador_desc   LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_docto_restitucion_complementario_1 RECORD
         v_docto_complementario_1 LIKE sep_expediente.docto_restitucion_complementario_1,
         v_ind_complementario_1   LIKE sep_expediente.ind_restitucion_complementario_1,
         v_indicador_desc         LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_docto_restitucion_complementario_2 RECORD
         v_docto_complementario_2 LIKE sep_expediente.docto_ajuste,
         v_ind_complementario_2   SMALLINT,
         v_indicador_desc         LIKE sep_cat_indicador_exp.indicador_desc
       END RECORD,
       v_estado_cod            LIKE sep_expediente.estado
       
DEFINE v_r_nss_inv      RECORD  -- Registro principal de nss asociados al expediente
          id_nss_expediente1 LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab1          LIKE sep_nss_expediente.nss,
          tipo_trab1         SMALLINT,
          nombre_invadido    LIKE sep_nss_expediente.nombre,
          telefono_invadido  LIKE sep_nss_expediente.tel_contacto1,
          telefono2_invadido LIKE sep_nss_expediente.tel_contacto2,
          correo_e_invadido  LIKE sep_nss_expediente.correo_e,
          celular_invadido   LIKE sep_nss_expediente.tel_celular,
          ind_contacto_invadido LIKE sep_nss_expediente.contactado,
          credito_invadido      LIKE sep_nss_expediente.num_credito,
          tipo_credito_invadido STRING,
          
          id_nss_expediente2 LIKE sep_nss_expediente.id_nss_expediente,
          nss_trab2	         LIKE sep_nss_expediente.nss,
          tipo_trab2         SMALLINT,
          nombre_asociado    LIKE sep_nss_expediente.nombre,
          telefono_asociado  LIKE sep_nss_expediente.tel_contacto1,
          telefono2_asociado LIKE sep_nss_expediente.tel_contacto2,
          correo_e_asociado  LIKE sep_nss_expediente.correo_e,
          celular_asociado   LIKE sep_nss_expediente.tel_celular,
          ind_contacto_asociado LIKE sep_nss_expediente.contactado,
          
          id_reclamante1 SMALLINT,
          id_reclamante2 SMALLINT
       END RECORD
DEFINE v_ar_docto_exp   DYNAMIC ARRAY OF RECORD -- registro principal de documentos asociados al expediente
          id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
          docto_cod     LIKE sep_cat_docto.docto_cod,
          ind_requerido LIKE sep_cat_docto.ind_requerido,
          docto_desc    LIKE sep_cat_docto.docto_desc,
          file_upload   STRING,
          docto_nota    CHAR(200),
          file_upload2  CHAR(200),
          ruta_docto    VARCHAR(200)
       END RECORD
DEFINE v_SqlQry         STRING
DEFINE cb               ui.ComboBox,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_ruta_docto     LIKE seg_modulo.ruta_docto,
       v_ventana        ui.Window,
       v_forma          ui.Form

MAIN

DEFINE v_bandera   SMALLINT
DEFINE v_c_sql     STRING,
       v_continua  BOOLEAN

   -- Parametros de entrada la modulo
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET g_proceso_cod = 2238
   LET g_opera_cod   = 1

   
   CALL fn_inicializa_consultas()

   OPEN WINDOW w_consulta_expediente WITH FORM "SEPR261"
   
   LET v_bandera = TRUE
   
   WHILE v_bandera
      -- Apertura de menú principal del módulo para el filtrado de expedientes
      CONSTRUCT  v_c_sql ON e.caso_adai,
                            e.id_expediente,
                            b.nss,
                            c.nss
                         FROM caso_adai ,
                              id_expediente,                          
                              nss_invadido ,
                              nss_asociado 
      
         BEFORE CONSTRUCT
            -- Asignación de titulo de ventana del módulo
            IF p_s_titulo IS NOT NULL THEN
               CALL ui.Interface.setText(p_s_titulo)
            END IF
         
         AFTER CONSTRUCT
            IF INT_FLAG THEN
               LET v_bandera = FALSE
            END IF
         
      END CONSTRUCT
      
      -- Indicador de filtrado capturado e inicia despliegue de informacion 
      IF v_bandera AND LENGTH(v_c_sql CLIPPED)> 0 THEN
         -- carga arreglo de expedientes
         CALL fn_carga_expedientes(v_c_sql) RETURNING v_continua
         IF(v_continua)THEN
            -- Despiegue de expedientes
            DISPLAY ARRAY v_arr_expediente TO tbl_expediente.*
             ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE, UNBUFFERED)
               BEFORE DISPLAY
                  DISPLAY vg_pos TO tot_expedientes
            
               ON ACTION seleccionar
                  -- Rutina para visualizar detalle del expediente
                  CALL fn_carga_detalle(v_arr_expediente[ARR_CURR()].v_id_expediente)
                  CALL v_arr_expediente.clear()
                  DISPLAY NULL TO tot_expedientes  
                  ACCEPT DISPLAY                

               ON ACTION cancelar
               
                  -- Limpia arreglos de pantalla
                  CALL v_arr_expediente.clear()
                  {DISPLAY ARRAY v_arr_expediente TO tbl_expediente.*
                     BEFORE DISPLAY 
                        EXIT DISPLAY
                  END DISPLAY}
                  DISPLAY NULL TO tot_expedientes
                  --LET v_bandera = FALSE
                  EXIT DISPLAY
               
            END DISPLAY
         ELSE
            CALL fn_mensaje(p_s_titulo,"No se encontró información con criterio dado","information")
         END IF
      END IF         
   END WHILE 
   CLOSE WINDOW w_consulta_expediente
   
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC22                                                   #
#Descripcion       => Función para inicializar las consultas SQL               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Junio 2014                                            #
################################################################################
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   PREPARE prp_recupera_max_id_docto FROM "SELECT NVL(MAX(id_sep_docto_exp)+1,1) FROM sep_docto_exp"

   SELECT ruta_listados
     INTO v_ruta_lst
     FROM seg_modulo
    WHERE modulo_cod = 'bat'

    SELECT ruta_bin,ruta_docto
     INTO v_ruta_ejecutable,v_ruta_docto
     FROM seg_modulo
    WHERE modulo_cod = "sep"

    LET v_consulta = " DELETE ",
                     "   FROM sep_docto_exp",
                     "  WHERE id_sep_docto_exp = ?"             
    PREPARE prp_elimina_reg_archivo FROM v_consulta

    LET v_consulta = "INSERT INTO sep_docto_exp(",
                     " id_sep_docto_exp,",
                     " docto_cod,",
                     " id_expediente,",
                     " docto_nombre,",
                     " docto_nota)",
                     " VALUES(?,?,?,?,?)"
   PREPARE prp_almacena_reg_docto FROM v_consulta

   LET v_consulta = "\n SELECT indicador_desc",           
                    "\n   FROM sep_cat_indicador_exp",
                    "\n  WHERE indicador_cod = ?"
   PREPARE prp_recuepra_desc_indicador_exp FROM v_consulta

   LET v_consulta = " SELECT id_expediente,
                               flujo_cod,
                               canal_cod,
                               nombre_reclamante,
                               folio_procesar,
                               f_recepcion_infonavit,
                               f_restitucion_infonavit,
                               caso_adai,
                               id_envio,
                               f_captura,
                               estado,
                               docto_dictamen,
                               ind_contacto,
                               docto_ajuste,
                               ind_ajuste,
                               docto_restitucion,
                               ind_restitucion,
                               docto_baja_notificacion,
                               ind_baja_notificacion,
                               docto_aviso_suspension,
                               ind_aviso_suspension ,
                               docto_restitucion_complementario_1,
                               ind_restitucion_complementario_1,
                               docto_restitucion_complementario_2,
                               ind_restitucion_complementario_2
                          FROM sep_expediente
                         WHERE id_expediente = ?"
                         
   PREPARE prp_rec_exp FROM v_consulta

   LET v_consulta = "\n SELECT descripcion",
                    "\n   FROM sep_estado_expediente",
                    "\n  WHERE estado = ?"
   PREPARE prp_rec_desc_exp FROM v_consulta

END FUNCTION

#############################################################################
# Funcion           => fn_carga_detalle - Pantalla principal de despliegue  #
#                      de detalle de expedientes                            #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_id_expediente - clave de expediente a desplegar    #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_carga_detalle(p_id_expediente)
DEFINE p_id_expediente   LIKE sep_expediente.id_expediente,
       v_comando         STRING,
       v_documento       STRING,
       v_cad_busqueda    base.StringTokenizer,
       v_cad_reemplazo   base.StringBuffer,
       v_archivo         STRING,
       v_elimina_archivo BOOLEAN,
       r_transfiere      BOOLEAN,
       v_cadena_tmp      STRING,
       v_confirma        SMALLINT
   
   OPEN WINDOW w_consulta_det_expediente WITH FORM "SEPR262"
   
   -- Despliegue de los arreglos de expedientes
  DISPLAY ARRAY v_ar_docto_exp TO tbl_sep_docto_exp.*
  ATTRIBUTES(ACCEPT=FALSE,CANCEL=FALSE, UNBUFFERED)   
--   INPUT ARRAY v_ar_docto_exp FROM tbl_sep_docto_exp.*
--    ATTRIBUTES (ACCEPT=FALSE, CANCEL = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, UNBUFFERED)
      BEFORE DISPLAY
      --BEFORE INPUT
         LET v_ventana = ui.Window.getCurrent()
         LET v_forma   = v_ventana.getForm()         
       --CALL DIALOG.setActionHidden("append",TRUE)
         -- Inicialización general de las pantallas de captura
         -- Recuperacion de expediente por clave seleccionada
         EXECUTE prp_rec_exp USING p_id_expediente
                              INTO v_r_captura_exp.id_expediente,
                                   v_r_captura_exp.flujo_cod,
                                   v_r_captura_exp.canal_cod,
                                   v_r_captura_exp.nombre_reclamante,
                                   v_r_captura_exp.folio_procesar,
                                   v_r_captura_exp.f_recepcion_infonavit,
                                   v_r_captura_exp.f_restitucion_infonavit,
                                   v_r_captura_exp.caso_adai,
                                   v_r_captura_exp.id_envio,
                                   v_r_captura_exp.f_captura,
                                   v_estado_cod,
                                   v_r_captura_exp.docto_dictamen,
                                   v_indicador_contacto,
                                   v_docto_ajuste.v_docto_ajuste,
                                   v_docto_ajuste.v_ind_ajuste,
                                   v_docto_restitucion.v_docto_restitucion,
                                   v_docto_restitucion.v_ind_restitucion,
                                   v_docto_baja_notificacion.v_docto_baja_notificacion,
                                   v_docto_baja_notificacion.v_ind_baja_notificacion,
                                   v_docto_aviso_suspension.v_docto_suspension,
                                   v_docto_aviso_suspension.v_ind_suspension,
                                   v_docto_restitucion_complementario_1.v_docto_complementario_1 ,
                                   v_docto_restitucion_complementario_1.v_ind_complementario_1   , 
                                   v_docto_restitucion_complementario_2.v_docto_complementario_2 ,
                                   v_docto_restitucion_complementario_2.v_ind_complementario_2
                                   
         # Recupera descripción estado expediente
         EXECUTE prp_rec_desc_exp USING v_estado_cod
                                   INTO v_r_captura_exp.estado 

         EXECUTE prp_recuepra_desc_indicador_exp USING v_indicador_contacto
                                                  INTO v_indicador_desc_analisis

         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_ajuste.v_ind_ajuste
                                                  INTO v_docto_ajuste.v_indicador_desc

         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_restitucion.v_ind_restitucion
                                                  INTO v_docto_restitucion.v_indicador_desc

         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_baja_notificacion.v_ind_baja_notificacion
                                                  INTO v_docto_baja_notificacion.v_indicador_desc

         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_aviso_suspension.v_ind_suspension
                                                  INTO v_docto_aviso_suspension.v_indicador_desc

         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_restitucion_complementario_1.v_ind_complementario_1
                                                  INTO v_docto_restitucion_complementario_1.v_indicador_desc
                                                  
         EXECUTE prp_recuepra_desc_indicador_exp USING v_docto_restitucion_complementario_2.v_ind_complementario_2
                                                  INTO v_docto_restitucion_complementario_2.v_indicador_desc

         -- Despliegue del expediente recuperado
         DISPLAY BY NAME v_r_captura_exp.*
      
         -- Rutina para cargar arreglo de documentos del expediente
         CALL fn_carga_docto(v_r_captura_exp.id_expediente)
         CALL init_combo_tipo_flujo()
         CALL init_combo_origen_exp()
      
         -- Rutina para la carga de nss asociados al expediente
         CALL fn_carga_nss(p_id_expediente)
         DISPLAY BY NAME v_r_nss_inv.*

         LET v_documento = ""
         IF LENGTH(v_r_captura_exp.docto_dictamen CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_r_captura_exp.docto_dictamen CLIPPED,"','sepdocto')\" target='nueva'>",v_r_captura_exp.docto_dictamen CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_dictamen
         DISPLAY v_indicador_desc_analisis TO flbl_analisis

         # para la pestaña de ajuste se desplegan los datos recuperados
         LET v_documento = ""
         IF LENGTH(v_docto_ajuste.v_docto_ajuste CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_ajuste.v_docto_ajuste CLIPPED,"','sepdocto')\" target='blank'>",v_docto_ajuste.v_docto_ajuste CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_ajuste
         DISPLAY v_docto_ajuste.v_indicador_desc TO indicador_desc_ajustes
         DISPLAY v_docto_ajuste.v_indicador_desc TO flbl_ajustes

         LET v_documento = ""
         IF LENGTH(v_docto_restitucion.v_docto_restitucion CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_restitucion.v_docto_restitucion CLIPPED,"','sepdocto')\" target='blank'>",v_docto_restitucion.v_docto_restitucion CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_restitucion
         DISPLAY v_docto_restitucion.v_indicador_desc TO indicador_desc_restitucion
         DISPLAY v_docto_restitucion.v_indicador_desc TO flbl_restitucion

         LET v_documento = ""
         IF LENGTH(v_docto_baja_notificacion.v_docto_baja_notificacion CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_baja_notificacion.v_docto_baja_notificacion CLIPPED,"','sepdocto')\" target='blank'>",v_docto_baja_notificacion.v_docto_baja_notificacion CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_baja_notificacion
         DISPLAY v_docto_baja_notificacion.v_indicador_desc TO indicador_desc_baja_notificacion
         DISPLAY v_docto_baja_notificacion.v_indicador_desc TO flbl_notificacion

         LET v_documento = ""
         IF LENGTH(v_docto_aviso_suspension.v_docto_suspension CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_aviso_suspension.v_docto_suspension CLIPPED,"','sepdocto')\" target='blank'>",v_docto_aviso_suspension.v_docto_suspension CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_aviso_suspension
         DISPLAY v_docto_aviso_suspension.v_indicador_desc TO indicador_desc_aviso_suspension
         DISPLAY v_docto_aviso_suspension.v_indicador_desc TO flbl_suspencion

         LET v_documento = ""
         IF LENGTH(v_docto_restitucion_complementario_1.v_docto_complementario_1 CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_restitucion_complementario_1.v_docto_complementario_1 CLIPPED,"','sepdocto')\" target='blank'>",v_docto_restitucion_complementario_1.v_docto_complementario_1 CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_restitucion_compl_1
         DISPLAY v_docto_restitucion_complementario_1.v_indicador_desc TO indicador_desc_rest_compl_1
         DISPLAY v_docto_restitucion_complementario_1.v_indicador_desc TO flbl_complementaria_1

         LET v_documento = ""
         IF LENGTH(v_docto_restitucion_complementario_2.v_docto_complementario_2 CLIPPED) > 0 THEN
         	  LET v_documento = "<a gwc:attributes=\"href resourceuri('",v_docto_restitucion_complementario_2.v_docto_complementario_2 CLIPPED,"','sepdocto')\" target='blank'>",v_docto_restitucion_complementario_2.v_docto_complementario_2 CLIPPED,"</a>"
         END IF
         DISPLAY v_documento TO docto_restitucion_compl_2
         DISPLAY v_docto_restitucion_complementario_2.v_indicador_desc TO indicador_desc_rest_compl_2
         DISPLAY v_docto_restitucion_complementario_2.v_indicador_desc TO flbl_complementaria_2


    ON ACTION btn_rev_restitucion
        LET g_proceso_cod = 2238
        LET g_opera_cod   = 1        

        IF (v_docto_restitucion.v_docto_restitucion IS NOT NULL             AND 
            v_docto_restitucion.v_ind_restitucion = 2                       AND 
            v_docto_restitucion_complementario_1.v_ind_complementario_1 = 0 AND 
            v_docto_restitucion_complementario_2.v_ind_complementario_2 = 0 ) THEN 
            
           IF (v_estado_cod = 45 OR v_estado_cod = 40 OR v_estado_cod = -1) THEN 

              SELECT "OK" 
              FROM   sep_115_restitucion 
              WHERE  id_expediente = p_id_expediente
              AND    ind_restitucion = 1
            UNION               
              SELECT "OK" 
              FROM   sep_mto_restitucion_analisis            
              WHERE  id_expediente = p_id_expediente
              AND    ind_restitucion = 1
            UNION  
              SELECT "OK" 
              FROM   sep_mto_restitucion_no_aplicados
              WHERE  id_expediente = p_id_expediente
              AND    ind_restitucion = 1
            GROUP BY 1

            IF (STATUS <> NOTFOUND)  OR (v_estado_cod = 45 or v_estado_cod = 40 or v_estado_cod = -1) THEN 
                  CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
                            RETURNING r_resultado_opera
                            
                  IF(r_resultado_opera <> 0)THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                  ELSE 
                  
                     CALL fn_ventana_confirma(p_s_titulo,"Confirmar Reverso Confirmación Archivo Restitucion ","question")
                     RETURNING v_confirma
                     
                     IF(v_confirma)THEN
                  
                        CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)
                         RETURNING g_pid
                        LET v_folio = 0
                        LET v_nom_archivo = v_docto_restitucion.v_docto_restitucion
                        CALL fn_inicializa_proceso(g_pid,
                                                   g_proceso_cod,
                                                   g_opera_cod,
                                                   v_folio,
                                                   "SEPR27",
                                                   v_nom_archivo,
                                                   g_usuario)
                              RETURNING r_resultado_opera
                        IF(r_resultado_opera <> 0)THEN
                           CALL fn_muestra_inc_operacion(r_resultado_opera)
                           CONTINUE DISPLAY
                        END IF
   
                        CALL fn_actualiza_opera_ini(g_pid,
                                                    g_proceso_cod,
                                                    g_opera_cod,
                                                    v_folio,
                                                    "SEPR27",
                                                    v_nom_archivo,
                                                    g_usuario)
                              RETURNING r_resultado_opera
                        IF(r_resultado_opera <> 0)THEN
                           CALL fn_muestra_inc_operacion(r_resultado_opera)
                           CONTINUE DISPLAY
                        END IF
   
                        LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPR27.42r ",
                                                        g_usuario, " ",
                                                        g_pid, " ",
                                                        g_proceso_cod," ",
                                                        g_opera_cod," ",
                                                        p_id_expediente, " ", -- en lugar del del folio se envia id_expediente
                                                        v_nom_archivo,
                                        " 1>", v_ruta_lst CLIPPED,
                                        "/nohup:",g_pid USING "&&&&&",":",
                                                  g_proceso_cod USING "&&&&&",":",
                                                  g_opera_cod USING "&&&&&",
                                        " 2>&1 &"
   
                        DISPLAY v_comando
                        RUN v_comando
                        IF(STATUS)THEN
                           CALL fn_mensaje(p_s_titulo,"Ocurrio un error al ejecutar reverso de archivo de restitucion","about")
                        ELSE
                           CALL fn_mensaje(p_s_titulo,"Se ha enviado la operaci?n.\nPodr? revisar el detalle en el monitoreo de procesos","about")
                        END IF
                        EXIT DISPLAY
                     END IF -- if pregunta confirmacion                     
                  END IF    -- if valida operacion
            ELSE  -- if STATUS
              CALL fn_mensaje("Aviso","No se encontro archivo para reverso","information")
            END IF -- if STATUS
           ELSE -- if validacion estado 
              CALL fn_mensaje("Aviso","Estado del expediente no válido para Reverso","information")
           END IF -- if validacion estado
       ELSE  -- if validacion ind
           CALL fn_mensaje("Aviso","No es posible realizar el Reverso","information")
           CONTINUE DISPLAY           
       END IF -- if validacion ind



  ON ACTION btn_rev_ajuste
  
        LET g_proceso_cod = 2239
        LET g_opera_cod   = 1
        
        IF (v_docto_ajuste.v_docto_ajuste IS NOT NULL  AND 
            v_docto_ajuste.v_ind_ajuste = 2  ) THEN
            
           IF (v_estado_cod = 40 OR v_estado_cod = -1 ) THEN 

                  CALL fn_valida_operacion(0,g_proceso_cod,g_opera_cod)
                            RETURNING r_resultado_opera
                            
                  IF(r_resultado_opera <> 0)THEN
                     CALL fn_muestra_inc_operacion(r_resultado_opera)
                  ELSE 
                  
                     CALL fn_ventana_confirma(p_s_titulo,"Confirmar Reverso - Confirmación Archivo Ajuste al Credito ","question")
                     RETURNING v_confirma
                     
                     IF(v_confirma)THEN
                  
                        CALL fn_genera_pid(g_proceso_cod,g_opera_cod,g_usuario)
                         RETURNING g_pid
                        LET v_folio = 0
                        LET v_nom_archivo = v_docto_restitucion.v_docto_restitucion
                        CALL fn_inicializa_proceso(g_pid,
                                                   g_proceso_cod,
                                                   g_opera_cod,
                                                   v_folio,
                                                   "SEPR28",
                                                   v_nom_archivo,
                                                   g_usuario)
                              RETURNING r_resultado_opera
                        IF(r_resultado_opera <> 0)THEN
                           CALL fn_muestra_inc_operacion(r_resultado_opera)
                           CONTINUE DISPLAY
                        END IF
   
                        CALL fn_actualiza_opera_ini(g_pid,
                                                    g_proceso_cod,
                                                    g_opera_cod,
                                                    v_folio,
                                                    "SEPR28",
                                                    v_nom_archivo,
                                                    g_usuario)
                              RETURNING r_resultado_opera
                        IF(r_resultado_opera <> 0)THEN
                           CALL fn_muestra_inc_operacion(r_resultado_opera)
                           CONTINUE DISPLAY
                        END IF
   
                        LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/SEPR28.42r ",
                                                        g_usuario, " ",
                                                        g_pid, " ",
                                                        g_proceso_cod," ",
                                                        g_opera_cod," ",
                                                        p_id_expediente, " ", -- en lugar del del folio se envia id_expediente
                                                        v_nom_archivo,
                                        " 1>", v_ruta_lst CLIPPED,
                                        "/nohup:",g_pid USING "&&&&&",":",
                                                  g_proceso_cod USING "&&&&&",":",
                                                  g_opera_cod USING "&&&&&",
                                        " 2>&1 &"
   
                        DISPLAY v_comando
                        RUN v_comando
                        IF(STATUS)THEN
                           CALL fn_mensaje(p_s_titulo,"Ocurrio un error al ejecutar reverso de archivo de ajuste al credito","about")
                        ELSE
                           CALL fn_mensaje(p_s_titulo,"Se ha enviado la operaci?n.\nPodr? revisar el detalle en el monitoreo de procesos","about")
                        END IF
                        EXIT DISPLAY
                     END IF -- if pregunta confirmacion                     
                  END IF    -- if valida operacion
           ELSE -- if validacion estado 
              CALL fn_mensaje("Aviso","Estado del expediente no válido para Reverso","information")
           END IF -- if validacion estado
       ELSE  -- if validacion ind
           CALL fn_mensaje("Aviso","No es posible realizar el Reverso","information")
           CONTINUE DISPLAY           
       END IF -- if validacion ind


       
{
      BEFORE FIELD tedi_ruta
         IF(v_ar_docto_exp[ARR_CURR()].docto_cod <> 6 AND  # Identificación invadido
          v_ar_docto_exp[ARR_CURR()].docto_cod <> 7)THEN # Identificación asociado            
            #--NEXT FIELD tedi_ruta
         END IF
  
      -- Rutina para seleccionar archivos a vincular desde el equipo local
      ON CHANGE tedi_ruta
         IF(v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED IS NOT NULL AND 
            (v_ar_docto_exp[ARR_CURR()].docto_cod = 6 OR
             v_ar_docto_exp[ARR_CURR()].docto_cod = 7) )THEN
            # reemplaza \ por |, para poder recuperar el nombre del archivo
            LET v_cad_reemplazo = base.StringBuffer.create()
            CALL v_cad_reemplazo.append(v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED) 
            CALL v_cad_reemplazo.replace("\\","|",0)

            LET v_archivo = v_cad_reemplazo.toString()

            # Recupera el ultimo elemento de la cadena 
            LET v_cad_busqueda = base.StringTokenizer.create(v_archivo,"|")
            WHILE v_cad_busqueda.hasMoreTokens()
               LET v_archivo = v_cad_busqueda.nextToken()
            END WHILE

            CALL GET_FLDBUF(tedi_ruta) RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
            LET v_ar_docto_exp[ARR_CURR()].file_upload  = v_archivo 
            CALL GET_FLDBUF(ruta_docto) RETURNING v_ar_docto_exp[ARR_CURR()].ruta_docto         
         --AFTER FIELD tedi_ruta
            --CALL GET_FLDBUF(tedi_ruta) RETURNING v_ar_docto_exp[ARR_CURR()].file_upload
            IF(v_ar_docto_exp[ARR_CURR()].file_upload IS NOT NULL)THEN
               # Rutina que renombra y transfiere el archivo al servidor
               CALL fn_transfiere_archivo(v_r_captura_exp.id_expediente,
                                          v_ar_docto_exp[ARR_CURR()].*) RETURNING v_ar_docto_exp[ARR_CURR()].*,
                                                                                  r_transfiere
               IF(r_transfiere)THEN                  
                  # Rutina para visualizar la liga y ruta de los documentos asociados al expediente
                  LET v_ar_docto_exp[ARR_CURR()].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"','sepdocto')\" target='nueva'>",v_ar_docto_exp[ARR_CURR()].file_upload CLIPPED,"</a>"                  
                  LET v_ar_docto_exp[ARR_CURR()].file_upload = NULL 
               ELSE
                  DISPLAY "Archivo no encontrado para transferencia"
               END IF
            END IF
         ELSE
            CALL fn_mensaje("Aviso","Este documento ya no puede ser modificado","information")
         END IF
}
      ON ACTION con_operacion27
         # Consulta de operacion 27 para los NSS especificados(invadido y asociado) 
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPC10.42r ",
                         g_usuario CLIPPED," ",p_tipo_ejecucion," '",p_s_titulo,"' ",
                         v_r_nss_inv.nss_trab1," ",v_r_nss_inv.nss_trab2
         RUN v_comando
 
{
      ON ACTION con_operacion28
         # Consulta de operacion 27 para los NSS especificados(invadido y asociado) 
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPC32.42r ",
                         g_usuario CLIPPED," ",p_tipo_ejecucion," '",p_s_titulo,"' ",
                         v_r_nss_inv.nss_trab1," ",v_r_nss_inv.nss_trab2
         RUN v_comando
         

      ON ACTION con_operacion29
         # Consulta de operacion 27 para los NSS especificados(invadido y asociado) 
         LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPC40.42r ",
                         g_usuario CLIPPED," ",p_tipo_ejecucion," '",p_s_titulo,"' ",
                         v_r_nss_inv.nss_trab1," ",v_r_nss_inv.nss_trab2
         RUN v_comando
}
      ON ACTION cancelar
         --EXIT INPUT
         EXIT DISPLAY
{      
      ON ACTION btn_quitar
         INITIALIZE v_cadena_tmp TO NULL
         LET v_cadena_tmp =  v_ar_docto_exp[ARR_CURR()].ruta_docto
         IF( v_cadena_tmp CLIPPED IS NOT NULL )THEN
            IF( v_ar_docto_exp[ARR_CURR()].docto_cod = 6 OR
                v_ar_docto_exp[ARR_CURR()].docto_cod = 7 )THEN
               CALL fn_quitar_archivo(v_ar_docto_exp[ARR_CURR()].file_upload2,
                                      v_ar_docto_exp[ARR_CURR()].id_sep_docto_exp) RETURNING v_elimina_archivo
               IF(v_elimina_archivo)THEN
                  LET v_ar_docto_exp[ARR_CURR()].file_upload  = NULL
                  LET v_ar_docto_exp[ARR_CURR()].file_upload2 = NULL
                  LET v_ar_docto_exp[ARR_CURR()].docto_nota   = NULL
                  LET v_ar_docto_exp[ARR_CURR()].ruta_docto   = NULL
                  CALL fn_mensaje("AVISO","Documento eliminado","information")
               ELSE
                  CALL fn_mensaje("AVISO","No se pudo eliminar archivo","information")
               END IF
            ELSE
               CALL fn_mensaje("AVISO","Documento no puede ser eliminado","information")
            END IF
         END IF
}

    


         

   --END INPUT
   END DISPLAY
   LET INT_FLAG = FALSE
   
   CLOSE WINDOW w_consulta_det_expediente
END FUNCTION

#############################################################################
# Funcion           => fn_carga_expedientes - Carga de los arreglos de      #
#                      acuerdo al filtro de expedientes                     #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_sql - cadena con filtro capturado en construct     #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_carga_expedientes(p_sql)
DEFINE p_sql       STRING
DEFINE v_c_sql     STRING
DEFINE v_r_nss_cons RECORD
          id_expediente   LIKE sep_expediente.id_expediente,
          caso_adai       LIKE sep_expediente.caso_adai,
          invadido        CHAR(11),    
          asociado        CHAR(11),
          folio_procesar  LIKE sep_expediente.folio_procesar,          
          flujo_cod       LIKE sep_expediente.flujo_cod,
          f_captura       LIKE sep_expediente.f_captura,
          canal_cod       LIKE sep_expediente.canal_cod,
          estado          CHAR(30)
       END RECORD
DEFINE v_pos              INTEGER
DEFINE v_pos_nss          INTEGER
DEFINE v_id_expediente    LIKE sep_expediente.id_expediente
DEFINE v_tipo_nss         LIKE sep_nss_expediente.tipo_trabajador
   
   -- Recuperacion de expedientes completos a partir de los criterios de busqueda capturados
--   LET v_c_sql = "SELECT e.id_expediente, e.caso_adai, e.flujo_cod, e.f_captura, e.canal_cod, n.nss, n.tipo_trabajador",

    LET v_c_sql = "SELECT e.id_expediente ,
                  e.caso_adai     ,
                  b.nss           ,
                  c.nss           ,
                  e.folio_procesar,
                  e.flujo_cod     ,
                  e.f_captura     ,
                  e.canal_cod     ,
                  e.estado|| ' '||d.descripcion
                  FROM   sep_expediente e ,
                         sep_nss_expediente b ,
                         sep_nss_expediente c  ,
                         sep_estado_expediente d
                  WHERE  e.id_expediente = b.id_expediente
                  AND    e.id_expediente = c.id_expediente
                  AND    e.estado = d.estado
                  AND    b.tipo_nss = 1
                  AND    c.tipo_nss = 2
                  AND    e.estado in (40,45,-1)
                  AND   ", p_sql  CLIPPED,
                  " ORDER by e.id_expediente,b.nss,c.nss "

{
   LET v_c_sql = "SELECT e.id_expediente,",
                 "       e.caso_adai,",
                 "       e.flujo_cod,",
                 "       e.f_captura,",
                 "       e.canal_cod,",
                 "       n.nss,",
                 "       n.tipo_nss",
                 "  FROM sep_expediente e, sep_nss_expediente n",
                 " WHERE e.id_expediente = n.id_expediente",
                 "   AND e.id_expediente in ",
                 "(SELECT UNIQUE e.id_expediente",
                 "  FROM sep_expediente e, sep_nss_expediente n",
                 " WHERE e.id_expediente = n.id_expediente",
                 "   AND e.estado in (40,45,-1) ",                 
                 "   AND ",p_sql,")",
                 " ORDER BY e.id_expediente, n.tipo_nss, n.nss"
}   
   PREPARE EnuExpPreliq FROM v_c_sql
   DECLARE CurExpPreliq CURSOR FOR EnuExpPreliq 
   
   LET v_pos = 0
   LET v_id_expediente = 0
   -- Almacenamiento por expediente de todos los nss asociados a el
   FOREACH CurExpPreliq INTO v_r_nss_cons.*
         LET v_pos = v_pos + 1
       -- Asignacion del nuevo expediente
         LET v_arr_expediente[v_pos].v_id_expediente = v_r_nss_cons.id_expediente
         LET v_arr_expediente[v_pos].v_caso_adai     = v_r_nss_cons.caso_adai
         LET v_arr_expediente[v_pos].v_nss_acred = v_r_nss_cons.invadido
         LET v_arr_expediente[v_pos].v_nss_trab1 = v_r_nss_cons.asociado
         LET v_arr_expediente[v_pos].v_folio_procesar = v_r_nss_cons.folio_procesar         
         CALL fn_rec_flujo(v_r_nss_cons.flujo_cod) RETURNING v_arr_expediente[v_pos].v_flujo_cod
         LET v_arr_expediente[v_pos].v_f_captura = v_r_nss_cons.f_captura
         CALL fn_rec_canal(v_r_nss_cons.canal_cod) RETURNING v_arr_expediente[v_pos].v_canal_cod
         LET v_arr_expediente[v_pos].v_estado = v_r_nss_cons.estado         
         -- Control para el cambio de registro de expediente
      -- Contador de posicion del nss por expediente
         LET v_pos_nss = v_pos_nss + 1
   END FOREACH
   
   LET vg_pos = v_pos
   IF(v_arr_expediente.getLength() > 0)THEN
      # indicada que se ha recuperado nformación
      RETURN TRUE
   ELSE
      # indicada que no se ha recuperado nformación
      RETURN FALSE
   END IF
   
END FUNCTION

#############################################################################
# Funcion           => fn_rec_canal - Recuperacion de la descripcion canal  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_canal_cod - clave de canal a recuperar             #
# Salida:           => v_canal_desc - descripción de canal recuerada        #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_rec_canal(p_canal_cod)
DEFINE p_canal_cod   LIKE sep_cat_canal_recepcion_exp.canal_cod
DEFINE v_canal_desc  CHAR(40)
   
   LET v_canal_desc = ''
   
   -- Recuperacion de descripción por clave de canal
   SELECT canal_desc
     INTO v_canal_desc
     FROM sep_cat_canal_recepcion_exp
    WHERE canal_cod = p_canal_cod
     
   RETURN v_canal_desc
END FUNCTION

#############################################################################
# Funcion           => fn_rec_flujo - Recuperacion de la descripcion flujo  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_flujo_cod - clave de flujo a recuperar             #
# Salida:           => v_flujo_desc - descripción de flujo recuerada        #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_rec_flujo(p_flujo_cod)
DEFINE p_flujo_cod   LIKE sep_cat_tipo_flujo.flujo_cod
DEFINE v_flujo_desc  CHAR(40)
   
   LET v_flujo_desc = ''
   
   -- Recuperacion de descripción por clave de flujo
   SELECT flujo_desc
     INTO v_flujo_desc
     FROM sep_cat_tipo_flujo
    WHERE flujo_cod = p_flujo_cod
     
   RETURN v_flujo_desc
END FUNCTION

#############################################################################
# Funcion           => fn_carga_nss - Carga nss's a separar del expediente  #
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => p_id_expediente - identificador de expediente del que#
#                      se obtendran los nss's a separar                     #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_carga_nss(p_id_expediente)
DEFINE p_id_expediente   LIKE sep_nss_expediente.id_expediente
DEFINE v_r_nss_exp       RECORD LIKE sep_nss_expediente.*
DEFINE v_qrytxt          STRING
DEFINE v_pos             SMALLINT
   
   -- Recuperacion de información de nss asociados al expediente
   LET v_qrytxt = "SELECT * ",
                  "  FROM sep_nss_expediente",
                  " WHERE id_expediente = ",p_id_expediente
   
   PREPARE EnuRecNSSExp FROM v_qrytxt
   DECLARE CurRecNSSExp CURSOR FOR EnuRecNSSExp
   
   -- Rutina para recuperar NSS's capturados por expediente
   LET v_pos = 1
   FOREACH CurRecNSSExp INTO v_r_nss_exp.*
      -- Indicador de nss para almacenar en el registro de asociados al expediente
      CASE v_pos
         WHEN 1
            LET v_r_nss_inv.id_nss_expediente1    = v_r_nss_exp.id_nss_expediente
            LET v_r_nss_inv.nss_trab1             = v_r_nss_exp.nss
            LET v_r_nss_inv.tipo_trab1            = v_r_nss_exp.tipo_trabajador
            
            LET v_r_nss_inv.id_reclamante1        = v_r_nss_exp.tipo_reclamante
            
            LET v_r_nss_inv.nombre_invadido       = v_r_nss_exp.nombre
            LET v_r_nss_inv.telefono_invadido     = v_r_nss_exp.tel_contacto1
            LET v_r_nss_inv.telefono2_invadido    = v_r_nss_exp.tel_contacto2
            LET v_r_nss_inv.correo_e_invadido     = v_r_nss_exp.correo_e
            LET v_r_nss_inv.celular_invadido      = v_r_nss_exp.tel_celular
            LET v_r_nss_inv.ind_contacto_invadido = v_r_nss_exp.contactado
            LET v_r_nss_inv.credito_invadido      = v_r_nss_exp.num_credito
            CASE v_r_nss_exp.tpo_credito
              WHEN 15
                 LET v_r_nss_inv.tipo_credito_invadido = "15 créditos tradicionales (transferencia de acreditados)"
              WHEN 19
                 LET v_r_nss_inv.tipo_credito_invadido = "19 créditos apoyo infonavit (créditos en garantía 43 bis)"
              WHEN 43
                 LET v_r_nss_inv.tipo_credito_invadido = "43 créditos cofinanciados (anualidades garantizadas)"
            END CASE
            --LET v_r_nss_inv.tipo_credito_invadido = v_r_nss_exp.tpo_credito
            
         WHEN 2
            LET v_r_nss_inv.id_nss_expediente2    = v_r_nss_exp.id_nss_expediente
            LET v_r_nss_inv.nss_trab2             = v_r_nss_exp.nss
            LET v_r_nss_inv.tipo_trab2            = v_r_nss_exp.tipo_trabajador
            
            LET v_r_nss_inv.id_reclamante2        = v_r_nss_exp.tipo_reclamante
            
            LET v_r_nss_inv.nombre_asociado       = v_r_nss_exp.nombre
            LET v_r_nss_inv.telefono_asociado     = v_r_nss_exp.tel_contacto1
            LET v_r_nss_inv.telefono2_asociado    = v_r_nss_exp.tel_contacto2
            LET v_r_nss_inv.correo_e_asociado     = v_r_nss_exp.correo_e
            LET v_r_nss_inv.celular_asociado      = v_r_nss_exp.tel_celular
            LET v_r_nss_inv.ind_contacto_asociado = v_r_nss_exp.contactado
         -- 2012 Mayo 30
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
# Fecha             => 17 Mayo  2012                                        #
#############################################################################
FUNCTION fn_carga_docto(p_id_expediente)
--DEFINE p_modo          CHAR(1)
DEFINE p_id_expediente LIKE sep_docto_exp.id_expediente
DEFINE v_pos    INTEGER
DEFINE v_c_archivo   CHAR(40),
       v_documento RECORD
         v_id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
         v_docto_cod        LIKE sep_cat_docto.docto_cod,
         v_ind_requerido    LIKE sep_cat_docto.ind_requerido,
         v_docto_desc       LIKE sep_cat_docto.docto_desc,
         v_docto_nombre     LIKE sep_docto_exp.docto_nombre,
         v_docto_nota       LIKE sep_docto_exp.docto_nota,
         v_docto_nombre2    LIKE sep_docto_exp.docto_nombre
       END RECORD

   CALL v_ar_docto_exp.clear()
   -- Recupera documentos del catalogo y agrega la informacion previamente capturada
   {LET v_SqlQry = "SELECT id_sep_docto_exp,",
                   "       sep_cat_docto.docto_cod,",
                   "       ind_requerido,",
                   "       docto_desc, ",
                   "       docto_nombre,",
                   "       docto_nota,",
                   "       docto_nombre",
                   "  FROM sep_cat_docto, sep_docto_exp",
                   " WHERE sep_cat_docto.docto_cod = sep_docto_exp.docto_cod",
                   "   AND sep_docto_exp.id_expediente = ",p_id_expediente,
                   " ORDER BY 3 DESC, 2 ASC"}

   LET v_SqlQry = " SELECT id_sep_docto_exp,",
                  "        cat.docto_cod,",
                  "        ind_requerido,",
                  "        docto_desc, ",
                  "        docto_nombre,",
                  "        docto_nota,",
                  "        docto_nombre",
                  --"   FROM sep_cat_docto cat LEFT OUTER JOIN ",
                  "   FROM sep_cat_docto cat JOIN ",
                  "        TABLE(MULTISET(SELECT *",
                  "                         FROM sep_docto_exp",
                  "                        WHERE id_expediente = ?)) doc",
                  "     ON cat.docto_cod = doc.docto_cod",
                  "  ORDER BY 3 DESC, 2 ASC"
   
   PREPARE EnuDoctoExp FROM v_SqlQry
   DECLARE CurDoctoExp CURSOR FOR EnuDoctoExp

   -- Carga del arreglo de documentos asociados
   LET v_pos = 1
   FOREACH CurDoctoExp USING p_id_expediente
                        INTO v_documento.* 
                        {INTO v_ar_docto_exp[v_pos].id_sep_docto_exp,
                             v_ar_docto_exp[v_pos].docto_cod,
                             v_ar_docto_exp[v_pos].ind_requerido,
                             v_ar_docto_exp[v_pos].docto_desc,
                             v_c_archivo,
                             v_ar_docto_exp[v_pos].docto_nota,
                             v_c_archivo}
                             
     IF(v_documento.v_id_sep_docto_exp IS NOT NULL) THEN
--     IF(v_documento.v_id_sep_docto_exp IS NOT NULL OR 
         --v_documento.v_docto_cod = 6 OR   # identificación invadido
         --v_documento.v_docto_cod = 7)THEN # identificación asociado
         
         LET v_ar_docto_exp[v_pos].id_sep_docto_exp = v_documento.v_id_sep_docto_exp
         LET v_ar_docto_exp[v_pos].docto_cod        = v_documento.v_docto_cod
         LET v_ar_docto_exp[v_pos].ind_requerido    = v_documento.v_ind_requerido
         LET v_ar_docto_exp[v_pos].docto_desc       = v_documento.v_docto_desc
         LET v_ar_docto_exp[v_pos].docto_nota       = v_documento.v_docto_nota         
         LET v_ar_docto_exp[v_pos].file_upload      = v_documento.v_docto_nombre
         LET v_ar_docto_exp[v_pos].file_upload2     = v_documento.v_docto_nombre
         -- Generación de la liga para visualizacion de documentos asociados
         LET v_ar_docto_exp[v_pos].ruta_docto = "<a gwc:attributes=\"href resourceuri('",v_documento.v_docto_nombre CLIPPED,"','sepdocto')\" target='nueva'>",v_documento.v_docto_nombre CLIPPED,"</a>"      
         LET v_pos = v_pos + 1
         
     END IF
      
   END FOREACH
   FREE CurDoctoExp

END FUNCTION

#############################################################################
# Funcion           => init_combo_tipo_flujo - Inicializa el combo tpo flujo#
# Propietario       => E.F.P                                                #
# Sistema           => SEP                                                  #
# Entrada:          => Ninguno                                              #
# Salida:           => Ninguno                                              #
# Modificacion      => Alexandro Hollmann, EFP                              #
# Fecha             => 17 Mayo    2012                                      #
#############################################################################
FUNCTION init_combo_tipo_flujo()

   DEFINE v_flujo_cod        LIKE sep_cat_tipo_flujo.flujo_cod
   DEFINE v_flujo_desc       LIKE sep_cat_tipo_flujo.flujo_desc
   DEFINE desc_combo        CHAR(50)

   LET cb   = ui.combobox.forname("flujo_cod")
   CALL cb.clear()
   
   -- Recuperacion de informacion del catalogo
   DECLARE cur_tipo_flujo CURSOR FOR
    SELECT tb.flujo_cod, tb.flujo_desc
      FROM sep_cat_tipo_flujo tb
     ORDER  BY 1
   
   -- Carga de la informacion al objeto combo de flujo
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
# Fecha             => 17 Mayo    2012                                      #
#############################################################################
FUNCTION init_combo_origen_exp()

   DEFINE v_canal_cod        LIKE sep_cat_canal_recepcion_exp.canal_cod 
   DEFINE v_canal_desc       LIKE sep_cat_canal_recepcion_exp.canal_desc
   DEFINE desc_combo         CHAR(50)

   LET cb   = ui.combobox.forname("canal_cod")
   CALL cb.clear()
   
   -- Recuperacion de informacion del catalogo
   DECLARE cur_origen_exp CURSOR FOR
    SELECT tb.canal_cod, tb.canal_desc
      FROM sep_cat_canal_recepcion_exp tb
     ORDER  BY 1
   
   -- Carga de la informacion al objeto combo de canales
   FOREACH cur_origen_exp INTO v_canal_cod, v_canal_desc
       LET desc_combo= v_canal_cod       USING "&&&", " ",
                       v_canal_desc CLIPPED
       CALL cb.additem( v_canal_cod, desc_combo )
   END FOREACH

END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC22                                                   #
#Descripcion       => Función para transferir el archivo al servidor de        #
#                     aplicación                                               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 26 Junio 2014                                            #
################################################################################
FUNCTION fn_transfiere_archivo(p_id_expediente,p_documento)
DEFINE p_id_expediente LIKE sep_expediente.id_expediente,
       p_documento     RECORD
         id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
         docto_cod     LIKE sep_cat_docto.docto_cod,
         ind_requerido LIKE sep_cat_docto.ind_requerido,
         docto_desc    LIKE sep_cat_docto.docto_desc,
         file_upload   STRING,
         docto_nota    CHAR(200),
         file_upload2  CHAR(200),
         ruta_docto    CHAR(200)
       END RECORD,
       v_id_sep_docto_exp LIKE sep_docto_exp.id_sep_docto_exp,
       v_nombre_archivo   VARCHAR(200),       
       v_verifica_file    STRING,
       v_elimina_archivo  BOOLEAN,
       v_transfiere       BOOLEAN

       
   --IF(os.Path.exists(p_documento.file_upload))THEN
      LET v_transfiere = TRUE
      # Recupera nombre de archivo de ruta
      CALL os.Path.basename(p_documento.file_upload) RETURNING v_nombre_archivo
     
      CALL fn_sustituye_cadena (v_nombre_archivo, # Cadena a procesar
                                " ", # carácter a ser reemplazado
                                "_") # caractér de reemplazo
                                RETURNING v_nombre_archivo # cadena reemplazada
 
      # Recupera el maximo id del documento
      EXECUTE prp_recupera_max_id_docto INTO v_id_sep_docto_exp
  
      LET v_nombre_archivo = p_id_expediente USING "&&&&","-",v_id_sep_docto_exp USING "&&&&","_",v_nombre_archivo

      --DISPLAY "Origen : ",p_documento.file_upload
      --DISPLAY "Destino: ",v_ruta_docto CLIPPED, v_nombre_archivo
      # Sí existe archivo en servidor se elimina
      IF(p_documento.file_upload2 IS NOT NULL)THEN
         # Elimina archivo anterior
         CALL fn_quitar_archivo(p_documento.file_upload2,
                                p_documento.id_sep_docto_exp) RETURNING v_elimina_archivo
      END IF
      SLEEP 5
      # Carga archivo al servidor en ruta estática
      CALL fgl_getfile(p_documento.file_upload, v_ruta_docto CLIPPED||v_nombre_archivo)
      
      EXECUTE prp_almacena_reg_docto USING v_id_sep_docto_exp,
                                           p_documento.docto_cod,
                                           p_id_expediente,
                                           v_nombre_archivo,
                                           p_documento.docto_nota
      # Re asinga nuevos valores para el registro del documento 
      LET p_documento.id_sep_docto_exp = v_id_sep_docto_exp
      LET p_documento.file_upload      = v_nombre_archivo
      LET p_documento.file_upload2     = v_nombre_archivo
      
   --ELSE
   --   LET v_transfiere = FALSE
   --END IF
   RETURN p_documento.*,
          v_transfiere
            
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC22                                                   #
#Descripcion       => Función para archivo del servidor                        #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 27 Junio 2014                                            #
################################################################################
FUNCTION fn_quitar_archivo(p_archivo,p_id_docto)
DEFINE p_archivo  STRING,
       p_id_docto LIKE sep_docto_exp.id_sep_docto_exp,
       v_elimina_archivo  BOOLEAN

   CALL os.Path.delete(v_ruta_docto CLIPPED||p_archivo CLIPPED) RETURNING v_elimina_archivo
   IF NOT(v_elimina_archivo)THEN
      DISPLAY "No se pudo eliminar archivo: ",v_ruta_docto CLIPPED||p_archivo
   ELSE
      EXECUTE prp_elimina_reg_archivo USING p_id_docto
   END IF
   
   RETURN v_elimina_archivo
END FUNCTION
