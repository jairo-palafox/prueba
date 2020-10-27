--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTC02                                        #
#Objetivo          =>Programa que realiza la consulta de historico #
#                    de Uso de garantía                            #
#Autor             =>
#Fecha inicio      =>27 Abril 2012                                 #
####################################################################

IMPORT OS
DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS
DEFINE g_pid               LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod       LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod         LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_tpo_originacion   LIKE cre_acreditado.tpo_originacion --tipo de originacion
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_tpo_originacion = 2 --Creditos en garantia

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTC10.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la funcion general de consulta de historico de acreditados
   CALL fn_consulta_historico_uso()
END MAIN

## ventana que muestra las opciones para consultas de acreditados ##
FUNCTION fn_consulta_historico_uso()
   DEFINE v_op_consulta CHAR(4)

   OPEN WINDOW w_op_consulta WITH FORM "GRTC021"
      INPUT v_op_consulta FROM opciones ATTRIBUTE (UNBUFFERED)
         ON ACTION ACCEPT
            IF v_op_consulta = "opt1" THEN
              CALL fn_consulta_derechohabiente()
            END IF
            IF v_op_consulta = "opt2" THEN
              CALL fn_consulta_estado()
            END IF
            IF v_op_consulta = "opt3" THEN
              CALL fn_consulta_historica()
            END IF
             IF v_op_consulta = "opt4" THEN
              CALL fn_consulta_archivo()
            END IF
         ON ACTION CANCEL
            EXIT INPUT
      END INPUT
   CLOSE WINDOW w_op_consulta
END FUNCTION

## Funcion que realiza la consulta de derechohabiente ##
FUNCTION fn_consulta_derechohabiente()
DEFINE v_s_condicion        STRING,
       v_nss                LIKE afi_derechohabiente.nss,
       v_curp               LIKE afi_derechohabiente.curp,
       v_rfc                LIKE afi_derechohabiente.rfc, 
       v_app_paterno        LIKE afi_derechohabiente.ap_paterno_af,
       v_app_materno        LIKE afi_derechohabiente.ap_materno_af,
       v_nombre             LIKE afi_derechohabiente.nombre_af,
       v_cta_nombre         SMALLINT,
       v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
       v_sqlqry             STRING,
       v_bandera            SMALLINT,
       v_cta_reg            SMALLINT   
 
   LET v_s_condicion = "1=1"
   LET v_bandera = TRUE 
   
   OPEN WINDOW w_op_derechohab WITH FORM "GRTC012"   
      CONSTRUCT v_s_condicion
          ON nss,
             curp,
             rfc,            
             ap_paterno_af,
             ap_materno_af,
             nombre_af             
        FROM nss,
             curp,
             rfc,             
             app_paterno,
             app_materno,
             nombre

         AFTER FIELD nombre
            NEXT FIELD nss

         ON ACTION ACCEPT          
            LET v_app_paterno = GET_FLDBUF(app_paterno)
            LET v_app_materno = GET_FLDBUF(app_paterno)
            LET v_nombre = GET_FLDBUF(nombre)
            LET v_nss = GET_FLDBUF(nss)
            LET v_curp = GET_FLDBUF(curp)
            LET v_rfc = GET_FLDBUF(rfc)         
       
            IF v_app_paterno IS NULL AND v_app_materno IS NULL AND v_nombre IS NULL AND 
               v_nss IS NULL AND v_curp IS NULL AND v_rfc IS NULL THEN 
               CALL fn_mensaje("Aviso","Se requiere capturar al menos un campo","stop")
               CONTINUE CONSTRUCT
            END IF             
      
            IF v_app_paterno IS NOT NULL AND v_nombre IS NULL THEN
               CALL fn_mensaje("Aviso","Se requiere capturar el Nombre del derechohabiente","stop")
               CONTINUE CONSTRUCT 
            END IF

            IF v_app_paterno IS NULL AND v_nombre IS NOT NULL THEN
               CALL fn_mensaje("Aviso","Se requiere capturar el Apellido paterno del derechohabiente","stop")
               CONTINUE CONSTRUCT
            END IF
            EXIT CONSTRUCT

         ON ACTION CANCEL
            LET v_bandera = FALSE
            EXIT CONSTRUCT
      END CONSTRUCT

   IF v_bandera = TRUE THEN
      LET v_sqlqry = " SELECT COUNT(*)",
                     "   FROM afi_derechohabiente ",
                     "  WHERE ",v_s_condicion

      PREPARE prp_cta_nombre FROM v_sqlqry
      EXECUTE prp_cta_nombre INTO v_cta_nombre

      IF v_cta_nombre = 0 THEN
         CALL fn_mensaje("Aviso","No existe información del derechohabiente","stop")
         LET v_bandera = FALSE
      ELSE 
         IF v_cta_nombre > 1 THEN 
            -- consulta listado de derechohabientes de afi para obtener el id derechohab
            CALL fn_obt_derechohabiente(v_s_condicion) RETURNING v_id_derechohabiente

            IF v_id_derechohabiente = -1 THEN
               LET v_bandera = FALSE
            ELSE
               SELECT COUNT(*)
                 INTO v_cta_reg
                 FROM cre_uso_garantia
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND tpo_transferencia IN ('18','48')

               IF v_cta_reg = 0 THEN
                  CALL fn_mensaje("Aviso","El derechohabiente no tiene créditos otorgados","stop")
                  LET v_bandera = FALSE
               END IF
            END IF
         ELSE
             --consulta id_derehohabiente
             LET v_sqlqry = " SELECT id_derechohabiente ",
                            "   FROM afi_derechohabiente ",
                            "  WHERE ", v_s_condicion       
             PREPARE prp_con_id FROM v_sqlqry              
             EXECUTE prp_con_id INTO v_id_derechohabiente
             DISPLAY "DERECHOHABIENTE ",  v_id_derechohabiente  

             SELECT COUNT(*) 
               INTO v_cta_reg
               FROM cre_uso_garantia 
              WHERE id_derechohabiente = v_id_derechohabiente               
                AND tpo_transferencia IN ('18','48')

             IF v_cta_reg = 0 THEN 
               CALL fn_mensaje("Aviso","El derechohabiente no tiene créditos otorgados","stop")
               LET v_bandera = FALSE  
             END IF          
         END IF 
         --LET v_bandera = TRUE          
      END IF

      IF v_bandera = TRUE THEN 
         --se llama la funcion que despliega la consulta
         --CALL fn_mensaje("Aviso","Llamdo a la función: fn_despliega_derechohab ","Info")
         CALL fn_despliega_derechohab(v_id_derechohabiente)
      END IF 
   END IF 
   CLOSE WINDOW w_op_derechohab
END FUNCTION

## Funcion que realiza la consulta de estado ##
FUNCTION fn_consulta_estado()
DEFINE v_s_condicion  STRING,
       estado         LIKE cre_his_acreditado.estado,
       edo_procesar   LIKE cre_his_acreditado.edo_procesar

   LET v_s_condicion = "1=1"

   OPEN WINDOW w_op_estado WITH FORM "GRTC014"
   CONSTRUCT v_s_condicion
          ON estado,
             edo_procesar
        FROM cmb_estado,
             cmb_procesar

      AFTER FIELD cmb_procesar
         NEXT FIELD cmb_estado
         
      ON ACTION ACCEPT
         CALL GET_FLDBUF(cmb_estado) RETURNING estado
         CALL GET_FLDBUF(cmb_procesar) RETURNING edo_procesar 
     
         IF estado IS NULL AND edo_procesar IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar al menos un campo","stop")
            CONTINUE CONSTRUCT                     
         END IF         
         EXIT CONSTRUCT 
      ON ACTION CANCEL 
         EXIT CONSTRUCT
   END CONSTRUCT

   CALL fn_despliega_estado(v_s_condicion)
   CLOSE WINDOW w_op_estado
END FUNCTION

## Funcion que realiza la consulta de historica ##
FUNCTION fn_consulta_historica()
DEFINE v_s_condicion        STRING,
       estado               LIKE cre_his_acreditado.estado,
       edo_procesar         LIKE cre_his_acreditado.edo_procesar,
       v_diagnostico        LIKE cre_his_acreditado.diagnostico,
       cb                   ui.ComboBox,
       v_sqlqry             STRING,       
       v_cve_diag           LIKE cre_his_acreditado.diagnostico,
       v_desc_diag          LIKE cat_rechazo.desc_rechazo,
       v_edo_procesar       LIKE cre_his_acreditado.edo_procesar   

   LET v_s_condicion = "1=1"

   OPEN WINDOW w_op_historica WITH FORM "GRTC016"
   CONSTRUCT v_s_condicion
          ON estado,
             edo_procesar,
             diagnostico
        FROM cmb_estado,
             cmb_procesar,
             cmb_diagnostico

      BEFORE CONSTRUCT 
          LET cb = ui.ComboBox.forName("cmb_diagnostico")   
          CALL cb.clear()
              LET v_sqlqry = " SELECT diagnostico, edo_procesar ",
                             "   FROM cre_uso_garantia ",
                             "  WHERE edo_procesar not in('10','05','30','60','120','210') ",
                             "    AND diagnostico IS NOT NULL ",
                             "  GROUP BY 1,2 "

          PREPARE prp_cons_diag FROM v_sqlqry        
          DECLARE cur_diagnostico CURSOR FOR prp_cons_diag
          FOREACH cur_diagnostico INTO v_cve_diag, v_edo_procesar
             LET v_desc_diag = fn_obt_desc_diagnostico(v_cve_diag,v_edo_procesar)
             CALL cb.addItem(v_cve_diag,v_cve_diag ||"-"||v_desc_diag)
          END FOREACH     
             
      AFTER FIELD cmb_diagnostico
         NEXT FIELD cmb_estado
         
      ON ACTION ACCEPT         
         CALL GET_FLDBUF(cmb_estado) RETURNING estado
         CALL GET_FLDBUF(cmb_procesar) RETURNING edo_procesar
         CALL GET_FLDBUF(cmb_diagnostico) RETURNING v_diagnostico
     
         IF estado IS NULL AND edo_procesar IS NULL AND  v_diagnostico IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar al menos un campo","stop")
            CONTINUE CONSTRUCT                     
         END IF         
         EXIT CONSTRUCT 
      ON ACTION CANCEL 
         EXIT CONSTRUCT
   END CONSTRUCT
   {IF estado IS NULL THEN 
      LET v_s_condicion = v_s_condicion, " AND estado = 10"
   END IF 
   IF edo_procesar IS NULL THEN 
      LET v_s_condicion = v_s_condicion, " AND edo_procesar IN (90,100,110,120)"
   END IF}
   DISPLAY "CONSTRUCT HISTORICA ", v_s_condicion
   CALL fn_despliega_historica(v_s_condicion)   
   CLOSE WINDOW w_op_historica
END FUNCTION

### Funcion que permite seleccionar el archivo a consultar
FUNCTION fn_consulta_archivo()
   DEFINE v_nom_archivo         LIKE cre_ctr_archivo.nom_archivo, --nombre del archivo          
          v_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo, --identificador del archivo
          v_id_cre_cmb          LIKE cre_ctr_archivo.id_cre_ctr_archivo, --identificador del archivo para el combo
          cb                    ui.ComboBox,
          v_sqlqry              STRING,
          v_id_proceso          LIKE cre_ctr_archivo.id_proceso

   OPEN WINDOW w_op_archivo WITH FORM "GRTC029"

   INPUT v_id_cre_ctr_archivo FROM cmb_archivo ATTRIBUTE (UNBUFFERED) 
      AFTER FIELD cmb_archivo
         NEXT FIELD cmb_archivo
     
      BEFORE INPUT
         LET v_id_proceso = g_id_proceso_grt_uso -- Uso de garantia 43 bis
         
         LET cb = ui.ComboBox.forName("cmb_archivo")   
         CALL cb.clear()
         --se consultan los archivos para el proceso correspondirnte 
         LET v_sqlqry = " SELECT nom_archivo, id_cre_ctr_archivo ",
                        "   FROM cre_ctr_archivo ",
                        "  WHERE id_proceso =  ",v_id_proceso,"\n",
                        "  ORDER BY f_proceso DESC"

          PREPARE prp_cons_arch FROM v_sqlqry        
          DECLARE cur_archivo CURSOR FOR prp_cons_arch
          --se llena el combo con los archivos existentes 
          FOREACH cur_archivo INTO v_nom_archivo, v_id_cre_cmb             
             CALL cb.addItem(v_id_cre_cmb,v_nom_archivo)
          END FOREACH     
             
         
      ON ACTION ACCEPT
         --se valida que se seleccione un archivo
         IF v_id_cre_ctr_archivo IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar el archivo a consultar","stop")
            CONTINUE INPUT
         ELSE
            SELECT nom_archivo
              INTO v_nom_archivo
              FROM cre_ctr_archivo
             WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo
               AND id_proceso = v_id_proceso
            
            CALL fn_despliega_archivo(v_id_cre_ctr_archivo, v_nom_archivo)   
         END IF           

      ON ACTION CANCEL 
         EXIT INPUT       
   END INPUT 
   CLOSE WINDOW w_op_archivo

END FUNCTION

###Funcion que muestra los registros encontrados para el archivo consultado
FUNCTION fn_despliega_archivo(p_id_cre_ctr_archivo, p_nom_archivo)
   DEFINE  p_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo, 
           p_nom_archivo         LIKE cre_ctr_archivo.nom_archivo, --nombre del archivo          
           v_sqlqry              STRING,
           v_cta_reg             INTEGER,
           v_indice              SMALLINT,
           v_estado              LIKE cre_acreditado.estado,
           v_edo_procesar        LIKE cre_acreditado.edo_procesar,
           v_rec_archivo          RECORD 
              estado             LIKE cre_acreditado.estado,
              edo_procesar       LIKE cre_acreditado.edo_procesar,
              total_cuentas      INTEGER 
           END RECORD,
           v_ar_archivo           DYNAMIC ARRAY OF RECORD 
              des_estado         LIKE cat_maq_credito.estado_desc,
              des_edo_procesar   LIKE cat_maq_credito.estado_desc,
              total_cuentas      INTEGER 
           END RECORD,
           --v_s_ruta_rpt_dest     STRING, -- ruta del archivo del reporte
           --v_s_ruta_rpt_orig     STRING, -- ruta del archivo del reporte
           f_w                   ui.form,
           w                     ui.window,
           v_existe_archivo      INTEGER,
           v_estado_aux          LIKE cre_acreditado.estado, 
           v_edo_procesar_aux    LIKE cre_acreditado.edo_procesar,
           v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin            LIKE seg_modulo.ruta_bin,
           v_nom_reporte         VARCHAR(80), -- nombre del reporte
           r_ruta_listados       LIKE seg_modulo.ruta_listados,
           v_indice_rep          SMALLINT    

   LET v_sqlqry = " SELECT COUNT(*)",
                  "   FROM cre_uso_garantia", 
                  "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo,
                  "    AND tpo_transferencia IN ('18','48')"
   
   PREPARE prp_cta_reg_arch FROM v_sqlqry
   EXECUTE prp_cta_reg_arch INTO v_cta_reg  

   OPEN WINDOW w_des_archivo WITH FORM "GRTC025"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()
      LET v_sqlqry = " SELECT estado,edo_procesar",
                     "   FROM cre_uso_garantia", 
                     "  WHERE id_cre_ctr_archivo = ",p_id_cre_ctr_archivo,
                     "    AND tpo_transferencia IN ('18','48')",
                     "  GROUP BY 1,2 "      
      
      PREPARE prp_cons_archivo FROM v_sqlqry
      DECLARE cur_arch CURSOR FOR prp_cons_archivo
      INITIALIZE  v_estado_aux, v_edo_procesar_aux TO NULL     
      
      LET v_indice = 1
      CALL v_ar_archivo.clear()
      OPEN cur_arch
      FOREACH cur_arch INTO v_estado, v_edo_procesar
         
         IF v_estado_aux = v_estado AND v_edo_procesar_aux = v_edo_procesar THEN 
            --DISPLAY "Salta registro"
         ELSE 
            LET v_estado_aux = v_estado
            LET v_edo_procesar_aux = v_edo_procesar
                     
            SELECT estado, edo_procesar, COUNT(*)
              INTO v_rec_archivo.estado, v_rec_archivo.edo_procesar,v_rec_archivo.total_cuentas
              FROM cre_uso_garantia
             WHERE estado = v_estado
               AND edo_procesar = v_edo_procesar
               AND id_cre_ctr_archivo = p_id_cre_ctr_archivo
               AND tpo_transferencia IN ('18','48')
             GROUP BY 1,2

            LET v_ar_archivo[v_indice].total_cuentas = v_rec_archivo.total_cuentas
            LET v_ar_archivo[v_indice].des_estado = fn_obt_desc_estado(v_rec_archivo.estado)
            LET v_ar_archivo[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_archivo.edo_procesar)
            LET v_indice = v_indice + 1
        END if  
      END FOREACH 
      FREE cur_arch
       
      DISPLAY ARRAY v_ar_archivo TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY 
            --CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
         IF v_cta_reg = 0 THEN 
            CALL fn_mensaje("Aviso","No existen registos para el archivo seleccionado","stop")
            EXIT DISPLAY
         END IF   

         ON ACTION ACCEPT
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("grt") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("GRTC104.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-GRTC024-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_archivo TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_archivo(v_ar_archivo[v_indice_rep].*, p_nom_archivo)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_archivo 

               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF
                              
               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','grt')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte

                 CALL ui.Interface.refresh()
                 CALL fn_mensaje("Aviso","Se ha generado el reporte por archivo","info") 
              ELSE
                 DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
              END IF      

            -- llamada a funcion que despliega el reporte en el EXPLORADOR de ejecucion
            --CALL ui.interface.frontCall("standard", "launchurl", [v_s_ruta_rpt_orig], [])
      END DISPLAY    
   CLOSE WINDOW w_des_archivo
   
END FUNCTION 


#### Funcion que muestra un listado de derechohabientes ### 
#### para elegir el que se desea consultar              ###
FUNCTION fn_obt_derechohabiente(p_s_condicion)
   DEFINE p_s_condicion         STRING,
          v_id_derechohabiente  LIKE afi_derechohabiente.id_derechohabiente,
          v_sqlqry              STRING,          
          v_indice              SMALLINT, 
          v_arr_derechohabiente DYNAMIC ARRAY OF RECORD 
             id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente,
             nss                LIKE afi_derechohabiente.nss,
             curp               LIKE afi_derechohabiente.curp,
             rfc                LIKE afi_derechohabiente.rfc,
             ind_nrp            LIKE afi_derechohabiente.ind_nrp,
             f_nacimiento       LIKE afi_derechohabiente.f_nacimiento,              
             nombre_imss        LIKE afi_derechohabiente.nombre_imss,  
             nombre_af          LIKE afi_derechohabiente.nombre_af,             
             ap_paterno_af      LIKE afi_derechohabiente.ap_materno_af,
             ap_materno_af      LIKE afi_derechohabiente.ap_paterno_af,
             tipo_trabajador    LIKE afi_derechohabiente.tipo_trabajador,
             origen_afiliacion  LIKE afi_derechohabiente.origen_afiliacion,
             id_credito         LIKE afi_derechohabiente.id_credito,
             f_credito          LIKE afi_derechohabiente.f_credito
          END RECORD    

   -- se inicializan variables
   LET v_indice = 1

   -- se realiza la consulta en el catalogo de derechohabientes para la condición de parametro
   LET v_sqlqry = " SELECT id_derechohabiente,nss,curp,rfc,ind_nrp,f_nacimiento, ",      
                  "        nombre_imss,nombre_af,ap_paterno_af,ap_materno_af,tipo_trabajador,",   
                  "        origen_afiliacion,id_credito,f_credito ",
                  "   FROM afi_derechohabiente ",
                  "  WHERE ", p_s_condicion 
   PREPARE prp_cons_derechohabiente FROM v_sqlqry
   DECLARE cur_derechohabiente CURSOR FOR prp_cons_derechohabiente
   FOREACH cur_derechohabiente INTO v_arr_derechohabiente[v_indice].*
      LET v_indice = v_indice + 1 
   END FOREACH    

   OPEN WINDOW w_list_derechohabiente WITH FORM "GRTC028" 

      DISPLAY ARRAY v_arr_derechohabiente TO tabla_derechohabiente.* ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )

         ON ACTION ACCEPT 
            LET v_indice = arr_curr()
            LET v_id_derechohabiente = v_arr_derechohabiente[v_indice].id_derechohabiente
            EXIT DISPLAY             
         ON ACTION CANCEL
            LET v_id_derechohabiente = -1 
            EXIT DISPLAY 

      END DISPLAY       
   CLOSE WINDOW w_list_derechohabiente
   RETURN v_id_derechohabiente
END FUNCTION 

### Funcion que muestra el resultado de la consulta de derechohabiente ###
FUNCTION fn_despliega_derechohab(p_id_derechohabiente)
  DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_curp                 LIKE afi_derechohabiente.curp,
         v_rfc                  LIKE afi_derechohabiente.rfc,
         v_ap_paterno           LIKE afi_derechohabiente.ap_paterno_af,
         v_ap_materno           LIKE afi_derechohabiente.ap_materno_af,
         v_nombre               LIKE afi_derechohabiente.nombre_af,
         v_tpo_credito          LIKE cre_acreditado.tpo_credito,
         v_num_credito          LIKE cre_acreditado.num_credito,
         v_f_otorga             LIKE cre_acreditado.f_otorga,
         v_estado               LIKE cre_acreditado.estado,
         v_des_estado           LIKE cat_maq_credito.estado_desc,
         v_edo_procesar         LIKE cre_acreditado.edo_procesar,
         v_des_procesar         LIKE cat_maq_credito.estado_desc,
         v_rec_his_transferencia RECORD
            --operacion           LIKE cre_his_acreditado.operacion,
            f_proceso           LIKE cre_his_acreditado.f_proceso,
            estado              LIKE cre_his_acreditado.estado,
            edo_procesar        LIKE cre_his_acreditado.edo_procesar,
            diagnostico         LIKE cre_his_acreditado.diagnostico,
            id_cre_ctr_archivo  LIKE cre_his_acreditado.id_cre_ctr_archivo
         END RECORD,
         v_ar_his_transferencia DYNAMIC ARRAY OF RECORD
            --operacion           LIKE cre_his_acreditado.operacion,
            f_proceso           LIKE cre_his_acreditado.f_proceso,
            des_estado          LIKE cat_maq_credito.estado_desc,
            des_edo_procesar    LIKE cat_maq_credito.estado_desc,
            des_diagnostico     LIKE cat_rechazo.desc_rechazo
         END RECORD,
         v_indice               SMALLINT,
         v_indice_deudor        SMALLINT,
         v_indice_rep           SMALLINT,
         v_rec_deudor           RECORD 
            movimiento          LIKE cre_saldo_deudor.movimiento,
            f_movimiento        LIKE cre_saldo_deudor.f_movimiento,
            monto_aivs          LIKE cre_saldo_deudor.monto_aivs,
            monto_pesos         LIKE cre_saldo_deudor.monto_pesos
         END RECORD,
         v_ar_deudor            DYNAMIC ARRAY OF RECORD 
            des_movimiento      LIKE cat_movimiento.movimiento_desc,
            f_movimiento        LIKE cre_saldo_deudor.f_movimiento,
            monto_aivs          LIKE cre_saldo_deudor.monto_aivs,
            monto_pesos         LIKE cre_saldo_deudor.monto_pesos
         END RECORD,
         v_tpo_enc         SMALLINT, -- tipo de encabezado que aparecerá en el reporte
         v_s_ruta_rpt_orig STRING, -- ruta del archivo del reporte
         v_s_ruta_rpt_dest STRING, -- ruta del archivo del reporte
         v_sqlqry               STRING,
         v_id_cre_acreditado    LIKE cre_acreditado.id_cre_acreditado,
         v_lsqry                CHAR(500),
         v_des_tpo_credito      LIKE cat_tipo_credito.desc_credito,
         v_manejador_rpt        OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
         r_ruta_bin             LIKE seg_modulo.ruta_bin,
         v_nom_reporte          VARCHAR(80), -- nombre del reporte
         f_w                   ui.form,
         w                     ui.window,
         v_existe_archivo      INTEGER,
         r_ruta_listados        LIKE seg_modulo.ruta_listados

  SELECT nss,curp,rfc,ap_paterno_af, ap_materno_af, nombre_af
    INTO v_nss,v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre
    FROM afi_derechohabiente 
   WHERE id_derechohabiente = p_id_derechohabiente 

  LET v_f_otorga = ""
  DISPLAY "v_nss,v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre: ", v_nss,v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre
  LET v_sqlqry = " SELECT FIRST 1 tpo_credito, num_credito,f_otorga,estado,edo_procesar, id_cre_acreditado ",  
                 "   FROM cre_acreditado ",
                 "  WHERE id_derechohabiente = ",p_id_derechohabiente,
                 "    AND tpo_originacion = ",g_tpo_originacion
  PREPARE prp_cons_transf FROM v_sqlqry
  EXECUTE prp_cons_transf INTO v_tpo_credito,v_num_credito,v_f_otorga,v_estado,v_edo_procesar, v_id_cre_acreditado

  SELECT estado_desc
    INTO v_des_estado
    FROM cat_maq_credito
   WHERE estado = v_estado

  SELECT estado_desc
    INTO v_des_procesar
    FROM cat_maq_credito
   WHERE estado = v_edo_procesar

   -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
   LET v_sqlqry = " SELECT FIRST 1 desc_credito\n",
                  "   FROM cat_tipo_credito\n",
                  "  WHERE tpo_credito = ",v_tpo_credito

   PREPARE prp_slctFrst_descCred FROM v_sqlqry
   EXECUTE prp_slctFrst_descCred INTO v_des_tpo_credito

  OPEN WINDOW w_des_derechohab WITH FORM "GRTC023"
     LET  w = ui.Window.getCurrent()
     LET  f_w = w.getForm()   
     DISPLAY p_id_derechohabiente,
             v_nss,               
             v_curp,              
             v_rfc,               
             v_ap_paterno,        
             v_ap_materno,       
             v_nombre,         
             v_des_tpo_credito,       
             v_num_credito USING "&&&&&&&&&&",       
             v_f_otorga,          
             v_des_estado,            
             v_des_procesar
          TO id_derechohabiente,
             nss,               
             curp,              
             rfc,               
             app_paterno,        
             app_materno,        
             nombre,         
             tpo_credito,       
             num_credito,       
             f_otorga,          
             estado,            
             edo_procesar           

     DIALOG ATTRIBUTES (FIELD ORDER FORM, UNBUFFERED)     
        DISPLAY ARRAY v_ar_his_transferencia TO tabla_trans.*
           BEFORE DISPLAY               
              LET v_lsqry = "SELECT f_proceso,estado,edo_procesar,diagnostico",
                            "  FROM cre_uso_garantia ",
                            " WHERE id_derechohabiente = ", p_id_derechohabiente,
                            "   AND tpo_transferencia IN ('18','48')"

              PREPARE prp_cons_his FROM v_lsqry
              DECLARE cur_his_trans CURSOR FOR prp_cons_his              

              DISPLAY "CONSULTA HIST ", v_lsqry
              LET v_indice = 1

              OPEN cur_his_trans
              FOREACH cur_his_trans INTO v_rec_his_transferencia.*
                 LET v_ar_his_transferencia[v_indice].f_proceso = v_rec_his_transferencia.f_proceso
                 LET v_ar_his_transferencia[v_indice].des_estado = fn_obt_desc_estado(v_rec_his_transferencia.estado)
                 LET v_ar_his_transferencia[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_his_transferencia.edo_procesar)
                 LET v_ar_his_transferencia[v_indice].des_diagnostico = fn_obt_desc_diagnostico(v_rec_his_transferencia.diagnostico, v_rec_his_transferencia.edo_procesar)

                 LET v_indice = v_indice + 1
              END FOREACH 
              FREE cur_his_trans        
              {
              LET v_indice_deudor = 1
              LET v_lsqry = " SELECT movimiento,f_movimiento,monto_aivs,monto_pesos ",
                            " FROM cre_saldo_deudor ",
                            " WHERE id_cre_acreditado = ",v_id_cre_acreditado,
                            "   AND id_cre_acreditado IN(SELECT id_cre_acreditado ",
                            "  FROM cre_acreditado WHERE tpo_originacion = ",g_tpo_originacion,")"   
                            
              PREPARE prp_cons_deudor FROM v_lsqry
              DECLARE cur_deudor CURSOR FOR prp_cons_deudor

              DISPLAY "CONSULTA DEUDOR ",v_lsqry              
              OPEN cur_deudor
              FOREACH cur_deudor INTO v_rec_deudor.*

                 SELECT movimiento_desc
                 INTO v_ar_deudor[v_indice_deudor].des_movimiento
                 FROM cat_movimiento 
                 WHERE movimiento = v_rec_deudor.movimiento

                 LET v_ar_deudor[v_indice_deudor].f_movimiento = v_rec_deudor.f_movimiento --USING "dd-mm-yyyy"                     
                 LET v_ar_deudor[v_indice_deudor].monto_aivs = v_rec_deudor.monto_aivs
                 LET v_ar_deudor[v_indice_deudor].monto_pesos = v_rec_deudor.monto_pesos
                 
                 LET v_indice_deudor = v_indice_deudor + 1     
              END FOREACH 

              FREE cur_deudor}
        END DISPLAY

{        DISPLAY ARRAY v_ar_deudor TO tabla_deudor.*
        END DISPLAY}

        ON ACTION ACCEPT 
           EXIT DIALOG  

        ON ACTION cancelar
           EXIT DIALOG  

        ON ACTION reporte
           # Recupera la ruta de listados en el que se enviara el archivo
           --CALL fn_rutas("acr") RETURNING r_ruta_bin, r_ruta_listados
           CALL fn_rutas("grt") RETURNING r_ruta_bin, r_ruta_listados

           # Se indica que el reporte usara la plantilla creada
           --IF fgl_report_loadCurrentSettings("ACRC101.4rp") THEN 
           IF fgl_report_loadCurrentSettings("GRTC101.4rp") THEN 
              CALL fgl_report_selectDevice("PDF") 
              --LET v_nom_reporte = p_usuario_cod CLIPPED || "-ACRC101-","00000","-","00000","-","00000" 
              LET v_nom_reporte = p_usuario_cod CLIPPED || "-GRTC021-","00000","-","00000","-","00000" 
              CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

              -- sin preview
              CALL fgl_report_selectPreview(0)

              LET v_manejador_rpt = fgl_report_commitCurrentSettings()

              # Inicia el reporte de consulta historica por derechohabiente
              START REPORT reporte_cons_derechohabiente TO XML HANDLER v_manejador_rpt

              -- se procesan los registro de cre historico
              FOR v_indice_rep = 1 TO v_indice -1
                 -- indica que el encabezado debe ser el de historico
                 LET v_tpo_enc = 1
                 OUTPUT TO REPORT reporte_cons_derechohabiente(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                               v_ap_paterno, v_ap_materno, v_nombre,
                                                               v_des_tpo_credito, v_num_credito, v_f_otorga,
                                                               v_ar_his_transferencia[v_indice_rep].*,
                                                               v_ar_deudor[1].*, v_tpo_enc)
              END FOR

              -- se procesan los registro de cre deudor
              FOR v_indice_rep = 1 TO v_indice_deudor -1
                 -- indica que el encabezado debe ser el de deudor
                 LET v_tpo_enc = 2
                 OUTPUT TO REPORT reporte_cons_derechohabiente(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                               v_ap_paterno, v_ap_materno, v_nombre,
                                                               v_des_tpo_credito, v_num_credito, v_f_otorga,
                                                               v_ar_his_transferencia[1].*,
                                                               v_ar_deudor[v_indice_rep].*, v_tpo_enc)
              END FOR

              #Finaliza el reporte
              FINISH REPORT reporte_cons_derechohabiente 
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF
                            
               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','grt')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()                
              CALL fn_mensaje("Aviso","Se ha generado el reporte por derechohabiente","info") 
           ELSE
              DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
           END IF 

           CONTINUE DIALOG
     END DIALOG  
  CLOSE WINDOW w_des_derechohab
END FUNCTION 

### funcion que despliega la informacion de la consulta por estado ###
FUNCTION fn_despliega_estado(p_s_condicion)
   DEFINE  p_s_condicion       STRING, 
           v_sqlqry            STRING,
           v_cta_reg           INTEGER,
           v_indice            SMALLINT,
           v_estado            LIKE cre_acreditado.estado,
           v_edo_procesar      LIKE cre_acreditado.edo_procesar,
           v_rec_estado        RECORD 
              estado           LIKE cre_acreditado.estado,
              edo_procesar     LIKE cre_acreditado.edo_procesar,
              total_cuentas    INTEGER 
           END RECORD,
           v_ar_estado         DYNAMIC ARRAY OF RECORD 
              des_estado       LIKE cat_maq_credito.estado_desc,
              des_edo_procesar LIKE cat_maq_credito.estado_desc,
              total_cuentas    INTEGER 
           END RECORD,
           v_estado_aux        LIKE cre_acreditado.estado, 
           v_edo_procesar_aux  LIKE cre_acreditado.edo_procesar,
           v_manejador_rpt     OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin          LIKE seg_modulo.ruta_bin,
           v_nom_reporte       VARCHAR(80), -- nombre del reporte
           r_ruta_listados     LIKE seg_modulo.ruta_listados,
           f_w                 ui.form,
           w                   ui.window,
           v_existe_archivo    INTEGER, 
           v_indice_rep        SMALLINT    

   LET v_sqlqry = " SELECT COUNT (*)",
                  "   FROM cre_uso_garantia", 
                  "  WHERE ",p_s_condicion,
                  "    AND tpo_transferencia IN ('18','48')"
   
   PREPARE prp_cta_reg FROM v_sqlqry
   EXECUTE prp_cta_reg INTO v_cta_reg  

   OPEN WINDOW w_des_estado WITH FORM "GRTC025"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()  
      LET v_sqlqry = " SELECT estado,edo_procesar",
                     "   FROM cre_uso_garantia", 
                     "  WHERE ",p_s_condicion,
                     "    AND tpo_transferencia IN ('18','48')",
                     "  GROUP BY 1,2"
      
      PREPARE prp_cons_edos FROM v_sqlqry
      DECLARE cur_edos CURSOR FOR prp_cons_edos
      INITIALIZE  v_estado_aux, v_edo_procesar_aux TO NULL     
      
      LET v_indice = 1
      CALL v_ar_estado.clear()
      OPEN cur_edos
      FOREACH cur_edos INTO v_estado, v_edo_procesar
         
         IF v_estado_aux = v_estado AND v_edo_procesar_aux = v_edo_procesar THEN 
            --DISPLAY "Salta registro"
         ELSE 
            LET v_estado_aux = v_estado
            LET v_edo_procesar_aux = v_edo_procesar
                     
            SELECT estado, edo_procesar, COUNT(*)
              INTO v_rec_estado.estado, v_rec_estado.edo_procesar,v_rec_estado.total_cuentas
              FROM cre_uso_garantia
             WHERE estado = v_estado_aux            
               AND edo_procesar = v_edo_procesar_aux
               AND tpo_transferencia IN ('18','48')
            GROUP BY 1,2

            LET v_ar_estado[v_indice].total_cuentas = v_rec_estado.total_cuentas
            LET v_ar_estado[v_indice].des_estado = fn_obt_desc_estado(v_rec_estado.estado)
            LET v_ar_estado[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_estado.edo_procesar)
            LET v_indice = v_indice + 1
        END if  
      END FOREACH 
      FREE cur_edos
       
      DISPLAY ARRAY v_ar_estado TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY 
         IF v_cta_reg = 0 THEN 
            CALL fn_mensaje("Aviso","No existen registos para los criterios seleccionados","stop")
            EXIT DISPLAY
         END IF   

         ON ACTION ACCEPT
            CALL v_ar_estado.CLEAR() 
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_estado.CLEAR() 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            --CALL fn_rutas("acr") RETURNING r_ruta_bin, r_ruta_listados
            CALL fn_rutas("grt") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            --IF fgl_report_loadCurrentSettings("ACRC102.4rp") THEN 
            IF fgl_report_loadCurrentSettings("GRTC102.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               --LET v_nom_reporte = p_usuario_cod CLIPPED || "-ACRC102-","00000","-","00000","-","00000" 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-GRTC022-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_estado TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_estado(v_ar_estado[v_indice_rep].*)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_estado 
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

                              
               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','grt')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()     

                 CALL fn_mensaje("Aviso","Se ha generado el reporte por estado","info") 
            ELSE
                 DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF      
      END DISPLAY    
   CLOSE WINDOW w_des_estado
   
END FUNCTION 

### Funcion que despliega la consulta historica ###
FUNCTION fn_despliega_historica(p_s_condicion)
   DEFINE  p_s_condicion          STRING, 
           v_sqlqry               STRING,
           v_cta_reg              INTEGER,
           v_indice               SMALLINT,
           v_id_cre_ctr_archivo   LIKE cre_his_acreditado.id_cre_ctr_archivo,            
           v_rec_historica        RECORD 
              estado              LIKE cre_his_acreditado.estado,
              edo_procesar        LIKE cre_his_acreditado.edo_procesar,
              diagnostico         LIKE cre_his_acreditado.diagnostico,              
              total_cuentas       INTEGER,
              id_cre_ctr_archivo  LIKE cre_his_acreditado.id_cre_ctr_archivo 
           END RECORD,
           v_ar_historica         DYNAMIC ARRAY OF RECORD 
              des_estado          LIKE cat_maq_credito.estado_desc,
              des_edo_procesar    LIKE cat_maq_credito.estado_desc,
              des_diagnostico     LIKE cat_rechazo.desc_rechazo,              
              total_cuentas       INTEGER 
           END RECORD,           
           v_manejador_rpt        OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin             LIKE seg_modulo.ruta_bin,
           v_nom_reporte          VARCHAR(80), -- nombre del reporte
           r_ruta_listados        LIKE seg_modulo.ruta_listados,
           f_w                    ui.form,
           w                      ui.window,
           v_existe_archivo       INTEGER,
           v_indice_rep           SMALLINT               
   
   LET v_sqlqry = " SELECT COUNT (*)",
                  "   FROM cre_uso_garantia", 
                  "  WHERE ",p_s_condicion,
                  "    AND tpo_transferencia IN ('18','48')"
   
   PREPARE prp_cta_reg1 FROM v_sqlqry
   EXECUTE prp_cta_reg1 INTO v_cta_reg  

   OPEN WINDOW w_des_historica WITH FORM "GRTC027"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()  
      LET v_sqlqry = " SELECT estado,edo_procesar,diagnostico, count(*) ",
                     "   FROM cre_uso_garantia", 
                     "  WHERE ",p_s_condicion,
                     "    AND tpo_transferencia IN ('18','48')",
                     "  GROUP BY 1,2,3",
                     "  ORDER BY 1,2,3"      

                     DISPLAY "CONSULTA HISTORICA ", v_sqlqry
      PREPARE prp_cons_hist FROM v_sqlqry
      DECLARE cur_hist CURSOR FOR prp_cons_hist      
      
      LET v_indice = 1
      OPEN cur_hist
      --FOREACH cur_hist INTO v_estado, v_edo_procesar, v_diagnostico,v_id_cre_ctr_archivo
      FOREACH cur_hist INTO v_rec_historica.estado, v_rec_historica.edo_procesar, v_rec_historica.diagnostico,v_rec_historica.total_cuentas         

         LET v_ar_historica[v_indice].total_cuentas = v_rec_historica.total_cuentas
         LET v_ar_historica[v_indice].des_estado = fn_obt_desc_estado(v_rec_historica.estado)
         LET v_ar_historica[v_indice].des_edo_procesar = fn_obt_desc_estado(v_rec_historica.edo_procesar)
         LET v_ar_historica[v_indice].des_diagnostico = fn_obt_desc_diagnostico(v_rec_historica.diagnostico, v_rec_historica.edo_procesar)
         LET v_indice = v_indice + 1
           
      END FOREACH 
      FREE cur_hist
       
      DISPLAY ARRAY v_ar_historica TO tabla_historica.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY 
         IF v_cta_reg = 0 THEN 
            CALL fn_mensaje("Aviso","No existen registos para los criterios seleccionados","stop")
            EXIT DISPLAY
         END IF   

         ON ACTION ACCEPT
            CALL v_ar_historica.CLEAR() 
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_historica.CLEAR() 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            --CALL fn_rutas("acr") RETURNING r_ruta_bin, r_ruta_listados
            CALL fn_rutas("grt") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            --IF fgl_report_loadCurrentSettings("ACRC103.4rp") THEN 
            IF fgl_report_loadCurrentSettings("GRTC103.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               --LET v_nom_reporte = p_usuario_cod CLIPPED || "-ACRC103-","00000","-","00000","-","00000" 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-GRTC023-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_historica TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_historica(v_ar_historica[v_indice_rep].*)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_historica 
               LET v_existe_archivo = 1

               IF(LENGTH(r_ruta_listados) > 0)THEN
                  # se revisa si existe el archivo en la ruta de listados
                  CALL os.Path.exists(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED||".pdf") RETURNING v_existe_archivo
               END IF

               # si no existe el archivo, se oculta la imagen link que visualiza el pdf
               IF NOT(v_existe_archivo)THEN
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",1)
               ELSE
                  CALL f_w.setElementHidden("formonly.lbl_ruta_reporte",0)
               END IF

                              
               # muestra una imagen(PDF) link que visualiza el reporte de la operacion en cuetion
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','grt')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()  
                 CALL fn_mensaje("Aviso","Se ha generado el reporte de consulta histórica","info") 
            ELSE
                 DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF       
      END DISPLAY    
   CLOSE WINDOW w_des_historica
   
END FUNCTION 

## Funcion que obtiene la descripcion de estado y estado procesar
FUNCTION fn_obt_desc_estado(p_cve_estado)
   DEFINE p_cve_estado LIKE cat_maq_credito.estado,
          v_des_estado LIKE cat_maq_credito.estado_desc

   SELECT estado_desc
     INTO v_des_estado
     FROM cat_maq_credito
    WHERE estado = p_cve_estado

   RETURN v_des_estado

END FUNCTION 

##OBJETIVO: Obtiene la descripcion para el diagnostico
FUNCTION fn_obt_desc_diagnostico(p_diagnostico, p_edo_procesar)
   DEFINE p_diagnostico        LIKE cre_his_acreditado.diagnostico,
          p_edo_procesar       LIKE cre_his_acreditado.edo_procesar,
          v_desc_diagnostico   LIKE cat_rechazo.desc_rechazo,          
          v_tpo_rechazo        LIKE cat_rechazo.tpo_rechazo
   
   CASE 
      WHEN (p_edo_procesar = 40 OR p_edo_procesar = 50  OR p_edo_procesar = 90 OR 
            p_edo_procesar =110 OR p_edo_procesar = 190 OR p_edo_procesar = 200)
         LET v_tpo_rechazo = "RCH"      
      WHEN p_edo_procesar = 100   
         LET v_tpo_rechazo = "DEV"
      WHEN p_edo_procesar = 150
         LET v_tpo_rechazo = "SIS"
      OTHERWISE 
         LET v_tpo_rechazo = NULL     
   END CASE    

   IF p_diagnostico = 0 THEN 
      LET v_desc_diagnostico = "N/A"
   ELSE      
      IF v_tpo_rechazo IS NOT NULL THEN
         SELECT desc_rechazo
           INTO v_desc_diagnostico
           FROM cat_rechazo
          WHERE tpo_rechazo = v_tpo_rechazo
            AND cod_rechazo = p_diagnostico
      END IF
   END IF    
   IF v_desc_diagnostico IS NULL THEN
      LET v_desc_diagnostico = "N/A"
   END IF 

   RETURN v_desc_diagnostico       
END FUNCTION   

#OBJETIVO: Genera el reporte de consulta por derechohabiente
REPORT reporte_cons_derechohabiente(p_id_derechohabiente,p_nss,p_curp,p_rfc,p_ap_paterno,
                                    p_ap_materno,p_nombre,p_des_tpo_credito,p_num_credito,
                                    p_f_otorga,p_rec_his_transferencia,p_rec_deudor,p_tpo_enc)
DEFINE p_id_derechohabiente     INTEGER ,
       p_nss                    LIKE afi_derechohabiente.nss,
       p_curp                   LIKE afi_derechohabiente.curp,
       p_rfc                    LIKE afi_derechohabiente.rfc,
       p_ap_paterno             LIKE afi_derechohabiente.ap_paterno_af,
       p_ap_materno             LIKE afi_derechohabiente.ap_materno_af,
       p_nombre                 LIKE afi_derechohabiente.nombre_af,
       p_des_tpo_credito        LIKE cat_tipo_credito.desc_credito,
       p_num_credito            LIKE cre_acreditado.num_credito,
       p_f_otorga               LIKE cre_acreditado.f_otorga,
       p_rec_his_transferencia  RECORD            
            f_proceso           LIKE cre_his_acreditado.f_proceso,
            des_estado          LIKE cat_maq_credito.estado_desc,
            des_edo_procesar    LIKE cat_maq_credito.estado_desc,
            diagnostico         LIKE cre_his_acreditado.diagnostico
       END RECORD,
       p_rec_deudor             RECORD 
            des_movimiento      LIKE cat_movimiento.movimiento_desc,
            f_movimiento        LIKE cre_saldo_deudor.f_movimiento,
            monto_aivs          LIKE cre_saldo_deudor.monto_aivs,
            monto_pesos         LIKE cre_saldo_deudor.monto_pesos
       END RECORD,
       p_tpo_enc                SMALLINT, -- tipo de encabezado que aparecerá en el reporte
       v_v_desc_detalle         VARCHAR(50),
       v_fecha_reporte          DATE   

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod
      PRINTX p_id_derechohabiente
      PRINTX p_nss               
      PRINTX p_curp              
      PRINTX p_rfc               
      PRINTX p_ap_paterno        
      PRINTX p_ap_materno        
      PRINTX p_nombre            
      PRINTX p_des_tpo_credito   
      PRINTX p_num_credito USING "&&&&&&&&&&"     
      PRINTX p_f_otorga USING "dd-mm-yyyy"                     

   BEFORE GROUP OF p_tpo_enc
      IF p_tpo_enc = 1 THEN
         LET v_v_desc_detalle = "TRANSFERENCIA"
      ELSE
         LET v_v_desc_detalle = "DEUDOR"
      END IF
      PRINT p_tpo_enc
      PRINTX v_v_desc_detalle

   ON EVERY ROW
      PRINTX p_rec_his_transferencia.f_proceso USING "dd-mm-yyyy" 
      PRINTX p_rec_his_transferencia.des_estado
      PRINTX p_rec_his_transferencia.des_edo_procesar
      PRINTX p_rec_his_transferencia.diagnostico
      PRINTX p_rec_deudor.des_movimiento
      PRINTX p_rec_deudor.f_movimiento USING "dd-mm-yyyy"
      PRINTX p_rec_deudor.monto_aivs
      PRINTX p_rec_deudor.monto_pesos
      
END REPORT

#OBJETIVO: Genera el reporte de consulta por estado
REPORT reporte_cons_estado(p_rec_estado)
DEFINE p_rec_estado        RECORD 
          des_estado       LIKE cat_maq_credito.estado_desc,
          des_edo_procesar LIKE cat_maq_credito.estado_desc,
          total_cuentas    INTEGER 
       END RECORD,
       v_fecha_reporte      DATE        

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod
                    
   ON EVERY ROW
      PRINTX p_rec_estado.*      
END REPORT

#OBJETIVO: Genera el reporte de consulta historica
REPORT reporte_cons_historica(p_rec_historica)
DEFINE p_rec_historica      RECORD 
          des_estado        LIKE cat_maq_credito.estado_desc,
          des_edo_procesar  LIKE cat_maq_credito.estado_desc,
          des_diagnostico   LIKE cat_rechazo.desc_rechazo,              
          total_cuentas     INTEGER 
       END RECORD,
       v_fecha_reporte      DATE        

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod
                    
   ON EVERY ROW
      PRINTX p_rec_historica.*      
END REPORT

#OBJETIVO: Genera el reporte de consulta por archivo
REPORT reporte_cons_archivo(p_rec_archivo, p_nom_archivo)
DEFINE p_rec_archivo        RECORD 
          des_estado        LIKE cat_maq_credito.estado_desc,
          des_edo_procesar  LIKE cat_maq_credito.estado_desc,
          total_cuentas     INTEGER 
       END RECORD,
       v_fecha_reporte      DATE,
       p_nom_archivo        LIKE cre_ctr_archivo.nom_archivo --nombre del archivo                 

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod
      PRINTX p_nom_archivo
                    
   ON EVERY ROW
      PRINTX p_rec_archivo.*      
END REPORT