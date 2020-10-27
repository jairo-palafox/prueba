--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRC07                                        #
#Objetivo          =>Programa que realiza la consulta de rechazos  #
#                    de anualidades garantizadas                   #
#Autor             =>Mauricio Sanchez, EFP                         #
#Fecha inicio      =>23 JUNIO 2012                                 #
####################################################################

IMPORT OS
DATABASE safre_viv
GLOBALS "AGRG01.4gl"
GLOBALS
DEFINE g_pid             LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod     LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod       LIKE cat_operacion.opera_cod, -- codigo de operacion
       p_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       g_tpo_originacion LIKE cre_acreditado.tpo_originacion --tipo de originacion 
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   LET g_tpo_originacion = 4 -- Anualidades Garantizadas

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".AGRC07.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- se invoca la funcion general de consulta de rechazos de acreditados
   CALL fn_consulta_rechazos_agr()
END MAIN

## ventana que muestra las opciones para consultas de rechazos de creditos en garantia ##
FUNCTION fn_consulta_rechazos_agr()
   DEFINE v_op_consulta CHAR(4)

   OPEN WINDOW w_op_consulta WITH FORM "AGRC071"

   INPUT v_op_consulta FROM opciones ATTRIBUTE (UNBUFFERED)

      BEFORE INPUT
         -- se asume la opcion para consulta por NSS
         LET v_op_consulta = "opt1"


      ON ACTION ACCEPT
         -- consulta por NSS
         IF v_op_consulta = "opt1" THEN           
           CALL fn_consulta_derechohabiente()
         END IF
         
         -- consulta por archivo
         IF v_op_consulta = "opt2" THEN         
           CALL fn_consulta_archivo()
           
         END IF
         
         -- consulta por fecha de proceso
         IF v_op_consulta = "opt3" THEN         
           CALL fn_consulta_fecha()
         END IF
         
         -- consulta por tipo de rechazo
         IF v_op_consulta = "opt4" THEN         
           CALL fn_consulta_tipo_rechazo()
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
   
   OPEN WINDOW w_op_derechohab WITH FORM "AGRC072"   
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
         LET v_nombre      = GET_FLDBUF(nombre)
         LET v_nss         = GET_FLDBUF(nss)
         LET v_curp        = GET_FLDBUF(curp)
         LET v_rfc         = GET_FLDBUF(rfc)         
       
         IF ( v_app_paterno IS NULL AND v_app_materno IS NULL AND v_nombre IS NULL ) AND 
            v_nss IS NULL AND v_curp IS NULL AND v_rfc IS NULL THEN 
            CALL fn_mensaje("Aviso","Se requiere capturar al menos un campo","stop")
            CONTINUE CONSTRUCT
         END IF             
      
         IF ( v_app_paterno IS NOT NULL AND v_nombre IS NULL ) THEN
            CALL fn_mensaje("Aviso","Se requiere capturar el Nombre del derechohabiente","stop")
            CONTINUE CONSTRUCT 
         END IF

         IF ( v_app_paterno IS NULL AND v_nombre IS NOT NULL ) THEN
            CALL fn_mensaje("Aviso","Se requiere capturar el Apellido paterno del derechohabiente","stop")
            CONTINUE CONSTRUCT 
         END IF

         EXIT CONSTRUCT  

      ON ACTION CANCEL 
         LET v_bandera = FALSE 
         EXIT CONSTRUCT
   END CONSTRUCT     

    DISPLAY "Condicion: "
    DISPLAY v_s_condicion

   -- si no se cancelo el construct
   IF ( v_bandera = TRUE ) THEN 
      IF v_nss IS NULL THEN
         -- se crea sentencia que verifica si existe el registro en afi
         LET v_sqlqry = " SELECT COUNT(*)",
                        "   FROM afi_derechohabiente ",
                        "  WHERE ",v_s_condicion
   
         PREPARE prp_cta_nombre FROM v_sqlqry              
         EXECUTE prp_cta_nombre INTO v_cta_nombre

         IF ( v_cta_nombre = 0 ) THEN 
            CALL fn_mensaje("Aviso","No existe información del derechohabiente","stop")

            LET v_bandera = FALSE 
         ELSE
            -- se verifica si aparece una o mas ocasiones en los rechazos
            IF ( v_cta_nombre > 1 ) THEN
               -- consulta listado de derechohabientes de afi para obtener el id derechohab
               CALL fn_obt_derechohabiente(v_s_condicion) RETURNING v_nss

               -- si se cancelo la consulta
               IF ( v_nss IS NULL ) THEN
                  LET v_bandera = FALSE  
               END IF
            ELSE
               -- se obtiene el nss de afi_derechohabiente
               LET v_sqlqry = " SELECT nss\n",
                              "   FROM afi_derechohabiente a\n",
                              "  WHERE ",v_s_condicion,"\n"

               PREPARE prp_cta_nss FROM v_sqlqry
               EXECUTE prp_cta_nss INTO v_nss
            END IF
         END IF
      END IF

      IF v_bandera THEN
         -- se verifica cuantas veces aparece como rechazado
         LET v_sqlqry = " SELECT COUNT(*)\n",
                        "   FROM cre_rch_acreditado\n",
                        "  WHERE nss = '",v_nss CLIPPED,"'\n",
                        "    AND tpo_originacion = ",g_tpo_originacion

         PREPARE prp_cuenta_nss FROM v_sqlqry
         EXECUTE prp_cuenta_nss INTO v_cta_nombre

         IF v_cta_nombre = 0 THEN
            CALL fn_mensaje("Aviso","No se encontraron registros rechazados con los criterios dados","stop")

            LET v_bandera = FALSE
         ELSE
            -- se llama la funcion que despliega la consulta
            CALL fn_despliega_por_derechohab(v_nss)
         END IF
      END IF
   END IF
   CLOSE WINDOW w_op_derechohab
END FUNCTION


### Funcion que permite seleccionar el archivo a consultar
FUNCTION fn_consulta_archivo()
   DEFINE v_nom_archivo         LIKE cre_ctr_archivo.nom_archivo, --nombre del archivo          
          v_id_cre_ctr_archivo  LIKE cre_ctr_archivo.id_cre_ctr_archivo, --identificador del archivo
          v_id_cre_cmb          LIKE cre_ctr_archivo.id_cre_ctr_archivo, --identificador del archivo para el combo
          cb                    ui.ComboBox,
          v_sqlqry              STRING

   OPEN WINDOW w_op_archivo WITH FORM "AGRC075"

   INPUT v_id_cre_ctr_archivo FROM cmb_archivo ATTRIBUTE (UNBUFFERED) 
      AFTER FIELD cmb_archivo
         NEXT FIELD cmb_archivo
     
      BEFORE INPUT
         LET cb = ui.ComboBox.forName("cmb_archivo")   
         CALL cb.clear()

         --se consultan los archivos para el proceso correspondirnte 
         LET v_sqlqry = " SELECT nom_archivo, id_cre_ctr_archivo\n",
                        "   FROM cre_ctr_archivo\n",
                        "  WHERE id_proceso IN (201,",g_id_proceso_agr,")\n",
                        "    AND operacion = 21\n" -- recurrente
                                                    
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
               AND id_proceso IN (201,g_id_proceso_agr)
            
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
           v_rec_archivo         RECORD 
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE,
              nom_archivo        VARCHAR(40)
           END RECORD,
           v_ar_archivo          DYNAMIC ARRAY OF RECORD 
              nom_archivo        VARCHAR(40),
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE
           END RECORD,
           v_estado_aux          LIKE cre_acreditado.estado, 
           v_edo_procesar_aux    LIKE cre_acreditado.edo_procesar,
           v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin            LIKE seg_modulo.ruta_bin,
           v_nom_reporte         VARCHAR(80), -- nombre del reporte
           r_ruta_listados       LIKE seg_modulo.ruta_listados,
           f_w                   ui.form,
           w                     ui.window,                   
           v_indice_rep          SMALLINT,
           v_existe_archivo       INTEGER             

   -- registros rechazados del archivo elegido
   LET v_sqlqry = " SELECT COUNT (*)\n",
                  "   FROM cre_rch_acreditado\n", 
                  "  WHERE id_cre_ctr_archivo = ", p_id_cre_ctr_archivo,
                  "    AND tpo_originacion = ",g_tpo_originacion
   
   PREPARE prp_cta_reg_arch FROM v_sqlqry
   EXECUTE prp_cta_reg_arch INTO v_cta_reg  

   OPEN WINDOW w_des_archivo WITH FORM "AGRC076"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()    
      LET v_sqlqry = " SELECT b.desc_estado, c.originacion_desc,\n",
                     "        SUM(a.sdo_deudor), d.f_proceso, d.nom_archivo\n",
                     "   FROM cre_rch_acreditado a,\n", 
                     "        cat_rch_acreditado b,\n",
                     "        cat_cre_originacion c,\n",
                     "        cre_ctr_archivo d\n",
                     "  WHERE a.id_cre_ctr_archivo = ",p_id_cre_ctr_archivo, "\n",
                     "    AND a.id_cre_ctr_archivo = d.id_cre_ctr_archivo\n",
                     "    AND a.estado = b.estado\n",
                     "    AND a.tpo_originacion = ",g_tpo_originacion,
                     "    AND a.tpo_originacion = c.tpo_originacion\n",
                     "  GROUP BY 1,2,4,5\n",
                     " ORDER BY 5,1,2,4"
      
      PREPARE prp_cons_archivo FROM v_sqlqry
      DECLARE cur_arch CURSOR FOR prp_cons_archivo
      INITIALIZE  v_estado_aux, v_edo_procesar_aux TO NULL     
      
      LET v_indice = 1
      CALL v_ar_archivo.clear()
      OPEN cur_arch
      FOREACH cur_arch INTO v_rec_archivo.*         
         LET v_ar_archivo[v_indice].sum_saldo_deudor = v_rec_archivo.sum_saldo_deudor
         LET v_ar_archivo[v_indice].estado           = v_rec_archivo.estado
         LET v_ar_archivo[v_indice].tpo_originacion  = v_rec_archivo.tpo_originacion
         LET v_ar_archivo[v_indice].f_proceso        = v_rec_archivo.f_proceso
         LET v_ar_archivo[v_indice].nom_archivo      = v_rec_archivo.nom_archivo
         LET v_indice = v_indice + 1
      END FOREACH 
      FREE cur_arch
       
      DISPLAY ARRAY v_ar_archivo TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         BEFORE DISPLAY 
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
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC071.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC072-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_archivo TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_archivo(v_ar_archivo[v_indice_rep].*)
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
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte

              CALL ui.Interface.refresh() 
              CALL fn_mensaje("Aviso","Se ha generado el reporte por archivo","info") 
            ELSE
              DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF      
              
      END DISPLAY    
   CLOSE WINDOW w_des_archivo
   
END FUNCTION

### Funcion que permite seleccionar la fecha de proceso para consultar
FUNCTION fn_consulta_fecha()
   DEFINE v_fecha_proceso       DATE, -- fecha de proceso
          v_sqlqry              STRING,
          v_conteo              SMALLINT

   OPEN WINDOW w_op_fecha WITH FORM "AGRC077"

   -- se inicia sin fecha
   LET v_fecha_proceso = NULL

   INPUT v_fecha_proceso FROM f_proceso
   ATTRIBUTES (UNBUFFERED) 
        
      ON ACTION ACCEPT
         --se valida que se seleccione un archivo
         IF ( v_fecha_proceso IS NULL ) THEN 
            CALL fn_mensaje("Aviso","Es necesario capturar una fecha","stop")
            CONTINUE INPUT
         ELSE
            SELECT COUNT(*)
              INTO v_conteo
              FROM cre_ctr_archivo
             WHERE f_proceso = v_fecha_proceso
            
            -- si no hay al menos un archivo no se muestran datos
            IF ( v_conteo < 1 ) THEN
               CALL fn_mensaje("Atención","No se tienen archivos cargados en la fecha dada","info")
               CONTINUE INPUT
            END IF
            
            CALL fn_despliega_archivo_fecha(v_fecha_proceso)
         END IF           

      ON ACTION CANCEL 
         EXIT INPUT       
   END INPUT 
   CLOSE WINDOW w_op_fecha

END FUNCTION

###Funcion que muestra los registros encontrados para el archivo consultado
FUNCTION fn_despliega_archivo_fecha(p_fecha_proceso)
   DEFINE  p_fecha_proceso       DATE, -- fecha de proceso elegida
           v_sqlqry              STRING,           
           v_indice              SMALLINT,        
           v_rec_archivo         RECORD 
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE,
              nom_archivo        VARCHAR(40)
           END RECORD,
           v_ar_archivo          DYNAMIC ARRAY OF RECORD 
              nom_archivo        VARCHAR(40),
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE
           END RECORD,
           v_estado_aux          LIKE cre_acreditado.estado, 
           v_edo_procesar_aux    LIKE cre_acreditado.edo_procesar,
           v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin            LIKE seg_modulo.ruta_bin,
           v_nom_reporte         VARCHAR(80), -- nombre del reporte
           r_ruta_listados       LIKE seg_modulo.ruta_listados,
           f_w                   ui.form,
           w                     ui.window,
           v_existe_archivo      INTEGER,
           v_indice_rep          SMALLINT    


   OPEN WINDOW w_des_archivo WITH FORM "AGRC076"
      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()   
      LET v_sqlqry = " SELECT b.desc_estado, c.originacion_desc,\n",
                     "        SUM(a.sdo_deudor), d.f_proceso, d.nom_archivo\n",
                     "   FROM cre_rch_acreditado a,\n",
                     "        cat_rch_acreditado b,\n",
                     "        cat_cre_originacion c,\n",
                     "        cre_ctr_archivo d\n",
                     "  WHERE d.f_proceso = '", p_fecha_proceso, "'\n",
                     "    AND a.id_cre_ctr_archivo = d.id_cre_ctr_archivo\n",
                     "    AND a.estado = b.estado\n",
                     "    AND a.tpo_originacion = ",g_tpo_originacion,
                     "    AND a.tpo_originacion = c.tpo_originacion\n",
                     "  GROUP BY 1,2,4,5\n",
                     "  ORDER BY 5,1,2,4"
      
      PREPARE prp_cons_archivo_fecha FROM v_sqlqry
      DECLARE cur_arch_fecha CURSOR FOR prp_cons_archivo_fecha
      INITIALIZE  v_estado_aux, v_edo_procesar_aux TO NULL     
      
      LET v_indice = 1
      CALL v_ar_archivo.clear()
      OPEN cur_arch_fecha
      FOREACH cur_arch_fecha INTO v_rec_archivo.*
         
            LET v_ar_archivo[v_indice].sum_saldo_deudor = v_rec_archivo.sum_saldo_deudor
            LET v_ar_archivo[v_indice].estado           = v_rec_archivo.estado
            LET v_ar_archivo[v_indice].tpo_originacion  = v_rec_archivo.tpo_originacion
            LET v_ar_archivo[v_indice].f_proceso        = v_rec_archivo.f_proceso
            LET v_ar_archivo[v_indice].nom_archivo      = v_rec_archivo.nom_archivo
            LET v_indice = v_indice + 1
      END FOREACH 
      FREE cur_arch_fecha
       
      DISPLAY ARRAY v_ar_archivo TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC071.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC073-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_fecha TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_fecha(v_ar_archivo[v_indice_rep].*)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_fecha 
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
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
               CALL ui.Interface.refresh()   
               CALL fn_mensaje("Aviso","Se ha generado el reporte por fecha","info") 
            ELSE
               DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF      
              
      END DISPLAY    
   CLOSE WINDOW w_des_archivo
   
END FUNCTION

### Funcion que permite seleccionar el tipo de rechazo para consultar
FUNCTION fn_consulta_tipo_rechazo()
   DEFINE v_estado              LIKE cat_rch_acreditado.estado, -- estado de rechazo
          v_desc_estado         LIKE cat_rch_acreditado.desc_estado, -- descripcion del estado
          cb                    ui.ComboBox, -- combo en pantalla
          v_sqlqry              STRING,
          v_conteo              INTEGER

   OPEN WINDOW w_op_archivo WITH FORM "AGRC078"

   -- se asume que no se captura estado
   LET v_estado = NULL

   INPUT v_estado FROM cmb_estado ATTRIBUTE (UNBUFFERED) 
      AFTER FIELD cmb_estado
         NEXT FIELD cmb_estado
     
      BEFORE INPUT        
         LET cb = ui.ComboBox.forName("cmb_estado")
         CALL cb.clear()
         
         -- se consultan los tipos de estado
         LET v_sqlqry = " SELECT estado, desc_estado\n",
                        " FROM   cat_rch_acreditado\n",
                        " ORDER BY estado\n"
                                                    
          PREPARE prp_cons_estados FROM v_sqlqry        
          DECLARE cur_estados CURSOR FOR prp_cons_estados
          
          -- se llena el combo con los estados existentes
          FOREACH cur_estados INTO v_estado, v_desc_estado
             CALL cb.addItem(v_estado, v_desc_estado)
          END FOREACH
             
          -- se deja en nulo el estado inicial
          LET v_estado = NULL
         
      ON ACTION ACCEPT
         --se valida que se seleccione un archivo
         IF ( v_estado IS NULL ) THEN 
            CALL fn_mensaje("Aviso","Se requiere seleccionar un estado","stop")
            CONTINUE INPUT
         ELSE
         
            -- se asume que no hay registros
            LET v_conteo = 0
         
            -- se cuentan cuantos rechazos hay con ese estado
            SELECT COUNT(*)
              INTO v_conteo
              FROM cre_rch_acreditado
             WHERE tpo_originacion = g_tpo_originacion
               AND estado = v_estado
         
            -- si no hay registros
            IF ( v_conteo < 1 ) THEN
               CALL fn_mensaje("Atención","No se encontraron registros rechazados con el estado elegido","stop")
               CONTINUE INPUT
            END IF
         
            -- si se encontro al menos un registro, se muestra
            CALL fn_despliega_rechazo(v_estado)   
         END IF           

      ON ACTION CANCEL 
         EXIT INPUT       
   END INPUT 
   CLOSE WINDOW w_op_archivo

END FUNCTION

###Funcion que muestra los registros encontrados para el archivo consultado
FUNCTION fn_despliega_rechazo(v_estado)
   DEFINE  v_estado              LIKE cat_rch_acreditado.estado, -- estado de rechazo           
           v_sqlqry              STRING,           
           v_indice              SMALLINT,           
           v_rec_archivo         RECORD 
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE,
              nom_archivo        VARCHAR(40)
           END RECORD,
           v_ar_archivo          DYNAMIC ARRAY OF RECORD 
              nom_archivo        VARCHAR(40),
              estado             VARCHAR(40),
              tpo_originacion    VARCHAR(40),
              sum_saldo_deudor   DECIMAL(12,2),
              f_proceso          DATE
           END RECORD,           
           v_manejador_rpt       OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
           r_ruta_bin            LIKE seg_modulo.ruta_bin,
           v_nom_reporte         VARCHAR(80), -- nombre del reporte
           r_ruta_listados       LIKE seg_modulo.ruta_listados,
           f_w                   ui.form,
           w                     ui.window,
           v_existe_archivo      INTEGER,
           v_indice_rep          SMALLINT    

   OPEN WINDOW w_des_rechazo WITH FORM "AGRC076"

      LET  w = ui.Window.getCurrent()
      LET  f_w = w.getForm()   
      LET v_sqlqry = " SELECT b.desc_estado, c.originacion_desc,\n",
                     "        SUM(a.sdo_deudor), d.f_proceso, d.nom_archivo\n",
                     "   FROM cre_rch_acreditado a,\n",
                     "        cat_rch_acreditado b,\n",
                     "        cat_cre_originacion c,\n",
                     "        cre_ctr_archivo d\n",
                     "  WHERE a.estado = ", v_estado, "\n",
                     "    AND a.id_cre_ctr_archivo = d.id_cre_ctr_archivo\n",
                     "    AND a.estado = b.estado\n",
                     "    AND a.tpo_originacion = ",g_tpo_originacion,
                     "    AND a.tpo_originacion = c.tpo_originacion\n",
                     "  GROUP BY 1,2,4,5\n",
                     "  ORDER BY 5,1,2,4"
      
      -- se prepara el enunciado
      PREPARE prp_cons_estado FROM v_sqlqry
      
      -- se crea el cursor
      DECLARE cur_rechazo CURSOR FOR prp_cons_estado
      
      LET v_indice = 1
      CALL v_ar_archivo.clear()
      OPEN cur_rechazo
      FOREACH cur_rechazo INTO v_rec_archivo.*
         
            LET v_ar_archivo[v_indice].sum_saldo_deudor = v_rec_archivo.sum_saldo_deudor
            LET v_ar_archivo[v_indice].estado           = v_rec_archivo.estado
            LET v_ar_archivo[v_indice].tpo_originacion  = v_rec_archivo.tpo_originacion
            LET v_ar_archivo[v_indice].f_proceso        = v_rec_archivo.f_proceso
            LET v_ar_archivo[v_indice].nom_archivo      = v_rec_archivo.nom_archivo
            LET v_indice = v_indice + 1
      END FOREACH 
      FREE cur_rechazo
       
      DISPLAY ARRAY v_ar_archivo TO tabla_estado.* ATTRIBUTES (UNBUFFERED)

         ON ACTION ACCEPT
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY 

         ON ACTION CANCEL
            CALL v_ar_archivo.CLEAR() 
            EXIT DISPLAY

         ON ACTION reporte
            # Recupera la ruta de listados en el que se enviara el archivo
            CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados
   
            # Se indica que el reporte usara la plantilla creada
            IF fgl_report_loadCurrentSettings("AGRC071.4rp") THEN 
               CALL fgl_report_selectDevice("PDF") 
               LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC074-","00000","-","00000","-","00000" 
               CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

               -- sin preview
               CALL fgl_report_selectPreview(0)
      
               LET v_manejador_rpt = fgl_report_commitCurrentSettings()

               # Inicia el reporte de consulta historica por derechohabiente
               START REPORT reporte_cons_tipo TO XML HANDLER v_manejador_rpt
               FOR v_indice_rep = 1 TO v_indice -1
                  OUTPUT TO REPORT reporte_cons_tipo(v_ar_archivo[v_indice_rep].*)
               END FOR
                 
               #Finaliza el reporte
               FINISH REPORT reporte_cons_tipo 
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
               DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte
                 CALL ui.Interface.refresh()  
               CALL fn_mensaje("Aviso","Se ha generado el reporte por tipo de rechazo","info") 
            ELSE
               DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
            END IF      
              
      END DISPLAY    
   CLOSE WINDOW w_des_rechazo
   
END FUNCTION

#### Funcion que muestra un listado de derechohabientes ### 
#### para elegir el que se desea consultar              ###
FUNCTION fn_obt_derechohabiente(p_s_condicion)
   DEFINE p_s_condicion         STRING,
          v_nss                 LIKE afi_derechohabiente.nss,
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

   -- se abre la ventana de consulta
   OPEN WINDOW w_list_derechohabiente WITH FORM "AGRC074" 
                     
   -- se asume que no se selecciona derechohabiente
   LET v_nss = NULL
                     
   DISPLAY ARRAY v_arr_derechohabiente TO tabla_derechohabiente.*
   ATTRIBUTES ( ACCEPT=FALSE, CANCEL=FALSE, UNBUFFERED )

      ON ACTION ACCEPT 
         LET v_indice = arr_curr()
         LET v_nss = v_arr_derechohabiente[v_indice].nss
         EXIT DISPLAY             
      ON ACTION CANCEL
         LET v_nss = NULL
         EXIT DISPLAY 

   END DISPLAY
   
   CLOSE WINDOW w_list_derechohabiente
   
   -- se devuelve el resultado de la consulta
   RETURN v_nss
END FUNCTION

### Funcion que muestra el resultado de la consulta de derechohabiente ###
FUNCTION fn_despliega_por_derechohab(v_nss)
  DEFINE p_id_derechohabiente   LIKE afi_derechohabiente.id_derechohabiente,
         v_nss                  LIKE afi_derechohabiente.nss,
         v_curp                 LIKE afi_derechohabiente.curp,
         v_rfc                  LIKE afi_derechohabiente.rfc,
         v_ap_paterno           LIKE afi_derechohabiente.ap_paterno_af,
         v_ap_materno           LIKE afi_derechohabiente.ap_materno_af,
         v_nombre               LIKE afi_derechohabiente.nombre_af,
         v_tpo_credito          LIKE cre_acreditado.tpo_credito,
         v_num_credito          LIKE cre_acreditado.num_credito,
         v_des_estado           LIKE cat_maq_credito.estado_desc,         
         -- datos de rechazo
         v_id_cre_ctr_archivo   DECIMAL(9,0) ,
         v_tpo_originacion      SMALLINT     ,
         v_tpo_registro         CHAR(2)      ,
         v_sdo_deudor           DECIMAL(22,2),
         v_f_otorga             DATE         ,
         v_f_culmina            DATE         ,
         v_edo_credito          SMALLINT     ,
         v_tpo_dscto            SMALLINT     ,
         v_valor_dscto          DECIMAL(8,4) ,
         v_nrp                  CHAR(11)     ,
         v_f_ini_dscto          DATE         ,
         v_nss_liberado         CHAR(11)     ,
         v_f_gen_arh            DATE         ,
         v_sdo_credito          DECIMAL(22,2),
         v_f_prox_liq           DATE         ,
         v_f_desde              DATE         ,
         v_f_hasta              DATE         ,
         v_tpo_rch              SMALLINT     ,
         v_estado               SMALLINT     ,
         -- arreglo de registros rechazados
         v_arr_nss_rechazados   DYNAMIC ARRAY OF RECORD
            id_cre_ctr_archivo   DECIMAL(9,0) ,
            tpo_originacion      SMALLINT     ,
            tpo_registro         CHAR(2)      ,
            sdo_deudor           DECIMAL(22,2),
            f_otorga             DATE         ,
            f_culmina            DATE         ,
            edo_credito          SMALLINT     ,
            tpo_dscto            SMALLINT     ,
            valor_dscto          DECIMAL(8,4) ,
            nrp                  CHAR(11)     ,
            f_ini_dscto          DATE         ,
            nss_liberado         CHAR(11)     ,
            f_gen_arh            DATE         ,
            sdo_credito          DECIMAL(22,2),
            f_prox_liq           DATE         ,
            f_desde              DATE         ,
            f_hasta              DATE         ,
            tpo_rch              SMALLINT     ,
            estado               SMALLINT     ,
            num_credito          LIKE cre_acreditado.num_credito
         END RECORD,
         -- nombre del archivo
         v_nombre_archivo       VARCHAR(40),
         v_originacion_desc     VARCHAR(40), -- descripcion de la originacion
         v_indice               SMALLINT,
         v_indice_aux           SMALLINT,                  
         v_indice_rep           SMALLINT,
         v_des_tpo_credito      LIKE cat_tipo_credito.desc_credito,
         v_manejador_rpt        OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
         r_ruta_bin             LIKE seg_modulo.ruta_bin,
         f_w                    ui.form,
         w                      ui.window,         
         v_nom_reporte          VARCHAR(80), -- nombre del reporte         
         r_ruta_listados        LIKE seg_modulo.ruta_listados,
         v_sqlqry               STRING,
         v_existe_archivo       INTEGER         

  -- se obtienen los datos del derechohabiente
  SELECT id_derechohabiente, curp,rfc,ap_paterno_af, ap_materno_af, nombre_af
    INTO p_id_derechohabiente, v_curp,v_rfc, v_ap_paterno,v_ap_materno,v_nombre
    FROM afi_derechohabiente 
   WHERE nss = v_nss

  -- se leen los datos del rechazo
  DECLARE cur_nss_rechazados CURSOR FOR
  SELECT id_cre_ctr_archivo,
         tpo_originacion   ,
         tpo_registro      ,
         sdo_deudor        ,
         f_otorga          ,
         f_culmina         ,
         edo_credito       ,
         tpo_dscto         ,
         valor_dscto       ,
         nrp               ,
         f_ini_dscto       ,
         nss_liberado      ,
         f_gen_arh         ,
         sdo_credito       ,
         f_prox_liq        ,
         f_desde           ,
         f_hasta           ,
         tpo_rch           ,
         estado            ,
         num_credito
    FROM cre_rch_acreditado
   WHERE nss = v_nss
     AND tpo_originacion = g_tpo_originacion

  -- se inicia el contador
  LET v_indice = 1
  
  -- se llena el arreglo de rechazados
  FOREACH cur_nss_rechazados
  INTO
     v_id_cre_ctr_archivo,
     v_tpo_originacion   ,
     v_tpo_registro      ,
     v_sdo_deudor        ,
     v_f_otorga          ,
     v_f_culmina         ,
     v_edo_credito       ,
     v_tpo_dscto         ,
     v_valor_dscto       ,
     v_nrp               ,
     v_f_ini_dscto       ,
     v_nss_liberado      ,
     v_f_gen_arh         ,
     v_sdo_credito       ,
     v_f_prox_liq        ,
     v_f_desde           ,
     v_f_hasta           ,
     v_tpo_rch           ,
     v_estado            ,
     v_num_credito

     LET v_arr_nss_rechazados[v_indice].id_cre_ctr_archivo = v_id_cre_ctr_archivo
     LET v_arr_nss_rechazados[v_indice].tpo_originacion    = v_tpo_originacion
     LET v_arr_nss_rechazados[v_indice].tpo_registro       = v_tpo_registro
     LET v_arr_nss_rechazados[v_indice].sdo_deudor         = v_sdo_deudor
     LET v_arr_nss_rechazados[v_indice].f_otorga           = v_f_otorga
     LET v_arr_nss_rechazados[v_indice].f_culmina          = v_f_culmina
     LET v_arr_nss_rechazados[v_indice].edo_credito        = v_edo_credito
     LET v_arr_nss_rechazados[v_indice].tpo_dscto          = v_tpo_dscto
     LET v_arr_nss_rechazados[v_indice].valor_dscto        = v_valor_dscto
     LET v_arr_nss_rechazados[v_indice].nrp                = v_nrp
     LET v_arr_nss_rechazados[v_indice].f_ini_dscto        = v_f_ini_dscto
     LET v_arr_nss_rechazados[v_indice].nss_liberado       = v_nss_liberado
     LET v_arr_nss_rechazados[v_indice].f_gen_arh          = v_f_gen_arh
     LET v_arr_nss_rechazados[v_indice].sdo_credito        = v_sdo_credito
     LET v_arr_nss_rechazados[v_indice].f_prox_liq         = v_f_prox_liq
     LET v_arr_nss_rechazados[v_indice].f_desde            = v_f_desde
     LET v_arr_nss_rechazados[v_indice].f_hasta            = v_f_hasta
     LET v_arr_nss_rechazados[v_indice].tpo_rch            = v_tpo_rch
     LET v_arr_nss_rechazados[v_indice].estado             = v_estado
     LET v_arr_nss_rechazados[v_indice].num_credito        = v_num_credito

     -- se incrementa el indice
     LET v_indice = v_indice + 1
  END FOREACH

  LET v_indice_aux = v_indice
  OPEN WINDOW w_des_derechohab WITH FORM "AGRC073" 
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
             v_des_estado            
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
             estado            
             
     DISPLAY ARRAY v_arr_nss_rechazados TO tbl_nss_rechazados.*
        BEFORE DISPLAY
           -- se muestran los datos del primer indice
           LET v_indice = 1

           LET v_id_cre_ctr_archivo = v_arr_nss_rechazados[v_indice].id_cre_ctr_archivo
           LET v_tpo_originacion    = v_arr_nss_rechazados[v_indice].tpo_originacion   
           LET v_tpo_registro       = v_arr_nss_rechazados[v_indice].tpo_registro      
           LET v_sdo_deudor         = v_arr_nss_rechazados[v_indice].sdo_deudor        
           LET v_f_otorga           = v_arr_nss_rechazados[v_indice].f_otorga          
           LET v_f_culmina          = v_arr_nss_rechazados[v_indice].f_culmina         
           LET v_edo_credito        = v_arr_nss_rechazados[v_indice].edo_credito       
           LET v_tpo_dscto          = v_arr_nss_rechazados[v_indice].tpo_dscto         
           LET v_valor_dscto        = v_arr_nss_rechazados[v_indice].valor_dscto       
           LET v_nrp                = v_arr_nss_rechazados[v_indice].nrp               
           LET v_f_ini_dscto        = v_arr_nss_rechazados[v_indice].f_ini_dscto       
           LET v_nss_liberado       = v_arr_nss_rechazados[v_indice].nss_liberado      
           LET v_f_gen_arh          = v_arr_nss_rechazados[v_indice].f_gen_arh         
           LET v_sdo_credito        = v_arr_nss_rechazados[v_indice].sdo_credito       
           LET v_f_prox_liq         = v_arr_nss_rechazados[v_indice].f_prox_liq        
           LET v_f_desde            = v_arr_nss_rechazados[v_indice].f_desde           
           LET v_f_hasta            = v_arr_nss_rechazados[v_indice].f_hasta           
           LET v_tpo_rch            = v_arr_nss_rechazados[v_indice].tpo_rch           
           LET v_estado             = v_arr_nss_rechazados[v_indice].estado            
           LET v_num_credito        = v_arr_nss_rechazados[v_indice].num_credito       
      
           -- se obtiene la descripcion del rechazo
           SELECT desc_estado
             INTO v_des_estado
             FROM cat_rch_acreditado
            WHERE estado = v_estado
           
           -- descripcion de la originacion
           SELECT originacion_desc
             INTO v_originacion_desc
             FROM cat_cre_originacion
            WHERE tpo_originacion = v_tpo_originacion
           
           -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
           LET v_sqlqry = " SELECT FIRST 1 desc_credito\n",
                          "   FROM cat_tipo_credito\n",
                          "  WHERE tpo_credito = ",v_tpo_credito

           PREPARE prp_slctFrst_descCred FROM v_sqlqry
           EXECUTE prp_slctFrst_descCred INTO v_des_tpo_credito

           -- se obtiene el nombre del archivo
           SELECT nom_archivo
             INTO v_nombre_archivo
             FROM cre_ctr_archivo
            WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo
             
           -- datos del rechazo
           DISPLAY
              v_id_cre_ctr_archivo,
              v_originacion_desc  ,
              v_tpo_registro      ,
              v_sdo_deudor        ,
              v_f_otorga          ,
              v_f_culmina         ,
              v_edo_credito       ,
              v_tpo_dscto         ,
              v_valor_dscto       ,
              v_nrp               ,
              v_f_ini_dscto       ,
              v_nss_liberado      ,
              v_f_gen_arh         ,
              v_sdo_credito       ,
              v_f_prox_liq        ,
              v_f_desde           ,
              v_f_hasta           ,
              v_tpo_rch           ,
              v_num_credito       ,
              v_nombre_archivo    ,
              v_des_estado
           TO
              id_cre_ctr_archivo,
              tpo_originacion   ,
              tpo_registro      ,
              sdo_deudor        ,
              f_otorga          ,
              f_culmina         ,
              edo_credito       ,
              tpo_dscto         ,
              valor_dscto       ,
              nrp               ,
              f_ini_dscto       ,
              nss_liberado      ,
              f_gen_arh         ,
              sdo_credito       ,
              f_prox_liq        ,
              f_desde           ,
              f_hasta           ,
              tpo_rch           ,
              num_credito       ,
              nom_archivo       ,
              estado

     
        ON ACTION ACCEPT 
           EXIT DISPLAY
           
        ON ACTION CANCEL 
           EXIT DISPLAY

        ON ACTION reporte
           # Recupera la ruta de listados en el que se enviara el archivo
           CALL fn_rutas("agr") RETURNING r_ruta_bin, r_ruta_listados

           # Se indica que el reporte usara la plantilla creada
           IF fgl_report_loadCurrentSettings("AGRC072.4rp") THEN 
              CALL fgl_report_selectDevice("PDF") 
              LET v_nom_reporte = p_usuario_cod CLIPPED || "-AGRC072-","00000","-","00000","-","00000" 
              DISPLAY " reporte:",v_nom_reporte
              CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)

              -- sin preview
              CALL fgl_report_selectPreview(0)

              LET v_manejador_rpt = fgl_report_commitCurrentSettings()

              # Inicia el reporte de consulta historica por derechohabiente
              START REPORT reporte_cons_derechohabiente TO XML HANDLER v_manejador_rpt

              -- se procesan los registro de cre historico
              FOR v_indice_rep = 1 TO v_indice_aux -1
                 DISPLAY " uSUARIO: ",p_usuario_cod
                 -- indica que el encabezado debe ser el de historico
                 OUTPUT TO REPORT reporte_cons_derechohabiente(p_id_derechohabiente, v_nss, v_curp, v_rfc,
                                                               v_ap_paterno, v_ap_materno, v_nombre,
                                                               v_des_tpo_credito, v_num_credito, v_originacion_desc,
                                                               v_arr_nss_rechazados[v_indice_rep].*,v_tpo_registro,
                                                               v_des_estado)
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
              DISPLAY "<a gwc:attributes=\"href resourceUri('"||v_nom_reporte CLIPPED||".pdf"||"','agr')\" target='_blank'><img gwc:attributes=\"src resourceuri('logo_pdf_descarga.gif','glo')\" title='Visualizar reporte'/></a>" TO lbl_ruta_reporte

                 CALL ui.Interface.refresh()
              CALL fn_mensaje("Aviso","Se ha generado el reporte por derechohabiente","info") 
           ELSE
              DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
           END IF        
        -- al cambiar el renglon
        BEFORE ROW
           -- se obtiene el indice del renglon en cuestion
           LET v_indice = ARR_CURR()

           LET v_id_cre_ctr_archivo = v_arr_nss_rechazados[v_indice].id_cre_ctr_archivo
           LET v_tpo_originacion    = v_arr_nss_rechazados[v_indice].tpo_originacion   
           LET v_tpo_registro       = v_arr_nss_rechazados[v_indice].tpo_registro      
           LET v_sdo_deudor         = v_arr_nss_rechazados[v_indice].sdo_deudor        
           LET v_f_otorga           = v_arr_nss_rechazados[v_indice].f_otorga          
           LET v_f_culmina          = v_arr_nss_rechazados[v_indice].f_culmina         
           LET v_edo_credito        = v_arr_nss_rechazados[v_indice].edo_credito       
           LET v_tpo_dscto          = v_arr_nss_rechazados[v_indice].tpo_dscto         
           LET v_valor_dscto        = v_arr_nss_rechazados[v_indice].valor_dscto       
           LET v_nrp                = v_arr_nss_rechazados[v_indice].nrp               
           LET v_f_ini_dscto        = v_arr_nss_rechazados[v_indice].f_ini_dscto       
           LET v_nss_liberado       = v_arr_nss_rechazados[v_indice].nss_liberado      
           LET v_f_gen_arh          = v_arr_nss_rechazados[v_indice].f_gen_arh         
           LET v_sdo_credito        = v_arr_nss_rechazados[v_indice].sdo_credito       
           LET v_f_prox_liq         = v_arr_nss_rechazados[v_indice].f_prox_liq        
           LET v_f_desde            = v_arr_nss_rechazados[v_indice].f_desde           
           LET v_f_hasta            = v_arr_nss_rechazados[v_indice].f_hasta           
           LET v_tpo_rch            = v_arr_nss_rechazados[v_indice].tpo_rch           
           LET v_estado             = v_arr_nss_rechazados[v_indice].estado            
           LET v_num_credito        = v_arr_nss_rechazados[v_indice].num_credito       
      
           -- se obtiene la descripcion del rechazo
           SELECT desc_estado
             INTO v_des_estado
             FROM cat_rch_acreditado
            WHERE estado = v_estado
           
           -- descripcion de la originacion
           SELECT originacion_desc
             INTO v_originacion_desc
             FROM cat_cre_originacion
            WHERE tpo_originacion = v_tpo_originacion
           
           -- se obtiene la descripción del crédito. Por el nuevo esquema se agrega el FIRST 1
           LET v_sqlqry = " SELECT FIRST 1 desc_credito\n",
                          "   FROM cat_tipo_credito\n",
                          "  WHERE tpo_credito = ",v_tpo_credito

           PREPARE prp_slctFrst_descCred2 FROM v_sqlqry
           EXECUTE prp_slctFrst_descCred2 INTO v_des_tpo_credito

           -- se obtiene el nombre del archivo
           SELECT nom_archivo
             INTO v_nombre_archivo
             FROM cre_ctr_archivo
            WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo
            
           -- datos del rechazo
           DISPLAY
              v_id_cre_ctr_archivo,
              v_originacion_desc  ,
              v_tpo_registro      ,
              v_sdo_deudor        ,
              v_f_otorga          ,
              v_f_culmina         ,
              v_edo_credito       ,
              v_tpo_dscto         ,
              v_valor_dscto       ,
              v_nrp               ,
              v_f_ini_dscto       ,
              v_nss_liberado      ,
              v_f_gen_arh         ,
              v_sdo_credito       ,
              v_f_prox_liq        ,
              v_f_desde           ,
              v_f_hasta           ,
              v_tpo_rch           ,
              v_num_credito       ,
              v_nombre_archivo    ,
              v_des_estado
           TO
              id_cre_ctr_archivo,
              tpo_originacion   ,
              tpo_registro      ,
              sdo_deudor        ,
              f_otorga          ,
              f_culmina         ,
              edo_credito       ,
              tpo_dscto         ,
              valor_dscto       ,
              nrp               ,
              f_ini_dscto       ,
              nss_liberado      ,
              f_gen_arh         ,
              sdo_credito       ,
              f_prox_liq        ,
              f_desde           ,
              f_hasta           ,
              tpo_rch           ,
              num_credito       ,
              nom_archivo       ,
              estado

     END DISPLAY
     
  CLOSE WINDOW w_des_derechohab
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
                                    p_ap_materno,p_nombre,p_des_tpo_credito, p_num_credito, 
                                    p_originacion_desc,p_rec_nss_rechazados,p_tpo_registro,
                                    p_des_estado)
                                    
DEFINE p_id_derechohabiente      INTEGER ,
       p_nss                     LIKE afi_derechohabiente.nss,
       p_curp                    LIKE afi_derechohabiente.curp,
       p_rfc                     LIKE afi_derechohabiente.rfc,
       p_ap_paterno              LIKE afi_derechohabiente.ap_paterno_af,
       p_ap_materno              LIKE afi_derechohabiente.ap_materno_af,
       p_nombre                  LIKE afi_derechohabiente.nombre_af,
       p_des_tpo_credito         LIKE cat_tipo_credito.desc_credito,
       p_num_credito             LIKE cre_acreditado.num_credito,
       p_originacion_desc        VARCHAR(40), -- descripcion de la originacion,
       p_tpo_registro            CHAR(2),
       p_des_estado              LIKE cat_maq_credito.estado_desc,         
       p_rec_nss_rechazados      RECORD
            id_cre_ctr_archivo   DECIMAL(9,0) ,
            tpo_originacion      SMALLINT     ,
            tpo_registro         CHAR(2)      ,
            sdo_deudor           DECIMAL(22,2),
            f_otorga             DATE         ,
            f_culmina            DATE         ,
            edo_credito          SMALLINT     ,
            tpo_dscto            SMALLINT     ,
            valor_dscto          DECIMAL(8,4) ,
            nrp                  CHAR(11)     ,
            f_ini_dscto          DATE         ,
            nss_liberado         CHAR(11)     ,
            f_gen_arh            DATE         ,
            sdo_credito          DECIMAL(22,2),
            f_prox_liq           DATE         ,
            f_desde              DATE         ,
            f_hasta              DATE         ,
            tpo_rch              SMALLINT     ,
            estado               SMALLINT     ,
            num_credito          LIKE cre_acreditado.num_credito
         END RECORD,       
       v_fecha_reporte           DATE   

   FORMAT

   FIRST PAGE HEADER
      DISPLAY "uSUARIO: ",p_usuario_cod
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
      PRINTX p_originacion_desc
      PRINTX p_tpo_registro
      PRINTX p_des_estado      


   ON EVERY ROW
      PRINTX p_rec_nss_rechazados.tpo_originacion
      PRINTX p_rec_nss_rechazados.sdo_deudor
      PRINTX p_rec_nss_rechazados.edo_credito
      PRINTX p_rec_nss_rechazados.tpo_dscto
      PRINTX p_rec_nss_rechazados.valor_dscto
      PRINTX p_rec_nss_rechazados.nrp
      PRINTX p_rec_nss_rechazados.tpo_rch
      PRINTX p_rec_nss_rechazados.edo_credito
      PRINTX p_rec_nss_rechazados.num_credito USING "&&&&&&&&&&"
      
END REPORT


#OBJETIVO: Genera el reporte de consulta por archivo
REPORT reporte_cons_archivo(p_rec_archivo)
DEFINE p_rec_archivo        RECORD 
          nom_archivo        VARCHAR(40), 
          estado             VARCHAR(40),
          tpo_originacion    VARCHAR(40),
          sum_saldo_deudor   DECIMAL(12,2),
          f_proceso          DATE
       END RECORD,
       v_fecha_reporte      DATE,
       v_titulo_reporte     VARCHAR (40)
    
   FORMAT
    
   FIRST PAGE HEADER
      LET v_titulo_reporte = "CONSULTA DE RECHAZOS POR ARCHIVO AGR" 
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod 
      PRINTX p_rec_archivo.nom_archivo
      PRINTX v_titulo_reporte
      
                    
   ON EVERY ROW
      PRINTX p_rec_archivo.estado
      PRINTX p_rec_archivo.tpo_originacion
      PRINTX p_rec_archivo.sum_saldo_deudor
      PRINTX p_rec_archivo.f_proceso USING "dd-mm-yyyy"
END REPORT

#OBJETIVO: Genera el reporte de consulta por fecha del archivo
REPORT reporte_cons_fecha(p_rec_archivo)
DEFINE p_rec_archivo        RECORD 
          nom_archivo        VARCHAR(40), 
          estado             VARCHAR(40),
          tpo_originacion    VARCHAR(40),
          sum_saldo_deudor   DECIMAL(12,2),
          f_proceso          DATE
       END RECORD,
       v_fecha_reporte      DATE,
       v_titulo_reporte     VARCHAR (40)
    
   FORMAT

   FIRST PAGE HEADER
      LET v_titulo_reporte = "CONSULTA DE RECHAZOS POR FECHA AGR" 
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod 
      PRINTX p_rec_archivo.nom_archivo  
      PRINTX v_titulo_reporte   
                    
   ON EVERY ROW
      PRINTX p_rec_archivo.estado
      PRINTX p_rec_archivo.tpo_originacion
      PRINTX p_rec_archivo.sum_saldo_deudor
      PRINTX p_rec_archivo.f_proceso USING "dd-mm-yyyy"
END REPORT

#OBJETIVO: Genera el reporte de consulta por tipo de rechazo
REPORT reporte_cons_tipo(p_rec_archivo)
DEFINE p_rec_archivo        RECORD 
          nom_archivo        VARCHAR(40), 
          estado             VARCHAR(40),
          tpo_originacion    VARCHAR(40),
          sum_saldo_deudor   DECIMAL(12,2),
          f_proceso          DATE
       END RECORD,
       v_fecha_reporte      DATE,
       v_titulo_reporte     VARCHAR (40)
    
   FORMAT

   FIRST PAGE HEADER
      LET v_titulo_reporte = "CONSULTA DE POR TIPO DE RECHAZO AGR" 
      LET v_fecha_reporte = TODAY      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"             
      PRINTX p_usuario_cod 
      PRINTX p_rec_archivo.nom_archivo  
      PRINTX v_titulo_reporte   
                    
   ON EVERY ROW
      PRINTX p_rec_archivo.estado
      PRINTX p_rec_archivo.tpo_originacion
      PRINTX p_rec_archivo.sum_saldo_deudor
      PRINTX p_rec_archivo.f_proceso USING "dd-mm-yyyy"
END REPORT