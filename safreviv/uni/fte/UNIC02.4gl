--===============================================================
-- Version: 1.0.0 
-- Fecha ultima modificacion: 28/09/2012
--===============================================================

################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIC02                                                        #
#Objetivo     => Consultar los datos cargados en la tabla de rechazos, para la #
#                Unificación de cuentas.                                       #
#Fecha inicio => Marzo 20, 2012                                                #
################################################################################

DATABASE safre_viv
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE v_r_glo_ctr_archivo RECORD
          folio                LIKE glo_folio.folio,
          nombre_archivo       LIKE glo_ctr_archivo.nombre_archivo
END RECORD

DEFINE v_ar_registro  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
          v_c_registro         CHAR(2), -- Tipo de registro del rechazo
          v_c_desc_registro    CHAR(20) -- Descripción del registro
END RECORD

DEFINE v_ar_rechazo  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
          v_folio_unificacion  DECIMAL(9,0),
          v_c_campo_valor      CHAR(50), -- Valor del campo
          v_s_diagnostico      SMALLINT, -- Valor del diagnostico
          v_c_desc_diagnostico CHAR(100) -- Descripción del diagnostico
END RECORD

DEFINE v_folio LIKE glo_folio.folio,
       w       ui.Window,
       f       ui.Form,
       QryTxT  STRING,
       v_manejador_rpt    OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   --CALL ui.interface.loadstyles("../../mdt/bin/mdtstyle")
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
     
   -- consulta de informacion recibida de OP98
   CALL fn_genera_consulta_rechazo(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: UNIC02
Nombre: fn_genera_consulta_rechazo
Fecha creacion: Mayo 29, 2012
Narrativa del proceso que realiza:
  Genera consulta de codigos de rechazo para el proceso UNI
======================================================================}
FUNCTION fn_genera_consulta_rechazo(p_usuario_cod)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_cbx_folios       ui.ComboBox, -- combo de folios
       v_cbx_registros    ui.ComboBox, -- combo de registros
       v_s_cadena         STRING, -- cadena de texto
       v_i_conArch        INTEGER,
       v_registro         CHAR(2)

   OPEN WINDOW win_ConsInfo WITH FORM "UNIC020"
   CLOSE WINDOW SCREEN
   -- Recupera punteros a ventana para control de grupos
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()   
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_registros = ui.ComboBox.forName("formonly.cmb_registro")
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

   -- Ocultar tablas de resultados
   CALL f.setElementHidden("gruregistro",1)
   CALL f.setElementHidden("grudetalle",1)
   
   LET INT_FLAG = FALSE
   -- se inicia el combobox en blanco
   CALL v_cbx_registros.clear()
   CALL v_cbx_folios.clear()

   CALL v_cbx_registros.addItem(1, "1 Encabezado.Unificar")
   CALL v_cbx_registros.addItem(2, "2 Detalle.Unificadora")
   CALL v_cbx_registros.addItem(3, "3 Detalle.Unificadas")
   CALL v_cbx_registros.addItem(9, "9 Sumario.Unificar")
         
   -- se llena el arreglo de folios
   DECLARE cur_folios CURSOR FOR
           SELECT DISTINCT fo.folio, ar.nombre_archivo
             FROM glo_ctr_archivo ar,
                  glo_folio fo
            WHERE ar.proceso_cod IN (2301, 2318)
              AND ar.estado = 2
              AND ar.folio = fo.folio
   
   LET v_i_conArch = 0   
   
   FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
      LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
          v_r_glo_ctr_archivo.nombre_archivo 
      CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio 
           ,v_s_cadena)
      -- Contador de archivos eoncontrados
      LET v_i_conArch = v_i_conArch + 1
   END FOREACH
   
   IF(v_i_conArch<1)THEN
      CALL fn_mensaje("Atención","No existen archivos recientemente integrados para consulta","info")
      CLOSE WINDOW win_ConsInfo
      RETURN
   END IF

   FREE cur_folios

   -- el primer elemento en el combo es la cadena que indica que se debe elegir un elemento
   CALL v_cbx_registros.addItem(-1," ")
   CALL v_cbx_folios.addItem(-1," ")
   -- se asignan los valores por omision
   LET v_folio = -1
   
   INPUT v_folio, v_registro WITHOUT DEFAULTS
   FROM cmb_folio, cmb_registro ATTRIBUTES (UNBUFFERED)
  
      BEFORE INPUT
         
      AFTER INPUT
         CONTINUE INPUT
      
      ON ACTION ACCEPT
         IF(v_folio = '-1')THEN
            CALL fn_mensaje("Atención","Indique al menos un folio","info")
            NEXT FIELD cmb_folio
         END IF
         -- Mostrar tablas de resultados
         CALL f.setElementHidden("grudetalle",0)
         CALL f.setElementHidden("gruregistro",0)
         CALL fn_muestra_consulta_rechazo(v_folio, v_registro, p_usuario_cod)
         LET INT_FLAG = FALSE

      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         EXIT INPUT
   
   END INPUT
      
   CLOSE WINDOW win_ConsInfo

END FUNCTION -- fn_genera_consulta_rechazo

{
======================================================================
Clave: UNIC02
Nombre: fn_muestra_consulta_rechazo
Fecha creacion: Mayo 29, 2012
Narrativa del proceso que realiza:
   Muetra detalle de proceso de rechazo: 
     "UNIFICACIÓN DE CUENTAS"
======================================================================}
FUNCTION fn_muestra_consulta_rechazo(p_folio, p_registro, p_usuario_cod)
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio               LIKE glo_folio.folio,
       p_registro            CHAR(2),
       v_si_indice           SMALLINT, -- indice de arreglo
       v_s_sql               STRING, -- cadena con una instruccion SQL
       v_i_conArch           INTEGER, -- Contador general
       v_cbx_folios          ui.ComboBox, -- combo de folios
       v_cbx_registros       ui.ComboBox, -- combo de registros
       v_s_cadena            STRING, -- cadena de texto
       r_ruta_bin            LIKE seg_modulo.ruta_bin,
       v_nom_reporte         VARCHAR(80), -- nombre del reporte
       r_ruta_listados       LIKE seg_modulo.ruta_listados,
       v_r_rechazo RECORD    LIKE uni_det_rechazos.*, -- Record almacena los rechazos
       v_s_reporte           STRING,
       v_descripcion_rechazo CHAR(30)

   CALL v_ar_rechazo.CLEAR()
   CALL v_ar_registro.CLEAR()
   
   DISPLAY p_folio, p_registro TO cmb_folio, cmb_registro
   
   -- si se envio un tipo de registro para la consulta
   IF ( p_registro IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED,
          " a.tipo_registro = '",p_registro CLIPPED,"'"
   ELSE
   	  LET v_s_sql = " 1=1"
   END IF
   
   CALL LLenaArryRegistros(v_s_sql, p_folio) RETURNING v_si_indice --Llamada a función que obtiene los registros 

   DIALOG ATTRIBUTES (UNBUFFERED)
   -- ====================================
   -- SE MUESTRAN LOS REGISTROS OBTENIDOS
   -- ====================================
      DISPLAY ARRAY v_ar_registro TO tb_registro.* 
         BEFORE DISPLAY
         	 DISPLAY v_s_reporte TO v_s_reporte
           IF v_si_indice = 0 THEN
              CALL v_ar_rechazo.CLEAR()
              CALL v_ar_registro.CLEAR()
              LET f = DIALOG.getForm()
              CALL f.setElementHidden("grudetalle",1)
              CALL f.setElementHidden("gruregistro",1)
              RETURN
           END IF
         
         BEFORE ROW
            -- Se toma el puntero para la selección del detalle
            LET QryTxt = " a.tipo_registro = ", 
                          v_ar_registro[ARR_CURR()].v_c_registro CLIPPED
            
            CALL LLenaArryDetalleRechazo(QryTxt, p_folio) --Llamada a función que obtiene el detalle del rechazo

            --ON ACTION hipervinculo

            ON ACTION regresa
               CALL v_ar_rechazo.CLEAR()
               CALL v_ar_registro.CLEAR()
               LET v_s_reporte = ""
               LET f = DIALOG.getForm()
               CALL f.setElementHidden("gruregistro",1)
               CALL f.setElementHidden("grudetalle",1)
               -- Recupera punteros a ventana para control de grupos
               LET w = ui.Window.getCurrent()
               LET f = w.getForm()   
               -- se le asigna el apuntado der combo a la variable
               LET v_cbx_registros = ui.ComboBox.forName("formonly.cmb_registro")
               LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
               -- se inicia el combobox en blanco
               CALL v_cbx_registros.clear()
               CALL v_cbx_folios.clear()
               ## Carga temporal de estados 20-03-2012
               CALL v_cbx_registros.addItem(1, "1 Encabezado.Unificar")
               CALL v_cbx_registros.addItem(2, "2 Detalle.Unificadora")
               CALL v_cbx_registros.addItem(3, "3 Detalle.Unificadas")
               CALL v_cbx_registros.addItem(9, "9 Sumario.Unificar")
               -- se llena el arreglo de folios
               DECLARE cur_folios_regresa CURSOR FOR
               SELECT DISTINCT d.folio, g.nombre_archivo
               FROM glo_ctr_archivo g, glo_folio d
               WHERE g.proceso_cod IN (2301, 2318)
                 AND g.estado = 2 -- integrado
                 AND g.folio = d.folio
               -- <Se cruzan los folios con  para obtener folios procesados>
               -- <  completamente y en forma exitosa para toda la afore.              >
               LET v_i_conArch = 0
               FOREACH cur_folios_regresa INTO v_r_glo_ctr_archivo.*
                  LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " -", 
                      v_r_glo_ctr_archivo.nombre_archivo 
                  CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio 
                       ,v_s_cadena)
                  -- Contador de archivos eoncontrados
                  LET v_i_conArch = v_i_conArch + 1
               END FOREACH
               IF(v_i_conArch<1)THEN
                  CALL fn_mensaje("Atención",
                       "No existen archivos recientemente integrados para consulta",
                       "info")
                  CLOSE WINDOW win_ConsInfo
                  RETURN
               END IF
               FREE cur_folios_regresa
               -- el primer elemento en el combo es la cadena que indica que se debe elegir
               -- una afore
               CALL v_cbx_registros.addItem(-1," ")
               CALL v_cbx_folios.addItem(-1," ")
               -- se asignan los valores por omision
               LET v_folio = -1
               EXIT DIALOG
            ON ACTION reporte
                # Recupera la ruta de listados en el que se enviara el archivo
                  CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
                  
                  # Se indica que el reporte usara la plantilla creada
                  IF fgl_report_loadCurrentSettings("UNIC02.4rp") THEN 
                     CALL fgl_report_selectDevice("PDF") 
                     DISPLAY "p_usuario_cod:",p_usuario_cod
                     LET v_nom_reporte = p_usuario_cod CLIPPED || "-UNIC02-","00000","-","00000","-","00000"||".pdf"
                     DISPLAY "v_nom_reporte:",v_nom_reporte
                     CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
                     -- sin preview
                     CALL fgl_report_selectPreview(0)                     
                     LET v_manejador_rpt = fgl_report_commitCurrentSettings()
                     
                     # Inicia el reporte de consulta historica por derechohabiente
                     START REPORT reporte_rechazos TO XML HANDLER v_manejador_rpt
                        DECLARE cur_rch_archivo CURSOR FOR
                        SELECT a.*, b.diag_desc_larga
                          FROM uni_det_rechazos a,
                               uni_diagnostico_sol b
                          WHERE a.diagnostico = b.id_diagnostico	
                            AND a.folio_unificacion = p_folio
                            ORDER BY a.tipo_registro

                       FOREACH cur_rch_archivo INTO v_r_rechazo.*, v_descripcion_rechazo
                           OUTPUT TO REPORT reporte_rechazos(v_r_rechazo.*, v_descripcion_rechazo, p_usuario_cod)
                       END FOREACH
                       
                     #Finaliza el reporte
                     FINISH REPORT reporte_rechazos
                     --CALL fn_mensaje("Aviso","Se ha generado el reporte para rechazos","info")
                     LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>",
                                        v_nom_reporte CLIPPED,"</a>"
                  ELSE
                     DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
                  END IF
                  
                  DISPLAY v_s_reporte TO v_s_reporte      
      END DISPLAY
      
      -- ====================================
      -- SE MUESTRAN EL DETALLE DEL RECHAZO
      -- ====================================
      DISPLAY ARRAY v_ar_rechazo TO tb_rechazo.*

      END DISPLAY
      
   AFTER DIALOG
      CONTINUE DIALOG
            
   
   END DIALOG
   
   CALL v_ar_registro.CLEAR()
   CALL v_ar_rechazo.CLEAR()
   
END FUNCTION -- fn_muestra_consulta_rechazo

{
======================================================================
Clave: UNIC02
Nombre: LLenaArryRegistros
Fecha creacion: Mayo 29, 2012
Narrativa del proceso que realiza:
   Muestra los registros con error de rechazo: 
     "UNIFICACIÓN DE CUENTAS"
======================================================================}
FUNCTION LLenaArryRegistros(ls_qry, p_folio)
DEFINE p_folio      LIKE glo_folio.folio,
       li_pos       INTEGER,
       ls_qry       STRING,
       v_c_registro LIKE dpe_rch_archivo.tipo_registro -- Tipo de registro del rechazo
    
   WHENEVER ERROR CONTINUE
     CALL v_ar_registro.CLEAR()
     LET QryTxT = "  SELECT a.tipo_registro",
                  "\n  FROM uni_det_rechazos a",
                  "\n WHERE ",  ls_qry CLIPPED,
                  "\n    AND a.folio_unificacion = ",p_folio,
                  "\n  GROUP BY 1",
                  "\n  ORDER BY 1"
                   
     DISPLAY "Consulta Registros: ", QryTxT CLIPPED
     PREPARE Prp_ObtRegistros FROM QryTxT CLIPPED
     
     LET li_pos = 0
      DECLARE Crs_ObtRegistros CURSOR FOR Prp_ObtRegistros
         FOREACH Crs_ObtRegistros INTO v_c_registro
            LET li_pos = li_pos + 1
            -- se transfieren los datos al arreglo
            CASE v_c_registro
            	WHEN '1'
            	   LET v_ar_registro[li_pos].v_c_registro = v_c_registro
            		 LET v_ar_registro[li_pos].v_c_desc_registro = "Encabezado Unificar"
            	WHEN '2'
            		 LET v_ar_registro[li_pos].v_c_registro = v_c_registro
            		 LET v_ar_registro[li_pos].v_c_desc_registro = "Detalle Unificadora"
            	WHEN '3'
                 LET v_ar_registro[li_pos].v_c_registro = v_c_registro
            		 LET v_ar_registro[li_pos].v_c_desc_registro = "Detalle Unificadas"
            	WHEN '9'
            		 LET v_ar_registro[li_pos].v_c_registro = v_c_registro
            		 LET v_ar_registro[li_pos].v_c_desc_registro = "Sumario Unificar"
            END CASE
         END FOREACH
     FREE Prp_ObtRegistros
      
     -- Se valida si el indice es en cero para arrojar mensaje de error
     IF li_pos = 0 THEN
        CALL v_ar_registro.CLEAR()
        CALL fn_mensaje("Atención",
             "No se encontraron datos con los criterios indicados",
             "information")
     ELSE
         IF(v_ar_registro[v_ar_registro.getLength()].v_c_registro IS NULL OR 
            v_ar_registro[v_ar_registro.getLength()].v_c_desc_registro CLIPPED = '')THEN
            CALL v_ar_registro.deleteElement(v_ar_registro.getLength())
         END IF        
     END IF
   WHENEVER ERROR STOP
 
 RETURN li_pos

END FUNCTION

{
======================================================================
Clave: UNIC02
Nombre: LLenaArryDetalleRechazo
Fecha creacion: Mayo 28, 2012
Narrativa del proceso que realiza:
   Muestra el detalle del rechazo, mediante el parametro del registro: 
     "UNIFICACIÓN DE CUENTAS"
======================================================================}
FUNCTION LLenaArryDetalleRechazo(ls_qry, p_folio)
DEFINE p_folio      LIKE glo_folio.folio,
       v_r_rechazo RECORD -- registro para consultar el detalle de los rechazos
          --v_c_registro_patronal CHAR(11), -- Registro patronal imss
          --v_c_periodo_pago      CHAR(6), -- Periodo de pago
          v_folio_unificacion   DECIMAL(9,0),
          v_c_campo_valor       CHAR(50), -- Valor del campo
          v_s_diagnostico       SMALLINT, -- Valor del diagnostico
          v_c_desc_diagnostico  CHAR(100) -- Descripción del diagnostico
       END RECORD,
       li_pos  INTEGER,
       ls_qry  STRING
    
   WHENEVER ERROR CONTINUE
     CALL v_ar_rechazo.CLEAR()

     LET QryTxT = "\n SELECT c.folio_unificacion,",
                  "\n        a.campo_valor,",
                  "\n        a.diagnostico,",
                  "\n        b.diag_desc_larga",
                  "\n   FROM uni_det_rechazos a,",
                  "\n        uni_diagnostico_sol b,",
                  "\n        uni_det_unificador c",
                  "\n  WHERE ",  ls_qry CLIPPED,
                  "\n    AND a.diagnostico = b.id_diagnostico",
                  "\n    AND a.folio_unificacion = c.folio_unificacion",
                  "\n    AND c.folio_unificacion = ",p_folio,
                  "\n  GROUP BY 1,2,3,4",
                  "\n  ORDER BY 2"
     
     --DISPLAY "Consulta Detalle Rechazo: ", QryTxT CLIPPED
     PREPARE Prp_ObtRegDetalle FROM QryTxT CLIPPED
     
     LET li_pos = 0
      DECLARE Crs_ObtRegDetalle CURSOR FOR Prp_ObtRegDetalle
         FOREACH Crs_ObtRegDetalle INTO v_r_rechazo.*
            LET li_pos = li_pos + 1
            
            LET v_ar_rechazo[li_pos].v_folio_unificacion = v_r_rechazo.v_folio_unificacion
            LET v_ar_rechazo[li_pos].v_c_campo_valor = v_r_rechazo.v_c_campo_valor
            LET v_ar_rechazo[li_pos].v_s_diagnostico = v_r_rechazo.v_s_diagnostico
            LET v_ar_rechazo[li_pos].v_c_desc_diagnostico = v_r_rechazo.v_c_desc_diagnostico
         END FOREACH
         DISPLAY  "tamañode registros: ", v_ar_rechazo.getLength()
      
      FREE Prp_ObtRegDetalle
      
      -- Se valida si el indice es en cero para arrojar mensaje de error
     IF li_pos = 0 THEN
        CALL v_ar_rechazo.CLEAR()
        CALL fn_mensaje("Atención",
             "No se encontraron datos con los criterios indicados",
             "information")
     ELSE
         IF(v_ar_rechazo[v_ar_rechazo.getLength()].v_folio_unificacion IS NULL OR 
            v_ar_rechazo[v_ar_rechazo.getLength()].v_folio_unificacion CLIPPED = '')THEN
            CALL v_ar_rechazo.deleteElement(v_ar_rechazo.getLength())
         END IF        
     END IF
   WHENEVER ERROR STOP

END FUNCTION

REPORT reporte_rechazos(v_r_rechazo, p_descripcion_rechazo, p_usuario_cod)
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_r_rechazo     RECORD LIKE uni_det_rechazos.*, -- Record almacena los rechazos
       	p_descripcion_rechazo CHAR(30),
	     v_fecha_reporte DATE
   
   FORMAT
   
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod
  
  ON EVERY ROW
      PRINTX v_r_rechazo.*
      PRINTX p_descripcion_rechazo
END REPORT