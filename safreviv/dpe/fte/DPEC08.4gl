--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12/06/2012
--===============================================================
################################################################################
#Modulo       => DPE                                                           #
#Programa     => DPEC08                                                        #
#Objetivo     => Consultar los datos de diagnósticos de DPE Créditos           #
#Fecha inicio => 04/05/2012                                                    #
################################################################################

DATABASE safre_viv
GLOBALS "DPEG01.4gl"
DEFINE w ui.Window,
       f ui.Form,
       g_usuario_cod LIKE seg_usuario.usuario_cod,
       g_tipo_ejecucion SMALLINT,
       g_s_titulo CHAR(25),
       v_manejador_rpt    OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
       -- Record que se llenan con la informacion recuperada
       v_r_det_solicitud     RECORD
          v_folio            DECIMAL(9,0),
          v_nss              CHAR(11),
          v_credito          DECIMAL(10,0),
          v_fecha_registro   DATE,
          v_aportacion_reg   DECIMAL(9,2),
          v_amortizacion_reg DECIMAL(9,2),
          v_aportacion_sol   DECIMAL(9,2),
          v_amortizacion_sol DECIMAL(9,2),
          v_estado_solicitud SMALLINT,
          v_diagnostico      SMALLINT,
          v_desc_larga       CHAR(50)
       END RECORD,   
       v_ar_det_solicitud    DYNAMIC ARRAY OF RECORD -- registro llenar el detalle de la solicitudes
          v_folio            DECIMAL(9,0),
          v_nss              CHAR(11),
          v_credito          DECIMAL(10,0),
          v_fecha_registro   DATE,
          v_aportacion_reg   DECIMAL(9,2),
          v_amortizacion_reg DECIMAL(9,2),
          v_aportacion_sol   DECIMAL(9,2),
          v_amortizacion_sol DECIMAL(9,2),
          v_estado_solicitud CHAR(10),
          v_diagnostico      CHAR(50)
       END RECORD

MAIN -- Inicio de la funcion principal
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana
       
   -- se recupera la clave de usuario desde parametro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   LET g_usuario_cod = p_usuario_cod
   LET g_tipo_ejecucion = p_tipo_ejecucion
   LET g_s_titulo = p_s_titulo
   
   -- Estilo para la etiqueta estilo vinculo, llamada del reporte
--   CALL ui.interface.loadstyles("../../mdt/bin/mdtstyle")
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   --CALL ui.Interface.setContainer("name_1")
   --CALL ui.Interface.setType("child")
   -- consulta de informacion recibida DPE solo INFONAVIT
   CALL fn_genera_consulta_integra_creditos(p_usuario_cod)

END MAIN       

{ ======================================================================
Clave: DPEC08
Nombre: fn_genera_consulta_integra_creditos
Fecha creacion: 04/05/2012
  Genera consulta de datos integrados DPE solo INFONAVIT
======================================================================
}

FUNCTION fn_genera_consulta_integra_creditos(p_usuario_cod)
DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_cbx_folios        ui.ComboBox, -- combo de folios
       v_cbx_estado        ui.ComboBox, -- combo de estados
       v_folio             LIKE glo_folio.folio,
       v_estado_solicitud  SMALLINT,
       v_credito           DECIMAL(10,0),
       v_nss               CHAR(11),
       v_f_gen_archivo     DATE,
       v_i_conArch         INTEGER,
       v_s_cadena          STRING, -- cadena de texto
       v_r_glo_ctr_archivo RECORD
          folio            LIKE glo_folio.folio,
          nombre_archivo   LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
   
   OPEN WINDOW win_ConsInfonavit WITH FORM "DPEC080"
   -- Recupera punteros a ventana para control de grupos
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      -- se le asigna el apuntado der combo a la variable
      LET v_cbx_estado = ui.ComboBox.forName("formonly.cmb_estado")
      LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")
      -- Ocultar tablas de resultados
      CALL f.setElementHidden("resumensolicitudes",1)
      CALL f.setElementHidden("grudetalle",1)
      CALL f.setElementHidden("grutotal",1)
      LET INT_FLAG = FALSE
      -- se inicia el combobox en blanco
      CALL v_cbx_estado.clear()
      CALL v_cbx_folios.clear()
      ## Carga tempral de estados 04-05-2012
      CALL v_cbx_estado.addItem(1, "1 Aceptada")
      CALL v_cbx_estado.addItem(2, "2 Rechazada")
      -- se llena el arreglo de folios
      DECLARE cur_folios CURSOR FOR
      SELECT DISTINCT d.folio, g.nombre_archivo
      FROM glo_ctr_archivo g, glo_folio d
      WHERE g.proceso_cod = g_proceso_cod_dpe_credito
        AND g.estado = 2 -- integrado
        AND g.folio = d.folio
        
      LET v_i_conArch = 0
      
      FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
         LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " - ", 
             v_r_glo_ctr_archivo.nombre_archivo 
         CALL v_cbx_folios.addItem(v_r_glo_ctr_archivo.folio 
              ,v_s_cadena)
         -- Contador de archivos eoncontrados
         LET v_i_conArch = v_i_conArch + 1
      END FOREACH
      --Se valida si existe información para mostrar
      IF v_i_conArch < 1 THEN
         CALL fn_mensaje("Atención",
                         "No existen archivos recientemente integrados para consulta",
                         "info")
         CLOSE WINDOW win_ConsInfonavit
         RETURN
      END IF
      -- Se libera el cursor
      FREE cur_folios
      
      CALL v_cbx_estado.addItem(-1," ")
      CALL v_cbx_folios.addItem(-1," ")
      -- se asignan los valores por omision
      LET v_folio            = -1
      LET v_estado_solicitud = -1
      LET v_nss              = NULL
      LET v_credito          = NULL
      LET v_f_gen_archivo    = NULL
      
      DISPLAY TODAY USING "dd-mm-yyyy" TO txt_fecha
      
      INPUT v_folio, v_nss, v_estado_solicitud, v_credito, v_f_gen_archivo WITHOUT DEFAULTS
         FROM cmb_folio, txt_nss, cmb_estado, txt_credito, txt_f_gen_archivo
      ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
      ON ACTION ACCEPT
         -- Mostrar tablas de resultados
         CALL f.setElementHidden("resumensolicitudes",0)
         CALL f.setElementHidden("grudetalle",0)
         CALL f.setElementHidden("grutotal",0)
         CALL fn_muestra_consulta_integra_creditos(p_usuario_cod, v_folio, 
                                                    v_nss, v_estado_solicitud, 
                                                    v_credito, v_f_gen_archivo)
         -- Ocultar nuevamente las tablas de resultados
         CALL f.setElementHidden("resumensolicitudes",1)
         CALL f.setElementHidden("grudetalle",1)
         CALL f.setElementHidden("grutotal",1)
         -- Limpia datos para nueva consulta
         LET v_folio            = -1
         LET v_estado_solicitud = -1
         LET v_nss              = NULL
         LET v_credito          = NULL
         LET v_f_gen_archivo    = NULL
         
         DISPLAY v_folio, v_nss, v_estado_solicitud, v_credito, v_f_gen_archivo
            TO cmb_folio, txt_nss, cmb_estado, txt_credito, txt_f_gen_archivo
         
         LET INT_FLAG = FALSE
         #EXIT INPUT

      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         EXIT INPUT
      
      END INPUT
   CLOSE WINDOW win_ConsInfonavit   
END FUNCTION

#OBJETIVO: Muestra el detalle de la consulta de créditos.
FUNCTION fn_muestra_consulta_integra_creditos(p_usuario_cod, p_folio, 
                                               p_nss, p_estado_solicitud, 
                                               p_credito, p_f_gen_archivo)

DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio               LIKE glo_folio.folio,
       p_nss                 CHAR(11),
       p_estado_solicitud    SMALLINT,
       p_credito             DECIMAL(10,0),
       p_f_gen_archivo       DATE,
       --
       v_s_sql               STRING, -- cadena con una instruccion SQL
       v_si_indice           SMALLINT, -- indice de arreglo
       v_r_res_solicitudes   RECORD -- registro llenar el resumen de solicitudes
       	  v_llave_sdd        DECIMAL(6,0),
       	  v_nrp              CHAR(11),
       	  v_folio_sua        INTEGER,
       	  v_fecha_pago       DATE,
       	  v_periodo_pago     CHAR(6),
       	  v_fec_archivo      DATE
       END RECORD,
       v_ar_res_solicitudes DYNAMIC ARRAY OF RECORD -- registro llenar el resumen de solicitudes
          v_llave_sdd        DECIMAL(6,0),
       	  v_nrp              CHAR(11),
       	  v_folio_sua        INTEGER,
       	  v_fecha_pago       DATE,
       	  v_periodo_pago     CHAR(6),
       	  v_fec_archivo      DATE
       END RECORD,
       -- Variables dedicadas al reporte
       r_ruta_bin         LIKE seg_modulo.ruta_bin,
       v_nom_reporte      VARCHAR(80), -- nombre del reporte
       r_ruta_listados    LIKE seg_modulo.ruta_listados,
       v_s_reporte        STRING,
       v_id_derechohabiente DECIMAL(9,0),
       v_comando STRING --Comando de ejecución de consultas generales
       
   CALL v_ar_res_solicitudes.clear()
   
   -- Consulta que llena el arreglo del resumen de la solicitudes
   LET v_s_sql = "SELECT a.llave_sdd, a.nrp, a.folio_sua, a.fecha_pago, a.periodo_pago,",
                 "\n b.fecha_archivo",
                 "\n FROM dpe_sol_creditos a,",
                 "\n      dpe_creditos_arch b",
                 "\n WHERE a.folio = b.folio"
                 
   
   -- Si se envio nss
   IF p_nss IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF p_estado_solicitud <> -1 THEN
      IF p_estado_solicitud = 1 THEN
         LET v_s_sql = v_s_sql CLIPPED
             ,"\n  AND a.estado_solicitud IN (1,3,4,5)"
      ELSE
         IF p_estado_solicitud = 2 THEN
            LET v_s_sql = v_s_sql CLIPPED
               ,"\n  AND a.estado_solicitud IN (2)"
      	 END IF      
      END IF       
   END IF


   -- si se envio folio
   IF (p_folio <> - 1 )THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n   AND a.folio = ",p_folio
   END IF

   -- si se envio credito
   IF p_credito IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.credito =",p_credito
   END IF
   
   -- si se envio fecha de generacion del archivo
   IF p_f_gen_archivo IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.fecha_archivo =",p_f_gen_archivo
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n GROUP BY 1,2,3,4,5,6"
       ,"\n ORDER BY 1,2,3,4,5,6"
   
   --DISPLAY "v_s_sql_resumen solicitudes: ", v_s_sql
   PREPARE Prpr_ObtResumen_solicitudes FROM v_s_sql CLIPPED
   DECLARE Curr_ObtResumen_solicitudes CURSOR FOR Prpr_ObtResumen_solicitudes
   
   LET v_si_indice = 0
   
   FOREACH Curr_ObtResumen_solicitudes INTO v_r_res_solicitudes.*
      LET v_si_indice = v_si_indice + 1
      LET v_ar_res_solicitudes[v_si_indice].v_llave_sdd = v_r_res_solicitudes.v_llave_sdd
      LET v_ar_res_solicitudes[v_si_indice].v_nrp = v_r_res_solicitudes.v_nrp
      LET v_ar_res_solicitudes[v_si_indice].v_folio_sua = v_r_res_solicitudes.v_folio_sua
      LET v_ar_res_solicitudes[v_si_indice].v_fecha_pago = v_r_res_solicitudes.v_fecha_pago
      LET v_ar_res_solicitudes[v_si_indice].v_periodo_pago = v_r_res_solicitudes.v_periodo_pago
      LET v_ar_res_solicitudes[v_si_indice].v_fec_archivo = v_r_res_solicitudes.v_fec_archivo
   END FOREACH
   
   DISPLAY p_folio, p_nss, p_estado_solicitud, p_credito, p_f_gen_archivo
      TO cmb_folio, txt_nss, cmb_estado, txt_credito, txt_f_gen_archivo

      IF v_si_indice = 0 THEN 
         -- Ocultar tablas de resultados
         CALL f.setElementHidden("resumensolicitudes",1)
         CALL f.setElementHidden("grudetalle",1)
         CALL f.setElementHidden("grutotal",1)
         
         CALL fn_mensaje("Atención","No se encontraron registros con los datos proporcionados","error")
      END IF 
      
   DIALOG ATTRIBUTES (UNBUFFERED)
      DISPLAY ARRAY v_ar_res_solicitudes TO scr_resumen_solicitudes.*
      BEFORE ROW

         CALL fn_genera_detalle_montos(p_folio, p_nss, p_estado_solicitud,
                                       p_credito, p_f_gen_archivo,
                                       v_ar_res_solicitudes[ARR_CURR()].v_llave_sdd,
                                       v_ar_res_solicitudes[ARR_CURR()].v_nrp,
                                       v_ar_res_solicitudes[ARR_CURR()].v_periodo_pago)
      BEFORE DISPLAY
         CALL fn_genera_totales(p_folio, p_nss, p_estado_solicitud, p_credito, 
                                p_f_gen_archivo)
      ON ACTION regresa
         LET v_s_reporte = "" -- Se Limpia la variable que almacena el reporte
         EXIT DIALOG
      ON ACTION reporte
         # Recupera la ruta de listados en el que se enviara el archivo
         CALL fn_rutas("dpe") RETURNING r_ruta_bin, r_ruta_listados
         # Se indica que el reporte usara la plantilla creada
           IF fgl_report_loadCurrentSettings("DPEC08.4rp") THEN
              CALL fgl_report_selectDevice("PDF")
              LET v_nom_reporte = p_usuario_cod CLIPPED || "-DPEC08-","00000","-","00000","-","00000"||".pdf"
              CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
                 -- llamado a la función que arma la consulta para el reporte
              CALL fn_reporte_integracion_creditos(p_usuario_cod, p_folio, 
                                                    p_nss, p_estado_solicitud, 
                                                    p_credito, p_f_gen_archivo)
                                                     RETURNING v_si_indice
              IF v_si_indice = 0 THEN
                    CALL fn_mensaje("Atención",
                                    "No se encontraron datos con los criterios indicados",
                                    "information")
              ELSE
                    LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','dpedocto')\" target='nueva'>",
                                                                               v_nom_reporte CLIPPED,"</a>"
              END IF
           ELSE
              DISPLAY "No fue posible generar el reporte"
           END IF
           
           DISPLAY v_s_reporte TO v_s_reporte   
         
      END DISPLAY
      
      DISPLAY ARRAY v_ar_det_solicitud TO scr_det_solicitud.*
      BEFORE DISPLAY
         ON ACTION Consulta_Derechohabiente
            -- Muestra la consulta del derechohabiente en general
            --CALL fn_preconsulta(v_ar_det_solicitud[ARR_CURR()].v_nss)
            {CALL fn_preconsulta(v_ar_det_solicitud[ARR_CURR()].v_nss,g_usuario_cod,
                                g_tipo_ejecucion, g_s_titulo)
            }
            --Se ejecuta la función general de consulta de Derechohabiente
            LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod || "1 'Consulta de Derechohabiente' " || v_ar_det_solicitud[ARR_CURR()].v_nss
            RUN v_comando
         
         ON ACTION Consulta_Saldo
         -- Muestra la consulta del saldo
            --CALL fn_eje_consulta_saldo(v_ar_det_solicitud[ARR_CURR()].v_nss)
            SELECT id_derechohabiente
              INTO v_id_derechohabiente
              FROM afi_derechohabiente
              WHERE nss = v_ar_det_solicitud[ARR_CURR()].v_nss
            
            CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente, 
                                          g_tipo_ejecucion, g_s_titulo)
         ON ACTION regresa
            EXIT DIALOG   
      END DISPLAY
      
   END DIALOG

END FUNCTION

{
===========================================================================
Clave: DPEC08
Nombre: fn_genera_detalle_montos
Fecha creacion: 04/05/2012
Narrativa del proceso que realiza:
"GENERA EL DETALLE DE LA SOLICITUD"
==========================================================================
}
FUNCTION fn_genera_detalle_montos(p_folio, p_nss, p_estado_solicitud,
                                  p_credito, p_f_gen_archivo, 
                                  p_llave_sdd, p_nrp, p_periodo_pago)
DEFINE p_folio              LIKE glo_folio.folio,
       p_nss                CHAR(11),
       p_estado_solicitud   SMALLINT,
       p_credito            DECIMAL(10,0),
       p_f_gen_archivo      DATE,
       p_llave_sdd          DECIMAL(6,0),
       p_nrp                CHAR(11),
       p_periodo_pago       CHAR(6),
       v_s_sql              STRING, -- cadena con una instruccion SQL
       v_si_detalle         SMALLINT
       
    CALL v_ar_det_solicitud.clear()
           
    LET v_s_sql = "SELECT a.folio, a.nss, a.credito, b.fecha_registro,",
                  "\n a.aportacion_reg, a.amortizacion_reg,0,0,",
                  "\n a.estado_solicitud, c.diagnostico,",
                  "\n d.diag_desc_larga",
                  "\n FROM dpe_sol_creditos a,",
                  "\n      dpe_creditos_arch b,",                 
                  "\n OUTER (dpe_rch_archivo c,",
                  "\n        dpe_diagnostico_sol d)",
                  "\n WHERE a.folio = b.folio",
                  "\n AND a.folio = c.folio",
                  "\n AND a.id_dpe_referencia = c.id_dpe_referencia",
                  "\n AND c.diagnostico = d.diagnostico",
                  "\n AND a.nrp = '",p_nrp,"'",
                  "\n AND a.periodo_pago = '",p_periodo_pago,"'",
                  "\n AND a.llave_sdd = ", p_llave_sdd
DISPLAY v_s_sql
   --si se envia folio
   IF (p_folio <> -1) THEN 
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n AND a.folio = ", p_folio
   END IF
   -- Si se envio nss
   IF p_nss IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF p_estado_solicitud <> -1 THEN
      IF p_estado_solicitud = 1 THEN
         LET v_s_sql = v_s_sql CLIPPED
             ,"\n  AND a.estado_solicitud IN (1,3,4,5)"
      ELSE
         IF p_estado_solicitud = 2 THEN
            LET v_s_sql = v_s_sql CLIPPED
               ,"\n  AND a.estado_solicitud IN (2)"
      	 END IF      
      END IF       
   END IF
   
   -- si se envio credito
   IF p_credito IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.credito =",p_credito
   END IF
   
   -- si se envio fecha de generacion del archivo
   IF p_f_gen_archivo IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.fecha_archivo =",p_f_gen_archivo
   END IF

   LET v_s_sql = v_s_sql CLIPPED
      ,"\n ORDER BY 9"

    --DISPLAY "v_s_sql_montos: ", v_s_sql
    PREPARE Prpr_Obtdetalle_solicitud FROM v_s_sql CLIPPED
    DECLARE Curr_Obtdetalle_solicitud CURSOR FOR Prpr_Obtdetalle_solicitud
    
    LET v_si_detalle = 0
    
    FOREACH Curr_Obtdetalle_solicitud INTO v_r_det_solicitud.*
       LET v_si_detalle = v_si_detalle + 1
       LET v_ar_det_solicitud[v_si_detalle].v_folio = v_r_det_solicitud.v_folio
       LET v_ar_det_solicitud[v_si_detalle].v_nss = v_r_det_solicitud.v_nss
       LET v_ar_det_solicitud[v_si_detalle].v_credito = v_r_det_solicitud.v_credito
       LET v_ar_det_solicitud[v_si_detalle].v_fecha_registro = v_r_det_solicitud.v_fecha_registro
       LET v_ar_det_solicitud[v_si_detalle].v_aportacion_reg = v_r_det_solicitud.v_aportacion_reg
       LET v_ar_det_solicitud[v_si_detalle].v_amortizacion_reg = v_r_det_solicitud.v_amortizacion_reg
       LET v_ar_det_solicitud[v_si_detalle].v_aportacion_sol = v_r_det_solicitud.v_aportacion_sol
       LET v_ar_det_solicitud[v_si_detalle].v_amortizacion_sol = v_r_det_solicitud.v_amortizacion_sol
       CASE v_r_det_solicitud.v_estado_solicitud
         WHEN 1 -- Aceptada
            LET v_ar_det_solicitud[v_si_detalle].v_estado_solicitud = "Aceptada"
            LET v_ar_det_solicitud[v_si_detalle].v_diagnostico = "Solicitud Integrada"
         WHEN 2 -- Rechazada
            LET v_ar_det_solicitud[v_si_detalle].v_estado_solicitud = "Rechazada"
            LET v_ar_det_solicitud[v_si_detalle].v_diagnostico = v_r_det_solicitud.v_diagnostico||" - "||
                                                                 v_r_det_solicitud.v_desc_larga
         WHEN 3 -- Aceptada preliquidada
            LET v_ar_det_solicitud[v_si_detalle].v_estado_solicitud = "Aceptada"
            LET v_ar_det_solicitud[v_si_detalle].v_diagnostico = "Solicitud Preliquidada"
         WHEN 4 -- Aceptada liquida
            LET v_ar_det_solicitud[v_si_detalle].v_estado_solicitud = "Aceptada"
            LET v_ar_det_solicitud[v_si_detalle].v_diagnostico = "Solicitud Liquidada"
         WHEN 5 -- Aceptada generadas archivo
            LET v_ar_det_solicitud[v_si_detalle].v_estado_solicitud = "Aceptada"
            LET v_ar_det_solicitud[v_si_detalle].v_diagnostico = "Se genero un archivo"
      END CASE
      
    END FOREACH	
END FUNCTION

{
===========================================================================
Clave: DPEC08
Nombre: fn_genera_totales
Fecha creacion: 07/05/2012
Narrativa del proceso que realiza:
"GENERA EL TOTAL DE LA TRANSACCION"
==========================================================================
}

FUNCTION fn_genera_totales(p_folio, p_nss, p_estado_solicitud, p_credito, 
                           p_f_gen_archivo)
DEFINE p_folio                   LIKE glo_folio.folio,
       p_nss                     CHAR(11),
       p_estado_solicitud        SMALLINT,
       p_credito                 DECIMAL(10,0),
       p_f_gen_archivo           DATE,
       --
       v_s_sql                   STRING, -- cadena con una instruccion SQL
       v_tot_aportacion_reg_acep DECIMAL(11,2),
       v_tot_amortiza_reg_acep   DECIMAL(11,2),
       v_tot_aportacion_sol_acep DECIMAL(11,2),
       v_tot_amortiza_sol_acep   DECIMAL(11,2),
       v_tot_aceptadas           SMALLINT,
       v_tot_aportacion_reg_rech DECIMAL(11,2),
       v_tot_amortiza_reg_rech   DECIMAL(11,2),
       v_tot_aportacion_sol_rech DECIMAL(11,2),
       v_tot_amortiza_sol_rech   DECIMAL(11,2),
       v_tot_rechazadas          SMALLINT,
       v_sum_tot_aportacion_reg  DECIMAL(11,2),
       v_sum_tot_amortiza_reg    DECIMAL(11,2),
       v_sum_tot_aportacion_sol  DECIMAL(11,2),
       v_sum_tot_amortiza_sol    DECIMAL(11,2),
       v_sum_tot_solicitudes     SMALLINT
   
   LET v_sum_tot_aportacion_reg = 0
   LET v_sum_tot_amortiza_reg = 0
   LET v_sum_tot_aportacion_sol = 0
   LET v_sum_tot_amortiza_sol = 0
   LET v_sum_tot_solicitudes = 0
   
   -- Se llenan los registros aceptados
   LET v_s_sql = "SELECT SUM(a.aportacion_reg), SUM(a.amortizacion_reg),",
                 --"\n     SUM(a.aportacion_sol), SUM(a.amortizacion_sol),",
                 "\n     COUNT(*)",
                 "\n FROM dpe_sol_creditos a,",
                 "\n      dpe_creditos_arch b",
                 --"\n FROM dpe_sol_soloinfonavit a,",
                 --"\n      dpe_soloinfonavit_arch b",
                 "\n WHERE a.folio = b.folio",
                 "\n  AND a.estado_solicitud IN (1,3,4,5)"

   --si se envia folio
   IF (p_folio <> -1) THEN 
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n AND a.folio = ", p_folio
   END IF                 
   
   -- Si se envio nss
   IF p_nss IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF p_estado_solicitud <> -1 THEN
      IF p_estado_solicitud = 1 THEN
         LET v_s_sql = v_s_sql CLIPPED
             ,"\n  AND a.estado_solicitud IN (1,3,4,5)"
      ELSE
         IF p_estado_solicitud = 2 THEN
            LET v_s_sql = v_s_sql CLIPPED
               ,"\n  AND a.estado_solicitud = 2"
      	 END IF      
      END IF       
   END IF
   
   -- si se envio credito
   IF p_credito IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.credito =",p_estado_solicitud
   END IF
   
   -- si se envio fecha de generacion del archivo
   IF p_f_gen_archivo IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.fecha_archivo =",p_estado_solicitud
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
   
   PREPARE Prpr_ObtAcep_solicitud FROM v_s_sql CLIPPED
   EXECUTE Prpr_ObtAcep_solicitud INTO v_tot_aportacion_reg_acep,
                                       v_tot_amortiza_reg_acep,
                                       --v_tot_aportacion_sol_acep,
                                       --v_tot_amortiza_sol_acep,
                                       v_tot_aceptadas
   
   -- Se llenan los registros rechazados
   LET v_s_sql = "SELECT SUM(a.aportacion_reg), SUM(a.amortizacion_reg),",
                 --"\n     SUM(a.aportacion_sol), SUM(a.amortizacion_sol),",
                 "\n     COUNT(*)",
                 --"\n FROM dpe_sol_soloinfonavit a, dpe_soloinfonavit_arch c",
                 "\n FROM dpe_sol_creditos a,",
                 "\n      dpe_creditos_arch c", 
                 "\n WHERE c.folio = a.folio",
                 "\n  AND a.estado_solicitud = 2"

   --si se envia folio
   IF (p_folio <> -1) THEN 
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n AND a.folio = ", p_folio
   END IF
                 
   -- Si se envio nss
   IF p_nss IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF p_estado_solicitud <> -1 THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.estado_solicitud =",p_estado_solicitud
   END IF
   
   -- si se envio credito
   IF p_credito IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.credito =",p_credito
   END IF
   
   -- si se envio fecha de generacion del archivo
   IF p_f_gen_archivo IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND c.fecha_archivo =",p_f_gen_archivo
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED

   PREPARE Prpr_ObtRech_solicitud FROM v_s_sql CLIPPED
   EXECUTE Prpr_ObtRech_solicitud INTO v_tot_aportacion_reg_rech,
                                       v_tot_amortiza_reg_rech,
                                       --v_tot_aportacion_sol_rech,
                                       --v_tot_amortiza_sol_rech,
                                       v_tot_rechazadas
  
  -- Se hace la sumatoria de los registros
  LET v_sum_tot_aportacion_reg = v_sum_tot_aportacion_reg + v_tot_aportacion_reg_acep +
                                 v_tot_aportacion_reg_rech
  LET v_sum_tot_amortiza_reg = v_sum_tot_amortiza_reg + v_tot_amortiza_reg_acep +
                               v_tot_amortiza_reg_rech
                               
  LET v_sum_tot_aportacion_sol = 0
  LET v_sum_tot_amortiza_sol =  0
  
--  LET v_sum_tot_aportacion_sol = v_sum_tot_aportacion_sol + v_tot_aportacion_sol_acep +
--                                 v_tot_aportacion_sol_rech
--  LET v_sum_tot_amortiza_sol = v_sum_tot_amortiza_sol + v_tot_amortiza_sol_acep +
--                               v_tot_amortiza_sol_rech

  LET v_sum_tot_solicitudes = v_sum_tot_solicitudes + v_tot_aceptadas + v_tot_rechazadas
  
  DISPLAY v_tot_aportacion_reg_acep, v_tot_amortiza_reg_acep, v_tot_aportacion_sol_acep,
          v_tot_amortiza_sol_acep, v_tot_aceptadas,
          v_tot_aportacion_reg_rech, v_tot_amortiza_reg_rech, v_tot_aportacion_sol_rech,
          v_tot_amortiza_sol_rech, v_tot_rechazadas,
          v_sum_tot_aportacion_reg, v_sum_tot_amortiza_reg, v_sum_tot_aportacion_sol,
          v_sum_tot_amortiza_sol, v_sum_tot_solicitudes
     TO txt_tot_aportacion_reg_acep, txt_tot_amortiza_reg_acep, txt_tot_aportacion_sol_acep,
        txt_tot_amortiza_sol_acep, txt_tot_aceptadas,
        txt_tot_aportacion_reg_rech, txt_tot_amortiza_reg_rech, txt_tot_aportacion_sol_rech,
        txt_tot_amortiza_sol_rech, txt_tot_rechazadas,
        txt_sum_tot_aportacion_reg, txt_sum_tot_amortiza_reg, txt_sum_tot_aportacion_sol,
        txt_sum_tot_amortiza_sol, txt_sum_tot_solicitudes
       
END FUNCTION

{
======================================================================
Clave: DPEC08
Nombre: fn_reporte_integracion_creditos
Fecha creacion: 07/05/2012
Narrativa del proceso que realiza:
Muetra detalle de proceso de integración: 
"REPORTE DE DIAGNOSTICOS DE DEVOLUCION DE PAGOS EN EXCESO SOLO INFONAVIT"
======================================================================
}

FUNCTION fn_reporte_integracion_creditos(p_usuario_cod, p_folio, 
                                          p_nss, p_estado_solicitud, 
                                          p_credito, p_f_gen_archivo)

DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio             LIKE glo_folio.folio,
       p_nss               CHAR(11),
       p_estado_solicitud  SMALLINT,
       p_credito           DECIMAL(10,0),
       p_f_gen_archivo     DATE,
       -- Record principal
       v_r_rep_solicitudes RECORD
          v_nss              CHAR(11),
          v_credito          DECIMAL(10),
          v_folio_sua        INTEGER,
          v_periodo_pago     CHAR(6),
          v_llave_sdd        DECIMAL(6),
          v_fecha_pago       DATE,
          v_nrp              CHAR(11),
          v_aportacion_reg   DECIMAL(9,2),
          v_amortizacion_reg DECIMAL(9,2),
          --v_aportacion_sol   DECIMAL(9,2),
          --v_amortizacion_sol DECIMAL(9,2),
          v_estado_solicitud CHAR(10)
       END RECORD,
       -- Record para alamancenar los montos aceptados
       v_r_rep_aceptadas RECORD
          v_tot_aportacion_reg DECIMAL(12,2),
          v_tot_amortiza_reg   DECIMAL(12,2),
          --v_tot_aportacion_sol DECIMAL(12,2),
          --v_tot_amortiza_sol   DECIMAL(12,2),
          v_tot_aceptadas      SMALLINT
       END RECORD,
       --Record para almacenar los montos de rechazo
       v_r_rep_rechazadas RECORD
          v_tot_aportacion_reg DECIMAL(12,2),
          v_tot_amortiza_reg   DECIMAL(12,2),
          --v_tot_aportacion_sol DECIMAL(12,2),
          --v_tot_amortiza_sol   DECIMAL(12,2),
          v_tot_rechazadas     SMALLINT
       END RECORD,
       v_r_rep_total RECORD
          v_sum_tot_aportacion_reg  DECIMAL(12,2),
          v_sum_tot_amortiza_reg    DECIMAL(12,2),
          --v_sum_tot_aportacion_sol  DECIMAL(12,2),
          --v_sum_tot_amortiza_sol    DECIMAL(12,2),
          v_sum_tot_solicitudes     SMALLINT
      END RECORD,    
       --
       v_s_sql                 STRING,  -- cadena con una instruccion SQL
       v_si_indice             SMALLINT, -- indice de arreglo
       cadena_estado_solicitud CHAR(10) -- Almancena el significado del estado de la solicitud
   
   LET v_r_rep_aceptadas.v_tot_aportacion_reg = 0
   LET v_r_rep_aceptadas.v_tot_amortiza_reg = 0
   --LET v_r_rep_aceptadas.v_tot_aportacion_sol = 0
   --LET v_r_rep_aceptadas.v_tot_amortiza_sol = 0
   LET v_r_rep_aceptadas.v_tot_aceptadas = 0
   LET v_r_rep_rechazadas.v_tot_aportacion_reg = 0
   LET v_r_rep_rechazadas.v_tot_amortiza_reg = 0
   --LET v_r_rep_rechazadas.v_tot_aportacion_sol = 0
   --LET v_r_rep_rechazadas.v_tot_amortiza_sol = 0
   LET v_r_rep_rechazadas.v_tot_rechazadas = 0
   LET v_r_rep_total.v_sum_tot_aportacion_reg = 0
   LET v_r_rep_total.v_sum_tot_amortiza_reg = 0
   --LET v_r_rep_total.v_sum_tot_aportacion_sol = 0
   --LET v_r_rep_total.v_sum_tot_amortiza_sol = 0 
   LET v_r_rep_total.v_sum_tot_solicitudes = 0  
   
   -- Se prepara la consulta del reporte
   LET v_s_sql = "SELECT a.nss, a.credito, a.folio_sua, a.periodo_pago, a.llave_sdd,",
                 "\n a.fecha_pago, a.nrp, a.aportacion_reg,a.amortizacion_reg,",
                 --"\n a.aportacion_sol,a.amortizacion_sol,",
                 "\n CASE a.estado_solicitud WHEN 1 THEN 'Aceptada'", 
                                           " WHEN 3 THEN 'Aceptada'",
                                           " WHEN 4 THEN 'Aceptada'",
                                           " WHEN 5 THEN 'Aceptada'",
                                           " ELSE 'Rechazada' END",
                 --"\n FROM dpe_sol_soloinfonavit a, dpe_soloinfonavit_arch b",
                 "\n FROM dpe_sol_creditos a,",
                 "\n      dpe_creditos_arch b",
                 "\n WHERE a.folio = b.folio",
                 "\n AND a.folio = ",p_folio
   
   -- Si se envio nss
   IF p_nss IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF p_estado_solicitud <> -1 THEN
      IF p_estado_solicitud = 1 THEN
         LET v_s_sql = v_s_sql CLIPPED
             ,"\n  AND a.estado_solicitud IN (1,3,4,5)"
      ELSE
         IF p_estado_solicitud = 2 THEN
            LET v_s_sql = v_s_sql CLIPPED
               ,"\n  AND a.estado_solicitud IN (2)"
      	 END IF      
      END IF       
   END IF
   
   -- si se envio credito
   IF p_credito IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.credito =",p_estado_solicitud
   END IF
   
   IF p_estado_solicitud = -1 THEN
      LET p_estado_solicitud = ""
   END IF
   
   -- si se envio fecha de generacion del archivo
   IF p_f_gen_archivo IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND b.fecha_archivo =",p_estado_solicitud
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n ORDER BY 10"
       
   CASE p_estado_solicitud
   WHEN -1
      LET cadena_estado_solicitud = ""
   WHEN 1
      LET cadena_estado_solicitud = "Aceptadas"
   WHEN 2
      LET cadena_estado_solicitud = "Rechazadas"
   END CASE
  
   DISPLAY v_s_sql
   --DISPLAY "v_s_sql_reporte solicitudes: ", v_s_sql
   PREPARE PrRp_ObtResumen_solicitudes FROM v_s_sql CLIPPED
   DECLARE CuRp_ObtResumen_solicitudes CURSOR FOR PrRp_ObtResumen_solicitudes
   
   LET v_si_indice = 0
   CALL fgl_report_selectPreview(0)
   LET v_manejador_rpt = fgl_report_commitCurrentSettings()
   # Inicia el reporte de consulta historica por derechohabiente
   START REPORT reporte_diagnostico_creditos TO XML HANDLER v_manejador_rpt
   FOREACH CuRp_ObtResumen_solicitudes INTO v_r_rep_solicitudes.*
      LET v_si_indice = v_si_indice + 1
      LET v_r_rep_total.v_sum_tot_aportacion_reg = 0
      LET v_r_rep_total.v_sum_tot_amortiza_reg = 0
      --LET v_r_rep_total.v_sum_tot_aportacion_sol = 0
      --LET v_r_rep_total.v_sum_tot_amortiza_sol = 0 
      LET v_r_rep_total.v_sum_tot_solicitudes = 0 
      -- Según sea el caso estado de la solicitud, se suman las aceptadas y rechazadas
      CASE v_r_rep_solicitudes.v_estado_solicitud
         WHEN "Aceptada"
            LET v_r_rep_aceptadas.v_tot_aportacion_reg = v_r_rep_aceptadas.v_tot_aportacion_reg +
                                                         v_r_rep_solicitudes.v_aportacion_reg
            LET v_r_rep_aceptadas.v_tot_amortiza_reg = v_r_rep_aceptadas.v_tot_amortiza_reg +
                                                       v_r_rep_solicitudes.v_amortizacion_reg
            {LET v_r_rep_aceptadas.v_tot_aportacion_sol = v_r_rep_aceptadas.v_tot_aportacion_sol +
                                                         v_r_rep_solicitudes.v_aportacion_sol
            LET v_r_rep_aceptadas.v_tot_amortiza_sol = v_r_rep_aceptadas.v_tot_amortiza_sol +
                                                       v_r_rep_solicitudes.v_amortizacion_sol
            }
            LET v_r_rep_aceptadas.v_tot_aceptadas = v_r_rep_aceptadas.v_tot_aceptadas + 1
         WHEN "Rechazada"
            LET v_r_rep_rechazadas.v_tot_aportacion_reg = v_r_rep_rechazadas.v_tot_aportacion_reg +
                                                          v_r_rep_solicitudes.v_aportacion_reg
            LET v_r_rep_rechazadas.v_tot_amortiza_reg = v_r_rep_rechazadas.v_tot_amortiza_reg +
                                                        v_r_rep_solicitudes.v_amortizacion_reg
            {LET v_r_rep_rechazadas.v_tot_aportacion_sol = v_r_rep_rechazadas.v_tot_aportacion_sol +
                                                          v_r_rep_solicitudes.v_aportacion_sol
            LET v_r_rep_rechazadas.v_tot_amortiza_sol = v_r_rep_rechazadas.v_tot_amortiza_sol +
                                                        v_r_rep_solicitudes.v_amortizacion_sol
            }
           LET v_r_rep_rechazadas.v_tot_rechazadas = v_r_rep_rechazadas.v_tot_rechazadas + 1
      END CASE
      
      -- Acumula totales, para hacer una sumatoria total
      LET v_r_rep_total.v_sum_tot_aportacion_reg = v_r_rep_total.v_sum_tot_aportacion_reg +
                                                   v_r_rep_aceptadas.v_tot_aportacion_reg +
                                                   v_r_rep_rechazadas.v_tot_aportacion_reg
      LET v_r_rep_total.v_sum_tot_amortiza_reg = v_r_rep_total.v_sum_tot_amortiza_reg +
                                                 v_r_rep_aceptadas.v_tot_amortiza_reg +
                                                 v_r_rep_rechazadas.v_tot_amortiza_reg
      {LET v_r_rep_total.v_sum_tot_aportacion_sol = v_r_rep_total.v_sum_tot_aportacion_sol +
                                                   v_r_rep_aceptadas.v_tot_aportacion_sol +
                                                   v_r_rep_rechazadas.v_tot_aportacion_sol
      LET v_r_rep_total.v_sum_tot_amortiza_sol = v_r_rep_total.v_sum_tot_amortiza_sol +
                                                 v_r_rep_aceptadas.v_tot_amortiza_sol +
                                                 v_r_rep_rechazadas.v_tot_amortiza_sol
      }
      LET v_r_rep_total.v_sum_tot_solicitudes = v_r_rep_total.v_sum_tot_solicitudes +
                                                v_r_rep_aceptadas.v_tot_aceptadas +
                                                v_r_rep_rechazadas.v_tot_rechazadas
      
      -- Se llena el reporte con la información recuperada
         OUTPUT TO REPORT reporte_diagnostico_creditos(p_usuario_cod, 
                                                       p_folio, 
                                                       p_nss, 
                                                       cadena_estado_solicitud, 
                                                       p_credito, 
                                                       p_f_gen_archivo,
                                                       v_r_rep_solicitudes.*,
                                                       v_r_rep_aceptadas.*,
                                                       v_r_rep_rechazadas.*,
                                                       v_r_rep_total.*)
   END FOREACH
   FINISH REPORT reporte_diagnostico_creditos
   FREE CuRp_ObtResumen_solicitudes
   RETURN v_si_indice
END FUNCTION

REPORT reporte_diagnostico_creditos(p_usuario_cod,
                                    p_folio,
                                    p_nss,
                                    p_cadena_solicitud,
                                    p_credito,
                                    p_f_gen_archivo,
                                    v_r_rep_solicitudes,
                                    v_r_rep_aceptadas,
                                    v_r_rep_rechazadas,
                                    v_r_rep_total)

DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio             LIKE glo_folio.folio,
       p_nss               CHAR(11),
       p_cadena_solicitud  CHAR(10),
       p_credito           DECIMAL(10,0),
       p_f_gen_archivo     DATE,
       -- Record principal
       v_r_rep_solicitudes RECORD
          v_nss              CHAR(11),
          v_credito          DECIMAL(10),
          v_folio_sua        INTEGER,
          v_periodo_pago     CHAR(6),
          v_llave_sdd        DECIMAL(6),
          v_fecha_pago       DATE,
          v_nrp              CHAR(11),
          v_aportacion_reg   DECIMAL(9,2),
          v_amortizacion_reg DECIMAL(9,2),
          --v_aportacion_sol   DECIMAL(9,2),
          --v_amortizacion_sol DECIMAL(9,2),
          v_estado_solicitud CHAR(10)
       END RECORD,
       -- Record para alamancenar los montos aceptados
       v_r_rep_aceptadas RECORD
          v_tot_aportacion_reg DECIMAL(12,2),
          v_tot_amortiza_reg   DECIMAL(12,2),
          --v_tot_aportacion_sol DECIMAL(12,2),
          --v_tot_amortiza_sol   DECIMAL(12,2),
          v_tot_aceptadas      SMALLINT
       END RECORD,
       --Record para almacenar los montos de rechazo
       v_r_rep_rechazadas RECORD
          v_tot_aportacion_reg DECIMAL(12,2),
          v_tot_amortiza_reg   DECIMAL(12,2),
          --v_tot_aportacion_sol DECIMAL(12,2),
          --v_tot_amortiza_sol   DECIMAL(12,2),
          v_tot_rechazadas     SMALLINT
       END RECORD,
       v_r_rep_total RECORD
          v_sum_tot_aportacion_reg  DECIMAL(12,2),
          v_sum_tot_amortiza_reg    DECIMAL(12,2),
          --v_sum_tot_aportacion_sol  DECIMAL(12,2),
          --v_sum_tot_amortiza_sol    DECIMAL(12,2),
          v_sum_tot_solicitudes     SMALLINT
      END RECORD, 
       v_fecha_reporte         DATE
   
   FORMAT
   
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod
      PRINTX p_folio
      PRINTX p_nss
      PRINTX p_cadena_solicitud
      PRINTX p_credito
      PRINTX p_f_gen_archivo
   
   ON EVERY ROW
      PRINTX v_r_rep_solicitudes.*
   
   ON LAST ROW
      PRINTX v_r_rep_aceptadas.*
      PRINTX v_r_rep_rechazadas.*
      PRINTX v_r_rep_total.*  

END REPORT