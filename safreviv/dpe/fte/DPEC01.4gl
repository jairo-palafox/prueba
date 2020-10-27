--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 27/07/2014
--===============================================================
#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPEC01                                                                 #
#Objetivo     => Consultar los datos cargados de informacion recibida para devolucion   #
#                por pagos indebidos cion                                               #
#Fecha inicio => Marzo 06, 2012                                                         #
#########################################################################################

DATABASE safre_viv
GLOBALS "DPEG01.4gl"
DEFINE w ui.Window,
       f ui.Form,
       g_usuario_cod LIKE seg_usuario.usuario_cod,
       g_tipo_ejecucion SMALLINT,
       g_s_titulo CHAR(25),
       r_detmov DYNAMIC ARRAY OF RECORD
          fecha_mov           DATE,
          tpo_mov             INTEGER,
          desc_mov            CHAR(80),
          fondo               SMALLINT,
          pesos               DECIMAL(16,6),
          acciones            DECIMAL(16,6),
          f_valor             DATE,
          folio               DECIMAL(10,0),
          origen              CHAR(20)
       END RECORD
       ,v_r_res_patron RECORD -- registro llenar el resumen de patrones
          reg_patronal_imss   LIKE dpe_patron.reg_patronal_imss,
          v_nss               LIKE afi_derechohabiente.nss,
          periodo_pago        LIKE dpe_patron.periodo_pago,
          folio_respuesta     DECIMAL(9,0),
          v_s_aceptadas       SMALLINT,
          v_s_rechazadas      SMALLINT,
          v_s_pendientes      SMALLINT,
          v_s_aceptadas_resp  SMALLINT,
          v_s_rechazadas_resp SMALLINT,
          v_s_pendientes_resp SMALLINT
       END RECORD
       ,v_ar_res_patron DYNAMIC ARRAY OF RECORD -- registro llenar el resumen de patrones
       	  reg_patronal_imss LIKE dpe_patron.reg_patronal_imss
          ,periodo_pago  LIKE dpe_patron.periodo_pago
          ,v_s_aceptadas SMALLINT
          ,v_s_rechazadas SMALLINT
          ,v_s_pendientes SMALLINT
          ,v_s_aceptadas_resp  SMALLINT
          ,v_s_rechazadas_resp SMALLINT
          ,v_s_pendientes_resp SMALLINT
       END RECORD
       ,v_r_cifras RECORD -- registro para consultar los datos
           consecutivo       SMALLINT,   -- consecutivo de registro
          folio             LIKE glo_folio.folio,
          nss               CHAR(11),
          nombre            CHAR(50),
          estado            SMALLINT,
          desc_estado       CHAR(30),
          diagnostico       SMALLINT,
          desc_diagnostico  CHAR(100),
          entidad_origen    CHAR(20) ,
          avis_viv_dev      DECIMAL(18,6),
          mto_retiro        DECIMAL(16,6),
          mto_act_retiro    DECIMAL(16,6),
          mto_cv_patron     DECIMAL(16,6),
          mto_cv_trabajador DECIMAL(16,6),
          mto_act_cv        DECIMAL(16,6),
          mto_vivienda      DECIMAL(16,6),
          aivs_respuesta    DECIMAL(18,6),
          m_viv_respuesta   DECIMAL(16,6),
          res_operacion     CHAR(25)
       END RECORD
       ,v_ar_cifras  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
           consecutivo       SMALLINT   -- consecutivo de registro
          ,folio             LIKE glo_folio.folio
          ,nss               CHAR(11)
          ,nombre            CHAR(50)
          ,estado            SMALLINT
          ,desc_estado       CHAR(30)
          ,diagnostico       SMALLINT
          ,desc_diagnostico  CHAR(100)
          ,entidad_origen    CHAR(20)
          ,avis_viv_dev      DECIMAL(18,6)
          ,mto_retiro        DECIMAL(16,6)
          ,mto_act_retiro    DECIMAL(16,6)
          ,mto_cv_patron     DECIMAL(16,6)
          ,mto_cv_trabajador DECIMAL(16,6)
          ,mto_act_cv        DECIMAL(16,6)
          ,mto_vivienda      DECIMAL(16,6)
          ,aivs_respuesta    DECIMAL(18,6)
          ,m_viv_respuesta   DECIMAL(16,6)
          ,res_operacion     CHAR(25)
       END RECORD
       -- Registro de cifras acumuladas
       ,v_r_acum  RECORD -- arreglo para desplegar consulta
           avis_viv_dev      DECIMAL(18,6)
          ,mto_retiro        DECIMAL(20,2)
          ,mto_act_retiro    DECIMAL(20,2)
          ,mto_cv_patron     DECIMAL(20,2)
          ,mto_cv_trabajador DECIMAL(20,2)
          ,mto_act_cv        DECIMAL(20,2)
          ,mto_vivienda      DECIMAL(20,2)
       END RECORD
       ,v_ar_solicitud  DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
           entidad_receptora   CHAR(3)
          ,aceptadas           DECIMAL(20,2)
          ,rechazadas          DECIMAL(20,2)
          ,pendientes          DECIMAL(20,2)
          ,totales             DECIMAL(20,2)
       END RECORD
       ,v_ar_contadores DYNAMIC ARRAY OF RECORD -- arreglo para pantalla
           entidad_receptora   CHAR(3)
          ,aceptadas           CHAR(50)
          ,rechazadas          CHAR(50)
          ,pendientes          SMALLINT --CHAR(50)
          ,totales             CHAR(50)
       END RECORD
       ,v_r_aux RECORD -- arreglo para desplegar consulta
           folio_sua     LIKE dpe_patron.folio_sua
          ,f_pago        LIKE dpe_patron.f_pago
          ,periodo_pago  LIKE dpe_patron.periodo_pago
          ,reg_patronal_imss LIKE dpe_patron.reg_patronal_imss
          ,clave_entidad_rec LIKE dpe_patron.clave_entidad_rec
          ,id_dpe_referencia LIKE dpe_sol_trabajador.id_dpe_referencia
       END RECORD
       ,v_ar_aux DYNAMIC ARRAY OF RECORD -- arreglo para desplegar consulta
           folio_sua     LIKE dpe_patron.folio_sua
          ,f_pago        LIKE dpe_patron.f_pago
          ,periodo_pago  LIKE dpe_patron.periodo_pago
          ,reg_patronal_imss LIKE dpe_patron.reg_patronal_imss
          ,clave_entidad_rec LIKE dpe_patron.clave_entidad_rec
          ,id_dpe_referencia LIKE dpe_sol_trabajador.id_dpe_referencia
       END RECORD
MAIN
   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario_cod    = ARG_VAL(1)
   LET g_tipo_ejecucion = ARG_VAL(2)
   LET g_s_titulo       = ARG_VAL(3)
   
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_s_titulo)
   END IF
   --CALL ui.Interface.setContainer("name_1")
   --CALL ui.Interface.setType("child")
      
   -- consulta de informacion recibida de OP98
   CALL fn_genera_consulta_integracion(g_usuario_cod)

END MAIN

{ ======================================================================
Clave: DPEC01
Nombre: fn_genera_consulta_integracion
Fecha creacion: Marzo 06, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
  Genera consulta de datos integrados

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_genera_consulta_integracion(p_usuario_cod)
   DEFINE 
      p_usuario_cod            LIKE seg_usuario.usuario_cod -- clave del usuario
      --
      ,v_folio                  LIKE glo_folio.folio
      ,v_estado_solicitud       SMALLINT
      ,v_cbx_folios             ui.ComboBox -- combo de folios
      ,v_cbx_estado             ui.ComboBox -- combo de estados
      ,v_s_cadena               STRING -- cadena de texto
      ,v_r_glo_ctr_archivo      RECORD
          folio           LIKE glo_folio.folio
          ,nombre_archivo LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
      ,v_i_conArch        INTEGER
      ,v_nss              CHAR(11)
      ,v_reg_patronal      CHAR(11)
      ,v_entidad_origen   CHAR(3)
      ,r_ruta_bin         LIKE seg_modulo.ruta_bin
      ,v_nom_reporte      VARCHAR(80) -- nombre del reporte
      ,r_ruta_listados    LIKE seg_modulo.ruta_listados
      ,v_manejador_rpt    OM.SaxDocumentHandler # Contenedor de Documentos para el reporte
      ,v_r_cifras RECORD -- registro para consultar los datos
          consecutivo       SMALLINT,   -- consecutivo de registro
          folio             LIKE glo_folio.folio,
          nss               CHAR(11),
          nombre            CHAR(50),
          estado            SMALLINT,
          desc_estado       CHAR(30),
          diagnostico       SMALLINT,
          desc_diagnostico  CHAR(100),
          entidad_origen    CHAR(20),
          avis_viv_dev      DECIMAL(18,6),
          mto_retiro        DECIMAL(16,6),
          mto_act_retiro    DECIMAL(16,6),
          mto_cv_patron     DECIMAL(16,6),
          mto_cv_trabajador DECIMAL(16,6),
          mto_act_cv        DECIMAL(16,6),
          mto_vivienda      DECIMAL(16,6)
       END RECORD
       
   CLOSE WINDOW SCREEN 
   OPEN WINDOW win_ConsInfo WITH FORM "DPEC010"

   -- Recupera punteros a ventana para control de grupos
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()   
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_estado = ui.ComboBox.forName("formonly.cmb_estado")
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

   -- Ocultar tablas de resultados
   CALL f.setElementHidden("gruresumenpatron",1)
   CALL f.setElementHidden("grudetalle",1)
   CALL f.setElementHidden("gruacum",1)
   
   LET INT_FLAG = FALSE
   -- se inicia el combobox en blanco
   CALL v_cbx_estado.clear()
   CALL v_cbx_folios.clear()

## Carga tempral de estados 06-03-2012
         CALL v_cbx_estado.addItem(1, "1 Aceptada")
         CALL v_cbx_estado.addItem(2, "2 Rechazada")
         CALL v_cbx_estado.addItem(3, "3 Pendiente")       
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT DISTINCT d.folio, g.nombre_archivo
         FROM glo_ctr_archivo g, glo_folio d
         WHERE g.proceso_cod = 1001
           AND g.estado = 2 -- integrado
           AND g.folio = d.folio
           AND g.opera_cod = 1

         -- <Se cruzan los folios con deo_det_op98 para obtener folios procesados>
         -- <  completamente y en forma exitosa para toda la afore.              >
         
         LET v_i_conArch = 0
         FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
            LET v_s_cadena = v_r_glo_ctr_archivo.folio ," - ", 
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

         FREE cur_folios

   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   CALL v_cbx_estado.addItem(-1," ")
   CALL v_cbx_folios.addItem(-1," ")
   -- se asignan los valores por omision
   LET v_folio            = -1
   LET v_estado_solicitud = -1
   LET v_nss              = NULL
   LET v_entidad_origen   = NULL

   DISPLAY TODAY USING "dd-mm-YYYY" TO TXT_FECHA
   
   INPUT v_folio, v_nss, v_reg_patronal, v_estado_solicitud, v_entidad_origen
   WITHOUT DEFAULTS
    FROM CMB_FOLIO, TXT_NSS, txt_reg_patronal, CMB_ESTADO, CMB_ENTIDAD_ORIGEN
   ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
      ON ACTION ACCEPT
         -- Mostrar tablas de resultados
         CALL f.setElementHidden("gruresumenpatron",0)
         CALL f.setElementHidden("grudetalle",0)
         CALL f.setElementHidden("gruacum",0)

         CALL fn_muestra_consulta_integracion(p_usuario_cod, v_folio, v_nss, v_reg_patronal, v_estado_solicitud, v_entidad_origen)
         
         -- Ocultar nuevamente las tablas de resultados
         CALL f.setElementHidden("gruresumenpatron",1)
         CALL f.setElementHidden("grudetalle",1)
         CALL f.setElementHidden("gruacum",1)

         -- Limpia datos para nueva consulta
         LET v_folio            = -1
         LET v_estado_solicitud = -1
         LET v_nss              = NULL
         LET v_entidad_origen   = NULL
         
         DISPLAY v_folio, v_nss, v_reg_patronal, v_estado_solicitud, v_entidad_origen
              TO CMB_FOLIO, TXT_NSS, txt_reg_patronal, CMB_ESTADO, CMB_ENTIDAD_ORIGEN
         
         LET INT_FLAG = FALSE
         #EXIT INPUT

      ON ACTION CANCEL
         LET INT_FLAG = TRUE
         EXIT INPUT
   
      END INPUT
   CLOSE WINDOW win_ConsInfo

END FUNCTION -- fn_genera_consulta_integracion

{
======================================================================
Clave: DPEC01
Nombre: fn_muestra_consulta_integracion
Fecha creacion: Marzo 06, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
   Muetra detalle de proceso de integración: 
     "CONSULTA DE DIAGNOSTICOS DE DEVOLUCION DE PAGOS EN EXCESO"

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_consulta_integracion(p_usuario_cod,p_folio, p_nss, 
                                         p_reg_patronal, p_estado_solicitud, 
                                         p_entidad_origen)

DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio            LIKE glo_folio.folio,
       p_nss              CHAR(11),
       p_reg_patronal     CHAR(11),
       p_estado_solicitud SMALLINT,
       p_entidad_origen   CHAR(3),
       --
       v_si_indice     INTEGER, -- indice de arreglo
       v_s_sql         STRING, -- cadena con una instruccion SQL
       v_s_sql_1       STRING, -- cadena con una instruccion SQL
       r_ruta_bin      LIKE seg_modulo.ruta_bin,
       v_nom_reporte   VARCHAR(80), -- nombre del reporte
       r_ruta_listados LIKE seg_modulo.ruta_listados,
       v_s_reporte     STRING,
       QryTxt          STRING,
       v_id_derechohabiente DECIMAL(9,0),
       v_comando   STRING   
   
   CALL v_ar_res_patron.CLEAR()


   --DISPLAY "--Consulta si no se ha recibido respuesta de procesar"   
      LET v_s_sql = "SELECT d.reg_patronal_imss, d.periodo_pago,d.folio_respuesta,",
                    "\n SUM(CASE d.estado_solicitud WHEN 1 THEN 1 ELSE 0 END),",
                    "\n SUM(CASE d.estado_solicitud WHEN 2 THEN 1 ELSE 0 END),",
                    "\n SUM(CASE d.estado_solicitud WHEN 3 THEN 1 ELSE 0 END)",
                    "\n FROM dpe_sol_trabajador d",
                    "\n WHERE 1=1"

------- sin folio de respuesta
      LET v_s_sql = "SELECT d.reg_patronal_imss, d.periodo_pago,0,",
                    "\n SUM(CASE d.estado_solicitud WHEN 1 THEN 1 ELSE 0 END),", -- aceptada
                    "\n SUM(CASE d.estado_solicitud WHEN 2 THEN 1 ELSE 0 END),", -- rechazada
                    "\n SUM(CASE d.estado_solicitud WHEN 3 THEN 1 ELSE 0 END)",  -- pendiente
                    "\n FROM dpe_sol_trabajador d",
                    "\n WHERE 1=1"
-----------
   IF ( p_folio <> -1 ) THEN 
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND d.folio  = ",p_folio,""
   END IF 
   
   IF ( p_reg_patronal IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.reg_patronal_imss = '",p_reg_patronal,"'"
   END IF
   
   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.estado_solicitud =",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_entidad_origen IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.cve_entidad_origen ='",p_entidad_origen,"'"
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n GROUP BY 1,2,3"
       ,"\n ORDER BY 1,2,3"
--DISPLAY "CONSULTA PRINCIPAL", v_s_sql
   PREPARE Prpr_ObtDatos_patron FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_patron CURSOR FOR Prpr_ObtDatos_patron
   
   LET v_si_indice = 0
   
   FOREACH Curr_ObtDatos_patron INTO v_r_res_patron.reg_patronal_imss,                                      
                                     v_r_res_patron.periodo_pago,    
                                     v_r_res_patron.folio_respuesta,
                                     v_r_res_patron.v_s_aceptadas,
                                     v_r_res_patron.v_s_rechazadas,
                                     v_r_res_patron.v_s_pendientes

      LET v_si_indice = v_si_indice + 1

      LET v_ar_res_patron[v_si_indice].reg_patronal_imss = v_r_res_patron.reg_patronal_imss  
      LET v_ar_res_patron[v_si_indice].periodo_pago      = v_r_res_patron.periodo_pago       
      LET v_ar_res_patron[v_si_indice].v_s_aceptadas     = v_r_res_patron.v_s_aceptadas      
      LET v_ar_res_patron[v_si_indice].v_s_rechazadas    = v_r_res_patron.v_s_rechazadas     
      LET v_ar_res_patron[v_si_indice].v_s_pendientes    = v_r_res_patron.v_s_pendientes     

                                     
      --Valida si se recibio respuesta de procesar
      LET v_s_sql_1 = "\n SELECT SUM(CASE rp.resul_op WHEN 1 THEN 1 ELSE 0 END),", -- aceptadas
                    "\n        SUM(CASE rp.resul_op WHEN 2 THEN 1 ELSE 0 END),", -- rechazadas
                    "\n        SUM(CASE rp.resul_op WHEN 3 THEN 1 ELSE 0 END)",  -- pendientes
                    "\n   FROM dpe_resp_procesar rp",
                    "\n   WHERE 1=1 ",
                    "\n  AND rp.reg_patronal_imss = '",v_r_res_patron.reg_patronal_imss,"'",
                    "\n    AND rp.periodo_pago = '",v_r_res_patron.periodo_pago,"'"
                    

--DISPLAY v_s_sql_1

{sin folio de respuesta
      IF ( v_r_res_patron.folio_respuesta IS NOT NULL ) THEN
         LET v_s_sql_1 = v_s_sql_1 CLIPPED
         ,"\n  AND rp.folio  = ",v_r_res_patron.folio_respuesta ,""
      END IF 

}
      PREPARE prp_resp_procesar FROM v_s_sql_1 CLIPPED
      DECLARE cur_resp_procesar CURSOR FOR prp_resp_procesar 
      
      
      FOREACH cur_resp_procesar INTO v_r_res_patron.v_s_aceptadas_resp,
                                     v_r_res_patron.v_s_rechazadas_resp,
                                     v_r_res_patron.v_s_pendientes_resp

      END FOREACH

      LET v_ar_res_patron[v_si_indice].v_s_aceptadas_resp   = v_r_res_patron.v_s_aceptadas_resp 
      LET v_ar_res_patron[v_si_indice].v_s_rechazadas_resp  = v_r_res_patron.v_s_rechazadas_resp
      LET v_ar_res_patron[v_si_indice].v_s_pendientes_resp  = v_r_res_patron.v_s_pendientes_resp
      
   END FOREACH

   --Si no encuentra registros envía mensaje 
   IF v_si_indice = 0 THEN
      -- Ocultar nuevamente las tablas de resultados
      CALL f.setElementHidden("gruresumenpatron",1)
      CALL f.setElementHidden("grudetalle",1)
      CALL f.setElementHidden("gruacum",1)
      CALL fn_mensaje ("Atención", "No se encontraron registros con los datos proporcionados", "stop" )
   END IF 

   DISPLAY p_folio, p_nss, p_reg_patronal, p_estado_solicitud, p_entidad_origen TO
              CMB_FOLIO, TXT_NSS, txt_reg_patronal, CMB_ESTADO, CMB_ENTIDAD_ORIGEN

   -- Funcion que desliega los totales del archivo
   CALL fn_totales(p_folio, p_nss, p_reg_patronal, 
      	             p_estado_solicitud, p_entidad_origen)

   DIALOG ATTRIBUTES (UNBUFFERED)

      DISPLAY ARRAY v_ar_res_patron TO scr_patron.*
      BEFORE ROW
         LET QryTxt = "\n AND d.reg_patronal_imss = '", 
                                  v_ar_res_patron[ARR_CURR()].reg_patronal_imss,"'",
                      "\n AND d.periodo_pago = '", 
                                  v_ar_res_patron[ARR_CURR()].periodo_pago,"'"
                                  
        CALL  v_ar_cifras.clear()
         
      	CALL fn_genera_detalle_trabajador(QryTxt,
                                          p_folio, 
                                          p_nss, 
                                          p_reg_patronal, 
      	                                  p_estado_solicitud, 
                                          p_entidad_origen)

      END DISPLAY

      DISPLAY ARRAY v_ar_cifras TO scr_detalle.*
         BEFORE DISPLAY
            DISPLAY v_s_reporte TO v_s_reporte

         ON ACTION Detalle_Solicitud
         --Muestra el detalle de la solicitud
            CALL fn_detalle_identificacion_solicitud(v_ar_cifras[ARR_CURR()].folio,
                  v_ar_cifras[ARR_CURR()].nss
                 ,v_ar_aux[ARR_CURR()].folio_sua, v_ar_aux[ARR_CURR()].f_pago
                 ,v_ar_aux[ARR_CURR()].periodo_pago
                 ,v_ar_aux[ARR_CURR()].reg_patronal_imss
                 ,v_ar_aux[ARR_CURR()].clave_entidad_rec)
            
         ON ACTION parciales
            -- Muestra cifras parciales para nss
               CALL fn_dpe_muestra_cifras_parciales(v_ar_cifras[ARR_CURR()].nss,
                                                    v_ar_aux[ARR_CURR()].id_dpe_referencia, 
                                                    v_ar_cifras[ARR_CURR()].folio,
                                                    v_ar_aux[ARR_CURR()].reg_patronal_imss, 
                                                    v_ar_aux[ARR_CURR()].periodo_pago )

         ON ACTION Consulta_Derechohabiente
            -- se ejecuta la consulta del derechohabiente usando la consulta general
            LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod || "1 'Consulta de Derechohabiente' " || v_ar_cifras[ARR_CURR()].nss
            RUN v_comando
            DISPLAY "fglrun", v_comando,    g_usuario_cod,              v_ar_cifras[ARR_CURR()].nss
         ON ACTION Consulta_Saldo
            SELECT id_derechohabiente
              INTO v_id_derechohabiente
              FROM afi_derechohabiente
              WHERE nss = v_ar_cifras[ARR_CURR()].nss
            
            CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente, 
                                          g_tipo_ejecucion, g_s_titulo)
                 
      END DISPLAY

      --Muestra el resumen de TOTALES en la consulta  
      DISPLAY ARRAY v_ar_contadores TO scr_conteo.*
      END DISPLAY
      
      ON ACTION regresa
         LET v_s_reporte = ""
         EXIT DIALOG
      
      -- Agregar reporte pdf de la consulta de diagnosticos
      -- JTSO 28-03-2012
      ON ACTION reporte
         # Recupera la ruta de listados en el que se enviara el archivo
           CALL fn_rutas("dpe") RETURNING r_ruta_bin, r_ruta_listados
           # Se indica que el reporte usara la plantilla creada
           IF fgl_report_loadCurrentSettings("DPEC01.4rp") THEN
              CALL fgl_report_selectDevice("PDF")
              LET v_nom_reporte = p_usuario_cod CLIPPED || "-DPEC01-","00000","-","00000","-","00000"||".pdf"
              CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
                 -- llamado a la función que arma la consulta para el reporte
                 CALL fn_muestra_reporte_integracion(p_usuario_cod, 
                                                     p_folio, 
                                                     p_nss, 
                                                     p_estado_solicitud, 
                                                     p_entidad_origen)
                 RETURNING v_si_indice
                 
                 IF(v_si_indice = 0)THEN
                    CALL fn_mensaje("Atención",
                                    "No se encontraron datos con los criterios indicados",
                                    "information")
                    NEXT FIELD cmb_folio                
                 ELSE
                    LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','dpedocto')\" target='nueva'>",
                                                                               v_nom_reporte CLIPPED,"</a>"
                 END IF	   
           ELSE
              DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
           END IF
           
           DISPLAY v_s_reporte TO v_s_reporte

      AFTER DIALOG
         CONTINUE DIALOG
            
   END DIALOG
   
   CALL v_ar_cifras.clear()
   CALL v_ar_solicitud.clear()

END FUNCTION -- fn_muestra_consulta_integracion


{ ======================================================================
Clave: DPEC01
Nombre: fn_detalle_identificacion_solicitud
Fecha creacion: Marzo 07, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_detalle_identificacion_solicitud(p_folio, p_nss, p_folio_sua, 
   p_f_pago, p_periodo_pago, p_nrp, p_cve_ent_receptora)
   DEFINE p_folio        LIKE glo_folio.folio,
          p_nss         CHAR(11),
          p_folio_sua    LIKE cta_his_pagos.folio_sua,
          p_f_pago       LIKE cta_his_pagos.f_pago,
          p_periodo_pago CHAR(6),
          p_nrp          LIKE cta_his_pagos.nrp,
          p_cve_ent_receptora LIKE cta_his_pagos.cve_ent_receptora,
      --
      v_ar_folios_solicitud DYNAMIC ARRAY OF RECORD
         descrip            CHAR(30),
         aporte_historico   CHAR(50),
         solicitud_devolver CHAR(50)
      END RECORD,
      v_ar_liquida_solicitud DYNAMIC ARRAY OF RECORD
         descrip            CHAR(30),
         aporte_historico   CHAR(50),
         aporte_liquidacion CHAR(50)
      END RECORD,
      v_s_qry               STRING,
      v_d_fecha_pago        DATE,
      v_s_valor_acciones    DECIMAL(16,6),
      v_d_monto_acciones    DECIMAL(16,2),
      v_d_monto_pesos       DECIMAL(16,2),
      v_ar_folio_liquida    LIKE dpe_preliquida.folio_liquida

   -- Llenar arreglo temporal para mostrar datos
   CALL v_ar_folios_solicitud.clear()
   CALL v_ar_liquida_solicitud.clear()
   LET v_ar_folios_solicitud[1].descrip = "Folio Lote"
   LET v_ar_folios_solicitud[2].descrip = "Folio SUA"
   LET v_ar_folios_solicitud[3].descrip = "Fecha Pago"
   LET v_ar_folios_solicitud[4].descrip = "Periodo de Pago"
   LET v_ar_folios_solicitud[5].descrip = "Registro Patronal IMSS"
   LET v_ar_folios_solicitud[6].descrip = "Clave Entidad Receptora"
   
   -- Llenar arreglo temporal para mostrar datos de liquidación
   CALL v_ar_liquida_solicitud.clear()
   LET v_ar_liquida_solicitud[1].descrip = "Sub Cuenta"
   LET v_ar_liquida_solicitud[2].descrip = "Siefore"
   LET v_ar_liquida_solicitud[3].descrip = "Movimiento"
   LET v_ar_liquida_solicitud[4].descrip = "Fecha de liquidación"
   LET v_ar_liquida_solicitud[5].descrip = "Precio acción"
   LET v_ar_liquida_solicitud[6].descrip = "AIV's"
   LET v_ar_liquida_solicitud[7].descrip = "Monto vivienda"
   LET v_ar_liquida_solicitud[8].descrip = "Folio Liquidación"
   
   LET v_s_qry = "\n SELECT folio, ",
                 "\n        folio_sua, ",
                 "\n        f_pago, ",
                 "\n        periodo_pago, ",
                 "\n        nrp, ",
                 "\n        cve_ent_receptora",
                 "\n   FROM cta_his_pagos",
                 "\n  WHERE folio     = ",p_folio,
                 "\n    AND folio_sua = ",p_folio_sua,
                 "\n    AND f_pago    = ","'",p_f_pago,"'",
                 "\n    AND periodo_pago = ","'",p_periodo_pago CLIPPED,"'",
                 "\n    AND nrp       = ","'",p_nrp CLIPPED,"'",
                 "\n    AND cve_ent_receptora = ","'",p_cve_ent_receptora CLIPPED,"'"
       
   --DISPLAY "Qry historicos: ", v_s_qry CLIPPED
   PREPARE Prpr_Obt_AporteHisto FROM v_s_qry CLIPPED
   EXECUTE Prpr_Obt_AporteHisto
      INTO v_ar_folios_solicitud[1].aporte_historico
          ,v_ar_folios_solicitud[2].aporte_historico
          ,v_d_fecha_pago -- Variable para almacenar la fecha
          ,v_ar_folios_solicitud[4].aporte_historico
          ,v_ar_folios_solicitud[5].aporte_historico
          ,v_ar_folios_solicitud[6].aporte_historico
          
   -- Se cambia el formato de la fecha para su visualizacións
   IF v_d_fecha_pago = '12/31/1899' THEN
      INITIALIZE v_d_fecha_pago TO NULL
      LET v_ar_folios_solicitud[3].aporte_historico = v_d_fecha_pago
   ELSE
      LET v_ar_folios_solicitud[3].aporte_historico = v_d_fecha_pago USING "dd-mm-yyyy"
   END IF

   LET v_s_qry = "\n SELECT DISTINCT a.folio,",             
                 "\n                 a.folio_sua, ",
                 "\n                 a.f_pago,",
                 "\n                 a.periodo_pago, ",
                 "\n                 a.reg_patronal_imss,",
                 "\n                 a.clave_entidad_rec, ",
                 "\n                 b.folio_liquida",
                 "\n            FROM dpe_patron a, ",
                 "\n                 dpe_sol_trab_parcial b",
                 "\n           WHERE a.folio             = ",p_folio,
                 "\n             AND a.folio_sua         = ",p_folio_sua,
                 "\n             AND a.f_pago            = ","'",p_f_pago,"'",
                 "\n             AND a.periodo_pago      = ","'",p_periodo_pago CLIPPED,"'",
                 "\n             AND a.reg_patronal_imss = ","'",p_nrp CLIPPED,"'",
                 "\n             AND a.clave_entidad_rec = ","'",p_cve_ent_receptora CLIPPED,"'",
                 "\n             AND a.folio             = b.folio"
                 
   --DISPLAY "Qry Solicitud a devolver: ", v_s_qry CLIPPED
   PREPARE Prpr_Obt_AporteDevol FROM v_s_qry CLIPPED
   EXECUTE Prpr_Obt_AporteDevol INTO v_ar_folios_solicitud[1].solicitud_devolver,
                                     v_ar_folios_solicitud[2].solicitud_devolver,
                                     v_d_fecha_pago,-- Variable para almacenar la fecha
                                     v_ar_folios_solicitud[4].solicitud_devolver,
                                     v_ar_folios_solicitud[5].solicitud_devolver,
                                     v_ar_folios_solicitud[6].solicitud_devolver,
                                     v_ar_folio_liquida
   
   -- Se cambia el formato de la fecha para su visualización
   LET v_ar_folios_solicitud[3].solicitud_devolver = v_d_fecha_pago USING "dd-mm-yyyy"
   
   IF v_ar_folio_liquida IS NULL THEN
   	  LET v_ar_folio_liquida = 0
   END IF
   
   -- Consulta que genera los movimiento aportados de la liquidación
   INITIALIZE v_d_fecha_pago TO NULL
   LET v_s_qry = 
       "SELECT DISTINCT i.subcuenta||'-'||s.subcuenta_desc, i.fondo_inversion, "
       ,"\n    i.movimiento||'-'||m.movimiento_desc, i.f_liquida"
       ,"\n,safre_viv:fn_obtiene_valor_accion(i.fondo_inversion, i.f_liquida)"
       ,"\n       ,SUM(i.monto_acciones), SUM(i.monto_pesos), i.folio_liquida"
       ,"\n  FROM (safre_viv:dpe_preliquida i LEFT OUTER JOIN cat_subcuenta s"
       ,"\n    ON i.subcuenta = s.subcuenta) LEFT OUTER JOIN cat_movimiento m"
       ,"\n    ON i.movimiento = m.movimiento"
       ,"\n WHERE folio_liquida = ", v_ar_folio_liquida
       ,"\n GROUP BY 1,2,3,4,5,8"
       
   --DISPLAY "Qry liquida: ", v_s_qry CLIPPED
   PREPARE Prpr_Obt_Aporteliquida FROM v_s_qry CLIPPED
   EXECUTE Prpr_Obt_Aporteliquida INTO v_ar_liquida_solicitud[1].aporte_liquidacion,
                                       v_ar_liquida_solicitud[2].aporte_liquidacion,
                                       v_ar_liquida_solicitud[3].aporte_liquidacion,
                                       v_d_fecha_pago,
                                       v_s_valor_acciones,
                                       v_d_monto_acciones,
                                       v_d_monto_pesos,
                                       v_ar_liquida_solicitud[8].aporte_liquidacion
   
   -- Se cambia el formato de la fecha para su visualización
   LET v_ar_liquida_solicitud[4].aporte_liquidacion = v_d_fecha_pago USING "dd-mm-yyyy"
   LET v_ar_liquida_solicitud[5].aporte_liquidacion = v_s_valor_acciones USING "###,###,##&.&&&&&&"
   LET v_ar_liquida_solicitud[6].aporte_liquidacion = v_d_monto_acciones USING "###,###,##&.&&"
   LET v_ar_liquida_solicitud[7].aporte_liquidacion = v_d_monto_pesos USING "###,###,##&.&&"
   
   OPEN WINDOW win_detsolicitud WITH FORM "DPEC011" ATTRIBUTE(STYLE="dialog")
   
   --LET w = ui.Window.getCurrent()
   --LET f = w.getForm()
   
   CALL fgl_settitle("DETALLE DE LLAVE DE IDENTIFICACION DE SOLICITUD. "||" NSS: "||p_nss)

   DIALOG
      DISPLAY ARRAY v_ar_folios_solicitud TO scr_det_solicitud.*
         
         --ON ACTION Liquidacion
            --CALL fn_detalle_liquidacion_aportacion(v_ar_folios_solicitud[7].solicitud_devolver, p_nss)
            
      END DISPLAY
      
      DISPLAY ARRAY v_ar_liquida_solicitud TO scr_det_liquida.*
         
      END DISPLAY
      
      
      ON ACTION regresa
         EXIT DIALOG
   END DIALOG
   
   CLOSE WINDOW win_detsolicitud 
   
      
END FUNCTION -- fn_detalle_identificacion_solicitud
{ ======================================================================
Clave: DPEC01
Nombre: fn_detalle_liquidacion_aportacion
Fecha creacion: Marzo 07, 2012
Autor: Felipe Nava, EFP
Narrativa del proceso que realiza:
 

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_detalle_liquidacion_aportacion(p_folio, p_nss)
   DEFINE
      p_folio LIKE glo_folio.folio
      ,p_nss         CHAR(11)
      --
      ,v_ar_liquida DYNAMIC ARRAY OF RECORD
          subcuenta     LIKE cat_subcuenta.subcuenta_desc
         ,siefore       LIKE dpe_preliquida.fondo_inversion
         ,movimiento    LIKE cat_movimiento.movimiento_desc
         ,f_liquida     LIKE dpe_preliquida.f_liquida
         ,precio_accion DECIMAL(18,6)
         ,acciones      LIKE dpe_preliquida.monto_acciones
         ,folio_liquida LIKE dpe_preliquida.folio_liquida
      END RECORD
      ,v_r_liquida RECORD
          subcuenta     LIKE cat_subcuenta.subcuenta_desc
         ,siefore       LIKE dpe_preliquida.fondo_inversion
         ,movimiento    LIKE cat_movimiento.movimiento_desc
         ,f_liquida     LIKE dpe_preliquida.f_liquida
         ,precio_accion DECIMAL(18,6)
         ,acciones      LIKE dpe_preliquida.monto_acciones
         ,folio_liquida LIKE dpe_preliquida.folio_liquida
      END RECORD
      ,v_si_contador    SMALLINT
      ,v_s_qry          STRING
      
      IF p_folio IS NULL THEN
      CALL fn_mensaje("Atención",
           "No se encontraron datos con los criterios indicados","information")
      RETURN
   END IF
   
   LET v_s_qry = 
       "SELECT DISTINCT i.subcuenta||'-'||s.subcuenta_desc, i.fondo_inversion, "
       ,"\n    i.movimiento||'-'||m.movimiento_desc, i.f_liquida"
       ,"\n,safre_tmp:gn_obtiene_valor_accion(i.fondo_inversion, i.f_liquida)"
       ,"\n       , i.monto_acciones, i.folio_liquida"
       ,"\n  FROM (safre_viv:dpe_preliquida i LEFT OUTER JOIN cat_subcuenta s"
       ,"\n    ON i.subcuenta = s.subcuenta) LEFT OUTER JOIN cat_movimiento m"
       ,"\n    ON i.movimiento = m.movimiento"
       ,"\n WHERE folio_liquida = ", p_folio
   
   --DISPLAY "Qry liquida: ", v_s_qry CLIPPED
   PREPARE Prpr_detliquida FROM v_s_qry CLIPPED
   DECLARE Curr_detliquida CURSOR FOR Prpr_detliquida
   
   CALL v_ar_liquida.clear()
   LET v_si_contador = 0
   FOREACH Curr_detliquida --USING p_folio
      INTO v_r_liquida.*
   
      LET v_si_contador = v_si_contador + 1
      LET v_ar_liquida[v_si_contador].* = v_r_liquida.*
   END FOREACH
   
   IF(v_si_contador <= 0)THEN
      -- NO hay datos
      CALL fn_mensaje("Atención",
           "No se encontraron datos con los criterios indicados","information")
      RETURN
   END IF
   
   OPEN WINDOW win_det_liquida WITH FORM "DPEC012" ATTRIBUTE(STYLE="dialog")
   
   CALL fgl_settitle("DETALLE DE LIQUIDACION DE APORTACION. "||" NSS: "||p_nss)
   
   DIALOG
      DISPLAY ARRAY v_ar_liquida TO scr_det_liquida.*
   
      END DISPLAY
      
      ON ACTION regresa
         EXIT DIALOG
   END DIALOG
   
   CLOSE WINDOW win_det_liquida
   
END FUNCTION -- fn_detalle_liquidacion_aportacion


{
   Funcion : fn_dpe_muestra_cifras_parciales
   Fecha   : 26-03-2012
   Autor   : Felipe Nava
   Descrip : Genera consulta de cifras parciales para un nss
}
FUNCTION fn_dpe_muestra_cifras_parciales(p_nss, p_id_dpe_referencia, p_folio, 
    p_reg_patronal_imss, p_periodo_pago )
DEFINE p_nss               CHAR(11),
       p_id_dpe_referencia DECIMAL(9,0),
       p_folio             DECIMAL(9,0),
       p_reg_patronal_imss CHAR(11),
       p_periodo_pago      CHAR(6)

DEFINE v_ar_parciales DYNAMIC ARRAY OF RECORD
          avis_viv_dev      DECIMAL(16,6),
          imp_viv_dev       DECIMAL(16,6),
          porcentaje        DECIMAL(5,2) ,
          avis_viv_dev_resp DECIMAL(16,6),
          imp_viv_dev_resp  DECIMAL(16,6),
          folio_liquida     DECIMAL(9,0) ,
          fecha_liquida     DATE         ,
          folio_gobierno    DECIMAL(9,0) ,
          fecha_gobierno    DATE         ,
          diagnostico       CHAR(100)
END RECORD
DEFINE v_r_parciales RECORD
          avis_viv_dev      DECIMAL(16,6),
          imp_viv_dev       DECIMAL(16,6),
          porcentaje        DECIMAL(5,2) ,
          avis_viv_dev_resp DECIMAL(16,6),
          imp_viv_dev_resp  DECIMAL(16,6),    
          folio_liquida     DECIMAL(9,0) ,
          fecha_liquida     DATE         ,
          folio_gobierno    DECIMAL(9,0) ,
          fecha_gobierno    DATE         ,
          diagnostico       CHAR(100)    ,
          folio_respuesta   DECIMAL(9,0) 
END RECORD
DEFINE li_posicion       INTEGER,
       li_posicion_1     INTEGER,
       v_s_qry           STRING

   LET v_s_qry = "\n SELECT a.avis_viv_dev,", 
                 "\n        a.imp_viv_dev,",
                 "\n        b.porcentaje_dev,",
                 "\n        a.folio_liquida,",
                 "\n        g.f_actualiza,",
                 "\n        a.diagnostico||' - '||d.diag_desc_larga,",
                 "\n        a.folio_respuesta",
                 "\n   FROM dpe_sol_trab_parcial a,", 
                 "\n        dpe_diagnostico_sol d,",
                 "\n        glo_folio g, ",
                 "\n        dpe_sol_trabajador b",
                 "\n  WHERE a.diagnostico = d.diagnostico",
                 "\n    AND a.id_dpe_referencia = ",p_id_dpe_referencia,
                 "\n    AND a.folio = ",p_folio,
                 "\n    AND a.reg_patronal_imss = ","'",p_reg_patronal_imss,"'",
                 "\n    AND a.periodo_pago = ","'",p_periodo_pago,"'",
                 "\n    AND a.folio_liquida = g.folio",
                 "\n   AND a.id_dpe_referencia = b.id_dpe_referencia"
--DISPLAY v_s_qry
   PREPARE Prpr_ObtDatParciales FROM v_s_qry CLIPPED
   DECLARE Curr_ObtDatParciales CURSOR FOR Prpr_ObtDatParciales
   
   LET li_posicion = 0;
   CALL v_ar_parciales.clear()
   FOREACH Curr_ObtDatParciales INTO v_r_parciales.avis_viv_dev ,
                                     v_r_parciales.imp_viv_dev  ,
                                     v_r_parciales.porcentaje   ,
                                     v_r_parciales.folio_liquida,
                                     v_r_parciales.fecha_liquida,
                                     v_r_parciales.diagnostico, 
                                     v_r_parciales.folio_respuesta

      LET v_s_qry = "\n SELECT c.aivs_viv_dev,   ",
                    "\n        c.imp_viv_dev,  ",                    
                    "\n        c.folio,         ",
                    "\n        c.f_respuesta    ",
                    "\n  FROM dpe_resp_procesar c",
                    "\n WHERE c.reg_patronal_imss = ","'",p_reg_patronal_imss,"'",
                    "\n   AND c.periodo_pago = ","'",p_periodo_pago,"'"
--DISPLAY v_s_qry
      IF v_r_parciales.folio_respuesta IS NOT NULL THEN
         LET v_s_qry = v_s_qry || "\n AND c.folio = ",v_r_parciales.folio_respuesta
      END IF 

      PREPARE prp_rep_proc_parciales FROM v_s_qry CLIPPED
      DECLARE cur_rep_proc_parciales CURSOR FOR prp_rep_proc_parciales 

      LET li_posicion_1 = 0
      FOREACH cur_rep_proc_parciales INTO v_r_parciales.avis_viv_dev_resp,
                                          v_r_parciales.imp_viv_dev_resp ,    
                                          v_r_parciales.folio_gobierno   ,
                                          v_r_parciales.fecha_gobierno
         LET li_posicion_1 = li_posicion_1 + 1
      END FOREACH       
      
      LET li_posicion = li_posicion + 1
      
      LET v_ar_parciales[li_posicion].avis_viv_dev       = v_r_parciales.avis_viv_dev     ;
      LET v_ar_parciales[li_posicion].imp_viv_dev        = v_r_parciales.imp_viv_dev      ;
      LET v_ar_parciales[li_posicion].porcentaje         = v_r_parciales.porcentaje       ;
      LET v_ar_parciales[li_posicion].avis_viv_dev_resp  = v_r_parciales.avis_viv_dev_resp;
      LET v_ar_parciales[li_posicion].imp_viv_dev_resp   = v_r_parciales.imp_viv_dev_resp ;
      LET v_ar_parciales[li_posicion].folio_liquida      = v_r_parciales.folio_liquida    ;
      LET v_ar_parciales[li_posicion].fecha_liquida      = v_r_parciales.fecha_liquida    ;
      LET v_ar_parciales[li_posicion].folio_gobierno     = v_r_parciales.folio_gobierno   ;

      IF ( v_r_parciales.fecha_gobierno = "12/31/1899" ) THEN 
         LET v_ar_parciales[li_posicion].fecha_gobierno =  NULL ;
      ELSE 
         LET v_ar_parciales[li_posicion].fecha_gobierno = v_r_parciales.fecha_gobierno   ;
      END IF 
      
      LET v_ar_parciales[li_posicion].diagnostico        = v_r_parciales.diagnostico      ;

   END FOREACH
   
   FREE Curr_ObtDatParciales

   IF(li_posicion <= 0)THEN
      -- NO hay datos
      CALL fn_mensaje("Atención",
           "No se encontraron datos con los criterios indicados","information")
      RETURN
   END IF

   IF v_ar_cifras[ARR_CURR()].estado = 2 THEN
      CALL fn_mensaje("Atención", "Operación con Rechazo", "stop")          
   ELSE 
      OPEN WINDOW win_det_liquida WITH FORM "DPEC013" ATTRIBUTE(STYLE="dialog")
   
         CALL fgl_settitle("DETALLE DE PAGOS PARCIALES. "||" NSS: "||p_nss)

         DISPLAY ARRAY v_ar_parciales TO scr_parciales.*
         ATTRIBUTES (ACCEPT = FALSE, CANCEL = FALSE)  
               ON ACTION regresa
                  EXIT DISPLAY
         END DISPLAY
      CLOSE WINDOW win_det_liquida
   END IF             
END FUNCTION -- fn_dpe_muestra_cifras_parciales

{
======================================================================
Clave: DPEC01
Nombre: fn_muestra_reporte_integracion
Fecha creacion: Marzo 28, 2012
Autor: Felipe Nava
Modificó: José Trinidad Soto Ortega, EFP
Narrativa del proceso que realiza:
   Muetra detalle de proceso de integración: 
     "REPORTE DE DIAGNOSTICOS DE DEVOLUCION DE PAGOS EN EXCESO"

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_muestra_reporte_integracion(p_usuario_cod, p_folio, p_nss, 
                                        p_estado_solicitud, p_entidad_origen)
DEFINE p_usuario_cod      LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio            LIKE glo_folio.folio,
       p_nss              CHAR(11),
       p_estado_solicitud SMALLINT,
       p_entidad_origen   CHAR(3),
       v_r_cifras RECORD -- registro para consultar los datos
          v_reg_patronal_imss CHAR(11),
          v_periodo_pago      CHAR(6),
          v_estado_solicitud  CHAR(10),
          v_imp_viv_dev       DECIMAL(16,6),
          v_avis_viv_dev      DECIMAL(16,6),
          v_cont_registros    SMALLINT
       END RECORD,
       v_si_indice        INTEGER, -- indice de arreglo
       v_s_sql            STRING, -- cadena con una instruccion SQL
       v_i_cont           INTEGER, -- Contador general
       v_sol_aceptadas    SMALLINT,
       v_mon_aceptadas    DECIMAL(20,6),
       v_sol_rechazadas   SMALLINT,
       v_mon_rechazadas   DECIMAL(20,6),
       v_sol_pendientes   SMALLINT,
       v_mon_pendientes   DECIMAL(20,6),
       v_manejador_rpt    OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
       v_sol_total_aceptadas  SMALLINT,     
       v_mon_total_aceptadas  DECIMAL(20,6),
       v_sol_total_rechazadas SMALLINT,     
       v_mon_total_rechazadas DECIMAL(20,6),
       v_sol_total_pendientes SMALLINT,     
       v_mon_total_pendientes DECIMAL(20,6),
       v_sol_total_total      SMALLINT,     
       v_mon_total_total      DECIMAL(20,6),
       v_total_pendientes     CHAR(1)
   
      -- Se inicializan variables de registros totales
      LET v_sol_aceptadas  = 0
      LET v_mon_aceptadas  = 0
      LET v_sol_rechazadas = 0
      LET v_mon_rechazadas = 0
      LET v_sol_pendientes = 0
      LET v_mon_pendientes = 0
      LET v_sol_total_aceptadas = 0
      LET v_mon_total_aceptadas = 0
      LET v_sol_total_rechazadas = 0
      LET v_mon_total_rechazadas = 0
      LET v_sol_total_pendientes = 0
      LET v_mon_total_pendientes = 0
      LET v_sol_total_total = 0
      LET v_mon_total_total = 0
             
   -- se realiza la consulta para obtener las cifras
   LET v_s_sql = "\n SELECT d.reg_patronal_imss, d.periodo_pago, CASE d.estado_solicitud",
                 "\n                                              WHEN 1 THEN 'Aceptadas'",
                 "\n                                              WHEN 2 THEN 'Rechazadas'",
                 "\n                                              WHEN 3 THEN 'Pendientes'",
                 "\n                                             END,",
                 "\n SUM(d.imp_viv_dev), SUM(avis_viv_dev), COUNT(*)",
                 "\n FROM ((safre_viv:dpe_sol_trabajador d",
                 "\n     LEFT OUTER JOIN dpe_patron p",
                 "\n  ON p.folio = d.folio) LEFT OUTER JOIN dpe_estado_solicitud e",
                 "\n  ON d.estado_solicitud = e.estado_solicitud)",
                 "\n     LEFT OUTER JOIN dpe_diagnostico_sol g",
                 "\n  ON d.diagnostico = g.diagnostico",
                 "\n AND p.reg_patronal_imss = d.reg_patronal_imss",
                 "\n WHERE d.folio = ",p_folio

   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.estado_solicitud = ",p_estado_solicitud
   END IF
   
   IF ( p_estado_solicitud = -1 ) THEN
      LET p_estado_solicitud = ""
   END IF
   
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n  GROUP BY 1, 2, 3"
   
   --DISPLAY "consulta:",v_s_sql CLIPPED
   -- se prepara y ejecuta la consulta
   PREPARE Prpr_ObtDatos_Integrados_Reporte FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_Integrados_Reporte CURSOR FOR Prpr_ObtDatos_Integrados_Reporte
   
   -- se llena el arreglo de despliegue
   LET v_si_indice = 0
   CALL fgl_report_selectPreview(0)
   LET v_manejador_rpt = fgl_report_commitCurrentSettings()
                     
   # Inicia el reporte de consulta historica por derechohabiente
   START REPORT reporte_diagnosticos TO XML HANDLER v_manejador_rpt
      FOREACH Curr_ObtDatos_Integrados_Reporte INTO v_r_cifras.*
         -- se incrementa el indice
         IF v_r_cifras.v_imp_viv_dev IS NULL THEN
            LET v_r_cifras.v_imp_viv_dev = 0
         END IF

         LET v_si_indice = v_si_indice + 1
         LET v_sol_total_aceptadas = 0 
         LET v_mon_total_aceptadas = 0 
         LET v_sol_total_rechazadas = 0
         LET v_mon_total_rechazadas = 0
         LET v_sol_total_pendientes = 0
         LET v_mon_total_pendientes = 0
         LET v_sol_total_total = 0     
         LET v_mon_total_total = 0     
{         
         CASE v_r_cifras.v_estado_solicitud
            WHEN "Aceptadas"
               LET v_sol_aceptadas = v_sol_aceptadas + v_r_cifras.v_cont_registros
               LET v_mon_aceptadas = v_mon_aceptadas + v_r_cifras.v_imp_viv_dev
               
            WHEN "Rechazadas"
               LET v_sol_rechazadas = v_sol_rechazadas + v_r_cifras.v_cont_registros
               LET v_mon_rechazadas = v_mon_rechazadas + v_r_cifras.v_imp_viv_dev
            WHEN "Pendientes"
            	 LET v_sol_pendientes = v_sol_pendientes + v_r_cifras.v_cont_registros
               LET v_mon_pendientes = v_mon_pendientes + v_r_cifras.v_imp_viv_dev
         END CASE

         
            LET v_sol_total_aceptadas = v_sol_total_aceptadas + v_sol_aceptadas
            
            LET v_mon_total_aceptadas = v_mon_total_aceptadas + v_mon_aceptadas
            
            LET v_sol_total_rechazadas = v_sol_total_rechazadas + v_sol_rechazadas
            
            LET v_mon_total_rechazadas = v_mon_total_rechazadas + v_mon_rechazadas
            
            LET v_sol_total_pendientes = v_sol_total_pendientes + v_sol_pendientes
            
            LET v_mon_total_pendientes = v_mon_total_pendientes + v_mon_pendientes
            
            LET v_sol_total_total = v_sol_total_total + v_sol_total_aceptadas +
                                    v_sol_total_rechazadas + v_sol_total_pendientes
                                    
            LET v_mon_total_total = v_mon_total_total + v_mon_total_aceptadas + 
                                    v_mon_total_rechazadas + v_mon_total_pendientes
}
            IF v_ar_contadores[1].aceptadas  IS NULL THEN 
               LET v_ar_contadores[1].aceptadas  = 0
            END IF
            IF v_ar_contadores[2].aceptadas  IS NULL THEN 
               LET v_ar_contadores[2].aceptadas  = 0
            END IF
            IF v_ar_contadores[1].rechazadas IS NULL THEN 
               LET v_ar_contadores[1].rechazadas = 0
            END IF
            IF v_ar_contadores[2].rechazadas IS NULL THEN 
               LET v_ar_contadores[2].rechazadas = 0
            END IF
            IF v_ar_contadores[1].pendientes IS NULL THEN 
               LET v_ar_contadores[1].pendientes = 0
            END IF
            IF v_ar_contadores[2].pendientes IS NULL THEN 
               LET v_ar_contadores[2].pendientes = 0
            END IF
            IF v_ar_contadores[1].totales    IS NULL THEN 
               LET v_ar_contadores[1].totales    = 0
            END IF
            IF v_ar_contadores[2].totales    IS NULL THEN
               LET v_ar_contadores[2].totales    = 0
            END IF
            LET v_sol_total_aceptadas  = v_ar_contadores[1].aceptadas 
            LET v_mon_total_aceptadas  = v_ar_contadores[2].aceptadas 
            LET v_sol_total_rechazadas = v_ar_contadores[1].rechazadas
            LET v_mon_total_rechazadas = v_ar_contadores[2].rechazadas
            LET v_total_pendientes = "0"
            LET v_mon_total_pendientes = v_ar_contadores[2].pendientes
            LET v_sol_total_total      = v_ar_contadores[1].totales
            LET v_mon_total_total      = v_ar_contadores[2].totales

         -- Se llena el reporte con la información recuperada
         OUTPUT TO REPORT reporte_diagnosticos(p_usuario_cod,p_folio, p_nss, 
                                                             p_estado_solicitud, 
                                                             p_entidad_origen, v_r_cifras.*,
                                                             v_sol_aceptadas,
                                                             v_mon_aceptadas,
                                                             v_sol_rechazadas,
                                                             v_mon_rechazadas,
                                                             v_sol_pendientes,
                                                             v_mon_pendientes,
                                                             v_sol_total_aceptadas,
                                                             v_mon_total_aceptadas, 
                                                             v_sol_total_rechazadas,
                                                             v_mon_total_rechazadas,
                                                             v_sol_total_pendientes,
                                                             v_mon_total_pendientes,
                                                             v_sol_total_total,
                                                             v_mon_total_total,
                                                             v_total_pendientes)
                                                             
                                                             
                                                             
      END FOREACH
   FINISH REPORT reporte_diagnosticos
   FREE Curr_ObtDatos_Integrados_Reporte
   RETURN v_si_indice
END FUNCTION -- fn_muestra_reporte_integracion

REPORT reporte_diagnosticos(p_usuario_cod, p_folio, p_nss, p_estado_solicitud, 
                            p_entidad_origen, v_r_cifras,
                            v_sol_aceptadas, v_mon_aceptadas, v_sol_rechazadas, v_mon_rechazadas,
                            v_sol_pendientes, v_mon_pendientes, v_sol_total_aceptadas, v_mon_total_aceptadas,
                            v_sol_total_rechazadas, v_mon_total_rechazadas, v_sol_total_pendientes, 
                            v_mon_total_pendientes,v_sol_total_total, v_mon_total_total,
                            p_total_pendientes)

DEFINE p_usuario_cod       LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_folio             LIKE glo_folio.folio,
       p_nss              CHAR(11),
       p_estado_solicitud SMALLINT,
       p_entidad_origen   CHAR(3),
       v_r_cifras RECORD -- registro para consultar los datos
          v_reg_patronal_imss CHAR(11),
          v_periodo_pago      CHAR(6),
          v_estado_solicitud  CHAR(10),
          v_imp_viv_dev       DECIMAL(16,6),
          v_avis_viv_dev      DECIMAL(16,6),
          v_cont_registros    SMALLINT
       END RECORD,
       v_fecha_reporte        DATE,
       v_i_folio              INTEGER,
       v_sol_aceptadas        SMALLINT,
       v_mon_aceptadas        DECIMAL(20,6),
       v_sol_rechazadas       SMALLINT,
       v_mon_rechazadas       DECIMAL(20,6),
       v_sol_pendientes       SMALLINT,
       v_mon_pendientes       DECIMAL(20,6), 
       v_sol_total_aceptadas  SMALLINT,
       v_mon_total_aceptadas  DECIMAL(20,6),
       v_sol_total_rechazadas SMALLINT,    
       v_mon_total_rechazadas DECIMAL(20,6),
       v_sol_total_pendientes SMALLINT,    
       v_mon_total_pendientes DECIMAL(20,6),
       v_sol_total_total      SMALLINT,    
       v_mon_total_total      DECIMAL(20,6),
       p_total_pendientes     CHAR(1)
          
   FORMAT
   
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY
      LET v_i_folio = p_folio
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"
      PRINTX p_usuario_cod
      PRINTX v_i_folio
      PRINTX p_nss              
      PRINTX p_estado_solicitud
      PRINTX p_entidad_origen

   ON EVERY ROW
      PRINTX v_r_cifras.*
   
   ON LAST ROW
      PRINTX v_sol_aceptadas USING "##&"
      PRINTX v_mon_aceptadas USING "###,###,###,##&.&&"
      PRINTX v_sol_rechazadas USING "##&"
      PRINTX v_mon_rechazadas USING "###,###,###,##&.&&"
      PRINTX v_sol_pendientes USING "##&"
      PRINTX v_mon_pendientes USING "###,###,###,##&.&&"
      PRINTX v_sol_total_aceptadas USING "##&"               
      PRINTX v_mon_total_aceptadas USING "###,###,###,##&.&&"
      PRINTX v_sol_total_rechazadas USING "##&"               
      PRINTX v_mon_total_rechazadas USING "###,###,###,##&.&&"
      --PRINTX v_sol_total_pendientes USING "##&"
      PRINTX p_total_pendientes
      PRINTX v_mon_total_pendientes USING "###,###,###,##&.&&"
      PRINTX v_sol_total_total USING "##&"               
      PRINTX v_mon_total_total USING "###,###,###,##&.&&"

END REPORT

-- Funcion que genera detalle de trabajadores
FUNCTION fn_genera_detalle_trabajador(p_QryTxt, p_folio, p_nss, p_reg_patronal,
                                      p_estado_solicitud, p_entidad_origen)
                                      
DEFINE v_si_indice        INTEGER, -- indice de arreglo
       v_s_sql            STRING, -- cadena con una instruccion SQL
       p_QryTxt           STRING,
       p_folio            DECIMAL(9,0),
       p_nss              CHAR(11),
       p_reg_patronal     CHAR(11),
       p_estado_solicitud SMALLINT,
       p_entidad_origen   CHAR(3),
       v_resultado        INTEGER,
       v_ind_parc_det     INTEGER
   
   CALL v_ar_solicitud.clear()
   -- Llenar arreglo de solicitudes.
   LET v_ar_solicitud[1].entidad_receptora = 'RCV'
   LET v_ar_solicitud[2].entidad_receptora = 'VIV'
   LET v_ar_solicitud[3].entidad_receptora = 'RCV'
   LET v_ar_solicitud[4].entidad_receptora = 'VIV'
   -- aceptadas
   LET v_ar_solicitud[1].aceptadas = 0
   LET v_ar_solicitud[2].aceptadas = 0
   LET v_ar_solicitud[3].aceptadas = 0
   LET v_ar_solicitud[4].aceptadas = 0
   -- rechazadas
   LET v_ar_solicitud[1].rechazadas = 0
   LET v_ar_solicitud[2].rechazadas = 0
   LET v_ar_solicitud[3].rechazadas = 0
   LET v_ar_solicitud[4].rechazadas = 0
   -- pendientes
   LET v_ar_solicitud[1].pendientes = 0
   LET v_ar_solicitud[2].pendientes = 0
   LET v_ar_solicitud[3].pendientes = 0
   LET v_ar_solicitud[4].pendientes = 0
   -- totales
   LET v_ar_solicitud[1].totales = 0
   LET v_ar_solicitud[2].totales = 0
   LET v_ar_solicitud[3].totales = 0
   LET v_ar_solicitud[4].totales = 0   

  --Corta la cadena si viene vacia
   LET p_QryTxt = p_QryTxt CLIPPED

                --Arreglo Cifras 
   LET v_s_sql = "\n SELECT 0,",--Consecutivo
                 "\n        d.folio, ",
                 "\n        d.nss,   ",
                 "\n        d.nombre,", 
                 "\n        d.estado_solicitud,",
                 "\n        e.estado_desc_corta, ",
                 "\n        d.diagnostico,",
                 "\n        g.diag_desc_larga,", 
                 "\n        100, ",
                 "\n        d.avis_viv_dev, ",
                 "\n        d.imp_retiro_dev,",
                 "\n        d.imp_act_retiro_dev, ",
                 "\n        d.imp_cv_pat_dev,",
                 "\n        d.imp_cv_trab_dev,",
                 "\n        d.imp_act_cv_dev,",
                 "\n        d.imp_viv_dev,",
                 --Arreglo de auxiliares
                 "\n        p.folio_sua, ",
                 "\n        p.f_pago, ",
                 "\n        d.periodo_pago,",
                 "\n        d.reg_patronal_imss, ",
                 "\n        p.clave_entidad_rec, ",
                 "\n        d.id_dpe_referencia",
                 "\n  FROM dpe_sol_trabajador d, ",
                 "\n       dpe_estado_solicitud e,",
                 "\n       dpe_diagnostico_sol g, ",
                 "\n       dpe_patron p",
                 "\n WHERE d.estado_solicitud = e.estado_solicitud",
                 "\n   AND d.diagnostico = g.diagnostico",
                 "\n   AND p.folio = d.folio",
                 "\n   AND d.periodo_pago = p.periodo_pago",
                 "\n   AND p.reg_patronal_imss = d.reg_patronal_imss"

   IF ( p_folio <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.folio = ",p_folio   
   END IF  

   IF length (p_QryTxt) > 0 THEN 
      LET v_s_sql = v_s_sql CLIPPED, p_QryTxt   
   END IF
   
   IF ( p_reg_patronal IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.reg_patronal_imss = '",p_reg_patronal,"'"
   END IF
   
   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.estado_solicitud =",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_entidad_origen IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.cve_entidad_origen ='",p_entidad_origen,"'"
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n  ORDER BY 5"
   
   DISPLAY "consulta detalle:",v_s_sql CLIPPED
   -- se prepara y ejecuta la consulta
   PREPARE Prpr_ObtDatos_Integrados FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_Integrados CURSOR FOR Prpr_ObtDatos_Integrados
   --DISPLAY v_s_sql
   -- Inicializa arreglo de acumulados
   LET v_r_acum.mto_retiro         = 0
   LET v_r_acum.mto_act_retiro     = 0
   LET v_r_acum.mto_cv_patron      = 0
   LET v_r_acum.mto_cv_trabajador  = 0
   LET v_r_acum.mto_act_cv         = 0
   LET v_r_acum.mto_vivienda       = 0
   LET v_r_acum.avis_viv_dev       = 0
   -- se llena el arreglo de despliegue
   LET v_si_indice = 0
   CALL v_ar_cifras.clear()
   CALL v_ar_aux.clear()
   FOREACH Curr_ObtDatos_Integrados INTO --Arreglo de cifras
                                         v_r_cifras.consecutivo      ,
                                         v_r_cifras.folio            ,
                                         v_r_cifras.nss              ,
                                         v_r_cifras.nombre           ,
                                         v_r_cifras.estado           ,
                                         v_r_cifras.desc_estado      ,
                                         v_r_cifras.diagnostico      ,
                                         v_r_cifras.desc_diagnostico ,
                                         v_r_cifras.entidad_origen   ,
                                         v_r_cifras.avis_viv_dev     ,
                                         v_r_cifras.mto_retiro       ,
                                         v_r_cifras.mto_act_retiro   ,
                                         v_r_cifras.mto_cv_patron    ,
                                         v_r_cifras.mto_cv_trabajador,
                                         v_r_cifras.mto_act_cv       ,
                                         v_r_cifras.mto_vivienda     ,
                                         --Arreglo de auxiliares
                                         v_r_aux.folio_sua           ,
                                         v_r_aux.f_pago              ,
                                         v_r_aux.periodo_pago        ,
                                         v_r_aux.reg_patronal_imss   ,
                                         v_r_aux.clave_entidad_rec   ,
                                         v_r_aux.id_dpe_referencia   
      -- se incrementa el indice
      LET v_si_indice = v_si_indice + 1
      -- se transfieren los datos al arreglo
      LET v_r_cifras.consecutivo = v_si_indice
      LET v_ar_cifras[v_si_indice].* = v_r_cifras.*
      --
      LET v_ar_cifras[v_si_indice].desc_estado =
          v_ar_cifras[v_si_indice].estado, " - "
          ,v_ar_cifras[v_si_indice].desc_estado
      --
      LET v_ar_cifras[v_si_indice].desc_diagnostico =
          v_ar_cifras[v_si_indice].diagnostico, " - "
          ,v_ar_cifras[v_si_indice].desc_diagnostico  

      -- se acumulan numero de registros y montos en registro de cifras totales
      LET v_r_acum.avis_viv_dev    = 
          v_r_acum.avis_viv_dev     + v_r_cifras.avis_viv_dev
          
      LET v_r_acum.mto_act_cv     = 
          v_r_acum.mto_act_cv     + v_r_cifras.mto_act_cv
      LET v_r_acum.mto_act_retiro = 
          v_r_acum.mto_act_retiro + v_r_cifras.mto_act_retiro
      LET v_r_acum.mto_cv_patron  = 
          v_r_acum.mto_cv_patron  + v_r_cifras.mto_cv_patron
      LET v_r_acum.mto_cv_trabajador = 
          v_r_acum.mto_cv_trabajador + v_r_cifras.mto_cv_trabajador
      LET v_r_acum.mto_retiro     = 
          v_r_acum.mto_retiro     + v_r_cifras.mto_retiro
      LET v_r_acum.mto_vivienda   = 
          v_r_acum.mto_vivienda   + v_r_cifras.mto_vivienda
      -- Guarda registro auxiliar -
      LET v_ar_aux[v_si_indice].* = v_r_aux.*

      LET v_s_sql = "\n SELECT rp.aivs_viv_dev,", 
                    "\n        rp.imp_viv_dev,",
                    "\n        rp.resul_op",
                    "\n   FROM dpe_resp_procesar rp",
                    "\n  WHERE rp.reg_patronal_imss = '",v_r_aux.reg_patronal_imss,"'",
                    "\n    AND rp.nss = '",v_r_cifras.nss,"'",
                    "\n    AND rp.periodo_pago = '",v_r_aux.periodo_pago,"'"

      PREPARE prp_rep_proc_detalle FROM v_s_sql CLIPPED
      DECLARE cur_rep_proc_detalle CURSOR FOR prp_rep_proc_detalle 

      --LET v_ind_parc_det = 0
      --DISPLAY v_s_sql

      -- se reinicia el registro
      INITIALIZE v_r_cifras.* TO NULL

      FOREACH cur_rep_proc_detalle INTO v_r_cifras.aivs_respuesta,
                                        v_r_cifras.m_viv_respuesta,
                                        v_r_cifras.res_operacion
        -- LET v_ind_parc_det = v_ind_parc_det + 1
--DISPLAY "RES_OP |", v_r_cifras.res_operacion CLIPPED, "|"
      END FOREACH
      LET v_ar_cifras[v_si_indice].aivs_respuesta =  v_r_cifras.aivs_respuesta
      LET v_ar_cifras[v_si_indice].m_viv_respuesta = v_r_cifras.m_viv_respuesta
      LET v_r_cifras.res_operacion = v_r_cifras.res_operacion CLIPPED
      --IF v_r_cifras.res_operacion IS NULL OR v_r_cifras.res_operacion  = 0 THEN
      IF v_r_cifras.res_operacion  = 0 OR v_r_cifras.res_operacion IS NULL THEN  
         LET v_ar_cifras[v_si_indice].res_operacion  = "SIN RESPUESTA"
      ELSE 
         -- Generar conteo de solicitudes aceptadas
         CASE (v_r_cifras.res_operacion)
            WHEN 1 -- Aceptadas
               LET v_ar_cifras[v_si_indice].res_operacion = "ACEPTADA"
            WHEN 2 -- Rechazadas
               LET v_ar_cifras[v_si_indice].res_operacion = "RECHAZADA"
            WHEN 3 -- Rechazadas
               LET v_ar_cifras[v_si_indice].res_operacion = "PENDIENTE"
            WHEN 4 -- Parciales
               LET v_ar_cifras[v_si_indice].res_operacion = "ACEPTADA PARCIAL"
         END CASE       
      END IF 
      LET v_r_cifras.res_operacion = NULL

      -- Generar conteo de solicitudes aceptadas
      CASE (v_r_cifras.estado)
         WHEN 1 -- Aceptadas
            IF(v_r_aux.clave_entidad_rec ='RCV')THEN
               -- Aceptadas RCV
               LET v_ar_solicitud[1].aceptadas = 
                   v_ar_solicitud[1].aceptadas + 1
               LET v_ar_solicitud[3].aceptadas = 
                   v_ar_solicitud[3].aceptadas  
                   + v_r_cifras.mto_vivienda
            ELSE
               -- Aceptadas VIV
               LET v_ar_solicitud[2].aceptadas = 
                   v_ar_solicitud[2].aceptadas + 1
               LET v_ar_solicitud[4].aceptadas = 
                   v_ar_solicitud[4].aceptadas  
                   + v_r_cifras.mto_vivienda
            END IF
         WHEN 2 -- Rechazadas
            IF(v_r_aux.clave_entidad_rec ='RCV')THEN
               -- Rechazadas RCV
               LET v_ar_solicitud[1].rechazadas = 
                   v_ar_solicitud[1].rechazadas + 1
               LET v_ar_solicitud[3].rechazadas = 
                   v_ar_solicitud[3].rechazadas
                   + v_r_cifras.mto_vivienda
            ELSE
               -- Rechazadas VIV
               LET v_ar_solicitud[2].rechazadas = 
                   v_ar_solicitud[2].rechazadas + 1
               LET v_ar_solicitud[4].rechazadas = 
                   v_ar_solicitud[4].rechazadas
                   + v_r_cifras.mto_vivienda
            END IF
         WHEN 3 -- Pendientes
           IF(v_r_aux.clave_entidad_rec ='RCV')THEN
               -- Pendientes RCV
               LET v_ar_solicitud[1].pendientes = 
                   v_ar_solicitud[1].pendientes + 1
               LET v_ar_solicitud[3].pendientes = 
                   v_ar_solicitud[3].pendientes
                   + v_r_cifras.mto_vivienda
            ELSE
               -- Pendientes VIV
               LET v_ar_solicitud[2].pendientes = 
                   v_ar_solicitud[2].pendientes + 1
               LET v_ar_solicitud[4].pendientes = 
                   v_ar_solicitud[4].pendientes
                   + v_r_cifras.mto_vivienda 
            END IF 
      END CASE
      --
   END FOREACH
   FREE Curr_ObtDatos_Integrados
   IF(v_si_indice = 0)THEN
      CALL fn_mensaje("Atención",
           "No se encontraron registros de detalle \n con los criterios indicados",
           "information")
      RETURN
   END IF

   INITIALIZE v_r_cifras TO NULL
   LET v_r_cifras.entidad_origen = "Subtotal"
   LET v_r_cifras.mto_retiro    = v_r_acum.mto_retiro    
   LET v_r_cifras.mto_act_retiro= v_r_acum.mto_act_retiro
   LET v_r_cifras.mto_cv_patron = v_r_acum.mto_cv_patron
   LET v_r_cifras.mto_cv_trabajador= v_r_acum.mto_cv_trabajador
   LET v_r_cifras.mto_act_cv    = v_r_acum.mto_act_cv
   LET v_r_cifras.mto_vivienda  = v_r_acum.mto_vivienda
   LET v_r_cifras.avis_viv_dev  = v_r_acum.avis_viv_dev
   
   -- Llenar arreglo de solicitudes.
   LET v_ar_solicitud[1].totales =
       v_ar_solicitud[1].aceptadas +
       v_ar_solicitud[1].rechazadas +
       v_ar_solicitud[1].pendientes
   LET v_ar_solicitud[2].totales = 
       v_ar_solicitud[2].aceptadas +
       v_ar_solicitud[2].rechazadas +
       v_ar_solicitud[2].pendientes
   LET v_ar_solicitud[3].totales =
       v_ar_solicitud[3].aceptadas +
       v_ar_solicitud[3].rechazadas +
       v_ar_solicitud[3].pendientes
   LET v_ar_solicitud[4].totales = 
       v_ar_solicitud[4].aceptadas +
       v_ar_solicitud[4].rechazadas +
       v_ar_solicitud[4].pendientes

   
   
   DISPLAY v_r_cifras.avis_viv_dev, v_r_cifras.mto_vivienda USING "###,###,###,##&.&&" 
        TO COL_SUB_AIVS, COL_SUB_VIV

END FUNCTION

-- Funcion que desliega los totales del archivo
FUNCTION fn_totales(p_folio, p_nss, p_reg_patronal, 
      	   p_estado_solicitud, p_entidad_origen)

DEFINE p_folio             LIKE glo_folio.folio,
       p_nss               CHAR(11),
       p_reg_patronal      CHAR(11),
       p_estado_solicitud  SMALLINT,
       p_entidad_origen    CHAR(3),
       v_s_sql             STRING

   CALL v_ar_contadores.clear()
   
   LET v_s_sql = "SELECT SUM(case d.estado_solicitud WHEN 1 THEN 1 ELSE 0 END),",
                 "\n     SUM(case d.estado_solicitud WHEN 2 THEN 1 ELSE 0 END),",
                 "\n     SUM(case d.estado_solicitud WHEN 3 THEN 1 ELSE 0 END),",
                 "\n     SUM(case d.estado_solicitud WHEN 1 THEN imp_viv_dev ELSE 0 END),",
                 "\n     SUM(case d.estado_solicitud WHEN 2 THEN imp_viv_dev ELSE 0 END),",
                 "\n     SUM(case d.estado_solicitud WHEN 3 THEN imp_viv_dev ELSE 0 END),",
                 "\n     COUNT(*),",
                 "\n     SUM(imp_viv_dev)",
                 "\n FROM dpe_sol_trabajador d",
                 "\n WHERE d.folio = ", p_folio
   
   
   IF ( p_reg_patronal IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.reg_patronal_imss = '",p_reg_patronal,"'"
   END IF
   
   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.nss = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.estado_solicitud =",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_entidad_origen IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND d.cve_entidad_origen ='",p_entidad_origen,"'"
   END IF
      
   LET v_s_sql = v_s_sql CLIPPED
   
   --DISPLAY "v_s_sql total: ", v_s_sql
   PREPARE Prpr_ObtDatos_total FROM v_s_sql CLIPPED
   EXECUTE Prpr_ObtDatos_total INTO v_ar_solicitud[2].aceptadas,
                                    v_ar_solicitud[2].rechazadas,
                                    v_ar_solicitud[2].pendientes,
                                    v_ar_solicitud[4].aceptadas,
                                    v_ar_solicitud[4].rechazadas,
                                    v_ar_solicitud[4].pendientes,
                                    v_ar_solicitud[2].totales,
                                    v_ar_solicitud[4].totales

   LET v_ar_contadores[1].entidad_receptora = v_ar_solicitud[2].entidad_receptora
   LET v_ar_contadores[2].entidad_receptora = v_ar_solicitud[4].entidad_receptora
   --
   LET v_ar_contadores[1].aceptadas         = v_ar_solicitud[2].aceptadas USING "###,###,###,##&"
   LET v_ar_contadores[2].aceptadas         = v_ar_solicitud[4].aceptadas USING "###,###,###,##&.&&"
   LET v_ar_contadores[1].rechazadas        = v_ar_solicitud[2].rechazadas USING "###,###,###,##&"
   LET v_ar_contadores[2].rechazadas        = v_ar_solicitud[4].rechazadas USING "###,###,###,##&.&&"
   LET v_ar_contadores[1].pendientes        = v_ar_solicitud[2].pendientes USING "###,###,###,##&"
   LET v_ar_contadores[2].pendientes        = v_ar_solicitud[4].pendientes USING "###,###,###,##&.&&"
   LET v_ar_contadores[1].totales           = v_ar_solicitud[2].totales USING "###,###,###,##&"
   LET v_ar_contadores[2].totales           = v_ar_solicitud[4].totales USING "###,###,###,##&.&&"       

END FUNCTION