################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIC01                                                        #
#Objetivo     => Consultar los datos cargados de informacion recibida para     #
#                Unificación de cuentas                                        #
#Fecha inicio => Mayo 29, 2012                                                 #
################################################################################
--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 
-- 20/11/2014 Se modifica consulta de folios para combo AG
--==============================================================================
 
DATABASE safre_viv
GLOBALS "UNIG01.4gl"
DEFINE w ui.Window,
       f ui.Form,
       g_usuario_cod          LIKE seg_usuario.usuario_cod,
       g_tipo_ejecucion       SMALLINT,
       g_s_titulo             CHAR(25),
       v_r_res_resumen RECORD -- registro llenar el resumen de patrones
          v_folio_unificacion LIKE glo_folio.folio,
          v_folio_liquidacion LIKE glo_folio.folio,
          cont_unificador     INTEGER,
          cont_unificado      INTEGER,
          v_f_notificacion    DATE,
          v_f_liquidacion     DATE
       END RECORD,
       v_ar_res_resumen DYNAMIC ARRAY OF RECORD -- registro llenar el resumen de patrones
       	  v_folio_unificacion LIKE glo_folio.folio,
          v_folio_liquidacion LIKE glo_folio.folio,
          cont_unificador     INTEGER,
          cont_unificado      INTEGER,
          v_f_notificacion    DATE,
          v_f_liquidacion     DATE
       END RECORD,
       v_r_res_unificador RECORD -- registro llenar el detalle del unificador
          v_id_unificador      DECIMAL(9,0),
          v_folio_unificacion  LIKE glo_folio.folio,
          v_folio_liquidacion  LIKE glo_folio.folio,
          v_nss_unificador     LIKE afi_derechohabiente.nss,
          v_curp               CHAR(18),
          v_estado_unificacion CHAR(30),
          v_diagnostico        CHAR(30),
          v_afore_aclaracion   SMALLINT,
          v_afore_receptora    SMALLINT,
          v_estado_familia     CHAR(10),
          v_confrontacion      CHAR(15),
          v_ind_procedencia    CHAR(13),
          v_st_convocatoria    CHAR(15)
       END RECORD,
       v_ar_res_unificador DYNAMIC ARRAY OF RECORD -- registro llenar el resumen de patrones
          v_id_unificador      DECIMAL(9,0),
          v_folio_unificacion  LIKE glo_folio.folio,
          v_folio_liquidacion  LIKE glo_folio.folio,
          v_nss_unificador     LIKE afi_derechohabiente.nss,
          v_curp_unificador    CHAR(18),
          v_estado_unificacion CHAR(30),
          v_diagnostico        CHAR(30),
          v_afore_aclaracion   CHAR(20),
          v_afore_receptora    CHAR(20),
          v_estado_familia     CHAR(15),
          v_confrontacion      CHAR(15),
          v_ind_procedencia    SMALLINT,
          v_st_convocatoria    CHAR(15)
       END RECORD,
       v_r_res_unificado RECORD -- registro llenar el detalle del unificador
          v_folio_unificacion   LIKE glo_folio.folio,
          v_nsscta1             LIKE afi_derechohabiente.nss,
          v_nombre              CHAR(50),
          v_estado_unificacion  CHAR(30),
          v_diagnostico         CHAR(30),
          v_afore_aclaracion    SMALLINT,
          v_id_derechohabiente  DECIMAL(9,0),
          ---
          v_tpo_originacion     CHAR(30),
          v_tpo_credito         CHAR(30),
          v_numero_credito      DECIMAL(10,0)

       END RECORD,
       v_ar_res_unificado DYNAMIC ARRAY OF RECORD -- registro llenar el resumen de patrones
          v_folio_unificacion   LIKE glo_folio.folio,
          v_nsscta1             LIKE afi_derechohabiente.nss,
          v_nombre              CHAR(50),
          v_estado_unificacion  CHAR(30),
          v_diagnostico         CHAR(30),
          v_afore_aclaracion    CHAR(20),
          v_tpo_originacion     CHAR(30),
          v_tpo_credito         CHAR(30),
          v_numero_credito      DECIMAL(10,0)
       END RECORD   
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   
   LET g_usuario_cod = p_usuario_cod
   LET g_tipo_ejecucion = p_tipo_ejecucion
   LET g_s_titulo = p_s_titulo

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
      
   -- consulta de informacion recibida de OP98
   CALL fn_genera_consulta_integracion(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: UNIC01
Nombre: fn_genera_consulta_integracion
Fecha creacion: Mayo 29, 2012
Narrativa del proceso que realiza:
  Genera consulta de datos integrados
======================================================================}
FUNCTION fn_genera_consulta_integracion(p_usuario_cod)
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_folio              LIKE glo_folio.folio,
       v_folio_liquida      LIKE glo_folio.folio,
       v_nss                CHAR(11),
       v_estado_solicitud   SMALLINT,
       v_cbx_folios         ui.ComboBox, -- combo de folios
       v_cbx_estado         ui.ComboBox, -- combo de estados
       v_fec_liquida        DATE,
       v_fec_notifica       DATE,
       v_s_cadena           STRING -- cadena de texto
DEFINE v_r_glo_ctr_archivo  RECORD
          folio             LIKE glo_folio.folio,
          nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo
       END RECORD
DEFINE v_i_conArch          INTEGER,
       v_cadena_condiciones STRING,
       v_si_indice          INTEGER
       
   OPEN WINDOW win_ConsInfo WITH FORM "UNIC010"
   -- Recupera punteros a ventana para control de grupos
   LET w = ui.Window.getCurrent()
   LET f = w.getForm()   
   -- se le asigna el apuntado der combo a la variable
   LET v_cbx_estado = ui.ComboBox.forName("formonly.cmb_estado")
   LET v_cbx_folios = ui.ComboBox.forName("formonly.cmb_folio")

   -- Ocultar tablas de resultados
   CALL f.setElementHidden("gru_resumen_glo",1)
   
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
           SELECT DISTINCT fo.folio, ar.nombre_archivo
             FROM glo_ctr_archivo ar,
                  glo_folio fo
            WHERE ar.proceso_cod IN ( 2301, 2309, 2318)
              AND ar.opera_cod IN (1,2)
              AND ar.folio = fo.folio

   LET v_i_conArch = 0
   
   FOREACH cur_folios INTO v_r_glo_ctr_archivo.*
      LET v_s_cadena = v_r_glo_ctr_archivo.folio USING "##########", " - ", 
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
   
   -- el primer elemento en el combo
   CALL v_cbx_estado.addItem(-1," ")
   CALL v_cbx_folios.addItem(-1," ")
   -- se asignan los valores por omision
   LET v_folio            = -1
   LET v_estado_solicitud = -1
   LET v_nss              = NULL
   LET v_fec_liquida      = NULL
   LET v_fec_notifica     = NULL

   DISPLAY TODAY USING "dd-mm-YYYY" TO txt_fecha
   
   INPUT v_folio, v_folio_liquida, v_nss, v_estado_solicitud,
   	     v_fec_liquida, v_fec_notifica WITHOUT DEFAULTS
    FROM cmb_folio, txt_folio_liquida, txt_nss, cmb_estado, 
         txt_fec_liquida, txt_fec_notifica
   ATTRIBUTES (UNBUFFERED)
      BEFORE INPUT
      ON ACTION ACCEPT
         -- Mostrar tablas de resultados
         CALL f.setElementHidden("gru_resumen_glo",0)

         --Arma las condiciones en base a los parámetros capturados 
         CALL fn_arma_condiciones(p_usuario_cod, v_folio, v_folio_liquida, 
                                  v_nss, v_estado_solicitud,v_fec_liquida,
                                  v_fec_notifica)
         RETURNING v_cadena_condiciones

         CALL fn_muestra_consulta_integracion(p_usuario_cod, v_folio, v_folio_liquida, 
                                              v_nss, v_estado_solicitud,v_fec_liquida,
                                              v_fec_notifica,v_cadena_condiciones)
         RETURNING v_si_indice
         
         -- Limpia datos para nueva consulta
         LET v_folio            = -1
         LET v_estado_solicitud = -1
         LET v_nss              = NULL
         LET v_fec_liquida      = NULL
         LET v_fec_notifica     = NULL

         DISPLAY v_folio, v_folio_liquida, v_nss, v_estado_solicitud,
   	             v_fec_liquida, v_fec_notifica
              TO cmb_folio, txt_folio_liquida, txt_nss, cmb_estado, 
                 txt_fec_liquida, txt_fec_notifica
         
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
Clave: UNIC01
Nombre: fn_muestra_consulta_integracion
Fecha creacion: Mayo 29, 2012
Narrativa del proceso que realiza:
   Muetra detalle de proceso de integración: 
     "CONSULTA DE DIAGNOSTICOS DE UNIFICACIÓN DE CUENTAS"
======================================================================}
FUNCTION fn_muestra_consulta_integracion(p_usuario_cod, p_folio, p_folio_liquida, 
                                         p_nss, p_estado_solicitud, p_fec_liquida,
                                         p_fec_notifica,v_cadena_condiciones)
DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_folio              LIKE glo_folio.folio,
       p_folio_liquida      LIKE glo_folio.folio,
       p_nss                CHAR(11),
       p_estado_solicitud   SMALLINT,
       p_fec_liquida        DATE,
       p_fec_notifica       DATE,
       --
       v_si_indice          INTEGER, -- indice de arreglo
       v_s_sql              STRING, -- cadena con una instruccion SQL
       v_s_cond             STRING, -- cadena con una instruccion SQL
       cadena_total         STRING,
       v_sql_tot            STRING,
       v_cadena_condiciones STRING
   
   CALL v_ar_res_resumen.CLEAR()
   -- Consulta que llena el arreglo del resumen global
   LET v_s_sql = "\n SELECT a.folio_unificacion,",
                 "\n        a.folio_liquidacion,",
                 "\n        COUNT(*),",        --Total unificadores
                 "\n        '',",              --Total unificados se consulta mas abajo    
                 "\n        '',",  --"\n        a.f_notificacion,",
                 "\n        a.f_liquidacion",                 
                 "\n FROM uni_det_unificador a",
                 "\n WHERE 1=1"

   IF ( p_folio <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.folio_unificacion = ", p_folio 
   END IF

   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.nss_unificador = '",p_nss,"'"
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.estado_familia = ",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_folio_liquida IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.folio_liquidacion = ",p_folio_liquida
   END IF
   
   IF ( p_fec_liquida IS NOT NULL) THEN
      IF p_fec_liquida <> '12/31/1899' THEN
         LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.f_liquidacion = '",p_fec_liquida,"'"
      END IF    
   END IF
   
   IF ( p_fec_notifica IS NOT NULL ) THEN
      IF p_fec_notifica <> '12/31/1899' THEN
         LET v_s_sql = v_s_sql CLIPPED
          ,"\n  AND a.f_notificacion = '",p_fec_notifica,"'"
      END IF    
   END IF
    
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n GROUP BY 1,2,4,5,6"
       ,"\n ORDER BY 1,2,4,5,6"

  --Arma condiciones para Unificados
  -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_cond = v_s_cond CLIPPED
          ,"\n  AND a.estado_familia = ",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_folio_liquida IS NOT NULL ) THEN
      LET v_s_cond = v_s_cond CLIPPED
          ,"\n  AND a.folio_liquidacion = ",p_folio_liquida
   END IF
   
   IF ( p_fec_liquida IS NOT NULL) THEN
      IF p_fec_liquida <> '12/31/1899' THEN
         LET v_s_cond = v_s_cond CLIPPED
          ,"\n  AND a.f_liquidacion = '",p_fec_liquida,"'"
      END IF    
   END IF
   
   IF ( p_fec_notifica IS NOT NULL ) THEN
      IF p_fec_notifica <> '12/31/1899' THEN
         LET v_s_cond = v_s_cond CLIPPED
          ,"\n  AND a.f_notificacion = '",p_fec_notifica,"'"
      END IF    
   END IF   
   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_cond = v_s_cond CLIPPED
          ,"\n  AND a.nss_unificador = '",p_nss,"'"
   END IF

   PREPARE Prpr_ObtDatos_global FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_global CURSOR FOR Prpr_ObtDatos_global
   LET v_si_indice = 0	
   FOREACH Curr_ObtDatos_global INTO v_r_res_resumen.*
      LET v_si_indice = v_si_indice + 1
      LET v_ar_res_resumen[v_si_indice].v_folio_unificacion = v_r_res_resumen.v_folio_unificacion
      LET v_ar_res_resumen[v_si_indice].v_folio_liquidacion = v_r_res_resumen.v_folio_liquidacion
      LET v_ar_res_resumen[v_si_indice].cont_unificador = v_r_res_resumen.cont_unificador
      -- OBTIENE TOTALES UNIFICADOS
      IF v_r_res_resumen.v_folio_liquidacion IS NULL THEN  
      	   LET cadena_total= "IS NULL"    
      ELSE 
      	   LET cadena_total= " = ", v_r_res_resumen.v_folio_liquidacion   
      END IF  
      --Obtiene total de unificados 
      LET v_sql_tot="\n SELECT COUNT (*)",
                    "\n   FROM uni_det_unificador a,",
                    "\n        uni_det_unificado b ",
                    "\n  WHERE a.folio_unificacion=b.folio_unificacion",
                    "\n    AND a.id_unificador=b.id_unificador",
                    "\n    AND a.folio_unificacion=",v_ar_res_resumen[v_si_indice].v_folio_unificacion,
                    "\n    AND a.folio_liquidacion ",cadena_total ,v_s_cond 
      --DISPLAY "v_sql_tot:",v_sql_tot
      PREPARE c_tot_unidicado FROM  v_sql_tot   
      EXECUTE c_tot_unidicado  INTO  v_ar_res_resumen[v_si_indice].cont_unificado   
      
      LET v_ar_res_resumen[v_si_indice].v_f_notificacion = v_r_res_resumen.v_f_notificacion
      LET v_ar_res_resumen[v_si_indice].v_f_liquidacion = v_r_res_resumen.v_f_liquidacion

   END FOREACH	

   DISPLAY p_folio, 
           p_folio_liquida, 
           p_nss, 
           p_estado_solicitud,
   	       p_fec_liquida, 
   	       p_fec_notifica
        TO cmb_folio, 
           txt_folio_liquida, 
           txt_nss, 
           cmb_estado, 
           txt_fec_liquida, 
           txt_fec_notifica
   
   DIALOG ATTRIBUTES (UNBUFFERED)
      DISPLAY ARRAY v_ar_res_resumen TO scr_resumen.*
      BEFORE DISPLAY
      --Valida si existen registros, si existen muestra en pantalla, si no envía mensaje
      IF v_si_indice = 0 THEN 
         CALL f.setElementHidden("gru_resumen_glo",1)                                     
         CALL fn_mensaje ("Atención", "No se encontraron registros con los datos proporcionados", "stop")
         EXIT DIALOG 
      END IF
      
         DISPLAY TODAY USING "dd-mm-YYYY" TO txt_fecha
         
         ON ACTION ACCEPT
           --display "condiciones :",v_cadena_condiciones
            CALL fn_muestra_detalles(v_ar_res_resumen[ARR_CURR()].v_folio_unificacion,
                                     v_ar_res_resumen[ARR_CURR()].v_folio_liquidacion,
                                     p_folio, p_folio_liquida, p_nss, p_estado_solicitud,
   	                                 p_fec_liquida, p_fec_notifica,p_usuario_cod,v_cadena_condiciones)
         
         ON ACTION regresa
            LET w = ui.Window.getCurrent()
            LET f = w.getForm()
            CALL f.setElementHidden("gru_resumen_glo",1)
            EXIT DIALOG
         
      END DISPLAY
      
     AFTER DIALOG
        CONTINUE DIALOG
   END DIALOG

   RETURN v_si_indice
END FUNCTION -- fn_muestra_consulta_integracion

{======================================================================
Clave: UNIC01
Nombre: fn_muestra_detalles
Fecha creacion: Mayo 30, 2012
Narrativa del proceso que realiza:
Muestra el detalle del unificador y unificado 
======================================================================}
FUNCTION fn_muestra_detalles(v_folio, 
                             v_folio_liquida, 
                             p_folio, 
                             p_folio_liquida, 
                             p_nss,
                             p_estado_solicitud, 
                             p_fec_liquida, 
                             p_fec_notifica,
                             p_usuario_cod,
                             v_cadena_condiciones)
                             
DEFINE p_folio              LIKE glo_folio.folio,
       p_folio_liquida      LIKE glo_folio.folio,
       p_nss                CHAR(11),
       p_estado_solicitud   SMALLINT,
       p_fec_liquida        DATE,
       p_fec_notifica       DATE,
       v_cadena_condiciones STRING,
       v_folio              DECIMAL(9,0),
       v_folio_liquida      DECIMAL(9,0),
       v_si_indice          INTEGER, -- indice de arreglo
       v_si_indice2         INTEGER, -- indice del reporte
       v_s_sql              STRING, -- cadena con una instruccion SQL
       QryTxt               STRING,
       v_s_reporte          STRING,-- Para insertar en la forma el botón de reporte
       v_nom_reporte        VARCHAR(80), -- nombre del reporte
       v_desc_afore         CHAR(15),
       r_ruta_bin           LIKE seg_modulo.ruta_bin,
       r_ruta_listados      LIKE seg_modulo.ruta_listados,
       p_usuario_cod        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_id_derechohabiente DECIMAL(9,0),
       v_comando            STRING

   CALL v_ar_res_unificador.CLEAR()
   CALL v_ar_res_unificado.CLEAR()
   
   -- Consulta que llena el arreglo de unificadores
   LET v_s_sql = "\n SELECT DISTINCT a.id_unificador,",
                 "\n                 a.folio_unificacion,",
                 "\n                 a.folio_liquidacion,",
                 "\n                 a.nss_unificador,", 
                 "\n                 a.curp_unificador, ",
                 "\n                 b.estado_desc_corta,",
                 "\n                 c.diag_desc_larga,",
                 "\n                 a.cve_afore_aclaracion,",
                 "\n                 a.clave_afore_receptora,", 
                 "\n                 b.estado_desc_corta,",
                 "\n                 d.diagnostico_uni,",
                 "\n                 a.ind_procedencia,",
                 "\n                 a.estatus_convocatoria",
                 "\n   FROM uni_det_unificador a,",
                 "\n        uni_estado_solicitud b,",
                 "\n        uni_diagnostico_sol c,",
                 "\n        uni_det_unificado d",
                 "\n  WHERE a.estado_familia = b.id_estado", --Se cambia a estado familia 
                 "\n    AND a.diagnostico = c.id_diagnostico",
                 "\n    AND a.folio_unificacion = d.folio_unificacion",
                 "\n    AND a.id_unificador = d.id_unificador",
                 "\n    AND a.folio_unificacion = ", v_folio
  
  
   IF ( v_folio_liquida ) IS NOT NULL THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.folio_liquidacion = ",v_folio_liquida
   ELSE
   	 LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.folio_liquidacion IS NULL"
   END IF
   
   IF ( p_nss IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.nss_unificador = '",p_nss,"'"
   END IF
    
   -- si se envio estado de solicitud
   IF ( p_estado_solicitud <> -1 ) THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.estado_familia = ",p_estado_solicitud
   END IF
   
   -- si se envio estado de solicitud
   IF ( p_folio_liquida IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.folio_liquidacion = ",p_folio_liquida
   END IF
   
   IF ( p_fec_liquida IS NOT NULL) THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.f_liquidacion = '",p_fec_liquida,"'"
   END IF
   
   IF ( p_fec_notifica IS NOT NULL ) THEN
      LET v_s_sql = v_s_sql CLIPPED
      ,"\n  AND a.f_notificacion = '",p_fec_notifica,"'"
   END IF
   
   LET v_s_sql = v_s_sql CLIPPED
       ,"\n ORDER BY 1,2,3,4,5,6,7,8,9,10"

   --DISPLAY v_s_sql    

   PREPARE Prpr_ObtDatos_unificador FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_unificador CURSOR FOR Prpr_ObtDatos_unificador
   LET v_si_indice = 0	
   FOREACH Curr_ObtDatos_unificador INTO v_r_res_unificador.*
      LET v_si_indice = v_si_indice + 1
      LET v_ar_res_unificador[v_si_indice].v_id_unificador = v_r_res_unificador.v_id_unificador
      LET v_ar_res_unificador[v_si_indice].v_folio_unificacion = v_r_res_unificador.v_folio_unificacion
      LET v_ar_res_unificador[v_si_indice].v_folio_liquidacion = v_r_res_unificador.v_folio_liquidacion
      LET v_ar_res_unificador[v_si_indice].v_nss_unificador = v_r_res_unificador.v_nss_unificador
      LET v_ar_res_unificador[v_si_indice].v_curp_unificador = v_r_res_unificador.v_curp
      LET v_ar_res_unificador[v_si_indice].v_estado_familia = v_r_res_unificador.v_estado_familia
      LET v_ar_res_unificador[v_si_indice].v_estado_unificacion = v_r_res_unificador.v_estado_unificacion
      LET v_ar_res_unificador[v_si_indice].v_diagnostico = v_r_res_unificador.v_diagnostico
      LET v_ar_res_unificador[v_si_indice].v_ind_procedencia = v_r_res_unificador.v_ind_procedencia
      LET v_ar_res_unificador[v_si_indice].v_st_convocatoria = v_r_res_unificador.v_st_convocatoria

      SELECT afore_desc
        INTO v_desc_afore
        FROM cat_afore
       WHERE afore_cod = v_r_res_unificador.v_afore_aclaracion
      
      LET v_ar_res_unificador[v_si_indice].v_afore_aclaracion = v_r_res_unificador.v_afore_aclaracion||"-"||v_desc_afore CLIPPED
      
      SELECT afore_desc
        INTO v_desc_afore
        FROM cat_afore
       WHERE afore_cod = = v_r_res_unificador.v_afore_receptora
      
      LET v_ar_res_unificador[v_si_indice].v_afore_receptora = v_r_res_unificador.v_afore_receptora||"-"||v_desc_afore CLIPPED
   
      CASE v_r_res_unificador.v_confrontacion
         WHEN '01'
            LET v_ar_res_unificador[v_si_indice].v_confrontacion = "PROCEDENTE"
         WHEN '02'
      	    LET v_ar_res_unificador[v_si_indice].v_confrontacion = "NO PROCEDENTE"
         WHEN '04'
      	    LET v_ar_res_unificador[v_si_indice].v_confrontacion = "NO SE PRESENTO"
         WHEN '05'
      	    LET v_ar_res_unificador[v_si_indice].v_confrontacion = "NO HAY ELEMENTOS"
      END CASE	          

      CASE v_r_res_unificador.v_ind_procedencia
         WHEN "0"
            LET v_ar_res_unificador[v_si_indice].v_ind_procedencia = "PENDIENTE"
         WHEN "1"
      	    LET v_ar_res_unificador[v_si_indice].v_ind_procedencia = "PROCEDENTE"
         WHEN "2"
      	    LET v_ar_res_unificador[v_si_indice].v_ind_procedencia = "NO PROCEDENTE"
      END CASE	      

      CASE v_r_res_unificador.v_st_convocatoria
         WHEN '0'
            LET v_ar_res_unificador[v_si_indice].v_st_convocatoria = "NO RESPONSABLE"
         WHEN '1'
      	    LET v_ar_res_unificador[v_si_indice].v_st_convocatoria = "RESPONSABLE"
         WHEN '3'
      	    LET v_ar_res_unificador[v_si_indice].v_st_convocatoria = "INVERTIDA"
      END CASE	
   END FOREACH
   
   OPEN WINDOW win_Condetalle WITH FORM "UNIC011"
      LET w = ui.Window.getCurrent()
      LET f = w.getForm()
      
   -- Funcion que desliega los totales del archivo
   CALL fn_llena_totales(v_cadena_condiciones)
   
      DIALOG ATTRIBUTES (UNBUFFERED)
         DISPLAY ARRAY v_ar_res_unificador TO scr_unificador.*
         BEFORE ROW
         	  CALL v_ar_res_unificado.CLEAR()
            LET QryTxt = "\n AND a.id_unificador = ", 
                                  v_ar_res_unificador[ARR_CURR()].v_id_unificador,
                         "\n AND a.folio_unificacion = ",
                                  v_ar_res_unificador[ARR_CURR()].v_folio_unificacion
          
      	    CALL fn_genera_detalle_unificado(QryTxt,p_folio, p_folio_liquida, p_nss,
                                             p_estado_solicitud, p_fec_liquida, 
                                             p_fec_notifica)
         

            ON ACTION consulta_unificador 
               CALL Consulta_Uni_Cuentas_Unificador(v_ar_res_unificador[ARR_CURR()].v_folio_unificacion,
                                                    v_ar_res_unificador[ARR_CURR()].v_nss_unificador)
            
            ON ACTION Consulta_liquidacion
               CALL consulta_liquidados(v_ar_res_unificador[ARR_CURR()].v_folio_liquidacion,v_ar_res_unificador[ARR_CURR()].v_nss_unificador)
                                                
            ON ACTION Consulta_Derechohabiente
               -- se ejecuta la consulta del derechohabiente usando la consulta general
               LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod CLIPPED || "1 'Consulta de Derechohabiente' " || v_ar_res_unificador[ARR_CURR()].v_nss_unificador  
               DISPLAY v_comando
               RUN v_comando
            
            ON ACTION Consulta_Saldo
               -- Muestra la consulta del saldo
               SELECT id_derechohabiente
                 INTO v_id_derechohabiente
                 FROM afi_derechohabiente
                WHERE nss = v_ar_res_unificador[ARR_CURR()].v_nss_unificador
             
               CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente,g_tipo_ejecucion, g_s_titulo)
            
            ON ACTION reporte
               --Genera el reporte de Unificador/Unificado
               CALL genera_reporte_unificados(p_usuario_cod) 
               RETURNING v_si_indice2,v_s_reporte
                
                  IF(v_si_indice2 = 0)THEN
                     CALL fn_mensaje("Atención",
                                     "No se pudo generar el reporte",
                                     "information")
                  END IF	  
            
                DISPLAY v_s_reporte TO v_s_reporte
             
            AFTER DISPLAY 
            
            DISPLAY v_s_reporte TO v_s_reporte
         END DISPLAY
         
         DISPLAY ARRAY v_ar_res_unificado TO scr_unificado.*
            ON ACTION consulta_unificado
               CALL Consulta_Uni_Cuentas_Unificado(v_ar_res_unificado[ARR_CURR()].v_folio_unificacion,
                                                   v_ar_res_unificado[ARR_CURR()].v_nsscta1)
         
            -- Muestra la consulta del derechohabiente en general            
            ON ACTION Consulta_Derechohabiente
               -- se ejecuta la consulta del derechohabiente usando la consulta general
               LET v_comando = "fglrun ../../afi/bin/AFIC01.42r " || g_usuario_cod CLIPPED || "1 'Consulta de Derechohabiente' " || v_ar_res_unificado[ARR_CURR()].v_nsscta1
               DISPLAY v_comando
               RUN v_comando

            -- Muestra la consulta del saldo
            ON ACTION Consulta_Saldo            
                 --CALL fn_eje_consulta_saldo(v_ar_res_unificado[ARR_CURR()].v_nsscta1)
                 SELECT id_derechohabiente
                   INTO v_id_derechohabiente
                   FROM afi_derechohabiente
                   WHERE nss = v_ar_res_unificado[ARR_CURR()].v_nsscta1
            
            CALL fn_eje_consulta(1,g_usuario_cod,v_id_derechohabiente, 
                                          g_tipo_ejecucion, g_s_titulo)
           
         END DISPLAY
      
      ON ACTION regresa         
         LET v_s_reporte = ""
         EXIT DIALOG
      AFTER DIALOG
         CONTINUE DIALOG
      
      END DIALOG     
  CLOSE WINDOW win_Condetalle        
END FUNCTION

{======================================================================
Clave: UNIC01
Nombre: fn_genera_detalle_unificado
Fecha creacion: Mayo 30, 2012
Narrativa del proceso que realiza:
Muestra el detalle del unificado 
======================================================================}
FUNCTION fn_genera_detalle_unificado(p_QryTxt, p_folio, p_folio_liquida, p_nss,
                                     p_estado_solicitud, p_fec_liquida, 
                                     p_fec_notifica)
DEFINE p_folio            LIKE glo_folio.folio,
       p_folio_liquida    LIKE glo_folio.folio,
       p_nss              CHAR(11),
       p_estado_solicitud SMALLINT,
       p_fec_liquida      DATE,
       p_fec_notifica     DATE,
       p_QryTxt           STRING,
       --
       v_si_indice        INTEGER, -- indice de arreglo
       v_s_sql            STRING, -- cadena con una instruccion SQL
       v_desc_afore       CHAR(20)
       
   LET v_s_sql = "\n SELECT a.folio_unificacion, a.nsscta1, a.nombre_imsscta1, ",
                 "\n        b.estado_desc_corta, c.diag_desc_larga,a.cve_entidadcta1, ",
                 "\n        a.id_derechohabiente ", 
                 "\n  FROM uni_det_unificado a, uni_estado_solicitud b, ",
                 "\n       uni_diagnostico_sol c ",
                 "\n WHERE a.estado_unificacion = b.id_estado ",
                 "\n   AND a.diagnostico = c.id_diagnostico ",
                 p_QryTxt
       
   PREPARE Prpr_ObtDatos_unificado FROM v_s_sql CLIPPED
   DECLARE Curr_ObtDatos_unificado CURSOR FOR Prpr_ObtDatos_unificado
   LET v_si_indice = 0	
   FOREACH Curr_ObtDatos_unificado INTO v_r_res_unificado.v_folio_unificacion THRU v_r_res_unificado.v_id_derechohabiente
   
      LET v_si_indice = v_si_indice + 1
      LET v_ar_res_unificado[v_si_indice].v_folio_unificacion = v_r_res_unificado.v_folio_unificacion  
      LET v_ar_res_unificado[v_si_indice].v_nsscta1 = v_r_res_unificado.v_nsscta1            
      LET v_ar_res_unificado[v_si_indice].v_nombre = v_r_res_unificado.v_nombre
      LET v_ar_res_unificado[v_si_indice].v_estado_unificacion = v_r_res_unificado.v_estado_unificacion
      LET v_ar_res_unificado[v_si_indice].v_diagnostico = v_r_res_unificado.v_diagnostico

      --Limpia las variables del registro
      LET v_r_res_unificado.v_tpo_originacion = ""
      LET v_r_res_unificado.v_tpo_credito = ""
      LET v_r_res_unificado.v_numero_credito = NULL 

      --Consulta las descripciones de Originacion, Tipo de Credito y el Numero de Credito
      SELECT tr.originacion_desc,
             tc.desc_credito,
             ca.num_credito
        FROM cta_credito ca,
             cat_cre_originacion tr,
             cat_tipo_credito tc,
             cre_acreditado cc
       WHERE ca.id_derechohabiente = v_r_res_unificado.v_id_derechohabiente
         AND cc.tpo_originacion = tc.tpo_originacion
         AND cc.tpo_credito = ca.tpo_credito
         AND tr.tpo_originacion = tc.tpo_originacion
         AND tc.tpo_credito = ca.tpo_credito
         AND ca.id_derechohabiente = cc.id_derechohabiente

      LET v_ar_res_unificado[v_si_indice].v_tpo_originacion = v_r_res_unificado.v_tpo_originacion
      LET v_ar_res_unificado[v_si_indice].v_tpo_credito =     v_r_res_unificado.v_tpo_credito
      LET v_ar_res_unificado[v_si_indice].v_numero_credito =  v_r_res_unificado.v_numero_credito

      SELECT afore_desc
        INTO v_desc_afore
        FROM cat_afore
       WHERE afore_cod = v_r_res_unificado.v_afore_aclaracion
      
      LET v_ar_res_unificado[v_si_indice].v_afore_aclaracion = v_r_res_unificado.v_afore_aclaracion||"-"||v_desc_afore CLIPPED
   END FOREACH
   
END FUNCTION

--Función para la creación del reporte de unificados
FUNCTION genera_reporte_unificados(p_usuario_cod)
   --Define las variables para la generación del reporte
DEFINE 
      v_ind_rep1        INTEGER, --índice para el reporte
      v_ind_rep2        INTEGER, --índice para el reporte
      v_indx            INTEGER, --índice para consulta
      v_manejador_rpt   om.SaxDocumentHandler,
      v_QryTxt          STRING, --Complemento de una consulta
      v_cond_reporte    SMALLINT, --Indicador para manejar la impresión por grupos del reporte
      v_nom_reporte     VARCHAR(80), -- nombre del reporte
      r_ruta_bin        LIKE seg_modulo.ruta_bin,
      r_ruta_listados   LIKE seg_modulo.ruta_listados,
      p_usuario_cod     LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
      v_s_reporte       STRING     

DEFINE 
      v_datos_unificador_unificado DYNAMIC ARRAY OF RECORD
         --Datos del unificador
          v_id_unificador      DECIMAL(9,0),
          v_folio_unificacion  LIKE glo_folio.folio,
          v_folio_liquidacion  LIKE glo_folio.folio,
          v_nss_unificador     LIKE afi_derechohabiente.nss,
          v_nombre             CHAR(50),
          v_estado_unificacion CHAR(30),
          v_diagnostico        CHAR(30),
          v_afore_aclaracion   SMALLINT,
          v_afore_receptora    SMALLINT,
          v_estado_familia     CHAR(15),
          v_confrontacion      CHAR(15),         
         --Datos del unificado
          v_folio_unificacion_u   LIKE glo_folio.folio,
          v_nsscta1_u             LIKE afi_derechohabiente.nss,
          v_nombre_u              CHAR(50),
          v_estado_unificacion_u  CHAR(30),
          v_diagnostico_u         CHAR(30),
          v_afore_aclaracion_u    SMALLINT,
          --Bandera que indica si es uno u otro
          v_flag                 SMALLINT -- 1 es unificador y 0 es unificado
      END RECORD

      LET v_indx = TRUE  --Se asume que no hay error
      LET v_ind_rep1 = 1
      LET v_ind_rep2 = 1

       --  DISPLAY "Longitud de v_ar_res_unificador -- ",v_ar_res_unificador.getLength()
         FOR v_ind_rep1 = 1 TO  v_ar_res_unificador.getLength()

            IF v_ar_res_unificador[v_ind_rep1].v_nss_unificador IS NULL THEN 
               EXIT FOR 
            END IF 
            
            LET v_QryTxt = "\n SELECT a.id_unificador,",
                           "\n        a.folio_unificacion,",
                           "\n		  a.folio_liquidacion,",
                           "\n        a.nss_unificador,",
                           "\n		  a.nombre_imssunificador,",
                           "\n        b.estado_desc_corta,",
                           "\n		  c.diag_desc_larga,",
                           "\n        a.cve_afore_aclaracion,",
                           "\n		  a.clave_afore_receptora,",
                           "\n        b.estado_desc_corta,",
                           "\n		  d.diagnostico_uni,",
                           "\n		  d.folio_unificacion,",
                           "\n		  d.nsscta1,",
                           "\n		  d.nombre_imsscta1,",
                           "\n		  d.cve_entidadcta1,",
                           "\n		  d.diagnostico_uni",
                           "\n   FROM uni_det_unificador   a,  ",
                           "\n        uni_estado_solicitud b,",
                           "\n        uni_diagnostico_sol  c,",
                           "\n        uni_det_unificado    d",
                           "\n  WHERE a.estado_unificacion = b.id_estado",
                           "\n 	  AND a.estado_familia     = b.id_estado",  
                           "\n 	  AND a.diagnostico        = c.id_diagnostico",
                           "\n 	  AND a.folio_unificacion  = d.folio_unificacion",
                           "\n 	  AND a.id_unificador      = d.id_unificador"

               LET v_QryTxt = v_QryTxt ||  "\n AND a.nss_unificador = ", 
                              v_ar_res_unificador[v_ind_rep1].v_nss_unificador

               PREPARE prp_datos_unificador_unificado FROM v_QryTxt CLIPPED
               DECLARE curr_datos_unificador_unificado CURSOR FOR prp_datos_unificador_unificado

               FOREACH curr_datos_unificador_unificado INTO v_datos_unificador_unificado[v_ind_rep2].v_id_unificador,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_folio_unificacion,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_folio_liquidacion,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_nss_unificador,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_nombre,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_estado_unificacion,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_diagnostico,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_afore_aclaracion,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_afore_receptora,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_estado_familia,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_diagnostico_u,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_folio_unificacion_u,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_nsscta1_u,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_nombre_u,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_afore_aclaracion_u,
                                                            v_datos_unificador_unificado[v_ind_rep2].v_confrontacion
                
                  CASE v_datos_unificador_unificado[v_ind_rep2].v_confrontacion 
                  WHEN '01'
                     LET  v_datos_unificador_unificado[v_ind_rep2].v_confrontacion = "PROCEDENTE"
      	          WHEN '02'
      	             LET  v_datos_unificador_unificado[v_ind_rep2].v_confrontacion = "NO PROCEDENTE"
      	          WHEN '04'
      	             LET  v_datos_unificador_unificado[v_ind_rep2].v_confrontacion = "NO SE PRESENTO"
      	          WHEN '05'
      	             LET  v_datos_unificador_unificado[v_ind_rep2].v_confrontacion = "NO HAY ELEMENTOS"
                  END CASE	  

                  LET v_ind_rep2 = v_ind_rep2 + 1
               END FOREACH 
         END FOR 

         CALL v_datos_unificador_unificado.deleteElement(v_datos_unificador_unificado.getLength())
         
         # Recupera la ruta de listados en el que se enviara el archivo
         CALL fn_rutas("uni") RETURNING r_ruta_bin, r_ruta_listados
         
         # Se indica que el reporte usara la plantilla creada
         IF fgl_report_loadCurrentSettings("UNIC012.4rp") THEN
            CALL fgl_report_selectDevice("PDF")
            LET v_nom_reporte = p_usuario_cod CLIPPED || "-UNIC01-","00000","-","00000","-","00000"||".pdf"
            CALL fgl_report_selectPreview(0)            
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte CLIPPED)
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()      
         ELSE
            DISPLAY "NO FUE POSIBLE GENERAR EL REPORTE"
         END IF

         START REPORT rp_unificados TO XML HANDLER v_manejador_rpt
            
            FOR  v_ind_rep2 = 1 TO v_datos_unificador_unificado.getLength()
               DISPLAY "Registro --------------- ", v_ind_rep2
               DISPLAY "\n ",v_datos_unificador_unificado[v_ind_rep2].v_nss_unificador
               DISPLAY "\n ",v_datos_unificador_unificado[v_ind_rep2].v_nsscta1_u
               OUTPUT TO REPORT rp_unificados(v_datos_unificador_unificado[v_ind_rep2].*,p_usuario_cod)
            END FOR           
         FINISH REPORT rp_unificados
         
         FREE curr_datos_unificador_unificado

         LET v_s_reporte = "<a gwc:attributes=\"href resourceuri('",v_nom_reporte CLIPPED,"','unidocto')\" target='nueva'>",
                                                                               v_nom_reporte CLIPPED,"</a>"
         
         RETURN v_ind_rep2,v_s_reporte
END FUNCTION 

#OBJETIVO : Llena el arreglo para mostrar los montos totales dentro de la consulta 
FUNCTION fn_llena_totales(v_cadena_condiciones)
DEFINE v_cadena_condiciones          STRING ,                                                                                                                               
	     v_sqltxt2                   STRING ,                                                                                                                         
	     v_sqltxt                    STRING,
	     v_sqltxt3                   STRING,                                                                                                                                      
	     indx2                       INTEGER ,
	     v_tot_unificador            INTEGER ,
	     v_tot_unificado, i          INTEGER ,
	     v_tot_folio_unificacion     INTEGER ,
	     v_tot_folio_liquidacion     INTEGER ,
	     v_cadena                    STRING,
	     v_tot_folio_unificacion_liq INTEGER, 
	     v_tot_folio_liquidacion_liq INTEGER,
	     v_tot_unificador_liq        INTEGER ,
	     v_tot_unificado_liq         INTEGER,
	     v_suma_unificador           INTEGER ,
	     v_suma_unificado            INTEGER ,
	     v_tot_iafore                INTEGER,
	     v_tot_iafore_liq            INTEGER,
	     v_suma_iafore               INTEGER,
	     v_tot_eafore                INTEGER,
	     v_tot_eafore_liq            INTEGER,
	     v_suma_eafore               INTEGER,
	     indx                        INTEGER,
	     v_arr_iafore      DYNAMIC ARRAY  OF DECIMAL(9,0),
	     v_arr_eafore      DYNAMIC ARRAY  OF DECIMAL(9,0),
	     v_arr_iafore_liq  DYNAMIC ARRAY  OF DECIMAL(9,0),
	     v_arr_eafore_liq  DYNAMIC ARRAY  OF DECIMAL(9,0)
DEFINE v_arr_aux_unificador DYNAMIC ARRAY OF RECORD -- registro 
          v_id_unificador     DECIMAL(9,0),
          v_folio_unificacion  LIKE glo_folio.folio,
          v_folio_liquidacion  LIKE glo_folio.folio                       
END RECORD
DEFINE v_arr_aux_unificado DYNAMIC ARRAY OF RECORD -- registro 
			 	  v_folio_unificacion   LIKE glo_folio.folio                     
END RECORD

   LET  v_tot_unificador            =0     
   LET  v_tot_unificado             =0     
   LET  v_tot_folio_unificacion     =0     
   LET  v_tot_folio_liquidacion     =0     
   LET  v_tot_folio_unificacion_liq =0     
   LET  v_tot_folio_liquidacion_liq =0     
   LET  v_tot_unificador_liq        =0     
   LET  v_tot_unificado_liq         =0     
   LET  v_suma_unificador           =0     
   LET  v_suma_unificado            =0 
   LET  v_tot_iafore                =0
   LET  v_tot_iafore_liq            =0
   LET  v_suma_iafore               =0
   LET  v_tot_eafore                =0
   LET  v_tot_eafore                =0
   LET  v_suma_eafore               =0
   CALL v_arr_iafore.clear()                                            
   CALL v_arr_eafore.clear()                                            
   CALL v_arr_iafore_liq.clear()                                         
   CALL v_arr_eafore_liq.clear()                                         
 
   LET v_sqltxt=  "\n SELECT a.id_unificador,     ",
                  "\n        a.folio_unificacion, ",
                  "\n        a.folio_liquidacion  ",
                  "\n   FROM uni_det_unificador a  ", v_cadena_condiciones 
   
   LET indx =1
   
   DECLARE  c_tot_unificador CURSOR FROM   v_sqltxt                                                                                                      
   FOREACH  c_tot_unificador INTO v_arr_aux_unificador[indx].* 
      LET indx =indx + 1
   END FOREACH
    
   CALL v_arr_aux_unificador.deleteElement(v_arr_aux_unificador.getLength())

   ---Obtiene  totales unificadores  no  liquidados                                                                                                                        
   LET v_sqltxt="\n SELECT a.folio_unificacion,", 
                "\n        a.folio_liquidacion,",
                "\n        count (a.folio_unificacion)",
                "\n   FROM uni_det_unificador a \n",v_cadena_condiciones,
                "\n    AND a.folio_liquidacion IS NULL ",
                "\n    AND a.folio_unificacion = ",v_arr_aux_unificador[ARR_CURR()].v_folio_unificacion,
                "\n  GROUP BY folio_unificacion ,folio_liquidacion"

   PREPARE c_total FROM v_sqltxt
   EXECUTE c_total INTO v_tot_folio_unificacion, 
                        v_tot_folio_liquidacion,
                        v_tot_unificador

   ---Obtiene  totales unificados  no  liquidados
   LET v_cadena = "IN ("
      FOR i=1 TO v_arr_aux_unificador.getLength()
      	--DISPLAY "FOLIO_LIQUIDACION:",v_arr_aux_unificador[i].v_folio_liquidacion 
          IF ( v_arr_aux_unificador[i].v_folio_liquidacion ) IS NULL THEN 
              IF ( i=v_arr_aux_unificador.getLength() )THEN 
                 LET v_cadena =v_cadena ||v_arr_aux_unificador[i].v_id_unificador|| ","
              ELSE 
                 LET v_cadena = v_cadena ||v_arr_aux_unificador[i].v_id_unificador ||","
              END IF 
          END IF 
      END FOR       
   LET v_cadena = v_cadena|| "0)"

   --Obtiene el total de unificados
   LET v_sqltxt2="\n SELECT COUNT (*) ",
                 "\n   FROM uni_det_unificado",
                 "\n  WHERE id_unificador ", v_cadena
                 
--   DISPLAY v_sqltxt2

   PREPARE c_tot_unificado FROM v_sqltxt2
   EXECUTE c_tot_unificado INTO v_tot_unificado
   
   --Obtiene el total de afores receptoras
   LET v_sqltxt3= "\n SELECT COUNT( clave_afore_receptora)",
                  "\n   FROM uni_det_unificador a ",
                  "\n  WHERE a.id_unificador " ,  v_cadena , 
                  "\n  GROUP BY  clave_afore_receptora"  --cve_afore_aclaracion
   LET indx =1
   
   DECLARE  c_iafore CURSOR FROM   v_sqltxt3                                                                                                       
   FOREACH  c_iafore INTO v_arr_iafore[indx] 
   	  LET indx =indx + 1
   END FOREACH
   
   CALL v_arr_iafore.deleteElement(v_arr_iafore.getLength())   
   --DISPLAY"TOTAL DE AFORES I ", v_arr_iafore.getLength()     
   LET   v_tot_iafore= v_arr_iafore.getLength()  
    
   --Obtiene el total de afores emisoras
   LET v_sqltxt3= "\n SELECT COUNT(cve_afore_aclaracion)",
                  "\n   FROM uni_det_unificador a",
                  "\n  WHERE a.id_unificador ", v_cadena,  
                  "\n  GROUP BY cve_afore_aclaracion"
   LET indx =1
   
   DECLARE  c_eafore CURSOR FROM   v_sqltxt3                                                                                                       
   FOREACH  c_eafore INTO v_arr_eafore[indx]
      LET indx =indx + 1
   END FOREACH
   
   CALL v_arr_eafore.deleteElement(v_arr_eafore.getLength())   

   LET   v_tot_eafore= v_arr_eafore.getLength()

   --Obtiene totales unificadores liquidados                                                                                                                              
   LET v_sqltxt="\n SELECT a.folio_unificacion,", 
                "\n        a.folio_liquidacion,",
                "\n        count (a.folio_unificacion)",
                "\n   FROM uni_det_unificador a \n",v_cadena_condiciones,
                "\n    AND a.folio_liquidacion IS NOT NULL ",    
                "\n  GROUP BY folio_unificacion ,folio_liquidacion"
DISPLAY v_sqltxt
                
   PREPARE c_total_liq FROM v_sqltxt
   EXECUTE c_total_liq INTO v_tot_folio_unificacion_liq,v_tot_folio_liquidacion_liq,v_tot_unificador_liq

   LET v_cadena = "IN ("
   FOR i=1 TO v_arr_aux_unificador.getLength()
   	  IF v_arr_aux_unificador[i].v_folio_liquidacion IS NOT NULL THEN 
         IF i=v_arr_aux_unificador.getLength() THEN 
            LET v_cadena =v_cadena ||v_arr_aux_unificador[i].v_id_unificador|| ","
         ELSE 
            LET v_cadena = v_cadena ||v_arr_aux_unificador[i].v_id_unificador ||","
         END IF 
      END IF 
   END FOR 
   
   LET v_cadena = v_cadena|| "0)"

   --Obtiene total de unificados
   LET v_sqltxt2="\n SELECT COUNT (*) ",
                 "\n   FROM uni_det_unificado ",
                 "\n  WHERE id_unificador " , v_cadena

   PREPARE c_tot_unificado_liq FROM v_sqltxt2
   EXECUTE c_tot_unificado_liq INTO v_tot_unificado_liq


   LET v_sqltxt3= "\n SELECT COUNT(clave_afore_receptora)",
                  "\n   FROM uni_det_unificador a ",
                  "\n  WHERE a.id_unificador ", v_cadena

--   DISPLAY v_sqltxt3
                  
   LET indx = 1
   
   DECLARE  c_iafore_liq CURSOR FROM v_sqltxt3                                                                                                       
   FOREACH  c_iafore_liq INTO v_arr_iafore_liq[indx]    
   	  LET indx =indx + 1
   END FOREACH
   
   IF v_arr_iafore_liq[indx]    = 0 THEN 
      CALL v_arr_iafore_liq.deleteElement(v_arr_iafore_liq.getLength())
   END IF
   
   CALL v_arr_iafore_liq.deleteElement(v_arr_iafore_liq.getLength())
   -- DISPLAY"tOTAL DE AFORES I ", v_arr_iafore_liq.getLength()  
   LET v_tot_iafore_liq= v_arr_iafore_liq.getLength()  
    
   LET v_sqltxt3= "\n SELECT COUNT(cve_afore_aclaracion)",
                  "\n   FROM uni_det_unificador a ",
                  "\n  WHERE a.id_unificador " , v_cadena,
                  "\n  GROUP  BY  cve_afore_aclaracion"
    LET indx =1
   DECLARE  c_eafore_liq CURSOR FROM   v_sqltxt3                                                                                                       
   FOREACH  c_eafore_liq INTO v_arr_eafore_liq[indx] 
   	  LET indx =indx + 1
   END FOREACH
   
   CALL v_arr_eafore_liq.deleteElement(v_arr_eafore_liq.getLength())
   --DISPLAY"tOTAL DE AFORES I ", v_arr_eafore_liq.getLength()  
   LET   v_tot_eafore_liq= v_arr_eafore_liq.getLength()     
      
   LET v_suma_unificador = v_tot_unificador_liq + v_tot_unificador
   LET v_suma_unificado = v_tot_unificado_liq + v_tot_unificado   
   LET v_suma_iafore= v_tot_iafore + v_tot_iafore_liq
   LET v_suma_eafore= v_tot_eafore + v_tot_eafore_liq

 DISPLAY BY NAME  v_tot_folio_unificacion_liq,v_tot_folio_liquidacion_liq,v_tot_unificador_liq,
                  v_tot_unificado_liq,v_suma_unificado,v_suma_unificador,v_tot_eafore_liq,
                  v_tot_iafore_liq ,v_suma_eafore,v_suma_iafore,v_tot_folio_unificacion,
                  v_tot_folio_liquidacion,v_tot_unificador,v_tot_unificado,v_tot_iafore,v_tot_eafore      
END FUNCTION         


#OBJETIVO: Generar el reporte de unificados
REPORT rp_unificados(v_datos_unificador_unificado,
                     p_usuario_cod)
DEFINE v_datos_unificador_unificado RECORD
      --Datos del unificador
       v_id_unificador        DECIMAL(9,0),
       v_folio_unificacion    LIKE glo_folio.folio,
       v_folio_liquidacion    LIKE glo_folio.folio,
       v_nss_unificador       LIKE afi_derechohabiente.nss,
       v_nombre               CHAR(50),
       v_estado_unificacion   CHAR(30),
       v_diagnostico          CHAR(30),
       v_afore_aclaracion     SMALLINT,
       v_afore_receptora      SMALLINT,
       v_estado_familia       CHAR(15),
       v_confrontacion        CHAR(15),
       
      --Datos del unificado
       v_folio_unificacion_u  LIKE glo_folio.folio,
       v_nsscta1_u            LIKE afi_derechohabiente.nss,
       v_nombre_u             CHAR(50),
       v_estado_unificacion_u CHAR(30),
       v_diagnostico_u        CHAR(30),
       v_afore_aclaracion_u   SMALLINT,

       --Bandera que indica si es uno u otro
       v_flag                 SMALLINT -- 1 es unificador y 0 es unificado
END RECORD
   
DEFINE r_fecha_reporte  DATE,
       p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado

   FORMAT 
      FIRST PAGE HEADER
         LET r_fecha_reporte = TODAY CLIPPED
         PRINTX r_fecha_reporte USING "dd-mm-yyyy"
         PRINTX p_usuario_cod

      ON EVERY ROW
         --Información de unificadores/unificados
         PRINTX   v_datos_unificador_unificado.*
END REPORT 

#OBJETIVO: Obtener las condiciones que se utilizaran al ejecutar una consulta 
FUNCTION fn_arma_condiciones(v_usuario_cod,
                             v_folio,
                             v_folio_liquida,
                             v_nss, 
                             v_estado_solicitud, 
                             v_fec_liquida,
                             v_fec_notifica)
                             
DEFINE v_usuario_cod        LIKE seg_usuario.usuario_cod,
    	 v_folio              LIKE glo_folio.folio,
     	 v_folio_liquida      LIKE glo_folio.folio,
    	 v_nss                CHAR(11),
     	 v_estado_solicitud   SMALLINT,
     	 v_fec_liquida        DATE,
     	 v_fec_notifica       DATE,
     	 v_cadena_condiciones STRING     	
   
   --Asigna valor nulo cuando no se selecciona ningún folio    	
   IF ( v_folio = -1 ) THEN 
   	  LET v_folio = NULL
   END IF 
   
   --Asigna valor nulo cuando no se selecciona ningún valor de la solicitud
   IF ( v_estado_solicitud = -1 ) THEN 
      LET v_estado_solicitud = NULL
   END IF
 
   LET v_cadena_condiciones =" WHERE 1=1 "

   --Concatena folio unificación
   IF ( v_folio IS NOT NULL ) THEN
      LET v_cadena_condiciones = v_cadena_condiciones ||" AND a.folio_unificacion="||v_folio CLIPPED
   END IF
                         
   --Concatena folio liquidación
   IF ( v_folio_liquida ) IS NOT NULL THEN
      LET v_cadena_condiciones = v_cadena_condiciones  ||" AND a.folio_liquidacion="||v_folio_liquida CLIPPED
   END IF
   
   --Concatena el NSS
   IF ( v_nss IS NOT NULL ) THEN
      LET v_cadena_condiciones = v_cadena_condiciones||" AND a.nss_unificador="||v_nss CLIPPED
   END IF
   
   --Concatena el Estado de la solicitud (Aceptadas, Rechazadas, Pendientes)
   IF ( v_estado_solicitud IS NOT NULL ) THEN
      LET v_cadena_condiciones = v_cadena_condiciones||" AND a.estado_unificacion="||v_estado_solicitud CLIPPED
   END IF
   
   --Concatena la fecha de liquidación 
   IF ( v_fec_liquida IS NOT NULL ) THEN
      LET v_cadena_condiciones = v_cadena_condiciones||" AND a.f_liquidacion="||v_fec_liquida CLIPPED
   END IF
   
   --Concatena la fecha de notificación
   IF ( v_fec_notifica IS NOT NULL ) THEN
      LET v_cadena_condiciones = v_cadena_condiciones||" AND a.f_notificacion="||v_fec_notifica CLIPPED
   END IF
     	
   RETURN v_cadena_condiciones 
   
END FUNCTION   