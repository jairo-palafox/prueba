--===============================================================
-- Version: 1.1.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          => CONSULTA PARA SELECCIONAR ACLARACIÓN                    #
#Fecha Inicio      =>                                                         #
#Modificacion      => se agrega archivo globales de aclaratorio y se sustitu- #
#                     yen las variables correspondientes; hilda rivas         #
###############################################################################

DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

DEFINE
    --g_pid                 DECIMAL(9,0),
    --g_proceso_cod         SMALLINT,
    --g_opera_cod           SMALLINT,
    g_usuario_cod         LIKE seg_usuario.usuario_cod
MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se asigna el codigo de usuario en la variable global
   LET g_usuario_cod = p_usuario_cod
   
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_selec_aclara(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: ACL010
Nombre: fn_consulta_selec_aclara
Fecha creacion: Febrero 07, 2012
Autor: Francisco López
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_selec_aclara(p_usuario_cod)
DEFINE manejador_rpt         om.SaxDocumentHandler, -- Contenedor de Documentos para el reporte
       v_tit_reporte         STRING,
       v_inicia              INTEGER,
       p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       v_cbx_folios             ui.ComboBox, -- combo de afores
       v_r_glo_folio            RECORD LIKE glo_folio.*,
       v_s_cadena               STRING, -- cadena de texto
       v_condicion              STRING,
       v_query                  STRING,
        v_folio                 DECIMAL(9,0),  --LIKE cta_his_pagos.folio,   
        v_nss                   CHAR(11),  --LIKE afi_derechohabiente.nss,
        v_id_derechohabiente    DECIMAL(9,0),  --LIKE cta_his_pagos.id_derechohabiente,
        v_nrp                   CHAR(11),  --LIKE cta_his_pagos.nrp,
        v_periodo_pago          CHAR(6),  --LIKE cta_his_pagos.periodo_pago,
        v_folio_sua             DECIMAL(6,0),  --LIKE cta_his_pagos.folio_sua,
        v_f_pago                DATE, --LIKE cta_his_pagos.f_pago,
        v_ent_receptora         CHAR(3),
        v_origen_pago           SMALLINT,
        v_registros             INTEGER,
       v_indice                 INTEGER,
        arr_salida_aclara         DYNAMIC ARRAY OF RECORD
          folio                   DECIMAL(9,0),
          origen_archivo          SMALLINT,
          id_referencia           DECIMAL(9,0),
          cve_ent_receptora       CHAR(3),
          nrp                     CHAR(11),
          periodo_pago            CHAR(6),
          folio_sua               DECIMAL(6,0),
          f_pago                  DATE, 
          id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente ,
          nss                     CHAR(11),
          localiza_trabajadador   CHAR(1),
          tpo_aclaracion          CHAR(2),
          imp_ap_pat              DECIMAL(12,2),
          imp_am_cre              DECIMAL(12,2),
          imp_ren_viv_pgo_e       DECIMAL(12,2),                    
          aiv_ap_pat              DECIMAL(18,6),                    
          valor_aiv               DECIMAL(18,6),                    
          int_gen_pgo_ext         DECIMAL(12,2),                    
          aiv_gen_pgo_ext         DECIMAL(18,6),                    
          result_operacion        SMALLINT,                         
          ind_liquidacion         SMALLINT,                         
          tpo_archivo             SMALLINT,                         
          origen_pago             INTEGER,                          
          tpo_afiliacion          SMALLINT,                         
          f_actualiza             DATE                              
        END RECORD,
        v_r_salida_aclara         RECORD
          folio                   DECIMAL(9,0),
          origen_archivo          SMALLINT,
          id_referencia           DECIMAL(9,0),
          cve_ent_receptora       CHAR(3),
          nrp                     CHAR(11),
          periodo_pago            CHAR(6),
          folio_sua               DECIMAL(6,0),
          f_pago                  DATE, 
          id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente ,
          localiza_trabajadador   CHAR(1),
          tpo_aclaracion          CHAR(2),
          imp_ap_pat              DECIMAL(12,2),
          imp_am_cre              DECIMAL(12,2),
          imp_ren_viv_pgo_e       DECIMAL(12,2),                    
          aiv_ap_pat              DECIMAL(18,6),                    
          valor_aiv               DECIMAL(18,6),                    
          int_gen_pgo_ext         DECIMAL(12,2),                    
          aiv_gen_pgo_ext         DECIMAL(18,6),                    
          result_operacion        SMALLINT,                         
          ind_liquidacion         SMALLINT,                         
          tpo_archivo             SMALLINT,                         
          origen_pago             INTEGER,                          
          tpo_afiliacion          SMALLINT,                         
          f_actualiza             DATE                              
        END RECORD,

        r_ruta_bin              LIKE seg_modulo.ruta_bin,
        r_ruta_listados         LIKE seg_modulo.ruta_listados,
        v_seleccionado          INTEGER,  --indice del registro seleccionado
        v_desc_origen_pago      VARCHAR(60),  --descripción del origen de pago
        v_crea_reporte          BOOLEAN,  --bandera para validar que se cree el reporte
        r_resultado_opera       SMALLINT,  --bandera para validar que se ejecuto correctamente el store procedur de preliquidación
        v_comando               STRING,
        p_num_folio             DECIMAL(9,0),
        p_nom_archivo           CHAR(40),
        v_ruta_archivo          STRING,
        v_consecutivo           INTEGER,
        v_f_lote                LIKE acl_ctr_lote.f_lote,
        v_lote_cod              SMALLINT,
        v_cadena                STRING,
        v_band_ind_liquida      SMALLINT,
        v_folio_preliquida      DECIMAL(9,0),
        p_programa_cod        VARCHAR(10)

   OPEN WINDOW w_consulta_folio WITH FORM "ACLC101"
   
   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.folio")
   LET v_lote_cod = 1
   LET v_band_ind_liquida = 0
   
   -- se inicia el combobox en blanco
   {CALL v_cbx_folios.clear()
   
    CONSTRUCT BY NAME v_condicion 
    ON a.folio, c.nss,  a.nrp,
       a.periodo_pago, a.folio_sua, a.f_pago,
       a.cve_ent_receptora, b.origen_pago
       BEFORE CONSTRUCT
         -- se asignan los valores por omision
         LET v_folio = -1}
         
         -- se llena el arreglo de folios
         {
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                acl_sum_aclaracion b --acl_sum_aclaracion
         WHERE  a.folio = b.folio
         }
         INPUT v_nss,v_nrp,v_periodo_pago,v_folio_sua 
          FROM NSS,NRP,PERIODO_PAGO,FOLIO_SUA
          ATTRIBUTES(	UNBUFFERED )

         {
         DECLARE cur_folios CURSOR FOR
         SELECT DISTINCT a.*
         FROM   glo_folio a,
                cta_his_pagos b,
                pag_ctr_pago c
         WHERE  a.folio = b.folio
         AND    c.id_derechohabiente = b.id_derechohabiente
         }
         {
         --llena el combo con los folios
         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
         }
 
        

         {
         LET v_folio = GET_FLDBUF(folio)
         LET v_nss = GET_FLDBUF(nss)
         LET v_id_derechohabiente = GET_FLDBUF(id_derechohabiente)
         LET v_nrp = GET_FLDBUF(nrp)
         LET v_periodo_pago = GET_FLDBUF(periodo_pago)
         LET v_folio_sua = GET_FLDBUF(folio_sua)
         LET v_f_pago = GET_FLDBUF(f_pago)
         LET v_origen_pago = GET_FLDBUF(origen_pago)
         LET v_ent_receptora = GET_FLDBUF(cve_ent_receptora)         
         }
         --valida que el usauario capture los criterios de busqueda                  
           --se valida que se cuapture el NSS
           
           AFTER FIELD NSS 
           	
           	  SELECT id_derechohabiente
   	            INTO v_id_derechohabiente
   	            FROM afi_derechohabiente    	            
   	           WHERE nss = v_nss 
  
  
       --Se valida que exista el nss en la tabla afi_derechohabiente 
          --para no extraer el id_derechohabiente en NULO 
          IF (v_id_derechohabiente IS NULL)  THEN 
          	   CALL fn_mensaje("Consulta","Capture un NSS válido",
                             "about")
              NEXT FIELD NSS
          END IF 

       ON ACTION ACCEPT
       
          IF (v_nss IS NULL ) THEN 
          	   CALL fn_mensaje("Consulta","Debe capturar el NSS",
                             "about")
              NEXT FIELD NSS
          END IF                       
         
          
          --AFTER FIELD NRP
          --se valida que se cuapture el NRP 
          IF ( v_nrp IS NULL) THEN 
          	CALL fn_mensaje("Consulta","Debe capturar el NRP",
                             "about")
              NEXT FIELD NRP
          	
          END IF 
          
          --AFTER FIELD PERIODO_PAGO
          --se valida que se cuapture el Periodo de pago
          IF ( v_periodo_pago IS NULL) THEN 
          	CALL fn_mensaje("Consulta","Debe capturar el Periodo de pago",
                             "about")
              NEXT FIELD PERIODO_PAGO
          END IF
          
          --AFTER FIELD FOLIO_SUA
          --se valida que se cuapture el Folio SUA
          IF v_folio_sua IS NULL THEN 
          	CALL fn_mensaje("Consulta","Debe capturar el Folio SUA",
                             "about")
              NEXT FIELD FOLIO_SUA
          END IF
          {
          SELECT id_derechohabiente 
            INTO v_id_derechohabiente
            FROM afi_derechohabiente
           WHERE nss = v_nss
           }
          LET INT_FLAG = FALSE
          
          EXIT INPUT
         
      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT

   IF NOT INT_FLAG THEN
      --hace el conteo de registros
      LET v_query = "SELECT COUNT(*) \n",
                    " FROM cta_his_pagos a, pag_ctr_pago b \n",
                    "WHERE a.id_derechohabiente = b.id_derechohabiente \n",
                    "  AND a.id_derechohabiente = ",v_id_derechohabiente,"\n",
                    "  AND a.nrp          = '",v_nrp,"'\n",
                    "  AND a.periodo_pago = '",v_periodo_pago, "'\n",
                    "  AND a.folio_sua    = ",v_folio_sua , "\n",
                    "  AND a.ind_liquidacion = 1  "
      display " v_query "  ,v_query
      PREPARE prp_count_aclara FROM v_query
      EXECUTE prp_count_aclara INTO v_registros

      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF

      --valida que se econtrarón registros
      IF v_registros > 0 THEN
         --realizala busqueda para llenar el arreglo
         LET v_query = "SELECT a.folio,a.origen_archivo,a.id_referencia ,a.cve_ent_receptora \n", 
                     "         ,a.nrp,a.periodo_pago,a.folio_sua,a.f_pago,a.id_derechohabiente  \n",
                     "         ,a.localiza_trabajador,a.tpo_aclaracion,a.imp_ap_pat,a.imp_am_cre   \n",       
                     "         ,a.imp_ren_viv_pgo_ext,a.aiv_ap_pat,a.valor_aiv,a.int_gen_pgo_ext    \n", 
                     "         ,a.aiv_gen_pgo_ext,a.result_operacion,a.ind_liquidacion     ,  \n",
                     --"         b.tpo_archivo, b.origen_pago, b.tpo_afiliacion, b.f_actualiza \n",
                     "         0, b.estado_pago, 0, b.f_actualiza \n",
                     "    FROM cta_his_pagos a, pag_ctr_pago b  \n",
                     "   WHERE a.id_derechohabiente = b.id_derechohabiente \n",
                     "     AND a.id_derechohabiente = ", v_id_derechohabiente,"\n",
                     "     AND a.id_referencia = b.id_referencia \n",
                     "     AND a.nrp          = '",v_nrp,"'\n",
                     "     AND a.periodo_pago = '",v_periodo_pago, "'\n",
                     "     AND a.folio_sua    = ",v_folio_sua , "\n",
                     "     AND a.ind_liquidacion = 1  "
         DISPLAY v_query
         PREPARE prp_cur_folio FROM v_query
         DECLARE cur_folio CURSOR FOR prp_cur_folio
         
         LET v_indice = 1
         --DISPLAY " v_indice",v_indice
         --llen ael arreglo
         FOREACH cur_folio INTO v_r_salida_aclara.*
            LET arr_salida_aclara[v_indice].folio                 = v_r_salida_aclara.folio                
            LET arr_salida_aclara[v_indice].origen_archivo        = v_r_salida_aclara.origen_archivo       
            LET arr_salida_aclara[v_indice].id_referencia         = v_r_salida_aclara.id_referencia        
            LET arr_salida_aclara[v_indice].cve_ent_receptora     = v_r_salida_aclara.cve_ent_receptora    
            LET arr_salida_aclara[v_indice].nrp                   = v_r_salida_aclara.nrp                  
            LET arr_salida_aclara[v_indice].periodo_pago          = v_r_salida_aclara.periodo_pago         
            LET arr_salida_aclara[v_indice].folio_sua             = v_r_salida_aclara.folio_sua            
            LET arr_salida_aclara[v_indice].f_pago                = v_r_salida_aclara.f_pago               
            LET arr_salida_aclara[v_indice].id_derechohabiente    = v_r_salida_aclara.id_derechohabiente        
            LET arr_salida_aclara[v_indice].localiza_trabajadador = v_r_salida_aclara.localiza_trabajadador
            LET arr_salida_aclara[v_indice].tpo_aclaracion        = v_r_salida_aclara.tpo_aclaracion       
            LET arr_salida_aclara[v_indice].imp_ap_pat            = v_r_salida_aclara.imp_ap_pat           
            LET arr_salida_aclara[v_indice].imp_am_cre            = v_r_salida_aclara.imp_am_cre           
            LET arr_salida_aclara[v_indice].imp_ren_viv_pgo_e     = v_r_salida_aclara.imp_ren_viv_pgo_e    
            LET arr_salida_aclara[v_indice].aiv_ap_pat            = v_r_salida_aclara.aiv_ap_pat           
            LET arr_salida_aclara[v_indice].valor_aiv             = v_r_salida_aclara.valor_aiv            
            LET arr_salida_aclara[v_indice].int_gen_pgo_ext       = v_r_salida_aclara.int_gen_pgo_ext      
            LET arr_salida_aclara[v_indice].aiv_gen_pgo_ext       = v_r_salida_aclara.aiv_gen_pgo_ext      
            LET arr_salida_aclara[v_indice].result_operacion      = v_r_salida_aclara.result_operacion     
            LET arr_salida_aclara[v_indice].ind_liquidacion       = v_r_salida_aclara.ind_liquidacion      
            LET arr_salida_aclara[v_indice].tpo_archivo           = v_r_salida_aclara.tpo_archivo          
            LET arr_salida_aclara[v_indice].origen_pago           = v_r_salida_aclara.origen_pago          
            LET arr_salida_aclara[v_indice].tpo_afiliacion        = v_r_salida_aclara.tpo_afiliacion       
            LET arr_salida_aclara[v_indice].f_actualiza           = v_r_salida_aclara.f_actualiza          
         	                                                                                    
            LET arr_salida_aclara[v_indice].nss                   = v_nss    
            
            -- si tiene ind_liquidacion = 1 entonces se puede preliquidar      
            IF arr_salida_aclara[v_indice].ind_liquidacion = 1 THEN 
               LET v_band_ind_liquida = 1
            END IF
            
            -- se incrementa el indice
            LET v_indice = v_indice + 1
         END FOREACH

        -- elimina ultimo renglon en blanco
        --CALL arr_salida_aclara.deleteElement(arr_salida_aclara.getLength())

        IF (v_band_ind_liquida = 1) THEN 
           DISPLAY ARRAY arr_salida_aclara TO salida_aclara.* 
           ATTRIBUTES (ACCEPT = FALSE)

           ON ACTION CANCEL
               EXIT DISPLAY

           ON ACTION Salida_Aclaratorio 
               --Validación de que selecciono un registro
               CALL ARR_CURR( ) RETURNING v_seleccionado
             
                  LET v_crea_reporte = FALSE
                  LET v_desc_origen_pago = NULL

                  -- busca descripción del nuevo origen de pago
                  SELECT archivo_descripcion 
                  INTO   v_desc_origen_pago
                  FROM   pag_tpo_archivo
                  WHERE  archivo_cod = arr_salida_aclara[v_seleccionado].origen_archivo
           
                  -- se genera un folio nuevo para el registro que se preliquidara
                  CALL fn_genera_folio(g_proceso_cod_acl_salida_manual, g_opera_cod_carga, g_usuario_cod)
                  RETURNING v_folio_preliquida
           
                  #Llamada a ejecución de procedimiento almacenado
                  CALL fn_ejecuta_preliquidacion_salida_aclara(v_folio_preliquida                                  , -- crea un nuevo folio para el registro preliquidado
                                                               arr_salida_aclara[v_seleccionado].id_derechohabiente,
                                                               arr_salida_aclara[v_seleccionado].id_referencia     ,
                                                               arr_salida_aclara[v_seleccionado].nrp               ,
                                                               arr_salida_aclara[v_seleccionado].periodo_pago      ,
                                                               arr_salida_aclara[v_seleccionado].folio_sua         ,
                                                               arr_salida_aclara[v_seleccionado].imp_ap_pat        ,
--                                                               arr_salida_aclara[v_seleccionado].valor_aiv         ,
                                                               arr_salida_aclara[v_seleccionado].aiv_ap_pat        ,
                                                               arr_salida_aclara[v_seleccionado].imp_am_cre        ,
                                                               arr_salida_aclara[v_seleccionado].f_pago            ,
                                                               arr_salida_aclara[v_seleccionado].imp_ren_viv_pgo_e ,
                                                               arr_salida_aclara[v_seleccionado].int_gen_pgo_ext   ,
                                                               arr_salida_aclara[v_seleccionado].aiv_gen_pgo_ext   ,
                                                               arr_salida_aclara[v_seleccionado].tpo_aclaracion    ,
                                                               arr_salida_aclara[v_seleccionado].tpo_archivo       ,
                                                               arr_salida_aclara[v_seleccionado].tpo_afiliacion    ,
                                                               arr_salida_aclara[v_seleccionado].origen_pago       ,
                                                               arr_salida_aclara[v_seleccionado].folio             , -- el folio original es ahora el folio de referencia
                                                               p_usuario_cod)
                      RETURNING r_resultado_opera
                      # cambia a estado errone si no se ejecutó correctamente el SP
                      IF ( r_resultado_opera ) THEN
                          CALL fn_mensaje("Error",
                             "Ocurrio un error al efectuar la preliquidación",
                             "info")
                         LET v_crea_reporte = FALSE
                      ELSE
                      
                         LET v_f_lote = TODAY
                         SELECT NVL(MAX(consecutivo),0) + 1
                         INTO   v_consecutivo
                         FROM   acl_ctr_lote
                         WHERE  f_lote = v_f_lote
                      
                         INSERT INTO acl_ctr_lote
                         VALUES (v_lote_cod, v_f_lote, v_consecutivo, 
                                 v_folio_preliquida, 
                                 arr_salida_aclara[v_seleccionado].origen_archivo, 
                                 arr_salida_aclara[v_seleccionado].id_referencia,
                                 NULL)
                         CALL fn_mensaje("Consulta", "Se realizó correctamente la Preliquidación \n",
                                         "info")
                         EXIT DISPLAY
                      END IF
           END DISPLAY
        ELSE
        	CALL fn_mensaje("Consulta",
                        "Registro seleccionado se liquido anteriormente.",
                        "about")   
        	
        END IF 	--IF (v_band_ind_liquida = 1) THEN 
      ELSE
        CALL fn_mensaje("Consulta",
                        "No existen registros con los criterios dados.",
                        "about")   
      END IF --IF v_registros > 0 THEN
      
   --RETURN v_registros
   END IF  --IF NOT INT_FLAG THEN   
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION



-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de Integración LQINFO
FUNCTION fn_reporte_carga_archivo(p_folio, arr_salida_aclara, p_b_despliegue_pantalla,v_desc_origen_pago)
    DEFINE p_folio                 INTEGER
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_desc_origen_pago    VARCHAR(60)  --descripción del origen de pago
    DEFINE arr_salida_aclara      RECORD
          folio                   decimal(9,0), 
          origen_archivo          SMALLINT,
          id_referencia           decimal(9,0),
          cve_ent_receptora       char(3),
          nrp                     char(11),
          periodo_pago            char(6),
          folio_sua               decimal(6,0),
          f_pago                  DATE,
          id_derechohabiente      LIKE afi_derechohabiente.id_derechohabiente ,
          nss                     CHAR(11),
          localiza_trabajadador   char(1),
          tpo_aclaracion          char(2),
          imp_ap_pat              decimal(12,2),
          imp_am_cre              decimal(12,2),
          imp_ren_viv_pgo_e       decimal(12,2),
          aiv_ap_pat              decimal(18,6),
          valor_aiv               decimal(18,6),
          int_gen_pgo_ext         decimal(12,2),
          aiv_gen_pgo_ext         decimal(18,6),
          result_operacion        SMALLINT,
          ind_liquidacion         smallint,
          tpo_archivo             SMALLINT,
          origen_pago             SMALLINT,
          tpo_afiliacion          SMALLINT,
          f_actualiza             DATE
        END RECORD,

      manejador_rpt         om.SaxDocumentHandler
    
    DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
 
    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("ACLC11.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","salida_aclaratorios"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_salida_aclaratorios TO XML HANDLER manejador_rpt
    -- Asigna el titulo del reporte
                
    OUTPUT TO REPORT rpt_salida_aclaratorios(arr_salida_aclara.*, g_usuario_cod, v_desc_origen_pago)                                                                
    FINISH REPORT rpt_salida_aclaratorios 
END FUNCTION


#OBJETIVO: Genera el reporte de salida aclaratorios                                                      
REPORT rpt_salida_aclaratorios(arr_salida_aclara, p_usuario_cod, p_desc_origen_pago)
DEFINE arr_salida_aclara         RECORD
          folio                   DECIMAL(9,0), 
          origen_archivo          SMALLINT,
          id_referencia           DECIMAL(9,0),
          cve_ent_receptora       CHAR(3),
          nrp                     CHAR(11),
          periodo_pago            CHAR(6),
          folio_sua               DECIMAL(6,0),
          f_pago                  DATE,
          id_derechohabiente      DECIMAL(9,0), 
          nss                     CHAR(11),
          localiza_trabajadador   CHAR(1),
          tpo_aclaracion          CHAR(2),
          imp_ap_pat              DECIMAL(12,2),
          imp_am_cre              DECIMAL(12,2),
          imp_ren_viv_pgo_e       DECIMAL(12,2),
          aiv_ap_pat              DECIMAL(18,6),
          valor_aiv               DECIMAL(18,6),
          int_gen_pgo_ext         DECIMAL(12,2),
          aiv_gen_pgo_ext         DECIMAL(18,6),
          result_operacion        SMALLINT,
          ind_liquidacion         SMALLINT,
          tpo_archivo             SMALLINT,
          origen_pago             SMALLINT,
          tpo_afiliacion          SMALLINT,
          f_actualiza             Date
        END RECORD,
        p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
        v_fecha_reporte           DATE,
        p_desc_origen_pago        VARCHAR(60)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                  
      PRINTX arr_salida_aclara.folio                                                                              
      PRINTX p_usuario_cod    
      PRINTX arr_salida_aclara.folio  
      PRINTX p_desc_origen_pago                                                                    
                                                                                              
   ON EVERY ROW                                                                               
      PRINTX arr_salida_aclara.*
                                                                                           
END REPORT          

#Objetivo: Executa el procedimiento almacenado para realizar la preliquidación
{ ==========================================================================
Clave:  fn_ejecuta_preliquidacion_salida_aclara
Nombre: fn_ejecuta_preliquidacion_salida_aclara
Fecha creacion: 08 de Marzo de 2012
Autor: Ilhuitemoc Ricardo Ortíz
Narrativa del proceso que realiza:
 Esta función executa el store procedure que almacena la información 
 de la preliquidación para el módulo de "Salida aclaratorio"
 Parametros de Entrada:
 -
 Parámetros de salida;
 -
Registro de modificaciones:
Autor           Fecha                   Descrip. cambio
============================================================================}
FUNCTION fn_ejecuta_preliquidacion_salida_aclara(p_folio              ,
                                                 p_id_derechohabiente ,
                                                 p_id_referencia      ,
                                                 p_nrp                ,
                                                 p_periodo_pago       ,
                                                 p_folio_sua          ,
                                                 p_imp_ap_pat         ,
                                                 p_valor_aiv          ,
                                                 p_imp_am_cre         ,
                                                 p_f_pago             ,
                                                 p_imp_ren_viv_pgo_e  ,
                                                 p_int_gen_pgo_ext    ,
                                                 p_aiv_gen_pgo_ext    ,
                                                 p_tpo_aclaracion     ,
                                                 p_tpo_archivo        ,
                                                 p_tpo_afiliacion     ,
                                                 p_origen_pago        ,
                                                 p_folio_referencia   ,
                                                 p_usuario_cod)
DEFINE  p_folio                   decimal(9,0),
        p_id_derechohabiente      decimal(9,0),
        p_id_referencia           decimal(9,0),
        p_nrp                     char(11),
        p_periodo_pago            char(6),
        p_folio_sua               decimal(6,0),
        p_imp_ap_pat              decimal(12,2),
        p_valor_aiv               decimal(18,6),
        p_imp_am_cre              decimal(12,2),
        p_f_pago                  DATE,
        p_imp_ren_viv_pgo_e       decimal(12,2),
        p_int_gen_pgo_ext         decimal(12,2),
        p_aiv_gen_pgo_ext         decimal(18,6),
        p_tpo_aclaracion          SMALLINT,
        p_tpo_archivo             SMALLINT,
        p_tpo_afiliacion          SMALLINT,
        p_origen_pago             SMALLINT,
        p_folio_referencia        decimal(9,0),
        p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
        v_sql_procedure           STRING,
        v_proceso_cod             LIKE cat_proceso.proceso_cod,
        v_opera_cod               LIKE cat_operacion.opera_cod,
        v_programa_cod            VARCHAR(10)

   DEFINE v_pid     INTEGER
         ,v_estatus SMALLINT,
         p_titulo              STRING, -- titulo del mensaje enviado en el correo
         p_mensaje             STRING -- cuerpo del mensaje enviado

    DEFINE v_ruta_listados      STRING -- ruta de los listados
    DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
    DEFINE v_log                STRING
    -- variables para control de proceso
    DEFINE v_error_sql          INTEGER
    DEFINE v_error_isam         INTEGER
    DEFINE v_mensaje            VARCHAR(250)
   
   
   
   
   --WHENEVER ERROR CONTINUE
   LET v_sql_procedure = "EXECUTE PROCEDURE sp_preliquida_manual_acl(?,?,?,?,?,?,?,?,?,
                                                                     ?,?,?,?,?,?,?,?,?,?)"

   PREPARE prp_sqlPreliquidacionsalidaAclara FROM v_sql_procedure
   EXECUTE prp_sqlPreliquidacionsalidaAclara USING p_folio,
                                                   p_id_derechohabiente,
                                                   p_id_referencia,
                                                   p_nrp,
                                                   p_periodo_pago,
                                                   p_folio_sua,
                                                   p_imp_ap_pat,
                                                   p_valor_aiv,
                                                   p_imp_am_cre,
                                                   p_f_pago,
                                                   p_imp_ren_viv_pgo_e,
                                                   p_int_gen_pgo_ext,
                                                   p_aiv_gen_pgo_ext,
                                                   p_tpo_aclaracion,
                                                   p_tpo_archivo,
                                                   p_tpo_afiliacion,
                                                   p_origen_pago,
                                                   p_folio_referencia,
                                                   p_usuario_cod
      INTO v_error_sql, v_error_isam, v_mensaje
      
   IF ( v_error_sql = 0 ) THEN
      --Se genera el PID
      #checar que se pueda inicializar el proceso: 

        LET v_proceso_cod = g_proceso_cod_acl_salida_manual -- 105 
        LET v_opera_cod = g_opera_cod_carga -- 1

   
      LET v_pid = fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod)
      
      -- display "v_pid ",v_pid
      
      CALL fn_rutas('bat') RETURNING v_ruta_ejecutable,v_ruta_listados

     

      LET v_log= v_ruta_listados CLIPPED,"/nohup:",
                 v_pid USING "&&&&&",":",
                 v_proceso_cod USING "&&&&&",":",
                 v_opera_cod USING "&&&&&"
      
      --display "v_log  ",v_log

      CALL STARTLOG(v_log CLIPPED)

      --CALL errorlog()
      --ruta(bat), pid_pro, proceso, operacion

      
      --Se inicia el proceso para el monitor de procesos
      LET v_estatus =  fn_inicializa_proceso(v_pid
                                            ,v_proceso_cod
                                            ,v_opera_cod
                                            ,p_folio
                                            ,"ACLC10"
                                            ,"N/A"
                                            ,p_usuario_cod)
      CALL fn_display_proceso_error(0,"Selección aclaratorio", v_opera_cod)
      --Se marca el proceso como inciado
      LET v_estatus = fn_actualiza_opera_ini(v_pid
                                          ,v_proceso_cod
                                          ,v_opera_cod
                                          ,p_folio
                                          ,"ACLC10"
                                          ,"N/A"
                                          ,p_usuario_cod)
      CALL fn_display_proceso_error(1,"Selección aclaratorio",v_opera_cod)

      
                         -- se obtiene el codigo de programa
                         SELECT programa_cod
                         INTO   v_programa_cod
                         FROM   cat_operacion
                         WHERE  proceso_cod = v_proceso_cod
                         AND    opera_cod   = 2
                         
                         CALL fn_reporte_liquidacion(p_folio, "acl_preliquida",
                                                     g_usuario_cod, v_pid,
                                                     v_proceso_cod, 2,
                                                     v_programa_cod, FALSE)

      
      --Se marca el proceso como inciado
      LET v_estatus = fn_actualiza_opera_fin(v_pid
                                             ,v_proceso_cod
                                             ,v_opera_cod)
      LET v_opera_cod = 2
      CALL fn_rutas('bat') RETURNING v_ruta_ejecutable,v_ruta_listados

      LET v_log= v_ruta_listados CLIPPED,"/nohup:",
                 v_pid USING "&&&&&",":",
                 v_proceso_cod USING "&&&&&",":",
                 v_opera_cod USING "&&&&&"

      CALL STARTLOG(v_log CLIPPED)
      --Se marca el proceso como inciado
      CALL fn_display_proceso_error(0,"Preliquidación aclaratorio", v_opera_cod)
      LET v_estatus = fn_actualiza_opera_ini(v_pid
                                          ,v_proceso_cod
                                          ,v_opera_cod
                                          ,p_folio
                                          ,"ACLC10"
                                          ,"N/A"
                                          ,p_usuario_cod)
      --Se marca el proceso como inIciado
      LET v_estatus = fn_actualiza_opera_fin(v_pid
                                             ,v_proceso_cod
                                             ,v_opera_cod)
      CALL fn_display_proceso_error(1,"Preliquidación aclaratorio", v_opera_cod)
      DISPLAY "Se ejecuto correctamente"
      LET p_mensaje = "Selección y Preliquidación realizadas con éxito.\nYa se puede continuar con la Liquidación."
      LET p_titulo = "Finalización de operación - Salida Aclaraciones - Selección y Preliquidación"
      CALL fn_correo_proceso(v_pid, v_proceso_cod, 
                          v_opera_cod, 
                          NULL, p_titulo,p_mensaje)
      RETURN FALSE
   ELSE
      LET p_mensaje = "El proceso de Selección y Preliquidación han finalizado pero con errores.\nNo se puede continuar con el proceso de Liquidación."  
      LET p_titulo = "Finalización de operación - Salida Aclaraciones - Selección y Preliquidación"
      CALL fn_correo_proceso(v_pid, v_proceso_cod, 
                             v_opera_cod, 
                             NULL, p_titulo,p_mensaje)      DISPLAY "\nError en sp_preliquida_manual_acl (Codigo):",SQLCA.SQLCODE
      DISPLAY "Error en sp_preliquida_manual_acl (Codigo):", v_error_sql,"\n"
      DISPLAY "Error isam: ", v_error_isam
      DISPLAY "Mensaje: ", v_mensaje

      RETURN TRUE
   END IF
   
END FUNCTION

FUNCTION fn_display_proceso_error(p_inicio_fin, p_etapa, v_opera_cod)
DEFINE 
   p_inicio_fin  SMALLINT,
   p_etapa       STRING,
   v_cadena      STRING,
   v_opera_cod   SMALLINT
   
   IF v_opera_cod = 1 THEN
       IF p_inicio_fin = 0 THEN
          LET v_cadena ="INICIO ETAPA : ", p_etapa CLIPPED,"\n"
          CALL ERRORLOG(v_cadena)
          --LET v_cadena =" FECHA        : ", TODAY
          --CALL ERRORLOG(v_cadena)
          --LET v_cadena = " HORA         : ", CURRENT HOUR TO SECOND
          --CALL ERRORLOG(v_cadena)
       END IF
       
       IF p_inicio_fin = 1 THEN
          LET v_cadena = "\n FIN ETAPA    : ", p_etapa CLIPPED,"\n"
          CALL ERRORLOG(v_cadena)
          --LET v_cadena = " FECHA        : ", TODAY 
          --CALL ERRORLOG(v_cadena)
          --LET v_cadena = " HORA         : ", CURRENT HOUR TO SECOND
          --CALL ERRORLOG(v_cadena)
       END IF
    END IF
    IF v_opera_cod = 2 THEN
       IF p_inicio_fin = 0 THEN
          LET v_cadena ="INICIO ETAPA : ", p_etapa CLIPPED,"\n"
          CALL ERRORLOG(v_cadena)
          --LET v_cadena =" FECHA        : ", TODAY
          --CALL ERRORLOG(v_cadena)
          --LET v_cadena = " HORA         : ", CURRENT HOUR TO SECOND
          --CALL ERRORLOG(v_cadena)
       END IF
       
       IF p_inicio_fin = 1 THEN
          LET v_cadena = "\n FIN ETAPA    : ", p_etapa CLIPPED,"\n"
          CALL ERRORLOG(v_cadena)
          --LET v_cadena = " FECHA        : ", TODAY 
          --CALL ERRORLOG(v_cadena)
          --LET v_cadena = " HORA         : ", CURRENT HOUR TO SECOND
          --CALL ERRORLOG(v_cadena)
       END IF
    END IF
END FUNCTION