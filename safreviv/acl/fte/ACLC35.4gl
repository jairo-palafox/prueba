-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGC35                                                                
-- Objetivo      => Consulta Detalle registros archivo especial (cta_especial_acl)
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 21 de Diciembre de 2018.
-- Requerimiento =>
-----------------------------------------------------------------------------------------
-- Modificación =>
-- Fecha        =>
-- Autor        =>
-- Clave cambio =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "ACLG02.4gl"

GLOBALS
DEFINE g_folio_param   LIKE glo_folio.folio,
       g_id_referencia LIKE cta_his_pagos.id_referencia
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
   -- parametros optativos
   LET g_folio_param    = ARG_VAL(4)
   LET g_id_referencia  = ARG_VAL(5)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- Feb13, 2013. Se verifica si LQINFO esta ejecutando
--   IF ( f_existe_proceso_operacion_ejecutando(g_proceso_cod_acl_reg_pag_sin_cambio, NULL) ) THEN
--      -- alguna operacion de LQINFO se encuentra ejecutando, por lo que no se permite realizar la consulta
--      CALL fn_mensaje("Atención","Un proceso de LQINFO se encuentra ejecutando en este momento.\nEspere a que finalice.","information")
--   ELSE
      -- consulta de informacion recibida LQINFO
      CALL fn_consulta_registros(p_usuario_cod)
--   END IF

END MAIN

FUNCTION fn_consulta_registros(p_usuario_cod)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       v_registros              INTEGER,      
       cve_origen_archivo       LIKE cta_his_pagos.origen_archivo,
       cve_tipo_aclaracion      LIKE pag_tpo_aclaracion.aclaracion_cod,
       v_query                  STRING,
       v_nss                    LIKE afi_derechohabiente.nss,
       v_id_derechohabiente     LIKE cta_his_pagos.id_derechohabiente,
       v_id_derechohabiente_nvo LIKE cta_his_pagos.id_derechohabiente, -- derechohabiente nuevo. ACL con cambio de nss
       v_indice                 INTEGER,
       v_indice_aux             INTEGER,
       v_ventana                ui.WINDOW,
       v_periodo_pago           CHAR (6) ,
       arr_registros            DYNAMIC ARRAY OF RECORD
       	    nss                      LIKE afi_derechohabiente.nss,
            desc_origen_archivo      LIKE pag_tpo_archivo.archivo_descripcion,
--            id_referencia            LIKE cta_his_pagos.id_referencia,
            folio                    LIKE cta_his_pagos.folio,
            nrp                      LIKE cta_his_pagos.nrp,
            periodo_pago             LIKE cta_his_pagos.periodo_pago,
            folio_sua                LIKE cta_his_pagos.folio_sua,
            f_pago                   LIKE cta_his_pagos.f_pago,
            id_derechohabiente       LIKE cta_his_pagos.id_derechohabiente,
            localiza_trabajador      LIKE pag_localiza_trabajador.localiza_desc,
            cve_entidad_receptora    LIKE cta_his_pagos.cve_ent_receptora,
            destino_ap_viv           LIKE cta_pag_complemento.destino_ap_viv,
            desc_tipo_aclaracion     LIKE pag_tpo_aclaracion.aclaracion_descripcion,
            imp_ap_pat               LIKE cta_his_pagos.imp_ap_pat,
            imp_am_cre               LIKE cta_his_pagos.imp_am_cre,
            aiv_ap_pat               LIKE cta_his_pagos.aiv_ap_pat, 
            valor_aiv                LIKE cta_his_pagos.valor_aiv,
            int_gen_pgo_ext          LIKE cta_his_pagos.int_gen_pgo_ext,
            aiv_gen_pgo_ext          LIKE cta_his_pagos.aiv_gen_pgo_ext,
            ind_liquidacion          CHAR (200),
            result_operacion         CHAR (200),       
            nss_nuevo                LIKE afi_derechohabiente.nss
       END RECORD,
       v_c_localiza_cod              LIKE pag_localiza_trabajador.localiza_cod
       DEFINE v_arr_curr           INTEGER
     
       
   OPEN WINDOW w_consulta_registros WITH FORM "../../acl/bin/ACLC351"
       LET  v_ventana = UI.WINDOW.GETCURRENT()
       CALL v_ventana.SETTEXT("Consulta detalle de pago")

   -- si no se recibio folio e id_referencia como parametros se captura normalmente
   IF ( g_folio_param IS NULL AND g_id_referencia IS NULL ) THEN
      INPUT v_nss
      FROM  ed_nss
      ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   
         ON ACTION ACCEPT

            --modificación de validación de  captura de parametros
            --valida que se  el NSS no sea nulo
            IF (v_nss IS NULL) THEN -- OR (v_folio IS NULL) THEN 
         	   CALL fn_mensaje("Consulta",
                               "Debe de ingresar un NSS",
                               "about")
               NEXT FIELD ed_nss         	 
            ELSE
               LET v_query = "SELECT id_derechohabiente \n",
                             "FROM   afi_derechohabiente \n",
                             "WHERE  nss = '",v_nss,"'"
               --DISPLAY v_query 
               PREPARE prp_id_derechohab FROM v_query
               EXECUTE prp_id_derechohab INTO v_id_derechohabiente
               IF v_id_derechohabiente IS NULL OR 
                  v_id_derechohabiente = 0 THEN
                  LET INT_FLAG = TRUE
                  CALL fn_mensaje("Consulta",
                               "No existen registros para el NSS ingresado",
                               "about")
                  CONTINUE INPUT
               ELSE
                  LET INT_FLAG = FALSE
                  EXIT INPUT
               END IF
            END IF 
         
         
          ON ACTION CANCEL
             LET INT_FLAG = TRUE                           
             EXIT INPUT
      END INPUT
   ELSE
      -- se recibieron los parametros
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   cta_especial_acl
      WHERE  folio = g_folio_param
      AND    id_referencia = g_id_referencia

      SELECT nss
        INTO v_nss
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_id_derechohabiente
      
      LET INT_FLAG = FALSE
   END IF

    IF NOT INT_FLAG THEN

      --hace el conteo de registros
      LET v_query = "SELECT COUNT(*) ",
                    "  FROM cta_especial_acl a ",
                    " WHERE ",
                    "id_derechohabiente = ? "
      DISPLAY " v_query  = ",v_query
      PREPARE prp_count_registros FROM v_query
      EXECUTE prp_count_registros USING v_id_derechohabiente 
                                   INTO v_registros

      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF

      --valida que se econtrarón registros
      IF v_registros > 0 THEN
        --realizala busqueda para llenar el arreglo
        LET v_query ="   SELECT a.origen_archivo        ,",
                     "\n        b.archivo_descripcion   ,",  
                     "\n        a.folio                 ,", 
                     "\n        a.nrp                   ,",
                     "\n        a.periodo_pago          ,",
                     "\n        fn_bimestre_pago (a.periodo_pago),",
                     "\n        a.folio_sua             ,",
                     "\n        a.f_pago                ,", 
                     "\n        a.id_derechohabiente    ,",
                     "\n        a.localiza_trabajador   ,", 
                     "\n        a.cve_ent_receptora     ,",
                     "\n        d.destino_ap_viv        ,",
                     "\n        a.tpo_aclaracion        ,",
                     "\n        c.aclaracion_descripcion,",
                     "\n        a.imp_ap_pat            ,",
                     "\n        a.imp_am_cre            ,",
                     "\n        a.aiv_ap_pat            ,",
                     "\n        a.valor_aiv             ,",
                     "\n        a.int_gen_pgo_ext       ,",
                     "\n        a.aiv_gen_pgo_ext       ,",
                     "\n        a.ind_liquidacion       ,",
                     "\n        a.result_operacion      ,",                     
                     "\n        d.id_derhab_nuevo        ",
                     "\n  FROM  cta_especial_acl a, pag_tpo_archivo b, cta_pag_complemento d, OUTER pag_tpo_aclaracion c ",
                     "\n WHERE  a.origen_archivo = b.archivo_cod           ",
                     "\n   AND  a.tpo_aclaracion = c.aclaracion_cod        ",
                     "\n   AND  d.id_derechohabiente = a.id_derechohabiente",
                     "\n   AND  a.folio = d.folio " ,
                     "\n   AND  a.id_referencia = d.id_referencia      " ,
                     "\n   AND  a.id_derechohabiente = ",v_id_derechohabiente

        -- si se recibio el folio e id_referencia como parametros se toman
        IF ( g_folio_param IS NOT NULL AND g_id_referencia IS NOT NULL ) THEN
           LET v_query = v_query,
                         "   AND  a.folio = ", g_folio_param, 
                         "   AND  a.id_referencia = ", g_id_referencia
        END IF
                     
        DISPLAY "@QUERY: ",v_query
        PREPARE prp_registros FROM v_query
        DECLARE cur_registros CURSOR FOR prp_registros

        LET v_indice = 1
        --llen ael arreglo
        LET v_indice_aux = 1
        FOREACH cur_registros INTO  cve_origen_archivo,
                                    arr_registros[v_indice].desc_origen_archivo,                                    
                                    arr_registros[v_indice].folio,
                                    arr_registros[v_indice].nrp,     
                                    arr_registros[v_indice].periodo_pago, 
                                    v_periodo_pago,
                                    arr_registros[v_indice].folio_sua,          
                                    arr_registros[v_indice].f_pago,              
                                    arr_registros[v_indice].id_derechohabiente, 
                                    v_c_localiza_cod ,                                    
                                    arr_registros[v_indice].cve_entidad_receptora,
                                    arr_registros[v_indice].destino_ap_viv,  
                                    cve_tipo_aclaracion ,
                                    arr_registros[v_indice].desc_tipo_aclaracion ,
                                    arr_registros[v_indice].imp_ap_pat,           
                                    arr_registros[v_indice].imp_am_cre,           
                                    arr_registros[v_indice].aiv_ap_pat,           
                                    arr_registros[v_indice].valor_aiv,            
                                    arr_registros[v_indice].int_gen_pgo_ext,      
                                    arr_registros[v_indice].aiv_gen_pgo_ext,
                                    arr_registros[v_indice].ind_liquidacion,
                                    arr_registros[v_indice].result_operacion,
                                    v_id_derechohabiente_nvo
                                                   
             --Se EL NSS obtiene de la tabla afi_derechohabiente.       
             SELECT nss
               INTO arr_registros[v_indice].nss
               FROM afi_derechohabiente
              WHERE id_derechohabiente  = arr_registros[v_indice].id_derechohabiente
              
             -- cuando se tiene derechohabiente nuevo, se obtiene su nss
             IF ( v_id_derechohabiente_nvo IS NOT NULL ) THEN
                SELECT nss
                INTO   arr_registros[v_indice].nss_nuevo
                FROM   afi_derechohabiente
                WHERE  id_derechohabiente = v_id_derechohabiente_nvo
             ELSE
                LET arr_registros[v_indice].nss_nuevo = NULL
             END IF
              
             SELECT localiza_desc 
               INTO arr_registros[v_indice].localiza_trabajador
               FROM pag_localiza_trabajador
              WHERE localiza_cod = v_c_localiza_cod
              
              
            LET arr_registros[v_indice].desc_origen_archivo = cve_origen_archivo, " - ", arr_registros[v_indice].desc_origen_archivo
            LET arr_registros[v_indice].desc_tipo_aclaracion = cve_tipo_aclaracion, " - ", arr_registros[v_indice].desc_tipo_aclaracion

            -- Si el origen archivo no es de Solo Infonavit se hace la conversion en bimestre
            -- si el origen archivo es de solo infonavit no se hace la conversion
            IF cve_origen_archivo <> 3 THEN 
                LET arr_registros[v_indice].periodo_pago = v_periodo_pago
            END IF
            
            IF arr_registros [v_indice].result_operacion CLIPPED = 1 THEN
               LET arr_registros [v_indice].result_operacion = "Aceptado"
            END IF
            IF arr_registros [v_indice].result_operacion CLIPPED = 2 THEN
               LET arr_registros [v_indice].result_operacion = "Rechazado"
            END IF
            
            -- se agrega descripcion de la ind_liquidacion
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 0 THEN 
               LET arr_registros [v_indice].ind_liquidacion = "Aportación Normal"
            END IF
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 1 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Aportación ACL Sin Liquidar"
            END IF
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 2 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Aportación ACL Sólo Infonavit Adelantada"
            END IF            
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 3 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Aportación ACL 13 ó 17 Adelantada"
            END IF            
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 4 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Aportación Conciliada con PROCESAR"
            END IF            
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 5 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Salida Normal del ACL"
            END IF            
            IF arr_registros [v_indice].ind_liquidacion CLIPPED = 6 THEN 
                LET arr_registros [v_indice].ind_liquidacion = "Aportación Salida Manual"
            END IF            

            LET v_indice = v_indice+ 1
        END FOREACH
        
        --elinina ultimo renglon en blanco
        LET v_indice = v_indice - 1
        IF arr_registros[arr_registros.getLength()].folio IS NULL THEN
            CALL arr_registros.deleteElement(arr_registros.getLength())
        END IF   

        IF (arr_registros[1].folio IS NULL OR arr_registros[1].folio = 0) THEN
          CALL fn_mensaje("Consulta",
                    "No se obtuvieron resultados con el NSS ingresado.",
                    "about") 
        ELSE 
          DIALOG  ATTRIBUTE(UNBUFFERED)
        
            DISPLAY ARRAY arr_registros TO tbl_registros.*
               BEFORE DISPLAY
                  DISPLAY v_nss TO ed_nss
               
               --boton de datos complementarios
               ON ACTION datos_complementarios
                  CALL ARR_CURR( ) RETURNING v_arr_curr                                                          	   
                  --se invoca a funcón que despliega en pantalla los datos complementarios de la consulta   
                  DISPLAY "@ seleccionado arr_registros[v_arr_curr].*: ", arr_registros[v_arr_curr].*
                  CALL fn_datos_complementarios (cve_origen_archivo, arr_registros[v_arr_curr].*)

            END DISPLAY

        ON ACTION cancelar
            EXIT DIALOG

          END DIALOG
        END IF 
      ELSE
        CALL fn_mensaje("Consulta",
                        "No existen registros con los criterios dados.",
                        "about")   
      END IF

   END IF 
    
   CLOSE WINDOW w_consulta_registros

END FUNCTION


FUNCTION fn_datos_complementarios(p_origen_archivo, arr_registros )
DEFINE p_origen_archivo          SMALLINT,
       arr_registros             RECORD    
  	        nss                      LIKE afi_derechohabiente.nss,
            desc_origen_archivo      LIKE pag_tpo_archivo.archivo_descripcion,
            folio                    LIKE cta_his_pagos.folio,
            nrp                      LIKE cta_his_pagos.nrp,
            periodo_pago             LIKE cta_his_pagos.periodo_pago,
            folio_sua                LIKE cta_his_pagos.folio_sua,
            f_pago                   LIKE cta_his_pagos.f_pago,
            id_derechohabiente       LIKE cta_his_pagos.id_derechohabiente,
            localiza_trabajador      LIKE pag_localiza_trabajador.localiza_desc,
            cve_entidad_receptora    LIKE cta_his_pagos.cve_ent_receptora,
            destino_ap_viv           LIKE cta_pag_complemento.destino_ap_viv,
            desc_tipo_aclaracion     LIKE pag_tpo_aclaracion.aclaracion_descripcion,
            imp_ap_pat               LIKE cta_his_pagos.imp_ap_pat,
            imp_am_cre               LIKE cta_his_pagos.imp_am_cre,
            aiv_ap_pat               LIKE cta_his_pagos.aiv_ap_pat, 
            valor_aiv                LIKE cta_his_pagos.valor_aiv,
            int_gen_pgo_ext          LIKE cta_his_pagos.int_gen_pgo_ext,
            aiv_gen_pgo_ext          LIKE cta_his_pagos.aiv_gen_pgo_ext,
            ind_liquidacion          CHAR (200),
            result_operacion         CHAR (200),            
            nss_nuevo                LIKE afi_derechohabiente.nss
       END RECORD,
       v_ruta_reporte            STRING ,-- ruta del archivo del reporte
       v_ruta_listados           STRING ,-- ruta de los listados
       v_ruta_ejecutable         STRING, -- ruta del ejecutable     
       v_ventana                 ui.WINDOW,
       manejador_rpt            om.SaxDocumentHandler,
       v_desc_destino_ap_viv      STRING
       
       
     LET INT_FLAG = 0
       
     --se abre ventana de datos complemmentarios 
     OPEN WINDOW w_complementarios WITH FORM "../../acl/bin/ACLC352"
       LET  v_ventana = UI.WINDOW.GETCURRENT()
       CALL v_ventana.SETTEXT("Datos complementarios")
     
     MENU ""
        BEFORE MENU
           DISPLAY  arr_registros.cve_entidad_receptora TO cve_entidad_receptora

           -- se muestra la descripcion del destino extraído desde cta_pag_complemento
           LET v_desc_destino_ap_viv = ''
           
           IF arr_registros.destino_ap_viv = 1 THEN
             LET v_desc_destino_ap_viv = "INFONAVIT"
             DISPLAY v_desc_destino_ap_viv       TO v_desc_destino_ap_viv
           END IF 
           
           IF arr_registros.destino_ap_viv = 2 THEN
             LET v_desc_destino_ap_viv = "AFORE"
             DISPLAY v_desc_destino_ap_viv       TO v_desc_destino_ap_viv
           END IF 

           DISPLAY  arr_registros.desc_tipo_aclaracion  TO tpo_aclaracion       
           DISPLAY  arr_registros.imp_ap_pat            TO imp_ap_pat           
           DISPLAY  arr_registros.imp_am_cre            TO imp_am_cre           

           DISPLAY  arr_registros.aiv_ap_pat            TO aiv_ap_pat           
           DISPLAY  arr_registros.valor_aiv             TO valor_aiv            
           DISPLAY  arr_registros.int_gen_pgo_ext       TO int_gen_pgo_ext     
           DISPLAY  arr_registros.aiv_gen_pgo_ext       TO aiv_gen_pgo_ext      
        
        ON ACTION reporte
           LET INT_FLAG = 1
           EXIT MENU 
        
        ON ACTION CANCEL 
          LET INT_FLAG = 0
          EXIT MENU
     END MENU 

     CLOSE WINDOW w_complementarios
   IF(INT_FLAG = 1)THEN  
      # Recupera la ruta de listados en el que se enviara el archivo
      CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
      --Se asigna la plantilla para generar el reporte
      IF fgl_report_loadCurrentSettings("../../acl/bin/ACLC35.4rp") THEN 
          CALL fgl_report_selectDevice ("PDF")
                      
          LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","consulta_detalle_pagos"                
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
          CALL fgl_report_selectPreview(1)
          LET manejador_rpt = fgl_report_commitCurrentSettings()
      ELSE         
          DISPLAY "No fue posible generar el reporte. No se encuentra la plantilla ACLC35.4rp"
          EXIT PROGRAM 
      END IF   
      --Inicia el reporte de registros con rechazo
      START REPORT rpt_detalle_pagos TO XML HANDLER manejador_rpt
      
      -- Asigna el titulo del reporte
      OUTPUT TO REPORT rpt_detalle_pagos(arr_registros.*, v_desc_destino_ap_viv, arr_registros.folio, arr_registros.desc_origen_archivo, p_origen_archivo)
     
      FINISH REPORT rpt_detalle_pagos 
   END IF
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_detalle_pagos(arr_registros, p_desc_destino_ap_viv, p_folio_titulo, p_desc_origen_archivo, p_origen_archivo)
  DEFINE arr_registros         RECORD    
  	        nss                      LIKE afi_derechohabiente.nss,
            desc_origen_archivo      LIKE pag_tpo_archivo.archivo_descripcion,
            folio                    LIKE cta_his_pagos.folio,
            nrp                      LIKE cta_his_pagos.nrp,
            periodo_pago             LIKE cta_his_pagos.periodo_pago,
            folio_sua                LIKE cta_his_pagos.folio_sua,
            f_pago                   LIKE cta_his_pagos.f_pago,
            id_derechohabiente       LIKE cta_his_pagos.id_derechohabiente,
            localiza_trabajador      LIKE pag_localiza_trabajador.localiza_desc,
            cve_entidad_receptora    LIKE cta_his_pagos.cve_ent_receptora,
            destino_ap_viv           LIKE cta_pag_complemento.destino_ap_viv,
            desc_tipo_aclaracion     LIKE pag_tpo_aclaracion.aclaracion_descripcion,
            imp_ap_pat               LIKE cta_his_pagos.imp_ap_pat,
            imp_am_cre               LIKE cta_his_pagos.imp_am_cre,
            aiv_ap_pat               LIKE cta_his_pagos.aiv_ap_pat, 
            valor_aiv                LIKE cta_his_pagos.valor_aiv,
            int_gen_pgo_ext          LIKE cta_his_pagos.int_gen_pgo_ext,
            aiv_gen_pgo_ext          LIKE cta_his_pagos.aiv_gen_pgo_ext,
            ind_liquidacion          CHAR (200),
            result_operacion         CHAR (200),            
            nss_nuevo                LIKE afi_derechohabiente.nss
       END RECORD,
        p_origen_archivo          SMALLINT,
        v_fecha_reporte           DATE,
        p_desc_destino_ap_viv     STRING,
        -- variables para conformar el nombre de los NSSs dados
        v_nombre                  VARCHAR(40),
        v_apellido_paterno        VARCHAR(40),
        v_apellido_materno        VARCHAR(40),
        v_nombre_nss              STRING,
        v_nombre_nss_nuevo        STRING,
        p_folio_titulo            DECIMAL(9,0),
        p_desc_origen_archivo     STRING

        
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                                                                                   
      PRINTX p_folio_titulo, p_desc_origen_archivo
      
   ON EVERY ROW
      -- se obtiene el nombre del NSS
      SELECT nombre_af,
             ap_paterno_af,
             ap_materno_af
      INTO v_nombre,
           v_apellido_paterno,
           v_apellido_materno
      FROM afi_derechohabiente
      WHERE nss = arr_registros.nss

      -- se conforma el nombre del NSS
      LET v_nombre_nss = v_nombre CLIPPED, " ", v_apellido_paterno CLIPPED, " ", v_apellido_materno CLIPPED

      -- si fue cambio de nss
      IF ( p_origen_archivo = 6 ) THEN
         SELECT nombre_af,
                ap_paterno_af,
                ap_materno_af
         INTO v_nombre,
              v_apellido_paterno,
              v_apellido_materno
         FROM afi_derechohabiente
         WHERE nss = arr_registros.nss_nuevo

         -- se conforma el nombre del NSS
         LET v_nombre_nss_nuevo = v_nombre CLIPPED, " ", v_apellido_paterno CLIPPED, " ", v_apellido_materno CLIPPED
      ELSE
         IF ( p_origen_archivo = 7 ) THEN
            -- el nombre nuevo es el que aparece en afi_derechohabiente
            LET v_nombre_nss_nuevo = v_nombre_nss
            
            -- el nombre original se obtiene de la tabla historica
            SELECT nombre_af,
                   ap_paterno_af,
                   ap_materno_af
            INTO v_nombre,
                 v_apellido_paterno,
                 v_apellido_materno
            FROM afi_his_derechohabiente
            WHERE id_derechohabiente = arr_registros.id_derechohabiente
            AND   folio_lote_modifica = arr_registros.folio 

            -- se conforma el nombre del NSS
            LET v_nombre_nss = v_nombre CLIPPED, " ", v_apellido_paterno CLIPPED, " ", v_apellido_materno CLIPPED
         END IF
            
      END IF
   
      PRINTX arr_registros.*
      PRINTX p_desc_destino_ap_viv,
             v_nombre_nss,
             v_nombre_nss_nuevo
                                                                                           
END REPORT  
