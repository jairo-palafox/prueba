--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 23 Agosto 2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGC24                                                                 #
#Objetivo     => Consulta Detalle del Pago FC                                           #
#########################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod -- clave del usuario firmado

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   -- consulta de informacion recibida LQINFO
   CALL fn_consulta_registros()

END MAIN

{ ============================================================================
Clave: PAGC19
Nombre: fn_consulta_registros
Fecha creacion: ABRIL 11, 2012
Autor: ILHUITEMOC RICARDO ORTIZ
Narrativa del proceso que realiza:
Registro de modificaciones:
Autor: Rubén Haro Castro         Fecha: 22 de Junio de 2012
Descrip. cambio: Se modifica el despliegue de información y diseño de reporte 

==============================================================================
}
FUNCTION fn_consulta_registros()
DEFINE --p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
      -- v_fecha                  DATE,
      -- v_bandera                SMALLINT,
       v_registros              INTEGER,      
      -- cve_origen_archivo       LIKE cta_his_pagos.origen_archivo,
      -- cve_tipo_aclaracion      LIKE pag_tpo_aclaracion.aclaracion_cod,
       v_query                  STRING,
      -- v_condicion              STRING,
      -- v_folio                  LIKE cta_his_pagos.folio,
       v_nss                    LIKE afi_derechohabiente.nss,
       v_id_derechohabiente     LIKE afi_derechohabiente.id_derechohabiente,
       v_indice                 INTEGER,
      -- manejador_rpt            om.SaxDocumentHandler,
       v_indice_aux             INTEGER,
       v_ventana                ui.WINDOW,
       arr_registros            DYNAMIC ARRAY OF RECORD
       	    nss                      LIKE afi_derechohabiente.nss,
            folio                    LIKE pag_det_fc.folio,
            origen_archivo           LIKE pag_det_fc.origen_archivo,
            id_referencia            LIKE pag_det_fc.id_referencia,
            f_pago                   LIKE pag_det_fc.f_pago,
            id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente,
            imp_ap_fc                LIKE pag_det_fc.imp_ap_fc,
            loc_trabajador           STRING 
       END RECORD
--       v_c_localiza_cod              LIKE pag_localiza_trabajador.localiza_cod
       
       {
       arr_registros_aux         DYNAMIC ARRAY OF RECORD
            imp_ap_pat               LIKE cta_his_pagos.imp_ap_pat,
            imp_am_cre               LIKE cta_his_pagos.imp_am_cre,
            imp_ren_viv_pgo_ext      LIKE cta_his_pagos.imp_ren_viv_pgo_ext,
            aiv_ap_pat               LIKE cta_his_pagos.aiv_ap_pat, 
            valor_aiv                LIKE cta_his_pagos.valor_aiv,
            int_gen_pgo_ext          LIKE cta_his_pagos.int_gen_pgo_ext,
            aiv_gen_pgo_ext          LIKE cta_his_pagos.aiv_gen_pgo_ext
       END RECORD
       }
      -- DEFINE v_ruta_reporte       STRING -- ruta del archivo del reporte
      -- DEFINE v_ruta_listados      STRING -- ruta de los listados
      -- DEFINE v_ruta_ejecutable    STRING -- ruta del ejecutable
       DEFINE v_arr_curr           INTEGER
   
   OPEN WINDOW w_consulta_registros WITH FORM "PAGC241"
       LET  v_ventana = UI.WINDOW.GETCURRENT()
       CALL v_ventana.SETTEXT("Consulta detalle de pago")
   
  
   INPUT v_nss
   FROM  ed_nss
   ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
   --CONSTRUCT BY NAME v_condicion 
   --ON        a.folio, a.id_derechohabiente
   
      ON ACTION ACCEPT

         --modificación de validación de  captura de parametros
         --valida que se  el NSS no sea nulo
         IF (v_nss IS NULL) THEN -- OR (v_folio IS NULL) THEN 
         	 -- IF (v_nss IS NULL) THEN 
         	     CALL fn_mensaje("Consulta",
                            "Debe de ingresar un NSS",
                            "about")
         	     NEXT FIELD ed_nss        	 
             -- END IF
            --valida que el número de folio no sea nulo 
            {IF v_folio IS NULL THEN 
            	  CALL fn_mensaje("Consulta",
                               "Debe de ingresar un Folio",
                               "about")
               NEXT FIELD ed_folio 
            END IF }	
   
         ELSE
            
            LET v_query = "SELECT a.id_derechohabiente
                             FROM afi_derechohabiente a
                            WHERE a.nss= '",v_nss,"'"
            DISPLAY v_query 
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
         
         AFTER FIELD ed_nss --ed_nss
         	--NEXT FIELD ed_nss
         	
         
       ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT INPUT
   END INPUT

    IF NOT INT_FLAG THEN     
      --hace el conteo de registros
      LET v_query = "SELECT COUNT(*) \n",
                    "  FROM afi_derechohabiente a \n",
                    " WHERE ",{folio = ",v_folio," \n",
                    "   AND } 
                    "a.id_derechohabiente = ",v_id_derechohabiente
      --DISPLAY " v_query  = ",v_query
      PREPARE prp_count_registros FROM v_query
      EXECUTE prp_count_registros INTO v_registros

      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF

      --valida que se econtrarón registros
      IF v_registros > 0 THEN
        --realizala busqueda para llenar el arreglo
        LET v_query ="\n select  a.folio                ,",
                     "\n         a.origen_archivo       ,",
                     "\n         a.id_referencia        ,",
                     "\n         a.f_pago               ,",
                     "\n         a.id_derechohabiente   ,",
                     "\n         a.imp_ap_fc            ",
                     "\n  from   pag_det_fc a            ",
                     "\n  where  a.id_derechohabiente = ",v_id_derechohabiente
                    
        DISPLAY "@QUERY: ",v_query
        PREPARE prp_registros FROM v_query
        DECLARE cur_registros CURSOR FOR prp_registros

        LET v_indice = 1
        --llen ael arreglo
        LET v_indice_aux = 1
        FOREACH cur_registros INTO  arr_registros[v_indice].folio,
                                    arr_registros[v_indice].origen_archivo,         
                                    arr_registros[v_indice].id_referencia,           
                                    arr_registros[v_indice].f_pago,              
                                    arr_registros[v_indice].id_derechohabiente, 
                                    arr_registros[v_indice].imp_ap_fc
                                LET arr_registros[v_indice].loc_trabajador = "CUENTA INDIVIDUAL" 
                                                   
             --Se obtiene EL NSS de la tabla afi_derechohabiente.       
             SELECT nss
               INTO arr_registros[v_indice].nss
               FROM afi_derechohabiente
              WHERE id_derechohabiente = arr_registros[v_indice].id_derechohabiente

            LET v_indice = v_indice+ 1
        END FOREACH
        
        --elimina ultimo renglon en blanco
        LET v_indice = v_indice - 1
        IF arr_registros[arr_registros.getLength()].folio IS NULL THEN
            CALL arr_registros.deleteElement(arr_registros.getLength())
        END IF   

        IF (arr_registros[1].folio IS NULL OR arr_registros[1].folio = 0) THEN
          CALL fn_mensaje("Consulta",
                    "No se obtuvieron resultados con el Consecutivo Cuenta ingresado.",
                    "about") 
        ELSE 
          DIALOG  ATTRIBUTE(UNBUFFERED)
        
            DISPLAY ARRAY arr_registros TO tbl_registros.*
            --boton de datos complementarios
             ON ACTION reporte
                CALL ARR_CURR( ) RETURNING v_arr_curr                                                          	   
                --se invoca a funcón que despliega en pantalla los datos complementarios de la consulta   
                DISPLAY "@ seleccionado arr_registros[v_arr_curr].*: ", arr_registros[v_arr_curr].*
                  CALL fn_reporte (arr_registros[v_arr_curr].*)
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

      
   --RETURN v_registros
   END IF 
    
   CLOSE WINDOW w_consulta_registros

END FUNCTION


FUNCTION fn_reporte( arr_registros )
   DEFINE arr_registros         RECORD    
       	    nss                      LIKE afi_derechohabiente.nss,
            folio                    LIKE pag_det_fc.folio,
            origen_archivo           LIKE pag_det_fc.origen_archivo,
            id_referencia            LIKE pag_det_fc.id_referencia,
            f_pago                   LIKE pag_det_fc.f_pago,
            id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente,
            imp_ap_fc                LIKE pag_det_fc.imp_ap_fc,
            loc_trabajador           STRING 
       END RECORD,
       v_ruta_reporte            STRING ,-- ruta del archivo del reporte
       v_ruta_listados           STRING ,-- ruta de los listados
       v_ruta_ejecutable         STRING, -- ruta del ejecutable     
      -- v_ventana                 ui.WINDOW,
      -- v_fecha                  DATE,
      -- v_bandera                SMALLINT,
      -- v_registros              INTEGER,      
      -- cve_origen_archivo       LIKE cta_his_pagos.origen_archivo,
      -- cve_tipo_aclaracion      LIKE pag_tpo_aclaracion.aclaracion_cod,
      -- v_query                  STRING,
      -- v_condicion              STRING,
      -- v_folio                  LIKE cta_his_pagos.folio,
      -- v_nss                    LIKE afi_derechohabiente.nss,
      -- v_id_derechohabiente     LIKE cta_his_pagos.id_derechohabiente,
      -- v_indice                 INTEGER,
       manejador_rpt            om.SaxDocumentHandler,
--       v_desc_destino_ap_viv      STRING
       v_usuario_desc   LIKE seg_usuario.usuario_desc
       
   --se obtiene el nombre del usuario logeado en el sistema
    SELECT usuario_desc 
      INTO v_usuario_desc 
      FROM seg_usuario
     WHERE usuario_cod= p_usuario_cod

    
   IF(INT_FLAG = 0)THEN  
      # Recupera la ruta de listados en el que se enviara el archivo
      CALL fn_rutas("pag") RETURNING v_ruta_ejecutable, v_ruta_listados 
    
      --DISPLAY "v_ruta_ejecutable  ",v_ruta_ejecutable
      --DISPLAY "v_ruta_listados  ",v_ruta_listados
    
      --Se asigna la plantilla para generar el reporte
      IF fgl_report_loadCurrentSettings("PAGC24.4rp") THEN 
          CALL fgl_report_selectDevice ("PDF")
                      
          LET v_ruta_reporte = v_ruta_ejecutable CLIPPED,"/","consulta_detalle_pagos_fc"                
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
          CALL fgl_report_selectPreview(1)
          LET manejador_rpt = fgl_report_commitCurrentSettings()
      ELSE         
          DISPLAY "no fue posible generar el reporte"
          EXIT PROGRAM 
      END IF   
      --Inicia el reporte de registros con rechazo
      START REPORT rpt_detalle_pagos_fc TO XML HANDLER manejador_rpt
      -- Asigna el titulo del reporte     
          OUTPUT TO REPORT rpt_detalle_pagos_fc(arr_registros.*, v_usuario_desc)
     
      FINISH REPORT rpt_detalle_pagos_fc 
   END IF
END FUNCTION

-- OBJETIVO: Emitir el reporte de liquidacion/preliquidacion
REPORT rpt_detalle_pagos_fc(arr_registros, p_usuario_desc)
   DEFINE arr_registros         RECORD    
       	    nss                      LIKE afi_derechohabiente.nss,
            folio                    LIKE pag_det_fc.folio,
            origen_archivo           LIKE pag_det_fc.origen_archivo,
            id_referencia            LIKE pag_det_fc.id_referencia,
            f_pago                   LIKE pag_det_fc.f_pago,
            id_derechohabiente       LIKE afi_derechohabiente.id_derechohabiente,
            imp_ap_fc                LIKE pag_det_fc.imp_ap_fc,
            loc_trabajador           STRING 
       END RECORD,
       v_fecha_reporte           DATE,
       p_usuario_desc            LIKE seg_usuario.usuario_desc

        
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER                                                                          
                                                                                              
      LET v_fecha_reporte = TODAY CLIPPED                                                     
                                                                                              
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"                                                                                                                   
      PRINTX "Folio"
      PRINTX p_usuario_cod
      PRINTX p_usuario_desc
      
   ON EVERY ROW                                                                               
      PRINTX arr_registros.*
                                                                                           
END REPORT  
