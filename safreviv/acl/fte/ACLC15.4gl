--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:29/06/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC15                                                                 #
#Objetivo     => Consulta, lanzador de reporte Salida de aclaratorio                    #
#Fecha inicio => Julio 25, 2012                                                         # 
#Autor        => Rubén Haro Castro                                                      #
#########################################################################################

DATABASE safre_viv
GLOBALS
DEFINE  v_ventana            ui.Window,
        v_forma              ui.form,
        p_ventana            STRING
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
  
   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- consulta de informacion salida aclratorio
   CALL f_reporte_salida_aclaracion()

END MAIN

FUNCTION  f_reporte_salida_aclaracion()
DEFINE 
     folio   LIKE cta_his_pagos.folio --variable para el folio capturado 
      
      
   CLOSE WINDOW SCREEN
   -- Se abre la ventana de captura de folio
   OPEN WINDOW W_ACLC15 WITH FORM "ACLC151"

   -- Se asigna el titulo de la ventana
      LET v_ventana = ui.Window.getCurrent()
      LET v_forma = v_ventana.getForm() 
      IF ( v_forma IS NOT NULL ) THEN
         CALL ui.Interface.setText(v_forma)         
         CALL v_ventana.setText(v_forma)
      END IF      
      

    INPUT folio 
     FROM folio  
    ATTRIBUTES (UNBUFFERED)      
      
     
      ON ACTION ACCEPT
         IF (folio IS NULL) THEN
            CALL fn_mensaje("Consulta","Debe de ingresar un folio válido","about")
            NEXT FIELD folio
        ELSE
           CALL f_consulta_info_reporte(folio)
           EXIT INPUT
        END IF
    
      ON ACTION CANCEL
        EXIT INPUT

    END INPUT

CLOSE WINDOW W_ACLC15

END FUNCTION

--realiza la cionsulta de la información relacionada al folio capturado
FUNCTION f_consulta_info_reporte(v_d_folio)
# Funcion que reliza las consultas de validacion de datos para ejecutar el reporte
DEFINE v_d_folio              LIKE cta_his_pagos.folio,
       v_i_cont_registros     INTEGER, 
       v_i_indice_array       INTEGER,
       v_s_consulta           STRING,
       v_id_derechohabiente   LIKE  afi_derechohabiente.id_derechohabiente,
       v_ar_datos_salida_aclara DYNAMIC ARRAY OF RECORD 
       	 nss                 LIKE afi_derechohabiente.nss     ,
       	 periodo_pago        LIKE cta_his_pagos.periodo_pago  ,
       	 f_pago              LIKE cta_his_pagos.f_pago        ,
       	 folio_sua           LIKE cta_his_pagos.folio_sua     ,
       	 imp_ap_pat          LIKE cta_his_pagos.imp_ap_pat    ,
       	 imp_am_cre          LIKE cta_his_pagos.imp_am_cre    ,        
       	 imp_ren_viv_pgo_ext LIKE cta_his_pagos.imp_ren_viv_pgo_ext,  
       	 int_gen_pgo_ext     LIKE cta_his_pagos.int_gen_pgo_ext , 
       	 aiv_ap_pat          LIKE cta_his_pagos.aiv_ap_pat      ,
       	 aiv_gen_pgo_ext     LIKE cta_his_pagos.aiv_gen_pgo_ext 
       END RECORD ,     
       manejador_rpt         om.SaxDocumentHandler,
       v_i_inicio_for       INTEGER,
       v_ruta_reporte       STRING, -- ruta del archivo del reporte
       v_ruta_listados      STRING, -- ruta de los listados
       v_ruta_ejecutable    STRING -- ruta del ejecutable
    

       LET v_i_cont_registros  = 1  --SE INICIALIZA EL CONTADOR DE REGISTROS 
       LET v_i_indice_array  = 1  --SE INICIALIZA EL INDICE DEL ARRAY

       LET v_s_consulta   = " SELECT COUNT (*) \n ",
                            "   FROM cta_his_pagos  \n ",
                            "  WHERE  ind_liquidacion = 5 \n ",
                            "    AND folio  =   ? "
       PREPARE prp_conteo  FROM v_s_consulta 
       EXECUTE prp_conteo 
       INTO v_i_cont_registros
       USING v_d_folio

       --Si v_i_cont_registros es mayor a 1 si existen registros = folio dado i ind_liquidacion = 5  para generación de reporte
       IF v_i_cont_registros > 0 THEN 
       	
          LET v_s_consulta = " SELECT id_derechohabiente,periodo_pago,f_pago, folio_sua,imp_ap_pat,imp_am_cre ,  \n",
                             "        imp_ren_viv_pgo_ext,int_gen_pgo_ext,aiv_ap_pat,aiv_gen_pgo_ext \n",
                             "   FROM cta_his_pagos \n",
                             "  WHERE folio = ? \n",  
                             "    AND ind_liquidacion = 5 \n"
          PREPARE con_datos from  v_s_consulta
          DECLARE cur_datos  CURSOR FOR  con_datos
          FOREACH cur_datos USING v_d_folio	
          	INTO  v_id_derechohabiente  ,              	     
       	          v_ar_datos_salida_aclara[v_i_indice_array].periodo_pago          ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].f_pago                ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].folio_sua             ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].imp_ap_pat            ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].imp_am_cre            ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].imp_ren_viv_pgo_ext   ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].int_gen_pgo_ext       ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].aiv_ap_pat            ,
       	          v_ar_datos_salida_aclara[v_i_indice_array].aiv_gen_pgo_ext

       	          SELECT nss 
       	            INTO v_ar_datos_salida_aclara[v_i_indice_array].nss
       	            FROM afi_derechohabiente
       	           WHERE id_derechohabiente  = v_id_derechohabiente

       	          LET v_i_indice_array = v_i_indice_array + 1

          END FOREACH        
          -- se borra el ultimo elemento porque el FOREACH agrega un elemento nulo al final
          LET v_i_indice_array = v_i_indice_array  - 1
          CALL v_ar_datos_salida_aclara.deleteElement(v_ar_datos_salida_aclara.getLength())          
          
          ---se abre la ventana de 
          OPEN WINDOW w_datos_consultados WITH FORM "ACLC152"           

          DISPLAY ARRAY v_ar_datos_salida_aclara TO sr_arr_salida_aclara.* ATTRIBUTES(UNBUFFERED, ACCEPT = FALSE)


          --boton para generar reporte
          ON ACTION reporte

             # Recupera la ruta de listados en el que se enviara el archivo
             CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
                                                --r_ruta_bin, r_ruta_listados
             --Se asigna la plantilla para generar el reporte
             IF fgl_report_loadCurrentSettings("rpt_acl_salida_aclaratorio.4rp") THEN 
                 CALL fgl_report_selectDevice ("PDF")
                             
                 LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","rpt_acl_salida_aclaratorio"
                 CALL fgl_report_setOutputFileName(v_ruta_reporte)
                 CALL fgl_report_selectPreview(1)
                 LET manejador_rpt = fgl_report_commitCurrentSettings()
             ELSE         
                 DISPLAY "no fue posible generar el reporte"
                 EXIT PROGRAM 
             END IF

             --Inicia el reporte de registros con rechazo
             START REPORT rpt_salida_aclaratorio TO XML HANDLER manejador_rpt
             -- Asigna el titulo del reporte
             FOR v_i_inicio_for = 1 TO v_i_indice_array
                OUTPUT TO REPORT rpt_salida_aclaratorio(v_d_folio, v_ar_datos_salida_aclara[v_i_inicio_for].*)
             END FOR  
             FINISH REPORT rpt_salida_aclaratorio
            EXIT DISPLAY  
            
          ON ACTION CANCEL 
            EXIT DISPLAY  
            
                  	                      
          END DISPLAY 
         CLOSE WINDOW w_datos_consultados 

       --caso contrario si no existen registron copn el folio dado i ind_liquidacion = 5 
       ELSE
           --se envia mensdaje de no exiten registros 
          CALL fn_mensaje  ("Atención","No existen registros con los criterios dados","Info") 
       END IF
    
END FUNCTION

--genera el reporte de salida aclaratorio 
REPORT rpt_salida_aclaratorio( v_d_folio, rec_datos_salida_aclara)
  DEFINE v_d_folio INTEGER , --LIKE cta_his_pagos.folio,
         rec_datos_salida_aclara RECORD 
         	 nss                 LIKE afi_derechohabiente.nss     ,
         	 periodo_pago        LIKE cta_his_pagos.periodo_pago  ,     
         	 f_pago              LIKE cta_his_pagos.f_pago        , 
         	 folio_sua           INTEGER ,--LIKE cta_his_pagos.folio_sua     ,
         	 imp_ap_pat          LIKE cta_his_pagos.imp_ap_pat    ,
         	 imp_am_cre          LIKE cta_his_pagos.imp_am_cre    ,        
         	 imp_ren_viv_pgo_ext LIKE cta_his_pagos.imp_ren_viv_pgo_ext,  
         	 int_gen_pgo_ext     LIKE cta_his_pagos.int_gen_pgo_ext , 
         	 aiv_ap_pat          LIKE cta_his_pagos.aiv_ap_pat      ,
         	 aiv_gen_pgo_ext     LIKE cta_his_pagos.aiv_gen_pgo_ext 
         END RECORD ,     
         v_fecha_reporte  DATE

FORMAT

   FIRST PAGE HEADER

      LET v_fecha_reporte = TODAY CLIPPED

      PRINTX v_d_folio
      PRINTX v_fecha_reporte USING "dd-mm-yyyy"

   ON EVERY ROW
    PRINTX rec_datos_salida_aclara.*

END REPORT  