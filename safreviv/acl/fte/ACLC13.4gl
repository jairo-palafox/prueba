--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLC13                                                                 #
#Objetivo     => Analizador Aclaracion Pendiente                                        # 
#Fecha inicio => 03 de Julio de 2012                                                    #
#Autor        => Rubén Haro Castro                                                      #
#########################################################################################

DATABASE safre_viv
GLOBALS 
DEFINE arr_salida_aclara         DYNAMIC ARRAY OF RECORD
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
          tpo_aclaracion          VARCHAR(150),
          imp_ap_pat              DECIMAL(12,2),
          imp_am_cre              DECIMAL(12,2),
          imp_ren_viv_pgo_e       DECIMAL(12,2),
          aiv_ap_pat              DECIMAL(18,6),
          valor_aiv               DECIMAL(18,6),
          int_gen_pgo_ext         DECIMAL(12,2),
          aiv_gen_pgo_ext         DECIMAL(18,6),
          result_operacion        SMALLINT,
          ind_liquidacion         VARCHAR(40),
          tpo_archivo             SMALLINT,
          origen_pago             VARCHAR(60),
          tpo_afiliacion          SMALLINT,
          f_actualiza             DATE
        END RECORD,
        g_ruta_envio                LIKE seg_modulo.ruta_envio,
        g_nom_archivo               VARCHAR(50),
        g_consulta         RECORD
         nss               CHAR(11),
         nrp               CHAR(11),
         periodo_pago      CHAR(6),
         folio_sua         DECIMAL(9,0)
        END RECORD
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
   
   -- consulta de informacion analizador de alcaracion pendiente 
   CALL fn_consulta_analizador_acl_pend(p_usuario_cod, p_s_titulo)

END MAIN

{ ======================================================================
Clave: ACLC13
Nombre: fn_consulta_analizador_acl_pend
Fecha creacion: 03 de Julio de 2012  
Autor: Rubén Haro Castro 
Narrativa del proceso que realiza:


Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
FUNCTION fn_consulta_analizador_acl_pend(p_usuario_cod, p_s_titulo)
DEFINE p_usuario_cod            LIKE seg_usuario.usuario_cod, -- clave del usuario
       p_s_titulo               STRING, -- titulo de la ventana
       v_folio                  DECIMAL(9,0), -- folio
       v_s_construct            STRING,
       v_query                  STRING,
       v_nss                    CHAR(11),  --LIKE afi_derechohabiente.nss,
       v_id_derechohabiente     DECIMAL(9,0),  --LIKE cta_his_pagos.id_derechohabiente,
       v_nrp                    CHAR(11),  --LIKE cta_his_pagos.nrp,
       v_f_pago                 DATE, --LIKE cta_his_pagos.f_pago,
       v_ent_receptora          CHAR(3),
       v_origen_pago            SMALLINT,
       v_registros              INTEGER,
       v_indice                	INTEGER,  --Indice generl de registrtos 
       v_seleccionado           INTEGER,  --indice del registro seleccionado
       v_desc_origen_pago       VARCHAR(60),  --descripción del origen de pago
       v_c_aux_acl_descripcion  VARCHAR(150),  --descripción del tipo de aclaracion
       v_c_aux_desc_ind_liquida VARCHAR(40),
       v_i_tpo_aclaracion       SMALLINT
       

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

    --se abre la ventana de  forma de captura de parametros de busqueda    
   OPEN WINDOW w_acl_analiza_pend WITH FORM "ACLC131"

      CONSTRUCT v_s_construct 
         ON c.nss, a.nrp,a.periodo_pago, a.folio_sua       
         FROM nss,nrp,periodo_pago,folio_sua

          BEFORE CONSTRUCT

          ON ACTION ACCEPT
             LET g_consulta.nss           = GET_FLDBUF(nss)
             LET g_consulta.nrp           = GET_FLDBUF(nrp)
             LET g_consulta.periodo_pago  = GET_FLDBUF(periodo_pago)
             LET g_consulta.folio_sua     = GET_FLDBUF(folio_sua)
            
             IF (g_consulta.nss IS NULL) AND
                (g_consulta.nrp IS NULL) AND
                (g_consulta.periodo_pago IS NULL) AND
                (g_consulta.folio_sua IS NULL) THEN
                CALL fn_mensaje("Consulta","Debe de ingresar un campo de búsqueda","about")
             ELSE
                ACCEPT CONSTRUCT
             END IF

          ON ACTION cancel
             LET INT_FLAG = TRUE
             EXIT CONSTRUCT

      END CONSTRUCT

      IF NOT INT_FLAG THEN
          --hace el conteo de registros
          --hace el conteo de registros
          LET v_query = "SELECT COUNT(*) \n",
                        "  FROM cta_his_pagos a, pag_ctr_pago b, afi_derechohabiente c \n",
                        " WHERE a.id_derechohabiente = b.id_derechohabiente \n",
                        "   AND a.id_derechohabiente = c.id_derechohabiente \n",
                        "   AND a.ind_liquidacion = 1  \n", 
                        "   AND ",v_s_construct

         --DISPLAY " v_query ",v_query
         PREPARE prp_count_aclara FROM v_query
         EXECUTE prp_count_aclara INTO v_registros

         IF v_registros IS NULL THEN
           LET v_registros = 0
         END IF

         --valida que se econtrarón registros
         IF v_registros > 0 THEN
            --realizala busqueda para llenar el arreglo
             LET v_query =  " SELECT a.folio,a.origen_archivo,a.id_referencia ,a.cve_ent_receptora \n", 
                            "        ,a.nrp,a.periodo_pago,a.folio_sua,a.f_pago,a.id_derechohabiente  \n",
                            "        ,a.localiza_trabajador,b.tpo_aclaracion,a.imp_ap_pat,a.imp_am_cre   \n",       
                            "        ,a.imp_ren_viv_pgo_ext,a.aiv_ap_pat,a.valor_aiv,a.int_gen_pgo_ext    \n", 
                            "        ,a.aiv_gen_pgo_ext,a.result_operacion,a.ind_liquidacion     ,  \n",
                            "        b.tpo_archivo, b.origen_pago, b.tpo_afiliacion, b.f_actualiza \n",
                            "   FROM cta_his_pagos a, pag_ctr_pago b, afi_derechohabiente c \n",
                            "  WHERE a.id_derechohabiente = b.id_derechohabiente \n",
                            "    AND a.id_derechohabiente = c.id_derechohabiente \n", 
                            "    AND a.ind_liquidacion = 1 \n", 
                            "    AND ",v_s_construct
                            --añadir not exist en acl_preliquida
            {
            LET v_query = "SELECT a.folio,a.origen_archivo,a.id_referencia ,a.cve_ent_receptora \n", 
                        "         ,a.nrp,a.periodo_pago,a.folio_sua,a.f_pago,a.id_derechohabiente  \n",
                        "         ,a.localiza_trabajador,a.tpo_aclaracion,a.imp_ap_pat,a.imp_am_cre   \n",       
                        "         ,a.imp_ren_viv_pgo_ext,a.aiv_ap_pat,a.valor_aiv,a.int_gen_pgo_ext    \n", 
                        "         ,a.aiv_gen_pgo_ext,a.result_operacion,a.ind_liquidacion     ,  \n",
                        "         b.tpo_archivo, b.origen_pago, b.tpo_afiliacion, b.f_actualiza \n",
                        "    FROM cta_his_pagos a, pag_ctr_pago b  \n",
                        "   WHERE a.id_derechohabiente = b.id_derechohabiente \n",
                        "     AND a.id_derechohabiente = ", v_id_derechohabiente,"\n",
                        "     AND a.nrp          = '",v_nrp,"'\n",
                        "     AND a.periodo_pago = ",v_periodo_pago, "\n",
                        "     AND a.folio_sua    = ",v_folio_sua , "\n",
                        "     AND a.ind_liquidacion = 1  "
             }           
            --DISPLAY v_query
            PREPARE prp_cur_folio FROM v_query
            DECLARE cur_folio CURSOR FOR prp_cur_folio

            LET v_indice = 1
            --DISPLAY " v_indice",v_indice
            --llen ael arreglo
            FOREACH cur_folio INTO  arr_salida_aclara[v_indice].folio
                                   ,arr_salida_aclara[v_indice].origen_archivo
                                   ,arr_salida_aclara[v_indice].id_referencia
                                   ,arr_salida_aclara[v_indice].cve_ent_receptora
                                   ,arr_salida_aclara[v_indice].nrp
                                   ,arr_salida_aclara[v_indice].periodo_pago
                                   ,arr_salida_aclara[v_indice].folio_sua
                                   ,arr_salida_aclara[v_indice].f_pago
                                   ,arr_salida_aclara[v_indice].id_derechohabiente
                                   ,arr_salida_aclara[v_indice].localiza_trabajadador
                                   ,arr_salida_aclara[v_indice].tpo_aclaracion
                                   ,arr_salida_aclara[v_indice].imp_ap_pat
                                   ,arr_salida_aclara[v_indice].imp_am_cre
                                   ,arr_salida_aclara[v_indice].imp_ren_viv_pgo_e
                                   ,arr_salida_aclara[v_indice].aiv_ap_pat
                                   ,arr_salida_aclara[v_indice].valor_aiv
                                   ,arr_salida_aclara[v_indice].int_gen_pgo_ext
                                   ,arr_salida_aclara[v_indice].aiv_gen_pgo_ext
                                   ,arr_salida_aclara[v_indice].result_operacion
                                   ,arr_salida_aclara[v_indice].ind_liquidacion
                                   ,arr_salida_aclara[v_indice].tpo_archivo
                                   ,arr_salida_aclara[v_indice].origen_pago 
                                   ,arr_salida_aclara[v_indice].tpo_afiliacion
                                   ,arr_salida_aclara[v_indice].f_actualiza
                                   
                      --se extrae la descripción delm tipo de aclaración
                      SELECT aclaracion_descripcion
                        INTO v_c_aux_acl_descripcion
                        FROM pag_tpo_aclaracion
                       WHERE aclaracion_cod = arr_salida_aclara[v_indice].tpo_aclaracion

                      --se asigna la descricpción al elemento del array 
                      LET arr_salida_aclara[v_indice].tpo_aclaracion = v_c_aux_acl_descripcion
                       
                       SELECT desc_ind_liquidacion  
                         INTO v_c_aux_desc_ind_liquida
                         FROM pag_ind_liquidacion
                        WHERE ind_liquidacion =  arr_salida_aclara[v_indice].ind_liquidacion
                        
                      --se asigna la descripción al registro correspondiente 
                      LET arr_salida_aclara[v_indice].ind_liquidacion = v_c_aux_desc_ind_liquida
                       
                      --se consulta el nss del registro correspondiente              
                      SELECT nss
                        INTO arr_salida_aclara[v_indice].nss
                       FROM  afi_derechohabiente
                       WHERE id_derechohabiente = arr_salida_aclara[v_indice].id_derechohabiente   
                     
                     --se consulta la descripción del tipo de pago
                      SELECT pago_descripcion
                        INTO v_desc_origen_pago
                        FROM pag_origen_pago
                       WHERE pago_cod = arr_salida_aclara[v_indice].origen_pago

                     --se asigna la descricpción al elemento del array 
                     LET arr_salida_aclara[v_indice].origen_pago = v_desc_origen_pago
                     
                     --display "origen pago= ",arr_salida_aclara[v_indice].origen_pago
                     
                     
                    LET v_indice = v_indice + 1
            END FOREACH
            FREE cur_folio 

           --elinina ultimo renglon en blanco
           LET v_indice = v_indice - 1             
           CALL arr_salida_aclara.deleteElement(arr_salida_aclara.getLength())
           
            DISPLAY ARRAY arr_salida_aclara TO salida_aclara.* 
            ATTRIBUTES (ACCEPT = FALSE)

            ON ACTION CANCEL
                EXIT DISPLAY

            ON ACTION reporte
               CALL fn_reporte_acl_analiza_pend(v_indice)
               
            
            ON ACTION genera_archivo               
               CALL fn_genera_archivo_txt_acl_analiza_pend (v_indice)

            END DISPLAY
          
         ELSE
           CALL fn_mensaje("Consulta","No existen registros con los criterios dados.","about")   
         END IF --IF v_registros > 0 THEN
      END IF  --IF NOT INT_FLAG THEN
   CLOSE WINDOW w_acl_analiza_pend
END FUNCTION
{
======================================================================
Clave: ACLC13
Nombre: fn_reporte_acl_analiza_pend
Fecha creacion: Julio 3 de 2012
Autor: Rubén Haro Castro
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
-- OBJETIVO: Obtener los datos necesarios para emitir el reporte Analizador Aclaracion Pendiente
FUNCTION fn_reporte_acl_analiza_pend(v_d_indice )
    DEFINE v_d_indice              	INTEGER,
           v_d_indice_for          	INTEGER, 
           p_b_despliegue_pantalla  SMALLINT, -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
           v_desc_origen_pago       VARCHAR(60),  --descripción del origen de pago
    {DEFINE arr_salida_aclara      RECORD
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
          origen_pago             SMALLINT,
          tpo_afiliacion          SMALLINT,
          f_actualiza             DATE
        END RECORD,}
      manejador_rpt               om.SaxDocumentHandler,    
      v_ruta_reporte              STRING, -- ruta del archivo del reporte
      v_ruta_listados             STRING, -- ruta de los listados
      v_ruta_ejecutable           STRING -- ruta del ejecutable
      
      LET v_desc_origen_pago = NULL    
      LET v_d_indice_for = 1

    # Recupera la ruta de listados en el que se enviara el archivo
    CALL fn_rutas("acl") RETURNING v_ruta_ejecutable, v_ruta_listados 
    --r_ruta_bin, r_ruta_listados
    --Se asigna la plantilla para generar el reporte
    IF fgl_report_loadCurrentSettings("rpt_acl_pendiente.4rp") THEN 
        CALL fgl_report_selectDevice ("PDF")
                    
        LET v_ruta_reporte = v_ruta_listados CLIPPED,"/","aclaracion_pendiente"
        CALL fgl_report_setOutputFileName(v_ruta_reporte)
        CALL fgl_report_selectPreview(1)
        LET manejador_rpt = fgl_report_commitCurrentSettings()
    ELSE         
        DISPLAY "no fue posible generar el reporte"
        EXIT PROGRAM 
    END IF   
    --Inicia el reporte de registros con rechazo
    START REPORT rpt_aclaracion_pendiente TO XML HANDLER manejador_rpt
    -- Asigna el titulo del reporte       
    FOR v_d_indice_for = 1 TO v_d_indice
    	  -- busca descripción del nuevo origen de pago
        SELECT archivo_descripcion 
          INTO v_desc_origen_pago
          FROM pag_tpo_archivo
         WHERE archivo_cod = arr_salida_aclara[v_d_indice_for].origen_archivo
    	
       OUTPUT TO REPORT rpt_aclaracion_pendiente(arr_salida_aclara[v_d_indice_for].*, v_desc_origen_pago)                                                                
    END FOR 
    FINISH REPORT rpt_aclaracion_pendiente 
END FUNCTION
{
======================================================================
Clave: ACLC13
Nombre: rpt_aclaracion_pendiente
Fecha creacion: Julio 3 de 2012
Autor: Rubén Haro Castro
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
REPORT rpt_aclaracion_pendiente(arr_salida_aclara, p_desc_origen_pago)
DEFINE arr_salida_aclara         RECORD
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
          tpo_aclaracion          VARCHAR(150),
          imp_ap_pat              DECIMAL(12,2),
          imp_am_cre              DECIMAL(12,2),
          imp_ren_viv_pgo_e       DECIMAL(12,2),
          aiv_ap_pat              DECIMAL(18,6),
          valor_aiv               DECIMAL(18,6),
          int_gen_pgo_ext         DECIMAL(12,2),
          aiv_gen_pgo_ext         DECIMAL(18,6),
          result_operacion        SMALLINT,
          ind_liquidacion         VARCHAR(40),
          tpo_archivo             SMALLINT,
          origen_pago             VARCHAR(60),
          tpo_afiliacion          SMALLINT,
          f_actualiza             DATE
        END RECORD,
        p_usuario_cod             LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
        v_fecha_reporte           DATE,
        p_desc_origen_pago        VARCHAR(60)
                                                                                                                                                                                          
FORMAT                                                                                        
                                                                                              
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED
      PRINTX v_fecha_reporte

      ON EVERY ROW                                                                               
      PRINTX arr_salida_aclara.*
                                                                                           
END REPORT          
{
======================================================================
Clave: ACLC13
Nombre: fn_genera_archivo_txt_acl_analiza_pend
Fecha creacion: Julio 4 de 2012
Autor: Rubén Haro Castro
Narrativa del proceso que realiza:

Registro de modificaciones:
Autor           Fecha                   Descrip. cambio

======================================================================
}
#OBJETIVO: Genera el reporte de aclaracion_pendiente
FUNCTION fn_genera_archivo_txt_acl_analiza_pend (v_indice)
DEFINE v_indice                    INTEGER,
       v_s_encabezado              STRING,
       v_s_detalle                 STRING, 
       v_s_sumario                 STRING,
       v_canal_archivo             BASE.CHANNEL,
       v_query                     STRING,
       v_string_detalle            STRING,
       v_string_encabezado         STRING,
       v_string_sumario            STRING,
       v_id_lote                   INTEGER,
       v_lote_x_dia                SMALLINT,
       v_total_detalle             INTEGER,
       v_total_imp_ap_pat          DECIMAL(22,2),
       v_total_imp_am_cre          DECIMAL(22,2),
       v_total_imp_ren_viv_pgo_ext LIKE cta_his_pagos.imp_ren_viv_pgo_ext,
       v_total_imp_aiv_ap_pat      LIKE cta_his_pagos.aiv_ap_pat,
       v_total_imp_int_gen_pgo_ext LIKE cta_his_pagos.int_gen_pgo_ext,
       v_total_imp_aiv_gen_pgo_ext LIKE cta_his_pagos.aiv_gen_pgo_ext,
       v_tabla_origen              VARCHAR (50),
       v_id_derechohabiente_tmp    DECIMAL (9,0),
       tpo_registro	               CHAR(2),
       id_operación	               CHAR(3), 
       f_creacion                  CHAR(8),
       v_c_d_tpo_registro	         CHAR(2),
   	   v_c_d_reg_patronal          CHAR(11),
       v_c_d_rfc_patron	           CHAR(13),
       v_c_d_per_pago	             CHAR(6),
       v_c_d_folio_sua             CHAR(6),
       v_c_d_nss	                 CHAR(11),       
       v_c_d_nume_credito          CHAR(10),       
       v_c_d_paterno_trabajador    CHAR(40),
       v_c_d_materno_trabajador	   CHAR(40),
       v_c_d_nombres_trabajador	   CHAR(40),
       v_c_d_tpo_aclaración	       CHAR(40),
       v_c_d_imp_ap_patronal       CHAR(14),
       v_c_d_imp_am_cre            CHAR(14),
       v_c_sum_tpo_reg             CHAR(2),
       v_c_sum_id_operación	       CHAR(3),
       v_c_sum_f_creación          CHAR(8),
       v_c_sum_total_registros     CHAR(9),
       v_c_sum_tot_imp_ap_pat      CHAR(9),
       v_c_sum_tot_imp_am_cre      CHAR(9),
       v_i_indice_for              INTEGER,
       v_s_consecutivo             SMALLINT, 
       v_desc_origen_pago          CHAR(60),
       v_c_nume_credito            DECIMAL(10,0) 
       
       
   LET v_i_indice_for = 1 
   --Se obtiene la ruta de envio de los archivos
   LET v_query = "\n SELECT ruta_envio         "
                ,"\n FROM   seg_modulo         "
                ,"\n WHERE  modulo_cod = 'acl' "
   PREPARE prp_ruta_archivo FROM v_query
   EXECUTE prp_ruta_archivo INTO g_ruta_envio
   
   {
   SELECT consecutivo
     INTO v_s_consecutivo 
     FROM acl_ctr_lote
    WHERE lote_cod  = 1
    } 

   --Formato del nombre del archivo
   --LET g_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
    --                  ,v_s_consecutivo USING "&&"
   LET v_total_imp_ap_pat = 0
   LET v_total_imp_am_cre = 0

   LET g_nom_archivo = TIME
   LET g_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                      , g_nom_archivo[4,5], g_nom_archivo[7,8],".ANA"

   -- se crea el manejador de archivo
   LET v_canal_archivo = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal_archivo.openFile(g_ruta_envio CLIPPED||"/"||g_nom_archivo, "w" )
   CALL v_canal_archivo.setDelimiter("")
   LET v_id_lote = 0
   LET v_lote_x_dia = 1
  
   --Se escribe el encabezado  
   LET tpo_registro = "01"
   LET id_operación = "ANA"
   LET f_creacion = YEAR(TODAY)  USING "&&&&", MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   
   
   --Se pasan a una sola cadena los datos del encabezado
   LET v_s_encabezado =   tpo_registro , 
                          id_operación , 
                          f_creacion     

   
   --Se escribe en el archivo los datos obtenidos del encabezado
   CALL v_canal_archivo.WRITE([v_s_encabezado])

   --Se obtienen los datos del detalle 
   -- Asigna el titulo del reporte       
   FOR v_i_indice_for = 1 TO v_indice
   	
   	  -- busca descripción del nuevo origen de pago
       SELECT archivo_descripcion 
         INTO v_desc_origen_pago
         FROM pag_tpo_archivo
        WHERE archivo_cod = arr_salida_aclara[v_i_indice_for].origen_archivo

      --Se realiza la suma de los importes
      LET v_total_imp_ap_pat = v_total_imp_ap_pat  + arr_salida_aclara[v_i_indice_for].imp_ap_pat
      LET v_total_imp_am_cre = v_total_imp_am_cre  + arr_salida_aclara[v_i_indice_for].imp_am_cre
    
      LET v_id_lote = v_id_lote + 1
 
      --Se pasan los detalles a su equivalente en tipo char     
      LET v_c_d_tpo_registro	 = "02" 
      
      --se extrae el RFC_PATRON 
      SELECT rfc_patron
        INTO v_c_d_rfc_patron
        FROM cta_pag_complemento
       WHERE folio = arr_salida_aclara[v_i_indice_for].folio
         AND id_referencia = arr_salida_aclara[v_i_indice_for].id_referencia

      --se extrae el numero de credito
      SELECT num_crd_ifv
        INTO v_c_d_nume_credito
        FROM cta_his_pagos 
       WHERE folio = arr_salida_aclara[v_i_indice_for].folio
         AND id_referencia = arr_salida_aclara[v_i_indice_for].id_referencia                     

     --se extrae el nombre de trabajador 
     SELECT nombre_af, ap_paterno_af,ap_materno_af
       INTO v_c_d_nombres_trabajador,v_c_d_paterno_trabajador,v_c_d_materno_trabajador
       FROM afi_derechohabiente
      WHERE nss = arr_salida_aclara[v_i_indice_for].nss

      LET v_c_d_per_pago	               = arr_salida_aclara[v_i_indice_for].periodo_pago
      LET v_c_d_folio_sua                = arr_salida_aclara[v_i_indice_for].folio_sua
      LET v_c_d_nss	                     = arr_salida_aclara[v_i_indice_for].nss
      LET v_c_d_tpo_aclaración	         = arr_salida_aclara[v_i_indice_for].tpo_aclaracion 
      LET v_c_d_imp_ap_patronal          = (arr_salida_aclara[v_i_indice_for].imp_ap_pat * 100) USING "&&&&&&&&&&&&&&" 
      LET v_c_d_imp_am_cre               = (arr_salida_aclara[v_i_indice_for].imp_am_cre * 100) USING "&&&&&&&&&&&&&&" 
      LET v_c_d_reg_patronal             = arr_salida_aclara[v_i_indice_for].nrp

      --Se asigna todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_s_detalle =  v_c_d_tpo_registro	        , 
                         v_c_d_reg_patronal         ,
                         v_c_d_rfc_patron           ,
                         v_c_d_per_pago	            ,
                         v_c_d_folio_sua            ,
                         v_c_d_nss	                ,
                         v_c_d_nume_credito         ,
                         v_c_d_paterno_trabajador   ,
                         v_c_d_materno_trabajador	  ,
                         v_c_d_nombres_trabajador	  ,
                         v_c_d_tpo_aclaración	      ,
                         v_c_d_imp_ap_patronal      ,
                         v_c_d_imp_am_cre 

      --Se escribe en el archivo los datos obtenidos
      CALL v_canal_archivo.WRITE([v_s_detalle])
   END FOR

   --Se obtienen los registros del sumario
   LET  v_c_sum_tpo_reg           = "09"
   LET  v_c_sum_id_operación	    = "ANA"
   LET  v_c_sum_f_creación        = YEAR(TODAY)  USING "&&&&", MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   LET  v_c_sum_total_registros   = v_id_lote 
   LET  v_c_sum_tot_imp_ap_pat    = ( v_total_imp_ap_pat * 100 ) USING "&&&&&&&&&"
   LET  v_c_sum_tot_imp_am_cre    = ( v_total_imp_am_cre * 100 ) USING "&&&&&&&&&"

   LET v_s_sumario = v_c_sum_tpo_reg           ,
                     v_c_sum_id_operación	     ,
                     v_c_sum_f_creación        ,
                     v_c_sum_total_registros   ,
                     v_c_sum_tot_imp_ap_pat    ,
                     v_c_sum_tot_imp_am_cre  
 
   
   --Se escribe en el archivo los datos obtenidos del sumario
      CALL v_canal_archivo.WRITE([v_s_sumario])
   
   --Se cierra el archivo
   CALL v_canal_archivo.CLOSE()
   --se envia mensaje de creación de archivo
   CALL fn_mensaje ("Consulta","Se genero el archivo en la ruta de envio.","about")

END FUNCTION       






