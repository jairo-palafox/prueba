--===============================================================
-- Version: 1.0.0
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
###############################################################################

DATABASE safre_viv
GLOBALS
DEFINE
   g_ruta_envio                LIKE seg_modulo.ruta_envio,
   g_nom_archivo               STRING,
   p_folio                     DECIMAL(9,0),
   p_origen_archivo  SMALLINT,
   p_id_referencia   DECIMAL(9,0)
DEFINE v_total_casos, v_total_casos_procesados DYNAMIC ARRAY OF RECORD
      estado  CHAR(1)
     ,casos   INTEGER
   END RECORD
DEFINE g_total_procesados INTEGER
DEFINE v_total_x_procesar INTEGER
DEFINE g_usuario_cod      LIKE seg_usuario.usuario_cod
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

   LET g_usuario_cod = p_usuario_cod
   
   -- consulta seleccion de aclaracion
   CALL fn_consulta_selec_aclara(p_usuario_cod)

END MAIN

{ ======================================================================
Clave: ACLC11
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
       v_inicia              SMALLINT,
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
       v_indice                 DECIMAL (12,0),
        arr_salida_aclara         DYNAMIC ARRAY OF RECORD
          folio                   DECIMAL(9,0), 
          origen_archivo          SMALLINT,     
          id_referencia           DECIMAL(9,0), 
          cve_ent_receptora       CHAR(3),      
          nrp                     CHAR(11),     
          periodo_pago            CHAR(6),      
          folio_sua               DECIMAL(6,0), 
          f_pago                  DATE,         
          id_derechohabiente      DECIMAL(9,0), 
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
          ind_liquidacion         INTEGER,                 
          num_crd_ifv             DECIMAL(10,0),
          f_proceso               DATE ,
          tpo_patron              CHAR(2),
          folio_referencia        DECIMAL(9,0),
          tpo_archivo             INTEGER,                               
          origen_pago             INTEGER,                               
          tpo_afiliacion          INTEGER,                                
          f_actualiza             DATE
        END RECORD,
        v_seleccionado        INTEGER,  --indice del registro seleccionado
        v_desc_origen_pago    VARCHAR(60),  --descripción del origen de pago
        v_consecutivo         SMALLINT,
        v_f_lote              LIKE acl_ctr_lote.f_lote,
        v_lote_cod            SMALLINT,
        v_nom_archivo_aux     LIKE acl_ctr_lote.nombre_archivo,
        v_nss_archivo         CHAR(11),
        ls_cadena             STRING,
        v_cadena              STRING
 

   OPEN WINDOW w_consulta_folio WITH FORM "ACLC111"

   -- se le asigna el apuntado del combo a la variable
   LET v_cbx_folios = ui.ComboBox.forName("formonly.folio")
   LET v_lote_cod = 2
   
   -- se inicia el combobox en blanco
   CALL v_cbx_folios.clear()
   
   -- el primer elemento en el combo es la cadena que indica que se debe elegir
   -- una afore
   --CALL v_cbx_folios.addItem(-1," ")
   
    CONSTRUCT v_condicion 
    ON a.folio, c.nss, a.id_derechohabiente, a.nrp,
       a.periodo_pago, a.folio_sua, a.f_pago,
       a.cve_ent_receptora, b.origen_pago
       
    FROM folio,  nss,id_derechohabiente, nrp, periodo_pago,
        folio_sua,f_pago,cve_ent_receptora, origen_pago
       BEFORE CONSTRUCT
         -- se asignan los valores por omision
         LET v_folio = -1
         
         -- se llena el arreglo de folios
         DECLARE cur_folios CURSOR FOR
         SELECT a.folio
         FROM   glo_folio a
         WHERE  a.proceso_cod = 105 -- salida manual
         AND    a.status > 0
{
         SELECT UNIQUE cta.folio
         FROM cta_his_pagos cta
         WHERE origen_archivo = 1
         AND  folio_referencia IN (
            SELECT UNIQUE folio
            FROM glo_folio a
            ,acl_preliquida b
            ,acl_enaclara_preliquida c
        
            WHERE
               a.folio = b.folio_liquida
            OR
               a.folio = c.folio_liquida
            )
}
         {-- modif 26 JULIO 2012
         DECLARE cur_folios CURSOR FOR
         SELECT a.*
         FROM   glo_folio a,
                acl_sum_aclaracion b --acl_sum_aclaracion
         WHERE  a.folio = b.folio
         }
         {SELECT a.*
         FROM   glo_folio a
         ORDER BY 1}
         --AND
         -- estado = 2 -- integrado

         --llena el combo con los folios
         FOREACH cur_folios INTO v_r_glo_folio.*
            LET v_s_cadena = v_r_glo_folio.folio
            CALL v_cbx_folios.addItem(v_r_glo_folio.folio, v_s_cadena)
         END FOREACH

         FREE cur_folios
 
       ON ACTION ACCEPT
         LET v_folio = GET_FLDBUF(folio)
         LET v_nss = GET_FLDBUF(nss)
         LET v_id_derechohabiente = GET_FLDBUF(id_derechohabiente)
         LET v_nrp = GET_FLDBUF(nrp)
         LET v_periodo_pago = GET_FLDBUF(periodo_pago)
         LET v_folio_sua = GET_FLDBUF(folio_sua)
         LET v_f_pago = GET_FLDBUF(f_pago)
         LET v_origen_pago = GET_FLDBUF(origen_pago)
         LET v_ent_receptora = GET_FLDBUF(cve_ent_receptora)         

         --valida que se seleccione por lomenos un criterio de bsuqueda
         IF v_folio IS NULL AND
            v_nss IS NULL AND
            v_id_derechohabiente IS NULL AND
            v_nrp IS NULL AND
            v_periodo_pago IS NULL AND
            v_folio_sua IS NULL AND
            v_f_pago IS NULL AND
            v_f_pago IS NULL AND
            v_ent_receptora IS NULL THEN
            CALL fn_mensaje("Consulta",
                            "Debe de ingresar un campo de búsqueda",
                            "about")
         ELSE
            ACCEPT CONSTRUCT
         END IF 
         
      ON ACTION cancel
         LET INT_FLAG = TRUE                           
         EXIT CONSTRUCT
   END CONSTRUCT

   IF NOT INT_FLAG THEN     
      --hace el conteo de registros
      LET v_query = "SELECT COUNT(*) \n",
                    "FROM   cta_his_pagos a, pag_ctr_pago b, afi_derechohabiente c \n",
                    "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                    "AND    a.id_derechohabiente = c.id_derechohabiente \n",
                    "AND    a.id_referencia = b.id_referencia \n",
                    "AND   ",v_condicion
       --DISPLAY " v_query ",v_query
      PREPARE prp_count_aclara FROM v_query
      EXECUTE prp_count_aclara INTO v_registros

      IF v_registros IS NULL THEN
        LET v_registros = 0
      END IF

      --valida que se econtrarón registros
      IF v_registros > 0 THEN
        --realizala busqueda para llenar el arreglo
        LET v_query = "SELECT a.*,  \n",
                    --"b.tpo_archivo, b.origen_pago, b.tpo_afiliacion, b.f_actualiza \n",
                    "0, b.estado_pago, 0, b.f_actualiza \n",
                    "FROM   cta_his_pagos a, pag_ctr_pago b, afi_derechohabiente c \n",
                    "WHERE  a.id_derechohabiente = b.id_derechohabiente \n",
                    "AND    a.id_derechohabiente = c.id_derechohabiente \n",
                    "AND    a.id_referencia = b.id_referencia \n",
                    "AND   ",v_condicion
                    --añadir not exist en acl_preliquida
        --DISPLAY v_query
        PREPARE prp_cur_folio FROM v_query
        DECLARE cur_folio CURSOR FOR prp_cur_folio

        LET v_indice = 1
        --llen ael arreglo
        FOREACH cur_folio INTO arr_salida_aclara[v_indice].*
            LET v_indice = v_indice + 1
        END FOREACH

        --elinina ultimo renglon en blanco
        LET v_indice = v_indice - 1
        IF arr_salida_aclara[arr_salida_aclara.getLength()].folio IS NULL THEN
            CALL arr_salida_aclara.deleteElement(arr_salida_aclara.getLength())
        END IF   

        DISPLAY ARRAY arr_salida_aclara TO salida_aclara.* 
        ATTRIBUTES    (ACCEPT = FALSE) 
        
        
        ON ACTION CANCEL
            EXIT DISPLAY
            
        ON ACTION Generar_Reporte
            --Validación de que selecciono un registro
            CALL ARR_CURR( ) RETURNING v_seleccionado
            IF v_seleccionado IS NULL THEN
                CALL fn_mensaje("Consulta",
                                "No se ha seleccionado ningún registro.",
                                "about")  
            ELSE
                -- busca descripción origen de pago original
                SELECT archivo_descripcion 
                INTO   v_desc_origen_pago
                FROM   pag_tpo_archivo
                WHERE  archivo_cod = arr_salida_aclara[v_seleccionado].origen_archivo

                LET v_cadena = "Se gerará el reporte para el código: ",v_desc_origen_pago CLIPPED
                CALL fn_mensaje("Consulta",
                        v_cadena,
                        "about")
                CALL fn_reporte_carga_archivo(arr_salida_aclara[v_seleccionado].folio , arr_salida_aclara[v_seleccionado].*, TRUE, v_desc_origen_pago)
            END IF

        ON ACTION Generar_Archivo
            CALL ARR_CURR( ) RETURNING v_seleccionado
            IF v_seleccionado IS NULL THEN
                CALL fn_mensaje("Consulta",
                                "No se ha seleccionado ningún registro.",
                                "about")  
            ELSE
                --Validación de que selecciono un registro
                LET v_f_lote = TODAY
                SELECT NVL(MAX(consecutivo),0) + 1
                INTO   v_consecutivo
                FROM   acl_ctr_lote
                WHERE  f_lote = v_f_lote
                --DISPLAY "v_consecutivo: ",v_consecutivo

                --Se obtiene la ruta de envio de los archivos
                LET v_query = "\n SELECT ruta_envio         "
                             ,"\n FROM   seg_modulo         "
                             ,"\n WHERE  modulo_cod = 'acl' "
                PREPARE prp_ruta_archivo FROM v_query
                EXECUTE prp_ruta_archivo INTO g_ruta_envio
      
                ##CREACIÖN DEL ARCHIVO
                --Se obtiene el numero de nss
                SELECT a.nss
                INTO   v_nss_archivo
                FROM   afi_Derechohabiente a, cta_his_pagos c
                WHERE  a.id_derechohabiente = c.id_derechohabiente
                AND    c.folio = arr_salida_aclara[v_seleccionado].folio
                AND    c.origen_archivo = arr_salida_aclara[v_seleccionado].origen_archivo
                AND    c.id_referencia = arr_salida_aclara[v_seleccionado].id_referencia

                --DISPLAY "v_nss_archivo: ",v_nss_archivo

                --Formato del nombre del archivo
                LET g_nom_archivo = YEAR(TODAY) USING "&&&&",MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
                                ,arr_salida_aclara[v_seleccionado].folio USING "&&&&&&&&&", v_nss_archivo USING "&&&&&&&&&&&",
                                v_lote_cod USING "&", ".SMA"




                               
                -- se obtiene el folio, origen del archivo y el id de referencia
                LET p_folio          = arr_salida_aclara[v_seleccionado].folio
                LET p_origen_archivo = arr_salida_aclara[v_seleccionado].origen_archivo
                LET p_id_referencia  = arr_salida_aclara[v_seleccionado].id_referencia

                --DISPLAY "origen de archivo y referencia: ", p_origen_archivo, p_id_referencia



                
                CALL fn_display_proceso(0,"GENERAR ARCHIVO DE SALIDA DE ACLARATORIO")

                --Se genera el archivo
                CALL f_genera_archivo()
          
                LET v_nom_archivo_aux = g_nom_archivo
                --Se actualiza el nombre del archivo generado

                INSERT INTO acl_ctr_lote
                VALUES (v_lote_cod, v_f_lote, v_consecutivo, 
                        arr_salida_aclara[v_seleccionado].folio, 
                        arr_salida_aclara[v_seleccionado].origen_archivo, 
                        arr_salida_aclara[v_seleccionado].id_referencia,
                        v_nom_archivo_aux)

                DISPLAY "\n TOTAL DE REGISTROS PROCESADOS:",g_total_procesados

                DISPLAY "\n ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",g_nom_archivo
                LET ls_cadena = "ARCHIVO GENERADO: ",g_ruta_envio CLIPPED,"/",g_nom_archivo CLIPPED
                CALL fn_mensaje("Consulta",
                        ls_cadena,
                        "about")   
                
                CALL fn_display_proceso(1,"GENERAR ARCHIVO DE SALIDA DE ACLARATORIO")
            END IF
        END DISPLAY
      ######      
      ELSE
        CALL fn_mensaje("Consulta",
                        "No existen registros con los criterios dados.",
                        "about")   
      END IF
   --RETURN v_registros
   END IF  
   
   CLOSE WINDOW w_consulta_folio

END FUNCTION


-- OBJETIVO: Obtener los datos necesarios para emitir el reporte de Integración LQINFO
FUNCTION fn_reporte_carga_archivo(p_folio, arr_salida_aclara, p_b_despliegue_pantalla,v_desc_origen_pago)
    DEFINE p_folio                 INTEGER
    DEFINE p_b_despliegue_pantalla SMALLINT -- booleana que indica si el archivo se escribe en disco o se envia a pantalla
    DEFINE v_desc_origen_pago    VARCHAR(60)  --descripción del origen de pago
    DEFINE arr_salida_aclara      RECORD
          folio                   DECIMAL(9,0), 
          origen_archivo          SMALLINT,
          id_referencia           DECIMAL(9,0),
          cve_ent_receptora       CHAR(3),
          nrp                     CHAR(11),
          periodo_pago            CHAR(6),
          folio_sua               DECIMAL(6,0),
          f_pago                  DATE,
          id_derechohabiente      DECIMAL(9,0), 
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
          num_crd_ifv             DECIMAL(10,0),
          f_proceso               DATE ,
          tpo_patron              CHAR(2),
          folio_referencia        DECIMAL(9,0),
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

    DISPLAY "Se envia al reporte folio y folio ref: ", arr_salida_aclara.folio, arr_salida_aclara.folio_referencia
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
          num_crd_ifv             DECIMAL(10,0),
          f_proceso               DATE ,
          tpo_patron              CHAR(2),
          folio_referencia        DECIMAL(9,0),
          tpo_archivo             SMALLINT,
          origen_pago             SMALLINT,
          tpo_afiliacion          SMALLINT,
          f_actualiza             DATE
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
      PRINTX p_desc_origen_pago                                                                    
                                                                                              
   ON EVERY ROW                                                                               
      PRINTX arr_salida_aclara.*
             --arr_salida_aclara.folio_referencia
                                                                                           
END REPORT          

###############################################################################
#Modulo            => ACL                                                     #
#Programa          => ACLS01                                                  #
#Objetivo          => Genera el archivo de mandatos                           #
#Autor             => Francisco López                                         #
#Fecha Inicio      =>                                                         #
#Modifica Ruben Haro Castro  25 de Junio de 2012                              #
###############################################################################
FUNCTION f_genera_archivo()
   --Se definen las variables para el archivo fisico
   DEFINE v_canal_archivo             BASE.CHANNEL
         ,v_query                     STRING
         ,v_string_detalle            STRING
         ,v_string_encabezado         STRING
         ,v_string_sumario            STRING
         ,v_id_lote                   INTEGER
         ,v_lote_x_dia                SMALLINT
         ,v_total_imp_ap_pat          DECIMAL (22,2)
         ,v_total_imp_am_cre          DECIMAL (22,2)
         ,v_total_imp_ren_viv_pgo_ext DECIMAL (22,2)
         ,v_total_imp_aiv_ap_pat      DECIMAL (18,6)
         ,v_total_imp_int_gen_pgo_ext DECIMAL (22,2)
         ,v_total_imp_aiv_gen_pgo_ext DECIMAL (18,6)
         ,v_id_derechohabiente_tmp    DECIMAL (9,0)


   --RECORD con el encabezado del archivo
   DEFINE v_enc RECORD
       tpo_registro         CHAR(2),  --Tipo Registro            X   2   0
       id_operación         CHAR(3),  --Identificador Operación  X   3   0
       f_creación_lote      CHAR(8),  --Fecha Creación Lote      X   8   0
       consecutivo_lote     CHAR(3)  --Consecutivo de Lote      X   3   0
   END RECORD
   
   --RECORD con el detalle del archivo
   DEFINE v_det RECORD
         tpo_registro                  CHAR(2)     , -- X   2   0   Tipo Registro                                                         
         consecutivo_reg_lote          DECIMAL(8,0), -- N   8       Consecutivo Registro dentro del Lote                                  
         cve_ent_receptora             CHAR(3)     , -- X   3   0   Clave de Entidad Receptora                                            
         reg_pat_imss                  CHAR(11)    , -- X   11  0   Registro Patronal IMSS                                                
         rfc_patron                    CHAR(13)    , -- X   13  0   RFC del Patrón                                                        
         per_pago                      DECIMAL(6,0), -- N   6   0   Periodo de Pago                                                       
         folio_sua                     DECIMAL(6,0), -- N   6   0   Folio de Pago SUA                                                     
         nss                           CHAR(11)    , -- X   11  0   Número de Seguriedad Social (NSS)                                     
         rfc_trabajador                CHAR(13)    , -- X   13  0   RFC del Trabajador                                                    
         curp                          CHAR(18)    , -- X   18  0   Clave Única de Registro de Población (CURP)                           
         num_cre_infonavit             LIKE cta_his_pagos.num_crd_ifv   ,     --DECIMAL(10,0)-- N   10  0   Número de Crédito INFONAVIT                                           
         f_inicio_desc_cre_infonavit   LIKE cta_pag_complemento.f_ini_desc_crd_ifv, --DECIMAL(8,0) -- N   8   0   Fecha de inicio del descuento de crédito INFONAVIT                    
         pat_trabajador                CHAR(40) ,    -- X   40  0   Paterno Trabajador                                                    
         mat_trabajador                CHAR(40) ,    -- X   40  0   Materno Trabajador                                                    
         nom_trabajador                CHAR(40) ,    -- X   40  0   Nombre Trabajador                                                     
         tpo_aclaración                CHAR(2)  ,    -- X   2   0   Tipo de Aclaración                                                    
         ult_sal_diario                LIKE cta_pag_complemento.ult_sdi,  --DECIMAL(7,2) -- N   5   2   Último Salario Diario                                                 
         días_coti_bim                 LIKE cta_pag_complemento.dias_cot_bim,    --DECIMAL(2,0) -- N   2   0   Días Cotizados en Bimestre                                            
         días_incapacidad_bim          LIKE cta_pag_complemento.dias_incap_bim,  --DECIMAL(2,0) -- N   2   0   Días Incapacidad en Bimestre                                          
         días_ausentismo_bim           LIKE cta_pag_complemento.dias_ausent_bim ,--DECIMAL(2,0) -- N   2   0   Días Ausentismo en Bimestre                                           
         imp_apo_pat                   LIKE cta_his_pagos.imp_ap_pat ,           --DECIMAL(7,2) -- N   5   2   Importe Aportación Patronal                                           
         imp_amor_cred_infonavit       LIKE cta_his_pagos.imp_am_cre ,           --DECIMAL(7,2) -- N   5   2   Importe Amortización Crédito INFONAVIT                                
         imp_rend_sbcta_viv            LIKE cta_his_pagos.imp_ren_viv_pgo_ext,  --DECIMAL(7,2) -- N   5   2   Importe Rendimientos Subcuenta Vivienda                               
         apl_intereses_viv             LIKE cta_his_pagos.aiv_ap_pat ,          --DECIMAL(15,6) -- N   9   6   Aplicaciones de Intereses Vivienda                                    
         pre_aplicación_int_viv        LIKE cta_his_pagos.valor_aiv  ,          --DECIMAL(11,6) -- N   5   6   Precio Aplicación de Intereses Vivienda                               
         int_gen_pag_exte_viv          LIKE cta_his_pagos.int_gen_pgo_ext,      --DECIMAL(7,2)  -- N   5   2   Intereses Generado por Pagos Extemporaneos de Vivienda                
         apli_inter_viv_pag_ext_viv    LIKE cta_his_pagos.aiv_gen_pgo_ext,      --DECIMAL(11,6) -- N   9   6   Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
         resul_peración                CHAR(2)       -- X   2   0   Resultado de la Operación                                             
   END RECORD
   
   --RECORD con el encabezado del archivo todos en char
   DEFINE v_det_string RECORD
     tpo_registro                CHAR(02)  ,   -- X   2   0   Tipo Registro                                                         
     consecutivo_reg_lote        CHAR(08)  ,   -- N   8       Consecutivo Registro dentro del Lote                                  
     cve_ent_receptora           CHAR(03)  ,   -- X   3   0   Clave de Entidad Receptora                                            
     reg_pat_imss                CHAR(11)  ,   -- X   11  0   Registro Patronal IMSS                                                
     rfc_patron                  CHAR(13)  ,   -- X   13  0   RFC del Patrón                                                        
     per_pago                    CHAR(06)  ,   -- N   6   0   Periodo de Pago                                                       
     folio_sua                   CHAR(06)  ,   -- N   6   0   Folio de Pago SUA                                                     
     nss                         CHAR(11)  ,   -- X   11  0   Número de Seguriedad Social (NSS)                                     
     rfc_trabajador              CHAR(13)  ,   -- X   13  0   RFC del Trabajador                                                    
     curp                        CHAR(18)  ,   -- X   18  0   Clave Única de Registro de Población (CURP)                           
     num_cre_infonavit           CHAR(10)  ,   -- N   10  0   Número de Crédito INFONAVIT                                           
     f_inicio_desc_cre_infonavit CHAR(08)  ,   -- N   8   0   Fecha de inicio del descuento de crédito INFONAVIT                    
     pat_trabajador              CHAR(40)  ,   -- X   40  0   Paterno Trabajador                                                    
     mat_trabajador              CHAR(40)  ,   -- X   40  0   Materno Trabajador                                                    
     nom_trabajador              CHAR(40)  ,   -- X   40  0   Nombre Trabajador                                                     
     tpo_aclaración              CHAR(02)  ,   -- X   2   0   Tipo de Aclaración                                                    
     ult_sal_diario              CHAR(07)  ,   -- N   5   2   Último Salario Diario                                                 
     días_coti_bim               CHAR(02)  ,   -- N   2   0   Días Cotizados en Bimestre                                            
     días_incapacidad_bim        CHAR(02)  ,   -- N   2   0   Días Incapacidad en Bimestre                                          
     días_ausentismo_bim         CHAR(02)  ,   -- N   2   0   Días Ausentismo en Bimestre                                           
     imp_apo_pat                 CHAR(07)  ,   -- N   5   2   Importe Aportación Patronal                                           
     imp_amor_cred_infonavit     CHAR(07)  ,   -- N   5   2   Importe Amortización Crédito INFONAVIT                                
     imp_rend_sbcta_viv          CHAR(07)  ,   -- N   5   2   Importe Rendimientos Subcuenta Vivienda                               
     apl_intereses_viv           CHAR(15)  ,   -- N   9   6   Aplicaciones de Intereses Vivienda                                    
     pre_aplicación_int_viv      CHAR(11)  ,   -- N   5   6   Precio Aplicación de Intereses Vivienda                               
     int_gen_pag_exte_viv        CHAR(07)  ,   -- N   5   2   Intereses Generado por Pagos Extemporaneos de Vivienda                
     apli_inter_viv_pag_ext_viv  CHAR(15)  ,   -- N   9   6   Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
     resul_peración              CHAR(02)     -- X   2   0   Resultado de la Operación                                             
   END RECORD
   
   --RECORD con el sumario del archivo
   DEFINE v_sum RECORD
       tpo_registro                  CHAR(02) ,   --X   2   0   Tipo Registro                                                               
       id_operacion                  CHAR(03) ,   --X   3   0   Identificador Operación                                                     
       f_creación_lote               CHAR(08) ,   --X   8   0   Fecha Creación Lote                                                         
       consecutivo_lote              CHAR(03) ,   --X   3   0   Consecutivo de Lote                                                         
       num_reg__etalle               CHAR(09) ,   --N   9   0   Numero Registros de Detalle                                                 
       tot_imp_apor_pat              CHAR(11) ,   --N   9   2   Total Importe Aportación Patronal                                           
       tot_imp_amor_cred_infonavit   CHAR(11) ,   --N   9   2   Total Importe Amortización Crédito INFONAVIT                                
       tot_imp_rend_sbcta_viv        CHAR(11) ,   --N   9   2   Total Importe Rendimientos Subcuenta Vivienda                               
       tot_apl_int_viv               CHAR(18) ,   --N   12  6   Total Aplicaciones de Intereses  Vivienda                                   
       tot_int_gen_pag_ext_viv       CHAR(11) ,   --N   9   2   Total Intereses Generado por Pagos Extemporaneos de  Vivienda               
       tot_apl_int_viv_pag_extem_viv CHAR(18)    --N   12  6   Total Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda
   END RECORD
   
   
   -- se crea el manejador de archivo
   LET v_canal_archivo = BASE.CHANNEL.CREATE()
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_canal_archivo.openFile(g_ruta_envio CLIPPED||"/"||g_nom_archivo, "w" )
   CALL v_canal_archivo.setDelimiter("")
   LET g_total_procesados = 0
   LET v_id_lote = 0
   LET v_lote_x_dia = 1
   
   LET v_query =  "\n SELECT '02'                                        " --Tipo Registro                                                                     
                 ,"\n       ,''                                          " --Consecutivo Registro dentro del Lote                                              
                 ,"\n       ,''                                          " --Clave de Entidad Receptora                                                        
                 ,"\n       ,''                                          " --Registro Patronal IMSS                                                            
                 ,"\n       ,''                                          " --RFC del Patrón                                                                    
                 ,"\n       ,h.periodo_pago                              " --Periodo de Pago                                                                   
                 ,"\n       ,h.folio_sua                                 " --Folio de Pago SUA                                                                 
                 ,"\n       ,a.nss                                       " --Número de Seguriedad Social (NSS)                                                 
                 ,"\n       ,a.rfc                                       " --RFC del Trabajador                                                                
                 ,"\n       ,a.curp                                      " --Clave Única de Registro de Población (CURP)                                       
                 ,"\n       ,''                                          " --Número de Crédito INFONAVIT                                                       
                 ,"\n       ,''                                          " --Fecha de inicio del descuento de crédito INFONAVIT                                
                 ,"\n       ,a.ap_paterno_af                             " --Paterno Trabajador                                                                
                 ,"\n       ,a.ap_materno_af                             " --Materno Trabajador                                                                
                 ,"\n       ,a.nombre_af                                 " --Nombre Trabajador                                                                 
                 ,"\n       ,p.tpo_aclaracion                            " --Tipo de Aclaración                                                                
                 ,"\n       ,''                                          " --Último Salario Diario                                                             
                 ,"\n       ,''                                          " --Días Cotizados en Bimestre                                                        
                 ,"\n       ,''                                          " --Días Incapacidad en Bimestre                                                      
                 ,"\n       ,''                                          " --Días Ausentismo en Bimestre                                                       
                 ,"\n       ,h.imp_ap_pat                                " --Importe Aportación Patronal                                                       
                 ,"\n       ,h.imp_am_cre                                " --Importe Amortización Crédito INFONAVIT                                            
                 ,"\n       ,0                                           " --Importe Rendimientos Subcuenta Vivienda                                           
                 ,"\n       ,0                                           " --Aplicaciones de Intereses Vivienda                                                
                 ,"\n       ,0                                           " --Precio Aplicación de Intereses Vivienda                                           
                 ,"\n       ,0                                           " --Intereses Generado por Pagos Extemporaneos de Vivienda                            
                 ,"\n       ,0                                           " --Aplicaciones de Intereses Vivienda por Pagos Extemporaneos de Vivienda            
                 ,"\n       ,h.result_operacion                          " --Resultado de la Operación                                                         
                 ,"\n       ,h.id_derechohabiente                        " 
                 ,"\n FROM  cta_his_pagos h                              " 
                 ,"\n      ,pag_ctr_pago p                               " 
                 ,"\n      ,afi_derechohabiente a                        " 
                 ,"\n WHERE h.id_derechohabiente = p.id_derechohabiente  " 
                 ,"\n AND   h.id_derechohabiente = a.id_derechohabiente  "
                 ,"\n AND   h.id_referencia = p.id_referencia            " 
                 ,"\n AND   h.folio = ?                                    " 
                 ,"\n AND   h.origen_archivo = ?                           " 
                 ,"\n AND   h.id_referencia = ?                            " 
                  
   PREPARE prp_datos_archivo FROM v_query
   DECLARE cur_datos_archivo CURSOR FOR prp_datos_archivo
   
   --Se inicializan los contadores en CERO
   LET v_total_imp_ap_pat          = 0
   LET v_total_imp_am_cre          = 0
   LET v_total_imp_ren_viv_pgo_ext = 0
   LET v_total_imp_aiv_ap_pat      = 0
   LET v_total_imp_int_gen_pgo_ext = 0
   LET v_total_imp_aiv_gen_pgo_ext = 0
   
   --Se escribe el encabezado
   LET v_enc.tpo_registro     = "01"
   LET v_enc.id_operación     = "ACL"
   LET v_enc.f_creación_lote  = YEAR(TODAY)  USING "&&&&", MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   LET v_enc.consecutivo_lote = v_lote_x_dia USING "&&&&&&&&&&&&&&&&"
   
   --Se pasan a una sola cadena los datos del encabezado
   LET v_string_encabezado = v_enc.tpo_registro     ,
                             v_enc.id_operación     ,
                             v_enc.f_creación_lote  ,
                             v_enc.consecutivo_lote
   
   --Se escribe en el archivo los datos obtenidos del encabezado
   CALL v_canal_archivo.WRITE([v_string_encabezado])

   --Se obtienen los datos del detalle con el query
   FOREACH cur_datos_archivo USING p_folio, p_origen_archivo, p_id_referencia
                             INTO v_det.*, v_id_derechohabiente_tmp

      --DISPLAY "Se encontro un registro para detalle"
      --DISPLAY v_det.*
                             
      --Se obtienen los detalles segun el origen del archivo
      CASE p_origen_archivo
         WHEN 1 --LQINFO
            --Se obtienen solo los datos que contiene LQINFO
            LET v_query =  "\n   SELECT  ctp.nrp                       "
                          ,"\n          ,ctc.rfc_patron                "
                          ,"\n          ,ctp.num_crd_ifv               "
                          ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                          ,"\n          ,ctc.ult_sdi                   "
                          ,"\n          ,ctc.dias_cot_bim              "
                          ,"\n          ,ctc.dias_incap_bim            "
                          ,"\n          ,ctc.dias_ausent_bim           "
                          ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                          ,"\n          ,ctp.aiv_ap_pat                "
                          ,"\n          ,ctp.valor_aiv                 "
                          ,"\n          ,ctp.int_gen_pgo_ext           "
                          ,"\n          ,ctp.aiv_gen_pgo_ext           "
                          ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                          ,"\n    WHERE ctp.folio  = ctc.folio  "
                          ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                          ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                          ,"\n      AND ctp.folio = ?   "
                          ,"\n     AND  ctp.id_derechohabiente = ? "
                          ,"\n     AND  ctp.id_referencia = ?      "
                         
            PREPARE prp_detalle_1 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_1 USING p_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO v_det.reg_pat_imss                ,
                                     v_det.rfc_patron                  ,
                                     v_det.num_cre_infonavit           , 
                                     v_det.f_inicio_desc_cre_infonavit , 
                                     v_det.ult_sal_diario              ,
                                     v_det.días_coti_bim               ,
                                     v_det.días_incapacidad_bim        ,                             
                                     v_det.días_ausentismo_bim         ,
                                     v_det.imp_rend_sbcta_viv          ,
                                     v_det.apl_intereses_viv           ,
                                     v_det.pre_aplicación_int_viv      ,
                                     v_det.int_gen_pag_exte_viv        ,
                                     v_det.apli_inter_viv_pag_ext_viv 
            FREE prp_detalle_1
         WHEN 2 --SAR92
            --Se obtienen solo los datos que contiene SAR92
            LET v_query = "\n SELECT rfc_patron            "
                         ,"\n FROM  pag_det_sar92          "
                         ,"\n WHERE folio = ?              "
                         ,"\n AND   id_referencia = ?      "
            PREPARE prp_detalle_2 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_2 USING p_folio, p_id_referencia
                                INTO  v_det.rfc_patron

                                
            FREE prp_detalle_2
         WHEN 3 --SOLO INFONAVIT
            --Se obtienen solo los datos que contiene SOLO INFONAVIT
            LET v_query = "\n SELECT nrp                   "
                         ,"\n FROM  pag_det_trab_sifv      "
                         ,"\n WHERE folio = ?              "
                         ,"\n AND   id_derechohabiente = ? "
                         ,"\n AND   id_referencia = ?      "
            PREPARE prp_detalle_3 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_3 USING p_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO  v_det.reg_pat_imss
            FREE prp_detalle_3
         WHEN 4 --CARGA INICIAL ACLARATORIO
         WHEN 5 --ACLARATORIO SIN CAMBIO NSS
            --Se obtienen solo los datos que contiene ACLARATORIO SIN CAMBIO NSS
            LET v_query = "\n   SELECT  ctp.nrp                       "
                         ,"\n          ,ctc.rfc_patron                "
                         ,"\n          ,ctp.num_crd_ifv               "
                         ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                         ,"\n          ,ctc.ult_sdi                   "
                         ,"\n          ,ctc.dias_cot_bim              "
                         ,"\n          ,ctc.dias_incap_bim            "
                         ,"\n          ,ctc.dias_ausent_bim           "
                         ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                         ,"\n          ,ctp.aiv_ap_pat                "
                         ,"\n          ,ctp.valor_aiv                 "
                         ,"\n          ,ctp.int_gen_pgo_ext           "
                         ,"\n          ,ctp.aiv_gen_pgo_ext           "
                         ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                         ,"\n    WHERE ctp.folio  = ctc.folio  "
                         ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                         ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                         ,"\n      AND ctp.folio = ?   "
                         ,"\n     AND  ctp.id_derechohabiente = ? "
                         ,"\n     AND  ctp.id_referencia = ?      "
                         
                         
            PREPARE prp_detalle_5 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_5 USING p_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO v_det.reg_pat_imss                ,     
                                     v_det.rfc_patron                  ,     
                                     v_det.num_cre_infonavit           ,     
                                     v_det.f_inicio_desc_cre_infonavit ,     
                                     v_det.ult_sal_diario              ,     
                                     v_det.días_coti_bim               ,     
                                     v_det.días_incapacidad_bim        ,     
                                     v_det.días_ausentismo_bim         ,     
                                     v_det.imp_rend_sbcta_viv          ,     
                                     v_det.apl_intereses_viv           ,     
                                     v_det.pre_aplicación_int_viv      ,     
                                     v_det.int_gen_pag_exte_viv        ,     
                                     v_det.apli_inter_viv_pag_ext_viv        
            FREE prp_detalle_5 
         WHEN 6 --ACLARATORIO CON CAMBIO NSS
            --Se obtienen solo los datos que contiene ACLARATORIO CON CAMBIO NSS
            LET v_query = "\n   SELECT  ctp.nrp                       "
                         ,"\n          ,ctc.rfc_patron                "
                         ,"\n          ,ctp.num_crd_ifv               "
                         ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                         ,"\n          ,ctc.ult_sdi                   "
                         ,"\n          ,ctc.dias_cot_bim              "
                         ,"\n          ,ctc.dias_incap_bim            "
                         ,"\n          ,ctc.dias_ausent_bim           "
                         ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                         ,"\n          ,ctp.aiv_ap_pat                "
                         ,"\n          ,ctp.valor_aiv                 "
                         ,"\n          ,ctp.int_gen_pgo_ext           "
                         ,"\n          ,ctp.aiv_gen_pgo_ext           "
                         ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                         ,"\n    WHERE ctp.folio  = ctc.folio  "
                         ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                         ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                         ,"\n      AND ctp.folio = ?   "
                         ,"\n     AND  ctp.id_derechohabiente = ? "
                         ,"\n     AND  ctp.id_referencia = ?      "
                         
            PREPARE prp_detalle_6 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_6 USING p_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO v_det.reg_pat_imss                ,   
                                     v_det.rfc_patron                  ,   
                                     v_det.num_cre_infonavit           ,   
                                     v_det.f_inicio_desc_cre_infonavit ,   
                                     v_det.ult_sal_diario              ,   
                                     v_det.días_coti_bim               ,   
                                     v_det.días_incapacidad_bim        ,   
                                     v_det.días_ausentismo_bim         ,   
                                     v_det.imp_rend_sbcta_viv          ,   
                                     v_det.apl_intereses_viv           ,   
                                     v_det.pre_aplicación_int_viv      ,   
                                     v_det.int_gen_pag_exte_viv        ,   
                                     v_det.apli_inter_viv_pag_ext_viv      
            FREE prp_detalle_6
         WHEN 7 --ACLARATORIO CON CAMBIO NOMBRE
         WHEN 8 --ENACLARA
            --Se obtienen solo los datos que contiene ENACLARA
            LET v_query ="\n   SELECT  ctp.nrp                       "
                         ,"\n          ,ctc.rfc_patron                "
                         ,"\n          ,ctp.num_crd_ifv               "
                         ,"\n          ,ctc.f_ini_desc_crd_ifv        "
                         ,"\n          ,ctc.ult_sdi                   "
                         ,"\n          ,ctc.dias_cot_bim              "
                         ,"\n          ,ctc.dias_incap_bim            "
                         ,"\n          ,ctc.dias_ausent_bim           "
                         ,"\n          ,ctp.imp_ren_viv_pgo_ext       "
                         ,"\n          ,ctp.aiv_ap_pat                "
                         ,"\n          ,ctp.valor_aiv                 "                       
                         ,"\n     FROM cta_his_pagos  ctp, cta_pag_complemento ctc "
                         ,"\n    WHERE ctp.folio  = ctc.folio  "
                         ,"\n      AND ctp.id_derechohabiente =  ctc.id_derechohabiente  "
                         ,"\n      AND ctp.id_referencia      =  ctc.id_referencia "
                         ,"\n      AND ctp.folio = ?   "
                         ,"\n     AND  ctp.id_derechohabiente = ? "
                         ,"\n     AND  ctp.id_referencia = ?      "
                         
                         
            PREPARE prp_detalle_8 FROM v_query
            --Ejecutamos la consulta
            EXECUTE prp_detalle_8 USING p_folio, v_id_derechohabiente_tmp, p_id_referencia
                                INTO v_det.reg_pat_imss                ,  
                                     v_det.rfc_patron                  ,  
                                     v_det.num_cre_infonavit           ,  
                                     v_det.f_inicio_desc_cre_infonavit ,  
                                     v_det.ult_sal_diario              ,  
                                     v_det.días_coti_bim               ,  
                                     v_det.días_incapacidad_bim        ,  
                                     v_det.días_ausentismo_bim         ,  
                                     v_det.imp_rend_sbcta_viv          ,  
                                     v_det.apl_intereses_viv           
                               
                               
                               
        FREE prp_detalle_8
      END CASE
      
      
      --Se realiza la suma de los importes
      LET v_total_imp_ap_pat          = v_total_imp_ap_pat          + v_det.imp_apo_pat                
      LET v_total_imp_am_cre          = v_total_imp_am_cre          + v_det.imp_amor_cred_infonavit    
      LET v_total_imp_ren_viv_pgo_ext = v_total_imp_ren_viv_pgo_ext + v_det.imp_rend_sbcta_viv         
      LET v_total_imp_aiv_ap_pat      = v_total_imp_aiv_ap_pat      + v_det.apl_intereses_viv          
      LET v_total_imp_int_gen_pgo_ext = v_total_imp_int_gen_pgo_ext + v_det.int_gen_pag_exte_viv    
      LET v_total_imp_aiv_gen_pgo_ext = v_total_imp_aiv_gen_pgo_ext + v_det.apli_inter_viv_pag_ext_viv      
      LET v_id_lote = v_id_lote + 1                                           
      LET g_total_procesados          = g_total_procesados + 1
      
      
      --Se pasan los detalles a su equivalente en tipo char
      LET v_det_string.tpo_registro                  = v_det.tpo_registro
      LET v_det_string.consecutivo_reg_lote          = v_det.consecutivo_reg_lote   USING "&&&&&&&&"
      LET v_det_string.cve_ent_receptora             = v_det.cve_ent_receptora  
      LET v_det_string.reg_pat_imss                  = v_det.reg_pat_imss       
      LET v_det_string.rfc_patron                    = v_det.rfc_patron         
      LET v_det_string.per_pago                      = v_det.per_pago           
      LET v_det_string.folio_sua                     = v_det.folio_sua          
      LET v_det_string.nss                           = v_det.nss               
      LET v_det_string.rfc_trabajador                = v_det.rfc_trabajador    
      LET v_det_string.curp                          = v_det.curp              
      LET v_det_string.num_cre_infonavit             = v_det.num_cre_infonavit 
      LET v_det_string.f_inicio_desc_cre_infonavit   = YEAR(v_det.f_inicio_desc_cre_infonavit) USING "&&&&", MONTH(v_det.f_inicio_desc_cre_infonavit) USING "&&",DAY(v_det.f_inicio_desc_cre_infonavit) USING "&&"
      LET v_det_string.pat_trabajador                = v_det.pat_trabajador
      LET v_det_string.mat_trabajador                = v_det.mat_trabajador
      LET v_det_string.nom_trabajador                = v_det.nom_trabajador
      LET v_det_string.tpo_aclaración                = v_det.tpo_aclaración
      LET v_det_string.ult_sal_diario                =(v_det.ult_sal_diario * 100) USING "&&&&&&&"
      LET v_det_string.días_coti_bim                 = v_det.días_coti_bim
      LET v_det_string.días_incapacidad_bim          = v_det.días_incapacidad_bim
      LET v_det_string.días_ausentismo_bim           = v_det.días_ausentismo_bim
      LET v_det_string.imp_apo_pat                   =(v_det.imp_apo_pat * 100) USING "&&&&&&&"   
      LET v_det_string.imp_amor_cred_infonavit       =(v_det.imp_amor_cred_infonavit * 100) USING "&&&&&&&"   
      LET v_det_string.imp_rend_sbcta_viv            =(v_det.imp_rend_sbcta_viv * 100) USING "&&&&&&&"   
      LET v_det_string.apl_intereses_viv             =(v_det.apl_intereses_viv * 1000000) USING "&&&&&&&&&&&&&&&"
      LET v_det_string.pre_aplicación_int_viv        =(v_det.pre_aplicación_int_viv * 1000000) USING "&&&&&&&&&&&"
      LET v_det_string.int_gen_pag_exte_viv          =(v_det.int_gen_pag_exte_viv * 100) USING "&&&&&&&"   
      LET v_det_string.apli_inter_viv_pag_ext_viv    =(v_det.apli_inter_viv_pag_ext_viv * 1000000) USING "&&&&&&&&&&&&&&&"
      LET v_det_string.resul_peración                = v_det.resul_peración
      
      --Se asigna todo lo obtenido a una sola cadena que sera escrita en el archivo
      LET v_string_detalle = v_det_string.tpo_registro                 
                            ,v_det_string.consecutivo_reg_lote         
                            ,v_det_string.cve_ent_receptora            
                            ,v_det_string.reg_pat_imss                 
                            ,v_det_string.rfc_patron                   
                            ,v_det_string.per_pago                     
                            ,v_det_string.folio_sua                    
                            ,v_det_string.nss                          
                            ,v_det_string.rfc_trabajador               
                            ,v_det_string.curp                         
                            ,v_det_string.num_cre_infonavit            
                            ,v_det_string.f_inicio_desc_cre_infonavit  
                            ,v_det_string.pat_trabajador               
                            ,v_det_string.mat_trabajador               
                            ,v_det_string.nom_trabajador               
                            ,v_det_string.tpo_aclaración               
                            ,v_det_string.ult_sal_diario               
                            ,v_det_string.días_coti_bim                
                            ,v_det_string.días_incapacidad_bim         
                            ,v_det_string.días_ausentismo_bim          
                            ,v_det_string.imp_apo_pat                  
                            ,v_det_string.imp_amor_cred_infonavit      
                            ,v_det_string.imp_rend_sbcta_viv           
                            ,v_det_string.apl_intereses_viv            
                            ,v_det_string.pre_aplicación_int_viv       
                            ,v_det_string.int_gen_pag_exte_viv         
                            ,v_det_string.apli_inter_viv_pag_ext_viv   
                            ,v_det_string.resul_peración               
      
      --Se escribe en el archivo los datos obtenidos
      CALL v_canal_archivo.WRITE([v_string_detalle])

      --Se dejan los arrays como nulos
      INITIALIZE v_det TO NULL
      INITIALIZE v_det_string TO NULL
   END FOREACH
   
   --Se obtienen los registros del sumario
   LET v_sum.tpo_registro                     = "09"
   LET v_sum.id_operacion                     = "ACL"
   LET v_sum.f_creación_lote                  = YEAR(TODAY)  USING "&&&&", MONTH(TODAY) USING "&&",DAY(TODAY) USING "&&"
   LET v_sum.consecutivo_lote                 = v_lote_x_dia USING "&&&"
   LET v_sum.num_reg__etalle                  = g_total_procesados USING "&&&&&&&&&"
   LET v_sum.tot_imp_apor_pat                 = (v_total_imp_ap_pat * 100) USING "&&&&&&&&&&&"
   LET v_sum.tot_imp_amor_cred_infonavit      = (v_total_imp_am_cre * 100) USING "&&&&&&&&&&&"
   LET v_sum.tot_imp_rend_sbcta_viv           = (v_total_imp_ren_viv_pgo_ext * 100)     USING "&&&&&&&&&&&"
   LET v_sum.tot_apl_int_viv                  = (v_total_imp_aiv_ap_pat * 1000000)      USING "&&&&&&&&&&&&&&&&&&"
   LET v_sum.tot_int_gen_pag_ext_viv          = (v_total_imp_int_gen_pgo_ext * 100)     USING "&&&&&&&&&&&"
   LET v_sum.tot_apl_int_viv_pag_extem_viv    = (v_total_imp_aiv_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&&&&&&"
   
   LET v_string_sumario = v_sum.tpo_registro                    
                         ,v_sum.id_operacion                    
                         ,v_sum.f_creación_lote                 
                         ,v_sum.consecutivo_lote                
                         ,v_sum.num_reg__etalle                 
                         ,v_sum.tot_imp_apor_pat                
                         ,v_sum.tot_imp_amor_cred_infonavit     
                         ,v_sum.tot_imp_rend_sbcta_viv          
                         ,v_sum.tot_apl_int_viv                 
                         ,v_sum.tot_int_gen_pag_ext_viv         
                         ,v_sum.tot_apl_int_viv_pag_extem_viv   
   
   --Se escribe en el archivo los datos obtenidos del sumario
      CALL v_canal_archivo.WRITE([v_string_sumario])
   
   --Se cierra el archivo
   CALL v_canal_archivo.CLOSE()

END FUNCTION