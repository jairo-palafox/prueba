################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 13/06/2012                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC06                                                   #
#Objetivo          => Consultar y generar reporte de las diferencias de montos #
#                     entre el proceso de aportaciones subsecuentes y el pro-  #
#                     ceso de registro de pagos                                #
#Fecha inicio      => 15/02/2012                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose

DATABASE safre_viv

GLOBALS
  DEFINE g_proceso_cod       LIKE glo_ctr_archivo.folio
  DEFINE g_desc_edo_arch     VARCHAR(23)
  DEFINE g_desc_edo_oper     VARCHAR(44)
  DEFINE g_sum_imp_apo_pat   DECIMAL(22,2)--LIKE dis_ap_subsecuente.imp_apo_pat
  DEFINE g_sum_imp_amo_cred  DECIMAL(22,2)--LIKE dis_ap_subsecuente.imp_amo_cred
  DEFINE g_imp_apo_pat       DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_imp_apo_pat
  DEFINE g_imp_amo_cred      DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_imp_amo_cred
  DEFINE g_tot_reg           DECIMAL(22,2)--LIKE dis_ctr_ap_subsecuente.tot_registros
  DEFINE g_tot_reg_dif       INTEGER
  DEFINE g_indice            SMALLINT
  DEFINE g_tot_pag           DECIMAL(22,2)
  DEFINE g_tot_sub           DECIMAL(22,2)
  DEFINE g_tot_dif           DECIMAL(22,2)
  
  DEFINE arr_dif_apo         DYNAMIC ARRAY OF RECORD
    nss                      LIKE dis_ap_subsecuente.nss,
    folio_pago               LIKE dis_ap_subsecuente.folio_liquida,
    f_pago                   LIKE dis_ap_subsecuente.f_pago,
    periodo_pago             LIKE dis_ap_subsecuente.periodo_pago,
    folio_sua                DECIMAL(6,0),
    nrp                      CHAR(11),
    num_credito              DECIMAL(10,0),
    estado                   CHAR(53),
    apo_pat_pag              DECIMAL(22,2),--LIKE dis_ap_subsecuente.imp_apo_pat,
    --apo_pat_sub              DECIMAL(22,2),--LIKE dis_interface_ef.imp_ap_pat,
    aiv_apo_pat_sub          DECIMAL(22,2),--LIKE dis_interface_ef.imp_ap_pat,
    apo_pat_dif              DECIMAL(22,2)
  END RECORD

  DEFINE arr_ctr_subsecuente DYNAMIC ARRAY OF RECORD 
    v_folio                  DECIMAL(9,0),
    v_f_transferencia        DATE,
    v_tot_reg                DECIMAL(10,0),
    v_tot_aivs               DECIMAL(22,2),
    v_tot_apo                DECIMAL(22,2),
    v_tot_amo                DECIMAL(22,2),
    v_estado                 VARCHAR(40)
  END RECORD 

  DEFINE arr_detalle_folio   DYNAMIC ARRAY OF RECORD 
    v_folio                  DECIMAL(9,0),
    v_folio_liquida          DECIMAL(9,0),
    v_ind_liquida            VARCHAR(40),
    v_total_apo              DECIMAL(22,2),
    v_total_reg              DECIMAL(10,0)
  END RECORD 

  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    manejador_rpt            om.SaxDocumentHandler  -- Contenedor de Documentos para el reporte

  DEFINE 
    rp_folio                 DECIMAL(9,0),
    rp_f_transferencia       DATE,
    rp_total_reg_ctr         DECIMAL(10,0),
    rp_total_apo             DECIMAL(22,2),
    rp_total_aivs            DECIMAL(22,2),
    rp_total_amo             DECIMAL(22,2),
    rp_estado                VARCHAR(40),
    rp_folio_liquida         DECIMAL(9,0),
    rp_indicador             VARCHAR(20),
    rp_total_apo_det         DECIMAL(22,2),
    rp_total_reg_det         DECIMAL(10,0),
    rp_descripcion           VARCHAR(80)
      
END GLOBALS

MAIN
  --Sección de Variables Locales
  DEFINE 
    v_folio_carga_ap_sub     LIKE dis_ctr_ap_subsecuente.folio,--Folio de Carga de Archivo de Aportaciones Subsecuentes
    v_fecha_trans            LIKE dis_ctr_ap_subsecuente.f_transferencia, -- Fecha liquidada
    
    v_nom_prog               VARCHAR(30)
    
  --Sección de Parámetros
  DEFINE 
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING -- Titulo de la ventana

  --Seccion de Variables de Retorno
  DEFINE r_folio_valido      SMALLINT
  DEFINE r_fecha_valida      DATE
  DEFINE 
    f_ventana                ui.Window,   -- Define las propìedades de la Ventana
    f_forma                  ui.Form,     -- Define las propiedades de la forma
    v_descripcion            STRING 
    
  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  LET v_nom_prog       = ARG_VAL(4)

   --validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
   IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
      MENU "No se puede ejecutar"
         ATTRIBUTES ( STYLE="dialog", COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
         IMAGE="information" )
         ON ACTION salir
            RETURN
      END MENU
   END IF

  LET v_fecha_trans    = ""
  LET g_proceso_cod    = 904

  -- si se obtuvo el titulo, se pone como titulo de programa
  IF (p_s_titulo IS NOT NULL) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  CLOSE WINDOW SCREEN    

  OPEN WINDOW vtn_cons_aportaciones WITH FORM "DISC061"   
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_carga_ap_sub, v_fecha_trans
      FROM  f_folio_ap_subs, f_fecha
      
        BEFORE INPUT 
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
             
          CALL f_forma.setElementHidden("gr_dif_apo",TRUE) --Oculta seccion de Aportaciones
          CALL f_forma.setElementHidden("gr_control",TRUE) --Oculta sección de control
          CALL f_forma.setElementHidden("gr_detalle_folio",TRUE) --Oculta sección de detalle
          CALL f_forma.setElementHidden("gr_descripcion",TRUE) --Oculta etiqueta de descripción 
   
      END INPUT 
        
      ON ACTION cancelar
         EXIT PROGRAM 

      ON ACTION ACCEPT
         IF v_folio_carga_ap_sub IS NULL AND v_fecha_trans IS NULL THEN
            CALL fn_mensaje("ATENCIÓN",
                            "No ha capturado ningún criterio de busqueda",
                            "about")
            NEXT FIELD f_fecha
         ELSE
            -- Valida que la fecha capturada no sea mayor a la fecha actual
            IF v_fecha_trans > TODAY THEN
               CALL fn_mensaje("ERROR","Fecha posterior a fecha actual","about")
               NEXT FIELD f_fecha
            END IF
            
            --Valida que el folio capturado exista en la tabla de historico
            CALL fn_valida_folio_ap_sub(v_folio_carga_ap_sub,v_fecha_trans)
            RETURNING r_folio_valido,r_fecha_valida

            -- Si el folio no existe en el histórico envia mensaje 
            IF r_folio_valido = 0 OR  r_folio_valido IS NULL  THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "No hay datos con los parámetros capturados",
                               "about")
               NEXT FIELD f_folio_ap_subs
            ELSE               
               DISPLAY "Folio: ",r_folio_valido
               
               -- Si el folio capturado existe en el histórico ejecuta la consulta
               CALL fn_consulta_aportacion(r_folio_valido)--RETURNING arr_dif_apo
               CALL fn_obtiene_detalle_folio(r_folio_valido)

               DISPLAY "longitud ",arr_dif_apo.getLength()
               
               DISPLAY ARRAY arr_ctr_subsecuente TO scr_control.* 
               ATTRIBUTES ( ACCEPT  = FALSE, CANCEL = FALSE  )
               
                 BEFORE DISPLAY
                   --Muestra secciones
                   CALL f_forma.setElementHidden("gr_control",FALSE) --Muestra sección de control
                   CALL f_forma.setElementHidden("gr_detalle_folio",FALSE) --Muestra sección de detalle
                   CALL f_forma.setElementHidden("gr_descripcion",FALSE) --Muestra etiqueta de de

                   --Despliega segundo array
                   DISPLAY ARRAY arr_detalle_folio TO scr_detalle.* 
                     BEFORE DISPLAY 
                       DISPLAY "INT_FLAG detalle: ",INT_FLAG
                       IF NOT INT_FLAG THEN 
                          ACCEPT DISPLAY 
                          EXIT DISPLAY  
                       END IF 
                   END DISPLAY

                   DISPLAY "INT_FLAG control: ",INT_FLAG
                   IF arr_dif_apo.getLength() < 1 THEN
                      LET rp_descripcion = "Folio Conciliado sin diferencias en montos."
                      CALL f_forma.setElementText("lbl_descripcion","Folio Conciliado sin diferencias en montos.")
                      EXIT DISPLAY  
                   ELSE 
                      DISPLAY ARRAY arr_dif_apo TO scr_aportaciones.* ATTRIBUTES ( ACCEPT  = FALSE )
                        BEFORE DISPLAY 
                          LET  rp_descripcion = "Folio conciliado con diferencias en montos."
                          CALL f_forma.setElementHidden("gr_dif_apo",FALSE) --Muestra seccion de Aportaciones
                          CALL f_forma.setElementText("lbl_descripcion","Folio conciliado con diferencias en montos.")
                          
                          ON ACTION reporte
                             CALL fn_genera_reporte_apo_subs(r_folio_valido,r_fecha_valida)

                          ON ACTION CANCEL 
                             EXIT PROGRAM   
                      END DISPLAY
                   END IF 

                   ON ACTION CANCEL
                      EXIT PROGRAM     
                        
               END DISPLAY              
            END IF
         END IF
   
    END DIALOG  
  CLOSE WINDOW vtn_cons_aportaciones 
END MAIN

FUNCTION fn_genera_reporte_apo_subs(f_folio,f_fecha)
  DEFINE 
    v_inicia                 SMALLINT,
    f_folio                  DECIMAL(9,0),
    f_fecha                  DATE 

  --Se asigna la plantilla para generar el reporte
    --IF fgl_report_loadCurrentSettings("arch_aport_subs.4rp") THEN
    IF fgl_report_loadCurrentSettings("DISC062.4rp") THEN  
       CALL fgl_report_selectDevice ("PDF")
       LET manejador_rpt = fgl_report_commitCurrentSettings()
    END IF

    --Inicia el reporte de registros con diferencia en Apo Sub
    START REPORT reporte_apo_sub TO XML HANDLER manejador_rpt

      -- Asigna el titulo del reporte
      FOR v_inicia = 1 TO arr_dif_apo.getLength()
          OUTPUT TO REPORT reporte_apo_sub(p_usuario_cod,
                                           f_folio,
                                           f_fecha,
                                           g_desc_edo_oper,
                                           g_desc_edo_arch,
                                           g_sum_imp_apo_pat,
                                           g_sum_imp_amo_cred,
                                           g_imp_apo_pat,
                                           g_imp_amo_cred,
                                           g_tot_reg,
                                           arr_dif_apo[v_inicia].*,
                                           g_tot_reg_dif,
                                           g_tot_pag,
                                           g_tot_sub,
                                           g_tot_dif)
      END FOR                                                                
    FINISH REPORT reporte_apo_sub 
         
END FUNCTION 

#OBJETIVO: Validar que el folio exista en tabla de proceso de aportaciones subsecuentes
FUNCTION fn_valida_folio_ap_sub(p_folio,p_fecha)
  DEFINE 
    p_folio                  DECIMAL(9,0),
    p_fecha                  DATE,
    v_folio_valido           LIKE dis_ctr_ap_subsecuente.folio,
    v_fecha_valida           DATE,
    v_qry_txt                STRING

  LET v_qry_txt = "\n SELECT MAX(folio),f_transferencia",
                  "\n FROM dis_ctr_ap_subsecuente",
                  "\n WHERE 1 = 1",
                  "\n AND estado <> 40"

  IF length(p_folio) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND folio = ",p_folio
  END IF

  IF length(p_fecha) > 0 THEN
     LET v_qry_txt = v_qry_txt||"\n AND f_transferencia = '",p_fecha,"'"
  END IF

  LET v_qry_txt = v_qry_txt||"\n GROUP BY 2"
  DISPLAY v_qry_txt

  PREPARE prp_valida_folio_liquidado FROM v_qry_txt
  EXECUTE prp_valida_folio_liquidado INTO v_folio_valido,v_fecha_valida

  RETURN v_folio_valido, v_fecha_valida

END FUNCTION 

#OBJETIVO: Ejecutar la función de consulta de aportaciones subsecuentes
FUNCTION fn_consulta_aportacion(p_folio)
  DEFINE 
    p_folio                  LIKE dis_ctr_ap_subsecuente.folio,--Folio de Carga de Archivo de Aportaciones Subsecuentes
    v_QryTxt                 STRING     -- Cadena para almacenar Query 

  DEFINE v_edo_archivo       LIKE glo_ctr_archivo.estado
  DEFINE v_desc_edo_ar       LIKE cat_edo_archivo.estado_descripcion 
  DEFINE v_edo_control       LIKE dis_ctr_ap_subsecuente.estado
  DEFINE v_desc_edo_op       LIKE cat_edo_ap_subsecuente.desc_edo_apo_sub
  DEFINE v_fecha_ctr         DATE 

  LET g_tot_reg_dif = 0
  LET g_tot_pag     = 0.00
  LET g_tot_sub     = 0.00
  LET g_tot_dif     = 0.00

  DISPLAY "Folio -- ",p_folio

  --Consulta para detalles
  LET v_QryTxt = "\n SELECT dd.nss, ",
                 "\n        dd.folio_liquida, ",
                 "\n        dd.f_pago, ", 
                 "\n        dd.periodo_pago, ",
                 "\n        di.folio_sua, ", 
                 "\n        di.nrp, ", 
                 "\n        di.num_crd_ifv,",
                 "\n        dd.estado||'-'||ce.desc_edo_apo_sub,",                 
                 "\n        dd.apl_apo_pat, ",
                 "\n        di.aiv_ap_pat, ",
                 "\n        SUM(dd.apl_apo_pat - di.aiv_ap_pat)",
                 "\n   FROM dis_ap_subsecuente dd,",
                 "\n        cat_edo_ap_subsecuente ce,",
                 "\n        dis_interface_ef       di", 
                 "\n  WHERE dd.id_derechohabiente = di.id_derechohabiente",
                 "\n    AND dd.folio_sua          = di.folio_sua",
                 "\n    AND dd.periodo_pago       = di.periodo_pago",
                 "\n    AND dd.f_pago             = di.f_pago",
                 "\n    AND dd.reg_pat_imss       = di.nrp",
                 "\n    AND dd.estado             = ce.cod_edo_apo_sub",
                 "\n    AND dd.folio              = ", p_folio,
                 "\n    group by 1,2,3,4,5,6,7,8,9,10 ",
                 "\n    having SUM(dd.apl_apo_pat - di.aiv_ap_pat) <> 0.00"
  DISPLAY "v_QryTxt---- ",v_QryTxt

  PREPARE prp_dif_ap FROM v_QryTxt
   
  ##################################################
  ##Obtiene datos de la seccion de cifras globales##
  ##################################################
  --Obtiene estado y descripción del archivo             
  SELECT estado 
  INTO   v_edo_archivo 
  FROM   glo_ctr_archivo 
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = p_folio   

  SELECT estado_descripcion 
  INTO   v_desc_edo_ar 
  FROM   cat_edo_archivo 
  WHERE  estado_cod =  v_edo_archivo 

  LET g_desc_edo_arch = v_edo_archivo||'-'|| v_desc_edo_ar
  DISPLAY "Nombre del archivo ",g_desc_edo_arch

  --Obtiene la descripción del estado de la operación
  SELECT estado 
  INTO   v_edo_control
  FROM   dis_ctr_ap_subsecuente
  WHERE  folio = p_folio

  SELECT desc_edo_apo_sub 
  INTO   v_desc_edo_op
  FROM   cat_edo_ap_subsecuente
  WHERE  cod_edo_apo_sub = v_edo_control

  LET g_desc_edo_oper = v_edo_control||'-'||v_desc_edo_op CLIPPED

  --Obtiene cifras globales detalle
  SELECT SUM(apl_apo_pat), SUM(imp_amo_cred)
  INTO   g_sum_imp_apo_pat, g_sum_imp_amo_cred
  FROM   dis_ap_subsecuente
  WHERE  folio = p_folio 

  --Obtiene cifras globales sumari 
  SELECT tot_imp_apo_aivs,
         tot_imp_amo_cred,
         tot_registros,
         f_transferencia
  INTO   g_imp_apo_pat,
         g_imp_amo_cred,
         g_tot_reg,
         v_fecha_ctr
  FROM   dis_ctr_ap_subsecuente
  WHERE  folio = p_folio 
  
  #####################################################
  ##Obtiene datos de la seccion de detalle y consulta##
  #####################################################
  LET g_indice = 1

  -- Declara el cursor para la consulta
  DECLARE cur_dif_apo CURSOR FOR prp_dif_ap
  FOREACH cur_dif_apo INTO arr_dif_apo[g_indice].*
    LET g_tot_pag     = g_tot_pag     + arr_dif_apo[g_indice].apo_pat_pag
    LET g_tot_sub     = g_tot_sub     + arr_dif_apo[g_indice].aiv_apo_pat_sub
    LET g_tot_dif     = g_tot_dif     + arr_dif_apo[g_indice].apo_pat_dif
    LET g_tot_reg_dif = g_tot_reg_dif + 1
    LET g_indice      = g_indice      + 1
  END FOREACH

  CALL arr_dif_apo.deleteElement(g_indice)
  --RETURN arr_dif_apo
END FUNCTION

--Muestra la información general del archivo de aportactiones subsecuentes
FUNCTION fn_obtiene_detalle_folio(f_folio)
  DEFINE   
    f_folio                  DECIMAL (10,0),
    v_tipo_liquida           SMALLINT 

  --Obtiene información de control
  SELECT   a.folio, 
           f_transferencia, 
           a.tot_registros, 
           a.tot_imp_apo_aivs, 
           a.tot_imp_apo_pat, 
           a.tot_imp_amo_cred,
           a.estado || '-' || b.desc_edo_apo_sub
  INTO     arr_ctr_subsecuente[1].*
  FROM     dis_ctr_ap_subsecuente a,
           cat_edo_ap_subsecuente b
  WHERE    a.folio  = f_folio
  AND      a.estado = b.cod_edo_apo_sub;


  
  SELECT   a.folio,
           a.folio_liquida,
           b.ind_liquidacion,
           SUM (a.apl_apo_pat) apo,
           COUNT(*) tot
  INTO     arr_detalle_folio[1].v_folio,
           arr_detalle_folio[1].v_folio_liquida,
           v_tipo_liquida,
           arr_detalle_folio[1].v_total_apo,
           arr_detalle_folio[1].v_total_reg
  FROM     dis_ap_subsecuente a,
           dis_interface_ef b
  WHERE    a.folio               = f_folio
  AND      a.folio_liquida       = b.folio_liquida
  AND      a.id_dis_interface_ef = b.id_dis_interface_ef
  GROUP BY 1,2,3
  ORDER BY 1,2,3

  IF v_tipo_liquida = 0 THEN 
     LET arr_detalle_folio[1].v_ind_liquida = v_tipo_liquida,"-","Liquidado"
  END IF

  IF v_tipo_liquida = 1 THEN 
     LET arr_detalle_folio[1].v_ind_liquida = v_tipo_liquida,"-","Conciliado"
  END IF 

  IF v_tipo_liquida = 2 THEN 
     LET arr_detalle_folio[1].v_ind_liquida = v_tipo_liquida,"-","Conciliado con Diferencias"
  END IF 

  --Asigna variables para reporte
  LET rp_folio           = arr_ctr_subsecuente[1].v_folio
  LET rp_f_transferencia = arr_ctr_subsecuente[1].v_f_transferencia
  LET rp_total_reg_ctr   = arr_ctr_subsecuente[1].v_tot_reg
  LET rp_total_apo       = arr_ctr_subsecuente[1].v_tot_apo
  LET rp_total_aivs      = arr_ctr_subsecuente[1].v_tot_aivs
  LET rp_total_amo       = arr_ctr_subsecuente[1].v_tot_amo
  LET rp_estado          = arr_ctr_subsecuente[1].v_estado
  LET rp_folio_liquida   = arr_detalle_folio[1].v_folio_liquida
  LET rp_indicador       = arr_detalle_folio[1].v_ind_liquida
  LET rp_total_apo_det   = arr_detalle_folio[1].v_total_apo
  LET rp_total_reg_det   = arr_detalle_folio[1].v_total_reg
   
END FUNCTION 

--Muestra el resumen del folio de aportaciones subsecuentes
FUNCTION fn_obtiene_resumen_folio(f_folio)
  DEFINE   
    f_folio  DECIMAL (10,0) 

END FUNCTION 

#OBJETIVO: Genera el reporte de diferencias
REPORT reporte_apo_sub(p_usuario_cod,
                       p_folio_carga_ap_sub,
                       p_fecha_trans,
                       p_estado_operacion,
                       p_desc_edo_arch,
                       p_tot_imp_apo_pat,
                       p_tot_imp_amo_cred,
                       p_imp_apo_pat_sum,
                       p_imp_amo_cred_sum,
                       p_sum_tot_reg,
                       p_arr_dif_apo,
                       p_tot_dif,
                       v_tot_pag,
                       v_tot_sub,
                       v_tot_dif)
                       
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, 
    p_folio_carga_ap_sub     VARCHAR(20),
    p_fecha_trans            DATE,
    p_desc_edo_arch          CHAR(23),
    p_estado_operacion       CHAR(44),
    p_tot_imp_apo_pat        DECIMAL(22,2),--LIKE dis_ap_subsecuente.imp_apo_pat,
    p_tot_imp_amo_cred       DECIMAL(22,2),--LIKE dis_ap_subsecuente.imp_amo_cred,
    p_tot_dif                SMALLINT,
    v_fecha_reporte          DATE,
    p_folio_pago             DECIMAL(10,0),
    p_imp_apo_pat_sum        LIKE dis_ctr_ap_subsecuente.tot_imp_apo_pat,
    p_imp_amo_cred_sum       LIKE dis_ctr_ap_subsecuente.tot_imp_amo_cred,
    p_sum_tot_reg            LIKE dis_ctr_ap_subsecuente.tot_registros

  DEFINE p_arr_dif_apo       RECORD    
    nss                      LIKE dis_ap_subsecuente.nss,
    folio_pago               LIKE dis_ap_subsecuente.folio_liquida,
    f_pago                   LIKE dis_ap_subsecuente.f_pago,
    periodo_pago             LIKE dis_ap_subsecuente.periodo_pago,
    folio_sua                DECIMAL (6,0),
    nrp                      CHAR (11),
    num_credito              DECIMAL (10,0),
    estado                   CHAR(53), 
    aiv_apo_pat_pag          DECIMAL(22,6),
    apo_pat_sub              DECIMAL(22,2),
    apo_pat_dif              DECIMAL(22,2)
  END RECORD

  DEFINE v_tot_pag           DECIMAL(22,2)
  DEFINE v_tot_sub           DECIMAL(22,2)
  DEFINE v_tot_dif           DECIMAL(22,2)

  FORMAT
 
    FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY CLIPPED

      --Imprime los montos del encabezado y cifras globales      
      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_usuario_cod
      PRINTX p_folio_carga_ap_sub
      PRINTX p_fecha_trans   USING "dd-mm-yyyy"
      PRINTX p_estado_operacion
      PRINTX p_desc_edo_arch
      PRINTX p_tot_imp_apo_pat
      PRINTX p_tot_imp_amo_cred
      PRINTX p_sum_tot_reg
      PRINTX p_folio_pago
      PRINTX p_imp_apo_pat_sum
      PRINTX p_imp_amo_cred_sum

      PRINTX rp_folio
      PRINTX rp_f_transferencia
      PRINTX rp_total_reg_ctr
      PRINTX rp_total_apo
      PRINTX rp_total_aivs
      PRINTX rp_total_amo
      PRINTX rp_estado
      PRINTX rp_folio_liquida
      PRINTX rp_indicador
      PRINTX rp_total_apo_det
      PRINTX rp_total_reg_det
      PRINTX rp_descripcion

    ON EVERY ROW
       --Imprime los detalles de las diferencias
       PRINTX p_arr_dif_apo.nss
       PRINTX p_arr_dif_apo.folio_pago
       PRINTX p_arr_dif_apo.f_pago USING "dd-mm-yyyy"
       PRINTX p_arr_dif_apo.periodo_pago
       PRINTX p_arr_dif_apo.estado
       PRINTX p_arr_dif_apo.aiv_apo_pat_pag
       PRINTX p_arr_dif_apo.apo_pat_sub
       PRINTX p_arr_dif_apo.apo_pat_dif
       PRINTX p_arr_dif_apo.folio_sua
       PRINTX p_arr_dif_apo.nrp
       PRINTX p_arr_dif_apo.num_credito

    ON LAST ROW
       --Imprime las sumatorias
       PRINTX v_tot_pag
       PRINTX v_tot_sub
       PRINTX v_tot_dif
       PRINTX p_tot_dif

END REPORT