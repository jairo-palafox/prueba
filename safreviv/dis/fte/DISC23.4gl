################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC23                                                    #
#Objetivo         => Programa de consulta de Facturaci�n por Concepto de Pago  # 
#Fecha de Inicio  => 03/06/2015                                                #
################################################################################
-- Base que se utilizar�
DATABASE safre_viv
-- Definici�n de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30)  --Almacena opci�n del men�
 
  --Datos de salida
  DEFINE a_datos_apo_sub     DYNAMIC ARRAY OF RECORD  
    tpo_credito              CHAR(30),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2)
  END RECORD 

  --Totales 
  DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(22,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2), --Total de aportaciones
    v_tot_reg_cred           DECIMAL(9,0)   --Total de registros

  DEFINE v_indice            SMALLINT
  DEFINE v_estado            SMALLINT

  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING,
    p_proceso_cod            SMALLINT

END GLOBALS

MAIN
  --Datos de entrada
  DEFINE 
    v_folio_liquida          DECIMAL(9,0),--Folio de liquidaci�n de la dispersi�n
    v_f_liquida_ini          DATE,        --Fecha de liquidaci�n inicial
    v_f_liquida_fin          DATE         --Fecha de liquidacion final   
 
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, --Define las prop�edades de la Ventana
    f_forma                  ui.Form    --Define las propiedades de la forma  

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  LET p_proceso_cod = 3904

  --Validaci�n que NO se tenga alguna operaci�n de Dispersi�n de Pagos ejecut�ndose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", p_proceso_cod,
                  "AND    a.cod_proceso_valida = b.proceso_cod ",
                  "AND    a.cod_convivencia    = 0             ",
                  "ORDER BY cod_proceso_valida   "
  PREPARE ps_val_proc FROM g_sql_txt
  DECLARE cur_val_proc CURSOR FOR ps_val_proc
  FOREACH cur_val_proc INTO v_proc_entra,
                            v_proc_val,
                            v_desc_proc_val
    IF f_existe_proceso_operacion_ejecutando(v_proc_val, "") THEN
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecut�ndose,\ningrese a esta opci�n cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC231"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          CALL f_forma.setElementHidden("gr_tot_aivs", 1)         --Oculta el total de aivs
          CALL f_forma.setElementHidden("lbl_totales", 1)         --Oculta etiqueta Totales

          NEXT FIELD v_folio_liquida
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           --Valida que se inserte al menos un par�metro
           IF (v_folio_liquida IS NULL AND v_f_liquida_ini IS NULL AND v_f_liquida_fin IS NULL) THEN
              CALL fn_mensaje("ATENCI�N",
                              "Debe capturar un Folio de Operaci�n o un Periodo de Fechas de Operaci�n.",
                              "about")
              NEXT FIELD v_folio_liquida   
           END IF   

           IF v_f_liquida_ini > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Operaci�n inicial no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_liquida_fin > TODAY THEN 
              CALL fn_mensaje("ATENCI�N",
                              "La Fecha de Operaci�n Final no puede ser mayor al D�a de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                              "Debe capturar una Fecha de Operaci�n Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 

           IF v_f_liquida_fin IS NOT NULL AND v_f_liquida_ini IS NULL THEN 
              CALL fn_mensaje("ATENCI�N", 
                             "Debe capturar una Fecha de Operaci�n Inicial.",
                             "about")
              NEXT FIELD v_f_liquida_fin
           END IF 

           IF (v_f_liquida_ini > v_f_liquida_fin) THEN
              CALL fn_mensaje("ATENCI�N", 
                              "La Fecha Inicial de Operaci�n no puede ser mayor a la Fecha Final de Operaci�n.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF

           IF (v_f_liquida_fin < v_f_liquida_ini) THEN
              CALL fn_mensaje("ATENCI�N", 
                              "La Fecha Final de Operaci�n no puede ser menor a la Fecha Inicial de Operaci�n.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF          

           CALL fn_consultar(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)            
          
           IF v_indice > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)          --muestra detalle de la consulta
              --CALL f_forma.setElementHidden("gr_tot_registros", 0)    --muestra el total de registros
              --CALL f_forma.setElementHidden("gr_tot_aportaciones", 0) --muestra el total de aportaciones
              --CALL f_forma.setElementHidden("gr_tot_aivs", 0)         --muestra el total de aivs
              CALL f_forma.setElementHidden("lbl_totales", 0)         --muestra etiqueta Totales

              DISPLAY ARRAY a_datos_apo_sub TO rec_detalle.* ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE DISPLAY                 
                  --DISPLAY v_tot_registros  TO txt_tot_registros
                  --DISPLAY v_tot_aivs       TO txt_tot_aivs
                  --DISPLAY v_tot_aportacion TO txt_tot_aportaciones
            
                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)                  
                  CALL DIALOG.setActionHidden("btn_liquidar", 0)                  
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)
                
                ON ACTION archivo
                   CALL fn_genera_archivo(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)   
              END DISPLAY                    
           ELSE
             IF (v_folio_liquida IS NOT NULL) THEN
                LET v_estado = 0;

                SELECT UNIQUE dc.estado
                INTO   v_estado
                FROM   dis_ctr_aps_tns dc
                WHERE  dc.folio_liquida = v_folio_liquida
                AND    dc.estado       IN (70)
                IF v_estado = 70 THEN
                   CALL fn_mensaje("ATENCI�N",
                                   "El Folio de Operaci�n ya fue Pagado.",
                                   "about")
                   CALL ui.interface.refresh()
                ELSE
                  SELECT UNIQUE dc.estado
                  INTO   v_estado
                  FROM   dis_ctr_aps_tns dc
                  WHERE  dc.folio_liquida = v_folio_liquida
                  AND    dc.estado       IN (60)
                  IF v_estado = 60 THEN
                     CALL fn_mensaje("ATENCI�N",
                                     "El Folio de Operaci�n est� en Tr�mite de Pago.",
                                     "about")
                     CALL ui.interface.refresh()
                  ELSE
                    SELECT UNIQUE dc.estado
                    INTO   v_estado
                    FROM   dis_ctr_aps_tns dc
                    WHERE  dc.folio_liquida = v_folio_liquida
                    AND    dc.estado       IN (55)
                    IF v_estado = 55 THEN
                       CALL fn_mensaje("ATENCI�N",
                                       "El Folio de Operaci�n ya fue Provisionado.",
                                       "about")
                       CALL ui.interface.refresh()
                    ELSE
                      SELECT UNIQUE dc.estado
                      INTO   v_estado
                      FROM   dis_ctr_aps_tns dc
                      WHERE  dc.folio_liquida = v_folio_liquida
                      AND    dc.estado       IN (30)
                      IF v_estado = 30 THEN
                         CALL fn_mensaje("ATENCI�N",
                                         "El Folio de Operaci�n ya fue Facturado.",
                                         "about")
                         CALL ui.interface.refresh()
                      ELSE
                        SELECT UNIQUE dc.estado
                        INTO   v_estado
                        FROM   dis_ctr_aps_tns dc
                        WHERE  dc.folio_liquida = v_folio_liquida
                        AND    dc.estado       IN (20)
                        IF v_estado = 20 THEN
                           CALL fn_mensaje("ATENCI�N",
                                           "El Folio de Operaci�n ya fue Confirmado.",
                                           "about")
                           CALL ui.interface.refresh()
                        ELSE
                          LET v_tot_reg_cred = 0

                          SELECT COUNT(*)
                          INTO   v_tot_reg_cred
                          FROM   dis_ctr_aps_tns dc
                          WHERE  dc.folio_liquida = v_folio_liquida
                          AND    dc.tpo_credito  IN (2,5)
                          IF v_tot_reg_cred = 0 THEN
                             CALL fn_mensaje("ATENCI�N",
                                             "El Folio de Operaci�n no es de un concepto de pago Apoyo INFONAVIT o COFINAVIT.",
                                             "about")
                             CALL ui.interface.refresh()
                          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
             ELSE
               CALL fn_mensaje("ATENCI�N",
                               "No se encontraron registros.",
                               "about")
               CALL ui.interface.refresh()
             END IF   
           END IF           
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
    END DIALOG 
  CLOSE WINDOW w1
END MAIN 

FUNCTION fn_consultar(p_folio_liquida, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_folio_liquida     DECIMAL(9,0)--Folio de liquidaci�n de la dispersi�n
  DEFINE p_f_liquida_ini     DATE        --Fecha de liquidaci�n inicial
  DEFINE p_f_liquida_fin     DATE        --Fecha de liquidacion final   
  DEFINE v_desc_credito      CHAR(50)
  DEFINE v_tpo_credito       SMALLINT 

  LET g_sql_txt = "\n SELECT dc.tpo_credito, ",
                  "\n        COUNT(dc.id_dis_interface_ef), ",
                  "\n        SUM(dc.aiv_ap_pat), ",
                  "\n        SUM(dc.imp_ap_pat) ",
                  "\n FROM   dis_ctr_aps_tns dc, ", 
                  "\n        glo_folio gf ",
                  "\n WHERE  gf.folio       = dc.folio_liquida ",
                  "\n AND   (dc.estado      = 10 ", 
                  "\n AND    gf.status      = 2 ",    --Verificar el status en glo_folio
                  "\n AND    gf.proceso_cod = 932 ",  --Falta modificar el proceso_cod
                  "\n OR     dc.estado      = 10 ", 
                  "\n AND    gf.proceso_cod = 1217) " --Falta modificar el proceso_cod
                                              
  IF p_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dc.folio_liquida = ",p_folio_liquida
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dc.f_liquida   >= '",p_f_liquida_ini,"'",
                               "\n AND dc.f_liquida   <= '",p_f_liquida_fin,"'"
  END IF
  
  LET g_sql_txt = g_sql_txt,"\n GROUP BY dc.tpo_credito ",
                            "\n ORDER BY dc.tpo_credito "
                
  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE pr_sl_inf FROM g_sql_txt
  DECLARE cur_sl_inf CURSOR FOR pr_sl_inf
  
  LET v_indice           = 1
  LET v_tot_registros    = 0
  LET v_tot_aivs         = 0.00
  LET v_tot_aportacion   = 0.00
  
  FOREACH cur_sl_inf INTO v_tpo_credito,    
                          a_datos_apo_sub[v_indice].tot_registros,
                          a_datos_apo_sub[v_indice].aiv_ap_pat,
                          a_datos_apo_sub[v_indice].imp_ap_pat                                                        
    
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = v_tpo_credito
    AND    a.ind_activo      = 1

    LET a_datos_apo_sub[v_indice].tpo_credito = v_tpo_credito, " - ", v_desc_credito    
                 
    LET v_tot_registros   = v_tot_registros    + a_datos_apo_sub[v_indice].tot_registros         
    LET v_tot_aivs        = v_tot_aivs         + a_datos_apo_sub[v_indice].aiv_ap_pat
    LET v_tot_aportacion  = v_tot_aportacion   + a_datos_apo_sub[v_indice].imp_ap_pat     
    LET v_indice          = v_indice + 1 
  END FOREACH

  FREE cur_sl_inf
  
  CALL a_datos_apo_sub.deleteElement(v_indice)
  LET v_indice    = v_indice - 1  
END FUNCTION  

FUNCTION fn_reporte(p_folio_liquida, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_folio_liquida     DECIMAL(9,0), 
         p_f_liquida_ini     DATE, 
         p_f_liquida_fin     DATE
  
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  
  LET v_rep_indice = 1

  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC231.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO  a_datos_apo_sub.getLength()
        OUTPUT TO REPORT rep_con_disp(p_folio_liquida, 
                                      p_f_liquida_ini, 
                                      p_f_liquida_fin,
                                      a_datos_apo_sub[v_rep_indice].*,
                                      v_tot_registros,
                                      v_tot_aivs,
                                      v_tot_aportacion, 
                                      g_usuario)
    END FOR
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Cr�dito igual a Cero
REPORT rep_con_disp(p_folio_liquida, 
                    p_f_liquida_ini, 
                    p_f_liquida_fin,
                    v_datos_apo_sub, 
                    v_rep_tot_registros, 
                    v_rep_sum_aivs, 
                    v_rep_sum_aportacion, 
                    v_usuario)

  DEFINE p_folio_liquida     DECIMAL(9,0), 
         p_f_liquida_ini     DATE, 
         p_f_liquida_fin     DATE
                    
  DEFINE v_datos_apo_sub     RECORD  
    tpo_credito              CHAR(30),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2)
  END RECORD                        
  
  DEFINE 
    v_fecha_consulta         DATE,         --Fecha de proceso
    v_usuario                VARCHAR(30),  --Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), --Total de registros
    v_rep_sum_aivs           DECIMAL(22,6),
    v_rep_sum_aportacion     DECIMAL(12,2)      
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX p_folio_liquida
      PRINTX p_f_liquida_ini USING "dd-mm-yyyy"
      PRINTX p_f_liquida_fin USING "dd-mm-yyyy"
                    
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

    PAGE HEADER
      LET v_fecha_consulta = TODAY

      PRINTX p_folio_liquida
      PRINTX p_f_liquida_ini USING "dd-mm-yyyy"
      PRINTX p_f_liquida_fin USING "dd-mm-yyyy"
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
   
    ON EVERY ROW
       PRINTX v_datos_apo_sub.tpo_credito
       PRINTX v_datos_apo_sub.tot_registros USING "###,###,###,###,##&.##"
       PRINTX v_datos_apo_sub.aiv_ap_pat    USING "###,###,###,##&.######"  
       PRINTX v_datos_apo_sub.imp_ap_pat    USING "###,###,###,###,##&.##"       

    ON LAST ROW
       PRINTX v_rep_tot_registros
       PRINTX v_rep_sum_aivs
       PRINTX v_rep_sum_aportacion
END REPORT

FUNCTION fn_genera_archivo(v_folio_liquida, v_f_liquida_ini, v_f_liquida_fin)
  DEFINE 
    v_folio_liquida          DECIMAL(9,0), --Folio de liquidaci�n de la dispersi�n
    v_f_liquida_ini          DATE,         --Fecha de liquidaci�n inicial
    v_f_liquida_fin          DATE          --Fecha de liquidacion final

  DEFINE  
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(40),
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_recorre_arreglo        INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_titulos                STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_folio                  DECIMAL(9,0)  --Folio liquidaci�n dispersi�n
       
  DEFINE
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 INTEGER

  DEFINE arr_info_apo        DYNAMIC ARRAY OF RECORD
    v_folio_liquida          DECIMAL(9,0),
    v_f_liquida              DATE, 
    v_nss                    CHAR(11), 
    v_nombre_af              CHAR(30),
    v_ap_paterno_af          CHAR(30), 
    v_ap_materno_af          CHAR(30), 
    v_periodo                CHAR(6), 
    v_f_pago                 DATE,
    v_folio_sua              DECIMAL(6,0), 
    v_nrp                    CHAR(11),
    v_aportacion             DECIMAL(8,2),
    v_aivs                   DECIMAL(22,6), 
    v_tpo_credito            CHAR(20),
    v_f_entrega              DATE, 
    v_interface              CHAR(2)
  END RECORD 

  DEFINE v_aportacion        CHAR(10)
  DEFINE v_desc_credito      CHAR(20) 

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/fact_conc_pago_", v_hora

  --se obtienen la ruta envio del m�dulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --se crea el manejador de archivo y se indica que se escribir� en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc23;
  WHENEVER ERROR STOP

  PREPARE eje_prio FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT H.id_dis_interface_ef, ",
                  "\n        A.id_derechohabiente,  ",
                  "\n        A.nss,                 ",
                  "\n        A.nombre_af,           ",
                  "\n        A.ap_paterno_af,       ",
                  "\n        A.ap_materno_af        ",
                  "\n FROM   dis_ctr_aps_tns H,     ",
                  "\n        afi_derechohabiente A, ",
                  "\n        glo_folio gf           ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente ",
                  "\n AND    gf.folio               = H.folio_liquida ", 
                  "\n AND   (H.estado               = 10 ",
                  "\n AND    gf.status              = 2 ", 
                  "\n AND    gf.proceso_cod         = 932 ",
                  "\n OR     H.estado               = 10 ",
                  "\n AND    gf.proceso_cod         = 1217) "

  IF v_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND H.folio_liquida = ",v_folio_liquida
  END IF

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND H.f_liquida  >= '",v_f_liquida_ini,"'",
                               "\n AND H.f_liquida  <= '",v_f_liquida_fin,"'" 
  END IF

  LET g_sql_txt = g_sql_txt, "\n INTO TEMP tmp_afi_disc23 "

  PREPARE prep_tmp_afi FROM g_sql_txt
  EXECUTE prep_tmp_afi

  UPDATE STATISTICS FOR TABLE tmp_afi_disc23

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT dca.folio_liquida, ",
                  "\n        dca.f_liquida, ",
                  "\n        ad.nss, ",
                  "\n        ad.nombre_af, ",
                  "\n        ad.ap_paterno_af, ",
                  "\n        ad.ap_materno_af, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.tpo_credito, ",
                  "\n        TODAY as f_entrega, ",
                  "\n        'AS' as interface ",
                  --"\n FROM   afi_derechohabiente ad, ",
                  --"\n        dis_ctr_aps_tns dca, ",
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_disc23 ad ",                 
                  --"\n        glo_folio gf ",   
                  "\n WHERE  dca.id_dis_interface_ef = ad.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = ad.id_derechohabiente " 
                  --"\n AND    gf.folio               = dca.folio_liquida ", 
                  --"\n AND   (dca.estado             = 10 ",
                  --"\n AND    gf.status              = 2 ", 
                  --"\n AND    gf.proceso_cod         = 932 ",
                  --"\n OR     dca.estado             = 10 ",
                  --"\n AND    gf.proceso_cod         = 1217) "

  IF v_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dca.folio_liquida = ",v_folio_liquida

    --Imprime encabezado del archivo
     LET v_encabezado = "FOLIO: ",v_folio_liquida
     CALL v_ch_arch_salida.write([v_encabezado])     
  END IF 

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",v_f_liquida_ini,"'",
                               "\n AND dca.f_liquida  <= '",v_f_liquida_fin,"'" 

     LET v_encabezado = "PERIODO FECHAS: ",v_f_liquida_ini USING "dd-mm-yyyy", " - ", v_f_liquida_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF
  
  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.folio_liquida, dca.f_liquida, ad.nss, dca.periodo_pago DESC, dca.f_pago DESC "  
  
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc
  
  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0
  
  --Imprime encabezado del archivo
  LET v_titulos = "FOLIO OPERACI�N |FECHA OPERACI�N |NSS |NOMBRE |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACI�N |CONCEPTO PAGO |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  FOREACH cur_sl_inf_arc INTO arr_info_apo[v_indice].v_folio_liquida,
                              arr_info_apo[v_indice].v_f_liquida,
                              arr_info_apo[v_indice].v_nss,     
                              arr_info_apo[v_indice].v_nombre_af,
                              arr_info_apo[v_indice].v_ap_paterno_af,
                              arr_info_apo[v_indice].v_ap_materno_af, 
                              arr_info_apo[v_indice].v_periodo, 
                              arr_info_apo[v_indice].v_f_pago,
                              arr_info_apo[v_indice].v_folio_sua,
                              arr_info_apo[v_indice].v_nrp,
                              arr_info_apo[v_indice].v_aportacion,
                              arr_info_apo[v_indice].v_aivs,
                              arr_info_apo[v_indice].v_tpo_credito,
                              arr_info_apo[v_indice].v_f_entrega,
                              arr_info_apo[v_indice].v_interface
                         
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = arr_info_apo[v_indice].v_tpo_credito
    AND    a.ind_activo      = 1

    IF arr_info_apo[v_indice].v_tpo_credito = 3 THEN
       LET arr_info_apo[v_indice].v_interface = 'UG'
    END IF

    LET v_detalle =  arr_info_apo[v_indice].v_folio_liquida, "|",
                     arr_info_apo[v_indice].v_f_liquida USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_nss, "|",
                     arr_info_apo[v_indice].v_nombre_af CLIPPED," ", arr_info_apo[v_indice].v_ap_paterno_af CLIPPED," ", arr_info_apo[v_indice].v_ap_materno_af CLIPPED, "|",
                     arr_info_apo[v_indice].v_periodo CLIPPED, "|",
                     arr_info_apo[v_indice].v_f_pago USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_folio_sua, "|",
                     arr_info_apo[v_indice].v_nrp , "|",
                     arr_info_apo[v_indice].v_aivs USING "#,###,##&.######", "|",
                     arr_info_apo[v_indice].v_aportacion USING "#,###,##&.##", "|",
                     arr_info_apo[v_indice].v_tpo_credito CLIPPED, "-", v_desc_credito CLIPPED, "|",
                     arr_info_apo[v_indice].v_f_entrega USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].v_interface, "|"

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_aivs       = v_tot_aivs       + arr_info_apo[v_indice].v_aivs
    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].v_aportacion
    LET v_indice         = v_indice + 1 
  END FOREACH
 
  FREE cur_sl_inf_arc
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice        = v_indice - 1

  LET v_tot_registros =  v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | | | |",
                                v_tot_aivs USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Informaci�n","Se ha generado el archivo de Consulta de Facturaci�n por Concepto de Pago \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION