################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC24                                                    #
#Objetivo         => Programa de consulta de Facturación por Transacción       #
#Fecha de Inicio  => 09/06/2015                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

-- Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30)  --Almacena opción del menú
 
  --Datos de salida
  DEFINE a_datos_apo_sub     DYNAMIC ARRAY OF RECORD 
    concepto                 CHAR(50),
    ent_financiera           CHAR(50),
    tpo_credito              CHAR(30),
    estado                   CHAR(50),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2)
    END RECORD 

   DEFINE a_datos_apo_sub_rep DYNAMIC ARRAY OF RECORD 
    concepto                 CHAR(50),
    ent_financiera           CHAR(50),
    tpo_credito              CHAR(30),
    estado                   CHAR(50),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2)
    END RECORD 
    
   DEFINE a_datos_apo_sub_rep_dev DYNAMIC ARRAY OF RECORD 
    concepto                 CHAR(50),
    ent_financiera           CHAR(50),
    tpo_credito              CHAR(30),
    estado                   CHAR(50),
    tot_registros            DECIMAL(9,0),    
    aiv_ap_pat               DECIMAL(22,6),    
    imp_ap_pat               DECIMAL(12,2)
    END RECORD 

  --Totales 
  DEFINE 
    v_tot_registros          DECIMAL(9,0),  --Total de registros
    v_tot_aivs               DECIMAL(22,6), --Total de AIVS
    v_tot_aportacion         DECIMAL(12,2), --Total de aportaciones
    v_tot_registros_dev      DECIMAL(9,0),  --Total de registros
    v_tot_aivs_dev           DECIMAL(18,6), --Total de AIVS
    v_tot_aportacion_dev     DECIMAL(12,2)  --Total de aportaciones
         
  DEFINE 
    v_indice                 INTEGER

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
    v_folio_liquida          DECIMAL(9,0), --Folio de liquidación de la dispersión
    v_f_liquida_ini          DATE,         --Fecha de liquidación inicial
    v_f_liquida_fin          DATE          --Fecha de liquidacion final   

  DEFINE 
    v_cb_estado              SMALLINT    

  --DEFINE cb                  ui.ComboBox
  DEFINE 
    v_ind_llena_cb           SMALLINT
  
  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form    --Define las propiedades de la forma

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET p_proceso_cod  = 3904

  --Se asigna el titulo del programa
  IF ( g_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(g_nom_prog)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
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
       LET v_mensaje_val = "Proceso ", v_desc_proc_val CLIPPED, " ejecutándose,\ningrese a esta opción cuando finalice."
       MENU "No se puede ejecutar" 
         ATTRIBUTES ( STYLE="dialog",
         COMMENT= v_mensaje_val,
         IMAGE="information" )

         ON ACTION salir
            RETURN
       END MENU
    END IF
  END FOREACH

  LET bnd_consulta    = 0
  LET v_folio_liquida = 0
  LET v_cb_estado     = 0
  LET v_f_liquida_ini = ""
  LET v_f_liquida_fin = ""
  LET v_ind_llena_cb  = 0

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISC241"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_folio_liquida, v_cb_estado,
                    v_f_liquida_ini, v_f_liquida_fin
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL fn_llenar_combo_estado() RETURNING v_cb_estado, v_ind_llena_cb

          LET v_cb_estado = v_cb_estado

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_tot_registros", 1)    --Oculta el total de registros
          CALL f_forma.setElementHidden("gr_tot_aportaciones", 1) --Oculta el total de aportaciones
          CALL f_forma.setElementHidden("gr_tot_aivs", 1)         --Oculta el total de aivs
          --CALL f_forma.setElementHidden("lbl_totales", 1)         --Oculta etiqueta Totales

          NEXT FIELD v_folio_liquida
          CALL ui.interface.refresh()

        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (v_folio_liquida IS NULL AND v_cb_estado IS NULL AND
               v_f_liquida_ini IS NULL AND v_f_liquida_fin IS NULL) THEN
               CALL fn_mensaje("ATENCIÓN",
                               "No ha capturado ningún criterio de búsqueda.",
                               "about")
               NEXT FIELD v_folio_liquida   
           END IF   

           IF v_f_liquida_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Operación Inicial no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF    

           IF v_f_liquida_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Operación Final no puede ser mayor al Día de Hoy.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 
          
           IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Operación Final.",
                              "about")
              NEXT FIELD v_f_liquida_fin
           END IF 

           IF v_f_liquida_fin IS NOT NULL AND v_f_liquida_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Operación Inicial.",
                              "about")
              NEXT FIELD v_f_liquida_ini
           END IF 

           IF (v_f_liquida_ini > v_f_liquida_fin) THEN
              CALL fn_mensaje("ATENCIÓN", 
                              "La Fecha Inicial de Operación no puede ser mayor a la Fecha Final de Operación.",
                              "about")
              NEXT FIELD v_f_liquida_ini 
           END IF  

           --DISPLAY "v_folio_liquida: -",v_folio_liquida,"-"

           --LET v_f_liquida_ini = v_f_liquida_ini USING "dd-mm-yyyy"
           --LET v_f_liquida_fin = v_f_liquida_fin USING "dd-mm-yyyy"

           --DISPLAY "v_f_liquida_ini: --",v_f_liquida_ini,"-"
           --DISPLAY "v_f_liquida_fin: --",v_f_liquida_fin,"-"          

           CALL fn_consultar(v_folio_liquida, v_cb_estado, v_f_liquida_ini, v_f_liquida_fin)  

           IF v_indice > 0 THEN 
              CALL f_forma.setElementHidden("gr_detalle", 0)          --Muestra detalle de la consulta
              --CALL f_forma.setElementHidden("gr_tot_registros", 0)    --Muestra el total de registros
              --CALL f_forma.setElementHidden("gr_tot_aportaciones", 0) --Muestra el total de aportaciones
              --CALL f_forma.setElementHidden("gr_tot_aivs", 0)         --Muestra el total de aivs
              --CALL f_forma.setElementHidden("lbl_totales", 0)         --Muestra etiqueta Totales

              DISPLAY ARRAY a_datos_apo_sub TO rec_detalle.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
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
                   CALL fn_reporte(v_folio_liquida, v_cb_estado, v_f_liquida_ini, v_f_liquida_fin)
                
                ON ACTION archivo
                   CALL fn_genera_archivo_apo_sub_concepto(v_folio_liquida, v_cb_estado, 
                                                           v_f_liquida_ini, v_f_liquida_fin)   
              END DISPLAY                    
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros.",
                              "about")
              CALL ui.interface.refresh()
           END IF           
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
    END DIALOG 
  CLOSE WINDOW w1 
END MAIN 

#Objetivo: Genera el ComboBox para los estados de la facturación
FUNCTION fn_llenar_combo_estado()              
  DEFINE arr_edo_apo         DYNAMIC ARRAY OF RECORD --Arreglo para almacenar el estado
    cod_edo_aps              SMALLINT,   --Campo de codigo de estado
    desc_edo_aps             VARCHAR(40)
  END RECORD
  
  DEFINE 
    v_cb_estado              DECIMAL(9,0), --Almacena folio en el combobox
    p_fecha                  DATE,         --Parámetro de fecha
    v_indice                 INTEGER,      --Variable del indice  
    v_QryTxt                 STRING,       --Cadena para almacenar Query
    cb                       ui.ComboBox   --Variable de Combobox

  DEFINE 
    i                        INTEGER
  
  LET cb = ui.ComboBox.forName("v_cb_estado") --Asignación del combo a la forma

  --Validación si el combo es nulo 
  IF cb IS NULL THEN
     ERROR "Form field not found in current form"
     EXIT PROGRAM
  END IF

  LET p_fecha  = TODAY
  LET v_indice = 1
   
  --Validación del tamaño de la fecha si es 0 no la incluye para el Query
  LET v_QryTxt = "\n SELECT ce.cod_edo_aps,",
                 "\n        ce.cod_edo_aps||'-'||ce.desc_edo_aps",
                 "\n FROM   cat_edo_aps ce",
                 "\n ORDER BY 1 "
                 
  --DISPLAY "Llena combo ",v_QryTxt

  --Prepara la consulta para obtener folios liquidados
  PREPARE ps_estados_apo_sub FROM v_QryTxt
   
  --Limpia el combo
  CALL cb.clear()

  --Declara el cursor para la consulta 
  DECLARE cur_estados_apo_sub CURSOR FOR ps_estados_apo_sub
  FOREACH cur_estados_apo_sub INTO arr_edo_apo[v_indice].cod_edo_aps,
                                   arr_edo_apo[v_indice].desc_edo_aps
    --Agrega elementos al combobox
    CALL cb.addItem(arr_edo_apo[v_indice].cod_edo_aps, arr_edo_apo[v_indice].desc_edo_aps)
      
    --DISPLAY arr_edo_apo[v_indice].cod_edo_aps, " - ", arr_edo_apo[v_indice].desc_edo_aps
    LET v_indice = v_indice + 1
  END FOREACH

  CALL arr_edo_apo.deleteElement(v_indice)
  LET v_indice = v_indice - 1
         
  RETURN v_cb_estado, v_indice
END FUNCTION

FUNCTION fn_consultar(p_folio_liquida, p_estado, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_folio_liquida     DECIMAL(9,0)  --Folio de liquidación de la dispersión
  DEFINE p_estado            SMALLINT      --Estado de la facturación
  DEFINE p_f_liquida_ini     DATE          --Fecha de liquidación inicial
  DEFINE p_f_liquida_fin     DATE          --Fecha de liquidacion final   

  DEFINE v_desc_concepto     CHAR(50)
  DEFINE v_desc_ent_financiera CHAR(50)
  DEFINE v_desc_credito      CHAR(50)
  DEFINE v_desc_estado       CHAR(50)
  
  DEFINE v_concepto          SMALLINT
  DEFINE v_ent_financiera    SMALLINT
  DEFINE v_tpo_credito       SMALLINT 
  DEFINE v_estado            SMALLINT

  LET g_sql_txt = "\n SELECT dca.concepto, dca.cve_ent_financiera, ",  
                  "\n        dca.tpo_credito, dca.estado, ",
                  "\n        COUNT(*),",
                  "\n        SUM(dca.aiv_ap_pat), ",
                  "\n        SUM(dca.imp_ap_pat) ",
                  "\n FROM   dis_ctr_aps_tns dca, ", 
                  "\n        glo_folio gf ",
                  "\n WHERE  gf.folio       = dca.folio_liquida ",
                  "\n AND   (gf.status      = 2 ",
                  "\n AND    gf.proceso_cod = 932 ",  --Falta modificar el proceso_cod
                  "\n OR     gf.status     <> 3 ",  
                  "\n AND    gf.proceso_cod = 1217) " --Falta modificar el proceso_cod  
                   
  IF p_folio_liquida IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND dca.folio_liquida = ",p_folio_liquida
  END IF

  IF p_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dca.estado = ",p_estado
  END IF

  IF p_f_liquida_ini IS NOT NULL AND p_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",p_f_liquida_ini,"'",
                               "\n AND dca.f_liquida  <= '",p_f_liquida_fin,"'" 
                               --"\n AND gf.f_actualiza >= '",p_f_liquida_ini,"'",
                               --"\n AND gf.f_actualiza <= '",p_f_liquida_fin,"'"
  END IF
        
  LET g_sql_txt = g_sql_txt,"\n GROUP BY dca.concepto, dca.cve_ent_financiera, dca.tpo_credito, dca.estado ",
                            "\n ORDER BY dca.concepto, dca.cve_ent_financiera, dca.tpo_credito, dca.estado "
                
  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_apo_sub_concepto FROM g_sql_txt
  DECLARE cur_apo_sub_concepto CURSOR FOR ps_apo_sub_concepto

  LET v_indice         = 1
  LET v_tot_registros  = 0
  LET v_tot_aivs       = 0.00
  LET v_tot_aportacion = 0.00

  LET v_concepto       = 0
  LET v_ent_financiera = 0
  LET v_tpo_credito    = 0
  LET v_estado         = 0

  INITIALIZE v_desc_concepto TO NULL
  INITIALIZE v_desc_ent_financiera TO NULL
  INITIALIZE v_desc_credito TO NULL
  INITIALIZE v_desc_estado TO NULL
  
  FOREACH cur_apo_sub_concepto INTO v_concepto,
                                    v_ent_financiera,
                                    v_tpo_credito,
                                    v_estado,
                                    a_datos_apo_sub[v_indice].tot_registros,
                                    a_datos_apo_sub[v_indice].aiv_ap_pat,
                                    a_datos_apo_sub[v_indice].imp_ap_pat                                                        

    INITIALIZE v_desc_credito,
               v_desc_estado,
               v_desc_concepto,
               v_desc_ent_financiera TO NULL
    
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = v_tpo_credito
    AND    a.ind_activo      = 1

    SELECT UNIQUE(desc_edo_aps)
    INTO   v_desc_estado 
    FROM   cat_edo_aps
    WHERE  cod_edo_aps = v_estado

    SELECT UNIQUE(desc_concepto_ocg)
    INTO   v_desc_concepto
    FROM   cat_concepto_ocg
    WHERE  cod_concepto_ocg = v_concepto

    SELECT UNIQUE(ent_financiera_desc)
    INTO   v_desc_ent_financiera
    FROM   cat_cta_cnt_ocg
    WHERE  cve_ent_financiera = v_ent_financiera
    AND    tpo_credito        = v_tpo_credito 

    LET a_datos_apo_sub[v_indice].concepto       = v_concepto CLIPPED , " - ", v_desc_concepto CLIPPED
    LET a_datos_apo_sub[v_indice].ent_financiera = v_ent_financiera USING "&&&", " - ", v_desc_ent_financiera
    LET a_datos_apo_sub[v_indice].tpo_credito    = v_tpo_credito, " - ", v_desc_credito  
    LET a_datos_apo_sub[v_indice].estado         = v_estado, " - ",v_desc_estado  
                 
    LET v_tot_registros  = v_tot_registros  + a_datos_apo_sub[v_indice].tot_registros         
    LET v_tot_aivs       = v_tot_aivs       + a_datos_apo_sub[v_indice].aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion + a_datos_apo_sub[v_indice].imp_ap_pat     
    LET v_indice         = v_indice         + 1 
  END FOREACH

  FREE cur_apo_sub_concepto
  
  CALL a_datos_apo_sub.deleteElement(v_indice)
  LET v_indice    = v_indice - 1  
END FUNCTION  

FUNCTION fn_reporte(p_folio_liquida, p_estado, p_f_liquida_ini, p_f_liquida_fin)
  DEFINE p_folio_liquida     DECIMAL(9,0), 
         p_estado            SMALLINT, 
         p_f_liquida_ini     DATE, 
         p_f_liquida_fin     DATE

  DEFINE v_desc_estado       CHAR(50)

  DEFINE v_indice_rep        INTEGER
  DEFINE v_indice_rep_dev    INTEGER

  DEFINE v_tot_reporte       SMALLINT
  DEFINE v_encabezado        SMALLINT 
   
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  
  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISC241.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  CALL a_datos_apo_sub_rep.clear()
  CALL a_datos_apo_sub_rep_dev.clear()

  LET v_indice_rep         = 1
  LET v_indice_rep_dev     = 1

  LET v_tot_registros      = 0
  LET v_tot_aivs           = 0.00
  LET v_tot_aportacion     = 0.00
  LET v_tot_registros_dev  = 0
  LET v_tot_aivs_dev       = 0.00
  LET v_tot_aportacion_dev = 0.00

  FOR v_rep_indice = 1 TO  a_datos_apo_sub.getLength()
      --DISPLAY "a_datos_apo_sub[v_rep_indice].concepto[1,7]: ", a_datos_apo_sub[v_rep_indice].concepto[1,7]
      IF a_datos_apo_sub[v_rep_indice].concepto[1,7] MATCHES "*8" THEN
         --DISPLAY "a_datos_apo_sub[v_rep_indice].concepto[1,3]: ", a_datos_apo_sub[v_rep_indice].concepto[1,7]
         
         LET a_datos_apo_sub_rep_dev[v_indice_rep_dev].* = a_datos_apo_sub[v_rep_indice].*

         LET v_tot_registros_dev  = v_tot_registros_dev  + a_datos_apo_sub_rep_dev[v_indice_rep_dev].tot_registros         
         LET v_tot_aivs_dev       = v_tot_aivs_dev       + a_datos_apo_sub_rep_dev[v_indice_rep_dev].aiv_ap_pat
         LET v_tot_aportacion_dev = v_tot_aportacion_dev + a_datos_apo_sub_rep_dev[v_indice_rep_dev].imp_ap_pat  
    
         LET v_indice_rep_dev     = v_indice_rep_dev     + 1
      ELSE
         LET a_datos_apo_sub_rep[v_indice_rep].* = a_datos_apo_sub[v_rep_indice].*

         LET v_tot_registros  = v_tot_registros  + a_datos_apo_sub_rep[v_indice_rep].tot_registros         
         LET v_tot_aivs       = v_tot_aivs       + a_datos_apo_sub_rep[v_indice_rep].aiv_ap_pat
         LET v_tot_aportacion = v_tot_aportacion + a_datos_apo_sub_rep[v_indice_rep].imp_ap_pat 
         
         LET v_indice_rep     = v_indice_rep     + 1
      END IF
  END FOR

  IF a_datos_apo_sub_rep.getLength() > 0 THEN
     IF a_datos_apo_sub_rep[a_datos_apo_sub_rep.getLength()].concepto IS NULL THEN
        CALL a_datos_apo_sub_rep.deleteElement(v_indice_rep)
     END IF
     LET v_indice_rep     = v_indice_rep - 1
  END IF

  IF a_datos_apo_sub_rep_dev.getLength() > 0 THEN
     IF a_datos_apo_sub_rep_dev[a_datos_apo_sub_rep_dev.getLength()].concepto IS NULL THEN
        CALL a_datos_apo_sub_rep_dev.deleteElement(v_indice_rep_dev)
     END IF
     LET v_indice_rep_dev = v_indice_rep_dev - 1
  END IF
       
  --DISPLAY "a_datos_apo_sub_rep.getLength()     : ", a_datos_apo_sub_rep.getLength()
  --DISPLAY "a_datos_apo_sub_rep_dev.getLength() : ", a_datos_apo_sub_rep_dev.getLength()
   
  --Inicia el reporte
  {START REPORT rep_apo_sub_por_concepto TO XML HANDLER manejador_rpt
    FOR v_rep_indice = 1 TO  a_datos_apo_sub.getLength()
        LET v_desc_estado = ""
     
        SELECT UNIQUE(desc_edo_aps)
        INTO   v_desc_estado 
        FROM   cat_edo_aps
        WHERE  cod_edo_aps = p_estado

        LET v_desc_estado = p_estado CLIPPED," - ", v_desc_estado
    
        OUTPUT TO REPORT rep_apo_sub_por_concepto(p_folio_liquida, 
                                                  v_desc_estado,
                                                  p_f_liquida_ini, 
                                                  p_f_liquida_fin,
                                                  a_datos_apo_sub[v_rep_indice].*,
                                                  v_tot_registros,
                                                  v_tot_aivs,
                                                  v_tot_aportacion, 
                                                  g_usuario)
    END FOR
  FINISH REPORT rep_apo_sub_por_concepto}

  START REPORT rep_apo_sub_por_concepto TO XML HANDLER manejador_rpt
    LET v_tot_reporte = 1
    LET v_encabezado  = 1
  
    FOR v_rep_indice = 1 TO  a_datos_apo_sub_rep.getLength()
        LET v_desc_estado = ""
      
        SELECT UNIQUE(desc_edo_aps)
        INTO   v_desc_estado 
        FROM   cat_edo_aps
        WHERE  cod_edo_aps = p_estado

        LET v_desc_estado = p_estado CLIPPED," - ", v_desc_estado
    
        OUTPUT TO REPORT rep_apo_sub_por_concepto(p_folio_liquida, 
                                                  v_desc_estado,
                                                  p_f_liquida_ini, 
                                                  p_f_liquida_fin,
                                                  a_datos_apo_sub_rep[v_rep_indice].*,
                                                  a_datos_apo_sub_rep_dev[v_rep_indice].*,
                                                  v_tot_registros,
                                                  v_tot_aivs,
                                                  v_tot_aportacion, 
                                                  v_tot_registros_dev,
                                                  v_tot_aivs_dev,
                                                  v_tot_aportacion_dev,
                                                  v_tot_reporte,
                                                  g_usuario)
    END FOR

    LET v_tot_reporte = 2
    LET v_encabezado  = 2

    DISPLAY "a_datos_apo_sub_rep_dev.getLength(): ", a_datos_apo_sub_rep_dev.getLength()

    IF a_datos_apo_sub_rep_dev.getLength() >= 1 THEN
       FOR v_rep_indice = 1 TO  a_datos_apo_sub_rep_dev.getLength()
           LET v_desc_estado = ""
         
           SELECT UNIQUE(desc_edo_aps)
           INTO   v_desc_estado 
           FROM   cat_edo_aps
           WHERE  cod_edo_aps = p_estado

           LET v_desc_estado = p_estado CLIPPED," - ", v_desc_estado
       
           OUTPUT TO REPORT rep_apo_sub_por_concepto(p_folio_liquida, 
                                                     v_desc_estado,
                                                     p_f_liquida_ini, 
                                                     p_f_liquida_fin,
                                                     a_datos_apo_sub_rep[v_rep_indice].*,
                                                     a_datos_apo_sub_rep_dev[v_rep_indice].*,
                                                     v_tot_registros,
                                                     v_tot_aivs,
                                                     v_tot_aportacion, 
                                                     v_tot_registros_dev,
                                                     v_tot_aivs_dev,
                                                     v_tot_aportacion_dev,
                                                     v_tot_reporte,
                                                     g_usuario)
       END FOR
    END IF
  FINISH REPORT rep_apo_sub_por_concepto
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_apo_sub_por_concepto(p_folio_liquida, 
                                p_desc_estado,
                                p_f_liquida_ini, 
                                p_f_liquida_fin,
                                v_datos_apo_sub,
                                v_datos_apo_sub_dev, 
                                v_rep_tot_registros, 
                                v_rep_sum_aivs, 
                                v_rep_sum_aportacion, 
                                v_rep_tot_registros_dev, 
                                v_rep_sum_aivs_dev, 
                                v_rep_sum_aportacion_dev,
                                v_tot_reporte,
                                v_usuario)

  DEFINE p_folio_liquida           DECIMAL(9,0), 
         p_desc_estado             CHAR(50), 
         p_f_liquida_ini           DATE, 
         p_f_liquida_fin           DATE
                                
  DEFINE v_datos_apo_sub           RECORD  
    concepto                       CHAR(50),
    ent_financiera                 CHAR(50),
    tpo_credito                    CHAR(30),
    estado                         CHAR(50),
    tot_registros                  DECIMAL(9,0),    
    aiv_ap_pat                     DECIMAL(22,6),    
    imp_ap_pat                     DECIMAL(12,2)
  END RECORD      

 DEFINE v_datos_apo_sub_dev   RECORD  
    concepto                       CHAR(50),
    ent_financiera                 CHAR(50),
    tpo_credito                    CHAR(30),
    estado                         CHAR(50),
    tot_registros                  DECIMAL(9,0),    
    aiv_ap_pat                     DECIMAL(22,6),    
    imp_ap_pat                     DECIMAL(12,2)
  END RECORD                          
  
  DEFINE 
    v_fecha_consulta               DATE,         --Fecha de proceso
    v_usuario                      VARCHAR(30),  --Almacena al usuario
    v_rep_tot_registros            DECIMAL(9,0), --Total de registros
    v_rep_sum_aivs                 DECIMAL(22,6),
    v_rep_sum_aportacion           DECIMAL(12,2),

    v_rep_tot_registros_dev        DECIMAL(9,0), --Total de registros
    v_rep_sum_aivs_dev             DECIMAL(22,6),
    v_rep_sum_aportacion_dev       DECIMAL(12,2)    

   DEFINE v_tot_reporte            SMALLINT
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX p_folio_liquida
      PRINTX p_desc_estado
      PRINTX p_f_liquida_ini USING "dd-mm-yyyy"
      PRINTX p_f_liquida_fin USING "dd-mm-yyyy"

    PAGE HEADER
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX p_folio_liquida
      PRINTX p_desc_estado
      PRINTX p_f_liquida_ini USING "dd-mm-yyyy"
      PRINTX p_f_liquida_fin USING "dd-mm-yyyy"

   BEFORE GROUP OF v_tot_reporte
      PRINTX v_tot_reporte
      
      PRINTX v_rep_tot_registros USING "###,###,###,##&"
      PRINTX v_rep_sum_aivs USING "###,###,###,###,##&.##"
      PRINTX v_rep_sum_aportacion USING "###,###,###,###,##&.##"
      
      PRINTX v_rep_tot_registros_dev USING "###,###,###,##&"
      PRINTX v_rep_sum_aivs_dev USING "###,###,###,###,##&.##"
      PRINTX v_rep_sum_aportacion_dev USING "###,###,###,###,##&.##"

    ON EVERY ROW
       PRINTX v_datos_apo_sub.concepto
       PRINTX v_datos_apo_sub.ent_financiera
       PRINTX v_datos_apo_sub.tpo_credito
       PRINTX v_datos_apo_sub.estado
       PRINTX v_datos_apo_sub.tot_registros USING "###,###,###,##&"
       PRINTX v_datos_apo_sub.aiv_ap_pat USING "###,###,###,##&.######"  
       PRINTX v_datos_apo_sub.imp_ap_pat USING "###,###,###,###,##&.&&"  

       PRINTX v_datos_apo_sub_dev.concepto
       PRINTX v_datos_apo_sub_dev.ent_financiera
       PRINTX v_datos_apo_sub_dev.tpo_credito
       PRINTX v_datos_apo_sub_dev.estado
       PRINTX v_datos_apo_sub_dev.tot_registros USING "###,###,###,##&"
       PRINTX v_datos_apo_sub_dev.aiv_ap_pat USING "###,###,###,###,##&.&&"  
       PRINTX v_datos_apo_sub_dev.imp_ap_pat USING "###,###,###,###,##&.&&"    


END REPORT

FUNCTION fn_genera_archivo_apo_sub_concepto(v_folio_liquida, p_cb_estado, v_f_liquida_ini, v_f_liquida_fin)
  DEFINE 
    v_folio_liquida          DECIMAL(9,0),--Folio de liquidación de la dispersión
    v_f_liquida_ini          DATE,        --Fecha de liquidación inicial
    v_f_liquida_fin          DATE         --Fecha de liquidacion final

  DEFINE p_cb_estado         SMALLINT

  DEFINE  
    v_nom_archivo            VARCHAR(60),  --Nombre del archivo de salida
    v_ruta_envio_dis         CHAR(60),
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_comando_dos            STRING,
    v_titulos                STRING,                 
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 INTEGER

  DEFINE arr_info_apo        DYNAMIC ARRAY OF RECORD
    nss                      CHAR(11), 
    nombre_af                CHAR(30),
    ap_paterno_af            CHAR(30), 
    ap_materno_af            CHAR(30), 
    periodo_pago             CHAR(6),
    f_pago                   DATE, 
    folio_sua                DECIMAL(6,0),
    nrp                      CHAR(11),
    imp_ap_pat               DECIMAL(12,2),
    aiv_ap_pat               DECIMAL(22,6),
    cve_ent_financiera       CHAR(50),
    tpo_credito              SMALLINT,
    concepto                 CHAR(50),
    folio_transaccion        DECIMAL(9,0),
    f_transaccion            DATE,
    estado                   SMALLINT
  END RECORD 

  DEFINE v_aportacion        CHAR(10)
  DEFINE v_desc_credito      CHAR(20) 
  DEFINE v_desc_estado       CHAR(50)
  DEFINE v_desc_concepto     CHAR(50)
  DEFINE v_desc_ent_financiera CHAR(50)
  DEFINE v_concepto          SMALLINT
  DEFINE v_interface         CHAR(2)
  
  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  --LET v_nom_archivo   = "/fact_tpo_conc_", v_hora
  LET v_nom_archivo   = "/fact_cons_trans_", v_hora 

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  --Imprime encabezado del archivo
  --LET v_encabezado = "NSS |NOMBRE |PERIODO |APORTACIÓN |FOLIO SUA |FECHA APLIACIÓN EN SAFRE |FECHA ENTREGA |INTERFACE |TIPO CRÉDITO"

  WHENEVER ERROR CONTINUE;
    DROP TABLE tmp_afi_disc24;
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
                  "\n        glo_folio gf  ",
                  "\n WHERE  H.id_derechohabiente = A.id_derechohabiente ",
                  "\n AND    gf.folio               = H.folio_liquida ", 
                  "\n AND   (gf.status              = 2 ", 
                  "\n AND    gf.proceso_cod         = 932 ",
                  "\n OR     gf.status             <> 3 ",
                  "\n AND    gf.proceso_cod         = 1217) "

  IF v_folio_liquida IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND H.folio_liquida = ",v_folio_liquida
  END IF 

  IF p_cb_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND H.estado        = ",p_cb_estado
  END IF

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND H.f_liquida    >= '",v_f_liquida_ini,"'",
                               "\n AND H.f_liquida    <= '",v_f_liquida_fin,"'" 
  END IF

  LET g_sql_txt    = g_sql_txt, "\n INTO TEMP tmp_afi_disc24 "

  PREPARE prep_tmp_afi1 FROM g_sql_txt
  EXECUTE prep_tmp_afi1

  UPDATE STATISTICS FOR TABLE tmp_afi_disc24
  
  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT ad.nss, ",
                  "\n        ad.nombre_af, ",
                  "\n        ad.ap_paterno_af, ",
                  "\n        ad.ap_materno_af, ",
                  "\n        dca.periodo_pago, ",
                  "\n        dca.f_pago, ",
                  "\n        dca.folio_sua, ",
                  "\n        dca.nrp, ",
                  "\n        dca.imp_ap_pat, ",
                  "\n        dca.aiv_ap_pat, ",
                  "\n        dca.cve_ent_financiera, ",
                  "\n        dca.tpo_credito, ",
                  "\n        dca.concepto, ",
                  "\n        dca.folio_transaccion, ",
                  "\n        dca.f_transaccion, ",
                  "\n        dca.estado ",
                  "\n FROM   dis_ctr_aps_tns dca, ",
                  "\n        tmp_afi_disc24 ad ",
                  "\n WHERE  dca.id_dis_interface_ef = ad.id_dis_interface_ef ",
                  "\n AND    dca.id_derechohabiente  = ad.id_derechohabiente "

                  {"\n FROM   afi_derechohabiente ad, ",
                  "\n        dis_ctr_aps_tns dca, ",
                  "\n        glo_folio gf ",
                  "\n WHERE  dca.id_derechohabiente = ad.id_derechohabiente ", 
                  "\n AND    gf.folio               = dca.folio_liquida ",
                  "\n AND   (gf.status              = 2 ",
                  "\n AND    gf.proceso_cod         = 932 ",  --Falta modificar el proceso_cod  
                  "\n OR     gf.status             <> 3 ",
                  "\n AND    gf.proceso_cod         = 1217) " --Falta modificar el proceso_cod}  
                  
  IF v_folio_liquida IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND folio_liquida = ",v_folio_liquida
     --Imprime encabezado del archivo
     LET v_encabezado = "FOLIO: ",v_folio_liquida
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF 

  IF p_cb_estado IS NOT NULL THEN
     LET g_sql_txt = g_sql_txt,"\n AND dca.estado    = ",p_cb_estado

     SELECT UNIQUE(desc_edo_aps)
     INTO   v_desc_estado 
     FROM   cat_edo_aps
     WHERE  cod_edo_aps = p_cb_estado

     LET v_encabezado = "STATUS: ",p_cb_estado, "-", v_desc_estado CLIPPED
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  IF v_f_liquida_ini IS NOT NULL AND v_f_liquida_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND dca.f_liquida  >= '",v_f_liquida_ini,"'",
                               "\n AND dca.f_liquida  <= '",v_f_liquida_fin,"'" 
                               --"\n AND gf.f_actualiza >= '",v_f_liquida_ini,"'",
                               --"\n AND gf.f_actualiza <= '",v_f_liquida_fin,"'"

     LET v_encabezado = "PERIODO FECHAS: ",v_f_liquida_ini USING "dd-mm-yyyy", " - ", v_f_liquida_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET g_sql_txt = g_sql_txt,"\n ORDER BY dca.folio_transaccion, dca.f_transaccion, ad.nss, dca.periodo_pago DESC, dca.f_pago DESC "  

  --DISPLAY "g_sql_txt: -",g_sql_txt,"-"
      
  PREPARE ps_arch_apo_sub_concepto FROM g_sql_txt
  DECLARE cur_arch_apo_sub_concepto CURSOR FOR ps_arch_apo_sub_concepto
  
  LET v_indice         = 1
  LET v_tot_aivs       = 0
  LET v_tot_aportacion = 0

  LET v_titulos = "NSS |PERIODO PAGO |FECHA PAGO |FOLIO SUA |NRP |AIVS |APORTACIÓN |ENTIDAD FINANCIERA |CONCEPTO PAGO |TRANSACCIÓN |FOLIO TRANSACCIÓN |FECHA TRANSACCIÓN |STATUS |FECHA ARCHIVO |INTERFACE "
  CALL v_ch_arch_salida.write([v_titulos])
  
  FOREACH cur_arch_apo_sub_concepto INTO arr_info_apo[v_indice].nss,     
                                         arr_info_apo[v_indice].nombre_af,
                                         arr_info_apo[v_indice].ap_paterno_af,
                                         arr_info_apo[v_indice].ap_materno_af, 
                                         arr_info_apo[v_indice].periodo_pago,
                                         arr_info_apo[v_indice].f_pago,
                                         arr_info_apo[v_indice].folio_sua,
                                         arr_info_apo[v_indice].nrp,
                                         arr_info_apo[v_indice].imp_ap_pat,
                                         arr_info_apo[v_indice].aiv_ap_pat,
                                         arr_info_apo[v_indice].cve_ent_financiera,
                                         arr_info_apo[v_indice].tpo_credito,
                                         arr_info_apo[v_indice].concepto,
                                         arr_info_apo[v_indice].folio_transaccion,
                                         arr_info_apo[v_indice].f_transaccion,
                                         arr_info_apo[v_indice].estado

    INITIALIZE v_desc_credito,
               v_desc_estado,
               v_desc_concepto,
               v_desc_ent_financiera TO NULL
       
    SELECT a.desc_credito_ocg
    INTO   v_desc_credito 
    FROM   cat_tpo_credito_ocg a
    WHERE  a.tpo_credito_ocg = arr_info_apo[v_indice].tpo_credito
    AND    a.ind_activo      = 1

    IF arr_info_apo[v_indice].tpo_credito <> 3 THEN
       LET v_interface = 'AS'
    ELSE 
       LET v_interface = 'UG'
    END IF

    SELECT UNIQUE(desc_edo_aps)
    INTO   v_desc_estado 
    FROM   cat_edo_aps
    WHERE  cod_edo_aps = arr_info_apo[v_indice].estado

    SELECT UNIQUE(desc_concepto_ocg)
    INTO   v_desc_concepto
    FROM   cat_concepto_ocg
    WHERE  cod_concepto_ocg = arr_info_apo[v_indice].concepto

    SELECT UNIQUE(ent_financiera_desc)
    INTO   v_desc_ent_financiera
    FROM   cat_cta_cnt_ocg
    WHERE  cve_ent_financiera = arr_info_apo[v_indice].cve_ent_financiera
    AND    tpo_credito        = arr_info_apo[v_indice].tpo_credito 

    --LET v_aportacion = arr_info_apo[v_indice].imp_ap_pat * 100 USING "&&&&&&&&&&"

    LET v_detalle = arr_info_apo[v_indice].nss, "|",
                    --arr_info_apo[v_indice].nombre_af CLIPPED," ", arr_info_apo[v_indice].ap_paterno_af CLIPPED," ", arr_info_apo[v_indice].ap_materno_af CLIPPED, "|",
                     arr_info_apo[v_indice].periodo_pago CLIPPED, "|",
                     arr_info_apo[v_indice].f_pago USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].folio_sua, "|",
                     arr_info_apo[v_indice].nrp, "|",
                     arr_info_apo[v_indice].aiv_ap_pat USING "#,###,##&.######", "|",
                     arr_info_apo[v_indice].imp_ap_pat USING "#,###,##&.##", "|",                       
                     --v_aportacion, "|",
                     arr_info_apo[v_indice].cve_ent_financiera USING "&&&", " - ", v_desc_ent_financiera CLIPPED, "|",
                     arr_info_apo[v_indice].tpo_credito CLIPPED, " - ", v_desc_credito CLIPPED, "|",
                     arr_info_apo[v_indice].concepto CLIPPED,  " - ", v_desc_concepto CLIPPED, "|", 
                     arr_info_apo[v_indice].folio_transaccion, "|",
                     arr_info_apo[v_indice].f_transaccion USING "dd-mm-yyyy", "|",
                     arr_info_apo[v_indice].estado CLIPPED, " - ", v_desc_estado CLIPPED, "|",
                     TODAY USING "dd-mm-yyyy", "|",
                     v_interface CLIPPED

    CALL v_ch_arch_salida.write([v_detalle])
        
    LET v_tot_aivs       = v_tot_aivs       + arr_info_apo[v_indice].aiv_ap_pat
    LET v_tot_aportacion = v_tot_aportacion + arr_info_apo[v_indice].imp_ap_pat
    LET v_indice         = v_indice         + 1
  END FOREACH

  FREE cur_arch_apo_sub_concepto
  
  CALL arr_info_apo.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros = v_indice

  LET v_sumario = "TOTALES: | ",v_tot_registros," | | | | ",
                                v_tot_aivs USING "###,###,##&.######"," | ",
                                v_tot_aportacion USING "###,###,##&.##", " | | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta de Facturación por Transacción \n en la ruta "||v_ruta_nomarch,"information") 
END FUNCTION