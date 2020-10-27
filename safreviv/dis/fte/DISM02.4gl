################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 11/04/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISM02                                                    #
#Objetivo         => Programa de consulta de Estado de la Factura              #
#                                                                              #
#Fecha de Inicio  => 03/03/2015                                                #
################################################################################
--Base que se utilizará
DATABASE safre_viv

--Definición de variables globales
GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_pid                    DECIMAL(9,0)
 
  --Datos de detalle
  DEFINE a_estado_factura    DYNAMIC ARRAY OF RECORD  
    documento_fico           CHAR(10),
    referencia               CHAR(16),
    entidad_financiera       CHAR(65),
    clabe                    CHAR(18),  
    cta_contable             CHAR(10),
    monto                    DECIMAL(18,6),
    edo_factura              CHAR(50),
    edo_valida_factura       CHAR(50),
    f_pago                   DATE,
    consultar                SMALLINT
  END RECORD   

  DEFINE a_estado_fac_rech   DYNAMIC ARRAY OF RECORD  
    documento_fico1          CHAR(10),
    referencia1              CHAR(16),
    entidad_financiera1      CHAR(65),
    clabe1                   CHAR(18),
    cta_contable1            CHAR(10),    
    monto1                   DECIMAL(18,6),
    edo_factura1             CHAR(50),
    edo_valida_factura1      CHAR(50)
  END RECORD   

  --Cifras totales de detalle y Totales por tipo de crédito y concepto
  DEFINE 
    v_tot_detalle            DECIMAL(18,6),
    v_tot_totales            DECIMAL(18,6)    

  DEFINE v_respuesta         SMALLINT
  DEFINE v_proceso_cod       SMALLINT --Código del proceso

  DEFINE v_folio_disp        DECIMAL(9,0),
         p_programa          CHAR(10),  
         r_bandera           SMALLINT,
         r_nom_archivo       CHAR(40)  

  DEFINE l_comando           STRING,
         v_ruta_ejecutable   CHAR(40),
         v_ruta_listados     CHAR(40),
         v_qwery_ibx         STRING, 
         v_mensaje           STRING         

  DEFINE v_tot_monto_pesos   DECIMAL(22,2),
         v_tot_monto_acciones DECIMAL(26,6),
         v_tot_registros     DECIMAL(12,0)

  DEFINE v_bnd_confirma      SMALLINT
  DEFINE v_indice1           SMALLINT
  DEFINE v_indice2           SMALLINT 
  DEFINE v_indice            SMALLINT
  DEFINE
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING
  
END GLOBALS

MAIN
  --Datos de entrada
  DEFINE v_folio_facturacion DECIMAL(9,0),--Folio de transacción
         v_f_facturacion_ini DATE,        --Fecha de transacción inicial
         v_f_facturacion_fin DATE         --Fecha de transacción final 
 
  DEFINE bnd_consulta        SMALLINT, 
         f_ventana           ui.Window,   --Define las propìedades de la Ventana
         f_forma             ui.Form      --Define las propiedades de la forma    

  DEFINE p_proceso_cod       SMALLINT,
         p_opera_cod         SMALLINT

  DEFINE wsstatus            INTEGER
  
  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)

  LET bnd_consulta   = 0
  LET p_proceso_cod  = 3914 --Codigo aún no definido ???       
  LET p_opera_cod    = 1    --Codigo aún no definido ???

  CALL STARTLOG (g_usuario CLIPPED||".DISM02.log")

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

  --Obtiene la ruta ejecutable de dispersión
  SELECT ruta_bin
  INTO   v_ruta_ejecutable
  FROM   seg_modulo
  WHERE  modulo_cod = 'ocg'
  --WHERE  modulo_cod = 'dis'

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "DISM021"
    DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT BY NAME v_folio_facturacion, v_f_facturacion_ini, 
                    v_f_facturacion_fin
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          CALL f_forma.setElementHidden("gr_detalle", 1)          --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_det_rechazados", 1)   --Oculta detalle rechazados
          --CALL f_forma.setElementHidden("gr_tot_aportaciones", 1)
          NEXT FIELD v_folio_facturacion
          CALL ui.interface.refresh()

        ON ACTION ACCEPT
           --Válidaciones de criterios de búsqueda
           IF v_folio_facturacion IS NULL AND 
              v_f_facturacion_ini IS NULL AND v_f_facturacion_fin IS NULL THEN 
             
              CALL fn_mensaje("ATENCIÓN",
                              "Debe seleccionar algun criterio de búsqueda.",
                              "about")
              NEXT FIELD v_folio_facturacion  
           END IF 
          
           IF v_f_facturacion_ini > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Facturación Inicial no puede ser mayor a la Fecha Actual",
                              "about")
              NEXT FIELD v_f_facturacion_ini
           END IF    

           IF v_f_facturacion_fin > TODAY THEN 
              CALL fn_mensaje("ATENCIÓN",
                              "La Fecha de Facturación Final no puede ser mayor a la Fecha Actual",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF 
          
           IF v_f_facturacion_ini IS NOT NULL AND v_f_facturacion_fin IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Facturación Final",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF 

           IF v_f_facturacion_fin IS NOT NULL AND v_f_facturacion_ini IS NULL THEN 
              CALL fn_mensaje("ATENCIÓN", 
                              "Debe capturar una Fecha de Facturación Inicial",
                              "about")
              NEXT FIELD v_f_facturacion_fin
           END IF 

           IF (v_f_facturacion_ini > v_f_facturacion_fin) THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "La Fecha Inicial de Facturación no puede ser mayor a la Fecha Final de Facturación.",
                               "about")
               NEXT FIELD v_f_facturacion_ini 
           END IF

           IF (v_f_facturacion_fin < v_f_facturacion_ini) THEN
               CALL fn_mensaje("ATENCIÓN", 
                               "La Fecha Final de Facturación no puede ser menor a la Fecha Inicial de Facturación.",
                               "about")
               NEXT FIELD v_f_facturacion_fin 
           END IF  

           CALL fn_consultar(v_folio_facturacion, 
                             v_f_facturacion_ini, v_f_facturacion_fin) 
           RETURNING v_indice1, v_indice2         
          
           IF v_indice1 > 0 OR v_indice2 > 0 THEN
           --IF v_indice1 > 0 THEN  
              CALL f_forma.setElementHidden("gr_detalle", 0)  --muestra detalle de la consulta
              CALL f_forma.setElementHidden("gr_det_rechazados", 0)  --muestra detalle rechazados
              --CALL f_forma.setElementHidden("gr_tot_aportaciones", 0)
               NEXT FIELD documento_fico
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros.",
                              "about")
              CALL ui.interface.refresh()
           END IF  
      END INPUT

      DISPLAY ARRAY a_estado_fac_rech TO rec_det_rech.*      
        --ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
      END DISPLAY

      INPUT ARRAY a_estado_factura FROM rec_estado_factura.*
      ATTRIBUTES (APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE) 
        BEFORE INPUT
          ON ACTION reporte
             CALL fn_reporte(v_indice1, 
                             v_folio_facturacion, 
                             v_f_facturacion_ini, 
                             v_f_facturacion_fin)

          ON ACTION archivo
             CALL fn_genera_archivo(v_folio_facturacion, v_f_facturacion_ini, v_f_facturacion_fin)

          ON ACTION consultar
             LET v_bnd_confirma = 0
             CALL fn_verificar_apo_sele() RETURNING v_bnd_confirma

             IF v_bnd_confirma = 1 THEN
                LET v_respuesta = 0
                CALL fn_ventana_confirma("CONFIRMAR", 
                                         "¿Está seguro que desea consultar las facturas seleccionadas?", 
                                         "quest")
                RETURNING v_respuesta

                IF v_respuesta = 1 THEN
                   --CALL fn_borra_crea_tmp_apo_sub()

                   --INITIALIZE v_edo_aux TO NULL
                   
                   FOR v_indice = 1 TO a_estado_factura.getLength()
                       IF a_estado_factura[v_indice].consultar = 1 THEN
                          LET l_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/DISWS03.42r ",a_estado_factura[v_indice].referencia, " ",
                                                                                              a_estado_factura[v_indice].documento_fico
                          RUN l_comando RETURNING wsstatus        
                                   
                          IF wsstatus != 0  THEN
                             IF wsstatus = 16128 THEN
                                CALL fn_mensaje("Aviso","Error de conexión al Web Service.","stop")
                             ELSE                                     
                                CALL fn_mensaje("Aviso","Error en WS.","stop")
                             END IF
                             CALL fn_mensaje("Aviso","Se han enviado a consultar el status de pago de las facturas.","information")
                             EXIT PROGRAM
                          END IF

                          --Rechazo por Consulta de Pago
                          CALL fn_val_rch_cons_pag(a_estado_factura[v_indice].referencia)
                        
                          {LET v_ent_financiera = a_fac_apo_sub[i].cve_ent_financiera CLIPPED 
                          --LET v_concepto       = a_fac_apo_sub[i].concepto CLIPPED
                          LET v_edo_aux        = a_fac_apo_sub[i].estado[1,2] CLIPPED
                          LET v_estado         = v_edo_aux
                          LET v_num_ctr_int_ef = a_fac_apo_sub[i].cuenta_bancaria CLIPPED
                                            
                          CALL fn_llen_tmp_fac_apo_sub(v_folio_transaccion, v_f_transaccion_ini, 
                                                       v_f_transaccion_fin, v_tipo_credito, v_ent_financiera,
                                                       v_concepto, v_estado, v_num_ctr_int_ef)}
                       END IF
                   END FOR   
                   CALL fn_mensaje("Aviso","Operación finalizada.","stop") 
                   EXIT PROGRAM 
                ELSE
                   CALL fn_mensaje("Aviso","Operación cancelada.","stop") 
                    EXIT PROGRAM
                END IF --v_respuesta
             ELSE --v_bnd_confirma = 0
                LET v_mensaje = "Aún no ha seleccionado ninguna factura para consultar."
                CALL fn_mensaje("Facturación", v_mensaje, "information")   
             END IF  --v_bnd_confirma = 1 
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      
          
    END DIALOG
  CLOSE WINDOW w1
END MAIN

FUNCTION fn_consultar(p_folio_facturacion, p_f_facturacion_ini, p_f_facturacion_fin) 
  DEFINE p_folio_facturacion DECIMAL(9,0), --Folio de transacción
         p_f_facturacion_ini DATE,         --Fecha de transacción inicial
         p_f_facturacion_fin DATE          --Fecha de transacción final

  --DEFINE v_indice1           SMALLINT,
  --        v_indice2            SMALLINT

  DEFINE v_documento_fico    CHAR(10),
         v_referencia        CHAR(16),
         v_cve_ent_financiera CHAR(5),
         v_ent_financiera    CHAR(65),
         v_clabe             CHAR(18), 
         v_cta_contable      CHAR(10),   
         v_monto             DECIMAL(18,6),
         v_estado_fac        CHAR(5),
         v_desc_estado_fac   VARCHAR(60),
         v_estado_val_fac    CHAR(5),
         v_desc_estado_val_fac VARCHAR(60),
         v_f_pago            DATE,
         v_consultar         SMALLINT

  -----Busqueda de Detalle
  LET g_sql_txt = "\n SELECT fa.num_poliza,                                ",
                  "\n        fa.referencia,                                ",
                  "\n        fa.cve_ent_financiera,                        ",
                  "\n        ef.ent_financiera_desc,                       ", 
                  "\n        fa.clabe,                                     ",
                  "\n        ef.cta_contable,                              ",
                  "\n        SUM(importe) AS monto,                        ",
                  "\n        fa.f_pago,                                    ",
                  "\n        1                                             ",
                  "\n FROM   dis_ctr_factura_aps fa,                       ",
                  "\n        OUTER cat_cta_cnt_ocg ef                      ",
                  "\n WHERE  fa.cve_ent_financiera = ef.cve_ent_financiera ",
                  "\n  AND   fa.tpo_credito	       = ef.tpo_credito        "                  
                
  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n    AND fa.folio_factura = ",p_folio_facturacion
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n    AND fa.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n    AND fa.f_factura <= '",p_f_facturacion_fin,"'"
  END IF   
     
  LET g_sql_txt = g_sql_txt,"\n GROUP BY 1,2,3,4,5,6,8 ",
                            "\n ORDER BY 3,1,2 "  

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"

  PREPARE ps_estado_factura FROM g_sql_txt
  DECLARE cur_estado_factura CURSOR FOR ps_estado_factura
  
  LET v_indice1         = 1
  LET v_indice2         = 1
  LET v_tot_registros   = 0.0   
  LET v_tot_monto_pesos = 0.0    

  INITIALIZE v_documento_fico TO NULL
  INITIALIZE v_referencia     TO NULL
  INITIALIZE v_ent_financiera TO NULL
  INITIALIZE v_clabe          TO NULL 
  INITIALIZE v_f_pago         TO NULL
  LET v_monto                  = 0
         
  CALL a_estado_factura.clear()
  CALL a_estado_fac_rech.clear()
  FOREACH cur_estado_factura INTO v_documento_fico,    
                                  v_referencia,
                                  v_cve_ent_financiera,
                                  v_ent_financiera,
                                  v_clabe,
                                  v_cta_contable, 
                                  v_monto,
                                  v_f_pago,
                                  v_consultar

    LET v_estado_fac = 0
    INITIALIZE v_desc_estado_fac TO NULL
      
    LET g_sql_txt = "\n SELECT UNIQUE ef.estado, cef.desc_edo_fac_aps    ",
                    "\n FROM   dis_ctr_factura_aps fac,                  ",
                    "\n	       dis_ctr_edo_factura_aps  ef,              ",
                    "\n        OUTER cat_edo_factura_aps cef             ",
                    "\n WHERE  fac.referencia    = ef.referencia         ",
                    "\n AND    fac.num_poliza    = ef.num_poliza         ",
                    "\n AND    ef.estado         = cef.cod_edo_fac_aps   ",
                    "\n AND    ef.tpo_estado     = 1                     ",
                    "\n AND    fac.num_poliza    = '",v_documento_fico,"'",
                    "\n AND    fac.referencia    = '",v_referencia    ,"'",
                    "\n AND    fac.clabe         = '",v_clabe         ,"'",
                    "\n AND    fac.cve_ent_financiera = ", v_cve_ent_financiera

    --DISPLAY g_sql_txt               
    PREPARE ps_edo_factura FROM g_sql_txt
    EXECUTE ps_edo_factura INTO v_estado_fac, v_desc_estado_fac

    LET v_estado_val_fac = 0
    INITIALIZE v_desc_estado_val_fac TO NULL
      
    LET g_sql_txt = "\n SELECT UNIQUE ef.estado, cevf.desc_edo_val_fac_aps  ",   
                    "\n FROM   dis_ctr_factura_aps fac,                     ",
                    "\n 	   dis_ctr_edo_factura_aps ef,                  ",
                    "\n        OUTER cat_edo_val_factura_aps cevf           ",
                    "\n WHERE  fac.referencia    = ef.referencia            ",
                    "\n AND    fac.num_poliza    = ef.num_poliza            ",
                    "\n AND    ef.estado         = cevf.cod_edo_val_fac_aps ",
                    "\n AND    ef.tpo_estado     = 2                        ",
                    "\n AND    fac.num_poliza    = '",v_documento_fico   ,"'",
                    "\n AND    fac.referencia    = '",v_referencia       ,"'",
                    "\n AND    fac.clabe         = '",v_clabe            ,"'",
                    "\n AND    fac.cve_ent_financiera = ", v_cve_ent_financiera

    DISPLAY g_sql_txt               
    PREPARE ps_edo_val_factura FROM g_sql_txt
    EXECUTE ps_edo_val_factura INTO v_estado_val_fac, v_desc_estado_val_fac

    {DISPLAY ""
    DISPLAY "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    DISPLAY "ESTADO FACTURA    : ", v_estado_fac," - ", v_desc_estado_fac
    DISPLAY "ESTADO VAL FACTURA: ", v_estado_val_fac," - ", v_desc_estado_val_fac
    DISPLAY "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    DISPLAY ""}
      
    LET v_tot_monto_pesos = v_tot_monto_pesos + v_monto

    IF v_estado_fac = 1 THEN
       LET a_estado_factura[v_indice1].documento_fico       = v_documento_fico   
       LET a_estado_factura[v_indice1].referencia           = v_referencia
       LET a_estado_factura[v_indice1].entidad_financiera   = v_cve_ent_financiera USING "&&&",' - ',v_ent_financiera CLIPPED  
       LET a_estado_factura[v_indice1].clabe                = v_clabe
       LET a_estado_factura[v_indice1].cta_contable         = v_cta_contable 
       LET a_estado_factura[v_indice1].monto                = v_monto
       LET a_estado_factura[v_indice1].edo_factura          = v_estado_fac CLIPPED, " - ", v_desc_estado_fac CLIPPED
       LET a_estado_factura[v_indice1].edo_valida_factura   = v_estado_val_fac CLIPPED, " - ", v_desc_estado_val_fac CLIPPED
       LET a_estado_factura[v_indice1].f_pago               = v_f_pago  
       LET a_estado_factura[v_indice1].consultar            = v_consultar
       LET v_indice1                                        = v_indice1 + 1
    ELSE
       LET a_estado_fac_rech[v_indice2].documento_fico1     = v_documento_fico   
       LET a_estado_fac_rech[v_indice2].referencia1         = v_referencia
       LET a_estado_fac_rech[v_indice2].entidad_financiera1 = v_cve_ent_financiera USING "&&&",' - ',v_ent_financiera CLIPPED  
       LET a_estado_fac_rech[v_indice2].clabe1              = v_clabe
       LET a_estado_fac_rech[v_indice2].cta_contable1       = v_cta_contable
       LET a_estado_fac_rech[v_indice2].monto1              = v_monto
       LET a_estado_fac_rech[v_indice2].edo_factura1        = v_estado_fac CLIPPED, " - ", v_desc_estado_fac CLIPPED
       LET a_estado_fac_rech[v_indice2].edo_valida_factura1 = v_estado_val_fac CLIPPED, " - ", v_desc_estado_val_fac CLIPPED
       --LET a_estado_fac_rech[v_indice2].f_pago              = v_f_pago  
       --LET a_estado_fac_rech[v_indice2].consultar           = v_consultar
       LET v_indice2                                        = v_indice2 + 1
    END IF      
  END FOREACH                           
  
  CALL a_estado_factura.deleteElement(v_indice1)
  LET v_indice1 = v_indice1 - 1  

  CALL a_estado_fac_rech.deleteElement(v_indice2)
  LET v_indice2 = v_indice2 - 1  

  FREE cur_estado_factura

  DISPLAY "ACEPTADOS    : ", v_indice1
  DISPLAY "RECHAZADOS   : ", v_indice2
  
  RETURN v_indice1, v_indice2
END FUNCTION  

FUNCTION fn_verificar_apo_sele()
  DEFINE v_bnd_ok            SMALLINT
  DEFINE i                   SMALLINT

  LET v_bnd_ok = 0

  FOR i = 1 TO a_estado_factura.getLength()
      IF a_estado_factura[i].consultar = 1 THEN
         LET v_bnd_ok = 1
         EXIT FOR
      END IF
  END FOR

  RETURN v_bnd_ok
END FUNCTION

FUNCTION fn_genera_archivo(p_folio_facturacion, p_f_facturacion_ini, p_f_facturacion_fin)
  DEFINE p_folio_facturacion DECIMAL(9,0), --Folio de transacción
         p_f_facturacion_ini DATE,         --Fecha de transacción inicial
         p_f_facturacion_fin DATE          --Fecha de transacción final 

  DEFINE  v_nom_archivo      VARCHAR(40),  --Nombre del archivo de salida
          v_ruta_envio_dis   CHAR(40),
          v_ruta_nomarch     VARCHAR(100), --Ruta y nombre del archivo de salida
          v_ch_arch_salida   BASE.CHANNEL,          
          v_comando_dos      STRING,
          v_encabezado       STRING,
          v_detalle          STRING,
          v_sumario          STRING

  DEFINE v_documento_fico    CHAR(10),
         v_referencia        CHAR(16),
         v_cve_ent_financiera CHAR(5),
         v_ent_financiera    CHAR(65),
         v_clabe             CHAR(18),
         v_cta_contable      CHAR(10),    
         v_monto             DECIMAL(18,6),
         v_estado_fac        CHAR(5),
         v_desc_estado_fac   VARCHAR(60),
         v_estado_val_fac    CHAR(5),
         v_desc_estado_val_fac VARCHAR(60),
         v_f_pago            DATE,
         v_consultar         SMALLINT,
         v_f_archivo,        DATE,
         v_interface         CHAR(02)

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING,
    v_indice                 SMALLINT

  DEFINE a_info_edo_factura  DYNAMIC ARRAY OF RECORD
    documento_fico           CHAR(10),
    referencia               CHAR(16),
    entidad_financiera       CHAR(65),
    clabe                    CHAR(18), 
    cta_contable             CHAR(10),   
    monto                    DECIMAL(18,6),
    edo_factura              CHAR(50),
    edo_valida_factura       CHAR(50),
    f_pago                   DATE,
    f_archivo                DATE, 
    interface                CHAR(2)
  END RECORD 
  
  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".ocg"
  LET v_nom_archivo   = "/fact_status_factura_", v_hora

  --Se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "ocg"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  --Se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")  

  LET g_sql_txt = ""
  LET g_sql_txt = "\n SELECT fa.num_poliza,                                  ",
                  "\n        fa.referencia,                                  ",
                  "\n        fa.cve_ent_financiera,                          ",
                  "\n        ef.ent_financiera_desc,                         ", 
                  "\n        fa.clabe,                                       ",
                  "\n        ef.cta_contable,                                ",
                  "\n        SUM(importe) AS monto,                          ",
                  "\n        fa.f_pago,                                      ",
                  "\n        TODAY AS f_archivo,                             ",
                  "\n        'AS' AS interface,                              ",
                  "\n        0                                               ",
                  "\n FROM   dis_ctr_factura_aps fa,                         ",
                  "\n        OUTER cat_cta_cnt_ocg ef                        ",
                  "\n WHERE  fa.cve_ent_financiera = ef.cve_ent_financiera   ",
                  "\n AND    fa.tpo_credito	       = ef.tpo_credito          "
  
  IF p_folio_facturacion IS NOT NULL THEN          
     LET g_sql_txt = g_sql_txt,"\n AND fa.folio_factura = ",p_folio_facturacion

     LET v_encabezado = "FOLIO FACTURA: ",p_folio_facturacion
     CALL v_ch_arch_salida.write([v_encabezado]) 
  END IF

  IF p_f_facturacion_ini IS NOT NULL AND p_f_facturacion_fin IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt,"\n AND fa.f_factura >= '",p_f_facturacion_ini,"'",
                               "\n AND fa.f_factura <= '",p_f_facturacion_fin,"'"

     LET v_encabezado = "PERIODO FECHAS: ",p_f_facturacion_ini USING "dd-mm-yyyy", " - ", p_f_facturacion_fin USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF  

  LET g_sql_txt = g_sql_txt,"\n GROUP BY 1,2,3,4,5,6,8 ",
                            "\n ORDER BY 3,1,2 "  

  DISPLAY "g_sql_txt: -",g_sql_txt,"-"  
      
  PREPARE pr_sl_inf_arc FROM g_sql_txt
  DECLARE cur_sl_inf_arc CURSOR FOR pr_sl_inf_arc

  --Imprime encabezado del archivo
  LET v_encabezado = "DOCUMENTO FICO | REFERENCIA | ENTIDAD FINANCIERA | CLABE | ACREEDOR | MONTO | STATUS FACTURA | STATUS VALIDA FACTURA | FECHA PAGO | FECHA ARCHIVO |INTERFACE   "
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice          = 1
  LET v_tot_monto_pesos = 0
  
  FOREACH cur_sl_inf_arc INTO v_documento_fico,    
                              v_referencia,
                              v_cve_ent_financiera,
                              v_ent_financiera,
                              v_clabe,
                              v_cta_contable,
                              v_monto,
                              v_f_pago,
                              v_f_archivo,
                              v_interface

    LET v_estado_fac              = 0
    INITIALIZE v_desc_estado_fac TO NULL
      
    LET g_sql_txt = "\n SELECT UNIQUE ef.estado, cef.desc_edo_fac_aps    ",
                    "\n FROM   dis_ctr_factura_aps fac,                  ",
                    "\n	       dis_ctr_edo_factura_aps  ef,              ",
                    "\n OUTER  cat_edo_factura_aps cef             ",
                    "\n WHERE  fac.referencia    = ef.referencia         ",
                    --"\n AND    fac.num_poliza    = ef.num_poliza         ",
                    "\n AND    ef.estado         = cef.cod_edo_fac_aps   ",
                    "\n AND    ef.tpo_estado     = 1                     ",
                    --"\n        AND fac.num_poliza = '",v_documento_fico   ,"'",
                    "\n AND    fac.referencia    = '",v_referencia       ,"'"

    --DISPLAY g_sql_txt               
    PREPARE ps_edo_factura1 FROM g_sql_txt
    EXECUTE ps_edo_factura1 INTO v_estado_fac, v_desc_estado_fac

    LET v_estado_val_fac              = 0
    INITIALIZE v_desc_estado_val_fac TO NULL
      
    LET g_sql_txt = "\n SELECT UNIQUE ef.estado, cevf.desc_edo_val_fac_aps  ",   
                    "\n FROM   dis_ctr_factura_aps fac,                     ",
                    "\n 	   dis_ctr_edo_factura_aps  ef,                 ",
                    "\n        OUTER cat_edo_val_factura_aps cevf           ",
                    "\n WHERE  fac.referencia         = ef.referencia            ",
                    --"\n        AND  fac.num_poliza     = ef.num_poliza            ",
                    "\n        AND  ef.estado         = cevf.cod_edo_val_fac_aps ",
                    "\n        AND  ef.tpo_estado     = 2                        ",
                    -- "\n       AND  fac.num_poliza     = '",v_documento_fico      ,"'",
                    "\n        AND  fac.referencia    = '",v_referencia          ,"'"

    --DISPLAY g_sql_txt               
    PREPARE ps_edo_val_factura1 FROM g_sql_txt
    EXECUTE ps_edo_val_factura1 INTO v_estado_val_fac, v_desc_estado_val_fac

    LET a_info_edo_factura[v_indice].documento_fico     = v_documento_fico   
    LET a_info_edo_factura[v_indice].referencia         = v_referencia
    LET a_info_edo_factura[v_indice].entidad_financiera = v_cve_ent_financiera USING "&&&",' - ',v_ent_financiera CLIPPED  
    LET a_info_edo_factura[v_indice].clabe              = v_clabe
    LET a_info_edo_factura[v_indice].cta_contable       = v_cta_contable
    LET a_info_edo_factura[v_indice].monto              = v_monto
    LET a_info_edo_factura[v_indice].edo_factura        = v_estado_fac CLIPPED, " - ", v_desc_estado_fac CLIPPED
    LET a_info_edo_factura[v_indice].edo_valida_factura = v_estado_val_fac CLIPPED, " - ", v_desc_estado_val_fac CLIPPED
    LET a_info_edo_factura[v_indice].f_pago             = v_f_pago
    LET a_info_edo_factura[v_indice].f_archivo          = v_f_archivo   
    LET a_info_edo_factura[v_indice].interface          = v_interface

    LET v_detalle = a_info_edo_factura[v_indice].documento_fico, "|",
                    a_info_edo_factura[v_indice].referencia, "|",
                    a_info_edo_factura[v_indice].entidad_financiera CLIPPED, "|",
                    a_info_edo_factura[v_indice].clabe, "|",
                    a_info_edo_factura[v_indice].cta_contable, "|",
                    a_info_edo_factura[v_indice].monto USING "###,###,##&.##", "|",
                    a_info_edo_factura[v_indice].edo_factura,"|",
                    a_info_edo_factura[v_indice].edo_valida_factura,"|",
                    a_info_edo_factura[v_indice].f_pago USING "dd-mm-yyyy", "|",
                    a_info_edo_factura[v_indice].f_archivo USING "dd-mm-yyyy", "|",
                    a_info_edo_factura[v_indice].interface

    CALL v_ch_arch_salida.write([v_detalle])

    LET v_tot_monto_pesos = v_tot_monto_pesos + a_info_edo_factura[v_indice].monto
    LET v_indice          = v_indice          + 1 
  END FOREACH

  FREE cur_sl_inf_arc
  
  CALL a_info_edo_factura.deleteElement(v_indice)
  LET v_indice  = v_indice - 1 

  LET v_sumario = "TOTALES: | | | | |",
                  v_tot_monto_pesos USING "###,###,##&.##", " | | | | | "
  CALL v_ch_arch_salida.write([v_sumario])

  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo del Status de la Factura \n en la ruta "||v_ruta_nomarch,"information")
END FUNCTION 

FUNCTION fn_reporte(v_indice1,
                    p_folio_facturacion, 
                    p_f_facturacion_ini, 
                    p_f_facturacion_fin)
  DEFINE p_folio_facturacion DECIMAL(9,0), 
         p_f_facturacion_ini DATE, 
         p_f_facturacion_fin DATE
                    
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos reporte
  DEFINE v_rep_indice        INTEGER
  DEFINE v_tot_reporte       SMALLINT 
  DEFINE v_indice1           SMALLINT
  DEFINE v_encabezado        SMALLINT --1 detalle, 2 totales por tipo de crédito y concepto

  --Genera el reporte en PDF
  IF fgl_report_loadCurrentSettings("DISM021.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte
  START REPORT rep_estado_factura TO XML HANDLER manejador_rpt
    LET v_tot_reporte = 1
    LET v_encabezado  = 1

    FOR v_rep_indice = 1 TO  v_indice1
        OUTPUT TO REPORT rep_estado_factura(p_folio_facturacion, 
                                            p_f_facturacion_ini, 
                                            p_f_facturacion_fin,
                                            a_estado_factura[v_rep_indice].*, 
                                            v_tot_monto_pesos,  
                                            v_encabezado, 
                                            v_tot_reporte, 
                                            v_indice1)
    END FOR

    FOR v_rep_indice = 1 TO  v_indice2
        OUTPUT TO REPORT rep_estado_factura(p_folio_facturacion, 
                                            p_f_facturacion_ini, 
                                            p_f_facturacion_fin,
                                            a_estado_fac_rech [v_rep_indice].*, "",0,
                                            v_tot_monto_pesos,  
                                            v_encabezado, 
                                            v_tot_reporte, 
                                            v_indice1)
    END FOR
  FINISH REPORT rep_estado_factura
END FUNCTION

#Objetivo: Estructura reporte de Estado de la Factura
REPORT rep_estado_factura(p_folio_facturacion, 
                          p_f_facturacion_ini, 
                          p_f_facturacion_fin,
                          r_estado_factura,  
                          v_t_pesos, 
                          v_encabezado, 
                          v_tot_reporte, 
                          v_total_registros)
  DEFINE p_folio_facturacion DECIMAL(9,0), 
         p_f_facturacion_ini DATE, 
         p_f_facturacion_fin DATE

  --Datos de detalle
  DEFINE r_estado_factura    RECORD    
    documento_fico           CHAR(10),
    referencia               CHAR(16),
    entidad_financiera       CHAR(65),
    clabe                    CHAR(18), 
    cta_contable             CHAR(10),
    monto                    DECIMAL(18,6),
    edo_factura              CHAR(50),
    edo_valida_factura       CHAR(50),
    f_pago                   DATE,
    consultar                SMALLINT
  END RECORD   

  --Cifras totales de 
  DEFINE v_t_pesos           DECIMAL(18,6)  
  
  DEFINE v_fecha_consulta    DATE, --Fecha de proceso 
         v_tot_reporte       SMALLINT, 
         v_encabezado        SMALLINT, 
         v_total_registros   SMALLINT
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      PRINTX g_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"

      PRINTX p_folio_facturacion 
      PRINTX p_f_facturacion_ini USING "dd-mm-yyyy"
      PRINTX p_f_facturacion_fin USING "dd-mm-yyyy"

    PAGE HEADER            
      PRINTX g_usuario

      PRINTX p_folio_facturacion 
      PRINTX p_f_facturacion_ini USING "dd-mm-yyyy"
      PRINTX p_f_facturacion_fin USING "dd-mm-yyyy"
    
    ON EVERY ROW
       PRINTX r_estado_factura.documento_fico
       PRINTX r_estado_factura.referencia 
       PRINTX r_estado_factura.entidad_financiera 
       PRINTX r_estado_factura.clabe
       PRINTX r_estado_factura.cta_contable
       PRINTX r_estado_factura.monto USING "###,###,###,###,##&.##"
       PRINTX r_estado_factura.edo_factura
       PRINTX r_estado_factura.edo_valida_factura
       PRINTX r_estado_factura.f_pago USING "dd-mm-yyyy"

    ON LAST ROW
       PRINTX v_t_pesos USING "###,###,###,###,##&.##"
END REPORT

FUNCTION fn_val_rch_cons_pag(g_referencia)
  DEFINE g_referencia        CHAR(16),
         v_folio_factura     DECIMAL(9,0),
         v_f_factura         DATE, 
         v_tipo_credito      SMALLINT,
         v_entidad_financiera SMALLINT

  DEFINE v_ident_rech        SMALLINT
  DEFINE v_indice2           SMALLINT
  DEFINE v_ls_query          STRING

  --LET g_referencia = g_referencia[1,13]
  
  SELECT COUNT(*)
  INTO   v_ident_rech
  FROM   dis_ctr_edo_factura_aps a
  WHERE  a.referencia = g_referencia
  AND    a.tpo_estado = 2
  AND    a.estado     = 4
  IF v_ident_rech >= 1 THEN
     -----Busqueda de Rechazo
     LET v_ls_query = ""
     LET v_ls_query = "\n SELECT UNIQUE df.folio_factura,                  ",
                      "\n        df.f_factura,                             ",
                      "\n        df.tpo_credito,                           ",
                      "\n        df.cve_ent_financiera                     ",
                      "\n FROM   dis_ctr_factura_aps df                    ",
                      "\n WHERE  df.referencia   = '", g_referencia,     "'"

     --DISPLAY "v_ls_query: -",v_ls_query,"-"

     PREPARE pr_val_rch_pag FROM v_ls_query
     DECLARE cur_val_rch_pag CURSOR FOR pr_val_rch_pag
     LET v_indice2 = 1
     FOREACH cur_val_rch_pag INTO v_folio_factura,
                                  v_f_factura,
                                  v_tipo_credito,
                                  v_entidad_financiera

       UPDATE dis_ctr_aps_tns
       SET    estado             = 30
       WHERE  folio_factura      = v_folio_factura
       AND    f_factura          = v_f_factura
       AND    cve_ent_financiera = v_entidad_financiera
       AND    tpo_credito        = v_tipo_credito
     END FOREACH
  END IF

  FREE cur_val_rch_pag

  LET v_ident_rech = 0
  LET v_indice2    = 0

  SELECT COUNT(*)
  INTO   v_ident_rech
  FROM   dis_ctr_edo_factura_aps a
  WHERE  a.referencia = g_referencia
  AND    a.tpo_estado = 2
  AND    a.estado     = 2
  IF v_ident_rech >= 1 THEN
     -----Busqueda de Pagado
     LET v_ls_query = ""
     LET v_ls_query = "\n SELECT UNIQUE df.folio_factura,                  ",
                      "\n        df.f_factura,                             ",
                      "\n        df.tpo_credito,                           ",
                      "\n        df.cve_ent_financiera                     ",
                      "\n FROM   dis_ctr_factura_aps df                    ",
                      "\n WHERE  df.referencia   = '", g_referencia,     "'"

     DISPLAY "v_ls_query: -",v_ls_query,"-"

     PREPARE pr_val_ace_pag FROM v_ls_query
     DECLARE cur_val_ace_pag CURSOR FOR pr_val_ace_pag
     LET v_indice2 = 1  
     FOREACH cur_val_ace_pag INTO v_folio_factura,
                                  v_f_factura,
                                  v_tipo_credito,
                                  v_entidad_financiera

       UPDATE dis_ctr_aps_tns
       SET    estado             = 70
       WHERE  folio_factura      = v_folio_factura
       AND    f_factura          = v_f_factura
       AND    cve_ent_financiera = v_entidad_financiera
       AND    tpo_credito        = v_tipo_credito
     END FOREACH
  END IF

  FREE cur_val_ace_pag
END FUNCTION