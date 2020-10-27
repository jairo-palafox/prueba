################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 12/03/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC20                                                   #
#Objetivo          => Programa para consultar la dispersion por NSS            #
#                                                                              #
#Fecha inicio      => 14/01/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE 
    g_sql_txt                STRING,      --Consultas
    g_usuario                VARCHAR(30), --Almacena al usuario
    g_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa
    g_nom_prog               VARCHAR(30), --Almacena opción del menú
    p_proceso_cod            SMALLINT,    --codigo del proceso
    v_proceso_cod            SMALLINT,    --codigo del proceso
    p_opera_cod              SMALLINT,    --codigo de operacion
    p_pid                    DECIMAL(9,0)

  DEFINE 
    v_tot_registros          DECIMAL(9,0), -- Total de registros
    v_tot_aivs               DECIMAL(18,6),
    v_tot_aportacion         DECIMAL(12,2), 
    v_tot_amortizacion       DECIMAL(12,2)

  DEFINE v_arr_movimientos DYNAMIC ARRAY OF RECORD
            v_nss                CHAR(11),
            v_fecha_liquida      DATE,
            v_folio_pago         DECIMAL(9,0),
            v_folio_liquida      DECIMAL(9,0),
            v_destino_dis        VARCHAR(25),
            v_periodo_pago       CHAR(06),
            v_fecha_pago         DATE,
            v_folio_sua          DECIMAL(10,0),
            v_nrp                VARCHAR(11),
            v_num_credito        DECIMAL(10,0),
            v_monto_aportacion   DECIMAL(12,2),
            v_monto_amortizacion DECIMAL(12,2),
            v_aivs               DECIMAL(18,6)
  END RECORD

  DEFINE  v_tipo_trabajador       VARCHAR(45),
          v_estado_credito        VARCHAR(45),
          v_tipo_credito_desc     VARCHAR(30),
          v_num_credito_vig       DECIMAL(10,0), 
          v_f_otorga              DATE

  DEFINE v_arr_cifras_reporte DYNAMIC ARRAY OF RECORD
            v_estado       SMALLINT,
            v_desc_estado  VARCHAR(50),
            v_tot_estado   INTEGER
         END RECORD

  DEFINE v_edo_credito_desc      VARCHAR(40),
         v_desc_tipo_trabajador  VARCHAR(40),
         v_desc_estado           VARCHAR(50)

  DEFINE 
    v_nombre_completo        VARCHAR(50), --Nombre del derechohabiente
    v_periodo_cvt1           VARCHAR(6),
    v_periodo_cvt2           VARCHAR(6),
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
  DEFINE 
    --f_folio                  DECIMAL(9,0),  --Folio de la liquidación de la dispersión
    v_nss                    CHAR(11),      --NSS trabajador
    v_num_credito            DECIMAL(10,0), --Número de Crédito
    r_registros              DECIMAL(9,0),  --Bandera de consulta avance de pago
    r_sum_aivs               DECIMAL(18,6),
    r_sum_aportacion         DECIMAL(12,2),
    r_sum_amortizacion       DECIMAL(12,2) 
  
  DEFINE 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form ,  --Define las propiedades de la forma
    v_ruta_listados          CHAR(40),
    v_id_derechohabiente     DECIMAL(9,0),
    --v_cuenta_folio           INTEGER,
    bnd_consulta             SMALLINT,
    r_bnd_periodo            SMALLINT,
    v_qwery_ibx              STRING 


   DEFINE l_comando           STRING
   DEFINE v_ruta_ejecutable   CHAR(40)
   DEFINE v_max_pid           LIKE bat_ctr_proceso.pid
   DEFINE v_folio             LIKE dis_det_avance_pago.folio
   DEFINE l_v_arch_proceso    VARCHAR(100)
   DEFINE v_cont_num_cred     INTEGER

  --Recibe valores de argumentos
  LET g_usuario      = ARG_VAL(1)
  LET g_tipo_proceso = ARG_VAL(2)
  LET g_nom_prog     = ARG_VAL(3)
  LET p_proceso_cod  = 920 --903
  LET p_opera_cod    = 1 
  LET r_bnd_periodo  = 0

  INITIALIZE l_v_arch_proceso TO NULL

  --validación que NO se tenga Preliquidación de Dispersión de Pagos ejecutándose
  {IF f_existe_proceso_operacion_ejecutando(901, 1) THEN
     MENU "No se puede ejecutar" ATTRIBUTES ( STYLE="dialog", 
       COMMENT="Preliquidación de Dispersión de Pagos ejecutándose,\ningrese a esta opción cuando finalice",
       IMAGE="information" )
       ON ACTION salir
          RETURN
     END MENU
  END IF}

  DATABASE safre_viv
  ##### Se añade modificación de la variable de informix para optimización de consulta #####
   
  --Actualizamos variable de informix para maximizar prioridad de la BD
  LET v_qwery_ibx = "SET PDQPRIORITY HIGH;"
  PREPARE prp_performance FROM v_qwery_ibx
  EXECUTE prp_performance
   
  --Obtiene ruta listados
  SELECT ruta_listados
  INTO   v_ruta_listados
  FROM   seg_modulo 
  WHERE  modulo_cod = 'bat'

   --Obtiene las rutas ejecutable
   SELECT ruta_bin 
   INTO   v_ruta_ejecutable
   FROM   seg_modulo 
   WHERE  modulo_cod = 'dis'

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

  LET bnd_consulta = 0

  CLOSE WINDOW SCREEN
   
  OPEN WINDOW w1 WITH FORM "DISC201"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT BY NAME v_nss, v_num_credito
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()

          
          CALL f_forma.setElementHidden("lbl_nombre", 1 ) --Oculta etiqueta de nombre
          CALL f_forma.setFieldHidden("txt_nombre", 1 ) --Oculta nombre dle trabajador
          CALL f_forma.setElementHidden("gr_credito", 1 ) --Oculta datos del crédito
          CALL f_forma.setElementHidden("gr_detalle", 1 ) --Oculta detalle de la consulta
          CALL f_forma.setElementHidden("gr_registros", 1 ) --Oculta total de registros
          CALL f_forma.setElementHidden("gr_aportaciones", 1 ) --Oculta total de aportaciones
          CALL f_forma.setElementHidden("gr_amortizaciones", 1 ) --Oculta total de amortizaciones
          CALL f_forma.setElementHidden("gr_aivs", 1 ) --Oculta total de aivs
          NEXT FIELD v_nss
          CALL ui.interface.refresh()

       
        ON ACTION ACCEPT 
           --Valida que se inserte al menos un parámetro
           IF (v_nss IS NULL AND v_num_credito IS NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un NSS ó un Número de Crédito para la búsqueda",
                              "about")
              NEXT FIELD v_nss   
           END IF             

           IF (v_nss IS NOT NULL AND v_num_credito IS NOT NULL) THEN
              CALL fn_mensaje("ATENCIÓN",
                              "Debe capturar un NSS ó un Número de Crédito para la búsqueda, no ambos.",
                              "about") 
              NEXT FIELD v_nss   
           END IF 
           {IF f_folio IS NOT NULL THEN
              --Valida que exista el folio
                 SELECT COUNT(*) 
                 INTO   v_cuenta_folio
                 FROM   dis_ctr_liq_incons
                 WHERE  folio = f_folio
                 IF v_cuenta_folio  = 0    OR 
                    v_cuenta_folio IS NULL THEN 
                    CALL fn_mensaje("Atención","No existen registros para el folio capturado",
                                    "about")
                    NEXT FIELD f_folio
                 END IF 
           END IF}
               
           IF v_nss IS NOT NULL THEN 
              IF LENGTH(v_nss) = 11 THEN 
                 --Valida que exista el nss
                 SELECT COUNT(id_derechohabiente) 
                 INTO   v_id_derechohabiente
                 FROM   afi_derechohabiente
                 WHERE  nss = v_nss
                 
                 IF v_id_derechohabiente = 0     OR 
                    v_id_derechohabiente IS NULL THEN 
                    CALL fn_mensaje("Atención","El NSS no existe",
                                    "about")
                    NEXT FIELD v_nss
                 END IF 
              ELSE 
                 CALL fn_mensaje("Atención","El NSS debe ser de 11 caracteres",
                                 "about")
                 NEXT FIELD v_nss
              END IF 
           END IF
           LET v_cont_num_cred = 0 
           IF v_num_credito IS NOT NULL AND v_nss IS NULL THEN 
              SELECT COUNT(num_crd_ifv)
                INTO v_cont_num_cred
                FROM dis_interface_hs 
               WHERE num_crd_ifv = v_num_credito

              IF v_cont_num_cred = 0 THEN 
                 SELECT COUNT(num_crd_ifv)
                   INTO v_cont_num_cred
                   FROM dis_interface_ef  
                  WHERE num_crd_ifv = v_num_credito

                  IF v_cont_num_cred = 0 THEN
                     SELECT COUNT(num_crd_ifv)
                       INTO v_cont_num_cred
                       FROM dis_info_inconsistente
                      WHERE num_crd_ifv = v_num_credito

                     IF v_cont_num_cred = 0 THEN  
                         SELECT COUNT(num_credito)
                           INTO v_cont_num_cred
                           FROM dse_devolucion 
                          WHERE num_credito = v_num_credito

                        IF  v_cont_num_cred = 0 THEN 
                           CALL fn_mensaje("Atención","No se encontraron registros.","about")
                           NEXT FIELD v_num_credito                               
                        END IF                          
                     END IF                      
                  END IF 
              END IF
           END IF
           
           SELECT id_derechohabiente 
             INTO v_id_derechohabiente
             FROM afi_derechohabiente
            WHERE nss = v_nss

           DISPLAY "v_nss -- ",v_nss
           DISPLAY "v_num_credito -- ",v_num_credito
           
           --DISPLAY "f_folio -- ",f_folio
            
           CALL fn_consultar_movimientos(v_nss, v_id_derechohabiente, v_num_credito) 
                                          RETURNING   r_registros,
                                                      r_sum_aivs,
                                                      r_sum_aportacion,
                                                      r_sum_amortizacion

           DISPLAY "r_registros -- ",r_registros

           IF r_registros > 0 THEN
              CALL f_forma.setElementHidden("lbl_nombre", 0 ) --muestra etiqueta de nombre
              CALL f_forma.setFieldHidden("txt_nombre", 0 ) --muestra nombre dle trabajador
              IF v_num_credito IS NULL THEN  
                 CALL f_forma.setElementHidden("gr_credito", 0 ) --muestra datos del credito de la consulta
              END IF              
              CALL f_forma.setElementHidden("gr_detalle", 0 ) --muestra detalle de la consulta
              CALL f_forma.setElementHidden("gr_registros", 0 ) --Muestra total de registros
              CALL f_forma.setElementHidden("gr_aportaciones", 0 ) --Muestra  total de aportaciones
              CALL f_forma.setElementHidden("gr_amortizaciones", 0 ) --Muestra total de amortizaciones
              CALL f_forma.setElementHidden("gr_aivs", 0 ) --Muestra total de aivs
              
              DISPLAY ARRAY v_arr_movimientos TO sr_det_movimientos.*
              ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE)
                BEFORE ROW
                  CALL fn_obtiene_nombre_derechohabiente(v_id_derechohabiente) 
                  DISPLAY ARR_CURR()
               
                BEFORE DISPLAY 
                  --DISPLAY r_registros        TO txt_tot_reg
                  DISPLAY r_sum_aivs         TO txt_tot_aivs
                  DISPLAY r_sum_aportacion   TO txt_tot_aportaciones
                  DISPLAY r_sum_amortizacion TO txt_tot_amortizaciones

                AFTER DISPLAY 
                  CALL ui.interface.refresh()
                  CALL DIALOG.setActionHidden("reporte",0)                  
                  CALL DIALOG.setActionHidden("btn_liquidar", 0)                  
                  CALL ui.interface.refresh()

                ON ACTION cancelar 
                   EXIT PROGRAM 
                    
                ON ACTION reporte
                   CALL fn_reporte_inconsistencias(v_nss, v_id_derechohabiente, v_num_credito)
                --Genera archivo
                
                ON ACTION archivo
                   CALL fn_genera_archivo_consulta(v_nss, v_id_derechohabiente, v_num_credito)   
                  
              END DISPLAY                    
           ELSE
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros",
                              "about")
              CALL ui.interface.refresh()
           END IF

        --Se invoca la generación de archivo masivo
        {ON ACTION btn_arcmas
           CALL fn_consultar_movimientos(v_nss, v_id_derechohabiente)
                                   RETURNING r_registros,
                                             r_sum_aivs,
                                             r_sum_aportacion,
                                             r_sum_amortizacion
                          
           IF r_registros >0 THEN          
              CALL fn_genera_archivo_masivo(v_nss, v_id_derechohabiente)
           ELSE 
              CALL fn_mensaje("ATENCIÓN",
                              "No se encontraron registros",
                              "about")
              CALL ui.interface.refresh()
           END IF}

         AFTER FIELD v_num_credito
            NEXT FIELD v_nss
            
      END INPUT
      
      ON ACTION cancelar
         EXIT DIALOG      

          
    END DIALOG 
  CLOSE WINDOW w1 

END MAIN

#Objetivo: Consulta para verificar si existe información con los parametros 
#          capturados
FUNCTION fn_consultar_movimientos(p_nss, p_id_derechohabiente, p_num_credito)
  DEFINE p_nss                CHAR(11),
         --f_folio              DECIMAL(9,0), --Folio de liquidación dispersión
         p_id_derechohabiente DECIMAL(9,0), -- Id
         p_num_credito        DECIMAL(10,0), --Número de Crédito
         v_indice             INTEGER
         

  DEFINE  v_edo_credito       SMALLINT, 
          v_tpo_originacion   SMALLINT, 
          v_tpo_credito       SMALLINT, 
--          v_num_credito_vig   DECIMAL(10,0), 
--          v_f_otorga          DATE,
          v_f_liquida         DATE,
          j                   SMALLINT, 
          v_dest_dis          STRING

      
   CALL fn_credito_vivienda(p_id_derechohabiente, 1) RETURNING  v_edo_credito,
                                                                v_tpo_originacion,
                                                                v_tpo_credito,
                                                                v_num_credito_vig,
                                                                v_f_otorga,
                                                                v_f_liquida

   --Valor v_edo_credito = -2: No hay NSS asociado
   --Valor v_edo_credito = -1: No hay identificador de derechohabiente
   --Valor v_edo_credito = 0: 	Existe crédito vigente
   --Valor v_edo_credito = 1: 	No tiene crédito
   --Valor v_edo_credito = 2: 	Tiene crédito Liquidado
   --DISPLAY "v_edo_credito: ", v_edo_credito
   CASE v_edo_credito
      WHEN "-2"
         LET v_edo_credito_desc = " SIN NSS ASOCIADO AL NÚMERO DE CRÉDITO"
      WHEN "-1"
         LET v_edo_credito_desc = " SIN IDENTIFICADOR DE DERECHOHABIENTE"
      WHEN "0"
         LET v_edo_credito_desc = " CRÉDITO VIGENTE"
      WHEN "1"
         LET v_edo_credito_desc = " SIN CRÉDITO"
      WHEN "2"
         LET v_edo_credito_desc = " CRÉDITO LIQUIDADO"
      OTHERWISE
         LET v_edo_credito_desc = " SIN CRÉDITO"
   END CASE

   --DISPLAY "v_tpo_originacion: ", v_tpo_originacion
   SELECT originacion_desc
   INTO v_desc_tipo_trabajador 
   FROM cat_cre_originacion
   WHERE tpo_originacion = v_tpo_originacion

   SELECT UNIQUE(desc_credito)
   INTO v_tipo_credito_desc
   FROM cat_tipo_credito
   WHERE tpo_credito = v_tpo_credito

   LET v_tipo_trabajador = v_tpo_originacion CLIPPED||" "||v_desc_tipo_trabajador
   LET v_estado_credito  = v_edo_credito CLIPPED||" "||v_edo_credito_desc 
   LET v_tipo_credito_desc =  v_tpo_credito CLIPPED||" "||v_tipo_credito_desc

   DISPLAY v_estado_credito TO txt_edo_credito
   DISPLAY v_tipo_trabajador TO txt_tpo_originacion
   DISPLAY v_tipo_credito_desc TO txt_tpo_credito
   DISPLAY v_num_credito_vig TO txt_num_cred_vig
   DISPLAY v_f_otorga USING "dd-mm-yyyy" TO de_fec_otorga

   {LET g_sql_txt = "EXECUTE FUNCTION fn_dis_informacion(?, ?)" 
   PREPARE ps_fn_dis_informacion FROM g_sql_txt
   EXECUTE ps_fn_dis_informacion USING p_nss, p_id_derechohabiente}

   DISPLAY "nss: ", p_nss
   DISPLAY "id_derechohabiente: ", p_id_derechohabiente
   DISPLAY "num_cred: ", p_num_credito

   LET g_sql_txt = "" 

   LET g_sql_txt = "\n   SELECT  afi.nss as nss,",
                   "\n           glo.f_actualiza as fecha_liquida,",   --Obtenemos la información de la tabla dis_interface_hs (Hipotecaria Social o Cartera)
                   "\n           glo.folio_referencia as folio_pago,", 
                   "\n           dhs.folio_liquida as folio_liquida,",
                   "\n           ","'","Hipotecaria Social","'"," as destino_dis,",
                   "\n           dhs.periodo_pago as periodo_pago,",
                   "\n           dhs.f_pago as fecha_pago,",
                   "\n           dhs.folio_sua as folio_sua,",
                   "\n           dhs.nrp as nrp,",
                   "\n           dhs.num_crd_ifv as num_credito,",
                   "\n           nvl(dhs.imp_ap_pat, 0) as monto_aportaciones,",
                   "\n           nvl(dhs.imp_am_cre, 0) as monto_amortizaciones,",
                   "\n           nvl(dhs.aiv_ap_pat,0) as aivs",
                   "\n   FROM  dis_interface_hs dhs,",
                   "\n         afi_derechohabiente afi,",
                   "\n   OUTER glo_folio glo",
                   "\n   WHERE dhs.folio_liquida = glo.folio",
                   "\n   AND   dhs.id_derechohabiente = afi.id_derechohabiente"

   IF p_num_credito IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt , "\n   AND   dhs.num_crd_ifv   = " , p_num_credito
   ELSE
      LET g_sql_txt = g_sql_txt , "\n   AND   dhs.id_derechohabiente = " , p_id_derechohabiente
   END IF

   LET g_sql_txt = g_sql_txt , "\n   UNION ALL" ,
                               "\n   SELECT   afi.nss as nss," ,
                               "\n            glo.f_actualiza as fecha_liquida," ,  --Obtenemos la información de la tabla dis_interface_ef (Entidades Financieras)
                               "\n            glo.folio_referencia as folio_pago," , 
                               "\n            def.folio_liquida as folio_liquida," ,
                               "\n            ","'","Entidades Financieras","'"," as destino_dis," ,
                               "\n            def.periodo_pago as periodo_pago," ,
                               "\n            def.f_pago as fecha_pago," ,
                               "\n            def.folio_sua as folio_sua," ,
                               "\n            def.nrp as nrp," ,
                               "\n            def.num_crd_ifv as num_credito," ,
                               "\n            nvl(def.imp_ap_pat, 0) as monto_aportaciones," ,
                               "\n            0 as monto_amortizaciones," ,
                               "\n            nvl(def.aiv_ap_pat,0) as aivs" ,
                               "\n   FROM dis_interface_ef def," ,
                               "\n        afi_derechohabiente afi," ,
                               "\n   OUTER glo_folio glo" ,
                               "\n   WHERE def.folio_liquida = glo.folio" ,
                               "\n   AND   def.id_derechohabiente = afi.id_derechohabiente"

   IF p_num_credito IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt , "\n   AND   def.num_crd_ifv = " , p_num_credito
   ELSE
      LET g_sql_txt = g_sql_txt , "\n   AND   def.id_derechohabiente = " , p_id_derechohabiente
   END IF 

   LET g_sql_txt = g_sql_txt , "\n   UNION ALL" ,
                               "\n   SELECT   afi.nss as nss," ,
                               "\n            glo.f_actualiza as fecha_liquida," , --Obtenemos la información de la tabla dse_devolucion (Devoluciones)
                               "\n            glo.folio_referencia as folio_pago," ,
                               "\n            dev.folio_referencia as folio_liquida," ,
                               "\n            ","'","Devolucion Saldos","'"," as destino_dis," ,
                               "\n            dev.periodo_pago as periodo_pago," ,
                               "\n            dev.f_pago as fecha_pago," ,
                               "\n            0 as folio_sua," ,
                               "\n            ","''"," as nrp," ,
                               "\n            dev.num_credito as num_credito," ,
                               "\n            nvl(dev.monto_aportacion, 0) as monto_aportaciones," ,
                               "\n            0 as monto_amortizaciones," ,
                               "\n            nvl(dev.aivs_aportacion,0) as aivs" ,
                               "\n   FROM dse_devolucion dev," ,
                               "\n        afi_derechohabiente afi," ,
                               "\n   OUTER glo_folio glo" ,
                               "\n   WHERE dev.folio_referencia = glo.folio" ,
                               "\n   AND  dev.id_derechohabiente = afi.id_derechohabiente"

   IF p_num_credito IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt , "\n   AND  dev.num_credito = " , p_num_credito
   ELSE
      LET g_sql_txt = g_sql_txt , "\n   AND  dev.id_derechohabiente = " , p_id_derechohabiente
   END IF 

   LET g_sql_txt = g_sql_txt , "\n   UNION ALL" ,
                               "\n   SELECT   afi.nss as nss," ,
                               "\n            glo.f_actualiza as fecha_liquida," , --Obtenemos la información de la tabla dis_info_inconsistente (Inconsistencias)
                               "\n            glo.folio_referencia as folio_pago," ,
                               "\n            dii.folio_liquida as folio_liquida," ,
                               "\n            ","'","Inconsistencias","'"," as destino_dis," ,
                               "\n            pag.periodo_pago as periodo_pago," ,
                               "\n            pag.f_pago as fecha_pago," ,
                               "\n            pag.folio_sua as folio_sua," ,
                               "\n            pag.nrp as nrp," ,
                               "\n            pag.num_crd_ifv as num_credito," ,
                               "\n            nvl(pag.imp_ap_pat, 0) as monto_aportaciones," ,
                               "\n            nvl(pag.imp_am_cre, 0) as monto_amortizaciones," ,
                               "\n            nvl(pag.aiv_ap_pat,0) as aivs" ,
                               "\n   FROM  dis_info_inconsistente dii," ,
                               "\n         glo_folio glo," ,
                               "\n         afi_derechohabiente afi," ,
                               "\n         cta_his_pagos pag" ,
                               "\n   WHERE dii.id_referencia = pag.id_referencia" ,
                               "\n   AND   dii.tpo_inconsistente = 0" ,
                               "\n   AND   dii.folio_liquida = glo.folio" ,
                               "\n   AND   dii.id_derechohabiente = afi.id_derechohabiente"

  IF p_num_credito IS NOT NULL THEN 
     LET g_sql_txt = g_sql_txt , "\n   AND   pag.num_crd_ifv = " , p_num_credito
  ELSE
     LET g_sql_txt = g_sql_txt , "\n   AND   dii.id_derechohabiente = " , p_id_derechohabiente
  END IF 

   LET g_sql_txt = g_sql_txt , "\n   UNION ALL" ,
                               "\n   SELECT   afi.nss as nss," ,
                               "\n            glo.f_actualiza as fecha_liquida," , --Obtenemos la información de la tabla dis_compensa_avance (Devoluciones)
                               "\n            glo.folio_referencia as folio_pago," ,
                               "\n            dca.folio_dis as folio_liquida," ,
                               "\n            ","'","Avance de Pago Recuperado","'"," as destino_dis," ,
                               "\n            dca.periodo_pago as periodo_pago," ,
                               "\n            dca.f_pago as fecha_pago," ,
                               "\n            pag.folio_sua as folio_sua," ,
                               "\n            dca.nrp as nrp," ,
                               "\n            dca.num_credito as num_credito," ,
                               "\n            nvl(dca.monto_apo_pag, 0) as monto_aportaciones," ,
                               "\n            nvl(dca.monto_amo_pag, 0) as monto_amortizaciones," ,
                               "\n            nvl(pag.aiv_ap_pat,0) as aivs" ,
                               "\n   FROM  dis_compensa_avance dca," ,
                               "\n         glo_folio glo," ,
                               "\n         afi_derechohabiente afi," ,
                               "\n         cta_his_pagos pag" ,
                               "\n   WHERE dca.id_referencia      = pag.id_referencia" ,
                               "\n   AND  dca.folio_pago          = pag.folio" ,
                               "\n   AND  dca.folio_dis           = glo.folio" ,
                               "\n   AND  dca.id_derechohabiente  = afi.id_derechohabiente"

   IF p_num_credito IS NOT NULL THEN 
      LET g_sql_txt = g_sql_txt , "\n   AND  dca.num_credito = " , p_num_credito
   ELSE
      LET g_sql_txt = g_sql_txt , "\n   AND  dca.id_derechohabiente = " , p_id_derechohabiente
   END IF 
  
 
  LET g_sql_txt = g_sql_txt , "\n   INTO TEMP tmp_dis_informacion"

  DISPLAY "--- g_sql_txt -- " , g_sql_txt

  PREPARE ps_sl_informacion FROM g_sql_txt

  EXECUTE ps_sl_informacion

  LET g_sql_txt = "\n SELECT * ",
                  "\n FROM tmp_dis_informacion ",
                  "\n ORDER BY fecha_liquida DESC, periodo_pago DESC " 

  --LET g_sql_txt = g_sql_txt||"\n ORDER BY it.estado, it.nss, it.folio_liquida"

  DISPLAY "g_sql_txt -- ",g_sql_txt
  PREPARE ps_dis_informacion FROM g_sql_txt
  
  LET v_indice           = 1
  LET v_tot_registros    = 1
  LET v_tot_aivs         = 0.00
  LET v_tot_aportacion   = 0.00
  LET v_tot_amortizacion = 0.00


  LET j = 1
  CALL v_arr_cifras_reporte.clear()
  LET v_arr_cifras_reporte[j].v_tot_estado = 0

  --Iteración de registros con base en la consulta temporal
  DECLARE cur_dis_informacion CURSOR FOR ps_dis_informacion
  FOREACH cur_dis_informacion INTO v_arr_movimientos[v_indice].v_nss,
                                   v_arr_movimientos[v_indice].v_fecha_liquida,
                                   v_arr_movimientos[v_indice].v_folio_pago,
                                   v_arr_movimientos[v_indice].v_folio_liquida,                                   
                                   v_arr_movimientos[v_indice].v_destino_dis,
                                   v_arr_movimientos[v_indice].v_periodo_pago,
                                   v_arr_movimientos[v_indice].v_fecha_pago,
                                   v_arr_movimientos[v_indice].v_folio_sua,
                                   v_arr_movimientos[v_indice].v_nrp,
                                   v_arr_movimientos[v_indice].v_num_credito,
                                   v_arr_movimientos[v_indice].v_monto_aportacion,
                                   v_arr_movimientos[v_indice].v_monto_amortizacion,
                                   v_arr_movimientos[v_indice].v_aivs
    
      LET v_dest_dis = v_arr_movimientos[v_indice].v_destino_dis    
      LET v_arr_movimientos[v_indice].v_destino_dis = v_dest_dis.toUpperCase()      
                                   
      LET v_tot_registros    = v_tot_registros    + 1
      LET v_tot_aivs         = v_tot_aivs         + v_arr_movimientos[v_indice].v_aivs
      LET v_tot_aportacion   = v_tot_aportacion   + v_arr_movimientos[v_indice].v_monto_aportacion
      LET v_tot_amortizacion = v_tot_amortizacion + v_arr_movimientos[v_indice].v_monto_amortizacion
      LET v_indice           = v_indice           + 1
  END FOREACH

  CALL v_arr_movimientos.deleteElement(v_indice)
  LET v_tot_registros    = v_tot_registros - 1

  RETURN v_tot_registros,
         v_tot_aivs,
         v_tot_aportacion,
         v_tot_amortizacion

END FUNCTION

#Objetivo: Genera reporte de inconsistencias de numero de crédito igual a cero
FUNCTION fn_reporte_inconsistencias(p_nss, p_id_derechohabiente, p_num_credito)
   DEFINE p_nss                    CHAR(11), --NSS trabajador
          p_id_derechohabiente DECIMAL(9,0),
          p_num_credito        DECIMAL(10,0),
          
          manejador_rpt            om.SaxDocumentHandler, --Contenedor documentos reporte
          v_rep_indice             INTEGER
  
  LET v_rep_indice = 1
  
  -- Botón para generar el reporte en PDF de la consulta
  IF fgl_report_loadCurrentSettings("DISC201.4rp") THEN 
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --Inicia el reporte de registros con rechazo
  START REPORT rep_con_disp TO XML HANDLER manejador_rpt
    {FOR v_rep_indice = 1 TO v_arr_movimientos.getLength()
        OUTPUT TO REPORT rep_con_disp(v_arr_movimientos[v_rep_indice].*,
                                        v_tot_registros,
                                        v_tot_aivs,
                                        v_tot_aportacion,
                                        v_tot_amortizacion,
                                        g_usuario,
                                        v_rfolio)
    END FOR}
    
      FOR v_rep_indice = 1 TO  v_arr_movimientos.getLength()
           OUTPUT TO REPORT rep_con_disp( v_arr_movimientos[v_rep_indice].*,
                                           v_tot_registros,
                                           v_tot_aivs,
                                           v_tot_aportacion,
                                           v_tot_amortizacion,
                                           g_usuario, p_nss,
                                           p_num_credito,
                                           v_tipo_trabajador,                                           
                                           v_estado_credito,
                                           v_tipo_credito_desc,
                                           v_num_credito_vig, 
                                           v_f_otorga)
                                           
      END FOR
  
  FINISH REPORT rep_con_disp
END FUNCTION

#Objetivo: Obtiene nombre del derechohabiente
FUNCTION fn_obtiene_nombre_derechohabiente(v_id_consulta)
  DEFINE   
    v_id_consulta            DECIMAL(9,0)

  SELECT   rtrim(nombre_af) ||" "|| rtrim(ap_paterno_af) ||" "|| rtrim(ap_materno_af)
  INTO     v_nombre_completo
  FROM     afi_derechohabiente
  WHERE    id_derechohabiente = v_id_consulta

  DISPLAY "Nombre: ",v_nombre_completo
  DISPLAY v_nombre_completo TO txt_nombre
   
END FUNCTION 

#Objetivo: genera el archivo con la consulta obtenida
FUNCTION fn_genera_archivo_consulta(p_nss, p_id_derechohabiente, p_num_credito) 
   DEFINE p_nss                    CHAR(11), --NSS trabajador
          p_id_derechohabiente DECIMAL(9,0),
          p_num_credito        DECIMAL(10,0), --Número de Crédito
         
          v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
          v_ruta_envio_dis         CHAR(40),
          v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
          v_ch_arch_salida         BASE.CHANNEL,
          v_recorre_arreglo        INTEGER,
          v_comando_dos            STRING,
          v_encabezado             STRING,
          v_detalle                STRING,
          v_sumario                STRING,
          v_folio                  DECIMAL(9,0) --Folio liquidación dispersión
       

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING

  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
   
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/consulta_info_disp_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida.write([v_encabezado])

  --Si se solicitó el folio de dispersión se incluye en el encabezado
  {IF  v_folio IS NOT NULL THEN
      LET v_encabezado = " FOLIO DE CONSULTA ",v_folio
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF}
    
  --Si se solicitó número de seguridad social se incluye en el encabezado
  IF  p_nss IS NOT NULL THEN
      LET v_encabezado = " NSS ",p_nss
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  IF  v_nombre_completo IS NOT NULL THEN
      LET v_encabezado = " NOMBRE ",v_nombre_completo
      CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  IF p_num_credito IS NOT NULL THEN      
     LET v_encabezado = " NÚMERO DE CRÉDITO ",p_num_credito
     CALL v_ch_arch_salida.write([v_encabezado])     
     LET v_encabezado = " " 
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF  
  
  IF p_num_credito IS NULL THEN      
     LET v_encabezado = " " 
     CALL v_ch_arch_salida.write([v_encabezado])
     LET v_encabezado = " CRÉDITO VIGENTE "
     CALL v_ch_arch_salida.write([v_encabezado])
     LET v_encabezado = " NÚMERO CRÉDITO ",v_num_credito_vig,"| |ESTADO CRÉDITO ",v_estado_credito ,"| |FECHA OTORGAMIENTO ",v_f_otorga USING "dd-mm-yyyy"
     CALL v_ch_arch_salida.write([v_encabezado])
     LET v_encabezado = " TIPO ORIGINACIÓN ",v_tipo_trabajador,"| |TIPO CRÉDITO ",v_tipo_credito_desc        
     CALL v_ch_arch_salida.write([v_encabezado])
     LET v_encabezado = " " 
     CALL v_ch_arch_salida.write([v_encabezado])
  END IF

  LET v_encabezado = " NSS |FECHA DISPERSIÓN |FOLIO DISPERSIÓN |DESTINO DISPERSIÓN |FOLIO PAGO |PERIODO DE PAGO |FECHA DE PAGO |FOLIO SUA |NRP |NÚMERO DE CREDITO |MONTO APORTACIÓN | MONTO AMORTIZACIÓN | AIVS |"
  CALL v_ch_arch_salida.write([v_encabezado])
  
  FOR v_recorre_arreglo = 1 TO v_arr_movimientos.getLength()      
  
      LET v_detalle =  v_arr_movimientos[v_recorre_arreglo].v_nss, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_fecha_liquida USING "dd-mm-yyyy", "|",                       
                       v_arr_movimientos[v_recorre_arreglo].v_folio_liquida, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_destino_dis, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_folio_pago, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_periodo_pago, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_fecha_pago USING "dd-mm-yyyy", "|",
                       v_arr_movimientos[v_recorre_arreglo].v_folio_sua, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_nrp, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_num_credito USING "&&&&&&&&&&", "|",
                       v_arr_movimientos[v_recorre_arreglo].v_monto_aportacion, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_monto_amortizacion, "|",
                       v_arr_movimientos[v_recorre_arreglo].v_aivs, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR
   
  --Escribe el sumario
  LET v_sumario = "TOTALES:|",v_tot_registros,"| | | | | | | | | ",
                  v_tot_aportacion, "|",
                  v_tot_amortizacion, "|",
                  v_tot_aivs
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta de Dispersion por NSS \n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: genera archivo masivo, sin parámetros de consulta
FUNCTION fn_genera_archivo_masivo(p_nss, p_id_derechohabiente) 
   DEFINE p_nss                    CHAR(11), --NSS trabajador
          p_id_derechohabiente DECIMAL(9,0)
          
  DEFINE         
    v_nom_archivo            VARCHAR(40), -- nombre del archivo de salida
    v_ruta_envio_dis         VARCHAR(40),
    v_ruta_nomarch           VARCHAR(100), -- ruta y nombre del archivo de salida
    v_ch_arch_salida         BASE.CHANNEL,
    v_indice                 INTEGER,
    v_comando_dos            STRING,
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING

  DEFINE 
    v_fecha_archivo          DATE,  
    v_hora_archivo           DATETIME HOUR TO HOUR ,
    v_min_archivo            DATETIME MINUTE TO MINUTE,
    v_sec_archivo            DATETIME SECOND TO SECOND,
    v_hora                   STRING
  
  --Fecha y hora del archivo
  LET v_fecha_archivo = TODAY 
  LET v_hora_archivo  = CURRENT HOUR TO HOUR
  LET v_min_archivo   = CURRENT MINUTE TO MINUTE
  LET v_sec_archivo   = CURRENT SECOND TO SECOND
      
  --Creación del encabezado del archivo 
  LET v_hora          = v_fecha_archivo USING "ddmmyyyy", "_",v_hora_archivo, v_min_archivo, v_sec_archivo,".dis"
  LET v_nom_archivo   = "/cons_mas_info_disp_", v_hora

  -- se obtienen la ruta envio del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = "dis"

  LET v_ruta_nomarch = v_ruta_envio_dis CLIPPED || v_nom_archivo CLIPPED 

  -- se crea el manejador de archivo y se indica que se escribirá en el mismo
  LET v_ch_arch_salida = base.Channel.create()
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA DE CORTE ",TODAY USING "dd-mm-yyyy"
  CALL v_ch_arch_salida.write([v_encabezado])
  
  LET v_encabezado = " NSS |FECHA DISPERSION |FOLIO DISPERSION |DESTINO DISPERSION |FOLIO PAGO |PERIODO DE PAGO |FECHA DE PAGO |FOLIO SUA |NRP |NÚMERO DE CREDITO |MONTO APORTACIÓN | MONTO AMORTIZACIÓN | AIVS |"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO v_arr_movimientos.getLength()
      --Concatenación del detalle
     LET v_detalle =   v_arr_movimientos[v_indice].v_nss, "|",
                       v_arr_movimientos[v_indice].v_fecha_liquida USING "dd-mm-yyyy", "|",                       
                       v_arr_movimientos[v_indice].v_folio_liquida, "|",
                       v_arr_movimientos[v_indice].v_destino_dis, "|",
                       v_arr_movimientos[v_indice].v_folio_pago, "|",
                       v_arr_movimientos[v_indice].v_periodo_pago, "|",
                       v_arr_movimientos[v_indice].v_fecha_pago USING "dd-mm-yyyy", "|",
                       v_arr_movimientos[v_indice].v_folio_sua, "|",
                       v_arr_movimientos[v_indice].v_nrp, "|",
                       v_arr_movimientos[v_indice].v_num_credito USING "&&&&&&&&&&", "|",
                       v_arr_movimientos[v_indice].v_monto_aportacion, "|",
                       v_arr_movimientos[v_indice].v_monto_amortizacion, "|",
                       v_arr_movimientos[v_indice].v_aivs, "|"
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  --Escribe el sumario
  LET v_sumario = "TOTALES:|",v_tot_registros,"| | | | | | | | |",
                  v_tot_aportacion, "|",
                  v_tot_amortizacion, "|",
                  v_tot_aivs
  CALL v_ch_arch_salida.write([v_sumario])
  --Cierra el archivo
   
  CALL v_ch_arch_salida.close()
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  CALL fn_mensaje("Información","Se ha generado el archivo de Consulta Números de Crédito igual a Cero\n en la ruta "||v_ruta_nomarch,"information")
   
END FUNCTION

#Objetivo: Estructura reporte de Numeros de Crédito igual a Cero
REPORT rep_con_disp(v_rec_movimientos, 
                      v_rep_tot_registros, 
                      v_rep_sum_aivs, 
                      v_rep_sum_aportacion,
                      v_rep_sum_amortizacion, 
                      v_usuario, p_nss, 
                      p_num_credito,
                      v_tipo_trabajador, --JANN                                           
                      v_estado_credito,
                      v_tipo_credito_desc,
                      v_num_credit_vig, 
                      v_f_otorga)

  DEFINE v_rec_movimientos  RECORD
            v_nss                CHAR(11),
            v_fecha_liquida      DATE,
            v_folio_pago         DECIMAL(9,0),
            v_folio_liquida      DECIMAL(9,0),
            v_destino_dis        VARCHAR(25),
            v_periodo_pago       CHAR(06),
            v_fecha_pago         DATE,
            v_folio_sua          DECIMAL(10,0),
            v_nrp                VARCHAR(11),
            v_num_credito        DECIMAL(10,0),
            v_monto_aportacion   DECIMAL(12,2),
            v_monto_amortizacion DECIMAL(12,2),
            v_aivs               DECIMAL(18,6)
  END RECORD
  
  DEFINE 
    v_fecha_consulta         DATE, -- Fecha de proceso
    v_usuario                VARCHAR(30), -- Almacena al usuario
    v_rep_tot_registros      DECIMAL(9,0), -- Total de registros
    v_rep_sum_aivs           DECIMAL(18,6),
    v_rep_sum_aportacion     DECIMAL(12,2),
    v_rep_sum_amortizacion   DECIMAL(12,2),
    v_tipo_trabajador        VARCHAR(45),
    v_estado_credito         VARCHAR(45),
    v_tipo_credito_desc      VARCHAR(30),
    v_num_credit_vig        DECIMAL(10,0), 
    v_f_otorga               DATE

   DEFINE p_nss              CHAR(11),
          p_num_credito      DECIMAL(10,0)
    
  FORMAT
    FIRST PAGE HEADER
      --Inicializa la fecha de consulta  
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX p_nss
      PRINTX p_num_credito
      PRINTX v_nombre_completo
      PRINTX v_num_credito_vig
      PRINTX v_estado_credito
      PRINTX v_f_otorga USING "dd-mm-yyyy"
      PRINTX v_tipo_trabajador
      PRINTX v_tipo_credito_desc                    

   PAGE HEADER
      LET v_fecha_consulta = TODAY
      
      PRINTX v_usuario
      PRINTX v_fecha_consulta USING "dd-mm-yyyy"
      PRINTX p_nss
      PRINTX v_nombre_completo      

    ON EVERY ROW
      PRINTX v_rec_movimientos.v_nss
      PRINTX v_rec_movimientos.v_fecha_liquida  USING "dd-mm-yyyy"      
      PRINTX v_rec_movimientos.v_folio_liquida  
      PRINTX v_rec_movimientos.v_destino_dis  
      PRINTX v_rec_movimientos.v_folio_pago
      PRINTX v_rec_movimientos.v_periodo_pago      
      PRINTX v_rec_movimientos.v_fecha_pago USING "dd-mm-yyyy"
      PRINTX v_rec_movimientos.v_folio_sua        
      PRINTX v_rec_movimientos.v_nrp              
      PRINTX v_rec_movimientos.v_num_credito       
      PRINTX v_rec_movimientos.v_monto_aportacion USING "###,###,###,###,##&.##"
      PRINTX v_rec_movimientos.v_monto_amortizacion USING "###,###,###,###,##&.##"
      PRINTX v_rec_movimientos.v_aivs USING "###,###,###,###,##&.##"

    ON LAST ROW
      PRINTX v_rep_tot_registros
      PRINTX v_rep_sum_aivs
      PRINTX v_rep_sum_aportacion
      PRINTX v_rep_sum_amortizacion

END REPORT