################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 22/03/2018                                      #
--------------------------------------------------------------------------------
#Proyecto          => SAFREWEB                                                 #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo           => DIS                                                       #
#Programa         => DISC37                                                    #
#Objetivo         => Programa de consulta de Dispersión Cuenta Individual      #
#Fecha de Inicio  => 06/09/2016                                                #
################################################################################
-- Base que se utilizará
DATABASE safre_viv

-- Definición de variables globales
GLOBALS
   DEFINE g_sql_txt             STRING,      --Consultas
          g_usuario             VARCHAR(30), --Almacena al usuario
          g_tipo_proceso        SMALLINT,    --Forma como ejecutara el programa
          g_nom_prog            VARCHAR(30)  --Almacena opción del menú
 
   --Datos de salida
   DEFINE g_nss                 CHAR(11)
   DEFINE g_nombre              VARCHAR(100)
            
   DEFINE a_destino_disp        DYNAMIC ARRAY OF RECORD
          folio_liquida         DECIMAL(9,0),
          folio_reg_pag         DECIMAL(9,0),
          num_cred              DECIMAL(10,0),
          origen_num_cred       VARCHAR(30),
          nrp	                CHAR(11),
          bimestre              CHAR(06),
          folio_sua             DECIMAL(6,0),
          f_pago                DATE,
          aiv_ap_pat            DECIMAL(18,6),
          imp_ap_pat            DECIMAL(12,2),
          imp_am_cre            DECIMAL(12,2),
          interface             VARCHAR(50),
          destino               VARCHAR(50)	            
          END RECORD
     
   --Totales 
   DEFINE v_tot_registros       DECIMAL(9,0),  --Total de registros
          v_tot_aivs            DECIMAL(18,6), --Total de AIVS
          v_tot_aportacion      DECIMAL(12,2)  --Total de aportaciones
         
   DEFINE v_indice              SMALLINT

   DEFINE g_id_derechohabiente  DECIMAL(9,0)
   DEFINE g_movimiento          SMALLINT
   DEFINE g_folio_liquida       DECIMAL(9,0) 
   DEFINE g_id_referencia       DECIMAL(9,0)
   DEFINE v_hist                SMALLINT

   DEFINE v_proc_entra          SMALLINT,
          v_proc_val            SMALLINT,
          v_cod_conv            SMALLINT,
          v_desc_proc_val       CHAR(40),
          v_mensaje_val         STRING,
          p_proceso_cod         SMALLINT
 
END GLOBALS

MAIN     
  DEFINE v_nss                  CHAR(11)
  DEFINE v_nombre               VARCHAR(100)

  DEFINE bnd_consulta           SMALLINT, 
         f_ventana              ui.Window, --Define las propiedades de la Ventana
         f_forma                ui.Form    --Define las propiedades de la forma

  --Recibe valores de argumentos
  LET g_usuario             = ARG_VAL(1)
  LET g_tipo_proceso        = ARG_VAL(2)
  LET g_nom_prog            = ARG_VAL(3)
  LET g_id_derechohabiente  = ARG_VAL(4)
  LET g_movimiento          = ARG_VAL(5)
  LET g_folio_liquida       = ARG_VAL(6) 
  LET g_id_referencia       = ARG_VAL(7)

  LET p_proceso_cod         = 901

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
  LET v_nss           = 0
  LET v_hist          = 0
  INITIALIZE v_nombre TO NULL

  DISPLAY "g_id_derechohabiente : ", g_id_derechohabiente
  DISPLAY "g_movimiento         : ", g_movimiento
  DISPLAY "g_folio_liquida      : ", g_folio_liquida
  DISPLAY "g_id_referencia      : ", g_id_referencia

  --IF g_id_derechohabiente IS NOT NULL AND 
  --   g_movimiento         IS NOT NULL AND  
  --   g_folio_liquida      IS NOT NULL THEN
     CALL fn_obtener_nss_nombre()

     DISPLAY "fn_consultar(): ", NULL
     CALL fn_consultar(NULL)

     LET v_nss      = g_nss
     LET v_nombre   = g_nombre
  --END IF

  CLOSE WINDOW SCREEN
  OPEN WINDOW w1 WITH FORM "DISC37"
   
    DIALOG ATTRIBUTES(UNBUFFERED) --ATTRIBUTES (ACCEPT=FALSE, CANCEL=FALSE) 
   
      {INPUT BY NAME v_nss, v_nombre  ATTRIBUTE(WITHOUT DEFAULTS)
      
         BEFORE INPUT

            IF v_nss IS NOT NULL THEN
               CALL DIALOG.setActionHidden("aceptar",1)
               NEXT FIELD t_folio_dis
            ELSE
            
            END IF
            
            NEXT FIELD v_nss
            CALL ui.interface.refresh()

         ON ACTION aceptar
            IF v_nss IS NOT NULL THEN
               DISPLAY "fn_consultar(): ", v_nss
               CALL fn_consultar(v_nss)
            END IF
            
      END INPUT}
      
      DISPLAY ARRAY a_destino_disp TO rec_detalle.*
        BEFORE DISPLAY      
          DISPLAY BY NAME v_nss 
          DISPLAY BY NAME v_nombre
   
        AFTER DISPLAY
 
          --CALL DIALOG.setActionHidden("reporte",0)                                
          CALL ui.interface.refresh()

         {ON ACTION reporte
             CALL fn_reporte()
          
          ON ACTION archivo
             --CALL fn_genera_archivo_transacciones_nss(g_transaccion, g_nss)}

          --ON ACTION aceptar 
          --    EXIT PROGRAM 
      END DISPLAY 
            
      ON ACTION cancelar
         EXIT DIALOG     

      BEFORE DIALOG
         DISPLAY "BEFORE DIALOG"

         LET f_ventana = ui.Window.getCurrent()
         LET f_forma   = f_ventana.getForm()

         DISPLAY "v_indice: ", v_indice
         IF v_indice > 0 THEN
            CALL fn_valida_historico(g_folio_liquida)
            RETURNING v_hist
            IF v_hist = 1 THEN
               CALL a_destino_disp.clear()
               CALL f_forma.setElementHidden("gr_detalle", 1)
               CALL fn_mensaje("ATENCIÓN",
                               "No se encontraron registros.",
                               "about")
               CALL ui.interface.refresh()
               EXIT PROGRAM
            ELSE
               CALL f_forma.setElementHidden("gr_detalle", 0)          --Muestra detalle de la consulta
            END IF
         ELSE
            CALL f_forma.setElementHidden("gr_detalle", 1)
            CALL fn_mensaje("ATENCIÓN",
                            "No se encontraron registros.",
                            "about")
            CALL ui.interface.refresh()
            EXIT PROGRAM
         END IF

    END DIALOG 
  CLOSE WINDOW w1 

END MAIN 

FUNCTION fn_consultar(p_nss)
DEFINE p_nss               CHAR(11)
DEFINE v_cve_destino       SMALLINT 
DEFINE v_desc_destino      VARCHAR(40)
DEFINE v_cve_trans         SMALLINT
DEFINE v_tabla             VARCHAR(40)
DEFINE v_periodo_pago      CHAR(06)
DEFINE v_imp_ap_pat        DECIMAL(12,2)
DEFINE v_f_actualiza       DATE
DEFINE v_precio_fondo      DECIMAL(19,14)

  LET v_cve_destino          = 0 
  INITIALIZE v_desc_destino  TO NULL
  LET v_cve_trans            = 0
  INITIALIZE g_nss, g_nombre TO NULL

  CALL a_destino_disp.clear()

  IF p_nss IS NULL THEN

  ELSE

  END IF

  DISPLAY "fn_obtener_destino_disp()"
  CALL fn_obtener_destino_disp() RETURNING v_cve_destino, v_desc_destino, v_tabla, v_cve_trans

  DISPLAY "v_cve_destino  : ",v_cve_destino
  DISPLAY "v_desc_destino : ",v_desc_destino
  DISPLAY "v_tabla        : ",v_tabla
  DISPLAY "v_cve_trans    : ",v_cve_trans

  --CARTERA
  IF v_cve_destino = 1 THEN
     DISPLAY "\n CARTERA \n"

     LET g_sql_txt = " \n SELECT hs.folio_liquida,   ",
                     " \n        hs.num_crd_ifv,     ",
                     " \n        hs.nrp,             ",
                     " \n        hs.periodo_pago,    ",
                     " \n        hs.folio_sua,       ",
                     " \n        hs.f_pago,          ",
                     " \n        hs.aiv_ap_pat,      ",
                     " \n        hs.imp_ap_pat,      ",
                     " \n        hs.imp_am_cre       ",
                     " \n FROM   dis_interface_hs hs ",
                     " \n WHERE  hs.folio_liquida       = ", g_folio_liquida,
                     " \n AND    hs.id_dis_interface_hs = ", g_id_referencia,
                     " \n AND    hs.id_derechohabiente  = ", g_id_derechohabiente

     DISPLAY g_sql_txt 
     PREPARE ps_destino_hs FROM g_sql_txt
     DECLARE cur_destino_hs CURSOR FOR ps_destino_hs

     LET v_indice = 1
     FOREACH cur_destino_hs INTO a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 a_destino_disp[v_indice].aiv_ap_pat,
                                 a_destino_disp[v_indice].imp_ap_pat,
                                 a_destino_disp[v_indice].imp_am_cre

       LET a_destino_disp[v_indice].destino =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED

       SELECT UNIQUE(glo.folio_referencia)
       INTO   a_destino_disp[v_indice].folio_reg_pag
       FROM   glo_folio glo, 
              dis_interface_hs hs
       WHERE  hs.folio_liquida = glo.folio 
       AND    glo.proceso_cod  = p_proceso_cod
       AND    hs.folio_liquida = g_folio_liquida

       SELECT UNIQUE arc.nombre_archivo
       INTO   a_destino_disp[v_indice].interface
       FROM   dis_ctr_archivo arc,
              dis_interface_hs hs
       WHERE  hs.folio_liquida = arc.folio_liquida 
       AND    arc.cve_destino  = v_cve_destino
       AND    hs.folio_liquida = g_folio_liquida

       CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                 a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 v_cve_destino)
       RETURNING a_destino_disp[v_indice].origen_num_cred,
                 a_destino_disp[v_indice].folio_sua,
                 a_destino_disp[v_indice].nrp
 
       LET v_indice = v_indice + 1
     END FOREACH
  END IF
  --CARTERA

  --AVANCE DE PAGOS
  IF v_cve_destino = 8 THEN
      
     DISPLAY "\n AVANCE DE PAGOS \n"
     LET g_sql_txt = " \n SELECT ap.folio,              ",
                     " \n        ap.num_credito,        ",
                     " \n        ap.nrp,                ",
                     " \n        ap.periodo_pago,       ",
                     " \n        ap.f_pago,             ",
                     " \n        ap.monto_aportacion,   ",
                     " \n        ap.monto_amortizacion  ",
                     " \n FROM   dis_det_avance_pago ap ",
                     " \n WHERE  ap.id_dis_det_avance_pago =  ", g_id_referencia,
                     " \n AND    ap.folio                  =  ", g_folio_liquida,
                     " \n AND    ap.id_derechohabiente     =  ", g_id_derechohabiente

     DISPLAY g_sql_txt
     PREPARE ps_destino_ap FROM g_sql_txt
     DECLARE cur_destino_ap CURSOR FOR ps_destino_ap
    
     LET v_indice = 1
     FOREACH cur_destino_ap INTO a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].f_pago,
                                 a_destino_disp[v_indice].imp_ap_pat,
                                 a_destino_disp[v_indice].imp_am_cre

       LET a_destino_disp[v_indice].folio_reg_pag = 0;
       LET a_destino_disp[v_indice].aiv_ap_pat    = 0;
       LET a_destino_disp[v_indice].destino       =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED

       SELECT UNIQUE arc.nombre_archivo
       INTO   a_destino_disp[v_indice].interface
       FROM   dis_ctr_archivo arc,
              dis_det_avance_pago ap
       WHERE  ap.folio        = arc.folio_liquida 
       AND    arc.cve_destino = v_cve_destino
       AND    ap.folio        = g_folio_liquida

       CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                 a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 v_cve_destino)
       RETURNING a_destino_disp[v_indice].origen_num_cred,
                 a_destino_disp[v_indice].folio_sua,
                 a_destino_disp[v_indice].nrp

       LET a_destino_disp[v_indice].origen_num_cred = " "
       LET v_indice                                 = v_indice + 1
     END FOREACH  
  END IF         
  --AVANCE DE PAGOS
  
  --RECUPERACIÓN AVANCE DE PAGOS
  IF (v_cve_destino = 2   OR 
      v_cve_destino = 9   OR 
      v_cve_destino = 10  OR 
      v_cve_destino = 11  OR 
      v_cve_destino = 12) THEN
      
      DISPLAY "\n RECUPERACIÓN AVANCE DE PAGOS \n"
      LET g_sql_txt = " \n SELECT ava.folio_dis,            ",
                      " \n        ava.folio_pago,           ",
                      " \n        ava.num_credito,          ",
                      " \n        ava.nrp,                  ",
                      " \n        ava.periodo_pago,         ",
                      " \n        ava.f_pago,               ",
                      " \n        ava.monto_apo_pag,        ",
                      " \n        ava.monto_amo_pag         ",
                      " \n FROM   dis_compensa_avance ava   ",
                      " \n WHERE  ava.id_referencia      =  ", g_id_referencia,
                      " \n AND    ava.folio_dis          =  ", g_folio_liquida,
                      " \n AND    ava.id_derechohabiente =  ", g_id_derechohabiente

      DISPLAY g_sql_txt
      PREPARE ps_destino_ava FROM g_sql_txt
      DECLARE cur_destino_ava CURSOR FOR ps_destino_ava
    
      LET v_indice = 1
      FOREACH cur_destino_ava INTO a_destino_disp[v_indice].folio_liquida,
                                   a_destino_disp[v_indice].folio_reg_pag,
                                   a_destino_disp[v_indice].num_cred,
                                   a_destino_disp[v_indice].nrp,
                                   a_destino_disp[v_indice].bimestre,
                                   a_destino_disp[v_indice].f_pago,
                                   a_destino_disp[v_indice].imp_ap_pat,
                                   a_destino_disp[v_indice].imp_am_cre

        LET a_destino_disp[v_indice].aiv_ap_pat = 0; 
        LET a_destino_disp[v_indice].destino    =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED

        SELECT UNIQUE(glo.folio_referencia)
        INTO   a_destino_disp[v_indice].folio_reg_pag
        FROM   glo_folio glo, 
               dis_interface_hs hs
        WHERE  hs.folio_liquida = glo.folio 
        AND    glo.proceso_cod  = p_proceso_cod
        AND    hs.folio_liquida = g_folio_liquida

        SELECT UNIQUE arc.nombre_archivo
        INTO   a_destino_disp[v_indice].interface
        FROM   dis_ctr_archivo arc,
               dis_compensa_avance ava
        WHERE  ava.folio_dis   = arc.folio_liquida 
        AND    arc.cve_destino = v_cve_destino
        AND    ava.folio_dis   = g_folio_liquida

        CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                  a_destino_disp[v_indice].folio_liquida,
                                  a_destino_disp[v_indice].num_cred,
                                  a_destino_disp[v_indice].nrp,
                                  a_destino_disp[v_indice].bimestre,
                                  a_destino_disp[v_indice].folio_sua,
                                  a_destino_disp[v_indice].f_pago,
                                  v_cve_destino)
        RETURNING a_destino_disp[v_indice].origen_num_cred,
                  a_destino_disp[v_indice].folio_sua,
                  a_destino_disp[v_indice].nrp

        LET v_indice = v_indice + 1
      END FOREACH  
  END IF         
  --RECUPERACIÓN AVANCE DE PAGOS

  --ENTIDAD FINANCIERA
  IF v_cve_destino = 3 THEN
     DISPLAY "\n ENTIDAD FINANCIERA \n"
     LET g_sql_txt = " \n SELECT ef.folio_liquida,   ",
                     " \n        ef.num_crd_ifv,     ",
                     " \n        ef.nrp,             ",
                     " \n        ef.periodo_pago,    ",
                     " \n        ef.folio_sua,       ",
                     " \n        ef.f_pago,          ",
                     " \n        ef.aiv_ap_pat,      ",
                     " \n        ef.imp_ap_pat       ",
                     " \n FROM   dis_interface_ef ef ",
                     " \n WHERE  ef.folio_liquida       = ", g_folio_liquida,
                     " \n AND    ef.id_dis_interface_ef = ", g_id_referencia,
                     " \n AND    ef.id_derechohabiente  = ", g_id_derechohabiente

     DISPLAY g_sql_txt
     PREPARE ps_destino_ef FROM g_sql_txt
     DECLARE cur_destino_ef CURSOR FOR ps_destino_ef

     LET v_indice = 1
     FOREACH cur_destino_ef INTO a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 a_destino_disp[v_indice].aiv_ap_pat,
                                 a_destino_disp[v_indice].imp_ap_pat

       LET a_destino_disp[v_indice].destino    =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED
       LET a_destino_disp[v_indice].imp_am_cre = 0

       SELECT UNIQUE(glo.folio_referencia)
       INTO   a_destino_disp[v_indice].folio_reg_pag
       FROM   glo_folio glo, 
              dis_interface_hs hs
       WHERE  hs.folio_liquida = glo.folio 
       AND    glo.proceso_cod  = p_proceso_cod
       AND    hs.folio_liquida = g_folio_liquida
            
       SELECT UNIQUE arc.nombre_archivo
       INTO   a_destino_disp[v_indice].interface
       FROM   dis_ctr_archivo arc,
              dis_interface_ef ef
       WHERE  ef.folio_liquida = arc.folio_liquida 
       AND    arc.cve_destino  = v_cve_destino
       AND    ef.folio_liquida = g_folio_liquida
            
       CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                 a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 v_cve_destino)
       RETURNING a_destino_disp[v_indice].origen_num_cred,
                 a_destino_disp[v_indice].folio_sua,
                 a_destino_disp[v_indice].nrp

       LET v_indice = v_indice + 1
     END FOREACH
  END IF         
  --ENTIDAD FINANCIERA

  --DEVOLUCIÓN DE PAGOS EN EXCESO
  IF (v_cve_destino = 4  OR 
      v_cve_destino = 6) THEN
      DISPLAY "\n DEVOLUCIÓN DE PAGOS EN EXCESO \n"
      LET g_sql_txt = " \n SELECT dev.folio_referencia,     ",
                      " \n        dev.num_credito,          ",
                      " \n        dev.periodo_pago,         ",
                      " \n        dev.f_pago,               ",
                      " \n        dev.aivs_aportacion,      ",
                      " \n        dev.monto_aportacion      ",
                      " \n FROM   dse_devolucion dev        ",
                      " \n WHERE  dev.id_dse_devolucion  =  ", g_id_referencia,
                      " \n AND    dev.folio_referencia   =  ", g_folio_liquida,
                      " \n AND    dev.id_derechohabiente =  ", g_id_derechohabiente

      DISPLAY g_sql_txt
      PREPARE ps_destino_dev FROM g_sql_txt
      DECLARE cur_destino_dev CURSOR FOR ps_destino_dev

      LET v_indice = 1
      FOREACH cur_destino_dev INTO a_destino_disp[v_indice].folio_liquida,
                                   a_destino_disp[v_indice].num_cred,
                                   a_destino_disp[v_indice].bimestre,
                                   a_destino_disp[v_indice].f_pago,
                                   a_destino_disp[v_indice].aiv_ap_pat,
                                   a_destino_disp[v_indice].imp_ap_pat
                                     
        LET a_destino_disp[v_indice].destino    =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED
        LET a_destino_disp[v_indice].imp_am_cre = 0

        SELECT UNIQUE(glo.folio_referencia)
        INTO   a_destino_disp[v_indice].folio_reg_pag
        FROM   glo_folio glo, 
               dis_interface_hs hs
        WHERE  hs.folio_liquida = glo.folio 
        AND    glo.proceso_cod  = p_proceso_cod
        AND    hs.folio_liquida = g_folio_liquida

        SELECT UNIQUE arc.nombre_archivo
        INTO   a_destino_disp[v_indice].interface
        FROM   dis_ctr_archivo arc,
               dse_devolucion dev
        WHERE  dev.folio_referencia = arc.folio_liquida 
        AND    arc.cve_destino      = v_cve_destino
        AND    dev.folio_referencia = g_folio_liquida

        CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                  a_destino_disp[v_indice].folio_liquida,
                                  a_destino_disp[v_indice].num_cred,
                                  a_destino_disp[v_indice].nrp,
                                  a_destino_disp[v_indice].bimestre,
                                  a_destino_disp[v_indice].folio_sua,
                                  a_destino_disp[v_indice].f_pago,
                                  v_cve_destino)
        RETURNING a_destino_disp[v_indice].origen_num_cred,
                  a_destino_disp[v_indice].folio_sua,
                  a_destino_disp[v_indice].nrp

        LET v_indice = v_indice + 1
      END FOREACH
  END IF         
  --DEVOLUCIÓN DE PAGOS EN EXCESO

  --CUENTA INDIVIDUAL
  IF v_cve_destino = 5 THEN
     DISPLAY "\n CUENTA INDIVIDUAL \n"
     LET g_sql_txt = " \n SELECT pau.folio_liquida         ",
                     " \n FROM   dis_cta_ind_pau pau       ",
                     " \n WHERE  pau.id_dis_cta_ind_pau  = ", g_id_referencia,
                     " \n AND    pau.folio_liquida       = ", g_folio_liquida,
                     " \n AND    pau.id_derechohabiente  = ", g_id_derechohabiente

     DISPLAY g_sql_txt 
     PREPARE ps_destino_pau FROM g_sql_txt
     DECLARE cur_destino_pau CURSOR FOR ps_destino_pau

     LET v_indice = 1
     FOREACH cur_destino_pau INTO a_destino_disp[v_indice].folio_liquida
       LET a_destino_disp[v_indice].destino =  v_cve_destino CLIPPED," - ", v_desc_destino CLIPPED

       INITIALIZE v_f_actualiza TO NULL
           
       SELECT UNIQUE(glo.folio_referencia), glo.f_actualiza
       INTO   a_destino_disp[v_indice].folio_reg_pag, v_f_actualiza
       FROM   glo_folio glo, 
              dis_cta_ind_pau pau
       WHERE  pau.folio_liquida = glo.folio 
       AND    glo.proceso_cod   = p_proceso_cod
       AND    pau.folio_liquida = g_folio_liquida

       INITIALIZE v_periodo_pago TO NULL
       LET v_imp_ap_pat   = 0
       LET v_precio_fondo = 0
                        
       SELECT pag.num_crd_ifv,
              pag.nrp,
              fn_bimestre_pago(pag.periodo_pago),
              pag.folio_sua,
              pag.f_pago,
              pag.aiv_ap_pat,
              pag.imp_ap_pat,
              pag.imp_am_cre
       INTO   a_destino_disp[v_indice].num_cred,
              a_destino_disp[v_indice].nrp,
              a_destino_disp[v_indice].bimestre,
              a_destino_disp[v_indice].folio_sua,
              a_destino_disp[v_indice].f_pago,
              a_destino_disp[v_indice].aiv_ap_pat,
              v_imp_ap_pat,
              a_destino_disp[v_indice].imp_am_cre
       FROM   cta_his_pagos pag,
              dis_cta_ind_pau pau
       WHERE  pau.folio_pago    = pag.fv_imp_ap_patolio 
       AND    pau.id_referencia = pag.id_referencia
       AND    pau.folio_liquida = g_folio_liquida

       SELECT valor.precio_fondo
       INTO   v_precio_fondo
       FROM   glo_valor_fondo valor
       WHERE  valor.fondo       = 11 
       AND    valor.f_valuacion = v_f_actualiza

       LET a_destino_disp[v_indice].imp_ap_pat = v_imp_ap_pat * v_precio_fondo

       SELECT UNIQUE arc.nombre_archivo
       INTO   a_destino_disp[v_indice].interface
       FROM   dis_ctr_archivo arc,
              dis_cta_ind_pau pau
       WHERE  pau.folio_liquida = arc.folio_liquida 
       AND    arc.cve_destino   = v_cve_destino
       AND    pau.folio_liquida = g_folio_liquida

       CALL fn_obtiene_origen_nc(g_id_derechohabiente,
                                 a_destino_disp[v_indice].folio_liquida,
                                 a_destino_disp[v_indice].num_cred,
                                 a_destino_disp[v_indice].nrp,
                                 a_destino_disp[v_indice].bimestre,
                                 a_destino_disp[v_indice].folio_sua,
                                 a_destino_disp[v_indice].f_pago,
                                 v_cve_destino)
       RETURNING a_destino_disp[v_indice].origen_num_cred,
                 a_destino_disp[v_indice].folio_sua,
                 a_destino_disp[v_indice].nrp
 
       LET v_indice = v_indice + 1
     END FOREACH

  END IF
  --CUENTA INDIVIDUAL
  
  CALL a_destino_disp.deleteElement(v_indice)
  LET v_indice        = v_indice - 1 
  LET v_tot_registros = v_indice 

  IF v_tot_registros >= 1 THEN
     DISPLAY "fn_obtener_nss_nombre()"
     CALL fn_obtener_nss_nombre()
  END IF
  
END FUNCTION  

FUNCTION fn_obtener_destino_disp()
  DEFINE v_cve_destino    SMALLINT
  DEFINE v_cve_trans      SMALLINT
  DEFINE v_desc_destino   VARCHAR(40)
  DEFINE v_tabla          VARCHAR(40)

  LET v_cve_destino = 0
  LET v_cve_trans   = 0

  SELECT agp.cve_destino, 
         dest.desc_destino, 
         dest.estructura_datos,
         agp.cve_transaccion
  INTO   v_cve_destino, 
         v_desc_destino, 
         v_tabla,
         v_cve_trans
  FROM   dis_agp_movimiento agp,
         cat_dis_destino dest
  WHERE  agp.cve_destino  = dest.cve_destino
  AND    agp.movimiento   = g_movimiento

  RETURN v_cve_destino, v_desc_destino, v_tabla, v_cve_trans
END FUNCTION

FUNCTION fn_obtiene_origen_nc (p_id_derechohabiente,
                               p_folio_liquida,
                               p_num_cred,
                               p_nrp,
                               p_bimestre,
                               p_folio_sua,
                               p_f_pago,
                               p_cve_destino)
  DEFINE p_id_derechohabiente   DECIMAL(9,0)
  DEFINE p_folio_liquida        DECIMAL(9,0)
  DEFINE p_num_cred             DECIMAL(10,0)
  DEFINE p_nrp	                CHAR(11)
  DEFINE p_bimestre             CHAR(06)
  DEFINE p_folio_sua	        DECIMAL(6,0)
  DEFINE p_f_pago               DATE
  DEFINE v_cve_origen_nc        SMALLINT
  DEFINE v_origen_num_cred      VARCHAR(30)
  DEFINE p_cve_destino          SMALLINT

  LET v_cve_origen_nc = 0

  IF (p_cve_destino = 1  OR
      p_cve_destino = 3  OR 
      p_cve_destino = 5) THEN
      SELECT a.origen_num_credito
      INTO   v_cve_origen_nc
      FROM   dis_his_transaccion a
      WHERE  a.id_derechohabiente = p_id_derechohabiente
      AND    a.folio_liquida      = p_folio_liquida
      AND    a.nrp                = p_nrp
      AND    a.periodo_pago       = p_bimestre
      AND    a.folio_sua          = p_folio_sua
      AND    a.f_pago             = p_f_pago
  END IF

  IF (p_cve_destino = 2   OR
      p_cve_destino = 8   OR
      p_cve_destino = 9   OR
      p_cve_destino = 10  OR
      p_cve_destino = 11  OR
      p_cve_destino = 12) THEN
      SELECT a.origen_num_credito, a.folio_sua
      INTO   v_cve_origen_nc, p_folio_sua
      FROM   dis_his_transaccion a
      WHERE  a.id_derechohabiente = p_id_derechohabiente
      AND    a.folio_liquida      = p_folio_liquida
      AND    a.nrp                = p_nrp
      AND    a.periodo_pago       = p_bimestre
      AND    a.f_pago             = p_f_pago
  END IF

  IF (p_cve_destino = 4  OR
      p_cve_destino = 6) THEN
      SELECT a.origen_num_credito, a.folio_sua, a.nrp 
      INTO   v_cve_origen_nc, p_folio_sua, p_nrp
      FROM   dis_his_transaccion a
      WHERE  a.id_derechohabiente = p_id_derechohabiente
      AND    a.folio_liquida      = p_folio_liquida
      AND    a.periodo_pago       = p_bimestre
      AND    a.f_pago             = p_f_pago
  END IF

  IF (p_cve_destino = 2   OR
      --p_cve_destino = 8   OR
      p_cve_destino = 9   OR
      p_cve_destino = 10  OR
      p_cve_destino = 11  OR
      p_cve_destino = 12) THEN
      LET v_cve_origen_nc = 3
  END IF

  CASE v_cve_origen_nc
    WHEN 0 LET v_origen_num_cred = v_cve_origen_nc || " - SIN ORIGEN"
    WHEN 1 LET v_origen_num_cred = v_cve_origen_nc || " - SACI"
    WHEN 2 LET v_origen_num_cred = v_cve_origen_nc || " - LQ"
    WHEN 3 LET v_origen_num_cred = v_cve_origen_nc || " - AVANCE"
    OTHERWISE LET v_origen_num_cred = v_cve_origen_nc || " - SIN ORIGEN"
  END CASE

  IF (p_cve_destino = 8) THEN
      LET v_origen_num_cred = " "
  END IF

  RETURN v_origen_num_cred, p_folio_sua, p_nrp
END FUNCTION

FUNCTION fn_obtener_nss_nombre()

  SELECT afi.nss, 
         TRIM(afi.nombre_af)||' '|| 
         TRIM(afi.ap_paterno_af)||' '|| 
         TRIM(afi.ap_materno_af) 
  INTO   g_nss, g_nombre
  FROM   afi_derechohabiente afi
  WHERE  id_derechohabiente = g_id_derechohabiente

  DISPLAY "id dererchohabiente: ", g_id_derechohabiente
  DISPLAY "nss                : ", g_nss
  DISPLAY "nombre             : ", g_nombre

END FUNCTION

FUNCTION fn_valida_historico(f_folio_liquida)
  DEFINE f_folio_liquida        DECIMAL(9,0)
  DEFINE f_hist                 SMALLINT
  DEFINE f_f_liquida            DATE

  LET f_hist = 0

  SELECT a.f_actualiza
  INTO   f_f_liquida
  FROM   glo_folio a
  WHERE  a.folio = f_folio_liquida
  IF f_f_liquida <= '03122018' THEN
     LET f_hist = 1
  END IF
  
  RETURN f_hist
END FUNCTION