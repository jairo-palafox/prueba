################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 21/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISC04                                                   #
#Objetivo          => CONSULTA Consulta Histórico Dispersión                   #
#Fecha Inicio      => 27/01/2012                                               #
################################################################################
#Registro de modificaciones:
#Autor           Fecha         Descrip. cambio
#Eneas Armas     18/12/2013    Se agrega validación, que no se tenga la
#                              Preliquidación de Dispersión de Pagos ejecutándose
#Eneas Armas     20140123      Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa

DATABASE
  safre_viv

GLOBALS
  CONSTANT Por_Folio = 0
  CONSTANT Por_Fecha = 1
  CONSTANT Sin       = 0
  DEFINE v_tbl_mov           VARCHAR(50),
         g_sql_txt           STRING,
         v_proc_entra        SMALLINT,
         v_proc_val          SMALLINT,
         v_cod_conv          SMALLINT,
         v_desc_proc_val     CHAR(40),
         v_mensaje_val       STRING,
         p_proceso_cod       SMALLINT 
END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod, -- Clave del usuario
    p_tipo_ejecucion         SMALLINT, -- Forma como ejecutara el programa
    p_s_titulo               STRING, -- Titulo de la ventana
    v_folio_liquida          LIKE cta_movimiento.folio_liquida, -- Folio liquidado
    v_fecha_liquida          LIKE cta_movimiento.f_liquida,
    r_existe_registro        INTEGER --SMALLINT,

  DEFINE r_arr_ctamovto      DYNAMIC ARRAY OF RECORD
    v_subcuenta              CHAR(55),--LIKE cta_movimiento.subcuenta,
    v_fondo_inversion        CHAR(55),--LIKE cta_movimiento.fondo_inversion,
    v_monto_pesos            LIKE cta_movimiento.monto_pesos,
    v_monto_acciones         LIKE cta_movimiento.monto_acciones,
    v_total_cuentas          LIKE cta_movimiento.monto_pesos 
  END RECORD

  DEFINE     
    r_tot_pesos              LIKE cta_movimiento.monto_pesos,
    r_tot_acciones           LIKE cta_movimiento.monto_acciones,
    r_tot_ctas               LIKE cta_movimiento.monto_pesos,
    v_indice_mdt             INTEGER,
    r_ind_inf                INTEGER,
    v_tot_monto_pesos_mdt    LIKE cta_movimiento.monto_pesos,
    v_tot_total_cuentas_mdt  LIKE cta_movimiento.monto_pesos
          
  DEFINE r_arr_mandatos      DYNAMIC ARRAY OF RECORD
    v_servicios              CHAR(55),
    v_monto_pesos            LIKE cta_movimiento.monto_pesos,
    v_total_cuentas          LIKE cta_movimiento.monto_pesos
  END RECORD 
       
  DEFINE 
    f_ventana                ui.Window,   -- Define las propìedades de la Ventana
    f_forma                  ui.Form     -- Define las propiedades de la forma

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET p_proceso_cod    = 901

  -- si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF
  --LET v_fecha_liquida = ""

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

  PREPARE fn_tbl_mov FROM "execute function fn_tab_movimiento(?,?,?)"
  
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_hist_disp WITH FORM "DISC041"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_liquida,v_fecha_liquida --WITHOUT DEFAULTS
       FROM f_folio,f_fecha -- ATTRIBUTES(UNBUFFERED)

        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_detalles_sbctas", 1) --Oculta el detalle de subcuentas 
          CALL f_forma.setElementHidden("gr_detalles_mdtos", 1) --Oculta el detalle de mandatos
          CALL DIALOG.setActionHidden("reporte", 1) --Oculta el botón reporte

        ON ACTION cancelar
           EXIT DIALOG 

        ON ACTION ACCEPT
           CALL fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
           RETURNING r_existe_registro    

           IF r_existe_registro > 0 THEN
              CALL f_forma.setElementHidden("gr_detalles_sbctas", 0) --Muestra el detalle de subcuentas            
              CALL f_forma.setElementHidden("gr_detalles_mdtos", 0) --Muestra el detalle de mandatos
              CALL DIALOG.setActionHidden("reporte", 0) --Muestra el botón reporte 
              CALL DIALOG.setActionHidden("accept", 1) --Muestra el botón reporte 

              --Obtiene informacion de subcuentas               
              CALL fn_obtiene_informacion(v_folio_liquida,v_fecha_liquida)
              RETURNING r_arr_ctamovto, r_ind_inf,
                        r_tot_pesos,r_tot_acciones,r_tot_ctas
                         
              --Obtiene informacion de mandatos 
              CALL fn_obtiene_inf_mandatos(v_folio_liquida, v_fecha_liquida)
              RETURNING r_arr_mandatos, v_indice_mdt, 
                        v_tot_monto_pesos_mdt, v_tot_total_cuentas_mdt
                         
              DISPLAY ARRAY r_arr_ctamovto TO scr_det_scta.* ATTRIBUTES (CANCEL = FALSE )
                BEFORE DISPLAY 
                  --Muestra totales de subcuentas
                  DISPLAY r_tot_pesos TO sum_pesos
                  DISPLAY r_tot_acciones TO sum_acciones
                  DISPLAY r_tot_ctas TO sum_cuentas

                  DISPLAY "INT_FLAG1 ",INT_FLAG
                  --Evalúa variable int_flag
                  IF NOT INT_FLAG THEN 
                     ACCEPT DISPLAY  
                  END IF 
              END DISPLAY

              DISPLAY ARRAY r_arr_mandatos TO scr_det_mandato.* ATTRIBUTES (CANCEL = FALSE )
                BEFORE DISPLAY 
                  --Muestra totales de mandatos
                  DISPLAY v_tot_monto_pesos_mdt TO sum_pesos_mdt
                  DISPLAY v_tot_total_cuentas_mdt TO sum_cuentas_mdt

                  DISPLAY "INT_FLAG2 ",INT_FLAG
                  --Evalúa variable int_flag
                  IF NOT INT_FLAG THEN 
                     ACCEPT DISPLAY  
                  END IF
              END DISPLAY
           END IF 

        ON ACTION reporte
           CALL fn_reporte_historico_dispersion(v_folio_liquida,v_fecha_liquida,
                                                p_usuario_cod)
      END INPUT 
    END DIALOG  
  CLOSE WINDOW vtn_hist_disp 
END MAIN

#Objetivo: Consulta para validar que exista el folio y/o fecha capturadas en la
#          la tabla de movimientos del proceso de liquidación
FUNCTION fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
  DEFINE
    v_folio_liquida          LIKE cta_movimiento.folio_liquida, --Folio liquidado
    v_fecha_liquida          LIKE cta_movimiento.f_liquida,  --Fecha liquidación
    v_existe_liquida_movto   INTEGER, -- Contador si existen registros
    v_existe_liquida_movto1  INTEGER, -- Contador si existen registros
    v_QryTxt                 STRING

  --Validar en caso de que se capturara el folio y la fecha
  IF length(v_folio_liquida) > 0 AND length(v_fecha_liquida) > 0 THEN
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio

     LET v_QryTxt =  "\n SELECT COUNT (*)        "
                    ,"\n FROM   ",v_tbl_mov
                    ,"\n WHERE  folio_liquida = ?"
                    ,"\n AND    f_liquida     = ?"
     PREPARE prp_valida_x_ffcm FROM v_QryTxt
     EXECUTE prp_valida_x_ffcm USING v_folio_liquida,v_fecha_liquida INTO v_existe_liquida_movto
       
     IF v_existe_liquida_movto > 0 THEN
        LET v_QryTxt = "\n SELECT COUNT (*)",
                       "\n FROM   glo_folio",
                       "\n WHERE  status      = 2",
                       "\n AND    proceso_cod = 901",
                       "\n AND    folio       = ", v_folio_liquida,
                       "\n AND    f_actualiza = '",v_fecha_liquida,"'"
        PREPARE prp_valida_x_ff FROM v_QryTxt
        EXECUTE prp_valida_x_ff INTO v_existe_liquida_movto1
     ELSE
        CALL fn_mensaje("Atención",
                        "No existe registro con los parametros capturados",
                        "about")
     END IF
  END IF

  --Valida en caso de captura del folio
  IF length(v_folio_liquida) > 0 AND length(v_fecha_liquida) = 0 THEN
     EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio

     LET v_QryTxt =  "\n SELECT COUNT (*)        "
                    ,"\n FROM   ",v_tbl_mov
                    ,"\n WHERE  folio_liquida = ?"
     PREPARE prp_valida_x_ffcm2 FROM v_QryTxt
     EXECUTE prp_valida_x_ffcm2 USING v_folio_liquida INTO v_existe_liquida_movto

     --Valida que el folio exista en glo_ctr_archivo 
     IF v_existe_liquida_movto > 0 THEN
        LET v_QryTxt = "\n SELECT COUNT (*)",
                       "\n FROM   glo_folio",
                       "\n WHERE  status      = 2",
                       "\n AND    proceso_cod = 901",
                       "\n AND    folio       = ", v_folio_liquida
        PREPARE prp_valida_x_folio FROM v_QryTxt
        EXECUTE prp_valida_x_folio INTO v_existe_liquida_movto1
          
        IF v_existe_liquida_movto1 = 0 THEN
           CALL fn_mensaje("Atención","Folio no liquidado","about")
        END IF
     ELSE
        CALL fn_mensaje("Atención","Folio no existe","about")
     END IF
  END IF

  --Valida en caso de captura de la fecha
  IF length(v_fecha_liquida) > 0 AND length(v_folio_liquida) = 0 THEN
     EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --por fecha de liquidación
     
     LET v_QryTxt = "\n  SELECT COUNT (*)"
                    ,"\n FROM   ",v_tbl_mov
                    ,"\n WHERE  f_liquida = '",v_fecha_liquida,"'"
          
     PREPARE prp_valida_fcm FROM v_QryTxt
     EXECUTE prp_valida_fcm INTO v_existe_liquida_movto
    
     --Valida que el folio exista en glo_ctr_archivo 
     IF v_existe_liquida_movto > 0 THEN
        LET v_existe_liquida_movto1 = 0
        LET v_QryTxt = "\n SELECT COUNT (*)                         ",
                       "\n FROM   glo_folio                         ",
                       "\n WHERE  status      = 2                   ",
                       "\n AND    proceso_cod = 901                 ",
                       "\n AND    f_actualiza = '",v_fecha_liquida,"'"
        PREPARE prp_valida_x_fecha FROM v_QryTxt
        EXECUTE prp_valida_x_fecha INTO v_existe_liquida_movto1

        IF v_existe_liquida_movto = 0 THEN
           CALL fn_mensaje("Atención","Fecha de liquidación no existe","about")
        END IF
     ELSE
        CALL fn_mensaje("Atención","Fecha de liquidación no existe","about")
     END IF
  END IF
   
  RETURN v_existe_liquida_movto1

END FUNCTION

#Objetivo: Función para obtener la información de la tabla cta_movimiento
FUNCTION fn_obtiene_informacion(v_folio_liquida,v_fecha_liquida)
DEFINE 
  v_folio_liquida            LIKE cta_movimiento.folio_liquida, --Folio liquidado
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,  --Fecha liquidación
  v_sql_txt                  STRING

DEFINE v_arr_ctamovto        DYNAMIC ARRAY OF RECORD
  v_subcuenta                CHAR(55),--LIKE cta_movimiento.subcuenta,
  v_fondo_inversion          CHAR(55),--LIKE cta_movimiento.fondo_inversion,
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_monto_acciones           LIKE cta_movimiento.monto_acciones,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos 
END RECORD

DEFINE
  v_tot_monto_pesos          LIKE cta_movimiento.monto_pesos,
  v_tot_monto_acciones       LIKE cta_movimiento.monto_acciones,
  v_tot_total_cuentas        LIKE cta_movimiento.monto_pesos,
  v_indice                   INTEGER 

  --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  CASE
    WHEN length(v_folio_liquida) > 0 --este el que tiene prioridad
      EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio
    WHEN length(v_fecha_liquida) > 0
      EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --por fecha de liquidación
  END CASE
  -- este es el que tiene la prioridad sobre la fecha por eso esta segundo

  LET v_sql_txt= "\n SELECT cm.subcuenta || '-' || ",
                 "\n        cs.subcuenta_desc AS SUBCUENTA, ",
                 "\n        cm.fondo_inversion || '-' || ",
                 "\n        cf.razon_social AS FONDO, ",                      
                 "\n        sum(cm.monto_pesos),",
                 "\n        sum(cm.monto_acciones), count(*)",
                 "\n FROM   ",v_tbl_mov," cm, cat_subcuenta cs, cat_fondo_local cf",
                 "\n WHERE  cm.subcuenta       = cs.subcuenta",
                 "\n AND    cm.fondo_inversion = cf.fondo"
                     
  IF length(v_folio_liquida) > 0 THEN
     LET v_sql_txt = v_sql_txt||"\n AND folio_liquida = ",v_folio_liquida
  END IF

  IF length(v_fecha_liquida) > 0 THEN
      LET v_sql_txt = v_sql_txt||"\n AND f_liquida = '",v_fecha_liquida,"'"
  END IF

  LET v_sql_txt = v_sql_txt||"\n GROUP BY 1,2"
  LET v_sql_txt = v_sql_txt||"\n ORDER BY 1,2"

  PREPARE prp_sql_his_dispersion FROM v_sql_txt

  LET v_indice             = 1
  LET v_tot_monto_pesos    = 0.00
  LET v_tot_monto_acciones = 0.00
  LET v_tot_total_cuentas  = 0.00

  DECLARE cur_cta_movimiento CURSOR FOR prp_sql_his_dispersion
  FOREACH cur_cta_movimiento INTO v_arr_ctamovto[v_indice].v_subcuenta,
                                  v_arr_ctamovto[v_indice].v_fondo_inversion,
                                  v_arr_ctamovto[v_indice].v_monto_pesos,
                                  v_arr_ctamovto[v_indice].v_monto_acciones,
                                  v_arr_ctamovto[v_indice].v_total_cuentas
    LET v_tot_monto_pesos    = v_tot_monto_pesos + 
                               v_arr_ctamovto[v_indice].v_monto_pesos
    LET v_tot_monto_acciones = v_tot_monto_acciones +
                               v_arr_ctamovto[v_indice].v_monto_acciones
    LET v_tot_total_cuentas  = v_tot_total_cuentas + 
                               v_arr_ctamovto[v_indice].v_total_cuentas
    LET v_indice             = v_indice + 1
  END FOREACH

  CALL v_arr_ctamovto.deleteElement(v_indice)
  RETURN v_arr_ctamovto, v_indice, v_tot_monto_pesos, v_tot_monto_acciones, v_tot_total_cuentas

END FUNCTION

#OBJETIVO: Función para recuperar la información de Mandatos
FUNCTION fn_obtiene_inf_mandatos(p_folio, p_fecha)
DEFINE 
  p_folio                    LIKE cta_movimiento.folio_liquida,
  p_fecha                    DATE,
  QryTxt                     STRING,
  v_indice                   INTEGER,
  v_tot_monto_pesos          LIKE cta_movimiento.monto_pesos,
  v_tot_total_cuentas        LIKE cta_movimiento.monto_pesos
          
DEFINE v_arr_mandatos        DYNAMIC ARRAY OF RECORD
  v_servicios                CHAR(55),
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos
END RECORD      

  --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  CASE
    WHEN length(p_folio) > 0 --este el que tiene prioridad
      EXECUTE fn_tbl_mov USING Por_Folio,p_folio,Sin INTO v_tbl_mov --por folio
    WHEN length(p_fecha) > 0
      EXECUTE fn_tbl_mov USING Por_Fecha,Sin,p_fecha INTO v_tbl_mov --por fecha de liquidación
  END CASE

  LET QryTxt = "   SELECT movimiento ||'-' ||       ",
               "\n       origen as SERVICIO,        ",
               "\n       SUM(monto_pesos),          ",
               "\n       COUNT(id_derechohabiente)  ",
               "\n FROM  ",v_tbl_mov,
               "\n WHERE movimiento IN (312,322,332)"

  IF length(p_folio) > 0 THEN
     LET QryTxt = QryTxt||"\n AND folio_liquida = ",p_folio
  END IF

  IF length(p_fecha) > 0 THEN
     LET QryTxt = QryTxt||"\n AND f_liquida     = '",p_fecha,"'"
  END IF

  LET QryTxt = QryTxt||"\n  GROUP BY movimiento, origen"
  LET QryTxt = QryTxt||"\n  ORDER BY 1"

  DISPLAY QryTxt          

  PREPARE prp_cons_mdt FROM QryTxt

  LET v_indice            = 1
  LET v_tot_monto_pesos   = 0.00
  LET v_tot_total_cuentas = 0.00

  DECLARE cur_cons_mdt CURSOR FOR prp_cons_mdt
  FOREACH cur_cons_mdt INTO v_arr_mandatos[v_indice].v_servicios,
                            v_arr_mandatos[v_indice].v_monto_pesos,
                            v_arr_mandatos[v_indice].v_total_cuentas
    LET v_tot_monto_pesos   = v_tot_monto_pesos + 
                              v_arr_mandatos[v_indice].v_monto_pesos
    LET v_tot_total_cuentas = v_tot_total_cuentas + 
                              v_arr_mandatos[v_indice].v_total_cuentas
    LET v_indice            = v_indice + 1
  END FOREACH

  CALL v_arr_mandatos.deleteElement(v_indice)
   
  RETURN v_arr_mandatos, v_indice, v_tot_monto_pesos, v_tot_total_cuentas

END FUNCTION 

#Objetivo: Genera reporte historico de la dispersión
FUNCTION fn_reporte_historico_dispersion(v_folio_liquida,v_fecha_liquida,p_usuario)
DEFINE 
  v_folio_liquida            LIKE cta_movimiento.folio_liquida, --Folio liquidado
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,  --Fecha liquidación
  p_usuario                  LIKE seg_usuario.usuario_cod, -- Clave del usuario
  manejador_rpt              om.SaxDocumentHandler  -- Contenedor documentos reporte

DEFINE v_arr_his_disp        DYNAMIC ARRAY OF RECORD
  v_subcuenta                CHAR (55),--LIKE cta_movimiento.subcuenta,
  v_fondo_inversion          CHAR(55),--LIKE cta_movimiento.fondo_inversion,
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_monto_acciones           LIKE cta_movimiento.monto_acciones,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos 
END RECORD

DEFINE
  v_tot_monto_pesos          LIKE cta_movimiento.monto_pesos,
  v_tot_monto_acciones       LIKE cta_movimiento.monto_acciones,
  v_tot_total_cuentas        INTEGER ,--SMALLINT,
  v_tot_cuentas_sctas        DECIMAL(20,0),--SMALLINT,
  v_sum_tot_cuentas          DECIMAL(20,0),--SMALLINT,
  v_indice                   INTEGER,
  v_sql_txt                  STRING,
  QryTxt                     STRING,
  v_tpo_enc                  STRING

DEFINE 
  v_tot_monto_pesos_rpt      LIKE cta_movimiento.monto_pesos,
  v_tot_total_cuentas_rpt    LIKE cta_movimiento.monto_pesos

DEFINE v_arr_mandatos_rpt    DYNAMIC ARRAY OF RECORD
  v_servicios                CHAR(55),
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos
END RECORD

  -- Botón para generar el reporte en PDF de la consulta
  --IF fgl_report_loadCurrentSettings("consulta_historica_disp.4rp") THEN
  IF fgl_report_loadCurrentSettings("DISC042.4rp") THEN  
     CALL fgl_report_selectDevice ("PDF")
     LET manejador_rpt = fgl_report_commitCurrentSettings()
  END IF

  --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
  CASE
    WHEN length(v_folio_liquida) > 0 --este el que tiene prioridad
      EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio
    WHEN length(v_fecha_liquida) > 0
      EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --por fecha de liquidación
  END CASE

  --Inicia el reporte de registros con rechazo
  START REPORT rp_his_disp TO XML HANDLER manejador_rpt
    LET v_sql_txt= "\n SELECT cm.subcuenta || '-' || ",
                   "\n        cs.subcuenta_desc AS SUBCUENTA, ",
                   "\n        cm.fondo_inversion || '-' || ",
                   "\n        cf.razon_social AS FONDO, ",                      
                   "\n        sum(cm.monto_pesos),",
                   "\n        sum(cm.monto_acciones), count(*)",
                   "\n FROM   ",v_tbl_mov," cm, cat_subcuenta cs, cat_fondo_local cf",
                   "\n WHERE  cm.subcuenta       = cs.subcuenta",
                   "\n AND    cm.fondo_inversion = cf.fondo"
    IF length(v_folio_liquida) > 0 THEN
       LET v_sql_txt = v_sql_txt||"\n AND folio_liquida = ",v_folio_liquida
    END IF

    IF length(v_fecha_liquida) > 0 THEN
       LET v_sql_txt = v_sql_txt||"\n AND f_liquida     = '",v_fecha_liquida,"'"
    END IF

    LET v_sql_txt = v_sql_txt||"\n GROUP BY 1,2"
    LET v_sql_txt = v_sql_txt||"\n ORDER BY 1,2"

    PREPARE prp_sql_his_dis FROM v_sql_txt
    LET v_indice             = 1
    LET v_tot_monto_pesos    = 0.00
    LET v_tot_monto_acciones = 0.00
    LET v_tot_total_cuentas  = 0.00
    LET v_tot_cuentas_sctas  = 0.00

    EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio

    IF length(v_fecha_liquida) = 0 THEN 
       LET v_sql_txt=  "\n SELECT f_liquida         "
                      ,"\n FROM   ",v_tbl_mov
                      ,"\n WHERE  folio_liquida = ? "
                      ,"\n GROUP BY 1               "
       PREPARE prp_sql_his_dis_c FROM v_sql_txt
       EXECUTE prp_sql_his_dis_c USING v_folio_liquida INTO v_fecha_liquida
    END IF 

    LET v_sql_txt=  "\n SELECT COUNT (*)        "
                   ,"\n FROM   ",v_tbl_mov
                   ,"\n WHERE  folio_liquida = ? "
    PREPARE prp_sql_his_dis_d FROM v_sql_txt
    EXECUTE prp_sql_his_dis_d USING v_folio_liquida INTO v_sum_tot_cuentas

    DECLARE cur_his_dispersion CURSOR FOR prp_sql_his_dis
    FOREACH cur_his_dispersion INTO v_arr_his_disp[v_indice].v_subcuenta,
                                    v_arr_his_disp[v_indice].v_fondo_inversion,
                                    v_arr_his_disp[v_indice].v_monto_pesos,
                                    v_arr_his_disp[v_indice].v_monto_acciones,
                                    v_arr_his_disp[v_indice].v_total_cuentas
      LET v_tot_monto_pesos    = v_tot_monto_pesos + 
                                 v_arr_his_disp[v_indice].v_monto_pesos
      LET v_tot_monto_acciones = v_tot_monto_acciones + 
                                 v_arr_his_disp[v_indice].v_monto_acciones
      LET v_tot_cuentas_sctas  = v_tot_cuentas_sctas + 
                                 v_arr_his_disp[v_indice].v_total_cuentas
      LET v_tpo_enc            = 1                                   

      OUTPUT TO REPORT rp_his_disp(v_arr_his_disp[v_indice].*,
                                   v_tot_monto_pesos,
                                   v_tot_monto_acciones,
                                   v_tot_cuentas_sctas, 
                                   v_folio_liquida,
                                   v_fecha_liquida, 
                                   p_usuario, 
                                   v_sum_tot_cuentas,
                                   v_arr_mandatos_rpt[v_indice].*,
                                   v_tot_monto_pesos_rpt,
                                   v_tot_total_cuentas_rpt,
                                   v_tpo_enc)
      LET v_indice = v_indice + 1
    END FOREACH

    --20140123 Se agrega uso de fn_tab_movimiento, que es fn_tbl_mov en el programa
    CASE
      WHEN length(v_folio_liquida) > 0 --este el que tiene prioridad
        EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio
      WHEN length(v_fecha_liquida) > 0
        EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --por fecha de liquidación
    END CASE

    LET QryTxt = "\n SELECT movimiento||'-'||origen,   ",
                 "\n        SUM(monto_pesos),          ",
                 "\n        COUNT(id_derechohabiente)  ",
                 "\n FROM   ",v_tbl_mov,
                 "\n WHERE  movimiento IN (312,322,332)"

    IF length(v_folio_liquida) > 0 THEN
       LET QryTxt = QryTxt||"\n AND folio_liquida = ",v_folio_liquida
    END IF

    IF length(v_fecha_liquida) > 0 THEN
       LET QryTxt = QryTxt||"\n AND f_liquida     = '",v_fecha_liquida,"'"
    END IF

    LET QryTxt = QryTxt||"\n  GROUP BY movimiento, origen"
    LET QryTxt = QryTxt||"\n  ORDER BY 1"

    PREPARE prp_cons_mdt_rpt FROM QryTxt
    LET v_indice                = 1
    LET v_tot_monto_pesos_rpt   = 0.00
    LET v_tot_total_cuentas_rpt = 0.00

    DECLARE cur_cons_mdt_rpt CURSOR FOR prp_cons_mdt_rpt
    FOREACH cur_cons_mdt_rpt INTO v_arr_mandatos_rpt[v_indice].v_servicios,
                                  v_arr_mandatos_rpt[v_indice].v_monto_pesos,
                                  v_arr_mandatos_rpt[v_indice].v_total_cuentas
                                                                      
      LET v_tot_monto_pesos_rpt   = v_tot_monto_pesos_rpt + 
                                    v_arr_mandatos_rpt[v_indice].v_monto_pesos
      LET v_tot_total_cuentas_rpt = v_tot_total_cuentas_rpt + 
                                    v_arr_mandatos_rpt[v_indice].v_total_cuentas
      LET v_tpo_enc               = 2                                   

      OUTPUT TO REPORT rp_his_disp(v_arr_his_disp[v_indice].*,
                                   v_tot_monto_pesos,
                                   v_tot_monto_acciones,
                                   v_tot_cuentas_sctas, 
                                   v_folio_liquida,
                                   v_fecha_liquida, 
                                   p_usuario, 
                                   v_sum_tot_cuentas,
                                   v_arr_mandatos_rpt[v_indice].*,
                                   v_tot_monto_pesos_rpt,
                                   v_tot_total_cuentas_rpt,
                                   v_tpo_enc)                                      
      LET v_indice = v_indice + 1
    END FOREACH
  FINISH REPORT rp_his_disp
      
END FUNCTION

#Objetivo: Genera repote de avances de pago
REPORT rp_his_disp(arr_his_disp,
                   v_tot_monto_pesos,
                   v_tot_monto_acciones,
                   v_tot_cuentas_scta, 
                   p_folio_liquida,
                   p_fecha_liquida, 
                   p_usuario, 
                   p_sum_tot_cuentas,
                   arr_mandatos_rpt, 
                   p_tot_monto_pesos_rpt,
                   p_tot_total_cuentas_rpt,
                   p_tpo_enc)
                   
DEFINE 
  p_tot_monto_pesos_rpt      LIKE cta_movimiento.monto_pesos,
  p_tot_total_cuentas_rpt    LIKE cta_movimiento.monto_pesos,
  p_tpo_enc                  SMALLINT,
  v_v_desc_detalle           STRING

DEFINE arr_mandatos_rpt      RECORD
  v_servicios                CHAR(55),
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos
END RECORD                                      

DEFINE arr_his_disp          RECORD
  v_subcuenta                CHAR(55),--LIKE cta_movimiento.subcuenta,
  v_fondo_inversion          CHAR(55),--LIKE cta_movimiento.fondo_inversion,
  v_monto_pesos              LIKE cta_movimiento.monto_pesos,
  v_monto_acciones           LIKE cta_movimiento.monto_acciones,
  v_total_cuentas            LIKE cta_movimiento.monto_pesos 
END RECORD

DEFINE          
  v_tot_monto_pesos          LIKE cta_movimiento.monto_pesos,
  v_tot_monto_acciones       LIKE cta_movimiento.monto_acciones,
  v_tot_cuentas_scta         DECIMAL(20,2),-- LIKE cta_movimiento.monto_pesos,
  v_fecha_reporte            DATE,
  sum_tot_pesos              LIKE dis_preliquida.monto_pesos,      
  sum_tot_acciones           LIKE dis_preliquida.monto_acciones, 
  sum_tot_ctas               DECIMAL(20,2),
  p_folio_liquida            LIKE cta_movimiento.folio_liquida, --Folio liquidado
  p_fecha_liquida            LIKE cta_movimiento.f_liquida,  --Fecha liquidación
  p_usuario                  LIKE seg_usuario.usuario_cod, -- Clave del usuario
  p_sum_tot_cuentas          DECIMAL(22,2)

FORMAT
  FIRST PAGE HEADER

    LET v_fecha_reporte = TODAY
    DISPLAY v_fecha_reporte    

    PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
    PRINTX p_folio_liquida
    PRINTX p_usuario
    PRINTX p_fecha_liquida USING "dd-mm-yyyy"

    BEFORE GROUP OF p_tpo_enc
      IF p_tpo_enc = 1 THEN
         LET v_v_desc_detalle = "Subcuentas"
      ELSE
         LET v_v_desc_detalle = "Mandatos"
      END IF

      PRINT p_tpo_enc
      PRINTX v_v_desc_detalle
      
    ON EVERY ROW
       PRINTX arr_his_disp.v_subcuenta
       PRINTX arr_his_disp.v_fondo_inversion
       PRINTX arr_his_disp.v_monto_pesos
       PRINTX arr_his_disp.v_monto_acciones
       PRINTX arr_his_disp.v_total_cuentas
       PRINTX arr_mandatos_rpt.*
       PRINTX p_tpo_enc      

    ON LAST ROW
       LET sum_tot_pesos    = v_tot_monto_pesos
       LET sum_tot_acciones = v_tot_monto_acciones
       LET sum_tot_ctas     = v_tot_cuentas_scta      
       PRINTX sum_tot_pesos
       PRINTX sum_tot_acciones
       PRINTX sum_tot_ctas 
       PRINTX p_tot_monto_pesos_rpt
       PRINTX p_tot_total_cuentas_rpt
       
END REPORT