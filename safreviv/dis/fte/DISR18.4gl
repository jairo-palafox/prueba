################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 05/04/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR18                                                        #
#Objetivo     => Programa que ejecuta el reverso de la liquidación de          #
#                aportaciones subsecuentes sin adelanto.                       #
#Fecha inicio => 19/11/2015                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --Id del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Código de operación
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

    CONSTANT Por_Folio = 0
    CONSTANT Por_Fecha = 1
    CONSTANT Sin       = 0

  DEFINE v_tbl_mov           VARCHAR(50)
  
END GLOBALS

MAIN
  DEFINE 
    p_usuario_cod            LIKE seg_usuario.usuario_cod,   --Clave del usuario
    p_tipo_ejecucion         SMALLINT, --Forma como ejecutara el programa
    p_s_titulo               STRING,   --Título de la ventana
    v_folio_liquida          LIKE cta_movimiento.folio_liquida,--Folio liquidado
    v_fecha_liquida          LIKE cta_movimiento.f_liquida,
    r_existe_registro        SMALLINT,
    r_respuesta              INTEGER,

    r_arr_ctamovto           DYNAMIC ARRAY OF RECORD
      v_subcuenta            CHAR(55),
      v_fondo_inversion      CHAR(55),
      v_monto_pesos          DECIMAL(22,2),
      v_monto_acciones       DECIMAL(26,6),
      v_total_cuentas        DECIMAL(12,0) 
    END RECORD,
  
    r_tot_pesos              DECIMAL(22,2),
    r_tot_acciones           DECIMAL(26,6),
    r_tot_ctas               DECIMAL(12,0),
    v_indice_mdt             INTEGER,
    v_tot_monto_pesos_mdt    DECIMAL(22,2),
    v_tot_total_cuentas_mdt  DECIMAL(12,0)
       
  DEFINE r_arr_mandatos      DYNAMIC ARRAY OF RECORD
    v_servicios              CHAR(55),
    v_monto_pesos            DECIMAL(22,2),
    v_total_cuentas          DECIMAL(12,0)
  END RECORD 
  
  DEFINE 
    f_ventana                ui.Window,   --Define las propìedades de la Ventana
    f_forma                  ui.Form,     --Define las propiedades de la forma
    r_sql_code               SMALLINT,
    r_bandera                SMALLINT,
    v_QryTxt                 STRING
    
  DEFINE v_seg_modulo        RECORD LIKE seg_modulo.*
  DEFINE seg_modulo_bat      RECORD LIKE seg_modulo.*

  SELECT *
  INTO   v_seg_modulo.*
  FROM   seg_modulo
  WHERE  modulo_cod = "dis" 

  SELECT *
  INTO   seg_modulo_bat.*
  FROM   seg_modulo
  WHERE  modulo_cod = "bat" 

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)
  LET g_proceso_cod    = 932
  LET g_opera_cod      = 4

  LET v_fecha_liquida  = ""

  --Si se obtuvo el titulo, se pone como titulo de programa
  IF ( p_s_titulo IS NOT NULL ) THEN
     CALL ui.Interface.setText(p_s_titulo)
  END IF

  --Validación que NO se tenga alguna operación de Dispersión de Pagos ejecutándose
  LET g_sql_txt = "SELECT a.cod_proceso_entra,   ",
                  "       a.cod_proceso_valida,  ",
                  "       b.proceso_desc         ",
                  "FROM   cat_convivencia_dis a, ",
                  "       cat_proceso b          ",
                  "WHERE  a.cod_proceso_entra  = ", g_proceso_cod,
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

  --Llama la función para obtener el PID
  --CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid   
  PREPARE fn_tbl_mov FROM "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"
   
  CLOSE WINDOW SCREEN 

  OPEN WINDOW vtn_prel_disp WITH FORM "DISR181"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_liquida,v_fecha_liquida --WITHOUT DEFAULTS
      FROM f_folio,f_fecha  --ATTRIBUTES(UNBUFFERED)
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("gr_detalles_sbctas", 1) --Oculta el detalle de subcuentas 
          CALL f_forma.setElementHidden("gr_detalles_mdtos", 1)  --Oculta el detalle de mandatos
          CALL DIALOG.setActionHidden("reverso", 1)            
      END INPUT

      DISPLAY ARRAY r_arr_ctamovto TO scr_det_scta.*
      END DISPLAY 

      DISPLAY ARRAY r_arr_mandatos TO scr_det_mandato.*
      END DISPLAY 

      ON ACTION cancelar
         EXIT DIALOG    
         
      ON ACTION ACCEPT
         CALL fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
         RETURNING v_folio_liquida    

         IF v_folio_liquida IS NOT NULL THEN
            CALL f_forma.setElementHidden("gr_detalles_sbctas", 0) --Muestra el detalle de subcuentas
            CALL f_forma.setElementHidden("gr_detalles_mdtos", 0)  --Muestra el detalle de mandatos
            CALL DIALOG.setActionHidden("reverso", 0) --Muestra el botón reporte 
            CALL DIALOG.setActionHidden("accept", 1)  --Muestra el botón reporte 

            --Obtiene informacion Subcuentas
            CALL fn_obtiene_informacion(v_folio_liquida)
            RETURNING r_arr_ctamovto,r_tot_pesos,r_tot_acciones,r_tot_ctas

            --Obtiene Información Mandatos
            CALL fn_obtiene_inf_mandatos(v_folio_liquida,v_fecha_liquida)
            RETURNING r_arr_mandatos, v_indice_mdt, 
                      v_tot_monto_pesos_mdt, v_tot_total_cuentas_mdt
               
            --Muestra las cifras totales de subcuentas
            DISPLAY r_tot_pesos    TO sum_pesos
            DISPLAY r_tot_acciones TO sum_acciones
            DISPLAY r_tot_ctas     TO sum_cuentas

            --Muestra las cifras totales de mandatos
            DISPLAY v_tot_monto_pesos_mdt   TO sum_pesos_m
            DISPLAY v_tot_total_cuentas_mdt TO sum_cuentas_m
         END IF 

      ON ACTION reverso
         --Solicita confirmar(1) o cancelar(0) la operación de Registro
         CALL fn_ventana_confirma("Aportaciones Subsecuentes", 
                                  "¿Desea ejecutar el reverso?", 
                                  "quest") 
         RETURNING r_respuesta

         IF r_respuesta = 1 THEN
            --Se obtiene pid de la operación de acuerdo al folio
            SELECT DISTINCT pid 
            INTO   g_pid
            FROM   bat_ctr_operacion
            WHERE  folio = v_folio_liquida

            --Si no existe el pid de acuerdo al folio, traerá el último
            IF g_pid IS NULL OR g_pid = 0 THEN
               CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid 
            END IF
                  
            --Llama la función para validar la ejecucución del reverso
            CALL fn_valida_reverso(g_pid, g_proceso_cod, g_opera_cod)
            RETURNING r_bandera

            IF r_bandera  <> 0 THEN
               CALL fn_muestra_inc_operacion(r_bandera)
               EXIT PROGRAM 
            END IF 

            LET v_QryTxt = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/DISR181.42r ",
                           p_usuario_cod CLIPPED, " ",
                           g_pid  , " " ,
                           g_proceso_cod , " " ,
                           g_opera_cod ," ",
                           v_folio_liquida ," '",
                           "0" CLIPPED,"' ",
                           " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                           "/nohup:",g_pid        USING "&&&&&",":",
                           g_proceso_cod USING "&&&&&",":",
                           g_opera_cod   USING "&&&&&" ,
                           " 2>&1 &"
            RUN v_QryTxt
                           
            CALL fn_mensaje("Atención",
                            "Se ha enviado el reverso de la Liquidación.\n"||
                            "Puede revisar el avance del proceso en el monitor de ejecución de procesos",
                            "information")
         ELSE
            CALL fn_mensaje("Atención","Se canceló el reverso","about")
         END IF

         EXIT DIALOG 
    END DIALOG   
  CLOSE WINDOW vtn_prel_disp
END MAIN

#Objetivo: Consulta para validar que exista el folio y/o fecha capturadas en la
#          la tabla de movimientos del proceso de liquidación
FUNCTION fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
DEFINE 
  v_folio_liquida            LIKE cta_movimiento.folio_liquida,--Folio liquidado
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,  --Fecha liquidación
  v_existe_liquida_movto     INTEGER, --Contador si existen registros
  v_existe_liquida_movto1    INTEGER, --Contador si existen registros
  v_QryTxt                   STRING,
  v_consulta                 STRING,
  v_folio_reg                LIKE cta_movimiento.folio_liquida

  CASE
    WHEN length(v_folio_liquida) > 0 --Este el que tiene prioridad
      EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --Por folio
    WHEN length(v_fecha_liquida) > 0
      EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --Por fecha de liquidación
  END CASE

  --Valida que existan registros en cta_movimiento de acuerdo a los criterios de búsqueda
  LET v_consulta = "\n SELECT COUNT (*) ",
                   "\n FROM ", v_tbl_mov,
                   "\n WHERE "

  --Regresa el folio en caso de que existan registros en cta_movimiento
  LET v_QryTxt = "\n SELECT folio ",
                 "\n FROM   glo_folio ",
                 "\n WHERE  status       = 2 ",
                 "\n AND    proceso_cod  = ",g_proceso_cod

                 --"\n   AND folio       = ", v_folio_liquida,
                 --"\n   AND f_actualiza = '",v_fecha_liquida,"'"
                    
  IF (v_folio_liquida IS NOT NULL) AND (v_fecha_liquida IS NOT NULL) THEN 
     LET v_consulta = v_consulta || " folio_liquida = ", v_folio_liquida,
                      " AND f_liquida = '",v_fecha_liquida,"'"

     LET v_QryTxt   = v_QryTxt || " AND folio = ", v_folio_liquida,
                      " AND f_actualiza = '",v_fecha_liquida,"'"
  END IF 

  IF (v_folio_liquida IS NOT NULL) AND (v_fecha_liquida IS NULL) THEN 
     LET v_consulta = v_consulta || " folio_liquida = ", v_folio_liquida

     LET v_QryTxt   = v_QryTxt || " AND folio = ", v_folio_liquida
  END IF 

  IF (v_folio_liquida IS NULL) AND (v_fecha_liquida IS NOT NULL) THEN 
     LET v_consulta = v_consulta || " f_liquida = '",v_fecha_liquida,"'"

     LET v_QryTxt   = v_QryTxt || " AND f_actualiza = '",v_fecha_liquida,"'"
  END IF 

  --Ejecuta la consulta para validar que existan registros
  PREPARE prp_valida_reg FROM v_consulta
  EXECUTE prp_valida_reg INTO v_existe_liquida_movto
   
  IF v_existe_liquida_movto > 0 THEN
     --Si existen movimientos con esos parámetros de búsqueda, se realiza una consulta en glo_folio
     PREPARE prp_reg_folio FROM v_QryTxt
     EXECUTE prp_reg_folio INTO v_folio_reg
   
     IF v_folio_reg IS NULL THEN
        CALL fn_mensaje("Atención","Folio no liquidado","about")
     END IF
  ELSE
     CALL fn_mensaje("Atención","No existe registro con los parámetros capturados","about")
  END IF 
   
  RETURN v_folio_reg
END FUNCTION

#Objetivo: Función para obtener la información de la tabla cta_movimiento
FUNCTION fn_obtiene_informacion(v_folio_liquida)
  DEFINE 
    v_folio_liquida          LIKE cta_movimiento.folio_liquida,--Folio liquidado
    v_fecha_liquida          LIKE cta_movimiento.f_liquida,  --Fecha liquidación
    v_sql_txt                STRING,

    v_arr_ctamovto           DYNAMIC ARRAY OF RECORD
      v_subcuenta            CHAR(55),
      v_fondo_inversion      CHAR(55),
      v_monto_pesos          DECIMAL(22,2),
      v_monto_acciones       DECIMAL(26,6),
      v_total_cuentas        DECIMAL(12,0) 
    END RECORD,
  
    v_tot_monto_pesos        DECIMAL(22,2),
    v_tot_monto_acciones     DECIMAL(26,6),
    v_tot_total_cuentas      DECIMAL(12,0),
    v_indice                 INTEGER 

  EXECUTE fn_tbl_mov USING Por_Folio,v_folio_liquida,Sin INTO v_tbl_mov --por folio

  LET v_sql_txt= "\n SELECT cm.subcuenta || '-' || ",
                 "\n        cs.subcuenta_desc AS SUBCUENTA, ",
                 "\n        cm.fondo_inversion || '-' || ",
                 "\n        cf.razon_social AS FONDO, ",                      
                 "\n        sum(cm.monto_pesos),",
                 "\n        sum(cm.monto_acciones), count(*)",
                 "\n FROM   ",v_tbl_mov," cm",
                 "\n JOIN   cat_subcuenta cs",
                 "\n ON     cm.subcuenta       = cs.subcuenta",
                 "\n JOIN   cat_fondo_local cf",
                 "\n ON     cm.fondo_inversion = cf.fondo",
                 "\n WHERE  1=1",
                 "\n AND    folio_liquida      = ",v_folio_liquida
                  
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
  
  RETURN v_arr_ctamovto,v_tot_monto_pesos,v_tot_monto_acciones,
         v_tot_total_cuentas
END FUNCTION

#OBJETIVO: Función para recuperar la información de Mandatos
FUNCTION fn_obtiene_inf_mandatos(p_folio,v_fecha_liquida)
  DEFINE 
    p_folio                  LIKE cta_movimiento.folio_liquida,
    v_fecha_liquida          LIKE cta_movimiento.f_liquida,
    QryTxt                   STRING,
    v_indice                 INTEGER,
    v_tot_monto_pesos        DECIMAL(22,2),
    v_tot_total_cuentas      DECIMAL(12,0)
          
  DEFINE 
    v_arr_mandatos           DYNAMIC ARRAY OF RECORD
      v_servicios            CHAR(55),
      v_monto_pesos          DECIMAL(22,2),
      v_total_cuentas        DECIMAL(12,0)
    END RECORD      

  CASE
    WHEN length(p_folio) > 0 --Este el que tiene prioridad
      EXECUTE fn_tbl_mov USING Por_Folio,p_folio,Sin INTO v_tbl_mov --Por folio
    WHEN length(v_fecha_liquida) > 0
      EXECUTE fn_tbl_mov USING Por_Fecha,Sin,v_fecha_liquida INTO v_tbl_mov --Por fecha de liquidación
  END CASE

  LET QryTxt = "\n SELECT movimiento ||'-' ||",
               "\n        origen as SERVICIO, ",
               "\n        SUM(monto_pesos), ",
               "\n        COUNT(id_derechohabiente)",
               "\n FROM ",v_tbl_mov,
               "\n WHERE  movimiento IN (312,322,332)"

  IF length(p_folio) > 0 THEN
     LET QryTxt = QryTxt||"\n AND folio_liquida = '",p_folio,"'"
  END IF

  IF length(v_fecha_liquida) > 0 THEN
     LET QryTxt = QryTxt||"\n AND f_liquida = '",v_fecha_liquida,"'"
  END IF

  LET QryTxt = QryTxt||"\n GROUP BY movimiento,origen"
  LET QryTxt = QryTxt||"\n ORDER BY Servicio"
                
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