################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 09/10/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR27                                                        #
#Objetivo     => Programa que ejecuta el reverso de la preliquidación de la    #
#                cancelación parcial de avances de pago.                       #
#Fecha inicio => 09/10/2018                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_pid                    LIKE bat_ctr_proceso.pid,     --Id del proceso
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --Código del proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --Código de operación
    v_b_rev_pre              SMALLINT,          --Bandera reversa preliquidación
    g_sql_txt                STRING,
    v_proc_entra             SMALLINT,
    v_proc_val               SMALLINT,
    v_cod_conv               SMALLINT,
    v_desc_proc_val          CHAR(40),
    v_mensaje_val            STRING

END GLOBALS

MAIN
DEFINE 
  p_usuario_cod              LIKE seg_usuario.usuario_cod, --Clave del usuario
  p_tipo_ejecucion           SMALLINT, --Forma como ejecutara el programa
  p_s_titulo                 STRING,   --Título de la ventana
  v_folio_liquida            LIKE cta_movimiento.folio_liquida,--Folio liquidado
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,
  r_respuesta                SMALLINT       

DEFINE 
  r_arr_preliquidacion       DYNAMIC ARRAY OF RECORD
    v_subcuenta              CHAR(55),
    v_fondo_inversion        CHAR(55),
    v_movimiento             CHAR(55),
    v_monto_pesos            DECIMAL(22,2),
    v_monto_acciones         DECIMAL(26,6),
    v_total_cuentas          DECIMAL(12,0) 
  END RECORD,

  r_tot_pesos                DECIMAL(22,2),
  r_tot_acciones             DECIMAL(26,6),
  r_tot_ctas                 DECIMAL(12,0)
       
DEFINE 
  f_ventana                  ui.Window, --Define las propìedades de la Ventana
  f_forma                    ui.Form,   --Define las propiedades de la forma
  v_bnd_val_rev              SMALLINT,  --Bandera valida reverso
  v_QryTxt                   STRING
  
DEFINE 
  v_seg_modulo               RECORD LIKE seg_modulo.*

DEFINE 
  seg_modulo_bat             RECORD LIKE seg_modulo.*

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET v_fecha_liquida  = ""
  LET g_proceso_cod    = 939
  LET g_opera_cod      = 3

  SELECT *
  INTO   v_seg_modulo.*
  FROM   seg_modulo
  WHERE  modulo_cod = "dis" 

  SELECT *
  INTO   seg_modulo_bat.*
  FROM   seg_modulo
  WHERE  modulo_cod = "bat" 
  -- Llama la función para obtener el PID
  --CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid 

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

  CLOSE WINDOW SCREEN

  OPEN WINDOW vtn_prel_disp WITH FORM "DISR271"
    DIALOG   ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_liquida
      FROM  f_folio
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()
          CALL f_forma.setElementHidden("grp_det_sub_cta", 1)  --Oculta el detalle de subcuentas 
          CALL DIALOG.setActionHidden("reverso", 1)
        END INPUT

        DISPLAY ARRAY r_arr_preliquidacion TO svr_detalle.* 
        END DISPLAY

        ON ACTION cancelar
           EXIT DIALOG 
         
        ON ACTION ACCEPT
           IF LENGTH(v_folio_liquida) = 0 THEN
              CALL fn_mensaje("Atención","Debe capturar al menos un parámetro",
                              "about")
              NEXT FIELD f_folio
           END IF

           CALL fn_movimiento_proceso(v_folio_liquida)
           RETURNING v_folio_liquida

           DISPLAY "v_folio_liquida -- ",v_folio_liquida
           IF v_folio_liquida > 0 THEN
              CALL DIALOG.setActionHidden("reverso", 0)
              CALL f_forma.setElementHidden("grp_det_sub_cta", 0)  --Muestra el detalle de subcuentas 
              CALL DIALOG.setActionHidden("accept", 1)

              --Obtiene informacion de preliquidación               
              CALL fn_obtiene_informacion(v_folio_liquida)
                                RETURNING r_arr_preliquidacion,
                                          r_tot_pesos,
                                          r_tot_acciones,
                                          r_tot_ctas
           ELSE
              CALL fn_mensaje("Reverso Preliquidación",
                              "Información no existe ","about")
              LET v_folio_liquida = NULL 
              LET v_fecha_liquida = NULL 
              CONTINUE DIALOG
           END IF 
        
        ON ACTION reverso
           CALL fn_ventana_confirma("Cancelación Parcial Avances de Pago", 
                                    "¿Desea ejecutar el reverso?", 
                                    "quest") RETURNING r_respuesta
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

              DISPLAY g_pid, g_proceso_cod, g_opera_cod
              CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod)
              RETURNING v_bnd_val_rev

              DISPLAY "validación del reverso -- ",v_bnd_val_rev
                           
              IF v_bnd_val_rev = 0 THEN
                 LET v_QryTxt = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/DISR271.42r ",
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
                                 "Se ha enviado el reverso de la Preliquidación.\n"||
                                 "Puede revisar el avance del proceso en el monitor de ejecución de procesos"
                                 ,"information")
              ELSE 
                 CALL fn_mensaje("Atención",
                                 "Ya se ha ejecutado la operación posterior. \n"
                                 ,"information")
              END IF--Valida reverso
           ELSE
              CALL fn_mensaje("Atención","Se canceló el reverso","about")
           END IF
           EXIT DIALOG
    END DIALOG
  CLOSE WINDOW vtn_prel_disp
END MAIN

#Objetivo: Consulta para validar que exista el folio y/o fecha capturadas en la
#          la tabla de movimisntos del proceso de liquidación
FUNCTION fn_movimiento_proceso(v_folio_liquida)
DEFINE v_folio_liquida       DECIMAL(9,0),   --Folio liquidado
       v_existe_reg          INTEGER,        --Contador si existen registros
       v_sql_txt             STRING
       
  --Valida que existan registros para preliquidación
  LET v_sql_txt = "\n SELECT folio ",
                  "\n FROM   glo_folio ",
                  "\n WHERE  1=1 "
      
  IF LENGTH(v_folio_liquida) > 0 THEN
     --Construye la primera consulta
     LET v_sql_txt = v_sql_txt || "\n AND folio = '",v_folio_liquida,"'"      
  END IF
     
  LET v_sql_txt = v_sql_txt || " GROUP BY 1 "
        
  PREPARE prp_busca_folio_preliquidado FROM v_sql_txt
  EXECUTE prp_busca_folio_preliquidado INTO v_existe_reg

  --Si hay registros para ejecutar, se filtra después por glo_folio
  IF v_existe_reg > 0 THEN
     DISPLAY "Folio encontrado -- ", v_existe_reg
  ELSE 
     DISPLAY "No existe folio en glo folio  "
  END IF 

  RETURN v_existe_reg
END FUNCTION

#Objetivo: Función para obtener la información de la tabla de preliquidación
FUNCTION fn_obtiene_informacion(v_folio_liquida)
DEFINE 
  v_folio_liquida            DECIMAL(9,0),   --Folio liquidado
  v_sql_txt                  STRING,
  
  r_arr_preliquidacion       DYNAMIC ARRAY OF RECORD
    v_subcuenta              CHAR(55),
    v_fondo_inversion        CHAR(55),
    v_movimiento             CHAR(55),
    v_monto_pesos            DECIMAL(22,2),
    v_monto_acciones         DECIMAL(26,6),
    v_total_cuentas          DECIMAL(12,0) 
  END RECORD,
  
  v_tot_monto_pesos          DECIMAL(22,2),
  v_tot_monto_acciones       DECIMAL(26,6),
  v_tot_total_cuentas        DECIMAL(12,0),
  v_indice                   INTEGER 

  LET v_sql_txt= "\n SELECT cm.subcuenta || '-' || ",
                 "\n        cs.subcuenta_desc AS SUBCUENTA, ",
                 "\n        cm.fondo_inversion || '-' || ",
                 "\n        cf.razon_social AS FONDO, ",
                 "\n        cm.movimiento || '-' || ",
                 "\n        mo.movimiento_desc AS MOVIMIENTO, ",                 
                 "\n        sum(cm.monto_pesos),",
                 "\n        sum(cm.monto_acciones), count(*)",
                 "\n FROM   dis_canc_pav_preliquida cm",
                 "\n JOIN   cat_subcuenta cs",
                 "\n ON     cm.subcuenta       = cs.subcuenta",
                 "\n JOIN   cat_fondo_local cf",
                 "\n ON     cm.fondo_inversion = cf.fondo",
                 "\n JOIN   cat_movimiento mo",
                 "\n ON     cm.movimiento = mo.movimiento",
                 "\n WHERE  1=1",
                 "\n AND    folio_liquida      = ",v_folio_liquida

  LET v_sql_txt = v_sql_txt||"\n GROUP BY 1,2,3"
  LET v_sql_txt = v_sql_txt||"\n ORDER BY 1,2,3"

  DISPLAY v_sql_txt

  PREPARE prp_sql_his_dispersion FROM v_sql_txt

  LET v_indice             = 1
  LET v_tot_monto_pesos    = 0.00
  LET v_tot_monto_acciones = 0.00
  LET v_tot_total_cuentas  = 0

  DECLARE cur_cta_movimiento CURSOR FOR prp_sql_his_dispersion
  FOREACH cur_cta_movimiento INTO r_arr_preliquidacion[v_indice].v_subcuenta,
                                  r_arr_preliquidacion[v_indice].v_fondo_inversion,
                                  r_arr_preliquidacion[v_indice].v_movimiento,
                                  r_arr_preliquidacion[v_indice].v_monto_pesos,
                                  r_arr_preliquidacion[v_indice].v_monto_acciones,
                                  r_arr_preliquidacion[v_indice].v_total_cuentas
    LET v_tot_monto_pesos    = v_tot_monto_pesos    + r_arr_preliquidacion[v_indice].v_monto_pesos
    LET v_tot_monto_acciones = v_tot_monto_acciones + r_arr_preliquidacion[v_indice].v_monto_acciones
    LET v_tot_total_cuentas  = v_tot_total_cuentas  + r_arr_preliquidacion[v_indice].v_total_cuentas
    LET v_indice             = v_indice             + 1

  END FOREACH

  CALL r_arr_preliquidacion.deleteElement(v_indice)

  RETURN r_arr_preliquidacion,
         v_tot_monto_pesos,
         v_tot_monto_acciones,
         v_tot_total_cuentas
END FUNCTION