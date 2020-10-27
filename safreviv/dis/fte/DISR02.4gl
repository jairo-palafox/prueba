################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 22/03/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo       => DIS                                                           #
#Programa     => DISR02                                                        #
#Objetivo     => Programa que ejecuta el reverso de la preliquiidacion de la   #
#                dispersion                                                    #
#Fecha inicio => 07/02/2012                                                    #
################################################################################
DATABASE safre_viv
GLOBALS
DEFINE 
  g_pid                      LIKE bat_ctr_proceso.pid,     --ID del proceso
  g_proceso_cod              LIKE cat_proceso.proceso_cod, --codigo del proceso
  g_opera_cod                LIKE cat_operacion.opera_cod, --codigo de operacion
  v_b_rev_pre                SMALLINT --Bandera reversa preliquidación
END GLOBALS

MAIN
DEFINE 
  p_usuario_cod              LIKE seg_usuario.usuario_cod, --Clave del usuario
  p_tipo_ejecucion           SMALLINT, --Forma como ejecutara el programa
  p_s_titulo                 STRING,   --Titulo de la ventana
  v_folio_pagos              LIKE glo_folio.folio_referencia,
  v_folio_liquida            LIKE cta_movimiento.folio_liquida, --Folio liquidado,
  --v_folio_dis_dae            DECIMAL (9,0), -- Folio dis dae
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,
  r_respuesta                SMALLINT,
  r_rev_mtd_eje              SMALLINT,
  r_rev_mtd_dia              SMALLINT,
  r_rev_mtd_afe              DECIMAL(9,0)
       
DEFINE 
  r_arr_preliquidacion       DYNAMIC ARRAY OF RECORD
    v_subcuenta              CHAR(55),
    v_fondo_inversion        CHAR(55),
    v_monto_pesos            DECIMAL(22,2),
    v_monto_acciones         DECIMAL(26,6),
    v_total_cuentas          DECIMAL(12,0) 
    END RECORD,

  r_tot_pesos                DECIMAL(22,2),
  r_tot_acciones             DECIMAL(26,6),
  r_tot_ctas                 DECIMAL(12,0)
       
DEFINE 
  r_arr_mandatos             DYNAMIC ARRAY OF RECORD
    v_servicios              CHAR(55),
    v_monto_pesos            DECIMAL(22,2),
    v_total_cuentas          DECIMAL(12,0) 
    END RECORD,

  v_tot_monto_pesos_mdt      DECIMAL(22,2),
  v_tot_total_cuentas_mdt    DECIMAL(12,0)

DEFINE 
  f_ventana                  ui.Window, --Define las propìedades de la Ventana
  f_forma                    ui.Form,   --Define las propiedades de la forma
  v_bnd_val_rev              SMALLINT,  --Bandera valida reverso
  v_bnd_rev_ope              SMALLINT,  --Bandera reversa operacion
  v_QryTxt                   STRING,
  v_indice_mdt               INTEGER,
  g_sql_txt                  STRING,
  v_proc_entra               SMALLINT,
  v_proc_val                 SMALLINT,
  v_cod_conv                 SMALLINT,
  v_desc_proc_val            CHAR(40),
  v_mensaje_val              STRING

DEFINE 
  v_seg_modulo               RECORD LIKE seg_modulo.*

DEFINE 
  seg_modulo_bat             RECORD LIKE seg_modulo.*

  LET p_usuario_cod    = ARG_VAL(1)
  LET p_tipo_ejecucion = ARG_VAL(2)
  LET p_s_titulo       = ARG_VAL(3)

  LET v_fecha_liquida = ""
  LET g_proceso_cod   = 901
  LET g_opera_cod     = 1

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

  -- si se obtuvo el titulo, se pone como titulo de programa
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

  OPEN WINDOW vtn_prel_disp WITH FORM "DISR021"
    DIALOG ATTRIBUTES(UNBUFFERED) 
      INPUT v_folio_liquida,v_fecha_liquida
      FROM f_folio,f_fecha

        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma = f_ventana.getForm()
          CALL f_forma.setElementHidden("grp_det_sub_cta", 1) --Oculta el detalle de subcuentas 
          CALL f_forma.setElementHidden("grp_det_serv_mtd", 1) --Oculta el detalle de mandatos
          CALL DIALOG.setActionHidden("reverso", 1)
        END INPUT

        DISPLAY ARRAY r_arr_preliquidacion TO svr_detalle.* 
        END DISPLAY

        DISPLAY ARRAY r_arr_mandatos TO svr_servicios.* 
        END DISPLAY               

        ON ACTION cancelar
           EXIT DIALOG 
         
        ON ACTION ACCEPT
           IF v_fecha_liquida > TODAY THEN
              CALL fn_mensaje("Atención","Fecha improcedente","about")
              NEXT FIELD f_fecha
           END IF

           IF length(v_folio_liquida) = 0 AND length(v_fecha_liquida) = 0 THEN
              CALL fn_mensaje("Atención","Debe capturar al menos un parámetro",
                               "about")
              NEXT FIELD f_folio
           END IF

           CALL fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
           RETURNING v_folio_liquida

           DISPLAY "v_folio_liquida -- ",v_folio_liquida
           IF v_folio_liquida > 0 THEN
              CALL DIALOG.setActionHidden("reverso", 0)
              CALL f_forma.setElementHidden("grp_det_sub_cta", 0) --Muestra el detalle de subcuentas 
              CALL f_forma.setElementHidden("grp_det_serv_mtd", 0) --Muestra el detalle de mandatos
              CALL DIALOG.setActionHidden("accept", 1)

              --Obtiene informacion de preliquidación               
              CALL fn_obtiene_informacion(v_folio_liquida,v_fecha_liquida)
              RETURNING r_arr_preliquidacion,
                        r_tot_pesos,
                        r_tot_acciones,
                        r_tot_ctas

              --Obtiene informacion de mandatos 
              CALL fn_obtiene_inf_mandatos(v_folio_liquida,v_fecha_liquida)
              RETURNING r_arr_mandatos,
                        v_indice_mdt,
                        v_tot_monto_pesos_mdt,
                        v_tot_total_cuentas_mdt

              DISPLAY r_tot_pesos    TO f_det_monto_pesos
              DISPLAY r_tot_acciones TO f_det_monto_acciones
              DISPLAY r_tot_ctas     TO f_det_tot_ctas
               
              DISPLAY v_tot_monto_pesos_mdt   TO f_man_monto_pesos
              DISPLAY v_tot_total_cuentas_mdt TO f_man_tot_ctas
           ELSE
              CALL fn_mensaje("Reverso Preliquidación",
                              "Información no existe ","about")
              LET v_folio_liquida = NULL 
              LET v_fecha_liquida = NULL 
              CONTINUE DIALOG
           END IF 
        
           ON ACTION reverso
              CALL fn_ventana_confirma("Dispersión de pagos", 
                                       "¿Desea ejecutar el reverso?", 
                                       "quest") 
              RETURNING r_respuesta

              IF r_respuesta = 1 THEN
                 --Se obtiene pid de la operación de acuerdo al folio
                 SELECT DISTINCT pid 
                 INTO g_pid
                 FROM bat_ctr_operacion
                 WHERE folio = v_folio_liquida

                 --Si no existe el pid de acuerdo al folio, traerá el último
                 IF g_pid IS NULL OR g_pid = 0 THEN
                    CALL fn_max_pid(g_proceso_cod, 1) RETURNING g_pid 
                 END IF  

                 CALL fn_valida_reverso(g_pid,g_proceso_cod,g_opera_cod)
                 RETURNING v_bnd_val_rev

                 DISPLAY "validación del reverso -- ",v_bnd_val_rev
                            
                 IF v_bnd_val_rev = 0 THEN
                    --Obtiene folio de pagos
                    {SELECT folio_referencia INTO v_folio_pagos
                    FROM glo_folio
                    WHERE folio = v_folio_liquida
                    --Ejecuta reverso instruccion mandatos
                    --Valores que regresa la función son tres:
                    -- 1.- Smallint determina si se ejecuto o no la instruccion,
                    --     valor 0 se ejecuto y 1 no se ejecuto
                    -- 2.- Smallint determina el diagnostico del reverso,
                    --     valor 0 correcto y 1 incorrecto
                    -- 3.- Decimal de nueve, numero de registros afectados
                    PREPARE prp_reversa_inst_mtd
                    FROM "EXECUTE FUNCTION safre_viv:fn_mdt_rev_aplica_inst(?,?)"
                    EXECUTE prp_reversa_inst_mtd USING v_folio_liquida,
                                                       v_folio_pagos
                                                  INTO r_rev_mtd_eje,
                                                       r_rev_mtd_dia,
                                                       r_rev_mtd_afe     

                    DISPLAY "Rev mandatos -- ", r_rev_mtd_eje

                    ----Eliminar cuando se active el proceso de mandatos.
                    LET r_rev_mtd_eje = 0
                  
                    IF r_rev_mtd_eje = 0 THEN
                       PREPARE prp_reversa_compensa_avance
                       FROM "EXECUTE PROCEDURE safre_viv:sp_dis_rev_compensa_avance(?)"
                       EXECUTE prp_reversa_compensa_avance USING v_folio_liquida
                  
                       WHENEVER ERROR CONTINUE 
                         CALL fn_reversa_preliquidacion(v_folio_liquida,
                                                        g_proceso_cod,g_opera_cod)
                         RETURNING v_b_rev_pre
                       WHENEVER ERROR STOP
                       IF SQLCA.sqlcode < 0 THEN
                          DISPLAY "Error en el reverso de preliquidación ",
                          SQLCA.sqlcode
                       END IF
                     
                       LET v_QryTxt = "\n UPDATE glo_folio",
                                      "\n    SET status = 0",
                                      "\n WHERE folio = ",v_folio_liquida,
                                      "\n   AND proceso_cod = ",g_proceso_cod,
                                      "\n   AND opera_cod   = 1"
                       DISPLAY "Reversa: \n", v_QryTxt
                                    
                       PREPARE prp_update_estatus_folio FROM v_QryTxt
                       EXECUTE prp_update_estatus_folio

                       CALL fn_reversa_operacion(g_pid,g_proceso_cod,g_opera_cod)
                       RETURNING v_bnd_rev_ope
                    END IF --Reversa mandatos

                    IF r_rev_mtd_eje = 1 THEN
                       CALL fn_mensaje("Reverso Preliquidación",
                                       "Folio de liquidación no es válido para el proceso de mandatos, no se puede realizar el reverso",
                                       "about")
                       NEXT FIELD f_folio
                    END IF}

                    LET v_QryTxt = " nohup time fglrun ",v_seg_modulo.ruta_bin CLIPPED,"/DISR021.42r ",
                                   p_usuario_cod CLIPPED, " ",
                                   g_pid  , " " ,
                                   g_proceso_cod , " " ,
                                   g_opera_cod ," ",
                                   v_folio_liquida ," '",
                                   "0" CLIPPED,"' ",
                                   --v_folio_dis_dae,
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
FUNCTION fn_movimiento_proceso(v_folio_liquida,v_fecha_liquida)
DEFINE 
  v_folio_liquida            LIKE dis_preliquida.folio_liquida, --Folio liquidado
  v_fecha_liquida            LIKE dis_preliquida.f_liquida,  --Fecha liquidación
  v_existe_reg               INTEGER, -- Contador si existen registros
  v_sql_txt                  STRING
       
  --Valida que existan registros para preliquidación
  LET v_sql_txt = "\n SELECT folio ",
                  "\n FROM   glo_folio ",
                  "\n WHERE  1 = 1 "
      
  IF length(v_folio_liquida) > 0 THEN
     --Construye la primera consulta
     LET v_sql_txt = v_sql_txt || "\n AND folio = '",v_folio_liquida,"'"   
  END IF
     
  IF length(v_fecha_liquida) > 0 THEN
     --Construye la primer consulta
     LET v_sql_txt = v_sql_txt || "\n AND f_actualiza = '",v_fecha_liquida,"'"
  END IF

  LET v_sql_txt = v_sql_txt || " GROUP BY 1 "
        
  --DISPLAY "Consulta que devuelve el folio \n", v_sql_txt
  PREPARE prp_busca_folio_preliquidado FROM v_sql_txt
  EXECUTE prp_busca_folio_preliquidado INTO v_existe_reg
     
  --Si hay registros para ejecutar, se filtra después por glo_folio
  IF v_existe_reg > 0 THEN       
     DISPLAY "Folio encontrado -- ", v_existe_reg
     --CALL fn_mensaje("Info","Folio econtrado -- " || v_existe_reg, "about")
  ELSE 
     DISPLAY "No existe folio en glo folio  "
     --CALL fn_mensaje("Info","No existen folios en dis_preliquida ", "about")
  END IF 

  RETURN v_existe_reg

END FUNCTION

#Objetivo: Función para obtener la información de la tabla cta_movimiento
FUNCTION fn_obtiene_informacion(v_folio_liquida,v_fecha_liquida)
DEFINE 
  v_folio_liquida            LIKE cta_movimiento.folio_liquida, --Folio liquidado
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,  --Fecha liquidación
  v_sql_txt                  STRING,
  
  r_arr_preliquidacion       DYNAMIC ARRAY OF RECORD
    v_subcuenta              CHAR(55),--LIKE dis_preliquida.subcuenta,
    v_fondo_inversion        CHAR(55),--LIKE dis_preliquida.fondo_inversion,
    v_monto_pesos            DECIMAL(22,2),
    v_monto_acciones         DECIMAL(26,6),
    v_total_cuentas          DECIMAL(12,0) 
  END RECORD,
  
  v_tot_monto_pesos          DECIMAL(22,2),
  v_tot_monto_acciones       DECIMAL(26,6),
  v_tot_total_cuentas        DECIMAL(12,0),
  v_indice                   INTEGER 

  {LET v_sql_txt= "\n SELECT dp.subcuenta ||'-'|| cs.subcuenta_desc AS SUBCTA,",
                 "\n        dp.fondo_inversion ||'-'|| cf.razon_social AS FONDO,",
                 "\n        sum(dp.monto_pesos),sum(dp.monto_acciones),count(dp.id_derechohabiente)",
                 "\n FROM   dis_preliquida dp, cat_subcuenta cs,cat_fondo_local cf",
                 "\n WHERE  dp.subcuenta       = cs.subcuenta",
                 "\n AND    dp.fondo_inversion = cf.fondo"
  IF length(v_folio_liquida) > 0 THEN
     LET v_sql_txt = v_sql_txt||"\n AND folio_liquida = ",v_folio_liquida
  END IF
  
  IF length(v_fecha_liquida) > 0 THEN
     LET v_sql_txt = v_sql_txt||"\n AND f_liquida = '",v_fecha_liquida,"'"
  END IF

  LET v_sql_txt = v_sql_txt||"\n GROUP BY 1,2"
  LET v_sql_txt = v_sql_txt||"\n ORDER BY 1,2"}

  LET v_sql_txt= "\n SELECT cm.subcuenta || '-' || ",
                 "\n        cs.subcuenta_desc AS SUBCUENTA, ",
                 "\n        cm.fondo_inversion || '-' || ",
                 "\n        cf.razon_social AS FONDO, ",
                 "\n        sum(cm.monto_pesos),",
                 "\n        sum(cm.monto_acciones), count(*)",
                 "\n FROM   dis_preliquida cm",
                 "\n JOIN   cat_subcuenta cs",
                 "\n   ON   cm.subcuenta       = cs.subcuenta",
                 "\n JOIN   cat_fondo_local cf",
                 "\n   ON   cm.fondo_inversion = cf.fondo",
                 "\n WHERE  1=1",
                 "\n AND    folio_liquida      = ",v_folio_liquida

  LET v_sql_txt = v_sql_txt||"\n GROUP BY 1,2"
  LET v_sql_txt = v_sql_txt||"\n ORDER BY 1,2"

  DISPLAY v_sql_txt
  PREPARE prp_sql_his_dispersion FROM v_sql_txt

  LET v_indice             = 1
  LET v_tot_monto_pesos    = 0.00
  LET v_tot_monto_acciones = 0.00
  LET v_tot_total_cuentas  = 0

  DECLARE cur_cta_movimiento CURSOR FOR prp_sql_his_dispersion
  FOREACH cur_cta_movimiento INTO r_arr_preliquidacion[v_indice].v_subcuenta,
                                  r_arr_preliquidacion[v_indice].v_fondo_inversion,
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

#OBJETIVO: Función para recuperar la información de Mandatos
FUNCTION fn_obtiene_inf_mandatos(p_folio,v_fecha_liquida)
DEFINE 
  p_folio                    LIKE cta_movimiento.folio_liquida,
  v_fecha_liquida            LIKE cta_movimiento.f_liquida,
  QryTxt                     STRING,
  v_indice_mdt               INTEGER
  
DEFINE 
  r_arr_mandatos             DYNAMIC ARRAY OF RECORD
  v_servicios                CHAR(55),
  v_monto_pesos              DECIMAL(22,2),
  v_total_cuentas            DECIMAL(12,0)
END RECORD

DEFINE 
  mtd_tot_monto_pesos        DECIMAL(22,2),
  mtd_tot_total_cuentas      INTEGER

  LET QryTxt = "   SELECT movimiento||'-'||origen as Servicio, ",
               "\n        SUM(monto_pesos), ",
               "\n        COUNT(id_derechohabiente)",
               "\n FROM   dis_preliquida",
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

  LET v_indice_mdt          = 1
  LET mtd_tot_monto_pesos   = 0.00
  LET mtd_tot_total_cuentas = 0

  DECLARE cur_cons_mdt CURSOR FOR prp_cons_mdt
  FOREACH cur_cons_mdt INTO r_arr_mandatos[v_indice_mdt].v_servicios,
                            r_arr_mandatos[v_indice_mdt].v_monto_pesos,
                            r_arr_mandatos[v_indice_mdt].v_total_cuentas
                                      
    LET mtd_tot_monto_pesos   = mtd_tot_monto_pesos   + 
                                r_arr_mandatos[v_indice_mdt].v_monto_pesos
    LET mtd_tot_total_cuentas = mtd_tot_total_cuentas + 
                                r_arr_mandatos[v_indice_mdt].v_total_cuentas
    LET v_indice_mdt          = v_indice_mdt + 1
  END FOREACH

  CALL r_arr_mandatos.deleteElement(v_indice_mdt)

  RETURN r_arr_mandatos,
         v_indice_mdt,
         mtd_tot_monto_pesos,
         mtd_tot_total_cuentas

END FUNCTION