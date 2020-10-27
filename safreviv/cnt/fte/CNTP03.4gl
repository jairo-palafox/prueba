################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 14/02/2018                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CNT                                                      #
#Programa          => CNTP03                                                   #
#Objetivo          => Permitir realizar el registro contable del nuevo proceso #
#                     de Ajuste de Saldo (2116).                               #
#Fecha inicio      => 09/10/2015                                               #
################################################################################
DATABASE safre_viv

GLOBALS
  DEFINE g_sql_txt           STRING,      --Consultas
         g_usuario           VARCHAR(30), --Almacena al usuario
         g_proceso_cod       LIKE cat_proceso.proceso_cod, --codigo del proceso
         g_opera_cod         LIKE cat_operacion.opera_cod, --codigo de operacion
         g_pid               DECIMAL(9,0),
         g_folio             DECIMAL(9,0),
         opera_cod           INTEGER 
   
  DEFINE v_mensaje           STRING
         
  DEFINE v_indice            SMALLINT
  DEFINE g_folio             LIKE dis_det_avance_pago.folio
   
  DEFINE v_arr_preliq        DYNAMIC ARRAY OF RECORD
    id                       INTEGER,
    id_subcuenta             SMALLINT, 
    subcuenta                CHAR(50),
    id_movimiento            SMALLINT,      
    movimiento               CHAR(50),
    monto_pesos              DECIMAL(12,2), 
    f_liquida                DATE,
    buscar                   INTEGER,
    cuenta                   CHAR(10), 
    desc_cuenta              CHAR(55), 
    estado                   CHAR(35)
  END RECORD

END GLOBALS

MAIN
  DEFINE 
    v_tipo_proceso           SMALLINT,    --Forma como ejecutara el programa 
    v_nom_prog               VARCHAR(30), --Almacena opción del menú 
    v_nom_archivo            STRING       --Nombre del archivo          
    
  --Datos de entrada
  DEFINE p_folio_liquida     DECIMAL(9,0)  -- Folio  

  --Datos para uso en funciones
  DEFINE v_estado            SMALLINT   -- Estado
  DEFINE v_desc_estado_cnt   CHAR(25)   -- Descripción del estado

  DEFINE v_subcuenta_desc    CHAR(40)
  DEFINE v_movimiento_desc   CHAR(40)

  DEFINE i                   INTEGER

  DEFINE v_cue_cont          CHAR(10)
  DEFINE v_des_cue_cont      CHAR(55)

  DEFINE 
    bnd_consulta             SMALLINT, 
    f_ventana                ui.Window, --Define las propìedades de la Ventana
    f_forma                  ui.Form    --Define las propiedades de la forma

  DEFINE v_bnd_existe_info   INTEGER

  DEFINE v_id                INTEGER
  DEFINE v_bnd_ok            SMALLINT
  DEFINE cod_proceso_cnt     SMALLINT
  DEFINE cod_transaccion_cnt SMALLINT
  DEFINE tpo_ajuste          SMALLINT

  DEFINE 
    r_bnd                    INTEGER, 
    v_status_err             INTEGER,
    v_desc_err               VARCHAR(200)

  DEFINE v_count_prel        INTEGER

  DEFINE p_pid               DECIMAL(9,0)  
  DEFINE r_nom_archivo       CHAR(40)
  DEFINE r_bandera           SMALLINT
  DEFINE p_programa          CHAR(10)  

  DEFINE v_count             INTEGER

  CALL STARTLOG (g_usuario CLIPPED|| ".CNTP03.log")
          
  --Recibe valores de argumentos
  LET p_programa     = "CNTP03"
  LET g_usuario      = ARG_VAL(1)
  LET v_tipo_proceso = ARG_VAL(2)
  LET v_nom_prog     = ARG_VAL(3)

  LET g_proceso_cod       = 2116
  LET cod_proceso_cnt     = 93
  LET cod_transaccion_cnt = 105
  LET g_opera_cod         = 4
  LET tpo_ajuste          = 0 

  LET v_bnd_existe_info   = 0

  --se asigna el titulo del programa
  IF ( v_nom_prog IS NOT NULL ) THEN
     CALL ui.Interface.setText(v_nom_prog)
  END IF

  LET v_nom_archivo = ""   
  LET bnd_consulta  = 0   

  CLOSE WINDOW SCREEN

  OPEN WINDOW w1 WITH FORM "CNTP031"
    DIALOG ATTRIBUTES(UNBUFFERED)
      INPUT BY NAME p_folio_liquida                    
        BEFORE INPUT
          LET f_ventana = ui.Window.getCurrent()
          LET f_forma   = f_ventana.getForm()                    
          -- NEXT FIELD p_folio_liquida
          --CALL ui.interface.refresh()    
            
        ON ACTION ACCEPT 
           CALL DIALOG.setFieldActive( "p_folio_liquida", FALSE)
           --DISPLAY "p_folio_liquida -",p_folio_liquida,"-"
            
           LET v_bnd_existe_info = 0
           CALL fn_verificar_info_disp(p_folio_liquida) 
           RETURNING v_bnd_existe_info

           --DISPLAY "v_bnd_existe_info: ",v_bnd_existe_info
           IF v_bnd_existe_info >= 1 THEN
              WHENEVER ERROR CONTINUE 
                PREPARE ps_sp_dup FROM "EXECUTE PROCEDURE fn_dup_ajuste_preliq(?)"  
                EXECUTE ps_sp_dup USING p_folio_liquida
                                   INTO v_estado, v_status_err, v_desc_err
              WHENEVER ERROR STOP 

              WHENEVER ERROR CONTINUE 
                PREPARE ps_sp FROM "EXECUTE PROCEDURE fn_val_ajuste_sdo_cnt(?,?)"  
                EXECUTE ps_sp USING p_folio_liquida, g_proceso_cod
                               INTO v_estado, v_status_err, v_desc_err
              WHENEVER ERROR STOP 

              --DISPLAY "v_estado -",v_estado,"-"
              IF v_estado = 30 THEN
                 CALL fn_mensaje("Aviso","El Folio capturado ya fue confirmado en SAP – FICO.","stop")
                 EXIT PROGRAM
              END IF

              IF v_estado <> 0 AND v_estado <> 10 AND v_estado <> 30 THEN               
                 CALL fn_mensaje("Aviso","El Folio capturado está en proceso de confirmación.","stop")
                 EXIT PROGRAM
              END IF
               
              IF v_estado = 10 THEN                 
                 LET g_sql_txt = "\n SELECT cta.id_cuenta_contable, ", 
                                 "\n        cta.subcuenta, ",
                                 "\n        cs.subcuenta_desc, ",
                                 "\n        cta.movimiento, ",
                                 "\n        cm.desc_naturaleza_mov, ", 
                                 "\n        cta.monto_pesos, ",
                                 "\n        cta.f_liquida, ",
                                 "\n        cta.cta_contable, ",
                                 "\n        cc.desc_cta_contable, ",
                                 "\n        '10 - REGISTRADO' ",  
                                 "\n FROM   cnt_trn_aj_sdo cta, ",
                                 "\n        cat_subcuenta cs, ",
                                 "\n        cat_naturaleza_mov cm, ",
                                 "\n        cat_cuenta_contable cc ",
                                 "\n WHERE  cta.subcuenta     = cs.subcuenta ",
                                 "\n AND    cta.movimiento    = cm.cod_naturaleza_mov ",
                                 "\n AND    cta.cta_contable  = cc.cta_contable ",
                                 "\n AND    cta.folio_liquida = ",p_folio_liquida, 
                                 "\n ORDER BY cta.id_cuenta_contable "

                 --DISPLAY "g_sql_txt -",g_sql_txt,"-"  
                 PREPARE ps_trn FROM g_sql_txt   
                 DECLARE cur_trn CURSOR FOR ps_trn 

                 LET i = 1
                 CALL v_arr_preliq.clear()

                 FOREACH cur_trn INTO v_arr_preliq[i].id, 
                                      v_arr_preliq[i].id_subcuenta,
                                      v_arr_preliq[i].subcuenta,
                                      v_arr_preliq[i].id_movimiento,
                                      v_arr_preliq[i].movimiento,
                                      v_arr_preliq[i].monto_pesos,
                                      v_arr_preliq[i].f_liquida,
                                      v_arr_preliq[i].cuenta,
                                      v_arr_preliq[i].desc_cuenta, 
                                      v_arr_preliq[i].estado

                   LET v_arr_preliq[i].subcuenta  = v_arr_preliq[i].id_subcuenta,' - ',v_arr_preliq[i].subcuenta
                   LET v_arr_preliq[i].movimiento = v_arr_preliq[i].id_movimiento,' - ',v_arr_preliq[i].movimiento
                     
                   LET i = i + 1
                 END FOREACH

                 LET i = i - 1   
                 CALL v_arr_preliq.deleteElement(v_arr_preliq.getLength()) 

                 CLOSE cur_trn
                 FREE cur_trn
              END IF

              IF v_estado = 0 THEN
                 LET g_sql_txt = "\n SELECT cp.subcuenta, ", 
                                 "\n        cs.subcuenta_desc, ",  
                                 "\n        cp.movimiento, ",
                                 "\n        cm.desc_naturaleza_mov, ",
                                 "\n        SUM(monto_pesos) AS monto_pesos, ", 
                                 "\n        cp.f_liquida ",
                                 "\n FROM   tmp_cbd_pre_ajuste_saldo cp, ", 
                                 "\n        cat_subcuenta cs, ",
                                 "\n        cat_naturaleza_mov cm ", 
                                 "\n WHERE  cp.subcuenta  = cs.subcuenta ", 
                                 "\n AND    cp.movimiento = cm.cod_naturaleza_mov ", 
                                 "\n AND    cp.folio_liquida = ",p_folio_liquida, 
                                 "\n GROUP BY cp.subcuenta, ", 
                                 "\n          cs.subcuenta_desc, ", 
                                 "\n          cp.movimiento, ",
                                 "\n          cm.desc_naturaleza_mov, ",
                                 "\n          cp.f_liquida ",
                                 "\n ORDER BY cp.subcuenta, ",
                                 "\n          cp.movimiento "

                 --DISPLAY "g_sql_txt: -",g_sql_txt
                 PREPARE ps_inf_preliq FROM g_sql_txt   
                 DECLARE cur_inf_preliq CURSOR FOR ps_inf_preliq 

                 LET i = 1

                 SELECT desc_estado_cnt 
                 INTO   v_desc_estado_cnt
                 FROM   cat_estado_cnt
                 WHERE  cod_estado_cnt = v_estado

                 FOREACH cur_inf_preliq INTO v_arr_preliq[i].id_subcuenta, 
                                             v_subcuenta_desc,
                                             v_arr_preliq[i].id_movimiento,
                                             v_movimiento_desc,
                                             v_arr_preliq[i].monto_pesos, 
                                             v_arr_preliq[i].f_liquida

                   LET v_arr_preliq[i].id         = i                
                   LET v_arr_preliq[i].subcuenta  = v_arr_preliq[i].id_subcuenta CLIPPED,' - ',v_subcuenta_desc CLIPPED
                   LET v_arr_preliq[i].movimiento = v_arr_preliq[i].id_movimiento CLIPPED,' - ',v_movimiento_desc CLIPPED                    
                   LET v_arr_preliq[i].estado     = v_estado,' - ',v_desc_estado_cnt
                     
                   LET i = i + 1
                 END FOREACH

                 LET i = i - 1   
                 CALL v_arr_preliq.deleteElement(v_arr_preliq.getLength()) 

                 CLOSE cur_inf_preliq
                 FREE cur_inf_preliq
              END IF
           ELSE
              CALL fn_mensaje("Aviso","El Folio capturado no se encuentra preliquidado.","stop") 
              EXIT PROGRAM
           END IF  
      END INPUT

      INPUT ARRAY v_arr_preliq FROM r_resultado.* 
        ATTRIBUTES(WITHOUT DEFAULTS, APPEND ROW = FALSE, INSERT ROW = FALSE, DELETE ROW = FALSE)
          ON ACTION consulta_cuenta     
             --DISPLAY "v_arr_preliq[ARR_CURR()].id: -",v_arr_preliq[ARR_CURR()].id,"-" 
             LET v_id = v_arr_preliq[ARR_CURR()].id
             CALL fn_consulta_cuenta() RETURNING v_cue_cont, v_des_cue_cont
               
             LET v_arr_preliq[v_id].cuenta      = v_cue_cont
             LET v_arr_preliq[v_id].desc_cuenta = v_des_cue_cont

             --DISPLAY "v_arr_preliq[v_id].*: -",v_arr_preliq[v_id].*,"-"
               
             LET f_ventana = ui.Window.getCurrent()
             LET f_forma   = f_ventana.getForm()
             CALL ui.Interface.refresh()

          ON ACTION Aceptar
             CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING p_pid
             DISPLAY "p_pid -",p_pid,"-"
             DISPLAY "g_proceso_cod -",g_proceso_cod,"-"
             DISPLAY "g_opera_cod -",g_opera_cod,"-"
             LET r_bandera = fn_valida_operacion(p_pid, g_proceso_cod, g_opera_cod)
             
             DISPLAY "r_bandera -",r_bandera 
             IF (r_bandera <> 0 ) THEN
                CALL fn_mensaje("Aviso","Este folio ya ha sido confirmado.","stop")
                EXIT PROGRAM                  
             END IF
             
             CALL fn_ver_cue_con() RETURNING v_bnd_ok
             IF v_bnd_ok = 1 THEN
                CALL fn_mensaje("Aviso","Falta asignar la cuenta contable a uno o más registros.","stop") 
             ELSE                
                FOR i = 1 TO v_arr_preliq.getLength()                   
                    {DISPLAY "\n v_arr_preliq[i].id: -",v_arr_preliq[i].id,"-",
                             "\n p_folio_liquida: -",p_folio_liquida,"-",
                             "\n v_arr_preliq[i].f_liquida: -",v_arr_preliq[i].f_liquida,"-",
                             "\n v_arr_preliq[i].id_subcuenta: -",v_arr_preliq[i].id_subcuenta,"-", 
                             "\n v_arr_preliq[i].id_movimiento: -",v_arr_preliq[i].id_movimiento,"-",
                             "\n v_arr_preliq[i].cuenta: -",v_arr_preliq[i].cuenta,"-",
                             "\n v_arr_preliq[i].monto_pesos: -",v_arr_preliq[i].monto_pesos,"-", 
                             "\n cod_proceso_cnt: -",cod_proceso_cnt,"-",
                             "\n g_proceso_cod: -",g_proceso_cod,"-",
                             "\n cod_transaccion_cnt: -",cod_transaccion_cnt,"-",
                             "\n tpo_ajuste: -",tpo_ajuste,"-"}

                    LET r_bnd        = 0; 
                    LET v_status_err = 0; 
                    LET v_desc_err   = "";

                    IF v_arr_preliq[i].monto_pesos < 0 THEN
                       LET v_arr_preliq[i].monto_pesos = v_arr_preliq[i].monto_pesos * -1
                    END IF
                   
                    IF v_estado = 10 THEN                   
                       LET tpo_ajuste = 1;
                    END IF

                    DISPLAY "-----> tpo_ajuste -",tpo_ajuste,"-"
                    WHENEVER ERROR CONTINUE 
                      PREPARE ps_sp_ajuste FROM "EXECUTE PROCEDURE fn_ajuste_sdo_cnt (?,?,?,?,?,?,?,?,?,?,?)"  
                      EXECUTE ps_sp_ajuste USING v_arr_preliq[i].id,
                                                 p_folio_liquida,
                                                 v_arr_preliq[i].f_liquida,
                                                 v_arr_preliq[i].id_subcuenta, 
                                                 v_arr_preliq[i].id_movimiento,
                                                 v_arr_preliq[i].cuenta,
                                                 v_arr_preliq[i].monto_pesos, 
                                                 cod_proceso_cnt,
                                                 g_proceso_cod,
                                                 cod_transaccion_cnt,
                                                 tpo_ajuste
                                            INTO r_bnd, v_status_err, v_desc_err

                      DISPLAY r_bnd, v_status_err, v_desc_err
                    WHENEVER ERROR STOP                   
                   
                    IF r_bnd <> 0 AND v_estado = 0 THEN
                       CALL fn_mensaje("Aviso","No pudo aplicarse el registro contable.","stop")
                       EXIT PROGRAM                  
                    END IF
                    
                    IF r_bnd <> 0 AND v_estado = 10 THEN
                       CALL fn_mensaje("Aviso","No pudo aplicarse la modificación del registro contable.","stop")
                       EXIT PROGRAM
                    END IF                                
                END FOR
                
                IF r_bnd = 0 AND (v_estado = 10) THEN
                   CALL fn_mensaje("Aviso","Se aplicó exitosamente la modificación del registro contable.","stop") 
                END IF

                DISPLAY "r_bnd -",r_bnd,"-"
                DISPLAY "v_estado -",v_estado,"-"
                IF r_bnd    = 0 AND 
                   v_estado = 0 THEN
                   LET v_estado = 10
                   DISPLAY "v_estado -",v_estado,"-"
                   FOR i = 1 TO v_arr_preliq.getLength()
                       LET v_arr_preliq[i].estado = '10 - REGISTRADO'
                   END FOR
                   --CALL ui.Interface.refresh()
                END IF
    
                LET r_bnd = 0

                WHENEVER ERROR CONTINUE 
                  PREPARE ps_sp_rev FROM "EXECUTE PROCEDURE fn_revisa_reg_cnt (?,?,?)"  
                  EXECUTE ps_sp_rev USING p_folio_liquida, v_arr_preliq[1].f_liquida, tpo_ajuste
                                     INTO r_bnd
                WHENEVER ERROR STOP

                LET v_count_prel = 0
                   
                LET g_sql_txt = "\n SELECT COUNT(*) ",
                                "\n FROM   cnt_transaccion ",
                                "\n WHERE  folio_liquida = ?"
                PREPARE ps_sp_prel FROM g_sql_txt
                EXECUTE ps_sp_prel USING p_folio_liquida
                                    INTO v_count_prel

                --DISPLAY "\n v_count_prel: -",v_count_prel,"-" 
                --DISPLAY "\n v_arr_preliq.getLength(): -",v_arr_preliq.getLength(),"-"

                IF v_count_prel <> v_arr_preliq.getLength() THEN
                   CALL fn_mensaje("Aviso","La información de la Pre Liquidación no se encuentra registrada.","stop")
                   EXIT PROGRAM 
                END IF
             END IF

          ON ACTION Confirmar
             LET v_count = 0
             
             SELECT COUNT(*) 
             INTO v_count 
             FROM cnt_trn_aj_sdo 
             WHERE folio_liquida = p_folio_liquida
             
             IF v_count > 0 THEN
                CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING p_pid

                LET r_bandera = fn_valida_operacion(p_pid, g_proceso_cod, g_opera_cod)             
             
                IF (r_bandera = 0 ) THEN
                   -- Inicia la operación asignando el estatus de PROCESANDO
                   CALL fn_actualiza_opera_ini(p_pid, g_proceso_cod, g_opera_cod, p_folio_liquida, 
                                               p_programa, r_nom_archivo, g_usuario)
                   RETURNING r_bandera

                   -- DISPLAY "r_bandera 2-",r_bandera,"-"

                   CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, 4)
                   RETURNING r_bandera
   
                   --DISPLAY "r_bandera 3-",r_bandera,"-"
                END IF

                IF r_bandera = 0 THEN
                   CALL fn_mensaje("Aviso","El proceso ha sido confirmado.","stop")
                   EXIT PROGRAM
                ELSE 
                   CALL fn_mensaje("Aviso","Este folio ya ha sido confirmado.","stop")
                   EXIT PROGRAM
                END IF
             END IF
          END INPUT      
      
      ON ACTION cancelar
         EXIT DIALOG
          
    END DIALOG 
  CLOSE WINDOW w1 

END MAIN 

FUNCTION fn_verificar_info_disp(p_folio) 
  DEFINE p_folio             DECIMAL(9,0)
  DEFINE v_tot_reg           INTEGER

  LET v_tot_reg = 0

  LET g_sql_txt = "\n SELECT COUNT(folio) ",
                  "\n FROM   glo_folio ",
                  "\n WHERE  proceso_cod = 2116 ",
                  "\n AND    status      = 1 ",
                  "\n AND    folio       = ? "

  -- DISPLAY "g_sql_txt: -",g_sql_txt
  PREPARE ps_existe_info FROM g_sql_txt   
  EXECUTE ps_existe_info USING p_folio INTO v_tot_reg

  RETURN v_tot_reg
END FUNCTION

FUNCTION fn_consulta_cuenta()
  DEFINE i                   INTEGER

  DEFINE v_arr_cta           DYNAMIC ARRAY OF RECORD
    cuenta                   CHAR(10),
    desc_cuenta              CHAR(55)
  END RECORD 

  DEFINE v_cue_cont          CHAR(10)
  DEFINE v_des_cue_cont      CHAR(55)

  OPEN WINDOW w2 WITH FORM "CNTP032"
    DIALOG ATTRIBUTES(UNBUFFERED)
      DISPLAY ARRAY v_arr_cta TO r_cuentas.*                       
        BEFORE DISPLAY
          LET g_sql_txt = "\n SELECT cta_contable, desc_cta_contable ", 
                          "\n FROM   cat_cuenta_contable ",
                          "\n ORDER BY cta_contable "   

          --DISPLAY "g_sql_txt: -",g_sql_txt
          PREPARE ps_inf_cuenta FROM g_sql_txt   
          DECLARE cur_inf_cuenta CURSOR FOR ps_inf_cuenta 
            LET i = 1
          FOREACH cur_inf_cuenta INTO v_arr_cta[i].cuenta, 
                                      v_arr_cta[i].desc_cuenta
            LET i = i + 1
          END FOREACH

          LET i = i - 1   
          CALL v_arr_cta.deleteElement(v_arr_cta.getLength()) 

          CLOSE cur_inf_cuenta
          FREE cur_inf_cuenta

        ON ACTION ACCEPT
           LET v_cue_cont     = v_arr_cta[ARR_CURR()].cuenta
           LET v_des_cue_cont = v_arr_cta[ARR_CURR()].desc_cuenta

           --DISPLAY "v_cue_cont: -",v_cue_cont,"-"
           --DISPLAY "v_des_cue_cont: -",v_des_cue_cont,"-"

           EXIT DIALOG 
      END DISPLAY
    END DIALOG
  CLOSE WINDOW w2

  RETURN v_cue_cont, v_des_cue_cont 
END FUNCTION

FUNCTION fn_ver_cue_con()
  DEFINE v_bnd_ok            SMALLINT
  DEFINE i                   SMALLINT

  LET v_bnd_ok = 0

  FOR i = 1 TO v_arr_preliq.getLength()
      IF v_arr_preliq[i].cuenta IS NULL OR v_arr_preliq[i].cuenta CLIPPED = "" THEN
         LET v_bnd_ok = 1
         EXIT FOR
      END IF
  END FOR

  RETURN v_bnd_ok
END FUNCTION