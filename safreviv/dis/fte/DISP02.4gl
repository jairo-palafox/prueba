###############################################################################
#VERSION                    => 1.0.1                                           #
#FECHA ULTIMA MODIFICACION  => 05/09/2014                                      #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#MODULO           => DIS                                                       #
#PROGRAMA         => DISP02                                                    #
#OBJETIVO         => EJECUTAR EL CIERRE DE LOS AVANCES ABIERTOS CON PAGOS POR  #
#                    CUBRIR                                                    #
#FECHA DE INICIO  => 05/05/2014                                                #
################################################################################
DATABASE safre_viv
GLOBALS 
  DEFINE 
    p_folio_cie              DECIMAL(9,0),
    p_folio_ava              DECIMAL(9,0),
    p_f_actualiza            DATE,         
    p_usuario                CHAR(20),
    p_pid                    DECIMAL(9,0),
    p_proceso_cod            SMALLINT,
    p_opera_cod              SMALLINT,
    r_bandera                SMALLINT,
    p_nom_archivo            CHAR(40),
    p_programa               CHAR(10),
    g_folio                  DECIMAL(9,0),
    v_origen_datos           STRING,
    v_ruta_listados          STRING,
    v_ruta_ejecutable        STRING,
    p_b_despliegue_pantalla  SMALLINT,
    v_ruta_rep               STRING,
    manejador_rpt            om.SaxDocumentHandler,
    p_transaccion            SMALLINT,
    v_folio_regpag           DECIMAL(9,0)
END GLOBALS        

MAIN 
  LET p_usuario     = ARG_VAL(1) --Usuario que ejecuta el proceso
  LET p_pid         = ARG_VAL(2) --ID
  LET p_proceso_cod = ARG_VAL(3) --Código del proceso
  LET p_opera_cod   = ARG_VAL(4) --Código de la operación
  LET p_folio_ava   = ARG_VAL(5) --Valor del folio de Avance de Pagos
  LET p_folio_cie   = ARG_VAL(6) --Valor del folio de Cierre de Avances

  CALL STARTLOG(p_usuario CLIPPED||".DISP02.log")

  DISPLAY "Inicia proceso cierre de avances ", TIME
  DISPLAY ""
  CALL fn_ava_cierre()
  DISPLAY ""
  DISPLAY "Finaliza proceso cierre de avances ", TIME
END MAIN 

#OBJETIVO: Genera el proceso de Cierre de Avances Abiertos
FUNCTION fn_ava_cierre()
  DEFINE 
    bnd_continuar            SMALLINT,
    v_fecha_01mm             CHAR(11),
    p_pid                    DECIMAL(9,0),
    p_proceso_cod            SMALLINT,
    p_opera_cod              SMALLINT, 
    r_bandera                SMALLINT,
    r_bnd_opera_err          SMALLINT,
    r_bnd_preliquidacion     SMALLINT,  
    r_ruta_reporte           STRING, 
    v_QryTxt                 STRING,
    p_programa_cod           VARCHAR(10),
    v_status_err             SMALLINT,
    v_desc_err               VARCHAR(200),
    r_id_derechohabinete     DECIMAL(9,0),
    r_c_ruta_bin             LIKE seg_modulo.ruta_bin,     -- ruta del bin del módulo
    r_ruta_listados          LIKE seg_modulo.ruta_listados -- ruta de listados del módulo
       
  LET p_proceso_cod = 914 --Codigo del Proceso de Cierre de Avances (914)   
  LET p_opera_cod   = 1   --Codigo de la operacion de cierre de avances (1)

  CALL fn_max_pid(p_proceso_cod, p_opera_cod) RETURNING p_pid
   
  LET p_transaccion  = 0

  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT  ind_tipo_ejecucion 
  INTO    p_transaccion
  FROM    bat_ctr_operacion 
  WHERE   proceso_cod = p_proceso_cod   
  AND     pid         = p_pid
  AND     opera_cod   = p_opera_cod
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario)
     RETURNING p_folio_cie
  END IF 
       
  LET v_fecha_01mm = MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)
  IF p_usuario IS NULL THEN 
     LET p_usuario = "infonavit"
  END IF 
   
  --Deshabilita indices 
  CALL fn_desha_indx_tablas()

  DISPLAY "PROCESO CIERRA AVANCE (FOLIO AVANCE DE PAGOS): ", p_folio_ava
  DISPLAY ""
  --CIERRE AVANCE  

  --Ejecuta función para el cierre de los avances abiertos
  WHENEVER ERROR CONTINUE
    PREPARE prp_fn_transaccion
    FROM    "EXECUTE FUNCTION fn_dis_cierra_ava(?,?,?,?,?,?,?)"
    EXECUTE prp_fn_transaccion INTO r_bnd_preliquidacion, v_status_err, v_desc_err, r_id_derechohabinete
                              USING v_fecha_01mm,
                                    p_folio_ava,
                                    p_folio_cie,
                                    p_pid,
                                    p_proceso_cod,
                                    p_opera_cod,
                                    p_usuario
  WHENEVER ERROR STOP 
                                                               
  DISPLAY "Función Transaccion: ", r_bnd_preliquidacion
  DISPLAY "Código             : ", v_status_err
  DISPLAY "Id_derechohabiente : ", r_id_derechohabinete
  DISPLAY "Mensaje            : ", v_desc_err
  DISPLAY ""
         
  IF r_bnd_preliquidacion <> 0 THEN
     -- Función para finalizar la operación en error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bnd_opera_err
     DISPLAY "Error en la transacción ", v_status_err, " ", v_desc_err
     EXIT PROGRAM
  END IF
  --CIERRE AVANCE

  IF r_bnd_preliquidacion = 0 THEN      
     WHENEVER ERROR CONTINUE
       --Termina proceso de cierre de avances pero no ejecuto datos
       CALL fn_valida_cierre_ava(p_pid, p_proceso_cod, p_opera_cod, p_folio_cie)
       --Actualiza el status del folio de cierre de avances a liquidado
       LET v_QryTxt = " UPDATE glo_folio",
                      " SET    status      = 2",
                      " WHERE  folio       = ", p_folio_cie,
                      " AND    proceso_cod = ", p_proceso_cod
       PREPARE prp_actualiza_folio_cierre FROM v_QryTxt
       EXECUTE prp_actualiza_folio_cierre

       LET v_QryTxt = " UPDATE bat_ctr_proceso",
                      " SET    folio       = ", p_folio_cie,
                      " WHERE  pid         = ", p_pid,
                      " AND    proceso_cod = ", p_proceso_cod
       PREPARE prp_actualiza_ctr_proc FROM v_QryTxt
       EXECUTE prp_actualiza_ctr_proc

       --Ejecuta registro contable
       CALL fn_registro_contable_77()

       --Genera el archivo o interface a las diferencias positivas por avance (Cargo para HS)
       DISPLAY "\n### Generación de Interface ###"
       
       CALL fn_rutas("dis") 
       RETURNING r_c_ruta_bin, r_ruta_listados

       --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
       LET v_QryTxt = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS04.42r ", p_folio_cie, " ", 914
       RUN v_QryTxt

       DISPLAY ""

       --Generar el archivo o interface a las diferencias positivas por avance (Cargo para HS)
       LET v_QryTxt = "fglrun ",r_c_ruta_bin CLIPPED,"/DISS08.42r ", p_folio_cie, " ", 914
       RUN v_QryTxt
       
       DISPLAY ""
       
       --Se obtiene el codigo de programa
       SELECT programa_cod
       INTO   p_programa_cod
       FROM   cat_operacion
       WHERE  proceso_cod = p_proceso_cod
       AND    opera_cod   = p_opera_cod

       CALL fn_reporte_liquidacion(p_folio_cie, "cta_movimiento", p_usuario, p_pid,
                                   p_proceso_cod, p_opera_cod, p_programa_cod,
                                   FALSE)
       LET bnd_continuar = 1
  ELSE
     DISPLAY "Error en el cierre de avances ", r_bnd_preliquidacion
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bnd_opera_err
     EXIT PROGRAM
  END IF

  IF bnd_continuar THEN
     CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bandera

     IF r_bandera = 0 THEN 
        --Envío de correo de notificación de proceso finalizado
        CALL fn_correo_proceso(p_pid,
                               p_proceso_cod,
                               p_opera_cod,
                               r_ruta_reporte CLIPPED,
                               "Cierre Avance de Pagos ",
                               "\n ID Proceso   : "||p_pid||
                               "\n Proceso      : Cierre Avance de Pagos"||
                               "\n Operacion    : Cierre Avance de Pagos"||
                               "\n Fecha Inicio : "||TODAY||
                               "\n Fecha Fin    : "||TODAY)
     END IF
  END IF      
END FUNCTION

#Objetivo: Deshabilitar indices de la tabla dis_compensa_avance
FUNCTION fn_desha_indx_tablas()

  WHENEVER ERROR CONTINUE;
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_compensa_avance DISABLED;"
  WHENEVER ERROR STOP;

END FUNCTION

#Objetivo:Al termino del proceso de cierre de avances de pagos, valida si
#         encontro información relacionada en las tabla de ctam_movimiento     
#         con el folio de cierre de avances
FUNCTION fn_valida_cierre_ava(v_p_pid, v_p_proceso_cod, v_p_opera_cod, v_p_folio_cie)
DEFINE 
  v_bnd_cierre               DECIMAL(10,0),
  v_bnd_eror_opera           SMALLINT

DEFINE 
  v_p_pid                    LIKE bat_ctr_proceso.pid,
  v_p_proceso_cod            LIKE cat_proceso.proceso_cod,
  v_p_opera_cod              LIKE cat_operacion.opera_cod,
  v_p_folio_cie              DECIMAL(9,0)

  LET v_bnd_cierre = 0

  SELECT COUNT(*) 
  INTO   v_bnd_cierre
  FROM   dis_compensa_avance
  WHERE  folio_dis = v_p_folio_cie
  IF v_bnd_cierre <= 0 THEN
     DISPLAY ""
     DISPLAY "No se encontraron registros para cerrar avances con el folio seleccionado."
     DISPLAY ""
     
     CALL fn_error_opera(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
     RETURNING v_bnd_eror_opera

     EXIT PROGRAM
  ELSE
     DISPLAY ""
     DISPLAY "Se cerraron  ",v_bnd_cierre," avance(s)."
     DISPLAY ""
  END IF

END FUNCTION

FUNCTION fn_registro_contable_77()
  DEFINE 
    p_cve_proceso_cnt        SMALLINT,
    p_transaccion            SMALLINT,
    r_bnd_proceso_cnt        SMALLINT,
    v_fecha_reg              DATE,
    r_bnd_opera_error        SMALLINT,
    v_cuenta_contable        DECIMAL(10,0),
    v_revisa_res             SMALLINT,
    v_si_resultado           SMALLINT,
    v_cadena                 STRING

  LET p_cve_proceso_cnt = 77  --Cierre avance de pagos
  LET p_transaccion     = 0   --Se deberá registrar el monto del cierre de ava
  LET r_bnd_proceso_cnt = 0   --Bandera del stored del registro de avance de pagos
  LET v_fecha_reg       = TODAY

  --Se agrega función para realizar el registro contable ##
  PREPARE prp_reg_contable FROM "EXECUTE PROCEDURE fn_dis_cnt19(?,?,?,?,?)"
  EXECUTE prp_reg_contable USING p_folio_cie,
                                 v_fecha_reg,
                                 p_cve_proceso_cnt,
                                 p_proceso_cod,
                                 p_transaccion
                            INTO r_bnd_proceso_cnt
  IF SQLCA.sqlcode < 0 THEN
     DISPLAY "Código de ERROR SQL de registro contable: ",SQLCA.sqlcode
     -- Función para finalizar la operación en error
     CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod)
     RETURNING r_bnd_opera_error
     EXIT PROGRAM
  END IF

  IF r_bnd_proceso_cnt = 1 THEN
     SELECT COUNT (*)
     INTO   v_cuenta_contable
     FROM   cnt_transaccion
     WHERE  folio_liquida = p_folio_cie
     IF v_cuenta_contable > 0 THEN
        LET v_revisa_res = 0
        
        -- se verifica el cuadre de montos Cargo vs Abono
        LET v_cadena = "EXECUTE PROCEDURE fn_revisa_reg_cnt(?,?,?)"
        PREPARE sid_fn_revisa_proceso_cnt FROM v_cadena
        EXECUTE sid_fn_revisa_proceso_cnt USING p_folio_cie, --Folio de liquidación del proceso
                                                v_fecha_reg, --Fecha de liquidación del proceso
                                                v_revisa_res --Tipo de Registro Contable
                                           INTO v_si_resultado

        DISPLAY ""
        IF v_si_resultado = 0 THEN
           DISPLAY "El registro contable se realizó exitosamente."
        ELSE
           IF v_si_resultado = 1 THEN
              DISPLAY "Error: No existe monto de abono en el registro contable."
           END IF
           IF v_si_resultado = 2 THEN
              DISPLAY "Error: No existe monto de cargo en el registro contable."
           END IF
           IF v_si_resultado = 3 THEN
              DISPLAY "Error: Diferencia de montos abono - cargo en el registro contable."
           END IF           
        END IF
        DISPLAY ""

        {DISPLAY ""
        DISPLAY "El registro contable de cierre de avance de pagos se realizó exitosamente."
        DISPLAY ""}
     ELSE
        DISPLAY ""
        DISPLAY "Error: El registro contable no se realizó debidamente."
        DISPLAY ""
     END IF
  ELSE
     DISPLAY ""
     DISPLAY "Ocurrió un error al realizar el registro contable."
     DISPLAY ""
  END IF

END FUNCTION
