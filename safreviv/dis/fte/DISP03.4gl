###############################################################################
#VERSION                    => 1.0.0                                           #
#FECHA ULTIMA MODIFICACION  => 13/06/2019                                      #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#MODULO           => DIS                                                       #
#PROGRAMA         => DISP03                                                    #
#OBJETIVO         => EJECUTA LA FUNCIÓN DE LA DISPERSIÓN DE CRÉDITOS CEROS     #
#                    (SIN ADELANTO).                                           #
#FECHA DE INICIO  => 24/02/2015                                                #
################################################################################
DATABASE safre_viv
GLOBALS
  DEFINE 
    g_usuario                VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    g_arch_proceso           VARCHAR(100),
    g_qrytxt                 STRING                        --Prepara consultas

  DEFINE 
    g_folio_regpag           DECIMAL(9,0),
    g_folio_disp             DECIMAL(9,0),
    g_f_actualiza            DATE,         
    p_pid                    DECIMAL(9,0),
    g_proceso_cod_reg_pago   SMALLINT,
    g_folio_cons             DECIMAL(9,0)

  DEFINE 
    p_b_despliegue_pantalla  SMALLINT,
    v_ruta_rep               STRING,
    manejador_rpt            om.SaxDocumentHandler,
    v_origen_datos           STRING,
    v_ruta_listados          STRING,
    v_ruta_ejecutable        STRING
END GLOBALS

MAIN
  LET g_folio_disp           = ARG_VAL(1) --Folio de preliquidación
  LET g_folio_regpag         = ARG_VAL(2) --Folio de pagos
  LET g_proceso_cod_reg_pago = ARG_VAL(3) --Codigo de operación
  LET g_f_actualiza          = ARG_VAL(4) --Fecha de actualización
  LET g_usuario              = ARG_VAL(5) --Usuario que ejecuta el proceso
  LET g_folio_cons           = ARG_VAL(6) --Folio de la consulta

  CALL STARTLOG(g_usuario CLIPPED||".DISP03.log")

  DISPLAY "Inicia proceso preliquidación Créditos Cero ",TIME
  CALL fn_preliquida_cred_cero()
  DISPLAY "Finaliza proceso preliquidación Créditos Cero ",TIME
END MAIN

FUNCTION fn_preliquida_cred_cero()
  DEFINE 
    bnd_continuar            SMALLINT,
    v_fecha_01mm             CHAR(11),
    p_transaccion            SMALLINT,
    r_bandera                SMALLINT,
    r_bnd_opera_err          SMALLINT,
    r_bnd_preliquidacion     SMALLINT,  
    r_ruta_reporte           STRING, 
    v_QryTxt                 STRING,
    p_programa_cod           VARCHAR(10),
    v_status_err             SMALLINT,
    v_desc_err               VARCHAR(200),
    r_id_derechohabinete     DECIMAL(9,0)

  DEFINE 
    p_programa               CHAR(10),  
    r_nom_archivo            CHAR(40)

  DEFINE 
    r_bnd_proceso_cnt        SMALLINT,
    p_cve_proceso_cnt        SMALLINT,
    v_fecha_reg              DATE

  DEFINE p_folio             DECIMAL(9,0)
  DEFINE v_bandera           SMALLINT --Para verificar resultado de iniciar la operacion
  DEFINE v_comando           STRING
  DEFINE l_bat_ruta_listado  CHAR(40)
  DEFINE v_ruta_origen       CHAR(40)
  DEFINE v_desc_salida       VARCHAR(100)
  DEFINE v_mensaje           STRING
  DEFINE v_folio             LIKE glo_folio.folio
  DEFINE v_nombre_archivo    LIKE glo_ctr_archivo.nombre_archivo

  LET g_proceso_cod = 901 --Codigo del Proceso de Dispersion (4)(901)       
  LET g_opera_cod   = 1   --Codigo de la operacion de preliquidacion (1)

  CALL fn_max_pid(g_proceso_cod,g_opera_cod) RETURNING p_pid
   
  LET p_transaccion  = 0

  --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT  ind_tipo_ejecucion 
  INTO    p_transaccion
  FROM    bat_ctr_operacion 
  WHERE   proceso_cod = g_proceso_cod   
  AND     pid         = p_pid
  AND     opera_cod   = g_opera_cod
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,g_usuario)
     RETURNING g_folio_disp
  END IF 
       
  --LET p_pid = r_bandera
  LET v_fecha_01mm =  MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)
  IF g_usuario IS NULL THEN 
     LET g_usuario = "infonavit"
  END IF 

  CALL fn_inhabilita_indx()

  WHENEVER ERROR CONTINUE
    PREPARE prep_fn_dis_transaccion11
    FROM "EXECUTE PROCEDURE fn_dis_transaccion11(?, ?, ?, ?, ?, ?, ?, ?, ?)"

    DISPLAY "INICIO fn_dis_transaccion11: ", CURRENT HOUR TO SECOND
    EXECUTE prep_fn_dis_transaccion11 INTO r_bnd_preliquidacion, v_status_err, v_desc_err, r_id_derechohabinete
                                     USING v_fecha_01mm,
                                           g_usuario,
                                           g_folio_disp,
                                           p_pid,
                                           g_proceso_cod,
                                           g_opera_cod,
                                           g_folio_regpag,
                                           g_proceso_cod_reg_pago,
                                           g_folio_cons
  WHENEVER ERROR STOP 

  DISPLAY "FIN fn_dis_transaccion11: ", CURRENT HOUR TO SECOND

  --PRELIQUIDACION 
  --DISPLAY "r_bnd_preliquidacion: ", r_bnd_preliquidacion
  IF r_bnd_preliquidacion = 0 THEN      
     --Ejecuta SP para llenar la tabla de cortes por periodo y total de montos
     WHENEVER ERROR CONTINUE
        PREPARE prp_cortes_por_periodo
        FROM    "EXECUTE PROCEDURE sp_dis_cortes_por_periodo(?)"
        EXECUTE prp_cortes_por_periodo INTO r_bnd_preliquidacion, v_status_err, v_desc_err
                                      USING g_folio_disp

        --DISPLAY "Función Cortes: ",r_bnd_preliquidacion
        --DISPLAY "Código:",v_status_err
        --DISPLAY "mensaje", v_desc_err
      
        IF r_bnd_preliquidacion <> 0 THEN
           DISPLAY "Error al efectuar cortes por periodo: ",v_status_err," ",v_desc_err
        END IF

        --Termina proceso de preliquidación pero no ejecuto datos
        CALL fn_valida_preliquidacion_cred_cero(p_pid, g_proceso_cod, g_opera_cod)
      
        --Actualiza el status del folio de dispersión a Preliquidado 
        LET v_QryTxt = " UPDATE glo_folio",
                       " SET    status      = 1",
                       " WHERE  folio       = ",g_folio_disp,
                       " AND    proceso_cod = ",g_proceso_cod,
                       " AND    opera_cod   = 1"
                     
        PREPARE prep_actualiza_folio_preliquidado FROM v_QryTxt
        EXECUTE prep_actualiza_folio_preliquidado
      
        LET bnd_continuar = 1
  ELSE
     DISPLAY "Error en la instrucción mandatos ",r_bnd_preliquidacion
     CALL fn_error_opera(p_pid,g_proceso_cod,g_opera_cod)
     RETURNING r_bnd_opera_err
     EXIT PROGRAM
  END IF

  --DISPLAY "bnd_continuar: ",bnd_continuar
  IF bnd_continuar THEN
     CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bandera

     --DISPLAY "r_bandera: ", r_bandera
     IF r_bandera = 0 THEN 
        --CALL fn_obtiene_datos_rpt_cred_cero(g_folio_disp, p_pid, g_proceso_cod, g_opera_cod, g_usuario) 
        --RETURNING r_ruta_reporte
        -- Envío de correo de notificación de proceso finalizado
        CALL fn_correo_proceso(p_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               r_ruta_reporte CLIPPED,
                               "Preliquidación de Dispersión de Pagos Créditos en Ceros",
                               "\n ID Proceso   : "||p_pid||
                               "\n Proceso      : Dispersión de Créditos en Ceros"||
                               "\n Operacion    : Preliquidación de dispersión de pagos Créditos en Ceros"||
                               "\n Fecha Inicio : "||TODAY||
                               "\n Fecha Fin    : "||TODAY)
     END IF
    
     ------------------------------------------Inicia Operacion 2 (LIQUIDACION)
     LET g_proceso_cod = 901 -- dispersion
     LET g_opera_cod   = 2 -- liquidacion
     LET p_programa    = "DISS33"
     LET r_nom_archivo = ""

     INSERT INTO cta_movimiento
     SELECT *
     FROM dis_preliquida
     WHERE folio_liquida = g_folio_disp

     --Este proceso inicia por webservices, no tiene archivo
     LET v_nombre_archivo = "NA"
               
     SELECT ruta_listados
     INTO   l_bat_ruta_listado
     FROM   seg_modulo
     WHERE  modulo_cod = 'bat'
               
     SELECT ruta_bin
     INTO   v_ruta_origen
     FROM   seg_modulo
     WHERE  modulo_cod = 'dis'

     --Se verifica si se puede continuar con la operacion
     LET v_bandera = fn_valida_operacion(p_pid, g_proceso_cod, g_opera_cod)

     --DISPLAY "v_bandera: ", v_bandera
     IF ( v_bandera = 0 ) THEN
        {CALL fn_inicializa_proceso (p_pid, g_proceso_cod, g_opera_cod, g_folio_disp, 
                                    p_programa, r_nom_archivo, g_usuario)
        RETURNING r_bandera}

        LET r_bandera = 0
        --DISPLAY "r_bandera: ", r_bandera
        IF ( r_bandera = 0 ) THEN
           CALL fn_actualiza_opera_ini(p_pid,
                                       g_proceso_cod,
                                       g_opera_cod,
                                       g_folio_disp, --v_folio,
                                       "DISS33",
                                       "",
                                       g_usuario)
           RETURNING v_bandera
            
           --DISPLAY "r_bandera: ", r_bandera
           --DISPLAY "g_folio_disp: ",g_folio_disp
           --DISPLAY "v_bandera: ", v_bandera
            
           --DISPLAY "DESPUES EJECUTAR nohup DISS33"
           --LET v_mensaje = "Se ha enviado la interface DISS33.\nPuede revisar el avance del proceso en el monitor de ejecución de procesos"
           --LET v_mensaje = "\nSe ha enviado la interface DISS33.\n"
           --CALL fn_mensaje("Atención",v_mensaje,"stop")
           --DISPLAY v_mensaje

           LET p_cve_proceso_cnt = 19  --Cierre avance de pagos
           LET p_transaccion     = 0   --Se deberá registrar el monto del cierre de ava
           LET r_bnd_proceso_cnt = 0   --Bandera del stored del registro de avance de pagos
           LET v_fecha_reg       = TODAY
           --LET v_folio           = 0
      
           WHENEVER ERROR CONTINUE
             PREPARE prep_fn_dis_cnt19
             FROM "EXECUTE PROCEDURE fn_dis_cnt19(?, ?, ?, ?, ?)"

             EXECUTE prep_fn_dis_cnt19 USING g_folio_disp,
                                             v_fecha_reg,
                                             p_cve_proceso_cnt,
                                             g_proceso_cod,
                                             p_transaccion
                                        INTO r_bnd_proceso_cnt
           WHENEVER ERROR STOP 
            
           PREPARE prep_fn_revisa_reg_cnt
           FROM "EXECUTE PROCEDURE fn_revisa_reg_cnt(?, ?, ?)"
            
           EXECUTE prep_fn_revisa_reg_cnt USING g_folio_disp,
                                                v_fecha_reg,
                                                p_transaccion
                                           INTO r_bnd_proceso_cnt
            
           LET v_comando = "nohup fglrun ",v_ruta_origen CLIPPED,"/DISS33.42r ",
                           g_usuario CLIPPED, " ",
                           p_pid            , " ",
                           g_proceso_cod    , " ",
                           g_opera_cod      , " ",
                           g_folio_disp     , " ",
                           g_folio_cons     , " ",
                           r_bnd_proceso_cnt,
                           " 1>", l_bat_ruta_listado CLIPPED ,
                           "/nohup:",p_pid  USING "&&&&&",":",
                           g_proceso_cod    USING "&&&&&",":",
                           g_opera_cod      USING "&&&&&",
                           " 2>&1 &"
           RUN v_comando
        ELSE
           --Se obtiene la descripcion del parametro de salida
           SELECT descripcion
           INTO   v_desc_salida
           FROM   cat_bat_parametro_salida
           WHERE  cod_salida = v_bandera
         
           --Se construye el mensaje de error
           LET v_comando = "\nNo se puede iniciar la operación. No se ha enviado el proceso de liquidación.\nError: ", v_desc_salida CLIPPED,"\n"
           --CALL fn_mensaje("Atención",v_comando,"stop")
           DISPLAY v_comando
        END IF
     ELSE
        --No se puede ejecutar la operacion
        CALL fn_recupera_inconsis_opera(v_bandera) 
        RETURNING v_mensaje

        --CALL fn_mensaje("Atención", v_mensaje, "stop")
        DISPLAY v_mensaje
     END IF      

     CALL fn_actualiza_opera_fin(p_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bandera

     IF r_bandera = 0 THEN 
        -- se obtiene el codigo de programa
        SELECT programa_cod
        INTO   p_programa_cod
        FROM   cat_operacion
        WHERE  proceso_cod = g_proceso_cod
        AND    opera_cod   = g_opera_cod

        CALL fn_reporte_liquidacion(g_folio_disp, "dis_preliquida", g_usuario, p_pid,
                                    g_proceso_cod,g_opera_cod,p_programa_cod,
                                    FALSE)
         
        -- Envío de correo de notificación de proceso finalizado
        CALL fn_correo_proceso(p_pid,
                               g_proceso_cod,
                               g_opera_cod,
                               r_ruta_reporte CLIPPED,
                               "Preliquidación de Dispersión de Pagos",
                               "\n ID Proceso   : "||p_pid||
                               "\n Proceso      : Dispersión de Pagos"||
                               "\n Operacion    : Preliquidación de dispersión de pagos"||
                               "\n Fecha Inicio : "||TODAY||
                               "\n Fecha Fin    : "||TODAY)
     END IF
  END IF
END FUNCTION

#Objetivo: Borrar y crear sobre la base de datos las estructuras de las tablas
#          dis_preliquida, dis_interface_hs y dis_interface_ef_ad
FUNCTION fn_inhabilita_indx()
  WHENEVER ERROR CONTINUE;
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_interface_ef_ad DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_compensa_avance DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_tramite DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_ceros DISABLED;"
  WHENEVER ERROR STOP;
END FUNCTION

#Objetivo:Al termino del proceso de preliquidación de dispersión, valida si
#         en contro información relacionada en las tablas de pag_det_trabajador
#         con el folio de registro de pagos, derechohabiente y numero de credito
#         de cta_credito
FUNCTION fn_valida_preliquidacion_cred_cero(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
  DEFINE 
    v_bnd_preliquidacion     DECIMAL(10,0),
    v_bnd_eror_opera         SMALLINT

  DEFINE 
    v_p_pid                  LIKE bat_ctr_proceso.pid,
    v_p_proceso_cod          LIKE cat_proceso.proceso_cod,
    v_p_opera_cod            LIKE cat_operacion.opera_cod

  LET v_bnd_preliquidacion = 0

  SELECT COUNT(*) 
  INTO   v_bnd_preliquidacion
  FROM   dis_preliquida

  IF v_bnd_preliquidacion <= 0 THEN
     DISPLAY "\nNo se encontraron registros a preliquidar con el folio de registro de pagos seleccionado \n"
      
     CALL fn_error_opera(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
     RETURNING v_bnd_eror_opera

     EXIT PROGRAM
  ELSE
     DISPLAY "\Se preliquidaron ",v_bnd_preliquidacion," registros.\n"
  END IF
END FUNCTION