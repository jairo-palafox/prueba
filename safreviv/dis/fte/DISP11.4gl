################################################################################
#VERSION                    => 1.0                                             #
#FECHA ULTIMA MODIFICACION  => 03/06/2019                                      #
################################################################################
################################################################################
#PROYECTO          => SAFRE VIVIENDA                                           #
#PROPIETARIO       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#MODULO           => DIS                                                       #
#PROGRAMA         => DISP11                                                    #
#OBJETIVO         => RECUPERAR LA INFORMACION DE LIQUIDACIÓN ESPECIAL DE LA    #
#                    DISPERSIÓN DE PAGOS DE SALIDAS DE ACLARATORIOS PARA       #
#                    MONTOS DE AMORTIZACIONES.                                 #
#FECHA DE INICIO  => 21/06/2017                                                #
################################################################################
--------------------------------------------------------------------------------
-- Modificado   => GERARDO ALFONSO VEGA PAREDES
-- Fecha        => 1-AGO-2017
-- Descripción  => Agregar liquidación, registro contable e interfaces
-- Clave cambio => --PROINFXVII-17-GVP 
--------------------------------------------------------------------------------
DATABASE safre_viv

GLOBALS 
  DEFINE 
    p_folio_regpag           DECIMAL(9,0),
    p_folio_disp             DECIMAL(9,0),
    p_f_actualiza            DATE,         
    p_usuario                CHAR(20),
    p_pid                    DECIMAL(9,0),
    p_proceso_cod            SMALLINT,
    p_proceso_cod_reg_pago   SMALLINT,
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
   LET p_folio_disp           = ARG_VAL(1) --Folio de preliquidación
   LET p_folio_regpag         = ARG_VAL(2) --Folio de pagos
   LET p_proceso_cod_reg_pago = ARG_VAL(3) --Codigo de operación
   LET p_f_actualiza          = ARG_VAL(4) --Fecha de actualización
   LET p_usuario              = ARG_VAL(5) --Usuario que ejecuta el proceso
      
   CALL STARTLOG(p_usuario CLIPPED||".DISP11.log")

   DISPLAY "Inicia proceso Liquidación Amo Acl Dest Incorr ",TIME
   CALL fn_dis_preliquida()
   DISPLAY "Finaliza proceso Liquidación Amo Acl Dest Incorr ",TIME

END MAIN
 
#OBJETIVO: Genera el proceso de Preliquidacion, de Aportaciones y Amortizaciones
FUNCTION fn_dis_preliquida()
DEFINE 
   bnd_continuar              SMALLINT,
   v_fecha_01mm               CHAR(11),
   p_pid                      DECIMAL(9,0),
   p_proceso_cod              SMALLINT,
   p_opera_cod                SMALLINT, 
   r_bandera                  SMALLINT,
   l_comando                  STRING,
   r_bnd_opera_err            SMALLINT,
   r_bnd_preliquidacion       SMALLINT,  
   r_ruta_reporte             STRING, 
   v_QryTxt                   STRING,
   p_programa_cod             VARCHAR(10),
   v_status_err               SMALLINT,
   v_desc_err                 VARCHAR(200),
   r_id_derechohabinete       DECIMAL(9,0),
   v_ind_masivo               SMALLINT,
   v_periodo_pago             CHAR(6),
   v_periodo_bimestre         CHAR(6), 
   v_ruta_list                CHAR(30), 
   v_ruta_ejec                CHAR(30)

DEFINE v_bnd_preliq_ok        SMALLINT
       
   LET p_proceso_cod = 901 --Codigo del Proceso de Dispersion (4)(901)       
   LET p_opera_cod   = 1   --Codigo de la operacion de preliquidacion (1)

   CALL fn_max_pid(p_proceso_cod,p_opera_cod) 
   RETURNING p_pid
   
   LET p_transaccion  = 0

   --Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
   SELECT  ind_tipo_ejecucion 
   INTO    p_transaccion
   FROM    bat_ctr_operacion 
   WHERE   proceso_cod = p_proceso_cod   
   AND     pid         = p_pid
   AND     opera_cod   = p_opera_cod  
   IF p_transaccion = 1 THEN 
      CALL fn_genera_folio(p_proceso_cod, p_opera_cod,p_usuario)
      RETURNING p_folio_disp
   END IF 
       
   LET v_fecha_01mm =  MONTH(TODAY) USING "&&"||"/"||"01"||"/"||YEAR(TODAY)

   IF p_usuario IS NULL THEN 
      LET p_usuario = "infonavit"
   END IF 
   
   --Borra y crea tablas dis_preliquida,dis_interfce_hs y dis_interface_ef_ad
   CALL fn_borra_crea_tablas()

   DISPLAY "PROCESO REG PAGOS: ", p_proceso_cod_reg_pago
   --PRELIQUIDACION 
  
   --Ejecuta función para la preliquidación de dispersión
   WHENEVER ERROR CONTINUE
     PREPARE prp_fn_transaccion17
     FROM    "EXECUTE FUNCTION fn_dis_transaccion17(?,?,?,?,?,?,?,?)"
     EXECUTE prp_fn_transaccion17 INTO r_bnd_preliquidacion, v_status_err, v_desc_err, r_id_derechohabinete
                                 USING v_fecha_01mm,
                                       p_usuario,
                                       p_folio_disp,
                                       p_pid,
                                       p_proceso_cod,
                                       p_opera_cod,
                                       p_folio_regpag,
                                       p_proceso_cod_reg_pago
   WHENEVER ERROR STOP 

   DISPLAY ""   
   DISPLAY "Función Transaccion 17 ",r_bnd_preliquidacion
   DISPLAY "Código:",v_status_err
   DISPLAY "Id_derechohabiente", r_id_derechohabinete
   DISPLAY "mensaje", v_desc_err
   DISPLAY ""
         
   --PRELIQUIDACION 
   IF r_bnd_preliquidacion = 0 THEN      
      --Ejecuta SP para llenar la tabla de cortes por periodo y total de montos
      WHENEVER ERROR CONTINUE
      
      PREPARE prp_cortes_por_periodo
      FROM    "EXECUTE FUNCTION sp_dis_cortes_por_periodo(?)"
      EXECUTE prp_cortes_por_periodo INTO r_bnd_preliquidacion, v_status_err, v_desc_err
                                    USING p_folio_disp
     
      IF r_bnd_preliquidacion <> 0 THEN
         DISPLAY "Error al efectuar cortes por periodo: ",v_status_err," ",v_desc_err
      END IF
      
      LET v_bnd_preliq_ok = 1
      --Termina proceso de preliquidación pero no ejecuto datos
      CALL fn_valida_preliquidacion(p_pid, p_proceso_cod, p_opera_cod) 
      RETURNING v_bnd_preliq_ok

      IF v_bnd_preliq_ok = 1 THEN
         --Actualiza el status del folio de dispersión a Preliquidado 
         LET v_QryTxt = " UPDATE glo_folio",
                        " SET    status      = 1",
                        " WHERE  folio       = ",p_folio_disp,
                        " AND    proceso_cod = ",p_proceso_cod,
                        " AND    opera_cod   = 1"
                        
         PREPARE prp_actualiza_folio_prliquidado FROM v_QryTxt
         EXECUTE prp_actualiza_folio_prliquidado
         
         -- se obtiene el codigo de programa
         SELECT programa_cod
         INTO   p_programa_cod
         FROM   cat_operacion
         WHERE  proceso_cod = p_proceso_cod
         AND    opera_cod   = p_opera_co

         CALL fn_reporte_liquidacion(p_folio_disp,"dis_preliquida",p_usuario,p_pid,
                                      p_proceso_cod,p_opera_cod,p_programa_cod,
                                      FALSE)

         LET bnd_continuar = 1         
      END IF
   ELSE
      DISPLAY "Error en la transacción 17 ",r_bnd_preliquidacion
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
      RETURNING r_bnd_opera_err
      EXIT PROGRAM
   END IF

   IF bnd_continuar THEN
      CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
      RETURNING r_bandera

      LET p_opera_cod = 2 

      --Inicia la operación asignando el estatus de PROCESANDO
      IF r_bandera = 0 THEN 
        CALL fn_actualiza_opera_ini(p_pid, p_proceso_cod, p_opera_cod, p_folio_disp, 
                                    p_programa, p_nom_archivo, p_usuario)
        RETURNING r_bandera        
      END IF

      SELECT ruta_bin
      INTO   v_ruta_ejec
      FROM   seg_modulo 
      WHERE  modulo_cod = 'dis'
      
      SELECT ruta_listados
      INTO   v_ruta_list
      FROM   seg_modulo 
      WHERE  modulo_cod = 'bat'

      LET l_comando = "nohup fglrun ",v_ruta_ejec CLIPPED,"/DISP111.42r ",
                                      p_usuario CLIPPED," ",
                                      "Liquidacion ",
                                      p_folio_disp,                        --PROINFXVII-17-GVP
                                      " 1>",v_ruta_list CLIPPED,"/nohup:",
                                      p_pid USING "&&&&&",":",
                                      p_proceso_cod USING "&&&&&",":",
                                      p_opera_cod USING "&&&&&" ," 2>&1 &"
      RUN l_comando
      
      IF r_bandera = 0 THEN 
        CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
        RETURNING r_bandera
      END IF 

      IF r_bandera = 0 THEN 
         CALL fn_obtiene_datos_rpt(p_folio_disp,p_pid,p_proceso_cod,p_opera_cod,p_usuario) 
         RETURNING r_ruta_reporte
         -- Envío de correo de notificación de proceso finalizado
         CALL fn_correo_proceso(p_pid,
                                p_proceso_cod,
                                p_opera_cod,
                                r_ruta_reporte CLIPPED,
                                "Liquidación de Dispersión de Pagos",
                                "\n ID Proceso   : "||p_pid||
                                "\n Proceso      : Dispersión de Pagos"||
                                "\n Operacion    : Liquidación de dispersión de pagos"||
                                "\n Fecha Inicio : "||TODAY||
                                "\n Fecha Fin    : "||TODAY)
      END IF
   END IF      
END FUNCTION

#OBJETIVO: Obtener los datos necesarios para el reporte de preliquidación
FUNCTION fn_obtiene_datos_rpt(p_folio_disp,v_pid,v_proceso,v_operacion,p_usuario)
DEFINE 
   v_ruta_reporte            STRING, 
   v_QryTxt                  STRING,
   v_indice                  INTEGER,
   p_folio_disp              DECIMAL(9,0),
   v_pid                     DECIMAL(9,0),
   v_proceso                 SMALLINT,
   v_operacion               SMALLINT 
--------------------------------------
DEFINE 
   p_usuario                 CHAR (20),
   manejador_rpt             om.SaxDocumentHandler  -- Contenedor documentos reporte

DEFINE
   v_arr_dis_pre DYNAMIC     ARRAY OF RECORD
   v_f_liquida               LIKE dis_preliquida.f_liquida,
   v_subcuenta               CHAR(55),
   v_fondo_inversion         CHAR(55),
   v_monto_pesos             LIKE dis_preliquida.monto_pesos,
   v_monto_acciones          LIKE dis_preliquida.monto_acciones,
   v_tot_cuentas             INTEGER 
   END RECORD,

   v_tot_monto_pesos         LIKE dis_preliquida.monto_pesos,
   v_tot_monto_acciones      LIKE dis_preliquida.monto_acciones,
   v_tot_total_cuentas       DECIMAL(10,0),
   v_tot_cuentas_sctas       DECIMAL(20,0),
   v_sum_tot_cuentas         DECIMAL(20,0),
   v_tpo_enc                 STRING

DEFINE 
   v_tot_monto_pesos_rpt     LIKE dis_preliquida.monto_pesos,
   v_tot_total_cuentas_rpt   LIKE dis_preliquida.monto_pesos

DEFINE 
   v_arr_srv_mtd             DYNAMIC ARRAY OF RECORD
   v_servicios               CHAR(55),
   v_monto_pesos             LIKE dis_preliquida.monto_pesos,
   v_total_cuentas           INTEGER 
   END RECORD

   --------------------------------------
   --Prepara consulta para obtener detalles de cuentas
   LET v_QryTxt = "\n SELECT dp.f_liquida, ",
                  "\n        dp.subcuenta||'-'||cs.subcuenta_desc AS SUBCUENTA,",
                  "\n        dp.fondo_inversion||'-'||cf.razon_social AS FONDO,",
                  "\n        SUM(dp.monto_pesos),SUM(dp.monto_acciones),count(*)",
                  "\n FROM   dis_preliquida dp",
                  "\n JOIN   cat_subcuenta cs",
                  "\n ON     dp.subcuenta       = cs.subcuenta",
                  "\n JOIN   cat_fondo_local cf",
                  "\n ON     dp.fondo_inversion = cf.fondo",
                  "\n WHERE  dp.folio_liquida   =" ,p_folio_disp,
                  "\n GROUP BY 2,3,1"
   PREPARE prp_Datos_Rpt FROM v_QryTxt

   --Prepara consulta para obtener servicios de mandatos
   LET v_QryTxt = " SELECT movimiento||'-'||origen,",
                  "        SUM(monto_pesos),COUNT(*)",
                  " FROM   dis_preliquida",
                  " WHERE  movimiento IN (312,322,332)",
                  " GROUP BY 1",
                  " ORDER BY 1"
   PREPARE prp_dts_mdts FROM v_QryTxt

   LET v_origen_datos = p_usuario
   
   -- se construye la ruta del archivo
   CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
   LET v_ruta_reporte = v_ruta_listados.trim(),
                        "/",
                        v_origen_datos.trim(),"-",
                        "DISP11","-",
                        v_pid USING "&&&&&","-",
                        v_proceso USING "&&&&&","-",
                        v_operacion USING "&&&&&",".pdf"

   --Se asigna la plantilla para generar el reporte
   IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISP111.4rp") THEN
      -- si no se pidio el reporte en pantalla
      IF ( NOT p_b_despliegue_pantalla ) THEN
          CALL fgl_report_selectDevice ("PDF")
          -- sin preview
          CALL fgl_report_selectPreview(0)
          -- se indica que se escriba en archivo
          CALL fgl_report_setOutputFileName(v_ruta_reporte)
      END IF

      LET manejador_rpt = fgl_report_commitCurrentSettings()
   ELSE
      DISPLAY "NO FUNCIONO"
      EXIT PROGRAM
   END IF

   --Inicia el reporte
   START REPORT rpt_dis_preliquida TO XML HANDLER manejador_rpt
      LET v_indice             = 1
      LET v_tot_monto_pesos    = 0.00
      LET v_tot_monto_acciones = 0.00
      LET v_tot_total_cuentas  = 0.00
      LET v_tot_cuentas_sctas  = 0.00

      DECLARE cur_Datos_Rpt CURSOR FOR prp_Datos_Rpt
      --Llenado del arreglo con los datos obtenidos en la consulta
      FOREACH cur_Datos_Rpt INTO v_arr_dis_pre[v_indice].v_f_liquida,
                                 v_arr_dis_pre[v_indice].v_subcuenta,
                                 v_arr_dis_pre[v_indice].v_fondo_inversion,
                                 v_arr_dis_pre[v_indice].v_monto_pesos,
                                 v_arr_dis_pre[v_indice].v_monto_acciones,
                                 v_arr_dis_pre[v_indice].v_tot_cuentas

        LET v_tot_monto_pesos    = v_tot_monto_pesos + 
                                   v_arr_dis_pre[v_indice].v_monto_pesos
                                    
        LET v_tot_monto_acciones = v_tot_monto_acciones + 
                                   v_arr_dis_pre[v_indice].v_monto_acciones

        LET v_tot_cuentas_sctas  = v_tot_cuentas_sctas + 
                                   v_arr_dis_pre[v_indice].v_tot_cuentas

        LET v_tpo_enc            = 1

        OUTPUT TO REPORT rpt_dis_preliquida(v_arr_dis_pre[v_indice].*,
                                            v_tot_monto_pesos,
                                            v_tot_monto_acciones,
                                            v_tot_cuentas_sctas, 
                                            p_folio_disp,
                                            p_usuario, 
                                            v_sum_tot_cuentas,
                                            v_arr_srv_mtd[v_indice].*,
                                            v_tot_monto_pesos_rpt,
                                            v_tot_total_cuentas_rpt,
                                            v_tpo_enc)
        LET v_indice = v_indice + 1
      END FOREACH

      LET v_indice = 1
      DECLARE cur_dts_mtd CURSOR FOR prp_dts_mdts
      FOREACH cur_dts_mtd INTO v_arr_srv_mtd[v_indice].v_servicios,
                               v_arr_srv_mtd[v_indice].v_monto_pesos,
                               v_arr_srv_mtd[v_indice].v_total_cuentas
        LET v_tpo_enc = 2

        OUTPUT TO REPORT rpt_dis_preliquida(v_arr_dis_pre[v_indice].*,
                                            v_tot_monto_pesos,
                                            v_tot_monto_acciones,
                                            v_tot_cuentas_sctas, 
                                            p_folio_disp,
                                            p_usuario, 
                                            v_sum_tot_cuentas,
                                            v_arr_srv_mtd[v_indice].*,
                                            v_tot_monto_pesos_rpt,
                                            v_tot_total_cuentas_rpt,
                                            v_tpo_enc)
        LET v_indice = v_indice + 1
      END FOREACH
   FINISH REPORT rpt_dis_preliquida

   RETURN v_ruta_reporte
END FUNCTION 

#Objetivo: Borrar y crear sobre la base de datos las estructuras de las tablas
#          dis_preliquida, dis_interface_hs y dis_interface_ef_ad
FUNCTION fn_borra_crea_tablas()
  WHENEVER ERROR CONTINUE;
    DROP TABLE IF EXISTS dis_preliquida;
    DROP TABLE IF EXISTS dis_pre_his_trans;
    DROP TABLE IF EXISTS dis_pre_interface_hs;

    EXECUTE IMMEDIATE "SET INDEXES FOR dis_info_inconsistente DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_interface_ef_ad DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_compensa_avance DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_tramite DISABLED;"
    EXECUTE IMMEDIATE "SET INDEXES FOR dis_crd_ceros DISABLED;"
  WHENEVER ERROR STOP;
END FUNCTION

#Objetivo: Genera repote de avances de pago
REPORT rpt_dis_preliquida(v_arr_dis_pre,
                          v_tot_monto_pesos,
                          v_tot_monto_acciones,
                          v_tot_cuentas_scta, 
                          p_folio_liquida,
                          p_usuario, 
                          p_sum_tot_cuentas,
                          v_arr_srv_mtd, 
                          p_tot_monto_pesos_rpt,
                          p_tot_total_cuentas_rpt,
                          p_tpo_enc)

DEFINE 
   p_tot_monto_pesos_rpt     LIKE dis_preliquida.monto_pesos,
   p_tot_total_cuentas_rpt   DECIMAL(10,0),
   p_tpo_enc                 SMALLINT,
   v_v_desc_detalle          STRING

DEFINE 
   v_arr_srv_mtd             RECORD
   v_servicios               CHAR(55),
   v_monto_pesos             LIKE dis_preliquida.monto_pesos,
   v_total_cuentas           INTEGER 
   END RECORD

DEFINE 
   v_arr_dis_pre             RECORD
   v_f_liquida               LIKE dis_preliquida.f_liquida,
   v_subcuenta               CHAR(55),
   v_fondo_inversion         CHAR(55),
   v_monto_pesos             LIKE dis_preliquida.monto_pesos,
   v_monto_acciones          LIKE dis_preliquida.monto_acciones,
   v_tot_cuentas             INTEGER 
   END RECORD,

   v_tot_monto_pesos         LIKE dis_preliquida.monto_pesos,
   v_tot_monto_acciones      LIKE dis_preliquida.monto_acciones,
   v_tot_cuentas_scta        DECIMAL(20,2),
   v_fecha_reporte           DATE,
   sum_tot_pesos             LIKE dis_preliquida.monto_pesos,      
   sum_tot_acciones          LIKE dis_preliquida.monto_acciones, 
   sum_tot_ctas              DECIMAL(20,2),
   p_folio_liquida           LIKE dis_preliquida.folio_liquida, --Folio liquidado
   p_usuario                 LIKE seg_usuario.usuario_cod, -- Clave del usuario
   p_sum_tot_cuentas         DECIMAL(22,2)

FORMAT
   FIRST PAGE HEADER
      LET v_fecha_reporte = TODAY

      PRINTX v_fecha_reporte USING "dd-mm-yyyy" 
      PRINTX p_folio_liquida
      PRINTX p_usuario
      PRINTX v_arr_dis_pre.v_f_liquida USING "dd-mm-yyyy"      
      
   BEFORE GROUP OF p_tpo_enc
      IF p_tpo_enc = 1 THEN
         LET v_v_desc_detalle = "Subcuentas"
      ELSE
         LET v_v_desc_detalle = "Mandatos"
      END IF

      PRINT p_tpo_enc
      PRINTX v_v_desc_detalle

   ON EVERY ROW
      PRINTX v_arr_dis_pre.v_subcuenta
      PRINTX v_arr_dis_pre.v_fondo_inversion
      PRINTX v_arr_dis_pre.v_monto_pesos
      PRINTX v_arr_dis_pre.v_monto_acciones
      PRINTX v_arr_dis_pre.v_tot_cuentas
      PRINTX v_arr_srv_mtd.*
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

#Objetivo:Al termino del proceso de preliquidación de dispersión, valida si
#         en contro información relacionada en las tablas de pag_det_trabajador
#         con el folio de registro de pagos, derechohabiente y numero de credito
#         de cta_credito
FUNCTION fn_valida_preliquidacion(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
DEFINE 
   v_bnd_preliquidacion      DECIMAL(10,0),
   v_bnd_eror_opera          SMALLINT

DEFINE 
   v_p_pid                   LIKE bat_ctr_proceso.pid,
   v_p_proceso_cod           LIKE cat_proceso.proceso_cod,
   v_p_opera_cod             LIKE cat_operacion.opera_cod

DEFINE v_query               STRING
DEFINE v_programa            CHAR(10)
DEFINE v_bandera             SMALLINT

DEFINE v_bnd_ok              SMALLINT

   LET v_bnd_preliquidacion = 0
   LET v_bnd_ok             = 1

   SELECT COUNT(*) 
   INTO   v_bnd_preliquidacion
   FROM   dis_preliquida
   IF v_bnd_preliquidacion <= 0 THEN    
      LET v_bnd_ok = 0
     
      -- PRODINF-823
      --------------------------------------------------------
      DISPLAY ""
      DISPLAY "No se encontraron registros a liquidar con el folio de registro de pagos seleccionado"

      CALL fn_correo_proceso(v_p_pid,
                             v_p_proceso_cod,
                             v_p_opera_cod,
                             "",
                             "Liquidación Amo Acl Dest Incorr",
                             "\n ID Proceso   : "||v_p_pid||
                             "\n Proceso      : Dispersión de Pagos"||
                             "\n Operacion    : Liquidación Amo Acl Dest Incorr"||
                             "\n Fecha Inicio : "||TODAY||
                             "\n Fecha Fin    : "||TODAY)
      
      --CALL fn_error_opera(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
      --RETURNING v_bnd_eror_opera
      
      LET v_query = " UPDATE glo_folio",
                    " SET    status      = 1",
                    " WHERE  folio       = ",p_folio_disp,
                    " AND    proceso_cod = ",v_p_proceso_cod,
                    " AND    opera_cod   = 1"
                     
      PREPARE ps_actualiza_folio_prliquidado FROM v_query
      EXECUTE ps_actualiza_folio_prliquidado

      CALL fn_actualiza_opera_fin(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
      RETURNING r_bandera  ---Se Cierra la Preliquidación
      
      LET v_p_proceso_cod = 901 -- dispersion
      LET v_p_opera_cod   = 2   -- liquidacion

      SELECT programa_cod
      INTO   v_programa
      FROM   cat_operacion
      WHERE  proceso_cod = v_p_proceso_cod
      AND    opera_cod   = v_p_opera_cod

      LET v_bandera = 0
      LET v_bandera = fn_valida_operacion(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
     
      IF ( v_bandera = 0 ) THEN                                     
         CALL fn_actualiza_opera_ini(v_p_pid,
                                     v_p_proceso_cod,
                                     v_p_opera_cod,
                                     p_folio_disp,
                                     v_programa,
                                     "",
                                     p_usuario)
            RETURNING v_bandera
         
         CALL fn_actualiza_opera_fin(v_p_pid, v_p_proceso_cod, v_p_opera_cod)
         RETURNING r_bandera

         IF r_bandera = 0 THEN 
            -- se obtiene el codigo de programa
            SELECT programa_cod
            INTO   v_programa
            FROM   cat_operacion
            WHERE  proceso_cod = v_p_proceso_cod
            AND    opera_cod   = v_p_opera_cod

            LET v_query = " UPDATE glo_folio",
                          " SET    status      = 2",
                          " WHERE  folio       = ",p_folio_disp,
                          " AND    proceso_cod = ",v_p_proceso_cod,
                          " AND   opera_cod    = 1"

            PREPARE ps_actualiza_folio_liquidado FROM v_query
            EXECUTE ps_actualiza_folio_liquidado
         END IF

         DISPLAY ""
         DISPLAY "No se encontraron registros a liquidar con el folio de registro de pagos seleccionado"
      END IF
      
      EXIT PROGRAM
   ELSE
      LET v_bnd_ok = 1
      DISPLAY "Se Liquidaron ",v_bnd_preliquidacion," registros"
   END IF

 RETURN v_bnd_ok
END FUNCTION