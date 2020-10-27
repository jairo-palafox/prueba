################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 23/06/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE14                                                   #
#Objetivo          => Programa para integrar el archivo de casos de excepción  #                   
#Fecha inicio      => 23/06/2014                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usurio                 VARCHAR(30),                  --Almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod, --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod, --codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio, --Folio generado
    l_pid                    LIKE glo_pid.pid,
    g_qrytxt                 STRING                        --Prepara consultas

  DEFINE
    v_tot_leidos             DECIMAL(10,0),
    v_tot_ceros              DECIMAL(10,0),
    v_tot_no_enc             DECIMAL(10,0),
    v_tot_si_enc             DECIMAL(10,0),
    v_tot_monto_igual        DECIMAL(10,0),
    v_tot_monto_dif          DECIMAL(10,0),
    v_tot_dup_ap             DECIMAL(10,0)

  DEFINE
    l_arch_proceso           VARCHAR(100)

  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin --ruta del bin del módulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados --ruta de listados del módulo
  DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
  DEFINE v_extension         LIKE cat_operacion.extension
  DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
  DEFINE v_layout            LIKE cat_operacion.layout_cod
  DEFINE v_ruta_rescate      STRING
  DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
  DEFINE v_usuario           LIKE seg_modulo.usuario
  DEFINE v_cadena            STRING
  DEFINE v_s_qry             STRING
      
END GLOBALS

--Objetivo: Funcion que realiza la carga de tablas historicas de casos por excepción
MAIN
  DEFINE 
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_ruta_reporte           STRING, --ruta del archivo del reporte
    r_edo_rech               SMALLINT,
    r_bnd_oera_error         SMALLINT,
    p_transaccion            SMALLINT, --bandera que indica si la ejecución es manual o automática

    r_bnd                    INTEGER, 
    v_status_err             INTEGER ,
    v_desc_err               VARCHAR(200)

  LET v_usurio       = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion  = 0

  --Obtiene tipo de ejecución; si es 0 es manual, 
  --si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod, v_usurio)
     RETURNING g_folio
  END IF

  --Validaciones de Negocio
  WHENEVER ERROR CONTINUE 
    PREPARE prp_sp_cas_exc1 FROM "EXECUTE PROCEDURE safre_viv:sp_dis_casos_excepcion(?,?,?)"
    EXECUTE prp_sp_cas_exc1 USING g_folio, r_edo_rech, v_usurio
                             INTO r_bnd, v_status_err, v_desc_err
  WHENEVER ERROR STOP 
      
  IF r_bnd <> 0 THEN
     DISPLAY "Error2: ", v_status_err, " - ", v_desc_err, " - ", r_bnd
         
     --Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usurio)
     RETURNING r_bnd_edo_act_archivo
         
     --Función para finalizar la operación en error
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error

     EXIT PROGRAM 
  END IF

  --Actualiza el estado del archivo procesado
  CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usurio)
  RETURNING r_bnd_edo_act_archivo
      
  --Función para finalizar la operacion
  CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
  RETURNING r_b_valida

  --Si la operación no se finaliza, envia mensaje de error
  IF r_b_valida <> 0 THEN
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error
  ELSE
     CALL fn_genera_reporte_casos(g_folio) 
     RETURNING r_ruta_reporte
  END IF

END MAIN

#Objetivo: Genera reporte de cifras globales de los casos de excepción
FUNCTION fn_genera_reporte_casos(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0) --Casos excepción

  DEFINE
    v_tot_casos              DECIMAL(9,0), --Total Casos
    v_sum_cas_apo            DECIMAL(12,2),--Suma Monto Cas Aportación
    v_sum_cas_amo            DECIMAL(12,2) --Suma Monto Cas Amortización
    
  DEFINE 
    v_fec_proc               DATE          --Fecha de proceso

  DEFINE 
    v_desc_edo_arch          CHAR(50),
    v_fol_archivo            SMALLINT,
    r_edo_archivo            CHAR(50)

  DEFINE v_origen_datos      STRING
  DEFINE v_ruta_reporte      STRING --ruta del archivo del reporte
  DEFINE v_ruta_listados     STRING --ruta de los listados 
  DEFINE v_ruta_ejecutable   STRING --ruta del ejecutable
  DEFINE manejador_rpt       om.SaxDocumentHandler --Contenedor documentos para reporte 

  DEFINE arr_cas_glob        DYNAMIC ARRAY OF RECORD 
    v_estado                 CHAR(50),
    v_cas_aportacion         DECIMAL(12,2), --Casos aportaciones
    v_cas_amortizacion       DECIMAL(12,2)  --Casos amortizaciones   
  END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --Indice para el reporte  
    v_cantidad               STRING   --Variable auxiliar para cantidades

  LET v_fec_proc = TODAY

  --Obtiene el estado del archivo
  SELECT estado
  INTO   v_fol_archivo
  FROM   glo_ctr_archivo
  WHERE  proceso_cod = g_proceso_cod
  AND    folio       = p_folio 

  --Obtiene la descripcion del estado del archivo
  SELECT estado_descripcion
  INTO   v_desc_edo_arch
  FROM   cat_edo_archivo
  WHERE  estado_cod = v_fol_archivo

  --Concatena el estado y la descripcion del archivo
  LET r_edo_archivo = v_fol_archivo || '-' || v_desc_edo_arch CLIPPED

  --Obtiene el total de casos excepción
  SELECT COUNT(cas.nss), 
         SUM(cas.aportacion/100),
         SUM(cas.amortizacion/100)
  INTO   v_tot_casos,
         v_sum_cas_apo,
         v_sum_cas_amo
  FROM   safre_tmp:tmp_casos_exc1 cas

  --Despliega información en el log
  DISPLAY "\n ############## INTEGRACIÓN ARCHIVO CASOS EXCEPCIÓN ##############"

  LET v_cantidad = v_tot_casos 
  DISPLAY " Total de registros en archivo            : ", v_cantidad CLIPPED 

  LET v_cantidad = v_sum_cas_apo
  DISPLAY " Total del monto de aportación (Casos)    : ", v_cantidad CLIPPED 

  LET v_cantidad = v_sum_cas_amo
  DISPLAY " Total del monto de amortización (Casos)  : ", v_cantidad CLIPPED 

  DISPLAY " Nombre del archivo                       : ", l_arch_proceso CLIPPED 

  LET g_qrytxt = " SELECT cas.estado || '-' || rtrim(cat.desc_edo_caso_exc), ",
                 "        SUM(cas.aportacion), ",
                 "        SUM(cas.amortizacion)",
                 " FROM   dis_caso_excepcion cas, ",
                 "        cat_edo_caso_excepcion cat ",
                 " WHERE  cas.folio  = ", p_folio,
                 " AND    cas.estado = cat.cod_edo_caso_exc ",
                 " GROUP BY 1 ",
                 " ORDER BY 1"

  PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usurio

  -- se construye la ruta del archivo
  CALL fn_rutas("dis") 
  RETURNING v_ruta_ejecutable, v_ruta_listados
  
  LET v_ruta_reporte = v_ruta_listados.trim(), "/",
                       v_origen_datos.trim(), "-",
                       "DISE14", "-",
                       l_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&", ".pdf"                         

  DISPLAY " Ruta del reporte                         : ", v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE141.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     --Inicializamos variables para suma de totales
     LET v_indice_1    = 1 
            
     --Inicia el reporte de registros con rechazo
     START REPORT rp_cas_glob TO XML HANDLER manejador_rpt
       DECLARE cur_cas_montos CURSOR FOR prp_consulta_detalle   
       FOREACH cur_cas_montos INTO arr_cas_glob[v_indice_1].v_estado,
                                   arr_cas_glob[v_indice_1].v_cas_aportacion,
                                   arr_cas_glob[v_indice_1].v_cas_amortizacion

         LET v_indice_1 = v_indice_1  + 1
                  
       END FOREACH

       CALL arr_cas_glob.deleteElement(v_indice_1)         

       LET v_indice_1 = v_indice_1 - 1

       IF v_indice_1 = 0 THEN 
          DISPLAY "No se puede generar el reporte por falta de información."
       END IF  

       IF v_usurio IS NULL THEN 
          LET v_usurio = "infonavit"
       END IF 
                  
       FOR v_ind_rpt = 1 TO v_indice_1
           OUTPUT TO REPORT rp_cas_glob(g_folio,
                                        v_usurio,
                                        v_fec_proc,
                                        arr_cas_glob[v_ind_rpt].*,
                                        r_edo_archivo,
                                        v_tot_casos,
                                        v_sum_cas_apo,
                                        v_sum_cas_amo)
       END FOR 
     FINISH REPORT rp_cas_glob
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION

#OBJETIVO: Generar el reporte de Casos Excepción
REPORT rp_cas_glob(v_rfolio,
                   v_rusurio,
                   v_rfec_proc,
                   arr_cas_glob,
                   r_redo_archivo,
                   v_tot_casos,
                   v_sum_cas_apo, 
                   v_sum_cas_amo)              
                       
  DEFINE 
    v_rfolio                 DECIMAL(9,0),
    v_rusurio                VARCHAR(30),   --Usuario de proceso
    v_rfec_proc              DATE,          --Fecha de procesos
    v_tot_casos              DECIMAL(9,0),
    v_sum_cas_apo            DECIMAL(12,2),
    v_sum_cas_amo            DECIMAL(12,2)  

  DEFINE r_redo_archivo      CHAR(50)--Descripción del estado del archivo

  DEFINE arr_cas_glob        RECORD 
    v_estado                 CHAR(50),
    v_cas_aportacion         DECIMAL(12,2), --Casos aportaciones
    v_cas_amortizacion       DECIMAL(12,2)  --Casos amortizaciones
  END RECORD

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rusurio
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
       
      PRINTX v_rfolio
      PRINTX v_tot_casos
      PRINTX v_sum_cas_apo
      PRINTX v_sum_cas_amo
      PRINTX l_arch_proceso      

    ON EVERY ROW
       PRINTX arr_cas_glob.v_estado
       PRINTX arr_cas_glob.v_cas_aportacion
       PRINTX arr_cas_glob.v_cas_amortizacion
       
    ON LAST ROW
       PRINTX v_sum_cas_apo
       PRINTX v_sum_cas_amo
        
END REPORT