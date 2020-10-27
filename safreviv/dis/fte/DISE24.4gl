################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 16/02/2016                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE24                                                   #
#Objetivo          => Programa para integrar el archivo de aportaciones        #                   
#                     subsecuentes sin conciliar                               #
#Fecha inicio      => 16/02/2016                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usuario                VARCHAR(30), --almacena al usuario
    g_proceso_cod            LIKE cat_proceso.proceso_cod,   --codigo de proceso
    g_opera_cod              LIKE cat_operacion.opera_cod,   --codigo de operacion
    g_folio                  LIKE dis_det_avance_pago.folio, --folio generdo
    l_pid                    LIKE glo_pid.pid,
    g_qrytxt                 STRING --prepara consultas

  DEFINE
    l_arch_proceso           VARCHAR(100)

  DEFINE arr_sin_gen          DYNAMIC ARRAY OF RECORD    
    v_nss                      CHAR(11),    
    v_periodo_pago             CHAR(6),
    v_num_credito              DECIMAL(10,0),
    v_imp_pesos                DECIMAL(12,2), 
    v_imp_aivs                 DECIMAL(15,6)
  END RECORD

  DEFINE v_comando_dos         STRING
      
END GLOBALS

-- Objetivo: Funcion que realiza la carga de tablas hitoricas
MAIN
  DEFINE 
    l_s_qryTxt               STRING, --guarda una sentencia SQL a ejecutar
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_ruta_reporte           STRING, --ruta del archivo del reporte 
    r_bnd_oera_error         SMALLINT,
    p_transaccion            SMALLINT, --bandera que indica si la ejecución es manual o automática

    r_bnd                    INTEGER, 
    v_status_err             INTEGER ,
    v_desc_err               VARCHAR(200)

  LET v_usuario      = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  LET p_transaccion  = 0

  -- Obtiene tipo de ejecución; si es 0 es manual, si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod

  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod,v_usuario)
     RETURNING g_folio
  END IF
 
  WHENEVER ERROR CONTINUE 
     -- Inserta datos de control del proceso de aportaciones subsecuentes
     PREPARE prp_sp_apo_sub FROM "EXECUTE PROCEDURE safre_viv:sp_dis_apo_subs_sc(?)"
     EXECUTE prp_sp_apo_sub USING g_folio
     INTO r_bnd, v_status_err, v_desc_err

    -- DISPLAY "Respuesta sp_dis_apo_subs_sc: ", r_bnd, " - ", v_status_err, " - ", v_desc_err
  WHENEVER ERROR STOP 

  IF r_bnd <> 0 THEN
     DISPLAY "Error1: ", r_bnd, " - ", v_status_err, " - ", v_desc_err, " - "

     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error

     EXIT PROGRAM
  END IF

  -- Actualiza el estado del archivo procesado
  CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usuario)
  RETURNING r_bnd_edo_act_archivo
      
  -- Función para finalizar la operacion
  CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
  RETURNING r_b_valida

  -- Si la operacin no se finaliza, envia mensaje de error
  IF r_b_valida <> 0 THEN
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error
  ELSE
     CALL fn_genera_reporte_aportaciones() 
     RETURNING r_ruta_reporte  
  END IF
  
END MAIN


#Objetivo: Genera reporte de cifras globales de aportaciones subsecuentes
FUNCTION fn_genera_reporte_aportaciones()
  DEFINE 
    v_tot_reg_int                  INTEGER, -- Total de registros integrados
    v_tot_imp_aivs_int             DECIMAL(15,6), -- Suma del importe de AIVS integradas    
    v_tot_imp_pes_int              DECIMAL(12,2), -- Suma del importe de pesos integrados
    v_tot_reg_dup                  INTEGER, -- Total de registros duplicados
    v_tot_imp_aivs_dup             DECIMAL(15,6), -- Suma del importe de AIVS duplicados
    v_tot_imp_pes_dup              DECIMAL(12,2), -- Suma del importe de pesos duplicados
    v_indice_1                     INTEGER, -- Total de registros no generados   
    v_tot_imp_aivs                 DECIMAL(15,6), -- Suma del importe de AIVS no generados
    v_tot_imp_pesos                DECIMAL(12,2), -- Suma del importe de pesos no generados
    v_tot_reg_arc                  INTEGER, -- Total de registros en archivo
    v_fec_proc                     DATE -- Fecha de proceso

  DEFINE v_origen_datos            STRING
  DEFINE v_ruta_reporte            STRING --ruta del archivo del reporte
  DEFINE v_ruta_listados           STRING --ruta de los listados 
  DEFINE v_ruta_ejecutable         STRING --ruta del ejecutable
  DEFINE manejador_rpt             om.SaxDocumentHandler --contenedor documentos para reporte 
       
  LET v_fec_proc = TODAY

  -- Obtiene las cifras globales de los registros integrados
  SELECT COUNT(id) AS tot_reg, 
         NVL(SUM(imp_apo_aivs),0) AS imp_apo_aivs, 
         NVL(SUM(imp_apo_pat),0) AS imp_apo_pat
    INTO v_tot_reg_int, 
         v_tot_imp_aivs_int, 
         v_tot_imp_pes_int
    FROM dis_as_sin_conciliar
   WHERE ind_concilia = 2;

     -- Obtiene las cifras globales de los registros duplicados
  SELECT NVL(SUM(tot_apo_subs),0) AS tot_reg, 
         NVL(SUM(imp_aivs),0) AS imp_apo_aivs, 
         NVL(SUM(imp_pesos),0) AS imp_apo_pat
    INTO v_tot_reg_dup, 
         v_tot_imp_aivs_dup, 
         v_tot_imp_pes_dup
    FROM dis_dup_ap_sc
   WHERE folio_ap_subs = g_folio;

  -- Obtiene el total de registros del archivo
  SELECT COUNT(*) 
    INTO v_tot_reg_arc
    FROM safre_tmp:tmp_dis_aposubs_sc   

  -- Obtiene los registros sin generar
  LET g_qrytxt = "\n    SELECT af.nss, ",
                 "\n           td.periodo_pago, ",
                 "\n           td.num_credito, ", 
                 "\n           NVL(td.imp_pesos,0)/100 AS imp_pesos, ", 
                 "\n           NVL(td.imp_aivs,0)/1000000 AS imp_aivs ",
                 "\n      FROM safre_tmp:tmp_dis_aposubs_sc td, ",
                 "\n           afi_derechohabiente af ",
                 "\n     WHERE td.nss = af.nss ",
                 "\n       AND NOT EXISTS( ",
                 "\n           SELECT sc.id_derechohabiente, ",
                 "\n                  sc.periodo_pago ",
                 "\n             FROM dis_as_sin_conciliar sc ",
                 "\n            WHERE sc.ind_concilia = 2 ",
                 "\n              AND sc.id_derechohabiente = af.id_derechohabiente ",
                 "\n              AND sc.periodo_pago = td.periodo_pago) "

  PREPARE prp_con_det FROM g_qrytxt   

  LET v_indice_1 = 1
  LET v_tot_imp_pesos = 0
  LET v_tot_imp_aivs = 0

  DECLARE cur_sin_gen CURSOR FOR prp_con_det   
  FOREACH cur_sin_gen INTO arr_sin_gen[v_indice_1].v_nss,
                           arr_sin_gen[v_indice_1].v_periodo_pago, 
                           arr_sin_gen[v_indice_1].v_num_credito,
                           arr_sin_gen[v_indice_1].v_imp_pesos,
                           arr_sin_gen[v_indice_1].v_imp_aivs

    LET v_tot_imp_pesos = v_tot_imp_pesos + arr_sin_gen[v_indice_1].v_imp_pesos
    LET v_tot_imp_aivs = v_tot_imp_aivs + arr_sin_gen[v_indice_1].v_imp_aivs
 
    LET v_indice_1 = v_indice_1  + 1
  END FOREACH

  CALL arr_sin_gen.deleteElement(v_indice_1)         
  LET v_indice_1 = v_indice_1 - 1



  -- Despliega información en el log
  DISPLAY "\n INTEGRACIÓN APORTACIONES SUBSECUENTES SIN CONCILIAR"  
  DISPLAY " Total de registros en archivo  : ", v_tot_reg_arc CLIPPED   
  DISPLAY " Total de registros integrados  : ", v_tot_reg_int CLIPPED 
  DISPLAY " Total de registros no generados: ", v_indice_1 CLIPPED 
  DISPLAY " Total de registros duplicados  : ", v_tot_reg_dup CLIPPED
  DISPLAY " Nombre del archivo   : ", l_arch_proceso CLIPPED 
   
  LET v_origen_datos = v_usuario

  -- Se construye la ruta del archivo
  CALL fn_rutas("dis") RETURNING v_ruta_ejecutable, v_ruta_listados
  
  LET v_ruta_reporte = v_ruta_listados.trim(),
                       "/",
                       v_origen_datos.trim(), "-",
                       "DISE24", "-",
                       l_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&",".pdf"                         

  DISPLAY "Ruta del reporte: ", v_ruta_reporte
  -- Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE241.4rp") THEN
    CALL fgl_report_selectDevice ("PDF")        
    CALL fgl_report_selectPreview(0)
    CALL fgl_report_setOutputFileName(v_ruta_reporte)

    LET manejador_rpt = fgl_report_commitCurrentSettings()  

    -- Inicia el reporte de registros con rechazo
    START REPORT rp_apo_sub_sc TO XML HANDLER manejador_rpt

    IF v_usuario IS NULL THEN 
      LET v_usuario = "infonavit"        
    END IF

    IF v_tot_reg_int IS NULL THEN 
      LET v_tot_reg_int = 0 
    END IF

    IF v_tot_imp_aivs_int IS NULL THEN 
      LET v_tot_imp_aivs_int = 0
    END IF

    IF v_tot_imp_pes_int IS NULL THEN 
      LET v_tot_imp_pes_int = 0 
    END IF

    IF v_tot_imp_pes_int IS NULL THEN 
      LET v_tot_imp_pes_int = 0 
    END IF 
    
    IF v_indice_1 IS NULL THEN 
      LET v_indice_1 = 0 
    END IF

    IF v_tot_imp_aivs IS NULL THEN 
      LET v_tot_imp_aivs = 0 
    END IF

    IF v_tot_imp_pesos IS NULL THEN 
      LET v_tot_imp_pesos = 0 
    END IF 

    IF v_tot_reg_arc IS NULL THEN 
      LET v_tot_reg_arc = 0
    END IF

    IF v_tot_reg_dup IS NULL THEN
      LET v_tot_reg_dup = 0 
    END IF 

    IF v_tot_imp_aivs_dup IS NULL THEN 
      LET v_tot_imp_aivs_dup = 0 
    END IF

    IF v_tot_imp_pes_dup IS NULL THEN 
      LET v_tot_imp_pes_dup = 0 
    END IF

    OUTPUT TO REPORT rp_apo_sub_sc(v_usuario,
                                   v_fec_proc,
                                   v_tot_reg_int, 
                                   v_tot_imp_aivs_int, 
                                   v_tot_imp_pes_int, 
                                   v_indice_1,
                                   v_tot_imp_aivs,
                                   v_tot_imp_pesos,
                                   v_tot_reg_arc, 
                                   v_tot_reg_dup, 
                                   v_tot_imp_aivs_dup, 
                                   v_tot_imp_pes_dup)

     FINISH REPORT rp_apo_sub_sc

     IF v_indice_1 >= 1 THEN
        CALL fn_genera_interfase()
     END IF
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION


#OBJETIVO: Generar el reporte de Aportaciones Subsecuentes
REPORT rp_apo_sub_sc(r_usuario,
                     r_fec_proc,
                     r_tot_reg_int, 
                     r_tot_imp_aivs_int, 
                     r_tot_imp_pes_int,
                     r_indice_1,
                     r_tot_imp_aivs, 
                     r_tot_imp_pesos,
                     r_tot_reg_arc, 
                     r_tot_reg_dup, 
                     r_tot_imp_aivs_dup, 
                     r_tot_imp_pes_dup)              
                       
  DEFINE 
    r_usuario                      VARCHAR(30), --usuario de proceso
    r_fec_proc                     DATE,        --fecha de procesos
    r_tot_reg_int                  INTEGER, -- Total de registros integrados
    r_tot_imp_aivs_int             DECIMAL(15,6), -- Suma del importe de AIVS integradas    
    r_tot_imp_pes_int              DECIMAL(12,2), -- Suma del importe de pesos integrados
    r_indice_1                     INTEGER, -- Total de registros no generados   
    r_tot_imp_aivs                 DECIMAL(15,6), -- Suma del importe de AIVS no generados
    r_tot_imp_pesos                DECIMAL(12,2), -- Suma del importe de pesos no generados
    r_tot_reg_dup                  INTEGER, -- Total de registros duplicados
    r_tot_imp_aivs_dup             DECIMAL(15,6), -- Suma del importe de AIVS duplicados    
    r_tot_imp_pes_dup              DECIMAL(12,2), -- Suma del importe de pesos duplicados
    r_tot_reg_arc                  INTEGER -- Total de registros en archivo

  FORMAT
    FIRST PAGE HEADER
      PRINTX r_usuario
      PRINTX r_fec_proc     USING "dd-mm-yyyy"
      PRINTX l_arch_proceso  
    
    ON EVERY ROW
       PRINTX r_tot_reg_int
       PRINTX r_tot_imp_aivs_int
       PRINTX r_tot_imp_pes_int
       PRINTX r_indice_1
       PRINTX r_tot_imp_aivs
       PRINTX r_tot_imp_pesos 
       PRINTX r_tot_reg_dup
       PRINTX r_tot_imp_aivs_dup
       PRINTX r_tot_imp_pes_dup    
       
    ON LAST ROW       
       PRINTX r_tot_reg_arc
      
END REPORT


# Genera un archivo txt con los datos del arreglo
FUNCTION fn_genera_interfase()
  DEFINE 
    v_ch_arch_salida         BASE.CHANNEL,
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40),  --Nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08),  --Fecha del archivo de salida
    v_busca_nom_archivo      STRING,       --Busca nombre de archivo
    v_cont_dia               SMALLINT,     --Consecutivo por dia de archivo generado
    v_reg_dia                CHAR(03),     --Parametro consecutivo de registro por dia
    v_ruta_nomarch           VARCHAR(100), --Ruta y nombre del archivo de salida
    v_encabezado             STRING,
    v_detalle                STRING,
    v_sumario                STRING,
    v_indice                 INTEGER

  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  --Se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_as_no_gen_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_as_no_gen_" || v_ddmmaaaa  --Concatena nombre a buscar

  --Obtiene consecutivo para archivo por día
  CALL fn_crea_nombre_archivo(v_ruta_envio_dis,v_busca_nom_archivo)
  RETURNING v_cont_dia

  LET v_reg_dia        = v_cont_dia USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo    = v_nom_archivo CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod
  LET v_ruta_nomarch   = v_ruta_envio_dis CLIPPED || v_nom_archivo
  LET v_ch_arch_salida = base.Channel.create()
   
  --Se crea archivo y se indica que se escribira en el mismo
  CALL v_ch_arch_salida.openFile(v_ruta_nomarch,"w" )
  CALL v_ch_arch_salida.setDelimiter("")

  --Imprime encabezado del archivo
  LET v_encabezado = " FECHA|",TODAY
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_encabezado = " NSS |PERIODO PAGO NO DISPERSADO |NÚMERO CRÉDITO VIGENTE O LÍQUIDADO |IMPORTE EN PESOS |IMPORTE EN AIVS |"
  CALL v_ch_arch_salida.write([v_encabezado])

  LET v_indice = 1
 
  FOR v_indice = 1 TO arr_sin_gen.getLength()
      
      LET v_detalle = arr_sin_gen[v_indice].v_nss, "|",
                      arr_sin_gen[v_indice].v_periodo_pago, "|",
                      arr_sin_gen[v_indice].v_num_credito USING "&&&&&&&&&&", "|",
                      arr_sin_gen[v_indice].v_imp_pesos, "|",
                      arr_sin_gen[v_indice].v_imp_aivs, "|"                      
      CALL v_ch_arch_salida.write([v_detalle])
  END FOR

  LET v_indice = v_indice - 1 

  --Escribe el sumario
  LET v_sumario = "TOTAL DE REGISTROS |",v_indice,"| "
  CALL v_ch_arch_salida.write([v_sumario])
   
  --Cierra el archivo
  CALL v_ch_arch_salida.close()
   
  --Cambia el formato del archivo a DOS
  LET v_comando_dos = "unix2dos ",v_ruta_envio_dis CLIPPED, " ", v_nom_archivo CLIPPED
  RUN v_comando_dos

  DISPLAY "Se ha generado el archivo de Aportaciones Subsecuentes Sin Conciliar No generadas\nen la ruta ",v_ruta_nomarch
  
END FUNCTION 

FUNCTION fn_crea_nombre_archivo(p_ruta_envio_dis,p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40),  --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,       --Cadena de comando a ejecutar
    v_consecutivo            INTEGER       --Consecutivo del archivo por día

  DEFINE 
    fn                       CHAR(22)      --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo = fn[16,18] --Posición del consecutivo dentro de la cadena
  END WHILE

  CALL ch.close()
  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  RETURN v_consecutivo  --Regresa el consecutivo del siguiente archivo del día

END FUNCTION