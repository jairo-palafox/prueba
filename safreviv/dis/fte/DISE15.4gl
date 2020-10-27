################################################################################
#Version                    => 1.1.0                                           #
#Fecha ultima modificacion  => 05/01/2014                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE15                                                   #
#Objetivo          => Programa para integrar el archivo de pagos por avance    # 
#                     Operacion 920                                            #
#                     Dispersión registros inconsistentes por número de        #
#                     crédito en ceros.                                        #                  
#Fecha inicio      => 08/01/2015                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usuario                VARCHAR(30),                  --Almacena al usuario
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
  DEFINE v_usuario1          LIKE seg_modulo.usuario
  DEFINE v_cadena            STRING
  DEFINE v_s_qry             STRING
      
END GLOBALS

--Objetivo: Funcion que realiza la carga de tablas hitoricas de avance de pago
MAIN
  DEFINE 
    r_b_valida               SMALLINT,
    r_bnd_edo_act_archivo    SMALLINT,
    r_ruta_reporte           STRING, --ruta del archivo del reporte
    r_edo_rech               SMALLINT,
    r_bnd_opera_error         SMALLINT,
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

  CALL STARTLOG (v_usuario CLIPPED||".DISE15.log")

  LET p_transaccion  = 0
  LET r_edo_rech     = 0

  --Obtiene tipo de ejecución; si es 0 es manual, 
  --si es 1 es automática y deberá generar folio del proceso
  SELECT ind_tipo_ejecucion 
  INTO   p_transaccion
  FROM   bat_ctr_operacion 
  WHERE  proceso_cod = g_proceso_cod   
  AND    pid         = l_pid
  AND    opera_cod   = g_opera_cod
  
  IF p_transaccion = 1 THEN 
     CALL fn_genera_folio(g_proceso_cod, g_opera_cod, v_usuario)
     RETURNING g_folio
  END IF
    
  --Validaciones de Negocio
  WHENEVER ERROR CONTINUE 
   EXECUTE IMMEDIATE "SET INDEXES FOR dis_arh_num_cred_0 DISABLED;"
   EXECUTE IMMEDIATE "SET INDEXES FOR dis_liq_inconsistente DISABLED;"

   
   DISPLAY "INICIÓ LA CARGA DE TABLAS TEMPORALES: ",CURRENT HOUR TO SECOND 
   PREPARE ps_sp_llenar_tablas_temp FROM "EXECUTE PROCEDURE sp_dis_cargar_tablas_tmp()"
   EXECUTE ps_sp_llenar_tablas_temp INTO r_bnd, v_status_err, v_desc_err
   DISPLAY "EXECUTE PROCEDURE sp_dis_cargar_tablas_tmp() ",r_bnd, v_status_err, v_desc_err
   DISPLAY "FINALIZÓ LA CARGA DE TABLAS TEMPORALES: ",CURRENT HOUR TO SECOND 

   
   
    PREPARE prp_sp_dis_cred_cero FROM "EXECUTE PROCEDURE safre_viv:sp_dis_val_cred_cero(?,?)"
    EXECUTE prp_sp_dis_cred_cero USING g_folio, r_edo_rech
                                 INTO r_bnd, v_status_err, v_desc_err
                                 
  WHENEVER ERROR STOP 
      
  IF r_bnd <> 0 THEN
     DISPLAY "Error2: ", v_status_err, " - ", v_desc_err, " - ", r_bnd
         
     --Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usuario)
     RETURNING r_bnd_edo_act_archivo
         
     --Función para finalizar la operación en error
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_opera_error

     EXIT PROGRAM 
  END IF

  --Actualiza el estado del archivo procesado
  CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usuario)
  RETURNING r_bnd_edo_act_archivo
      
  --Función para finalizar la operacion
  CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
  RETURNING r_b_valida

  --Si la operación no se finaliza, envia mensaje de error
  IF r_b_valida <> 0 THEN
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_opera_error
  ELSE
     CALL fn_genera_reporte_creditos_ceros(g_folio) 
     RETURNING r_ruta_reporte

  END IF

END MAIN

#Objetivo: Genera reporte de cifras de control creditos ceros
FUNCTION fn_genera_reporte_creditos_ceros(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0) --Pagos por Avance

  DEFINE
    v_tot_nss                DECIMAL(9,0), --Total Avances
    v_tot_inconsis           DECIMAL(9,0)  --Total Pagos
    
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

  DEFINE arr_liq_incons      DYNAMIC ARRAY OF RECORD 
             v_estado            SMALLINT,
             v_desc_estado       CHAR(50), 
             v_total_edo         DECIMAL(9,0)  
  END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --Indice para el reporte  
    v_cantidad               STRING   --Variable auxiliar para cantidades

  DEFINE  
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(40), --nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08), --fecha del archivo de salida
    v_busca_nom_archivo      STRING,      --busca nombre de archivo
    v_precio_fondo           DECIMAL(19,14)
    
  LET v_fec_proc = TODAY

  LET v_tot_nss = 0
  LET v_tot_inconsis = 0

  --Obtiene el valor de fondo (AIVS)
  SELECT f.precio_fondo
  INTO   v_precio_fondo
  FROM   glo_valor_fondo f
  WHERE  f.fondo       = 11
  AND    f.f_valuacion = v_fec_proc
  
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

  --Obtiene el total de nss
  SELECT COUNT(dcc.nss)
  INTO   v_tot_nss
  FROM   safre_tmp:tmp_dis_cred_cero1 dcc

  --Obtiene el total de rgistros con inconsistencias
  SELECT COUNT(*)
  INTO   v_tot_inconsis
  FROM   dis_arh_num_cred_0
  WHERE  folio = p_folio

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "dis"

  --Se obtienen la ruta envío del módulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_liq_inconsis_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_liq_inconsis_" || v_ddmmaaaa  --Concatena nombre a buscar

  CALL fn_obtiene_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo)
  RETURNING v_nom_archivo
  
  --Despliega información en el log
  DISPLAY "\n ####### INTEGRACIÓN ARCHIVO NSS CON NÚMEROS DE CRED CERO ########"

  LET v_cantidad = v_tot_nss
  DISPLAY " Total de nss en archivo                    : ", v_cantidad CLIPPED 

  DISPLAY " Nombre del archivo (Integrado)             : ", l_arch_proceso CLIPPED 

  {DISPLAY "\n ########## ARCHIVO NSS CON NÚMEROS DE CRED CERO SALIDA ##########"

  LET v_cantidad = v_tot_inconsis
  DISPLAY " Total de registros en archivo              : ", v_cantidad CLIPPED 

  DISPLAY " Nombre del archivo (Salida)                : ", v_nom_archivo CLIPPED 
  DISPLAY ""}
  
  LET g_qrytxt = " SELECT dcli.estado, COUNT(*) ",
                 " FROM   dis_arh_num_cred_0 dcli ",
                 " WHERE  dcli.folio = ",p_folio,
                 " GROUP BY dcli.estado ",
                 " ORDER BY dcli.estado "

  PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usuario

  -- se construye la ruta del archivo
  CALL fn_rutas("dis") 
  RETURNING v_ruta_ejecutable, v_ruta_listados
  
  LET v_ruta_reporte = v_ruta_listados.trim(), "/",
                       v_origen_datos.trim(), "-",
                       "DISE15", "-",
                       l_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&", ".pdf"                         

  DISPLAY " Ruta del reporte                         : ", v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE151.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     --Inicializamos variables para suma de totales
     LET v_indice_1    = 1 
            
     --Inicia el reporte de registros con rechazo
     START REPORT rp_cur_liq_incons TO XML HANDLER manejador_rpt
     
       DECLARE cur_liq_incons CURSOR FOR prp_consulta_detalle   
       FOREACH cur_liq_incons INTO arr_liq_incons[v_indice_1].v_estado,
                                   arr_liq_incons[v_indice_1].v_total_edo

         CASE arr_liq_incons[v_indice_1].v_estado
            WHEN 0
               LET arr_liq_incons[v_indice_1].v_desc_estado = "PENDIENTES POR LIQUIDAR"

            WHEN 1
               LET arr_liq_incons[v_indice_1].v_desc_estado = "RECHAZADOS"

            OTHERWISE
               LET arr_liq_incons[v_indice_1].v_desc_estado = "ESTADO NO RECONOCIDO"

         END CASE

         LET v_indice_1 = v_indice_1  + 1
                  
       END FOREACH

       CALL arr_liq_incons.deleteElement(v_indice_1)         

       LET v_indice_1 = v_indice_1 - 1

       IF v_indice_1 = 0 THEN 
          DISPLAY "No se puede generar el reporte por falta de información."
       END IF  

       IF v_usuario IS NULL THEN 
          LET v_usuario = "infonavit"
       END IF 
                  
       FOR v_ind_rpt = 1 TO arr_liq_incons.getLength()
           OUTPUT TO REPORT rp_cur_liq_incons(g_folio,
                                              v_usuario,
                                              v_fec_proc,
                                              arr_liq_incons[v_ind_rpt].*,
                                              r_edo_archivo,
                                              v_tot_inconsis,
                                              v_nom_archivo,
                                              v_precio_fondo)
       END FOR 
     FINISH REPORT rp_cur_liq_incons
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION

#OBJETIVO: Generar el reporte de Avances
REPORT rp_cur_liq_incons(v_rfolio,
                         v_rusurio,
                         v_rfec_proc,
                         rec_liq_incons,
                         r_redo_archivo,
                         v_tot_inconsis,
                         v_nom_archivo,
                         v_precio_fondo)
                       
  DEFINE 
    v_rfolio                 DECIMAL(9,0),
    v_rusurio                VARCHAR(30),   --Usuario de proceso
    v_rfec_proc              DATE,          --Fecha de procesos
    v_tot_inconsis           DECIMAL(9,0),
    v_nom_archivo            CHAR(40),
    v_precio_fondo           DECIMAL(19,14)

  DEFINE r_redo_archivo      CHAR(50)--Descripción del estado del archivo

  DEFINE rec_liq_incons        RECORD 
             v_estado            SMALLINT,
             v_desc_estado       CHAR(50), 
             v_total_edo         DECIMAL(9,0) 
  END RECORD

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rusurio
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
       
      PRINTX v_rfolio
      PRINTX l_arch_proceso
      PRINTX v_nom_archivo
      PRINTX v_precio_fondo

    ON EVERY ROW
       PRINTX rec_liq_incons.v_estado
       PRINTX rec_liq_incons.v_desc_estado
       PRINTX rec_liq_incons.v_total_edo

       
    ON LAST ROW
       PRINTX v_tot_inconsis

        
END REPORT

FUNCTION fn_obtiene_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40), --Nombre del archivo a buscar(nombre||fecha)
    v_cmd                    STRING,      --Cadena de comando a ejecutar
    v_consecutivo            INTEGER,     --Consecutivo del archivo por día
    v_nom_archivo_sal        VARCHAR(40)  --Nombre del archivo de salida

  DEFINE 
    fn                       CHAR(28)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensión

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  DEFINE  
    v_reg_dia                CHAR(03), --Parametro consecutivo de registro por dia
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_ddmmaaaa               VARCHAR(08) --fecha del archivo de salida

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "dis"
  
  LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  WHILE ch.read([fn])
    LET v_consecutivo     = fn[26,28]  --Posición del consecutivo dentro de la cadena
    LET v_nom_archivo_sal = fn
  END WHILE

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del día

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del día
     LET v_consecutivo = 1
  END IF

  LET v_nom_archivo_sal = "dis_liq_inconsis_" --nombre de archivo
  LET v_ddmmaaaa        = TODAY USING "ddmmyyyy"  --Fecha del archivo sin separadores
  LET v_reg_dia         = v_consecutivo USING "&&&"  --Consecutivo del día de numerico a char
  LET v_nom_archivo_sal = v_nom_archivo_sal CLIPPED || v_ddmmaaaa || v_reg_dia||"."|| v_modulo_cod

  RETURN v_nom_archivo_sal --Regresa el nombre del archivo del día

END FUNCTION
