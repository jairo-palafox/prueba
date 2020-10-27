################################################################################
#Version                    => 1.0.0                                           #
#Fecha ultima modificacion  => 29/07/2015                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DIS                                                      #
#Programa          => DISE22                                                   #
#Objetivo          => Programa para integrar el archivo de COnsulta Pagos      # 
#                     Hist�ricos (Proceso 927)                  
#Fecha inicio      => 23/07/2015                                               #
################################################################################
DATABASE
  safre_viv

GLOBALS
  DEFINE 
    v_usuario                 VARCHAR(30),                  --Almacena al usuario
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

  DEFINE r_c_ruta_bin        LIKE seg_modulo.ruta_bin --ruta del bin del m�dulo
  DEFINE r_ruta_listados     LIKE seg_modulo.ruta_listados --ruta de listados del m�dulo
  DEFINE v_proceso_desc      LIKE cat_proceso.proceso_desc
  DEFINE v_extension         LIKE cat_operacion.extension
  DEFINE v_opera_desc        LIKE cat_operacion.opera_desc
  DEFINE v_layout            LIKE cat_operacion.layout_cod
  DEFINE v_ruta_rescate      STRING
  DEFINE v_ruta_listados     LIKE seg_modulo.ruta_listados
  --DEFINE v_usuario           LIKE seg_modulo.usuario
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
    r_bnd_oera_error         SMALLINT,
    p_transaccion            SMALLINT, --bandera que indica si la ejecuci�n es manual o autom�tica

    r_bnd                    INTEGER, 
    v_status_err             INTEGER ,
    v_desc_err               VARCHAR(200)

   
  LET v_usuario      = ARG_VAL(1)
  LET l_pid          = ARG_VAL(2)
  LET g_proceso_cod  = ARG_VAL(3)
  LET g_opera_cod    = ARG_VAL(4)
  LET g_folio        = ARG_VAL(5)
  LET l_arch_proceso = ARG_VAL(6)

  CALL STARTLOG (v_usuario CLIPPED|| ".DISE22.log")

  LET p_transaccion  = 0

  --Obtiene tipo de ejecuci�n; si es 0 es manual, 
  --si es 1 es autom�tica y deber� generar folio del proceso
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

   --Borra y crea tablas dis_cons_pag_hist, dis_mov_cons_pag_h
   --CALL fn_borra_crea_tablas_tmp()

  --Validaciones de Negocio
  WHENEVER ERROR CONTINUE 

   --EXECUTE IMMEDIATE "SET INDEXES FOR dis_arh_num_cred_0 DISABLED;"
   --EXECUTE IMMEDIATE "SET INDEXES FOR dis_liq_inconsistente DISABLED;"

   DISPLAY "INICI� LA CARGA DE PAGOS, MOVIMIENTOS: ",CURRENT HOUR TO SECOND 
   PREPARE ps_sp_pagos_mov_tmp FROM "EXECUTE PROCEDURE sp_dis_cargar_pag_mov_tmp()"
   EXECUTE ps_sp_pagos_mov_tmp INTO r_bnd, v_status_err, v_desc_err
   DISPLAY "EXECUTE PROCEDURE sp_dis_cargar_pag_mov_tmp() : ", r_bnd, v_status_err, v_desc_err
   DISPLAY "FINALIZ� LA CARGA DE PAGOS, MOVIMIENTOS: ",CURRENT HOUR TO SECOND 

   DISPLAY ""
   DISPLAY "INICI� LA VALIDACION DE PAGOS  Y MOVIMIENTOS HIST�RICOS: ",CURRENT HOUR TO SECOND
   PREPARE ps_sp_dis_cons_pag_hist FROM "EXECUTE PROCEDURE safre_viv:sp_dis_cons_pag_hist(?)"
   EXECUTE ps_sp_dis_cons_pag_hist USING g_folio
                                   INTO r_bnd, v_status_err, v_desc_err
   DISPLAY "FOLIO (?): ",g_folio
   DISPLAY "safre_viv:sp_dis_cons_pag_hist(?) : ", r_bnd, v_status_err, v_desc_err
   DISPLAY "FINALIZ� LA VALIDACION DE PAGOS  Y MOVIMIENTOS HIST�RICOS: ",CURRENT HOUR TO SECOND                                   
                                 
  WHENEVER ERROR STOP 

 
  IF r_bnd <> 0 THEN
     DISPLAY "Error2: ", v_status_err, " - ", v_desc_err, " - ", r_bnd
         
     --Actualiza el estado del archivo procesado
     CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usuario)
     RETURNING r_bnd_edo_act_archivo
         
     --Funci�n para finalizar la operaci�n en error
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error

     EXIT PROGRAM 
  END IF

  --Actualiza el estado del archivo procesado
  CALL fn_act_edo_archivo(l_arch_proceso, g_folio, 2, v_usuario)
  RETURNING r_bnd_edo_act_archivo
      
  --Funci�n para finalizar la operacion
  --CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
  --RETURNING r_b_valida

  --Si la operaci�n no se finaliza, envia mensaje de error
  IF r_bnd_edo_act_archivo <> 0 THEN
     CALL fn_error_opera(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_bnd_oera_error
  ELSE
     CALL fn_genera_reporte_pagos_hist(g_folio) 
     RETURNING r_ruta_reporte

     CALL fn_rutas("dis") RETURNING r_c_ruta_bin, r_ruta_listados

     DISPLAY "\n### Generaci�n del Archivo ###\n"

     CALL fn_recupera_inf_proceso(g_proceso_cod, g_opera_cod)
     RETURNING v_proceso_desc,
               v_extension,
               v_opera_desc,
               v_layout,
               v_ruta_rescate,
               v_ruta_listados,
               v_usuario

     --se despliega el inicio de la etapa
     LET v_cadena = " PROCESO            : ", v_proceso_desc, "\n",
                    " OPERACI�N          : ", v_opera_desc, "\n",
                    " FOLIO              : ", g_folio, "\n",
                    " FECHA              : ", TODAY, "\n",
                    " HORA               : ", TIME(CURRENT),"\n \n \n",
                    " INICIO ETAPA       : GENERACI�N ARCHIVO",
                    " FECHA              : ", TODAY, "\n",
                    " HORA               : ", TIME(CURRENT), "\n \n \n"
     DISPLAY v_cadena

     ----Generar el archivo o interface de pagos por avance
     LET v_s_qry  = "fglrun ", r_c_ruta_bin CLIPPED, "/DISS41.42r ", g_folio
     RUN v_s_qry

     LET v_cadena = " FIN ETAPA          : GENERACI�N ARCHIVO",
                    " FECHA              : ", TODAY, "\n",
                    " HORA               : ", TIME(CURRENT), "\n"

     --Funci�n para finalizar la operacion
     CALL fn_actualiza_opera_fin(l_pid, g_proceso_cod, g_opera_cod)
     RETURNING r_b_valida
  
  END IF

END MAIN

#Objetivo: Genera reporte de cifras globales de los pagos por avance
FUNCTION fn_genera_reporte_pagos_hist(p_folio)
  DEFINE 
    p_folio                  DECIMAL(10,0) --Pagos por Avance

  DEFINE
    v_tot_nss                DECIMAL(9,0), --Total NSS
    v_tot_nss1              DECIMAL(9,0), --Total Pagos
    v_sum_pag_apo            DECIMAL(12,2),--Suma Monto Pag Aportaci�n
    v_sum_pag_amo            DECIMAL(12,2)--Suma Monto Pag Amortizaci�n

    
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

  DEFINE arr_dis_pag_hist   DYNAMIC ARRAY OF RECORD 
            estado         SMALLINT,
            desc_estado    VARCHAR(50),
            tot_estado     INTEGER,
            sum_pag_apo    DECIMAL(12,2),
            sum_pag_amo    DECIMAL(12,2)
         END RECORD
       
  DEFINE 
    v_indice_1               INTEGER,    
    v_ind_rpt                INTEGER, --Indice para el reporte  
    v_cantidad               STRING   --Variable auxiliar para cantidades

  DEFINE  
    v_ruta_envio_dis         LIKE seg_modulo.ruta_envio,
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_nom_archivo            VARCHAR(60), --nombre del archivo de salida
    v_ddmmaaaa               VARCHAR(08), --fecha del archivo de salida
    v_busca_nom_archivo      STRING,      --busca nombre de archivo
    v_precio_fondo           DECIMAL(19,14)
    
  LET v_fec_proc = TODAY

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

  --Obtiene el total de disperion a cartera integrados
  SELECT COUNT(cons_pag2.nss)
  INTO   v_tot_nss
  FROM   safre_tmp:tmp_dis_cons_pag_his2 cons_pag2
  
  --Obtiene el total de montos de avances y pendientes encontradas
  SELECT COUNT(cons_pag.nss),
         SUM(cons_pag.monto_aportacion),
         SUM(cons_pag.monto_amortizacion) 
  INTO   v_tot_nss1,
         v_sum_pag_apo,
         v_sum_pag_amo 
  FROM   safre_viv:dis_cons_pag_hist cons_pag

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "dis"

  --Se obtienen la ruta env�o del m�dulo
  SELECT ruta_envio 
  INTO   v_ruta_envio_dis
  FROM   seg_modulo
  WHERE  modulo_cod = v_modulo_cod

  
  -- se crea el nombre del archivo y posteriormente se concatena con la ruta
  LET v_nom_archivo       = "/dis_ext_cpht_" --nombre de archivo
  LET v_ddmmaaaa          = TODAY USING "yyyymmdd"  --Fecha del archivo sin separadores
  LET v_busca_nom_archivo = "dis_ext_cpht_" || v_ddmmaaaa  --Concatena nombre a buscar

  CALL fn_obtiene_nombre_archivo(v_ruta_envio_dis, v_busca_nom_archivo, p_folio)
  RETURNING v_nom_archivo
  DISPLAY "v_nom_archivo: ", v_nom_archivo
  
  --Despliega informaci�n en el log
  DISPLAY "\n ############## INTEGRACI�N ARCHIVO CONSULTA PAGOS HIST�RICOS ##############"
  LET v_cantidad = v_tot_nss
  DISPLAY " Total de registros en archivo              : ", v_cantidad CLIPPED USING "###,###,###,##&"
  DISPLAY " Nombre del archivo (Integrado)             : ", l_arch_proceso CLIPPED 

  DISPLAY "\n ############## ARCHIVO EXTRACTOR CONSULTA PAGOS HIST�RICOS ##############"
  LET v_cantidad = v_tot_nss1
  DISPLAY " Total de registros en archivo              : ", v_cantidad CLIPPED USING "###,###,###,##&"
  LET v_cantidad = v_sum_pag_apo
  DISPLAY " Total del monto de aportaci�n (Pagos)      : ", v_cantidad CLIPPED USING "###,###,###,##&.&&"
  LET v_cantidad = v_sum_pag_amo
  DISPLAY " Total del monto de amortizaci�n (Pagos)    : ", v_cantidad CLIPPED USING "###,###,###,##&.&&"
  DISPLAY " Nombre del archivo (Salida)                : ", v_nom_archivo CLIPPED 
  DISPLAY ""
  
  LET g_qrytxt = " SELECT estado, " , ASCII 10,
                   " CASE estado ", ASCII 10,
                       " WHEN 0 THEN 'NSS NO EXISTE COMO DERECHOHABIENTE' ", ASCII 10,
                       " WHEN 10 THEN 'PAGO NO REGISTRADO' ", ASCII 10,
                       " WHEN 20 THEN 'PAGO REGISTRADO' ", ASCII 10,
                       " WHEN 30 THEN 'ABONO POR REGISTRO DE PAGOS' ", ASCII 10,
                       " WHEN 40 THEN 'CARGO POR RETIROS' ", ASCII 10,
                       " WHEN 50 THEN 'CARGO POR DISPERSI�N' ", ASCII 10,
                    " END AS desc_estado, ", ASCII 10,
                      "  COUNT(estado), ", ASCII 10,
                       " SUM(monto_aportacion),", ASCII 10,
                       " SUM(monto_amortizacion) ", ASCII 10,
                " FROM dis_cons_pag_hist ", ASCII 10,
                " GROUP BY estado ", ASCII 10,
                " ORDER BY estado ASC "

   --DISPLAY g_qrytxt
                
  PREPARE prp_consulta_detalle FROM g_qrytxt

  LET v_origen_datos = v_usuario

  -- se construye la ruta del archivo
  CALL fn_rutas("dis") 
  RETURNING v_ruta_ejecutable, v_ruta_listados
  
  LET v_ruta_reporte = v_ruta_listados.trim(), "/",
                       v_origen_datos.trim(), "-",
                       "DISE22", "-",
                       l_pid USING "&&&&&", "-",
                       g_proceso_cod USING "&&&&&", "-",
                       g_opera_cod USING "&&&&&", ".pdf"                         

  DISPLAY " Ruta del reporte                         : ", v_ruta_reporte
  --Se asigna la plantilla para generar el reporte
  IF fgl_report_loadCurrentSettings(v_ruta_ejecutable CLIPPED ||"/DISE221.4rp") THEN

     CALL fgl_report_selectDevice ("PDF")        
     CALL fgl_report_selectPreview(0)
     CALL fgl_report_setOutputFileName(v_ruta_reporte)

     LET manejador_rpt = fgl_report_commitCurrentSettings()

     ### Inicia Reporte ###
     --Inicializamos variables para suma de totales
     LET v_indice_1    = 1 
            
     --Inicia el reporte de registros con rechazo
     START REPORT rep_cons_pag_hist TO XML HANDLER manejador_rpt
       DECLARE cur_cons_pag_hist CURSOR FOR prp_consulta_detalle   
       FOREACH cur_cons_pag_hist INTO arr_dis_pag_hist[v_indice_1].estado,
                                   arr_dis_pag_hist[v_indice_1].desc_estado,
                                   arr_dis_pag_hist[v_indice_1].tot_estado,
                                   arr_dis_pag_hist[v_indice_1].sum_pag_apo,
                                   arr_dis_pag_hist[v_indice_1].sum_pag_amo
                                   
         LET v_indice_1 = v_indice_1  + 1
                  
       END FOREACH

       CALL arr_dis_pag_hist.deleteElement(v_indice_1)         

       LET v_indice_1 = v_indice_1 - 1

       IF v_indice_1 = 0 THEN 
          DISPLAY "No se puede generar el reporte por falta de informaci�n."
       END IF  

       IF v_usuario IS NULL THEN 
          LET v_usuario = "infonavit"
       END IF 
                  
       FOR v_ind_rpt = 1 TO v_indice_1
           OUTPUT TO REPORT rep_cons_pag_hist(g_folio,
                                              v_usuario,
                                              v_fec_proc,
                                              arr_dis_pag_hist[v_ind_rpt].*,
                                              r_edo_archivo,
                                              v_tot_nss,
                                              v_tot_nss1,
                                              v_nom_archivo,
                                              v_sum_pag_apo,
                                              v_sum_pag_amo)
       END FOR 
     FINISH REPORT rep_cons_pag_hist
  ELSE
     DISPLAY "no funciono"
     EXIT PROGRAM
  END IF

  RETURN v_ruta_reporte
END FUNCTION

#OBJETIVO: Generar el reporte de Avances
REPORT rep_cons_pag_hist(v_rfolio,
                         v_rusuario,
                         v_rfec_proc,
                         rec_dis_pag_hist,
                         r_redo_archivo,
                         v_tot_nss,
                         v_tot_nss1,
                         v_nom_archivo,
                         v_sum_pag_apo,
                         v_sum_pag_amo)
                       
   DEFINE v_rfolio            DECIMAL(9,0),
          v_rusuario          VARCHAR(30),   --Usuario de proceso
          v_rfec_proc         DATE,          --Fecha de procesos
          v_tot_nss           DECIMAL(9,0),
          v_tot_nss1          DECIMAL(9,0),
          v_nom_archivo       CHAR(40)

   DEFINE r_redo_archivo     CHAR(50)--Descripci�n del estado del archivo

   DEFINE rec_dis_pag_hist  RECORD 
            estado            SMALLINT,
            desc_estado       VARCHAR(50), 
            tot_estado        INTEGER,
            sum_pag_apo       DECIMAL(12,2),
            sum_pag_amo       DECIMAL(12,2) 
         END RECORD

   DEFINE v_sum_pag_apo        DECIMAL(12,2)
   DEFINE v_sum_pag_amo        DECIMAL(12,2)

  FORMAT
    FIRST PAGE HEADER
      PRINTX v_rusuario
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
       
      PRINTX v_rfolio
      PRINTX l_arch_proceso
      PRINTX v_nom_archivo

   PAGE HEADER
      PRINTX v_rusuario
      PRINTX v_rfec_proc     USING "dd-mm-yyyy" 
       
      PRINTX v_rfolio
      PRINTX l_arch_proceso
      PRINTX v_nom_archivo

    ON EVERY ROW
       PRINTX rec_dis_pag_hist.estado
       PRINTX rec_dis_pag_hist.desc_estado
       PRINTX rec_dis_pag_hist.tot_estado
       PRINTX rec_dis_pag_hist.sum_pag_apo
       PRINTX rec_dis_pag_hist.sum_pag_amo
       
    ON LAST ROW
      PRINTX v_tot_nss1
      PRINTX v_sum_pag_apo
      PRINTX v_sum_pag_amo
        
END REPORT


FUNCTION fn_obtiene_nombre_archivo(p_ruta_envio_dis, p_busca_nom_archivo, p_folio)
  DEFINE 
    p_ruta_envio_dis         LIKE seg_modulo.ruta_envio, --Ruta donde se genera archivo
    p_busca_nom_archivo      VARCHAR(40), --Nombre del archivo a buscar(nombre||fecha)
    p_folio                  DECIMAL(10,0),
    v_cmd                    STRING,      --Cadena de comando a ejecutar
    v_consecutivo            INTEGER,     --Consecutivo del archivo por d�a
    v_nom_archivo_sal        VARCHAR(40)  --Nombre del archivo de salida

  DEFINE 
    fn                       CHAR(32)  --Almacena el nombre completo del nombre del archivo en el servidor con su extensi�n

  DEFINE 
    ch                       base.Channel  --Canal de lectura

  DEFINE  
    v_reg_dia                CHAR(03), --Parametro consecutivo de registro por dia
    v_modulo_cod             LIKE seg_modulo.modulo_cod,
    v_ddmmaaaa               VARCHAR(08) --fecha del archivo de salida

  --Obtiene nombre del archivo de salida
  LET v_modulo_cod = "dis"
  
  --LET v_cmd = "ls -lrt ",p_ruta_envio_dis CLIPPED,"/ | grep -i '",p_busca_nom_archivo CLIPPED,"' |awk '{print $9}'"

  LET ch = base.Channel.create()

  CALL ch.setDelimiter(".")
  CALL ch.openPipe(v_cmd,"r")

  {WHILE ch.read([fn])
    LET v_consecutivo     = fn[30,32]  --Posici�n del consecutivo dentro de la cadena
    LET v_nom_archivo_sal = fn
  END WHILE}

  CALL ch.close()

  LET v_consecutivo = v_consecutivo + 1  --Incrementa consecutivo del d�a

  IF length(v_consecutivo) = 0 THEN  --Si es el primero del d�a
     LET v_consecutivo = 1
  END IF

  LET v_nom_archivo_sal = "dis_ext_cpht_" --nombre de archivo
  LET v_ddmmaaaa        = TODAY USING "yyyymmdd"  --Fecha del archivo sin separadores
  --LET v_reg_dia         = v_consecutivo USING "&&&"  --Consecutivo del d�a de numerico a char
  LET v_nom_archivo_sal = v_nom_archivo_sal CLIPPED || v_ddmmaaaa ||"_"|| p_folio||"."|| v_modulo_cod

  RETURN v_nom_archivo_sal --Regresa el nombre del archivo del d�a

END FUNCTION

FUNCTION fn_borra_crea_tablas_tmp()

   WHENEVER ERROR CONTINUE;

   DROP TABLE dis_cons_pag_hist;
   DROP TABLE dis_mov_cons_pag_h;

   WHENEVER ERROR STOP;

END FUNCTION

