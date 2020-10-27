################################################################################
#Modulo            => SEP                                                      #
#Programa          => PRTX31                                                   #
#Objetivo          => Generar batch de archivo contingente Portavilidad V92    # 
#Fecha inicio      => Noviembre 23 de 2018                                     #
################################################################################

DATABASE safre_viv

DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE sep_batch_contabilidad.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       v_enc_batch_con   RECORD
         v_fecha         STRING,
         v_archivo       STRING,
         v_folio_con     INTEGER,--LIKE glo_folio.folio,
         v_total_expedientes INTEGER,
         v_total_registros   INTEGER
       END RECORD,
       v_expedientes DYNAMIC ARRAY OF DECIMAL(9,0) ,
       v_txt STRING,
       r_id_prt_solicitud_cedente LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
       r_id_prt_traspaso_cedente LIKE prt_traspaso_cedente.id_prt_traspaso_cedente,
       r_ind        INTEGER,
       r_diag       CHAR(3),
       r_error_sql  INTEGER,
       r_error_isam INTEGER,
       r_msg_sql    VARCHAR(254),
       r_estado_destino SMALLINT,
       n_id_cred    dec(10,0) ,
       v_tot_aivs_92 decimal(16,6),
       v_tot_pesos_92 decimal(16,2)
 

     CONSTANT v_tipo_trans CHAR(4) = "8871" 
     CONSTANT c_bus_proceso_cod SMALLINT = 2
     CONSTANT c_bus_opera_cod SMALLINT   = 428 
     CONSTANT C_DIAGNOSTICO_INTERNO_1 CHAR(3) = "1"    
     DEFINE   v_pso_id dec(10,0)     
     
     DEFINE rec_prt_solicitud_cedente RECORD LIKE prt_solicitud_cedente.*
     DEFINE rec_prt_traspaso_cedente RECORD LIKE prt_traspaso_cedente.*

     DEFINE rec_archivo RECORD 
            tipo_registro CHAR(2),
            folio_cliente CHAR(50),
            nss_imss  char(11),
            paterno char(40),
            materno char(40) ,
            nombre  char(40),
            curp    char(18),
            tipo_operacion  char(2),
            instituto_origen char(3),
            id_credito_infonavit char(10),
            filler_1 char(50),
            id_credito_fov char(10),
            filler_2 char(50),
            f_credito    char(8)    ,
            f_valor_transferencia char(8),
            diagnostico  char(3),
            motivo_rechazo char(3),
            resultado_operacion char(2),
            monto_pesos_infonavit_92 char(12),
            valor_aiv_infonavit_92 char(15),
            f_valor_aiv_infonavit_92 char(8),
            monto_aivis_infonavit_92 char(15),
            monto_pesos_fov_92 char(12),
            valor_aiv_fov_92 char(15),
            f_valor_aiv_fov_92 char(8),
            monto_aivs_fov_92 char(15),
            monto_credito_infonavit char(12),
            monto_credito_fov char(12) ,
            filler_3 char(50)
     END RECORD            

DEFINE g_linea char(650)     

MAIN
DEFINE v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_conteo_registros INTEGER,
       v_consulta        STRING,
       r_folio           INTEGER,--LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       v_archivo_salida  STRING,
       v_canal           base.Channel,
       v_ruta_envio      LIKE seg_modulo.ruta_envio,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_tipo_registro   CHAR(1),
       v_fecha_actual    STRING,
       v_fecha_ctrl      CHAR(10),
       v_espacios        STRING,
       v_indice          INTEGER,
       v_batch_cont RECORD
          v_nss        LIKE sep_batch_contabilidad.nss,
          v_id_credito LIKE sep_batch_contabilidad.id_credito,
          v_bimestre   LIKE sep_batch_contabilidad.bimestre,
          v_clave      LIKE sep_batch_contabilidad.clave,
          v_nrp        LIKE sep_batch_contabilidad.nrp,
          v_monto      LIKE sep_batch_contabilidad.monto
       END RECORD,
       v_linea  STRING,
       v_total_registros2   INTEGER,
       v_total_amortizacion_aux VARCHAR(17),--LIKE sep_batch_contabilidad.monto,
       v_total_amortizacion STRING,--LIKE sep_batch_contabilidad.monto,
       v_tot_reg_archivo    INTEGER,
      -- v_id_expediente      LIKE sep_expediente.id_expediente,
      -- v_senial             SMALLINT,
      -- v_ind                SMALLINT,
      -- v_diag               CHAR(3),
      -- v_estado_destino     SMALLINT,
       r_resultado_opera    SMALLINT
       --v_comando            STRING



       

  CALL STARTLOG(p_usuario CLIPPED|| ".PRTX31.log")   
             
   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)

   LET v_txt = "EXECUTE PROCEDURE sp_prt_solicita_traspaso_cedente_conts92(?,?,?,?)"
   PREPARE prp_solicita_traspaso_conts92 FROM v_txt
   
   LET v_txt = " UPDATE prt_solicitud_cedente",
               "    SET diagnostico_interno = ?",
               "  WHERE id_prt_solicitud_cedente = ?"
   PREPARE prp_actualiza_solicitud FROM v_txt
 
   WHENEVER ERROR CONTINUE
   DROP TABLE safre_tmp:tmp_sep_integrados
   CREATE TABLE safre_tmp:tmp_sep_integrados(folio DECIMAL(9,0),
                                             id_det_02_op27 DECIMAL(9),
                                             tipo_ocurrencia SMALLINT,
                                             diag_confronta CHAR(2),
                                             clasifica_separacion_ant CHAR(1),
                                             clasifica_separacion_nva CHAR(1)
                                             );

    
   LET v_consulta = " SELECT COUNT(*)
                       FROM prt_solicitud_cedente a
                      WHERE a.estado  = 40
                        AND (a.pesos_saldo_viv97_infonavit in (0,'',' ') OR 
                             a.pesos_saldo_viv97_infonavit is null)
                            AND f_ini_tramite = TODAY "
                        
   PREPARE prp_rec_conteo FROM v_consulta
   EXECUTE prp_rec_conteo INTO v_conteo_registros

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   # Imprime datos en el log
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "TOTAL DE REGISTROS A PROCESAR: ",v_conteo_registros

   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   
   LET v_fecha_ctrl = MONTH(TODAY) USING "&&" CLIPPED,"-",
                      DAY(TODAY) USING "&&" CLIPPED,"-",
                      v_fecha_actual.trim()
                      
   LET v_fecha_actual = YEAR(TODAY) CLIPPED,
                        MONTH(TODAY) USING "&&" CLIPPED,
                        DAY(TODAY) USING "&&" CLIPPED
                      
      
   LET v_fecha_actual = v_fecha_actual.trim()
   
   --LET v_archivo_salida = v_fecha_actual,
   --                       "_CONTINGENTE_PORTA_V92.txt"
   
   --LET v_archivo_salida = "S",TODAY USING "YYMMDD",".SCONTINPORTAB.C001"
     LET v_archivo_salida = "CONTINPORTAB.GDG"   
   
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "prt"

   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )


   LET v_txt = " SELECT a.* 
                 FROM prt_solicitud_cedente a 
                 WHERE a.estado = 40
                 AND (a.pesos_saldo_viv97_infonavit in (0,'',' ') OR 
                      a.pesos_saldo_viv97_infonavit is null)
                     AND a.f_ini_tramite = TODAY "

   PREPARE qry_envio FROM v_txt

   DECLARE cur_envio CURSOR FOR qry_envio

   FOREACH cur_envio INTO rec_prt_solicitud_cedente.*
   
         EXECUTE prp_solicita_traspaso_conts92 USING rec_prt_solicitud_cedente.id_prt_solicitud_cedente,
                                       c_bus_proceso_cod ,
                                       c_bus_opera_cod   ,
                                       p_usuario         
                                  INTO r_ind,
                                       r_diag,
                                       r_error_sql,
                                       r_error_isam,
                                       r_msg_sql,
                                       r_estado_destino ,
                                       r_id_prt_traspaso_cedente

        IF( r_error_sql <> 0 )THEN
           DISPLAY "Error solicitar traspaso:"
           DISPLAY "ID prt solicitud: ",r_id_prt_solicitud_cedente
           DISPLAY "Codigo: ",r_error_sql
           DISPLAY "Mensaje: ",r_msg_sql

           EXECUTE prp_actualiza_solicitud USING C_DIAGNOSTICO_INTERNO_1,
                                                 r_id_prt_solicitud_cedente
                                            
           --LET g_errores.v_sql_error  = r_error_sql
           --LET g_errores.v_isam_error = r_error_isam
           --LET g_errores.v_msg_error  = r_msg_sql
           --LET g_errores.v_ind        = r_ind
           --LET g_errores.v_diag       = "Error al solicitar traspaso cedente "||p_id_prt_solicitud_cedente
     
           --CALL fn_genera_error_bus(p_id_bus_solicitud_tramite,
                                    --C_BUS_PROCESO_COD_SOL_TRASP,
                                    --C_BUS_OPERACION_COD_SOL_TRASP,
                                    --g_errores.*)
           --INITIALIZE g_errores TO NULL
        END IF

        SELECT a.* 
        INTO   rec_prt_traspaso_cedente.*        
        FROM prt_traspaso_cedente a 
        WHERE a.id_prt_traspaso_cedente = r_id_prt_traspaso_cedente 
        
        LET rec_archivo.tipo_registro = "02"
        LET g_linea[1,2] = "02"
        LET rec_archivo.folio_cliente = r_id_prt_traspaso_cedente USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
        LET g_linea[3,52] =  r_id_prt_traspaso_cedente USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
        LET rec_archivo.nss_imss      = rec_prt_solicitud_cedente.nss
        LET g_linea[53,63] = rec_prt_solicitud_cedente.nss
        LET rec_archivo.paterno       = rec_prt_solicitud_cedente.paterno
        LET g_linea[64,103] = rec_prt_solicitud_cedente.paterno
        LET rec_archivo.materno       = rec_prt_solicitud_cedente.materno
        LET g_linea[104,143] =    rec_prt_solicitud_cedente.materno
        LET rec_archivo.nombre       = rec_prt_solicitud_cedente.nombre
        LET g_linea[144,183] =    rec_prt_solicitud_cedente.nombre
        LET rec_archivo.curp         = rec_prt_solicitud_cedente.curp
        LET g_linea[184,201] =    rec_prt_solicitud_cedente.curp
        LET rec_archivo.tipo_operacion = "01"  -- total
        LET g_linea[202,203] =    "01"
        LET rec_archivo.instituto_origen = "001" -- infonavit
        LET g_linea[204,206] =    "001"
        LET rec_archivo.id_credito_infonavit = 10 SPACES       
        LET g_linea[207,216] =  10 SPACES
        LET rec_archivo.filler_1  = 50 SPACES        
        LET g_linea[217,266] =  50 SPACES
        LET n_id_cred = 0        
        LET n_id_cred = rec_prt_solicitud_cedente.id_credito_fovissste 
        LET rec_archivo.id_credito_fov = n_id_cred USING "&&&&&&&&&&"
        LET g_linea[267,276] = n_id_cred USING "&&&&&&&&&&"
        LET rec_archivo.filler_2  = 50 SPACES        
        LET g_linea[277,326] = 50 SPACES
        LET rec_archivo.f_credito  = rec_prt_solicitud_Cedente.f_originacion_fovissste USING "YYYYMMDD"
        LET g_linea[327,334] = rec_prt_solicitud_Cedente.f_originacion_fovissste USING "YYYYMMDD"
        LET rec_archivo.f_valor_transferencia = rec_prt_traspaso_cedente.f_precio_aiv_infonavit92 USING "YYYYMMDD"
        LET g_linea[335,342] = rec_prt_traspaso_cedente.f_precio_aiv_infonavit92 USING "YYYYMMDD"
        
        SELECT FIRST 1 a.id_prt_traspaso_cedente,a.diag_procesar
        INTO v_pso_id,rec_archivo.diagnostico
        FROM prt_traspaso_cedente a 
        WHERE a.nss = rec_prt_solicitud_cedente.nss   
        AND   a.estado = 40
        AND   a.diag_procesar IN ("100","101")     
        ORDER BY a.id_prt_traspaso_cedente

        IF rec_archivo.diagnostico IS NULL THEN 
           LET rec_archivo.diagnostico = "100"
        END IF

        LET g_linea[343,345] = rec_archivo.diagnostico
        LET rec_archivo.motivo_rechazo = 3 SPACES         
        LET g_linea[346,348] = 3 SPACES
        LET rec_archivo.resultado_operacion = 2 SPACES        
        LET g_linea[349,350] = 2 SPACES
        LET rec_archivo.monto_pesos_infonavit_92 = rec_prt_traspaso_cedente.mto_pesos_infonavit92 * 100 USING "&&&&&&&&&&&&"
        LET g_linea[351,362] = rec_prt_traspaso_cedente.mto_pesos_infonavit92 * 100 USING "&&&&&&&&&&&&"
        LET rec_archivo.valor_aiv_infonavit_92        = rec_prt_traspaso_cedente.precio_aiv_infonavit92 * 1000000 USING "&&&&&&&&&&&&&&&"
        LET g_linea[363,377] = rec_prt_traspaso_cedente.precio_aiv_infonavit92 * 1000000 USING "&&&&&&&&&&&&&&&"
        LET rec_archivo.f_valor_aiv_infonavit_92      = rec_prt_traspaso_cedente.f_precio_aiv_infonavit92  USING "YYYYMMDD"
        LET g_linea[378,385] = rec_prt_traspaso_cedente.f_precio_aiv_infonavit92  USING "YYYYMMDD"
        LET rec_archivo.monto_aivis_infonavit_92      = rec_prt_traspaso_cedente.mto_aivs_infonavit92  * 1000000  USING "&&&&&&&&&&&&&&&"  
        LET g_linea[386,400] = rec_prt_traspaso_cedente.mto_aivs_infonavit92  * 1000000  USING "&&&&&&&&&&&&&&&"
        LET rec_archivo.monto_pesos_fov_92      = 12 SPACES
        LET g_linea[401,412] = 12 SPACES
        LET rec_archivo.valor_aiv_fov_92        = 15 SPACES 
        LET g_linea[413,427] = 15 SPACES
        LET rec_archivo.f_valor_aiv_fov_92      = "00010101" 
        LET g_linea[428,435] = rec_archivo.f_valor_aiv_fov_92
        LET rec_archivo.monto_aivs_fov_92       = 15 SPACES
        LET g_linea[436,450] = 15 SPACES
        LET rec_archivo.monto_credito_infonavit = 12 SPACES
        LET g_linea[451,462] = 12 SPACES
        LET rec_archivo.monto_credito_fov       = rec_prt_traspaso_cedente.sdo_insoluto_fovissste * 100 USING "&&&&&&&&&&&&"
        LET g_linea[463,474] = rec_prt_traspaso_cedente.sdo_insoluto_fovissste * 100 USING "&&&&&&&&&&&&"
        LET g_linea[475,650] = 176 SPACES
    
   
   LET v_tot_reg_archivo = 0

   CALL v_canal.writeLine(g_linea)
   
   LET v_tot_reg_archivo = v_tot_reg_archivo + 1
   
   END FOREACH 
   FREE cur_envio

   CALL v_canal.close()

   SELECT sum(b.mto_aivs_infonavit92),
          sum(b.mto_pesos_infonavit92)
   INTO v_tot_aivs_92 ,
        v_tot_pesos_92 
   FROM prt_solicitud_cedente a ,
        prt_traspaso_cedente b
   WHERE a.estado = 44 
   AND   a.pesos_saldo_viv97_infonavit = 0
   AND   a.id_prt_solicitud_cedente = b.id_prt_solicitud_cedente
   AND   a.f_ini_tramite = TODAY 
   
   DISPLAY "TOTAL AIVS  VIV92 :",v_tot_aivs_92
   DISPLAY "TOTAL PESOS VIV92 :",v_tot_pesos_92
   
   #******** REPORTE *************
--   CALL fn_configura_rpt_ajuste_credito()

   DISPLAY ""
   DISPLAY "RUTA: ",v_ruta_envio
   DISPLAY "ARCHIVO GENERADO: ",v_archivo_salida
   DISPLAY ""

   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   ELSE
      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '',
                             'Generar archivo para ajustes al crédito',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||v_fecha_actual||
                             'Fecha Fin    : '||DATE
                             )
   END IF      

END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP26                                                   #
#Descripcion       => Reporte de ajuste al crédito                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 18, 2012                                           #
################################################################################
FUNCTION fn_configura_rpt_ajuste_credito()
DEFINE r_ruta_ejecutable LIKE seg_modulo.ruta_bin, 
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       v_nom_reporte     STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_consulta        STRING,
       v_movimientos     RECORD
         v_consecutivo   INTEGER,
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_invadido      LIKE sep_nss_expediente.nss,
         v_asociado      LIKE sep_nss_expediente.nss,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_monto         LIKE sep_batch_contabilidad.monto
         --v_estado_desc   LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_ajustes RECORD
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         --v_estado_desc   LIKE sep_estado_expediente.descripcion,
         v_monto         LIKE sep_batch_contabilidad.monto
       END RECORD,
       v_indice          INTEGER,
       v_gen_rpt         BOOLEAN
       

   LET v_gen_rpt = FALSE
   # CREACIÓN DE REPORTE
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPP261.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario CLIPPED, "-SEPP26-", 
                          p_pid USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_genera_rpt_ajuste_credito TO XML HANDLER v_manejador_rpt
         LET v_consulta = "\n SELECT nss",
                          "\n   FROM sep_nss_expediente",
                          "\n  WHERE id_expediente = ?",
                          "\n    AND tipo_nss = 1"
         PREPARE prp_rec_nss_inv FROM v_consulta

         LET v_consulta = "\n SELECT nss",
                          "\n   FROM sep_nss_expediente",
                          "\n  WHERE id_expediente = ?",
                          "\n    AND tipo_nss = 2"
         PREPARE prp_rec_nss_asc FROM v_consulta

         LET v_consulta = "\n SELECT exp.id_expediente,con.nss,",
                          "\n        SUM(con.monto)",
                          "\n   FROM sep_batch_contabilidad con JOIN sep_expediente exp",
                          "\n     ON con.id_expediente = exp.id_expediente",
                          --"\n        LEFT OUTER JOIN sep_estado_expediente edo",
                          --"\n     ON edo.estado = exp.estado",
                          "\n  WHERE exp.estado in (40,45,46,50) ",
                          "\n    AND con.ind_envio = 0",
                          "\n  GROUP BY 1,2"
         PREPARE prp_recupera_ajustes FROM v_consulta
         DECLARE cur_recupera_ajustes CURSOR FOR prp_recupera_ajustes
         LET v_indice = 1
         FOREACH cur_recupera_ajustes INTO v_ajustes.*
            LET v_gen_rpt = TRUE
            LET v_movimientos.v_consecutivo   = v_indice
            LET v_expedientes[v_indice] = v_ajustes.v_id_expediente
            LET v_movimientos.v_id_expediente = v_ajustes.v_id_expediente
            LET v_movimientos.v_nss_ajuste    = v_ajustes.v_nss_ajuste 
            
            LET v_movimientos.v_monto         = v_ajustes.v_monto
            # Recupera invadido del expediente
            EXECUTE prp_rec_nss_inv USING v_ajustes.v_id_expediente 
                                     INTO v_movimientos.v_invadido
            # Recupera asociado del expediente
            EXECUTE prp_rec_nss_asc USING v_ajustes.v_id_expediente 
                                     INTO v_movimientos.v_asociado
            
            OUTPUT TO REPORT fn_genera_rpt_ajuste_credito(v_movimientos.*)
            LET v_indice = v_indice + 1
            
         END FOREACH
         IF(v_gen_rpt = FALSE)THEN
            DISPLAY "\nNO SE ENCONTRARON REGISTROS PARA GENERAR REPORTE\n"
         END IF
      FINISH REPORT fn_genera_rpt_ajuste_credito
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP26                                                   #
#Descripcion       => Reporte de ajuste al crédito                             #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 18, 2012                                           #
################################################################################
REPORT fn_genera_rpt_ajuste_credito(v_movimientos)
DEFINE v_movimientos     RECORD
         v_consecutivo   INTEGER,
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_invadido      LIKE sep_nss_expediente.nss,
         v_asociado      LIKE sep_nss_expediente.nss,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_monto         LIKE sep_batch_contabilidad.monto
         --v_estado_desc   LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_pagina          SMALLINT,
       v_total_registros INTEGER,
       v_monto_total     LIKE sep_batch_contabilidad.monto

   FORMAT

      FIRST PAGE HEADER
         PRINTX v_enc_batch_con.*

      ON EVERY ROW
         PRINTX v_movimientos.*
         LET v_total_registros = COUNT(*) 
         
         LET v_monto_total = SUM(v_movimientos.v_monto) 
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         PRINTX v_total_registros, v_monto_total

END REPORT