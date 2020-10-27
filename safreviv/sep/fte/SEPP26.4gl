--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-06-2012
--===============================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPP26                                                   #
#Objetivo          => Generar batch de archivo ajustes al crédito              # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => Junio 16, 2012                                           #
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
       v_expedientes DYNAMIC ARRAY OF DECIMAL(9,0)
       

MAIN
DEFINE v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_conteo_registros INTEGER,
       v_consulta        STRING,
       r_folio           INTEGER,--LIKE glo_ctr_archivo.folio, # Folio generado por la operacion
       v_archivo_salida  STRING,
       v_archivo_salida_axway  STRING,
       v_ruta_script_axway  STRING,
       v_canal           base.Channel,
       v_ruta_envio      LIKE seg_modulo.ruta_envio,
       v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_tipo_registro   CHAR(1),
       v_fecha_actual    STRING,
       v_fecha_ctrl      CHAR(10),
       v_espacios        STRING,
       v_indice          INTEGER,
       v_f_pago          CHAR(8)    ,   
       v_batch_cont RECORD
          v_nss        LIKE sep_batch_contabilidad.nss,
          v_id_credito LIKE sep_batch_contabilidad.id_credito,
          v_bimestre   LIKE sep_batch_contabilidad.bimestre,
          v_f_pago     DATE,
          v_clave      LIKE sep_batch_contabilidad.clave,
          v_nrp        LIKE sep_batch_contabilidad.nrp,
          v_monto      LIKE sep_batch_contabilidad.monto
       END RECORD,
       v_cad_monto CHAR(10),
       v_linea  STRING,
       v_total_registros2   INTEGER,
       v_total_amortizacion_aux VARCHAR(17),--LIKE sep_batch_contabilidad.monto,
       v_total_amortizacion STRING,--LIKE sep_batch_contabilidad.monto,
       v_total_amortizacion_d DECIMAL(16,2),--LIKE sep_batch_contabilidad.monto,
       v_tot_reg_archivo    INTEGER,
      -- v_id_expediente      LIKE sep_expediente.id_expediente,
      -- v_senial             SMALLINT,
      -- v_ind                SMALLINT,
      -- v_diag               CHAR(3),
      -- v_estado_destino     SMALLINT,
       r_resultado_opera    SMALLINT
      -- v_comando            STRING
       
CONSTANT v_tipo_trans CHAR(4) = "8871" 

  CALL STARTLOG(p_usuario CLIPPED|| ".SEPP26.log")   
             
   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)
   
   WHENEVER ERROR CONTINUE
   DROP TABLE safre_tmp:tmp_sep_integrados
   CREATE TABLE safre_tmp:tmp_sep_integrados(folio DECIMAL(9,0),
                                             id_det_02_op27 DECIMAL(9),
                                             tipo_ocurrencia SMALLINT,
                                             diag_confronta CHAR(2),
                                             clasifica_separacion_ant CHAR(1),
                                             clasifica_separacion_nva CHAR(1)
                                             );

    
   # Conteo de regidtros a procesar
   {LET v_consulta = "\n SELECT COUNT(*)",
                    "\n   FROM sep_expediente exp JOIN sep_batch_contabilidad con",
                    "\n     ON con.id_expediente = exp.id_expediente",
                    "\n  WHERE exp.estado = 50",
                    "\n    AND con.ind_envio = 0"}
                    
   LET v_consulta = "\n SELECT COUNT(id_expediente)",
                    "\n   FROM sep_expediente",
                    "\n  WHERE estado in (40,45,46,50) ",
                    "\n    AND ind_ajuste = 2  ",                  
                    "\n    AND id_expediente IN (SELECT id_expediente",
                    "\n                            FROM sep_batch_contabilidad",
                    "\n                           WHERE ind_envio = 0)"
   PREPARE prp_rec_conteo FROM v_consulta
   EXECUTE prp_rec_conteo INTO v_conteo_registros

   # Encabezado reporte
   LET v_enc_batch_con.v_total_expedientes = v_conteo_registros

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   # Imprime datos en el log
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "TOTAL DE EXPEDIENTES A PROCESAR: ",v_conteo_registros

   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario)
                        RETURNING r_folio
   # Encabezado reporte
   LET v_enc_batch_con.v_folio_con = r_folio 
                        
   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   
   LET v_fecha_ctrl = MONTH(TODAY) USING "&&" CLIPPED,"-",
                      DAY(TODAY) USING "&&" CLIPPED,"-",
                      v_fecha_actual.trim()
                      
   LET v_fecha_actual = YEAR(TODAY) CLIPPED,
                        MONTH(TODAY) USING "&&" CLIPPED,
                        DAY(TODAY) USING "&&" CLIPPED
                      
      
   LET v_fecha_actual = v_fecha_actual.trim()
   # encabezado de reporte
   LET v_enc_batch_con.v_fecha = v_fecha_actual 
   
   LET v_archivo_salida       = v_fecha_actual, "_BATCH_AJUSTE_475.txt"
   LET v_archivo_salida_axway = "BATCH_AJUSTE_475.txt"
   LET v_ruta_script_axway    = "/opt/Interpel/Scripts/SOA19056QA.txt"
   --LET v_ruta_script_axway    = "/opt/Interpel/Scripts/SOA19056.txt"
   
   # Encabezado reporte
   LET v_enc_batch_con.v_archivo = v_archivo_salida
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = "sep"

   LET v_canal = base.Channel.create()
   CALL v_canal.openFile(v_ruta_envio CLIPPED||"/"||v_archivo_salida, "w" )

   LET v_tipo_registro = '0'
   LET v_espacios = " "
   # genera cadena con 76 espacios
   FOR v_indice = 1 TO 75
      LET v_espacios = v_espacios," "
   END FOR

   # Escribe primer registro
   CALL v_canal.writeLine(v_tipo_registro||v_fecha_actual||v_espacios)

   LET v_tipo_registro = 1
   LET v_espacios = " "
   # genera cadena con 72 espacios
   FOR v_indice = 1 TO 71
      LET v_espacios = v_espacios," "
   END FOR
   # Escribe segundo registro
   CALL v_canal.writeLine(v_tipo_registro||v_tipo_trans||v_fecha_actual||v_espacios)



   # Escribe tercer registro (detalle)
   LET v_tipo_registro = '2'
   LET v_tot_reg_archivo = 0
   LET v_consulta = "\n SELECT nss,id_credito,",
                    "\n        bimestre,f_pago,clave,nrp,monto",
                    "\n   FROM sep_batch_contabilidad",
                    "\n  WHERE ind_envio = 0",
                    "\n    AND id_expediente ",
                    "\n         IN (SELECT id_expediente",
                    "\n               FROM sep_expediente",
                    "\n              WHERE estado in (40,45,46,50)",
                    "\n                AND ind_ajuste = 2   )"

   PREPARE prp_rec_batch_con FROM v_consulta
   DECLARE cur_rec_batch_con CURSOR FOR prp_rec_batch_con
   FOREACH cur_rec_batch_con INTO v_batch_cont.*
   
      LET v_f_pago = v_batch_cont.v_f_pago USING"YYYYMMDD"
      INITIALIZE v_cad_monto TO NULL
      LET v_cad_monto = v_batch_cont.v_monto * 100 # Pasa los 2 decimales a enteros
      
      LET v_linea = v_tipo_registro CLIPPED,
                    v_batch_cont.v_nss        USING "&&&&&&&&&&&" CLIPPED,    # NSS
                    v_batch_cont.v_id_credito USING "&&&&&&&&&&",     # crédito
                    v_batch_cont.v_bimestre USING "&&&&&&", # Preiodo de pago
                    v_fecha_actual,                         # Fecha de pago
                    --v_f_pago    ,# SACI2019-38 Debe ir fecha actual
                    v_batch_cont.v_clave USING "###",        # entidad recaudadora
                    v_batch_cont.v_nrp USING "&&&&&&&&&&&", # NRP (Patron)
                    "000000000", # Importe Aportacion
                    --v_batch_cont.v_monto USING "&&&&&&&&&", # Importe amortizacion
                    v_cad_monto USING "&&&&&&&&&", # Importe amortizacion
                    "000000",    # Folio sua
                    " ",         # indicador de seguridad
                    "          " # clave de rechazo
      IF(v_linea IS NULL)THEN
         LET v_linea = " "
      END IF
      CALL v_canal.writeLine(v_linea)
      LET v_tot_reg_archivo = v_tot_reg_archivo + 1
   END FOREACH 
   FREE cur_rec_batch_con

   # Encabezado reporte
   LET v_enc_batch_con.v_total_registros = v_tot_reg_archivo
   
   

   # Escribe cuarto registro (sumario 1)
   LET v_tipo_registro = '3'
   LET v_total_registros2 = 0
   LET v_enc_batch_con.v_total_registros = 0
   # Total registros tipo 2
   SELECT COUNT(*)
     INTO v_total_registros2
     FROM sep_batch_contabilidad
    WHERE ind_envio = 0
      AND id_expediente 
           IN (SELECT id_expediente
                 FROM sep_expediente
                WHERE estado IN (40,45,46,50)
                  AND ind_ajuste = 2 )
      AND tpo_registro = 2
   LET v_enc_batch_con.v_total_registros = v_total_registros2
  DISPLAY "total registros: ",v_enc_batch_con.v_total_registros 
   LET v_total_amortizacion_aux = '0.000000'
   SELECT SUM(monto)
     --INTO v_total_amortizacion_aux
     INTO v_total_amortizacion_d
     FROM sep_batch_contabilidad
    WHERE ind_envio = 0
      AND id_expediente 
           IN (SELECT id_expediente
                 FROM sep_expediente
                WHERE estado IN (40,45,46,50)
                  AND ind_ajuste = 2)
      AND tpo_registro = 2
   IF(v_total_amortizacion_d IS NULL)THEN
      LET v_total_amortizacion_d = 0
   END IF
   
   LET v_espacios = " "
   # genera cadena con 22 espacios
   FOR v_indice = 1 TO 21
      LET v_espacios = v_espacios," "
   END FOR

   LET v_total_amortizacion = v_total_amortizacion_d * 100 USING"&&&&&&&&&&&&" 
   
   LET v_linea = v_tipo_registro CLIPPED,   
                 v_tipo_trans CLIPPED,     # clave de transaccion
                 v_total_registros2 USING "&&&&&&&&&&" CLIPPED,  # Total de registros
                 "000000000000" CLIPPED,                         # Total de aportacion
                 v_total_amortizacion CLIPPED, # Total de amortizacion
                 "000000000000" CLIPPED,                            # Total de seguridad
                 v_total_amortizacion CLIPPED, # Total general = aportacion + amortizacion + seguridad   
                 v_espacios                                 # Filer

   IF(v_linea IS NULL)THEN
      LET v_linea = " "
   END IF
   CALL v_canal.writeLine(v_linea)

   # Escribe cuarto registro (sumario 2)
   LET v_tipo_registro = '4'
   LET v_tot_reg_archivo = v_tot_reg_archivo + 4 # v_tot_reg_archivo = Total de registros en archivo (v_tipo_registro 0,1,2,3,4)
   LET v_total_amortizacion_aux = '0.000000'
   SELECT SUM(monto)
     INTO v_total_amortizacion_d
     FROM sep_batch_contabilidad
    WHERE ind_envio = 0
      AND id_expediente 
           IN (SELECT id_expediente
                 FROM sep_expediente
                WHERE estado IN (40,45,46,50))
      AND tpo_registro = 2

   IF(v_total_amortizacion_d IS NULL)THEN
      LET v_total_amortizacion_aux = 0
   END IF
   LET v_total_amortizacion = v_total_amortizacion_d * 100 USING "&&&&&&&&&&&&"
   LET v_espacios = " "
   # genera cadena con 22 espacios
   FOR v_indice = 1 TO 21
      LET v_espacios = v_espacios," "
   END FOR
   LET v_linea = v_tipo_registro,
                 v_tot_reg_archivo USING "&&&&&&&&&&",     # Total registros
                 "0000",                                   # Filler
                 "000000000000",                           # Total aportacion
                 v_total_amortizacion CLIPPED,# Total de amortizacion
                 "000000000000",                           # Total de seguridad
                 v_total_amortizacion CLIPPED,# Total general = aportacion + amortizacion + seguridad 
                 v_espacios                                # Filler

   IF(v_linea IS NULL)THEN
      LET v_linea = " "
   END IF
   CALL v_canal.writeLine(v_linea)

   CALL v_canal.close()

   #******** REPORTE *************
   CALL fn_configura_rpt_ajuste_credito()


   # Actualiza sep_batch_contabilidad
   UPDATE sep_batch_contabilidad
      SET ind_envio = 1, 
          folio     = r_folio
    WHERE ind_envio = 0
      AND id_expediente 
           IN (SELECT id_expediente
                 FROM sep_expediente
                WHERE estado IN (40,45,46, 50)
                  AND ind_ajuste = 2)


   {UPDATE sep_expediente 
      SET ind_ajuste = 3 
    --WHERE estado IN (40,45,50)
    WHERE estado = 50  
      AND ind_ajuste = 2
      AND id_expediente IN (SELECT id_expediente 
                              FROM sep_batch_contabilidad
                             WHERE ind_envio = 1)}

   LET v_consulta = "\n UPDATE sep_expediente", 
                    "\n    SET ind_ajuste = 3", 
                    "\n  WHERE estado in (40,45,46,50) ",  
                    "\n    AND ind_ajuste = 2",
                    "\n    AND id_expediente = ?"
   PREPARE prp_act_ind_expediente FROM v_consulta

   FOR v_indice = 1 TO v_expedientes.getLength() 
      
      EXECUTE prp_act_ind_expediente USING v_expedientes[v_indice]

   END FOR 
                               
   # Actualiza sep_expediente por medio de maquinaria
   --LET v_consulta = "EXECUTE FUNCTION fn_maquinaria_individual(?,?,?,?,?)"
   ----PREPARE prp_cambia_edo_maq FROM v_consulta

   --LET v_senial = 65
   --DECLARE cur_rec_id_expediente CURSOR FOR SELECT id_expediente
    --                                          FROM sep_expediente
     --                                        WHERE estado = 50
  -- FOREACH cur_rec_id_expediente INTO v_id_expediente
    --  EXECUTE prp_cambia_edo_maq USING "maq_sep_expediente",
    --                                   v_id_expediente,
     --                                  "id_expediente",
      --                                 v_senial,
       --                                p_usuario
        --                          INTO v_ind,
         --                              v_diag,
          --                             v_estado_destino
     -- DISPLAY "v_ind:",v_ind
     -- DISPLAY "v_diag:",v_diag
     -- DISPLAY "v_estado_destino:",v_estado_destino
      
     -- IF(v_estado_destino = -1)THEN
      --   DISPLAY "Registro imposible de actualizar estado"
       --  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
        --                        RETURNING r_resultado_opera
      --   # si ocurrió un error con la actualizacion de la operacion operacion 
      --   # muestra el mensaje
      --   IF(r_resultado_opera)THEN
      --      CALL fn_desplega_inc_operacion(r_resultado_opera)
      --   END IF
      --   EXIT PROGRAM
      --END IF
      
     -- IF(v_ind <> 0)THEN
      --   DISPLAY "Ocurrió error en maquinaria"
       --  CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
        --                        RETURNING r_resultado_opera
      --   # si ocurrió un error con la actualizacion de la operacion operacion 
      --   # muestra el mensaje
      --   IF(r_resultado_opera)THEN
      --      CALL fn_desplega_inc_operacion(r_resultado_opera)
      --   END IF
      --   EXIT PROGRAM
      --END IF

   --END FOREACH
   --FREE cur_rec_id_expediente

   # Almacena registro de operacion
   LET v_consulta = "\n INSERT INTO sep_ctr_batch_contabilidad(folio,f_proceso)",
                    "\n VALUES(?,?)"
   PREPARE prp_ins_ctr_ajuste FROM v_consulta
   EXECUTE prp_ins_ctr_ajuste USING r_folio,v_fecha_ctrl
   
   DISPLAY "\n\n"
   DISPLAY "FOLIO GENERADO: ",r_folio
   DISPLAY "RUTA: ",v_ruta_envio
   DISPLAY "ARCHIVO GENERADO: ",v_archivo_salida
   DISPLAY "\n\n"

   
#---se agrega la funcionalidad para AXWAY

   LET v_consulta = "cp ",
                    v_ruta_envio CLIPPED     ,"/",
                    v_archivo_salida CLIPPED ," ",
                    
                    v_ruta_envio CLIPPED     ,"/",
                    v_archivo_salida_axway CLIPPED
   RUN v_consulta

   DISPLAY "COPIA ARCHIVO PARA ENVIO AXWAY: ",v_consulta CLIPPED 
   DISPLAY "EJECUTANDO SCRIPT AXWAY       : ",v_ruta_script_axway CLIPPED

   LET v_consulta = "sh ",v_ruta_script_axway CLIPPED
   RUN v_consulta   

#---fin agrega la funcionalidad para AXWAY

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
      # Ejecuta elaboración de reporte
      {LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI13.42r '",
                                p_usuario CLIPPED, "' ",p_pid CLIPPED, " ",
                                p_proceso_cod CLIPPED," ",p_opera_cod CLIPPED," ",
                                r_folio CLIPPED, " '",v_nom_archivo CLIPPED,"'"
      RUN v_comando
      IF(STATUS)THEN
         DISPLAY "Ocurrió un error al generar el reporte"
      END IF}
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
         v_credito       LIKE sep_batch_contabilidad.id_credito,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_monto         LIKE sep_batch_contabilidad.monto
         --v_estado_desc   LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_ajustes RECORD
         v_id_expediente LIKE sep_batch_contabilidad.id_expediente,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_credito       LIKE sep_batch_contabilidad.id_credito,
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

         LET v_consulta = "\n SELECT exp.id_expediente,",
                          "          con.nss,",
                          "          con.id_credito,",
                          "\n        SUM(con.monto)",
                          "\n   FROM sep_batch_contabilidad con JOIN sep_expediente exp",
                          "\n     ON con.id_expediente = exp.id_expediente",
                          --"\n        LEFT OUTER JOIN sep_estado_expediente edo",
                          --"\n     ON edo.estado = exp.estado",
                          "\n  WHERE exp.estado in (40,45,46,50) ",
                          "\n    AND con.ind_envio = 0",
                          "\n  GROUP BY 1,2,3"
         PREPARE prp_recupera_ajustes FROM v_consulta
         DECLARE cur_recupera_ajustes CURSOR FOR prp_recupera_ajustes
         LET v_indice = 1
         FOREACH cur_recupera_ajustes INTO v_ajustes.*
            LET v_gen_rpt = TRUE
            LET v_movimientos.v_consecutivo   = v_indice
            LET v_expedientes[v_indice] = v_ajustes.v_id_expediente
            LET v_movimientos.v_id_expediente = v_ajustes.v_id_expediente
            LET v_movimientos.v_credito       = v_ajustes.v_credito
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
         v_credito       LIKE sep_batch_contabilidad.id_credito,
         v_nss_ajuste    LIKE sep_batch_contabilidad.nss,
         v_monto         LIKE sep_batch_contabilidad.monto
         --v_estado_desc   LIKE sep_estado_expediente.descripcion
       END RECORD,
       v_pagina          SMALLINT,
       v_total_registros INTEGER,
       v_monto_total     LIKE sep_batch_contabilidad.monto

   FORMAT

      FIRST PAGE HEADER
DISPLAY "TOT REP:",v_enc_batch_con.v_total_registros      
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