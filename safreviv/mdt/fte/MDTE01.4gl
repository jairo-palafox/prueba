--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18-04-2012
-- Fecha ultima modificacion: 13-07-2012 se modifica ruta en duro
-- del reporte, se agrega variable v_prog_report
--===============================================================

####################################################################
#Modulo            =>MDTE                                          #
#Programa          =>MDTE10                                        #
#Objetivo          =>Programa que genera el archivo de salida de   # 
#                    mandatos                                      #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>14 FEBRERO 2012                               #
####################################################################
GLOBALS "MDTG02.4gl"
DATABASE safre_viv
DEFINE v_prog_reporte      CHAR(200)
DEFINE p_v_usuario         LIKE seg_usuario.usuario, -- nombre del usuario
          p_d_pid          LIKE bat_ctr_proceso.pid, -- pid
          p_i_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
          p_i_opera_cod    LIKE cat_operacion.opera_cod, -- codigo de la operacion de la etapa
          p_d_folio        LIKE glo_ctr_archivo.folio, -- numero de folio
          p_folio          LIKE mdt_solicitud_mandato.folio,--DATE, -- fecha del lote a validar
          v_v_nom_archivo  VARCHAR(80) -- nombre del archivo de salida

MAIN
   --DEFINE ppp_estado  smallint
   DEFINE v_ch_arch_solTransf  BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_folio_liquidacion  LIKE cta_movimiento.folio_liquida,
          v_f_liquida          LIKE cta_movimiento.f_liquida, -- fecha de liquidacion
          v_r_detalle          RECORD
             --CAMPO	LONG.	POSC. DE	POSC. A	TIPO TRANSACCION	REQUERIDO	OBSERVACIONES
             id_mdt           CHAR(7)  , -- 7   1   a 7   TTCCCCC Tpo mandato y Consecutivo
             tpo_operacion    CHAR(1)  , -- 1   8   a 8   A, B y M (Altas, bajas y modificaciones)
             nivel1           CHAR(100), -- 100 9   a 108 descripcion nivel 1
             nivel2           CHAR(100), -- 100 109 a 208 filler
             nivel3           CHAR(100), -- 100 209 a 308 filler
             num_cuenta       CHAR(40) , -- 40  309 a 348
             convenio         CHAR(40) , -- 40  349 a 388
             referencia       CHAR(40) , -- 40  389 a 428
             cta_clabe        CHAR(18) , -- 18  429 a 446 ???
             fecha_captura    CHAR(8)  , -- 8   447 a 454 AAAAMMDD
             res_operacion    CHAR(2)  , -- 2   455 a 456 01 Aceptado 02 Rechazado
             diagostico       CHAR(3)    -- 3   457 a 459 ???
          END RECORD,
          v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente, -- ID del derechohabiente
          v_pesos              LIKE cta_movimiento.monto_pesos, -- monto en pesos
          v_deudor_txt         CHAR(13), -- para obtener cifras del saldo
          v_nss                LIKE afi_derechohabiente.nss, -- NSS del derechohabiente
          --v_num_credito        LIKE acr_transferencia.num_credito, -- numero del credito
          v_s_registro         STRING, -- registro a insertar
          v_c_ruta_env_mdt     LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_i_contrador_reg    INTEGER, -- contrador de registros
          v_c_programa_cod     LIKE bat_ctr_operacion.programa_cod, -- nombre del programa
          v_s_qryTxt           STRING, -- guarda una sentencia sql a ejecutar
          r_b_valida           SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
   DEFINE v_r_mandato          RECORD LIKE mdt_solicitud_mandato.* 

   DEFINE tot_altas               INTEGER,
          tot_bajas               INTEGER,
          tot_modificaciones      INTEGER,
          tot_reactivacion        INTEGER
   DEFINE tot_altas_proc          INTEGER,
          tot_bajas_proc          INTEGER,
          tot_modificaciones_proc INTEGER,
          tot_reactivacion_proc   INTEGER
   DEFINE v_sp_estado             INTEGER
   DEFINE v_reg_102_a,
          v_reg_102_b,
          v_reg_102_m,
          v_reg_102_r             INTEGER
   DEFINE v_reg_106_a,
          v_reg_106_b,
          v_reg_106_m,
          v_reg_106_r             INTEGER
   DEFINE p_fec_ejecucion         DATE
   DEFINE v_r_rpt_res   RECORD -- registro de resumen
             des_origen   LIKE mdt_cat_origen.des_origen,
             proceso_desc LIKE cat_proceso.proceso_desc,
             folio         LIKE mdt_solicitud_mandato.folio,
             f_proceso    CHAR(10),
             altas        INTEGER,
             bajas        INTEGER,
             modif        INTEGER
          END RECORD
   DEFINE v_r_rpt_res_edo   RECORD -- registro de resumen por estado
             altas_aceptadas  INTEGER,
             bajas_aceptadas  INTEGER,
             modif_aceptadas  INTEGER,
             altas_rechazadas INTEGER,
             bajas_rechazadas INTEGER,
             modif_rechazadas INTEGER
          END RECORD,
          v_r_reporte_det   DYNAMIC ARRAY OF RECORD -- registro de detalle
             nss            string,
             mandato_desc   LIKE mdt_cat_mandato.desc_mandato,
             diagnostico    string
          END RECORD,
          v_v_nom_reporte   VARCHAR(80), -- nombre del reporte
          v_manejador_rpt   OM.SaxDocumentHandler, # Contenedor de Documentos para el reporte
          r_ruta_bin        LIKE seg_modulo.ruta_bin, -- rutal de bin
          r_ruta_listados   LIKE seg_modulo.ruta_listados -- ruta de listados
   DEFINE v_pos_rep_det        INTEGER
   DEFINE v_pos_rep_det_fin    INTEGER
   DEFINE v_mandato_desc    LIKE mdt_cat_mandato.desc_mandato
   DEFINE v_estado          CHAR(1)
   DEFINE v_tpo_mandato     LIKE mdt_tpo_mandato.desc_tpo_mandato
   DEFINE v_tipo_operacion  LIKE mdt_solicitud_mandato.tipo_operacion
   DEFINE v_tipo_operacion_desc   CHAR(20)
   DEFINE v_tot_mdt         INTEGER
   DEFINE v_pos_acep        SMALLINT
   DEFINE v_pos_rech        SMALLINT
   DEFINE v_day             CHAR(2)
   DEFINE v_mes             CHAR(2)
   DEFINE v_ano             CHAR(4)
   DEFINE v_f_proceso  DATE
   DEFINE p_nom_archivo CHAR(20)

   SLEEP 5

   
   -- se recuperan los parametros que envia el programa lanzador
   LET p_v_usuario = ARG_VAL(1)
   LET p_d_pid = ARG_VAL(2)
   LET p_i_proceso_cod = ARG_VAL(3)
   LET p_i_opera_cod = ARG_VAL(4)
   LET p_folio = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6) -- Fecha del lote 

   {SELECT a.folio 
   INTO   p_v_fecha_lote 
   FROM   mdt_lote_mandato a
   WHERE  a.estado = 102}
   
   --WHENEVER ERROR STOP
   WHENEVER ERROR CONTINUE
   
   -- se crear el archivo log
   CALL STARTLOG(p_v_usuario CLIPPED|| ".MDTE01.log")
   
   LET p_fec_ejecucion = DATE
   LET v_v_nom_reporte = p_v_usuario CLIPPED, "-MDTE01-", p_d_pid USING "&&&&&", "-", p_i_proceso_cod USING "&&&&&", "-", p_i_opera_cod USING "&&&&&"
   
   DISPLAY "PID asignado: ",p_d_pid
   DISPLAY "Proceso asignado: ",p_i_proceso_cod
   DISPLAY "Operacion asignado: ",p_i_opera_cod
   DISPLAY "Fecha de Proceso: ",today
   DISPLAY "Folio: ",p_folio

   LET v_s_qryTxt = "create TEMP table tmp_procesa_mdt ",
                    "  (",
                    "    estado CHAR(1),",
                    "    cve_mandato CHAR(18),",
                    "    tipo_operacion CHAR(1)",
                    "  );"
   PREPARE EnuTmpProc FROM v_s_qryTxt
   EXECUTE EnuTmpProc 

   LET v_s_qryTxt = "DROP table tmp_procesa_mdt; "
   PREPARE EnuTmpProcDrop FROM v_s_qryTxt

   LET v_s_qryTxt = " SELECT NVL(count(*),0) ",
                    "   FROM mdt_solicitud_mandato ",
                    "  WHERE estado = 101 ",
                    "    AND folio = ? ",
                    "    AND tipo_operacion = ? "
                    
   PREPARE EnuTotReg FROM v_s_qryTxt
   
   EXECUTE EnuTotReg USING p_folio,"A" INTO tot_altas
   EXECUTE EnuTotReg USING p_folio,"B" INTO tot_bajas
   EXECUTE EnuTotReg USING p_folio,"M" INTO tot_modificaciones
   EXECUTE EnuTotReg USING p_folio,"R" INTO tot_reactivacion

   DISPLAY "TOTAL ALTAS: ",tot_altas
   DISPLAY "TOTAL BAJAS: ",tot_bajas
   DISPLAY "TOTAL MODIFICACIONES: ",tot_modificaciones
   DISPLAY "TOTAL REACTIVACIONES: ",tot_reactivacion
   
   DISPLAY "PROCESANDO VALIDACION DE INSTRUCCIONES DE MANDATO"
   
   -- asigna el folio en la variable de folio liquidaci�n
   LET v_c_programa_cod = "MDTE01"

   UPDATE bat_ctr_operacion 
   SET nom_archivo = p_nom_archivo ,
       folio       = p_folio
   WHERE pid  =  p_d_pid
   AND   proceso_cod = p_i_proceso_cod   
   AND   opera_cod = p_i_opera_cod   

   -- se invoca la funci�n que deja la operaci�n en estado Procesando
 --  LET r_b_valida = fn_actualiza_opera_ini(p_d_pid, p_i_proceso_cod, p_i_opera_cod,
                                           --p_d_folio, v_c_programa_cod,
                                           --v_v_nom_reporte, p_v_usuario)

   -- se verifica si fue posible inicializar la operacion
   --IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
    --  DISPLAY "Program stopped ERROR en fn_actualiza_opera_ini"
    --  EXIT PROGRAM
  -- END IF

   LET v_s_qryTxt = " SELECT cat.desc_mandato ",
                    "   FROM mdt_cat_mandato cat JOIN mdt_cat_mandato_paquete pte",
                    "     ON cat.id_cat_mandato = pte.id_cat_mandato",
                    "  WHERE pte.cve_mandato = ? "
                    
   PREPARE EnuRegMDT FROM v_s_qryTxt

   LET v_s_qryTxt = " SELECT * ",
                    "   FROM mdt_solicitud_mandato",
                    "  WHERE estado = 101 ",
                    "    AND folio = ? "
                    
   PREPARE EnuRegProc FROM v_s_qryTxt
   DECLARE CurRegProc CURSOR FOR EnuRegProc
   
   LET v_reg_102_a = 0
   LET v_reg_106_a = 0
   LET v_reg_102_b = 0
   LET v_reg_106_b = 0
   LET v_reg_102_m = 0
   LET v_reg_106_m = 0
   LET v_reg_102_r = 0
   LET v_reg_106_r = 0
   LET tot_altas_proc          = 0
   LET tot_bajas_proc          = 0
   LET tot_modificaciones_proc = 0
   LET tot_reactivacion_proc   = 0
   LET v_pos_rep_det = 1

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_alta_mandato(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la alta de mandatos
   PREPARE prp_alta_mdt FROM v_s_qryTxt

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_baja_mandato(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la baja de mandatos
   PREPARE prp_baja_mdt FROM v_s_qryTxt

   LET v_s_qryTxt = "EXECUTE PROCEDURE safre_viv:sp_modifica_mandato(?,?,?,?)"
   # Se prepara la ejecucion del stored procedure para la modificacion y reactivacion de mandatos
   PREPARE prp_modi_mdt FROM v_s_qryTxt

   FOREACH CurRegProc USING p_folio INTO v_r_mandato.*
      
      CASE v_r_mandato.tipo_operacion
         WHEN "A"
            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato 
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1
            
            EXECUTE prp_alta_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       p_v_usuario
                                 INTO v_sp_estado
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_a = v_reg_102_a + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  INITIALIZE v_r_mandato.diagnostico TO NULL
                  SELECT diagnostico INTO v_r_mandato.diagnostico                  
                    FROM mdt_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato

                  IF(v_r_mandato.diagnostico IS NULL)THEN
                     LET v_r_mandato.diagnostico = '000'
                  END IF
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico
                  
                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_a = v_reg_106_a + 1
               END IF
               LET tot_altas_proc = tot_altas_proc + 1
            ELSE

               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion alta: ",v_sp_estado
            
               DISPLAY "Program stopped at 'MDTE01.4gl' sp alta"
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         WHEN "B"

            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato
            
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1

            EXECUTE prp_baja_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       p_v_usuario
                                 INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_b = v_reg_102_b + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  INITIALIZE v_r_mandato.diagnostico TO NULL
                  SELECT diagnostico INTO v_r_mandato.diagnostico
                    FROM mdt_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato
                  IF(v_r_mandato.diagnostico IS NULL)THEN
                     LET v_r_mandato.diagnostico = '000'
                  END IF
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico

                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_b = v_reg_106_b + 1
               END IF
               LET tot_bajas_proc = tot_bajas_proc + 1
            ELSE
            
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion baja: ",v_sp_estado
               
               DISPLAY "Program stopped at 'MDTE01.4gl' sp baja"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         WHEN "M"

            LET v_r_reporte_det[v_pos_rep_det].nss          = v_r_mandato.nss
            EXECUTE EnuRegMDT USING v_r_mandato.cve_mandato INTO v_mandato_desc
            ########## Recuepera decripcion de mandato
            --LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_mandato_desc
            LET v_r_reporte_det[v_pos_rep_det].mandato_desc = v_r_mandato.cve_mandato
            LET v_r_reporte_det[v_pos_rep_det].diagnostico  = v_r_mandato.diagnostico
            LET v_pos_rep_det = v_pos_rep_det + 1

            EXECUTE prp_modi_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       v_r_mandato.tipo_operacion      
                                    INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  INSERT INTO tmp_procesa_mdt VALUES ('A', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_102_m = v_reg_102_m + 1
                  LET v_pos_rep_det = v_pos_rep_det - 1 -- Ajuste 20120411 Se omite del detalle de rechazadas
               END IF
               IF v_sp_estado = 106 THEN
                  SELECT diagnostico INTO v_r_mandato.diagnostico
                    FROM mdt_solicitud_mandato
                   WHERE id_solicitud_mandato = v_r_mandato.id_solicitud_mandato
                  
                  LET v_r_reporte_det[v_pos_rep_det-1].diagnostico = v_r_mandato.diagnostico

                  INSERT INTO tmp_procesa_mdt VALUES ('R', v_r_mandato.cve_mandato, v_r_mandato.tipo_operacion)
                  --DISPLAY "SQLERRD: ", sqlca.sqlerrd[3]
                  LET v_reg_106_m = v_reg_106_m + 1
               END IF
               LET tot_modificaciones_proc = tot_modificaciones_proc + 1
            ELSE
            
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion mod : ",v_sp_estado
               
               DISPLAY "Program stopped at 'MDTE01.4gl' sp modificacion"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         WHEN "R"
            EXECUTE prp_modi_mdt USING v_r_mandato.id_solicitud_mandato,
                                       v_r_mandato.id_credito          ,
                                       v_r_mandato.id_derechohabiente  ,
                                       v_r_mandato.tipo_operacion
                                 INTO v_sp_estado
                                 
            IF(SQLCA.SQLCODE = 0)THEN
               # Sin Error en el SP
               IF v_sp_estado = 102 THEN
                  LET v_reg_102_r = v_reg_102_r + 1
               END IF
               IF v_sp_estado = 106 THEN
                  LET v_reg_106_r = v_reg_106_r + 1
               END IF
               LET tot_reactivacion_proc = tot_reactivacion_proc + 1
            ELSE
            
               DISPLAY "Error              : ", SQLCA.sqlcode            
               DISPLAY "Pid                : ", v_r_mandato.id_solicitud_mandato
               DISPLAY "Num Cred           : ", v_r_mandato.id_credito          
               DISPLAY "Id Derechohabiente : ", v_r_mandato.id_derechohabiente  
               DISPLAY "Usuario            : ", p_v_usuario
               DISPLAY "Salida Funcion reac: ",v_sp_estado
               
               DISPLAY "Program stopped at 'MDTE01.4gl' sp reactivacion"
               
               CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida      
               IF(r_b_valida <> 0)THEN
                  # En caso de error se muestra un mensaje a usuario y no continua
                  CALL fn_desplega_inc_operacion(r_b_valida)
               END IF
               # Error en el SP
               EXIT PROGRAM
            END IF
         
         
      END CASE
      
   END FOREACH
   LET v_pos_rep_det_fin = v_pos_rep_det
   
   DISPLAY "TOTAL DE REGISTROS PROCESADOS: ",tot_altas_proc+tot_bajas_proc+tot_modificaciones_proc+tot_reactivacion_proc
   -- ahm temp poR GRUPO ESTADO (102 y 106)?????????????
   DISPLAY "TOTAL DE ALTAS PROCESADAS:          ",tot_altas_proc
   DISPLAY "TOTAL DE BAJAS PROCESADAS:          ",tot_bajas_proc
   DISPLAY "TOTAL DE MODIFICACIONES PROCESADAS: ",tot_modificaciones_proc
   DISPLAY "TOTAL DE REACTIVACIONES PROCESADAS: ",tot_reactivacion_proc
   DISPLAY "REGISTROS VALIDADOS: ",v_reg_102_a+v_reg_102_b+v_reg_102_m+v_reg_102_r
   DISPLAY "REGISTROS RECHAZADOS: ",v_reg_106_a+v_reg_106_b+v_reg_106_m+v_reg_106_r
   DISPLAY "FIN DEL PROCESO: ",DATE

   IF v_reg_106_a+v_reg_106_b+v_reg_106_m+v_reg_106_r > 0 THEN
      UPDATE mdt_lote_mandato
        SET estado = 102
        WHERE folio = p_folio -- AHM TMP Validarlo si es por fecha
   ELSE
      UPDATE mdt_lote_mandato
        SET estado = 103
        WHERE folio = p_folio -- AHM TMP Validarlo si es por fecha
   END IF
   
   -- se invoca la funci�n que deja la operaci�n en estado Finalizado
--   LET r_b_valida = 0
--SELECT estado_cod 
--INTO ppp_estado 
--FROM bat_ctr_operacion 
--WHERE pid = p_d_pid
--AND   proceso_cod = p_i_proceso_cod
--AND  opera_cod = p_i_opera_cod

--DISPLAY "estado: ",ppp_estado
   
--DISPLAY p_d_pid,p_i_proceso_cod, p_i_opera_cod
   # Finaliza la operacion 3 (validaci�n de acreditados recurrentes)
   LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, p_i_opera_cod)
--DISPLAY r_b_valida
   
   -- se verifica si fue posible finalizar la operacion
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "Program stopped ERROR en fn_actualiza_opera_fin Op: ",p_i_opera_cod
   ELSE

      -- recupera la ruta de listados en el que se enviara el archivo
      CALL fn_rutas("mdt") RETURNING r_ruta_bin, r_ruta_listados
      --DISPLAY "Ruta bin - ", r_ruta_bin
      --DISPLAY "Ruta lst - ", r_ruta_listados
      
      

      LET v_r_rpt_res_edo.altas_aceptadas  = v_reg_102_a
      LET v_r_rpt_res_edo.bajas_aceptadas  = v_reg_102_b
      LET v_r_rpt_res_edo.modif_aceptadas  = v_reg_102_m
      LET v_r_rpt_res_edo.altas_rechazadas = v_reg_106_a
      LET v_r_rpt_res_edo.bajas_rechazadas = v_reg_106_b
      LET v_r_rpt_res_edo.modif_rechazadas = v_reg_106_m


      -- Recupera descripcion del tipo de origen
      SELECT des_origen 
        INTO v_r_rpt_res.des_origen 
        FROM mdt_cat_origen
      WHERE id_origen = 1
      
      -- Recupera descripcion del proceso
      SELECT proceso_desc 
        INTO v_r_rpt_res.proceso_desc 
        FROM cat_proceso
      WHERE proceso_cod = p_i_proceso_cod

      {LET v_s_qryTxt = " SELECT first 1 lote ",
                       "   FROM mdt_solicitud_mandato ",
                       "  WHERE estado in (102,106) ",
                       "    AND f_lote = '",p_v_fecha_lote,"'"
	    
	    PREPARE EnuBusLote FROM v_s_qryTxt
	    EXECUTE EnuBusLote INTO} 
        LET v_r_rpt_res.folio = p_folio
      SELECT f_proceso
        INTO v_f_proceso
        FROM mdt_lote_mandato
       WHERE folio = p_folio
      
      
      LET v_day = DAY(v_f_proceso) USING "&&"
      LET v_mes = MONTH(v_f_proceso) USING "&&"
      LET v_ano = YEAR(v_f_proceso) USING "&&&&"
      LET v_r_rpt_res.f_proceso = v_day,"-",v_mes,"-",v_ano
      LET v_r_rpt_res.altas  = tot_altas_proc
      LET v_r_rpt_res.bajas  = tot_bajas_proc
      LET v_r_rpt_res.modif  = tot_modificaciones_proc

      -- Carga de arreglos de aceptadas y rechazadas
      LET v_s_qryTxt = {" SELECT a.estado, m.desc_tpo_mandato, a.tipo_operacion, count(*) ",
                       "   FROM tmp_procesa_mdt a,",
                       "        mdt_cat_mandato_paquete b,",
                       "        mdt_cat_mandato c,",
                       "        mdt_tpo_mandato m",
                       "  WHERE a.cve_mandato = b.cve_mandato",
                       "    AND c.id_cat_mandato = b.id_cat_mandato",
                       "    AND c.tpo_mandato = m.tpo_mandato",
                       "  GROUP BY 1,2,3 ",
                       "  ORDER BY estado, desc_tpo_mandato, tipo_operacion"}
                       "\n SELECT a.estado, m.desc_tpo_mandato, a.tipo_operacion, count(*)",
                       "\n   FROM tmp_procesa_mdt a LEFT OUTER JOIN mdt_cat_mandato_paquete b",
                       "\n     ON a.cve_mandato = b.cve_mandato",
                       "\n        LEFT OUTER JOIN mdt_cat_mandato c",
                       "\n     ON c.id_cat_mandato = b.id_cat_mandato",
                       "\n        LEFT OUTER JOIN mdt_tpo_mandato m",
                       --"\n     ON c.tpo_mandato = m.tpo_mandato",
                       "\n     ON a.cve_mandato[2] = m.tpo_mandato",
                       "\n  --WHERE ",
                       "\n  GROUP BY 1,2,3 ",
                       "\n  ORDER BY estado, desc_tpo_mandato, tipo_operacion"
	    
	    PREPARE EnuAgrupaMDT FROM v_s_qryTxt
      DECLARE CurAgrupaMDT CURSOR FOR EnuAgrupaMDT
      
      LET v_pos_acep = 1
      LET v_pos_rech = 1
      CALL v_r_rpt_aceptadas.clear()
      CALL v_r_rpt_canceladas.clear()
      FOREACH CurAgrupaMDT INTO v_estado, v_tpo_mandato, v_tipo_operacion, v_tot_mdt
         IF v_estado = 'A' THEN
            IF v_pos_acep < 13 THEN
               IF(v_tpo_mandato IS NULL OR v_tpo_mandato = ' ')THEN
                  

               END IF
               LET v_r_rpt_aceptadas[v_pos_acep].tpo_mandato = v_tpo_mandato
               CASE v_tipo_operacion 
                  WHEN 'A' 
                     LET v_tipo_operacion_desc = "ALTA"
                  WHEN 'B' 
                     LET v_tipo_operacion_desc = "BAJA"
                  WHEN 'M' 
                     LET v_tipo_operacion_desc = "MODIFICACION"
                  OTHERWISE 
                     LET v_tipo_operacion_desc = ""
               END CASE
               LET v_r_rpt_aceptadas[v_pos_acep].operacion   = v_tipo_operacion_desc
               LET v_r_rpt_aceptadas[v_pos_acep].total_mdt   = v_tot_mdt
               --DISPLAY "Aceptadas[",v_pos_acep,": ",v_r_rpt_aceptadas[v_pos_acep].*
               LET v_pos_acep = v_pos_acep + 1
            END IF
         END IF
         IF v_estado = 'R' THEN
            IF v_pos_rech < 13 THEN
               LET v_r_rpt_canceladas[v_pos_rech].tpo_mandato = v_tpo_mandato
               CASE v_tipo_operacion 
                  WHEN 'A' 
                     LET v_tipo_operacion_desc = "ALTA"
                  WHEN 'B' 
                     LET v_tipo_operacion_desc = "BAJA"
                  WHEN 'M' 
                     LET v_tipo_operacion_desc = "MODIFICACION"
                  OTHERWISE 
                     LET v_tipo_operacion_desc = ""
               END CASE
               LET v_r_rpt_canceladas[v_pos_rech].operacion   = v_tipo_operacion_desc
               LET v_r_rpt_canceladas[v_pos_rech].total_mdt   = v_tot_mdt
               --DISPLAY "Rechazadas[",v_pos_rech,": ",v_r_rpt_canceladas[v_pos_rech].*
               LET v_pos_rech = v_pos_rech + 1
            END IF
         END IF
      END FOREACH
      
      --Borra tabla temporal 
      EXECUTE EnuTmpProcDROP 

      --DISPLAY "v_pos_acep: ", v_pos_acep
      --DISPLAY "v_pos_rech: ", v_pos_rech
      
      IF v_pos_rep_det_fin = 1 THEN
         CALL v_r_reporte_det.clear() 
         LET v_r_reporte_det[1].nss = '  '
         CALL fn_cierra_proceso_mdt_batch(p_v_usuario)
      END IF
      # se quita esta condicion para poder generar el reporte
      --IF(v_pos_rep_det_fin > 1)THEN
         -- se indica que el reporte usara la plantilla creada
         
         LET v_prog_reporte = r_ruta_bin CLIPPED, "/MDTG011.4rp"
         
         IF fgl_report_loadCurrentSettings(v_prog_reporte) THEN
         
            -- se indica la salida del reporte
            CALL fgl_report_selectDevice("PDF")
            CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)
            -- sin indica que no es necesario el preview
            CALL fgl_report_selectPreview(0)
             -- se asigna la configuraci�n en el menejo del reporte
            LET v_manejador_rpt = fgl_report_commitCurrentSettings()
         ELSE
            DISPLAY "no fue posible generar el reporte"
            {CALL fn_error_opera(p_d_pid, p_i_proceso_cod, p_i_opera_cod) 
                               RETURNING r_b_valida
      
            IF(r_b_valida <> 0)THEN
               # En caso de error se muestra un mensaje a usuario y no continua
               CALL fn_desplega_inc_operacion(r_b_valida)
            END IF}
         END IF
      
         -- inicia el reporte de registros procesados
         START REPORT rpt_solicitudes_mandatos TO XML HANDLER v_manejador_rpt
      
         FOR v_pos_rep_det = 1 TO (v_pos_rep_det_fin - 1) -- AHM 20120416 v_r_reporte_det.getLength()
            OUTPUT TO REPORT rpt_solicitudes_mandatos(v_r_rpt_res.*, v_r_rpt_res_edo.*,
                                                  v_r_reporte_det[v_pos_rep_det].*)
         END FOR
         OUTPUT TO REPORT rpt_solicitudes_mandatos(v_r_rpt_res.*, v_r_rpt_res_edo.*,
                                               " "," "," ")
      
         -- finaliza el reporte
         FINISH REPORT rpt_solicitudes_mandatos
      {ELSE
         DISPLAY "No fu� posible crear reporte"
      END IF}
      

         -- Env�o de correo de notificaci�n de proceso finalizado
         CALL fn_correo_proceso(p_d_pid, 
                                p_i_proceso_cod, 
                                p_i_opera_cod, 
                                '', -- TMP AHM adjunto ?
                                'Registrar instrucciones de mandatos - Origen recurrente',
                             'ID Proceso   : '||p_d_pid||
                             'Proceso      : '||p_i_proceso_cod||
                             'Operacion    : '||p_i_opera_cod||
                             'Fecha Inicio : '||p_fec_ejecucion||
                             'Fecha Fin    : '||DATE
                             )

   END IF
END MAIN

####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTE10                                        #
#Descripcion       =>Genera el cierre autom�tico del proceso cuando#
#                    no hay rechazos en la operaci�n anterior      #
#Autor             =>Alexandro Hollmann, EFP                       #
#Fecha inicio      =>25 Abril 2012                                 #
####################################################################
FUNCTION fn_cierra_proceso_mdt_batch(p_v_usuario)
   DEFINE p_v_usuario          LIKE seg_usuario.usuario -- nombre del usuario
   DEFINE --v_i_proceso_cod     LIKE cat_proceso.proceso_cod, -- proceso que llama las funciones
          v_i_opera_cod       LIKE cat_operacion.opera_cod, -- operaci�n que llama la funcion
          --v_d_pid             DECIMAL(9,0), -- identificador del proceso
          r_b_valida          SMALLINT -- booleana que indica si el proceso se puede ejecutar o no
DEFINE v_hora_actual DATETIME HOUR TO MINUTE
          
   -- se inicializan las variables
   --LET v_i_proceso_cod = 1303 -- 46 anterior -- lanzar recurrentes -- AHM 2012 04 25 Cambio de procesos
   # En caso de no haber rechazos se finaiza en autom�tico la 
   # operaci�n 4 de rechazos
   
   LET v_i_opera_cod = 4 -- genera archivo mandatos
   --LET v_d_pid = 0
   
   --CALL fn_max_pid(v_i_proceso_cod, 1) RETURNING v_d_pid
   
   ---- se invoca la funcion que valida la operacion
   {CALL fn_valida_operacion(p_d_pid,p_i_proceso_cod,v_i_opera_cod) RETURNING r_b_valida

   -- se verifica si la operacion en proceso es valida
   IF r_b_valida <> 0 THEN
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "Program stopped ERROR en cierre automatico fn_valida_operacion"
      CALL fn_error_opera(p_d_pid, p_i_proceso_cod, v_i_opera_cod) 
              RETURNING r_b_valida
      
      IF(r_b_valida <> 0)THEN
         # En caso de error se muestra un mensaje a usuario y no continua
         CALL fn_desplega_inc_operacion(r_b_valida)
      END IF
      EXIT PROGRAM
   END IF

   IF r_b_valida = 0 THEN}
      -- LET r_b_valida = fn_actualiza_opera_ini(v_d_pid, v_i_proceso_cod, v_i_opera_cod,
      --                                          v_d_pid, 'MDTL04',
      --                                          v_d_pid, p_v_usuario)
      -- 
      -- IF r_b_valida <> 0 THEN
      --    -- en caso de error se muestra un mensaje a usuario y no continua
      --    --CALL fn_muestra_inc_operacion(r_b_valida)
      --    DISPLAY "Program stopped ERROR en cierre automatico fn_actualiza_opera_ini"
      --    EXIT PROGRAM
      -- END IF
      
      -- se invoca la funci�n que deja la operaci�n en estado Finalizado
     { LET r_b_valida = fn_actualiza_opera_fin(p_d_pid, p_i_proceso_cod, v_i_opera_cod)
      
      -- se verifica si fue posible finalizar la operacion
      IF r_b_valida <> 0 THEN
         -- en caso de error se muestra un mensaje a usuario y no continua
         --CALL fn_muestra_inc_operacion(r_b_valida)
         DISPLAY "Program stopped ERROR en cierre automatico fn_actualiza_opera_fin"
         --EXIT PROGRAM
         CALL fn_error_opera(p_d_pid, p_i_proceso_cod, v_i_opera_cod) 
                RETURNING r_b_valida
      
         IF(r_b_valida <> 0)THEN
            # En caso de error se muestra un mensaje a usuario y no continua
            CALL fn_desplega_inc_operacion(r_b_valida)
         END IF
      ELSE
         DISPLAY "Cierre automatico del proceso de generacion de archivo de rechazadas a recurrente de acreditados"
         -- Env�o de correo de notificaci�n de proceso finalizado
         CALL fn_correo_proceso(p_d_pid, 
                                p_i_proceso_cod, 
                                v_i_opera_cod, 
                                '', -- TMP AHM adjunto ?
                                'Cierre automatico del proceso de generaci�n de archivo de rechazadas a recurrente de acreditados',
                                'ID Proceso   : '||p_d_pid||
                                'Proceso      : '||p_i_proceso_cod||
                                'Operacion    : '||v_i_opera_cod||
                                'Fecha Inicio : '||DATE||
                                'Fecha Fin    : '||DATE
                                )
      
      END IF    }  
   {ELSE
      -- en caso de error se muestra un mensaje a usuario y no continua
      --CALL fn_muestra_inc_operacion(r_b_valida)
      DISPLAY "Program stopped ERROR en fn_inicializa_proceso"
   END IF}
   LET v_hora_actual = CURRENT YEAR TO MINUTE

   UPDATE bat_ctr_operacion
      SET fecha_fin   = v_hora_actual ,
          estado_cod  = 4
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod
      AND opera_cod   = v_i_opera_cod

   UPDATE bat_ctr_proceso
      SET fecha_fin   = TODAY,
          estado_cod  = 4
    WHERE pid         = p_d_pid
      AND proceso_cod = p_i_proceso_cod

END FUNCTION
