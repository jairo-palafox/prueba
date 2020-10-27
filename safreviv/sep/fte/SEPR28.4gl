--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05-05-2019
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPR28                                                   #
#Objetivo          => Programa batch Reverso de Archivo de Ajuste al Cred      # 
#Fecha inicio      => SEP 5 MAY 2019                                           #
################################################################################
DATABASE safre_viv

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
       v_registros_restitucion RECORD
         v_id_expediente 	VARCHAR(9),--DECIMAL(9,0),
         v_invadido 		CHAR(011),
         v_asociado 		CHAR(011),
         v_nss_restitucion  CHAR(011),
         v_aivs_sar92 		DECIMAL(22,2),
         v_pesos_sar92 		DECIMAL(22,2),
         v_Aivs_viv97 		DECIMAL(22,2),
         v_pesos_viv97 		DECIMAL(22,2),
         v_aivs_subsc 		DECIMAL(22,2),
         v_pesos_subsc 		DECIMAL(22,2),
         v_tipo_restitucion CHAR(40)
       END RECORD,
       v_manejador_rpt     OM.SaxDocumentHandler,
       v_fecha_actual      STRING,
       v_total_procesados  INTEGER ,
       p_id_expediente     LIKE sep_expediente.id_expediente ,
       v_arch_restitucion  LIKE sep_expediente.docto_restitucion

MAIN
DEFINE v_consulta           STRING,
       v_proceso_desc       LIKE cat_proceso.proceso_desc,
       v_opera_desc         LIKE cat_operacion.opera_desc,
       v_conteo_solicitudes INTEGER,
       v_conteo_sol_anlisis INTEGER,
       v_conteo_sol_115     INTEGER,
       v_conteo_sol_no_apl  INTEGER,
       r_resultado_opera    SMALLINT,
       v_sql_error          INTEGER,
       v_msg_error          VARCHAR(200),
       v_msg_correo         STRING,
       r_ruta_ejecutable    LIKE seg_modulo.ruta_bin,
       r_ruta_lst           LIKE seg_modulo.ruta_listados,
       v_nom_reporte        STRING,
       v_contador           INTEGER

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5) -- se recibe el id_expediente
   LET v_nom_archivo = ARG_VAL(6)

   LET p_id_expediente = p_folio
   
   --WHENEVER ERROR CONTINUE
   LET v_total_procesados = 0
   LET v_msg_correo = ' ' 
   LET v_fecha_actual = YEAR(TODAY) CLIPPED
   LET v_fecha_actual = MONTH(TODAY) USING "&&" CLIPPED,"-",
                        DAY(TODAY) USING "&&" CLIPPED,"-",
                        v_fecha_actual.trim()

   --CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario_cod)
      --   RETURNING p_folio

      
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_opera_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = 1

   DATABASE safre_viv

   DISPLAY "PROCESO   : ",v_proceso_desc
   DISPLAY "OPERACIÓN : ",v_opera_desc
   DISPLAY "FECHA     : ",TODAY USING "dd-mm-yyyy"
   DISPLAY ""   
   DISPLAY "EXPEDIENTE: ",p_id_expediente   
   DISPLAY "ARCHIVO   : ",v_nom_archivo

   # Reverso de archivo de ajuste al credito

   DELETE FROM sep_batch_contabilidad
   WHERE  id_expediente = p_id_expediente 

DISPLAY "Actualizando estado a 40 (DICTAMEN REGISTRADO..."   

   # Actualiza los documentos de restitucion a procesados
    UPDATE sep_expediente
       SET ind_ajuste                = 0  ,
           docto_ajuste              = "" 
     WHERE id_expediente  = p_id_expediente 
       AND estado IN (40,45,-1,50) # 40 = Dictamen Registrado, 45 = Restitucion Solicitada, 50 = Restitucion Liquidada 
       AND ind_ajuste = 2
            
         CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                                     RETURNING r_resultado_opera
         # si ocurrió un error con la actualizacion de la operacion operacion 
         # muestra el mensaje
         IF(r_resultado_opera)THEN
            CALL fn_desplega_inc_operacion(r_resultado_opera)
            LET v_msg_correo = "OCURRIÓ UN ERROR AL FINALIZAR OPERACIÓN"
         ELSE 
         
DISPLAY ""
DISPLAY "REVERSO ARCHIVO AJUSTE AL CREDITO EXITOSO"
         
         END IF

   # Envío de correo de notificación de proceso finalizado
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          '',
                          'Reverso Archivo AJUSTE AL CREDITO SEP',
                          v_msg_correo||
                          ''||
                          'ID Proceso   : '||p_pid||
                          'Proceso      : '||p_proceso_cod||
                          'Operacion    : '||p_opera_cod||
                          'Fecha Inicio : '||v_fecha_actual||
                          'Fecha Fin    : '||DATE
                          )
   
END MAIN
