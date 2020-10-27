################################################################################
#Modulo            => OCG                                                      #
#Programa          => OCGP101                                                  #
#Objetivo          => Programa que realiza las validaciones del subproceso     #
#                     (1) Trámite                                              #
#Autor             => Héctor F. Jiménez Lara                                   #
#Fecha inicio      => 02 Octube 2015                                           #
#Modificaciones    => 27 Noviembre 2015  Se cambia el WS para la consulta de   #
#                                        saldo en PROCESAR                     # 
################################################################################
DATABASE safre_viv

--GLOBALS "ocgW101.inc" -- saldo preliminar PROCESAR
GLOBALS "../../cta/fte/CTAW15.inc"  -- Variables globales del Web Service

   DEFINE p_usuario             LIKE seg_usuario.usuario       -- nombre del usuario
   DEFINE v_opera_cod           LIKE cat_operacion.opera_cod   -- codigo de la operacion de la etapa
   DEFINE p_v_arch_proceso      VARCHAR(100)                   -- nombre del archivo a integrar
   DEFINE v_s_qry               STRING
   DEFINE p_pid                 DECIMAL(9,0) --EVB

   -- Variales para el uso del WS de consulta de saldo en PROCESAR
   #Parametros de conexion
   DEFINE v_url_servidor        LIKE wsv_cliente.ruta_servidor
   DEFINE v_usuario             LIKE wsv_cliente.usuario
   DEFINE v_password            LIKE wsv_cliente.password
   DEFINE v_intentos            LIKE wsv_cliente.num_reintento

MAIN

   DEFINE v_id_ocg_ctr_arch     LIKE ocg_ctr_archivo.id_ocg_ctr_archivo
   DEFINE v_programa_cod        LIKE seg_programa.programa_cod
   DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod    -- codigo del proceso
   
   DEFINE v_rec_edo_pr       RECORD
      id_derechohabiente        LIKE afi_Derechohabiente.id_derechohabiente,
      id_ocg_detalle            DECIMAL(9,0),
      id_ocg_tramite            DECIMAL(9,0),
      curp                      CHAR(18),
      nss                       CHAR(11)
   END RECORD
   
   DEFINE v_sp_error            SMALLINT
   DEFINE r_b_valida            SMALLINT 
   DEFINE v_inconsistencia      CHAR(2)
   DEFINE v_subproceso          SMALLINT 
   DEFINE v_f_inconsistencia    DATE 
   DEFINE v_cnt_aceptados       INTEGER 
   DEFINE v_cnt_rechazados      INTEGER  
   DEFINE v_estado              SMALLINT
   DEFINE v_si_credito          SMALLINT
   DEFINE v_existe_error        SMALLINT
   DEFINE v_v_nom_reporte       STRING
   DEFINE v_c_ruta_listados     LIKE seg_modulo.ruta_listados
   DEFINE v_tot_rch_sp1_val     INTEGER 

   -- se recuperan los parametros que envia el programa lanzador
   LET p_usuario        = ARG_VAL(1)
   LET p_v_arch_proceso = ARG_VAL(2)
   LET p_pid            = ARG_VAL(3)

   LET v_proceso_cod     = 3902
   LET v_opera_cod       = 1
   LET v_programa_cod    = "OCGP01"
   LET v_id_ocg_ctr_arch = 0 
   LET p_v_arch_proceso  = p_v_arch_proceso CLIPPED     -- Se eliminan los espacios en blanco
   LET v_subproceso      = 1
   LET v_estado          = 0
   
   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP01.log")

   DISPLAY "=INICIA OCGP01="
   DISPLAY "   USUARIO       : ",p_usuario
   DISPLAY "   PID           : ",p_pid  

   -- Se ibtuene la ruta listador 
   LET v_s_qry = " SELECT ruta_listados
                     FROM seg_modulo
                    WHERE modulo_cod = 'ocg' "

   PREPARE prp_r_lst FROM v_s_qry
   EXECUTE prp_r_lst INTO v_c_ruta_listados
   
   -- Se obtiene el id del archivo 
   LET v_s_qry = " SELECT id_ocg_ctr_archivo
                     FROM ocg_ctr_archivo
                    WHERE nom_archivo = ? "

   PREPARE prp_obt_id_arch FROM v_s_qry
   EXECUTE prp_obt_id_arch INTO v_id_ocg_ctr_arch 
                          USING p_v_arch_proceso 

   -- Se ejecuta la función que realiza el proceso
   LET v_s_qry = "EXECUTE FUNCTION fn_ocg_valida_subpr1_tramite(?,?)"

   PREPARE prp_exe_fn_proceso FROM v_s_qry
   EXECUTE prp_exe_fn_proceso USING v_id_ocg_ctr_arch,p_usuario
                               INTO v_sp_error

   -- verifica si ocurrió un error durante el proceos de marcaje
   IF v_sp_error <> 0 THEN
      -- Ocurrió un error, se muestra el error
      DISPLAY "OCURRIÓ UN ERROR EN EL PROCESO DE TRÁMITE: ",v_sp_error

      -- ocurrió un error y se marca como rechazado la operación
      LET r_b_valida = fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)

      LET v_estado = 1
--      EXIT PROGRAM
   END IF

   -- Se invoca a la funciòn que valida el estado en INFONAVIT
   LET v_s_qry = "EXECUTE PROCEDURE sp_valida_estado_infonavit()"

   PREPARE prp_exe_sp_edo_int FROM v_s_qry
   EXECUTE prp_exe_sp_edo_int

   -- Se obtienen los registros aceptado para consultar su estado en PROCESAR 
   LET v_s_qry = "SELECT id_derechohabiente,
                         curp,
                         id_ocg_detalle,
                         id_ocg_tramite
                    FROM ocg_tramite
                   WHERE estado    = 20 
                     AND situacion = 10"

   PREPARE prp_cons_edo_pr FROM v_s_qry
   DECLARE cur_cons_edo_pr CURSOR FOR prp_cons_edo_pr

   -- Cursor para consulta los registros procedentes, en PROCESAR
   FOREACH cur_cons_edo_pr INTO v_rec_edo_pr.id_derechohabiente,
                                v_rec_edo_pr.curp,
                                v_rec_edo_pr.id_ocg_detalle,
                                v_rec_edo_pr.id_ocg_tramite

      -- Se obtiene el nss
      LET v_s_qry = "SELECT nss 
                       FROM afi_derechohabiente
                      WHERE id_derechohabiente = ? "

      PREPARE prp_cons_nss FROM v_s_qry
      EXECUTE prp_cons_nss INTO v_rec_edo_pr.nss 
                          USING v_rec_edo_pr.id_derechohabiente

      -- Se consulta el estado con PROCESAR
      --CALL fn_cons_edo_procesar(v_rec_edo_pr.curp, v_rec_edo_pr.nss) RETURNING v_resp_diagnostico, v_res_estatus

      CALL fn_cons_procesar(v_rec_edo_pr.nss) RETURNING v_si_credito
      
      -- Si es aceptado o procedente
      --IF v_resp_diagnostico = 101 AND v_res_estatus = 101 THEN
      -- Si no tiene crédito
      IF v_si_credito = FALSE THEN  
         -- P R E G U N T A R      Q U E       H A C E R
         CONTINUE FOREACH
      ELSE 
         LET v_inconsistencia = "28"  -- Solcitud rechazada     P E N D I E N T E

         -- Se inserta la inconsistencia
         LET v_s_qry = " INSERT INTO ocg_inconsistencia
                              VALUES(?,?,?,?) "

         PREPARE prp_ins_incons FROM v_s_qry
         EXECUTE prp_ins_incons USING v_rec_edo_pr.id_ocg_tramite,
                                      v_subproceso,
                                      v_inconsistencia,
                                      v_f_inconsistencia

         -- Se actualiza  la situación de ocg_tramite a rechazado
         LET v_s_qry = " UPDATE ocg_tramite
                            SET diagnostico = 2
                          WHERE id_ocg_tramite = ? "

         PREPARE prp_upd_tram FROM v_s_qry
         EXECUTE prp_upd_tram USING v_rec_edo_pr.id_ocg_tramite
      END IF 
   END FOREACH

   -- Se actualiza el estado del SP001 a Finalizado
   LET v_s_qry = " UPDATE ocg_ctr_proceso
                      SET fin_sp1 = 1 
                    WHERE id_ocg_ctr_archivo = ?"

   PREPARE prp_upd_edo FROM v_s_qry
   EXECUTE prp_upd_edo USING v_id_ocg_ctr_arch
--******************************************************************************
   -- Se consulta el total de registros aceptados
   LET v_s_qry = " SELECT COUNT(*)
                     FROM ocg_tramite a, 
                          ocg_detalle b 
                    WHERE diagnostico = 1
                      AND a.id_ocg_detalle = b.id_ocg_detalle
                      AND b.id_ocg_ctr_archivo = ? "

   PREPARE prp_tot_acept FROM v_s_qry
   EXECUTE prp_tot_acept INTO v_cnt_aceptados USING v_id_ocg_ctr_arch

   -- Se consulta el total de registros rechazados
   LET v_s_qry = " SELECT COUNT(*)
                     FROM ocg_tramite a, 
                          ocg_detalle b 
                    WHERE diagnostico = 2
                      AND a.id_ocg_detalle = b.id_ocg_detalle
                      AND b.id_ocg_ctr_archivo = ? "


   PREPARE prp_tot_rech FROM v_s_qry
   EXECUTE prp_tot_rech INTO v_cnt_rechazados  USING v_id_ocg_ctr_arch

   -- Obtiene total rechazos en validación del SP001  
   LET v_s_qry = "SELECT tot_sp1_rch_val
                    FROM safre_tmp:tmp_ocg_cifras;"

   PREPARE prp_rch_sp1 FROM v_s_qry
   EXECUTE prp_rch_sp1 INTO v_tot_rch_sp1_val
   
   DISPLAY "\n=> TOTAL ACEPTADOS:",v_cnt_aceptados
   DISPLAY "=> TOTAL RECHAZOS OPERATIVOS:",v_cnt_rechazados
   DISPLAY "=> TOTAL RECHAZOS VALIDACIÓN:",v_tot_rch_sp1_val
   
--******************************************************************************
 {  LET v_s_qry = " UPDATE ocg_tramite
                      SET f_vigencia = (today + 149)
                    WHERE diagnostico = 1
                      AND id_ocg_detalle = (SELECT id_ocg_detalle 
                                              FROM ocg_detalle 
                                             WHERE id_ocg_ctr_archivo = ? )"

   PREPARE prp_updt FROM v_s_qry
   EXECUTE prp_updt USING v_id_ocg_ctr_arch
}
 
--******************************************************************************
   IF v_estado = 0 THEN

       UPDATE tmp_ctr_subproceso
                        SET sp001 = 1
                      WHERE id_ocg_ctr_arch = v_id_ocg_ctr_arch

      CALL fn_actualiza_opera_fin(p_pid,
                                  v_proceso_cod,
                                  v_opera_cod) RETURNING v_estado
   ELSE
      --Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, v_proceso_cod, v_opera_cod)  RETURNING v_estado
   END IF
--******************************************************************************
   DISPLAY "=FIN="

END MAIN

FUNCTION fn_cons_procesar(p_nss)
   DEFINE p_nss CHAR(11)
   DEFINE v_datos_entrada       tConsultaSaldoVO
   -- Variables para los valores de retorno WS
   DEFINE soapStatus            INTEGER
   DEFINE v_rec_retorno         tConsultaSaldoRespVO
   DEFINE v_rec_afi         RECORD
      ap_paterno                CHAR(40),
      ap_materno                CHAR(40),
      nombre                    CHAR(40)
   END RECORD
   DEFINE v_ax_nss              CHAR(11)
   DEFINE v_ws_status           LIKE wsv_his_err_cliente.ws_status
   DEFINE v_cod_error           LIKE wsv_his_err_cliente.cod_error
   DEFINE v_bnd_credito         SMALLINT

   LET v_datos_entrada.nss = p_nss

   -- Se invoca a la función que configura el WS obteniendo los datos de la tabla wsv_cliente
   CALL fn_configura_ws() RETURNING v_url_servidor,v_usuario,v_password

   LET v_ax_nss = v_datos_entrada.nss
   --Realiza la consulta de los datos del derechohabiente con el nss ingresado
   LET v_s_qry = " SELECT ap_paterno_af,
                          ap_materno_af,
                          nombre_af
                     FROM afi_derechohabiente
                    WHERE nss = ? "

   PREPARE prp_cons_dh FROM v_s_qry
   EXECUTE prp_cons_dh USING v_ax_nss INTO v_rec_afi.ap_paterno,
                                           v_rec_afi.ap_materno,
                                           v_rec_afi.nombre

   LET v_datos_entrada.apePaterno = v_rec_afi.ap_paterno CLIPPED
   LET v_datos_entrada.apeMaterno = v_rec_afi.ap_materno CLIPPED
   LET v_datos_entrada.nombres    = v_rec_afi.nombre CLIPPED 

    -- Se invoca a la función que ejecuta el web service
   CALL consultaSaldo(v_url_servidor CLIPPED,
                      v_usuario,
                      v_password,
                      v_datos_entrada.apeMaterno,
                      v_datos_entrada.apePaterno,
                      v_datos_entrada.nombres,
                      v_datos_entrada.nss)
                      RETURNING soapStatus,
                                ConsultaSaldoRespVO.apeMaternoBD,
                                ConsultaSaldoRespVO.apePaternoBD,
                                ConsultaSaldoRespVO.diagProceso,
                                ConsultaSaldoRespVO.nombresBD,
                                ConsultaSaldoRespVO.nss,
                                ConsultaSaldoRespVO.numAIVS92,
                                ConsultaSaldoRespVO.numAIVS97,
                                ConsultaSaldoRespVO.origenTipoCredito,
                                ConsultaSaldoRespVO.resultOperacion,
                                ConsultaSaldoRespVO.tramiteJudicial

   -- Si no hay ningún error, se despliegan los datos obtenidos del WS en la forma
   IF soapStatus = 0 THEN
      IF ConsultaSaldoRespVO.origenTipoCredito IS NULL OR ConsultaSaldoRespVO.origenTipoCredito = " " THEN  
         LET v_bnd_credito = FALSE
      ELSE 
         -- se indica que si tiene credito 
         LET v_bnd_credito = TRUE
      END IF 
   ELSE
      LET v_rec_retorno.diagProceso     = "-1"
      LET v_rec_retorno.resultOperacion = "-1"

      LET v_ws_status = soapStatus
      LET v_cod_error = wsError.code

      DISPLAY "Código de error : ",v_cod_error
      DISPLAY "Error con el WS"
      DISPLAY "WS ESTATUS : ",soapStatus
      DISPLAY "Errores"
      DISPLAY wsError.action
      DISPLAY wsError.code
      DISPLAY wsError.codeNS
      DISPLAY wsError.description
      LET v_bnd_credito = TRUE

   END IF

   RETURN v_bnd_credito 
END FUNCTION 

FUNCTION fn_configura_ws()
   DEFINE v_consulta            STRING

   #La clave 'cre_3' del catalogo de clientes de webServices corresponde a la solicitud de saldo
   LET v_consulta = " SELECT ruta_servidor,
                             usuario,
                             password,
                             num_reintento
                        FROM wsv_cliente
                       WHERE cve_cliente = 'cre_3' "

   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta INTO  v_url_servidor,
                              v_usuario,
                              v_password,
                              v_intentos

   RETURN v_url_servidor, v_usuario,v_password

END FUNCTION

{ -- Cambios del 27/11/2015 de deja de usar este WS para la consulta de slado en PROCESAR
#Objetivo  : Consume el WS de consulta Saldo preeliminar con PROCESAR                            
FUNCTION fn_cons_edo_procesar(p_curp, p_nss)
   DEFINE v_resultado   SMALLINT
   DEFINE v_diagnostico SMALLINT       -- respuesta del WS, 101 - Aceptado
   DEFINE v_estatus     SMALLINT       -- estatus de la cuenta individual
   DEFINE p_curp        CHAR(18)       -- curp como parametro recibido por la función
   DEFINE p_nss         CHAR(11)       -- nss como parametro recibido por la función

   DISPLAY "Prueba de comunicaciòn con PROCESAR, saldo previo"
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idSistema      = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idEbusiness    = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idPortafolio   = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idServicio     = 49
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idCliente      = 30 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.idCanal        = 7 
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.codoperCliente = "INFONAVIT"    
   LET ns2parametersConsultarSaldoPreliminarRequest.idssn.fecha          = CURRENT YEAR TO FRACTION

   -- R E V I S A R    C O N    M A U R O
   LET ns2parametersConsultarSaldoPreliminarRequest.cuerpo.curp = p_curp
   LET ns2parametersConsultarSaldoPreliminarRequest.cuerpo.nss  = p_nss
  
   -- se invoca el servicio
   DISPLAY "Ejecutando WSSaldoPreliminar..."

   CALL ConsultarSaldoPreliminar_g() RETURNING v_resultado

   IF ( v_resultado <> 0 ) THEN      
      DISPLAY "========================================================="
      DISPLAY " Resultado de la ejecución: ", v_resultado
      DISPLAY " CODE       : ", wsError.code
      DISPLAY " CODENS     : ", wsError.codeNS
      DISPLAY " DESCRIPTION: ", wsError.description
      DISPLAY " ACTION     : ", wsError.action
      DISPLAY "=========================================================\n"

      -- se devuelve el codigo de error del WS y fecha nula
      RETURN wsError.code, NULL
   ELSE 
      LET v_diagnostico = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.diagnostico
      LET v_estatus     = ns2parametersConsultarSaldoPreliminarResponse.objetoRespuesta.estatusCuentaIndividual
   END IF

   RETURN v_diagnostico, v_estatus
END FUNCTION 
}
