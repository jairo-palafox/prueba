##########################################################################
#Modulo            => AGR                                                #
#Programa          => AGRP53                                             #
#Objetivo          => Actualiza Remanentes para solicitud de saldo a     #
#                     Procesar.                                          #
#Autor             => Emilio Abarca, EFP                                 #
#Fecha inicio      => 09/Octubre/2018.                                   #
##########################################################################

DATABASE safre_viv

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

GLOBALS 
   DEFINE p_usuario        CHAR(20)
   DEFINE p_pid            DECIMAL(9,0)
   DEFINE p_proceso_cod    SMALLINT  
   DEFINE p_opera_cod      SMALLINT
   DEFINE p_archivo        CHAR(40)
   DEFINE p_ruta_archivo   STRING
   DEFINE v_ruta_bin       CHAR(40)
   DEFINE v_ruta_envio     CHAR(40)
   DEFINE v_ruta_lst       CHAR(40)
   DEFINE r_b_valida       SMALLINT

   #Parametros de conexion
   DEFINE v_url_servidor LIKE wsv_cliente.ruta_servidor
   DEFINE v_usuario      LIKE wsv_cliente.usuario
   DEFINE v_password     LIKE wsv_cliente.password
   DEFINE v_intentos     LIKE wsv_cliente.num_reintento
   DEFINE soapStatus     INTEGER

   -- Variables para el reporte pdf
   TYPE rec_total        RECORD
      total          INTEGER,
      aivs92         DECIMAL(16,6),
      aivs97         DECIMAL(16,6),
      porcentaje     CHAR(12)
   END RECORD
   -- total global
   DEFINE r_total_rema    rec_total
   -- Aceptados
   DEFINE r_total_glo     rec_total
   DEFINE r_t_marca1      rec_total
   DEFINE r_t_marca4      rec_total
   -- Rechazados
   DEFINE r_total_glo_rch rec_total
   DEFINE r_t_marca1_rch  rec_total
   DEFINE r_t_marca4_rch  rec_total
   DEFINE v_f_ini_opera   LIKE bat_ctr_operacion.fecha_ini
   DEFINE v_f_fin_opera   LIKE bat_ctr_operacion.fecha_fin

END GLOBALS 

MAIN
   --Recibe parámetros enviados por AGRL75
   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_archivo      = ARG_VAL(5)
   LET p_ruta_archivo = ARG_VAL(6)

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP53.log")

   SELECT ruta_bin,
          ruta_listados,
          ruta_envio
     INTO v_ruta_bin,
          v_ruta_lst,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr'

   CALL fn_display_proceso(0,"CARGA REMANENTES")
   DISPLAY ""
   DISPLAY " ARCHIVO: ",p_archivo CLIPPED
   DISPLAY " RUTA   : ",p_ruta_archivo CLIPPED
   DISPLAY ""

   -- Carga de registros del archivo a la temporal
   CALL fn_carga_archivo_temporal()

   -- Se invoca a la función que configura el WS obteniendo los datos de la tabla wsv_cliente
   CALL fn_configura_ws() RETURNING v_url_servidor,v_usuario,v_password

   -- Procesa información
   CALL fn_procesa_informacion()

   --Al pasar los filtros finaliza el proceso como OK.
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   -- Genera reporte PDF
   CALL fn_genera_reporte()
   
   CALL fn_display_proceso(1,"CARGA REMANENTES")
   
END MAIN 

FUNCTION fn_carga_archivo_temporal()

   DEFINE ch              base.channel
   DEFINE v_linea         CHAR(66)
   DEFINE r_inf_remanente RECORD
      nss         CHAR(11),
      aivs92      CHAR(15),
      aivs97      CHAR(15),
      tpo_credito CHAR(3) ,
      num_credito CHAR(10),
      f_origina   CHAR(8) ,
      marca_ifv   CHAR(3) ,
      tpo_origina CHAR(1)
   END RECORD

   -- Crea tabla temporal
   CALL crea_temporal()

   INITIALIZE r_inf_remanente.* TO NULL

   LET ch = base.Channel.create() # Creamos un objeto de la clase channel
   CALL ch.openFile(p_ruta_archivo,"r")

   DISPLAY " > Carga información en temporal"
   DISPLAY ""

   WHILE TRUE

      LET v_linea = ch.readLine()

      IF(ch.isEof()) THEN
         EXIT WHILE
         DISPLAY "no encuentra registros"
      ELSE 
         -- Recupera información por linea
         LET r_inf_remanente.nss         = v_linea[1,11]
         LET r_inf_remanente.aivs92      = v_linea[12,26]
         LET r_inf_remanente.aivs97      = v_linea[27,41]
         LET r_inf_remanente.tpo_credito = v_linea[42,44]
         LET r_inf_remanente.num_credito = v_linea[45,54]
         LET r_inf_remanente.f_origina   = v_linea[59,60],v_linea[61,62],v_linea[55,58]  -- Fecha MMDDYYYY
         LET r_inf_remanente.marca_ifv   = v_linea[63,65]
         LET r_inf_remanente.tpo_origina = v_linea[66,66]

         INSERT INTO safre_tmp:tmp_det_remanente
            VALUES (r_inf_remanente.nss,
                    r_inf_remanente.aivs92,
                    r_inf_remanente.aivs97,
                    r_inf_remanente.tpo_credito,
                    r_inf_remanente.num_credito,
                    r_inf_remanente.f_origina,
                    r_inf_remanente.marca_ifv,
                    r_inf_remanente.tpo_origina);
      END IF 
      
   END WHILE

   -- Cierra archivo
   CALL ch.close()

END FUNCTION

FUNCTION fn_procesa_informacion()

   DEFINE r_remanente       RECORD
      nss                 CHAR(11)     ,
      tpo_credito         SMALLINT     ,
      num_credito         DECIMAL(10,0),
      marca_ifv           SMALLINT     ,
      tpo_originacion     SMALLINT
   END RECORD

   DEFINE bnd_diag         SMALLINT
   DEFINE v_id_dh          DECIMAL(9,0)
   DEFINE v_t_tpo_cred     INTEGER
   DEFINE v_aux_marca_ifv  SMALLINT
   DEFINE v_aux_marca_prc  SMALLINT
   DEFINE v_precio_fondo   DECIMAL(19,14)
   DEFINE v_aux_aivs92     DECIMAL(16,6)
   DEFINE v_aux_aivs97     DECIMAL(16,6)
   DEFINE v_saldo_total    DECIMAL(18,2)
   DEFINE v_marca_impide   SMALLINT
   DEFINE v_ind_conv       SMALLINT
   DEFINE v_pesos_92_ws    DECIMAL(16,2)
   DEFINE v_pesos_97_ws    DECIMAL(16,2)
   DEFINE v_total_saldo_ws DECIMAL(18,2)

   DEFINE r_acreditado     RECORD
      id_cre_acreditado   DECIMAL(9,0),
      id_derechohabiente  DECIMAL(9,0),
      ap_paterno_af       LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af       LIKE afi_derechohabiente.ap_materno_af,
      nombre_af           LIKE afi_derechohabiente.nombre_af,
      sdo_deudor          DECIMAL(12,2),
      estado              SMALLINT,
      edo_procesar        SMALLINT,
      entidad             SMALLINT
   END RECORD
   DEFINE v_t_registros    INTEGER
   DEFINE v_t_aceptados    INTEGER
   DEFINE v_t_rechazados   INTEGER
   DEFINE v_id_cre_ctr_arh DECIMAL(9,0)
   DEFINE v_aux_origen     CHAR(2)

   --Obtiene el valor de precio fondo al día
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
      AND fondo       = 11;

   SELECT COUNT(*)
     INTO v_t_registros
     FROM safre_tmp:tmp_det_remanente
    WHERE nss IS NOT NULL;

   -- Guarda control del archivo.
   INSERT INTO cre_ctr_archivo(
                  id_cre_ctr_archivo,
                  folio_archivo     ,
                  lote              ,
                  f_lote            ,
                  id_proceso        ,
                  operacion         ,
                  nom_archivo       ,
                  tot_registros     ,
                  tot_aceptados     ,
                  tot_rechazados    ,
                  tot_sin_origen    ,
                  estado            ,
                  f_proceso         ,
                  usuario)
           VALUES(seq_cre_archivo.NEXTVAL,
                  0               ,
                  0               ,
                  NULL            ,
                  p_proceso_cod   ,
                  p_opera_cod     ,
                  p_archivo       ,
                  v_t_registros   ,
                  0               ,
                  0               ,
                  0               ,
                  10              ,
                  TODAY           ,
                  p_usuario);

   -- Recupera identificador
   SELECT FIRST 1 id_cre_ctr_archivo
     INTO v_id_cre_ctr_arh
     FROM cre_ctr_archivo
    WHERE id_proceso = p_proceso_cod
      AND operacion  = p_opera_cod
      AND estado     = 10
   ORDER BY id_cre_ctr_archivo DESC;

   DECLARE crs_inf_temporal CURSOR FOR
   SELECT UNIQUE nss ,
          tpo_credito,
          num_credito,
          marca_ifv  ,
          tpo_originacion
     FROM safre_tmp:tmp_det_remanente
    WHERE nss IS NOT NULL;

   INITIALIZE r_remanente.* TO NULL
   INITIALIZE r_acreditado.* TO NULL

   DISPLAY " > Procesa información"
   DISPLAY ""

   LET v_t_aceptados  = 0
   LET v_t_rechazados = 0

   FOREACH crs_inf_temporal INTO r_remanente.nss           ,
                                 r_remanente.tpo_credito   ,
                                 r_remanente.num_credito   ,
                                 r_remanente.marca_ifv     ,
                                 r_remanente.tpo_originacion

      LET bnd_diag         = 0  -- Procedente
      LET v_aux_aivs92     = 0
      LET v_aux_aivs97     = 0
      LET v_saldo_total    = 0
      LET v_aux_origen     = NULL

      # Verifica DH
      SELECT id_derechohabiente
        INTO v_id_dh
        FROM afi_derechohabiente
       WHERE nss = r_remanente.nss

      IF(v_id_dh IS NULL) THEN
         LET bnd_diag = 11  -- Trabajador no existe
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss            ,
                   NULL                       ,
                   NULL                       ,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   NULL                       ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Valida tipo de crédito cargado
      SELECT COUNT(*)
        INTO v_t_tpo_cred
        FROM cat_tipo_cred_rem
       WHERE tpo_credito =  r_remanente.tpo_credito;

      IF(v_t_tpo_cred = 0) THEN
         LET bnd_diag = 16  -- Tipo de crédito no válido
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss            ,
                   NULL                       ,
                   NULL                       ,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   NULL                       ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Verifica que sea un acreditado vigente
      SELECT FIRST 1                                               
             a.id_cre_acreditado ,
             a.id_derechohabiente,
             f.ap_paterno_af     ,
             f.ap_materno_af     ,
             f.nombre_af         ,
             a.sdo_deudor        ,
             a.estado            ,
             a.edo_procesar      ,
             m.entidad
        INTO r_acreditado.id_cre_acreditado ,
             r_acreditado.id_derechohabiente,
             r_acreditado.ap_paterno_af     ,
             r_acreditado.ap_materno_af     ,
             r_acreditado.nombre_af         ,
             r_acreditado.sdo_deudor        ,
             r_acreditado.estado            ,
             r_acreditado.edo_procesar      ,
             r_acreditado.entidad
        FROM cre_acreditado a  ,
             afi_derechohabiente f,
             cat_maq_credito m
       WHERE a.id_derechohabiente = v_id_dh
         AND a.id_derechohabiente = f.id_derechohabiente
         AND a.num_credito        = r_remanente.num_credito
         AND a.tpo_credito        = r_remanente.tpo_credito
         AND a.tpo_originacion    = r_remanente.tpo_originacion
      -- AND a.f_otorga           = r_remanente.f_originacion
         AND a.estado = m.estado
         AND m.entidad IN (1,2)
       ORDER BY a.id_cre_acreditado DESC;

      IF(r_acreditado.id_cre_acreditado IS NULL)  OR
        (r_acreditado.entidad = 2) THEN
        LET bnd_diag = 13  -- No existe marca crédito vigente
        INSERT INTO safre_tmp:tmp_det_remanente_diag
           VALUES(r_remanente.nss            ,
                  NULL                       ,
                  NULL                       ,
                  v_aux_aivs92               ,
                  v_aux_aivs97               ,
                  r_remanente.tpo_originacion,
                  NULL                       ,
                  bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Verifica Deudor liquidado
      IF(r_acreditado.estado <> 140) AND
        (r_acreditado.estado <> 145) AND
        (r_acreditado.estado <> 220) AND
        (r_acreditado.estado <> 900) THEN
         LET bnd_diag = 31  -- Deudor vigente
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss                ,
                   r_acreditado.id_cre_acreditado ,
                   r_acreditado.id_derechohabiente,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   r_acreditado.estado        ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Verifica Saldo transferido Procesar
      IF(r_acreditado.edo_procesar <> 120) THEN
         LET bnd_diag = 15  -- Estado del crédito no válido
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss                ,
                   r_acreditado.id_cre_acreditado ,
                   r_acreditado.id_derechohabiente,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   r_acreditado.estado        ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Valida marca activa
      SELECT MAX(marca)
        INTO v_aux_marca_ifv
        FROM sfr_marca_activa
       WHERE id_derechohabiente = v_id_dh
         AND marca = r_remanente.marca_ifv;

      IF(v_aux_marca_ifv IS NULL) THEN
          LET bnd_diag    = 13  -- No existe marca de crédito vigente
          INSERT INTO safre_tmp:tmp_det_remanente_diag
             VALUES(r_remanente.nss                ,
                    r_acreditado.id_cre_acreditado ,
                    r_acreditado.id_derechohabiente,
                    v_aux_aivs92               ,
                    v_aux_aivs97               ,
                    r_remanente.tpo_originacion,
                    r_acreditado.estado        ,
                    bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Valida marca interna Procesar
      -- Dependiendo del tipo de originación es la marca interna Procesar
      LET v_aux_marca_prc = NULL

      CASE
         WHEN r_remanente.tpo_originacion = 1
            LET v_aux_marca_prc = 231
            LET v_aux_origen    = "01"
         WHEN r_remanente.tpo_originacion = 4
            LET v_aux_marca_prc = 234
            LET v_aux_origen    = "04"
      END CASE

     SELECT MAX(marca)
       INTO v_aux_marca_prc
       FROM sfr_marca_activa
      WHERE id_derechohabiente = v_id_dh
        AND marca = v_aux_marca_prc;

      IF(v_aux_marca_prc IS NULL) THEN
         LET bnd_diag    = 30  -- Registro no marcado en Procesar
         INSERT INTO safre_tmp:tmp_det_remanente_diag
             VALUES(r_remanente.nss                ,
                    r_acreditado.id_cre_acreditado ,
                    r_acreditado.id_derechohabiente,
                    v_aux_aivs92               ,
                    v_aux_aivs97               ,
                    r_remanente.tpo_originacion,
                    r_acreditado.estado        ,
                    bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Valida el saldo en SACI
      -- vivienda 92
      SELECT SUM(monto_acciones)
        INTO v_aux_aivs92
        FROM cta_movimiento
       WHERE id_derechohabiente = v_id_dh
         AND subcuenta = 8
         AND fondo_inversion = 11;

      IF(v_aux_aivs92 IS NULL) THEN
         LET v_aux_aivs92 = 0
      END IF

      SELECT SUM(monto_acciones)
        INTO v_aux_aivs97
        FROM cta_movimiento
       WHERE id_derechohabiente = v_id_dh
         AND subcuenta = 4
         AND fondo_inversion = 11;

      IF(v_aux_aivs97 IS NULL) THEN
         LET v_aux_aivs97 = 0
      END IF

      -- Saldo en pesos
      LET v_saldo_total = (v_aux_aivs92 + v_aux_aivs97) * v_precio_fondo

      IF(v_saldo_total = 0) THEN
         LET bnd_diag = 8   -- Cuenta con saldo cero
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss                ,
                   r_acreditado.id_cre_acreditado ,
                   r_acreditado.id_derechohabiente,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   r_acreditado.estado        ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      ELSE
         IF(v_saldo_total < 0) THEN
            LET bnd_diag = 34   -- Cuenta con saldo negativo
            INSERT INTO safre_tmp:tmp_det_remanente_diag
               VALUES(r_remanente.nss                ,
                      r_acreditado.id_cre_acreditado ,
                      r_acreditado.id_derechohabiente,
                      v_aux_aivs92               ,
                      v_aux_aivs97               ,
                      r_remanente.tpo_originacion,
                      r_acreditado.estado        ,
                      bnd_diag);

            LET v_t_rechazados = v_t_rechazados + 1
            CONTINUE FOREACH
         END IF
      END IF

      # Verifica si existe una marca que impida la petición
      DECLARE crs_marca_operativa CURSOR FOR
      SELECT marca
        FROM sfr_marca_activa
       WHERE id_derechohabiente = v_id_dh
         AND marca NOT IN (r_remanente.marca_ifv,v_aux_marca_prc);

      LET v_ind_conv = 0 -- convive

      FOREACH crs_marca_operativa INTO v_marca_impide
         -- Verifica convivencia
         SELECT ind_convivencia
           INTO v_ind_conv
           FROM sfr_convivencia
          WHERE marca_activa = v_marca_impide
            AND marca_entra  = r_remanente.marca_ifv;

         -- Con uno que no conviva se rechaza
         IF(v_ind_conv = 20) THEN
            EXIT FOREACH
         END IF
      END FOREACH

      IF(v_ind_conv = 20) THEN
         LET bnd_diag = 32
         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss                ,
                   r_acreditado.id_cre_acreditado ,
                   r_acreditado.id_derechohabiente,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   r_acreditado.estado        ,
                   bnd_diag);

         LET v_t_rechazados = v_t_rechazados + 1
         CONTINUE FOREACH
      END IF

      # Valida consulta saldo WS Procesar
      LET v_pesos_92_ws    = 0
      LET v_pesos_97_ws    = 0
      LET v_total_saldo_ws = 0

      -- Se invoca a la función que ejecuta el web service
      CALL consultaSaldo(v_url_servidor CLIPPED,
                         v_usuario,
                         v_password,
                         r_acreditado.ap_materno_af CLIPPED,
                         r_acreditado.ap_paterno_af CLIPPED,
                         r_acreditado.nombre_af CLIPPED,
                         r_remanente.nss)
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

      {
      DISPLAY ""
      DISPLAY "NSS: ",r_remanente.nss
      DISPLAY "AP_MATERNO: ",r_acreditado.ap_materno_af ," WS: ",ConsultaSaldoRespVO.apeMaternoBD
      DISPLAY "AP_PATERNO: ",r_acreditado.ap_paterno_af ," WS: ",ConsultaSaldoRespVO.apePaternoBD
      DISPLAY "NOMBRE: ", r_acreditado.nombre_af," WS: ",ConsultaSaldoRespVO.nombresBD
      DISPLAY "SOAPSTATUS: ",soapStatus
      DISPLAY ""
      }

      IF(soapStatus = 0) THEN

         {
         DISPLAY "AIVS92: ",ConsultaSaldoRespVO.numAIVS92
         DISPLAY "AIVS97: ",ConsultaSaldoRespVO.numAIVS97
         }

         LET v_pesos_92_ws    = ConsultaSaldoRespVO.numAIVS92 * v_precio_fondo
         LET v_pesos_97_ws    = ConsultaSaldoRespVO.numAIVS97 * v_precio_fondo
         LET v_total_saldo_ws = v_pesos_92_ws + v_pesos_97_ws

         -- Verifica marca y saldo en Procesar
         IF(v_aux_origen <> ConsultaSaldoRespVO.origenTipoCredito) THEN
            LET bnd_diag = 30  -- Registro no marcado en Procesar
         ELSE
            IF(v_total_saldo_ws <= 0) THEN
               LET bnd_diag = 35 -- Saldo cero en Procesar
            END IF
         END IF
         
         IF(bnd_diag <> 0) THEN
            INSERT INTO safre_tmp:tmp_det_remanente_diag
               VALUES(r_remanente.nss                ,
                      r_acreditado.id_cre_acreditado ,
                      r_acreditado.id_derechohabiente,
                      v_aux_aivs92               ,
                      v_aux_aivs97               ,
                      r_remanente.tpo_originacion,
                      r_acreditado.estado        ,
                      bnd_diag);

            LET v_t_rechazados = v_t_rechazados + 1
            CONTINUE FOREACH
         END IF

      END IF

      -- Aceptados
      IF(bnd_diag = 0) THEN

         INSERT INTO safre_tmp:tmp_det_remanente_diag
            VALUES(r_remanente.nss                ,
                   r_acreditado.id_cre_acreditado ,
                   r_acreditado.id_derechohabiente,
                   v_aux_aivs92               ,
                   v_aux_aivs97               ,
                   r_remanente.tpo_originacion,
                   r_acreditado.estado        ,  -- Guarda el estado anterior
                   bnd_diag);

         IF(r_acreditado.sdo_deudor IS NULL) OR
           (r_acreditado.sdo_deudor = " ") OR
           (r_acreditado.sdo_deudor = 0) THEN
            LET r_acreditado.sdo_deudor = 1
         END IF

         -- Actualiza la tabla maestra para la solicitud de saldo a Procesar
         UPDATE cre_acreditado
            SET sdo_deudor   = r_acreditado.sdo_deudor,
                estado       = 25, -- Vigente saldo remanente
                edo_procesar = 70 -- Solicitud por reenviar
          WHERE id_cre_acreditado  = r_acreditado.id_cre_acreditado
            AND id_derechohabiente = v_id_dh;

         -- Guarda en tabla histórica la solicitud remanente.
         INSERT INTO cre_his_remanente(
                        id_referencia,
                        id_cre_ctr_archivo,
                        id_derechohabiente,
                        tpo_originacion,
                        aivs92,
                        aivs97,
                        f_proceso)
                 VALUES(r_acreditado.id_cre_acreditado,
                        v_id_cre_ctr_arh,
                        r_acreditado.id_derechohabiente,
                        r_remanente.tpo_originacion,
                        v_aux_aivs92,
                        v_aux_aivs97,
                        TODAY);

         LET v_t_aceptados = v_t_aceptados + 1

      END IF
      
   END FOREACH

   -- Actualiza archivo con estado = 2 (Integrado)
   PREPARE prp_act_archivo FROM "EXECUTE PROCEDURE sp_act_cre_ctr_archivo(0,?,?,0,?)"
   EXECUTE prp_act_archivo USING v_t_aceptados,
                                 v_t_rechazados,
                                 v_id_cre_ctr_arh

END FUNCTION

FUNCTION fn_genera_reporte()

   DEFINE v_reporte_bin    STRING
   DEFINE v_ruta_rpt       STRING
   DEFINE object_rpt       om.SaxDocumentHandler

   DEFINE r_aux_total      RECORD
      tpo_originacion   SMALLINT,
      diagnostico       SMALLINT,
      aivs92            DECIMAL(16,6),
      aivs97            DECIMAL(16,6),
      total             INTEGER
   END RECORD
   DEFINE v_aux_porcentaje DECIMAL(6,2)

   -- Se obtiene la inf. de la temporal de aceptados y rechazados
   DECLARE crs_remanente_diag CURSOR FOR
   SELECT tpo_originacion,
          diagnostico    ,
          SUM(aivs92)    ,
          SUM(aivs97)    ,
          COUNT(*)
     FROM safre_tmp:tmp_det_remanente_diag
     GROUP BY 1,2;

   INITIALIZE r_aux_total.* TO NULL

   -- Total global
   LET r_total_rema.total  = 0
   LET r_total_rema.aivs92 = 0
   LET r_total_rema.aivs97 = 0
   -- Inicializa valores aceptados
   LET r_total_glo.total  = 0
   LET r_total_glo.aivs92 = 0
   LET r_total_glo.aivs97 = 0
   LET r_t_marca1.total   = 0
   LET r_t_marca1.aivs92  = 0
   LET r_t_marca1.aivs97  = 0
   LET r_t_marca4.total   = 0
   LET r_t_marca4.aivs92  = 0
   LET r_t_marca4.aivs97  = 0
   -- Inicializa valores rechazados
   LET r_total_glo_rch.total  = 0
   LET r_total_glo_rch.aivs92 = 0
   LET r_total_glo_rch.aivs97 = 0
   LET r_t_marca1_rch.total   = 0
   LET r_t_marca1_rch.aivs92  = 0
   LET r_t_marca1_rch.aivs97  = 0
   LET r_t_marca4_rch.total   = 0
   LET r_t_marca4_rch.aivs92  = 0
   LET r_t_marca4_rch.aivs97  = 0

   DISPLAY " > Calcula información para reporte PDF"
   DISPLAY ""

   FOREACH crs_remanente_diag INTO r_aux_total.tpo_originacion,
                                   r_aux_total.diagnostico,
                                   r_aux_total.aivs92,
                                   r_aux_total.aivs97,
                                   r_aux_total.total

      LET r_total_rema.total  = r_total_rema.total  + r_aux_total.total
      LET r_total_rema.aivs92 = r_total_rema.aivs92 + r_aux_total.aivs92
      LET r_total_rema.aivs97 = r_total_rema.aivs97 + r_aux_total.aivs97

      CASE
         -- ACEPTADOS
         WHEN r_aux_total.diagnostico = 0
            LET r_total_glo.total  = r_total_glo.total  + r_aux_total.total
            LET r_total_glo.aivs92 = r_total_glo.aivs92 + r_aux_total.aivs92
            LET r_total_glo.aivs97 = r_total_glo.aivs97 + r_aux_total.aivs97

            IF(r_aux_total.tpo_originacion = 1) THEN
               LET r_t_marca1.total   = r_t_marca1.total  + r_aux_total.total
               LET r_t_marca1.aivs92  = r_t_marca1.aivs92 + r_aux_total.aivs92
               LET r_t_marca1.aivs97  = r_t_marca1.aivs97 + r_aux_total.aivs97
            ELSE
               IF(r_aux_total.tpo_originacion = 4) THEN
                  LET r_t_marca4.total   = r_t_marca4.total  + r_aux_total.total
                  LET r_t_marca4.aivs92  = r_t_marca4.aivs92 + r_aux_total.aivs92
                  LET r_t_marca4.aivs97  = r_t_marca4.aivs97 + r_aux_total.aivs97
               END IF
            END IF

         -- RECHAZADOS
         WHEN r_aux_total.diagnostico <> 0
             LET r_total_glo_rch.total  = r_total_glo_rch.total  + r_aux_total.total
             LET r_total_glo_rch.aivs92 = r_total_glo_rch.aivs92 + r_aux_total.aivs92
             LET r_total_glo_rch.aivs97 = r_total_glo_rch.aivs97 + r_aux_total.aivs97

             IF(r_aux_total.tpo_originacion = 1) THEN
                LET r_t_marca1_rch.total   = r_t_marca1_rch.total  + r_aux_total.total
                LET r_t_marca1_rch.aivs92  = r_t_marca1_rch.aivs92 + r_aux_total.aivs92
                LET r_t_marca1_rch.aivs97  = r_t_marca1_rch.aivs97 + r_aux_total.aivs97
             ELSE
                IF(r_aux_total.tpo_originacion = 4) THEN
                   LET r_t_marca4_rch.total   = r_t_marca4_rch.total  + r_aux_total.total
                   LET r_t_marca4_rch.aivs92  = r_t_marca4_rch.aivs92 + r_aux_total.aivs92
                   LET r_t_marca4_rch.aivs97  = r_t_marca4_rch.aivs97 + r_aux_total.aivs97
                END IF
             END IF

      END CASE

   END FOREACH

   -- Calcula porcentajes.
   LET v_aux_porcentaje = 0

   -- Porcentaje global
   LET v_aux_porcentaje = (r_total_rema.total / r_total_rema.total) * 100
   LET r_total_rema.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje ACEPTADOS
   LET v_aux_porcentaje = (r_total_glo.total / r_total_glo.total) * 100
   LET r_total_glo.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje total marca 1
   LET v_aux_porcentaje = (r_t_marca1.total / r_total_glo.total) * 100
   LET r_t_marca1.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje total marca 4
   LET v_aux_porcentaje = (r_t_marca4.total / r_total_glo.total) * 100
   LET r_t_marca4.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje RECHAZADOS
   LET v_aux_porcentaje = (r_total_glo_rch.total / r_total_glo_rch.total) * 100
   LET r_total_glo_rch.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje total marca 1
   LET v_aux_porcentaje = (r_t_marca1_rch.total / r_total_glo_rch.total) * 100
   LET r_t_marca1_rch.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Porcentaje total marca 4
   LET v_aux_porcentaje = (r_t_marca4_rch.total / r_total_glo_rch.total) * 100
   LET r_t_marca4_rch.porcentaje = v_aux_porcentaje CLIPPED,"%"

   -- Obtiene datos del proceso
   SELECT fecha_ini,
          fecha_fin
     INTO v_f_ini_opera,
          v_f_fin_opera
     FROM bat_ctr_operacion
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod;

   DISPLAY " > Genera reporte PDF"

   # -----> CONFIGURACIÓN DEL REPORTE PDF <-----
   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP531.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",p_usuario CLIPPED,"-AGRP53-",
                       p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&","-",
                       p_opera_cod USING "&&&&&",".pdf"

   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         START REPORT imprime_pdf TO XML HANDLER object_rpt
         OUTPUT TO REPORT imprime_pdf()
         FINISH REPORT imprime_pdf
      ELSE
         DISPLAY " Error: No se pudo abrir la platilla del reporte PDF"
      END IF
   END IF

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

REPORT imprime_pdf()

   DEFINE v_fecha      DATE

   FORMAT
      FIRST PAGE HEADER
         LET v_fecha = TODAY
         
         #Encabezado
         PRINTX p_usuario
         PRINTX v_fecha USING "dd/mm/yyyy"
         PRINTX p_archivo
         PRINTX v_f_ini_opera
         PRINTX v_f_fin_opera

         #RESUMEN
         -- Total global
         PRINTX r_total_rema.total
         PRINTX r_total_rema.aivs92
         PRINTX r_total_rema.aivs97
         PRINTX r_total_rema.porcentaje
         -- Aceptados
         PRINTX r_total_glo.total 
         PRINTX r_total_glo.aivs92
         PRINTX r_total_glo.aivs97
         PRINTX r_total_glo.porcentaje
         PRINTX r_t_marca1.total  
         PRINTX r_t_marca1.aivs92 
         PRINTX r_t_marca1.aivs97
         PRINTX r_t_marca1.porcentaje
         PRINTX r_t_marca4.total  
         PRINTX r_t_marca4.aivs92 
         PRINTX r_t_marca4.aivs97
         PRINTX r_t_marca4.porcentaje

         -- Rechazados
         PRINTX r_total_glo_rch.total 
         PRINTX r_total_glo_rch.aivs92
         PRINTX r_total_glo_rch.aivs97
         PRINTX r_total_glo_rch.porcentaje
         PRINTX r_t_marca1_rch.total  
         PRINTX r_t_marca1_rch.aivs92 
         PRINTX r_t_marca1_rch.aivs97
         PRINTX r_t_marca1_rch.porcentaje
         PRINTX r_t_marca4_rch.total  
         PRINTX r_t_marca4_rch.aivs92 
         PRINTX r_t_marca4_rch.aivs97
         PRINTX r_t_marca4_rch.porcentaje

END REPORT

FUNCTION crea_temporal()

   DATABASE safre_tmp
   
   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_det_remanente
      DROP TABLE tmp_det_remanente_diag


   WHENEVER ERROR STOP 
      CREATE TABLE tmp_det_remanente(
                       nss             CHAR(11)     ,
                       aivs92          DECIMAL(16,6),
                       aivs97          DECIMAL(16,6),
                       tpo_credito     SMALLINT     ,
                       num_credito     DECIMAL(10,0),
                       f_originacion   DATE         ,
                       marca_ifv       SMALLINT     ,
                       tpo_originacion SMALLINT);

      CREATE TABLE tmp_det_remanente_diag(
                      nss                CHAR(11)     ,
                      id_cre_acreditado  DECIMAL(9,0) ,
                      id_derechohabiente DECIMAL(9,0) ,
                      aivs92             DECIMAL(16,6),
                      aivs97             DECIMAL(16,6),
                      tpo_originacion    SMALLINT     ,
                      estado             SMALLINT     ,
                      diagnostico        SMALLINT)

   DATABASE safre_viv
   
END FUNCTION 
