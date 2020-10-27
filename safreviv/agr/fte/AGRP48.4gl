######################################################################
#Modulo            => AGR                                            #
#Programa          => AGRP48                                         #
#Objetivo          => Lanzado para aplicación de saldos remanentes,  #
#                     el cual es llamado por los lanzadores AGRL66,  # 
#                     AGRL67 y AGRL68.                               #
#Autor             => Emilio Abarca, EFP                             #
#Fecha inicio      => 12/Enero/2018                                  #
#Autor Modifica    => Emilio Abarca, EFP.                            #
#Objetivo          => Automatiza recuperación de acreditados por     #
#                     ejecución por parámetros.                      #
#Fecha Modifica    => 21/Mayo/2019                                   #
######################################################################

DATABASE safre_viv

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

GLOBALS 
   DEFINE p_usuario           CHAR(20)
   DEFINE p_proceso_cod       SMALLINT  
   DEFINE p_opera_cod         SMALLINT
   DEFINE p_pid               DECIMAL(9,0)
   DEFINE p_tpo_ejecuta       SMALLINT
   DEFINE g_nss               CHAR(11)
   --El valor del nombre del archivo lo recibe del lanzador AGRL67 
   DEFINE p_archivo           STRING
   DEFINE v_ind_ejecuta       STRING
   DEFINE p_nom_archivo       STRING 
   DEFINE v_precio_fondo      DECIMAL(19,14)
   DEFINE r_b_valida          SMALLINT
   --variables archivos de salida
   DEFINE v_arh_rechazos      STRING
   DEFINE v_arh_aceptados     STRING 
   DEFINE v_ruta_envio        CHAR(40)
   DEFINE v_ruta_lst          CHAR(40)
   DEFINE v_ruta_bin          CHAR(40)
   --Variables globales para manejo de tablas tmp
   DEFINE v_tab_glo_rechazos  CHAR(30)
   DEFINE v_tab_glo_aceptados CHAR(30)
   --variables globales para el reporte
   DEFINE r_nss               CHAR(11)
   DEFINE r_bloque_archivo    STRING
   DEFINE v_reporte_bin       STRING
   DEFINE v_ruta_rpt          STRING 
   DEFINE object_rpt          om.SaxDocumentHandler
   DEFINE v_dia               DATE  
   --variables para formato de la hora,minuto,segundos
   DEFINE v_dt              DATETIME HOUR TO SECOND 
   DEFINE v_aux_hora        CHAR(8)
   DEFINE v_hora            CHAR(6) 
   
   DEFINE r_parametros_report RECORD
      monto_rema_ini           DECIMAL(13,2),
      monto_rema_fin           DECIMAL(13,2),
      f_orig_ini               DATE,
      f_orig_fin               DATE
   END RECORD
   
   DEFINE arr_tpo_credito DYNAMIC ARRAY OF RECORD
      tpo_credito    SMALLINT,
      tpo_desc       CHAR(30),
      conc_credito   CHAR(35)
   END RECORD 

   DEFINE r_arh_rch_rpt   RECORD
      nombre_archivo   STRING, 
      total_registros  INTEGER,
      aivs92           DECIMAL(16,2),
      aivs97           DECIMAL(16,2),
      pesos            DECIMAL(18,2)
   END RECORD 

   DEFINE r_arh_acep_rpt   RECORD
      nombre_archivo   STRING, 
      total_registros  INTEGER,
      aivs92           DECIMAL(16,2),
      aivs97           DECIMAL(16,2),
      pesos            DECIMAL(18,2)
   END RECORD 

   DEFINE r_detalle_rpt   RECORD
      tot_reg_acep      INTEGER,
      aivs92_acep       DECIMAL(16,2),
      aivs97_acep       DECIMAL(16,2),
      pesos_acep        DECIMAL(18,2),
      porc_acep         CHAR(6),
      tot_reg_rch       INTEGER,
      aivs92_rch        DECIMAL(16,2),
      aivs97_rch        DECIMAL(16,2),
      pesos_rch         DECIMAL(18,2),
      porc_rch          CHAR(6)
   END RECORD 

   --Arreglo rechazos
   DEFINE arr_hallazgos DYNAMIC ARRAY OF RECORD
      causal       SMALLINT,
      causal_desc  CHAR(40),
      aivs92       DECIMAL(16,2),
      aivs97       DECIMAL(16,2),
      pesos        DECIMAL(18,2),
      tot_reg      INTEGER 
   END RECORD

   DEFINE v_ind_valida   SMALLINT
   #Parametros de conexion
   DEFINE v_url_servidor LIKE wsv_cliente.ruta_servidor
   DEFINE v_usuario      LIKE wsv_cliente.usuario
   DEFINE v_password     LIKE wsv_cliente.password
   DEFINE v_intentos     LIKE wsv_cliente.num_reintento
   DEFINE soapStatus     INTEGER
   
END GLOBALS

MAIN

   --Recibe valores que envían los lanzadores (AGRL66,AGRL67,AGRL68)
   LET p_usuario        = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET p_tpo_ejecuta    = ARG_VAL(5)
   LET g_nss            = ARG_VAL(6)
   LET p_archivo        = ARG_VAL(7)
   LET p_nom_archivo    = ARG_VAL(8)

   --Obtiene el valor de precio fondo al día
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = TODAY
      AND fondo       = 11;

   SELECT ruta_bin,
          ruta_listados,
          ruta_envio
     INTO v_ruta_bin,
          v_ruta_lst,
          v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'agr';

   -- Log en caso de errores
   CALL STARTLOG(p_usuario CLIPPED|| ".AGRP48.log")

   LET v_dia = TODAY
   LET v_ind_ejecuta  = NULL
   LET v_ind_valida   = 0
   
   CASE
      WHEN p_tpo_ejecuta = 1
         LET v_ind_ejecuta = "SELECCIÓN POR NSS"
         CALL crea_tmp_remanentes(1)
         --Asigna que tabla se va a ocupar
         LET v_tab_glo_rechazos  = "tmp_rch_sel_nss" 
         LET v_tab_glo_aceptados = "tmp_sel_nss_aceptado"
      WHEN p_tpo_ejecuta = 2
         LET v_ind_ejecuta = "SELECCIÓN POR BLOQUES DE NSS"
         CALL crea_tmp_remanentes(2)
         --Asigna que tabla se va a ocupar
         LET v_tab_glo_rechazos  = "tmp_rch_sel_bloque"
         LET v_tab_glo_aceptados = "tmp_sel_bloque_aceptado"
      WHEN p_tpo_ejecuta = 3
         LET v_ind_ejecuta = "SELECCIÓN POR PARÁMETROS"
         CALL crea_tmp_remanentes(3)
         --Asigna que tabla se va a ocupar
         LET v_tab_glo_rechazos = "tmp_rch_sel_parametro"
         LET v_tab_glo_aceptados =  "tmp_sel_parametro_aceptado"
   END CASE 

   CALL fn_display_proceso(0,"APLICACIÓN SALDOS REMANENTES")
   DISPLAY ""
   DISPLAY " TIPO EJECUCIÓN: ",v_ind_ejecuta CLIPPED
   IF(p_archivo <> " ")  THEN
      DISPLAY " ARCHIVO: ",p_nom_archivo
   END IF 
   DISPLAY ""

   DISPLAY " > PROCESA INFORMACIÓN "

   -- Se invoca a la función que configura el WS obteniendo los datos de la tabla wsv_cliente
   CALL fn_configura_ws() RETURNING v_url_servidor,v_usuario,v_password

   CASE 
      WHEN p_tpo_ejecuta = 1
         -- Selección por NSS: Se ejecuta imediatamente la función que verifica los datos
         CALL f_valida_datos(g_nss) RETURNING v_ind_valida

      WHEN p_tpo_ejecuta = 2
         --Ejecuta función modo Selección por bloques de NSS
         CALL fn_procesa_seleccion_bloques(p_archivo)

      WHEN p_tpo_ejecuta = 3
         --Ejecuta función modo Selección por parámetros
         CALL fn_procesa_seleccion_parametros()
         
   END CASE 

   DISPLAY " > GENERA ARCHIVOS DE SALIDA "
   DISPLAY ""

   CALL genera_archivos()

   DISPLAY " Archivo rechazos"
   DISPLAY " ",v_arh_rechazos
   DISPLAY ""
   DISPLAY " Archivo aceptados"
   DISPLAY " ",v_arh_aceptados
   DISPLAY ""

   DISPLAY " > GENERA REPORTE PDF"
   CALL informacion_reporte(p_tpo_ejecuta)
   
   --finaliza la operación como correcta
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_b_valida

   CALL fn_display_proceso(1,"APLICACIÓN SALDOS REMANENTES")
    
END MAIN 

FUNCTION fn_procesa_seleccion_bloques(p_ruta_arh_rec)

   DEFINE p_ruta_arh_rec     STRING 
   DEFINE ch                 base.channel
   DEFINE v_linea            CHAR(11)
   DEFINE v_arh_nss          CHAR(11)
   DEFINE v_nss_bloque       CHAR(11)

   LET ch = base.Channel.create() # Creamos un objeto de la clase channel
   CALL ch.openFile(p_ruta_arh_rec,"r")

   LET v_arh_nss   = NULL

   DISPLAY " > CARGA DE INFORMACIÓN EN TEMPORAL"

   WHILE TRUE
      LET v_linea = ch.readLine()
      IF(ch.isEof()) THEN
         EXIT WHILE
         DISPLAY "no encuentra registros"
      ELSE 
         -- Recupera información por linea
         LET v_arh_nss = v_linea[1,11]

         INSERT INTO safre_tmp:tmp_sel_bloque
            VALUES (v_arh_nss);
      END IF
   END WHILE

   -- Cierra archivo
   CALL ch.close()

   DECLARE crs_sel_bloque CURSOR FOR
   SELECT nss
     FROM safre_tmp:tmp_sel_bloque;

   LET v_nss_bloque = NULL

   FOREACH crs_sel_bloque INTO v_nss_bloque
      -- Ejecuta función de validación de información
      CALL f_valida_datos(v_nss_bloque) RETURNING v_ind_valida
   END FOREACH

   
END FUNCTION 

FUNCTION fn_procesa_seleccion_parametros()

   DEFINE r_parametros  RECORD
      monto_rema_ini DECIMAL(13,2),
      monto_rema_fin DECIMAL(13,2),
      f_orig_ini     DATE,
      f_orig_fin     DATE
   END RECORD

   DEFINE r_remanente_aceptado RECORD
      nss                CHAR(11),
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      tpo_credito        SMALLINT,
      num_credito        DECIMAL(10,0),
      tpo_originacion    SMALLINT,
      f_otorga           DATE,
      marca_ifv          SMALLINT,
      marca_prcr         SMALLINT,
      sdo92              DECIMAL(16,2),
      sdo97              DECIMAL(16,2),
      suma_saldo         DECIMAL(18,2),
      aivs92             DECIMAL(16,2),
      aivs97             DECIMAL(18,2),
      suma_aivs          DECIMAL(18,2),
      estado             SMALLINT,
      nombre_af          LIKE afi_derechohabiente.nombre_af,
      ap_paterno_af      LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af      LIKE afi_derechohabiente.ap_materno_af
   END RECORD
   
   DEFINE v_sqry             STRING
   DEFINE v_error            SMALLINT;
   DEFINE v_isam_err         INTEGER;
   DEFINE v_c_msj            VARCHAR(250);
   DEFINE v_pesos_92_ws      DECIMAL(16,2)
   DEFINE v_pesos_97_ws      DECIMAL(16,2)
   DEFINE v_total_saldo_ws   DECIMAL(18,2)
   DEFINE v_causal_rch       SMALLINT
   DEFINE v_aux_orig_credito CHAR(2)
   DEFINE v_datos_entrada    tConsultaSaldoVO
     
   INITIALIZE r_parametros.* TO NULL 

   #Recupera valores de la temporal con valores que envía lanzador AGRL68
   SELECT *
     INTO r_parametros.*
     FROM safre_tmp:tmp_parametro_remanente;
   
   LET v_sqry = "EXECUTE FUNCTION fn_agr_obtiene_remanente(?,?,?,?)"

   PREPARE prp_obt_remanente FROM v_sqry
   EXECUTE prp_obt_remanente USING r_parametros.monto_rema_ini,
                                   r_parametros.monto_rema_fin,
                                   r_parametros.f_orig_ini,
                                   r_parametros.f_orig_fin
                              INTO v_error,
                                   v_isam_err,
                                   v_c_msj

   IF(v_error <> 0) THEN
      DISPLAY " Ocurrió un error durante la recuperación de remanentes:"
      DISPLAY " ERROR      : ",v_error
      DISPLAY " ISAM ERR   : ",v_isam_err
      DISPLAY " MENSAJE ERR: ",v_c_msj

      -- ocurrió un error y se marca como rechazado la operación
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_b_valida
      EXIT PROGRAM

   END IF

   DISPLAY " "
   DISPLAY " Inicia consulta ws Procesar ", CURRENT HOUR TO SECOND 
   DISPLAY " "

   INITIALIZE r_remanente_aceptado TO NULL
   
   LET  v_sqry = " SELECT tmp.nss,
                       \n tmp.id_cre_acreditado, 
                       \n tmp.id_derechohabiente,
                       \n tmp.tpo_credito,
                       \n tmp.num_credito,
                       \n tmp.tpo_originacion,
                       \n tmp.f_otorga,
                       \n tmp.marca_operativa,
                       \n tmp.marca_procesar,
                       \n tmp.sdo92,
                       \n tmp.sdo97,
                       \n tmp.suma_saldo,
                       \n tmp.aivs92,
                       \n tmp.aivs97,
                       \n tmp.suma_aivs,
                       \n tmp.estado,
                       \n afi.nombre_af,
                       \n afi.ap_paterno_af,
                       \n afi.ap_materno_af
                  \n FROM safre_tmp:tmp_sel_parametro_aceptado tmp,
                       \n afi_derechohabiente afi
                 \n WHERE tmp.id_derechohabiente = afi.id_derechohabiente"

   PREPARE prp_ws_procesar FROM v_sqry
   DECLARE cur_ws_procesar CURSOR FOR prp_ws_procesar

   FOREACH cur_ws_procesar INTO r_remanente_aceptado.nss               ,
                                r_remanente_aceptado.id_cre_acreditado ,
                                r_remanente_aceptado.id_derechohabiente,
                                r_remanente_aceptado.tpo_credito       ,
                                r_remanente_aceptado.num_credito       ,
                                r_remanente_aceptado.tpo_originacion   ,
                                r_remanente_aceptado.f_otorga          ,
                                r_remanente_aceptado.marca_ifv         ,
                                r_remanente_aceptado.marca_prcr        ,
                                r_remanente_aceptado.sdo92             ,
                                r_remanente_aceptado.sdo97             ,
                                r_remanente_aceptado.suma_saldo        ,
                                r_remanente_aceptado.aivs92            ,
                                r_remanente_aceptado.aivs97            ,
                                r_remanente_aceptado.suma_aivs         ,
                                r_remanente_aceptado.estado            ,
                                r_remanente_aceptado.nombre_af         ,
                                r_remanente_aceptado.ap_paterno_af     ,
                                r_remanente_aceptado.ap_materno_af

      LET v_pesos_92_ws      = 0
      LET v_pesos_97_ws      = 0
      LET v_total_saldo_ws   = 0
      LET v_causal_rch       = 0
      LET v_aux_orig_credito = NULL

      LET v_datos_entrada.apePaterno = r_remanente_aceptado.ap_paterno_af CLIPPED
      LET v_datos_entrada.apeMaterno = r_remanente_aceptado.ap_materno_af CLIPPED
      LET v_datos_entrada.nombres    = r_remanente_aceptado.nombre_af CLIPPED
      LET v_datos_entrada.nss        = r_remanente_aceptado.nss CLIPPED

      -- Se invoca función que ejecuta el web service
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

      IF(r_remanente_aceptado.tpo_originacion = 1)THEN
         LET v_aux_orig_credito = "01"
      ELSE
         IF(r_remanente_aceptado.tpo_originacion = 4) THEN
            LET v_aux_orig_credito = "04"
         END IF
      END IF
      
      IF (soapStatus = 0) THEN
         -- Obtiene saldo Procesar
         LET v_pesos_92_ws    = ConsultaSaldoRespVO.numAIVS92 * v_precio_fondo
         LET v_pesos_97_ws    = ConsultaSaldoRespVO.numAIVS97 * v_precio_fondo
         LET v_total_saldo_ws = v_pesos_92_ws + v_pesos_97_ws

         -- Verifica que esté marcado y tenga saldo en Procesar
         IF(v_aux_orig_credito <> ConsultaSaldoRespVO.origenTipoCredito) THEN
            LET v_causal_rch = 30   --> Registro no marcado en Procesar
         ELSE
            IF( v_total_saldo_ws <= 0) THEN
               LET v_causal_rch = 35   --> Saldo cero en Procesar 
            END IF
         END IF 

      END IF

      IF(v_causal_rch <> 0) THEN
         -- Registro Rechazado
         INSERT INTO safre_tmp:tmp_rch_sel_parametro(
                               nss            ,
                               sdo92          ,
                               sdo97          ,
                               suma_saldo     ,
                               aivs92         ,
                               aivs97         ,
                               suma_aivs      ,
                               tpo_credito    ,
                               num_credito    ,
                               f_otorga       ,
                               marca_operativa,
                               marca_procesar ,
                               rch_cod_marca  ,
                               tpo_originacion,
                               causal_rch)
                        VALUES(r_remanente_aceptado.nss        ,
                               r_remanente_aceptado.sdo92      ,
                               r_remanente_aceptado.sdo97      ,
                               r_remanente_aceptado.suma_saldo ,
                               r_remanente_aceptado.aivs92     ,
                               r_remanente_aceptado.aivs97     ,
                               r_remanente_aceptado.suma_aivs  ,
                               r_remanente_aceptado.tpo_credito,
                               r_remanente_aceptado.num_credito,
                               r_remanente_aceptado.f_otorga   ,
                               r_remanente_aceptado.marca_ifv  ,
                               r_remanente_aceptado.marca_prcr ,
                               0,
                               r_remanente_aceptado.tpo_originacion,
                               v_causal_rch);   --> Saldo cero en Procesar
      END IF

   END FOREACH

   FREE cur_ws_procesar

   -- Elimina registros que fueron rechazados en la consulta de saldo Procesar
   DELETE FROM safre_tmp:tmp_sel_parametro_aceptado
      WHERE nss IN (SELECT nss 
                      FROM safre_tmp:tmp_rch_sel_parametro
                     WHERE causal_rch IN (30,35));

   DISPLAY " Fin consulta ws Procesar ", CURRENT HOUR TO SECOND 
   DISPLAY " "

END FUNCTION

FUNCTION f_valida_datos(p_val_nss)

   DEFINE p_val_nss     CHAR(11)
   DEFINE bnd_diag      SMALLINT
   DEFINE v_id_dh       DECIMAL(9,0)
   DEFINE v_t_rema      INTEGER

   -- Record acreditado
   DEFINE r_acreditado   RECORD
      id_cre_acreditado  DECIMAL(9,0),
      id_derechohabiente DECIMAL(9,0),
      ap_paterno_af      LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_af      LIKE afi_derechohabiente.ap_materno_af,
      nombre_af          LIKE afi_derechohabiente.nombre_af,
      tpo_originacion    SMALLINT,
      tpo_credito        SMALLINT,
      num_credito        DECIMAL(10,0),
      sdo_deudor         DECIMAL(12,2),
      f_otorga           DATE,
      estado             SMALLINT,
      edo_procesar       SMALLINT,
      entidad            SMALLINT 
   END RECORD

   DEFINE v_marca_ifv      SMALLINT
   DEFINE v_marca_prc      SMALLINT
   DEFINE v_pesos_97       DECIMAL(16,2)
   DEFINE v_aivs_97        DECIMAL(16,2)
   DEFINE v_pesos_92       DECIMAL(16,2)
   DEFINE v_aivs_92        DECIMAL(16,2)
   DEFINE v_total_saldo    DECIMAL(18,2)
   DEFINE v_total_aivs     DECIMAL(18,2)
   DEFINE v_marca_activa   SMALLINT
   DEFINE v_ind_conv       SMALLINT
   DEFINE v_rch_cod        SMALLINT
   DEFINE v_inserta_rch    STRING
   DEFINE v_inserta_acep   STRING
   DEFINE v_pesos_92_ws    DECIMAL(16,2)
   DEFINE v_pesos_97_ws    DECIMAL(16,2)
   DEFINE v_total_saldo_ws DECIMAL(18,2)
   DEFINE v_aux_origen     CHAR(2)

   -- Inicializa variables
   LET bnd_diag        = 10  -- Inicia bandera procedente
   LET v_t_rema        = 0
   LET v_marca_ifv     = 0
   LET v_marca_prc     = 0
   LET v_pesos_97      = 0
   LET v_aivs_97       = 0
   LET v_pesos_92      = 0
   LET v_aivs_92       = 0
   LET v_total_saldo   = 0
   LET v_total_aivs    = 0
   LET v_marca_activa  = NULL
   LET v_ind_conv      = 0
   LET v_rch_cod       = 0
   LET v_aux_origen    = NULL

   INITIALIZE r_acreditado.* TO NULL

   LET v_inserta_rch  = "INSERT INTO safre_tmp:",v_tab_glo_rechazos CLIPPED,"\n",
                         "  VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_inserta_rechazo FROM v_inserta_rch

   LET v_inserta_acep = "INSERT INTO safre_tmp:",v_tab_glo_aceptados CLIPPED,"\n",
                         "  VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
   PREPARE prp_inserta_aceptado FROM v_inserta_acep

   # --> VALIDA DH
   SELECT id_derechohabiente
     INTO v_id_dh
     FROM afi_derechohabiente
    WHERE nss = p_val_nss

   IF(v_id_dh IS NULL) THEN
      LET bnd_diag = 11  -- Trabajador no existe
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag    -- Ya no continúa con las otras validaciones
   END IF

   # --> VALIDA TIPO DE CRÉDITO
   SELECT COUNT(*)
     INTO v_t_rema
     FROM cre_acreditado a,
          cat_tipo_cred_rem c
    WHERE a.id_derechohabiente = v_id_dh
      AND a.tpo_credito        = c.tpo_credito;

   IF(v_t_rema = 0) THEN
      LET bnd_diag = 16   -- Tipo de crédito no válido
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA ACREDITADO VIGENTE
   SELECT FIRST 1
          a.id_cre_acreditado ,
          a.id_derechohabiente,
          f.ap_paterno_af     ,
          f.ap_materno_af     ,
          f.nombre_af         ,
          a.tpo_originacion   ,
          a.tpo_credito       ,
          a.num_credito       ,
          a.sdo_deudor        ,
          a.f_otorga          ,
          a.estado            ,
          a.edo_procesar      ,
          m.entidad
     INTO r_acreditado.id_cre_acreditado ,
          r_acreditado.id_derechohabiente,
          r_acreditado.ap_paterno_af     ,
          r_acreditado.ap_materno_af     ,
          r_acreditado.nombre_af         ,
          r_acreditado.tpo_originacion   ,
          r_acreditado.tpo_credito       ,
          r_acreditado.num_credito       ,
          r_acreditado.sdo_deudor        ,
          r_acreditado.f_otorga          ,
          r_acreditado.estado            ,
          r_acreditado.edo_procesar      ,
          r_acreditado.entidad
    FROM cre_acreditado a  ,
         afi_derechohabiente f,
         cat_maq_credito m,
         cat_tipo_cred_rem c
   WHERE a.id_derechohabiente = v_id_dh
     AND a.id_derechohabiente = f.id_derechohabiente
     AND a.estado = m.estado
     AND m.entidad IN (1,2)
     AND a.tpo_credito = c.tpo_credito
   ORDER BY m.entidad;

   IF(r_acreditado.entidad = 2) THEN
      LET bnd_diag = 13  -- No existe marca crédito vigente
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA DEUDOR LIQUIDADO
   IF(r_acreditado.estado <> 140) AND 
     (r_acreditado.estado <> 145) AND
     (r_acreditado.estado <> 220) AND
     (r_acreditado.estado <> 900) THEN
      LET bnd_diag = 31  -- Deudor vigente
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA SALDO TRANSFERIDO PROCESAR
   IF(r_acreditado.edo_procesar <> 120) THEN
      LET bnd_diag = 15  -- Estado del crédito no válido
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA MARCA INFONAVIT
   -- Por el tipo de crédito es la marca interna infornavit
   CASE
      WHEN r_acreditado.tpo_credito = 1
         LET v_marca_ifv = 201
      WHEN r_acreditado.tpo_credito = 3
         LET v_marca_ifv = 203
      WHEN r_acreditado.tpo_credito = 4
         LET v_marca_ifv = 204
      WHEN r_acreditado.tpo_credito = 5
         LET v_marca_ifv = 205
      WHEN r_acreditado.tpo_credito = 10
         LET v_marca_ifv = 210
      WHEN r_acreditado.tpo_credito = 11
         LET v_marca_ifv = 211
      WHEN r_acreditado.tpo_credito = 15
         LET v_marca_ifv = 215
   END CASE

   -- Verifica que tenga esa marca activa
   SELECT MAX(marca)
     INTO v_marca_ifv
     FROM sfr_marca_activa
    WHERE id_derechohabiente = r_acreditado.id_derechohabiente
      AND marca = v_marca_ifv;

   IF(v_marca_ifv IS NULL) THEN
      LET bnd_diag    = 13  -- No existe marca de crédito vigente
      LET v_marca_ifv = 0
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA MARCA INTERNA PROCESAR
   -- Dependiendo del tipo de originación es la marca interna Procesar
   CASE
      WHEN r_acreditado.tpo_originacion = 1
         LET v_marca_prc = 231
         LET v_aux_origen = "01"   -- Se usa para verificar la marca en Procesar
      WHEN r_acreditado.tpo_originacion = 4
         LET v_marca_prc = 234
         LET v_aux_origen = "04"   -- Se usa para verificar la marca en Procesar
   END CASE

   SELECT MAX(marca)
     INTO v_marca_prc
     FROM sfr_marca_activa
    WHERE id_derechohabiente = r_acreditado.id_derechohabiente
      AND marca = v_marca_prc;

   IF(v_marca_prc IS NULL) THEN
      LET bnd_diag    = 30  -- Registro no marcado en Procesar
      LET v_marca_prc = 0
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # --> VALIDA SSV 92 Y 97
   -- Subcuenta 97
   SELECT ROUND(SUM(monto_acciones) * v_precio_fondo,2),
          SUM(monto_acciones)
     INTO v_pesos_97,
          v_aivs_97
     FROM cta_movimiento
    WHERE id_derechohabiente = r_acreditado.id_derechohabiente
      AND subcuenta = 4
      AND fondo_inversion = 11;

   IF (v_pesos_97 IS NULL) THEN
      LET v_pesos_97 = 0
   END IF 

   IF (v_aivs_97 IS NULL) THEN
      LET v_aivs_97 = 0
   END IF

   -- Subcuenta 92
   SELECT ROUND(SUM(monto_acciones) * v_precio_fondo,2),
          SUM(monto_acciones)
     INTO v_pesos_92,
          v_aivs_92
     FROM cta_movimiento
    WHERE id_derechohabiente = r_acreditado.id_derechohabiente
      AND subcuenta = 8
      AND fondo_inversion = 11;

   IF (v_pesos_92 IS NULL) THEN
      LET v_pesos_92 = 0
   END IF 

   IF (v_aivs_92 IS NULL) THEN
      LET v_aivs_92 = 0
   END IF

   -- Suma totales
   LET v_total_saldo = v_pesos_97 + v_pesos_92
   LET v_total_aivs  = v_aivs_97  + v_aivs_92

   IF(v_total_saldo = 0) THEN
      LET bnd_diag = 8   -- Cuenta con saldo cero
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   ELSE
      IF(v_total_saldo < 0) THEN
         LET bnd_diag = 34   -- Cuenta con saldo negativo
         EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
         RETURN bnd_diag
      END IF
   END IF

   # --> VALIDA SI EXISTE UNA MARCA QUE IMPIDA LA PETICIÓN
   -- Busca las marcas activas que No sea la marca interna infonavit y la marca interna Procesar
   DECLARE crs_marca_operativa CURSOR FOR
    SELECT marca
      FROM sfr_marca_activa
     WHERE id_derechohabiente = r_acreditado.id_derechohabiente
       AND marca NOT IN (v_marca_ifv,v_marca_prc);

   FOREACH crs_marca_operativa INTO v_marca_activa
      -- Verifica convivencia
      SELECT ind_convivencia,rch_cod
        INTO v_ind_conv,
             v_rch_cod
        FROM sfr_convivencia
       WHERE marca_activa = v_marca_activa
         AND marca_entra  = v_marca_ifv;

      -- Con uno que no conviva se rechaza
      IF(v_ind_conv = 20) THEN
         EXIT FOREACH
      END IF
   END FOREACH

   IF(v_ind_conv = 20) THEN
      LET bnd_diag = 32   -- Marca operativa en proceso
      EXECUTE prp_inserta_rechazo USING p_val_nss,
                                        v_pesos_92,
                                        v_pesos_97,
                                        v_total_saldo,
                                        v_aivs_92,
                                        v_aivs_97,
                                        v_total_aivs,
                                        r_acreditado.tpo_credito,
                                        r_acreditado.num_credito,
                                        r_acreditado.f_otorga,
                                        v_marca_ifv,
                                        v_marca_prc,
                                        v_rch_cod,
                                        r_acreditado.tpo_originacion,
                                        bnd_diag
      RETURN bnd_diag
   END IF

   # Valida que tenga saldo y esté marcado en Procesar.
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
                      p_val_nss)
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

   --DISPLAY "SOAPSTATUS: ",soapStatus

   -- Si no hay ningún error, se despliegan los datos obtenidos del WS en la forma
   IF (soapStatus = 0) THEN
      -- calcula pesos
      LET v_pesos_92_ws    = ConsultaSaldoRespVO.numAIVS92 * v_precio_fondo
      LET v_pesos_97_ws    = ConsultaSaldoRespVO.numAIVS97 * v_precio_fondo
      LET v_total_saldo_ws = v_pesos_92_ws + v_pesos_97_ws

      -- Verifica marca y saldo en Procesar
      IF(v_aux_origen <> ConsultaSaldoRespVO.origenTipoCredito) THEN
         LET bnd_diag = 30   -- Registro no marcado en Procesar
      ELSE
         IF( v_total_saldo_ws <= 0) THEN
            LET bnd_diag = 35  -- Saldo cero en Procesar
         END IF
      END IF
      
      IF( bnd_diag <> 10) THEN
         EXECUTE prp_inserta_rechazo USING p_val_nss,
                                           v_pesos_92,
                                           v_pesos_97,
                                           v_total_saldo,
                                           v_aivs_92,
                                           v_aivs_97,
                                           v_total_aivs,
                                           r_acreditado.tpo_credito,
                                           r_acreditado.num_credito,
                                           r_acreditado.f_otorga,
                                           v_marca_ifv,
                                           v_marca_prc,
                                           v_rch_cod,
                                           r_acreditado.tpo_originacion,
                                           bnd_diag
         RETURN bnd_diag
      END IF
   END IF 

   -- Si fué procedente realiza la petición.
   IF(bnd_diag = 10) THEN
      EXECUTE prp_inserta_aceptado USING p_val_nss,
                                         r_acreditado.id_cre_acreditado,
                                         r_acreditado.id_derechohabiente,
                                         r_acreditado.tpo_credito,
                                         r_acreditado.num_credito,
                                         r_acreditado.tpo_originacion,
                                         r_acreditado.f_otorga,
                                         v_marca_ifv,
                                         v_marca_prc,
                                         v_pesos_92,
                                         v_pesos_97,
                                         v_total_saldo,
                                         v_aivs_92,
                                         v_aivs_97,
                                         v_total_aivs,
                                         r_acreditado.estado

      RETURN bnd_diag

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


FUNCTION genera_archivos()

   DEFINE v_qry_rechazos    STRING
   DEFINE v_qry_aceptados   STRING
   DEFINE arh_rch           base.channel
   DEFINE arh_acept         base.channel
   DEFINE v_detalle         STRING
   DEFINE v_aux_estado_desc CHAR(40)
   DEFINE v_aux_cod_causal  CHAR(3)
  
   --Record para archivo rechazos
   DEFINE r_rch_remanente  RECORD 
      nss             CHAR(11),
      aivs92          CHAR(15),
      aivs97          CHAR(15),
      tpo_credito     CHAR(3),
      num_credito     CHAR(10),
      f_originacion   DATE,
      marca_operativa CHAR(3),
      tpo_originacion CHAR(1),
      causal          SMALLINT,
      causal_desc     CHAR(60),
      cod_rch_marca   CHAR(3)
   END RECORD
   --record archivo para aceptados
   DEFINE r_acep_remanente  RECORD 
      nss             CHAR(11),
      aivs92          CHAR(15),
      aivs97          CHAR(15),
      tpo_credito     CHAR(3),
      num_credito     CHAR(10),
      f_originacion   DATE,
      marca_operativa CHAR(3),
      tpo_originacion CHAR(1)
   END RECORD
   DEFINE v_nom_arh_acep STRING

   -- Formato a la hora 
   LET v_dt = CURRENT 
   LET v_aux_hora = v_dt
   LET v_hora = v_aux_hora[1,2],v_aux_hora[4,5],v_aux_hora[7,8]
   
   #ARCHIVO PARA RECHAZOS
   LET v_detalle       = NULL
   LET v_arh_rechazos  = v_ruta_envio CLIPPED,"/SR",TODAY USING "yyyymmdd","_",v_hora CLIPPED,".srha"

   LET v_qry_rechazos = "SELECT nss,
                             \n aivs92,
                             \n aivs97,
                             \n tpo_credito,
                             \n num_credito,
                             \n f_otorga,
                             \n marca_operativa,
                             \n rch_cod_marca,
                             \n tpo_originacion,
                             \n causal_rch
                        \n FROM safre_tmp:",v_tab_glo_rechazos

   PREPARE prp_rch_remanente FROM v_qry_rechazos
   DECLARE crs_rch_remanente CURSOR FOR prp_rch_remanente

   LET arh_rch = base.Channel.create()
   CALL arh_rch.openFile(v_arh_rechazos,"w")

   LET v_aux_estado_desc = NULL 
   LET v_aux_cod_causal  = NULL
   
   INITIALIZE r_rch_remanente.* TO NULL 
   
   FOREACH crs_rch_remanente INTO r_rch_remanente.nss,
                                   r_rch_remanente.aivs92,
                                   r_rch_remanente.aivs97,
                                   r_rch_remanente.tpo_credito,
                                   r_rch_remanente.num_credito,
                                   r_rch_remanente.f_originacion,
                                   r_rch_remanente.marca_operativa,
                                   r_rch_remanente.cod_rch_marca,
                                   r_rch_remanente.tpo_originacion,
                                   r_rch_remanente.causal

      -- Búsca descripción del rechazo
      SELECT desc_estado
        INTO v_aux_estado_desc
        FROM cat_rch_acreditado
       WHERE estado = r_rch_remanente.causal

      LET v_aux_cod_causal = r_rch_remanente.causal USING "&&&"

      --concatena
      LET r_rch_remanente.causal_desc = v_aux_cod_causal CLIPPED,"-",v_aux_estado_desc

      IF(r_rch_remanente.tpo_credito IS NULL) THEN
         LET r_rch_remanente.tpo_credito = 0
      END IF

      IF(r_rch_remanente.num_credito IS NULL) THEN
         LET r_rch_remanente.num_credito = 0
      END IF

      IF(r_rch_remanente.tpo_originacion IS NULL) THEN
         LET r_rch_remanente.tpo_originacion = 0
      END IF

      LET v_detalle = r_rch_remanente.nss,
                      r_rch_remanente.aivs92          USING "&&&&&&&&&&&&.&&",
                      r_rch_remanente.aivs97          USING "&&&&&&&&&&&&.&&",
                      r_rch_remanente.tpo_credito     USING "&&&",
                      r_rch_remanente.num_credito     USING "&&&&&&&&&&",
                      r_rch_remanente.f_originacion   USING "yyyymmdd",
                      r_rch_remanente.marca_operativa USING "&&&",
                      r_rch_remanente.tpo_originacion,
                      r_rch_remanente.causal_desc,
                      r_rch_remanente.cod_rch_marca   USING "&&&"

      --escribe en archivo
      CALL arh_rch.writeLine(v_detalle)
      
   END FOREACH

   --cierra archivo
   CALL arh_rch.close() 

   #ARCHIVO PARA REGISTROS ACEPTADOS
   LET v_detalle       = NULL
   LET v_nom_arh_acep  = "SR",TODAY USING "yyyymmdd","_",v_hora CLIPPED,".srac"
   LET v_arh_aceptados = v_ruta_envio CLIPPED,"/",v_nom_arh_acep

   LET v_qry_aceptados = "SELECT nss,
                              \n aivs92,
                              \n aivs97,
                              \n tpo_credito,
                              \n num_credito,
                              \n f_otorga,
                              \n marca_operativa,
                              \n tpo_originacion
                       \n FROM safre_tmp:",v_tab_glo_aceptados
   
    --escribe encabezado en el archivo
   LET arh_acept = base.Channel.create()
   CALL arh_acept.openFile(v_arh_aceptados,"w")
  
   PREPARE prp_arh_aceptados FROM v_qry_aceptados
   DECLARE crs_arh_aceptados CURSOR FOR prp_arh_aceptados

   INITIALIZE r_acep_remanente.* TO NULL

   FOREACH crs_arh_aceptados INTO r_acep_remanente.nss,
                                   r_acep_remanente.aivs92,
                                   r_acep_remanente.aivs97,
                                   r_acep_remanente.tpo_credito,
                                   r_acep_remanente.num_credito,
                                   r_acep_remanente.f_originacion,
                                   r_acep_remanente.marca_operativa,
                                   r_acep_remanente.tpo_originacion

      LET v_detalle =  r_acep_remanente.nss,
                       r_acep_remanente.aivs92          USING "&&&&&&&&&&&&.&&",
                       r_acep_remanente.aivs97          USING "&&&&&&&&&&&&.&&",
                       r_acep_remanente.tpo_credito     USING "&&&",
                       r_acep_remanente.num_credito     USING "&&&&&&&&&&",
                       r_acep_remanente.f_originacion   USING "yyyymmdd",
                       r_acep_remanente.marca_operativa USING "&&&",
                       r_acep_remanente.tpo_originacion USING "&"
                    
      --escribe en archivo
      CALL arh_acept.writeLine(v_detalle)

   END FOREACH 
   
   --cierra el archivo
   CALL arh_acept.close()

END FUNCTION 

FUNCTION informacion_reporte(p_ind_reporte)

   DEFINE p_ind_reporte     SMALLINT
   DEFINE i                 INTEGER 
   DEFINE v_qry_report      STRING
   DEFINE v_aux_tot_glo     INTEGER 
   DEFINE v_aux_porcentaje  INTEGER

   --Inicializa parametros
   LET r_nss = NULL 
   LET r_bloque_archivo = NULL
   
   INITIALIZE r_parametros_report.* TO NULL
   CALL arr_tpo_credito.clear()

   --evalúa que parametros se recibieron
   CASE 
      WHEN p_ind_reporte = 1
         LET r_nss = g_nss

      WHEN p_ind_reporte = 2
         LET r_bloque_archivo = p_nom_archivo

      WHEN p_ind_reporte = 3
         SELECT monto_rema_ini,
                monto_rema_fin,
                f_orig_ini,
                f_orig_fin
          INTO r_parametros_report.monto_rema_ini,
               r_parametros_report.monto_rema_fin,
               r_parametros_report.f_orig_ini,
               r_parametros_report.f_orig_fin
         FROM safre_tmp:tmp_parametro_remanente;

         DECLARE crs_creditos CURSOR FOR 
            SELECT tpo_credito
              FROM safre_tmp:tmp_parametro_credito
              ORDER BY 1;
              
         LET i = 1

         FOREACH crs_creditos INTO arr_tpo_credito[i].tpo_credito

            --busca desc del crédito
            SELECT MAX (desc_credito)
              INTO arr_tpo_credito[i].tpo_desc
              FROM cat_tipo_credito
             WHERE tpo_credito = arr_tpo_credito[i].tpo_credito

             LET arr_tpo_credito[i].conc_credito = arr_tpo_credito[i].tpo_credito,"-",arr_tpo_credito[i].tpo_desc
             
            LET i = i + 1
            
         END FOREACH 
         
   END CASE 
   
   INITIALIZE r_arh_rch_rpt.* TO NULL
   INITIALIZE r_arh_acep_rpt.* TO NULL
   
   --Obtiene valores para el archivo de rechazos
   LET r_arh_rch_rpt.nombre_archivo = "SR",TODAY USING "yyyymmdd","_",v_hora CLIPPED,".srha"

   LET v_qry_report = "SELECT COUNT(*)
                         FROM safre_tmp:",v_tab_glo_rechazos

   PREPARE prp_conteo_rch FROM v_qry_report
   EXECUTE prp_conteo_rch INTO r_arh_rch_rpt.total_registros

   LET v_qry_report = "SELECT SUM(aivs92),
                           \n SUM(aivs97),
                           \n SUM(suma_saldo)
                         FROM safre_tmp:",v_tab_glo_rechazos

   PREPARE prp_saldo_rch FROM v_qry_report
   EXECUTE prp_saldo_rch INTO r_arh_rch_rpt.aivs92,
                               r_arh_rch_rpt.aivs97,
                               r_arh_rch_rpt.pesos

   IF (r_arh_rch_rpt.aivs92 IS NULL) THEN 
      LET r_arh_rch_rpt.aivs92 = 0
   END IF

   IF (r_arh_rch_rpt.aivs97 IS NULL) THEN 
      LET r_arh_rch_rpt.aivs97 = 0
   END IF 

   IF (r_arh_rch_rpt.pesos IS NULL) THEN 
      LET r_arh_rch_rpt.pesos = 0
   END IF 
     
   LET r_arh_acep_rpt.nombre_archivo = "SR",TODAY USING "yyyymmdd","_",v_hora CLIPPED,".srac"

   LET v_qry_report = "SELECT COUNT(*)
                         FROM safre_tmp:",v_tab_glo_aceptados

   PREPARE prp_conteo_acep FROM v_qry_report
   EXECUTE prp_conteo_acep INTO r_arh_acep_rpt.total_registros

   LET v_qry_report = "SELECT SUM(aivs92),
                           \n SUM(aivs97),
                           \n SUM(suma_saldo)
                      \n FROM safre_tmp:",v_tab_glo_aceptados

   PREPARE prp_saldo_acep FROM v_qry_report
   EXECUTE prp_saldo_acep INTO r_arh_acep_rpt.aivs92,
                                r_arh_acep_rpt.aivs97,
                                r_arh_acep_rpt.pesos

   IF (r_arh_acep_rpt.aivs92 IS NULL) THEN 
      LET r_arh_acep_rpt.aivs92 = 0
   END IF

   IF (r_arh_acep_rpt.aivs97 IS NULL) THEN 
      LET r_arh_acep_rpt.aivs97 = 0
   END IF 

   IF (r_arh_acep_rpt.pesos IS NULL) THEN 
      LET r_arh_acep_rpt.pesos = 0
   END IF
  
   INITIALIZE r_detalle_rpt.* TO NULL

   LET r_detalle_rpt.tot_reg_acep = r_arh_acep_rpt.total_registros
   LET r_detalle_rpt.aivs92_acep  = r_arh_acep_rpt.aivs92
   LET r_detalle_rpt.aivs97_acep  = r_arh_acep_rpt.aivs97
   LET r_detalle_rpt.pesos_acep   = r_arh_acep_rpt.pesos
   LET r_detalle_rpt.tot_reg_rch  = r_arh_rch_rpt.total_registros
   LET r_detalle_rpt.aivs92_rch   = r_arh_rch_rpt.aivs92
   LET r_detalle_rpt.aivs97_rch   = r_arh_rch_rpt.aivs97
   LET r_detalle_rpt.pesos_rch    = r_arh_rch_rpt.pesos
   
   --calcula porcentajes
   LET v_aux_tot_glo           = r_arh_acep_rpt.total_registros + r_arh_rch_rpt.total_registros
   LET v_aux_porcentaje        = (r_arh_acep_rpt.total_registros / v_aux_tot_glo) * 100
   LET r_detalle_rpt.porc_acep = v_aux_porcentaje CLIPPED,"%"
   LET v_aux_porcentaje        = (r_arh_rch_rpt.total_registros / v_aux_tot_glo) * 100
   LET r_detalle_rpt.porc_rch  = v_aux_porcentaje CLIPPED,"%"


  CALL arr_hallazgos.clear()

  LET v_qry_report = "SELECT  causal_rch,
                           \n SUM(aivs92),
                           \n SUM(aivs97),
                           \n SUM(suma_saldo),
                           \n COUNT(*)
                       \n FROM safre_tmp:",v_tab_glo_rechazos,
                      "\n GROUP by 1" 
                        

   PREPARE prp_hallazgos FROM v_qry_report
   DECLARE crs_hallazgos CURSOR FOR prp_hallazgos

   LET i = 1

   FOREACH crs_hallazgos INTO arr_hallazgos[i].causal,
                               arr_hallazgos[i].aivs92,
                               arr_hallazgos[i].aivs97,
                               arr_hallazgos[i].pesos,
                               arr_hallazgos[i].tot_reg

      --obtiene desc del rechazo
      SELECT desc_estado
        INTO arr_hallazgos[i].causal_desc
        FROM cat_rch_acreditado
       WHERE estado = arr_hallazgos[i].causal;
       
      LET i = i + 1
      
   END FOREACH 

   #Al término de cálcular la información configura salida del reporte
   LET v_reporte_bin = v_ruta_bin CLIPPED,"/AGRP48.4rp"
   LET v_ruta_rpt    = v_ruta_lst CLIPPED,"/",p_usuario CLIPPED,"-AGRP48-",p_pid USING "&&&&&","-",p_proceso_cod USING "&&&&&","-",p_opera_cod USING "&&&&&",".pdf"
 
   IF (fgl_report_loadCurrentSettings(v_reporte_bin)) THEN 
      CALL fgl_report_selectDevice ("PDF")
      CALL fgl_report_selectPreview(0)
      CALL fgl_report_setOutputFileName(v_ruta_rpt)
      LET object_rpt = fgl_report_commitCurrentSettings()

      IF (object_rpt IS NOT NULL) THEN
         
         START REPORT genera_PDF TO XML HANDLER object_rpt

            OUTPUT TO REPORT genera_PDF()

         FINISH REPORT genera_PDF 

      END IF
   ELSE
      DISPLAY "ERROR: No fué posible abrir la plantilla del reporte"
   END IF

END FUNCTION 


REPORT genera_PDF()

   DEFINE k       INTEGER
   
   FORMAT 

      FIRST PAGE HEADER 
         #INF. USUARIO
         PRINTX p_usuario
         PRINTX v_dia USING "dd/mm/yyyy"
         #PARÁMETROS SELECCIONADOS
         PRINTX r_nss  
         PRINTX r_bloque_archivo 
         PRINTX r_parametros_report.monto_rema_ini
         PRINTX r_parametros_report.monto_rema_fin
         PRINTX r_parametros_report.f_orig_ini USING "dd/mm/yyyy"
         PRINTX r_parametros_report.f_orig_fin USING "dd/mm/yyyy"
         PRINTX r_arh_rch_rpt.nombre_archivo 
         PRINTX r_arh_rch_rpt.total_registros
         PRINTX r_arh_rch_rpt.aivs92
         PRINTX r_arh_rch_rpt.aivs97
         PRINTX r_arh_rch_rpt.pesos
         PRINTX r_arh_acep_rpt.nombre_archivo
         PRINTX r_arh_acep_rpt.total_registros
         PRINTX r_arh_acep_rpt.aivs92
         PRINTX r_arh_acep_rpt.aivs97
         PRINTX r_arh_acep_rpt.pesos
         PRINTX r_detalle_rpt.tot_reg_acep 
         PRINTX r_detalle_rpt.aivs92_acep  
         PRINTX r_detalle_rpt.aivs97_acep  
         PRINTX r_detalle_rpt.pesos_acep 
         PRINTX r_detalle_rpt.porc_acep  
         PRINTX r_detalle_rpt.tot_reg_rch  
         PRINTX r_detalle_rpt.aivs92_rch   
         PRINTX r_detalle_rpt.aivs97_rch   
         PRINTX r_detalle_rpt.pesos_rch
         PRINTX r_detalle_rpt.porc_rch

      ON EVERY ROW 
         --tpo credito seleccionado
         FOR k = 1 TO arr_tpo_credito.getLength()
            PRINTX arr_tpo_credito[k].conc_credito
         END FOR 
      
         --detalle hallazgos
         FOR k = 1 TO arr_hallazgos.getLength()
            PRINTX arr_hallazgos[k].tot_reg
            PRINTX arr_hallazgos[k].aivs92
            PRINTX arr_hallazgos[k].aivs97
            PRINTX arr_hallazgos[k].pesos
            PRINTX arr_hallazgos[k].causal
            PRINTX arr_hallazgos[k].causal_desc
         END FOR 

END REPORT

FUNCTION crea_tmp_remanentes(p_crea_tmp)

   DEFINE p_crea_tmp     SMALLINT 
   
   DATABASE safre_tmp

   CASE
      --crea tmp's para la opción 1 "Selección por NSS"
      WHEN p_crea_tmp = 1
         WHENEVER ERROR CONTINUE
            DROP TABLE tmp_rch_sel_nss
            DROP TABLE tmp_sel_nss_aceptado
         WHENEVER ERROR STOP 
            CREATE TABLE tmp_rch_sel_nss (
                           nss             CHAR(11),
                           sdo92           DECIMAL(16,2),
                           sdo97           DECIMAL(16,2),
                           suma_saldo      DECIMAL(18,2),
                           aivs92          DECIMAL(16,2),
                           aivs97          DECIMAL(16,2),
                           suma_aivs       DECIMAl(18,2),
                           tpo_credito     SMALLINT,
                           num_credito     DECIMAL(10,0),
                           f_otorga        DATE,
                           marca_operativa SMALLINT,
                           marca_procesar  SMALLINT,
                           rch_cod_marca   SMALLINT,
                           tpo_originacion SMALLINT,
                           causal_rch      SMALLINT);
                           
            CREATE TABLE tmp_sel_nss_aceptado(
                           nss                CHAR(11),
                           id_cre_acreditado  DECIMAL(9,0),
                           id_derechohabiente DECIMAL(9,0),
                           tpo_credito        SMALLINT,
                           num_credito        DECIMAL(10,0),
                           tpo_originacion    SMALLINT,
                           f_otorga           DATE,
                           marca_operativa    SMALLINT,
                           marca_procesar     SMALLINT,
                           sdo92              DECIMAL(16,2),
                           sdo97              DECIMAL(16,2),
                           suma_saldo         DECIMAL(18,2),
                           aivs92             DECIMAL(16,2),
                           aivs97             DECIMAL(16,2),
                           suma_aivs          DECIMAl(18,2),
                           estado             SMALLINT);

      --crea tmp's para la opción 2 "Selección por bloques de NSS"
      WHEN p_crea_tmp = 2
         WHENEVER ERROR CONTINUE
            DROP TABLE tmp_sel_bloque
            DROP TABLE tmp_rch_sel_bloque
            DROP TABLE tmp_sel_bloque_aceptado
         WHENEVER ERROR STOP 
            CREATE TABLE tmp_sel_bloque(
                           nss CHAR(11));
            CREATE TABLE tmp_rch_sel_bloque(
                           nss             CHAR(11),
                           sdo92           DECIMAL(16,2),
                           sdo97           DECIMAL(16,2),
                           suma_saldo      DECIMAL(18,2),
                           aivs92          DECIMAL(16,2),
                           aivs97          DECIMAL(16,2),
                           suma_aivs       DECIMAl(18,2),
                           tpo_credito     SMALLINT,
                           num_credito     DECIMAL(10,0),
                           f_otorga        DATE,
                           marca_operativa SMALLINT,
                           marca_procesar  SMALLINT,
                           rch_cod_marca   SMALLINT, 
                           tpo_originacion SMALLINT,
                           causal_rch      SMALLINT);

            CREATE TABLE tmp_sel_bloque_aceptado(
                           nss                CHAR(11),
                           id_cre_acreditado  DECIMAL(9,0),
                           id_derechohabiente DECIMAL(9,0),
                           tpo_credito        SMALLINT,
                           num_credito        DECIMAL(10,0),
                           tpo_originacion    SMALLINT,
                           f_otorga           DATE,
                           marca_operativa    SMALLINT,
                           marca_procesar     SMALLINT,
                           sdo92              DECIMAL(16,2),
                           sdo97              DECIMAL(16,2),
                           suma_saldo         DECIMAL(18,2),
                           aivs92             DECIMAL(16,2),
                           aivs97             DECIMAL(16,2),
                           suma_aivs          DECIMAl(18,2),
                           estado             SMALLINT);
                              
      --crea tmp's para la opción 3 "Selección por parámetros"
      WHEN p_crea_tmp = 3
         WHENEVER ERROR CONTINUE 
            DROP TABLE tmp_rch_sel_parametro
            DROP TABLE tmp_sel_parametro_aceptado
         WHENEVER ERROR STOP 
            CREATE TABLE tmp_rch_sel_parametro(
                          nss             CHAR(11),
                          sdo92           DECIMAL(16,2),
                          sdo97           DECIMAL(16,2),
                          suma_saldo      DECIMAL(18,2),
                          aivs92          DECIMAL(16,2),
                          aivs97          DECIMAL(16,2),
                          suma_aivs       DECIMAl(18,2),
                          tpo_credito     SMALLINT,
                          num_credito     DECIMAL(10,0),
                          f_otorga        DATE,
                          marca_operativa SMALLINT,
                          marca_procesar  SMALLINT,
                          rch_cod_marca   SMALLINT, 
                          tpo_originacion SMALLINT,
                          causal_rch      SMALLINT);

            CREATE TABLE tmp_sel_parametro_aceptado(
                           nss                CHAR(11),
                           id_cre_acreditado  DECIMAL(9,0),
                           id_derechohabiente DECIMAL(9,0),
                           tpo_credito        SMALLINT,
                           num_credito        DECIMAL(10,0),
                           tpo_originacion    SMALLINT,
                           f_otorga           DATE,
                           marca_operativa    SMALLINT,
                           marca_procesar     SMALLINT,
                           sdo92              DECIMAL(16,2),
                           sdo97              DECIMAL(16,2),
                           suma_saldo         DECIMAL(18,2),
                           aivs92             DECIMAL(16,2),
                           aivs97             DECIMAL(16,2),
                           suma_aivs          DECIMAl(18,2),
                           estado             SMALLINT);
   END CASE 
   
   DATABASE safre_viv

      -- Tabla donde se obienen remanentes de cre_acreditado para procesar 
      WHENEVER ERROR CONTINUE
         DROP TABLE tmp_remanente_agr;
         DROP TABLE tmp_saldo_remanente;

      WHENEVER ERROR STOP
         CREATE TABLE tmp_remanente_agr(
                         nss           CHAR(11),
                         id_cre_acreditado  DECIMAL(9,0),
                         id_derechohabiente DECIMAL(9,0), 
                         tpo_originacion SMALLINT,
                         marca_prcr   SMALLINT,
                         tpo_credito  SMALLINT,
                         marca_ifv    SMALLINT,
                         num_credito  DECIMAL(10,0),
                         sdo_deudor   DECIMAL(12,2),
                         f_otorga     DATE,
                         estado       SMALLINT,
                         edo_procesar SMALLINT);

         CREATE TABLE tmp_saldo_remanente(
                         id_derechohabiente DECIMAL(9,0),
                         subcuenta SMALLINT,
                         pesos     DECIMAL(16,2),
                         acciones  DECIMAL(16,2));

END FUNCTION 