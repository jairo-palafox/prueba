##########################################################################
#Modulo            => OCG                                                #
#Programa          => OCGP12                                             #
#Objetivo          => Programa de Reactivación de crédito liquidado por  #
#                     error.                                             #
#Autor             => Emilio Abarca Sánchez, EFP                         #
#Fecha inicio      => 19 SEPTIEMBRE 2016                                 #
##########################################################################

DATABASE safre_viv

GLOBALS "../../cta/fte/CTAW15.inc"    #archivo de variables globales del WS de consulta de saldo

   #Parámetros de conexión
   PRIVATE DEFINE v_url_servidor      LIKE wsv_cliente.ruta_servidor 
   PRIVATE DEFINE v_usuario           LIKE wsv_cliente.usuario
   PRIVATE DEFINE v_password          LIKE wsv_cliente.password
   PRIVATE DEFINE v_intentos          LIKE wsv_cliente.num_reintento

   --Definiciòn de variables globales, parametros enviados del menu
   DEFINE g_usuario      CHAR(20)
   DEFINE g_tipo_proceso SMALLINT
   DEFINE g_nom_ventana  STRING

MAIN 
   -- se incorpora como parametros enviados desde el menu el proceso y codigo de la operaciòn
   LET g_usuario      = ARG_VAL(1)
   LET g_tipo_proceso = ARG_VAL(2)
   LET g_nom_ventana  = ARG_VAL(3)

   -- se genera el archivo log en caso de Error
   CALL STARTLOG(g_usuario CLIPPED|| ".OCGP12.log")

    -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( g_nom_ventana IS NOT NULL ) THEN
      CALL ui.Interface.setText(g_nom_ventana)
   END IF

   CLOSE WINDOW SCREEN 
   CALL ingresa_nss_ocg()
   
END MAIN

FUNCTION ingresa_nss_ocg()

   DEFINE e_nss                LIKE afi_derechohabiente.nss
   DEFINE bandera              BOOLEAN
   DEFINE v_ex_tbjdr           INTEGER
   DEFINE v_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_exist_ocg_liq      INTEGER 
   DEFINE v_cnt_fz             INTEGER 

OPEN WINDOW w1 WITH FORM "OCGP121"
   INPUT BY NAME e_nss ATTRIBUTE(UNBUFFERED, WITHOUT DEFAULTS)
      
      ON ACTION ACCEPT
         IF (LENGTH (e_nss) < 11) THEN
            CALL fn_mensaje("Información","El nss no debe ser menor a 11 dígitos ","about")
         ELSE 
            --se evalua que sea numerico
            LET bandera = fn_es_numerico(e_nss)

            IF(bandera == 1) THEN
               CALL fn_mensaje("Información","El nss debe ser numérico","exclamation")
            ELSE 
               --verifica que exista en la tabla afi_derechohabiente
               SELECT COUNT(*)
                  INTO v_ex_tbjdr  
                  FROM afi_derechohabiente
                 WHERE nss = e_nss
                               
               IF (v_ex_tbjdr == 0) THEN
                  CALL fn_mensaje("Información","El trabajador no existe","about")
               ELSE
                  --obtenemos el id_derechohabiente
                  SELECT id_derechohabiente INTO v_id_derechohabiente 
                                            FROM afi_derechohabiente
                                            WHERE nss = e_nss;

                       -- valida que no tenga un cédito vigemte
                       SELECT COUNT(*)
                          INTO v_cnt_fz
                          FROM ocg_formalizacion
                         WHERE id_derechohabiente = v_id_derechohabiente
                           AND diagnostico = 1
                           AND estado      = 20
                           AND situacion IN (55,60,70,80)

                       IF (v_cnt_fz >= 1) THEN
                          CALL fn_mensaje("","No es posible realizar la reactivación, ya que el trabajador cuenta con un crédito 43Bis vigente","")
                          CONTINUE INPUT 
                       ELSE 
                          -- verifica en ocg_liquidación si el credito está liquidado
                          SELECT COUNT(*) 
                             INTO v_exist_ocg_liq 
                             FROM ocg_liquidacion
                            WHERE id_derechohabiente = v_id_derechohabiente 
                              AND diagnostico = 1      -- Vállido
                              AND estado      = 30     -- Liquidado
                              AND situacion   = 160;   -- Desmarcado Procesar

                          IF (v_exist_ocg_liq >= 1) THEN
                             CALL carga_inf_dh_ocg(e_nss,v_id_derechohabiente)
                             LET e_nss = NULL
                          ELSE 
                             CALL fn_mensaje("información","El trabajador no cuenta con un crédito liquidado 43Bis ","about")
                          END IF
                       END IF 
               END IF  
            END IF 
         END IF 
            
      ON ACTION CANCEL
         EXIT INPUT
            
   END INPUT   
CLOSE WINDOW w1

END FUNCTION 

FUNCTION carga_inf_dh_ocg(p_nss,p_id_derechohabiente)

   DEFINE p_nss                LIKE afi_derechohabiente.nss
   DEFINE p_id_derechohabiente LIKE afi_derechohabiente.id_derechohabiente
   DEFINE v_answer             BOOLEAN 
   
   --var´s para obtener información que se muestra en el formulario OCGP122
   DEFINE v_id_ocg_liquidacion LIKE ocg_liquidacion.id_ocg_liquidacion
   DEFINE v_id_ocg_detalle     LIKE ocg_liquidacion.id_ocg_detalle
   DEFINE v_id_ocg_frm         LIKE ocg_liquidacion.id_ocg_formalizacion
   DEFINE v_id_ocg_tmte        LIKE ocg_liquidacion.id_ocg_tramite
   DEFINE e_rfc                LIKE ocg_formalizacion.rfc
   DEFINE e_curp               LIKE ocg_formalizacion.curp
   DEFINE v_ap_paterno         LIKE ocg_formalizacion.ap_paterno
   DEFINE v_ap_materno         LIKE ocg_formalizacion.ap_materno
   DEFINE v_nombre             LIKE ocg_formalizacion.nombre
   DEFINE v_cve_ent_fin        LIKE ocg_formalizacion.cve_ent_financiera
   DEFINE v_desc_ent_fin       LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE e_tpo_credito        LIKE ocg_formalizacion.tpo_credito
   DEFINE v_estado             LIKE ocg_formalizacion.estado
   DEFINE v_estado_desc        LIKE cat_ocg_estado.edo_credito_desc
   DEFINE v_situacion          LIKE ocg_formalizacion.situacion
   DEFINE v_situacion_desc     LIKE cat_ocg_situacion.situacion_desc
   DEFINE e_num_ctr_interno    LIKE ocg_formalizacion.num_ctr_int_ef
   DEFINE v_f_liberacion_gtia  LIKE ocg_liquidacion.f_liberacion_gtia
   DEFINE v_id_causa_liquida   LIKE ocg_liquidacion.id_causa_liquida
   DEFINE v_diagnostico        LIKE ocg_liquidacion.diagnostico
   DEFINE v_id_cre_acreditado  LIKE cre_acreditado.id_cre_acreditado
   DEFINE v_id_cre_ctr_archivo LIKE cre_acreditado.id_cre_ctr_archivo
   DEFINE v_edo_procesar       LIKE cre_acreditado.edo_procesar
   DEFINE v_folio_archivo      LIKE cre_ctr_archivo.folio_archivo
   DEFINE v_query              STRING 
   
   --var´s para concatenar 
   DEFINE e_nombre_dh      CHAR(126)
   DEFINE e_ent_financiera CHAR(67)
   DEFINE e_edo_credito    CHAR(50)
   DEFINE e_situacion      CHAR(47)

   --var´s conteo de registros
   DEFINE v_sfr_marca_activa  INTEGER
   DEFINE ret_marca_cuenta    SMALLINT
   DEFINE v_sfr_marca_act_232 INTEGER   

   --var´s WS
   DEFINE v_tpo_originacion   CHAR(3)
   
OPEN WINDOW w2 WITH FORM "OCGP122"
   
   --obtenermos la información en ocg_liquidación
   SELECT FIRST 1
          liq.id_ocg_liquidacion,
          liq.id_ocg_detalle,
          liq.id_ocg_formalizacion,
          liq.id_ocg_tramite,
          frm.rfc,
          frm.curp,
          frm.ap_paterno,
          frm.ap_materno,
          frm.nombre,
          liq.cve_ent_financiera,
          ent.ent_financiera_desc,
          liq.num_ctr_int_ef,
          liq.f_liberacion_gtia,
          liq.id_causa_liquida,
          liq.tpo_credito,
          liq.diagnostico,
          liq.estado,
          edo.edo_credito_desc,
          liq.situacion,
          sit.situacion_desc
     INTO v_id_ocg_liquidacion,
          v_id_ocg_detalle,
          v_id_ocg_frm,
          v_id_ocg_tmte,
          e_rfc,
          e_curp,
          v_ap_paterno,
          v_ap_materno,
          v_nombre,
          v_cve_ent_fin,
          v_desc_ent_fin,
          e_num_ctr_interno,
          v_f_liberacion_gtia,
          v_id_causa_liquida,
          e_tpo_credito,
          v_diagnostico,
          v_estado,
          v_estado_desc,
          v_situacion,
          v_situacion_desc
     FROM ocg_liquidacion liq, 
          ocg_formalizacion frm,
          cat_entidad_financiera ent,
          cat_ocg_estado edo, 
          cat_ocg_situacion sit
    WHERE liq.id_derechohabiente = p_id_derechohabiente
      AND liq.diagnostico = 1  
      AND liq.estado      = 30
      AND liq.situacion   = 160 
      AND liq.id_ocg_formalizacion = frm.id_ocg_formalizacion
      AND liq.cve_ent_financiera = ent.cve_ent_financiera 
      AND liq.estado = edo.edo_credito 
      AND liq.situacion = sit.situacion
      ORDER BY liq.f_liberacion_gtia DESC;
         
    --concatena
    LET e_nombre_dh = v_ap_paterno CLIPPED ||" ",v_ap_materno CLIPPED||" ",v_nombre CLIPPED
    LET e_ent_financiera = v_cve_ent_fin || " - ",v_desc_ent_fin
    LET e_edo_credito = v_estado ||" - ", v_estado_desc
    LET e_situacion = v_situacion || " - ",v_situacion_desc
        
    MENU " "
       BEFORE MENU  
       DISPLAY BY NAME p_nss,
                       e_rfc,
                       e_curp,
                       e_nombre_dh,
                       e_ent_financiera,
                       e_tpo_credito,
                       e_edo_credito,
                       e_situacion,
                       e_num_ctr_interno
                            
       ON ACTION Reactivar 
          LET v_answer = fn_ventana_confirma("Información","¿Esta seguro de reactivar el crédito?","question")

          IF (v_answer == 0) THEN 
             CALL fn_mensaje("Información","Reactivaciòn cancelada","about")
             EXIT MENU 
          ELSE 
             ##Guarda información original del registro en ocg_his_liquidacion
             INSERT INTO ocg_his_liquidacion(id_ocg_liquidacion,
                                             id_derechohabiente,
                                             cve_ent_financiera,
                                             num_ctr_int_ef,
                                             f_liberacion_gtia,
                                             id_causa_liquida,
                                             tpo_credito,
                                             diagnostico,
                                             estado,
                                             situacion,
                                             f_proceso,
                                             usuario)
                                      VALUES(v_id_ocg_liquidacion,
                                             p_id_derechohabiente,
                                             v_cve_ent_fin,
                                             e_num_ctr_interno,
                                             v_f_liberacion_gtia,
                                             v_id_causa_liquida,
                                             e_tpo_credito,
                                             v_diagnostico,
                                             v_estado,
                                             v_situacion,
                                             TODAY,
                                             g_usuario) 

             UPDATE ocg_liquidacion
                SET estado = 50,    -- Cancelado
                    situacion = 190 --Reactivado
                WHERE id_derechohabiente = p_id_derechohabiente 
                  AND id_ocg_liquidacion = v_id_ocg_liquidacion
                  AND diagnostico = 1     --valido
                  AND estado      = 30    --liquidado 
                  AND situacion   = 160;  --liquidado 

             UPDATE ocg_formalizacion
                SET estado    = 20, --vigente
                    situacion = 60  -- Acreditado
                WHERE id_ocg_formalizacion =   v_id_ocg_frm  
                  AND cve_ent_financiera   =   v_cve_ent_fin; 

             UPDATE ocg_acreditado
                SET estado    = 20, --vigente
                    situacion = 60, --Acreditado
                    f_liquida_credito     = NULL ,
                    f_solic_marca_prcr    = NULL ,
                    f_conf_marca_prcr     = NULL ,
                    f_solic_desmarca_prcr = NULL ,
                    f_conf_desmarca_prcr  = NULL 
              WHERE id_ocg_formalizacion  = v_id_ocg_frm; 

             ##Actualiza en ocg_tramite 
             IF (v_id_ocg_tmte IS NOT NULL) AND (v_id_ocg_tmte = " ") THEN  
             UPDATE ocg_tramite 
                SET estado    = 20, --vigente
                    situacion = 60  --Acreditado
                WHERE id_ocg_tramite     = v_id_ocg_tmte #id de ocg_liquidacion
                  AND cve_ent_financiera = v_cve_ent_fin
             END IF 

             ##Verifica que exista crédito vigente en cre_acreditado con la fecha de otorgamiento mas reciente
             SELECT FIRST 1 id_cre_acreditado INTO v_id_cre_acreditado
                    FROM cre_acreditado c, 
                         cat_maq_credito m
                   WHERE c.id_derechohabiente = p_id_derechohabiente
                     AND c.tpo_credito = 2  --crédito apoyo infonavit 
                     AND c.estado      = m.estado
                     AND m.entidad     = 1
                     ORDER BY c.f_otorga DESC;

             --DISPLAY "id_cre_acreditado ", v_id_cre_acreditado
             
             ##En caso de que no encuentre ningun acreditado con crédito vigente
             IF(v_id_cre_acreditado IS NULL) THEN 
             
                --debe tomar el crédito liquidado mas reciente y reactivarlo como vigente
                SELECT FIRST 1 id_cre_acreditado INTO v_id_cre_acreditado
                       FROM cre_acreditado c, cat_maq_credito m
                       WHERE c.id_derechohabiente = p_id_derechohabiente
                         AND c.tpo_credito = 2
                         AND c.estado = m.estado
                         AND m.entidad = 2
                         ORDER BY c.f_otorga DESC;
                                          
             END IF 

             -- Reactiva el crédito como vigente en cre_acreditado
             UPDATE cre_acreditado
                SET estado       = 20, --vigente marcada
                    edo_procesar = 10
               WHERE id_cre_acreditado = v_id_cre_acreditado;
    
             ##verifica que este marcado en sfr_marca_activa
             SELECT COUNT(*) INTO v_sfr_marca_activa
                    FROM sfr_marca_activa
                    WHERE id_derechohabiente = p_id_derechohabiente
                      AND marca = 202; --marca infonavit para apoyo infonavit
                      
              DISPLAY "marcado en sfr_marca_activa", v_sfr_marca_activa
              
             ##En caso de que no exista 
             IF (v_sfr_marca_activa == 0)THEN
             
                --rescata de cre_acreditado más reciente 
                SELECT FIRST 1 c.id_cre_acreditado,
                               c.id_cre_ctr_archivo,
                               c.edo_procesar,
                               r.folio_archivo
                          INTO v_id_cre_acreditado,
                               v_id_cre_ctr_archivo,
                               v_edo_procesar,
                               v_folio_archivo
                          FROM cre_acreditado c, OUTER cre_ctr_archivo r
                         WHERE c.id_derechohabiente = p_id_derechohabiente
                           AND c.tpo_credito = 2
                           AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo
                           ORDER BY f_otorga DESC;

                IF (v_folio_archivo IS NULL) THEN
                   LET v_folio_archivo = 1;
                END IF 

               { 
                DISPLAY "Datos para marca: "
                DISPLAY  p_id_derechohabiente
                DISPLAY  "202"
                DISPLAY  v_id_cre_acreditado
                DISPLAY  v_folio_archivo
                DISPLAY  "0"
                DISPLAY  " "
                DISPLAY  "0"
                DISPLAY  " "
                DISPLAY  "301"
                DISPLAY  g_usuario
                }

                ##Se ejecuta la funcion fn_marca_cuenta para insertar en sfr_marca_activa
                LET v_query = "EXECUTE FUNCTION fn_marca_cuenta(?,?,?,?,?,?,?,?,?,?)"
                PREPARE prp_exe_marca  FROM v_query
                EXECUTE prp_exe_marca  USING p_id_derechohabiente,--id_derechohabiente
                                              '202',              --marca entra
                                              v_id_cre_acreditado,--id_cre_acreditado rescatado
                                              v_folio_archivo,    --folio rescatado
                                              '0',                --estado marca
                                              " ",                --codigo rechazo
                                              '0',                --marca causa
                                              " ",                --fecha causa
                                              g_usuario,          --usuario
                                              '301'               --proceso_cod
                                              INTO ret_marca_cuenta
                                              
                DISPLAY "Return de la funcion" , ret_marca_cuenta
                
              

             ##Inserta en cre_his_acreditado
             INSERT INTO cre_his_acreditado(id_cre_acreditado,
                                            id_cre_ctr_archivo,
                                            tpo_transferencia,
                                            edo_procesar,
                                            diagnostico,
                                            estado,
                                            nss_afore,
                                            rfc_afore,
                                            paterno_afore,
                                            materno_afore,
                                            nombre_afore,
                                            nom_imss,
                                            f_proceso)
                                    VALUES (v_id_cre_acreditado,
                                            v_id_cre_ctr_archivo,
                                            16,
                                            55,
                                            NULL,
                                            20,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            NULL,
                                            TODAY);
                                            
             ##Elimina solicitud de desmarca en cta_marca_ws
             DELETE FROM cta_marca_ws
                    WHERE id_derechohabiente = p_id_derechohabiente
                      AND tpo_credito = 2  --Apoyo infonavit
                      AND situacion   = 0;  
             
             ##Verifica en sfr_marca_activa que el registro esté marcado con 232
             SELECT COUNT(*) INTO v_sfr_marca_act_232
                   FROM sfr_marca_activa
                   WHERE id_derechohabiente = p_id_derechohabiente
                     AND marca = 232; #Marca procesar para crédito apoyo infonavit
                     
             DISPLAY "marca 232 : ", v_sfr_marca_act_232
             
                IF (v_sfr_marca_act_232 = 0) THEN
                   --Se ejecuta WS de consulta de saldo a Procesar
               
                   CALL fn_crea_tablas_tmp() --Crea tabla tmp donde guardará la originación de credito que regresará la consulta del WS
                   CALL fn_configura_ws()    --Se configura el WS cliente para solicitud de consulta de saldos procesar
                   CALL fn_recupera_marca(p_nss,v_ap_paterno,v_ap_materno,v_nombre)  --Se ejecuta el WS  

                   ##_Consulta para recuperar el tipo de originacion de crédito 
                
                   SELECT tpo_originacion INTO v_tpo_originacion
                     FROM safre_tmp:tmp_orgn_tpo_credito
                     WHERE nss = p_nss;

                   DISPLAY "v_tpo_originacion : ", v_tpo_originacion 
                
                   CASE 
                      WHEN v_tpo_originacion = 2
                         UPDATE cre_acreditado
                            SET edo_procesar = 120
                          WHERE id_cre_acreditado = v_id_cre_acreditado;

                      WHEN v_tpo_originacion IS NULL 
                         UPDATE cre_acreditado
                            SET edo_procesar = 50
                          WHERE id_cre_acreditado = v_id_cre_acreditado;

                      WHEN v_tpo_originacion = 01 OR v_tpo_originacion = 04
                         UPDATE cre_acreditado
                            SET edo_procesar = 40
                          WHERE id_cre_acreditado = v_id_cre_acreditado;
                   END CASE 

                   ##Inserta solicitud de marca a Procesar en cta_marca_ws
               
                   INSERT INTO cta_marca_ws(id_derechohabiente,
                                            id_origen,
                                            modulo_cod,
                                            tpo_credito,
                                            marca,
                                            f_solicita,
                                            intento,
                                            cod_result_op,
                                            diagnostico,
                                            situacion,
                                            num_credito,
                                            f_infonavit,
                                            marca_procesar,
                                            folio_archivo,
                                            usuario)
                                    VALUES (p_id_derechohabiente,
                                            v_id_cre_acreditado,
                                            18,
                                            2,
                                            232,
                                            TODAY,
                                            1,
                                            NULL,
                                            NULL,
                                            0,
                                            0,
                                            TODAY,
                                            02,
                                            v_folio_archivo,
                                            g_usuario)
                END IF 
             END IF 
            
             IF (SQLCA.SQLCODE = 0 AND ret_marca_cuenta = 0) THEN
                CALL fn_mensaje("Información","La reactivación se ha realizado correctamente","about")
                EXIT MENU 
             ELSE 
                CALL fn_mensaje("Información","No se pudo realizar la reactivación","stop")
                EXIT MENU  
             END IF
          END IF

       ON ACTION CANCEL
            EXIT MENU 

       ON ACTION CLOSE 
            EXIT MENU 
        
    END MENU 
CLOSE WINDOW w2
   
END FUNCTION 

FUNCTION fn_es_numerico(p_cadena)

   DEFINE p_cadena   STRING 
   DEFINE v_idx      INTEGER
   DEFINE indicador  BOOLEAN 

   FOR v_idx = 1 TO p_cadena.getLength()
      IF(p_cadena.subString(v_idx,v_idx) MATCHES '[0-9]') THEN
         LET indicador = 0
      ELSE 
         LET indicador = 1
         IF(indicador == 1) THEN
            EXIT FOR 
         END IF 
      END IF 
   END FOR 

   RETURN indicador 

END FUNCTION 

FUNCTION fn_crea_tablas_tmp()

DATABASE safre_tmp

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_orgn_tpo_credito

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_orgn_tpo_credito (nss              CHAR(11),
                                         tpo_originacion  CHAR(3))

DATABASE safre_viv
END FUNCTION 

FUNCTION fn_configura_ws()

   DEFINE v_consulta    STRING
   DEFINE cve_cliente   CHAR(10)

   #La clave 'cre_3' del catálogo de clientes de webServices corresponde a la solicitud de consulta de saldos con procesar
   LET v_consulta = "SELECT ruta_servidor, 
                            usuario, 
                            password, 
                            num_reintento 
                     FROM   wsv_cliente 
                     WHERE  cve_cliente = ?"

    PREPARE exe_consulta FROM v_consulta
    LET cve_cliente = "cre_3"

    EXECUTE exe_consulta USING cve_cliente INTO v_url_servidor,
                                                v_usuario,
                                                v_password,
                                                v_intentos
END FUNCTION 

FUNCTION fn_recupera_marca(p_nss,p_paterno,p_materno,p_nombre)

    DEFINE p_nss                 CHAR(11)
    DEFINE p_paterno             CHAR(40)
    DEFINE p_materno             CHAR(40)
    DEFINE p_nombre              CHAR(40)
    
    DEFINE soapStatus           INTEGER
    DEFINE v_tpo_originacion    SMALLINT
    
    ##se invoca la función que ejecuta el web service de consulta de saldo a procesar
    CALL consultaSaldo(v_url_servidor,
                       v_usuario,
                       v_password,
                       p_paterno CLIPPED,
                       p_materno CLIPPED,
                       p_nombre CLIPPED,
                       p_nss)
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
                       
    ##Si no hay ningun error, se obtienene la información
    DISPLAY "soapStatus : ", soapStatus

    IF(soapStatus == 0) THEN 
       --Recuperamos el origen de tipo de crédito de la consulta del WS
       LET v_tpo_originacion = ConsultaSaldoRespVO.origenTipoCredito

       INSERT INTO safre_tmp:tmp_orgn_tpo_credito 
              VALUES (p_nss,v_tpo_originacion); 
    END IF 
    
END FUNCTION 

