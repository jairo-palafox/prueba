############################################################################### 
#Modulo            => OCG                                                     #
#Programa          => OCGP20                                                  #
#Objetivo          => Programa para extracci�n de aportaciones subsecuentes   # 
#                     cofinavit pendientes por pagar a la EF.                 # 
#Autor             => Emilio Abarca S�nchez  EFP                              #
#Fecha inicio      => 02 ENERO 2017                                           #
###############################################################################

DATABASE safre_viv

GLOBALS 

   DEFINE p_usuario                CHAR(20)   # Usuario logueado
   DEFINE p_titulo                 STRING     # Titulo de la ventana
   DEFINE p_tipo_ejecucion         SMALLINT   # Tipo e ejecuci�n
   DEFINE v_ruta_envio             LIKE seg_modulo.ruta_envio
   
END GLOBALS 

MAIN

   LET p_usuario          =   ARG_VAL  (1)
   LET p_tipo_ejecucion   =   ARG_VAL  (2)
   LET p_titulo           =   ARG_VAL  (3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario CLIPPED|| ".OCGP20.log")

   -- Se asigna el t�tulo de la ventana
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   --Se obtiene la ruta donde se alojar�n lor archivos de salida
   SELECT ruta_envio
     INTO v_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'ocg'

   CLOSE WINDOW SCREEN 
   CALL extrae_ap_subsecuentes()
   
END MAIN 

FUNCTION extrae_ap_subsecuentes()

   DEFINE f_desde              DATE 
   DEFINE f_hasta              DATE 
   DEFINE v_f_ejecucion        DATE
   DEFINE cbx_ef               CHAR(67) --combobox
   DEFINE v_resp               BOOLEAN
   DEFINE v_id_ocg_form        LIKE ocg_formalizacion.id_ocg_formalizacion
   DEFINE v_id_ocg_det         LIKE ocg_formalizacion.id_ocg_detalle
   DEFINE v_id_ocg_tmt         LIKE ocg_formalizacion.id_ocg_tramite
   DEFINE v_id_dh              LIKE ocg_formalizacion.id_derechohabiente
   DEFINE v_cve_ef             LIKE ocg_formalizacion.cve_ent_financiera
   DEFINE v_num_ctrl_int       LIKE ocg_formalizacion.num_ctr_int_ef
   DEFINE v_tpo_credito        LIKE ocg_formalizacion.tpo_credito
   DEFINE v_f_liq_cofi         LIKE ocg_fecha_mig.f_liquida_cofi
   DEFINE v_exists_pagados     SMALLINT 
   DEFINE v_exist_ctr          SMALLINT 
   DEFINE v_peticion_ug        INTEGER
   DEFINE v_pag_ug             INTEGER 
   DEFINE v_pag_dev            INTEGER 
   DEFINE v_folio              LIKE cta_his_pagos.folio
   DEFINE v_id_referencia      LIKE cta_his_pagos.id_referencia
   DEFINE v_nrp                LIKE cta_his_pagos.nrp
   DEFINE v_periodo_pago       LIKE cta_his_pagos.periodo_pago
   DEFINE v_folio_sua          LIKE cta_his_pagos.folio_sua
   DEFINE v_f_pago             LIKE cta_his_pagos.f_pago
   DEFINE v_imp_ap_pat         LIKE cta_his_pagos.imp_ap_pat
   DEFINE aux_periodo_a�o      CHAR(4)
   DEFINE aux_periodo_mes      CHAR(2)
   DEFINE v_bim_resc           CHAR(2)
   DEFINE v_bimestre           CHAR(6)
   DEFINE v_f_venc_calculada   CHAR(8)
   DEFINE tmp_id_ocg_fz        DECIMAL(9,0)
   DEFINE tmp_id_ocg_tmt       DECIMAL(9,0)
   DEFINE tmp_id_dh            LIKE ocg_formalizacion.id_derechohabiente
   DEFINE tmp_tpo_credito      LIKE ocg_formalizacion.tpo_credito
   DEFINE tmp_nss              CHAR(11)
   DEFINE tmp_cve_ef           LIKE ocg_formalizacion.cve_ent_financiera
   DEFINE tmp_num_ctr_int      LIKE ocg_formalizacion.num_ctr_int_ef
   DEFINE tmp_periodo_pago     LIKE cta_his_pagos.periodo_pago
   DEFINE tmp_f_venc_cal       CHAR(8)
   DEFINE tmp_f_pago           LIKE cta_his_pagos.f_pago
   DEFINE tmp_imp_ap_pat       LIKE cta_his_pagos.imp_ap_pat
   DEFINE tmp_monto_pesos      DECIMAL(12,2)
   DEFINE tmp_f_liquidacion    LIKE ocg_fecha_mig.f_liquida_cofi
   DEFINE tmp_bimestre         CHAR(6)
   DEFINE seq_ug               DECIMAL(9,0)
   DEFINE seq_detalle          DECIMAL(9,0)
   DEFINE v_nss                CHAR(11)
   DEFINE v_mnsj_confirma      STRING 
   DEFINE v_mnsj1              STRING
   DEFINE v_exit_mov           SMALLINT
   DEFINE v_f_query            STRING 
   DEFINE v_qry_tab_mov        STRING
   DEFINE v_qry_acciones       STRING 
   DEFINE v_criterio           SMALLINT
   DEFINE v_tabla              VARCHAR(20)
   DEFINE v_monto_acciones     DECIMAL(16,6)
   DEFINE v_ef_desc            CHAR(60)
   DEFINE v_precio_fondo       DECIMAL(19,14)
   DEFINE v_monto_pesos        DECIMAL(12,2)
   DEFINE v_bandera            BOOLEAN
   DEFINE v_ind_trx            INTEGER 
   DEFINE v_exist_trx          INTEGER 
   DEFINE v_qry_fn             STRING 
   DEFINE v_retorno            SMALLINT  

   --Contadores 
   DEFINE contcofi             INTEGER
   DEFINE k                    INTEGER
   DEFINE v_f_venc_ug          CHAR(8)

OPEN WINDOW vtn1 WITH FORM "OCGP211"

   LET f_desde       = NULL
   LET f_hasta       = TODAY -1  
   LET v_f_ejecucion = TODAY
   LET contcofi      = 0
   LET k             = 0
   LET v_criterio    = 0

   LET v_id_ocg_form        = NULL 
   LET v_id_ocg_det         = NULL 
   LET v_id_ocg_tmt         = NULL 
   LET v_id_dh              = NULL 
   LET v_cve_ef             = NULL  
   LET v_num_ctrl_int       = NULL
   LET v_tpo_credito        = NULL
   LET v_f_liq_cofi         = NULL 
   LET v_folio              = NULL 
   LET v_id_referencia      = NULL 
   LET v_nrp                = NULL 
   LET v_periodo_pago       = NULL 
   LET v_folio_sua          = NULL 
   LET v_f_pago             = NULL 
   LET v_imp_ap_pat         = NULL
   LET seq_ug               = NULL
   LET seq_detalle          = NULL 
   LET v_f_pago             = NULL 
   LET tmp_id_ocg_fz        = NULL 
   LET tmp_id_ocg_tmt       = NULL 
   LET tmp_id_dh            = NULL 
   LET tmp_tpo_credito      = NULL 
   LET tmp_nss              = NULL 
   LET tmp_cve_ef           = NULL 
   LET tmp_num_ctr_int      = NULL 
   LET tmp_periodo_pago     = NULL 
   LET tmp_f_venc_cal       = NULL 
   LET tmp_f_pago           = NULL 
   LET tmp_imp_ap_pat       = NULL 
   LET tmp_monto_pesos      = NULL 
   LET tmp_f_liquidacion    = NULL 
   LET tmp_bimestre         = NULL
   LET v_monto_acciones     = NULL 
   LET v_exit_mov           = 0   
   LET v_peticion_ug        = 0
   LET v_pag_ug             = 0
   LET v_exists_pagados     = 0
   LET v_exist_ctr          = 0
   LET v_bandera            = 0
   LET v_pag_dev            = 0
   LET v_ind_trx            = 0
   LET v_exist_trx          = 0

   
   INPUT BY NAME f_desde, f_hasta,cbx_ef ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS)
      BEFORE INPUT  
       DISPLAY  v_f_ejecucion TO f_ejecucion
       CALL carga_cbx_ent_financiera() 

      ON ACTION ACCEPT
         LET contcofi = 0

         IF (f_desde IS NULL) THEN
            CALL fn_mensaje("","Seleccione la fecha del campo 'Fecha desde'","")
            NEXT FIELD f_desde
         ELSE 
            IF (f_desde = TODAY) THEN
               CALL fn_mensaje("","La 'Fecha desde'  no debe ser la fecha actual","")
               NEXT FIELD f_desde
            END IF 
         END IF 

         IF (f_hasta IS NULL) THEN
            CALL fn_mensaje("","Seleccione la fecha del campo 'Fecha hasta' ","")
            NEXT FIELD f_hasta
         ELSE 
            IF (f_hasta = TODAY) THEN 
               CALL fn_mensaje("","La 'Fecha hasta' debe ser un d�a anterior","")
               NEXT FIELD f_hasta
            END IF 
         END IF

         IF (cbx_ef IS NULL) THEN
            CALL fn_mensaje("","Seleccione la entidad financiera","")
            NEXT FIELD cbx_ef
         END IF 

         -- Obtiene la descripci�n de la EF
         SELECT ent_financiera_desc
            INTO v_ef_desc
            FROM cat_entidad_financiera
           WHERE cve_ent_financiera = cbx_ef
           
         LET v_mnsj_confirma = "� Esta seguro de generar la extracci�n para la entidad financiera: ",cbx_ef CLIPPED,"-",v_ef_desc CLIPPED," ?"
         LET v_resp = fn_ventana_confirma("",v_mnsj_confirma,"")
         
           IF (v_resp = 0) THEN
               CALL fn_mensaje("","Se ha cancelado la operaci�n","")
               EXIT INPUT 
           ELSE
               # Se ejecuta la funcion que actualiza el campo f_liquida_cofi por si es NULO
               # ya que se toma como pivote para el rango de fechas para los registros a extraer.
               LET v_qry_fn = "EXECUTE FUNCTION fn_act_liq_cofi()"

               PREPARE prp_exe_fn FROM v_qry_fn
               EXECUTE prp_exe_fn INTO v_retorno

               --DISPLAY "v_retorno: ",v_retorno
               
               # Se crean tablas temporales  
               CALL crea_tablas_tmp()

               -- Se calcula el valor de la aiv
               SELECT precio_fondo
                 INTO v_precio_fondo
                 FROM glo_valor_fondo
                WHERE f_valuacion = TODAY 
                  AND fondo       = 11;
               
               -- Recupera informaci�n de ocg_formalizacion de acuerdo 
               -- al rango de la fecha inicial y final.
               -- se agrega tipo de rabajador I 15/03/2019
               DECLARE crsr_1 CURSOR FOR 
                  SELECT frm.id_ocg_formalizacion,
                         frm.id_ocg_detalle, 
                         frm.id_ocg_tramite,
                         frm.id_derechohabiente,
                         frm.cve_ent_financiera,
                         frm.num_ctr_int_ef, 
                         frm.tpo_credito
                    FROM ocg_formalizacion frm,
                         ocg_fecha_mig mg,
                         afi_derechohabiente a
                   WHERE frm.id_ocg_formalizacion = mg.id_ocg_referencia
                     AND frm.id_derechohabiente   = mg.id_derechohabiente
                     AND frm.tpo_credito IN (7,8) # Cr�dito cofinavit.
                     AND frm.cve_ent_financiera = cbx_ef   # Clave entidad financiera
                     AND frm.diagnostico = 1   # V�lido 
                     --AND frm.estado      = 20  # Vigente
                     AND frm.situacion   = 80  # Marcado en procesar
                     AND mg.f_liquida_cofi >= f_desde
                     AND mg.f_liquida_cofi <= f_hasta
                     AND mg.subproceso   = 2
                     AND frm.id_derechohabiente = a.id_derechohabiente
                     AND a.tipo_trabajador = 'I' ;
                     
               FOREACH crsr_1 INTO v_id_ocg_form, v_id_ocg_det, v_id_ocg_tmt, v_id_dh, v_cve_ef, 
                                   v_num_ctrl_int, v_tpo_credito

                  -- Se obtiene el nss
                  SELECT nss
                    INTO v_nss
                    FROM afi_derechohabiente
                   WHERE id_derechohabiente = v_id_dh

                  -- Recupera la fecha de liquidaci�n
                  SELECT f_liquida_cofi
                     INTO v_f_liq_cofi
                     FROM ocg_fecha_mig
                    WHERE id_ocg_referencia  = v_id_ocg_form
                      AND id_derechohabiente = v_id_dh
                      AND subproceso         = 2                 

                  IF (v_f_liq_cofi IS NOT NULL) THEN 

                     WHENEVER ERROR CONTINUE 

                     -- Busca las aportaciones subsecuentes pendientes por pagar
                     DECLARE crsr_2 CURSOR FOR 
                        SELECT folio, 
                               id_referencia,
                               nrp, 
                               periodo_pago, 
                               folio_sua, 
                               f_pago,
                               imp_ap_pat
                          FROM cta_his_pagos
                         WHERE id_derechohabiente = v_id_dh
                           AND f_pago >= v_f_liq_cofi 
                           AND origen_archivo  = 1 -- Indicador de pago LQ
                           AND ind_liquidacion = 0

                     IF (SQLCA.SQLCODE = -710) OR (SQLCA.SQLCODE = -206) THEN
                        CALL fn_mensaje("","No se puede continuar con la extracci�n, debido a que la tabla historica de pagos se encuentra bloqueada o en mantenimiento","") 
                        EXIT INPUT 
                     END IF 

                     WHENEVER ERROR STOP 
                     
                     FOREACH crsr_2 INTO v_folio, v_id_referencia, v_nrp, v_periodo_pago, 
                                         v_folio_sua, v_f_pago, v_imp_ap_pat

                        LET v_bandera = 0

                        -- Funci�n para b�squeda de folios historicos
                        LET v_f_query = "EXECUTE FUNCTION fn_tab_movimiento(?,?,?)"

                        PREPARE prp_obt_mov FROM v_f_query
                        EXECUTE prp_obt_mov USING v_criterio, -- Debe ser 0
                                                  v_folio,    -- Folio de Liquidaci�n
                                                  " "         -- En este caso no es necesario la fecha
                                             INTO v_tabla

                        LET v_qry_tab_mov = "SELECT COUNT(*)
                                               FROM ",v_tabla,
                                            " WHERE folio_liquida      = ",v_folio,
                                              " AND id_referencia      = ",v_id_referencia,
                                              " AND id_derechohabiente = ",v_id_dh,
                                              " AND monto_pesos        = ",v_imp_ap_pat
                                             

                        PREPARE prp_obt_tab FROM v_qry_tab_mov
                        EXECUTE prp_obt_tab INTO v_exit_mov 
                        
                        IF (v_exit_mov = 0 ) OR (v_exit_mov IS NULL ) THEN
                           LET v_exit_mov = 0
                           CONTINUE FOREACH 
                        END IF
                      
                        -- Obtiene monto de acciones de cta_movimiento
                        LET v_qry_acciones = "SELECT monto_acciones
                                                FROM ",v_tabla,
                                             " WHERE folio_liquida      = ",v_folio,
                                             "   AND id_referencia      = ",v_id_referencia,
                                             "   AND id_derechohabiente = ",v_id_dh,
                                             "   AND monto_pesos        = ",v_imp_ap_pat
                                           
                        PREPARE prp_monto_acciones FROM v_qry_acciones
                        EXECUTE prp_monto_acciones INTO v_monto_acciones
                        
                        IF(v_monto_acciones IS NOT NULL) THEN

                           LET v_monto_pesos = v_monto_acciones * v_precio_fondo

                          -- DISPLAY "nss:", v_nss," id_dh:",v_id_dh CLIPPED," f_pago: ", v_f_pago," periodo_pago: ",v_periodo_pago,
                          -- " imp_ap_pat:",v_imp_ap_pat," monto_acciones:", v_monto_acciones, " monto_pesos_cal:", v_monto_pesos
                            
                           SELECT COUNT(*)
                              INTO v_exists_pagados
                              FROM dis_interface_ef
                             WHERE id_derechohabiente = v_id_dh
                               AND folio_sua    = v_folio_sua
                               AND f_pago       = v_f_pago
                               AND nrp          = v_nrp
                            -- AND aiv_ap_pat   = v_monto_acciones

                            --DISPLAY "Existe Dis: ", v_exists_pagados

                           IF(v_exists_pagados >= 1) THEN
                              LET v_exists_pagados = 0
                              CONTINUE FOREACH 
                           ELSE
                              -- Conversi�n periodo_pago a bimestre 
                              LET aux_periodo_a�o = v_periodo_pago[1,4]
                              LET aux_periodo_mes = v_periodo_pago[5,6]

                              LET v_f_venc_ug     = aux_periodo_mes||"01"||aux_periodo_a�o

                              SELECT bimestre
                                INTO v_bim_resc
                                FROM ocg_ctrl_bimestres
                               WHERE periodo = aux_periodo_mes
                        
                              LET v_bimestre = aux_periodo_a�o||v_bim_resc

                              -- Calcula la fecha de vencimiento(mes-dia-a�o)
                              LET v_f_venc_calculada = v_bim_resc||"01"||aux_periodo_a�o

                              LET v_ind_trx = 0

                              -- Se verifica que no exista en ocg_ctr_transaccion
                              SELECT COUNT(*)
                                 INTO v_exist_ctr
                                 FROM ocg_ctr_transaccion
                                WHERE id_ocg_formalizacion = v_id_ocg_form
                                  AND id_derechohabiente   = v_id_dh
                                --AND cve_ent_financiera   = v_cve_ef 
                                  AND periodo_pago         = v_bimestre   
                                  AND concepto IN (107,307,807,117,317,817)
                                  AND estado IN (70,80);

                             --DISPLAY "Existe Trans: ", v_exist_ctr

                              IF (v_exist_ctr >= 1) THEN
                                 LET v_ind_trx = 1
                                 CONTINUE FOREACH 
                              END IF 

                              SELECT COUNT(*)
                                 INTO v_exist_trx
                                 FROM ocg_trx
                                WHERE id_derechohabiente = v_id_dh
                                --AND cve_ent_financiera = v_cve_ef
                                  AND bimestre = v_bimestre

                              --DISPLAY "Existe trx: ",v_exist_trx
                                  
                              IF (v_exist_trx >= 1) THEN
                                 LET v_ind_trx = 1
                                 CONTINUE FOREACH  
                              END IF 

                              IF (v_ind_trx = 0) THEN 

                                 -- Verifica en ocg_solicitud_uso_garantia que no exista una petici�n.
                                 SELECT COUNT(*)
                                    INTO v_peticion_ug
                                    FROM ocg_solicitud_uso_garantia
                                   WHERE id_ocg_formalizacion = v_id_ocg_form
                                     AND id_derechohabiente   = v_id_dh
                                   --AND cve_ent_financiera   = v_cve_ef
                                     AND f_vencimiento        = v_f_venc_ug
                                     AND situacion IN (50,90)
                                  
                                --DISPLAY "Petici�n UG: ", v_peticion_ug
                              
                                 IF(v_peticion_ug >= 1) THEN
                                    LET v_peticion_ug = 0
                                    CONTINUE FOREACH
                                 ELSE 
                                    -- valida si la solicitud ya fue pagada
                                    SELECT COUNT(*)
                                       INTO v_pag_ug
                                       FROM ocg_solicitud_uso_garantia
                                      WHERE id_ocg_formalizacion = v_id_ocg_form
                                        AND id_derechohabiente   = v_id_dh
                                      --AND cve_ent_financiera   = v_cve_ef
                                        AND f_vencimiento        = v_f_venc_ug
                                        AND situacion IN (100,110)

                                   --DISPLAY "Est� pagada: ",v_pag_ug

                                    IF(v_pag_ug >= 1) THEN

                                       -- valida si el pago se devolvi�  por la EF
                                       SELECT COUNT(*)
                                          INTO v_pag_dev
                                          FROM ocg_devolucion
                                         WHERE id_derechohabiente = v_id_dh
                                         --AND cve_ent_financiera = v_cve_ef
                                           AND periodo_pago       = v_bimestre 
                                           AND situacion NOT IN (120,130)

                                      --DISPLAY "Fue devuelta: ",v_pag_dev

                                       IF(v_pag_dev >= 1) THEN
                                          LET v_bandera = 1
                                       END IF
                                    ELSE 
                                       -- No fue pagada
                                       LET v_bandera = 1
                                    END IF 
                                 END IF 

                                 --DISPLAY "Bandera: ",v_bandera
                                 
                                 IF (v_bandera = 1) THEN 
                                 
                                    -- Los registros prospectos para ug se almacenan en una temporal 
                                    INSERT INTO tmp_prospecto_ug_cofi(
                                                     id_ocg_formalizacion,
                                                     id_ocg_tramite,
                                                     id_derechohabiente,
                                                     tpo_credito,
                                                     nss, 
                                                     cve_ent_financiera,
                                                     num_ctr_int_ef,
                                                     periodo_pago,
                                                     f_venc_calculada,
                                                     f_pago,
                                                     imp_ap_pat,
                                                     monto_pesos,
                                                     f_liquidacion,
                                                     bimestre)
                                              VALUES(v_id_ocg_form,
                                                     v_id_ocg_tmt,
                                                     v_id_dh,
                                                     v_tpo_credito,
                                                     v_nss,
                                                     v_cve_ef,
                                                     v_num_ctrl_int,
                                                     v_periodo_pago,
                                                     v_f_venc_ug, --v_f_venc_calculada,
                                                     v_f_pago,
                                                     v_imp_ap_pat,
                                                     v_monto_pesos,
                                                     v_f_liq_cofi,
                                                     v_bimestre)
                                END IF 
                              END IF 
                           END IF 
                        END IF 
                     END FOREACH
                  END IF
                  
                  LET contcofi = contcofi + 1 
                  
               END FOREACH

               # Generaci�n del archivo de salida con aportaciones pendientes por pagar a la EF
               # de cr�ditos cofinavit
               
               CALL Gen_arhs_salida()

               # Al terminar de generarse el archivo, se realizan las solicitudes del SP003
               
               -- Se recuperan los registros prospectos para ug
               DECLARE csr_sol_sp003 CURSOR FOR 
                  SELECT UNIQUE id_ocg_formalizacion,
                                id_ocg_tramite,
                                id_derechohabiente,
                                tpo_credito,
                                nss,
                                cve_ent_financiera,
                                num_ctr_int_ef,
                                periodo_pago,
                                f_venc_calculada,
                            SUM (monto_pesos)
                           FROM tmp_prospecto_ug_cofi
                       GROUP BY id_ocg_formalizacion,
                                id_ocg_tramite,
                                id_derechohabiente,
                                tpo_credito,
                                nss,
                                cve_ent_financiera,
                                num_ctr_int_ef,
                                periodo_pago,
                                f_venc_calculada

               LET k = 1

               FOREACH csr_sol_sp003 INTO tmp_id_ocg_fz,
                                          tmp_id_ocg_tmt,
                                          tmp_id_dh,
                                          tmp_tpo_credito,
                                          tmp_nss,
                                          tmp_cve_ef,
                                          tmp_num_ctr_int,
                                          tmp_periodo_pago,
                                          tmp_f_venc_cal,
                                          tmp_monto_pesos

                  -- se recuperan valore de la secuencia
                  SELECT seq_ocg_detalle.NEXTVAL
                      INTO seq_detalle
                      FROM systables
                     WHERE tabid = 1
                              
                  SELECT seq_ocg_solic_ug.NEXTVAL 
                     INTO seq_ug
                     FROM systables
                    WHERE tabid = 1

                  INSERT INTO ocg_detalle(
                              id_ocg_detalle,
                              id_ocg_ctr_archivo,
                              id_derechohabiente,
                              subproceso,
                              f_proceso,
                              cve_ent_financiera,
                              nss)
                       VALUES(seq_detalle,
                              0,
                              tmp_id_dh,
                              3,       --- Uso de garantia
                              TODAY,
                              tmp_cve_ef,
                              tmp_nss);
                           
                  INSERT INTO ocg_fecha_mig(
                              id_ocg_referencia,
                              id_ocg_detalle,
                              id_derechohabiente,
                              f_envio, 
                              f_carga,
                              f_respuesta,
                              f_liquida_cofi,
                              subproceso,
                              f_alta_registro)
                      VALUES( seq_ug,
                              seq_detalle,
                              tmp_id_dh,
                              TODAY,
                              TODAY,
                              NULL,
                              NULL,
                              3,
                              TODAY);

                  -- Solicitud de uso de garant�a (SP003)
                  INSERT INTO ocg_solicitud_uso_garantia(
                                 id_ocg_solicitud_ug,
                                 id_ocg_detalle, 
                                 id_ocg_formalizacion,
                                 id_ocg_tramite, 
                                 id_derechohabiente,
                                 cve_ent_financiera,
                                 num_ctr_int_ef,
                                 importe_solicitado,
                                 f_vencimiento,
                                 importe_utilizado,
                                 tpo_credito,
                                 solicitud_saldo,
                                 diagnostico,
                                 estado,
                                 situacion)
                         VALUES( seq_ug,
                                 seq_detalle,
                                 tmp_id_ocg_fz,
                                 tmp_id_ocg_tmt,
                                 tmp_id_dh,
                                 tmp_cve_ef,
                                 tmp_num_ctr_int,
                                 tmp_monto_pesos,
                                 tmp_f_venc_cal,
                                 0,
                                 tmp_tpo_credito,
                                 3,
                                 1,   -- Diagnostico: v�lido
                                 20,  -- estado: vigente
                                 50); -- situaci�n: aceptado

                  LET k = k + 1
                                         
               END FOREACH
 
               LET v_mnsj1 = "Total de registros liquidados en este periodo: ",contcofi, " registros."
                            
               CALL fn_mensaje("",v_mnsj1,"")
               
               CALL fn_mensaje("","El archivo se ha generado en la ruta '/safreviv_int/ocg/envio' con nombre: ' Extractor_Ap_Sub_COFI.txt '","")
               EXIT INPUT  
           END IF 
         
      ON ACTION CANCEL 
         EXIT INPUT  
         
   END INPUT  
   
CLOSE WINDOW vtn1
END FUNCTION 

FUNCTION crea_tablas_tmp()

   WHENEVER ERROR CONTINUE 
      DROP TABLE tmp_prospecto_ug_cofi
     -- DROP TABLE tmp_cofinavit

   WHENEVER ERROR STOP 
      CREATE TABLE tmp_prospecto_ug_cofi(id_ocg_formalizacion DECIMAL(9,0),
                                         id_ocg_tramite       DECIMAL(9,0),
                                         id_derechohabiente   DECIMAL(9,0),
                                         tpo_credito          CHAR(1),
                                         nss                  CHAR(11),
                                         cve_ent_financiera   SMALLINT,
                                         num_ctr_int_ef       CHAR(18),
                                         periodo_pago         CHAR(6),
                                         f_venc_calculada     CHAR(8),
                                         f_pago               DATE,
                                         imp_ap_pat           DECIMAL(12,2),
                                         monto_pesos          DECIMAL(12,2),
                                         f_liquidacion        DATE,
                                         bimestre             CHAR(6)
                                         )

END FUNCTION


FUNCTION Gen_arhs_salida()

   --var�s
   DEFINE v_arh_cofi         STRING
   DEFINE ch_cofi            base.Channel # Objeto para Apoyo Infonavit
   DEFINE v_query1           STRING 
   DEFINE i                  INTEGER
   DEFINE v_detalle_cofi     STRING
   DEFINE comando_cp         STRING 
   DEFINE v_f_pago_calculada LIKE cta_his_pagos.f_pago 

   --Arreglos
   DEFINE arr_cofi DYNAMIC ARRAY OF RECORD
      producto           CHAR(1),
      nss                CHAR(11),
      cve_ent_financiera CHAR(3),
      num_ctr_int_ef     CHAR(18),
      periodo_pago       CHAR(6),
      f_pago             DATE,
      impt_ap_pat        CHAR(14),
      monto_pesos        CHAR(14),
      f_liquidacion      DATE,
      bimestre           CHAR(6)
   END RECORD 

   

   LET v_arh_cofi = v_ruta_envio CLIPPED,"/Extractor_Ap_Sub_COFI.txt"

   # Generaci�n del archivo de salida Apoyo Infonavit
   LET v_query1 = "SELECT tpo_credito,
                          nss,
                          cve_ent_financiera,
                          num_ctr_int_ef,
                          periodo_pago,
                          f_pago,
                          imp_ap_pat,
                          monto_pesos,
                          f_liquidacion,
                          bimestre
                     FROM tmp_prospecto_ug_cofi
                     ORDER BY nss,
                          cve_ent_financiera,
                          num_ctr_int_ef,
                          periodo_pago,
                          f_pago,
                          f_liquidacion,
                          bimestre"

   PREPARE cons_tmp_cofi FROM v_query1
   DECLARE crsr1 CURSOR FOR cons_tmp_cofi

   LET ch_cofi = base.Channel.create()
   CALL ch_cofi.openFile(v_arh_cofi,"w")

   LET i = 1

   CALL arr_cofi.clear()

   FOREACH crsr1 INTO arr_cofi[i].producto,
                      arr_cofi[i].nss,
                      arr_cofi[i].cve_ent_financiera,
                      arr_cofi[i].num_ctr_int_ef,
                      arr_cofi[i].periodo_pago,
                      arr_cofi[i].f_pago,
                      arr_cofi[i].impt_ap_pat,
                      arr_cofi[i].monto_pesos,
                      arr_cofi[i].f_liquidacion,
                      arr_cofi[i].bimestre

      -- Se recalcula la fecha de pago para obtener  el �ltimo d�a del mes anterior.
      LET v_f_pago_calculada = arr_cofi[i].f_pago -  DAY(arr_cofi[i].f_pago)

      LET v_detalle_cofi = arr_cofi[i].producto           CLIPPED,"|",
                           arr_cofi[i].nss                CLIPPED,"|",
                           arr_cofi[i].cve_ent_financiera USING "&&&" CLIPPED,"|",
                           arr_cofi[i].num_ctr_int_ef     CLIPPED,"|",
                           arr_cofi[i].periodo_pago       CLIPPED,"|",
                           v_f_pago_calculada             USING "yyyymmdd" CLIPPED,"|",
                           arr_cofi[i].impt_ap_pat        CLIPPED,"|",
                           arr_cofi[i].monto_pesos        ,"|",
                           arr_cofi[i].f_liquidacion      USING "yyyymmdd" CLIPPED,"|",
                           arr_cofi[i].bimestre           CLIPPED,"|"

      LET i = i + 1

      -- Se escribe en el archivo de salida
      CALL ch_cofi.writeLine(v_detalle_cofi)
      
   END FOREACH 

   -- Se cierra el archivo
   CALL ch_cofi.close()

   -- Realiza copia del archivo generado
   LET comando_cp = "cp ",v_arh_cofi," ",v_ruta_envio CLIPPED,"/Extractor_Ap_Sub_COFI_" CLIPPED, TODAY USING "ddmmyyyy" CLIPPED,".txt"
   RUN comando_cp 

   
END FUNCTION 

FUNCTION carga_cbx_ent_financiera()

   DEFINE cbx_ent_fin     ui.ComboBox
   DEFINE v_cve_ent_fin   LIKE cat_entidad_financiera.cve_ent_financiera
   DEFINE v_ent_fin_desc  LIKE cat_entidad_financiera.ent_financiera_desc
   DEFINE v_concat_ent    CHAR (67) 

   LET cbx_ent_fin = ui.ComboBox.forName("cbx_ef")

   DECLARE cur_ent_fin CURSOR FOR
                       SELECT cve_ent_financiera, ent_financiera_desc
                       FROM cat_entidad_financiera
                       WHERE estado_ef = 10
                       ORDER BY cve_ent_financiera;

    CALL cbx_ent_fin.clear()

    FOREACH cur_ent_fin INTO v_cve_ent_fin, v_ent_fin_desc
        LET v_concat_ent = v_cve_ent_fin || " - ",v_ent_fin_desc
        CALL cbx_ent_fin.addItem(v_cve_ent_fin, v_concat_ent)
    END FOREACH 


END FUNCTION 
