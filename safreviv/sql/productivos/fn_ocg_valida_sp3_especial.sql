






CREATE FUNCTION "safreviv".fn_ocg_valida_sp3_especial(p_id_ocg_ctr_arch DECIMAL(9,0))

   RETURNING SMALLINT,INTEGER,INTEGER

   DEFINE v_error                   SMALLINT       ;
   DEFINE v_cnt_aceptados           INTEGER        ;
   DEFINE v_cnt_rechazados          INTEGER        ;
   DEFINE v_subproceso              SMALLINT       ;
   DEFINE v_tpo_envio               CHAR(1)        ;
   DEFINE v_cve_ent_financiera      DECIMAL(3,0)   ;
   DEFINE v_nss                     CHAR(11)       ;
   DEFINE v_num_ctrl_ef             CHAR(18)       ;
   DEFINE v_f_envio                 DATE           ;
   DEFINE v_tpo_credito             CHAR(1)        ;
   DEFINE v_viv97                   DECIMAL(8,0)   ;
   DEFINE v_imp_solic_uti_ocg       DECIMAL(12,2)  ;
   DEFINE v_f_venc_imp_solic        DATE           ;
   DEFINE v_solic_saldo             SMALLINT       ;
   DEFINE v_id_ocg_detalle          DECIMAL(9,0)   ;
   DEFINE v_id_ocg_solic_ug         DECIMAL(9,0)   ;
   DEFINE v_estado                  SMALLINT       ;
   DEFINE v_bnd_inconsistencia      SMALLINT       ;
   DEFINE v_situacion               SMALLINT       ;
   DEFINE v_diagnostico             DECIMAL(2,0)   ;
   DEFINE v_ax_f_envio              DATE           ;
   DEFINE v_bnd_rl                  SMALLINT       ;
   DEFINE v_id_dh                   DECIMAL(9,0)   ;
   DEFINE v_tpo_trabajador          CHAR(1)        ;
   DEFINE v_ind_edo_cuenta          SMALLINT       ;
   DEFINE v_idx_nss                 SMALLINT       ;
   DEFINE v_f_proceso               DATE           ;
   DEFINE v_inconsistencia          CHAR(2)        ;
   DEFINE v_nss_unificado           CHAR(11)       ;
   DEFINE v_nss_unificador          CHAR(11)       ;
   DEFINE v_id_dh_unificador        DECIMAL(9,0)   ;
   DEFINE v_diag                    SMALLINT       ;
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0)   ;
   DEFINE v_id_ocg_tramite          DECIMAL(9,0)   ;
   DEFINE v_ax_cve_ent_financiera   DECIMAL(3,0)   ;
   DEFINE v_ax_edo                  SMALLINT       ;
   DEFINE v_cnt_cve_ef              SMALLINT       ;
   DEFINE v_f_registro_carta        DATE           ;
   DEFINE v_aux_ind                 SMALLINT       ;
   DEFINE bnd_rel_laboral           SMALLINT       ;
   DEFINE v_nss_rl                  CHAR(11)       ;
   DEFINE v_f_alta                  CHAR(10)       ;
   DEFINE v_f_baja_rl               DATE           ;
   DEFINE v_f_alta_rl               DATE           ; 
  


   ON EXCEPTION SET v_error
      LET v_cnt_aceptados      = 0;
      LET v_cnt_rechazados     = 0;
      RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_valida_sp3_especial.trace';
   TRACE ON;

   -- Inicializa variables
   LET v_error                 = 0    ;
   LET v_subproceso            = "003";
   LET v_cnt_aceptados         = 0    ;
   LET v_cnt_rechazados        = 0    ;
   LET v_tpo_envio             = ""   ;
   LET v_cve_ent_financiera    = 0    ;
   LET v_nss                   = ""   ;
   LET v_num_ctrl_ef           = ""   ;
   LET v_f_envio               = ""   ;
   LET v_tpo_credito           = ""   ;
   LET v_imp_solic_uti_ocg     = 0    ;
   LET v_f_venc_imp_solic      = NULL ;
   LET v_id_dh                 = NULL ;
   LET v_tpo_trabajador        = NULL ;
   LET v_ind_edo_cuenta        = NULL ;
   LET v_f_proceso             = TODAY;
   LET v_inconsistencia        = NULL ;
   LET v_nss_unificado         = ""   ;
   LET v_nss_unificador        = ""   ;
   LET v_id_dh_unificador      = ""   ;
   LET v_diag                  = 0    ;
   LET v_id_ocg_formalizacion  = NULL ;
   LET v_id_ocg_tramite        = NULL ;
   LET v_ax_cve_ent_financiera = 0    ;
   LET v_ax_edo                = NULL ;
   LET v_cnt_cve_ef            = 0    ;
   LET v_f_registro_carta      = NULL ;
   LET bnd_rel_laboral         = 0    ;
   LET v_nss_rl                = ""   ;
   LET v_f_alta                = ""   ;
   LET v_f_baja_rl             = NULL ;
   LET v_f_alta_rl             = NULL ;
   

   UPDATE safre_tmp:tmp_sol_sp3
      SET f_venc_imp_solic = NULL
    WHERE subproceso = v_subproceso
      And f_venc_imp_solic = ""; 

   -- Recupera el registro individual para la solicitud
   SELECT tpo_envio,
          cve_ent_financiera ,
          nss                ,
          num_ctrl_ef        ,
          f_envio[5,6] ||"/"|| f_envio[7,8] ||"/"|| f_envio[1,4],
          cred_convenidos    ,
          imp_solic_uti_ocg  ,
          f_venc_imp_solic[5,6] ||"/"|| f_venc_imp_solic[7,8] ||"/"|| f_venc_imp_solic[1,4],
          solic_saldo        
     INTO v_tpo_envio          ,
          v_cve_ent_financiera ,
          v_nss                ,
          v_num_ctrl_ef        ,
          v_f_envio            ,
          v_tpo_credito        ,
          v_imp_solic_uti_ocg  ,
          v_f_venc_imp_solic   ,
          v_solic_saldo        
     FROM safre_tmp:tmp_sol_sp3
    WHERE subproceso = v_subproceso;
    
	  LET v_id_ocg_detalle        = seq_ocg_detalle.nextval ;
	  LET v_id_ocg_solic_ug       = seq_ocg_solic_ug.nextval;
	  LET v_estado                = 20   ;                       -- Vigente
	  LET v_bnd_inconsistencia    = 0    ;                       -- Inicializado sin inconsistencias
	  LET v_situacion             = 10   ;                       -- Recibido
	  LET v_diagnostico           = 01   ;                       -- Inicializado como diagnostico aceptado
	  LET v_ax_f_envio            = v_f_envio;
	  LET v_bnd_rl                = 0    ;

    IF (v_tpo_credito <> "7") AND (v_tpo_credito <> "8") AND (v_tpo_credito <> "C") THEN
       LET v_tpo_credito = "A";
    END IF

    IF (v_tpo_credito = "") OR (v_tpo_credito = " ")THEN
       LET v_tpo_credito = "A";
    END IF

     IF (v_imp_solic_uti_ocg IS NULL) OR (v_imp_solic_uti_ocg < 0) THEN
         LET v_imp_solic_uti_ocg = 0;
     END IF;

     LET v_imp_solic_uti_ocg = v_imp_solic_uti_ocg / 100;

     -- Se valida que el NSS sea correcto #1
     FOR v_idx_nss = 1 TO LENGTH(v_nss)
        IF SUBSTR(v_nss,v_idx_nss,1) NOT MATCHES '[0-9]' THEN
           --TRACE 'El nss no cumple con la valiacón: ' || v_nss;
           --TRACE 'En la posición : ' || v_idx_nss;

           LET v_inconsistencia = "02";
           LET v_bnd_inconsistencia = 1;

           INSERT INTO ocg_inconsistencia
                VALUES( v_id_ocg_solic_ug,
                        v_subproceso,
                        v_inconsistencia,
                        v_f_proceso );
           EXIT FOR;
        END IF
     END FOR;

      -- Se obtiene el id_derechohabiente
      SELECT id_derechohabiente,
             tipo_trabajador,
             ind_estado_cuenta
        INTO v_id_dh,
             v_tpo_trabajador,
             v_ind_edo_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;
      
      -- Se verfica que el derechohabiente exista en la base #2
      IF (v_id_dh < 0) OR (v_id_dh IS NULL) THEN
         LET v_id_dh              = 0;
         LET v_inconsistencia     = "02";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia     = "43";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
 
      -- Se verifica que el nss sea tpo_trabajador = "I" #3
      IF (v_tpo_trabajador <> "I") THEN
   
         LET v_inconsistencia = "01";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica si la cuenta está inhabilitada por unificación #4
      IF (v_ind_edo_cuenta <> 0) THEN

         EXECUTE PROCEDURE fn_busca_nss_unificador (v_nss)
                      INTO v_nss_unificador, v_id_dh_unificador, v_diag;

         IF v_diag = 1 THEN
            LET v_nss_unificado  = v_nss;
            LET v_nss            = v_nss_unificador;
            LET v_id_dh          = v_id_dh_unificador;
            LET v_ind_edo_cuenta = 0;

            INSERT INTO safre_tmp:tmp_ocg_uni
            VALUES(v_nss_unificado,
                   v_nss_unificador,
                   5,
                   TODAY);
         ELSE
            LET v_inconsistencia = "43";  --"03";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF
      END IF
      
      -- Se inserta en la tabla de detalle
      INSERT INTO ocg_detalle
           VALUES( v_id_ocg_detalle,
                   p_id_ocg_ctr_arch,
                   v_id_dh,
                   v_subproceso,
                   v_f_proceso,
                   v_cve_ent_financiera,
                   v_nss );
                   
      -- Se da por hecho que es un crédito vigente porque se valida en el programa OCGE12
      -- que ejecuta esta función, por lo que recupera la inforación de ocg_formalizacion
      
      SELECT id_ocg_formalizacion,
             id_ocg_tramite,
             cve_ent_financiera,
             situacion
        INTO v_id_ocg_formalizacion,
             v_id_ocg_tramite,
             v_ax_cve_ent_financiera,
             v_ax_edo
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_dh
         AND diagnostico        = 1
         AND situacion          IN (55,60,70,80);
         
      -- Valida que la EF exista en el catálogo
      SELECT COUNT(*)
       INTO v_cnt_cve_ef
       FROM cat_entidad_financiera
      WHERE cve_ent_financiera = v_cve_ent_financiera;
   
      IF (v_cnt_cve_ef = 0) THEN

         LET v_inconsistencia = "26";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso ); 
      END IF
      
      -- Se valida que la fecha de envío sea menor o igual a hoy #9
      IF (v_ax_f_envio > TODAY) THEN

         LET v_inconsistencia = "46";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
      
     
      -- Se valida que el estado en formalizacion sea acreditado   ESTADO ACREDITADO   #13
      LET v_aux_ind = 0;
      
      -- Si es diferente a MARCADO EN PROCESAR
      IF (v_ax_edo <> 80) THEN
         LET v_aux_ind = 1; -- Prende la bandera, indicando que no es un acreditado ó en proceso de registro
      END IF
      
      IF (v_aux_ind = 1) THEN
   
         LET v_inconsistencia     = "62";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      IF (v_ax_edo = 130) THEN --  ESTADO CANCELADO    #14
    
         LET v_inconsistencia     = "53";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
            
      -- Se valida que la fecha de vencimiento sea mayor o igual a la fecha de registro de carta #15
      SELECT f_registro_carta
        INTO v_f_registro_carta
        FROM ocg_formalizacion
       WHERE id_ocg_formalizacion = v_id_ocg_formalizacion;

      IF (v_f_venc_imp_solic < v_f_registro_carta) THEN

          LET v_inconsistencia = "46";
          LET v_bnd_inconsistencia = 1;

          INSERT INTO ocg_inconsistencia
               VALUES( v_id_ocg_solic_ug,
                       v_subproceso,
                       v_inconsistencia,
                       v_f_proceso );
      END IF
      
      -- El importe solicitado debe ser menor a 55000  #17
      IF (v_imp_solic_uti_ocg > 55000) THEN

         LET v_inconsistencia = "59";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
                      
      ELIF (v_imp_solic_uti_ocg IS NULL) OR (v_imp_solic_uti_ocg <= 0) THEN

         LET v_inconsistencia = "45";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_solic_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF
      
      -- se valida que el tipo de credito sea solamente "A", "C" ó " " #20
      IF (v_tpo_credito IS NULL) THEN
         SELECT tpo_credito
           INTO v_tpo_credito
           FROM ocg_formalizacion
          WHERE id_derechohabiente = v_id_dh
            AND situacion IN (55,60,70,80);
      END IF
      
      IF (v_tpo_envio <> "E") THEN

        IF -- (v_tpo_credito IS NULL) OR
           (v_tpo_credito = "A")   OR
           (v_tpo_credito = "C") THEN
        ELSE
           LET v_inconsistencia = "60";
           LET v_bnd_inconsistencia = 1;

           INSERT INTO ocg_inconsistencia
                   VALUES( v_id_ocg_solic_ug,
                           v_subproceso,
                           v_inconsistencia,
                           v_f_proceso );
         END IF
      END IF
      
      -- Se valida si existieron inconsistencias
      IF (v_bnd_inconsistencia = 1) THEN
         LET v_situacion       = 40;  -- Rechazo directo, ya que no debe publicarse a la EF por tratarse de un registro de carga especial
         LET v_diagnostico     = 02;  -- Se rechaza
         LET v_cnt_rechazados  = v_cnt_rechazados + 1;
      ELSE
         -- Se valida Relación Laboral para el tipo de solicitu de saldo 3 (Solicitud Uso Garantía)
         IF(v_solic_saldo = 2) THEN 

             -- Validación para no relacion laboral   #16
		         IF bnd_rel_laboral = 0 THEN
		            FOREACH
		               SELECT nss,
		                      (lpad(month(f_alta)+1,2,0))||
		                      "/01/"||
		                      (year(f_alta)),
		                      (f_baja-(day(f_baja)))
		                 INTO v_nss_rl,
		                      v_f_alta,
		                      v_f_baja_rl
		                 FROM safre_tmp:tmp_relacion_laboral
		                WHERE nss = v_nss
		
		               IF (v_f_alta[1,2] = 13) THEN
		                  LET v_f_alta = "01"||"/"||"01"||"/"||((v_f_alta[7,10])+1);
		                  LET v_f_alta_rl = v_f_alta;
		               ELSE
		                  LET v_f_alta_rl = v_f_alta;
		               END IF
		      
		               -- Si tiene relación laboral se prende una bandera
		               IF (v_f_venc_imp_solic BETWEEN v_f_alta_rl AND v_f_baja_rl) THEN
		                  LET v_bnd_rl = 1;
		                  EXIT FOREACH;
		               END IF
		            END FOREACH;
		         END IF
         END IF 
         
         IF (v_bnd_rl = 1) THEN
            LET v_inconsistencia = "44";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_solic_ug,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF;
         
         IF v_bnd_inconsistencia = 1 THEN
            LET v_situacion      = 40;  -- Rechazo directo, ya que no debe publicarse a la EF por tratarse de un registro de carga especial
            LET v_diagnostico    = 02;  -- Se rechaza
            LET v_cnt_rechazados = v_cnt_rechazados + 1;
         ELSE
            LET v_situacion      = 50;
            LET v_cnt_aceptados = v_cnt_aceptados + 1;
         END IF
         
      END IF
      
      -- Se inserta en la tabla ocg_solicitud_uso_garantia
      INSERT INTO ocg_fecha_mig (
                     id_ocg_referencia  ,
                     id_ocg_detalle     ,
                     id_derechohabiente ,
                     f_envio            ,
                     f_carga            ,
                     f_respuesta        ,
                     f_liquida_cofi     ,
                     subproceso         ,
                     f_alta_registro )    
            VALUES ( v_id_ocg_solic_ug ,
                     v_id_ocg_detalle  ,
                     v_id_dh           ,
                     v_f_proceso       ,
                     v_f_proceso       ,
                     ''                ,
                     ''                ,
                     3                 ,
                     v_f_proceso);

      INSERT INTO ocg_solicitud_uso_garantia (
                     id_ocg_solicitud_ug  ,
									   id_ocg_detalle       ,
									   id_ocg_formalizacion ,
									   id_ocg_tramite       ,
									   id_derechohabiente   ,
									   cve_ent_financiera   ,
									   num_ctr_int_ef       ,
									   importe_solicitado   ,
									   f_vencimiento        ,
									   importe_utilizado    ,
									   tpo_credito          ,
									   solicitud_saldo      ,
									   diagnostico          ,
									   estado               ,
									   situacion )
            VALUES ( v_id_ocg_solic_ug      ,
			               v_id_ocg_detalle       ,
			               v_id_ocg_formalizacion ,
			               v_id_ocg_tramite       ,
			               v_id_dh                ,
			               v_cve_ent_financiera   ,
			               v_num_ctrl_ef          ,  
			               v_imp_solic_uti_ocg    , 
			               v_f_venc_imp_solic     ,
			               0                      , 
			               v_tpo_credito          ,
			               v_solic_saldo          ,
			               v_diagnostico          ,
			               v_estado               ,
			               v_situacion
			               ); 
			               
			RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;      
      
END FUNCTION;


