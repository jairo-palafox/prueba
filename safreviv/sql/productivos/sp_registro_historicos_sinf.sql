






CREATE PROCEDURE "safreviv".sp_registro_historicos_sinf(p_folio DECIMAL(9,0),
                                             p_pid DECIMAL(9,0),
                                             p_proceso_cod SMALLINT
                                             )     
   RETURNING SMALLINT, SMALLINT, VARCHAR(255), CHAR(11)

   --Se inicia con la declaracion de variables
   --Tabla: tmp_det_trab_sifv                    
   
    DEFINE v_tmp_pag_det_tpo_registro              CHAR(2)       ;
    DEFINE v_tmp_pag_det_con_registro              DECIMAL(10,0) ;
    DEFINE v_tmp_pag_det_cve_ent_recaudadora       DECIMAL(3,0)  ;
    DEFINE v_tmp_pag_det_reg_patronal              CHAR(11)      ;
    DEFINE v_tmp_pag_det_rfc_patron                CHAR(13)      ;
    DEFINE v_tmp_pag_det_per_pago                  CHAR (6)  ;
    DEFINE v_tmp_pag_det_mes_pago                  CHAR(2)       ;
    DEFINE v_tmp_pag_det_num_doc_pago              DECIMAL(12,0) ;
    DEFINE v_tmp_pag_det_f_pago                    VARCHAR(8)  ;
    DEFINE v_tmp_pag_det_f_proceso                 VARCHAR(8)  ;
    DEFINE v_tmp_pag_det_num_crd_ifv               DECIMAL(10,0) ;
    DEFINE v_tmp_pag_det_cve_concepto              DECIMAL(3,0)  ;
    DEFINE v_tmp_pag_det_nss                       CHAR(11)      ;
    DEFINE v_tmp_pag_det_rfc_trabajador            CHAR(13)      ;
    DEFINE v_tmp_pag_det_curp                      CHAR(18)      ;
    DEFINE v_tmp_pag_det_paterno_trabajador        CHAR(40)      ;
    DEFINE v_tmp_pag_det_materno_trabajador        CHAR(40)      ;
    DEFINE v_tmp_pag_det_nombre_trabajador         CHAR(40)      ;
    DEFINE v_tmp_pag_det_ult_salario_diario        DECIMAL(12,2)  ;
    DEFINE v_tmp_pag_det_imp_ap_pat                DECIMAL(12,2)  ;
    DEFINE v_tmp_pag_det_imp_am_cre_ifv            DECIMAL(12,2)  ;
    DEFINE v_tmp_pag_det_aivs                      DECIMAL(18,2) ;
    DEFINE v_tmp_pag_det_val_intereses             DECIMAL(11,0) ;
    DEFINE v_tmp_pag_det_result_operacion          CHAR(2)       ;

   --Tabla: pag_det_trab_sifv
   DEFINE v_pag_det_trab_sifv_folio                DECIMAL(9,0) ;
   DEFINE v_pag_det_trab_sifv_id_referencia        DECIMAL(9,0) ;
   DEFINE v_pag_det_trab_sifv_id_derechohabiente   DECIMAL(9,0) ;
   DEFINE v_pag_det_trab_sifv_nrp                  CHAR(11)     ;
   DEFINE v_pag_det_trab_sifv_periodo_pago         CHAR(6)      ;
   DEFINE v_pag_det_trab_sifv_folio_sua            DECIMAL(6,0) ;
   DEFINE v_pag_det_trab_sifv_sdi                  DECIMAL(10,2);
   DEFINE v_pag_det_trab_sifv_imp_ap_pat           DECIMAL(12,2);
   DEFINE v_pag_det_trab_sifv_cve_concepto         SMALLINT     ;
   DEFINE v_pag_det_trab_sifv_f_pago               DATE         ;
   DEFINE v_pag_det_trab_sifv_f_proceso            DATE         ;
   DEFINE v_pag_det_trab_sifv_num_crd_trab         DECIMAL(10,0);
   DEFINE v_pag_det_trab_sifv_cve_ent_recauda      CHAR(3)      ;
   DEFINE v_pag_det_trab_sifv_imp_am_crd           DECIMAL(12,2);
   DEFINE v_tipo_trabajador                        CHAR(1);
   DEFINE li_contador                              INTEGER;
   
   --tabla  tmp_cza_sifv
   DEFINE v_tmp_pag_cza_tpo_registro               CHAR(2);
   DEFINE v_tmp_pag_cza_id_proceso                 CHAR(4);
   DEFINE v_tmp_pag_cza_id_operacion               CHAR(2);
   DEFINE v_tmp_pag_cza_f_archivo                  CHAR(8);
   
   --tabla pag_sum_sifv
   DEFINE v_tmp_pag_sum_tpo_registro               CHAR(02)    ;
   DEFINE v_tmp_pag_sum_num_registros              DECIMAL(9,0);
   DEFINE v_tmp_pag_sum_tot_ap_pat                 DECIMAL(14,2);
   DEFINE v_tmp_pag_sum_tot_am_cre                 DECIMAL(14,2);
   DEFINE v_tmp_pag_sum_tot_aiv                    DECIMAL(14,2);

   -- para validar sumario
   DEFINE v_tmp_det_num_registros              DECIMAL(9,0);
   DEFINE v_tmp_det_tot_ap_pat                 DECIMAL(14,2);
   DEFINE v_tmp_det_tot_am_cre                 DECIMAL(14,2);
   DEFINE v_tmp_det_tot_aiv                    DECIMAL(14,2);
   DEFINE v_error_en_sumario                   SMALLINT; -- booleana para saber si hubo error en sumario
   -- Control de Excepciones
   DEFINE sql_err                                  SMALLINT;
   DEFINE isam_err                                 SMALLINT;
   DEFINE err_txt                                  VARCHAR(255);
   DEFINE v_si_resultado                           SMALLINT;   
   --varibales para cambio de importes a decimales 
   DEFINE v_pag_det_ult_salario_diario             DECIMAL(12,2);
   DEFINE v_pag_det_imp_ap_pat                     DECIMAL(12,2);
   DEFINE v_pag_det_imp_am_cre_ifv                 DECIMAL(12,2);
   DEFINE v_pag_det_aivs                           DECIMAL(18,2);
   DEFINE v_pag_det_val_intereses                  DECIMAL(18,2);
         -- para insertar fecha valida en BD
   DEFINE v_d_tmp_pag_cza_f_archivo                DATE;
   DEFINE v_d_tmp_pag_det_f_pago                   DATE;
   DEFINE v_d_tmp_pag_det_f_proceso                DATE;     
   DEFINE v_c_tmp_nombre                           VARCHAR(50); 
   DEFINE v_c_tipo_paton                           CHAR(2)     ;
   DEFINE v_nombre_archivo                         CHAR(40);
   --manejo de excepciones
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_pag_det_nss;
   END EXCEPTION

    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET folio = P_folio,
           estado = 2 -- integrado
     WHERE proceso_cod    = p_proceso_cod
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga

      -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;
      
      -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_proceso 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND pid         = p_pid;
            
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_registro_historicos_sinf.--TRACE';
   --TRACE 'Inicia el store procedure de registro historicos de SOLO INFONAVIT';
   --TRACE "Folio:"||p_folio;
   
   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;
   
   LET v_tmp_pag_det_nss = NULL;
   
   -- se revisa el sumario contra el detalle
   	SELECT tpo_registro      ,
           num_registros     ,
           tot_imp_ap_pat    ,
           tot_am_cre_ifv    ,
           tot_val_intereses
      INTO v_tmp_pag_sum_tpo_registro    , 
           v_tmp_pag_sum_num_registros   , 
           v_tmp_pag_sum_tot_ap_pat      ,
           v_tmp_pag_sum_tot_am_cre      ,
           v_tmp_pag_sum_tot_aiv 
     FROM safre_tmp:tmp_sum_sifv;

     -- se obtienen los totales de detalle     
     SELECT COUNT(*)
     INTO   v_tmp_det_num_registros
     FROM   safre_tmp:tmp_det_trab_sifv;
     
     SELECT SUM(imp_ap_pat)
     INTO   v_tmp_det_tot_ap_pat
     FROM   safre_tmp:tmp_det_trab_sifv;
     
     SELECT SUM(imp_am_cre_ifv)
     INTO   v_tmp_det_tot_am_cre
     FROM   safre_tmp:tmp_det_trab_sifv;
     
     SELECT SUM(aivs)
     INTO   v_tmp_det_tot_aiv
     FROM   safre_tmp:tmp_det_trab_sifv;
     
     -- con alguno que no coincida, es causa de rechazo de lote
     -- NUMERO DE REGISTROS
     IF ( v_tmp_pag_sum_num_registros <> v_tmp_det_num_registros ) THEN
        LET v_si_resultado = 10;
        LET isam_err       = 0;
        LET err_txt        = "Número de registros en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF

     -- IMPORTE DE APORTACION PATRONAL
     IF ( v_tmp_pag_sum_tot_ap_pat <> v_tmp_det_tot_ap_pat ) THEN
        LET v_si_resultado = 20;
        LET isam_err       = 0;
        LET err_txt        = "Importe de aportación patronal en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF
     
     -- AMORTIZACION DEL CREDITO
     IF ( v_tmp_pag_sum_tot_am_cre <> v_tmp_det_tot_am_cre ) THEN
        LET v_si_resultado = 30;
        LET isam_err       = 0;
        LET err_txt        = "Importe de amortización del crédito en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF
     
     -- AIVs
     IF ( v_tmp_pag_sum_tot_aiv <> v_tmp_det_tot_aiv ) THEN
        LET v_si_resultado = 40;
        LET isam_err       = 0;
        LET err_txt        = "Suma de AIVs en detalle y en sumario no coincide";
        
        LET v_error_en_sumario = 1;
     END IF
     
     -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
     IF ( v_error_en_sumario = 1 ) THEN
        -- se actualiza el folio en turno como erroneo
        UPDATE glo_folio
        SET    status = -1
        WHERE  folio = p_folio;

        -- se obtiene el nombre del archivo
        SELECT nombre_archivo
        INTO   v_nombre_archivo
        FROM   glo_ctr_archivo
        WHERE  proceso_cod = p_proceso_cod 
        AND    folio IS NULL;
        
        -- se deja el archivo en estatus de error
        UPDATE glo_ctr_archivo
           SET folio = P_folio,
               estado = 2 -- integrado
         WHERE proceso_cod    = p_proceso_cod
           AND opera_cod      = 1 -- archivo cargado
           AND estado         = 1; -- etapa de carga
        
        -- se devuelve el error
        RETURN v_si_resultado, isam_err, err_txt, v_tmp_pag_det_nss;
     END IF   
   
--   --Reinicia el valor de la SECUENCIA de la tabla pag_det_trabajador del campo: id_referencia
--   ALTER SEQUENCE seq_pag_det_trab_si RESTART 1;
             
   --TRACE 'Inicia el FOREACH tmp_cza_sifv';  
   --se hace el foreach e insert de las tablas de encbezado solo infonavit 
   FOREACH
   	
   --se seleccionan los registros de la tabla temporal de encabezado	
   SELECT tpo_registro ,
          id_proceso   ,
          id_operacion ,
          f_archivo     
     INTO v_tmp_pag_cza_tpo_registro  ,
          v_tmp_pag_cza_id_proceso    ,
          v_tmp_pag_cza_id_operacion  ,
          v_tmp_pag_cza_f_archivo     
     FROM safre_tmp:tmp_cza_sifv  
     
     
     --se le asigan elk fomato soportado por la base de datos
     LET v_d_tmp_pag_cza_f_archivo =   v_tmp_pag_cza_f_archivo[5,6]||"/"||v_tmp_pag_cza_f_archivo[7,8]||"/"||v_tmp_pag_cza_f_archivo[1,4] ;

      --TRACE ("fecha de encabezado=  "||v_d_tmp_pag_cza_f_archivo );
     --se insertan los valores obtenidos de la tabla temporal
     INSERT INTO  pag_cza_sifv
                  (
                    folio             ,
                    tpo_registro      ,
                    ident_proceso     ,
                    ident_operacion   ,
                    f_archivo         
                  )
              VALUES 
                  ( 
                    p_folio                     ,
                    v_tmp_pag_cza_tpo_registro  ,
                    v_tmp_pag_cza_id_proceso    ,
                    v_tmp_pag_cza_id_operacion  ,
                    v_d_tmp_pag_cza_f_archivo     
                  );
   END FOREACH ;

   --se hace el foreach e insert de las tablas de sumario solo infonavit 
   FOREACH   
   	--se seleccionan los registros de la tabla temporal de sumario solo infonavit	 
   	SELECT tpo_registro      ,
           num_registros     ,
           tot_imp_ap_pat    ,
           tot_am_cre_ifv    ,
           tot_val_intereses
      INTO v_tmp_pag_sum_tpo_registro    , 
           v_tmp_pag_sum_num_registros   , 
           v_tmp_pag_sum_tot_ap_pat      ,
           v_tmp_pag_sum_tot_am_cre      ,
           v_tmp_pag_sum_tot_aiv 
     FROM safre_tmp:tmp_sum_sifv

      --se insertan los datos datos obtenidos 
      INSERT INTO pag_sum_sifv
                  (   
                    folio            ,
                    tpo_registro     ,
                    num_registros    ,
                    tot_ap_pat       ,
                    tot_am_cre       ,
                    tot_aiv          
                  )
          VALUES  
                  ( 
                    p_folio                       ,
                    v_tmp_pag_sum_tpo_registro    ,
                    v_tmp_pag_sum_num_registros   ,
                    v_tmp_pag_sum_tot_ap_pat      ,
                    v_tmp_pag_sum_tot_am_cre      ,
                    v_tmp_pag_sum_tot_aiv          
                  );
   END FOREACH;
   
    
   --TRACE 'Inicia el FOREACH tmp_det_trab_sifv';  
                       
   --Se obtienen todos los registros de la tabla tmp_det_trab_sifv
   FOREACH SELECT
                tpo_registro         ,
                con_registro         ,
                cve_ent_recaudadora  ,
                reg_patronal         ,
                rfc_patron           ,
                per_pago             ,
                mes_pago             ,
                num_doc_pago         ,
                f_pago               ,
                f_proceso            ,
                num_crd_ifv          ,
                cve_concepto         ,
                nss                  ,
                rfc_trabajador       ,
                curp                 ,
                paterno_trabajador   ,
                materno_trabajador   ,
                nombre_trabajador    ,
                ult_salario_diario   ,
                imp_ap_pat           ,
                imp_am_cre_ifv       ,
                aivs                 ,
                val_intereses        ,
                result_operacion     
          INTO  v_tmp_pag_det_tpo_registro          ,
                v_tmp_pag_det_con_registro          ,
                v_tmp_pag_det_cve_ent_recaudadora   ,
                v_tmp_pag_det_reg_patronal          ,
                v_tmp_pag_det_rfc_patron            ,
                v_tmp_pag_det_per_pago              ,
                v_tmp_pag_det_mes_pago              ,
                v_tmp_pag_det_num_doc_pago          ,
                v_tmp_pag_det_f_pago                ,
                v_tmp_pag_det_f_proceso             ,
                v_tmp_pag_det_num_crd_ifv           ,
                v_tmp_pag_det_cve_concepto          ,
                v_tmp_pag_det_nss                   ,
                v_tmp_pag_det_rfc_trabajador        ,
                v_tmp_pag_det_curp                  ,
                v_tmp_pag_det_paterno_trabajador    ,
                v_tmp_pag_det_materno_trabajador    ,
                v_tmp_pag_det_nombre_trabajador     ,
                v_tmp_pag_det_ult_salario_diario    ,
                v_tmp_pag_det_imp_ap_pat            ,
                v_tmp_pag_det_imp_am_cre_ifv        ,
                v_tmp_pag_det_aivs                  ,
                v_tmp_pag_det_val_intereses         ,
                v_tmp_pag_det_result_operacion     
          FROM safre_tmp:tmp_det_trab_sifv
       
       --TRACE "Inicia for";
       --Se transforma a decimales
       LET v_pag_det_ult_salario_diario     =  v_tmp_pag_det_ult_salario_diario   / 100;  
       LET v_pag_det_imp_ap_pat             =  v_tmp_pag_det_imp_ap_pat           / 100;  
       LET v_pag_det_imp_am_cre_ifv         =  v_tmp_pag_det_imp_am_cre_ifv       / 100;  
       LET v_pag_det_aivs                   =  v_tmp_pag_det_aivs                 / 100;
       LET v_pag_det_val_intereses          =  v_tmp_pag_det_val_intereses        / 10000;
       
       LET v_d_tmp_pag_det_f_pago           =  v_tmp_pag_det_f_pago[5,6]||"/"||v_tmp_pag_det_f_pago[7,8]||"/"||v_tmp_pag_det_f_pago[1,4] ;
       --LET v_d_tmp_pag_det_f_proceso        =  v_tmp_pag_det_f_proceso[5,6]||"/"||v_tmp_pag_det_f_proceso[7,8]||"/"||v_tmp_pag_det_f_proceso[1,4];
       
       LET v_c_tmp_nombre = TRIM(v_tmp_pag_det_paterno_trabajador)||"$"||TRIM(v_tmp_pag_det_materno_trabajador)||"$"||TRIM(v_tmp_pag_det_nombre_trabajador);
       
       --TRIM(v_tmp_pag_det_nombre_trabajador)||"$"||TRIM(v_tmp_pag_det_paterno_trabajador) ||"$"||TRIM(v_tmp_pag_det_materno_trabajador) ;
       
       --se asigna el tipo de patron con las posiciones 1,2 del nrp 
       LET v_c_tipo_paton = v_tmp_pag_det_reg_patronal ;
       
       --TRACE ("  FECHA E PAGO "||v_d_tmp_pag_det_f_pago );
         
       --Inicializacion de variables     
       LET v_pag_det_trab_sifv_id_derechohabiente = NULL;
       
       --TRACE ("  v_pag_det_trab_sifv_id_derechohabiente"||v_pag_det_trab_sifv_id_derechohabiente );
       --TRACE ("  v_tmp_pag_det_reg_patronal"||v_tmp_pag_det_reg_patronal );
       --Obtiene el identificador del derechohabiente
       SELECT FIRST 1 id_derechohabiente 
         INTO v_pag_det_trab_sifv_id_derechohabiente
         FROM afi_derechohabiente
        WHERE nss = v_tmp_pag_det_nss;
       
       IF (v_tmp_pag_det_reg_patronal[1,2] = "99") THEN
			  	LET v_tipo_trabajador = "S";
       ELSE
          LET v_tipo_trabajador = "I";
       END IF
                  
      --TRACE ("  v_tmp_pag_det_reg_patronal"||v_tmp_pag_det_reg_patronal[1,2] );   
      --TRACE ("  v_tipo_trabajador"||v_tipo_trabajador );   

      
      --TRACE "Tipo trabajador:"||v_tipo_trabajador;
      --Si no existe informacion en la tabla "afi_derechohabiente",
      --entonces se inserta el registro en dicha tabla (aperturar una cuenta)
      IF (v_pag_det_trab_sifv_id_derechohabiente IS NULL) THEN
         --TRACE "Obtiene derechohabiente";
         ----TRACE "Tipo v_tmp_det_trab_sifv_nss:"||v_tmp_det_trab_sifv_nss;
         ----TRACE "Tipo v_tipo_trabajador:"||v_tipo_trabajador;

        --TRACE (" entra fn_apertura_cuenta_pag ="||"  "||v_tmp_pag_det_nss||"  "||v_tipo_trabajador||"  "||p_folio ||"  "||v_tmp_pag_det_curp || v_tmp_pag_det_rfc_trabajador ||v_c_tmp_nombre             );

         LET v_pag_det_trab_sifv_id_derechohabiente = fn_apertura_cuenta_pag(
                                                                v_tmp_pag_det_nss , 
                                                                v_tmp_pag_det_curp ,  
                                                                v_tmp_pag_det_rfc_trabajador,
                                                                "1",
                                                                v_c_tmp_nombre,  --Nombre del trabajador
                                                                v_tipo_trabajador,
                                                                0,
                                                                3,  --solo infonavit = pag_tpo_archivo
                                                                p_folio,
                                                                "R" );
       --TRACE (" v_pag_det_trab_sifv_id_derechohabiente "||v_pag_det_trab_sifv_id_derechohabiente);   

      END IF
      
      --TRACE " obtener secuencia";   
      -- se obtiene el numero de la secuencia
      SELECT seq_cta_his_pagos.NEXTVAL
      --SELECT seq_pag_det_trab_si.NEXTVAL
      INTO v_pag_det_trab_sifv_id_referencia
      FROM systables
      WHERE tabid = 1;
      
      --TRACE " insert cta_his_pagos";   
      

--TRACE " folio              : " || p_folio                            ;   
--TRACE " origen_archivo     : 3                                     ";
--TRACE " id_referencia      : "|| v_pag_det_trab_sifv_id_referencia     ; 
--TRACE " cve_ent_receptora  : "|| v_tmp_pag_det_cve_ent_recaudadora     ;
--TRACE " nrp                : "|| v_tmp_pag_det_reg_patronal            ;
--TRACE " periodo_pago       : "|| v_tmp_pag_det_per_pago                ;
--TRACE " folio_sua          : "|| v_tmp_pag_det_num_doc_pago            ;
--TRACE " f_pago             : "|| v_d_tmp_pag_det_f_pago                ;
--TRACE " id_derechohabiente : "|| v_pag_det_trab_sifv_id_derechohabiente;
--TRACE " localiza_trabajador:  NULL                                      ";
--TRACE " tpo_aclaracion     :  NULL                                      ";
--TRACE " imp_ap_pat         : "|| v_pag_det_imp_ap_pat                ;  
--TRACE " imp_am_cre         : "|| v_pag_det_imp_am_cre_ifv            ;  
--TRACE " imp_ren_viv_pgo_ext:  NULL                                    ";
--TRACE " aiv_ap_pat         : "|| v_pag_det_aivs                        ;
--TRACE " valor_aiv          :  NULL                                  ";
--TRACE " int_gen_pgo_ext    :  NULL                                  ";
--TRACE " aiv_gen_pgo_ext    :  NULL                                  ";
--TRACE " result_operacion   :  NULL                                  ";
--TRACE " ind_liquidacion    :  0";                                    
--TRACE " num_crd_ifv        : "|| v_tmp_pag_det_num_crd_ifv;
--TRACE " f_proceso          : "|| TODAY;            
--TRACE " tpo_patron         : "|| v_c_tipo_paton ;
--TRACE " folio_referencia   :  NULL               ";
      
      -- Inserta a tabla cta_his_pagos para control de historicos
      INSERT INTO cta_his_pagos (
                    folio               ,
                    origen_archivo      ,
                    id_referencia       ,
                    cve_ent_receptora   ,
                    nrp                 ,
                    periodo_pago        ,
                    folio_sua           ,
                    f_pago              ,
                    id_derechohabiente  ,
                    localiza_trabajador ,
                    tpo_aclaracion      ,
                    imp_ap_pat          ,
                    imp_am_cre          ,
                    imp_ren_viv_pgo_ext ,
                    aiv_ap_pat          ,
                    valor_aiv           ,
                    int_gen_pgo_ext     ,
                    aiv_gen_pgo_ext     ,
                    result_operacion    ,
                    ind_liquidacion     ,
                    num_crd_ifv         ,
                    f_proceso           ,
                    tpo_patron          ,
                    folio_referencia     
                   )
           VALUES (
                    p_folio                                   ,
                    3                                         ,  -- origen_archivo=3  
                    v_pag_det_trab_sifv_id_referencia                 ,
                    v_tmp_pag_det_cve_ent_recaudadora         ,
                    v_tmp_pag_det_reg_patronal                ,
                    v_tmp_pag_det_per_pago                    ,
                    0                                         , -- antes v_tmp_pag_det_num_doc_pago
                    v_d_tmp_pag_det_f_pago                    ,
                    v_pag_det_trab_sifv_id_derechohabiente    ,
                    NULL                                      ,
                    NULL                                      ,
                    v_pag_det_imp_ap_pat                      ,
                    v_pag_det_imp_am_cre_ifv                  ,
                    NULL                                      ,
                    v_pag_det_aivs                                      ,
                    NULL                                      ,
                    NULL                                      ,
                    NULL                                      ,
                    NULL                                      ,
                    0,
                    v_tmp_pag_det_num_crd_ifv, 
                    TODAY ,
                    v_c_tipo_paton ,
                    NULL 
                  );
                   
  
  --TRACE " insert cta_pag_complemento";             
  INSERT INTO cta_pag_complemento
  
              (
                folio                ,
                origen_archivo       ,
                id_referencia        ,
                id_derechohabiente   ,
                rfc_patron           ,
                rfc                  ,
                curp                 ,
                num_mov_periodo      ,
                f_ini_desc_crd_ifv   ,
                ult_sdi              ,
                tpo_trabajador       ,
                jornada              ,
                destino_ap_viv       ,
                dias_cot_bim         ,                
                dias_incap_bim       ,
                dias_ausent_bim      ,
                marca_sua            ,
                marca_bdnsar
              )
       VALUES
              (
                p_folio                                 ,
                3                                       ,
                v_pag_det_trab_sifv_id_referencia       ,
                v_pag_det_trab_sifv_id_derechohabiente  ,
                v_tmp_pag_det_rfc_patron                ,
                v_tmp_pag_det_rfc_trabajador            ,
                v_tmp_pag_det_curp                      ,
                v_tmp_pag_det_num_doc_pago              , -- antes NULL
                NULL                                    ,
                v_tmp_pag_det_ult_salario_diario        ,
                NULL                                    ,
                NULL                                    ,
                NULL           ,                         
                NULL           ,                         
                NULL           ,                         
                NULL           ,                         
                NULL           ,                         
                NULL                                     
              );
   END FOREACH;
   


   UPDATE STATISTICS FOR TABLE cta_his_pagos;
   UPDATE STATISTICS FOR TABLE cta_pag_complemento;
   
   --TRACE 'Finaliza el store procedure de registro historicos de SOLO INFONAVIT';

   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integracion de SOLO INFONAVIT terminó correctamente";
   
   RETURN v_si_resultado, isam_err, err_txt, v_tmp_pag_det_nss;
END PROCEDURE;


