






CREATE FUNCTION "safreviv".fn_sep_genera_marca_op27(p_proceso SMALLINT, p_opera_cod SMALLINT, p_usuario CHAR(20))

RETURNING INTEGER,
          INTEGER,
          CHAR(200);
          
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(200);
DEFINE v_folio      DECIMAL(9,0);

DEFINE v_id_det_02_op27             DECIMAL(9,0) ;
DEFINE v_f_proceso                  CHAR(40)     ;
DEFINE v_tipo_registro              CHAR(2)      ;
DEFINE v_contador_servicio          DECIMAL(10,0);
DEFINE v_invadido                   CHAR(11)     ;
DEFINE v_rfc                        CHAR(13)     ;
DEFINE v_curp                       CHAR(18)     ;
DEFINE v_tipo_entidad_admon         CHAR(2)      ;
DEFINE v_cve_entidad_admon          CHAR(3)      ;
DEFINE v_paterno                    CHAR(40)     ;
DEFINE v_materno                    CHAR(40)     ;
DEFINE v_nombre                     CHAR(40)     ;
DEFINE v_f_nacimiento               DATE         ;
DEFINE v_entidad_nacimiento         CHAR(2)      ;
DEFINE v_sexo                       SMALLINT     ;
DEFINE v_nombre_procanase           CHAR(50)     ;
DEFINE v_f_registro                 DATE         ;
DEFINE v_f_marca_infosar            DATE         ;
DEFINE v_diag_confronta             CHAR(2)      ;
DEFINE v_clasifica_separacion       CHAR(1)      ;
DEFINE v_credito_infonavit          CHAR(1)      ;
DEFINE v_resultado_operacion        CHAR(2)      ;
DEFINE v_diagnostico1               CHAR(3)      ;
DEFINE v_diagnostico2               CHAR(3)      ;
DEFINE v_diagnostico3               CHAR(3)      ;
DEFINE v_traspaso_previo            CHAR(2)      ;
DEFINE v_ind_cambio_clasificacion   CHAR(1)      ;
DEFINE v_id_expediente              DECIMAL(9,0) ;
DEFINE v_id_derechohabiente         DECIMAL(9,0) ;
DEFINE v_ind_marca_safre            SMALLINT     ;

DEFINE v_cod_marca                  INTEGER      ;
DEFINE v_diag_marca                 SMALLINT     ;
DEFINE v_estado                     SMALLINT     ;

          
   ON EXCEPTION SET v_sql_error,
                    v_isam_error,
                    v_msg_error
      
      RETURN v_sql_error,
             v_isam_error,
             v_msg_error;
   END EXCEPTION WITH RESUME;

   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = 'Ejecución realizada correctamente';
             
   EXECUTE FUNCTION fn_genera_folio(p_proceso, 
                                    p_opera_cod, 
                                    p_usuario) 
                                    INTO v_folio;

   UPDATE glo_folio
      SET status = 1
    WHERE folio = v_folio;
    
   EXECUTE FUNCTION fn_maquinaria('maq_sep_op27', -- maquinaria operación 27
                                   10, -- Registro marcado
                                   5) -- estado de proceso
                                   INTO v_cod_marca, 
                                        v_diag_marca, 
                                        v_estado;
    
   -- Constantes, algunas para indicar que el proceso es correcto
   LET v_id_expediente   = NULL; -- sin expediente registrado
   LET v_ind_marca_safre = 1; -- Registro marcado
   LET v_diag_marca      = 0; -- Sin error de estado de proceso
    
   INSERT INTO sep_cza_op27
   SELECT v_folio, 
          TODAY, 
          '',
          tipo_registro     ,
          id_servicio       ,
          id_operacion      ,
          tipo_ent_origen   ,
          cve_ent_origen    ,
          tipo_ent_destino  ,
          cve_ent_destino   ,
          sp_cambia_formato_fecha(f_trans_lote),
          consecutivo_dia   ,
          resultado_operacion,
          diagnostico1      ,
          diagnostico2      ,
          diagnostico3      
   FROM safre_tmp:tmp_sep_cza_op27_tmp;
   

   
   FOREACH SELECT tipo_registro           ,
                  contador_servicio       ,
                  invadido                ,
                  rfc                     ,
                  curp                    ,
                  tipo_entidad_admon      ,
                  cve_entidad_admon       ,
                  paterno                 ,
                  materno                 ,
                  nombre                  ,
                  sp_cambia_formato_fecha(f_nacimiento),
                  entidad_nacimiento      ,
                  sexo                    ,
                  nombre_procanase        ,
                  f_registro,
                  sp_cambia_formato_fecha(f_marca_infosar),
                  diag_confronta          ,
                  clasifica_separacion    ,
                  credito_infonavit       ,
                  resultado_operacion     ,
                  diagnostico1            ,
                  diagnostico2            ,
                  diagnostico3            ,
                  traspaso_previo         ,
                  ind_cambio_clasificacion 
             INTO v_tipo_registro           ,
                  v_contador_servicio       ,
                  v_invadido                ,
                  v_rfc                     ,
                  v_curp                    ,
                  v_tipo_entidad_admon      ,
                  v_cve_entidad_admon       ,
                  v_paterno                 ,
                  v_materno                 ,
                  v_nombre                  ,
                  v_f_nacimiento            ,
                  v_entidad_nacimiento      ,
                  v_sexo                    ,
                  v_nombre_procanase        ,
                  v_f_registro              ,
                  v_f_marca_infosar         ,
                  v_diag_confronta          ,
                  v_clasifica_separacion    ,
                  v_credito_infonavit       ,
                  v_resultado_operacion     ,
                  v_diagnostico1            ,
                  v_diagnostico2            ,
                  v_diagnostico3            ,
                  v_traspaso_previo         ,
                  v_ind_cambio_clasificacion
             FROM safre_tmp:tmp_sep_det_02_op27_tmp
             
             
      SELECT FIRST 1 id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_invadido;
       
      -- identificador de operación 27
      SELECT NVL(MAX(id_det_02_op27),0)+1
        INTO v_id_det_02_op27
        FROM sep_det_02_op27;

      LET v_f_proceso = TODAY;
         
      INSERT INTO sep_det_02_op27 (id_det_02_op27           ,
                                   folio                    ,
                                   f_proceso                ,
                                   tipo_registro            ,
                                   contador_servicio        ,
                                   invadido                 ,
                                   rfc                      ,
                                   curp                     ,
                                   tipo_entidad_admon       ,
                                   cve_entidad_admon        ,
                                   paterno                  ,
                                   materno                  ,
                                   nombre                   ,
                                   f_nacimiento             ,
                                   entidad_nacimiento       ,
                                   sexo                     ,
                                   nombre_procanase         ,
                                   f_registro               ,
                                   f_marca_infosar          ,
                                   diag_confronta           ,
                                   clasifica_separacion     ,
                                   credito_infonavit        ,
                                   resultado_operacion      ,
                                   diagnostico1             ,
                                   diagnostico2             ,
                                   diagnostico3             ,
                                   traspaso_previo          ,
                                   ind_cambio_clasificacion ,
                                   id_expediente            ,
                                   id_derechohabiente_invadido,
                                   ind_marca_safre           ,
                                   diag_marca                ,
                                   estado) 
                           VALUES (v_id_det_02_op27           ,
                                   v_folio                    ,
                                   v_f_proceso                ,
                                   v_tipo_registro            ,
                                   v_contador_servicio        ,
                                   v_invadido                 ,
                                   v_rfc                      ,
                                   v_curp                     ,
                                   v_tipo_entidad_admon       ,
                                   v_cve_entidad_admon        ,
                                   v_paterno                  ,
                                   v_materno                  ,
                                   v_nombre                   ,
                                   v_f_nacimiento             ,
                                   v_entidad_nacimiento       ,
                                   v_sexo                     ,
                                   v_nombre_procanase         ,
                                   v_f_registro               ,
                                   v_f_marca_infosar          ,
                                   ''           ,
                                   ''     ,
                                   v_credito_infonavit        ,
                                   '01'      ,
                                   v_diagnostico1             ,
                                   v_diagnostico2             ,
                                   v_diagnostico3             ,
                                   v_traspaso_previo          ,
                                   '' ,
                                   v_id_expediente            ,
                                   v_id_derechohabiente       ,
                                   0 ,--v_ind_marca_safre          , 
                                   v_diag_marca               ,
                                   10 --v_estado               
                                   );
             
   END FOREACH;
   
   INSERT INTO sep_sum_op27
   SELECT v_folio, 
          tipo_registro,
          total_registro_det2,
          total_registro_det3,
          total_registros
   FROM safre_tmp:tmp_sep_sum_op27_tmp;
   
   UPDATE STATISTICS FOR TABLE sep_cza_op27;
   UPDATE STATISTICS FOR TABLE sep_det_02_op27;
   UPDATE STATISTICS FOR TABLE sep_sum_op27;

   RETURN v_sql_error,
          v_isam_error,
          v_msg_error;
          
END FUNCTION;


