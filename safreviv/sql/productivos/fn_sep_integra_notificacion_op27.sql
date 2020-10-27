






CREATE FUNCTION "safreviv".fn_sep_integra_notificacion_op27(p_folio           DECIMAL(9,0),
                                                 p_nombre_archivo  CHAR(40)    ,
                                                 p_usuario         CHAR(20),
                                                 p_proceso_cod SMALLINT)
                             RETURNING INTEGER, INTEGER, INTEGER;

DEFINE v_total_marcados   INTEGER;
DEFINE v_total_rechazados INTEGER;
DEFINE v_total_integrados INTEGER;

-- Registro tmp detalle 02 op27
DEFINE v_t_tipo_registro              CHAR(2) ;
DEFINE v_t_invadido                   CHAR(11);
DEFINE v_t_rfc                        CHAR(13);
DEFINE v_t_curp                       CHAR(18);
DEFINE v_t_cve_entidad_admon          CHAR(3) ;
DEFINE v_t_paterno                    CHAR(40);
DEFINE v_t_materno                    CHAR(40);
DEFINE v_t_nombre                     CHAR(40);
DEFINE v_t_f_nacimiento               DATE    ; -- AHM por definición de SPL CHAR(8) ;
DEFINE v_t_entidad_nacimiento         CHAR(2) ;
DEFINE v_t_sexo                       CHAR(1) ;
DEFINE v_t_nombre_procanase           CHAR(50);
DEFINE v_t_f_registro                 DATE    ; -- AHM por definición de SPL CHAR(8) ;
DEFINE v_t_f_marca_infosar            DATE    ; -- AHM por definición de SPL CHAR(8) ;
DEFINE v_t_credito_infonavit          CHAR(1) ;
DEFINE v_t_traspaso_previo            CHAR(2) ;
DEFINE v_t_f_notificacion			  DATE ;
DEFINE v_t_ent_administradoras		  CHAR(12);

-- Registro detalle 02 op27
DEFINE v_m_id_det_02_op27             DECIMAL(9)   ;
DEFINE v_m_id_det_04_op27             DECIMAL(9)   ;
DEFINE v_m_f_proceso                  CHAR(40)     ;
DEFINE v_m_tipo_registro              CHAR(2)      ;
DEFINE v_m_contador_servicio          DECIMAL(10,0);
DEFINE v_m_invadido                   CHAR(11)     ;
DEFINE v_m_rfc                        CHAR(13)     ;
DEFINE v_m_curp                       CHAR(18)     ;
DEFINE v_m_tipo_entidad_admon         CHAR(2)      ;
DEFINE v_m_cve_entidad_admon          CHAR(3)      ;
DEFINE v_m_paterno                    CHAR(40)     ;
DEFINE v_m_materno                    CHAR(40)     ;
DEFINE v_m_nombre                     CHAR(40)     ;
DEFINE v_m_f_nacimiento               DATE         ;
DEFINE v_m_entidad_nacimiento         CHAR(2)      ;
DEFINE v_m_sexo                       SMALLINT     ;
DEFINE v_m_nombre_procanase           CHAR(50)     ;
DEFINE v_m_f_registro                 DATE         ;
DEFINE v_m_f_marca_infosar            DATE         ;
DEFINE v_m_diag_confronta             CHAR(2)      ;
DEFINE v_m_clasifica_separacion       CHAR(1)      ;
DEFINE v_m_credito_infonavit          CHAR(1)      ;
DEFINE v_m_resultado_operacion        CHAR(2)      ;
DEFINE v_m_diagnostico1               CHAR(3)      ;
DEFINE v_m_diagnostico2               CHAR(3)      ;
DEFINE v_m_diagnostico3               CHAR(3)      ;
DEFINE v_m_traspaso_previo            CHAR(2)      ;
DEFINE v_m_ind_cambio_clasificacion   CHAR(1)      ;
DEFINE v_m_id_expediente              DECIMAL(9,0) ;
DEFINE v_m_id_derechohabiente         DECIMAL(9,0) ;
DEFINE v_m_ind_marca_safre            SMALLINT     ;
DEFINE v_m_diag_marca                 SMALLINT     ;
DEFINE v_m_estado                     SMALLINT     ;
DEFINE v_m_f_notificacion			  DATE 	   	   ;
DEFINE v_m_ent_administradoras		  CHAR(12)     ;

-- Variables generales
DEFINE v_marca                        SMALLINT     ;
DEFINE v_senal                        INTEGER      ;
DEFINE v_fn_marca_cuenta              CHAR(100)    ;   
DEFINE v_cod_rch                      INTEGER      ;
DEFINE v_cod_marca                    INTEGER      ;
DEFINE v_si_dia                       SMALLINT     ;
DEFINE v_si_mes                       SMALLINT     ;
DEFINE v_si_ano                       SMALLINT     ;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_sep_integra_notificacion_op27.trace';
   --TRACE ON;

   LET v_total_marcados   = 0;
   LET v_total_rechazados = 0;
   LET v_total_integrados = 0;
   LET v_m_contador_servicio = 1; --Contador servicio será interno solo valores nones

   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio = p_folio
    WHERE nombre_archivo = p_nombre_archivo;

   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;

   UPDATE bat_ctr_operacion
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    nom_archivo = p_nombre_archivo;
   
   --TRACE 'Inicia Insert en sep_cza_op27';
   INSERT INTO safre_viv:sep_cza_op27
   SELECT p_folio, 
          TODAY, 
          p_nombre_archivo,
          tipo_registro     ,
          id_servicio       ,
          id_operacion      ,
          tipo_ent_origen   ,
          cve_ent_origen    ,
          tipo_ent_destino  ,
          cve_ent_destino   ,
		  MDY(f_trans_lote[5,6],f_trans_lote[7,8],f_trans_lote[1,4]),
          --safre_viv:sp_cambia_formato_fecha(f_trans_lote[5,6],f_trans_lote[7,8],f_trans_lote[1,4]),
		  --LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);
          consecutivo_dia   ,
          resultado_operacion,
          diagnostico1      ,
          diagnostico2      ,
          diagnostico3      
   FROM safre_tmp:tmp_sep_cza_op27;
   --TRACE 'Inserto en sep_cza_op27';

   FOREACH SELECT cve_afore			      ,
                  invadido                ,
                  rfc                     ,
                  curp                    ,
                  paterno                 ,
                  materno                 ,
                  nombre                  ,
                  MDY(f_nacimiento[5,6],f_nacimiento[7,8],f_nacimiento[1,4]),
				  --safre_viv:sp_cambia_formato_fecha(p_proceso_cod,f_nacimiento),
                  entidad_nacimiento      ,
                  sexo                    ,
                  nombre_procanase        ,
				  MDY(f_registro[5,6],f_registro[7,8],f_registro[1,4]),
				  MDY(f_marca_infosar[5,6],f_marca_infosar[7,8],f_marca_infosar[1,4]),
                  --safre_viv:sp_cambia_formato_fecha(p_proceso_cod,f_registro),
                  --safre_viv:sp_cambia_formato_fecha(p_proceso_cod,f_marca_infosar),
                  credito_infonavit       ,
                  traspaso_previo         ,
				  MDY(f_notificacion[5,6],f_notificacion[7,8],f_notificacion[1,4]),
				  --safre_viv:sp_cambia_formato_fecha(p_proceso_cod,f_notificacion),
				  ent_administradoras
             INTO v_t_cve_entidad_admon       ,
                  v_t_invadido                ,
                  v_t_rfc                     ,
                  v_t_curp                    ,
                  v_t_paterno                 ,
                  v_t_materno                 ,
                  v_t_nombre                  ,
                  v_t_f_nacimiento            ,
                  v_t_entidad_nacimiento      ,
                  v_t_sexo                    ,
                  v_t_nombre_procanase        ,
                  v_t_f_registro              ,
                  v_t_f_marca_infosar         ,
                  v_t_credito_infonavit       ,
                  v_t_traspaso_previo         ,
				  v_t_f_notificacion		  ,
				  v_t_ent_administradoras     
             FROM safre_tmp:tmp_sep_det_02_op27
      
      SELECT id_derechohabiente
        INTO v_m_id_derechohabiente
        FROM safre_viv:afi_derechohabiente
       WHERE nss = v_t_invadido;
      --TRACE 'v_t_invadido: '||v_t_invadido;
      --TRACE 'v_m_id_derechohabiente: '||v_m_id_derechohabiente;

      SELECT FIRST 1 seq_sep_det_02_op27.NEXTVAL
        INTO v_m_id_det_02_op27
        FROM safre_viv:sep_det_02_op27;
      --TRACE 'v_m_id_det_02_op27: '||v_m_id_det_02_op27;

      -- Marcaje
      EXECUTE FUNCTION safre_viv:fn_marca_cuenta(v_m_id_derechohabiente,
                                                 280, -- marca entrada
                                                 v_m_id_det_02_op27,
                                                 p_folio,
                                                 0, -- estado de marca
                                                 0, -- Cod rechazo
                                                 NULL, -- Marca causa
                                                 NULL, -- Fecha causa
                                                 p_usuario,
                                                 p_proceso_cod)
         INTO v_cod_rch;
      --TRACE 'v_cod_rch: '||v_cod_rch;
      
      IF v_cod_rch <> 0 THEN
         LET v_marca = 0;
         LET v_senal = 5;
         INSERT INTO safre_viv:tmp_sep_rch_marca VALUES(v_t_invadido, 280, v_cod_rch, v_m_id_det_02_op27);
         LET v_total_rechazados = v_total_rechazados + 1;
         --TRACE 'INSERT en tmp_sep_rch_marca';
      ELSE
         LET v_total_marcados = v_total_marcados + 1;
         LET v_marca = 1;
         LET v_senal = 10;
      END IF;
        
      -- LET v_m_id_det_02_op27           = ;
      LET v_m_f_proceso                = TODAY; -- TMP AHM por definir
      LET v_m_tipo_registro            = "02";
      LET v_m_invadido                 = v_t_invadido                ;
      LET v_m_rfc                      = v_t_rfc                     ;
      LET v_m_curp                     = v_t_curp                    ;
      LET v_m_tipo_entidad_admon       = "01"; --SIEMPRE ES UNA AFORE
      LET v_m_cve_entidad_admon        = v_t_cve_entidad_admon       ;
      LET v_m_paterno                  = v_t_paterno                 ;
      LET v_m_materno                  = v_t_materno                 ;
      LET v_m_nombre                   = v_t_nombre                  ;
      LET v_m_f_nacimiento             = v_t_f_nacimiento            ;
      LET v_m_entidad_nacimiento       = v_t_entidad_nacimiento      ;
      --TRACE 'v_t_sexo'||v_t_sexo;
      LET v_m_sexo                     = v_t_sexo                    ;
      --TRACE 'v_t_nombre_procanase '||v_t_nombre_procanase;
      LET v_m_nombre_procanase         = v_t_nombre_procanase        ;
      --TRACE 'v_t_f_registro '||v_t_f_registro;
      LET v_m_f_registro               = v_t_f_registro              ;
      --TRACE 'v_t_f_marca_infosar '||v_t_f_marca_infosar;
      LET v_m_f_marca_infosar          = v_t_f_marca_infosar         ;
      --TRACE 'v_t_diag_confronta '||v_t_diag_confronta;
      LET v_m_diag_confronta           = "";
      --TRACE 'v_t_clasifica_separacion '||v_t_clasifica_separacion;
      LET v_m_clasifica_separacion     = "";
      LET v_m_credito_infonavit        = v_t_credito_infonavit       ;
      LET v_m_resultado_operacion      = "01";
      LET v_m_diagnostico1             = "";
      LET v_m_diagnostico2             = "";
      LET v_m_diagnostico3             = "";
      LET v_m_traspaso_previo          = v_t_traspaso_previo         ;
      LET v_m_ind_cambio_clasificacion = "";
      LET v_m_id_expediente            = NULL; -- Constante
	  --DETALLE04
	  LET v_m_f_notificacion           = v_t_f_notificacion			 ;
	  LET v_m_ent_administradoras	   = v_t_ent_administradoras	 ;
      
      -- LET v_m_id_derechohabiente       = ;
      SELECT NVL(id_derechohabiente,0)
        INTO v_m_id_derechohabiente
        FROM safre_viv:afi_derechohabiente
       WHERE nss = v_t_invadido;
      
      LET v_m_ind_marca_safre          = v_marca; --Constante
      
      EXECUTE FUNCTION safre_viv:fn_maquinaria('maq_sep_op27',
                                                v_senal, -- marca entrada
                                                5)
         INTO v_cod_marca, v_m_diag_marca, v_m_estado;
     
      IF v_cod_rch <> 0 THEN
         LET v_m_diag_marca = v_cod_rch;
      END IF
    
      --TRACE 'v_cod_marca: '||v_cod_marca;
      --TRACE 'v_m_diag_marca: '||v_m_diag_marca;
      --TRACE 'v_m_estado: '||v_m_estado;

      --TRACE 'Signacion completa'; 
      
      INSERT INTO safre_viv:sep_det_02_op27 (id_det_02_op27           ,
											 folio					  ,
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
                                             estado 
                                             ) 
                                     VALUES (v_m_id_det_02_op27           ,
											 p_folio					  ,
                                             v_m_f_proceso                ,
                                             v_m_tipo_registro            ,
                                             v_m_contador_servicio        ,
                                             v_m_invadido                 ,
                                             v_m_rfc                      ,
                                             v_m_curp                     ,
                                             v_m_tipo_entidad_admon       ,
                                             v_m_cve_entidad_admon        ,
                                             v_m_paterno                  ,
                                             v_m_materno                  ,
                                             v_m_nombre                   ,
                                             v_m_f_nacimiento             ,
                                             v_m_entidad_nacimiento       ,
                                             v_m_sexo                     ,
                                             v_m_nombre_procanase         ,
                                             v_m_f_registro               ,
                                             v_m_f_marca_infosar          ,
                                             v_m_diag_confronta           ,
                                             v_m_clasifica_separacion     ,
                                             v_m_credito_infonavit        ,
                                             v_m_resultado_operacion      ,
                                             v_m_diagnostico1             ,
                                             v_m_diagnostico2             ,
                                             v_m_diagnostico3             ,
                                             v_m_traspaso_previo          ,
                                             v_m_ind_cambio_clasificacion ,
                                             v_m_id_expediente            ,
                                             v_m_id_derechohabiente       ,
                                             v_m_ind_marca_safre          , 
                                             v_m_diag_marca               ,
                                             v_m_estado
                                             );
	  --INSERTA EN DETALLE04 los nuevos valores de Layout
	  SELECT NVL(MAX(id_det_04_op27),0) + 1 
	     INTO v_m_id_det_04_op27 
		 FROM safre_viv:sep_det_04_op27;
	  INSERT INTO safre_viv:sep_det_04_op27(id_det_04_op27,
								  id_det_02_op27,
								  f_notificacion,
								  ent_administradoras)
			 VALUES(v_m_id_det_04_op27, 
					v_m_id_det_02_op27,
					v_m_f_notificacion,
					v_m_ent_administradoras);
      --TRACE 'Insert en sep_det_02_op27';
      LET v_total_integrados = v_total_integrados + 1;
	  LET v_m_contador_servicio = v_m_contador_servicio + 2; --Cuenta interna
   END FOREACH;

   --TRACE 'Inicia Insert en sep_sum_op27';
   INSERT INTO safre_viv:sep_sum_op27
   SELECT p_folio, 
          tipo_registro,
          total_registro_det2,
		  0,
          total_registros
   FROM safre_tmp:tmp_sep_sum_op27;
   --TRACE 'Inserto en sep_sum_op27';

   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio = p_folio
    WHERE nombre_archivo = p_nombre_archivo;
    
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
   
   UPDATE STATISTICS FOR TABLE safre_viv:sep_cza_op27;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_det_02_op27;
   --UPDATE STATISTICS FOR TABLE safre_viv:sep_det_03_op27;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_sum_op27;
   UPDATE STATISTICS FOR TABLE safre_viv:sep_det_04_op27;
   --TRACE 'Fin de SPL';
   
   RETURN v_total_marcados, v_total_rechazados, v_total_integrados;

END FUNCTION;


