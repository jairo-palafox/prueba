






CREATE FUNCTION "safreviv".fn_sep_integra_nrp_op29_tmp(p_folio           DECIMAL(9,0),
                                            p_nombre_archivo  CHAR(40))
RETURNING INTEGER, INTEGER, CHAR(254),INTEGER, INTEGER, INTEGER;

-- variables para control de errores sql
DEFINE v_sql_error        INTEGER  ;
DEFINE v_isam_error       INTEGER  ;
DEFINE v_msg_error        CHAR(254);

DEFINE v_total_rechazados INTEGER;
DEFINE v_total_integrados INTEGER;
DEFINE v_total_aceptadas  INTEGER;

-- Registro tmp detalle 02 op29
DEFINE v_t_det02_tipo_registro             char(2) ;
DEFINE v_t_det02_contador_servicio         char(10);
DEFINE v_t_det02_invadido                  char(11);
DEFINE v_t_det02_resultado_operacion       char(2) ;
DEFINE v_t_det02_diagnostico               char(9) ;

-- Registro detalle 02 op29
DEFINE v_m_det02_id_det_02_op29                decimal(9,0);
DEFINE v_m_det02_id_det_02_op28                char(18)    ;
DEFINE v_m_det02_folio                         decimal(9,0);
DEFINE v_m_det02_f_proceso                     date        ;
DEFINE v_m_det02_tipo_registro                 char(2)     ;
DEFINE v_m_det02_contador_servicio             integer     ;
DEFINE v_m_det02_invadido                      char(11)    ;
DEFINE v_m_det02_id_derechohabiente_invadido   decimal(9,0);
DEFINE v_m_det02_resultado_operacion           char(2)     ;
DEFINE v_m_det02_diagnostico                   char(15)    ;
DEFINE v_m_det02_estado                        smallint    ;

-- Registro tmp detalle 03 op29
DEFINE v_t_det03_tipo_registro             char(2) ;
DEFINE v_t_det03_contador_servicio         char(10);
DEFINE v_t_det03_asociado                  char(11);
DEFINE v_t_det03_resultado_operacion       char(2) ;
DEFINE v_t_det03_diagnostico               char(9) ;

-- Registro detalle 03 op29
DEFINE v_m_det03_id_det_03_op29                char(18)    ;
DEFINE v_m_det03_id_det_02_op29                decimal(9,0);
DEFINE v_m_det03_tipo_registro                 char(2)     ;
DEFINE v_m_det03_contador_servicio             integer     ;
DEFINE v_m_det03_asociado                      char(11)    ;
DEFINE v_m_det03_id_derechohabiente_asociado   decimal(9,0);
DEFINE v_m_det03_resultado_operacion           char(2)     ;
DEFINE v_m_det03_diagnostico                   char(2)     ;

-- Registro tmp detalle 05 op29
DEFINE v_t_det05_tipo_registro             char(2) ;
DEFINE v_t_det05_contador_servicio         char(10);
DEFINE v_t_det05_nrp                       char(11);
DEFINE v_t_det05_resultado_operacion       char(2) ;
DEFINE v_t_det05_diagnostico               char(9) ;


-- Registro detalle 05 op29
DEFINE v_m_det05_id_det_02_op29       decimal(9,0);
DEFINE v_m_det05_tipo_registro        char(2)     ;
DEFINE v_m_det05_contador_servicio    integer     ;
DEFINE v_m_det05_nrp                  char(10)    ;
DEFINE v_m_det05_resultado_operacion  char(2)     ;
DEFINE v_m_det05_diagnostico          char(15)    ;


-- Registro tmp detalle 06 op29
DEFINE v_t_det06_tipo_registro             char(2) ;
DEFINE v_t_det06_contador_servicio         char(10);
DEFINE v_t_det06_nrp                       char(11);
DEFINE v_t_det06_resultado_operacion       char(2) ;
DEFINE v_t_det06_diagnostico               char(9) ;

-- Registro detalle 06 op29
DEFINE v_m_det06_id_det_03_op29       decimal(9,0);
DEFINE v_m_det06_tipo_registro        char(2)     ;
DEFINE v_m_det06_contador_servicio    integer     ;
DEFINE v_m_det06_nrp                  char(10)    ;
DEFINE v_m_det06_resultado_operacion  char(2)     ;
DEFINE v_m_det06_diagnostico          char(15)    ;


-- Variables generales
DEFINE v_marca                        SMALLINT     ;
DEFINE v_senal                        INTEGER      ;
DEFINE v_fn_marca_cuenta              CHAR(100)    ;   
DEFINE v_cod_rch                      INTEGER      ;
DEFINE v_cod_marca                    INTEGER      ;
DEFINE v_m_diag_marca                 CHAR(3)      ;
DEFINE v_si_dia                       SMALLINT     ;
DEFINE v_si_mes                       SMALLINT     ;
DEFINE v_si_ano                       SMALLINT     ;
DEFINE v_estado                       SMALLINT     ;
DEFINE v_m_estado                     SMALLINT     ;
DEFINE v_estado_des                   SMALLINT     ;
DEFINE v_cont_det03                   SMALLINT     ;
DEFINE v_pos_ini_det_05               INTEGER      ;
DEFINE v_pos_ini_det_06               INTEGER      ;
DEFINE v_bandera                      SMALLINT     ;
DEFINE v_pos_det03                    INTEGER      ;
DEFINE v_verifica_repet_op28          SMALLINT; -- bandera para verificar repetidos en operación 28

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_total_integrados = 0;
      LET v_total_aceptadas = 0;
      LET v_total_rechazados = 0;
      RETURN v_sql_error,v_isam_error,v_msg_error,v_total_integrados, v_total_aceptadas, v_total_rechazados;
   END EXCEPTION;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_integra_nrp_op29.trace';
   
   LET v_sql_error        = 0  ;
   LET v_isam_error       = 0  ;
   LET v_msg_error        = "" ;
   LET v_total_rechazados = 0  ;
   LET v_total_integrados = 0  ;
   LET v_total_aceptadas  = 0  ;
   
   INSERT INTO safre_viv:sep_cza_op29
   SELECT p_folio            , 
          TODAY              , 
          p_nombre_archivo   ,
          tipo_registro      ,
          id_servicio        ,
          id_operacion       ,
          tipo_ent_origen    ,
          cve_ent_origen     ,
          tipo_ent_destino   ,
          cve_ent_destino    ,
          safre_viv:sp_cambia_formato_fecha(f_trans_lote),
          --f_trans_lote,
          consecutivo_dia    ,
          resultado_operacion,
          diagnostico        
   FROM safre_tmp:tmp_sep_cza_op29;
   
   FOREACH SELECT tipo_registro      ,
                  contador_servicio  ,
                  invadido           ,
                  resultado_operacion,
                  diagnostico        
             INTO v_t_det02_tipo_registro      ,
                  v_t_det02_contador_servicio  ,
                  v_t_det02_invadido           ,
                  v_t_det02_resultado_operacion,
                  v_t_det02_diagnostico        
             FROM safre_tmp:tmp_sep_det_02_op29

      LET v_m_det02_tipo_registro       = v_t_det02_tipo_registro      ;
      LET v_m_det02_contador_servicio   = v_t_det02_contador_servicio  ;
      LET v_m_det02_invadido            = v_t_det02_invadido           ;
      LET v_m_det02_resultado_operacion = v_t_det02_resultado_operacion;
      LET v_m_det02_diagnostico         = v_t_det02_diagnostico        ;
                                                                        
      LET v_pos_det03 = v_t_det02_contador_servicio + 1;
      LET v_bandera = 1;
      
      WHILE v_bandera = 1 -- se elimina ya que en caso de no existir la pareja de det02, se cicla infinitamente
              SELECT tipo_registro      ,
                     contador_servicio  ,
                     asociado           ,
                     resultado_operacion,
                     diagnostico        
                INTO v_t_det03_tipo_registro      ,
                     v_t_det03_contador_servicio  ,
                     v_t_det03_asociado           ,
                     v_t_det03_resultado_operacion,
                     v_t_det03_diagnostico        
                FROM safre_tmp:tmp_sep_det_03_op29
               WHERE contador_servicio = v_pos_det03;
         
        IF v_t_det03_tipo_registro = '03' THEN
        	  LET v_bandera = 0;
        ELSE
        	  LET v_pos_det03 = v_pos_det03 + 1;
        END IF
      END WHILE;

      LET v_m_det03_tipo_registro       = v_t_det03_tipo_registro      ;
      LET v_m_det03_contador_servicio   = v_t_det03_contador_servicio  ;
      LET v_m_det03_asociado            = v_t_det03_asociado           ;
      LET v_m_det03_resultado_operacion = v_t_det03_resultado_operacion;
      LET v_m_det03_diagnostico         = v_t_det03_diagnostico        ;
      	  
      SELECT id_derechohabiente
        INTO v_m_det02_id_derechohabiente_invadido
        FROM safre_viv:afi_derechohabiente
       WHERE nss = v_m_det02_invadido;
      
      SELECT id_derechohabiente
        INTO v_m_det03_id_derechohabiente_asociado
        FROM safre_viv:afi_derechohabiente
       WHERE nss = v_m_det03_asociado;
      
	    LET v_m_det02_id_det_02_op28 = 0;	    
	    
	    -- Pueden existir varios registros de OP28 para un mismo NSS de OP29, por lo tanto recorre cada id y lo descarta si
	    -- se encuentra en OP29
      FOREACH SELECT id_det_02_op28 , 
                     estado
                INTO v_m_det02_id_det_02_op28,
                     v_estado
                FROM sep_det_02_op28
               WHERE id_derechohabiente_invadido = v_m_det02_id_derechohabiente_invadido
                 AND estado IN (25)

         LET v_verifica_repet_op28 = 0;
         
         -- Verifica si el id_det_02_op28 ya existe en op29
         SELECT FIRST 1 1
           INTO v_verifica_repet_op28
           FROM sep_det_02_op29
          WHERE id_det_02_op28 = v_m_det02_id_det_02_op28;
         -- Si no encuentra id_28 en op29 ligar op8 con op29
         IF(v_verifica_repet_op28 = 0 OR v_verifica_repet_op28 IS NULL)THEN 
            -- Al verificar que el id_28 no existe en 29, termina ciclo y conserva el id_28 para registrar en op29
            -- En el caso donde todos los id_28 están asignados a 29 conserva el último id_28 recuperado y lo asigna a 29
            EXIT FOREACH;
         END IF;         
      END FOREACH
      -- Sólo para el caso donde no existe un op28 para el op29, lo deja id_op28 = 0
	    IF( v_m_det02_id_det_02_op28 IS NULL )THEN
         LET v_m_det02_id_det_02_op28 = 0;
      END IF
      LET v_total_aceptadas = v_total_aceptadas + 1;
      LET v_m_estado = 15; -- No habrá rechazados, sólo aceptados
      
      SELECT seq_sep_det_02_op29.NEXTVAL
        INTO v_m_det02_id_det_02_op29
        FROM systables
       WHERE tabname = "sep_det_02_op29";

      SELECT seq_sep_det_03_op29.NEXTVAL
        INTO v_m_det03_id_det_03_op29
        FROM systables
       WHERE tabname = "sep_det_03_op29";
       
      --LET v_m_det02_id_det_02_op29              = ;
      --LET v_m_det02_id_det_02_op28              = ;
      LET v_m_det02_folio                       = p_folio;
      LET v_m_det02_f_proceso                   = MONTH(TODAY)||'/'||DAY(TODAY)||'/'||YEAR(TODAY);
      LET v_m_det02_tipo_registro               = v_t_det02_tipo_registro      ;
      LET v_m_det02_contador_servicio           = v_t_det02_contador_servicio  ;
      LET v_m_det02_invadido                    = v_t_det02_invadido           ;
      --LET v_m_det02_id_derechohabiente_invadido = ;
      LET v_m_det02_resultado_operacion         = v_t_det02_resultado_operacion;
      LET v_m_det02_diagnostico                 = v_t_det02_diagnostico        ;
      LET v_m_det02_estado                      = v_m_estado;

      --LET v_m_det03_id_det_03_op29              = ;
      LET v_m_det03_id_det_02_op29              = v_m_det02_id_det_02_op29;
      LET v_m_det03_tipo_registro               = v_t_det03_tipo_registro      ;
      LET v_m_det03_contador_servicio           = v_t_det03_contador_servicio  ;
      LET v_m_det03_asociado                    = v_t_det03_asociado           ;
      --LET v_m_det03_id_derechohabiente_asociado = ;
      LET v_m_det03_resultado_operacion         = v_t_det03_resultado_operacion;
      LET v_m_det03_diagnostico                 = v_t_det03_diagnostico        ;
   
      INSERT INTO safre_viv:sep_det_02_op29 (id_det_02_op29             ,
                                             id_det_02_op28             ,
                                             folio                      ,
                                             f_proceso                  ,
                                             tipo_registro              ,
                                             contador_servicio          ,
                                             invadido                   ,
                                             id_derechohabiente_invadido,
                                             resultado_operacion        ,
                                             diagnostico                ,
                                             estado                     
                                             ) 
                                     VALUES (v_m_det02_id_det_02_op29             ,
                                             v_m_det02_id_det_02_op28             ,
                                             v_m_det02_folio                      ,
                                             v_m_det02_f_proceso                  ,
                                             v_m_det02_tipo_registro              ,
                                             v_m_det02_contador_servicio          ,
                                             v_m_det02_invadido                   ,
                                             v_m_det02_id_derechohabiente_invadido,
                                             v_m_det02_resultado_operacion        ,
                                             v_m_det02_diagnostico                ,
                                             v_m_det02_estado                     
                                             );
      
      
      INSERT INTO safre_viv:sep_det_03_op29 (id_det_03_op29                       ,
                                             id_det_02_op29                       ,
                                             tipo_registro                        ,
                                             contador_servicio                    ,
                                             asociado                             ,
                                             id_derechohabiente_asociado          ,
                                             resultado_operacion                  ,
                                             diagnostico                          ) 
                                     VALUES (v_m_det03_id_det_03_op29,
                                             v_m_det03_id_det_02_op29                       ,
                                             v_m_det03_tipo_registro                        ,
                                             v_m_det03_contador_servicio                    ,
                                             v_m_det03_asociado                             ,
                                             v_m_det03_id_derechohabiente_asociado          ,
                                             v_m_det03_resultado_operacion                  ,
                                             v_m_det03_diagnostico                          
                                             );
      
      LET v_pos_ini_det_05 = v_m_det02_contador_servicio + 1;
      LET v_pos_ini_det_06 = v_m_det03_contador_servicio + 1;
      
      -- Detalle 05 op29      
      LET v_bandera = 1;
      WHILE v_bandera = 1 -- se elimina ya que solo realiza un ciclo
         SELECT tipo_registro      ,
                contador_servicio  ,
                nrp                ,
                resultado_operacion,
                diagnostico        
           INTO v_t_det05_tipo_registro      ,
                v_t_det05_contador_servicio  ,
                v_t_det05_nrp                ,
                v_t_det05_resultado_operacion,
                v_t_det05_diagnostico        
      	    FROM safre_tmp:tmp_sep_det_05_op29
      	   WHERE contador_servicio = v_pos_ini_det_05;
      	   
      	   IF v_t_det05_tipo_registro = '05' THEN
             LET v_m_det05_id_det_02_op29       = v_m_det02_id_det_02_op29     ;
             LET v_m_det05_tipo_registro        = v_t_det05_tipo_registro      ;
             LET v_m_det05_contador_servicio    = v_t_det05_contador_servicio  ;
             LET v_m_det05_nrp                  = v_t_det05_nrp                ;
             LET v_m_det05_resultado_operacion  = v_t_det05_resultado_operacion;
             LET v_m_det05_diagnostico          = v_t_det05_diagnostico        ;
             
             INSERT INTO safre_viv:sep_det_05_op29 (id_det_02_op29     ,
                                                    tipo_registro      ,
                                                    contador_servicio  ,
                                                    nrp                ,
                                                    resultado_operacion,
                                                    diagnostico        )
                                            VALUES (v_m_det05_id_det_02_op29     ,
                                                    v_m_det05_tipo_registro      ,
                                                    v_m_det05_contador_servicio  ,
                                                    v_m_det05_nrp                ,
                                                    v_m_det05_resultado_operacion,
                                                    v_m_det05_diagnostico        );
             LET v_pos_ini_det_05 = v_pos_ini_det_05 + 1;
          ELSE
          	  EXIT WHILE;
          END IF;
      END WHILE;

      -- Detalle 06 op29
      
      LET v_bandera = 1;
      WHILE v_bandera = 1
         SELECT tipo_registro      ,
                contador_servicio  ,
                nrp                ,
                resultado_operacion,
                diagnostico        
           INTO v_t_det06_tipo_registro      ,
                v_t_det06_contador_servicio  ,
                v_t_det06_nrp                ,
                v_t_det06_resultado_operacion,
                v_t_det06_diagnostico        
      	    FROM safre_tmp:tmp_sep_det_06_op29
      	   WHERE contador_servicio = v_pos_ini_det_06;
      	   
      	   IF v_t_det06_tipo_registro = '06' THEN
             LET v_m_det06_id_det_03_op29       = v_m_det03_id_det_03_op29     ;
             LET v_m_det06_tipo_registro        = v_t_det06_tipo_registro      ;
             LET v_m_det06_contador_servicio    = v_t_det06_contador_servicio  ;
             LET v_m_det06_nrp                  = v_t_det06_nrp                ;
             LET v_m_det06_resultado_operacion  = v_t_det06_resultado_operacion;
             LET v_m_det06_diagnostico          = v_t_det06_diagnostico        ;
             
             INSERT INTO safre_viv:sep_det_06_op29 (id_det_03_op29     ,
                                                    tipo_registro      ,
                                                    contador_servicio  ,
                                                    nrp                ,
                                                    resultado_operacion,
                                                    diagnostico        )
                                            VALUES (v_m_det06_id_det_03_op29     ,
                                                    v_m_det06_tipo_registro      ,
                                                    v_m_det06_contador_servicio  ,
                                                    v_m_det06_nrp                ,
                                                    v_m_det06_resultado_operacion,
                                                    v_m_det06_diagnostico        );
             LET v_pos_ini_det_06 = v_pos_ini_det_06 + 1;
          ELSE
          	  EXIT WHILE;
          END IF;
          
          
      END WHILE;
         
      LET v_total_integrados = v_total_integrados + 1;
   END FOREACH;

   INSERT INTO safre_viv:sep_sum_op29
   SELECT p_folio, 
          tipo_registro,
          total_registro_det2,
          total_registro_det5,
          total_registro_det3,
          total_registro_det6,
          total_registros
   FROM safre_tmp:tmp_sep_sum_op29;
   
   UPDATE glo_ctr_archivo
      SET estado = 2,
          folio = p_folio
    WHERE nombre_archivo = p_nombre_archivo;
    
   UPDATE glo_folio
      SET status = 1
    WHERE folio = p_folio;
   
   RETURN v_sql_error,
          v_isam_error,
          v_msg_error,
          v_total_integrados, 
          v_total_aceptadas, 
          v_total_rechazados;

END FUNCTION;


