






CREATE FUNCTION "safreviv".fn_afi_integra_contacto_electronico(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                                    p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                                    p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), VARCHAR(11)
 
-- campos de la tabla origen
-- tmp_det_contacto_elect
DEFINE tmp_det_ce_tipo_registro      char(2)     ;
DEFINE tmp_det_ce_nss                char(11)    ;
DEFINE tmp_det_ce_tipo_correo        decimal(1,0);
DEFINE tmp_det_ce_correo_electronico char(60)    ;

-- campos de la tabla destino
-- afi_contacto_electronico
DEFINE afi_ce_id_derechohabiente      decimal(9,0);
DEFINE afi_ce_id_contacto_electronico smallint    ;
DEFINE afi_ce_tpo_correo              smallint    ;
DEFINE afi_ce_valor                   char(40)    ;
DEFINE afi_ce_folio_lote              decimal(9)  ;
DEFINE afi_ce_f_actualiza             date        ;
DEFINE afi_ce_usuario                 char(20)    ;

-- tabla de rechazos
-- afi_contacto_electronico_rch
DEFINE afi_ce_rch_tipo_registro      CHAR(2)     ;
DEFINE afi_ce_rch_nss                CHAR(11)    ;
DEFINE afi_ce_rch_tipo_correo        DECIMAL(1,0);
DEFINE afi_ce_rch_correo_electronico CHAR(60)    ;
DEFINE afi_ce_rch_folio_lote         DECIMAL(9,0);
DEFINE afi_ce_rch_cod_rechazo        SMALLINT    ;

-- para control de excepciones  
DEFINE v_error_code INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_mensaje    VARCHAR(255);

DEFINE v_regs_aceptados   INTEGER;
DEFINE v_regs_rechazados  INTEGER;

   -- en caso de error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
   
      RETURN v_error_code, v_error_isam, v_mensaje, tmp_det_ce_nss;
   END EXCEPTION
   
   -- se asume que no hay errores
   LET v_error_code = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Finalizado correctamente.";
   LET tmp_det_ce_nss = NULL;
   LET v_regs_aceptados  = 0;
   LET v_regs_rechazados = 0;
   
   SET PDQPRIORITY HIGH;

   -- se obtienen los datos
   FOREACH
   SELECT
      tipo_registro      ,
      nss                ,
      tipo_correo        ,
      correo_electronico 
   INTO
      tmp_det_ce_tipo_registro      ,
      tmp_det_ce_nss                ,
      tmp_det_ce_tipo_correo        ,
      tmp_det_ce_correo_electronico 
   FROM safre_tmp:tmp_det_contacto_elect
   
      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   afi_ce_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_det_ce_nss;
      
      -- si no existe, se rechaza
      IF ( afi_ce_id_derechohabiente IS NULL ) THEN
         LET v_regs_rechazados = v_regs_rechazados + 1;

         LET afi_ce_rch_tipo_registro      = tmp_det_ce_tipo_registro;
         LET afi_ce_rch_nss                = tmp_det_ce_nss;
         LET afi_ce_rch_tipo_correo        = tmp_det_ce_tipo_correo;
         LET afi_ce_rch_correo_electronico = tmp_det_ce_correo_electronico;
         LET afi_ce_rch_folio_lote         = p_folio;
         LET afi_ce_rch_cod_rechazo        = 1; -- NSS no existe
         
         INSERT INTO afi_contacto_electronico_rch (
            tipo_registro      ,
            nss                ,
            tipo_correo        ,
            correo_electronico ,
            folio_lote         ,
            cod_rechazo        
         ) VALUES (
            afi_ce_rch_tipo_registro      ,
            afi_ce_rch_nss                ,
            afi_ce_rch_tipo_correo        ,
            afi_ce_rch_correo_electronico ,
            afi_ce_rch_folio_lote         ,
            afi_ce_rch_cod_rechazo        
         );
         
         -- se continua con el siguiente registros
         CONTINUE FOREACH;
      END IF
      
      -- se obtiene el id_del contacto
      SELECT MAX(id_contacto_electronico)
      INTO   afi_ce_id_contacto_electronico
      FROM   afi_contacto_electronico
      WHERE  id_derechohabiente = afi_ce_id_derechohabiente;
      
      -- si no se encontro, entonces es el primero
      IF ( afi_ce_id_contacto_electronico IS NULL OR afi_ce_id_contacto_electronico <= 0 ) THEN
         LET afi_ce_id_contacto_electronico = 1;
      ELSE
         -- se incrementa
         LET afi_ce_id_contacto_electronico = afi_ce_id_contacto_electronico + 1;
      END IF
      
      -- se asignan los datos a la tabla destino
      LET afi_ce_tpo_correo   = tmp_det_ce_tipo_correo;
      LET afi_ce_valor        = tmp_det_ce_correo_electronico;
      LET afi_ce_folio_lote   = p_folio;
      LET afi_ce_f_actualiza  = TODAY;
      LET afi_ce_usuario      = p_usuario_cod;

      -- se inserta el correo
      INSERT INTO afi_contacto_electronico (
         id_derechohabiente      ,
         id_contacto_electronico ,
         tpo_correo              ,
         valor                   ,
         folio_lote              ,
         f_actualiza             ,
         usuario                 
      ) VALUES (
         afi_ce_id_derechohabiente      ,
         afi_ce_id_contacto_electronico ,
         afi_ce_tpo_correo              ,
         afi_ce_valor                   ,
         afi_ce_folio_lote              ,
         afi_ce_f_actualiza             ,
         afi_ce_usuario                 
      );
   
   END FOREACH;
   
   SET PDQPRIORITY DEFAULT;
   
   -- se actualizan las estadisticas de la tabla cargada
   UPDATE STATISTICS FOR TABLE afi_contacto_electronico;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
      SET 
       folio = p_folio,
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
   
   -- se devuelve el resultado de la integracion
   RETURN v_error_code, v_error_isam, v_mensaje, tmp_det_ce_nss;
END FUNCTION
;


