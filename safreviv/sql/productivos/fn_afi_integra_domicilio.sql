






CREATE FUNCTION "safreviv".fn_afi_integra_domicilio(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                         p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                         p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), INTEGER, INTEGER, INTEGER, VARCHAR(11)

-- campos de la tabla temporal
-- tmp_det_domicilio
DEFINE tmp_det_dom_tipo_registro  char(2) ;
DEFINE tmp_det_dom_nss            char(11);
DEFINE tmp_det_dom_tipo_domicilio char(1) ;
DEFINE tmp_det_dom_calle          char(60);
DEFINE tmp_det_dom_num_exterior   char(25);
DEFINE tmp_det_dom_num_interior   char(25);
DEFINE tmp_det_dom_colonia        char(50);
DEFINE tmp_det_dom_cp             char(5) ;
DEFINE tmp_det_dom_entre_calle1   char(40);
DEFINE tmp_det_dom_entre_calle2   char(40);

-- campos de la tabla destino
-- afi_domicilio
DEFINE afi_dom_id_derechohabiente decimal(9,0);
DEFINE afi_dom_id_domicilio       smallint    ;
DEFINE afi_dom_tpo_domicilio      char(1)     ;
DEFINE afi_dom_ind_envio          char(1)     ;
DEFINE afi_dom_calle              char(40)    ;
DEFINE afi_dom_num_exterior       char(25)    ;
DEFINE afi_dom_num_interior       char(25)    ;
DEFINE afi_dom_colonia            char(50)    ;
DEFINE afi_dom_cp                 char(5)     ;
DEFINE afi_dom_entre_calle1       char(40)    ;
DEFINE afi_dom_entre_calle2       char(40)    ;
DEFINE afi_dom_folio_lote         decimal(9)  ;
DEFINE afi_dom_f_actualiza        date        ;
DEFINE afi_dom_usuario            char(20)    ;

-- campos de la tabla de rechazos
-- afi_domicilio_rch
DEFINE afi_dom_rch_tipo_registro  char(2)     ;
DEFINE afi_dom_rch_nss            char(11)    ;
DEFINE afi_dom_rch_tipo_domicilio char(1)     ;
DEFINE afi_dom_rch_calle          char(60)    ;
DEFINE afi_dom_rch_num_exterior   char(25)    ;
DEFINE afi_dom_rch_num_interior   char(25)    ;
DEFINE afi_dom_rch_colonia        char(50)    ;
DEFINE afi_dom_rch_cp             char(5)     ;
DEFINE afi_dom_rch_entre_calle1   char(40)    ;
DEFINE afi_dom_rch_entre_calle2   char(40)    ;
DEFINE afi_dom_rch_folio_lote     decimal(9,0);
DEFINE afi_dom_rch_cod_rechazo    smallint    ;


-- para control de excepciones  
DEFINE v_error_code      INTEGER;
DEFINE v_error_isam      INTEGER;
DEFINE v_mensaje         VARCHAR(255);
DEFINE v_regs_aceptados  INTEGER;
DEFINE v_regs_rechazados INTEGER;
DEFINE v_regs_totales    INTEGER;
DEFINE v_nss_error       VARCHAR(11); -- nss que genera el error

   -- en caso de ocurrir una excepcion
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
   
      RETURN v_error_code, v_error_isam, v_mensaje, v_regs_totales, v_regs_aceptados, v_regs_rechazados, v_nss_error;
   END EXCEPTION
   
   -- se asume que no hay errores
   LET v_error_code = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Finalizado correctamente.";
   LET v_nss_error  = NULL; -- se asume que no hay error

   -- se inician los contadores
   LET v_regs_aceptados  = 0;
   LET v_regs_rechazados = 0;
   LET v_regs_totales    = 0;

   SET PDQPRIORITY HIGH;

   ---SET INDEXES FOR afi_domicilio DISABLED;

   FOREACH
   SELECT 
      tipo_registro  ,
      nss            ,
      tipo_domicilio ,
      calle          ,
      num_exterior   ,
      num_interior   ,
      colonia        ,
      cp             ,
      entre_calle1   ,
      entre_calle2   
   INTO
      tmp_det_dom_tipo_registro  ,
      tmp_det_dom_nss            ,
      tmp_det_dom_tipo_domicilio ,
      tmp_det_dom_calle          ,
      tmp_det_dom_num_exterior   ,
      tmp_det_dom_num_interior   ,
      tmp_det_dom_colonia        ,
      tmp_det_dom_cp             ,
      tmp_det_dom_entre_calle1   ,
      tmp_det_dom_entre_calle2   
   FROM safre_tmp:tmp_det_domicilio

      -- se asigna el nss en turno
      LET v_nss_error = tmp_det_dom_nss;

      -- se busca el id_derechohabiente
      SELECT id_derechohabiente
      INTO   afi_dom_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_det_dom_nss;
      
      -- si no se encuentra el derechohabiente se rechaza
      IF ( afi_dom_id_derechohabiente IS NULL ) THEN
      
         LET afi_dom_rch_tipo_registro  = tmp_det_dom_tipo_registro ;
         LET afi_dom_rch_nss            = tmp_det_dom_nss           ;
         LET afi_dom_rch_tipo_domicilio = tmp_det_dom_tipo_domicilio;
         LET afi_dom_rch_calle          = tmp_det_dom_calle         ;
         LET afi_dom_rch_num_exterior   = tmp_det_dom_num_exterior  ;
         LET afi_dom_rch_num_interior   = tmp_det_dom_num_interior  ;
         LET afi_dom_rch_colonia        = tmp_det_dom_colonia       ;
         LET afi_dom_rch_cp             = tmp_det_dom_cp            ;
         LET afi_dom_rch_entre_calle1   = tmp_det_dom_entre_calle1  ;
         LET afi_dom_rch_entre_calle2   = tmp_det_dom_entre_calle2  ;
         LET afi_dom_rch_folio_lote     = p_folio;
         LET afi_dom_rch_cod_rechazo    = 1; -- nss no existe

         -- se inserta en la tabla de rechazos
         INSERT INTO afi_domicilio_rch (
            tipo_registro  ,
            nss            ,
            tipo_domicilio ,
            calle          ,
            num_exterior   ,
            num_interior   ,
            colonia        ,
            cp             ,
            entre_calle1   ,
            entre_calle2   ,
            folio_lote     ,
            cod_rechazo    
         ) VALUES (
            afi_dom_rch_tipo_registro  ,
            afi_dom_rch_nss            ,
            afi_dom_rch_tipo_domicilio ,
            afi_dom_rch_calle          ,
            afi_dom_rch_num_exterior   ,
            afi_dom_rch_num_interior   ,
            afi_dom_rch_colonia        ,
            afi_dom_rch_cp             ,
            afi_dom_rch_entre_calle1   ,
            afi_dom_rch_entre_calle2   ,
            afi_dom_rch_folio_lote     ,
            afi_dom_rch_cod_rechazo             
         );
      
         -- se cuenta un registro rechazado
         LET v_regs_rechazados = v_regs_rechazados + 1;
         CONTINUE FOREACH;
      END IF
      
      -- se obtiene el maximo id_domicilio del derechohabiente
      SELECT MAX(id_domicilio)
      INTO   afi_dom_id_domicilio
      FROM   afi_domicilio
      WHERE  id_derechohabiente = afi_dom_id_derechohabiente;
      
      -- si es el primer domicilio, le toca el 1
      IF ( afi_dom_id_domicilio IS NULL ) THEN
         LET afi_dom_id_domicilio = 1;
      ELSE
         LET afi_dom_id_domicilio = afi_dom_id_domicilio + 1;
      END IF

      LET afi_dom_tpo_domicilio      = tmp_det_dom_tipo_domicilio;
      LET afi_dom_ind_envio          = "1"; 
      LET afi_dom_calle              = tmp_det_dom_calle;
      LET afi_dom_num_exterior       = tmp_det_dom_num_exterior;
      LET afi_dom_num_interior       = tmp_det_dom_num_interior;
      LET afi_dom_colonia            = tmp_det_dom_colonia;
      LET afi_dom_cp                 = tmp_det_dom_cp;
      LET afi_dom_entre_calle1       = tmp_det_dom_entre_calle1;
      LET afi_dom_entre_calle2       = tmp_det_dom_entre_calle2;
      LET afi_dom_folio_lote         = p_folio;
      LET afi_dom_f_actualiza        = TODAY;
      LET afi_dom_usuario            = p_usuario_cod;
      
      INSERT INTO afi_domicilio (
         id_derechohabiente ,
         id_domicilio       ,
         tpo_domicilio      ,
         ind_envio          ,
         calle              ,
         num_exterior       ,
         num_interior       ,
         colonia            ,
         cp                 ,
         entre_calle1       ,
         entre_calle2       ,
         folio_lote         ,
         f_actualiza        ,
         usuario            
      ) VALUES (
         afi_dom_id_derechohabiente ,
         afi_dom_id_domicilio       ,
         afi_dom_tpo_domicilio      ,
         afi_dom_ind_envio          ,
         afi_dom_calle              ,
         afi_dom_num_exterior       ,
         afi_dom_num_interior       ,
         afi_dom_colonia            ,
         afi_dom_cp                 ,
         afi_dom_entre_calle1       ,
         afi_dom_entre_calle2       ,
         afi_dom_folio_lote         ,
         afi_dom_f_actualiza        ,
         afi_dom_usuario                  
      );
   
      -- se cuenta un registro aceptado
      LET v_regs_aceptados = v_regs_aceptados + 1;

   END FOREACH

   ---SET INDEXES FOR afi_domicilio ENABLED;
   SET PDQPRIORITY DEFAULT;

   -- se actualizan las estadisticas de la tabla cargada
   UPDATE STATISTICS FOR TABLE afi_domicilio;
   UPDATE STATISTICS FOR TABLE afi_domicilio_rch;
   
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
     
   -- se cuentan los registros procesados
   SELECT COUNT(*)
   INTO   v_regs_totales
   FROM   safre_tmp:tmp_det_domicilio;
   
   -- se devuelve el resultado de la integracion
   RETURN v_error_code, v_error_isam, v_mensaje, v_regs_totales, v_regs_aceptados, v_regs_rechazados, v_nss_error;
END FUNCTION
;


