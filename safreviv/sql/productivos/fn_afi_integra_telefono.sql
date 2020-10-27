






CREATE FUNCTION "safreviv".fn_afi_integra_telefono(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                        p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                        p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255), VARCHAR(11)
 
-- para control de excepciones  
DEFINE v_error_code  INTEGER;
DEFINE v_error_isam  INTEGER;
DEFINE v_mensaje     VARCHAR(255);
DEFINE v_nss_error   VARCHAR(11); -- NSS que se encontro al ocurrir un error

-- campos de la tabla temporal
-- tmp_det_telefono

DEFINE tmp_det_tel_tipo_registro char(2)     ;
DEFINE tmp_det_tel_nss           char(11)    ;
DEFINE tmp_det_tel_tipo_telefono decimal(1,0);
DEFINE tmp_det_tel_lada          char(3)     ;
DEFINE tmp_det_tel_telefono      char(13)    ;
DEFINE tmp_det_tel_extension     char(10)    ;

-- campos de la tabla de telefono
DEFINE v_id_derechohabiente   DECIMAL(9,0);
DEFINE v_id_telefono          SMALLINT    ; -- contador de id_telefono por derechohabiente
DEFINE v_cve_lada             CHAR(3)     ;
DEFINE v_extension            CHAR(10)    ;
DEFINE v_telefono             CHAR(13)    ;
DEFINE v_tpo_telefono         SMALLINT    ;

-- tabla de rechazos
-- afi_telefono_rch
DEFINE afi_tel_rch_tipo_registro  CHAR(2)     ;
DEFINE afi_tel_rch_nss            CHAR(11)    ;
DEFINE afi_tel_rch_tipo_telefono  DECIMAL(1,0);
DEFINE afi_tel_rch_lada           CHAR(3)     ;
DEFINE afi_tel_rch_telefono       CHAR(13)    ;
DEFINE afi_tel_rch_extension      CHAR(10)    ;
DEFINE afi_tel_rch_folio_lote     DECIMAL(9,0);
DEFINE afi_tel_rch_cod_rechazo    SMALLINT    ;

   -- en caso de ocurrir un error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
   
      RETURN v_error_code, v_error_isam, v_mensaje, v_nss_error;
   END EXCEPTION
   
   -- se asume que no hay errores
   LET v_error_code = 0;
   LET v_error_isam = 0;
   LET v_mensaje    = "Finalizado correctamente.";
   LET v_nss_error  = NULL;
   
   SET PDQPRIORITY HIGH;

   -- se obtienen los numeros telefonicos de los derechohabientes existentes
   FOREACH
   SELECT 
      tipo_registro ,
      nss           ,
      tipo_telefono ,
      lada          ,
      telefono      ,
      extension     
   INTO 
      tmp_det_tel_tipo_registro ,
      tmp_det_tel_nss           ,
      tmp_det_tel_tipo_telefono ,
      tmp_det_tel_lada          ,
      tmp_det_tel_telefono      ,
      tmp_det_tel_extension     
     FROM safre_tmp:tmp_det_telefono 


       -- se obtiene el nss que pudiera haber generado el error
       LET v_nss_error = tmp_det_tel_nss;
       
       -- se obtiene el id_derechohabiente
       SELECT id_derechohabiente
       INTO   v_id_derechohabiente
       FROM   afi_derechohabiente
       WHERE  nss = tmp_det_tel_nss;
       
       -- si no se encontro al derechohabiente
       IF ( v_id_derechohabiente IS NULL ) THEN
          -- se rechaza
          LET afi_tel_rch_tipo_registro  = tmp_det_tel_tipo_registro;
          LET afi_tel_rch_nss            = tmp_det_tel_nss;
          LET afi_tel_rch_tipo_telefono  = tmp_det_tel_tipo_telefono;
          LET afi_tel_rch_lada           = tmp_det_tel_lada;
          LET afi_tel_rch_telefono       = tmp_det_tel_telefono;
          LET afi_tel_rch_extension      = tmp_det_tel_extension;
          LET afi_tel_rch_folio_lote     = p_folio;
          LET afi_tel_rch_cod_rechazo    = 1; -- NSS no existe
                                            
          INSERT INTO afi_telefono_rch (
             tipo_registro  ,
             nss            ,
             tipo_telefono  ,
             lada           ,
             telefono       ,
             extension      ,
             folio_lote     ,
             cod_rechazo              
          ) VALUES (
             afi_tel_rch_tipo_registro  ,
             afi_tel_rch_nss            ,
             afi_tel_rch_tipo_telefono  ,
             afi_tel_rch_lada           ,
             afi_tel_rch_telefono       ,
             afi_tel_rch_extension      ,
             afi_tel_rch_folio_lote     ,
             afi_tel_rch_cod_rechazo    
          );
          
          CONTINUE FOREACH;
       END IF 

       -- se obtiene el siguiente id_telefono
       SELECT MAX(id_telefono)
       INTO   v_id_telefono
       FROM   afi_telefono
       WHERE  id_derechohabiente = v_id_derechohabiente;

       -- si no se tienen telefonos, entonces es el primero
       IF ( v_id_telefono IS NULL ) THEN
          LET v_id_telefono = 1;
       ELSE
          LET v_id_telefono = v_id_telefono + 1;
       END IF

       -- se asignan los datos a los campos
       LET v_cve_lada     = tmp_det_tel_lada;
       LET v_extension    = tmp_det_tel_extension;
       LET v_telefono     = tmp_det_tel_telefono;
       LET v_tpo_telefono = tmp_det_tel_tipo_telefono;

       INSERT INTO afi_telefono (
          id_derechohabiente,
          id_telefono       ,
          cve_lada          ,
          extension         ,
          telefono          ,
          tpo_telefono      ,
          folio_lote        
       )
       VALUES (
          v_id_derechohabiente,
          v_id_telefono       ,
          v_cve_lada          ,
          v_extension         ,
          v_telefono          ,
          v_tpo_telefono      ,
          p_folio        
       );

   END FOREACH;

   SET PDQPRIORITY DEFAULT;
   
   -- se actualizan las estadisticas de la tabla cargada
   UPDATE STATISTICS FOR TABLE afi_telefono;
   
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
   RETURN v_error_code, v_error_isam, v_mensaje, v_nss_error;
END FUNCTION
;


