






CREATE FUNCTION "safreviv".fn_ret_integra_det_consulta_cargos(   p_usuario_cod    CHAR(20)
                                              , p_folio          DECIMAL(9,0)
                                              , p_nombre_archivo VARCHAR(40)
                                              , p_pid            DECIMAL(9,0)
                                              , p_proceso_cod    SMALLINT
                                              )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)
   

-- campos de la tabla de detalle de retiros de fondo ahorro (sin filler)
DEFINE tmp_ret_det_nss                         CHAR(11)   ;

-- detalle de la tabla historica/integrada 
DEFINE ret_fondo_ahorro_nss                  CHAR(11)               ;
DEFINE ret_fondo_ahorro_folio                DECIMAL(9,0)           ;
DEFINE v_reg_det_insert_15                   INTEGER;

-- Control de Excepciones
DEFINE v_si_resultado                            INTEGER; --SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION
     
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';
   LET tmp_ret_det_nss = NULL;
   LET v_reg_det_insert_15 = 0;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio  = p_folio,
          estado = 2             -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;
   
   -- se obtienen los datos del detalle
   FOREACH
      SELECT
          nss
      INTO
          tmp_ret_det_nss
      FROM safre_tmp:tmp_ret_det_cargo_ssv  --tabla para almacenar el detalle
      -- se asume que no hay rechazos en el detalle del archivo

      -- se asignan los datos al registro de rechazo de detalle                   
	   LET ret_fondo_ahorro_nss                 = tmp_ret_det_nss                ;
      LET ret_fondo_ahorro_folio               = p_folio                        ;     

      -- se inserta en la tabla historia de detalle de retiro de fondo ahorro
      INSERT INTO ret_det_cargo_ssv_consulta( 
                      nss
                     ,folio
                             )
         VALUES (
                      ret_fondo_ahorro_nss
                     ,ret_fondo_ahorro_folio
                );
       LET v_reg_det_insert_15 = v_reg_det_insert_15 + 1;
   END FOREACH;
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_det_cargo_ssv_consulta;

   -- si no hubo error
   IF ( v_si_resultado = 0 ) THEN 
      -- si se insertaron registro
      IF ( v_reg_det_insert_15 > 0 ) THEN  
         LET isam_err =  v_reg_det_insert_15;
      END IF
   END IF 

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


