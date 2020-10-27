






CREATE FUNCTION "safreviv".fn_ret_integra_oficios_rojos(p_usuario_cod    CHAR(20),    p_folio DECIMAL(9,0),
                                             p_nombre_archivo VARCHAR(40), p_pid   DECIMAL(9,0),
                                             p_proceso_cod    SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)


-- detalle de la tabla temporal


DEFINE tmp_reg_of_rojos_nss              CHAR(11);
DEFINE tmp_reg_of_rojos_fch_contable     CHAR(10);
DEFINE tmp_reg_of_rojos_imp_pago         CHAR(12);

-- tablas destino
DEFINE registro_of_rojos_nss              CHAR(11);  
DEFINE registro_of_rojos_folio            DECIMAL(9,0);
DEFINE registro_of_rojos_fch_contable     DATE;
DEFINE registro_of_rojos_imp_pago         DECIMAL(14,2);


-- variables de soporte al proceso
DEFINE v_importe                               DECIMAL(14,2);
DEFINE v_anio_cont                             CHAR(4);
DEFINE v_mes_cont                              CHAR(2);
DEFINE v_dia_cont                              CHAR(2);

-- =============================================================================

DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;

-- conteo de rechazos e inserciones
DEFINE v_reg_det_insertados                    INTEGER; -- total de registros de detalle insertados

-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_reg_of_rojos_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/safreviv_int/BD/integra_ssv_siaff.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_importe = 0;

   -- Inicializa las variables temporales
   LET tmp_reg_of_rojos_nss              = NULL;  
   LET tmp_reg_of_rojos_fch_contable     = NULL;  
   LET tmp_reg_of_rojos_imp_pago         = NULL;  

   LET v_anio_cont = NULL;
   LET v_mes_cont  = NULL;
   LET v_dia_cont  = NULL;
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso    = 0;
   LET v_si_resultado       = 0;
   LET isam_err             = 0;
   LET v_c_msj              = 'El proceso finalizó exitosamente.';
   LET tmp_reg_of_rojos_nss = NULL;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;


   LET registro_of_rojos_folio = p_folio;
   
   --- se cuentan los registros de la tabla temporal de detalle
   -- integracion de detalle
   FOREACH
      SELECT nss, fch_contable, imp_pago
      INTO 
           tmp_reg_of_rojos_nss,
           tmp_reg_of_rojos_fch_contable,
           tmp_reg_of_rojos_imp_pago
      FROM
      safre_tmp:tmp_ret_oficios_rojos
   
      LET v_importe = tmp_reg_of_rojos_imp_pago;

      -- se transfieren los datos a registro

      LET registro_of_rojos_folio                       = p_folio;
      LET registro_of_rojos_nss                         = tmp_reg_of_rojos_nss;
      IF tmp_reg_of_rojos_fch_contable IS NOT NULL THEN 
         LET registro_of_rojos_fch_contable = mdy(tmp_reg_of_rojos_fch_contable[4,5],tmp_reg_of_rojos_fch_contable[1,2],tmp_reg_of_rojos_fch_contable[7,10]);
      ELSE 
         LET registro_of_rojos_fch_contable = NULL;
      END IF

      LET registro_of_rojos_imp_pago = v_importe;
      
      -- se inserta en tabla destino
      INSERT INTO ret_oficios_rojos (
          nss, fch_contable, imp_pago, folio
      ) VALUES (
          registro_of_rojos_nss,
          registro_of_rojos_fch_contable,
          registro_of_rojos_imp_pago, 
          registro_of_rojos_folio);
      LET v_reg_det_insertados = v_reg_det_insertados + 1;
   END FOREACH;
   --trace off;
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_oficios_rojos;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_reg_of_rojos_nss;
END FUNCTION;


