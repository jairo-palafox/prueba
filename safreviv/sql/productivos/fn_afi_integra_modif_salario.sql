






CREATE FUNCTION "safreviv".fn_afi_integra_modif_salario(p_usuario_cod CHAR(20)   , p_folio DECIMAL(10), 
                                    p_nombre_archivo CHAR(18), p_pid DECIMAL(9,0),
                                    p_proceso_cod SMALLINT)
   RETURNING INTEGER, INTEGER, VARCHAR(255)
  
-- campos de tmp_afi_modif_salario
DEFINE tmp_afi_modif_salario_tpo_movimiento      char(2)     ;
DEFINE tmp_afi_modif_salario_espacios            char(2)     ;
DEFINE tmp_afi_modif_salario_nrp                 char(11)    ;
DEFINE tmp_afi_modif_salario_f_movimiento        char(8)     ;
DEFINE tmp_afi_modif_salario_curp_rfc            char(18)    ;
DEFINE tmp_afi_modif_salario_t_trabajador        decimal(1,0);
DEFINE tmp_afi_modif_salario_nss                 char(11)    ;
DEFINE tmp_afi_modif_salario_nombre              char(50)    ;
DEFINE tmp_afi_modif_salario_presentacion_extemp decimal(1,0);
DEFINE tmp_afi_modif_salario_jornada_semana      decimal(1,0);
DEFINE tmp_afi_modif_salario_sdi                 decimal(6,0);
DEFINE tmp_afi_modif_salario_sexo                decimal(1,0);
DEFINE tmp_afi_modif_salario_nss_correcto        char(11)    ;
DEFINE tmp_afi_modif_salario_nombre_correcto     char(50)    ;

-- campos de la tabla afi_relacion_laboral
DEFINE afi_relacion_laboral_id_derechohabiente decimal(9,0);
DEFINE afi_relacion_laboral_nrp                char(11)    ;
DEFINE afi_relacion_laboral_f_alta_nrp         date        ;
DEFINE afi_relacion_laboral_ind_relacion       smallint    ;
DEFINE afi_relacion_laboral_folio_lote         decimal(9,0);
DEFINE afi_relacion_laboral_f_actualiza        date        ;
DEFINE afi_relacion_laboral_usuario            char(20)    ;
  
DEFINE v_rfc                  VARCHAR(13);
DEFINE v_curp                 VARCHAR(18);
DEFINE v_rfc_curp             VARCHAR(18);
                                                     
-- Control de Excepciones
DEFINE v_i_resultado             SMALLINT;
DEFINE sql_err                   INTEGER;
DEFINE isam_err                  INTEGER;
DEFINE err_txt                   VARCHAR(255);
-- Variables de validaciones
DEFINE v_d_id_referencia                 DECIMAL(9,0);
DEFINE v_id_derechohabiente              DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado    DECIMAL(9,0);
DEFINE v_diagnostico_unificador          SMALLINT;
DEFINE v_diagnostico_unificadas          SMALLINT;
DEFINE v_diagnostico_rechazo             SMALLINT;
DEFINE v_estado_familia_unificador       SMALLINT;
DEFINE v_estado_familia_unificado        SMALLINT;

-- numero de cambios realizados
DEFINE v_num_cambios_realizados          INTEGER;
DEFINE v_num_altas_rechazadas            INTEGER;

-- Variable para marca de cuenta
DEFINE v_i_estado_marca                  INTEGER;

   -- se indica que hacer en caso de ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION

   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado      = 0;
   LET v_d_id_referencia  = 0;
   LET sql_err            = 0;
   LET isam_err           = 0;
   LET err_txt            = "El proceso de integración de cambio de salario finalizó correctamente.";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_fn_afi_integra_modif_salario.trace";


   -- se inicia el contador de cambios realizados
   LET v_num_cambios_realizados  = 0;

   --------------------------- CAMBIO DE NOMBRE ----------------------------- 
   --trace "Se leen los cambios de nombre";
   FOREACH SELECT
      tpo_movimiento      ,
      espacios            ,
      nrp                 ,
      f_movimiento        ,
      curp_rfc            ,
      t_trabajador        ,
      nss                 ,
      nombre              ,
      presentacion_extemp ,
      jornada_semana      ,
      sdi                 ,
      sexo                ,
      nss_correcto        ,
      nombre_correcto     
   INTO 
      tmp_afi_modif_salario_tpo_movimiento      ,
      tmp_afi_modif_salario_espacios            ,
      tmp_afi_modif_salario_nrp                 ,
      tmp_afi_modif_salario_f_movimiento        ,
      tmp_afi_modif_salario_curp_rfc            ,
      tmp_afi_modif_salario_t_trabajador        ,
      tmp_afi_modif_salario_nss                 ,
      tmp_afi_modif_salario_nombre              ,
      tmp_afi_modif_salario_presentacion_extemp ,
      tmp_afi_modif_salario_jornada_semana      ,
      tmp_afi_modif_salario_sdi                 ,
      tmp_afi_modif_salario_sexo                ,
      tmp_afi_modif_salario_nss_correcto        ,
      tmp_afi_modif_salario_nombre_correcto     
   FROM safre_tmp:tmp_afi_cambio_nombre

      -- se cambia el nombre IMSS con el nombre   
      
      LET v_num_cambios_realizados = v_num_cambios_realizados + 1;
      
   END FOREACH;
           
   --trace "Finaliza el proceso";
   
   UPDATE STATISTICS FOR TABLE afi_derechohabiente;
   UPDATE STATISTICS FOR TABLE afi_relacion_laboral;
  
   -- se indica cuantas cuentas se abrieron
   LET err_txt = "El proceso de integración de movimientos afilitarios de cambio de salario finalizó correctamente.\Cambios realizados: " || v_num_cambios_realizados;
  
   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_i_resultado, isam_err, err_txt;
END FUNCTION
;


