






CREATE FUNCTION "safreviv".fn_ret_fahorro_conting_rev_operativo(p_folio DECIMAL(9,0),
                                  p_proceso_cod SMALLINT, p_usuario_cod CHAR(20)
                                  ) 
   RETURNING INTEGER, VARCHAR(100)

-- Control de Excepciones
DEFINE v_si_resultado     SMALLINT;
DEFINE sql_err            INTEGER;
DEFINE isam_err           INTEGER;
DEFINE err_txt            VARCHAR(250);
DEFINE v_c_msj            VARCHAR(250);
DEFINE v_edo_reversado    SMALLINT;

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, err_txt;
   END EXCEPTION

   -- el reverso operativo no hace nada
   LET v_si_resultado = 0;
   LET v_c_msj        = "Reverso operativo finalizado exitosamente.";

   -- se asigna el estado de reversado por proceso operativo a las solicitudes
   LET v_edo_reversado = 200;
   
   -- se cambian las solicitudes a reversadas por reverso operativo
   UPDATE ret_fondo_ahorro
   SET    estado_solicitud = v_edo_reversado
   WHERE  folio = p_folio;
     
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, v_c_msj;
END FUNCTION;


