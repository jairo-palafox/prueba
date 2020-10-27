






CREATE FUNCTION "safreviv".fn_deo_prevalida_op98(p_usuario_cod CHAR(20))
  RETURNING INTEGER, INTEGER, VARCHAR(255)

 DEFINE v_f_valor_devolucion CHAR(10); -- la fecha viene como un char se tiene que convertir
 DEFINE v_cve_afore_archivo  SMALLINT;  -- Clave afore de tabla archivo
 DEFINE v_importe_archivo    DECIMAL(20,2); -- Importe de tabla de archivo
 --
 DEFINE v_importe_capturado  DECIMAL(20,2); -- Importe capturado de saldos.
 --
 DEFINE v_i_procesados       INTEGER; -- Numero de afores comparadas
 DEFINE v_i_resultado                   INTEGER; -- resultado de la operacion
 DEFINE v_dte_f_valor_devolucion     DATE; -- fecha en formato DATE
 DEFINE v_f_devolucion_captura       DATE; -- fecha de devolucion capturada

 -- Control de Excepciones
 DEFINE sql_err                         INTEGER;
 DEFINE isam_err                        INTEGER;
 DEFINE err_txt                         VARCHAR(255);

 -- si no se encuentra la tabla tmp_detalle_op98 entonces levantamos una excepcion
 -- The specified table (safre_tmp:tmp_detalle_op98) is not in the database
 ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_i_resultado = sql_err;

   RETURN v_i_resultado, isam_err, err_txt;
 END EXCEPTION

 -- Inicializa estatus para el caso de NO encontrar datos de archivo.
 LET v_i_procesados = 0;
 -- Inicializa estado correcto
 LET v_i_resultado = 0;
 LET isam_err = 0;
 LET err_txt = "El proceso finalizó correctamente.";
 -- Comparar datos

 -- Obtene datos de archivo para comparar
 FOREACH
    SELECT cve_afore, f_valor_devolucion, SUM(CASE (importe_viv97)
     WHEN 0 THEN 0 ELSE importe_viv97        /100
    END + CASE (importe_viv92)
     WHEN 0 THEN 0 ELSE importe_viv92        /100
    END)
    INTO v_cve_afore_archivo, v_f_valor_devolucion, v_importe_archivo
     FROM safre_tmp:tmp_detalle_op98
    GROUP BY cve_afore, f_valor_devolucion

    -- Corrige fecha de DDMMYYYY a MMDDYYY
    EXECUTE PROCEDURE sp_cambia_formato_fecha(v_f_valor_devolucion)
       INTO v_dte_f_valor_devolucion;
    -- Inicializa
    LET v_importe_capturado = NULL;
    -- Obtener datos de Montos Capturados
    SELECT SUM(tot_pes_devolucion)
      INTO v_importe_capturado
      FROM safre_viv:deo_mto_deposito
     WHERE cve_afore = v_cve_afore_archivo
       AND f_valor_devol_inf = v_dte_f_valor_devolucion
       AND estado_devolucion = 1; -- solo capturados

    -- si el importe capturado no existe
    IF ( v_importe_capturado IS NULL ) THEN
       -- Termina proceso con ERROR al NO encontrar importe

       -- se verifica si el problema es que las fechas no coinciden
       SELECT MAX(f_valor_devol_inf)
       INTO v_f_devolucion_captura
       FROM safre_viv:deo_mto_deposito
       WHERE cve_afore = v_cve_afore_archivo
       AND estado_devolucion = 1; -- solo capturados

       -- si no existe la fecha entonces no esta capturado
       IF ( v_f_devolucion_captura IS NOT NULL ) THEN
          -- se revisa si las fechas no son iguales
          IF ( v_dte_f_valor_devolucion <> v_f_devolucion_captura ) THEN
             LET v_i_resultado = 4;
             LET err_txt = 'Las fechas no son iguales.';
          END IF
       ELSE
          -- no se tienen montos capturados
          LET v_i_resultado = 1;
          LET err_txt = 'No se tienen montos capturados.';
       END IF


       RETURN v_i_resultado, isam_err, err_txt;
    END IF

    IF ( v_importe_capturado <> v_importe_archivo ) THEN
     -- Ocurrio ERROR al ser los importes diferentes
      LET v_i_resultado = 2;
      LET err_txt = 'ERROR al ser los importes diferentes.';
      RETURN v_i_resultado, isam_err, err_txt;
    END IF

    -- Indica que si entro al ciclo al menos una ves
    LET v_i_procesados = 1;
 END FOREACH -- archivo

 IF ( v_i_procesados = 0 ) THEN
    -- ERROR al NO encontrar datos de archivo
    LET err_txt = 'ERROR al no encontrar datos de archivo.';
    RETURN 3, isam_err, err_txt;
 END IF

 IF (v_i_resultado = 0) THEN
                LET err_txt = 'El proceso finalizó correctamente.';
 END IF

 RETURN v_i_resultado, isam_err, err_txt;
END FUNCTION
;


