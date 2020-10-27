






CREATE PROCEDURE "safreviv".sp_dis_transaccion24 ( p_proceso_cod     SMALLINT,
                                        p_opera_cod       SMALLINT,
                                        p_nombre_archivo  CHAR(40),
                                        p_folio_liquida   DECIMAL(10,0),
                                        p_estado          SMALLINT,
                                        p_destino         SMALLINT,
                                        p_usuario         CHAR(20)) 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 01082017
--Declaracion de variables
DEFINE v_status          SMALLINT;
DEFINE sql_err           INTEGER;
DEFINE isam_err          INTEGER;
DEFINE error_info        CHAR(70);
DEFINE v_char            CHAR(20);
DEFINE v_bnd_proceso     SMALLINT;
DEFINE v_bnd_transaccion SMALLINT;
DEFINE v_hoy             DATE;

ON EXCEPTION 
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN v_status, isam_err, error_info;
END EXCEPTION

  LET v_bnd_proceso = 0;
  LET v_hoy         = TODAY;
   
  INSERT INTO glo_ctr_archivo(proceso_cod,
                              opera_cod,
                              nombre_archivo,
                              folio,
                              estado,
                              f_actualiza,
                              usuario)
  VALUES                     (p_proceso_cod,
                              p_opera_cod,
                              p_nombre_archivo,
                              p_folio_liquida,
                              p_estado,
                              v_hoy,
                              p_usuario);
	   
  INSERT INTO dis_ctr_archivo(folio_liquida,
                              nombre_archivo,
                              cve_destino)
  VALUES                     (p_folio_liquida,
                              p_nombre_archivo,
                              p_destino);
	   
 LET error_info = "Termina transacción 24 correctamente";
 RETURN v_bnd_proceso, 0, error_info;

END PROCEDURE;


