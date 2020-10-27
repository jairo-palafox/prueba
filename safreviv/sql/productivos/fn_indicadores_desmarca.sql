






CREATE FUNCTION "safreviv".fn_indicadores_desmarca(p_id_unificador     DECIMAL(9,0), 
			 	                                p_id_unificado      DECIMAL(9,0), 
                                        p_id_dh_unificado   DECIMAL(9,0), 
                                        p_id_dh_unificador  DECIMAL(9,0),
                                        p_folio_unificacion DECIMAL(9,0),
                                        p_f_lote	          DATE        ,
                                        p_id_proceso	      INTEGER     , 
                                        p_nom_archivo       CHAR(40)    ,
                                        p_usuario           CHAR(20)    ,
                                        p_secuencia         INTEGER     ,
                                        p_ctr_folio_archivo DECIMAL(9,0),
                                        p_proceso_cod       SMALLINT    )

   RETURNING SMALLINT, INTEGER, CHAR(200)

DEFINE v_f_actualiza        DATE     ;
DEFINE v_marca_unificado    INTEGER  ;
DEFINE v_resp_datos_credito INTEGER  ;
DEFINE v_estado_marca       SMALLINT ;
DEFINE sql_err              INTEGER  ;
DEFINE v_si_resultado       INTEGER  ;
DEFINE isam_err             INTEGER  ;
DEFINE err_txt              CHAR(200);
DEFINE v_c_msj              CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      LET v_c_msj        = err_txt;
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

LET v_estado_marca = 0;
LET v_c_msj        = "Ok";
LET v_si_resultado = 0;


   FOREACH
      --Recupera la fecha de actualización.
      SELECT f_actualiza 
      INTO   v_f_actualiza
      FROM   glo_folio
      WHERE  folio = p_folio_unificacion

      SELECT id_derechohabiente
      INTO   v_marca_unificado
      FROM   sfr_marca_activa
      WHERE  marca  IN (221,223,225)
      AND    id_derechohabiente = p_id_dh_unificado
      GROUP BY 1 ;

      IF v_marca_unificado IS NULL THEN 
         --Ejecuta función de actualización de datos de crédito en tablas histórica
         EXECUTE FUNCTION fn_unifica_cuenta(p_id_unificador     ,    
                                            p_id_unificado      ,
                                            p_id_dh_unificado   ,
                                            p_id_dh_unificador  ,
                                            p_folio_unificacion ,
                                            v_f_actualiza	      ,
                                            p_id_proceso	      ,
                                            p_nom_archivo       ,
                                            p_usuario           ,
                                            p_secuencia         ,
                                            p_ctr_folio_archivo)
                                       INTO v_resp_datos_credito;

         EXECUTE FUNCTION fn_uni_posliquida_imss(p_usuario          ,
                                                 p_folio_unificacion,
                                                 p_proceso_cod      ,
                                                 p_id_dh_unificado  ,
                                                 p_id_dh_unificador ,
                                                 v_estado_marca)
                                            INTO v_si_resultado,
                                                 isam_err,
                                                 v_c_msj;
         IF( v_si_resultado = 0 )THEN
            UPDATE uni_det_unificador
               SET diagnostico = 5 -- indicadores
             WHERE diagnostico = 4 -- liquidados
               AND folio_unificacion = p_folio_unificacion
               AND id_unificador = p_id_unificador;

            UPDATE uni_det_unificado
               SET diagnostico = 5 -- Indicadores
             WHERE diagnostico = 4 -- Liquidados
               AND folio_unificacion = p_folio_unificacion
               AND id_unificado = p_id_unificado;
         END IF;
      END IF;   
   END FOREACH;

   RETURN v_si_resultado, isam_err, v_c_msj;

END FUNCTION;


