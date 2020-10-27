






CREATE FUNCTION "safreviv".fn_consulta_precio_fondo (p_d_importe  DECIMAL(18,2) ,
                                          p_f_liquida   DATE   ,
                                          p_fondo   SMALLINT  )
RETURNING DECIMAL(18,2)  
   DEFINE v_status                SMALLINT;
   DEFINE v_error                 SMALLINT;
   DEFINE isam_err                INTEGER ;
   DEFINE v_d_precio_fondo        DECIMAL(19,14);
   DEFINE v_d_importe_calculado   DECIMAL(18,2);
   DEFINE err_txt                 VARCHAR(255)  ;
{
   ON EXCEPTION 
      SET v_error
      LET v_d_importe_calculado =  NULL ;
      RETURN v_error  ;
   END EXCEPTION 
}
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_registro_historicos_pag.trace';

   --TRACE  ("precio_fondo"|| p_f_liquida ) ;

   SELECT precio_fondo 
     INTO v_d_precio_fondo
     FROM glo_valor_fondo 
    WHERE fondo = p_fondo
      AND f_valuacion = p_f_liquida ;

    --TRACE  ("v_d_precio_fondo" || v_d_precio_fondo ) ;

    IF (v_d_precio_fondo IS NULL )  THEN 
    	 RETURN NULL ;
    ELSE
    	 LET v_d_importe_calculado = (p_d_importe /v_d_precio_fondo) ;
    END IF

     --TRACE  ("v_d_importe_calculado"|| v_d_importe_calculado );

   RETURN v_d_importe_calculado;
END FUNCTION;


