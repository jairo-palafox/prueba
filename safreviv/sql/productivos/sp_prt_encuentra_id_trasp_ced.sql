






create procedure "safreviv".sp_prt_encuentra_id_trasp_ced(p_folio_cliente varchar(10),
                                               p_folio_procesar char(50))

returning smallint ,
          dec(10,0);

define v_id_prt_nuevo_anterior dec(10,0);
define v_id_prt_nuevo_actual   dec(10,0);
define r_id_prt_traspaso_cedente   dec(10,0);
define v_folio_procesar        CHAR(50);
define v_diag smallint;
define i smallint;


SET DEBUG FILE TO '/tmp/debug.trace';
trace on;
let i = 0;
     select a.id_prt_nuevo
     INTO  v_id_prt_nuevo_anterior
     FROM  prt_his_id_folio a
     where a.folio_procesar = p_folio_procesar
     group by 1;

     IF DBINFO('SQLCA.SQLERRD2') = 1 THEN
     -- si encuentra el folio_procesar
        SELECT a.id_prt_traspaso_cedente
        INTO   r_id_prt_traspaso_cedente 
        FROM   prt_traspaso_cedente a
        WHERE  a.id_prt_traspaso_cedente = v_id_prt_nuevo_anterior;

        IF DBINFO('SQLCA.SQLERRD2') = 1 THEN
        -- si lo encuentra retorna el id_prt_traspaso_cedente acutal
           LET v_diag = 1;
           RETURN v_diag,
                  v_id_prt_nuevo_anterior;
        ELSE 
           WHILE i < 10 

                 SELECT a.folio_procesar ,
                        a.id_prt_nuevo
                 INTO   v_folio_procesar  ,
                        v_id_prt_nuevo_actual
                 FROM   prt_his_id_folio a
                 WHERE  a.id_prt_origen = v_id_prt_nuevo_anterior
                 AND    a.id_prt_origen <> a.id_prt_nuevo;

                IF DBINFO('SQLCA.SQLERRD2') = 1 THEN
                -- verifica si existe el id 

                   LET v_id_prt_nuevo_anterior = v_id_prt_nuevo_actual;

                  SELECT a.id_prt_traspaso_cedente
                  INTO   r_id_prt_traspaso_cedente 
                  FROM   prt_traspaso_cedente a
                  WHERE  a.id_prt_traspaso_cedente = v_id_prt_nuevo_anterior;

                 IF DBINFO('SQLCA.SQLERRD2') = 1 THEN
                 -- si lo encuentra retorna el id_prt_traspaso_cedente acutal
                    LET v_diag = 1;
                    RETURN v_diag,
                           v_id_prt_nuevo_anterior; 

                ELSE 

                   LET i = i + 1 ;
                END IF;
              ELSE 
                 LET v_diag = 0;
                 RETURN v_diag,
                        "";
              END IF;

        END WHILE;

      END IF;
   ELSE 
      LET v_diag = 0;
      RETURN v_diag,
             "";

   END IF;

end procedure;


