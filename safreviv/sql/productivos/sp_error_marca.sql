






CREATE PROCEDURE "safreviv".sp_error_marca(sql_err    INT, 
                                isam_err   INT,
                                error_info CHAR(70),
                                id_derechohabiente DECIMAL(9,0),
                                folio DECIMAL(9,0),
                                marca        SMALLINT, 
                                n_referencia INTEGER,
                                usuario      CHAR(20), 
                                funcion      CHAR(20),
                                fecha        DATETIME YEAR TO SECOND)
 SET LOCK MODE TO WAIT 5;

        INSERT INTO sfr_error_marca VALUES (
                                            id_derechohabiente,
                                            marca,
                                            n_referencia      ,
                                            folio             ,
                                            funcion           ,
                                            sql_err           ,          
                                            isam_err          ,
                                            error_info        , 
                                            fecha             , 
                                            usuario);
SET LOCK MODE TO NOT WAIT ;
END PROCEDURE;


