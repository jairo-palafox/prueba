






CREATE PROCEDURE "safreviv".sp_bus_ejecuta_insert(p_txt LVARCHAR(5000));
    EXECUTE IMMEDIATE p_txt;
END PROCEDURE;


