






CREATE PROCEDURE "safreviv".sp_unix2dos(p_ruta CHAR(200),
                        p_archivo CHAR(40))

DEFINE v_comando CHAR(400);
DEFINE v_archivo CHAR(40);


LET v_comando = "cd "||"/opt/IBM/informix/bin;"||"./unix2dos "||TRIM(p_ruta)||" "||TRIM(p_archivo);

system v_comando;

END PROCEDURE

;


