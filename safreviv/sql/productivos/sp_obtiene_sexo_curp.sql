






CREATE PROCEDURE "safreviv".sp_obtiene_sexo_curp()

UPDATE afi_derechohabiente
SET sexo = decode(curp[11],"H",1,"M",2,0)
WHERE (sexo not in(1,2)
OR  sexo is null)
AND curp is not null
AND curp not matches " *"
;
END PROCEDURE;


