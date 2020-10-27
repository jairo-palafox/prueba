






CREATE PROCEDURE "safreviv".sp_crea_grp_unificacion()

DROP TABLE IF EXISTS dse_grp_unificacion;

CREATE TABLE dse_grp_unificacion (

   id_derechohabiente     DECIMAL(9,0),
   id_dse_grp_devolucion  DECIMAL(9,0),
   id_dh_unificador       DECIMAL(9,0),
   id_dse_grp_unificador  DECIMAL(9,0),
   f_proceso              DATE,
   usuario                CHAR(20))
 in dse_dbs extent size 16 next size 16 lock mode row;

create index xie1dse_grp_uni on dse_grp_unificacion (id_derechohabiente) 
using btree in dse_ix_dbs;

END PROCEDURE;


