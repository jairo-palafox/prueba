create table ret_simula_procesar (
nss         char(11),
diagnostico smallint,
estatus     smallint 
)
extent size 16
lock mode row;

create index xpkret_simula_procesar on ret_simula_procesar(nss);

CREATE TABLE safreviv.ret_solicitud_generico
(
   id_solicitud            decimal(9),
   id_derechohabiente      decimal(9),
   nss                     char(11),
   rfc                     char(13),
   modalidad_retiro        smallint,
   folio                   decimal(9),
   caso_adai               char(10),
   id_archivo_envio        decimal(9),
   id_archivo_respuesta    decimal(9),
   folio_restitucion       decimal(9),
   id_archivo_cancela_cxp  decimal(9),
   id_archivo_resp_cxp     decimal(9),
   f_solicitud             date,
   h_solicitud             datetime hour to second,
   estado_solicitud        smallint,
   cod_rechazo             smallint
)
EXTENT SIZE 16
NEXT SIZE 16
LOCK MODE ROW;

CREATE INDEX safreviv.xif7ret_solicitud_generico
   ON safreviv.ret_solicitud_generico (cod_rechazo);

CREATE UNIQUE INDEX safreviv.xpkret_solicitud_generico
   ON safreviv.ret_solicitud_generico (id_solicitud);

CREATE INDEX safreviv.xpk10ret_solicitud_generico
   ON safreviv.ret_solicitud_generico (id_solicitud, estado_solicitud, id_derechohabiente);

CREATE INDEX safreviv.xif8ret_solicitud_generico
   ON safreviv.ret_solicitud_generico (estado_solicitud);

CREATE INDEX safreviv.xif9ret_solicitud_generico
   ON safreviv.ret_solicitud_generico (id_derechohabiente, nss);
