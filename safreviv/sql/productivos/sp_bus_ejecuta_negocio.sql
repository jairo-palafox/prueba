






CREATE PROCEDURE "safreviv".sp_bus_ejecuta_negocio(p_id_bus_tramite           DECIMAL(9,0),
                                        p_folio_procesar           CHAR(50),
                                        p_id_bus_solicitud_tramite DECIMAL(9,0),
                                        p_id_cat_bus_contrato      DECIMAL(9,0))
RETURNING LVARCHAR(2000);

--------------------------------------------------------------------------------
-- Creado : 15 nov 2013
-- Autor  : Jesus David Yañez Moreno
-- Sistema: Bus Generico (SAFRE)
-- Descripcion: Sirve de puente entre el bus generico y SAFRE, extrae los datos
-- de un contrato y los mapea generando una inserción en la tabala productiva
-- inicial del proceso correspondiente
-- Modificado: 21 abr 2014
-- Autor  : Jesus David Yañez Moreno
-- Descripcion: Se modifica para soportar arreglos en las notificaciones xml
-- se modifica el insert como la llamada a un procedimiento en la bd destino
-- ya que informix no soporta el insert de un tipo de dato coleccion en querys
-- distribuidos.
--------------------------------------------------------------------------------

DEFINE v_programa              CHAR(40);
DEFINE v_txt                   LVARCHAR(4000);
DEFINE v_entidad_negocio       CHAR(40);
DEFINE v_cod_proceso_bus       CHAR(3);
DEFINE v_cod_opera_bus         CHAR(4);
DEFINE i                       SMALLINT;
DEFINE v_atributo_negocio      VARCHAR(50);
DEFINE v_valor                 VARCHAR(120);
DEFINE v_valor_row             VARCHAR(120);
DEFINE v_campo                 LVARCHAR(500); --revisar si es suficiente
DEFINE v_id_tipo_contrato      SMALLINT;

--DEFINE v_dato                  CHAR(2000); --revisar si es suficiente
--DEFINE v_row                   CHAR(2000); --revisar si es suficiente
--DEFINE v_dato_arreglo          CHAR(2000); --revisar si es suficiente

DEFINE v_dato                  LVARCHAR(5000); --revisar si es suficiente
DEFINE v_row                   LVARCHAR(5000); --revisar si es suficiente
DEFINE v_dato_arreglo          LVARCHAR(5000); --revisar si es suficiente

DEFINE v_ultimo                SMALLINT;
DEFINE v_cod_error             CHAR(004);
DEFINE v_desc_error            CHAR(100);
DEFINE v_cadena_error          LVARCHAR(1000);
DEFINE v_tipo_dato             CHAR(001);
DEFINE v_id_bus_solicitud_detalle DECIMAL(9,0);
DEFINE v_id_bus_agrupa_bloque DECIMAL(9,0);
DEFINE v_id_cat_bus_detalle_contrato DECIMAL(9,0);
DEFINE v_nombre_campo          VARCHAR(50);
DEFINE v_longitud              INTEGER;

--SET DEBUG FILE TO '/tmp/ejecuta_negocio.trace';
--TRACE ON;
-- buscar el sp a ejecutar

   SET LOCK MODE TO WAIT 10;

   SELECT a.programa         ,
          b.entidad_negocio  ,
          c.cod_opera_bus    ,
          d.cod_proceso_bus  ,
          e.id_tipo_contrato 
   INTO   v_programa        ,
          v_entidad_negocio ,
          v_cod_opera_bus   ,
          v_cod_proceso_bus ,
          v_id_tipo_contrato 
   FROM   cat_bus_negocio       a ,
          cat_bus_contrato      b ,
          cat_bus_operacion     c ,
          cat_bus_proceso       d ,
          outer cat_bus_cza_operacion e 
   WHERE  b.id_cat_bus_contrato  = p_id_cat_bus_contrato
   AND    b.id_cat_bus_negocio   = a.id_cat_bus_negocio
   AND    c.id_cat_bus_operacion = b.id_cat_bus_operacion
   AND    d.id_cat_bus_proceso   = c.id_cat_bus_proceso
   AND    c.id_cat_bus_operacion = e.id_cat_bus_operacion;

-- insertar en entidad de negocio cuando no es iniciador

   IF v_id_tipo_contrato IS NULL OR v_id_tipo_contrato = " " OR v_id_tipo_contrato = "" OR v_id_tipo_contrato = 0 THEN
      LET v_id_tipo_contrato = 1;
   END IF;

   LET v_campo = "";
   LET v_dato  = "";

 IF v_id_tipo_contrato <> 2 THEN  -- tipos distintos de iniciador (genericos)

   FOREACH SELECT b.atributo_negocio         ,
                  a.valor                    ,
                  b.tipo_dato                ,
                  a.id_bus_solicitud_detalle ,
                  b.id_cat_bus_detalle_contrato,
                  a.nombre_campo
           INTO   v_atributo_negocio         ,
                  v_valor                    ,
                  v_tipo_dato                ,
                  v_id_bus_solicitud_detalle ,
                  v_id_cat_bus_detalle_contrato,
                  v_nombre_campo
           FROM   bus_detalle_solicitud    a  ,
                  cat_bus_detalle_contrato b  ,
                  bus_tramite              c  ,
                  bus_solicitud_tramite    d
           WHERE  c.id_bus_tramite           = p_id_bus_tramite
           AND    c.id_cat_bus_contrato      = b.id_cat_bus_contrato
           AND    b.cve_natural              = a.nombre_campo
           AND    c.id_bus_tramite           = d.id_bus_tramite
           AND    d.id_bus_solicitud_tramite = a.id_bus_solicitud_tramite

           IF v_valor IS NULL OR v_valor = "" OR v_valor = " " THEN
              LET v_valor = " ";
           END IF;

           LET v_campo = CONCAT(TRIM(v_campo),TRIM(v_atributo_negocio)||",");

           IF v_tipo_dato <> "A"  THEN
              LET v_dato  = CONCAT(TRIM(v_dato),"'"||RTRIM(v_valor)||"',");   -- verificar si con esto es suficiente
           ELSE

              LET v_dato_arreglo = "";

              FOREACH SELECT a.id_bus_agrupa_bloque
                      INTO   v_id_bus_agrupa_bloque
                      FROM   bus_detalle_bloque  a
                      WHERE  a.id_bus_solicitud_detalle = v_id_bus_solicitud_detalle
                      AND    a.id_bus_agrupa_bloque <> 1
                      GROUP BY 1
                      ORDER BY 1

                      LET v_row = "";

                FOREACH SELECT b.valor
                        INTO   v_valor_row
                        FROM  bus_detalle_bloque       b ,
                              cat_bus_detalle_contrato c ,
                              cat_bus_bloque           d ,
                              cat_bus_detalle_bloque   e
                        WHERE b.id_bus_solicitud_detalle = v_id_bus_solicitud_detalle
                        AND   b.id_bus_agrupa_bloque     = v_id_bus_agrupa_bloque
                        AND   c.id_cat_bus_detalle_contrato = v_id_cat_bus_detalle_contrato
                        AND   c.id_cat_bus_detalle_contrato = d.id_cat_bus_detalle_contrato
                        AND   d.id_cat_bus_bloque           = e.id_cat_bus_bloque
                        AND   e.cve_natural_bloque          = b.nombre_campo_bloque
                        ORDER BY e.orden

                        IF v_valor_row IS NULL OR v_valor_row = "" OR v_valor_row = " " THEN
                           LET v_row  = CONCAT(TRIM(v_row),"NULL"||',');   -- insertar valor nulo
                        ELSE
                           LET v_row  = CONCAT(TRIM(v_row),'"'||RTRIM(v_valor_row)||'",');   -- verificar si con esto es suficiente
                        END IF;

                END FOREACH;

                let v_longitud = 0;
                LET v_longitud = LENGTH(TRIM(v_row));
                LET v_row = substr(v_row,1,v_longitud-1);
                LET v_row = CONCAT(TRIM(v_row),")");

                IF v_id_bus_agrupa_bloque = 2 THEN
                   LET v_dato_arreglo = CONCAT("ROW(",TRIM(v_row));
                ELIF v_id_bus_agrupa_bloque > 2 THEN
                   LET v_dato_arreglo = CONCAT(TRIM(v_dato_arreglo),",ROW("||TRIM(v_row));
                END IF;
              END FOREACH;
              LET v_dato_arreglo =  CONCAT("'MULTISET{",TRIM(v_dato_arreglo)||"}',");
              LET v_dato          = CONCAT(TRIM(v_dato),TRIM(v_dato_arreglo));
           END IF;

   END FOREACH;

 END IF;

           LET v_campo = CONCAT(TRIM(v_campo),"cad_error");

-- se pega la cadena variable de error

   LET v_cadena_error = " ";

   FOREACH SELECT a.cod_error       ,
                  a.desc_error
           INTO   v_cod_error       ,
                  v_desc_error
           FROM   bus_error_solicitud      a  ,
                  bus_tramite              c  ,
                  bus_solicitud_tramite    d
           WHERE  c.id_bus_tramite           = p_id_bus_tramite
           AND    c.id_bus_tramite           = d.id_bus_tramite
           AND    d.id_bus_solicitud_tramite = a.id_bus_solicitud_tramite


           LET v_cadena_error = CONCAT(TRIM(v_cadena_error),TRIM(v_cod_error)||" "||TRIM(v_desc_error)||"|");

   END FOREACH;

           LET v_dato = CONCAT(TRIM(v_dato),"'"||TRIM(v_cadena_error)||"'");



           IF v_id_tipo_contrato = 2 THEN -- para iniciador solo se actualiza

                LET v_txt  = TRIM("UPDATE safre_viv@vivop_tcp:"||
                --LET v_txt  = TRIM("UPDATE safre_af@iss_tcp:"||
                             TRIM(v_entidad_negocio)||
                             " SET folio_procesar = "||
                             "'"||TRIM(p_folio_procesar)||"'"||
                             " WHERE id_bus_solicitud_tramite = "||
                             p_id_bus_solicitud_tramite||";");

           ELSE   -- para genericos se inserta el registro
         
           LET v_txt  = TRIM("INSERT INTO safre_viv@vivop_tcp:"||TRIM(v_entidad_negocio)||   --invonavit
           --LET v_txt  = TRIM("INSERT INTO "||"safre_af@afod_tcp:"||TRIM(v_entidad_negocio)||  --safreweb
           --LET v_txt  = TRIM("INSERT INTO "||"safre_af@iss_tcp:"||TRIM(v_entidad_negocio)||   --desa pension efp
                        "(id_bus_solicitud_tramite,folio_procesar,bus_proceso_cod,bus_operacion_cod,"||TRIM(v_campo)||") VALUES ("||
                        p_id_bus_solicitud_tramite||",'"||TRIM(p_folio_procesar)||"','"||v_cod_proceso_bus||"','"||v_cod_opera_bus||"',"||TRIM(v_dato)||")");
           END IF;

           EXECUTE PROCEDURE safre_viv@vivop_tcp:sp_bus_ejecuta_insert(v_txt);  --infonavit
           --EXECUTE PROCEDURE safre_af@afod_tcp:sp_bus_ejecuta_insert(v_txt); --safreweb
           --EXECUTE PROCEDURE safre_af@iss_tcp:sp_bus_ejecuta_insert(v_txt);  --desa efp peiss


           LET v_txt = "EXECUTE PROCEDURE safre_viv@vivop_tcp:"||TRIM(v_programa)|| --invonavit
           --LET v_txt = "EXECUTE PROCEDURE safre_af@afod_tcp:"||TRIM(v_programa)|| --safreweb
           --LET v_txt = "EXECUTE PROCEDURE safre_af@iss_tcp:"||TRIM(v_programa)||  --desa efp peiss
                       --"("||p_id_bus_solicitud_tramite||",'"||TRIM(p_folio_procesar)||"','"||v_cod_proceso_bus||"','"||v_cod_opera_bus||"')";
                         "("||p_id_bus_solicitud_tramite||",'"||TRIM(p_folio_procesar)||"')";
           EXECUTE IMMEDIATE v_txt;

RETURN v_txt;

END PROCEDURE;


