






CREATE PROCEDURE "safreviv".sp_bus_convierte_mensaje(p_folio_procesar  CHAR(50)      ,
                                          p_id_tabla        DECIMAL(9,0)  ,
                                          p_proceso         CHAR(03)      ,
                                          p_operacion       CHAR(04)      ,
                                          p_campo           CHAR(50)      ,
                                          p_cad_error       LVARCHAR(1000))
;

--------------------------------------------------------------------------------
-- Creado : 15 nov 2013
-- Autor  : Jesus David Yañez Moreno
-- Sistema: Bus Generico (SAFRE)
-- Descripcion: Sirve de puente entre el negocio y el BUS generico, rescata los
-- campos y valores de una tabla de negocio y los descompone en la estructura de
-- la bitacora del bus, tanto mensaje como cadena de errores.
-- version que recibe una cadena con los errores separados por "|"
-- Modificacion: 22 abr 2014
-- Autor: Jesus David Yanez Moreno
-- Descripcion: Se agrega funcionalidad para soportar envio de arreglos en 
-- una estructura tipo registro de coleccion de datos
--------------------------------------------------------------------------------

DEFINE v_atributo_negocio             CHAR(40);
DEFINE v_cve_natural                  CHAR(40);
DEFINE v_orden                        CHAR(40);
DEFINE v_tipo_dato                    CHAR(1);
DEFINE v_qry                          CHAR(2000);
DEFINE v_qry_arreglo                  CHAR(2000);
DEFINE v_entidad_negocio              CHAR(40);
DEFINE v_valor                        CHAR(80);
DEFINE v_diag_cod                     CHAR(004);
DEFINE v_diag_desc                    CHAR(100);
DEFINE v_existe_cad_error             SMALLINT;
DEFINE v_modulo_cod_neg               CHAR(003);
DEFINE v_tipo_contrato                SMALLINT;
DEFINE v_programa_servicio            VARCHAR(20);
DEFINE v_ruta_neg                     VARCHAR(100);

-- varibales de control de ciclo
DEFINE v_salida                       SMALLINT;
DEFINE v_hay_elemento                 SMALLINT;

DEFINE v_id_tabla                     DECIMAL(9,0);
DEFINE v_txt                          CHAR(1500);
DEFINE v_ruta_bus                     VARCHAR(100);

-- varibales para soportar valores del arreglo

DEFINE v_orden_arreglo                SMALLINT;
DEFINE v_id_bus_agrupa_bloque         DECIMAL(9,0);
DEFINE v_cve_natural_arreglo          CHAR(50);
DEFINE v_valor_arreglo                CHAR(120);


SET DEBUG FILE TO '/tmp/sp_bus_convierte_mensaje.trace';
TRACE ON;

   SELECT a.ruta_bin
   INTO   v_ruta_bus
   FROM   seg_modulo a  
   --FROM   safre_bus@bus_tcp:seg_modulo a  -- desa efp peiss
   --FROM   safre_bus@safrebus_tcp:seg_modulo a  -- peiss
   WHERE  a.modulo_cod = "bus";

   LET v_id_tabla = p_id_tabla;

   FOREACH SELECT c.entidad_negocio    ,
                  d.atributo_negocio   ,
                  d.cve_natural        ,
                  d.orden              ,
                  d.tipo_dato          
           INTO   v_entidad_negocio    ,
                  v_atributo_negocio   ,
                  v_cve_natural        ,
                  v_orden              ,
                  v_tipo_dato          
-- originalmente de bus_tcp  (instancia de safrebus)
           FROM   cat_bus_proceso           a    , -- peiss
                  cat_bus_operacion         b    ,
                  cat_bus_contrato          c    ,
                  cat_bus_detalle_contrato  d
           WHERE  a.cod_proceso_bus      = p_proceso
           AND    b.cod_opera_bus        = p_operacion
           AND    a.id_cat_bus_proceso   = b.id_cat_bus_proceso
           AND    b.id_cat_bus_operacion = c.id_cat_bus_operacion
           AND    c.ind_vigencia         = 1
           AND    c.id_cat_bus_contrato  = d.id_cat_bus_contrato
           ORDER BY d.orden

              LET v_qry = "SELECT a."||TRIM(v_atributo_negocio)||" FROM "||
                          TRIM(v_entidad_negocio)||" a "||" WHERE a."||
                          TRIM(p_campo)||" = "||p_id_tabla;
        
              PREPARE prp_inserta_bus FROM v_qry;
              DECLARE cur_inserta_bus CURSOR FOR prp_inserta_bus;
              OPEN cur_inserta_bus;
        
                IF v_tipo_dato <> "A" THEN
                 LET v_valor = "";
                 FETCH cur_inserta_bus INTO v_valor;
                   -- insertar en bitacora temporal
                   INSERT INTO bus_tmp_detalle_solicitud VALUES(p_id_tabla    , --peiss
                   --INSERT INTO safre_bus@safrebus_tcp:bus_tmp_detalle_solicitud VALUES(p_id_tabla    , --peiss
                   --INSERT INTO safre_bus@bus_tcp:bus_tmp_detalle_solicitud VALUES(p_id_tabla    ,       -- desa efp peiss
                                                                                       v_cve_natural ,
                                                                                       v_valor       ,
                                                                                       v_orden       );
                ELSE 
                   --INSERT INTO safre_bus@safrebus_tcp:bus_tmp_detalle_solicitud VALUES(p_id_tabla    ,     -- peiss
                   --INSERT INTO safre_bus@bus_tcp:bus_tmp_detalle_solicitud VALUES(p_id_tabla    ,       -- desa efp peiss
                   INSERT INTO bus_tmp_detalle_solicitud VALUES(p_id_tabla    ,       -- desa efp peiss
                                                                v_cve_natural ,
                                                                ""            ,
                                                                v_orden       );
                 
                END IF;
             CLOSE cur_inserta_bus;
             FREE cur_inserta_bus;
             FREE prp_inserta_bus;

             IF v_tipo_dato = "A" THEN -- cuando el tipo de dato es de tipo arreglo

                 LET v_qry_arreglo = " EXECUTE PROCEDURE sp_bus_convierte_arreglo("
                                     ||p_id_tabla
                                     ||","||"'"
                                     ||TRIM(p_campo)||"'"
                                     ||","||"'"
                                     ||TRIM(v_atributo_negocio)||"'"
                                     ||","||"'"
                                     ||TRIM(v_entidad_negocio)||"'"
                                     ||","||"'"
                                     ||TRIM(p_proceso)||"'"
                                     ||","||"'"
                                     ||TRIM(p_operacion)||"'"
                                     ||")";
                 PREPARE prp_inserta_bus_arreglo  FROM v_qry_arreglo;    
                 DECLARE cur_inserta_bus_arreglo CURSOR FOR prp_inserta_bus_arreglo;
                 OPEN cur_inserta_bus_arreglo;
                 LET v_hay_elemento = 1;

                 WHILE (v_hay_elemento = 1)
                     FETCH cur_inserta_bus_arreglo INTO v_orden_arreglo,v_id_bus_agrupa_bloque,v_cve_natural_arreglo,v_valor_arreglo;
                     IF SQLCODE == 0 THEN
                        --INSERT INTO safre_bus@safrebus_tcp:bus_tmp_detalle_bloque VALUES (p_id_tabla             ,  --peiss
                        --INSERT INTO safre_bus@bus_tcp:bus_tmp_detalle_bloque VALUES (p_id_tabla             ,         -- desa efp peiss
                        INSERT INTO bus_tmp_detalle_bloque VALUES (p_id_tabla             ,         -- desa efp peiss
                                                                   v_id_bus_agrupa_bloque ,
                                                                   v_cve_natural_arreglo  ,
                                                                   v_valor_arreglo        ,
                                                                   v_orden_arreglo        ,
                                                                   v_orden                );
                     ELSE 
                        LET v_hay_elemento = 0;
                     END IF

                 END WHILE;

                 CLOSE cur_inserta_bus_arreglo;
                 FREE cur_inserta_bus_arreglo;
                 FREE prp_inserta_bus_arreglo;

            END IF;

   END FOREACH;

   -- insertar en bitacora temporal los errores operativos

   IF (p_cad_error <> " " AND p_cad_error <> ''  AND p_cad_error IS NOT NULL) THEN
      LET v_qry = "EXECUTE PROCEDURE sp_descompone_cadena('"||TRIM(p_cad_error)||"');";
      PREPARE prp_descompone_cadena FROM v_qry;
      DECLARE cur_descompone_cadena CURSOR FOR prp_descompone_cadena;
      OPEN cur_descompone_cadena;
      LET v_salida = 1;
      WHILE (v_salida = 1)

         FETCH cur_descompone_cadena INTO v_diag_cod,v_diag_desc, v_existe_cad_error;
         IF SQLCODE == 0 AND v_existe_cad_error = 0 THEN
           --INSERT INTO safre_bus@safrebus_tcp:bus_tmp_error_solicitud VALUES (p_id_tabla                     , --peiss
           --INSERT INTO safre_bus@bus_tcp:bus_tmp_error_solicitud VALUES (p_id_tabla                     ,        -- desa efp peiss
           INSERT INTO bus_tmp_error_solicitud VALUES (p_id_tabla                     ,        -- desa efp peiss
                                                       v_diag_cod                     ,
                                                      v_diag_desc                    );
         ELSE
            LET v_salida = 0;
         END IF;
      END WHILE;
      CLOSE cur_descompone_cadena;
      FREE cur_descompone_cadena;
   END IF;

   -- se ejecuta el sp para generar el mensaje a enviar por el bus generico

   SELECT c.id_tipo_contrato      ,
          a.modulo_cod            ,
          c.programa_servicio     ,
          d.ruta_bin
   INTO   v_tipo_contrato         ,
          v_modulo_cod_neg        ,
          v_programa_servicio     ,
          v_ruta_neg
   FROM   cat_bus_proceso       a ,
          cat_bus_operacion     b ,
          OUTER cat_bus_cza_operacion c ,
          OUTER seg_modulo            d
   --FROM   safre_bus@bus_tcp:cat_bus_proceso       a ,
          --safre_bus@bus_tcp:cat_bus_operacion     b ,
          --OUTER safre_bus@bus_tcp:cat_bus_cza_operacion c ,
          --OUTER safre_bus@bus_tcp:seg_modulo            d
   WHERE a.cod_proceso_bus = p_proceso
   AND   b.cod_opera_bus   = p_operacion
   AND   a.id_cat_bus_proceso   = b.id_cat_bus_proceso
   AND   b.id_cat_bus_operacion = c.id_cat_bus_operacion
   AND   a.modulo_cod           = d.modulo_cod;

   IF (v_tipo_contrato <> 2    OR
       v_tipo_contrato IS NULL OR 
       v_tipo_contrato = " "   OR 
       v_tipo_contrato = "") THEN 
      LET v_tipo_contrato = 1;
   END IF;

   IF v_tipo_contrato = 1 THEN  -- generico tipo contrato  <> 2 

      LET v_txt = "EXECUTE PROCEDURE safre_exe@vivex_tcp:sp_bus_envia_mensaje("|| -- 
      --LET v_txt = "EXECUTE PROCEDURE safre_exe@safrexe_tcp:sp_bus_envia_mensaje("|| -- 
      --LET v_txt = "EXECUTE PROCEDURE safre_af@afod_tcp:sp_bus_envia_mensaje("|| -- para desa efp pension issste
                  v_id_tabla||",'"||TRIM(p_folio_procesar)||"','"||
                  TRIM(p_proceso)||"','"||TRIM(p_operacion)||"','"||TRIM(v_ruta_bus)||"')";
 
   ELSE  -- iniciador tipo contrato 2

      LET v_txt = "EXECUTE PROCEDURE safre_exe@vivex_tcp:sp_bus_ejecuta_iniciador("|| -- para pension issste y desa efp safreweb
      --LET v_txt = "EXECUTE PROCEDURE safre_exe@safrexe_tcp:sp_bus_ejecuta_iniciador("|| -- para pension issste y desa efp safreweb
      --LET v_txt = "EXECUTE PROCEDURE safre_af@afod_tcp:sp_bus_ejecuta_iniciador("|| -- para desa efp pension issste
                  v_id_tabla||",'"||TRIM(p_proceso)||"','"||TRIM(p_operacion)||"','"||TRIM(v_ruta_bus)||"','"||TRIM(v_programa_servicio)||
                  "','"||TRIM(v_ruta_neg)||"')";
   END IF;

   EXECUTE IMMEDIATE v_txt;

END PROCEDURE;


