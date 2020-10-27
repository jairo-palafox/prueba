






CREATE FUNCTION "safreviv".fn_ret_integra_marca_embargo_todas(p_usuario_cod    CHAR(20)    ,
                                                   p_folio          DECIMAL(9,0),
                                                   p_nombre_archivo VARCHAR(40) ,
                                                   p_pid            DECIMAL(9,0),
                                                   p_proceso_cod    SMALLINT    ) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)

-- detalle de la tabla temporal
--tmp_ret_marca_embargo
DEFINE tmp_ret_marca_embargo_todas_nss                     CHAR    (11);
DEFINE tmp_ret_marca_embargo_todas_tpo_proceso             CHAR    (13);
DEFINE tmp_ret_marca_embargo_todas_tpo_marca               CHAR    (40);
DEFINE tmp_ret_marca_embargo_todas_doc_contable            CHAR    (14);
DEFINE tmp_ret_marca_embargo_todas_ejercicio_fiscal        CHAR    (40);

-- tablas destino
DEFINE ret_marca_embargo_todas_id_derechohabiente      DECIMAL(9,0);
DEFINE ret_marca_embargo_todas_nss                     CHAR(11)    ;
DEFINE ret_marca_embargo_todas_folio                   DECIMAL(9,0);
DEFINE ret_marca_embargo_todas_tpo_proceso             CHAR(4 )    ;
DEFINE ret_marca_embargo_todas_tpo_marca               CHAR(4 )    ;
DEFINE ret_marca_embargo_todas_doc_contable            CHAR(11)    ;
DEFINE ret_marca_embargo_todas_ejercicio_fiscal        CHAR(4 )    ;
DEFINE ret_marca_embargo_todas_estado_solicitud        SMALLINT    ;
DEFINE ret_marca_embargo_todas_cod_rechazo             SMALLINT    ;

-- registros marcados
DEFINE v_marcados                                SMALLINT;
-- registros con error en marca
DEFINE v_con_error_marca                         SMALLINT;
-- registros procesados
DEFINE v_procesados                              SMALLINT;
-- marca correspondiente para 0027
DEFINE v_marca_593                               SMALLINT;
-- marca correspondiente para 0028
DEFINE v_marca_594                               SMALLINT;
-- marca correspondiente para 0016
DEFINE v_marca_595                               SMALLINT;
-- marca correspondiente para 0026
DEFINE v_marca_596                               SMALLINT;
-- marca correspondiente para 0036
DEFINE v_marca_597                               SMALLINT;
-- marca paso
DEFINE v_marca                                   SMALLINT;
-- estado solicitud
DEFINE v_estado_solicitud                        SMALLINT;
-- codigo de rechazo
DEFINE v_cod_rechazo                             SMALLINT;
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
-- cantidad de registro para el mismo nss
DEFINE v_recurrencias                            SMALLINT;
DEFINE v_recurrencia_nss												 SMALLINT;
-- Control de Excepciones
DEFINE err_nss                                   CHAR(11);
DEFINE v_si_resultado                            SMALLINT;
DEFINE v_resultado                               SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      LET err_nss = tmp_ret_marca_embargo_todas_nss;
      
      RETURN v_si_resultado, isam_err, err_txt, err_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_marca_embargo.txt";

   -- se inician los contadores de registros insertados y rechazados

   LET v_marcados             = 0;
   LET v_con_error_marca      = 0;
   LET v_procesados           = 0;
   LET v_marca_593            = 593;
   LET v_marca_594            = 594;
   LET v_marca_595            = 595;
   LET v_marca_596            = 596;
   LET v_marca_597            = 597;
   LET v_estado_solicitud     = 0;
   LET v_cod_rechazo          = 0;
   LET v_marca                = 0;

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';
  

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

   -- integracion de detalle
   FOREACH
   SELECT
        nss             ,
        tpo_proceso     ,
        tpo_marca       ,
        doc_contable    ,
        ejercicio_fiscal
   INTO
        tmp_ret_marca_embargo_todas_nss             ,
        tmp_ret_marca_embargo_todas_tpo_proceso     ,
        tmp_ret_marca_embargo_todas_tpo_marca       ,
        tmp_ret_marca_embargo_todas_doc_contable    ,
        tmp_ret_marca_embargo_todas_ejercicio_fiscal
   FROM
        safre_tmp:tmp_ret_marca_embargo_todas
   
      -- se busca el derechohabiente
      LET v_recurrencias = 0;
      SELECT COUNT(*)
      INTO   v_recurrencias
      FROM   afi_derechohabiente
      WHERE  nss = tmp_ret_marca_embargo_todas_nss;
      
      LET v_recurrencia_nss = 0;
      SELECT COUNT(*)
      INTO   v_recurrencia_nss
      FROM   safre_tmp:tmp_ret_marca_embargo_todas
      WHERE  nss = tmp_ret_marca_embargo_todas_nss;

      IF v_recurrencias > 1 THEN
          LET ret_marca_embargo_todas_id_derechohabiente = 0;
          LET v_estado_solicitud                   = 100;
          LET v_cod_rechazo                        = 52;   -- Se encontro mas de un registro para el NSS
      ELSE
          IF v_recurrencias = 0 THEN
              LET ret_marca_embargo_todas_id_derechohabiente = 0;
              LET v_estado_solicitud                   = 100;
              LET v_cod_rechazo                        = 7;    -- No existe el NSS
          ELSE
              SELECT id_derechohabiente
              INTO   ret_marca_embargo_todas_id_derechohabiente
              FROM   afi_derechohabiente
              WHERE  nss = tmp_ret_marca_embargo_todas_nss;

              LET v_marca = 0;
              -- Se identifica la marca
              IF tmp_ret_marca_embargo_todas_tpo_marca = "0027" THEN
                  LET v_marca = v_marca_593;
              END IF
              IF tmp_ret_marca_embargo_todas_tpo_marca = "0028" THEN
                  LET v_marca = v_marca_594;
              END IF
              IF tmp_ret_marca_embargo_todas_tpo_marca = "0016" THEN
                  LET v_marca = v_marca_595;
              END IF
              IF tmp_ret_marca_embargo_todas_tpo_marca = "0026" THEN
                  LET v_marca = v_marca_596;
              END IF
              IF tmp_ret_marca_embargo_todas_tpo_marca = "0036" THEN
                  LET v_marca = v_marca_597;
              END IF
              
              -- Si la marca fue identificada correctamente
              IF v_marca <> 0 THEN
                  CALL fn_marca_cuenta(ret_marca_embargo_todas_id_derechohabiente, v_marca, 0, p_folio,0,0,0,NULL,"SAFREVIV",1579) RETURNING v_resultado;                  IF v_resultado <> 0 THEN
                      LET v_estado_solicitud = 100;
                      LET v_cod_rechazo      = v_resultado;
                  ELSE 
                      LET v_estado_solicitud = 0;
                      LET v_cod_rechazo      = 0;       -- no hubo problemas con la marca
                  END IF -- v_resultado <> 0

              ELSE 
                  LET v_estado_solicitud = 100;
                  LET v_cod_rechazo      = 333;         -- Problemas de datos
              END IF -- v_marca <> 0

          END IF -- v_recurrencias = 0
      END IF -- v_recurrencias > 1

      -- se transfieren los datos a registro
      LET ret_marca_embargo_todas_nss                = tmp_ret_marca_embargo_todas_nss;
      LET ret_marca_embargo_todas_folio              = p_folio;
      LET ret_marca_embargo_todas_tpo_proceso        = tmp_ret_marca_embargo_todas_tpo_proceso;
      LET ret_marca_embargo_todas_tpo_marca          = tmp_ret_marca_embargo_todas_tpo_marca;
      LET ret_marca_embargo_todas_doc_contable       = tmp_ret_marca_embargo_todas_doc_contable;
      LET ret_marca_embargo_todas_ejercicio_fiscal   = tmp_ret_marca_embargo_todas_ejercicio_fiscal;
      LET ret_marca_embargo_todas_estado_solicitud   = v_estado_solicitud;
      LET ret_marca_embargo_todas_cod_rechazo        = v_cod_rechazo;

      -- se inserta en tabla destino
      INSERT INTO ret_marca_embargo_todas(id_derechohabiente      ,
                                          nss                     ,
                                          folio                   ,
                                          tpo_proceso             ,
                                          tpo_marca               ,
                                          doc_contable            ,
                                          ejercicio_fiscal        ,
                                          estado_solicitud        ,
                                          cod_rechazo             )
                                  VALUES (ret_marca_embargo_todas_id_derechohabiente,
                                          ret_marca_embargo_todas_nss               ,
                                          ret_marca_embargo_todas_folio             ,
                                          ret_marca_embargo_todas_tpo_proceso       ,
                                          ret_marca_embargo_todas_tpo_marca         ,
                                          ret_marca_embargo_todas_doc_contable      ,
                                          ret_marca_embargo_todas_ejercicio_fiscal  ,
                                          ret_marca_embargo_todas_estado_solicitud  ,
                                          ret_marca_embargo_todas_cod_rechazo       );
   
   END FOREACH;
   
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_marca_embargo_todas;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj,tmp_ret_marca_embargo_todas_nss;
END FUNCTION;


