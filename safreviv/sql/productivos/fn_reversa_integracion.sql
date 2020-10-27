






CREATE FUNCTION "safreviv".fn_reversa_integracion(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT)
RETURNING SMALLINT;
   
   DEFINE v_sqlQry      CHAR(1024);
   DEFINE v_nom_tabla   CHAR(50);
   DEFINE v_bnd_reverso SMALLINT;
   DEFINE v_sql_error   SMALLINT;
   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      --TRACE 'Ocurrio el error:'||v_sql_error;
   END EXCEPTION WITH RESUME
   
   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/reverso_integracion.trace';
   LET v_sql_error = 0;
   
   -- Inicializa bandera de ejecucion de reverso
   LET v_bnd_reverso = 0;
   LET v_nom_tabla = NULL;
   
   -- Recupera el nombre de las tablas de integracion correspondiente
   -- al proceso y operacion
   FOREACH SELECT nombre_tabla
             INTO v_nom_tabla
             FROM cat_tabla_reverso
            WHERE proceso_cod = p_proceso_cod
   
      -- Si se ha recuperado una tabla se elimina los registros
      IF(v_nom_tabla IS NOT NULL)THEN
         LET v_sql_error = 0;
         LET v_sqlQry = 'DELETE '||
                        '  FROM '||TRIM(v_nom_tabla)||
                        ' WHERE folio = '||p_folio||';';
         --TRACE v_sqlQry;
         -- Ejecuta la consulta
         EXECUTE IMMEDIATE v_sqlQry;
         
         IF(SQLCODE != 0)THEN
            -- Ocurrió un error en la consulta
            --TRACE 'Ocurrió el error:'||v_sql_error;
            RETURN v_sql_error  ;
         END IF
         
      ELSE
         -- No se encontro nombre de tabla y no se realizó el reverso
         LET v_sql_error = 100;
         --TRACE 'No se encontró el nombre de la tabla de preliquidacion';
         RETURN v_sql_error ; --WITH RESUME;
      END IF
      LET v_nom_tabla = NULL;
   END FOREACH
   
   -- se actualiza el estatus del folio
   UPDATE glo_folio
   SET status = -1
   WHERE folio = p_folio;

   
   RETURN v_sql_error;
END FUNCTION;


