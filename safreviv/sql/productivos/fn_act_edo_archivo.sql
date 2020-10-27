






CREATE FUNCTION "safreviv".fn_act_edo_archivo(p_nombre_archivo CHAR(40)    ,
                                   p_folio          DECIMAL(9,0),
                                   p_estado         SMALLINT    , --#1 CARGADO, 2 INTEGRADO, 3 REVERSADO
                                   p_usuario        CHAR(20)    )
RETURNING SMALLINT;

   DEFINE r_actualiza    SMALLINT;

   LET r_actualiza = 0;

   IF NOT EXISTS (SELECT glo.nombre_archivo
                    FROM glo_ctr_archivo glo
                   WHERE glo.nombre_archivo = p_nombre_archivo) THEN
      LET r_actualiza = 1; --#NO EXISTE EL ARCHIVO
   ELSE
      IF p_estado = 2 THEN
         IF EXISTS (SELECT glo.nombre_archivo
                      FROM glo_ctr_archivo glo
                     WHERE glo.nombre_archivo = p_nombre_archivo
                       AND glo.estado         = 1) THEN

            UPDATE glo_ctr_archivo
               SET folio       = p_folio  ,
                   estado      = p_estado ,
                   f_actualiza = TODAY    ,
                   usuario     = p_usuario
             WHERE nombre_archivo = p_nombre_archivo;
         ELSE
            LET r_actualiza = 2; --#NO SE PUEDE ACTUALIZAR AL ESTADO INTEGRAR
         END IF
      ELIF p_estado =  3 THEN
            UPDATE glo_ctr_archivo
               SET estado      = p_estado ,
                   f_actualiza = TODAY    ,
                   usuario     = p_usuario
             WHERE nombre_archivo = p_nombre_archivo;
      ELSE
         LET r_actualiza = 3; --#NO EXISTE EL ESTADO
      END IF
   END IF

RETURN r_actualiza;
END FUNCTION;


