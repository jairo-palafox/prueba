






CREATE FUNCTION "safreviv".fn_inicializa_proceso (p_pid         DECIMAL(9,0),
                                       p_proceso_cod SMALLINT    ,
                                       p_opera_cod   SMALLINT    ,
                                       p_folio       DECIMAL(9,0),
                                       p_programa    CHAR(20)    ,
                                       p_archivo     CHAR(40)    ,
                                       p_usuario     CHAR(20)    )
RETURNING SMALLINT;
DEFINE r_inserta        SMALLINT;

DEFINE v_proceso_cod    SMALLINT;
DEFINE v_opera_cod      SMALLINT;
DEFINE v_programa_cod   CHAR(10);
DEFINE v_estado         SMALLINT;
DEFINE v_fecha          DATE    ;

DEFINE v_recurrencia    SMALLINT;
DEFINE v_ejecuta_cod    SMALLINT;
DEFINE v_descripcion    CHAR(10);
DEFINE v_hora_prog      DATETIME HOUR TO MINUTE;

DEFINE v_op_recurrencia   SMALLINT;
DEFINE v_op_ejecuta_cod   SMALLINT;
DEFINE v_op_descripcion   CHAR(10);
DEFINE v_op_hora_prog     DATETIME HOUR TO MINUTE;
DEFINE v_op_categoria     SMALLINT;
DEFINE v_op_n_parametros  SMALLINT;
 

LET r_inserta = 0;
LET v_estado  = 1;
LET v_fecha   = TODAY;

IF EXISTS (SELECT pid
            FROM glo_pid
           WHERE pid         = p_pid
             AND proceso_cod = p_proceso_cod
             AND opera_cod   = p_opera_cod) THEN

   SELECT proceso_cod, 
          recurrencia, 
          ejecuta_cod, 
          descripcion, 
          hora_prog
     INTO v_proceso_cod,
          v_recurrencia,
          v_ejecuta_cod,
          v_descripcion,
          v_hora_prog
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod;

   IF v_proceso_cod > 0 THEN 
      INSERT INTO bat_ctr_proceso 
                  (
                    pid            ,
                    proceso_cod    ,
                    folio          ,
                    fecha_ini      ,
                    fecha_fin      ,
                    nom_archivo    ,
                    recurrencia    ,
                    ejecuta_cod    ,
                    descripcion    ,
                    hora_prog      ,
                    estado_cod     ,
                    factualiza     ,
                    usuario       
                  )
      
           VALUES(
                    p_pid        ,
                    p_proceso_cod,
                    p_folio      ,
                    CURRENT YEAR TO SECOND,
                    NULL         ,
                    p_archivo    ,
                    v_recurrencia,
                    v_ejecuta_cod,
                    v_descripcion,
                    v_hora_prog  ,
                    v_estado     ,
                    v_fecha      ,
                    p_usuario    
                  );

      FOREACH SELECT opera_cod   ,
                     programa_cod,
                     recurrencia ,
                     ejecuta_cod ,
                     descripcion ,
                     hora_prog   ,
                     categoria   ,
                     n_parametros
                INTO v_opera_cod ,
                     v_programa_cod,
                     v_op_recurrencia ,
                     v_op_ejecuta_cod ,
                     v_op_descripcion ,
                     v_op_hora_prog   ,
                     v_op_categoria   ,
                     v_op_n_parametros
                FROM cat_operacion
               WHERE proceso_cod = p_proceso_cod
               ORDER BY 1

         INSERT INTO bat_ctr_operacion   
                        (
                          pid                 ,
                          proceso_cod         ,
                          opera_cod           ,
                          programa_cod        ,
                          folio               ,
                          fecha_ini           ,
                          fecha_fin           ,
                          nom_archivo         ,
                          reg_procesados      ,
                          recurrencia         ,
                          ejecuta_cod         ,
                          descripcion         ,
                          hora_prog           ,
                          categoria           ,
                          n_parametros        ,
                          ind_tipo_ejecucion  ,
                          nom_pdf             ,
                          estado_cod          ,
                          factualiza          ,
                          usuario             
                        )
                  VALUES(
                          p_pid            ,         
                          p_proceso_cod    ,
                          v_opera_cod      ,
                          v_programa_cod   ,
                          p_folio          ,
                          NULL             ,
                          NULL             ,
                          p_archivo        ,
                          NULL             ,
                          v_op_recurrencia , 
                          v_op_ejecuta_cod ,
                          v_op_descripcion ,
                          v_op_hora_prog   ,
                          v_op_categoria   ,
                          v_op_n_parametros,
                          0                ,
                          NULL             ,
                          v_estado         ,
                          v_fecha          ,
                          p_usuario     
                        );
      END FOREACH
   END IF
ELSE
   LET r_inserta = 1;  --NO SE HA GENERADO EL PID PARA EL PROCESO Y LA OPERACION
END IF

RETURN r_inserta;
END FUNCTION;


