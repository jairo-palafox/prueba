






CREATE PROCEDURE "safreviv".sp_prt_error_bus(p_id_bus_solicitud_tramite DECIMAL(9,0) ,
                           p_proceso_cod              CHAR(003)    ,
                           p_operacion_cod            CHAR(004)    ,
                           p_usuario_cod              CHAR(020)    ,
                           p_f_error                  DATETIME YEAR TO SECOND, 
                           p_error_sql                INTEGER      ,
                           p_error_isam               INTEGER      ,
                           p_msg_sql                  CHAR(255)    ,
                           p_origen                   CHAR(255)    ,
                           p_ind                      CHAR(005)    ,
                           p_diag                     CHAR(255)     )                           
         

 
       INSERT INTO prt_error_bus VALUES (p_id_bus_solicitud_tramite    ,
                                 p_proceso_cod             ,
                                 p_operacion_cod           ,
                                 p_usuario_cod             ,
                                 p_error_sql               ,
                                 p_error_isam              ,
                                 p_msg_sql                 ,
                                 p_origen                  , -- funcion de donde proviene el error
                                 p_ind                     ,
                                 p_diag                    ,
                                 p_f_error                 );


  RAISE EXCEPTION p_error_sql, p_error_isam, p_msg_sql;

END PROCEDURE;


