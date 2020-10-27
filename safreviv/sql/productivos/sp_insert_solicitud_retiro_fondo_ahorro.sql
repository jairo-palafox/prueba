






CREATE PROCEDURE "safreviv".sp_insert_solicitud_retiro_fondo_ahorro(p_id_solicitud       DECIMAL(9,0),    --id_solicitud
                                                         p_id_derechohabiente DECIMAL(9,0),    --id_derechohabiente
                                                         p_rechazo            SMALLINT,        --cod_rechazo
                                                         p_estado             SMALLINT,        --estado_solicitud
                                                         p_refer              CHAR(18),        --cve_referencia
                                                         p_pes_viv97          DECIMAL(14,2),   --pes_viv72
                                                         p_tanto_adicional    DECIMAL(14,2),   --pes_viv72
                                                         p_causal_retiro      SMALLINT,        --causal_retiro
                                                         p_id_datamart        DECIMAL(9,0),    --id_datamart
                                                         p_ident              SMALLINT,        -- Identificador de beneficiario (si aplica)
                                                         p_nombre             CHAR(18),        -- Nombre del beneficiario
                                                         p_ape_pat            CHAR(18),        -- Apellido paterno
                                                         p_ape_mat            CHAR(18),        -- Apellido materno
                                                         p_entidad            SMALLINT,        -- Entidad federativa 
                                                         p_caso_adai          SMALLINT,        -- Caso adai
                                                         v_modulo             CHAR(1)    
                                                         )
                                                         
   RETURNING INTEGER, INTEGER, VARCHAR(250)
   
   DEFINE v_folio                     DECIMAL(9,0);
   DEFINE v_i_estado_marca            INTEGER;
   DEFINE v_marca_fondo_ahorro        INTEGER; -- 801 de acuerdo a catalogo
   DEFINE v_proceso_cod               SMALLINT;
   
   -- variables para la manipulacion de errores
   DEFINE v_sql_error  INTEGER     ;
   DEFINE v_isam_error INTEGER     ;
   DEFINE v_mensaje    VARCHAR(250);
   
   -- en caso de ocurrir un error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje
   
      RETURN v_sql_error, v_isam_error, v_mensaje;
   END EXCEPTION
                                    
   -- se asume que no hay error
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_mensaje    = "Solicitud creada exitosamente";
                                    
                                    
   -- se inician las variables para marca
   LET v_marca_fondo_ahorro   = 802; -- marca para disposicion de recursos
   LET v_i_estado_marca       = 0;
   LET v_folio                = 0 ;
   LEt v_proceso_cod          = 1503;
   

   --LET p_id_derechohabiente   =  0   ;   -- identificador de derechohabiente
   --LET p_id_solicitud         =  0   ;   -- identificador solicitud de retiro
   --LET p_tipo_r               =  0   ;   -- tipo del retiro 2 = fondo de ahorro (72-92)
   --LET p_rechazo              =  0   ;   -- causa del rechazo ligado a ret_rechazo
   --LET p_estado               =  0   ;   -- Estado de solicitud ligado a ret_estado_solicitud
   --LET p_pes_viv97            =  0.0 ;   -- total de acciones en pesos de la cuenta 72-92
   --LET p_refer                =  NULL;   -- Clabe de referencia bancaria
   --LET v_modulo               =  NULL;   -- A = alta de solicitud  ,M = modificacion de solicitud

   -- si se trata de un alta de solicitud
   IF ( v_modulo = 'A' ) THEN
   
      INSERT INTO ret_fondo_ahorro(id_solicitud                 --decimal(9,0)
                                   ,id_derechohabiente          --decimal(9,0)
                                   ,f_solicitud                 --date
                                   ,estado_solicitud            --smallint
                                   ,causal_retiro               --smallint
                                   ,id_datamart                 --decimal(9,0)
                                   ,folio                       --decimal(9,0)
                                   ,cve_referencia              --varchar(20,0)
                                   ,saldo_viv72                 --decimal(14,2)
                                   ,tanto_adicional             --decimal(14,2)
                                   ,caso_adai                   --integer
                                   ,entidad_federativa          --smallint
                                   ,f_captura                   --date
                                   ,h_captura                   --datetime hour to second
                                   ,usuario                     --varchar(20,0)
                                   ,cod_rechazo                 --smallint
                                   )
                            VALUES(p_id_solicitud,
                                   --seq_ret_solicitud.NEXTVAL,   --id_solicitud
                                   p_id_derechohabiente,        --id_derechohabiente
                                   today,                       --f_solicitud
                                   p_estado,                    --estado_solicitud
                                   p_causal_retiro,             --causal_retiro
                                   p_id_datamart,               --datamart
                                   v_folio,                     --folio
                                   p_refer,                     --cve_referencia
                                   p_pes_viv97,                 --importe_viv72
                                   p_tanto_adicional,           --tanto_adicional
                                   p_caso_adai,                 --caso_adai
                                   p_entidad,                   --entidad_federativa
                                   today,                       --f_captura
                                   CURRENT HOUR TO SECOND,      --h_solicitud
                                   USER,                        --usuario
                                   p_rechazo                    --cod_rechazo
                                   );
      
      INSERT INTO ret_beneficiario(id_solicitud                      --decimal(9,0)
                                   ,tpo_beneficiario                 --smallint
                                   ,nombre_beneficiario              --char(40)
                                   ,paterno_beneficiario             --char(40)
                                   ,materno_beneficiario             --char(40)
                                   ,f_registro                       --date
                                   
                                   )
                             VALUES(p_id_solicitud,
                                   --seq_ret_solicitud.CURRVAL ,         --id_solicitud           decimal(9,0)
                                   p_ident ,                            --tpo_beneficiario       smallint
                                   p_nombre,                            --nombre_beneficiario    char(40)
                                   p_ape_pat,                           --paterno_beneficiario   char(40)
                                   p_ape_mat,                           --materno_beneficiario   char(40)
                                   TODAY
                                   );
       
           
      -- se marca la cuenta
      LET v_i_estado_marca = 0;
      EXECUTE FUNCTION fn_marca_cuenta(
               p_id_derechohabiente
              ,v_marca_fondo_ahorro      -- marca de solo infonavitt
              ,p_id_solicitud
              --,seq_ret_solicitud.CURRVAL
              ,0                         -- folio se asignara en la solicitd
              ,0                         -- estado marca
              ,0                         -- codigo de rechazo
              ,0                         -- marca de la causa
              ,NULL                      -- fecha de la causa
              ,USER
              ,v_proceso_cod )           --proceso_cod 
          INTO v_i_estado_marca;
      
      -- se desmarca la cuenta cuando se rechaza
      IF ( p_estado = 100 ) THEN  
         EXECUTE FUNCTION fn_desmarca_cuenta(
              p_id_derechohabiente
             ,v_marca_fondo_ahorro            -- desmarca de solo infonavit
             ,p_id_solicitud
             --,seq_ret_solicitud.CURRVAL     -- identificador de registro de archivo o lote
             ,40                              -- estado marca
             ,v_marca_fondo_ahorro           -- marca de la causa
             ,USER
             ,v_proceso_cod  )                --proceso_cod 
         INTO v_i_estado_marca; 
      END IF  
   END IF 

   -- si se trata de una modificacion
   IF ( v_modulo = 'M' ) THEN
      UPDATE  ret_fondo_ahorro 
      SET     estado_solicitud   = p_estado,
              cod_rechazo        = p_rechazo
      WHERE  id_solicitud        = p_id_solicitud;
   END IF

   -- se devuelve el resultado de la solicitud
   RETURN v_sql_error, v_isam_error, v_mensaje;
END PROCEDURE;


