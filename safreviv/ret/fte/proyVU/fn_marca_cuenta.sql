DROP FUNCTION IF EXISTS fn_marca_cuenta;
CREATE FUNCTION fn_marca_cuenta(p_id_derechohabiente DECIMAL(9,0),
                                p_marca_entra        SMALLINT,
                                p_n_referencia       INTEGER ,
                                p_folio              DECIMAL(9,0),
                                p_estado_marca       SMALLINT,
                                p_codigo_rechazo     SMALLINT,
                                p_marca_causa        SMALLINT,
                                p_fecha_causa        DATE,
                                p_usuario            CHAR(20),
                                p_proceso_cod        SMALLINT)

   RETURNING INTEGER ; 

   DEFINE v_marca_activa   SMALLINT;
   DEFINE r_cod_rechazo    SMALLINT;
   DEFINE v_fecha_ini      DATE;
   DEFINE v_fecha_fin      DATE;
   DEFINE v_fecha_aux      DATE;
   DEFINE v_f_vigencia     DATE;
   DEFINE v_hora_ini       DATETIME HOUR TO SECOND;
   DEFINE v_contador       SMALLINT;
   DEFINE v_contador_aux   SMALLINT;
   DEFINE v_cod_excep      SMALLINT;
   DEFINE v_des_excep      CHAR(100);
   DEFINE v_dias_activa    SMALLINT;

   DEFINE sql_err INT;
   DEFINE isam_err INT;
   DEFINE error_info CHAR(70);


   ON EXCEPTION
      SET sql_err, isam_err, error_info

      IF sql_err = -691 THEN
         ----  Valida  constraints  
         LET v_cod_excep, v_des_excep= ( SELECT cod_excepcion,
                                                des_excepcion
                                           FROM cat_excep_marca
                                          WHERE constraint = error_info
                                      );
         IF v_des_excep IS NULL THEN
            IF error_info IS NULL THEN
               LET error_info = " ";
            END IF

            CALL sp_error_marca(sql_err, isam_err, error_info, 
                             p_id_derechohabiente,
                             p_folio             , 
                             p_marca_entra       ,
                             p_n_referencia      ,
                             p_usuario           , "marca_cuenta",CURRENT);
	        LET v_des_excep = error_info ; 
         ELSE
            LET v_fecha_ini = TODAY;
            LET v_fecha_fin = TODAY;
            LET v_hora_ini  = CURRENT;

            INSERT INTO sfr_excep_marca VALUES (
                                                p_id_derechohabiente,  -- nss
                                                p_marca_entra       ,  -- marca_cod
                                                v_fecha_ini         ,  -- fecha_ini
                                                v_hora_ini          ,  -- hora_ini
                                                p_n_referencia      ,  -- n_referencia 
                                                v_fecha_fin         ,  -- fecha_fin
                                                p_folio             ,  -- folio
                                                p_proceso_cod       ,  -- proceso_cod
                                                ''                   ,  -- proceso_desmarca
                                                90                  ,  -- estado      
                                                v_cod_excep         ,  -- rch_cod --Codigo de excepcion
                                                p_marca_entra       ,  -- marca_causa --Es correcto poner pmarca_entra
                                                v_fecha_ini         ,  -- fecha_causa
                                                p_usuario
                                               );
         END IF
      ELSE

         IF error_info IS NULL THEN
            LET error_info = " ";
         END IF
         CALL sp_error_marca(sql_err, isam_err, error_info, 
                          p_id_derechohabiente,
                          p_folio             , 
                          p_marca_entra       ,
                          p_n_referencia      ,
                          p_usuario           , "marca_cuenta",CURRENT);
         LET v_des_excep = error_info ;
      END IF

      LET v_des_excep = 'MARCAJE:'|| v_des_excep ||' '|| p_id_derechohabiente ||' '||p_marca_entra;

      RETURN sql_err;

      --EXECUTE PROCEDURE error  (-746,v_des_excep);

   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_marca_cuenta.trace';
   --TRACE ON;

   ---- Rechazo por validacion, por lo tanto envian la pmarca_entra
   ---- corresp., la pestado_marca = 30, el pcodigo_rechazo = corresp.

   LET v_fecha_ini = TODAY;
   LET v_fecha_fin = TODAY;
   LET v_fecha_aux = TODAY;
   LET v_hora_ini  = CURRENT;

    SELECT dias_activa
      INTO v_dias_activa 
      FROM sfr_marca
     WHERE marca = p_marca_entra ;

   LET v_f_vigencia = v_fecha_ini + v_dias_activa ;


   IF p_estado_marca = 30 OR
      p_estado_marca = 100 THEN
     
      SET LOCK MODE TO WAIT;
     
      INSERT INTO sfr_marca_historica VALUES (
                                         p_id_derechohabiente ,  -- nss
                                         p_marca_entra        ,  -- marca_cod
                                         p_n_referencia       ,  -- n_referencia 
                                         v_fecha_ini          ,  -- f_ini
                                         v_hora_ini           ,  -- h_ini
                                         v_fecha_fin          ,  -- f_fin
                                         p_folio              ,  -- folio
                                         p_proceso_cod        ,  -- proceso_marca
                                         ''                   ,  -- proceso_desmarca
                                         p_estado_marca       ,  -- estado      --El pestado_marca como param
                                         p_codigo_rechazo     ,  -- rch_cod --El codigo rechazo como param
                                         p_marca_entra        ,  -- marca_causa 
                                         v_fecha_fin          ,  -- f_causa
                                         v_f_vigencia         ,  -- f_vigencia
                                         p_usuario            ,  -- usuario que marca
                                         p_usuario               -- usuario que desmarca
                                        );
      SET LOCK MODE TO NOT WAIT;

      LET r_cod_rechazo = p_codigo_rechazo;

   ELSE 

   --- Valida que este cuadrada la tabla cta_convivencia, si no esta
   --- cuadrada regresa 9003 genera excepcion

      IF EXISTS ( SELECT marca
                    FROM sfr_marca
                   WHERE marca <> 0
                     AND NOT EXISTS 
                       ( SELECT marca_activa 
                           FROM sfr_convivencia
                          WHERE marca_entra  = p_marca_entra 
                            AND marca_activa = marca 
                           )
                ) THEN

         LET v_cod_excep    = 9003;
         LET r_cod_rechazo  = 9003;
   
         INSERT INTO sfr_excep_marca VALUES (
                                             p_id_derechohabiente ,  -- id_derechohabiente
                                             p_marca_entra        ,  -- marca
                                             v_fecha_ini          ,  -- f_ini
                                             v_hora_ini           ,  -- h_ini
                                             p_n_referencia       ,  -- n_referencia 
                                             v_fecha_fin          ,  -- f_fin
                                             p_folio              ,  -- folio
                                             p_proceso_cod        ,  -- folio
                                             ''                   ,  -- proceso_desmarca
                                             90                   ,  -- estado      
                                             v_cod_excep          ,  -- codigo de excepcion 
                                             p_marca_entra        ,  -- marca_causa --Es correcto poner p_marca_entra
                                             v_fecha_fin          ,  -- f_causa
                                             p_usuario
                                             );
   
         --EXECUTE PROCEDURE error  (-746,'MARCAJE: Tabla sfr_convivencia no cuadrada');
      ELSE 
   
      ---- Verifica si convive la marca entra con la marca activa:
      ---- envian p_marca_entra correspondiente, p_estado_marca = 0 y
      ---- p_codigo_rechazo = 0.
   
         LET v_contador = 0;

         FOREACH
            SELECT c.marca_activa,
                   c.rch_cod,
                   a.f_inicio
              INTO v_marca_activa,
                   r_cod_rechazo,
                   v_fecha_aux
              FROM sfr_convivencia c, 
                   sfr_marca_activa a
             WHERE a.id_derechohabiente = p_id_derechohabiente
               AND a.marca        = c.marca_activa
               AND c.marca_entra  = p_marca_entra
               AND c.rch_cod  > 0  
             ORDER  BY a.f_inicio desc
     
           -- El orden de convivencia de marcas es de la más antigua a la mas actual.
     
           LET v_contador = v_contador + 1;
     
           EXIT FOREACH;
     
        END FOREACH;
		
         -- 20201217 PLACG141 VU. Retiro Ley73 permite marcar cuando se tiene un credito 43BIS
		 IF ( v_contador = 1 AND p_proceso_cod = 1506 ) THEN
		    LET v_contador_aux = 0;
		 
		    -- clausulas para retiro Ley73
            SELECT count(*)
              INTO v_contador_aux
              FROM sfr_convivencia c, 
                   sfr_marca_activa a
             WHERE a.id_derechohabiente = p_id_derechohabiente
               AND a.marca        = c.marca_activa
               AND c.marca_entra  = p_marca_entra
			   AND a.marca NOT IN (202,212,218,220,222,223,224,226,227,232,233)
               AND c.rch_cod  > 0;
			 
			 -- si no se encontraron, entonces la marca que no convive es una de estas y el proceso de marcado de debe proceder
			 IF ( v_contador_aux < 1 ) THEN
			    LET v_contador = 0;
			 END IF;
		 END IF;
     
     
        IF ( v_contador = 1 ) THEN
           ---- Marca entrante rechazada porque no convive con la marca activa.
           ---- Inserta en cta_his_marca con marca causa.
     
           LET v_fecha_fin = TODAY;
           LET v_hora_ini  = CURRENT;
           LET p_estado_marca = 20;
     
           SET LOCK MODE TO WAIT;
     
           INSERT INTO sfr_marca_historica VALUES (
                                              p_id_derechohabiente , -- id_derechohabiente
                                              p_marca_entra        , -- marca
                                              p_n_referencia       , -- n_referencia 
                                              v_fecha_ini          , -- f_ini
                                              v_hora_ini           , -- h_ini
                                              v_fecha_fin          , -- f_fin
                                              p_folio              , -- folio
                                              p_proceso_cod        , -- proceso_marca
                                              ''                   , -- proceso_desmarca
                                              p_estado_marca       , -- estado
                                              r_cod_rechazo        , -- rch_cod
                                              v_marca_activa       , -- marca_causa
                                              v_fecha_ini          , -- f_causa 
                                              v_fecha_ini          , -- f_vigencia 
                                              p_usuario            , -- usuario que marca
                                              p_usuario              -- usuario que desmarca
                                              );
     
           SET LOCK MODE TO NOT WAIT;
     
        ELSE
     
     
        ---- vcontador = 0 por lo que la marca entrante es aceptada
        ---- (convive con marca activa).
     
           LET v_fecha_fin    = NULL;
           LET v_hora_ini     = CURRENT;
           LET p_estado_marca = 0;
           LET r_cod_rechazo  = 0;
     
           SET LOCK MODE TO WAIT;
     
           INSERT INTO sfr_marca_historica VALUES (
                                              p_id_derechohabiente ,  -- id_derechohabiente
                                              p_marca_entra        ,  -- marca
                                              p_n_referencia       ,  -- n_referencia 
                                              v_fecha_ini          ,  -- f_ini
                                              v_hora_ini           ,  -- h_ini
                                              v_fecha_fin          ,  -- f_fin
                                              p_folio              ,  -- folio
                                              p_proceso_cod        , -- proceso_marca
                                              ''                   , -- proceso_desmarca
                                              p_estado_marca       ,  -- estado
                                              r_cod_rechazo        ,  -- rch_cod
                                              p_marca_causa        ,  -- marca_causa como parametro
                                              p_fecha_causa        ,  -- f_causa , pfecha_causa como parametro
                                              v_f_vigencia         ,  -- f_vigencia , pfecha_causa como parametro
                                              p_usuario            ,  -- usuario que marca
                                              ''                      -- usuario que desmarca
                                             );
     
           INSERT INTO sfr_marca_activa VALUES (
                                              p_id_derechohabiente ,  -- id_derechohabiente
                                              p_marca_entra        ,  -- marca
                                              p_n_referencia       ,  -- n_referencia 
                                              v_fecha_ini          ,  -- f_ini
                                              v_hora_ini           ,  -- h_ini
                                              p_folio              ,  -- folio
                                              p_proceso_cod        , -- proceso_marca
                                              p_marca_causa        ,  -- marca_causa , como parametro
                                              p_fecha_causa        ,  -- fecha_causa , pfecha_causa como parametro
                                              v_f_vigencia         ,  -- f_vigencia , pfecha_causa como parametro
                                              p_usuario               -- usuario
                                             );
     
     
            SET LOCK MODE TO NOT WAIT;
     
        END IF -- v_contador
      END IF -- convivencia
   END IF -- estado_marca
  
   RETURN r_cod_rechazo;

END FUNCTION;                                                                                                                                                                                                                           