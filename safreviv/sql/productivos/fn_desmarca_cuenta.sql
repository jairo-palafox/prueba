






CREATE FUNCTION "safreviv".fn_desmarca_cuenta( p_id_derechohabiente      DECIMAL(9,0),
                                    p_marca_entra             SMALLINT,
                                    p_n_referencia             INTEGER,
                                    p_estado_marca            SMALLINT,
                                    p_marca_causa             SMALLINT,
                                    p_usuario                 CHAR(12),
                                    p_proceso_cod             SMALLINT)
   RETURNING INTEGER ;


   DEFINE v_fecha_ini      	DATE;
   DEFINE v_hora_ini       	DATETIME HOUR TO SECOND;
   DEFINE v_contador       	SMALLINT;
   DEFINE v_marca_resulta       SMALLINT;
   DEFINE v_ind_saldo      	SMALLINT;
   DEFINE v_ind_habilita     	SMALLINT;

   DEFINE v_cod_excep      	SMALLINT;
   DEFINE v_des_excep      	CHAR(100);

   DEFINE v_fecha_hoy      	DATE;
   DEFINE v_hora_actual       	DATETIME HOUR TO SECOND;
   DEFINE v_cod_rechazo         SMALLINT;
   DEFINE v_folio               DECIMAL(9,0);

   DEFINE r_marca_entra         SMALLINT;
   DEFINE r_cod_rechazo         SMALLINT;
   DEFINE r_rch_marca           SMALLINT;

   DEFINE sql_err INT;
   DEFINE isam_err INT;
   DEFINE error_info CHAR(70);



   ------------------ Valida excepciones ---------------------------
   ON EXCEPTION
      SET sql_err, isam_err, error_info

      LET v_fecha_hoy    = TODAY;
      LET v_hora_actual  = CURRENT;

      IF v_folio IS NULL THEN
         LET v_folio = 0;
      END IF

      IF sql_err = -691 THEN
         ----  Valida  constraints
         LET v_cod_excep, v_des_excep= ( SELECT cod_excepcion,
                                                des_excepcion
                                           FROM cat_excep_marca
                                          WHERE constraint = error_info
                                      ) ;
         IF v_des_excep IS NULL THEN
            CALL sp_error_marca(sql_err, isam_err, error_info,
                             p_id_derechohabiente, 0,p_marca_entra,p_n_referencia,
                             p_usuario, "desmarca_cuenta",v_hora_actual);
            LET v_des_excep = error_info ;
         ELSE

            INSERT INTO sfr_excep_marca VALUES (
                        p_id_derechohabiente,  -- nss
                        p_marca_entra ,      -- marca
                        v_fecha_hoy   ,      -- f_ini
                        v_hora_actual ,      -- h_ini
                        p_n_referencia,      -- n_referencia
                        v_fecha_hoy   ,      -- f_fin
                        v_folio       ,      -- folio
                        ''            ,      -- proceso_marca
                        p_proceso_cod ,      -- proceso_desmarca
                        90,                  -- estado Ocurrió una excepción
                        v_cod_excep,         -- codigo de  excepcion
                        p_marca_entra ,      -- marca_causa --Es correcto poner pmarca_entra
                        v_fecha_hoy   ,      -- f_causa
                        p_usuario            -- usuario 
                       );
         END IF
      ELSE
         CALL sp_error_marca(sql_err, isam_err, error_info,
                          p_id_derechohabiente, 0,p_marca_entra,p_n_referencia,
                          p_usuario, "desmarca_cuenta",v_hora_actual);
         LET v_des_excep = error_info ;
      END IF

      LET v_des_excep = 'DESMARCAJE:'||v_des_excep||' '||p_id_derechohabiente||' '||p_marca_entra;

      RETURN sql_err;

      --EXECUTE PROCEDURE error  (-746,vdes_excep);

   END EXCEPTION

   -----------------------------------------------

   LET v_fecha_hoy    = TODAY;
   LET v_hora_actual  = CURRENT;

   LET r_cod_rechazo  = 0;
   LET r_rch_marca  = 0;

   LET v_contador = 0;

   -------  Busca  marca_entra en sfr_marca_activa y verifica  si inhabilita o no
   -------  en cat_marca

   FOREACH
      SELECT f_inicio         ,
             h_inicio         ,
             marca_resulta    ,
             folio            ,
             ind_saldo        ,
             ind_habilita
        INTO v_fecha_ini      ,
             v_hora_ini       ,
             v_marca_resulta  ,
             v_folio          ,
             v_ind_saldo      ,
             v_ind_habilita
        FROM sfr_marca_activa c, 
             sfr_marca t
       WHERE c.id_derechohabiente = p_id_derechohabiente
         AND c.marca  = p_marca_entra
         AND n_referencia = p_n_referencia
         AND t.marca  = c.marca
       ORDER  BY 1,2

      LET v_contador = v_contador + 1;

      EXIT FOREACH;

   END FOREACH;

   IF ( v_contador = 1) THEN

   --- Para prevenir alguna actualizacion en catalogos

      SET LOCK MODE TO WAIT ;

   --- Borra marca activa

      DELETE
        FROM sfr_marca_activa
       WHERE id_derechohabiente = p_id_derechohabiente
         AND marca        = p_marca_entra
         AND n_referencia = p_n_referencia
         AND f_inicio     = v_fecha_ini;

      IF v_ind_habilita = 2 OR 
         v_ind_habilita = 3 THEN
          ---  Verifica si es una cuenta  a  habilitar
          -- Cierra  registro en el historico

          UPDATE sfr_marca_historica
             SET f_fin        = v_fecha_hoy,
                 proceso_desmarca = p_proceso_cod,
                 usuario_desmarca = p_usuario
           WHERE id_derechohabiente  = p_id_derechohabiente
             AND marca        = p_marca_entra
             AND n_referencia = p_n_referencia
             AND f_inicio     = v_fecha_ini;

          -- Inserta  registro en historico como habilitado


          INSERT INTO sfr_marca_historica VALUES (
                                             p_id_derechohabiente,  -- p_id_derechohabiente
                                             v_marca_resulta    ,  -- marca
                                             p_n_referencia     ,  -- n_referencia
                                             v_fecha_hoy        ,  -- f_ini
                                             v_hora_actual      ,  -- h_ini
                                             v_fecha_hoy        ,  -- f_fin
                                             v_folio            ,  -- folio
                                             p_proceso_cod      ,  -- proceso_marca
                                             p_proceso_cod      ,  -- proceso_desmarca
                                             p_estado_marca     ,  -- estado      --El pestado_marca como param
                                             r_rch_marca        ,  -- rch_cod --El codigo rechazo como param
                                             p_marca_causa      ,  -- marca_causa
                                             v_fecha_hoy        ,  -- f_causa
                                             v_fecha_hoy        ,  -- f_causa
                                             p_usuario          ,  -- usuario que marca
                                             p_usuario             -- usuario que desmarca
                                         );
         
         IF v_ind_habilita = 3 THEN                               
            --Se reactiva indicador estado de cuenta por marca 110 de Unificación (unificación inversa)
            UPDATE afi_derechohabiente
               SET ind_estado_cuenta = 0,
                   f_estado_cuenta = v_fecha_hoy
             WHERE id_derechohabiente = p_id_derechohabiente;
         END IF
      ELSE -- vind_habilita = 0 o 1

          -- Cierra  registro en el historico
          IF p_estado_marca = 40 THEN
             LET r_rch_marca = 900;
          END IF

          UPDATE sfr_marca_historica
             SET f_fin           = v_fecha_hoy   ,
                 estado_marca    = p_estado_marca,
                 proceso_desmarca= p_proceso_cod ,
                 rch_cod         = r_rch_marca ,
                 marca_causa     = p_marca_causa ,
                 f_marca_causa   = v_fecha_hoy   ,
                 usuario_desmarca = p_usuario
          WHERE  id_derechohabiente = p_id_derechohabiente
            AND  marca        = p_marca_entra
            AND  n_referencia = p_n_referencia
            AND  f_inicio     = v_fecha_ini;

         IF ( v_ind_saldo = 1 ) THEN  -- Checa si tiene que verificar Saldo
             IF EXISTS (
                    SELECT fondo_inversion ,
                           subcuenta       ,
                           sum(monto_acciones) monto_acciones
                      FROM cta_movimiento
                     WHERE id_derechohabiente = p_id_derechohabiente
                     GROUP BY 1,2
                     HAVING sum(monto_acciones) > 0
                      ) THEN
                LET v_ind_habilita = 0 ; -- No inhabilita cuenta con Saldo > 0
             END IF
         END IF --- vind_saldo  = 1

      --- En caso que inhabilite marca_entra, Ingresa una cuenta inhabilitada

         IF ( v_ind_habilita = 1 )  THEN  -- Verifica si inhabilita cuenta
                                     -- en caso contrario solo desmarca
                                     -- sin inhabilitar estado = 40 o 30

            IF (p_estado_marca = 0 ) THEN -- Desmarcaje procedente

               --  Agrega registro de inhabilitacion

               EXECUTE FUNCTION fn_marca_cuenta (
                                                 p_id_derechohabiente ,  -- id_derechohabiente
                                                 v_marca_resulta      ,  -- marca_cod
                                                 p_n_referencia       ,  -- n_referencia
                                                 v_folio              ,  -- n_referencia
                                                 p_estado_marca       ,  -- pestado_marca = 0
                                                 r_cod_rechazo        ,  -- rechazo_cod = 0
                                                 p_marca_entra        ,  -- marca causa
                                                 v_fecha_hoy          ,  -- fecha_causa
                                                 p_usuario            ,  -- usuario que marca
                                                 p_proceso_cod        )  -- proceso_cod que marca
                                                        INTO r_cod_rechazo;
                IF r_cod_rechazo = 0 THEN

                   UPDATE afi_derechohabiente
                      SET ind_estado_cuenta = 1,
                          f_estado_cuenta = v_fecha_hoy
                    WHERE id_derechohabiente = p_id_derechohabiente;

                END IF

            END IF ---p_estado_marca = 0  --- Procede con inhabilitacion

         END IF --- v_ind_habilita = 1 Procede inhabilita por saldo/catalogo

      SET LOCK MODE TO NOT WAIT ;

      END IF --  v_ind_habilita = 2 Desmarcar  inhabilitadas

   END IF  ---  v_contador = 1 Encontro registros para desmarcar

   RETURN r_cod_rechazo;

END FUNCTION;


