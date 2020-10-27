






CREATE PROCEDURE "safreviv".sp_registra_bitacora_acceso(p_usuario CHAR(20), 
                                    p_estado_acceso SMALLINT, 
                                    p_host_ip CHAR(20), 
                                    p_id_session_web CHAR(60))
                                    
   DEFINE v_fecha      	      DATE;
   DEFINE v_hora       	      DATETIME HOUR TO SECOND;
   DEFINE v_estado_ant        SMALLINT;
   DEFINE v_intentos          SMALLINT;
   DEFINE v_ind_activo        SMALLINT;
   DEFINE v_estatus_usu       CHAR(15);

   LET v_fecha    = TODAY;
   LET v_hora  = CURRENT;
   
   --Se inserta en la bitacora el acceso
   INSERT INTO safre_viv:seg_bitacora_acceso VALUES(p_usuario,
                                          v_fecha,
                                          v_hora,
                                          p_estado_acceso,
                                          p_host_ip,
                                          p_id_session_web);

   --Si el error fue por la firma de LDAP se busca el numero de intentos erroneos
   IF (p_estado_acceso = 4 OR p_estado_acceso = 5) THEN
      --Se valida si los ultimos 3 intentos de acceso fueron erroneos
      LET v_intentos = 0;
      FOREACH
         SELECT FIRST 3 
            f_acceso, 
            h_acceso, 
            estado_acceso
         INTO
            v_fecha,
            v_hora,
            v_estado_ant
         FROM safre_viv:seg_bitacora_acceso 
         WHERE usuario_cod = p_usuario
         ORDER BY 
            f_acceso DESC, 
            h_acceso DESC

         IF (v_estado_ant = 4 OR v_estado_ant = 5) THEN
            LET v_intentos = v_intentos + 1;
         END IF
      END FOREACH;

      --Si se encuentran 3 intentos erroneos seguidos se bloquea el usuario en safre
      IF (v_intentos = 3) THEN
         -- Recupera indicador de estatus de usuario
         SELECT ind_activo 
           INTO v_ind_activo
           FROM seg_usuario 
          WHERE usuario_cod = p_usuario;
          
         UPDATE safre_viv:seg_usuario 
            SET ind_activo = 3, -- Usuario bloqueado
                f_modifica = TODAY 
          WHERE usuario_cod = p_usuario;
         
         -- Regitra bitácora de catálogo de usuario
         IF( v_ind_activo = 0 )THEN
            LET v_estatus_usu = 'INACTIVO';
         ELSE
            LET v_estatus_usu = 'ACTIVO';
         END IF
         INSERT INTO seg_bitacora_catalogo_usuario
         (usuario_mod,
          desc_evento,
          valor_actual,
          valor_anterior,
          f_modifica,
          hora_mod,
          usuario_log)
         VALUES(p_usuario,
                'BLOQUEO AUTOMÁTICO DE USUARIO',
                'BLOQUEADO',
                v_estatus_usu,
                TODAY,
                CURRENT HOUR TO SECOND,
                '');

      END IF
   END IF

END PROCEDURE;


