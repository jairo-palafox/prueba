






CREATE PROCEDURE "safreviv".sp_modifica_mandato(p_id_solicitud DECIMAL(10), p_credito DECIMAL(10), 
                                     p_id_derechohabiente DECIMAL(9), p_tpo_accion CHAR(1))
   RETURNING INTEGER;

   --mdt_ctr_mandato
   DEFINE v_id_derechohabiente DECIMAL(9);
   DEFINE v_id_ctr_mandato     DECIMAL(9);
   DEFINE v_estado             SMALLINT;

   DEFINE val1  CHAR(40) ;
   DEFINE val2  CHAR(40) ;
   DEFINE vtxt1 CHAR(300);

   DEFINE nombre_campo             CHAR(40);
   DEFINE vtabid                   SMALLINT; 
   DEFINE vid_cat_dato_actualizado SMALLINT;
   DEFINE vid__mdt_his_mandato     SMALLINT;
   
   DEFINE v_id_cat_mandato         SMALLINT;
   DEFINE v_cve_mandato            CHAR(18);
   DEFINE v_id_det_ctr_mandato     DECIMAL(9,0);
   DEFINE v_existe                 INTEGER;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_modifica_mandato.trace';
   --TRACE "Inicia SPL de modificaciones";
   --TRACE "P1 "||p_id_solicitud;
   --TRACE "P2 "||p_id_derechohabiente;
   --TRACE "P3 "||p_credito;
   --TRACE "P4 "||p_tpo_accion;
   --TRACE "Valida existencia en mdt_ctr_mandato";

   -- inicializa id de derechohabiente
   SELECT NVL(id_derechohabiente,0), id_ctr_mandato
     INTO v_id_derechohabiente, v_id_ctr_mandato
     FROM safre_viv:mdt_ctr_mandato 
    WHERE id_derechohabiente = p_id_derechohabiente
      AND id_credito = p_credito;
   
   --TRACE "v_id_derechohabiente = "||v_id_derechohabiente;
   --TRACE "p_credito            = "||p_credito;
   
   IF(v_id_derechohabiente = 0 OR v_id_derechohabiente IS NULL)THEN
      -- rechazo por derechohabiente sin mandato registrado
      UPDATE safre_viv:mdt_solicitud_mandato 
         SET estado = 106, diagnostico = '003'
       WHERE id_solicitud_mandato = p_id_solicitud;
   --TRACE 'rechazo por derechohabiente sin mandato registrado';
       RETURN 106;
   END IF;

   --TRACE "Recupera id_cat_mandato de solicitud";
   SELECT p.id_cat_mandato  ,
          p.cve_mandato    
     INTO v_id_cat_mandato ,
          v_cve_mandato
     FROM safre_viv:mdt_solicitud_mandato   s , 
          safre_viv:mdt_cat_mandato         c , 
          safre_viv:mdt_cat_mandato_paquete p
    WHERE s.id_solicitud_mandato = p_id_solicitud
      AND s.cve_mandato    = p.cve_mandato 
      AND p.id_cat_mandato = c.id_cat_mandato ;
   
   --TRACE "Valida existencia en mdt_det_ctr_mandato - v_id_cat_mandato: "||v_id_cat_mandato;
   SELECT UNIQUE estado, id_det_ctr_mandato
     INTO v_estado, v_id_det_ctr_mandato
     FROM safre_viv:mdt_det_ctr_mandato
     WHERE id_ctr_mandato = v_id_ctr_mandato
     AND   id_cat_mandato = v_id_cat_mandato
     AND   cve_mandato    = v_cve_mandato  ;
   
   --TRACE "v_estado: "||v_estado;
   --TRACE "v_id_det_ctr_mandato: "||v_id_det_ctr_mandato;
   IF(v_estado = 0 OR v_estado IS NULL)THEN
      -- rechazo porque no existe mandato
      UPDATE safre_viv:mdt_solicitud_mandato 
         SET estado = 106, diagnostico = '004'
       WHERE id_solicitud_mandato = p_id_solicitud;
   --TRACE 'rechazo porque no existe mdt_cat_mandato';
       RETURN 106;
   END IF
   
   --TRACE "Valida que no este en baja el mdt_det_ctr_mandato";
   IF(v_estado = 106)THEN         
      -- rechazo porque mandato ya se encuentra con baja
      UPDATE safre_viv:mdt_solicitud_mandato 
         SET estado = 106, diagnostico = '005'
       WHERE id_solicitud_mandato = p_id_solicitud;
   --TRACE 'rechazo porque mandato ya se encuentra con baja';
       RETURN 106;
   END IF;

   -- M  Modificacion
   IF(p_tpo_accion = 'M')THEN
  --TRACE "Valida estado 104 en tipo M";
      -- rechazo porque mandato no vigente
      IF(v_estado = 104)THEN         
         UPDATE safre_viv:mdt_solicitud_mandato 
            SET estado = 106, diagnostico = '006'
          WHERE id_solicitud_mandato = p_id_solicitud;
   --TRACE 'rechazo porque mandato no vigente';
         RETURN 106;
      END IF;
   ELSE
      IF(p_tpo_accion = 'R')THEN
      -- R reactivacion
   --TRACE "Valida estado 104 en tipo R";
         IF(v_estado = 104)THEN
            UPDATE safre_viv:mdt_solicitud_mandato 
               SET estado = 101
             WHERE id_solicitud_mandato = p_id_solicitud;
         END IF;
      END IF;
      
   END IF;
   
   IF(v_estado = 103) OR (v_estado = 104 AND p_tpo_accion = 'R') THEN
      
      -- AHM TMP Falta insert a historicos
   --TRACE "Inicia insert en historicos";
      SELECT a.tabid 
      INTO vtabid	
      from safre_viv:systables a
      WHERE a.tabname = "mdt_solicitud_mandato";
      
   --TRACE "tabname - "||vtabid;
      FOREACH SELECT a.colname 
                INTO nombre_campo 
                FROM safre_viv:syscolumns a
               WHERE a.tabid = vtabid
   
         SELECT NVL(COUNT(*),0) INTO v_existe
         FROM safre_viv:mdt_cat_dato_actualizado a
         WHERE a.cve_natural = nombre_campo;
         
    --TRACE "Verificando campo en mdt_cat_dato_actualizado: "||nombre_campo;
    --TRACE "v_existe - "||v_existe;
         IF v_existe > 0 THEN
            IF nombre_campo <> 'estado' THEN
               LET vtxt1 = " SELECT a."||nombre_campo||" FROM safre_viv:mdt_det_ctr_mandato a WHERE id_det_ctr_mandato = "||v_id_det_ctr_mandato;
               PREPARE ptxt1 FROM vtxt1;
               DECLARE cptxt1 CURSOR FOR ptxt1;
               OPEN cptxt1;
               FETCH cptxt1 INTO val1;
               CLOSE cptxt1;
               FREE cptxt1;
               FREE ptxt1;               
               LET vtxt1 = " SELECT a."||nombre_campo||" FROM safre_viv:mdt_solicitud_mandato a WHERE id_solicitud_mandato = "||p_id_solicitud;
               PREPARE ptxt2 FROM vtxt1;
               DECLARE cptxt2 CURSOR FOR ptxt2;
               OPEN cptxt2;
               FETCH cptxt2 INTO val2;
               CLOSE cptxt2;
               FREE cptxt2;
               FREE ptxt2;               
               
   --TRACE "Campo: Antes - "||val1||", despues - "||val2;
               IF val1 <> val2 THEN
                 
                  LET vid__mdt_his_mandato = seq_mdt_his_mandato.NEXTVAL;
 
                  SELECT a.id_cat_dato_actualizado
                  INTO vid_cat_dato_actualizado
                  FROM safre_viv:mdt_cat_dato_actualizado a
                  WHERE a.cve_natural = nombre_campo;
               -- se agrega el parametro de usuario p_v_usuairo SDL JUL12
                  INSERT INTO safre_viv:mdt_his_mandato VALUES (
                              vid__mdt_his_mandato     ,   
                              v_id_det_ctr_mandato     ,
                              vid_cat_dato_actualizado ,
                              TODAY ,
                              val1  ,
                              val2,
                              p_id_solicitud 
                              );  
                  
   --TRACE "Arma update";
                  LET vtxt1 = "UPDATE safre_viv:mdt_det_ctr_mandato SET "||
                              nombre_campo||" = '"||val2||"' WHERE id_det_ctr_mandato = "||v_id_det_ctr_mandato||";";
   --TRACE "Prepare SQL - "||vtxt1;
                  EXECUTE IMMEDIATE vtxt1;

               END IF;
            ELSE
               LET vid__mdt_his_mandato = seq_mdt_his_mandato.NEXTVAL;
 
               SELECT a.id_cat_dato_actualizado
               INTO vid_cat_dato_actualizado
               FROM safre_viv:mdt_cat_dato_actualizado a
               WHERE a.cve_natural = nombre_campo;
               
               IF(p_tpo_accion = 'M')THEN
                  LET val2 = 103;
               ELSE
                  LET val2 = 103;
               END IF;
               
               INSERT INTO safre_viv:mdt_his_mandato VALUES (
                           vid__mdt_his_mandato          ,   
                           v_id_det_ctr_mandato     ,
                           vid_cat_dato_actualizado ,
                           TODAY ,
                           v_estado ,
                           val2,
                           p_id_solicitud);  
                           
                UPDATE safre_viv:mdt_det_ctr_mandato
                  SET estado = val2
                 WHERE id_det_ctr_mandato = v_id_det_ctr_mandato;
                 
            END IF;
         END IF;
      END FOREACH;
      
           UPDATE safre_viv:mdt_solicitud_mandato 
           SET estado = 102 
           WHERE id_solicitud_mandato = p_id_solicitud;
   END IF;
   
   --TRACE "Finaliza correctamente SPL";
   
   RETURN 102;
   
END PROCEDURE ;


